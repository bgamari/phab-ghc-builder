{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Build where

import Shelly.Lifted
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Time.Clock
import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Error
import System.IO (openFile, IOMode(..), hClose)
import System.Directory
import qualified Filesystem.Path.CurrentOS as Path
import Prelude hiding (FilePath)

import Servant
import Phabricator.Types

-- * Things given to us by Harbormaster
-- | A git commit SHA
newtype Commit = Commit Text
               deriving (Eq, Ord, Show, ToText, FromText)

repo :: Repository
repo = Repo "rGHC"

sourceRepo :: Text
sourceRepo = "git://git.haskell.org/ghc"

data Options = Options { maxThreads :: Maybe Int
                       , referenceRepo :: Maybe FilePath
                       , archivePath :: FilePath
                       }

data BuildDesc = BuildDesc Options BuildId

type BuildM = ReaderT BuildDesc Sh

getOptions :: BuildM Options
getOptions = do
    BuildDesc opts _ <- ask
    return opts

getBuildId :: BuildM BuildId
getBuildId = do
    BuildDesc _ buildId <- ask
    return buildId

runBuildM :: BuildM a -> Options -> BuildId -> IO a
runBuildM action opts bid = shelly $ do
    ref <- traverse canonicalize (referenceRepo opts)
    let opts' = opts { referenceRepo = ref }
    runReaderT action (BuildDesc opts' bid)

cpuCount :: BuildM Int
cpuCount =
    fromMaybe 1 <$> runMaybeT (config <|> windows <|> linux <|> freebsd)
  where
    config, windows, linux, freebsd :: MaybeT BuildM Int
    config = MaybeT $ fmap maxThreads getOptions
    windows = MaybeT $ (>>= readT) <$> get_env "NUMBER_OF_PROCESSORS"
    linux = MaybeT $ readT <$> cmd "getconf" "_NPROCESSORS_ONLN"
    freebsd = MaybeT $ readT <$> cmd "getconf" "NPROCESSORS_ONLN"

    readT :: Read a => Text -> Maybe a
    readT = readZ . T.unpack

logStr :: String -> BuildM ()
logStr = liftIO . putStr

timeIt :: String -> BuildM a -> BuildM a
timeIt what action = do
    start <- liftIO getCurrentTime
    logStr $ "- "<>what
    r <- action
    end <- liftIO getCurrentTime
    let delta = end `diffUTCTime` start
    logStr $ " took "<>show delta<>"\n"
    return r

cloneGhc :: RepoDir -> BuildM ()
cloneGhc (RepoDir repoDir) = timeIt "cloning tree" $ do
    opts <- getOptions
    rm_rf repoDir
    case referenceRepo opts of
      Just refRepo -> do
        refExists <- liftIO $ doesDirectoryExist (Path.encodeString refRepo)
        unless refExists $
            cmd "git" "clone" "--bare" sourceRepo refRepo
        () <- cmd "git" "clone" "--reference" refRepo sourceRepo repoDir
        chdir repoDir $
            cmd "git" "submodule" "update" "--init"
      Nothing -> do
        cmd "git" "clone" sourceRepo repoDir
        cmd "git" "submodule" "update" "--init"

newtype RepoDir = RepoDir FilePath

inRepo :: RepoDir -> BuildM a -> BuildM a
inRepo (RepoDir dir) = chdir dir

applyDiff :: RepoDir -> Diff -> BuildM ()
applyDiff repoDir (Diff d) = inRepo repoDir $
    cmd "arc" "patch" "--nobranch" "--force" "--nocommit" "--diff" (T.pack $ show d)

checkout :: RepoDir -> Commit -> BuildM ()
checkout repoDir (Commit commit) = timeIt "checking out commit" $ inRepo repoDir $
    cmd "git" "checkout" commit

updateSubmodules :: RepoDir -> BuildM ()
updateSubmodules repoDir = timeIt "updating submodules" $ inRepo repoDir $
    cmd "git" "submodule" "update" "--init"

logCmd :: String -> FilePath -> [Text] -> BuildM ExitCode
logCmd logName cmd args = do
    opts <- getOptions
    let log = archivePath opts </> toText logName
    hdl <- liftIO $ openFile (Path.encodeString log) WriteMode
    let handles = [ OutHandle $ UseHandle hdl
                  , ErrorHandle $ UseHandle hdl
                  ]
    runHandles cmd args handles (\_ _ _ -> return ())
    c <- lastExitCode
    liftIO $ T.hPutStrLn hdl $ T.pack $ "Exited with code "<>show c
    liftIO $ hClose hdl
    return $ case c of
                 0 -> ExitSuccess
                 _ -> ExitFailure c

validate :: RepoDir -> FilePath -> BuildM ExitCode
validate repoDir log = timeIt "validating" $ inRepo repoDir $ errExit False $ do
    opts <- getOptions
    mapM_ (setenv "THREADS" . T.pack . show) (maxThreads opts)
    c <- logCmd "validate" "sh" ["validate"]
    liftIO $ putStrLn $ case c of
                            ExitSuccess -> "Validate finished successfully"
                            ExitFailure c -> "Validate failed with exit code "<>show c
    return c

archiveFile :: FilePath -> BuildM ()
archiveFile path = do
    opts <- getOptions
    bid <- getBuildId
    let compressedPath = path <.> "xz"
        archiveDir = archivePath opts </> toText bid
    () <- cmd "xz" "-9f" path
    mkdir_p archiveDir
    mv compressedPath archiveDir

showTestsuiteSummary :: RepoDir -> BuildM ()
showTestsuiteSummary (RepoDir dir) = do
    let summary = dir </> "testsuite_summary.txt"
    exists <- test_f summary
    when exists $ do
      c <- readfile summary
      liftIO $ do
          putStrLn ""
          putStrLn "================== Testsuite summary =================="
          T.putStrLn c

getMakeVar :: RepoDir -> Text -> BuildM Text
getMakeVar repoDir var = inRepo repoDir $ do
    out <- cmd "make" "--silent" "show!" ("VALUE="<>var)
    let res :: Maybe Text
        res = T.stripPrefix (var<>"=\"") out >>= T.stripSuffix "\""
    case res of
        Just o -> pure o
        Nothing -> fail $ "Failed to get make variable \""<>T.unpack out<>"\": "<>T.unpack out

binaryDist :: RepoDir -> BuildM [FilePath]
binaryDist repoDir = inRepo repoDir $ do
    logCmd "bindist" "make" ["binary-dist"]
    ls "*.tar.xz"

testDiff :: FilePath -> Revision -> Diff -> Commit -> BuildM ExitCode
testDiff rootDir rev diff baseCommit = chdir rootDir $ do
    let repoDir = RepoDir "ghc-test"
    cloneGhc repoDir
    checkout repoDir baseCommit
    updateSubmodules repoDir
    applyDiff repoDir diff
    updateSubmodules repoDir
    touchfile "build.log"
    logPath <- canonicalize "build.log"
    code <- validate repoDir logPath
    archiveFile logPath
    showTestsuiteSummary repoDir
    return code

testCommit :: FilePath -> Commit -> BuildM ExitCode
testCommit rootDir commit = chdir rootDir $ do
    let repoDir = RepoDir "ghc-test"
    cloneGhc repoDir
    checkout repoDir commit
    updateSubmodules repoDir
    touchfile "build.log"
    logPath <- canonicalize "build.log"
    code <- validate repoDir logPath
    archiveFile logPath
    showTestsuiteSummary repoDir
    return code
