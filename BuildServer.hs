{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Exception
import Control.Monad (forever, replicateM_)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Exit

import Options.Applicative
import qualified Data.Text as T

import Data.Aeson
import Servant
import Servant.Client
import Servant.Server
import Network.Wai
import Network.Wai.Handler.Warp

import Api
import Build
import Build.Parsers
import Harbormaster

data ServerOpts = ServerOpts { maxBuilds :: Int  -- ^ maximum number of concurrent builds
                             , rootDir   :: FilePath
                             , port      :: Int
                             , apiToken  :: ApiToken
                             , buildOpts :: Options
                             }

data BuildTask = BuildTask { buildPhid   :: Phid
                           , buildId     :: BuildId
                           , buildAction :: BuildM ExitCode
                           }

type BuildQueue = TQueue BuildTask

serverOpts :: Parser ServerOpts
serverOpts = ServerOpts
    <$> option auto (long "max-builds" <> short 'B' <> help "Maximum number of concurrent builds" <> value 1)
    <*> option auto (long "root" <> short 'd' <> help "Source root directory" <> value ".")
    <*> option auto (long "port" <> short 'p' <> help "Port number on which to listen" <> value 80)
    <*> option (ApiToken . T.pack <$> str) (long "api-token" <> short 'a' <> help "The API token to use to submit results back to Phabricator")
    <*> options

phabBase :: BaseUrl
phabBase = BaseUrl Https "phabricator.haskell.org" 80

handleAny :: SomeException -> IO ()
handleAny = print

worker :: ServerOpts -> TQueue BuildTask -> IO ()
worker opts buildQueue = forever $ handle handleAny $ do
  b <- atomically $ readTQueue buildQueue
  let phid = buildPhid b
  code <- runBuildM (buildAction b) (buildOpts opts) (buildId b)
  r <- runEitherT $ case code of
    ExitSuccess   -> sendMessage phabBase (apiToken opts) phid $ Message TargetPassed []
    ExitFailure _ -> sendMessage phabBase (apiToken opts) phid $ Message TargetFailed []
  case r of
    Left err -> error $ show err
    Right () -> return ()

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> serverOpts) mempty
  buildQueue <- newTQueueIO
  replicateM_ (maxBuilds opts) $ async $ worker opts buildQueue
  run (port opts) $ serve Api.api $ server opts (atomically . writeTQueue buildQueue)
  return ()

server :: ServerOpts -> (BuildTask -> IO ()) -> Server Api
server opts q = buildDiff opts q :<|> buildCommit opts q

buildDiff :: ServerOpts -> (BuildTask -> IO ())
          -> Maybe BuildId
          -> Maybe Revision -> Maybe Diff
          -> Maybe Commit -> Maybe Phid
          -> EitherT ServantErr IO ()
buildDiff opts queueBuild (Just buildId) (Just rev) (Just diff) (Just baseCommit) (Just phid) =
  liftIO $ queueBuild $ BuildTask phid buildId $ testDiff rev diff baseCommit
buildDiff _ _ _ _ _ _ _ = fail "ouch"

buildCommit :: ServerOpts -> (BuildTask -> IO ())
            -> Maybe BuildId -> Maybe Commit -> Maybe Phid
            -> EitherT ServantErr IO ()
buildCommit opts queueBuild (Just buildId) (Just commit) (Just phid) =
  liftIO $ queueBuild $ BuildTask phid buildId $ testCommit commit
buildCommit _ _ _ _ _ = fail "ouch"
