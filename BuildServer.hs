{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Exception
import Control.Monad (forever, replicateM_)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.String (fromString)
import System.Exit

import Options.Applicative
import qualified Data.Text as T
import System.IO.Temp

import Servant
import Servant.Client
import Network.Wai.Handler.Warp as Warp

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
                           , buildAction :: FilePath -> BuildM ExitCode
                           }

serverOpts :: Parser ServerOpts
serverOpts = ServerOpts
    <$> option auto (long "max-builds" <> short 'B' <> help "Maximum number of concurrent builds" <> value 1)
    <*> option auto (long "root" <> short 'd' <> help "Source root directory" <> value ".")
    <*> option auto (long "port" <> short 'p' <> help "Port number on which to listen" <> value 80)
    <*> option (ApiToken . T.pack <$> str) (long "api-token" <> short 'a' <> help "The API token to use to submit results back to Phabricator")
    <*> options

phabBase :: BaseUrl
phabBase = BaseUrl Https "phabricator.haskell.org" 443

handleAny :: SomeException -> IO ()
handleAny = print

worker :: ServerOpts -> TQueue BuildTask -> IO ()
worker opts buildQueue = forever $ handle handleAny $ do
  b <- atomically $ readTQueue buildQueue
  withTempDirectory (rootDir opts) "build." $ \dir -> do
    let phid = buildPhid b
    code <- runBuildM (buildAction b dir) (buildOpts opts) (buildId b)
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
  Warp.run (port opts) $ serve Api.api $ server (atomically . writeTQueue buildQueue)
  return ()

server :: (BuildTask -> IO ()) -> Server Api
server q = buildDiff q :<|> buildCommit q

buildDiff :: (BuildTask -> IO ())
          -> Maybe BuildId
          -> Maybe Revision -> Maybe Diff
          -> Maybe Commit -> Maybe Phid
          -> EitherT ServantErr IO ()
buildDiff queueBuild (Just buildId) (Just rev) (Just diff) (Just baseCommit) (Just phid) =
  liftIO $ queueBuild $ BuildTask phid buildId $ \dir -> testDiff (fromString dir) rev diff baseCommit
buildDiff _ a b c d e = fail $ "invalid build diff request"<>show (a,b,c,d,e)

buildCommit :: (BuildTask -> IO ())
            -> Maybe BuildId -> Maybe Commit -> Maybe Phid
            -> EitherT ServantErr IO ()
buildCommit queueBuild (Just buildId) (Just commit) (Just phid) =
  liftIO $ queueBuild $ BuildTask phid buildId $ \dir -> testCommit (fromString dir) commit
buildCommit _ a b c = fail $ "invalid build commit request: "<>show (a,b,c)
