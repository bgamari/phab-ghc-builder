{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
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

serverOpts :: Parser ServerOpts
serverOpts = ServerOpts
    <$> option auto (long "max-builds" <> short 'B' <> help "Maximum number of concurrent builds" <> value 1)
    <*> option auto (long "root" <> short 'd' <> help "Source root directory" <> value ".")
    <*> option auto (long "port" <> short 'p' <> help "Port number on which to listen" <> value 80)
    <*> option (ApiToken . T.pack <$> str) (long "api-token" <> short 'a' <> help "The API token to use to submit results back to Phabricator")
    <*> options

phabBase :: BaseUrl
phabBase = BaseUrl Https "phabricator.haskell.org" 80

app :: ServerOpts -> Application
app opts = serve Api.api (server opts)

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> serverOpts) mempty
  run (port opts) $ app opts
  return ()

server :: ServerOpts -> Server Api
server opts = buildDiff opts :<|> buildCommit opts

buildDiff :: ServerOpts
          -> Maybe BuildId
          -> Maybe Revision -> Maybe Diff
          -> Maybe Commit -> Maybe Phid
          -> EitherT ServantErr IO ()
buildDiff opts (Just buildId) (Just rev) (Just diff) (Just baseCommit) (Just phid) = do
  liftIO $ runBuild opts phid $ testDiff rev diff baseCommit
buildDiff _ _ _ _ _ _ = fail "ouch"

buildCommit :: ServerOpts -> Maybe BuildId -> Maybe Commit -> Maybe Phid -> EitherT ServantErr IO ()
buildCommit opts (Just buildId) (Just commit) (Just phid) =
  liftIO $ runBuild opts phid $ testCommit commit
buildCommit _ _ _ _ = fail "ouch"


runBuild :: ServerOpts -> Phid -> BuildM ExitCode -> IO ()
runBuild opts phid action = do
  code <- runBuildM action (buildOpts opts)
  r <- runEitherT $ case code of
    ExitSuccess   -> sendMessage phabBase (apiToken opts) phid $ Message TargetPassed []
    ExitFailure _ -> sendMessage phabBase (apiToken opts) phid $ Message TargetFailed []
  case r of
    Left err -> error $ show err
    Right () -> return ()
