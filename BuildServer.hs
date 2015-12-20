{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Concurrent.STM

import Options.Applicative
import qualified Data.Text as T

import Data.Aeson
import Servant
import Servant.Client
import Servant.Server

import Api
import Build
import Build.Parsers
import Harbormaster

data ServerOpts = ServerOpts { maxBuilds :: Int  -- ^ maximum number of concurrent builds
                             , rootDir   :: FilePath
                             , port      :: Int
                             , buildOpts :: Options
                             }

opts = ServerOpts
    <$> option auto (long "max-builds" <> short 'B' <> help "Maximum number of concurrent builds")
    <*> option auto (long "root" <> short 'd' <> help "Source root directory")
    <*> option auto (long "port" <> short 'p' <> help "Port number on which to listen")
    <*> options

-- http://host/build/commit?id=${build.id}&commit=${buildable.commit}&phid=${target.phid}

main :: IO ()
main = return ()

server :: ServerOpts -> Server Api
server opts = buildDiff opts :<|> buildCommit opts

buildDiff :: ServerOpts
          -> Maybe BuildId
          -> Maybe Revision -> Maybe Diff
          -> Maybe Commit -> Maybe Phid
          -> EitherT ServantErr IO ()
buildDiff opts (Just buildId) (Just rev) (Just diff) (Just baseCommit) (Just phid) = do
  code <- liftIO $ flip runBuildM (buildOpts opts) $ testDiff rev diff baseCommit
  return ()
buildDiff _ _ _ _ _ _ = fail "ouch"

buildCommit :: ServerOpts -> Maybe BuildId -> Maybe Commit -> Maybe Phid -> EitherT ServantErr IO ()
buildCommit opts (Just buildId) (Just commit) (Just phid) = return ()
buildCommit _ _ _ _ = fail "ouch"
