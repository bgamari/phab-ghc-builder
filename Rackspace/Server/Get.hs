{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Rackspace.Server.Get where

import qualified Data.Text as T
import Data.Aeson hiding (Error)
import Control.Monad.Trans.Either

import Servant
import Servant.Client
import Rackspace.Types as Types

type Api = "servers" :> Header "X-Auth-Token" AuthToken
        :> Capture "server" (UUID Types.Server) :> Get '[JSON] ServerInfo

data Status = Active | HardReboot | Migrating | Password | Reboot
            | Rebuild | Resize | Build | Error
            | Paused | Rescue | VerifyResize | RevertResize | Deleted
            | Shutoff | Suspended
            deriving (Show, Eq, Ord, Enum, Bounded)

data ServerInfo = ServerInfo { serverStatus     :: Status
                             , serverAccessIPv4 :: Maybe T.Text
                             , serverAccessIPv6 :: Maybe T.Text
                             }
                deriving (Show)

getServerInfo :: BaseUrl -> AuthToken -> UUID Types.Server -> EitherT ServantError IO ServerInfo
getServerInfo baseUrl authToken req =
    client api baseUrl (Just authToken) req
  where
    api :: Proxy Api
    api = Proxy

instance FromJSON ServerInfo where
    parseJSON = withObject "server" $ \obj ->
      ServerInfo <$> obj .:  "status"
                 <*> obj .:? "accessIPv4"
                 <*> obj .:? "accessIPv6"

instance FromJSON Status where
    parseJSON "ACTIVE"       = pure Active
    parseJSON "HARDREBOOT"   = pure HardReboot
    parseJSON "MIGRATING"    = pure Migrating
    parseJSON "PASSWORD"     = pure Password
    parseJSON "REBOOT"       = pure Reboot
    parseJSON "REBUILD"      = pure Rebuild
    parseJSON "RESIZE"       = pure Resize
    parseJSON "BUILD"        = pure Build
    parseJSON "ERROR"        = pure Error
    parseJSON "PAUSED"       = pure Paused
    parseJSON "RESCUE"       = pure Rescue
    parseJSON "VERIFYRESIZE" = pure VerifyResize
    parseJSON "REVERTRESIZE" = pure RevertResize
    parseJSON "DELETED"      = pure Deleted
    parseJSON "SHUTOFF"      = pure Shutoff
    parseJSON "SUSPENDED"    = pure Suspended
    parseJSON _              = fail "Unknown server status"
