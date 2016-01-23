{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Rackspace.Server.Delete where

import Control.Monad.Trans.Either

import Servant
import Servant.Client
import Rackspace.Types as Types

type Api = "servers" :> Header "X-Auth-Token" AuthToken
        :> Capture "server" (UUID Types.Server) :> Delete '[JSON] ()

deleteServer :: BaseUrl -> AuthToken -> UUID Types.Server -> EitherT ServantError IO ()
deleteServer baseUrl authToken req =
    client api baseUrl (Just authToken) req
  where
    api :: Proxy Api
    api = Proxy
