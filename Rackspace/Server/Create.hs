{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Rackspace.Server.Create
    ( Req(..)
    , Resp(..)
    , Image, Flavor
    , createServer
    ) where

import qualified Data.Text as T
import Data.Aeson
import Network.URI (parseURI)
import Control.Monad.Trans.Either

import Servant
import Servant.Client
import Rackspace.Types as Types

data Image
data Flavor

data Req = Req { csName   :: T.Text
               , csImage  :: UUID Image
               , csFlavor :: UUID Flavor
               }

data ServerLink = ServerLink { linkRel :: T.Text, linkUri :: URI }

data Resp = Resp { csId    :: UUID Types.Server
                 , csLinks :: [ServerLink]
                 }

type Api = "servers" :> Header "X-Auth-Token" AuthToken
        :> ReqBody '[JSON] Req :> Post '[JSON] Resp


instance ToJSON Req where
  toJSON c = object
    [
      "server" .= object
        [ "imageRef"  .= csImage c
        , "flavorRef" .= csFlavor c
        , "name"      .= csName c
        ]
    ]

instance FromJSON Resp where
  parseJSON =
    withObject "response" $ \resp -> do
      server <- resp .: "server"
      Resp <$> server .: "id"
           <*> server .: "links"

instance FromJSON ServerLink where
  parseJSON = withObject "link" $ \obj -> do
    rel <- obj .: "rel"
    Just href <- parseURI <$> obj .: "href"
    pure $ ServerLink rel href

createServer :: BaseUrl -> AuthToken -> Req -> EitherT ServantError IO Resp
createServer baseUrl authToken req =
    client api baseUrl (Just authToken) req
  where
    api :: Proxy Api
    api = Proxy
