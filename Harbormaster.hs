{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Harbormaster HTTP message
-- See https://phabricator.haskell.org/conduit/method/harbormaster.sendmessage/

module Harbormaster
    ( -- * Harbormaster API
      sendMessage
    , Phid(..)
    , ApiToken(..)
      -- * Messages
    , Message(..)
    , MessageType(..)
      -- * Unit test results
    , Outcome(..)
    , UnitResult(..)
    ) where

import Control.Monad.Trans.Either

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T

import Servant
import Servant.Client

newtype Phid = Phid T.Text
             deriving (FromJSON, ToJSON, ToText)

newtype ApiToken = ApiToken T.Text
                 deriving (FromJSON, ToJSON, ToText)

sendMessage :: BaseUrl -> ApiToken -> Phid -> Message -> EitherT ServantError IO ()
sendMessage baseUrl apiToken buildTargetPhid msg =
    client api baseUrl (Just apiToken) (Just buildTargetPhid)
                                       (Just $ msgType msg)
                                       (Just $ AsJson $ msgUnits msg)
  where
    api :: Proxy SendMessage
    api = Proxy

newtype AsJson a = AsJson a

instance ToJSON a => ToText (AsJson a) where
  toText = undefined

-- | The @harbormaster.sendmessage@ endpoint.
type SendMessage = "api" :> "sendmessage"
                   :> QueryParam "api.token" ApiToken
                   :> QueryParam "buildTargetPHID" Phid
                   :> QueryParam "type" MessageType
                   :> QueryParam "units" (AsJson [UnitResult])
                   :> Get '[JSON] ()

data Message = Message { msgType  :: MessageType
                       , msgUnits :: [UnitResult]
                       }

data MessageType = TargetPassed
                 | TargetFailed
                 | Work

data Outcome = Pass | Fail | Skip | Broken | Unsound

data UnitResult = UnitResult { unitName      :: T.Text
                             , unitResult    :: Outcome
                             , unitNamespace :: Maybe T.Text
                             , unitEngine    :: Maybe T.Text
                             , unitDuration  :: Maybe Float
                             , unitPath      :: Maybe FilePath
                             }

instance ToText MessageType where
  toText TargetPassed = "passed"
  toText TargetFailed = "failed"
  toText Work         = "work"

instance ToJSON UnitResult where
  toJSON r = object $
      [ "name"   .= unitName r
      , "result" .= unitResult r
      ] ++
      catMaybes
      [ ("namespace" .=) <$> unitNamespace r
      , ("engine"    .=) <$> unitEngine r
      , ("duration"  .=) <$> unitDuration r
      , ("path"      .=) <$> unitPath r
      ]
    where

instance ToJSON Outcome where
  toJSON r = case r of
               Pass    -> "pass"
               Fail    -> "fail"
               Skip    -> "skip"
               Broken  -> "broken"
               Unsound -> "unsound"
