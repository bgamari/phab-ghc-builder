{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Harbormaster HTTP message
-- See https://phabricator.haskell.org/conduit/method/harbormaster.sendmessage/

module Phabricator.Harbormaster
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
import Phabricator.Types

sendMessage :: BaseUrl -> ApiToken -> Phid -> Message -> EitherT ServantError IO ()
sendMessage baseUrl apiToken buildTargetPhid msg = do
    r <- client api baseUrl (Just apiToken) (Just buildTargetPhid)
                                            (Just $ msgType msg)
                                            (Just $ AsJson $ msgUnits msg)
    _ <- liftResponse r
    return ()
  where
    api :: Proxy SendMessage
    api = Proxy

-- | The @harbormaster.sendmessage@ endpoint.
type SendMessage = "api" :> "harbormaster.sendmessage"
                   :> QueryParam "api.token" ApiToken
                   :> QueryParam "buildTargetPHID" Phid
                   :> QueryParam "type" MessageType
                   :> QueryParam "unit" (AsJson [UnitResult])
                   :> Get '[JSON] (Response JsonNull)

data Message = Message { msgType  :: MessageType
                       , msgUnits :: [UnitResult]
                       }

data MessageType = TargetPassed
                 | TargetFailed
                 | Work
                 deriving (Show)

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
