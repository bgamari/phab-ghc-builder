{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phabricator.Types where

import Servant
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

newtype AsJson a = AsJson a

instance ToJSON a => ToText (AsJson a) where
  toText (AsJson v) = TL.toStrict $ TB.toLazyText $ encodeToTextBuilder $ toJSON v

newtype Phid = Phid T.Text
             deriving (Eq, Ord, Show, FromJSON, ToJSON, FromText, ToText)

newtype ApiToken = ApiToken T.Text
                 deriving (FromJSON, ToJSON, ToText)
