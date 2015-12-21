{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Phabricator.Types where

import Servant
import Data.Monoid
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

-- | Use the JSON representation of a value for its 'ToText' instance
newtype AsJson a = AsJson a

instance ToJSON a => ToText (AsJson a) where
  toText (AsJson v) = TL.toStrict $ TB.toLazyText $ encodeToTextBuilder $ toJSON v

newtype Phid = Phid T.Text
             deriving (Eq, Ord, Show, FromJSON, ToJSON, FromText, ToText)

-- | A Phabricator API token
newtype ApiToken = ApiToken T.Text
                 deriving (FromJSON, ToJSON, ToText)

-- | A Harbormaster repository name, e.g. @"rGHC"@
newtype Repository = Repo T.Text
                   deriving (Eq, Ord, Show, ToText, FromText)

-- | A Harbormaster revision number, e.g. @D1234@
newtype Revision = Rev Integer
                 deriving (Eq, Ord, Show)

-- | A Harbormaster diff number, e.g. @R1234@
newtype Diff = Diff Integer
             deriving (Eq, Ord, Show)

-- | A Harbormaster build ID, e.g. @B1234@
newtype BuildId = BuildId Integer
                deriving (Eq, Ord, Show)

data Response a = Response { respResult :: a
                           , respErrorCode :: Maybe T.Text
                           , respErrorInfo :: Maybe T.Text
                           }
                deriving (Show)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "response" $ \obj ->
    Response <$> obj .: "result"
             <*> obj .: "error_code"
             <*> obj .: "error_info"

fromTextPrefix :: Char -> T.Text -> Maybe Integer
fromTextPrefix prefix s
  | c:rest <- T.unpack s
  , c == prefix
  , [(n,"")] <- reads rest
  = Just n
  | otherwise
  = Nothing

toTextPrefix :: Char -> Integer -> T.Text
toTextPrefix c n = T.singleton c <> T.pack (show n)

instance ToText Revision where toText (Rev n) = toTextPrefix 'R' n
instance FromText Revision where fromText t = Rev <$> fromTextPrefix 'R' t

instance ToText Diff where toText (Diff n) = toTextPrefix 'D' n
instance FromText Diff where fromText t = Diff <$> fromTextPrefix 'D' t

instance ToText BuildId where toText (BuildId n) = toTextPrefix 'B' n
instance FromText BuildId where fromText t = BuildId <$> fromTextPrefix 'B' t

data JsonNull = JsonNull

instance FromJSON JsonNull where
  parseJSON Null = pure JsonNull
  parseJSON v    = fail $ "Expected null, saw "++show v

instance ToJSON JsonNull where
  toJSON JsonNull = Null
