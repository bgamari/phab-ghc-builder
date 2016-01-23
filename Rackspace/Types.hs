{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rackspace.Types where

import qualified Data.Text as T
import Data.Aeson
import Data.String
import Servant

newtype AuthToken = AuthToken T.Text
                  deriving (Eq, Ord, Show, IsString, ToText, ToJSON, FromJSON)

-- | A Rackspace UUID tagged with the type of resource it represents
newtype UUID a = UUID T.Text
               deriving (Eq, Ord, Show, IsString, ToJSON, FromJSON, ToText, FromText)

-- * Resource types
data Server
