{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pharbricator.Remarkup
   ( -- * Basic types
     Remarkup
     -- * Formatting
   , bold
   , italic
   , monospace
   , deleted
   , underlined
   , highlighted
     -- * Lifting other entities into documents
   , ToMarkup(..)
   , mkup
   ) where

import qualified Data.Text as T
import Data.Monoid
import Data.String (IsString(..))
import Data.Aeson

import Servant (ToText(..))
import Phabricator.Types

newtype Remarkup = Markup T.Text
                 deriving (Show, ToJSON)

instance Monoid Remarkup where
  mempty = Markup mempty
  Markup a `mappend` Markup b = Markup (a <> b)

instance IsString Remarkup where
  fromString = Markup . T.pack

surround :: T.Text -> Remarkup -> Remarkup
surround s (Markup t) = Markup $ s<>t<>s

bold :: Remarkup -> Remarkup
bold = surround "**"

italic :: Remarkup -> Remarkup
italic = surround "//"

monospace :: Remarkup -> Remarkup
monospace m@(Markup t)
  | T.any (== '`') t = surround "##" m
  | otherwise        = surround "`" m

deleted :: Remarkup -> Remarkup
deleted = surround "~~"

underlined :: Remarkup -> Remarkup
underlined = surround "__"

highlighted :: Remarkup -> Remarkup
highlighted = surround "!!"

class ToMarkup a where
  toMarkup :: a -> Remarkup

mkup :: ToMarkup a => a -> Remarkup
mkup = toMarkup

instance ToMarkup Revision where
  toMarkup = Markup . toText
