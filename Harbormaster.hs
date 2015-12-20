{-# LANGUAGE OverloadedStrings #-}

-- | Harbormaster HTTP message
-- See https://phabricator.haskell.org/conduit/method/harbormaster.sendmessage/

module Harbormaster
    ( -- * Messages
      Message(..)
    , MessageType(..)
      -- * Unit test results
    , Outcome(..)
    , UnitResult(..)
    ) where

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T

data Message = Message { msgType  :: MessageType
                       , msgUnits :: [UnitResult]
                       }

data MessageType = TargetPassed
                 | Targetfailed
                 | Work

data Outcome = Pass | Fail | Skip | Broken | Unsound

data UnitResult = UnitResult { unitName      :: T.Text
                             , unitResult    :: Outcome
                             , unitNamespace :: Maybe T.Text
                             , unitEngine    :: Maybe T.Text
                             , unitDuration  :: Maybe Float
                             , unitPath      :: Maybe FilePath
                             }

instance ToJSON Message where
  toJSON m = object
      [ "type"   .= msgType m
      , "unit"   .= msgUnits m
      ]

instance ToJSON UnitResult where
  toJSON r = object
      [ "name"   .= unitName r
      , "result" .= res
      ] ++
      catMaybes
      [ ("namespace" .=) <$> unitNamespace r
      , ("engine"    .=) <$> unitEngine r
      , ("duration"  .=) <$> unitDuration r
      , ("path"      .=) <$> unitPath r
      ]
    where
      res :: T.Text
      res = case unitResult r of
              Pass    -> "pass"
              Fail    -> "fail"
              Skip    -> "skip"
              Broken  -> "broken"
              Unsound -> "unsound"