{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseValidate
    ( parseLog
    , TestFailure(..)
    , ValidateLog(..)
    ) where

import Text.Trifecta
import Text.Trifecta.Delta
import qualified Data.Text as T
import           Data.Text (Text)

data TestFailure = TestFailure { testCategory :: Text
                               , testName     :: Text
                               , reason       :: Text
                               , failedWays   :: [Text]
                               }
    deriving (Show)

data ValidateLog = ValidateLog { failures :: [TestFailure]
                               , statFailures :: [TestFailure]
                               }
    deriving (Show)

parseLog :: Text -> ValidateLog
parseLog log =
    vlog
  where
    -- Only look in the last few kilobytes of the log for the beginning of the
    -- testsuite results
    testsuiteLines :: [Text]
    testsuiteLines = map T.strip
                   $ dropWhile (not . T.isPrefixOf "OVERALL SUMMARY") $ T.lines $ T.takeEnd 40000 log

    failures     = map parseFailure $ stanza "Unexpected failures"
    statFailures = map parseFailure $ stanza "Unexpected stat failures"

    -- A group of lines starting with string and ending with a blank line
    stanza :: Text -> [Text]
    stanza start =
        takeWhile (not . T.null . T.strip)
        $ drop 1 $ dropWhile (not . T.isPrefixOf start)
        $ testsuiteLines

    parseFailure :: Text -> Maybe TestFailure
    parseFailure line =
        case parseString testFailure delta (T.unpack line) of
            Success r -> r
            Failure doc -> error $ show doc
      where
        delta = Columns 0 0

    vlog = ValidateLog {..}


testFailure :: Parser TestFailure
testFailure = do
    spaces
    testCategory <- T.pack <$> manyTill anyChar space
    spaces
    testName <- T.pack <$> manyTill anyChar space
    spaces
    char '['
    reason <- T.pack <$> manyTill anyChar (char ']')
    spaces
    char '('
    failedWays <- sepBy (T.pack <$> many (noneOf ",)")) (char ',')
    char ')'
    return TestFailure{..}
