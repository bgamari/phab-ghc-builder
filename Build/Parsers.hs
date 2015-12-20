{-# LANGUAGE OverloadedStrings #-}

module Build.Parsers where

import Data.String (fromString)
import Options.Applicative
import Build

options :: Parser Options
options = Options
    <$> option auto
               (short 't' <> long "threads" <> value Nothing)
    <*> option (Just . fromString <$> str)
               (short 'r' <> long "reference-repo" <> value Nothing)
    <*> option (BuildId <$> auto)
               (short 'B' <> long "build-id")
    <*> option (fromString <$> str)
               (short 'a' <> long "archive" <> metavar "DIR" <> value ".")
