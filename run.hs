{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.String (fromString)
import System.Exit

import qualified Data.Text as T
import Shelly.Lifted
import Options.Applicative as O

import Build
import Build.Parsers

main :: IO ()
main = do
    (opts, action) <- execParser $ info (helper <*> ((,) <$> options <*> task)) mempty
    code <- flip runBuildM opts $ verbosely action
    exitWith code

task :: Parser (BuildM ExitCode)
task = subparser $ buildDiff <> buildCommit
  where
    commit = option (Commit . T.pack <$> str) (short 'c' <> long "commit" <> metavar "COMMIT")

    buildDiff = O.command "diff" $ info
        (testDiff <$> option (Rev <$> auto) (short 'r' <> long "revision" <> metavar "REV")
                  <*> option (Diff <$> auto) (short 'd' <> long "diff" <> metavar "DIFF")
                  <*> commit
        )
        mempty

    buildCommit = O.command "commit" $ info
        (testCommit <$> commit)
        mempty
