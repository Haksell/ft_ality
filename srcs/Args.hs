{-# LANGUAGE OverloadedStrings #-}

module Args (Args (..), parseAndValidateArgs) where

import Data.List (isSuffixOf)
import Options.Applicative
  ( Parser,
    argument,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    short,
    str,
    switch,
    (<**>),
  )
import System.Exit (ExitCode (ExitFailure), exitWith)

grammarExtension :: String
grammarExtension = ".gmr"

data Args = Args
  { argFilename :: String,
    argDebug :: Bool
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> argument str (metavar ("grammar" ++ grammarExtension) <> help "Description of the keymap and combos")
    <*> switch (long "debug" <> short 'd' <> help "Debug mode")

validateArgs :: Args -> IO Args
validateArgs args =
  if grammarExtension `isSuffixOf` argFilename args
    then return args
    else do
      putStrLn $ "Error: the file path must end with '" ++ grammarExtension ++ "'."
      exitWith (ExitFailure 1)

parseAndValidateArgs :: IO Args
parseAndValidateArgs = do
  args <- execParser $ info (parseArgs <**> helper) fullDesc
  validateArgs args
