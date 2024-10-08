module Args (Args (..), parseAndValidateArgs) where

import Data.List (isSuffixOf)
import Options.Applicative (
  Parser,
  argument,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  str,
  switch,
  (<**>),
 )
import Utils (panic)

data Args = Args
  { argFilename :: FilePath
  , argDebug :: Bool
  , argGamepad :: Bool
  }

parseAndValidateArgs :: IO Args
parseAndValidateArgs = do
  args <- execParser $ info (parseArgs <**> helper) fullDesc
  validateArgs args
 where
  grammarExtension = ".gmr"

  parseArgs :: Parser Args
  parseArgs =
    Args
      <$> argument str (metavar ("grammar" ++ grammarExtension) <> help "Description of the keymap and combos")
      <*> switch (long "debug" <> help "Debug mode")
      <*> switch (long "gamepad" <> help "Enter combos using a gamepad")

  validateArgs :: Args -> IO Args
  validateArgs args =
    if grammarExtension `isSuffixOf` argFilename args
      then return args
      else panic $ "Error: the file path must end with '" ++ grammarExtension ++ "'."
