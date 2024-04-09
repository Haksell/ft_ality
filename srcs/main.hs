{-# LANGUAGE OverloadedStrings #-}

import Colors (Color (..), putColorful)
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

data Args = Args
  { argFilename :: String,
    argDebug :: Bool
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> argument str (metavar "grammar.gmr" <> help "Description of the keymap and combos")
    <*> switch (long "debug" <> short 'd' <> help "Debug mode")

validateArgs :: Args -> IO Args
validateArgs args =
  if ".gmr" `isSuffixOf` argFilename args
    then return args
    else do
      putStrLn "Error: the file path must end with '.gmr'."
      exitWith (ExitFailure 1)

parseAndValidateArgs :: IO Args
parseAndValidateArgs = do
  args <- execParser $ info (parseArgs <**> helper) fullDesc
  validateArgs args

main :: IO ()
main = do
  validArgs <- parseAndValidateArgs
  putStrLn $ if argDebug validArgs then "debug" else "quiet"
  fileContents <- readFile $ argFilename validArgs
  putColorful Green fileContents