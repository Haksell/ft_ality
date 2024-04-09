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

validateArgs :: Args -> Either Args String
validateArgs args =
  if ".gmr" `isSuffixOf` argFilename args
    then Left args
    else Right "Error: the file path must end with '.gmr'."

main :: IO ()
main = do
  args <- execParser $ info (parseArgs <**> helper) fullDesc
  case validateArgs args of
    Left validArgs -> do
      putStrLn $ if argDebug validArgs then "debug" else "quiet"
      fileContents <- readFile $ argFilename validArgs
      putColorful Green fileContents
    Right errorMsg -> do
      putStrLn errorMsg
      exitWith (ExitFailure 1)