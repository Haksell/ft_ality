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

data Args = Args
  { argFilename :: String,
    argDebug :: Bool
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> argument str (metavar "grammar.gmr" <> help "Description of the keymap and combos")
    <*> switch (long "debug" <> short 'd' <> help "Debug mode")

validateArgs :: Args -> Either String Args
validateArgs args =
  if ".gmr" `isSuffixOf` argFilename args
    then Right args
    else Left "Error: the file path must end with '.gmr'."

main :: IO ()
main = do
  args <- execParser $ info (parseArgs <**> helper) fullDesc
  case validateArgs args of
    Right validArgs -> do
      let filename = argFilename validArgs
      let debug = argDebug validArgs
      putStrLn $ if debug then "debug" else "quiet"
      fileContents <- readFile filename
      putColorful Green fileContents
    Left errorMsg -> putStrLn errorMsg
