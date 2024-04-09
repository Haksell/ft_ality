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

main :: IO ()
main = do
  args <- execParser $ info (parseArgs <**> helper) fullDesc
  let filename = argFilename args
  let debug = argDebug args
  putStrLn $ if debug then "debug" else "quiet"
  if ".gmr" `isSuffixOf` filename
    then do
      fileContents <- readFile filename
      putColorful Green fileContents
    else putStrLn "Error: the file path must end with '.gmr'."