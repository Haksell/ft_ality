{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)

parseFile :: FilePath -> IO (String, String)
parseFile filePath = do
  content <- readFile filePath
  putColorful Green content
  return ("keymap", "combos")

main :: IO ()
main = do
  args <- parseAndValidateArgs
  putColorful Red $ if argDebug args then "debug" else "quiet"
  (keymap, combos) <- parseFile (argFilename args)
  putColorful Green keymap
  putColorful Blue combos