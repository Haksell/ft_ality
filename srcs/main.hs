{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)

type ParsedContent = (String, String)

parseContent :: String -> ParsedContent
parseContent _ = ("ok", "ok")

parseFile :: FilePath -> IO ParsedContent
parseFile filePath = do
  content <- readFile filePath
  return $ parseContent content

main :: IO ()
main = do
  args <- parseAndValidateArgs
  putColorful Red $ if argDebug args then "debug" else "quiet"
  (keymap, combos) <- parseFile (argFilename args)
  putColorful Green keymap
  putColorful Blue combos