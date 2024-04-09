{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)

main :: IO ()
main = do
  args <- parseAndValidateArgs
  putColorful Red $ if argDebug args then "debug" else "quiet"
  fileContents <- readFile $ argFilename args
  putColorful Green fileContents