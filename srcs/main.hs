{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Control.Monad (when)
import Data.Char (isDigit, isLower, isUpper, toUpper)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    hSetEcho,
    stdin,
  )

type ParsedContent = (String, String)

parseContent :: String -> ParsedContent
parseContent _ = ("ok", "ok")

parseFile :: FilePath -> IO ParsedContent
parseFile filePath = do
  content <- readFile filePath
  return $ parseContent content

displayColored :: Char -> IO ()
displayColored char
  | isDigit char = putColorful Yellow [char]
  | isUpper char = putColorful Red [char]
  | isLower char = putColorful Red [toUpper char]
  | otherwise = return ()

loop :: IO ()
loop = do
  char <- getChar
  case char of
    '\ESC' -> do
      next <- getChar
      when (next == '[') $ do
        direction <- getChar
        case direction of
          'A' -> putColorful Green "[Up]"
          'B' -> putColorful Green "[Down]"
          'C' -> putColorful Green "[Right]"
          'D' -> putColorful Green "[Left]"
          _ -> return ()
    _ -> displayColored char
  loop

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  putStrLn $ if argDebug args then "debug" else "quiet"
  putStrLn keymap
  putStrLn combos
  loop