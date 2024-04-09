{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Data.Char (isAlpha, isAscii, toUpper)
import System.IO
  ( BufferMode (NoBuffering),
    hReady,
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

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isAlpha c

getKeyPress :: IO [Char]
getKeyPress = reverse <$> getKeyPress' ""
  where
    getKeyPress' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKeyPress' else return) (char : chars)

getAction :: IO String
getAction = do
  chars <- getKeyPress
  case chars of
    "\ESC[A" -> return "[UP]"
    "\ESC[B" -> return "[DOWN]"
    "\ESC[C" -> return "[RIGHT]"
    "\ESC[D" -> return "[LEFT]"
    [c] | isAsciiLetter c -> return [toUpper c]
    _ -> getAction

-- TODO: accept keymap and combos
execute :: IO ()
execute = do
  action <- getAction
  putColorful Green action
  execute

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  putStrLn $ if argDebug args then "debug" else "quiet"
  putStrLn keymap
  putStrLn combos
  execute
