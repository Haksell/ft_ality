{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Data.Char (isAlpha, isAscii, isSpace, toUpper)
import System.IO
  ( BufferMode (NoBuffering),
    hReady,
    hSetBuffering,
    hSetEcho,
    stdin,
  )

type ParsedContent = (String, String) -- WIP

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseContent :: String -> ParsedContent
parseContent _ = ("ok", "ok")

parseFile :: FilePath -> IO ParsedContent
parseFile filePath = do
  content <- trim <$> readFile filePath
  let (actionsSection, combosSection) = break (== "") $ lines content
  print actionsSection
  print combosSection
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

execute :: String -> String -> IO ()
execute _ _ = do
  action <- getAction
  putColorful Green action
  execute "" ""

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  execute keymap combos
