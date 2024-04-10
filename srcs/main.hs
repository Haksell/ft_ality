{-# LANGUAGE OverloadedStrings #-}

import Action (Keymap, parseKeymap)
import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), parseCombos)
import Data.Char (toUpper)
import qualified Data.Map as Map
import System.IO (
  BufferMode (NoBuffering),
  hReady,
  hSetBuffering,
  hSetEcho,
  stdin,
 )
import Utils (isAsciiLetter, panic, trim)

type ParsedContent = (Keymap, [Combo])

splitSections :: [String] -> [[String]]
splitSections = foldr f []
 where
  f "" [] = []
  f line [] = [[line]]
  f "" ([] : acc) = [] : acc
  f "" acc = [] : acc
  f line (x : xs) = (line : x) : xs

parseFile :: FilePath -> IO ParsedContent
parseFile filename = do
  content <- trim <$> readFile filename
  let sections = splitSections $ map trim $ lines content
  case sections of
    [keymapSection, combosSection] -> do
      keymap <- parseKeymap keymapSection
      combos <- parseCombos combosSection
      return (keymap, combos)
    _ -> panic $ "Error: wrong number of sections: " ++ show (length sections)

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

execute :: Map.Map String String -> [Combo] -> IO ()
execute keymap combos = do
  action <- getAction
  putColorful Green action
  execute keymap combos

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  print keymap
  print combos
  execute keymap combos
