{-# LANGUAGE OverloadedStrings #-}

import Action (Keymap, parseKeymap)
import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), parseCombos)
import Keyboard (getAction)
import System.IO (
  BufferMode (NoBuffering),
  hSetBuffering,
  hSetEcho,
  stdin,
 )
import Utils (panic, trim)

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

execute :: Keymap -> [Combo] -> IO ()
execute keymap combos = do
  action <- getAction keymap
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
