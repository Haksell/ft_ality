module Parsing (parseFile) where

import Combo (Combo, parseCombos)
import Keymap (Keymap, parseKeymap)
import Utils (panic, trim)

splitSections :: [String] -> [[String]]
splitSections = foldr f []
  where
    f "" [] = []
    f line [] = [[line]]
    f "" ([] : acc) = [] : acc
    f "" acc = [] : acc
    f line (x : xs) = (line : x) : xs

parseFile :: FilePath -> IO (Keymap, [Combo])
parseFile filename = do
  content <- trim <$> readFile filename
  let sections = splitSections $ map trim $ lines content
  case sections of
    [keymapSection, combosSection] -> do
      keymap <- parseKeymap keymapSection
      combos <- parseCombos combosSection
      return (keymap, combos)
    _ -> panic $ "Error: wrong number of sections: " ++ show (length sections)
