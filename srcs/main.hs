{-# LANGUAGE OverloadedStrings #-}

import Action (Keymap, parseKeymap)
import Args (Args (..), parseAndValidateArgs)
import Combo (Combo (..), parseCombos)
import Data.List (intercalate)
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

enqueue :: Int -> a -> [a] -> [a]
enqueue maxSize x xs = take maxSize (x : xs)

execute :: Keymap -> [Combo] -> [String] -> Int -> IO ()
execute keymap combos actions maxSize = do
  action <- getAction keymap
  let newActions = enqueue maxSize action actions
  putStrLn $ intercalate ", " newActions
  putStrLn ""
  execute keymap combos newActions maxSize

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  print keymap
  print combos
  execute keymap combos [] (maximum $ map comboLen combos)
