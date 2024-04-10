{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), advanceCombo, parseCombos, printCombos, printSuccessfulCombo)
import Control.Monad (when)
import Data.List (intercalate)
import Keyboard (getAction)
import Keymap (Keymap, parseKeymap, printKeymap)
import System.IO (
  BufferMode (NoBuffering),
  hSetBuffering,
  hSetEcho,
  stdin,
 )
import Utils (enqueue, panic, trim)

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

advanceAndPrint :: Combo -> String -> IO Combo
advanceAndPrint combo action = do
  let (isFinished, newCombo) = advanceCombo combo action
  when isFinished $ printSuccessfulCombo newCombo
  return newCombo

execute :: Keymap -> [Combo] -> [String] -> Int -> IO ()
execute keymap combos actions maxSize = do
  action <- getAction keymap
  let newActions = enqueue maxSize action actions
  putStrLn $ intercalate ", " (reverse newActions)
  newCombos <- mapM (`advanceAndPrint` action) combos
  putStrLn ""
  execute keymap newCombos newActions maxSize

printInfo :: Keymap -> [Combo] -> IO ()
printInfo keymap combos = do
  printKeymap keymap
  printCombos combos
  putColorful Green (replicate 40 '=')

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  printInfo keymap combos
  execute keymap combos [] (maximum $ map comboLen combos)
