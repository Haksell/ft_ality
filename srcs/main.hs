{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), advanceCombo, printCombos, printSuccessfulCombo, printUnsuccessfulCombo)
import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as Map
import Keyboard (getKey)
import Keymap (Keymap, printKeymap)
import Parsing (parseFile)
import System.IO (
  BufferMode (NoBuffering),
  hSetBuffering,
  hSetEcho,
  stdin,
 )
import Utils (enqueue)

printInfo :: Keymap -> [Combo] -> IO ()
printInfo keymap combos = do
  printKeymap keymap
  printCombos combos
  putColorful Green (replicate 40 '=')

advanceQuiet :: Combo -> String -> IO Combo
advanceQuiet combo action = do
  let (isFinished, newCombo) = advanceCombo combo action
  when isFinished $ printSuccessfulCombo newCombo
  return newCombo

advanceDebug :: Combo -> String -> IO Combo
advanceDebug combo action = do
  let (isFinished, newCombo) = advanceCombo combo action
  (if isFinished then printSuccessfulCombo else printUnsuccessfulCombo) newCombo
  return newCombo

-- TODO: boolean to check if gamepad
getAction :: Keymap -> IO String
getAction keymap = do
  key <- getKey
  case Map.lookup key keymap of
    Just action -> return action
    Nothing -> getAction keymap

execute :: Bool -> Keymap -> [Combo] -> [String] -> Int -> IO ()
execute debug keymap combos actions maxSize = do
  action <- getAction keymap
  let newActions = enqueue maxSize action actions
  putStrLn $ intercalate ", " (reverse newActions)
  let advanceFunc = if debug then advanceDebug else advanceQuiet
  newCombos <- mapM (`advanceFunc` action) combos
  putStrLn ""
  execute debug keymap newCombos newActions maxSize

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  printInfo keymap combos
  execute (argDebug args) keymap combos [] (maximum $ map comboLen combos)
