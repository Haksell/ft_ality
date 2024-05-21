module Combo (Combo (..), advanceCombo, printCombos, printSuccessfulCombo, printUnsuccessfulCombo) where

import Colors (Color (..), putColorful)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

type DFA = [Map.Map String Int]

data Combo = Combo
  { comboLen :: Int,
    comboActions :: [String],
    comboName :: String,
    comboFighter :: String,
    comboState :: Int,
    comboDFA :: DFA
  }

advanceCombo :: Combo -> String -> (Bool, Combo)
advanceCombo combo action = do
  let mapping = comboDFA combo !! comboState combo
  let newState = fromMaybe 0 (Map.lookup action mapping)
  let isComplete = newState == comboLen combo
  (isComplete, combo {comboState = if isComplete then 0 else newState})

printInfoCombo :: Combo -> IO ()
printInfoCombo combo = putStrLn $ comboFighter combo ++ ": " ++ comboName combo ++ ": " ++ intercalate ", " (comboActions combo)

printCombos :: [Combo] -> IO ()
printCombos combos = do
  putColorful Green "=== COMBOS ==="
  mapM_ printInfoCombo combos

printSuccessfulCombo :: Combo -> IO ()
printSuccessfulCombo combo = putStrLn $ comboFighter combo ++ " uses " ++ comboName combo ++ " !!"

printUnsuccessfulCombo :: Combo -> IO ()
printUnsuccessfulCombo combo =
  putStrLn $
    comboFighter combo
      ++ ": "
      ++ comboName combo
      ++ ": "
      ++ show (comboState combo)
      ++ "/"
      ++ show (comboLen combo)