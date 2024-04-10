module Combo (Combo (..), parseCombos, advanceCombo) where

import Data.List (find, nub)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Utils (panic, uncurry3)

type DFA = [Map.Map String Int]

data Combo = Combo
  { comboLen :: Int
  , comboStr :: String
  , comboState :: Int
  , comboDFA :: DFA
  }
  deriving (Show)

type ComboCache = Set.Set (String, String)

findPositionDFA :: Int -> [String] -> String -> Int
findPositionDFA i actions action = do
  let sublist j = take (j - 1) (drop (i - j + 1) actions) ++ [action]
  let bestFind = find (\j -> sublist j == take j actions) [i + 1, i .. 1]
  fromMaybe 0 bestFind

buildState :: Int -> [String] -> [String] -> Map.Map String Int
buildState i actions uniqueActions = Map.fromList (map (\a -> (a, findPositionDFA i actions a)) uniqueActions)

buildDFA :: [String] -> [Map.Map String Int]
buildDFA actions = do
  let uniqueActions = nub actions
  map (uncurry3 buildState) $ zip3 [0 .. length uniqueActions] (repeat actions) (repeat uniqueActions)

-- TODO: maybe handle action not in keymap
newCombo :: [String] -> String -> String -> Combo
newCombo actions name fighter =
  Combo
    { Combo.comboLen = length actions
    , Combo.comboStr = name ++ " (" ++ fighter ++ ") !!"
    , Combo.comboState = 0
    , Combo.comboDFA = buildDFA actions
    }

parseCombo :: String -> ComboCache -> IO (Combo, ComboCache)
parseCombo comboLine comboCache = do
  let parts = splitOn "/" comboLine
  case parts of
    [actions, name, fighter] ->
      if Set.member (name, fighter) comboCache
        then panic $ "Duplicate combo: " ++ name ++ "(" ++ fighter ++ ")"
        else
          return
            ( newCombo (splitOn "," actions) name fighter
            , Set.insert (name, fighter) comboCache
            )
    _ -> panic "Combo line should be in the following format: moves/name/fighter"

parseCombos' :: [String] -> [Combo] -> ComboCache -> IO [Combo]
parseCombos' [] prevCombos _ = return prevCombos
parseCombos' (comboLine : comboLines) prevCombos comboCache = do
  (combo, newComboCache) <- parseCombo comboLine comboCache
  parseCombos' comboLines (combo : prevCombos) newComboCache

parseCombos :: [String] -> IO [Combo]
parseCombos comboLines = parseCombos' comboLines [] Set.empty

advanceCombo :: Combo -> String -> (Bool, Combo)
advanceCombo combo action = do
  let mapping = comboDFA combo !! comboState combo
  let newState = fromMaybe 0 (Map.lookup action mapping)
  if newState == comboLen combo
    then (True, combo{comboState = 0})
    else (False, combo{comboState = newState})