module Combo (Combo (..), parseCombos) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils (panic)

data Combo = Combo
  { comboLen :: Int
  , comboStr :: String
  , comboState :: Int
  , comboDFA :: [Map.Map String Int]
  }
  deriving (Show)

type ComboCache = Set.Set (String, String)

-- TODO: maybe handle action not in keymap
newCombo :: [String] -> String -> String -> Combo
newCombo actions name fighter =
  Combo
    { Combo.comboLen = length actions
    , Combo.comboStr = name ++ "(" ++ fighter ++ ") !!"
    , Combo.comboState = 0
    , Combo.comboDFA = []
    }

parseCombo :: String -> ComboCache -> IO (Combo, ComboCache)
parseCombo comboLine comboCache = do
  let parts = splitOn "/" comboLine
  case parts of
    [actions, name, fighter] ->
      if Set.member (name, fighter) comboCache
        then do
          panic $ "Duplicate combo: " ++ name ++ "(" ++ fighter ++ ")"
        else
          return
            ( newCombo (splitOn "," actions) name fighter
            , Set.insert (name, fighter) comboCache
            )
    _ -> do
      panic "Combo line should be in the following format: moves/name/fighter"

parseCombos' :: [String] -> [Combo] -> ComboCache -> IO [Combo]
parseCombos' [] prevCombos _ = return prevCombos
parseCombos' (comboLine : comboLines) prevCombos comboCache = do
  (combo, newComboCache) <- parseCombo comboLine comboCache
  parseCombos' comboLines (combo : prevCombos) newComboCache

parseCombos :: [String] -> IO [Combo]
parseCombos comboLines = parseCombos' comboLines [] Set.empty
