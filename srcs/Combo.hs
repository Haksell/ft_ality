module Combo (Combo (..), parseCombos) where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Combo = Combo
  { comboLen :: Int,
    comboStr :: String,
    comboState :: Int,
    comboDFA :: [Map.Map String Int]
  }
  deriving (Show)

type ComboCache = Set.Set (String, String)

parseCombo :: String -> ComboCache -> (Combo, ComboCache)
parseCombo comboLine comboCache = do
  ( Combo
      { Combo.comboLen = 0,
        Combo.comboStr = "",
        Combo.comboState = 0,
        Combo.comboDFA = []
      },
    comboCache
    )

parseCombos' :: [String] -> [Combo] -> ComboCache -> [Combo]
parseCombos' [] prevCombos _ = prevCombos
parseCombos' (comboLine : comboLines) prevCombos comboCache = do
  let (combo, newComboCache) = parseCombo comboLine comboCache
  parseCombos' comboLines (combo : prevCombos) newComboCache

parseCombos :: [String] -> [Combo]
parseCombos comboLines = parseCombos' comboLines [] Set.empty
