{-# LANGUAGE InstanceSigs #-}

module Combo (Combo (..), parseCombos) where

import Colors (Color (..), colored, putColorful)
import Control.Monad (when)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Utils (panic)

data Combo = Combo
  { comboName :: String
  , comboFighter :: String
  , comboActions :: [String]
  , comboLen :: Int
  }

instance Eq Combo where
  (==) :: Combo -> Combo -> Bool
  (==) c1 c2 = comboName c1 == comboName c2 && comboFighter c1 == comboFighter c2

parseCombo :: String -> Set.Set String -> IO Combo
parseCombo comboLine possibleActions = do
  let parts = splitOn "/" comboLine
  case parts of
    [actionsStr, name, fighter] -> do
      let actions = splitOn "," actionsStr
      let representation = name ++ " (" ++ fighter ++ ")"
      when (null actions) $ panic ("No actions for combo " ++ representation)
      when ("" `elem` actions) $ panic ("Empty action for combo " ++ representation)
      when (any (`Set.notMember` possibleActions) actions) $
        panic ("Unknown action for combo " ++ representation)
      return
        Combo
          { comboName = colored Blue name
          , comboFighter = colored Red fighter
          , comboActions = actions
          , comboLen = length actions
          }
    _ -> panic "Combo line should be in the following format: actions/name/fighter"

type ComboCache = Set.Set (String, String)

parseCombos' :: [String] -> Set.Set String -> ComboCache -> IO [Combo]
parseCombos' [] _ _ = return []
parseCombos' (comboLine : comboLines) possibleActions cache = do
  combo <- parseCombo comboLine possibleActions
  let comboIdentity = (comboName combo, comboFighter combo)
  when (Set.member comboIdentity cache) $
    panic ("Duplicate combo: " ++ comboName combo ++ " (" ++ comboFighter combo ++ ")")
  combos <- parseCombos' comboLines possibleActions (Set.insert comboIdentity cache)
  return (combo : combos)

parseCombos :: [String] -> Set.Set String -> IO [Combo]
parseCombos comboLines possibleActions = parseCombos' comboLines possibleActions Set.empty