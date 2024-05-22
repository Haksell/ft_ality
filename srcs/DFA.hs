module DFA (Combo (..), printCombos, printSuccessfulCombo, parseDFA) where

import Colors (Color (..), colored, putColorful)
import Control.Monad (unless, when)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils (panic)

-- COMBO SECTION START (TODO: Combo.hs)

data Combo = Combo
  { comboName :: String
  , comboFighter :: String
  , comboActions :: [String]
  , comboLen :: Int
  }

printInfoCombo :: Combo -> IO ()
printInfoCombo combo = putStrLn $ comboFighter combo ++ ": " ++ comboName combo ++ ": " ++ intercalate ", " (comboActions combo)

printCombos :: [Combo] -> IO ()
printCombos combos = do
  putColorful Green "=== COMBOS ==="
  mapM_ printInfoCombo combos

printSuccessfulCombo :: Combo -> IO ()
printSuccessfulCombo combo = putStrLn $ comboFighter combo ++ " uses " ++ comboName combo ++ " !!"

-- COMBO SECTION END

data DFA = DFA
  { dfaMaxLen :: Int
  , dfaActions :: Map.Map String Int
  }

type ComboCache = Set.Set (String, String)

-- TODO: only return Combo
parseCombo :: String -> Set.Set String -> ComboCache -> IO (ComboCache, Combo)
parseCombo comboLine possibleActions cache = do
  let parts = splitOn "/" comboLine
  case parts of
    [actionsStr, name, fighter] -> do
      let actions = splitOn "," actionsStr
      let representation = name ++ " (" ++ fighter ++ ")"
      when (null actions) $ panic ("No actions for combo " ++ representation)
      when ("" `elem` actions) $ panic ("Empty action for combo " ++ representation)
      when (any (`Set.notMember` possibleActions) actions) $
        panic ("Unknown action for combo " ++ representation) -- TODO: say which one
      let comboIdentity = (name, fighter)
      when (Set.member comboIdentity cache) $ panic ("Duplicate combo: " ++ representation)
      return
        ( Set.insert comboIdentity cache
        , Combo
            { comboName = colored Blue name
            , comboFighter = colored Red fighter
            , comboActions = actions
            , comboLen = length actions
            }
        )
    _ -> panic "Combo line should be in the following format: actions/name/fighter"

parseCombos :: [String] -> Set.Set String -> ComboCache -> IO [Combo]
parseCombos [] _ _ = return []
parseCombos (comboLine : comboLines) possibleActions cache = do
  (newCache, combo) <- parseCombo comboLine possibleActions cache
  combos <- parseCombos comboLines possibleActions newCache
  return (combo : combos)

parseDFA :: [String] -> Set.Set String -> IO DFA
parseDFA comboLines possibleActions = do
  combos <- parseCombos comboLines possibleActions Set.empty
  return $
    DFA
      { dfaMaxLen = 0
      , dfaActions = Map.empty
      }