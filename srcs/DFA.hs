module DFA (Combo (..), printCombos, printSuccessfulCombo, parseDFA) where

import Colors (Color (..), colored, putColorful)
import qualified Control.Applicative as Array
import Control.Monad (when)
import Data.Array (array)
import qualified Data.Array as Array
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

-- TODO: only return Combo
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
        panic ("Unknown action for combo " ++ representation) -- TODO: say which action
      return
        Combo
          { comboName = colored Blue name
          , comboFighter = colored Red fighter
          , comboActions = actions
          , comboLen = length actions
          }
    _ -> panic "Combo line should be in the following format: actions/name/fighter"

type ComboCache = Set.Set (String, String)

parseCombos :: [String] -> Set.Set String -> ComboCache -> IO [Combo]
parseCombos [] _ _ = return []
parseCombos (comboLine : comboLines) possibleActions cache = do
  combo <- parseCombo comboLine possibleActions
  let comboIdentity = (comboName combo, comboFighter combo)
  when (Set.member comboIdentity cache) $
    panic ("Duplicate combo: " ++ comboName combo ++ " (" ++ comboFighter combo ++ ")")
  combos <- parseCombos comboLines possibleActions (Set.insert comboIdentity cache)
  return (combo : combos)

data DFA = DFA
  { dfaMaxLen :: Int
  , dfaActions :: Map.Map String Int
  , dfaFinishingStates :: Array.Array Int (Array.Array Int [Combo])
  , dfaTransitions :: Array.Array Int (Array.Array Int Int)
  , dfaState :: Int
  }

buildActions :: [Combo] -> Map.Map String Int
buildActions combos = Map.empty

buildDFA :: [Combo] -> DFA
buildDFA combos = do
  DFA
    { dfaMaxLen = maximum (map (length . comboActions) combos)
    , dfaActions = buildActions combos
    , dfaFinishingStates = array (0, -1) []
    , dfaTransitions = array (0, -1) []
    , dfaState = 0
    }

parseDFA :: [String] -> Set.Set String -> IO DFA
parseDFA comboLines possibleActions = do
  combos <- parseCombos comboLines possibleActions Set.empty
  return $ buildDFA combos