module DFA (Combo (..), printCombos, printSuccessfulCombo, parseDFA) where

import Colors (Color (..), colored, putColorful)
import Control.Monad (when)
import Data.Array (listArray)
import qualified Data.Array as Array
import Data.List (intercalate, nub)
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

type DFAActions = Map.Map String Int
type DFAStates = Map.Map [String] Int
type DFATransitions = Array.Array Int (Array.Array Int Int)
type DFAFinishingStates = Array.Array Int (Array.Array Int [Combo])

data DFA = DFA
  { dfaMaxLen :: Int
  , dfaActions :: DFAActions
  , dfaFinishingStates :: DFAFinishingStates
  , dfaTransitions :: DFATransitions
  , dfaState :: Int
  }

reverseIndex :: (Ord a) => [a] -> Map.Map a Int
reverseIndex lst = Map.fromList $ zip (nub lst) [0 ..]

buildActions :: [Combo] -> DFAActions
buildActions combos = reverseIndex $ concatMap comboActions combos

buildStates :: [Combo] -> DFAStates
buildStates combos = reverseIndex $ concatMap ((\x -> map (`take` x) [0 .. length x]) . comboActions) combos

arrayFull :: Int -> Int -> a -> Array.Array Int (Array.Array Int a)
arrayFull rows cols value = listArray (0, rows - 1) (replicate rows row)
 where
  row = listArray (0, cols - 1) (replicate cols value)

buildFinishingStates :: [Combo] -> DFAStates -> DFAActions -> DFAFinishingStates
buildFinishingStates combos states actions = do
  let numStates = length states
  let numActions = length actions
  let empty = arrayFull numStates numActions []
  foldl f empty combos
 where
  f finishingStates combo = do
    let start = init $ comboActions combo
    -- TODO
    finishingStates

buildTransitions :: [Combo] -> DFAStates -> DFAActions -> DFATransitions
buildTransitions combos states actions = do
  let numStates = length states
  let numActions = length actions
  let transitions = arrayFull numStates numActions 0
  transitions

buildDFA :: [Combo] -> DFA
buildDFA combos = do
  let actions = buildActions combos
  let states = buildStates combos
  DFA
    { dfaMaxLen = maximum $ map (length . comboActions) combos
    , dfaActions = actions
    , dfaFinishingStates = buildFinishingStates combos states actions
    , dfaTransitions = buildTransitions combos states actions
    , dfaState = 0
    }

parseDFA :: [String] -> Set.Set String -> IO DFA
parseDFA comboLines possibleActions = do
  combos <- parseCombos comboLines possibleActions Set.empty
  return $ buildDFA combos