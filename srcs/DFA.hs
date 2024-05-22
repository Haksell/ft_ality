module DFA (DFA, advanceDFA, buildDFA) where

import Combo (Combo (..))
import qualified Data.Array as Array
import Data.List (isSuffixOf, nub)
import qualified Data.Map as Map
import Utils (arrayFull)

type DFAActions = Map.Map String Int
type DFAStates = Map.Map [String] Int
type DFATransitions = Array.Array Int (Array.Array Int Int)
type DFAFinishingStates = Array.Array Int (Array.Array Int [Combo])

data DFA = DFA
  { dfaState :: Int
  , dfaActions :: DFAActions
  , dfaFinishingStates :: DFAFinishingStates
  , dfaTransitions :: DFATransitions
  }

reverseIndex :: (Ord a) => [a] -> Map.Map a Int
reverseIndex lst = Map.fromList $ zip (nub lst) [0 ..]

buildActions :: [Combo] -> DFAActions
buildActions combos = reverseIndex $ concatMap comboActions combos

buildStates :: [Combo] -> DFAStates
buildStates combos = reverseIndex $ concatMap ((\x -> map (`take` x) [0 .. length x]) . comboActions) combos

buildFinishingStates :: [Combo] -> DFAStates -> DFAActions -> DFAFinishingStates
buildFinishingStates combos states actions = do
  let empty = arrayFull (length states) (length actions) []
  foldl updateFinishingStates empty combos
 where
  updateFinishingStates :: DFAFinishingStates -> Combo -> DFAFinishingStates
  updateFinishingStates finishingStates combo = do
    let start = init $ comboActions combo
    foldl (addFinishingState combo start) finishingStates (Map.toList states)

  addFinishingState :: Combo -> [String] -> DFAFinishingStates -> ([String], Int) -> DFAFinishingStates
  addFinishingState combo start finishingStates (state, stateIdx) = do
    if start `isSuffixOf` state
      then do
        let lastAction = last $ comboActions combo
        let actionIdx = actions Map.! lastAction
        let row = finishingStates Array.! stateIdx Array.// [(actionIdx, (finishingStates Array.! stateIdx Array.! actionIdx) ++ [combo])]
        finishingStates Array.// [(stateIdx, row)]
      else
        finishingStates

buildTransitions :: DFAStates -> DFAActions -> DFATransitions
buildTransitions states actions = do
  let empty = arrayFull (length states) (length actions) 0
  foldl updateTransitions empty (Map.keys states)
 where
  updateTransitions :: DFATransitions -> [String] -> DFATransitions
  updateTransitions transitions state = do
    foldl (updateStateAction state) transitions (Map.keys actions)

  updateStateAction :: [String] -> DFATransitions -> String -> DFATransitions
  updateStateAction state transitions action = do
    let suffix = state ++ [action]
    let transitions' = findAndUpdateTransition transitions state action suffix
    transitions'

  findAndUpdateTransition :: DFATransitions -> [String] -> String -> [String] -> DFATransitions
  findAndUpdateTransition transitions state action suffix = do
    let stateIdx = states Map.! state
    let actionIdx = actions Map.! action
    if Map.member suffix states
      then do
        let suffixIdx = states Map.! suffix
        let updatedInnerArray = transitions Array.! stateIdx Array.// [(actionIdx, suffixIdx)]
        transitions Array.// [(stateIdx, updatedInnerArray)]
      else
        if not (null suffix)
          then
            findAndUpdateTransition transitions state action (tail suffix)
          else
            transitions

buildDFA :: [Combo] -> DFA
buildDFA combos = do
  let actions = buildActions combos
  let states = buildStates combos
  DFA
    { dfaState = 0
    , dfaActions = actions
    , dfaFinishingStates = buildFinishingStates combos states actions
    , dfaTransitions = buildTransitions states actions
    }

advanceDFA :: DFA -> String -> (DFA, [Combo])
advanceDFA dfa action =
  case Map.lookup action (dfaActions dfa) of
    Nothing -> (dfa{dfaState = 0}, [])
    Just actionIdx -> do
      let completedCombos = dfaFinishingStates dfa Array.! dfaState dfa Array.! actionIdx
      let newState = dfaTransitions dfa Array.! dfaState dfa Array.! actionIdx
      (dfa{dfaState = newState}, completedCombos)