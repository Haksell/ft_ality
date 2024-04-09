{-# LANGUAGE ImportQualifiedPost #-}

module Executor where

import Control.Monad (when)
import Data.List (sort)
import Data.Map.Strict as Map (Map, fromList, insert, lookup, member, toList)
import Data.Maybe (fromJust, listToMaybe)
import Data.Set qualified as Set
import Machine (Machine (..), Transition (..))

type CompleteState = (String, String, Maybe Integer)

type Tape = Map.Map Integer Char

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

createTape :: String -> Tape
createTape input = fromList $ enumerate input

stringifyTapeWithPos :: Tape -> Integer -> Char -> (String, Maybe Integer)
stringifyTapeWithPos tape pos blankChar =
  ( "[" ++ concatMap showCell nonBlankCells ++ "]",
    listToMaybe $ map fst nonBlankCells
  )
  where
    sortedCells = sort (Map.toList tape)
    dropStartBlanks = dropWhile (\(_, c) -> c == blankChar)
    filterBlanks = reverse . dropStartBlanks . reverse . dropStartBlanks
    showCell (k, v) = if k == pos then "<" ++ [v] ++ ">" else [v]
    nonBlankCells = filterBlanks sortedCells

stringifyTape :: Tape -> Integer -> Char -> String
stringifyTape tape pos blankChar = fst $ stringifyTapeWithPos tape pos blankChar

execute :: Bool -> Integer -> Integer -> Machine -> Tape -> String -> Integer -> Set.Set CompleteState -> IO (Tape, Integer, String, Integer)
execute debug maxSteps remainingSteps machine tape state pos visitedStates
  | state `elem` finals machine = return (tape, pos, "Final state: " ++ state, maxSteps - remainingSteps)
  | remainingSteps == 0 = return (tape, pos, "No final state found after " ++ show maxSteps ++ " steps", maxSteps - remainingSteps)
  | Set.member completeState visitedStates = return (tape, pos, "Infinite loop detected at state: " ++ state, maxSteps - remainingSteps)
  | otherwise = do
      case Map.lookup (state, cell) $ transitions machine of
        Nothing -> return (newTape, pos, "Unexpected transition: (" ++ state ++ ", " ++ [cell] ++ ")", maxSteps - remainingSteps)
        Just tv -> do
          when debug $
            putStrLn $
              stringifyTape newTape pos blankChar
                ++ " ("
                ++ state
                ++ ", "
                ++ [cell]
                ++ ") -> ("
                ++ toState tv
                ++ ", "
                ++ [writeChar tv]
                ++ ", "
                ++ (if isLeft tv then "LEFT" else "RIGHT")
                ++ ")"
          execute
            debug
            maxSteps
            (remainingSteps - 1)
            machine
            (insert pos (writeChar tv) newTape)
            (toState tv)
            (if isLeft tv then pos - 1 else pos + 1)
            (Set.insert completeState visitedStates)
  where
    blankChar = blank machine
    (strTape, start) = stringifyTapeWithPos tape pos blankChar
    completeState = (state, strTape, fmap (pos -) start)
    newTape = if pos `member` tape then tape else insert pos blankChar tape
    cell = fromJust $ Map.lookup pos newTape