module Complexity where

import Control.Monad (when)
import qualified Control.Monad
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Executor (createTape, execute, stringifyTape)
import Machine (Machine (..), isValidInput)
import System.Random (randomRIO)
import Text.Printf (printf)

type Datapoint = (Integer, Float)

type Dataset = [Datapoint]

type ComplexityFunc = Integer -> Float

linearRegressionNoBias :: [(Float, Float)] -> Float
linearRegressionNoBias [] = 0
linearRegressionNoBias points = sumXY / sumXX
  where
    sumXY = sum [x * y | (x, y) <- points]
    sumXX = sum [x * x | (x, _) <- points]

findConstantAndLoss :: Dataset -> ComplexityFunc -> (Float, Float)
findConstantAndLoss points f = (constant, mseLoss)
  where
    transformedPoints = map (first f) points
    constant = linearRegressionNoBias transformedPoints
    mseLoss = sum [(y - constant * x) ** 2 | (x, y) <- transformedPoints] / fromIntegral (length points)

findComplexity :: Dataset -> [(String, ComplexityFunc)] -> (String, Float)
findComplexity dataset complexityFuncs = (bestName, bestConstant)
  where
    results = [(name_, findConstantAndLoss dataset f) | (name_, f) <- complexityFuncs]
    (bestName, (bestConstant, _)) = minimumBy (comparing (\(_, (_, loss)) -> loss)) results

transformFunc :: (Float -> Float) -> (Integer -> Float)
transformFunc f x = f (fromIntegral x)

allComplexities :: [(String, ComplexityFunc)]
allComplexities =
  [ ("O(n)", fromIntegral),
    ("O(n log n)", transformFunc (\n -> n * logBase 2 n)),
    ("O(n^2)", transformFunc (** 2)),
    ("O(n^2 log n)", transformFunc (\n -> n ** 2 * logBase 2 n)),
    ("O(n^3)", transformFunc (** 3)),
    ("O(n^3 log n)", transformFunc (\n -> n ** 3 * logBase 2 n)),
    ("O(n^4)", transformFunc (** 4)),
    ("O(n^5)", transformFunc (** 5)),
    ("O(n^6)", transformFunc (** 6))
  ]

chooseRandom :: Int -> String -> IO String
chooseRandom k str = Control.Monad.replicateM k (randomChoice str)

randomChoice :: String -> IO Char
randomChoice str
  | null str = error "Error, '[]' is detected, pattern is incorrect."
  | otherwise = do
      randomIndex <- randomRIO (0, length str - 1)
      return $ str !! randomIndex

generateInputByPattern :: String -> Int -> IO String
generateInputByPattern pattern size = go pattern ""
  where
    go [] acc = return acc
    go ('[' : xs) acc = do
      let (choiceStr, rest) = break (== ']') xs
      if null rest
        then error "Malformed pattern: missing closing ']'"
        else do
          let remaining = tail rest
          chosen <- chooseRandom size choiceStr
          go remaining (acc ++ chosen)
    go (x : xs) acc
      | x == ']' = go xs acc
      | otherwise = go xs (acc ++ [x])

executeMachine :: Integer -> Machine -> String -> Bool -> IO Integer
executeMachine maxSteps machine input debug = do
  (finalTape, finalPos, finalMessage, stepsCount) <- execute False maxSteps maxSteps machine (createTape input) (initial machine) 0 Set.empty
  when debug $ putStrLn (stringifyTape finalTape finalPos (blank machine) ++ " " ++ finalMessage ++ " " ++ show stepsCount)
  return stepsCount

calculateComplexity :: String -> Machine -> Bool -> IO ()
calculateComplexity pattern machine debug = do
  results <- loop 1 []
  if null results
    then putStrLn "No valid results were obtained."
    else do
      let (complexity, constant) = findComplexity (map (bimap toInteger fromIntegral) results) allComplexities
      printf "The estimated complexity is %.4g * %s.\n" constant complexity
  where
    maxSteps = 50000
    loop size results
      | size > 60 = return results
      | otherwise = do
          inp <- generateInputByPattern pattern size
          case isValidInput (alphabet machine) (blank machine) inp of
            Just err -> do
              putStrLn err
              return []
            Nothing -> do
              steps <- executeMachine maxSteps machine inp debug
              let newResults = (length inp, steps) : results
              if steps >= maxSteps
                then return results
                else loop (size + 1) newResults
