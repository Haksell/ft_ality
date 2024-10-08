module Utils where

import Data.Array (listArray)
import qualified Data.Array as Array
import Data.Char (isAlpha, isAscii, isSpace)
import Data.List (nub)
import qualified Data.Map as Map
import System.Exit (ExitCode (ExitFailure), exitWith)

arrayFull :: Int -> Int -> a -> Array.Array Int (Array.Array Int a)
arrayFull rows cols value = listArray (0, rows - 1) (replicate rows row)
 where
  row = listArray (0, cols - 1) (replicate cols value)

enqueue :: Int -> a -> [a] -> [a]
enqueue maxSize x xs = take maxSize (x : xs)

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isAlpha c

panic :: String -> IO a
panic s = do
  putStrLn s
  exitWith (ExitFailure 1)

prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes xs = xs : prefixes (init xs)

reverseIndex :: (Ord a) => [a] -> Map.Map a Int
reverseIndex lst = Map.fromList $ zip (nub lst) [0 ..]

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace
