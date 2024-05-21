{-# LANGUAGE RankNTypes #-}

module Utils where

import Data.Char (isAlpha, isAscii, isSpace)
import System.Exit (ExitCode (ExitFailure), exitWith)

enqueue :: Int -> a -> [a] -> [a]
enqueue maxSize x xs = take maxSize (x : xs)

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isAlpha c

panic :: forall a. String -> IO a
panic s = do
  putStrLn s
  exitWith (ExitFailure 1)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c