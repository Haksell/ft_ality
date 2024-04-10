{-# LANGUAGE RankNTypes #-}

module Utils where

import Data.Char (isAlpha, isAscii, isSpace)
import System.Exit (ExitCode (ExitFailure), exitWith)

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isAlpha c

panic :: forall a. String -> IO a
panic s = do
  putStrLn s
  exitWith (ExitFailure 1)

safeIndex :: Int -> [a] -> a -> a
safeIndex _ [] def = def
safeIndex n (x : xs) def
  | n == 0 = x
  | n > 0 = safeIndex (n - 1) xs def
  | otherwise = def

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c