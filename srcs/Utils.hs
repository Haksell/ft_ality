{-# LANGUAGE RankNTypes #-}

module Utils where

import Data.Char (isSpace)
import System.Exit (ExitCode (ExitFailure), exitWith)

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

panic :: forall a. String -> IO a
panic s = do
  putStrLn s
  exitWith (ExitFailure 1)