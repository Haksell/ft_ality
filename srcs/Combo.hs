module Combo (Combo (..), parseCombos) where

import Utils (trim)

data Combo = Combo
  { currentStateIndex :: Int
  , states :: [String]
  , characterName :: String
  }
  deriving (Show)

parseCombos :: [String] -> IO [Combo]
parseCombos combosSection = do
  putStrLn "combos section"
  let combos = map parseCombo combosSection
  putStrLn (unlines $ map show combos)
  return combos

parseCombo :: String -> Combo
parseCombo line =
  let (statesAndMove, character) = break (== '/') $ reverse line
      (move, statesStr) = break (== '/') $ reverse statesAndMove
   in Combo
        { currentStateIndex = 0
        , states = parseStates $ reverse statesStr
        , characterName = drop 1 character
        }

parseStates :: String -> [String]
parseStates statesStr = map trim $ splitOn ',' statesStr

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
 where
  f c l@(x : xs)
    | c == delimiter = [] : l
    | otherwise = (c : x) : xs
  f _ [] = []
