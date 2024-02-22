module Parsing where

import Data.List.Split (splitOn)

data Move = Move
  { key :: String,
    action :: String
  }
  deriving (Show)

data Combo = Combo
  { comboKeys :: String,
    comboName :: String,
    character :: String
  }
  deriving (Show)

data ParsedContent = ParsedContent
  { moves :: [Move],
    combos :: [Combo]
  }
  deriving (Show)

-- TODO: remove print functions

printMove :: Move -> IO ()
printMove (Move key action) = putStrLn $ key ++ " - " ++ action

printCombo :: Combo -> IO ()
printCombo (Combo comboKeys comboName character) =
  putStrLn $ comboKeys ++ " - " ++ comboName ++ " / " ++ character

parseContent :: String -> ParsedContent
parseContent content =
  let (movesSection, combosSection) = break (== "") $ lines content
   in ParsedContent
        { moves = map parseMove $ filter (not . null) movesSection,
          combos = map parseCombo $ filter (not . null) . tail $ combosSection
        }

parseMove :: String -> Move
parseMove line =
  let [key, action] = splitOn "/" line
   in Move key action

parseCombo :: String -> Combo
parseCombo line =
  let [comboKeys, comboName, character] = splitOn "/" line
   in Combo comboKeys comboName character

parseFile :: FilePath -> IO ParsedContent
parseFile filePath = do
  content <- readFile filePath
  return $ parseContent content
