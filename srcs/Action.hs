module Action (Action (..), parseActions) where

import Utils (trim)

data Action = Action
  { key :: String
  , action :: String
  }
  deriving (Show)

parseActions :: [String] -> IO [Action]
parseActions actionsSection = do
  putStrLn "action section"
  let actions = map parseAction actionsSection
  putStrLn (unlines $ map show actions)
  return actions

parseAction :: String -> Action
parseAction line =
  let (k, a) = break (== '/') line
   in Action (trim k) (trim $ drop 1 a)
