module Action (parseActions) where

import qualified Data.Map as Map

import Utils (trim)

parseActions :: [String] -> IO (Map.Map String String)
parseActions actionsSection = do
  putStrLn "action section"
  let actions = map parseAction actionsSection
  putStrLn (unlines $ map show actions)
  return $ Map.fromList actions

parseAction :: String -> (String, String)
parseAction line =
  let (k, a) = break (== '/') line
   in (trim k, trim $ drop 1 a)