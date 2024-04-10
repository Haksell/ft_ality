module Action (parseActions) where

import Control.Monad (when)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Utils (panic)

parseActions :: [String] -> IO (Map.Map String String)
parseActions actionsSection = do
  putStrLn "action section"
  actions <- mapM parseAction actionsSection
  let mappedActions = Map.fromList actions
  -- TODO: print the first duplicate, like in Python version
  when (length actions /= length mappedActions) $ panic "Duplicate action keys have been found in the grammar file."
  mapM_ print actions
  return mappedActions

parseAction :: String -> IO (String, String)
parseAction actionLine = do
  let parts = splitOn "/" actionLine
  case parts of
    [key, action] -> return (key, action)
    _ -> panic "Action line should be in the following format: key/action"
