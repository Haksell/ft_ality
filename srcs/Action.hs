module Action (parseActions) where

import Control.Monad (when)
import qualified Data.Map as Map
import Utils (panic, trim)

parseActions :: [String] -> IO (Map.Map String String)
parseActions actionsSection = do
  putStrLn "action section"
  actions <- mapM parseAction actionsSection
  let mappedActions = Map.fromList actions
  when (length actions /= Map.size mappedActions) $ do
    panic "Duplicate action keys have been found in the grammar file."
  mapM_ print actions
  return mappedActions

parseAction :: String -> IO (String, String)
parseAction line = do
  let (k, a) = break (== '/') line
      key = trim k
      value = trim $ drop 1 a
  if null key || null value
    then do
      panic "Empty action key or action value has been found in the grammar file."
    else return (key, value)