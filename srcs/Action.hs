module Action (parseActions) where

import Control.Monad (when)
import qualified Data.Map as Map
import System.Exit (ExitCode (ExitFailure), exitWith)
import Utils (trim)

parseActions :: [String] -> IO (Map.Map String String)
parseActions actionsSection = do
  putStrLn "action section"
  actions <- mapM parseAction actionsSection
  let mappedActions = Map.fromList actions
  when (length actions /= Map.size mappedActions) $ do
    putStrLn "Duplicate action keys have been found in the grammar file."
    exitWith (ExitFailure 1)
  mapM_ print actions
  return mappedActions

parseAction :: String -> IO (String, String)
parseAction line = do
  let (k, a) = break (== '/') line
      key = trim k
      value = trim $ drop 1 a
  if null key || null value
    then do
      putStrLn "Empty action key or action value has been found in the grammar file."
      exitWith (ExitFailure 1)
    else return (key, value)