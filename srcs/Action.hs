module Action (Keymap, parseKeymap) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Utils (panic)

type Keymap = Map.Map String String

parseKeymap :: [String] -> IO Keymap
parseKeymap keymapSection = do
  mappings <- mapM parseAction keymapSection
  case buildKeymap mappings of
    Left keymap -> return keymap
    Right dup -> panic $ "Duplicate key found: " ++ dup

buildKeymap :: [(String, String)] -> Either Keymap String
buildKeymap = go Map.empty
 where
  go keymap [] = Left keymap
  go keymap ((k, a) : xs) =
    if Map.member k keymap
      then Right k
      else go (Map.insert k a keymap) xs

parseAction :: String -> IO (String, String)
parseAction actionLine = do
  let parts = splitOn "/" actionLine
  case parts of
    [key, action] -> return (key, action)
    _ -> panic "Action line should be in the following format: key/action"
