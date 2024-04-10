module Action (Keymap, parseKeymap) where

import Data.Char (toUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Utils (isAsciiLetter, panic)

type Keymap = Map.Map String String

parseKeymap :: [String] -> IO Keymap
parseKeymap keymapSection = do
  mappings <- mapM parseAction keymapSection
  keymapOrDuplicate <- buildKeymap mappings
  case keymapOrDuplicate of
    Left keymap -> return keymap
    Right duplicate -> panic $ "Duplicate key found: " ++ duplicate

buildKeymap :: [(String, String)] -> IO (Either Keymap String)
buildKeymap = go Map.empty
 where
  go keymap [] = return $ Left keymap
  go keymap ((k, a) : xs) =
    if Map.member k keymap
      then return $ Right k
      else do
        checkedKey <- checkKey $ map toUpper k
        go (Map.insert checkedKey a keymap) xs

validKeys :: [String]
validKeys = ["UP", "RIGHT", "DOWN", "LEFT"]

checkKey :: String -> IO String
checkKey k =
  if length k == 1 && isAsciiLetter (head k) || k `elem` validKeys
    then
      return k
    else
      panic $ "Invalid key: " ++ k

parseAction :: String -> IO (String, String)
parseAction actionLine = do
  let parts = splitOn "/" actionLine
  case parts of
    [key, action] -> return (key, action)
    _ -> panic "Action line should be in the following format: key/action"
