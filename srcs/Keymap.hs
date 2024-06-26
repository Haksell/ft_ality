module Keymap (Keymap, parseKeymap, printKeymap, validKeys, validButtons) where

import Colors (Color (..), putColorful)
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Utils (isAsciiLetter, panic)

type Keymap = Map.Map String String

parseKeymap :: [String] -> IO Keymap
parseKeymap keymapSection = do
  mappings <- mapM parseMapping keymapSection
  keymapOrDuplicate <- buildKeymap mappings
  case keymapOrDuplicate of
    Left keymap -> return keymap
    Right duplicate -> panic $ "Duplicate key found: " ++ duplicate

buildKeymap :: [(String, String)] -> IO (Either Keymap String)
buildKeymap = f Map.empty
 where
  f keymap [] = return $ Left keymap
  f keymap ((k, a) : xs) =
    if Map.member k keymap
      then return $ Right k
      else do
        checkedKey <- checkKey $ map toUpper k
        f (Map.insert checkedKey a keymap) xs

validKeys :: [String]
validKeys = ["UP", "RIGHT", "DOWN", "LEFT"]

validButtons :: [String]
validButtons =
  [ "CONTROLLERBUTTONA"
  , "CONTROLLERBUTTONB"
  , "CONTROLLERBUTTONX"
  , "CONTROLLERBUTTONY"
  , "CONTROLLERBUTTONDPADRIGHT"
  , "CONTROLLERBUTTONDPADUP"
  , "CONTROLLERBUTTONDPADLEFT"
  , "CONTROLLERBUTTONDPADDOWN"
  , "CONTROLLERBUTTONRIGHTSHOULDER"
  , "CONTROLLERBUTTONLEFTSHOULDER"
  , "CONTROLLERBUTTONRIGHTSTICK"
  , "CONTROLLERBUTTONLEFTSTICK"
  ]

checkKey :: String -> IO String
checkKey k =
  if length k == 1 && isAsciiLetter (head k) || k `elem` validKeys || k `elem` validButtons
    then
      return k
    else
      panic $ "Invalid key: " ++ k

parseMapping :: String -> IO (String, String)
parseMapping keymapLine = do
  let parts = splitOn "/" keymapLine
  case parts of
    [key, action] -> return (key, action)
    _ -> panic "Action line should be in the following format: key/action"

printKeymap :: Keymap -> Bool -> IO ()
printKeymap keymap gamepad = do
  putColorful Green "=== KEYMAP ==="
  let filterFunc (k, _) = (if gamepad then elem else notElem) k validButtons
  mapM_
    (\(k, v) -> putStrLn $ k ++ " -> " ++ v)
    (sortBy (compare `on` length . fst) (filter filterFunc (Map.toList keymap)))