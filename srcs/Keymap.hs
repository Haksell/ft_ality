module Keymap (Keymap, parseKeymap, printKeymap, validKeys, validButtons) where

import Colors (Color (..), putColorful)
import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Utils (isAsciiLetter, panic)

type Keymap = Map.Map String String

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

parseKeymap :: [String] -> IO Keymap
parseKeymap keymapSection = do
  mappings <- mapM parseMapping keymapSection
  keymapOrDuplicate <- buildKeymap mappings
  case keymapOrDuplicate of
    Left keymap -> return keymap
    Right duplicate -> panic $ "Duplicate key found: " ++ duplicate
 where
  parseMapping :: String -> IO (String, String)
  parseMapping keymapLine = do
    let parts = splitOn "/" keymapLine
    case parts of
      [key, action] -> return (key, action)
      _ -> panic "Action line should be in the following format: key/action"

  buildKeymap :: [(String, String)] -> IO (Either Keymap String)
  buildKeymap = foldM updateKeyMap (Left Map.empty)

  updateKeyMap :: Either Keymap String -> (String, String) -> IO (Either Keymap String)
  updateKeyMap (Right duplicateKey) _ = return $ Right duplicateKey
  updateKeyMap (Left keymap) (k, a) =
    if Map.member k keymap
      then return $ Right k
      else do
        checkedKey <- checkKey $ map toUpper k
        return $ Left (Map.insert checkedKey a keymap)

  checkKey :: String -> IO String
  checkKey k =
    if length k == 1 && isAsciiLetter (head k) || k `elem` validKeys || k `elem` validButtons
      then return k
      else panic $ "Invalid key: " ++ k

printKeymap :: Keymap -> Bool -> IO ()
printKeymap keymap gamepad = do
  putColorful Green "=== KEYMAP ==="
  let filterFunc (k, _) = (if gamepad then elem else notElem) k validButtons
  mapM_
    (\(k, v) -> putStrLn $ k ++ " -> " ++ v)
    (sortBy (compare `on` length . fst) (filter filterFunc (Map.toList keymap)))