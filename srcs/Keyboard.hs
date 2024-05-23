module Keyboard (getActionKeyboard, initKeyboard) where

import Data.Char (toUpper)
import qualified Data.Map as Map
import Keymap (Keymap)
import qualified System.IO
import Utils (isAsciiLetter)

initKeyboard :: IO ()
initKeyboard = do
  System.IO.hSetEcho System.IO.stdin False
  System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering

getKeyPress :: IO [Char]
getKeyPress = reverse <$> getKeyPress' ""
 where
  getKeyPress' chars = do
    char <- getChar
    more <- System.IO.hReady System.IO.stdin
    (if more then getKeyPress' else return) (char : chars)

getActionKeyboard :: Keymap -> IO String
getActionKeyboard keymap = do
  key <- getKey
  case Map.lookup key keymap of
    Just action -> return action
    Nothing -> getActionKeyboard keymap
 where
  getKey :: IO String
  getKey = do
    chars <- getKeyPress
    case chars of
      "\ESC[A" -> return "UP"
      "\ESC[B" -> return "DOWN"
      "\ESC[C" -> return "RIGHT"
      "\ESC[D" -> return "LEFT"
      [c] | isAsciiLetter c -> return [toUpper c]
      _ -> getKey