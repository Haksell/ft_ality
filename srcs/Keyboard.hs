module Keyboard (getAction) where

import Data.Char (toUpper)
import qualified Data.Map as Map
import Keymap (Keymap)
import System.IO (hReady, stdin)
import Utils (isAsciiLetter)

getKeyPress :: IO [Char]
getKeyPress = reverse <$> getKeyPress' ""
 where
  getKeyPress' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKeyPress' else return) (char : chars)

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

-- TODO: also use for Gamepad
getAction :: Keymap -> IO String
getAction keymap = do
  key <- getKey
  case Map.lookup key keymap of
    Just action -> return action
    Nothing -> getAction keymap
