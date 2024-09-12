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

getActionKeyboard :: Keymap -> IO (Maybe String)
getActionKeyboard keymap = do
  key <- getKey
  case key of
    Nothing -> return Nothing
    Just k -> case Map.lookup k keymap of
      Just action -> return (Just action)
      Nothing -> getActionKeyboard keymap
 where
  getKey :: IO (Maybe String)
  getKey = do
    chars <- getKeyPress
    case chars of
      "\ESC" -> return Nothing
      "\ESC[A" -> return (Just "UP")
      "\ESC[B" -> return (Just "DOWN")
      "\ESC[C" -> return (Just "RIGHT")
      "\ESC[D" -> return (Just "LEFT")
      [c] | isAsciiLetter c -> return (Just [toUpper c])
      _ -> getKey

  getKeyPress :: IO [Char]
  getKeyPress = reverse <$> getKeyPress' ""
   where
    getKeyPress' chars = do
      char <- getChar
      more <- System.IO.hReady System.IO.stdin
      (if more then getKeyPress' else return) (char : chars)