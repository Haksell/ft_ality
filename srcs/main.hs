import Colors (Color (..), putColorful)
import Control.Monad (when)
import Data.Char (isDigit, isLower, isUpper)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    hSetEcho,
    stdin,
  )

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  loop

loop :: IO ()
loop = do
  char <- getChar
  case char of
    '\ESC' -> do
      next <- getChar
      when (next == '[') $ do
        direction <- getChar
        case direction of
          'A' -> putColorful Green "[Up]"
          'B' -> putColorful Green "[Down]"
          'C' -> putColorful Green "[Right]"
          'D' -> putColorful Green "[Left]"
          _ -> return ()
    _ -> displayColored char
  loop

displayColored :: Char -> IO ()
displayColored char
  | isDigit char = putColorful Yellow [char]
  | isUpper char = putColorful Red [char]
  | isLower char = putColorful Blue [char]
  | otherwise = return ()