import Colors (Color (..), putColorful)
import Control.Monad (when)
import Data.Char (isDigit, isLower, isUpper, toUpper)
import Parsing (ParsedContent (..), parseFile, printCombo, printMove)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    hSetEcho,
    stdin,
  )

displayColored :: Char -> IO ()
displayColored char
  | isDigit char = putColorful Yellow [char]
  | isUpper char = putColorful Red [char]
  | isLower char = putColorful Red [toUpper char]
  | otherwise = return ()

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

main :: IO ()
main = do
  parsedContent <- parseFile "grammars/valid/mk9_baraka.gmr"
  putStrLn "Moves:"
  mapM_ printMove (moves parsedContent)
  putStrLn "Combos:"
  mapM_ printCombo (combos parsedContent)
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  loop
