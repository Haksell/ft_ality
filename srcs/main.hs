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

data CommandLineArgs = CommandLineArgs
  { argJsonFilePath :: String,
    argInput :: Maybe String,
    argQuiet :: Bool,
    argComplex :: Maybe String,
    argMaxSteps :: Maybe Integer
  }

positiveInteger :: ReadM Integer
positiveInteger = do
  value <- auto
  if value > 0
    then return value
    else readerError "max-steps must be a positive integer"

complexityArgDesctiption :: String
complexityArgDesctiption =
  intercalate
    "\n"
    [ "complexity pattern to match. \"[]\" characters is reserved for the patter matching.",
      "Do not use the reserved characters in the machine alphabet.",
      "Such pattern for unary_add algo: \"[1]+[1]=\" translates to:",
      "\"[Any number of 1s] + [Any number of 1s] =\", then to input like \"111+111=\", \"11111+11111=\", etc."
    ]

parseCommandLineArgs :: Parser CommandLineArgs
parseCommandLineArgs =
  CommandLineArgs
    <$> argument str (metavar "machine.json" <> help "json description of the machine")
    <*> optional (argument str (metavar "input" <> help "input of the machine"))
    <*> switch (long "quiet" <> short 'q' <> help "only show final tape")
    <*> optional (strOption (long "complexity" <> metavar "pattern" <> short 'c' <> help complexityArgDesctiption))
    <*> optional (option positiveInteger (long "max-steps" <> short 'm' <> metavar "n" <> help "maximum number of iterations (must be positive)"))

main :: IO ()
main = do
  args <- execParser $ info (parseCommandLineArgs <**> helper) fullDesc
  parsedContent <- parseFile "grammars/valid/mk9_baraka.gmr"
  putStrLn "Moves:"
  mapM_ printMove (moves parsedContent)
  putStrLn "Combos:"
  mapM_ printCombo (combos parsedContent)
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  loop
