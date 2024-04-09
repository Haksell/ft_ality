{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Complexity (calculateComplexity)
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List
  ( intercalate,
    isSuffixOf,
  )
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Executor (createTape, execute, stringifyTape)
import Machine (Machine (..), buildMachine, isValidInput, printMachine)
import Options.Applicative
  ( Parser,
    ReadM,
    argument,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    readerError,
    short,
    str,
    switch,
    (<**>),
  )
import Options.Applicative.Builder (strOption)

defaultMaxSteps :: Integer
defaultMaxSteps = 100000

ftTuring :: Machine -> String -> Bool -> Integer -> IO ()
ftTuring machine input debug maxSteps = do
  when debug $ putStrLn $ printMachine machine
  (finalTape, finalPos, finalMessage, _stepsCount) <-
    execute
      debug
      maxSteps
      maxSteps
      machine
      (createTape input)
      (initial machine)
      0
      Set.empty
  putStrLn $ stringifyTape finalTape finalPos (blank machine) ++ " " ++ finalMessage

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
  let jsonFilePath = argJsonFilePath args
  let input = argInput args
  let complexity = argComplex args
  let debug = not $ argQuiet args
  let maxSteps = fromMaybe defaultMaxSteps $ argMaxSteps args
  if ".json" `isSuffixOf` jsonFilePath
    then do
      jsonFileContents <- BL.readFile jsonFilePath
      case buildMachine jsonFileContents of
        Left parsingError -> putStrLn parsingError
        Right machine ->
          case complexity of
            Just pattern -> calculateComplexity pattern machine debug
            Nothing -> case input of
              Nothing -> putStrLn "Error: input is required."
              Just actualInput ->
                case isValidInput (alphabet machine) (blank machine) actualInput of
                  Just err -> putStrLn $ "Error: " ++ err
                  Nothing -> ftTuring machine actualInput debug maxSteps
    else putStrLn "Error: the file path must end with '.json'."