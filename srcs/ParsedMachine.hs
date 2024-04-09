{-# LANGUAGE DeriveGeneric #-}

module ParsedMachine where

import Data.Aeson
  ( defaultOptions,
    eitherDecode,
    withObject,
    (.:),
  )
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (FromJSON (parseJSON), Options, Parser, genericParseJSON, rejectUnknownFields)
import qualified Data.ByteString.Lazy as B
import Data.Either (lefts)
import Data.List (nub)
import qualified Data.Map as Map
import GHC.Generics (Generic)

data ParsedTransition = ParsedTransition
  { read :: Char,
    to_state :: String,
    write :: Char,
    action :: String
  }
  deriving (Show, Generic)

type ParsedTransitions = Map.Map String [ParsedTransition]

data ParsedMachine = ParsedMachine
  { name :: String,
    alphabet :: [Char],
    blank :: Char,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: ParsedTransitions
  }
  deriving (Show, Generic)

instance FromJSON ParsedMachine where
  parseJSON = withObject "ParsedMachine" $ \v ->
    ParsedMachine
      <$> v .: fromString "name"
      <*> (mapM parseSingleChar =<< v .: fromString "alphabet")
      <*> v .: fromString "blank"
      <*> v .: fromString "states"
      <*> v .: fromString "initial"
      <*> v .: fromString "finals"
      <*> v .: fromString "transitions"
    where
      parseSingleChar :: String -> Parser Char
      parseSingleChar [c] = return c
      parseSingleChar _ = fail "Alphabet must contain single-character strings"

instance FromJSON ParsedTransition where
  parseJSON = genericParseJSON $ aesonOptions "ParsedTransition"

aesonOptions :: String -> Options
aesonOptions _ = defaultOptions {rejectUnknownFields = True}

parseMachine :: B.ByteString -> Either String ParsedMachine
parseMachine bs = case eitherDecode bs of
  Left parsingError -> Left parsingError
  Right machine -> validateMachine machine

validateMachine :: ParsedMachine -> Either String ParsedMachine
validateMachine machine
  | null (name machine) = Left "Machine name can't be empty"
  | length (alphabet machine) < 2 = Left "Alphabet len should be at least 2 characters"
  | any ((/= 1) . length . (: [])) (alphabet machine) = Left "All elements in the alphabet must be a single character"
  | length (nub (alphabet machine)) /= length (alphabet machine) = Left "Alphabet must contain unique elements"
  | blank machine `notElem` alphabet machine = Left "Blank should be in the alphabet"
  | length (states machine) < 2 = Left "At least 2 states are expected (initial, finals)"
  | any null (states machine) = Left "State name can't be empty"
  | length (nub (states machine)) /= length (states machine) = Left "States names should be unique"
  | initial machine `notElem` states machine = Left "Initial state is not in the list of states."
  | null (finals machine) = Left "At least 1 finals expected."
  | not (all (`elem` states machine) (finals machine)) = Left "Not all final states are in the list of states."
  | length (nub (finals machine)) /= length (finals machine) = Left "Duplicates in finals doesn't make sense."
  | initial machine `elem` finals machine = Left "Initial value is equal to finals, program will exit immediately."
  | otherwise = areTransitionsValid (transitions machine) machine >> Right machine

isValidTransition :: ParsedTransition -> ParsedMachine -> Either String ()
isValidTransition t machine
  | action t /= "LEFT" && action t /= "RIGHT" = Left $ "Invalid action: " ++ action t
  | to_state t `notElem` states machine = Left $ "Invalid to_state: " ++ to_state t ++ " is not in the list of states."
  | write t `notElem` alphabet machine = Left $ "Invalid write: " ++ [write t] ++ " should be in the alphabet."
  | ParsedMachine.read t `notElem` alphabet machine = Left $ "Invalid read: " ++ [ParsedMachine.read t] ++ " should be in the alphabet."
  | otherwise = Right ()

areTransitionsValid :: ParsedTransitions -> ParsedMachine -> Either String ()
areTransitionsValid machineTransitions machine =
  let errors = concatMap validateStateTransitions (Map.toList machineTransitions)
      finalStatesErrors = validateFinalStatesCoverage machineTransitions (finals machine)
   in case errors ++ finalStatesErrors of
        [] -> Right ()
        errs -> Left (unlines errs)
  where
    validateStateTransitions :: (String, [ParsedTransition]) -> [String]
    validateStateTransitions (state, ts) =
      if state `notElem` states machine
        then ["State '" ++ state ++ "' is not in the list of states."]
        else
          let readChars = map ParsedMachine.read ts
              duplicates = filter (\c -> length (filter (== c) readChars) > 1) readChars
           in if not (null duplicates)
                then ["Ambiguous transitions: multiple reads of '" ++ duplicates ++ "' in state '" ++ state ++ "'"]
                else lefts $ map (`isValidTransition` machine) ts

    validateFinalStatesCoverage :: ParsedTransitions -> [String] -> [String]
    validateFinalStatesCoverage mt finalStates =
      let toStates = concatMap (map to_state . snd) (Map.toList mt)
          missingFinals = filter (`notElem` toStates) finalStates
       in (["Final states not covered in any transition's to_state: " ++ unwords missingFinals | not (null missingFinals)])