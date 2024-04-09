{-# LANGUAGE DeriveGeneric #-}

module Machine where

import qualified Data.ByteString.Lazy as B
import Data.List (intercalate, nub, sort)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import ParsedMachine
  ( ParsedMachine
      ( alphabet,
        blank,
        finals,
        initial,
        name,
        states,
        transitions
      ),
    ParsedTransition (action, read, to_state, write),
    ParsedTransitions,
    parseMachine,
  )

type Transitions = Map.Map (String, Char) Transition

data Transition = Transition
  { toState :: String,
    writeChar :: Char,
    isLeft :: Bool
  }
  deriving (Show, Generic)

data Machine = Machine
  { name :: String,
    alphabet :: [Char],
    blank :: Char,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Transitions
  }
  deriving (Show, Generic)

isValidInput :: [Char] -> Char -> String -> Maybe String
isValidInput machineAlphabet machineBlank input
  | machineBlank `elem` input = Just "Input contains the blank symbol"
  | not $ null unknownChars = Just $ "Input contains symbols not in the alphabet: " ++ intercalate ", " (map (: []) (sort $ nub unknownChars))
  | otherwise = Nothing
  where
    unknownChars = filter (`notElem` machineAlphabet) input

buildMachine :: B.ByteString -> Either String Machine
buildMachine bs = case parseMachine bs of
  Left err -> Left err
  Right parsedMachine -> Right $ mapMachine parsedMachine

mapTransitions :: ParsedTransitions -> Transitions
mapTransitions = Map.foldrWithKey insertTransitions Map.empty
  where
    insertTransitions :: String -> [ParsedTransition] -> Transitions -> Transitions
    insertTransitions stateKey parsedTransitions acc = foldr (insertTransition stateKey) acc parsedTransitions

    insertTransition :: String -> ParsedTransition -> Transitions -> Transitions
    insertTransition stateKey parsedTransition acc =
      let key = (stateKey, ParsedMachine.read parsedTransition)
          value =
            Transition
              { toState = to_state parsedTransition,
                writeChar = write parsedTransition,
                isLeft = action parsedTransition == "LEFT"
              }
       in Map.insert key value acc

mapMachine :: ParsedMachine -> Machine
mapMachine parsedMachine =
  Machine
    { Machine.name = ParsedMachine.name parsedMachine,
      Machine.alphabet = ParsedMachine.alphabet parsedMachine,
      Machine.blank = ParsedMachine.blank parsedMachine,
      Machine.states = ParsedMachine.states parsedMachine,
      Machine.initial = ParsedMachine.initial parsedMachine,
      Machine.finals = ParsedMachine.finals parsedMachine,
      Machine.transitions = mapTransitions $ ParsedMachine.transitions parsedMachine
    }

lineSize :: Int
lineSize = 80

borderLine :: String
borderLine = replicate lineSize '*'

paddingLine :: String
paddingLine = "*" ++ replicate (lineSize - 2) ' ' ++ "*"

centerString :: Int -> Char -> String -> String
centerString width padChar s = leftPad ++ s ++ rightPad
  where
    (leftPadSize, parity) = (width - length s) `divMod` 2
    leftPad = replicate leftPadSize padChar
    rightPad = replicate (leftPadSize + parity) padChar

commaSeparated :: [Char] -> [Char] -> [String] -> String
commaSeparated start end xs = start ++ intercalate ", " xs ++ end

showLikeTuple :: [String] -> String
showLikeTuple = commaSeparated "(" ")"

showLikeArray :: [String] -> String
showLikeArray = commaSeparated "[" "]"

printMachine :: Machine -> String
printMachine machine =
  intercalate
    "\n"
    [ borderLine,
      paddingLine,
      "*" ++ centerString (lineSize - 2) ' ' (Machine.name machine) ++ "*",
      paddingLine,
      borderLine,
      "Alphabet: " ++ showLikeArray (map (: []) (Machine.alphabet machine)),
      "States: " ++ showLikeArray (Machine.states machine),
      "Initial: " ++ Machine.initial machine,
      "Finals: " ++ showLikeArray (Machine.finals machine),
      printTransitions (Machine.transitions machine),
      borderLine
    ]

printTransitions :: Transitions -> String
printTransitions tr = intercalate "\n" $ map printTransition (Map.toList tr)

printTransition :: ((String, Char), Transition) -> String
printTransition ((state, char), transition) =
  showLikeTuple [state, [char]]
    ++ " -> "
    ++ showLikeTuple [toState transition, [writeChar transition], if isLeft transition then "LEFT" else "RIGHT"]