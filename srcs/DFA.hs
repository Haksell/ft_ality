module DFA (Combo (..), advanceCombo, printCombos, printSuccessfulCombo, printUnsuccessfulCombo, parseDFA) where

import Colors (Color (..), colored, putColorful)
import Data.Function (on)
import Data.List (find, intercalate, nub, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Utils (panic, uncurry3)

type DFA = [Map.Map String Int]

-- COMBO SECTION START (TODO: Combo.hs)

data Combo = Combo
  { comboLen :: Int
  , comboActions :: [String]
  , comboName :: String
  , comboFighter :: String
  , comboState :: Int
  , comboDFA :: DFA
  }

advanceCombo :: Combo -> String -> (Bool, Combo)
advanceCombo combo action = do
  let mapping = comboDFA combo !! comboState combo
  let newState = fromMaybe 0 (Map.lookup action mapping)
  let isComplete = newState == comboLen combo
  (isComplete, combo{comboState = if isComplete then 0 else newState})

printInfoCombo :: Combo -> IO ()
printInfoCombo combo = putStrLn $ comboFighter combo ++ ": " ++ comboName combo ++ ": " ++ intercalate ", " (comboActions combo)

printCombos :: [Combo] -> IO ()
printCombos combos = do
  putColorful Green "=== COMBOS ==="
  mapM_ printInfoCombo combos

printSuccessfulCombo :: Combo -> IO ()
printSuccessfulCombo combo = putStrLn $ comboFighter combo ++ " uses " ++ comboName combo ++ " !!"

printUnsuccessfulCombo :: Combo -> IO ()
printUnsuccessfulCombo combo =
  putStrLn $
    comboFighter combo
      ++ ": "
      ++ comboName combo
      ++ ": "
      ++ show (comboState combo)
      ++ "/"
      ++ show (comboLen combo)

-- COMBO SECTION END

type ComboCache = Set.Set (String, String)

findPositionDFA :: Int -> [String] -> String -> Int
findPositionDFA i actions action = do
  let sublist j = take (j - 1) (drop (i - j + 1) actions) ++ [action]
  let bestFind = find (\j -> sublist j == take j actions) [i + 1, i .. 1]
  fromMaybe 0 bestFind

buildState :: Int -> [String] -> [String] -> Map.Map String Int
buildState i actions uniqueActions = Map.fromList (map (\a -> (a, findPositionDFA i actions a)) uniqueActions)

buildDFA :: [String] -> [Map.Map String Int]
buildDFA actions = do
  let uniqueActions = nub actions
  map (uncurry3 buildState) $ zip3 [0 .. length actions - 1] (repeat actions) (repeat uniqueActions)

-- TODO: maybe handle action not in keymap
newCombo :: [String] -> String -> String -> Combo
newCombo actions name fighter =
  Combo
    { comboLen = length actions
    , comboActions = actions
    , comboName = colored Blue name
    , comboFighter = colored Red fighter
    , comboState = 0
    , comboDFA = buildDFA actions
    }

parseCombo :: String -> ComboCache -> IO (Combo, ComboCache)
parseCombo comboLine comboCache = do
  let parts = splitOn "/" comboLine
  case parts of
    [actions, name, fighter] ->
      if Set.member (name, fighter) comboCache
        then panic $ "Duplicate combo: " ++ name ++ "(" ++ fighter ++ ")"
        else
          return
            ( newCombo (splitOn "," actions) name fighter
            , Set.insert (name, fighter) comboCache
            )
    _ -> panic "Combo line should be in the following format: moves/name/fighter"

parseDFA' :: [String] -> [Combo] -> ComboCache -> IO [Combo]
parseDFA' [] prevCombos _ = return prevCombos
parseDFA' (comboLine : comboLines) prevCombos comboCache = do
  (combo, newComboCache) <- parseCombo comboLine comboCache
  parseDFA' comboLines (combo : prevCombos) newComboCache

parseDFA :: [String] -> Set.Set String -> IO [Combo]
parseDFA comboLines _ = do
  combos <- parseDFA' comboLines [] Set.empty
  return $ sortBy (compare `on` (\c -> (comboFighter c, comboName c))) combos
