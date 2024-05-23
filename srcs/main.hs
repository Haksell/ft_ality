{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..))
import Control.Monad (unless, when)
import DFA (DFA, advanceDFA)
import Data.Char (toUpper)
import Data.List (find, intercalate, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Gamepad (getActionGamepad, initGamepad)
import Keyboard (getActionKeyboard, initKeyboard)
import Keymap (Keymap, printKeymap)
import Parsing (parseFile)
import qualified SDL
import Utils (enqueue, prefixes)

printInfo :: Keymap -> [Combo] -> Bool -> IO ()
printInfo keymap combos gamepad = do
  printKeymap keymap gamepad
  putColorful Green "=== COMBOS ==="
  mapM_ printCombo combos
  putColorful Green (replicate 40 '=')
 where
  printCombo :: Combo -> IO ()
  printCombo combo = putStrLn $ comboFighter combo ++ ": " ++ comboName combo ++ ": " ++ intercalate ", " (comboActions combo)

handleAction :: Bool -> String -> [Combo] -> DFA -> [String] -> Int -> IO ([String], DFA)
handleAction debug action combos dfa queue maxSize = do
  putStrLn $ intercalate ", " (reverse newQueue)
  mapM_
    ( \c ->
        if c `elem` finishedCombos
          then printSuccessfulCombo c
          else when debug $ printUnsuccessfulCombo c
    )
    combos
  putStrLn ""
  return (newQueue, newDFA)
 where
  newQueue = enqueue maxSize action queue
  (newDFA, finishedCombos) = advanceDFA dfa action

  printSuccessfulCombo :: Combo -> IO ()
  printSuccessfulCombo combo = putStrLn $ comboFighter combo ++ " uses " ++ comboName combo ++ " !!"

  printUnsuccessfulCombo :: Combo -> IO ()
  printUnsuccessfulCombo combo = do
    let revQueue = reverse newQueue
    let comboState =
          maybe
            0
            length
            (find (`isSuffixOf` revQueue) (prefixes $ comboActions combo))
    putStrLn $
      comboFighter combo
        ++ ": "
        ++ comboName combo
        ++ ": "
        ++ show comboState
        ++ "/"
        ++ show (comboLen combo)

-- TODO: refactor 3 execute functions

executeTerminal :: Bool -> Maybe SDL.Renderer -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()
executeTerminal debug renderer keymap combos dfa queue maxSize = do
  action <- getActionKeyboard keymap
  case action of
    Nothing -> return ()
    Just a -> do
      (newQueue, newDFA) <- handleAction debug a combos dfa queue maxSize
      executeTerminal debug renderer keymap combos newDFA newQueue maxSize

executeGamepad :: Bool -> Maybe SDL.Renderer -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()
executeGamepad debug renderer keymap combos dfa queue maxSize = do
  action <- getActionGamepad keymap
  case action of
    Nothing -> return ()
    Just a -> do
      (newQueue, newDFA) <- handleAction debug a combos dfa queue maxSize
      executeGamepad debug renderer keymap combos newDFA newQueue maxSize

getActionGUI :: Keymap -> IO String
getActionGUI keymap = do
  events <- SDL.pollEvents
  let keyPresses =
        [ e
        | SDL.KeyboardEvent e <- map SDL.eventPayload events
        , SDL.keyboardEventKeyMotion e == SDL.Pressed
        ]
  unless (null keyPresses) (print keyPresses)
  case mapMaybe getActionFromKey keyPresses of
    [] -> getActionGUI keymap
    (action : _) -> return action
 where
  getActionFromKey :: SDL.KeyboardEventData -> Maybe String
  getActionFromKey keyPress =
    Map.lookup
      (map toUpper $ show $ SDL.keysymKeycode $ SDL.keyboardEventKeysym keyPress)
      keymap

executeGUI :: Bool -> Maybe SDL.Renderer -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()
executeGUI debug renderer keymap combos dfa queue maxSize = do
  action <- getActionGUI keymap
  (newQueue, newDFA) <- handleAction debug action combos dfa queue maxSize
  executeGUI debug renderer keymap combos newDFA newQueue maxSize

main :: IO ()
main = do
  SDL.initializeAll
  args <- parseAndValidateArgs
  (keymap, combos, dfa) <- parseFile (argFilename args)
  printInfo keymap combos (argGamepad args)
  if argGamepad args then initGamepad else initKeyboard
  let maxSize = maximum $ map (length . comboActions) combos
  if argGUI args
    then do
      window <- SDL.createWindow "ft_ality" SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 800 800}
      renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
      let executeFunc = if argGamepad args then executeGamepad else executeGUI
      executeFunc (argDebug args) (Just renderer) keymap combos dfa [] maxSize
      SDL.destroyWindow window
    else do
      let executeFunc = if argGamepad args then executeGamepad else executeTerminal
      executeFunc (argDebug args) Nothing keymap combos dfa [] maxSize
