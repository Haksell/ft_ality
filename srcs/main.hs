{-# LANGUAGE OverloadedStrings #-}

import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..))
import Control.Monad (when)
import DFA (DFA, advanceDFA)
import Data.List (find, intercalate, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import GHC.Char (chr)
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

type GameLoop = Bool -> Maybe SDL.Renderer -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()

loopTerminal :: GameLoop
loopTerminal debug _ keymap combos dfa queue maxSize = do
  action <- getActionKeyboard keymap
  case action of
    Nothing -> return ()
    Just a -> do
      (newQueue, newDFA) <- handleAction debug a combos dfa queue maxSize
      loopTerminal debug Nothing keymap combos newDFA newQueue maxSize

loopGamepad :: GameLoop
loopGamepad debug _todo keymap combos dfa queue maxSize = do
  action <- getActionGamepad keymap
  case action of
    Nothing -> return ()
    Just a -> do
      (newQueue, newDFA) <- handleAction debug a combos dfa queue maxSize
      loopGamepad debug _todo keymap combos newDFA newQueue maxSize

getActionGUI :: Keymap -> IO (Maybe String)
getActionGUI keymap = do
  events <- SDL.pollEvents
  let keyPresses =
        [ e
        | SDL.KeyboardEvent e <- map SDL.eventPayload events
        , SDL.keyboardEventKeyMotion e == SDL.Pressed
        ]
  if any isQuitEvent events
    then return Nothing
    else case mapMaybe getActionFromKey keyPresses of
      [] -> getActionGUI keymap
      (action : _) -> return $ Just action
 where
  getKeyPress :: SDL.KeyboardEventData -> SDL.Keycode
  getKeyPress keyboardEvent = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent

  isQuitEvent :: SDL.Event -> Bool
  isQuitEvent event =
    case SDL.eventPayload event of
      SDL.QuitEvent -> True
      SDL.WindowClosedEvent _ -> True
      SDL.KeyboardEvent keyboardEvent ->
        case SDL.keyboardEventKeyMotion keyboardEvent of
          SDL.Pressed -> getKeyPress keyboardEvent == SDL.KeycodeEscape
          SDL.Released -> False
      _ -> False

  getActionFromKey :: SDL.KeyboardEventData -> Maybe String
  getActionFromKey keyPress = do
    let code = SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym keyPress
    case code of
      1073741903 -> Map.lookup "RIGHT" keymap
      1073741904 -> Map.lookup "LEFT" keymap
      1073741905 -> Map.lookup "DOWN" keymap
      1073741906 -> Map.lookup "UP" keymap
      c | 97 <= c && c <= 122 -> Map.lookup [chr (fromIntegral c - 32)] keymap
      _ -> Nothing

loopGUI :: GameLoop
loopGUI debug renderer keymap combos dfa queue maxSize = do
  action <- getActionGUI keymap
  case action of
    Nothing -> return ()
    Just a -> do
      let justRenderer = fromJust renderer
      SDL.rendererDrawColor justRenderer SDL.$= SDL.V4 255 0 0 255
      SDL.clear justRenderer
      SDL.present justRenderer
      (newQueue, newDFA) <- handleAction debug a combos dfa queue maxSize
      loopGUI debug renderer keymap combos newDFA newQueue maxSize

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
      let gameLoop = if argGamepad args then loopGamepad else loopGUI
      gameLoop (argDebug args) (Just renderer) keymap combos dfa [] maxSize
      SDL.destroyWindow window
    else do
      let gameLoop = if argGamepad args then loopGamepad else loopTerminal
      gameLoop (argDebug args) Nothing keymap combos dfa [] maxSize
