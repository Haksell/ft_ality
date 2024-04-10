import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), advanceCombo, printCombos, printSuccessfulCombo, printUnsuccessfulCombo)
import Control.Monad (when)
import Data.List (intercalate)
import Keyboard (getAction)
import Keymap (Keymap, printKeymap)
import Parsing (parseFile)
import System.IO (
  BufferMode (NoBuffering),
  hSetBuffering,
  hSetEcho,
  stdin,
 )
import Utils (enqueue)
import GameControllerManager
import qualified SDL.Internal.Types

printInfo :: Keymap -> [Combo] -> IO ()
printInfo keymap combos = do
  printKeymap keymap
  printCombos combos
  putColorful Green (replicate 40 '=')

advanceQuiet :: Combo -> String -> IO Combo
advanceQuiet combo action = do
  let (isFinished, newCombo) = advanceCombo combo action
  when isFinished $ printSuccessfulCombo newCombo
  return newCombo

advanceDebug :: Combo -> String -> IO Combo
advanceDebug combo action = do
  let (isFinished, newCombo) = advanceCombo combo action
  (if isFinished then printSuccessfulCombo else printUnsuccessfulCombo) newCombo
  return newCombo

execute :: Bool -> Keymap -> [Combo] -> [String] -> Int -> IO ()
execute debug keymap combos actions maxSize = do
  action <- getAction keymap
  let newActions = enqueue maxSize action actions
  putStrLn $ intercalate ", " (reverse newActions)
  let advanceFunc = if debug then advanceDebug else advanceQuiet
  newCombos <- mapM (`advanceFunc` action) combos
  putStrLn ""
  execute debug keymap newCombos newActions maxSize

executeGamePad :: Bool -> Keymap -> [Combo] -> [String] -> Int -> IO ()
executeGamePad debug keymap combos actions maxSize = do
  pressedButtons <- getActionGamepad keymap
  when (null pressedButtons) $ executeGamePad debug keymap combos actions maxSize
  let action = head pressedButtons -- TODO: handle multiple buttons
  -- putStrLn $ "Button pressed: " ++ action
  -- init
  let newActions = enqueue maxSize action actions
  putStrLn $ intercalate ", " (reverse newActions)
  let advanceFunc = if debug then advanceDebug else advanceQuiet
  newCombos <- mapM (`advanceFunc` action) combos
  putStrLn ""
  executeGamePad debug keymap newCombos newActions maxSize 

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  printInfo keymap combos
  controller <- initGameContoller
  executeGamePad (argDebug args) keymap combos [] (maximum $ map comboLen combos)
