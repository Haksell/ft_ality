import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), advanceCombo, printCombos, printSuccessfulCombo, printUnsuccessfulCombo)
import Control.Monad (when)
import Data.List (intercalate)
import Gamepad (getActionGamepad, initGameContoller)
import Keyboard (getActionKeyboard)
import Keymap (Keymap, printKeymap)
import Parsing (parseFile)
import System.IO (
  BufferMode (NoBuffering),
  hSetBuffering,
  hSetEcho,
  stdin,
 )
import Utils (enqueue)

printInfo :: Keymap -> [Combo] -> Bool -> IO ()
printInfo keymap combos gamepad = do
  printKeymap keymap gamepad
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

handleOneAction :: Bool -> String -> [Combo] -> [String] -> Int -> IO ([String], [Combo])
handleOneAction debug action combos queue maxSize = do
  let newQueue = enqueue maxSize action queue
  putStrLn $ intercalate ", " (reverse newQueue)
  let advanceFunc = if debug then advanceDebug else advanceQuiet
  newCombos <- mapM (`advanceFunc` action) combos
  putStrLn ""
  return (newQueue, newCombos)

handleMultipleActions :: Bool -> [String] -> [Combo] -> [String] -> Int -> IO ([String], [Combo])
handleMultipleActions _ [] combos queue _ = return (queue, combos)
handleMultipleActions debug (action : newActions) combos queue maxSize = do
  (newQueue, newCombos) <- handleOneAction debug action combos queue maxSize
  handleMultipleActions debug newActions newCombos newQueue maxSize

executeKeyboard :: Bool -> Keymap -> [Combo] -> [String] -> Int -> IO ()
executeKeyboard debug keymap combos queue maxSize = do
  action <- getActionKeyboard keymap
  (newQueue, newCombos) <- handleOneAction debug action combos queue maxSize
  executeKeyboard debug keymap newCombos newQueue maxSize

executeGamePad :: Bool -> Keymap -> [Combo] -> [String] -> Int -> IO ()
executeGamePad debug keymap combos queue maxSize = do
  actions <- getActionGamepad keymap
  when (null actions) $ executeGamePad debug keymap combos queue maxSize
  (newQueue, newCombos) <- handleMultipleActions debug actions combos queue maxSize
  executeGamePad debug keymap newCombos newQueue maxSize

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos) <- parseFile (argFilename args)
  printInfo keymap combos (argGamepad args)
  when (argGamepad args) initGameContoller
  let executeFunc = if argGamepad args then executeGamePad else executeKeyboard
  executeFunc (argDebug args) keymap combos [] (maximum $ map comboLen combos)
