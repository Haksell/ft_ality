import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (comboActions), printCombos, printSuccessfulCombo)
import Control.Monad (foldM, when)
import DFA (DFA, advanceDFA)
import Data.List (intercalate)
import Gamepad (getActionGamepad, initGameContoller)
import Keyboard (getActionKeyboard)
import Keymap (Keymap, printKeymap)
import Parsing (parseFile)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import Utils (enqueue)

printInfo :: Keymap -> [Combo] -> Bool -> IO ()
printInfo keymap combos gamepad = do
  printKeymap keymap gamepad
  printCombos combos
  putColorful Green (replicate 40 '=')

advanceQuiet :: DFA -> String -> IO DFA
advanceQuiet dfa action = do
  let (newDFA, finishedCombos) = advanceDFA dfa action
  mapM_ printSuccessfulCombo finishedCombos
  return newDFA

-- advanceDebug :: DFA -> String -> IO DFA
-- advanceDebug combo action = do
--   let (isFinished, newCombo) = advanceCombo combo action
--   (if isFinished then printSuccessfulCombo else printUnsuccessfulCombo) newCombo
--   return newCombo

handleOneAction :: Bool -> String -> DFA -> [String] -> Int -> IO ([String], DFA)
handleOneAction debug action dfa queue maxSize = do
  let newQueue = enqueue maxSize action queue
  putStrLn $ intercalate ", " (reverse newQueue)
  -- let advanceFunc = if debug then advanceDebug else advanceQuiet
  newDFA <- advanceQuiet dfa action
  putStrLn ""
  return (newQueue, newDFA)

-- TODO: verify it works the same as old recursion
handleMultipleActions :: Bool -> [String] -> DFA -> [String] -> Int -> IO ([String], DFA)
handleMultipleActions debug actions dfa queue maxSize =
  foldM (\(q, d) action -> handleOneAction debug action d q maxSize) (queue, dfa) actions

executeKeyboard :: Bool -> Keymap -> DFA -> [String] -> Int -> IO ()
executeKeyboard debug keymap dfa queue maxSize = do
  action <- getActionKeyboard keymap
  (newQueue, newDFA) <- handleOneAction debug action dfa queue maxSize
  executeKeyboard debug keymap newDFA newQueue maxSize

executeGamePad :: Bool -> Keymap -> DFA -> [String] -> Int -> IO ()
executeGamePad debug keymap dfa queue maxSize = do
  actions <- getActionGamepad keymap
  when (null actions) $ executeGamePad debug keymap dfa queue maxSize
  (newQueue, newDFA) <- handleMultipleActions debug actions dfa queue maxSize
  executeGamePad debug keymap newDFA newQueue maxSize

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  args <- parseAndValidateArgs
  (keymap, combos, dfa) <- parseFile (argFilename args)
  printInfo keymap combos (argGamepad args)
  when (argGamepad args) initGameContoller
  let executeFunc = if argGamepad args then executeGamePad else executeKeyboard
  let maxSize = maximum $ map (length . comboActions) combos
  executeFunc (argDebug args) keymap dfa [] maxSize
