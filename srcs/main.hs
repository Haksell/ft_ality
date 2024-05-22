import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..), printCombos, printSuccessfulCombo, printUnsuccessfulCombo)
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

-- TODO: advanceQuiet, advanceDebug

handleOneAction :: Bool -> String -> [Combo] -> DFA -> [String] -> Int -> IO ([String], DFA)
handleOneAction debug action combos dfa queue maxSize = do
  let newQueue = enqueue maxSize action queue
  putStrLn $ intercalate ", " (reverse newQueue)
  let (newDFA, finishedCombos) = advanceDFA dfa action
  mapM_
    ( \c ->
        if c `elem` finishedCombos
          then printSuccessfulCombo c
          else when debug $ printUnsuccessfulCombo c
    )
    combos
  putStrLn ""
  return (newQueue, newDFA)

-- TODO: verify it works the same as old recursion
handleMultipleActions :: Bool -> [String] -> [Combo] -> DFA -> [String] -> Int -> IO ([String], DFA)
handleMultipleActions debug actions combos dfa queue maxSize =
  foldM (\(q, d) action -> handleOneAction debug action combos d q maxSize) (queue, dfa) actions

executeKeyboard :: Bool -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()
executeKeyboard debug keymap combos dfa queue maxSize = do
  action <- getActionKeyboard keymap
  (newQueue, newDFA) <- handleOneAction debug action combos dfa queue maxSize
  executeKeyboard debug keymap combos newDFA newQueue maxSize

executeGamePad :: Bool -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()
executeGamePad debug keymap combos dfa queue maxSize = do
  actions <- getActionGamepad keymap
  when (null actions) $ executeGamePad debug keymap combos dfa queue maxSize
  (newQueue, newDFA) <- handleMultipleActions debug actions combos dfa queue maxSize
  executeGamePad debug keymap combos newDFA newQueue maxSize

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
  executeFunc (argDebug args) keymap combos dfa [] maxSize
