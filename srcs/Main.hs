import Args (Args (..), parseAndValidateArgs)
import Colors (Color (..), putColorful)
import Combo (Combo (..))
import Control.Monad (when)
import DFA (DFA, advanceDFA)
import Data.List (find, intercalate, isSuffixOf)
import Gamepad (getActionGamepad, initGamepad)
import Keyboard (getActionKeyboard, initKeyboard)
import Keymap (Keymap, printKeymap)
import Parsing (parseFile)
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

type GetAction = Keymap -> IO (Maybe String)

gameLoop :: GetAction -> Bool -> Keymap -> [Combo] -> DFA -> [String] -> Int -> IO ()
gameLoop getAction debug keymap combos dfa queue maxSize = do
  action <- getAction keymap
  case action of
    Nothing -> return ()
    Just a -> do
      (newQueue, newDFA) <- handleAction debug a combos dfa queue maxSize
      gameLoop getAction debug keymap combos newDFA newQueue maxSize

main :: IO ()
main = do
  args <- parseAndValidateArgs
  (keymap, combos, dfa) <- parseFile (argFilename args)
  printInfo keymap combos (argGamepad args)
  if argGamepad args then initGamepad else initKeyboard
  let getAction = if argGamepad args then getActionGamepad else getActionKeyboard
  let debug = argDebug args
  let maxSize = maximum $ map (length . comboActions) combos
  gameLoop getAction debug keymap combos dfa [] maxSize
