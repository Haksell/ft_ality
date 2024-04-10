module GameControllerManager(initGameContoller, getActionGamepad) where
import SDL
import SDL.Input.GameController
import qualified SDL.Internal.Types
import qualified Data.Vector as V
import Control.Monad (unless)
import Utils (panic)
import Keymap
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Char (toUpper)

initGameContoller :: IO SDL.Internal.Types.GameController
initGameContoller = do
  SDL.initialize [SDL.InitGameController]

  controllers <- availableControllers
  if V.null controllers
    then
      panic "No game controllers connected!"
    else do
      let firstController = V.head controllers
      -- TODO DEGUG? putStrLn $ "Opening controller: " ++ show (gameControllerDeviceName firstController)

      openController firstController
      -- TODO DEBUG? putStrLn "Game Controller opened!"

-- TODO: also use for Gamepad
getActionGamepad :: Keymap -> IO [String]
getActionGamepad keymap = do
  events <- SDL.pollEvents
  let buttonPresses = [e | SDL.ControllerButtonEvent e <- map SDL.eventPayload events, SDL.controllerButtonEventState e == ControllerButtonPressed]
  -- unless (null buttonPresses) $ do putStrLn $ "Button pressed: " ++ show buttonPresses
  -- mapM_ (liftIO . printButtonPressed) buttonPresses
  let actions = map (`getActionFromButton` keymap) buttonPresses
  return $ catMaybes actions

getActionFromButton :: ControllerButtonEventData -> Keymap -> Maybe String
getActionFromButton buttonPress = Map.lookup (map toUpper $ show $ SDL.controllerButtonEventButton buttonPress)
