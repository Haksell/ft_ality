module Gamepad (initGamepad, getActionGamepad) where

import Control.Monad (void)
import Data.Char (toUpper)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V -- TODO: directly SDL vector?
import Keymap (Keymap)
import SDL (ControllerButtonEventData (..))
import qualified SDL
import qualified SDL.Input.GameController as Controller
import Utils (panic)

initGamepad :: IO ()
initGamepad = do
  SDL.initialize [SDL.InitGameController] -- TODO: remove since SDL.initializeAll already done?
  controllers <- Controller.availableControllers
  if V.null controllers
    then panic "No game controllers connected!"
    else void (Controller.openController (V.head controllers))

getActionGamepad :: Keymap -> IO (Maybe String)
getActionGamepad keymap = do
  events <- SDL.pollEvents
  let buttonPresses =
        [ e
        | SDL.ControllerButtonEvent e <- map SDL.eventPayload events
        , SDL.controllerButtonEventState e == Controller.ControllerButtonPressed
        ]
  if any (\b -> SDL.controllerButtonEventButton b == Controller.ControllerButtonGuide) buttonPresses
    then return Nothing
    else case mapMaybe getActionFromButton buttonPresses of
      [] -> getActionGamepad keymap
      (action : _) -> return $ Just action
 where
  getActionFromButton :: SDL.ControllerButtonEventData -> Maybe String
  getActionFromButton buttonPress =
    Map.lookup
      (map toUpper $ show $ SDL.controllerButtonEventButton buttonPress)
      keymap
