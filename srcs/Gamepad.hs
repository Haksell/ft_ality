module Gamepad (initGamepad, getActionGamepad) where

import Control.Monad (void)
import Data.Char (toUpper)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Vector as V -- TODO: directly SDL vector?
import Keymap (Keymap)
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

getActionGamepad :: Keymap -> IO [String]
getActionGamepad keymap = do
  events <- SDL.pollEvents
  let buttonPresses =
        [ e
        | SDL.ControllerButtonEvent e <- map SDL.eventPayload events
        , SDL.controllerButtonEventState e == Controller.ControllerButtonPressed
        ]
  let actions = map getActionFromButton buttonPresses
  return $ catMaybes actions
 where
  getActionFromButton :: SDL.ControllerButtonEventData -> Maybe String
  getActionFromButton buttonPress =
    Map.lookup
      (map toUpper $ show $ SDL.controllerButtonEventButton buttonPress)
      keymap
