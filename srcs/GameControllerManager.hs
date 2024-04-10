{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GameControllerManager (initGameContoller, getActionGamepad) where

import Data.Char (toUpper)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Keymap (Keymap)
import SDL (
  ControllerButtonEventData (controllerButtonEventButton, controllerButtonEventState),
  Event (eventPayload),
  EventPayload (ControllerButtonEvent),
  InitFlag (InitGameController),
  initialize,
  pollEvents,
 )
import SDL.Input.GameController (
  ControllerButtonState (ControllerButtonPressed),
  availableControllers,
  openController,
 )
import Utils (panic)

initGameContoller :: IO ()
initGameContoller = do
  SDL.initialize [SDL.InitGameController]

  controllers <- availableControllers
  if V.null controllers
    then
      panic "No game controllers connected!"
    else do
      let firstController = V.head controllers
      openController firstController
      return ()

getActionGamepad :: Keymap -> IO [String]
getActionGamepad keymap = do
  events <- SDL.pollEvents
  let buttonPresses = [e | SDL.ControllerButtonEvent e <- map SDL.eventPayload events, SDL.controllerButtonEventState e == ControllerButtonPressed]
  let actions = map (`getActionFromButton` keymap) buttonPresses
  return $ catMaybes actions

getActionFromButton :: ControllerButtonEventData -> Keymap -> Maybe String
getActionFromButton buttonPress = Map.lookup (map toUpper $ show $ SDL.controllerButtonEventButton buttonPress)
