{-  This file is part of rwm.
    rwm is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    rwm is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with rwm. If not, see <https://www.gnu.org/licenses/>.  -}

import Control.Monad

import Data.Int
  
import Graphics.X11
import Graphics.X11.Xlib.Extras

data LoopState = NoPrev | LoopState { prevButtonEvent :: XButtonEvent,
                                      prevButtonWindowAttr :: WindowAttributes
                           }

isPrev :: LoopState -> Bool
isPrev NoPrev = False
isPrev _ = True

loop :: Display -> LoopState -> IO ()
loop dpy state = do
  newState <- allocaXEvent $ \e -> do
    nextEvent dpy e
    t <- get_EventType e
    w <- get_Window e
    if (t == keyPress) then do
      raiseWindow dpy w
      return state
    else if (t == buttonPress) then do
      --motionEvent <- get_MotionEvent e
      --windowAttr <- getWindowAttributes dpy w
      --let (_, _, _, _, _, x, y, _, _, _) = motionEvent
      --    xDiff = x - (prevMouseX state)
      --    yDiff = y - (prevMouseY state)
      --moveWindow dpy w (fromIntegral (xDiff + (wa_x windowAttr))) (fromIntegral (yDiff + (wa_y windowAttr)))
      buttonEvent <- get_ButtonEvent e
      windowAttr <- getWindowAttributes dpy w
      return $ LoopState buttonEvent windowAttr
    else if (t == motionNotify && isPrev state) then do
      motionEvent <- get_MotionEvent e
      let (_, _, _, _, _, nx, ny, _, _, _) = motionEvent
          (_, _, _, _, _, px, py, _, _, _) = prevButtonEvent state
      moveResizeWindow dpy w (fromIntegral $ (wa_x $ prevButtonWindowAttr state) + nx - px) (fromIntegral $ (wa_y $ prevButtonWindowAttr state) + ny - py) (fromIntegral $ wa_width $ prevButtonWindowAttr state) (fromIntegral $ wa_height $ prevButtonWindowAttr state)
      return state
    else if (t == buttonRelease) then do
      return NoPrev
    else return state
  loop dpy newState
    
main :: IO ()
main = do
  dpy <- openDisplay ""
  f1Key <- keysymToKeycode dpy (stringToKeysym "F1")
  grabKey dpy f1Key mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeSync
  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  loop dpy NoPrev
