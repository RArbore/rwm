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
import Graphics.X11

loop :: Display -> IO ()
loop dpy = do
  allocaXEvent $ \e -> do
    nextEvent dpy e
    t <- get_EventType e
    w <- get_Window e
    loop dpy
    
main :: IO ()
main = do
  dpy <- openDisplay ""
  f1Key <- keysymToKeycode dpy (stringToKeysym "F1")
  grabKey dpy f1Key mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeSync
  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  loop dpy
