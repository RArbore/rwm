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

data RWMDisplay = RWMDisplay { windows :: [Window],
                               showing :: Bool }

data MouseState = NoPrevMouse | MouseState { prevButtonEvent :: Event,
                                             prevButtonWindowAttr :: WindowAttributes }

data MasterState = MasterState { mouseState :: MouseState,
                                 displays :: [RWMDisplay] }

mouseIsPrev :: MouseState -> Bool
mouseIsPrev NoPrevMouse = False
mouseIsPrev _ = True

makeWindow :: MasterState -> Window -> MasterState
makeWindow ms w = MasterState (mouseState ms) (addToFirstEnabled w $ displays ms)
  where addToFirstEnabled _ [] = []
        addToFirstEnabled win (disp:disps)
          | showing disp = (RWMDisplay (win:(windows disp)) True):disps
          | otherwise = disp:(addToFirstEnabled win disps)

discardWindow :: MasterState -> Window -> MasterState
discardWindow ms w = MasterState (mouseState ms) (removeFromFirstAppearance w $ displays ms)
  where removeFromFirstAppearance _ [] = []
        removeFromFirstAppearance win (disp:disps)
          | win `elem` (windows disp) = (RWMDisplay (filter (\x -> x /= win) $ windows disp) $ showing disp):disps
          | otherwise = disp:(removeFromFirstAppearance win disps)

loop :: Display -> MasterState -> IO ()
loop dpy state = do
  newState <- allocaXEvent $ \e -> do
    nextEvent dpy e
    t <- get_EventType e
    w <- get_Window e
    if t == createNotify then do return $ makeWindow state w
    else if t == destroyNotify then do return $ discardWindow state w
    else do return state
  loop dpy newState
    
main :: IO ()
main = do
  dpy <- openDisplay ""
  f1Key <- keysymToKeycode dpy (stringToKeysym "F1")
  grabKey dpy f1Key mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeSync
  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  loop dpy $ MasterState NoPrevMouse $ (RWMDisplay [] True):(take 8 $ repeat $ RWMDisplay [] False)
