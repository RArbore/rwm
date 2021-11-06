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
                               showing :: Bool } deriving (Show, Eq)

instance Show WindowAttributes where
  show _ = ""
data MouseState = NoPrevMouse | MouseState { prevButtonEvent :: Event,
                                             prevButtonWindowAttr :: WindowAttributes } deriving (Show)

instance Eq MasterState where
  ms1 == ms2 = ((displays ms1) == (displays ms2)) && ((screenWidth ms1) == (screenWidth ms2)) && ((screenHeight ms1) == (screenHeight ms2))
data MasterState = MasterState { mouseState :: MouseState,
                                 displays :: [RWMDisplay],
                                 screenWidth :: Int,
                                 screenHeight :: Int } deriving (Show)

mouseIsPrev :: MouseState -> Bool
mouseIsPrev NoPrevMouse = False
mouseIsPrev _ = True

makeWindow :: MasterState -> Window -> MasterState
makeWindow ms w = MasterState (mouseState ms) (addToFirstEnabled w $ displays ms) (screenWidth ms) (screenHeight ms)
  where addToFirstEnabled _ [] = []
        addToFirstEnabled win (disp:disps)
          | showing disp = (RWMDisplay (win:(windows disp)) True):disps
          | otherwise = disp:(addToFirstEnabled win disps)

discardWindow :: MasterState -> Window -> MasterState
discardWindow ms w = MasterState (mouseState ms) (removeFromFirstAppearance w $ displays ms) (screenWidth ms) (screenHeight ms)
  where removeFromFirstAppearance _ [] = []
        removeFromFirstAppearance win (disp:disps)
          | win `elem` (windows disp) = (RWMDisplay (filter (\x -> x /= win) $ windows disp) $ showing disp):disps
          | otherwise = disp:(removeFromFirstAppearance win disps)

extractWindowsToShow :: MasterState -> [Window]
extractWindowsToShow ms = extractWindowsFromRWMDs $ displays ms
  where extractWindowsFromRWMDs [] = []
        extractWindowsFromRWMDs (rwmd:rwmds)
          | showing rwmd = (windows rwmd) ++ (extractWindowsFromRWMDs rwmds)
          | otherwise = extractWindowsFromRWMDs rwmds

positionWindows :: Display -> MasterState -> IO ()
positionWindows dpy ms = positionWindowsHelper 0 $ extractedWindows
  where extractedWindows = extractWindowsToShow ms
        positionWindowsHelper _ [] = return ()
        positionWindowsHelper n [win]
          | n == 0 = moveResizeWindow dpy win 0 0 (fromIntegral scrW) (fromIntegral scrH)
          | otherwise = moveResizeWindow dpy win (fromIntegral $ scrW `div` 2) (fromIntegral $ (n - 1) * scrH `div` stackHeight) (fromIntegral $ scrW `div` 2) (fromIntegral $ scrH `div` stackHeight)
        positionWindowsHelper n (win:wins)
          | n == 0 = do
              moveResizeWindow dpy win 0 0 (fromIntegral $ scrW `div` 2) $ fromIntegral scrH
              positionWindowsHelper (n + 1) wins
          | otherwise = do
              moveResizeWindow dpy win (fromIntegral $ scrW `div` 2) (fromIntegral $ (n - 1) * scrH `div` stackHeight) (fromIntegral $ scrW `div` 2) (fromIntegral $ scrH `div` stackHeight)
              positionWindowsHelper (n + 1) wins
        scrW = screenWidth ms
        scrH = screenHeight ms
        stackHeight = length extractedWindows - 1

loop :: Display -> MasterState -> IO ()
loop dpy state = do
  newState <- allocaXEvent $ \e -> do
    nextEvent dpy e
    t <- get_EventType e
    w <- get_Window e
    ev <- getEvent e
    appendFile "/home/russel/Work/rwm/rwm.log" $ "EVENT : " ++ (show t) ++ " " ++ (show w) ++ " " ++ (show ev) ++ ['\n']
    if t == mapRequest then do
      mapWindow dpy (ev_window ev)
      return $ makeWindow state $ ev_window ev
    else if t == destroyNotify then do
      return $ discardWindow state $ ev_window ev
    else do return state
  if state /= newState then do positionWindows dpy newState
  else do return ()
  loop dpy newState
    
main :: IO ()
main = do
  dpy <- openDisplay ""
  selectInput dpy (defaultRootWindow dpy) (substructureRedirectMask + substructureNotifyMask + buttonPressMask + pointerMotionMask + enterWindowMask + leaveWindowMask + structureNotifyMask)
  f1Key <- keysymToKeycode dpy (stringToKeysym "F1")
  grabKey dpy f1Key mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeSync
  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  loop dpy $ MasterState NoPrevMouse ((RWMDisplay [] True):(take 8 $ repeat $ RWMDisplay [] False)) 2256 1504
