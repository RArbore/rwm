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
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HM
  
import Graphics.X11
import Graphics.X11.Xlib.Extras

import System.Posix.Process

data RWMAction = CloseWindow deriving (Show, Eq)
data UserAction = RunCommand { command :: String } | RunAction RWMAction deriving (Show, Eq)

keybindings :: [(KeySym, UserAction)]
keybindings = [
  (xK_c, RunAction CloseWindow),
  (xK_r, RunCommand "dmenu_run"),
  (xK_e, RunCommand "emacsclient -c -a emacs"),
  (xK_b, RunCommand "icecat"),
  (xK_q, RunCommand "killall rwm"),
  (xK_Return, RunCommand "alacritty")
              ]

data RWMDisplay = RWMDisplay { windows :: [Window],
                               showing :: Bool } deriving (Show, Eq)

data MouseState = NoPrevMouse | MouseState { prevButtonEvent :: Event,
                                             prevButtonWindowAttr :: WindowAttributes }
instance Show MouseState where
  show s = show $ prevButtonEvent s

data MasterState = MasterState { mouseState :: MouseState,
                                 displays :: [RWMDisplay],
                                 screenWidth :: Int,
                                 screenHeight :: Int,
                                 focusedWindow :: Window,
                                 xDisplay :: Display,
                                 gap :: Int,
                                 keycodesToAction :: HM.HashMap KeyCode UserAction }
instance Show MasterState where
  show ms = show (focusedWindow ms) ++ " | " ++ (concat $ intersperse " " $ map show $ concat $ map windows $ displays ms)
instance Eq MasterState where
  ms1 == ms2 = ((displays ms1) == (displays ms2)) &&
               ((screenWidth ms1) == (screenWidth ms2)) &&
               ((screenHeight ms1) == (screenHeight ms2)) &&
               ((focusedWindow ms1) == (focusedWindow ms2)) &&
               ((xDisplay ms1) == (xDisplay ms2)) &&
               ((gap ms1) == (gap ms2)) &&
               ((keycodesToAction ms1) == (keycodesToAction ms2))

mouseIsPrev :: MouseState -> Bool
mouseIsPrev NoPrevMouse = False
mouseIsPrev _ = True

executeAction :: MasterState -> UserAction -> IO MasterState
executeAction ms (RunCommand cmd) = do
  forkProcess $ executeFile "/bin/sh" False ["-c", cmd] Nothing
  return ms
executeAction ms (RunAction CloseWindow) = do
  if focusedWindow ms /= (defaultRootWindow $ xDisplay ms) && (focusedWindow ms) `elem` (concat $ map windows $ displays ms) then do
    destroyWindow (xDisplay ms) $ focusedWindow ms
    return $ discardWindow ms $ focusedWindow ms
  else do return (ms)

executeKeyCode :: MasterState -> KeyCode -> IO MasterState
executeKeyCode ms kc
  | isJust lookupResult = executeAction ms $ fromJust lookupResult
  | otherwise = return ms
  where lookupResult = HM.lookup kc $ keycodesToAction ms

makeWindow :: MasterState -> Window -> MasterState
makeWindow ms w = if w `elem` (concat $ map windows $ displays ms) then ms else ms {displays = addToFirstEnabled w $ displays ms, focusedWindow = w}
  where addToFirstEnabled _ [] = []
        addToFirstEnabled win (disp:disps)
          | showing disp = (RWMDisplay (win:(windows disp)) True):disps
          | otherwise = disp:(addToFirstEnabled win disps)

discardWindow :: MasterState -> Window -> MasterState
discardWindow ms w = ms {displays = removedFirstAppearance, focusedWindow = if w == focusedWindow ms then (if isJust firstWindow then fromJust firstWindow else defaultRootWindow $ xDisplay ms) else focusedWindow ms}
  where removedFirstAppearance = removeFromFirstAppearance w $ displays ms
        removeFromFirstAppearance _ [] = []
        removeFromFirstAppearance win (disp:disps)
          | win `elem` (windows disp) = (RWMDisplay (filter (\x -> x /= win) $ windows disp) $ showing disp):disps
          | otherwise = disp:(removeFromFirstAppearance win disps)
        findFirstWindow [] = Nothing
        findFirstWindow (disp:disps)
          | showing disp && (length $ windows disp) > 0 = Just $ head $ windows disp
          | otherwise = findFirstWindow disps
        firstWindow = findFirstWindow removedFirstAppearance

extractWindowsToShow :: MasterState -> [Window]
extractWindowsToShow ms = extractWindowsFromRWMDs $ displays ms
  where extractWindowsFromRWMDs [] = []
        extractWindowsFromRWMDs (rwmd:rwmds)
          | showing rwmd = (windows rwmd) ++ (extractWindowsFromRWMDs rwmds)
          | otherwise = extractWindowsFromRWMDs rwmds

positionWindows :: Display -> MasterState -> IO ()
positionWindows dpy ms = positionWindowsHelper 0 $ extractedWindows
  where windowGap = gap ms
        extractedWindows = extractWindowsToShow ms
        positionWindowsHelper _ [] = return ()
        positionWindowsHelper n [win]
          | n == 0 = moveResizeWindow dpy win (fromIntegral windowGap) (fromIntegral windowGap) (fromIntegral $ scrW - 2 * windowGap) (fromIntegral $ scrH - 2 * windowGap)
          | otherwise = stackMoveResizeWindow n win
        positionWindowsHelper n (win:wins)
          | n == 0 = do
              moveResizeWindow dpy win (fromIntegral windowGap) (fromIntegral windowGap) (fromIntegral $ scrW `div` 2 - 3 * windowGap `div` 2) $ fromIntegral (scrH - 2 * windowGap)
              positionWindowsHelper (n + 1) wins
          | otherwise = do
              stackMoveResizeWindow n win
              positionWindowsHelper (n + 1) wins
        scrW = screenWidth ms
        scrH = screenHeight ms
        stackHeight = length extractedWindows - 1
        stackMoveResizeWindow n win = moveResizeWindow dpy win (fromIntegral $ (scrW `div` 2) + (windowGap `div` 2)) (fromIntegral $ (n - 1) * (scrH - 2 * windowGap) `div` stackHeight + ((n - 1) * windowGap `div` stackHeight) + windowGap) (fromIntegral $ (scrW `div` 2) - (3 * windowGap `div` 2)) (fromIntegral $ ((scrH - (1 + stackHeight) * windowGap) `div` stackHeight))

handle :: MasterState -> Event -> IO MasterState
handle ms (KeyEvent {ev_event_type = etype, ev_keycode = keycode})
  | etype == keyPress = executeKeyCode ms $ keycode
  | otherwise = return ms
handle ms (MapRequestEvent {ev_window = win}) = do
  if win `elem` (concat $ map windows $ displays ms) then return () else mapWindow (xDisplay ms) (win)
  return $ makeWindow ms $ win
handle ms (DestroyWindowEvent {ev_window = win}) = return $ discardWindow ms win
handle ms e@(ConfigureRequestEvent {ev_window = win}) = do
  if win `elem` (concat $ map windows $ displays ms) then do
    attr <- getWindowAttributes (xDisplay ms) win
    configureWindow (xDisplay ms) win (ev_value_mask e) $ WindowChanges {
        wc_x            = wa_x attr
      , wc_y            = wa_y attr
      , wc_width        = wa_width attr
      , wc_height       = wa_height attr
      , wc_border_width = wa_border_width attr
      , wc_sibling      = ev_above e
      , wc_stack_mode   = ev_detail e
                                                                        }
    return ms
  else return ms
handle ms _ = return ms

loop :: Display -> MasterState -> IO ()
loop dpy state = do
  newState <- allocaXEvent $ \e -> do
    nextEvent dpy e
    t <- get_EventType e
    ev <- getEvent e
    handle state ev
  if state /= newState then do positionWindows dpy newState
  else do return ()
  loop dpy newState

grabKeys :: Display -> [(KeySym, UserAction)] -> IO [(KeyCode, UserAction)]
grabKeys _ [] = return []
grabKeys dpy (x:xs) = do
  key <- keysymToKeycode dpy (fst x)
  grabKey dpy key mod4Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  rest <- grabKeys dpy xs
  return $ (key, snd x):rest
    
main :: IO ()
main = do
  dpy <- openDisplay ""
  setErrorHandler (\d -> (\p -> do
                                  error <- getErrorEvent p
                                  appendFile "/home/russel/Work/rwm/rwm.log" $ "ERROR: "
                                    ++ (show $ ev_type error) ++ ['\n']
                                    ++ (show $ ev_display error) ++ ['\n']
                                    ++ (show $ ev_serialnum error) ++ ['\n']
                                    ++ (show $ ev_error_code error) ++ ['\n']
                                    ++ (show $ ev_request_code error) ++ ['\n']
                                    ++ (show $ ev_minor_code error) ++ ['\n']
                                    ++ (show $ ev_resourceid error) ++ ['\n']
                             ))
  selectInput dpy (defaultRootWindow dpy) (substructureRedirectMask + substructureNotifyMask + buttonPressMask + pointerMotionMask + enterWindowMask + leaveWindowMask + structureNotifyMask)
  keycodeActions <- grabKeys dpy $ keybindings
  grabButton dpy 1 mod4Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  grabButton dpy 3 mod4Mask (defaultRootWindow dpy) True (buttonPressMask + buttonReleaseMask + pointerMotionMask) grabModeAsync grabModeAsync 0 0
  sync dpy False
  loop dpy $ MasterState NoPrevMouse ((RWMDisplay [] True):(take 8 $ repeat $ RWMDisplay [] False)) (fromIntegral $ displayWidth dpy $ defaultScreen dpy) (fromIntegral $ displayHeight dpy $ defaultScreen dpy) (defaultRootWindow dpy) dpy 6 (HM.fromList keycodeActions)
