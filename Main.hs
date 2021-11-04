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

import Graphics.X11

blockUntilMapNotify :: Display -> IO ()
blockUntilMapNotify display = do
  allocaXEvent $ \e -> do
    nextEvent display e
    t <- get_EventType e
    if (t == mapNotify) then return () else blockUntilMapNotify display

main :: IO ()
main = do
  display <- openDisplay ""
  let black = blackPixel display (defaultScreen display)
  let white = whitePixel display (defaultScreen display)
  window <- createSimpleWindow display (defaultRootWindow display) 0 0 200 100 0 black black
  selectInput display window structureNotifyMask
  mapWindow display window
  gc <- createGC display window
  setForeground display gc white
  blockUntilMapNotify display
  drawLine display window gc 10 60 180 20
  flush display
  blockUntilMapNotify display
  print "Hello"
