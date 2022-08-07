module Main where

import Control.Monad (unless)
import Foreign (Ptr, nullPtr, toBool, with)
import Foreign.C (newCString, withCString)
import Raylib
  ( Color (Color),
    beginDrawing,
    clearBackground,
    closeWindow,
    drawText,
    endDrawing,
    getMonitorPhysicalWidth,
    getMonitorPosition,
    getMonitorWidth,
    initWindow,
    setTargetFPS,
    setWindowOpacity,
    windowShouldClose, getWindowPosition, saveFileData
  )
import Raylib.Colors (lightGray, rayWhite)

main = do
  initWindow 600 450 "Hello world"
  pos <- getWindowPosition
  print pos
  setWindowOpacity 0.5
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop = do
  beginDrawing
  clearBackground rayWhite
  str <- newCString "Testing Raylib"
  with lightGray $ drawText str 190 200 20
  endDrawing
  shouldClose <- windowShouldClose
  unless shouldClose gameLoop