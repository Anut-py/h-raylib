module Main where

import Control.Monad (unless)
import Foreign (Ptr, nullPtr, toBool, with)
import Foreign.C (newCString, withCString)
import Raylib
    ( drawText,
      checkCollisionLines,
      setTargetFPS,
      endDrawing,
      beginDrawing,
      clearBackground,
      getWindowPosition,
      setWindowOpacity,
      closeWindow,
      windowShouldClose,
      initWindow, drawFPS )
import Raylib.Colors (lightGray, rayWhite)
import Raylib.Types (Vector2(Vector2))

main = do
  initWindow 600 450 "Hello world"
  pos <- getWindowPosition
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop = do
  beginDrawing
  clearBackground rayWhite
  drawText "Testing raylib" 190 200 20 lightGray
  drawFPS 50 50
  endDrawing
  shouldClose <- windowShouldClose
  unless shouldClose gameLoop