module Main where

import Control.Monad (unless)
import Foreign (Ptr, nullPtr, toBool, with)
import Foreign.C (newCString)
import Raylib
  ( Color (Color),
    beginDrawing,
    clearBackground,
    closeWindow,
    drawText,
    endDrawing,
    initWindow,
    setTargetFPS,
    windowShouldClose,
  )
import Raylib.Colors (lightGray, rayWhite)

main = do
  str <- newCString "Hello world"
  initWindow 600 450 str
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop = do
  beginDrawing
  with rayWhite clearBackground
  str <- newCString "Testing Raylib"
  with lightGray $ drawText str 190 200 20
  endDrawing
  shouldClose <- windowShouldClose
  unless (toBool shouldClose) gameLoop