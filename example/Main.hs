module Main where

import Control.Monad (unless)
import Foreign (nullPtr, toBool, with, Ptr)
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

main = do
  str <- newCString "Hello world"
  initWindow 600 450 str
  setTargetFPS 60
  with (Color 255 255 255 255) (with (Color 200 200 200 255) . gameLoop)
  closeWindow

gameLoop :: Ptr Color -> Ptr Color -> IO ()
gameLoop white gray = do
  beginDrawing
  clearBackground white
  str <- newCString "Testing Raylib"
  drawText str 190 200 20 gray
  endDrawing
  shouldClose <- windowShouldClose
  unless (toBool shouldClose) (gameLoop white gray)