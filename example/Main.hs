module Main where

import Control.Monad (unless)
import Foreign (nullPtr, toBool, with)
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
  gameLoop
  closeWindow

gameLoop = do
  shouldClose <- windowShouldClose
  beginDrawing
  with
    (Color 255 255 255 255)
    ( \x -> do
        clearBackground x
        with
          (Color 200 200 200 255)
          ( \y -> do
              str <- newCString "Testing Raylib"
              drawText str 190 200 20 y
              endDrawing
              unless (toBool shouldClose) gameLoop
          )
    )