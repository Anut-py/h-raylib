module Main where

import Control.Monad (unless)
import Foreign (nullPtr, toBool, with)
import Foreign.C (newCString)
import Raylib
    ( setTargetFPS,
      endDrawing,
      beginDrawing,
      closeWindow,
      windowShouldClose,
      initWindow )

main = do
  str <- newCString "Hello world"
  initWindow 600 450 str
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop = do
  shouldClose <- windowShouldClose
  beginDrawing
  endDrawing
  unless (toBool shouldClose) gameLoop