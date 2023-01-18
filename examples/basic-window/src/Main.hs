{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless)
import Raylib.Colors (lightGray, rayWhite)
import Raylib.Core
  ( beginDrawing,
    clearBackground,
    closeWindow,
    endDrawing,
    initWindow,
    setTargetFPS,
    windowShouldClose,
  )
import Raylib.Text (drawText)

main :: IO ()
main = do
  initWindow 600 450 "raylib [core] example - basic window"
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop :: IO ()
gameLoop = do
  beginDrawing

  clearBackground rayWhite
  drawText "Basic raylib window" 30 40 18 lightGray

  endDrawing

  shouldClose <- windowShouldClose
  unless shouldClose gameLoop
