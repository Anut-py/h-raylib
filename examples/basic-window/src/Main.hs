{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core
  ( beginDrawing,
    clearBackground,
    closeWindow,
    endDrawing,
    initWindow,
    setTargetFPS,
  )
import Raylib.Core.Text (drawText)
import Raylib.Util (whileWindowOpen0)
import Raylib.Util.Colors (lightGray, rayWhite)

main :: IO ()
main = do
  initWindow 600 450 "raylib [core] example - basic window"
  setTargetFPS 60

  whileWindowOpen0
    ( do
        beginDrawing

        clearBackground rayWhite
        drawText "Basic raylib window" 30 40 18 lightGray

        endDrawing
    )

  closeWindow
