{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core (clearBackground)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (lightGray, rayWhite)

main :: IO ()
main = do
  withWindow
    600
    450
    "raylib [core] example - basic window"
    60
    ( \_ -> do
        whileWindowOpen0
          ( drawing
              ( do
                  clearBackground rayWhite
                  drawText "Basic raylib window" 30 40 18 lightGray
              )
          )
    )
