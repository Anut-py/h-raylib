{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core (clearBackground)
import Raylib.Util (drawing, whileWindowOpen_, withWindow)
import Raylib.Util.Colors (rayWhite)
import Raylib.Util.GUI (guiLabel, guiSetStyleTextSize)
import Raylib.Types (Rectangle (Rectangle))

main :: IO ()
main = do
  withWindow
    1000
    800
    "raylib [raygui] test suite"
    60
    ( \_ -> do
        guiSetStyleTextSize 20
        whileWindowOpen_
          ( \_ -> drawing
              ( do
                  clearBackground rayWhite

                  guiLabel (Rectangle 20 20 200 20) "raygui test suite"
              ) >> return ()
          ) ()
    )
