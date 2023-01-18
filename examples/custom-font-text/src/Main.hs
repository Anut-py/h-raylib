{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless)
import Foreign (fromBool)
import Raylib.Colors (black, rayWhite)
import Raylib.Core
  ( beginDrawing,
    changeDirectory,
    clearBackground,
    endDrawing,
    getApplicationDirectory,
    initWindow,
    isKeyPressed,
    setTargetFPS,
    windowShouldClose,
  )
import Raylib.Text (drawText, drawTextEx, loadFont, unloadFont)
import Raylib.Types (Font, KeyboardKey (KeyDown, KeyUp), Vector2 (Vector2))

mainFontPath :: String
mainFontPath = "../../../../../../../../../examples/custom-font-text/assets/Lato-Regular.ttf"

main :: IO ()
main = do
  initWindow 800 450 "raylib [text] example - custom font text"
  setTargetFPS 60
  _ <- getApplicationDirectory >>= changeDirectory

  mainFont <- loadFont mainFontPath
  gameLoop mainFont 20
  unloadFont mainFont

gameLoop :: Font -> Int -> IO ()
gameLoop mainFont size = do
  beginDrawing
  clearBackground rayWhite

  drawTextEx mainFont "Testing drawTextEx" (Vector2 20.0 12.0) (fromIntegral size) 1.0 black
  drawText "Press the up and down arrows to change the font size" 20 (size + 15) 24 black

  increaseSize <- isKeyPressed KeyUp
  decreaseSize <- isKeyPressed KeyDown

  endDrawing

  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop mainFont (size + fromBool increaseSize - fromBool decreaseSize)
