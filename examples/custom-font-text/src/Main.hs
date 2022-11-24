{-# OPTIONS -Wall #-}
module Main where

import Control.Monad ( unless )
import Raylib
    ( beginDrawing,
      clearBackground,
      drawTextEx,
      endDrawing,
      initWindow,
      loadFont,
      setTargetFPS,
      windowShouldClose, changeDirectory, getApplicationDirectory, unloadFont, isKeyPressed )
import Raylib.Types (Vector2 (Vector2), Font)
import Raylib.Colors (rayWhite, black)
import Raylib.Constants (key'up, key'down)
import Foreign (fromBool)

mainFontPath :: String
mainFontPath = "../../../../../../../../../assets/Lato-Regular.ttf"

main :: IO ()
main = do
  initWindow 600 450 "raylib example - custom font text"
  setTargetFPS 60
  _ <- getApplicationDirectory >>= changeDirectory

  mainFont <- loadFont mainFontPath
  gameLoop mainFont 20.0
  unloadFont mainFont

gameLoop :: Font -> Float -> IO ()
gameLoop mainFont size = do
  beginDrawing
  clearBackground rayWhite

  drawTextEx mainFont "Testing drawTextEx" (Vector2 20.0 12.0) size 1.0 black

  increaseSize <- isKeyPressed key'up
  decreaseSize <- isKeyPressed key'down

  endDrawing

  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop mainFont (size + fromBool increaseSize - fromBool decreaseSize)