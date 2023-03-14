{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless, void)
import Foreign (fromBool)
import Raylib.Core
  ( beginDrawing,
    changeDirectory,
    clearBackground,
    closeWindow,
    endDrawing,
    getApplicationDirectory,
    initWindow,
    isKeyPressed,
    setTargetFPS,
  )
import Raylib.Core.Text (drawText, drawTextEx, loadFont)
import Raylib.Types (KeyboardKey (KeyDown, KeyUp), Vector2 (Vector2))
import Raylib.Util (inGHCi, whileWindowOpen_)
import Raylib.Util.Colors (black, rayWhite)

mainFontPath :: String
mainFontPath = (if not inGHCi then "../../../../../../../../../" else "./") ++ "examples/custom-font-text/assets/Lato-Regular.ttf"

main :: IO ()
main = do
  initWindow 800 450 "raylib [text] example - custom font text"
  setTargetFPS 60
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

  mainFont <- loadFont mainFontPath

  whileWindowOpen_
    ( \size -> do
        beginDrawing
        clearBackground rayWhite

        drawTextEx mainFont "Testing drawTextEx" (Vector2 20.0 12.0) (fromIntegral size) 1.0 black
        drawText "Press the up and down arrows to change the font size" 20 (size + 15) 24 black

        increaseSize <- isKeyPressed KeyUp
        decreaseSize <- isKeyPressed KeyDown

        endDrawing

        return (size + fromBool increaseSize - fromBool decreaseSize)
    )
    20

  closeWindow
