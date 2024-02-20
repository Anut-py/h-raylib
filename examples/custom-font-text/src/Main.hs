{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless, void)
import Foreign (fromBool)
import Raylib.Core
  ( changeDirectory,
    clearBackground,
    getApplicationDirectory,
    isKeyPressed,
  )
import Raylib.Core.Text (drawText, drawTextEx, loadFont)
import Raylib.Types (KeyboardKey (KeyDown, KeyUp), Vector2 (Vector2))
import Raylib.Util (inGHCi, whileWindowOpen_, withWindow, drawing)
import Raylib.Util.Colors (black, rayWhite)

mainFontPath :: String
mainFontPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/custom-font-text/assets/Lato-Regular.ttf"

main :: IO ()
main = do
  withWindow
    800
    450
    "raylib [text] example - custom font text"
    60
    ( \window -> do
        unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

        mainFont <- loadFont mainFontPath window

        whileWindowOpen_
          ( \size -> do
              drawing
                ( do
                    clearBackground rayWhite
  
                    drawTextEx mainFont "Testing drawTextEx" (Vector2 20.0 12.0) (fromIntegral size) 1.0 black
                    drawText "Press the up and down arrows to change the font size" 20 (size + 15) 24 black
                )
                
              increaseSize <- isKeyPressed KeyUp
              decreaseSize <- isKeyPressed KeyDown
              return (size + fromBool increaseSize - fromBool decreaseSize)
          )
          20
    )
