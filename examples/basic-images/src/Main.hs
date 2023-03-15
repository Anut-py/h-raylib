{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless, void)
import Raylib.Core
  ( beginDrawing,
    beginTextureMode,
    changeDirectory,
    clearBackground,
    closeWindow,
    endDrawing,
    endTextureMode,
    getApplicationDirectory,
    initWindow,
    setTargetFPS,
  )
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures
  ( drawTexture,
    drawTexturePro,
    genImagePerlinNoise,
    loadImage,
    loadRenderTexture,
    loadTextureFromImage,
  )
import Raylib.Types (Rectangle (Rectangle), RenderTexture (renderTexture'texture), Vector2 (Vector2))
import Raylib.Util (inGHCi, whileWindowOpen0)
import Raylib.Util.Colors (black, lightGray, orange, white)

logoPath :: String
logoPath = (if not inGHCi then "../../../../../../../../../" else "./") ++ "examples/basic-images/assets/raylib-logo.png"

main :: IO ()
main = do
  window <- initWindow 600 450 "raylib [textures] example - basic images"
  setTargetFPS 60
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

  texture <- genImagePerlinNoise 600 450 20 20 2 >>= (`loadTextureFromImage` window)
  logo <- loadImage logoPath >>= (`loadTextureFromImage` window)
  rt <- loadRenderTexture 200 200 window

  whileWindowOpen0
    ( do
        beginDrawing

        beginTextureMode rt

        clearBackground lightGray
        drawText "This is scaled up" 10 10 20 black

        endTextureMode

        clearBackground white
        drawTexture texture 0 0 orange
        drawTexturePro (renderTexture'texture rt) (Rectangle 0 0 200 (-200)) (Rectangle 50 50 300 300) (Vector2 0 0) 0 white
        drawTexturePro logo (Rectangle 0 0 256 256) (Rectangle 375 50 175 175) (Vector2 0 0) 0 white

        endDrawing
    )

  closeWindow window
