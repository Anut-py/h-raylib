{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Control.Monad (unless, void)
import Raylib.Core
  ( changeDirectory,
    clearBackground,
    getApplicationDirectory,
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
import Raylib.Types (Rectangle (Rectangle), RenderTexture (renderTexture'texture), pattern Vector2)
import Raylib.Util (drawing, inGHCi, textureMode, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (black, lightGray, orange, white)

logoPath :: String
logoPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/basic-images/assets/raylib-logo.png"

main :: IO ()
main = do
  withWindow
    600
    450
    "raylib [textures] example - basic images"
    60
    ( \window -> do
        unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

        texture <- genImagePerlinNoise 600 450 20 20 2 >>= (`loadTextureFromImage` window)
        logo <- loadImage logoPath >>= (`loadTextureFromImage` window)
        rt <- loadRenderTexture 200 200 window

        whileWindowOpen0
          ( drawing
              ( do
                  textureMode
                    rt
                    ( do
                        clearBackground lightGray
                        drawText "This is scaled up" 10 10 20 black
                    )

                  clearBackground white
                  drawTexture texture 0 0 orange
                  drawTexturePro (renderTexture'texture rt) (Rectangle 0 0 200 (-200)) (Rectangle 50 50 300 300) (Vector2 0 0) 0 white
                  drawTexturePro logo (Rectangle 0 0 256 256) (Rectangle 375 50 175 175) (Vector2 0 0) 0 white
              )
          )
    )
