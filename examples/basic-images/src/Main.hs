{-# LANGUAGE PatternSynonyms #-}

module Main where

import Paths_h_raylib (getDataFileName)
import Raylib.Core (clearBackground)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures
  ( drawTexture,
    drawTexturePro,
    genImagePerlinNoise,
    loadImage,
    loadRenderTexture,
    loadTextureFromImage,
  )
import Raylib.Types (Image, Rectangle (Rectangle), RenderTexture (renderTexture'texture), Texture, pattern Vector2)
import Raylib.Util (drawing, managed, textureMode, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (black, lightGray, orange, white)

logoPath :: String
logoPath = "examples/basic-images/assets/raylib-logo.png"

main :: IO ()
main = do
  withWindow
    600
    450
    "raylib [textures] example - basic images"
    60
    ( \window -> do
        texture <- managed window $ loadTextureFromImage =<< (genImagePerlinNoise 600 450 20 20 2 :: IO Image) :: IO Texture
        logo <- managed window $ loadTextureFromImage =<< (loadImage =<< getDataFileName logoPath :: IO Image) :: IO Texture
        rt <- managed window $ loadRenderTexture 200 200 :: IO RenderTexture

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
