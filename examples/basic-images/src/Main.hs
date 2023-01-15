{-# OPTIONS -Wall #-}
module Main where
import Raylib (initWindow, closeWindow, drawTexture, loadTextureFromImage, beginDrawing, endDrawing, windowShouldClose, setTargetFPS, clearBackground, loadRenderTexture, beginTextureMode, endTextureMode, drawText, drawTexturePro, genImagePerlinNoise, loadImage, changeDirectory, getApplicationDirectory)
import Raylib.Colors (orange, white, lightGray, black)
import Raylib.Types (Texture, RenderTexture (rendertexture'texture), Rectangle (Rectangle), Vector2 (Vector2))
import Control.Monad (unless)

logoPath :: String
logoPath = "../../../../../../../../../examples/basic-images/assets/raylib-logo.png"

main :: IO ()
main = do
    initWindow 600 450 "raylib [textures] example - basic images"
    setTargetFPS 60
    _ <- getApplicationDirectory >>= changeDirectory

    texture <- genImagePerlinNoise 600 450 20 20 2 >>= loadTextureFromImage
    logo <- loadImage logoPath >>= loadTextureFromImage
    rt <- loadRenderTexture 200 200

    gameLoop texture logo rt

    closeWindow

gameLoop :: Texture -> Texture -> RenderTexture -> IO ()
gameLoop texture logo rt = do
    beginDrawing

    beginTextureMode rt

    clearBackground lightGray
    drawText "This is scaled up" 10 10 20 black

    endTextureMode

    clearBackground white
    drawTexture texture 0 0 orange
    drawTexturePro (rendertexture'texture rt) (Rectangle 0 0 200 (-200)) (Rectangle 50 50 300 300) (Vector2 0 0) 0 white
    drawTexturePro logo (Rectangle 0 0 256 256) (Rectangle 375 50 175 175) (Vector2 0 0) 0 white

    endDrawing

    shouldClose <- windowShouldClose
    unless shouldClose $ gameLoop texture logo rt