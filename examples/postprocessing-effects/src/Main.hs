{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad (unless, void)
import Raylib.Core (changeDirectory, clearBackground, getApplicationDirectory, isKeyPressed, loadShader, setShaderValue)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawCube, drawGrid, drawSphere)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (drawTextureRec, loadRenderTexture)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeOrbital), CameraProjection (CameraPerspective), KeyboardKey (KeyLeft, KeyRight), Rectangle (Rectangle), RenderTexture (renderTexture'texture), ShaderUniformData (ShaderUniformVec2), pattern Vector2, pattern Vector3)
import Raylib.Util (inGHCi, mode3D, textureMode, whileWindowOpen_, withWindow, drawing, shaderMode)
import Raylib.Util.Colors (black, blue, darkBlue, darkGreen, green, maroon, orange, red, white)

assetsPath :: String
assetsPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/postprocessing-effects/assets/"

main :: IO ()
main = do
  let width = 1300
      height = 800

  withWindow
    width
    height
    "raylib [shaders] example - postprocessing effects"
    60
    ( \window -> do
        unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

        let camera = Camera3D (Vector3 3 4 3) (Vector3 0 1 0) (Vector3 0 1 0) 45 CameraPerspective

        rt <- loadRenderTexture width height window

        -- Most of the shaders here are based on the ones at https://github.com/raysan5/raylib/tree/master/examples/shaders/resources/shaders/glsl330
        defaultShader <- loadShader Nothing Nothing window
        grayscaleShader <- loadShader Nothing (Just $ assetsPath ++ "grayscale.frag") window
        blurShader <- loadShader Nothing (Just $ assetsPath ++ "blur.frag") window
        pixelateShader <- loadShader Nothing (Just $ assetsPath ++ "pixelate.frag") window
        bloomShader <- loadShader Nothing (Just $ assetsPath ++ "bloom.frag") window

        let shaders = [("None", defaultShader), ("Grayscale", grayscaleShader), ("Blur", blurShader), ("Pixelate", pixelateShader), ("Bloom", bloomShader)]

        setShaderValue blurShader "renderSize" (ShaderUniformVec2 (Vector2 (fromIntegral width) (fromIntegral height))) window
        setShaderValue pixelateShader "renderSize" (ShaderUniformVec2 (Vector2 (fromIntegral width) (fromIntegral height))) window
        setShaderValue bloomShader "renderSize" (ShaderUniformVec2 (Vector2 (fromIntegral width) (fromIntegral height))) window

        whileWindowOpen_
          ( \(c, currentShader) -> do
              let (shaderName, selectedShader) = shaders !! currentShader

              textureMode
                rt
                ( mode3D
                    c
                    ( do
                        clearBackground white

                        drawGrid 30 1.0
                        drawCube (Vector3 0 1 0) 2.0 2.0 2.0 orange
                        drawSphere (Vector3 0 2 0) 0.5 green
                        drawSphere (Vector3 0 0 0) 0.5 darkGreen
                        drawSphere (Vector3 1 1 0) 0.5 red
                        drawSphere (Vector3 (-1) 1 0) 0.5 maroon
                        drawSphere (Vector3 0 1 1) 0.5 blue
                        drawSphere (Vector3 0 1 (-1)) 0.5 darkBlue
                    )
                )

              drawing
                ( do
                    clearBackground white

                    shaderMode selectedShader $
                      drawTextureRec (renderTexture'texture rt) (Rectangle 0 0 (fromIntegral width) (fromIntegral $ - height)) (Vector2 0 0) white

                    drawText "Press the left and right arrow keys to change the effect" 20 20 20 black
                    drawText ("Current effect: " ++ shaderName) 20 50 20 black
                )

              leftDown <- isKeyPressed KeyLeft
              rightDown <- isKeyPressed KeyRight
              let newShaderIdx = clamp (currentShader + change)
                    where
                      total = length shaders
                      clamp x
                        | x < 0 = total + x
                        | x >= total = x - total
                        | otherwise = x
                      change
                        | leftDown = -1
                        | rightDown = 1
                        | otherwise = 0

              newCam <- updateCamera c CameraModeOrbital
              return (newCam, newShaderIdx)
          )
          (camera, 0)
    )
