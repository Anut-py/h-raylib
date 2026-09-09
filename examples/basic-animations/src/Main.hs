-- adapted from https://github.com/futu2/h-raylib-examples/blob/master/src/ModelsLoadingGltf.hs
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Paths_h_raylib (getDataFileName)
import Raylib.Core (isMouseButtonPressed, clearBackground)
import Raylib.Core.Models (loadModel, loadModelAnimations, drawModel, updateModelAnimation, drawGrid)
import Raylib.Core.Text (drawText)
import Raylib.Types (Camera3D (..), MouseButton (..), CameraProjection (..), pattern Vector3)
import Raylib.Util (withWindow, managed, whileWindowOpen_, mode3D, drawing)
import Raylib.Util.Colors (rayWhite, white, gray)

modelPath :: String
modelPath = "examples/basic-animations/assets/robot.glb"

main :: IO ()
main = do
  withWindow
    800
    450
    "raylib [models] example - basic animations"
    60
    ( \window -> do

        model <- managed window $ loadModel =<< getDataFileName modelPath
        animations <- loadModelAnimations =<< getDataFileName modelPath

        let camera = Camera3D (Vector3 6 6 6) (Vector3 0 2 0) (Vector3 0 1 0) 45 CameraPerspective

        let anims = length animations
        whileWindowOpen_
          ( \(frame, anim) -> do
            leftPressed <- isMouseButtonPressed MouseButtonLeft
            rightPressed <- isMouseButtonPressed MouseButtonRight
            let anim' = if leftPressed then (anim - 1 + anims) `mod` anims
                else if rightPressed then (anim + 1) `mod` anims
                else anim
            drawing
              ( do
                  clearBackground rayWhite
                  mode3D
                    camera
                    ( do
                        drawModel model 0 1 white
                        drawGrid 10 1
                        updateModelAnimation model (animations !! anim') frame
                    )
                  drawText "Use the LEFT/RIGHT mouse buttons to switch animation" 10 10 20 gray
              )
            return ((frame + 1), anim')
          )
          (0, 0)
    )
