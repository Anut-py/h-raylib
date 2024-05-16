{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, disableCursor)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawCircle3D, drawCubeWiresV, drawLine3D)
import Raylib.Core.Text (drawFPS)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow)
import Raylib.Util.Colors (black, white)

main :: IO ()
main = do
  withWindow
    600
    450
    "raylib [core] example - first person camera"
    60
    ( \_ -> do
        disableCursor

        let camera = Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 CameraPerspective

        whileWindowOpen_
          ( \c ->
              drawing
                ( do
                    clearBackground black
                    drawFPS 10 20

                    mode3D
                      c
                      ( do
                          drawCircle3D (Vector3 2 0 1) 2 (Vector3 0 0 0) 0 white
                          drawLine3D (Vector3 3 (-1) 1) (Vector3 1 1 1) white
                          drawLine3D (Vector3 4 2 2) (Vector3 1 (-1) 1) white
                          drawCubeWiresV (Vector3 (-2) 0 0) (Vector3 1 1 1) white
                      )
                )
                >> updateCamera c CameraModeFirstPerson
          )
          camera
    )
