{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core
  ( beginDrawing,
    beginMode3D,
    clearBackground,
    closeWindow,
    disableCursor,
    endDrawing,
    endMode3D,
    initWindow,
    setTargetFPS,
  )
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawCircle3D, drawCubeWiresV, drawLine3D)
import Raylib.Core.Text (drawFPS)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), Vector3 (Vector3))
import Raylib.Util (whileWindowOpen_)
import Raylib.Util.Colors (black, white)

main :: IO ()
main = do
  window <- initWindow 600 450 "raylib [core] example - first person camera"
  setTargetFPS 60
  disableCursor

  let camera = Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 CameraPerspective

  whileWindowOpen_
    ( \c -> do
        beginDrawing

        clearBackground black
        drawFPS 10 20

        beginMode3D c

        drawCircle3D (Vector3 2 0 1) 2 (Vector3 0 0 0) 0 white
        drawLine3D (Vector3 3 (-1) 1) (Vector3 1 1 1) white
        drawLine3D (Vector3 4 2 2) (Vector3 1 (-1) 1) white
        drawCubeWiresV (Vector3 (-2) 0 0) (Vector3 1 1 1) white

        endMode3D

        endDrawing
        updateCamera c CameraModeFirstPerson
    )
    camera

  closeWindow window
