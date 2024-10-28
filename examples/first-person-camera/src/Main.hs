{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Raylib.Core (initWindowUnmanaged, setTargetFPS, windowShouldClose, closeWindow, clearBackground, disableCursor)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawCircle3D, drawCubeWiresV, drawLine3D)
import Raylib.Core.Text (drawFPS)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3)
import Raylib.Util (drawing, mode3D, raylibApplication)
import Raylib.Util.Colors (black, white)

startup :: IO Camera3D
startup = do
  initWindowUnmanaged 600 450 "raylib [core] example - first person camera"
  setTargetFPS 60
  disableCursor

  return $ Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 CameraPerspective

mainLoop :: Camera3D -> IO Camera3D
mainLoop camera =
  drawing
    ( do
        clearBackground black
        drawFPS 10 20

        mode3D
          camera
          ( do
              drawCircle3D (Vector3 2 0 1) 2 (Vector3 0 0 0) 0 white
              drawLine3D (Vector3 3 (-1) 1) (Vector3 1 1 1) white
              drawLine3D (Vector3 4 2 2) (Vector3 1 (-1) 1) white
              drawCubeWiresV (Vector3 (-2) 0 0) (Vector3 1 1 1) white
          )
    )
    >> updateCamera camera CameraModeFirstPerson

shouldClose :: Camera3D -> IO Bool
shouldClose _ = windowShouldClose

teardown :: Camera3D -> IO ()
teardown _ = closeWindow Nothing

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
