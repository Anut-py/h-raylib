{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless)
import Raylib
  ( beginDrawing,
    beginMode3D,
    clearBackground,
    closeWindow,
    drawCircle3D,
    drawFPS,
    drawLine3D,
    endDrawing,
    endMode3D,
    initWindow,
    setCameraMode,
    setTargetFPS,
    updateCamera,
    windowShouldClose, drawCubeWiresV
  )
import Raylib.Colors (black, white)
import Raylib.Constants (cameraMode'firstPerson, cameraProjection'perspective)
import Raylib.Types (Camera3D (Camera3D), Vector3 (Vector3))

main :: IO ()
main = do
  initWindow 600 450 "raylib example - first person camera"
  let camera = Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 cameraProjection'perspective
  setCameraMode camera cameraMode'firstPerson
  setTargetFPS 60
  gameLoop camera
  closeWindow

gameLoop :: Camera3D -> IO ()
gameLoop camera = do
  beginDrawing

  clearBackground black
  drawFPS 10 20

  beginMode3D camera

  drawCircle3D (Vector3 2 0 1) 2 (Vector3 0 0 0) 0 white
  drawLine3D (Vector3 3 (-1) 1) (Vector3 1 1 1) white
  drawLine3D (Vector3 4 2 2) (Vector3 1 (-1) 1) white
  drawCubeWiresV (Vector3 (-2) 0 0) (Vector3 1 1 1) white

  endMode3D

  endDrawing
  newCam <- updateCamera camera
  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop newCam