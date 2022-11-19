{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless, when)
import Raylib
  ( beginDrawing,
    beginMode3D,
    clearBackground,
    closeWindow,
    drawFPS,
    endDrawing,
    endMode3D,
    initWindow,
    setCameraMode,
    setTargetFPS,
    updateCamera,
    windowShouldClose, getRayCollisionQuad, drawBoundingBox, drawPoint3D
  )
import Raylib.Colors (black, white, red)
import Raylib.Constants (cameraMode'firstPerson, cameraProjection'perspective)
import Raylib.Types (Camera3D (Camera3D, camera3D'position, camera3D'target), Vector3 (Vector3), Ray (Ray), BoundingBox (BoundingBox), RayCollision (raycollision'hit, rayCollision'point))
import Foreign (toBool)

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

  let ray = Ray (camera3D'position camera) (camera3D'target camera -.- camera3D'position camera)
  let collision = getRayCollisionQuad ray (Vector3 0 0 0) (Vector3 0 2 0) (Vector3 0 2 4) (Vector3 0 0 4)
  let col = if toBool (raycollision'hit collision) then red else white

  print collision

  beginMode3D camera

  drawBoundingBox (BoundingBox (Vector3 0 0 0) (Vector3 0 2 4)) col
  when (raycollision'hit collision > 0) $ drawPoint3D (rayCollision'point collision) red

  endMode3D

  endDrawing
  newCam <- updateCamera camera
  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop newCam

(-.-) :: Vector3 -> Vector3 -> Vector3
(Vector3 x1 x2 x3) -.- (Vector3 y1 y2 y3) = Vector3 (x1 - y1) (x2 - y2) (x3 - y3)