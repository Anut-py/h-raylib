{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (unless, when)
import Raylib.Colors (black, red, white)
import Raylib.Core
  ( beginDrawing,
    beginMode3D,
    clearBackground,
    closeWindow,
    endDrawing,
    endMode3D,
    initWindow,
    setTargetFPS,
    updateCamera,
    windowShouldClose, disableCursor
  )
import Raylib.Models
  ( drawBoundingBox,
    drawPoint3D,
    getRayCollisionQuad,
  )
import Raylib.Text (drawFPS)
import Raylib.Types (BoundingBox (BoundingBox), Camera3D (Camera3D, camera3D'position, camera3D'target), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), Ray (Ray), RayCollision (rayCollision'hit, rayCollision'point), Vector3 (Vector3))

main :: IO ()
main = do
  initWindow 600 450 "raylib [core] example - camera ray collision"
  setTargetFPS 60
  disableCursor
  
  let camera = Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 CameraPerspective
  
  gameLoop camera

  closeWindow

gameLoop :: Camera3D -> IO ()
gameLoop camera = do
  beginDrawing

  clearBackground black
  drawFPS 10 20

  let ray = Ray (camera3D'position camera) (camera3D'target camera -.- camera3D'position camera)
  let collision = getRayCollisionQuad ray (Vector3 0 0 0) (Vector3 0 2 0) (Vector3 0 2 4) (Vector3 0 0 4)
  let col = if rayCollision'hit collision then red else white

  beginMode3D camera

  drawBoundingBox (BoundingBox (Vector3 0 0 0) (Vector3 0 2 4)) col
  when (rayCollision'hit collision) $ drawPoint3D (rayCollision'point collision) red

  endMode3D

  endDrawing
  newCam <- updateCamera camera CameraModeFirstPerson
  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop newCam

(-.-) :: Vector3 -> Vector3 -> Vector3
(Vector3 x1 x2 x3) -.- (Vector3 y1 y2 y3) = Vector3 (x1 - y1) (x2 - y2) (x3 - y3)
