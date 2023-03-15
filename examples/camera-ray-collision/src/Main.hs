{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (when)
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
import Raylib.Core.Models
  ( drawBoundingBox,
    drawPoint3D,
    getRayCollisionQuad,
  )
import Raylib.Core.Text (drawFPS)
import Raylib.Types
  ( BoundingBox (BoundingBox),
    Camera3D (Camera3D),
    CameraMode (CameraModeFirstPerson),
    CameraProjection (CameraPerspective),
    RayCollision (rayCollision'hit, rayCollision'point),
    Vector3 (Vector3),
  )
import Raylib.Util (cameraDirectionRay, whileWindowOpen_)
import Raylib.Util.Colors (black, red, white)

main :: IO ()
main = do
  window <- initWindow 600 450 "raylib [core] example - camera ray collision"
  setTargetFPS 60
  disableCursor

  let camera = Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 CameraPerspective

  whileWindowOpen_
    ( \c -> do
        beginDrawing

        clearBackground black
        drawFPS 10 20

        let collision = getRayCollisionQuad (cameraDirectionRay c) (Vector3 0 0 0) (Vector3 0 2 0) (Vector3 0 2 4) (Vector3 0 0 4)
        let color = if rayCollision'hit collision then red else white

        beginMode3D c

        drawBoundingBox (BoundingBox (Vector3 0 0 0) (Vector3 0 2 4)) color
        when (rayCollision'hit collision) $ drawPoint3D (rayCollision'point collision) red

        endMode3D

        endDrawing
        updateCamera c CameraModeFirstPerson
    )
    camera

  closeWindow window
