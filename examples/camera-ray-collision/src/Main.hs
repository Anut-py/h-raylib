{-# OPTIONS -Wall #-}
module Main where

import Control.Monad (when)
import Raylib.Core (clearBackground, disableCursor)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawBoundingBox, drawPoint3D, getRayCollisionQuad)
import Raylib.Core.Text (drawFPS)
import Raylib.Types
  ( BoundingBox (BoundingBox),
    Camera3D (Camera3D),
    CameraMode (CameraModeFirstPerson),
    CameraProjection (CameraPerspective),
    RayCollision (rayCollision'hit, rayCollision'point),
    Vector3 (Vector3),
  )
import Raylib.Util (cameraDirectionRay, drawing, mode3D, whileWindowOpen_, withWindow)
import Raylib.Util.Colors (black, red, white)

main :: IO ()
main = do
  withWindow
    600
    450
    "raylib [core] example - camera ray collision"
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

                    let collision = getRayCollisionQuad (cameraDirectionRay c) (Vector3 0 0 0) (Vector3 0 2 0) (Vector3 0 2 4) (Vector3 0 0 4)
                    let color = if rayCollision'hit collision then red else white

                    mode3D
                      c
                      ( do
                          drawBoundingBox (BoundingBox (Vector3 0 0 0) (Vector3 0 2 4)) color
                          when (rayCollision'hit collision) $ drawPoint3D (rayCollision'point collision) red
                      )
                )
                >> updateCamera c CameraModeFirstPerson
          )
          camera
    )
