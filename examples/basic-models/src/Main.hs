{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (unless, void)
import Raylib.Core (changeDirectory, clearBackground, disableCursor, getApplicationDirectory)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid, drawModel, genMeshCube, loadModel, loadModelFromMesh)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), Vector3 (Vector3))
import Raylib.Util (drawing, inGHCi, mode3D, whileWindowOpen_, withWindow)
import Raylib.Util.Colors (orange, white)

modelPath :: String
modelPath = (if not inGHCi then "../../../../../../../../../" else "./") ++ "examples/basic-models/assets/Model.obj"

main :: IO ()
main = do
  withWindow
    650
    400
    "raylib [models] example - basic models"
    60
    ( \window -> do
        disableCursor
        unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

        mesh <- genMeshCube 2 3 4 window
        cubeModel <- loadModelFromMesh mesh window
        customModel <- loadModel modelPath window

        let camera = Camera3D (Vector3 3 2 3) (Vector3 0 0 0) (Vector3 0 1 0) 70 CameraPerspective

        whileWindowOpen_
          ( \c -> do
              drawing
                ( do
                    clearBackground white
                    mode3D
                      c
                      ( do
                          drawGrid 20 2.0
                          drawModel cubeModel (Vector3 0 1.5 0) 1 orange
                          drawModel customModel (Vector3 (-5) 1 0) 1 white
                      )
                )
              updateCamera c CameraModeFirstPerson
          )
          camera
    )
