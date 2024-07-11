{-# LANGUAGE PatternSynonyms #-}

module Main where

import Paths_h_raylib (getDataFileName)
import Control.Monad (unless, void)
import Raylib.Core (changeDirectory, clearBackground, disableCursor, getApplicationDirectory)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid, drawModel, genMeshCube, loadModel, loadModelFromMesh)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3)
import Raylib.Util (drawing, inGHCi, mode3D, whileWindowOpen_, withWindow, managed)
import Raylib.Util.Colors (orange, white)

modelPath :: String
modelPath = "examples/basic-models/assets/Model.obj"

main :: IO ()
main = do
  withWindow
    650
    400
    "raylib [models] example - basic models"
    60
    ( \window -> do
        disableCursor

        mesh <- managed window $ genMeshCube 2 3 4
        cubeModel <- managed window $ loadModelFromMesh mesh
        customModel <- managed window $ loadModel =<< getDataFileName modelPath

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
