{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (unless, void)
import Raylib.Core (beginDrawing, beginMode3D, changeDirectory, clearBackground, closeWindow, disableCursor, endDrawing, endMode3D, getApplicationDirectory, initWindow, setTargetFPS, updateCamera)
import Raylib.Core.Models (drawGrid, drawModel, genMeshCube, loadModel, loadModelFromMesh)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), Vector3 (Vector3))
import Raylib.Util (inGHCi, whileWindowOpen_)
import Raylib.Util.Colors (orange, white)

modelPath :: String
modelPath = (if not inGHCi then "../../../../../../../../../" else "./") ++ "examples/basic-models/assets/Model.obj"

main :: IO ()
main = do
  initWindow 650 400 "raylib [models] example - basic models"
  setTargetFPS 60
  disableCursor
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

  mesh <- genMeshCube 2 3 4
  cubeModel <- loadModelFromMesh mesh
  customModel <- loadModel modelPath

  let camera = Camera3D (Vector3 3 2 3) (Vector3 0 0 0) (Vector3 0 1 0) 70 CameraPerspective

  whileWindowOpen_
    ( \c -> do
        beginDrawing

        clearBackground white
        beginMode3D c

        drawGrid 20 2.0
        drawModel cubeModel (Vector3 0 1.5 0) 1 orange
        drawModel customModel (Vector3 (-5) 1 0) 1 white

        endMode3D

        endDrawing

        updateCamera c CameraModeFirstPerson
    )
    camera

  closeWindow
