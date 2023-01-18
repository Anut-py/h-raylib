{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (unless)
import Raylib.Core (changeDirectory, closeWindow, getApplicationDirectory, initWindow, setTargetFPS, beginDrawing, endDrawing, windowShouldClose, clearBackground, beginMode3D, endMode3D, updateCamera, setCameraMode)
import Raylib.Models (genMeshCube, loadModelFromMesh, drawModel, loadModel, drawGrid)
import Raylib.Types (Model, Camera3D (Camera3D), Vector3 (Vector3), CameraProjection (CameraPerspective), Camera, CameraMode (CameraModeFirstPerson))
import Raylib.Colors (white, orange)

modelPath :: String
modelPath = "../../../../../../../../../examples/basic-models/assets/Model.obj"

main :: IO ()
main = do
  initWindow 650 400 "raylib [models] example - basic models"
  setTargetFPS 60
  _ <- getApplicationDirectory >>= changeDirectory

  mesh <- genMeshCube 2 3 4
  cubeModel <- loadModelFromMesh mesh
  customModel <- loadModel modelPath
  let camera = Camera3D (Vector3 3 2 3) (Vector3 0 0 0) (Vector3 0 1 0) 70 CameraPerspective
  setCameraMode camera CameraModeFirstPerson

  gameLoop cubeModel customModel camera

  closeWindow

gameLoop :: Model -> Model -> Camera -> IO ()
gameLoop cubeModel customModel camera = do
  beginDrawing

  clearBackground white
  beginMode3D camera

  drawGrid 20 2.0
  drawModel cubeModel (Vector3 0 1.5 0) 1 orange
  drawModel customModel (Vector3 (-5) 1 0) 1 white

  endMode3D

  endDrawing

  newCamera <- updateCamera camera
  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop cubeModel customModel newCamera