module Main where

import Control.Monad (unless)
import Foreign (Ptr, nullPtr, toBool, with)
import Foreign.C (newCString, withCString)
import Raylib
    ( drawText,
      checkCollisionLines,
      setTargetFPS,
      endDrawing,
      beginDrawing,
      clearBackground,
      getWindowPosition,
      setWindowOpacity,
      closeWindow,
      windowShouldClose,
      initWindow, drawFPS, codepointToUTF8, getWorkingDirectory, loadModel, c'loadModel, setCameraMode, updateCamera, beginMode3D, endMode3D, drawModel, drawCircle3D, drawLine3D )
import Raylib.Colors (lightGray, rayWhite, white, black)
import Raylib.Types (Camera3D (Camera3D), Vector3 (Vector3), Model)
import Raylib.Constants (cameraProjection'perspective, cameraMode'firstPerson)

main = do
  getWorkingDirectory >>= putStrLn
  initWindow 600 450 "Hello world"
  let camera = Camera3D (Vector3 0 0 0) (Vector3 2 0 1) (Vector3 0 1 0) 70 cameraProjection'perspective
  setCameraMode camera cameraMode'firstPerson
  pos <- getWindowPosition
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

  endMode3D

  endDrawing
  newCam <- updateCamera camera
  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop newCam