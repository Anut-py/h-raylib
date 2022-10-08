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
      initWindow, drawFPS, codepointToUTF8, getWorkingDirectory, loadModel, c'loadModel )
import Raylib.Colors (lightGray, rayWhite)
import Raylib.Types (Vector2(Vector2))

main = do
  getWorkingDirectory >>= putStrLn
  -- Neither of these model loading functions work properly
  -- They result in an access violation error

  -- model <- loadModel "Cube.obj"
  -- model <- withCString "Cube.obj" c'loadModel
  -- withCString "Cube.obj" c'loadModel

  -- The error is as follows:

  -- Access violation in generated code when executing data at 0x0
  -- 
  --  Attempting to reconstruct a stack trace...
  -- 
  --    Frame        Code address
  -- Null address

  -- The Cube.obj file is found and loaded, as it shows the following lines before the error

  -- INFO: FILEIO: [Cube.obj] Text file loaded successfully
  -- INFO: MODEL: [Cube.obj] OBJ data loaded successfully: 1 meshes/1 materials
  -- INFO: MODEL: model has 1 material meshes

  initWindow 600 450 "Hello world"
  pos <- getWindowPosition
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop = do
  beginDrawing
  clearBackground rayWhite
  drawText "Testing raylib" 190 200 20 lightGray
  drawFPS 50 50
  endDrawing
  shouldClose <- windowShouldClose
  unless shouldClose gameLoop