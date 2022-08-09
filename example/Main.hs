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
      initWindow )
import Raylib.Colors (lightGray, rayWhite)
import Raylib.Types (Vector2(Vector2))

main = do
  let col = checkCollisionLines (Vector2 (-1) (-3)) (Vector2 2 7) (Vector2 (-3) 6) (Vector2 7 1)
  case col of
    Nothing -> putStrLn "No collision found"
    Just point -> putStrLn $ "Collision at " ++ show point
  initWindow 600 450 "Hello world"
  pos <- getWindowPosition
  print pos
  setWindowOpacity 0.5
  setTargetFPS 60
  gameLoop
  closeWindow

gameLoop = do
  beginDrawing
  clearBackground rayWhite
  str <- newCString "Testing Raylib"
  with lightGray $ drawText str 190 200 20
  endDrawing
  shouldClose <- windowShouldClose
  unless shouldClose gameLoop