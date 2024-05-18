{-# LANGUAGE TemplateHaskell #-}
module Main where

import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose, closeWindow, setLoadFileTextCallback, loadFileText, getApplicationDirectory)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, raylibApplication, WindowResources, inGHCi)
import Raylib.Util.Colors (black, rayWhite)

filePath :: String
filePath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/basic-callbacks/assets/data.txt"

type AppState = (String, WindowResources)

startup :: IO AppState
startup = do
  window <- initWindow 600 450 "raylib [core] example - basic callbacks"
  setTargetFPS 60
  executableDir <- getApplicationDirectory
  _ <- setLoadFileTextCallback (\s -> putStrLn ("opening file: " ++ executableDir ++ s) >> readFile (executableDir ++ s)) window
  text <- loadFileText filePath
  return (text, window)

mainLoop :: AppState -> IO AppState
mainLoop (text, window) =
  drawing
    ( do
        clearBackground rayWhite
        drawText "File contents:" 30 40 24 black
        drawText text 30 70 24 black
    ) >> return (text, window)

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown = closeWindow . snd

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
