{-# LANGUAGE TemplateHaskell #-}

module Main where

import Paths_h_raylib (getDataFileName)
import Raylib.Core (clearBackground, closeWindow, initWindow, loadFileText, setLoadFileTextCallback, setTargetFPS, setTraceLogCallback, windowShouldClose)
import Raylib.Core.Text (drawText)
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (black, rayWhite)

filePath :: String
filePath = "examples/basic-callbacks/assets/data.txt"

type AppState = (String, WindowResources)

startup :: IO AppState
startup = do
  setTraceLogCallback (\logLevel text -> putStrLn (show logLevel ++ ": " ++ text))
  window <- initWindow 600 450 "raylib [core] example - basic callbacks"
  setTargetFPS 60
  setLoadFileTextCallback (\s -> putStrLn ("opening file: " ++ s) >> readFile s)
  text <- loadFileText =<< getDataFileName filePath
  return (text, window)

mainLoop :: AppState -> IO AppState
mainLoop (text, window) =
  drawing
    ( do
        clearBackground rayWhite
        drawText "File contents:" 30 40 24 black
        drawText text 30 70 24 black
    )
    >> return (text, window)

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown = closeWindow . Just . snd

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
