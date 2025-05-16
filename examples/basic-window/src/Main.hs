{-# LANGUAGE TemplateHaskell #-}

module Main where

import Raylib.Core (clearBackground, closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Text (drawText)
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (lightGray, rayWhite)

startup :: IO WindowResources
startup = do
  window <- initWindow 600 450 "raylib [core] example - basic window"
  setTargetFPS 60
  return window

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
  drawing
    ( do
        clearBackground rayWhite
        drawText "Basic raylib window" 30 40 18 lightGray
    )
    >> return window

shouldClose :: WindowResources -> IO Bool
shouldClose _ = windowShouldClose

teardown :: WindowResources -> IO ()
teardown = closeWindow . Just

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
