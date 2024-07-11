{-# LANGUAGE TemplateHaskell #-}
module Main where

import Paths_h_raylib (getDataFileName)
import Control.Monad (unless, void)
import Raylib.Core (changeDirectory, clearBackground, getApplicationDirectory, initWindow, setTargetFPS, windowShouldClose, closeWindow)
import Raylib.Core.Audio (closeAudioDevice, initAudioDevice, loadMusicStream, playMusicStream, updateMusicStream)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, inGHCi, whileWindowOpen0, withWindow, managed, WindowResources, raylibApplication)
import Raylib.Util.Colors (lightGray, rayWhite)
import Raylib.Types (Music)

type AppState = (WindowResources, Music)

musicPath :: String
musicPath = "examples/basic-audio/assets/mini1111.xm"

startup :: IO AppState
startup = do
  window <- initWindow 650 400 "raylib [audio] example - basic audio"
  setTargetFPS 60
  initAudioDevice

  music <- managed window $ loadMusicStream =<< getDataFileName musicPath

  playMusicStream music

  return (window, music)

mainLoop :: AppState -> IO AppState
mainLoop state@(_, music) = do
  drawing $ do
    clearBackground rayWhite
    drawText "You should hear music playing!" 20 20 20 lightGray
  updateMusicStream music
  return state

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (window, _) = do
  closeAudioDevice (Just window)
  closeWindow (Just window)

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
