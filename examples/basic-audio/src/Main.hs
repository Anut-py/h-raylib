{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (forkOS)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign (ForeignPtr)
import Paths_h_raylib (getDataFileName)
import Raylib.Core (clearBackground, closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Audio (closeAudioDevice, initAudioDevice, loadMusicStream, playMusicStream, updateMusicStream)
import Raylib.Core.Text (drawText)
import Raylib.Types (Music)
import Raylib.Util (WindowResources, drawing, managed, raylibApplication)
import Raylib.Util.Colors (lightGray, rayWhite)

type AppState = (WindowResources, IORef (Maybe (ForeignPtr Music)))

musicPath :: String
musicPath = "examples/basic-audio/assets/mini1111.xm"

startup :: IO AppState
startup = do
  window <- initWindow 650 400 "raylib [audio] example - basic audio"
  setTargetFPS 60

  -- Multithreaded to avoid pause on startup when initAudioDevice is called
  mref <- newIORef Nothing
  _ <- forkOS $ do
    initAudioDevice
    music <- managed window $ loadMusicStream =<< getDataFileName musicPath
    playMusicStream music
    writeIORef mref (Just music)

  return (window, mref)

mainLoop :: AppState -> IO AppState
mainLoop state@(_, mref) = do
  music <- readIORef mref
  drawing $ do
    clearBackground rayWhite
    case music of
      Nothing -> drawText "Music is loading..." 20 20 20 lightGray
      Just _ -> drawText "You should hear music playing!" 20 20 20 lightGray
  maybe (return ()) updateMusicStream music
  return state

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (window, _) = do
  closeAudioDevice (Just window)
  closeWindow (Just window)

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
