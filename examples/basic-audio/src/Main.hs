{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (unless)
import Raylib.Core (changeDirectory, closeWindow, getApplicationDirectory, initWindow, setTargetFPS, beginDrawing, endDrawing, windowShouldClose, clearBackground)
import Raylib.Types (Music)
import Raylib.Colors (rayWhite, lightGray)
import Raylib.Audio (loadMusicStream, playMusicStream, initAudioDevice, closeAudioDevice, updateMusicStream)
import Raylib.Text (drawText)

musicPath :: String
musicPath = "../../../../../../../../../examples/basic-audio/assets/mini1111.xm"

main :: IO ()
main = do
  initWindow 650 400 "raylib [audio] example - basic audio"
  initAudioDevice

  setTargetFPS 60
  _ <- changeDirectory =<< getApplicationDirectory

  music <- loadMusicStream musicPath
  playMusicStream music

  gameLoop music

  closeAudioDevice
  closeWindow

gameLoop :: Music -> IO ()
gameLoop music = do
  beginDrawing

  clearBackground rayWhite
  drawText "You should hear music playing!" 20 20 20 lightGray

  endDrawing

  updateMusicStream music
  shouldClose <- windowShouldClose
  unless shouldClose $ gameLoop music