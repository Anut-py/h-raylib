{-# OPTIONS -Wall #-}

module Main where

import Control.Monad (unless, void)
import Raylib.Core (beginDrawing, changeDirectory, clearBackground, closeWindow, endDrawing, getApplicationDirectory, initWindow, setTargetFPS)
import Raylib.Core.Audio (closeAudioDevice, initAudioDevice, loadMusicStream, playMusicStream, updateMusicStream)
import Raylib.Core.Text (drawText)
import Raylib.Util (inGHCi, whileWindowOpen0)
import Raylib.Util.Colors (lightGray, rayWhite)

musicPath :: String
musicPath = (if not inGHCi then "../../../../../../../../../" else "./") ++ "examples/basic-audio/assets/mini1111.xm"

main :: IO ()
main = do
  initWindow 650 400 "raylib [audio] example - basic audio"
  initAudioDevice

  setTargetFPS 60
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

  music <- loadMusicStream musicPath
  playMusicStream music

  whileWindowOpen0
    ( do
        beginDrawing

        clearBackground rayWhite
        drawText "You should hear music playing!" 20 20 20 lightGray

        endDrawing

        updateMusicStream music
    )

  closeAudioDevice
  closeWindow
