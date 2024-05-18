module Main where

import Control.Monad (unless, void)
import Raylib.Core (changeDirectory, clearBackground, getApplicationDirectory)
import Raylib.Core.Audio (closeAudioDevice, initAudioDevice, loadMusicStream, playMusicStream, updateMusicStream)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, inGHCi, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (lightGray, rayWhite)

musicPath :: String
musicPath = (if not inGHCi then "../../../../../../../../../../" else "./") ++ "examples/basic-audio/assets/mini1111.xm"

main :: IO ()
main = do
  withWindow
    650
    400
    "raylib [audio] example - basic audio"
    60
    ( \window -> do
        initAudioDevice
        unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)

        music <- loadMusicStream musicPath window
        playMusicStream music

        whileWindowOpen0
          ( drawing
              ( do
                  clearBackground rayWhite
                  drawText "You should hear music playing!" 20 20 20 lightGray
              )
              >> updateMusicStream music
          )
          
        closeAudioDevice window
    )
