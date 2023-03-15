{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Core.Audio where

import Foreign
  ( Ptr,
    Storable (peek, sizeOf),
    castPtr,
    toBool,
  )
import Foreign.C (CUChar, withCString)
import Raylib.ForeignUtil
  ( pop,
    popCArray,
    withFreeable,
    withFreeableArrayLen,
  )
import Raylib.Internal (addAudioBuffer, addCtxData, unloadAudioBuffers, unloadCtxData, unloadSingleAudioBuffer, unloadSingleCtxDataPtr, WindowResources)
import Raylib.Native
  ( c'closeAudioDevice,
    c'exportWave,
    c'exportWaveAsCode,
    c'getMusicTimeLength,
    c'getMusicTimePlayed,
    c'isAudioDeviceReady,
    c'isAudioStreamPlaying,
    c'isAudioStreamProcessed,
    c'isAudioStreamReady,
    c'isMusicReady,
    c'isMusicStreamPlaying,
    c'isSoundPlaying,
    c'isSoundReady,
    c'isWaveReady,
    c'loadAudioStream,
    c'loadMusicStream,
    c'loadMusicStreamFromMemory,
    c'loadSound,
    c'loadSoundFromWave,
    c'loadWave,
    c'loadWaveFromMemory,
    c'loadWaveSamples,
    c'pauseAudioStream,
    c'pauseMusicStream,
    c'pauseSound,
    c'playAudioStream,
    c'playMusicStream,
    c'playSound,
    c'resumeAudioStream,
    c'resumeMusicStream,
    c'resumeSound,
    c'seekMusicStream,
    c'setAudioStreamPan,
    c'setAudioStreamPitch,
    c'setAudioStreamVolume,
    c'setMasterVolume,
    c'setMusicPan,
    c'setMusicPitch,
    c'setMusicVolume,
    c'setSoundPan,
    c'setSoundPitch,
    c'setSoundVolume,
    c'stopAudioStream,
    c'stopMusicStream,
    c'stopSound,
    c'updateAudioStream,
    c'updateMusicStream,
    c'updateSound,
    c'waveCopy,
    c'waveCrop,
    c'waveFormat,
  )
import Raylib.Types
  ( AudioStream (audioStream'buffer),
    Music (music'ctxData, music'ctxType, music'stream),
    Sound (sound'stream),
    Wave (wave'channels, wave'frameCount),
  )

foreign import ccall safe "raylib.h InitAudioDevice"
  initAudioDevice ::
    IO ()

closeAudioDevice :: WindowResources -> IO ()
closeAudioDevice wr = do
  unloadCtxData wr
  unloadAudioBuffers wr
  c'closeAudioDevice

isAudioDeviceReady :: IO Bool
isAudioDeviceReady = toBool <$> c'isAudioDeviceReady

setMasterVolume :: Float -> IO ()
setMasterVolume volume = c'setMasterVolume (realToFrac volume)

loadWave :: String -> IO Wave
loadWave fileName = withCString fileName c'loadWave >>= pop

loadWaveFromMemory :: String -> [Integer] -> IO Wave
loadWaveFromMemory fileType fileData = withCString fileType (\f -> withFreeableArrayLen (map fromIntegral fileData) (\size d -> c'loadWaveFromMemory f d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

loadSound :: String -> IO Sound
loadSound fileName = withCString fileName c'loadSound >>= pop

loadSoundFromWave :: Wave -> IO Sound
loadSoundFromWave wave = withFreeable wave c'loadSoundFromWave >>= pop

updateSound :: Sound -> Ptr () -> Int -> IO ()
updateSound sound dataValue sampleCount = withFreeable sound (\s -> c'updateSound s dataValue (fromIntegral sampleCount))

-- | Unloads an sound from RAM. Sounds are automatically unloaded
-- when `closeAudioDevice` is called, so manually unloading sounds is
-- not required. In larger projects, you may want to manually unload
-- sounds to avoid having them in RAM for too long.
unloadSound :: Sound -> WindowResources -> IO ()
unloadSound sound = unloadAudioStream (sound'stream sound)

isWaveReady :: Wave -> IO Bool
isWaveReady wave = toBool <$> withFreeable wave c'isWaveReady

isSoundReady :: Sound -> IO Bool
isSoundReady sound = toBool <$> withFreeable sound c'isSoundReady

exportWave :: Wave -> String -> IO Bool
exportWave wave fileName = toBool <$> withFreeable wave (withCString fileName . c'exportWave)

exportWaveAsCode :: Wave -> String -> IO Bool
exportWaveAsCode wave fileName = toBool <$> withFreeable wave (withCString fileName . c'exportWaveAsCode)

playSound :: Sound -> IO ()
playSound sound = withFreeable sound c'playSound

stopSound :: Sound -> IO ()
stopSound sound = withFreeable sound c'stopSound

pauseSound :: Sound -> IO ()
pauseSound sound = withFreeable sound c'pauseSound

resumeSound :: Sound -> IO ()
resumeSound sound = withFreeable sound c'resumeSound

isSoundPlaying :: Sound -> IO Bool
isSoundPlaying sound = toBool <$> withFreeable sound c'isSoundPlaying

setSoundVolume :: Sound -> Float -> IO ()
setSoundVolume sound volume = withFreeable sound (\s -> c'setSoundVolume s (realToFrac volume))

setSoundPitch :: Sound -> Float -> IO ()
setSoundPitch sound pitch = withFreeable sound (\s -> c'setSoundPitch s (realToFrac pitch))

setSoundPan :: Sound -> Float -> IO ()
setSoundPan sound pan = withFreeable sound (\s -> c'setSoundPan s (realToFrac pan))

waveCopy :: Wave -> IO Wave
waveCopy wave = withFreeable wave c'waveCopy >>= pop

waveCrop :: Wave -> Int -> Int -> IO Wave
waveCrop wave initSample finalSample = do
  new <- waveCopy wave
  withFreeable new (\w -> c'waveCrop w (fromIntegral initSample) (fromIntegral finalSample) >> peek w)

waveFormat :: Wave -> Int -> Int -> Int -> IO ()
waveFormat wave sampleRate sampleSize channels = do
  new <- waveCopy wave
  withFreeable new (\n -> c'waveFormat n (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels))

loadWaveSamples :: Wave -> IO [Float]
loadWaveSamples wave =
  withFreeable
    wave
    (\w -> map realToFrac <$> (popCArray (fromIntegral $ wave'frameCount wave * wave'channels wave) =<< c'loadWaveSamples w))

loadMusicStream :: String -> WindowResources -> IO Music
loadMusicStream fileName wr = do
  music <- withCString fileName c'loadMusicStream >>= pop
  addAudioBuffer (castPtr (audioStream'buffer $ music'stream music)) wr
  addCtxData (fromEnum $ music'ctxType music) (music'ctxData music) wr
  return music

loadMusicStreamFromMemory :: String -> [Integer] -> WindowResources -> IO Music
loadMusicStreamFromMemory fileType streamData wr = do
  music <- withCString fileType (\t -> withFreeableArrayLen (map fromIntegral streamData) (\size d -> c'loadMusicStreamFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop
  addAudioBuffer (castPtr (audioStream'buffer $ music'stream music)) wr
  addCtxData (fromEnum $ music'ctxType music) (music'ctxData music) wr
  return music

-- | Unloads a music stream from RAM. Music streams are automatically unloaded
-- when `closeAudioDevice` is called, so manually unloading music streams is
-- not required. In larger projects, you may want to manually unload music
-- streams to avoid having them in RAM for too long.
unloadMusicStream :: Music -> WindowResources -> IO ()
unloadMusicStream music = unloadSingleCtxDataPtr (fromEnum $ music'ctxType music) (music'ctxData music)

isMusicReady :: Music -> IO Bool
isMusicReady music = toBool <$> withFreeable music c'isMusicReady

playMusicStream :: Music -> IO ()
playMusicStream music = withFreeable music c'playMusicStream

isMusicStreamPlaying :: Music -> IO Bool
isMusicStreamPlaying music = toBool <$> withFreeable music c'isMusicStreamPlaying

updateMusicStream :: Music -> IO ()
updateMusicStream music = withFreeable music c'updateMusicStream

stopMusicStream :: Music -> IO ()
stopMusicStream music = withFreeable music c'stopMusicStream

pauseMusicStream :: Music -> IO ()
pauseMusicStream music = withFreeable music c'pauseMusicStream

resumeMusicStream :: Music -> IO ()
resumeMusicStream music = withFreeable music c'resumeMusicStream

seekMusicStream :: Music -> Float -> IO ()
seekMusicStream music position = withFreeable music (\m -> c'seekMusicStream m (realToFrac position))

setMusicVolume :: Music -> Float -> IO ()
setMusicVolume music volume = withFreeable music (\m -> c'setMusicVolume m (realToFrac volume))

setMusicPitch :: Music -> Float -> IO ()
setMusicPitch music pitch = withFreeable music (\m -> c'setMusicPitch m (realToFrac pitch))

setMusicPan :: Music -> Float -> IO ()
setMusicPan music pan = withFreeable music (\m -> c'setMusicPan m (realToFrac pan))

getMusicTimeLength :: Music -> IO Float
getMusicTimeLength music = realToFrac <$> withFreeable music c'getMusicTimeLength

getMusicTimePlayed :: Music -> IO Float
getMusicTimePlayed music = realToFrac <$> withFreeable music c'getMusicTimePlayed

loadAudioStream :: Integer -> Integer -> Integer -> WindowResources -> IO AudioStream
loadAudioStream sampleRate sampleSize channels wr = do
  stream <- c'loadAudioStream (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels) >>= pop
  addAudioBuffer (castPtr (audioStream'buffer stream)) wr
  return stream

-- | Unloads an audio stream from RAM. Audio streams are automatically unloaded
-- when `closeAudioDevice` is called, so manually unloading audio streams is
-- not required. In larger projects, you may want to manually unload audio
-- streams to avoid having them in RAM for too long.
unloadAudioStream :: AudioStream -> WindowResources -> IO ()
unloadAudioStream stream = unloadSingleAudioBuffer (castPtr $ audioStream'buffer stream)

isAudioStreamReady :: AudioStream -> IO Bool
isAudioStreamReady stream = toBool <$> withFreeable stream c'isAudioStreamReady

updateAudioStream :: AudioStream -> Ptr () -> Int -> IO ()
updateAudioStream stream value frameCount = withFreeable stream (\s -> c'updateAudioStream s value (fromIntegral frameCount))

isAudioStreamProcessed :: AudioStream -> IO Bool
isAudioStreamProcessed stream = toBool <$> withFreeable stream c'isAudioStreamProcessed

playAudioStream :: AudioStream -> IO ()
playAudioStream stream = withFreeable stream c'playAudioStream

pauseAudioStream :: AudioStream -> IO ()
pauseAudioStream stream = withFreeable stream c'pauseAudioStream

resumeAudioStream :: AudioStream -> IO ()
resumeAudioStream stream = withFreeable stream c'resumeAudioStream

isAudioStreamPlaying :: AudioStream -> IO Bool
isAudioStreamPlaying stream = toBool <$> withFreeable stream c'isAudioStreamPlaying

stopAudioStream :: AudioStream -> IO ()
stopAudioStream stream = withFreeable stream c'stopAudioStream

setAudioStreamVolume :: AudioStream -> Float -> IO ()
setAudioStreamVolume stream volume = withFreeable stream (\s -> c'setAudioStreamVolume s (realToFrac volume))

setAudioStreamPitch :: AudioStream -> Float -> IO ()
setAudioStreamPitch stream pitch = withFreeable stream (\s -> c'setAudioStreamPitch s (realToFrac pitch))

setAudioStreamPan :: AudioStream -> Float -> IO ()
setAudioStreamPan stream pan = withFreeable stream (\s -> c'setAudioStreamPan s (realToFrac pan))

setAudioStreamBufferSizeDefault :: Int -> IO ()
setAudioStreamBufferSizeDefault = setAudioStreamBufferSizeDefault . fromIntegral
