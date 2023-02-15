{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Audio where

import Control.Monad (void)
import Foreign
  ( FunPtr,
    Ptr,
    Storable (peek, sizeOf),
    newForeignPtr,
    peekArray,
    toBool,
    withArrayLen,
  )
import Foreign.C (CUChar, CUInt, withCString)
import qualified Foreign.Concurrent as C
import Raylib.Native
  ( c'exportWave,
    c'exportWaveAsCode,
    c'getMusicTimeLength,
    c'getMusicTimePlayed,
    c'getSoundsPlaying,
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
    c'playSoundMulti,
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
    c'unloadAudioStream,
    c'unloadMusicStream,
    c'unloadMusicStreamData,
    c'unloadSound,
    c'unloadWave,
    c'unloadWaveSamples,
    c'updateAudioStream,
    c'updateMusicStream,
    c'updateSound,
    c'waveCopy,
    c'waveCrop,
    c'waveFormat,
    p'unloadAudioBuffer,
  )
import Raylib.Types
  ( AudioStream (audioStream'buffer),
    Music (music'ctxData, music'ctxType, music'stream),
    Sound,
    Wave (wave'channels, wave'frameCount),
  )
import Raylib.Util
  ( pop,
    withFreeable,
  )

foreign import ccall safe "raylib.h InitAudioDevice"
  initAudioDevice ::
    IO ()

foreign import ccall safe "raylib.h CloseAudioDevice"
  closeAudioDevice ::
    IO ()

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())

-- TODO: redesign this
isAudioDeviceReady :: IO Bool
isAudioDeviceReady = toBool <$> c'isAudioDeviceReady

setMasterVolume :: Float -> IO ()
setMasterVolume volume = c'setMasterVolume (realToFrac volume)

loadWave :: String -> IO Wave
loadWave fileName = withCString fileName c'loadWave >>= pop

loadWaveFromMemory :: String -> [Integer] -> IO Wave
loadWaveFromMemory fileType fileData = withCString fileType (\f -> withArrayLen (map fromIntegral fileData) (\size d -> c'loadWaveFromMemory f d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

loadSound :: String -> IO Sound
loadSound fileName = withCString fileName c'loadSound >>= pop

loadSoundFromWave :: Wave -> IO Sound
loadSoundFromWave wave = withFreeable wave c'loadSoundFromWave >>= pop

updateSound :: Sound -> Ptr () -> Int -> IO ()
updateSound sound dataValue sampleCount = withFreeable sound (\s -> c'updateSound s dataValue (fromIntegral sampleCount))

isWaveReady :: Wave -> IO Bool
isWaveReady wave = toBool <$> withFreeable wave c'isWaveReady

unloadWave :: Wave -> IO ()
unloadWave wave = withFreeable wave c'unloadWave

isSoundReady :: Sound -> IO Bool
isSoundReady sound = toBool <$> withFreeable sound c'isSoundReady

unloadSound :: Sound -> IO ()
unloadSound sound = withFreeable sound c'unloadSound

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

playSoundMulti :: Sound -> IO ()
playSoundMulti sound = withFreeable sound c'playSoundMulti

foreign import ccall safe "raylib.h StopSoundMulti"
  stopSoundMulti ::
    IO ()

getSoundsPlaying :: IO Int
getSoundsPlaying = fromIntegral <$> c'getSoundsPlaying

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
    ( \w -> do
        ptr <- c'loadWaveSamples w
        arr <- peekArray (fromIntegral $ wave'frameCount wave * wave'channels wave) ptr
        c'unloadWaveSamples ptr
        return $ map realToFrac arr
    )

loadMusicStream :: String -> IO Music
loadMusicStream fileName = do
  music <- withCString fileName c'loadMusicStream >>= pop
  addFinalizerToAudioStream $ music'stream music
  addFinalizerToMusicStream music
  return music

loadMusicStreamFromMemory :: String -> [Integer] -> IO Music
loadMusicStreamFromMemory fileType streamData = do
  music <- withCString fileType (\t -> withArrayLen (map fromIntegral streamData) (\size d -> c'loadMusicStreamFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop
  addFinalizerToAudioStream $ music'stream music
  addFinalizerToMusicStream music
  return music

isMusicReady :: Music -> IO Bool
isMusicReady music = toBool <$> withFreeable music c'isMusicReady

unloadMusicStream :: Music -> IO ()
unloadMusicStream music = withFreeable music c'unloadMusicStream

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

loadAudioStream :: Integer -> Integer -> Integer -> IO AudioStream
loadAudioStream sampleRate sampleSize channels = do
  stream <- c'loadAudioStream (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels) >>= pop
  addFinalizerToAudioStream stream
  return stream

isAudioStreamReady :: AudioStream -> IO Bool
isAudioStreamReady stream = toBool <$> withFreeable stream c'isAudioStreamReady

unloadAudioStream :: AudioStream -> IO ()
unloadAudioStream stream = withFreeable stream c'unloadAudioStream

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

addFinalizerToAudioStream :: AudioStream -> IO ()
addFinalizerToAudioStream stream = void (newForeignPtr p'unloadAudioBuffer (audioStream'buffer stream)) -- Automatically unload audio buffer when it is no longer being used

-- Foreign.Concurrent might not call the finalizer, so this may need to be revised in the future
addFinalizerToMusicStream :: Music -> IO ()
addFinalizerToMusicStream music = do
  let ctxData = music'ctxData music
  _ <-
    C.newForeignPtr
      ctxData
      (c'unloadMusicStreamData (fromIntegral $ fromEnum $ music'ctxType music) ctxData) -- Automatically unload music context data when it is no longer being used
  return ()
