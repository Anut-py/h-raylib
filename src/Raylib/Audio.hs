{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Audio where

import Foreign
  ( FunPtr,
    Ptr,
    Storable (peek, sizeOf),
    peekArray,
    toBool,
    with,
    withArrayLen,
  )
import Foreign.C (CUChar, CUInt, withCString)
import Raylib.Native
  ( c'exportWave,
    c'exportWaveAsCode,
    c'getMusicTimeLength,
    c'getMusicTimePlayed,
    c'getSoundsPlaying,
    c'isAudioDeviceReady,
    c'isAudioStreamPlaying,
    c'isAudioStreamProcessed,
    c'isMusicStreamPlaying,
    c'isSoundPlaying,
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
    c'unloadSound,
    c'unloadWave,
    c'unloadWaveSamples,
    c'updateAudioStream,
    c'updateMusicStream,
    c'updateSound,
    c'waveCopy,
    c'waveCrop,
    c'waveFormat,
  )
import Raylib.Types
  ( AudioStream,
    Music,
    Sound,
    Wave (wave'channels, wave'frameCount),
  )
import Raylib.Util (pop)

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
loadSoundFromWave wave = with wave c'loadSoundFromWave >>= pop

updateSound :: Sound -> Ptr () -> Int -> IO ()
updateSound sound dataValue sampleCount = with sound (\s -> c'updateSound s dataValue (fromIntegral sampleCount))

unloadWave :: Wave -> IO ()
unloadWave wave = with wave c'unloadWave

unloadSound :: Sound -> IO ()
unloadSound sound = with sound c'unloadSound

exportWave :: Wave -> String -> IO Bool
exportWave wave fileName = toBool <$> with wave (withCString fileName . c'exportWave)

exportWaveAsCode :: Wave -> String -> IO Bool
exportWaveAsCode wave fileName = toBool <$> with wave (withCString fileName . c'exportWaveAsCode)

playSound :: Sound -> IO ()
playSound sound = with sound c'playSound

stopSound :: Sound -> IO ()
stopSound sound = with sound c'stopSound

pauseSound :: Sound -> IO ()
pauseSound sound = with sound c'pauseSound

resumeSound :: Sound -> IO ()
resumeSound sound = with sound c'resumeSound

playSoundMulti :: Sound -> IO ()
playSoundMulti sound = with sound c'playSoundMulti

foreign import ccall safe "raylib.h StopSoundMulti"
  stopSoundMulti ::
    IO ()

getSoundsPlaying :: IO Int
getSoundsPlaying = fromIntegral <$> c'getSoundsPlaying

isSoundPlaying :: Sound -> IO Bool
isSoundPlaying sound = toBool <$> with sound c'isSoundPlaying

setSoundVolume :: Sound -> Float -> IO ()
setSoundVolume sound volume = with sound (\s -> c'setSoundVolume s (realToFrac volume))

setSoundPitch :: Sound -> Float -> IO ()
setSoundPitch sound pitch = with sound (\s -> c'setSoundPitch s (realToFrac pitch))

setSoundPan :: Sound -> Float -> IO ()
setSoundPan sound pan = with sound (\s -> c'setSoundPan s (realToFrac pan))

waveCopy :: Wave -> IO Wave
waveCopy wave = with wave c'waveCopy >>= pop

waveCrop :: Wave -> Int -> Int -> IO Wave
waveCrop wave initSample finalSample = do
  new <- waveCopy wave
  with new (\w -> c'waveCrop w (fromIntegral initSample) (fromIntegral finalSample) >> peek w)

waveFormat :: Wave -> Int -> Int -> Int -> IO ()
waveFormat wave sampleRate sampleSize channels = do
  new <- waveCopy wave
  with new (\n -> c'waveFormat n (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels))

loadWaveSamples :: Wave -> IO [Float]
loadWaveSamples wave =
  with
    wave
    ( \w -> do
        ptr <- c'loadWaveSamples w
        arr <- peekArray (fromIntegral $ wave'frameCount wave * wave'channels wave) ptr
        c'unloadWaveSamples ptr
        return $ map realToFrac arr
    )

loadMusicStream :: String -> IO Music
loadMusicStream fileName = withCString fileName c'loadMusicStream >>= pop

loadMusicStreamFromMemory :: String -> [Integer] -> IO Music
loadMusicStreamFromMemory fileType streamData = withCString fileType (\t -> withArrayLen (map fromIntegral streamData) (\size d -> c'loadMusicStreamFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

unloadMusicStream :: Music -> IO ()
unloadMusicStream music = with music c'unloadMusicStream

playMusicStream :: Music -> IO ()
playMusicStream music = with music c'playMusicStream

isMusicStreamPlaying :: Music -> IO Bool
isMusicStreamPlaying music = toBool <$> with music c'isMusicStreamPlaying

updateMusicStream :: Music -> IO ()
updateMusicStream music = with music c'updateMusicStream

stopMusicStream :: Music -> IO ()
stopMusicStream music = with music c'stopMusicStream

pauseMusicStream :: Music -> IO ()
pauseMusicStream music = with music c'pauseMusicStream

resumeMusicStream :: Music -> IO ()
resumeMusicStream music = with music c'resumeMusicStream

seekMusicStream :: Music -> Float -> IO ()
seekMusicStream music position = with music (\m -> c'seekMusicStream m (realToFrac position))

setMusicVolume :: Music -> Float -> IO ()
setMusicVolume music volume = with music (\m -> c'setMusicVolume m (realToFrac volume))

setMusicPitch :: Music -> Float -> IO ()
setMusicPitch music pitch = with music (\m -> c'setMusicPitch m (realToFrac pitch))

setMusicPan :: Music -> Float -> IO ()
setMusicPan music pan = with music (\m -> c'setMusicPan m (realToFrac pan))

getMusicTimeLength :: Music -> IO Float
getMusicTimeLength music = realToFrac <$> with music c'getMusicTimeLength

getMusicTimePlayed :: Music -> IO Float
getMusicTimePlayed music = realToFrac <$> with music c'getMusicTimePlayed

loadAudioStream :: Integer -> Integer -> Integer -> IO AudioStream
loadAudioStream sampleRate sampleSize channels = c'loadAudioStream (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels) >>= pop

unloadAudioStream :: AudioStream -> IO ()
unloadAudioStream stream = with stream c'unloadAudioStream

updateAudioStream :: AudioStream -> Ptr () -> Int -> IO ()
updateAudioStream stream value frameCount = with stream (\s -> c'updateAudioStream s value (fromIntegral frameCount))

isAudioStreamProcessed :: AudioStream -> IO Bool
isAudioStreamProcessed stream = toBool <$> with stream c'isAudioStreamProcessed

playAudioStream :: AudioStream -> IO ()
playAudioStream stream = with stream c'playAudioStream

pauseAudioStream :: AudioStream -> IO ()
pauseAudioStream stream = with stream c'pauseAudioStream

resumeAudioStream :: AudioStream -> IO ()
resumeAudioStream stream = with stream c'resumeAudioStream

isAudioStreamPlaying :: AudioStream -> IO Bool
isAudioStreamPlaying stream = toBool <$> with stream c'isAudioStreamPlaying

stopAudioStream :: AudioStream -> IO ()
stopAudioStream stream = with stream c'stopAudioStream

setAudioStreamVolume :: AudioStream -> Float -> IO ()
setAudioStreamVolume stream volume = with stream (\s -> c'setAudioStreamVolume s (realToFrac volume))

setAudioStreamPitch :: AudioStream -> Float -> IO ()
setAudioStreamPitch stream pitch = with stream (\s -> c'setAudioStreamPitch s (realToFrac pitch))

setAudioStreamPan :: AudioStream -> Float -> IO ()
setAudioStreamPan stream pan = with stream (\s -> c'setAudioStreamPan s (realToFrac pan))

setAudioStreamBufferSizeDefault :: Int -> IO ()
setAudioStreamBufferSizeDefault = setAudioStreamBufferSizeDefault . fromIntegral
