{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings to @raudio@
module Raylib.Core.Audio
  ( -- * High level
    initAudioDevice,
    closeAudioDevice,
    isAudioDeviceReady,
    setMasterVolume,
    getMasterVolume,
    loadWave,
    loadWaveFromMemory,
    loadSound,
    loadSoundFromWave,
    loadSoundAlias,
    unloadSoundAlias,
    updateSound,
    unloadSound,
    isWaveValid,
    isSoundValid,
    exportWave,
    exportWaveAsCode,
    playSound,
    stopSound,
    pauseSound,
    resumeSound,
    isSoundPlaying,
    setSoundVolume,
    setSoundPitch,
    setSoundPan,
    waveCopy,
    waveCrop,
    waveFormat,
    loadWaveSamples,
    loadMusicStream,
    loadMusicStreamFromMemory,
    unloadMusicStream,
    isMusicValid,
    playMusicStream,
    isMusicStreamPlaying,
    updateMusicStream,
    stopMusicStream,
    pauseMusicStream,
    resumeMusicStream,
    seekMusicStream,
    setMusicVolume,
    setMusicPitch,
    setMusicPan,
    getMusicTimeLength,
    getMusicTimePlayed,
    loadAudioStream,
    unloadAudioStream,
    isAudioStreamValid,
    updateAudioStream,
    isAudioStreamProcessed,
    playAudioStream,
    pauseAudioStream,
    resumeAudioStream,
    isAudioStreamPlaying,
    stopAudioStream,
    setAudioStreamVolume,
    setAudioStreamPitch,
    setAudioStreamPan,
    setAudioStreamBufferSizeDefault,
    setAudioStreamCallback,
    attachAudioStreamProcessor,
    detachAudioStreamProcessor,
    attachAudioMixedProcessor,
    detachAudioMixedProcessor,

    -- * Native
    c'initAudioDevice,
    c'closeAudioDevice,
    c'isAudioDeviceReady,
    c'setMasterVolume,
    c'getMasterVolume,
    c'loadWave,
    c'loadWaveFromMemory,
    c'loadSound,
    c'loadSoundFromWave,
    c'loadSoundAlias,
    c'updateSound,
    c'isWaveValid,
    c'unloadWave,
    c'isSoundValid,
    c'unloadSound,
    c'unloadSoundAlias,
    c'exportWave,
    c'exportWaveAsCode,
    c'playSound,
    c'stopSound,
    c'pauseSound,
    c'resumeSound,
    c'isSoundPlaying,
    c'setSoundVolume,
    c'setSoundPitch,
    c'setSoundPan,
    c'waveCopy,
    c'waveCrop,
    c'waveFormat,
    c'loadWaveSamples,
    c'unloadWaveSamples,
    c'loadMusicStream,
    c'loadMusicStreamFromMemory,
    c'isMusicValid,
    c'unloadMusicStream,
    c'playMusicStream,
    c'isMusicStreamPlaying,
    c'updateMusicStream,
    c'stopMusicStream,
    c'pauseMusicStream,
    c'resumeMusicStream,
    c'seekMusicStream,
    c'setMusicVolume,
    c'setMusicPitch,
    c'setMusicPan,
    c'getMusicTimeLength,
    c'getMusicTimePlayed,
    c'loadAudioStream,
    c'isAudioStreamValid,
    c'unloadAudioStream,
    c'updateAudioStream,
    c'isAudioStreamProcessed,
    c'playAudioStream,
    c'pauseAudioStream,
    c'resumeAudioStream,
    c'isAudioStreamPlaying,
    c'stopAudioStream,
    c'setAudioStreamVolume,
    c'setAudioStreamPitch,
    c'setAudioStreamPan,
    c'setAudioStreamBufferSizeDefault,
    c'setAudioStreamCallback,
    c'attachAudioStreamProcessor,
    c'detachAudioStreamProcessor,
    c'attachAudioMixedProcessor,
    c'detachAudioMixedProcessor,

    -- * Callbacks
    mk'audioCallback,
    createAudioCallback,
  )
where

import Foreign (Ptr, Storable (peek, sizeOf), castFunPtr, castPtr, toBool)
import Foreign.C
  ( CBool (..),
    CFloat (..),
    CInt (..),
    CString,
    CUChar (..),
    CUInt (..),
    withCString,
  )
import Raylib.Internal (WindowResources, addFunPtr, releaseAudioWindowResources, unloadSingleAudioBuffer, unloadSingleAudioBufferAlias, unloadSingleCtxDataPtr, unloadSingleFunPtr)
import Raylib.Internal.Foreign
  ( ALike (..),
    Mutable (peekMutated),
    PALike,
    PLike,
    StringLike,
    TLike (..),
  )
import Raylib.Internal.TH (genNative)
import Raylib.Types
  ( AudioCallback,
    AudioStream,
    C'AudioCallback,
    Music,
    Sound,
    Wave,
    p'audioStream'buffer,
    p'music'ctxData,
    p'music'ctxType,
    p'sound'stream,
    p'wave'channels,
    p'wave'frameCount,
  )

$( genNative
     [ ("c'initAudioDevice", "InitAudioDevice_", "rl_bindings.h", [t|IO ()|]),
       ("c'closeAudioDevice", "CloseAudioDevice_", "rl_bindings.h", [t|IO ()|]),
       ("c'isAudioDeviceReady", "IsAudioDeviceReady_", "rl_bindings.h", [t|IO CBool|]),
       ("c'setMasterVolume", "SetMasterVolume_", "rl_bindings.h", [t|CFloat -> IO ()|]),
       ("c'getMasterVolume", "GetMasterVolume_", "rl_bindings.h", [t|IO CFloat|]),
       ("c'loadWave", "LoadWave_", "rl_bindings.h", [t|CString -> IO (Ptr Wave)|]),
       ("c'loadWaveFromMemory", "LoadWaveFromMemory_", "rl_bindings.h", [t|CString -> Ptr CUChar -> CInt -> IO (Ptr Wave)|]),
       ("c'loadSound", "LoadSound_", "rl_bindings.h", [t|CString -> IO (Ptr Sound)|]),
       ("c'loadSoundFromWave", "LoadSoundFromWave_", "rl_bindings.h", [t|Ptr Wave -> IO (Ptr Sound)|]),
       ("c'loadSoundAlias", "LoadSoundAlias_", "rl_bindings.h", [t|Ptr Sound -> IO (Ptr Sound)|]),
       ("c'updateSound", "UpdateSound_", "rl_bindings.h", [t|Ptr Sound -> Ptr () -> CInt -> IO ()|]),
       ("c'isWaveValid", "IsWaveValid_", "rl_bindings.h", [t|Ptr Wave -> IO CBool|]),
       ("c'unloadWave", "UnloadWave_", "rl_bindings.h", [t|Ptr Wave -> IO ()|]),
       ("c'isSoundValid", "IsSoundValid_", "rl_bindings.h", [t|Ptr Sound -> IO CBool|]),
       ("c'unloadSound", "UnloadSound_", "rl_bindings.h", [t|Ptr Sound -> IO ()|]),
       ("c'unloadSoundAlias", "UnloadSoundAlias_", "rl_bindings.h", [t|Ptr Sound -> IO ()|]),
       ("c'exportWave", "ExportWave_", "rl_bindings.h", [t|Ptr Wave -> CString -> IO CBool|]),
       ("c'exportWaveAsCode", "ExportWaveAsCode_", "rl_bindings.h", [t|Ptr Wave -> CString -> IO CBool|]),
       ("c'playSound", "PlaySound_", "rl_bindings.h", [t|Ptr Sound -> IO ()|]),
       ("c'stopSound", "StopSound_", "rl_bindings.h", [t|Ptr Sound -> IO ()|]),
       ("c'pauseSound", "PauseSound_", "rl_bindings.h", [t|Ptr Sound -> IO ()|]),
       ("c'resumeSound", "ResumeSound_", "rl_bindings.h", [t|Ptr Sound -> IO ()|]),
       ("c'isSoundPlaying", "IsSoundPlaying_", "rl_bindings.h", [t|Ptr Sound -> IO CBool|]),
       ("c'setSoundVolume", "SetSoundVolume_", "rl_bindings.h", [t|Ptr Sound -> CFloat -> IO ()|]),
       ("c'setSoundPitch", "SetSoundPitch_", "rl_bindings.h", [t|Ptr Sound -> CFloat -> IO ()|]),
       ("c'setSoundPan", "SetSoundPan_", "rl_bindings.h", [t|Ptr Sound -> CFloat -> IO ()|]),
       ("c'waveCopy", "WaveCopy_", "rl_bindings.h", [t|Ptr Wave -> IO (Ptr Wave)|]),
       ("c'waveCrop", "WaveCrop_", "rl_bindings.h", [t|Ptr Wave -> CInt -> CInt -> IO ()|]),
       ("c'waveFormat", "WaveFormat_", "rl_bindings.h", [t|Ptr Wave -> CInt -> CInt -> CInt -> IO ()|]),
       ("c'loadWaveSamples", "LoadWaveSamples_", "rl_bindings.h", [t|Ptr Wave -> IO (Ptr CFloat)|]),
       ("c'unloadWaveSamples", "UnloadWaveSamples_", "rl_bindings.h", [t|Ptr CFloat -> IO ()|]),
       ("c'loadMusicStream", "LoadMusicStream_", "rl_bindings.h", [t|CString -> IO (Ptr Music)|]),
       ("c'loadMusicStreamFromMemory", "LoadMusicStreamFromMemory_", "rl_bindings.h", [t|CString -> Ptr CUChar -> CInt -> IO (Ptr Music)|]),
       ("c'isMusicValid", "IsMusicValid_", "rl_bindings.h", [t|Ptr Music -> IO CBool|]),
       ("c'unloadMusicStream", "UnloadMusicStream_", "rl_bindings.h", [t|Ptr Music -> IO ()|]),
       ("c'playMusicStream", "PlayMusicStream_", "rl_bindings.h", [t|Ptr Music -> IO ()|]),
       ("c'isMusicStreamPlaying", "IsMusicStreamPlaying_", "rl_bindings.h", [t|Ptr Music -> IO CBool|]),
       ("c'updateMusicStream", "UpdateMusicStream_", "rl_bindings.h", [t|Ptr Music -> IO ()|]),
       ("c'stopMusicStream", "StopMusicStream_", "rl_bindings.h", [t|Ptr Music -> IO ()|]),
       ("c'pauseMusicStream", "PauseMusicStream_", "rl_bindings.h", [t|Ptr Music -> IO ()|]),
       ("c'resumeMusicStream", "ResumeMusicStream_", "rl_bindings.h", [t|Ptr Music -> IO ()|]),
       ("c'seekMusicStream", "SeekMusicStream_", "rl_bindings.h", [t|Ptr Music -> CFloat -> IO ()|]),
       ("c'setMusicVolume", "SetMusicVolume_", "rl_bindings.h", [t|Ptr Music -> CFloat -> IO ()|]),
       ("c'setMusicPitch", "SetMusicPitch_", "rl_bindings.h", [t|Ptr Music -> CFloat -> IO ()|]),
       ("c'setMusicPan", "SetMusicPan_", "rl_bindings.h", [t|Ptr Music -> CFloat -> IO ()|]),
       ("c'getMusicTimeLength", "GetMusicTimeLength_", "rl_bindings.h", [t|Ptr Music -> IO CFloat|]),
       ("c'getMusicTimePlayed", "GetMusicTimePlayed_", "rl_bindings.h", [t|Ptr Music -> IO CFloat|]),
       ("c'loadAudioStream", "LoadAudioStream_", "rl_bindings.h", [t|CUInt -> CUInt -> CUInt -> IO (Ptr AudioStream)|]),
       ("c'isAudioStreamValid", "IsAudioStreamValid_", "rl_bindings.h", [t|Ptr AudioStream -> IO CBool|]),
       ("c'unloadAudioStream", "UnloadAudioStream_", "rl_bindings.h", [t|Ptr AudioStream -> IO ()|]),
       ("c'updateAudioStream", "UpdateAudioStream_", "rl_bindings.h", [t|Ptr AudioStream -> Ptr () -> CInt -> IO ()|]),
       ("c'isAudioStreamProcessed", "IsAudioStreamProcessed_", "rl_bindings.h", [t|Ptr AudioStream -> IO CBool|]),
       ("c'playAudioStream", "PlayAudioStream_", "rl_bindings.h", [t|Ptr AudioStream -> IO ()|]),
       ("c'pauseAudioStream", "PauseAudioStream_", "rl_bindings.h", [t|Ptr AudioStream -> IO ()|]),
       ("c'resumeAudioStream", "ResumeAudioStream_", "rl_bindings.h", [t|Ptr AudioStream -> IO ()|]),
       ("c'isAudioStreamPlaying", "IsAudioStreamPlaying_", "rl_bindings.h", [t|Ptr AudioStream -> IO CBool|]),
       ("c'stopAudioStream", "StopAudioStream_", "rl_bindings.h", [t|Ptr AudioStream -> IO ()|]),
       ("c'setAudioStreamVolume", "SetAudioStreamVolume_", "rl_bindings.h", [t|Ptr AudioStream -> CFloat -> IO ()|]),
       ("c'setAudioStreamPitch", "SetAudioStreamPitch_", "rl_bindings.h", [t|Ptr AudioStream -> CFloat -> IO ()|]),
       ("c'setAudioStreamPan", "SetAudioStreamPan_", "rl_bindings.h", [t|Ptr AudioStream -> CFloat -> IO ()|]),
       ("c'setAudioStreamBufferSizeDefault", "SetAudioStreamBufferSizeDefault_", "rl_bindings.h", [t|CInt -> IO ()|]),
       ("c'setAudioStreamCallback", "SetAudioStreamCallback_", "rl_bindings.h", [t|Ptr AudioStream -> C'AudioCallback -> IO ()|]),
       ("c'attachAudioStreamProcessor", "AttachAudioStreamProcessor_", "rl_bindings.h", [t|Ptr AudioStream -> C'AudioCallback -> IO ()|]),
       ("c'detachAudioStreamProcessor", "DetachAudioStreamProcessor_", "rl_bindings.h", [t|Ptr AudioStream -> C'AudioCallback -> IO ()|]),
       ("c'attachAudioMixedProcessor", "AttachAudioMixedProcessor_", "rl_bindings.h", [t|C'AudioCallback -> IO ()|]),
       ("c'detachAudioMixedProcessor", "DetachAudioMixedProcessor_", "rl_bindings.h", [t|C'AudioCallback -> IO ()|])
     ]
 )

initAudioDevice :: IO ()
initAudioDevice = c'initAudioDevice

closeAudioDevice :: Maybe WindowResources -> IO ()
closeAudioDevice wr = do
  mapM_ releaseAudioWindowResources wr
  c'closeAudioDevice

isAudioDeviceReady :: IO Bool
isAudioDeviceReady = toBool <$> c'isAudioDeviceReady

setMasterVolume :: Float -> IO ()
setMasterVolume volume = c'setMasterVolume (realToFrac volume)

getMasterVolume :: IO Float
getMasterVolume = realToFrac <$> c'getMasterVolume

loadWave :: (StringLike string, PLike Wave wave) => string -> IO wave
loadWave fileName = withTLike fileName c'loadWave >>= popTLike

loadWaveFromMemory :: (PALike CUChar contents, PLike Wave wave) => String -> contents -> IO wave
loadWaveFromMemory fileType fileData = withCString fileType (\f -> withALikeLen fileData (\size d -> c'loadWaveFromMemory f d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= popTLike

loadSound :: (StringLike string, PLike Sound sound) => string -> IO sound
loadSound fileName = withTLike fileName c'loadSound >>= popTLike

loadSoundFromWave :: (PLike Wave wave, PLike Sound sound) => wave -> IO sound
loadSoundFromWave wave = withTLike wave c'loadSoundFromWave >>= popTLike

loadSoundAlias :: (PLike Sound sound1, PLike Sound sound2) => sound1 -> IO sound2
loadSoundAlias source = withTLike source c'loadSoundAlias >>= popTLike

-- | Unloads a `managed` sound alias from RAM
unloadSoundAlias :: (PLike Sound sound) => sound -> WindowResources -> IO ()
unloadSoundAlias sound wr =
  withTLike
    sound
    ( \soundPtr -> do
        buf <- castPtr <$> peek (p'audioStream'buffer (p'sound'stream soundPtr))
        unloadSingleAudioBufferAlias buf wr
    )

updateSound :: (PLike Sound sound) => sound -> Ptr () -> Int -> IO ()
updateSound sound dataValue sampleCount = withTLike sound (\s -> c'updateSound s dataValue (fromIntegral sampleCount))

-- | Unloads a `managed` sound from RAM
unloadSound :: (PLike Sound sound) => sound -> WindowResources -> IO ()
unloadSound sound wr =
  withTLike
    sound
    ( \soundPtr -> do
        stream <- peek (p'sound'stream soundPtr)
        unloadAudioStream stream wr
    )

isWaveValid :: (PLike Wave wave) => wave -> IO Bool
isWaveValid wave = toBool <$> withTLike wave c'isWaveValid

isSoundValid :: (PLike Sound sound) => sound -> IO Bool
isSoundValid sound = toBool <$> withTLike sound c'isSoundValid

exportWave :: (PLike Wave wave, StringLike string) => wave -> string -> IO Bool
exportWave wave fileName = toBool <$> withTLike wave (withTLike fileName . c'exportWave)

exportWaveAsCode :: (PLike Wave wave, StringLike string) => wave -> string -> IO Bool
exportWaveAsCode wave fileName = toBool <$> withTLike wave (withTLike fileName . c'exportWaveAsCode)

playSound :: (PLike Sound sound) => sound -> IO ()
playSound sound = withTLike sound c'playSound

stopSound :: (PLike Sound sound) => sound -> IO ()
stopSound sound = withTLike sound c'stopSound

pauseSound :: (PLike Sound sound) => sound -> IO ()
pauseSound sound = withTLike sound c'pauseSound

resumeSound :: (PLike Sound sound) => sound -> IO ()
resumeSound sound = withTLike sound c'resumeSound

isSoundPlaying :: (PLike Sound sound) => sound -> IO Bool
isSoundPlaying sound = toBool <$> withTLike sound c'isSoundPlaying

setSoundVolume :: (PLike Sound sound) => sound -> Float -> IO ()
setSoundVolume sound volume = withTLike sound (\s -> c'setSoundVolume s (realToFrac volume))

setSoundPitch :: (PLike Sound sound) => sound -> Float -> IO ()
setSoundPitch sound pitch = withTLike sound (\s -> c'setSoundPitch s (realToFrac pitch))

setSoundPan :: (PLike Sound sound) => sound -> Float -> IO ()
setSoundPan sound pan = withTLike sound (\s -> c'setSoundPan s (realToFrac pan))

waveCopy :: (PLike Wave wave) => wave -> IO wave
waveCopy wave = withTLike wave c'waveCopy >>= popTLike

waveCrop :: (PLike Wave wave, Mutable wave mut) => wave -> Int -> Int -> IO mut
waveCrop wave initFrame finalFrame = withTLike wave (\w -> c'waveCrop w (fromIntegral initFrame) (fromIntegral finalFrame) >> peekMutated wave w)

waveFormat :: (PLike Wave wave, Mutable wave mut) => wave -> Int -> Int -> Int -> IO mut
waveFormat wave sampleRate sampleSize channels = withTLike wave (\w -> c'waveFormat w (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels) >> peekMutated wave w)

loadWaveSamples :: (PLike Wave wave, PALike CFloat samples) => wave -> IO samples
loadWaveSamples wave =
  withTLike
    wave
    ( \wavePtr -> do
        fc <- peek (p'wave'frameCount wavePtr)
        c <- peek (p'wave'channels wavePtr)
        popALike (fromIntegral $ fc * c) =<< c'loadWaveSamples wavePtr
    )

loadMusicStream :: (StringLike string, PLike Music music) => string -> IO music
loadMusicStream fileName = withTLike fileName c'loadMusicStream >>= popTLike

loadMusicStreamFromMemory :: (PALike CUChar contents, PLike Music music) => String -> contents -> IO music
loadMusicStreamFromMemory fileType streamData =
  withCString fileType (\t -> withALikeLen streamData (\size d -> c'loadMusicStreamFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= popTLike

-- | Unloads a `managed` music stream from RAM
unloadMusicStream :: (PLike Music music) => music -> WindowResources -> IO ()
unloadMusicStream music wr =
  withTLike
    music
    ( \musicPtr -> do
        ct <- peek (p'music'ctxType musicPtr)
        cd <- peek (p'music'ctxData musicPtr)
        unloadSingleCtxDataPtr (fromEnum ct) cd wr
    )

isMusicValid :: (PLike Music music) => music -> IO Bool
isMusicValid music = toBool <$> withTLike music c'isMusicValid

playMusicStream :: (PLike Music music) => music -> IO ()
playMusicStream music = withTLike music c'playMusicStream

isMusicStreamPlaying :: (PLike Music music) => music -> IO Bool
isMusicStreamPlaying music = toBool <$> withTLike music c'isMusicStreamPlaying

updateMusicStream :: (PLike Music music) => music -> IO ()
updateMusicStream music = withTLike music c'updateMusicStream

stopMusicStream :: (PLike Music music) => music -> IO ()
stopMusicStream music = withTLike music c'stopMusicStream

pauseMusicStream :: (PLike Music music) => music -> IO ()
pauseMusicStream music = withTLike music c'pauseMusicStream

resumeMusicStream :: (PLike Music music) => music -> IO ()
resumeMusicStream music = withTLike music c'resumeMusicStream

seekMusicStream :: (PLike Music music) => music -> Float -> IO ()
seekMusicStream music position = withTLike music (\m -> c'seekMusicStream m (realToFrac position))

setMusicVolume :: (PLike Music music) => music -> Float -> IO ()
setMusicVolume music volume = withTLike music (\m -> c'setMusicVolume m (realToFrac volume))

setMusicPitch :: (PLike Music music) => music -> Float -> IO ()
setMusicPitch music pitch = withTLike music (\m -> c'setMusicPitch m (realToFrac pitch))

setMusicPan :: (PLike Music music) => music -> Float -> IO ()
setMusicPan music pan = withTLike music (\m -> c'setMusicPan m (realToFrac pan))

getMusicTimeLength :: (PLike Music music) => music -> IO Float
getMusicTimeLength music = realToFrac <$> withTLike music c'getMusicTimeLength

getMusicTimePlayed :: (PLike Music music) => music -> IO Float
getMusicTimePlayed music = realToFrac <$> withTLike music c'getMusicTimePlayed

loadAudioStream :: (PLike AudioStream audioStream) => Integer -> Integer -> Integer -> IO audioStream
loadAudioStream sampleRate sampleSize channels = c'loadAudioStream (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels) >>= popTLike

-- | Unloads a `managed` audio stream from RAM
unloadAudioStream :: (PLike AudioStream audioStream) => audioStream -> WindowResources -> IO ()
unloadAudioStream stream wr =
  withTLike
    stream
    ( \streamPtr -> do
        buf <- peek (p'audioStream'buffer streamPtr)
        unloadSingleAudioBuffer (castPtr buf) wr
    )

isAudioStreamValid :: (PLike AudioStream audioStream) => audioStream -> IO Bool
isAudioStreamValid stream = toBool <$> withTLike stream c'isAudioStreamValid

updateAudioStream :: (PLike AudioStream audioStream) => audioStream -> Ptr () -> Int -> IO ()
updateAudioStream stream value frameCount = withTLike stream (\s -> c'updateAudioStream s value (fromIntegral frameCount))

isAudioStreamProcessed :: (PLike AudioStream audioStream) => audioStream -> IO Bool
isAudioStreamProcessed stream = toBool <$> withTLike stream c'isAudioStreamProcessed

playAudioStream :: (PLike AudioStream audioStream) => audioStream -> IO ()
playAudioStream stream = withTLike stream c'playAudioStream

pauseAudioStream :: (PLike AudioStream audioStream) => audioStream -> IO ()
pauseAudioStream stream = withTLike stream c'pauseAudioStream

resumeAudioStream :: (PLike AudioStream audioStream) => audioStream -> IO ()
resumeAudioStream stream = withTLike stream c'resumeAudioStream

isAudioStreamPlaying :: (PLike AudioStream audioStream) => audioStream -> IO Bool
isAudioStreamPlaying stream = toBool <$> withTLike stream c'isAudioStreamPlaying

stopAudioStream :: (PLike AudioStream audioStream) => audioStream -> IO ()
stopAudioStream stream = withTLike stream c'stopAudioStream

setAudioStreamVolume :: (PLike AudioStream audioStream) => audioStream -> Float -> IO ()
setAudioStreamVolume stream volume = withTLike stream (\s -> c'setAudioStreamVolume s (realToFrac volume))

setAudioStreamPitch :: (PLike AudioStream audioStream) => audioStream -> Float -> IO ()
setAudioStreamPitch stream pitch = withTLike stream (\s -> c'setAudioStreamPitch s (realToFrac pitch))

setAudioStreamPan :: (PLike AudioStream audioStream) => audioStream -> Float -> IO ()
setAudioStreamPan stream pan = withTLike stream (\s -> c'setAudioStreamPan s (realToFrac pan))

setAudioStreamBufferSizeDefault :: Int -> IO ()
setAudioStreamBufferSizeDefault = c'setAudioStreamBufferSizeDefault . fromIntegral

setAudioStreamCallback :: (PLike AudioStream audioStream) => audioStream -> AudioCallback -> WindowResources -> IO C'AudioCallback
setAudioStreamCallback stream callback window =
  withTLike
    stream
    ( \s ->
        do
          c <- createAudioCallback callback
          addFunPtr (castFunPtr c) window
          c'setAudioStreamCallback s c
          return c
    )

attachAudioStreamProcessor :: (PLike AudioStream audioStream) => audioStream -> AudioCallback -> WindowResources -> IO C'AudioCallback
attachAudioStreamProcessor stream callback window =
  withTLike
    stream
    ( \s ->
        do
          c <- createAudioCallback callback
          addFunPtr (castFunPtr c) window
          c'attachAudioStreamProcessor s c
          return c
    )

detachAudioStreamProcessor :: (PLike AudioStream audioStream) => audioStream -> C'AudioCallback -> WindowResources -> IO ()
detachAudioStreamProcessor stream callback window =
  withTLike stream (`c'detachAudioStreamProcessor` callback) >> unloadSingleFunPtr (castFunPtr callback) window

attachAudioMixedProcessor :: AudioCallback -> WindowResources -> IO C'AudioCallback
attachAudioMixedProcessor callback window =
  do
    c <- createAudioCallback callback
    addFunPtr (castFunPtr c) window
    c'attachAudioMixedProcessor c
    return c

detachAudioMixedProcessor :: C'AudioCallback -> WindowResources -> IO ()
detachAudioMixedProcessor callback window = c'detachAudioMixedProcessor callback >> unloadSingleFunPtr (castFunPtr callback) window

foreign import ccall unsafe "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO C'AudioCallback

-- foreign import ccall unsafe "dynamic"
--   mK'audioCallback ::
--     C'AudioCallback -> (Ptr () -> CUInt -> IO ())

createAudioCallback :: AudioCallback -> IO C'AudioCallback
createAudioCallback callback =
  mk'audioCallback
    (\bufferData frames -> callback bufferData (fromIntegral frames))
