{-# LANGUAGE DeriveAnyClass #-}

-- | Bindings for types used in @raudio@
module Raylib.Types.Core.Audio
  ( -- * Enumerations
    MusicContextType (..),
    AudioBufferUsage (..),

    -- * Structures
    Wave (..),
    RAudioBuffer (..),
    RAudioProcessor (..),
    AudioStream (..),
    Sound (..),
    Music (..),

    -- * Pointer utilities
    p'wave'frameCount,
    p'wave'sampleRate,
    p'wave'sampleSize,
    p'wave'channels,
    p'wave'data,
    p'rAudioBuffer'converter,
    p'rAudioBuffer'callback,
    p'rAudioBuffer'processor,
    p'rAudioBuffer'volume,
    p'rAudioBuffer'pitch,
    p'rAudioBuffer'pan,
    p'rAudioBuffer'playing,
    p'rAudioBuffer'paused,
    p'rAudioBuffer'looping,
    p'rAudioBuffer'usage,
    p'rAudioBuffer'isSubBufferProcessed,
    p'rAudioBuffer'sizeInFrames,
    p'rAudioBuffer'frameCursorPos,
    p'rAudioBuffer'framesProcessed,
    p'rAudioBuffer'data,
    p'rAudioBuffer'next,
    p'rAudioBuffer'prev,
    p'rAudioProcessor'process,
    p'rAudioProcessor'next,
    p'rAudioProcessor'prev,
    p'audioStream'buffer,
    p'audioStream'processor,
    p'audioStream'sampleRate,
    p'audioStream'sampleSize,
    p'audioStream'channels,
    p'sound'stream,
    p'sound'frameCount,
    p'music'stream,
    p'music'frameCount,
    p'music'looping,
    p'music'ctxType,
    p'music'ctxData,

    -- * Callbacks
    AudioCallback,
    C'AudioCallback,
  )
where

import Data.Maybe (fromMaybe)
import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, poke, sizeOf),
    Word8,
    castPtr,
    fromBool,
    malloc,
    newArray,
    nullFunPtr,
    nullPtr,
    peekArray,
    plusPtr,
    toBool,
  )
import Foreign.C
  ( CBool,
    CFloat,
    CInt (..),
    CShort,
    CUChar,
    CUInt,
  )
import Raylib.Internal (Closeable (..), addAudioBuffer, addCtxData, c'unloadAudioBuffer, c'unloadMusicStreamData)
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, peekMaybe, peekStaticArray, pokeMaybe, pokeStaticArray)

---------------------------------------
-- audio enums ------------------------
---------------------------------------

data MusicContextType
  = MusicAudioNone
  | MusicAudioWAV
  | MusicAudioOGG
  | MusicAudioFLAC
  | MusicAudioMP3
  | MusicAudioQOA
  | MusicModuleXM
  | MusicModuleMOD
  deriving (Eq, Show, Enum)

instance Storable MusicContextType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

data AudioBufferUsage
  = AudioBufferUsageStatic
  | AudioBufferUsageStream
  deriving (Eq, Show, Enum)

instance Storable AudioBufferUsage where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

---------------------------------------
-- audio structures -------------------
---------------------------------------

data Wave = Wave
  { wave'frameCount :: Integer,
    wave'sampleRate :: Integer,
    wave'sampleSize :: Integer,
    wave'channels :: Integer,
    wave'data :: [Int]
  }
  deriving (Eq, Show)

instance Storable Wave where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    frameCount <- fromIntegral <$> peek (p'wave'frameCount _p)
    sampleRate <- fromIntegral <$> peek (p'wave'sampleRate _p)
    sampleSize <- fromIntegral <$> peek (p'wave'sampleSize _p)
    channels <- fromIntegral <$> peek (p'wave'channels _p)
    wDataPtr <- peek (p'wave'data _p)
    wData <- map fromIntegral <$> peekArray (fromInteger $ frameCount * channels) wDataPtr
    return $ Wave frameCount sampleRate sampleSize channels wData
  poke _p (Wave frameCount sampleRate sampleSize channels wData) = do
    poke (p'wave'frameCount _p) (fromIntegral frameCount)
    poke (p'wave'sampleRate _p) (fromIntegral sampleRate)
    poke (p'wave'sampleSize _p) (fromIntegral sampleSize)
    poke (p'wave'channels _p) (fromIntegral channels)
    poke (p'wave'data _p) =<< newArray (map fromIntegral wData)
    return ()

p'wave'frameCount :: Ptr Wave -> Ptr CUInt
p'wave'frameCount = (`plusPtr` 0)

p'wave'sampleRate :: Ptr Wave -> Ptr CUInt
p'wave'sampleRate = (`plusPtr` 4)

p'wave'sampleSize :: Ptr Wave -> Ptr CUInt
p'wave'sampleSize = (`plusPtr` 8)

p'wave'channels :: Ptr Wave -> Ptr CUInt
p'wave'channels = (`plusPtr` 12)

-- array (wave'frameCount *  wave'channels)
p'wave'data :: Ptr Wave -> Ptr (Ptr CShort)
p'wave'data = (`plusPtr` 16)

instance Freeable Wave where
  rlFreeDependents _ ptr = do
    dataPtr <- peek (p'wave'data ptr)
    c'free $ castPtr dataPtr

-- RAudioBuffer/Processor are bound weirdly. They are currently used as `Ptr`s
-- because peeking/poking them every time an audio function is called doesn't
-- work properly (they are stored in a linked list in C, which makes it very
-- difficult to properly marshal them).
--
-- The types defined here are actually unnecessary because the pointers are
-- never dereferenced.
data RAudioBuffer = RAudioBuffer
  { rAudioBuffer'converter :: [Int], -- Implemented as an array of 78 integers because the entire `ma_data_converter` type is too complex
    rAudioBuffer'callback :: Maybe C'AudioCallback,
    rAudioBuffer'processor :: Maybe RAudioProcessor,
    rAudioBuffer'volume :: Float,
    rAudioBuffer'pitch :: Float,
    rAudioBuffer'pan :: Float,
    rAudioBuffer'playing :: Bool,
    rAudioBuffer'paused :: Bool,
    rAudioBuffer'looping :: Bool,
    rAudioBuffer'usage :: AudioBufferUsage,
    rAudioBuffer'isSubBufferProcessed :: [Bool],
    rAudioBuffer'sizeInFrames :: Integer,
    rAudioBuffer'frameCursorPos :: Integer,
    rAudioBuffer'framesProcessed :: Integer,
    rAudioBuffer'data :: [Word8],
    rAudioBuffer'next :: Maybe RAudioBuffer,
    rAudioBuffer'prev :: Maybe RAudioBuffer
  }
  deriving (Eq, Show, Freeable)

instance Storable RAudioBuffer where
  sizeOf _ = 392
  alignment _ = 8
  peek _p = do
    base <- loadBase _p
    nextPtr <- peek (p'rAudioBuffer'next _p)
    next <- loadNext nextPtr
    prevPtr <- peek (p'rAudioBuffer'prev _p)
    prev <- loadPrev prevPtr
    return $
      let p =
            base
              ((\a -> a {rAudioBuffer'prev = Just p}) <$> next)
              ((\a -> a {rAudioBuffer'next = Just p}) <$> prev)
       in p
    where
      getBytesPerSample = ([0, 1, 2, 3, 4, 4] !!)
      loadBase ptr = do
        converter <- map fromIntegral <$> peekStaticArray 78 (castPtr (p'rAudioBuffer'converter ptr) :: Ptr CInt)
        let formatIn =
              case converter of
                [] -> error "invalid miniaudio converter"
                x : _ -> x
        funPtr <- peek (p'rAudioBuffer'callback ptr)
        let callback = if funPtr == nullFunPtr then Nothing else Just funPtr
        processor <- peekMaybe (p'rAudioBuffer'processor ptr)

        volume <- realToFrac <$> peek (p'rAudioBuffer'volume ptr)
        pitch <- realToFrac <$> peek (p'rAudioBuffer'pitch ptr)
        pan <- realToFrac <$> peek (p'rAudioBuffer'pan ptr)

        playing <- toBool <$> peek (p'rAudioBuffer'playing ptr)
        paused <- toBool <$> peek (p'rAudioBuffer'paused ptr)
        looping <- toBool <$> peek (p'rAudioBuffer'looping ptr)
        usage <- peek (p'rAudioBuffer'usage ptr)

        isSubBufferProcessed <- map toBool <$> peekStaticArray 2 (p'rAudioBuffer'isSubBufferProcessed ptr)
        sizeInFrames <- fromIntegral <$> peek (p'rAudioBuffer'sizeInFrames ptr)
        frameCursorPos <- fromIntegral <$> peek (p'rAudioBuffer'frameCursorPos ptr)
        framesProcessed <- fromIntegral <$> peek (p'rAudioBuffer'framesProcessed ptr)

        bData <- map fromIntegral <$> (peekArray (fromIntegral $ sizeInFrames * 2 * getBytesPerSample formatIn) =<< peek (p'rAudioBuffer'data ptr))

        return $ RAudioBuffer converter callback processor volume pitch pan playing paused looping usage isSubBufferProcessed sizeInFrames frameCursorPos framesProcessed bData
      loadNext ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            base <- loadBase ptr
            nextPtr <- peek (p'rAudioBuffer'next ptr)
            next <- loadNext nextPtr
            let p = base ((\a -> a {rAudioBuffer'prev = Just p}) <$> next) Nothing
             in return (Just p)

      loadPrev ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            base <- loadBase ptr
            prevPtr <- peek (p'rAudioBuffer'prev ptr)
            prev <- loadPrev prevPtr
            let p = base Nothing ((\a -> a {rAudioBuffer'next = Just p}) <$> prev)
             in return (Just p)
  poke _p a = do
    pokeBase _p a
    pokeNext _p $ rAudioBuffer'next a
    pokePrev _p $ rAudioBuffer'prev a
    return ()
    where
      pokeBase ptr (RAudioBuffer converter callback processor volume pitch pan playing paused looping usage isSubBufferProcessed sizeInFrames frameCursorPos framesProcessed bData _ _) = do
        pokeStaticArray (castPtr (p'rAudioBuffer'converter ptr) :: Ptr CInt) (map fromIntegral converter)
        poke (p'rAudioBuffer'callback ptr) (fromMaybe nullFunPtr callback)
        pokeMaybe (p'rAudioBuffer'processor ptr) processor

        poke (p'rAudioBuffer'volume ptr) (realToFrac volume)
        poke (p'rAudioBuffer'pitch ptr) (realToFrac pitch)
        poke (p'rAudioBuffer'pan ptr) (realToFrac pan)

        poke (p'rAudioBuffer'playing ptr) (fromBool playing)
        poke (p'rAudioBuffer'paused ptr) (fromBool paused)
        poke (p'rAudioBuffer'looping ptr) (fromBool looping)
        poke (p'rAudioBuffer'usage ptr) usage

        pokeStaticArray (p'rAudioBuffer'isSubBufferProcessed ptr) (map fromBool isSubBufferProcessed)
        poke (p'rAudioBuffer'sizeInFrames ptr) (fromIntegral sizeInFrames)
        poke (p'rAudioBuffer'frameCursorPos ptr) (fromIntegral frameCursorPos)
        poke (p'rAudioBuffer'framesProcessed ptr) (fromIntegral framesProcessed)

        poke (p'rAudioBuffer'data ptr) =<< newArray (map fromIntegral bData :: [CUChar])

        return ()
      pokeNext basePtr pNext =
        case pNext of
          Nothing -> poke (p'rAudioBuffer'next basePtr) nullPtr
          Just val -> do
            nextPtr <- malloc
            pokeBase nextPtr val
            pokeNext nextPtr (rAudioBuffer'next val)
            poke (p'rAudioBuffer'prev nextPtr) basePtr
            poke (p'rAudioBuffer'next basePtr) nextPtr
      pokePrev basePtr pPrev =
        case pPrev of
          Nothing -> poke (p'rAudioBuffer'prev basePtr) nullPtr
          Just val -> do
            prevPtr <- malloc
            pokeBase prevPtr val
            poke (p'rAudioBuffer'next prevPtr) basePtr
            pokePrev prevPtr (rAudioBuffer'prev val)
            poke (p'rAudioBuffer'prev basePtr) prevPtr

-- bytes (312)
p'rAudioBuffer'converter :: Ptr RAudioBuffer -> Ptr ()
p'rAudioBuffer'converter = (`plusPtr` 0)

-- maybe funptr
p'rAudioBuffer'callback :: Ptr RAudioBuffer -> Ptr C'AudioCallback
p'rAudioBuffer'callback = (`plusPtr` 312)

-- maybe
p'rAudioBuffer'processor :: Ptr RAudioBuffer -> Ptr (Ptr RAudioProcessor)
p'rAudioBuffer'processor = (`plusPtr` 320)

p'rAudioBuffer'volume :: Ptr RAudioBuffer -> Ptr CFloat
p'rAudioBuffer'volume = (`plusPtr` 328)

p'rAudioBuffer'pitch :: Ptr RAudioBuffer -> Ptr CFloat
p'rAudioBuffer'pitch = (`plusPtr` 332)

p'rAudioBuffer'pan :: Ptr RAudioBuffer -> Ptr CFloat
p'rAudioBuffer'pan = (`plusPtr` 336)

p'rAudioBuffer'playing :: Ptr RAudioBuffer -> Ptr CBool
p'rAudioBuffer'playing = (`plusPtr` 340)

p'rAudioBuffer'paused :: Ptr RAudioBuffer -> Ptr CBool
p'rAudioBuffer'paused = (`plusPtr` 341)

p'rAudioBuffer'looping :: Ptr RAudioBuffer -> Ptr CBool
p'rAudioBuffer'looping = (`plusPtr` 342)

p'rAudioBuffer'usage :: Ptr RAudioBuffer -> Ptr AudioBufferUsage
p'rAudioBuffer'usage = (`plusPtr` 344)

-- static array (2)
p'rAudioBuffer'isSubBufferProcessed :: Ptr RAudioBuffer -> Ptr CBool
p'rAudioBuffer'isSubBufferProcessed = (`plusPtr` 348)

p'rAudioBuffer'sizeInFrames :: Ptr RAudioBuffer -> Ptr CUInt
p'rAudioBuffer'sizeInFrames = (`plusPtr` 352)

p'rAudioBuffer'frameCursorPos :: Ptr RAudioBuffer -> Ptr CUInt
p'rAudioBuffer'frameCursorPos = (`plusPtr` 356)

p'rAudioBuffer'framesProcessed :: Ptr RAudioBuffer -> Ptr CUInt
p'rAudioBuffer'framesProcessed = (`plusPtr` 360)

-- array (rAudioBuffer'sizeInFrames * 2 * ([0, 1, 2, 3, 4, 4] !! (peek (rAudioBuffer'converter :: Ptr CInt))))
p'rAudioBuffer'data :: Ptr RAudioBuffer -> Ptr (Ptr CUChar)
p'rAudioBuffer'data = (`plusPtr` 368)

-- maybe
p'rAudioBuffer'next :: Ptr RAudioBuffer -> Ptr (Ptr RAudioBuffer)
p'rAudioBuffer'next = (`plusPtr` 376)

-- maybe
p'rAudioBuffer'prev :: Ptr RAudioBuffer -> Ptr (Ptr RAudioBuffer)
p'rAudioBuffer'prev = (`plusPtr` 384)

data RAudioProcessor = RAudioProcessor
  { rAudioProcessor'process :: Maybe C'AudioCallback,
    rAudioProcessor'next :: Maybe RAudioProcessor,
    rAudioProcessor'prev :: Maybe RAudioProcessor
  }
  deriving (Eq, Show, Freeable)

instance Storable RAudioProcessor where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    process <- loadProcess _p
    nextPtr <- peek (p'rAudioProcessor'next _p)
    next <- loadNext nextPtr
    prevPtr <- peek (p'rAudioProcessor'prev _p)
    prev <- loadPrev prevPtr
    return $ let p = RAudioProcessor process ((\a -> a {rAudioProcessor'prev = Just p}) <$> next) ((\a -> a {rAudioProcessor'next = Just p}) <$> prev) in p
    where
      loadProcess ptr = do
        funPtr <- peek (p'rAudioProcessor'process ptr)
        if funPtr == nullFunPtr then return Nothing else return (Just funPtr)
      loadNext ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            process <- loadProcess ptr
            nextPtr <- peek (p'rAudioProcessor'next ptr)
            next <- loadNext nextPtr
            let p = RAudioProcessor process ((\a -> a {rAudioProcessor'prev = Just p}) <$> next) Nothing
             in return (Just p)

      loadPrev ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            process <- loadProcess ptr
            prevPtr <- peek (p'rAudioProcessor'prev ptr)
            prev <- loadPrev prevPtr
            let p = RAudioProcessor process Nothing ((\a -> a {rAudioProcessor'next = Just p}) <$> prev)
             in return (Just p)
  poke _p (RAudioProcessor process next prev) = do
    poke (p'rAudioProcessor'process _p) (fromMaybe nullFunPtr process)
    pokeNext _p next
    pokePrev (castPtr _p) prev
    return ()
    where
      pokeNext basePtr pNext =
        case pNext of
          Nothing -> poke (p'rAudioProcessor'next basePtr) nullPtr
          Just val -> do
            nextPtr <- malloc
            poke (p'rAudioProcessor'process nextPtr) (fromMaybe nullFunPtr (rAudioProcessor'process val))
            pokeNext nextPtr (rAudioProcessor'next val)
            poke (p'rAudioProcessor'prev nextPtr) basePtr
            poke (p'rAudioProcessor'next basePtr) nextPtr
      pokePrev basePtr pPrev =
        case pPrev of
          Nothing -> poke (p'rAudioProcessor'prev basePtr) nullPtr
          Just val -> do
            prevPtr <- malloc
            poke (p'rAudioProcessor'process prevPtr) (fromMaybe nullFunPtr (rAudioProcessor'process val))
            poke (p'rAudioProcessor'next prevPtr) basePtr
            pokePrev prevPtr (rAudioProcessor'prev val)
            poke (p'rAudioProcessor'prev basePtr) prevPtr

-- maybe funptr
p'rAudioProcessor'process :: Ptr RAudioProcessor -> Ptr C'AudioCallback
p'rAudioProcessor'process = (`plusPtr` 0)

-- maybe
p'rAudioProcessor'next :: Ptr RAudioProcessor -> Ptr (Ptr RAudioProcessor)
p'rAudioProcessor'next = (`plusPtr` 8)

-- maybe
p'rAudioProcessor'prev :: Ptr RAudioProcessor -> Ptr (Ptr RAudioProcessor)
p'rAudioProcessor'prev = (`plusPtr` 16)

data AudioStream = AudioStream
  { audioStream'buffer :: Ptr RAudioBuffer,
    audioStream'processor :: Ptr RAudioProcessor,
    audioStream'sampleRate :: Integer,
    audioStream'sampleSize :: Integer,
    audioStream'channels :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable AudioStream where
  sizeOf _ = 32
  alignment _ = 8
  peek _p = do
    buffer <- peek (p'audioStream'buffer _p)
    processor <- peek (p'audioStream'processor _p)
    sampleRate <- fromIntegral <$> peek (p'audioStream'sampleRate _p)
    sampleSize <- fromIntegral <$> peek (p'audioStream'sampleSize _p)
    channels <- fromIntegral <$> peek (p'audioStream'channels _p)
    return $ AudioStream buffer processor sampleRate sampleSize channels
  poke _p (AudioStream buffer processor sampleRate sampleSize channels) = do
    poke (p'audioStream'buffer _p) buffer
    poke (p'audioStream'processor _p) processor
    poke (p'audioStream'sampleRate _p) (fromIntegral sampleRate)
    poke (p'audioStream'sampleSize _p) (fromIntegral sampleSize)
    poke (p'audioStream'channels _p) (fromIntegral channels)
    return ()

instance Closeable AudioStream where
  close stream = c'unloadAudioBuffer (castPtr (audioStream'buffer stream))
  addToWindowResources window stream = addAudioBuffer (castPtr (audioStream'buffer stream)) window

-- maybe
p'audioStream'buffer :: Ptr AudioStream -> Ptr (Ptr RAudioBuffer)
p'audioStream'buffer = (`plusPtr` 0)

-- maybe
p'audioStream'processor :: Ptr AudioStream -> Ptr (Ptr RAudioProcessor)
p'audioStream'processor = (`plusPtr` 8)

p'audioStream'sampleRate :: Ptr AudioStream -> Ptr CUInt
p'audioStream'sampleRate = (`plusPtr` 16)

p'audioStream'sampleSize :: Ptr AudioStream -> Ptr CUInt
p'audioStream'sampleSize = (`plusPtr` 20)

p'audioStream'channels :: Ptr AudioStream -> Ptr CUInt
p'audioStream'channels = (`plusPtr` 24)

data Sound = Sound
  { sound'stream :: AudioStream,
    sound'frameCount :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable Sound where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    stream <- peek (p'sound'stream _p)
    frameCount <- fromIntegral <$> peek (p'sound'frameCount _p)
    return $ Sound stream frameCount
  poke _p (Sound stream frameCount) = do
    poke (p'sound'stream _p) stream
    poke (p'sound'frameCount _p) (fromIntegral frameCount)
    return ()

instance Closeable Sound where
  close sound = close (sound'stream sound)
  addToWindowResources window = addToWindowResources window . sound'stream

p'sound'stream :: Ptr Sound -> Ptr AudioStream
p'sound'stream = (`plusPtr` 0)

p'sound'frameCount :: Ptr Sound -> Ptr CUInt
p'sound'frameCount = (`plusPtr` 32)

data Music = Music
  { music'stream :: AudioStream,
    music'frameCount :: Integer,
    music'looping :: Bool,
    music'ctxType :: MusicContextType,
    music'ctxData :: Ptr ()
  }
  deriving (Eq, Show, Freeable)

instance Storable Music where
  sizeOf _ = 56
  alignment _ = 4
  peek _p = do
    stream <- peek (p'music'stream _p)
    frameCount <- fromIntegral <$> peek (p'music'frameCount _p)
    looping <- toBool <$> peek (p'music'looping _p)
    ctxType <- peek (p'music'ctxType _p)
    ctxData <- peek (p'music'ctxData _p)
    return $ Music stream frameCount looping ctxType ctxData
  poke _p (Music stream frameCount looping ctxType ctxData) = do
    poke (p'music'stream _p) stream
    poke (p'music'frameCount _p) (fromIntegral frameCount)
    poke (p'music'looping _p) (fromBool looping)
    poke (p'music'ctxType _p) ctxType
    poke (p'music'ctxData _p) ctxData
    return ()

instance Closeable Music where
  close music = c'unloadMusicStreamData (fromIntegral (fromEnum (music'ctxType music))) (music'ctxData music)
  addToWindowResources window music = addCtxData (fromEnum $ music'ctxType music) (music'ctxData music) window

p'music'stream :: Ptr Music -> Ptr AudioStream
p'music'stream = (`plusPtr` 0)

p'music'frameCount :: Ptr Music -> Ptr CUInt
p'music'frameCount = (`plusPtr` 32)

p'music'looping :: Ptr Music -> Ptr CBool
p'music'looping = (`plusPtr` 36)

p'music'ctxType :: Ptr Music -> Ptr MusicContextType
p'music'ctxType = (`plusPtr` 40)

-- bytes (?)
p'music'ctxData :: Ptr Music -> Ptr (Ptr ())
p'music'ctxData = (`plusPtr` 48)

---------------------------------------
-- audio callbacks --------------------
---------------------------------------

type AudioCallback = Ptr () -> Integer -> IO ()

type C'AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())
