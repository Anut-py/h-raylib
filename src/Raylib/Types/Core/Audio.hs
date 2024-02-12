{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}

module Raylib.Types.Core.Audio
  ( MusicContextType (..),
    Wave (..),
    RAudioBuffer (..),
    RAudioProcessor (..),
    AudioStream (..),
    Sound (..),
    Music (..),
    AudioCallback,
    C'AudioCallback,
  )
where

import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    Word8,
    castPtr,
    fromBool,
    malloc,
    newArray,
    nullFunPtr,
    nullPtr,
    peekArray,
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
import Raylib.ForeignUtil (Freeable (rlFreeDependents), c'free, peekStaticArray, peekStaticArrayOff, pokeMaybeOff, pokeStaticArray, pokeStaticArrayOff)

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
    frameCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    sampleRate <- fromIntegral <$> (peekByteOff _p 4 :: IO CUInt)
    sampleSize <- fromIntegral <$> (peekByteOff _p 8 :: IO CUInt)
    channels <- fromIntegral <$> (peekByteOff _p 12 :: IO CUInt)
    wDataPtr <- (peekByteOff _p 16 :: IO (Ptr CShort))
    wData <- map fromIntegral <$> peekArray (fromInteger $ frameCount * channels) wDataPtr
    return $ Wave frameCount sampleRate sampleSize channels wData
  poke _p (Wave frameCount sampleRate sampleSize channels wData) = do
    pokeByteOff _p 0 (fromIntegral frameCount :: CUInt)
    pokeByteOff _p 4 (fromIntegral sampleRate :: CUInt)
    pokeByteOff _p 8 (fromIntegral sampleSize :: CUInt)
    pokeByteOff _p 12 (fromIntegral channels :: CUInt)
    pokeByteOff _p 16 =<< newArray (map fromIntegral wData :: [CShort])
    return ()

instance Freeable Wave where
  rlFreeDependents _ ptr = do
    dataPtr <- peekByteOff ptr 16 :: IO (Ptr CShort)
    c'free $ castPtr dataPtr

-- RAudioBuffer/Processor are bound weirdly. They are currently used as `Ptr`s
-- because peeking/poking them every time an audio function is called doesn't
-- work properly (they are stored in a linked list in C, which makes it very
-- difficult to properly marshal them).
--
-- The types defined here are actually unnecessary because the pointers are
-- never dereferenced.
data RAudioBuffer = RAudioBuffer
  { rAudioBuffer'converter :: [Int], -- Implemented as an array of 39 integers because the entire `ma_data_converter` type is too complex
    rAudioBuffer'callback :: C'AudioCallback,
    rAudioBuffer'processor :: Maybe RAudioProcessor,
    rAudioBuffer'volume :: Float,
    rAudioBuffer'pitch :: Float,
    rAudioBuffer'pan :: Float,
    rAudioBuffer'playing :: Bool,
    rAudioBuffer'paused :: Bool,
    rAudioBuffer'looping :: Bool,
    rAudioBuffer'usage :: Int,
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
    nextPtr <- peekByteOff _p 376
    next <- loadNext nextPtr
    prevPtr <- peekByteOff _p 384
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
        converter <- map fromIntegral <$> (peekStaticArray 39 (castPtr ptr) :: IO [CInt])
        callback <- peekByteOff ptr 312
        pPtr <- peekByteOff ptr 320 :: IO (Ptr RAudioProcessor)
        processor <- if pPtr == nullPtr then return Nothing else Just <$> peek pPtr

        volume <- realToFrac <$> (peekByteOff ptr 328 :: IO CFloat)
        pitch <- realToFrac <$> (peekByteOff ptr 332 :: IO CFloat)
        pan <- realToFrac <$> (peekByteOff ptr 336 :: IO CFloat)

        playing <- toBool <$> (peekByteOff ptr 340 :: IO CBool)
        paused <- toBool <$> (peekByteOff ptr 341 :: IO CBool)
        looping <- toBool <$> (peekByteOff ptr 342 :: IO CBool)
        usage <- fromIntegral <$> (peekByteOff ptr 344 :: IO CInt)

        isSubBufferProcessed <- map toBool <$> peekStaticArrayOff 2 (castPtr ptr :: Ptr CBool) 348
        sizeInFrames <- fromIntegral <$> (peekByteOff ptr 352 :: IO CUInt)
        frameCursorPos <- fromIntegral <$> (peekByteOff ptr 356 :: IO CUInt)
        framesProcessed <- fromIntegral <$> (peekByteOff ptr 360 :: IO CUInt)

        bData <- map fromIntegral <$> (peekArray (fromIntegral $ sizeInFrames * 2 * getBytesPerSample (head converter)) =<< (peekByteOff ptr 368 :: IO (Ptr CUChar)))

        return $ RAudioBuffer converter callback processor volume pitch pan playing paused looping usage isSubBufferProcessed sizeInFrames frameCursorPos framesProcessed bData
      loadNext ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            base <- loadBase ptr
            nextPtr <- peekByteOff ptr 376
            next <- loadNext nextPtr
            let p = base ((\a -> a {rAudioBuffer'prev = Just p}) <$> next) Nothing
             in return (Just p)

      loadPrev ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            base <- loadBase ptr
            prevPtr <- peekByteOff ptr 384
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
        pokeStaticArray (castPtr ptr) (map fromIntegral converter :: [CInt])
        pokeByteOff ptr 312 callback
        pokeMaybeOff (castPtr ptr) 320 processor

        pokeByteOff ptr 328 (realToFrac volume :: CFloat)
        pokeByteOff ptr 332 (realToFrac pitch :: CFloat)
        pokeByteOff ptr 336 (realToFrac pan :: CFloat)

        pokeByteOff ptr 340 (fromBool playing :: CBool)
        pokeByteOff ptr 341 (fromBool paused :: CBool)
        pokeByteOff ptr 342 (fromBool looping :: CBool)
        pokeByteOff ptr 344 (fromIntegral usage :: CInt)

        pokeStaticArrayOff (castPtr ptr) 348 (map fromBool isSubBufferProcessed :: [CBool])
        pokeByteOff ptr 352 (fromIntegral sizeInFrames :: CUInt)
        pokeByteOff ptr 356 (fromIntegral frameCursorPos :: CUInt)
        pokeByteOff ptr 360 (fromIntegral framesProcessed :: CUInt)

        pokeByteOff ptr 368 =<< newArray (map fromIntegral bData :: [CUChar])

        return ()
      pokeNext basePtr pNext =
        case pNext of
          Nothing -> pokeByteOff basePtr 376 nullPtr
          Just val -> do
            nextPtr <- malloc
            pokeBase nextPtr val
            pokeNext nextPtr (rAudioBuffer'next val)
            pokeByteOff nextPtr 384 basePtr
            pokeByteOff basePtr 376 nextPtr
      pokePrev basePtr pPrev =
        case pPrev of
          Nothing -> pokeByteOff basePtr 384 nullPtr
          Just val -> do
            prevPtr <- malloc
            pokeBase prevPtr val
            pokeByteOff prevPtr 376 basePtr
            pokePrev prevPtr (rAudioBuffer'prev val)
            pokeByteOff basePtr 384 prevPtr

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
    nextPtr <- peekByteOff _p 8
    next <- loadNext nextPtr
    prevPtr <- peekByteOff _p 16
    prev <- loadPrev prevPtr
    return $ let p = RAudioProcessor process ((\a -> a {rAudioProcessor'prev = Just p}) <$> next) ((\a -> a {rAudioProcessor'next = Just p}) <$> prev) in p
    where
      loadProcess ptr = do
        funPtr <- peekByteOff ptr 0
        if funPtr == nullFunPtr then return Nothing else return (Just funPtr)
      loadNext ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            process <- loadProcess ptr
            nextPtr <- peekByteOff ptr 8
            next <- loadNext nextPtr
            let p = RAudioProcessor process ((\a -> a {rAudioProcessor'prev = Just p}) <$> next) Nothing
             in return (Just p)

      loadPrev ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            process <- loadProcess ptr
            prevPtr <- peekByteOff ptr 16
            prev <- loadPrev prevPtr
            let p = RAudioProcessor process Nothing ((\a -> a {rAudioProcessor'next = Just p}) <$> prev)
             in return (Just p)
  poke _p (RAudioProcessor process next prev) = do
    pokeMaybeOff (castPtr _p) 0 process
    pokeNext (castPtr _p) next
    pokePrev (castPtr _p) prev
    return ()
    where
      pokeNext basePtr pNext =
        case pNext of
          Nothing -> pokeByteOff basePtr 8 nullPtr
          Just val -> do
            nextPtr <- malloc
            pokeMaybeOff nextPtr 0 (rAudioProcessor'process val)
            pokeNext nextPtr (rAudioProcessor'next val)
            pokeByteOff nextPtr 16 basePtr
            pokeByteOff basePtr 8 nextPtr
      pokePrev basePtr pPrev =
        case pPrev of
          Nothing -> pokeByteOff basePtr 16 nullPtr
          Just val -> do
            prevPtr <- malloc
            pokeMaybeOff prevPtr 0 (rAudioProcessor'process val)
            pokeByteOff prevPtr 8 basePtr
            pokePrev prevPtr (rAudioProcessor'prev val)
            pokeByteOff basePtr 16 prevPtr

data AudioStream = AudioStream
  { audioStream'buffer :: Ptr RAudioBuffer,
    audioStream'processor :: Ptr RAudioProcessor,
    audioStream'sampleRate :: Integer,
    audioStream'sampleSize :: Integer,
    audiostream'channels :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable AudioStream where
  sizeOf _ = 32
  alignment _ = 8
  peek _p = do
    buffer <- peekByteOff _p 0
    processor <- peekByteOff _p 8
    sampleRate <- fromIntegral <$> (peekByteOff _p 16 :: IO CUInt)
    sampleSize <- fromIntegral <$> (peekByteOff _p 20 :: IO CUInt)
    channels <- fromIntegral <$> (peekByteOff _p 24 :: IO CUInt)
    return $ AudioStream buffer processor sampleRate sampleSize channels
  poke _p (AudioStream buffer processor sampleRate sampleSize channels) = do
    pokeByteOff _p 0 buffer
    pokeByteOff _p 8 processor
    pokeByteOff _p 16 (fromIntegral sampleRate :: CUInt)
    pokeByteOff _p 20 (fromIntegral sampleSize :: CUInt)
    pokeByteOff _p 24 (fromIntegral channels :: CUInt)
    return ()

data Sound = Sound
  { sound'stream :: AudioStream,
    sound'frameCount :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable Sound where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    stream <- peekByteOff _p 0
    frameCount <- fromIntegral <$> (peekByteOff _p 32 :: IO CUInt)
    return $ Sound stream frameCount
  poke _p (Sound stream frameCount) = do
    pokeByteOff _p 0 stream
    pokeByteOff _p 32 (fromIntegral frameCount :: CUInt)
    return ()

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
    stream <- peekByteOff _p 0
    frameCount <- fromIntegral <$> (peekByteOff _p 32 :: IO CUInt)
    looping <- toBool <$> (peekByteOff _p 36 :: IO CBool)
    ctxType <- peekByteOff _p 40
    ctxData <- peekByteOff _p 48
    return $ Music stream frameCount looping ctxType ctxData
  poke _p (Music stream frameCount looping ctxType ctxData) = do
    pokeByteOff _p 0 stream
    pokeByteOff _p 32 (fromIntegral frameCount :: CUInt)
    pokeByteOff _p 36 (fromBool looping :: CInt)
    pokeByteOff _p 40 ctxType
    pokeByteOff _p 48 ctxData
    return ()

---------------------------------------
-- audio callbacks --------------------
---------------------------------------

type AudioCallback = Ptr () -> Integer -> IO ()

-- TODO: Add FunPtrs to WindowResources for automatic memory management

type C'AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())