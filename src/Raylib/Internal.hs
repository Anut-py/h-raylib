{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internal h-raylib utilities for automatic memory management
module Raylib.Internal
  ( WindowResources (..),
    defaultWindowResources,
    releaseNonAudioWindowResources,
    releaseAudioWindowResources,
    releaseAllWindowResources,
    Closeable (..),
    managed,

    -- * Unloading individual resources
    unloadSingleShader,
    unloadSingleTexture,
    unloadSingleFrameBuffer,
    unloadSingleVaoId,
    unloadSingleVboIdList,
    unloadSingleCtxDataPtr,
    unloadSingleAudioBuffer,
    unloadSingleAudioBufferAlias,
    unloadSingleAutomationEventList,
    unloadSingleFunPtr,

    -- * Unloading all resources
    unloadShaders,
    unloadTextures,
    unloadFrameBuffers,
    unloadVaoIds,
    unloadVboIds,
    unloadCtxData,
    unloadAudioBuffers,
    unloadAudioBufferAliases,
    unloadAutomationEventLists,
    unloadFunPtrs,

    -- * Adding resources
    addShaderId,
    addTextureId,
    addFrameBuffer,
    addVaoId,
    addVboIds,
    addCtxData,
    addAudioBuffer,
    addAudioBufferAlias,
    addAutomationEventList,
    addFunPtr,

    -- * Native unload functions
    c'rlUnloadShaderProgram,
    c'rlUnloadTexture,
    c'rlUnloadFramebuffer,
    c'rlUnloadVertexArray,
    c'rlUnloadVertexBuffer,
    c'unloadMusicStreamData,
    c'unloadAudioBuffer,
    c'unloadAudioBufferAlias,
    c'getPixelDataSize,
    _unloadAutomationEventList,

    -- * Miscellaneous
    c'rlGetShaderIdDefault,
    c'rlGetShaderLocsDefault,
    getPixelDataSize,
  )
where

import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign (FunPtr, Ptr, Storable (peekByteOff), free, freeHaskellFunPtr, castFunPtr)
import Foreign.C (CInt (..), CUInt (..))
import GHC.IO (unsafePerformIO)
import Raylib.Internal.TH (genNative)

#ifdef WEB_FFI

import Raylib.Internal.Web.Native (callRaylibFunction)

#endif

-- | Tracks all raylib resources which cannot be immediately freed.
--
--   Each field is an `IORef` to a list, and the list contains the data to be
--   tracked. Typically, data allocated on the GPU is stored here.
data WindowResources = WindowResources
  { shaderIds :: IORef [CUInt],
    shaderLocations :: IORef (Map Integer (Map String Int)),
    textureIds :: IORef [CUInt],
    frameBuffers :: IORef [CUInt],
    vaoIds :: IORef [CUInt],
    vboIds :: IORef [CUInt],
    ctxDataPtrs :: IORef [(CInt, Ptr ())],
    audioBuffers :: IORef [Ptr ()],
    audioBufferAliases :: IORef [Ptr ()],
    automationEventLists :: IORef [Ptr ()],
    funPtrs :: IORef [FunPtr ()]
  }

-- | Typeclass to conveniently release resources
class Closeable a where
  -- | Release a resource; this is only necessary when using an unmanaged resource
  --
  --   WARNING: Do not use this on a managed resource, doing so will cause it to be freed twice
  close :: a -> IO ()

  -- | Add an unmanaged resource to a `WindowResources` handle to be freed later
  addToWindowResources :: WindowResources -> a -> IO ()

instance {-# OVERLAPPABLE #-} (Closeable a) => Closeable [a] where
  close xs = forM_ xs close
  addToWindowResources window xs = forM_ xs (addToWindowResources window)

-- | Use this when loading a resource for automatic memory management
managed :: (Closeable a) => WindowResources -> IO a -> IO a
managed window resource = do
  resource' <- resource
  addToWindowResources window resource'
  return resource'

defaultWindowResources :: IO WindowResources
defaultWindowResources = do
  sIds <- newIORef []
  sLocs <- newIORef Map.empty
  tIds <- newIORef []
  fbs <- newIORef []
  vaos <- newIORef []
  vbos <- newIORef []
  cdps <- newIORef []
  aBufs <- newIORef []
  aliases <- newIORef []
  eventLists <- newIORef []
  fPtrs <- newIORef []
  return
    WindowResources
      { shaderIds = sIds,
        shaderLocations = sLocs,
        textureIds = tIds,
        frameBuffers = fbs,
        vaoIds = vaos,
        vboIds = vbos,
        ctxDataPtrs = cdps,
        audioBuffers = aBufs,
        audioBufferAliases = aliases,
        automationEventLists = eventLists,
        funPtrs = fPtrs
      }

$( genNative
     [ ("c'rlGetShaderIdDefault", "rlGetShaderIdDefault_", "rlgl_bindings.h", [t|IO CUInt|]),
       ("c'rlGetShaderLocsDefault", "rlGetShaderLocsDefault_", "rlgl_bindings.h", [t|IO (Ptr CInt)|]),
       ("c'rlUnloadShaderProgram", "rlUnloadShaderProgram_", "rlgl_bindings.h", [t|CUInt -> IO ()|]),
       ("c'rlUnloadTexture", "rlUnloadTexture_", "rlgl_bindings.h", [t|CUInt -> IO ()|]),
       ("c'rlUnloadFramebuffer", "rlUnloadFramebuffer_", "rlgl_bindings.h", [t|CUInt -> IO ()|]),
       ("c'rlUnloadVertexArray", "rlUnloadVertexArray_", "rlgl_bindings.h", [t|CUInt -> IO ()|]),
       ("c'rlUnloadVertexBuffer", "rlUnloadVertexBuffer_", "rlgl_bindings.h", [t|CUInt -> IO ()|]),
       ("c'unloadMusicStreamData", "UnloadMusicStreamData", "rl_internal.h", [t|CInt -> Ptr () -> IO ()|]),
       ("c'unloadAudioBuffer", "UnloadAudioBuffer_", "rl_internal.h", [t|Ptr () -> IO ()|]),
       ("c'unloadAudioBufferAlias", "UnloadAudioBufferAlias", "rl_internal.h", [t|Ptr () -> IO ()|]),
       ("c'getPixelDataSize", "GetPixelDataSize_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> IO CInt|])
     ]
 )

unloadSingleShader :: (Integral a) => a -> WindowResources -> IO ()
unloadSingleShader sId' wr = do
  shaderIdDefault <- c'rlGetShaderIdDefault
  unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId)
  modifyIORef (shaderIds wr) (delete sId)
  where
    sId = fromIntegral sId'

unloadSingleTexture :: (Integral a) => a -> WindowResources -> IO ()
unloadSingleTexture tId' wr = do
  when (tId > 0) (c'rlUnloadTexture tId)
  modifyIORef (textureIds wr) (delete tId)
  where
    tId = fromIntegral tId'

unloadSingleFrameBuffer :: (Integral a) => a -> WindowResources -> IO ()
unloadSingleFrameBuffer fbId' wr = do
  when (fbId > 0) (c'rlUnloadFramebuffer fbId)
  modifyIORef (frameBuffers wr) (delete fbId)
  where
    fbId = fromIntegral fbId'

unloadSingleVaoId :: (Integral a) => a -> WindowResources -> IO ()
unloadSingleVaoId vaoId' wr = do
  c'rlUnloadVertexArray vaoId
  modifyIORef (vaoIds wr) (delete vaoId)
  where
    vaoId = fromIntegral vaoId'

unloadSingleVboIdList :: (Integral a) => Maybe [a] -> WindowResources -> IO ()
unloadSingleVboIdList Nothing _ = return ()
unloadSingleVboIdList (Just vboIdList') wr = do
  forM_
    vboIdList
    ( \vboId -> do
        c'rlUnloadVertexBuffer vboId
        modifyIORef (vboIds wr) (delete vboId)
    )
  where
    vboIdList = map fromIntegral vboIdList'

unloadSingleCtxDataPtr :: (Integral a) => a -> Ptr () -> WindowResources -> IO ()
unloadSingleCtxDataPtr ctxType' ctxData wr = do
  c'unloadMusicStreamData ctxType ctxData
  modifyIORef (ctxDataPtrs wr) (delete (ctxType, ctxData))
  where
    ctxType = fromIntegral ctxType'

unloadSingleAudioBuffer :: Ptr () -> WindowResources -> IO ()
unloadSingleAudioBuffer buffer wr = do
  c'unloadAudioBuffer buffer
  modifyIORef (audioBuffers wr) (delete buffer)

unloadSingleAudioBufferAlias :: Ptr () -> WindowResources -> IO ()
unloadSingleAudioBufferAlias buffer wr = do
  c'unloadAudioBufferAlias buffer
  modifyIORef (audioBufferAliases wr) (delete buffer)

unloadSingleAutomationEventList :: Ptr () -> WindowResources -> IO ()
unloadSingleAutomationEventList eventList wr = do
  _unloadAutomationEventList eventList
  modifyIORef (automationEventLists wr) (delete eventList)

unloadSingleFunPtr :: FunPtr () -> WindowResources -> IO ()
unloadSingleFunPtr fPtr wr = do
  freeHaskellFunPtr fPtr
  modifyIORef (funPtrs wr) (delete fPtr)

unloadShaders :: WindowResources -> IO ()
unloadShaders wr = do
  shaderIdDefault <- c'rlGetShaderIdDefault
  vals <- readIORef (shaderIds wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals (\sId -> unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId))
        putStrLn $ "INFO: SHADER: h-raylib successfully auto-unloaded shaders (" ++ show l ++ " in total)"
    )

unloadTextures :: WindowResources -> IO ()
unloadTextures wr = do
  vals <- readIORef (textureIds wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals (\tId -> when (tId > 0) (c'rlUnloadTexture tId))
        putStrLn $ "INFO: TEXTURE: h-raylib successfully auto-unloaded textures (" ++ show l ++ " in total)"
    )

unloadFrameBuffers :: WindowResources -> IO ()
unloadFrameBuffers wr = do
  vals <- readIORef (frameBuffers wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals (\fbId -> when (fbId > 0) (c'rlUnloadFramebuffer fbId))
        putStrLn $ "INFO: FBO: h-raylib successfully auto-unloaded frame buffers (" ++ show l ++ " in total)"
    )

unloadVaoIds :: WindowResources -> IO ()
unloadVaoIds wr = do
  vals <- readIORef (vaoIds wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'rlUnloadVertexArray
        putStrLn $ "INFO: VAO: h-raylib successfully auto-unloaded vertex arrays (" ++ show l ++ " in total)"
    )

unloadVboIds :: WindowResources -> IO ()
unloadVboIds wr = do
  vals <- readIORef (vboIds wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'rlUnloadVertexBuffer
        putStrLn $ "INFO: VBO: h-raylib successfully auto-unloaded vertex buffers (" ++ show l ++ " in total)"
    )

unloadCtxData :: WindowResources -> IO ()
unloadCtxData wr = do
  vals <- readIORef (ctxDataPtrs wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals $ uncurry c'unloadMusicStreamData
        putStrLn $ "INFO: AUDIO: h-raylib successfully auto-unloaded music data (" ++ show l ++ " in total)"
    )

unloadAudioBuffers :: WindowResources -> IO ()
unloadAudioBuffers wr = do
  vals <- readIORef (audioBuffers wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'unloadAudioBuffer
        putStrLn $ "INFO: AUDIO: h-raylib successfully auto-unloaded audio buffers (" ++ show l ++ " in total)"
    )

unloadAudioBufferAliases :: WindowResources -> IO ()
unloadAudioBufferAliases wr = do
  vals <- readIORef (audioBufferAliases wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'unloadAudioBufferAlias
        putStrLn $ "INFO: AUDIO: h-raylib successfully auto-unloaded audio buffer aliases (" ++ show l ++ " in total)"
    )

unloadAutomationEventLists :: WindowResources -> IO ()
unloadAutomationEventLists wr = do
  vals <- readIORef (automationEventLists wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals _unloadAutomationEventList
        putStrLn $ "INFO: AUTOMATION: h-raylib successfully auto-unloaded automation event lists (" ++ show l ++ " in total)"
    )

unloadFunPtrs :: WindowResources -> IO ()
unloadFunPtrs wr = do
  vals <- readIORef (funPtrs wr)
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals freeHaskellFunPtr
        putStrLn $ "INFO: h-raylib successfully auto-unloaded `FunPtr`s (" ++ show l ++ " in total)"
    )

addShaderId :: (Integral a) => a -> WindowResources -> IO ()
addShaderId sId' wr = do
  modifyIORef (shaderIds wr) (\xs -> if sId `elem` xs then xs else sId : xs)
  where
    sId = fromIntegral sId'

addTextureId :: (Integral a) => a -> WindowResources -> IO ()
addTextureId tId' wr = do
  modifyIORef (textureIds wr) (\xs -> if tId `elem` xs then xs else tId : xs)
  where
    tId = fromIntegral tId'

addFrameBuffer :: (Integral a) => a -> WindowResources -> IO ()
addFrameBuffer fbId' wr = do
  modifyIORef (frameBuffers wr) (\xs -> if fbId `elem` xs then xs else fbId : xs)
  where
    fbId = fromIntegral fbId'

addVaoId :: (Integral a) => a -> WindowResources -> IO ()
addVaoId vaoId' wr = do
  modifyIORef (vaoIds wr) (\xs -> if vaoId `elem` xs then xs else vaoId : xs)
  where
    vaoId = fromIntegral vaoId'

addVboIds :: (Integral a) => Maybe [a] -> WindowResources -> IO ()
addVboIds Nothing _ = return ()
addVboIds (Just bIds') wr = do
  forM_ bIds (\x -> modifyIORef (vboIds wr) (\xs -> if x `elem` xs then xs else x : xs))
  where
    bIds = map fromIntegral bIds'

addCtxData :: (Integral a) => a -> Ptr () -> WindowResources -> IO ()
addCtxData ctxType' ctxData wr = do
  modifyIORef (ctxDataPtrs wr) (\xs -> if (ctxType, ctxData) `elem` xs then xs else (ctxType, ctxData) : xs)
  where
    ctxType = fromIntegral ctxType'

addAudioBuffer :: Ptr () -> WindowResources -> IO ()
addAudioBuffer buffer wr = do
  modifyIORef (audioBuffers wr) (\xs -> if buffer `elem` xs then xs else buffer : xs)

addAudioBufferAlias :: Ptr () -> WindowResources -> IO ()
addAudioBufferAlias alias wr = do
  modifyIORef (audioBufferAliases wr) (\xs -> if alias `elem` xs then xs else alias : xs)

addAutomationEventList :: Ptr () -> WindowResources -> IO ()
addAutomationEventList eventList wr = do
  modifyIORef (automationEventLists wr) (\xs -> if eventList `elem` xs then xs else eventList : xs)

addFunPtr :: FunPtr () -> WindowResources -> IO ()
addFunPtr fPtr wr = do
  modifyIORef (funPtrs wr) (\xs -> if fPtr `elem` xs then xs else fPtr : xs)

instance Closeable (FunPtr a) where
  close fun = freeHaskellFunPtr fun
  addToWindowResources window fun = addFunPtr (castFunPtr fun) window

releaseNonAudioWindowResources :: WindowResources -> IO ()
releaseNonAudioWindowResources wr = do
  unloadShaders wr
  unloadTextures wr
  unloadFrameBuffers wr
  unloadVaoIds wr
  unloadVboIds wr
  unloadAutomationEventLists wr
  unloadFunPtrs wr

releaseAudioWindowResources :: WindowResources -> IO ()
releaseAudioWindowResources wr = do
  unloadCtxData wr
  unloadAudioBuffers wr

releaseAllWindowResources :: WindowResources -> IO ()
releaseAllWindowResources wr = do
  releaseNonAudioWindowResources wr
  releaseAudioWindowResources wr

_unloadAutomationEventList :: Ptr () -> IO ()
_unloadAutomationEventList ptr = (free =<< (peekByteOff ptr 8 :: IO (Ptr ()))) >> free ptr

getPixelDataSize :: Int -> Int -> Int -> Int
getPixelDataSize width height format = unsafePerformIO (fromIntegral <$> c'getPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral format))
