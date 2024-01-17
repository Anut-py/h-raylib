{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Internal
  ( WindowResources (..),
    defaultWindowResources,
    unloadSingleShader,
    unloadSingleTexture,
    unloadSingleFrameBuffer,
    unloadSingleVaoId,
    unloadSingleVboIdList,
    unloadSingleCtxDataPtr,
    unloadSingleAudioBuffer,
    unloadSingleAudioBufferAlias,
    unloadSingleAutomationEventList,
    unloadShaders,
    unloadTextures,
    unloadFrameBuffers,
    unloadVaoIds,
    unloadVboIds,
    unloadCtxData,
    unloadAudioBuffers,
    unloadAudioBufferAliases,
    unloadAutomationEventLists,
    addShaderId,
    addTextureId,
    addFrameBuffer,
    addVaoId,
    addVboIds,
    addCtxData,
    addAudioBuffer,
    addAudioBufferAlias,
    addAutomationEventList,
    c'rlGetShaderIdDefault,
    getPixelDataSize,
  )
where

import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign (Ptr, Storable (peekByteOff), free)
import Foreign.C (CInt (..), CUInt (..))
import GHC.IO (unsafePerformIO)

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
    automationEventLists :: IORef [Ptr ()]
  }

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
        automationEventLists = eventLists
      }

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

foreign import ccall safe "rlgl.h rlGetShaderIdDefault" c'rlGetShaderIdDefault :: IO CUInt

foreign import ccall safe "rlgl.h rlUnloadShaderProgram" c'rlUnloadShaderProgram :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadTexture" c'rlUnloadTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadFramebuffer" c'rlUnloadFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexArray" c'rlUnloadVertexArray :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexBuffer" c'rlUnloadVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rl_internal.h UnloadMusicStreamData" c'unloadMusicStreamData :: CInt -> Ptr () -> IO ()

foreign import ccall safe "rl_internal.h UnloadAudioBuffer_" c'unloadAudioBuffer :: Ptr () -> IO ()

foreign import ccall safe "rl_internal.h UnloadAudioBufferAlias" c'unloadAudioBufferAlias :: Ptr () -> IO ()

_unloadAutomationEventList :: Ptr () -> IO ()
_unloadAutomationEventList ptr = (free =<< (peekByteOff ptr 8 :: IO (Ptr ()))) >> free ptr

foreign import ccall safe "raylib.h GetPixelDataSize"
  c'getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

getPixelDataSize :: Int -> Int -> Int -> Int
getPixelDataSize width height format = unsafePerformIO (fromIntegral <$> c'getPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral format))