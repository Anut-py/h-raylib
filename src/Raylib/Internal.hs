{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Internal (unloadShaders, unloadTextures, unloadFrameBuffers, unloadVaoIds, unloadVboIds, unloadCtxData, unloadAudioBuffers, addShaderId, addTextureId, addFrameBuffer, addVaoId, addVboIds, addCtxData, addAudioBuffer, c'rlGetShaderIdDefault) where

import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Foreign (Ptr)
import Foreign.C (CInt (..), CUInt (..))

shaderIds :: IO (IORef [CUInt])
shaderIds = newIORef []

textureIds :: IO (IORef [CUInt])
textureIds = newIORef []

frameBuffers :: IO (IORef [CUInt])
frameBuffers = newIORef []

vaoIds :: IO (IORef [CUInt])
vaoIds = newIORef []

vboIds :: IO (IORef [CUInt])
vboIds = newIORef []

ctxDataPtrs :: IO (IORef [(CInt, Ptr ())])
ctxDataPtrs = newIORef []

audioBuffers :: IO (IORef [Ptr ()])
audioBuffers = newIORef []

unloadShaders :: IO ()
unloadShaders = do
  shaderIdDefault <- c'rlGetShaderIdDefault
  shaderIds' <- shaderIds
  vals <- readIORef shaderIds'
  forM_ vals (\sId -> unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId))
  putStrLn "INFO: SHADER: h-raylib successfully auto-unloaded shaders"

unloadTextures :: IO ()
unloadTextures = do
  textureIds' <- textureIds
  vals <- readIORef textureIds'
  forM_ vals (\tId -> when (tId > 0) (c'rlUnloadTexture tId))
  putStrLn "INFO: TEXTURE: h-raylib successfully auto-unloaded textures"

unloadFrameBuffers :: IO ()
unloadFrameBuffers = do
  frameBuffers' <- frameBuffers
  vals <- readIORef frameBuffers'
  forM_ vals (\fbId -> when (fbId > 0) (c'rlUnloadFramebuffer fbId))
  putStrLn "INFO: FBO: h-raylib successfully auto-unloaded frame buffers"

unloadVaoIds :: IO ()
unloadVaoIds = do
  vaoIds' <- vaoIds
  vals <- readIORef vaoIds'
  forM_ vals c'rlUnloadVertexArray
  putStrLn "INFO: VAO: h-raylib successfully auto-unloaded vertex arrays"

unloadVboIds :: IO ()
unloadVboIds = do
  vboIds' <- vboIds
  vals <- readIORef vboIds'
  forM_ vals c'rlUnloadVertexBuffer
  putStrLn "INFO: VBO: h-raylib successfully auto-unloaded vertex buffers"

unloadCtxData :: IO ()
unloadCtxData = do
  ctxDataPtrs' <- ctxDataPtrs
  vals <- readIORef ctxDataPtrs'
  forM_ vals $ uncurry c'unloadMusicStreamData
  putStrLn "INFO: AUDIO: h-raylib successfully auto-unloaded music data"

unloadAudioBuffers :: IO ()
unloadAudioBuffers = do
  audioBuffers' <- audioBuffers
  vals <- readIORef audioBuffers'
  forM_ vals c'unloadAudioBuffer
  putStrLn "INFO: AUDIO: h-raylib successfully auto-unloaded audio buffers"

addShaderId :: (Integral a) => a -> IO ()
addShaderId sId' = do
  shaderIds' <- shaderIds
  modifyIORef shaderIds' (\xs -> if sId `elem` xs then xs else sId : xs)
  where
    sId = fromIntegral sId'

addTextureId :: (Integral a) => a -> IO ()
addTextureId tId' = do
  textureIds' <- textureIds
  modifyIORef textureIds' (\xs -> if tId `elem` xs then xs else tId : xs)
  where
    tId = fromIntegral tId'

addFrameBuffer :: (Integral a) => a -> IO ()
addFrameBuffer fbId' = do
  frameBuffers' <- frameBuffers
  modifyIORef frameBuffers' (\xs -> if fbId `elem` xs then xs else fbId : xs)
  where
    fbId = fromIntegral fbId'

addVaoId :: (Integral a) => a -> IO ()
addVaoId vaoId' = do
  vaoIds' <- vaoIds
  modifyIORef vaoIds' (\xs -> if vaoId `elem` xs then xs else vaoId : xs)
  where
    vaoId = fromIntegral vaoId'

addVboIds :: (Integral a) => Maybe [a] -> IO ()
addVboIds Nothing = return ()
addVboIds (Just bIds') = do
  vboIds' <- vboIds
  forM_ bIds (\x -> modifyIORef vboIds' (\xs -> if x `elem` xs then xs else x : xs))
  where
    bIds = map fromIntegral bIds'

addCtxData :: (Integral a) => a -> Ptr () -> IO ()
addCtxData ctxType' ctxData = do
  ctxDataPtrs' <- ctxDataPtrs
  modifyIORef ctxDataPtrs' (\xs -> if (ctxType, ctxData) `elem` xs then xs else (ctxType, ctxData) : xs)
  where
    ctxType = fromIntegral ctxType'

addAudioBuffer :: Ptr () -> IO ()
addAudioBuffer buffer = do
  audioBuffers' <- audioBuffers
  modifyIORef audioBuffers' (\xs -> if buffer `elem` xs then xs else buffer : xs)

foreign import ccall safe "rlgl.h rlGetShaderIdDefault" c'rlGetShaderIdDefault :: IO CUInt

foreign import ccall safe "rlgl.h rlUnloadShaderProgram" c'rlUnloadShaderProgram :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadTexture" c'rlUnloadTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadFramebuffer" c'rlUnloadFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexArray" c'rlUnloadVertexArray :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexBuffer" c'rlUnloadVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rl_internal.h UnloadMusicStreamData" c'unloadMusicStreamData :: CInt -> Ptr () -> IO ()

foreign import ccall safe "rl_internal.h UnloadAudioBuffer_" c'unloadAudioBuffer :: Ptr () -> IO ()
