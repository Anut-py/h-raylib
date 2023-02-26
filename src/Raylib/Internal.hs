{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Internal (shaderLocations, unloadSingleShader, unloadSingleTexture, unloadSingleFrameBuffer, unloadSingleVaoId, unloadSingleVboIdList, unloadSingleCtxDataPtr, unloadSingleAudioBuffer, unloadShaders, unloadTextures, unloadFrameBuffers, unloadVaoIds, unloadVboIds, unloadCtxData, unloadAudioBuffers, addShaderId, addTextureId, addFrameBuffer, addVaoId, addVboIds, addCtxData, addAudioBuffer, c'rlGetShaderIdDefault, getPixelDataSize) where

import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign (Ptr)
import Foreign.C (CInt (..), CUInt (..))
import GHC.IO (unsafePerformIO)

shaderIds :: IORef [CUInt]
{-# NOINLINE shaderIds #-}
shaderIds = unsafePerformIO $ newIORef []

shaderLocations :: IORef (Map Integer (Map String Int))
{-# NOINLINE shaderLocations #-}
shaderLocations = unsafePerformIO $ newIORef Map.empty

textureIds :: IORef [CUInt]
{-# NOINLINE textureIds #-}
textureIds = unsafePerformIO $ newIORef []

frameBuffers :: IORef [CUInt]
{-# NOINLINE frameBuffers #-}
frameBuffers = unsafePerformIO $ newIORef []

vaoIds :: IORef [CUInt]
{-# NOINLINE vaoIds #-}
vaoIds = unsafePerformIO $ newIORef []

vboIds :: IORef [CUInt]
{-# NOINLINE vboIds #-}
vboIds = unsafePerformIO $ newIORef []

ctxDataPtrs :: IORef [(CInt, Ptr ())]
{-# NOINLINE ctxDataPtrs #-}
ctxDataPtrs = unsafePerformIO $ newIORef []

audioBuffers :: IORef [Ptr ()]
{-# NOINLINE audioBuffers #-}
audioBuffers = unsafePerformIO $ newIORef []

unloadSingleShader :: (Integral a) => a -> IO ()
unloadSingleShader sId' = do
  shaderIdDefault <- c'rlGetShaderIdDefault
  unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId)
  modifyIORef shaderIds (delete sId)
  where
    sId = fromIntegral sId'

unloadSingleTexture :: (Integral a) => a -> IO ()
unloadSingleTexture tId' = do
  when (tId > 0) (c'rlUnloadTexture tId)
  modifyIORef textureIds (delete tId)
  where
    tId = fromIntegral tId'

unloadSingleFrameBuffer :: (Integral a) => a -> IO ()
unloadSingleFrameBuffer fbId' = do
  when (fbId > 0) (c'rlUnloadFramebuffer fbId)
  modifyIORef frameBuffers (delete fbId)
  where
    fbId = fromIntegral fbId'

unloadSingleVaoId :: (Integral a) => a -> IO ()
unloadSingleVaoId vaoId' = do
  c'rlUnloadVertexArray vaoId
  modifyIORef vaoIds (delete vaoId)
  where
    vaoId = fromIntegral vaoId'

unloadSingleVboIdList :: (Integral a) => Maybe [a] -> IO ()
unloadSingleVboIdList Nothing = return ()
unloadSingleVboIdList (Just vboIdList') = do
  forM_
    vboIdList
    ( \vboId -> do
        c'rlUnloadVertexBuffer vboId
        modifyIORef vboIds (delete vboId)
    )
  where
    vboIdList = map fromIntegral vboIdList'

unloadSingleCtxDataPtr :: (Integral a) => a -> Ptr () -> IO ()
unloadSingleCtxDataPtr ctxType' ctxData = do
  c'unloadMusicStreamData ctxType ctxData
  modifyIORef ctxDataPtrs (delete (ctxType, ctxData))
  where
    ctxType = fromIntegral ctxType'

unloadSingleAudioBuffer :: Ptr () -> IO ()
unloadSingleAudioBuffer buffer = do
  c'unloadAudioBuffer buffer
  modifyIORef audioBuffers (delete buffer)

unloadShaders :: IO ()
unloadShaders = do
  shaderIdDefault <- c'rlGetShaderIdDefault
  vals <- readIORef shaderIds
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals (\sId -> unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId))
        putStrLn $ "INFO: SHADER: h-raylib successfully auto-unloaded shaders (" ++ show l ++ " in total)"
    )

unloadTextures :: IO ()
unloadTextures = do
  vals <- readIORef textureIds
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals (\tId -> when (tId > 0) (c'rlUnloadTexture tId))
        putStrLn $ "INFO: TEXTURE: h-raylib successfully auto-unloaded textures (" ++ show l ++ " in total)"
    )

unloadFrameBuffers :: IO ()
unloadFrameBuffers = do
  vals <- readIORef frameBuffers
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals (\fbId -> when (fbId > 0) (c'rlUnloadFramebuffer fbId))
        putStrLn $ "INFO: FBO: h-raylib successfully auto-unloaded frame buffers (" ++ show l ++ " in total)"
    )

unloadVaoIds :: IO ()
unloadVaoIds = do
  vals <- readIORef vaoIds
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'rlUnloadVertexArray
        putStrLn $ "INFO: VAO: h-raylib successfully auto-unloaded vertex arrays (" ++ show l ++ " in total)"
    )

unloadVboIds :: IO ()
unloadVboIds = do
  vals <- readIORef vboIds
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'rlUnloadVertexBuffer
        putStrLn $ "INFO: VBO: h-raylib successfully auto-unloaded vertex buffers (" ++ show l ++ " in total)"
    )

unloadCtxData :: IO ()
unloadCtxData = do
  vals <- readIORef ctxDataPtrs
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals $ uncurry c'unloadMusicStreamData
        putStrLn $ "INFO: AUDIO: h-raylib successfully auto-unloaded music data (" ++ show l ++ " in total)"
    )

unloadAudioBuffers :: IO ()
unloadAudioBuffers = do
  vals <- readIORef audioBuffers
  let l = length vals
  when
    (l > 0)
    ( do
        forM_ vals c'unloadAudioBuffer
        putStrLn $ "INFO: AUDIO: h-raylib successfully auto-unloaded audio buffers (" ++ show l ++ " in total)"
    )

addShaderId :: (Integral a) => a -> IO ()
addShaderId sId' = do
  modifyIORef shaderIds (\xs -> if sId `elem` xs then xs else sId : xs)
  where
    sId = fromIntegral sId'

addTextureId :: (Integral a) => a -> IO ()
addTextureId tId' = do
  modifyIORef textureIds (\xs -> if tId `elem` xs then xs else tId : xs)
  where
    tId = fromIntegral tId'

addFrameBuffer :: (Integral a) => a -> IO ()
addFrameBuffer fbId' = do
  modifyIORef frameBuffers (\xs -> if fbId `elem` xs then xs else fbId : xs)
  where
    fbId = fromIntegral fbId'

addVaoId :: (Integral a) => a -> IO ()
addVaoId vaoId' = do
  modifyIORef vaoIds (\xs -> if vaoId `elem` xs then xs else vaoId : xs)
  where
    vaoId = fromIntegral vaoId'

addVboIds :: (Integral a) => Maybe [a] -> IO ()
addVboIds Nothing = return ()
addVboIds (Just bIds') = do
  forM_ bIds (\x -> modifyIORef vboIds (\xs -> if x `elem` xs then xs else x : xs))
  where
    bIds = map fromIntegral bIds'

addCtxData :: (Integral a) => a -> Ptr () -> IO ()
addCtxData ctxType' ctxData = do
  modifyIORef ctxDataPtrs (\xs -> if (ctxType, ctxData) `elem` xs then xs else (ctxType, ctxData) : xs)
  where
    ctxType = fromIntegral ctxType'

addAudioBuffer :: Ptr () -> IO ()
addAudioBuffer buffer = do
  modifyIORef audioBuffers (\xs -> if buffer `elem` xs then xs else buffer : xs)

foreign import ccall safe "rlgl.h rlGetShaderIdDefault" c'rlGetShaderIdDefault :: IO CUInt

foreign import ccall safe "rlgl.h rlUnloadShaderProgram" c'rlUnloadShaderProgram :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadTexture" c'rlUnloadTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadFramebuffer" c'rlUnloadFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexArray" c'rlUnloadVertexArray :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexBuffer" c'rlUnloadVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rl_internal.h UnloadMusicStreamData" c'unloadMusicStreamData :: CInt -> Ptr () -> IO ()

foreign import ccall safe "rl_internal.h UnloadAudioBuffer_" c'unloadAudioBuffer :: Ptr () -> IO ()

foreign import ccall safe "raylib.h GetPixelDataSize"
  c'getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

getPixelDataSize :: Int -> Int -> Int -> Int
getPixelDataSize width height format = unsafePerformIO (fromIntegral <$> c'getPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral format))