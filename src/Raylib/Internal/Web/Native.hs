{-# OPTIONS -Wall -Wno-unrecognised-pragmas #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Redundant lambda" #-}

-- | Internal functions for running in a browser
--
--   /NOTE: This module is only used when building for the web/
module Raylib.Internal.Web.Native
  ( callRaylibFunction,
    jslog,
    jsfree,
    p'jsfree,
    CallRaylibType (..),
  )
where

import Foreign (FunPtr, Ptr, Storable (peek, sizeOf), castPtr, free, mallocArray, pokeArray)
import Foreign.C (CChar, CString, CUChar (..), CUInt (..), castCharToCChar, withCStringLen)
import Raylib.Internal.Web.Processable
  ( Processable (processableType),
    ProcessedParam (..),
    processParam,
  )

-- | For \"varargs\" function calls, based on https://wiki.haskell.org/Varargs
class CallRaylibType t where
  callRaylibFunction' :: String -> IO [ProcessedParam] -> t

instance (Storable a, Processable a) => CallRaylibType (IO a) where
  callRaylibFunction' func params' = do
    params <- params'
    callRaylibFunctionRaw func params

instance (Storable a, Processable a, CallRaylibType r) => CallRaylibType (a -> r) where
  callRaylibFunction' func params' = \x ->
    callRaylibFunction'
      func
      ( do
          params <- params'
          param <- processParam x
          return $ params ++ [param]
      )

callRaylibFunctionRaw :: forall a. (Storable a, Processable a) => String -> [ProcessedParam] -> IO a
callRaylibFunctionRaw func params = do
  let l = length func
      p = length params
  namePtr <- mallocArray l
  pokeArray namePtr (map castCharToCChar func :: [CChar])
  let nameLen = fromIntegral l :: CUInt
      ptrs = map (\(ProcessedParam ptr _ _) -> ptr) params
      sizes = map (\(ProcessedParam _ size _) -> fromIntegral size) params :: [CUInt]
      signs = map (\(ProcessedParam _ _ pType) -> fromIntegral pType) params :: [CUChar]
      numParams = fromIntegral p :: CUInt

  ptrsPtr <- mallocArray p
  pokeArray ptrsPtr ptrs

  sizesPtr <- mallocArray p
  pokeArray sizesPtr sizes

  typesPtr <- mallocArray p
  pokeArray typesPtr signs

  resPtr <- c'callRaylibFunction namePtr nameLen ptrsPtr sizesPtr typesPtr numParams (fromIntegral $ sizeOf (undefined :: a)) (fromIntegral $ fromEnum $ processableType (undefined :: a))
  res <- peek (castPtr resPtr)

  jsfree resPtr
  free namePtr
  free ptrsPtr
  mapM_ free ptrs
  free sizesPtr
  free typesPtr

  return res

jslog :: String -> IO ()
jslog str = withCStringLen str (\(s, len) -> c'jslog s (fromIntegral len))

-- | This is an interfacing function that allows Haskell to call raylib
--   functions that have been compiled with emscripten. This has to be done in
--   a roundabout way because we cannot directly call these functions through
--   Haskell; we have to call a JS proxy function that calls the actual raylib
--   functions.
callRaylibFunction :: (CallRaylibType t) => String -> t
callRaylibFunction func = callRaylibFunction' func (return [])

#ifdef WEB_FFI

foreign import ccall "main.h jslog" c'jslog :: CString -> CUInt -> IO ()

foreign import ccall "main.h jsfree" jsfree :: Ptr () -> IO ()

foreign import ccall "main.h &jsfree" p'jsfree :: FunPtr (Ptr a -> IO ())

foreign import ccall "main.h callRaylibFunction" c'callRaylibFunction :: CString -> CUInt -> Ptr (Ptr ()) -> Ptr CUInt -> Ptr CUChar -> CUInt -> CUInt -> CUChar -> IO (Ptr ())

#else

c'jslog :: CString -> CUInt -> IO ()
c'jslog = error "(c'jslog) Not running in the web!"

jsfree :: Ptr () -> IO ()
jsfree = error "(jsfree) Not running in the web!"

p'jsfree :: FunPtr (Ptr a -> IO ())
p'jsfree = error "(p'jsfree) Not running in the web!"

c'callRaylibFunction :: CString -> CUInt -> Ptr (Ptr ()) -> Ptr CUInt -> Ptr CUChar -> CUInt -> CUInt -> CUChar -> IO (Ptr ())
c'callRaylibFunction = error "(c'callRaylibFunction): Not running in the web!"

#endif
