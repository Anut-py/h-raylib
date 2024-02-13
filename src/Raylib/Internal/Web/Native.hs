{-# OPTIONS -Wall -Wno-unrecognised-pragmas #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Redundant lambda" #-}

#ifdef WEB_FFI

module Raylib.Internal.Web.Native (ParamType (..), ProcessedParam (..), processParam, callRaylibFunction, jslog, jsfree, p'jsfree) where

import Foreign (Ptr, FunPtr, Storable (peek, sizeOf), castPtr, free, mallocArray, pokeArray)
import Foreign.C (CChar, CString, CUChar (..), CUInt (..), castCharToCChar, withCStringLen)
import Raylib.Internal.Web.Processable
  ( ParamType (..),
    Processable (processableType),
    ProcessedParam (..),
    processParam,
  )

foreign import ccall "main.h jslog" c'jslog :: CString -> CUInt -> IO ()

foreign import ccall "main.h jsfree" jsfree :: Ptr () -> IO ()

foreign import ccall "main.h &jsfree" p'jsfree :: FunPtr (Ptr a -> IO ())

foreign import ccall "main.h callRaylibFunction" c'callRaylibFunction :: CString -> CUInt -> Ptr (Ptr ()) -> Ptr CUInt -> Ptr CUChar -> CUInt -> CUInt -> CUChar -> IO (Ptr ())

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

-- For "varargs" function calls, based on https://wiki.haskell.org/Varargs
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

callRaylibFunction :: (CallRaylibType t) => String -> t
callRaylibFunction func = callRaylibFunction' func (return [])

#else

module Raylib.Internal.Web.Native where

#endif