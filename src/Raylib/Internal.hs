{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Internal (c'rlGetShaderIdDefault) where

import Foreign.C (CInt (..))

foreign import ccall safe "rlgl.h rlGetShaderIdDefault" c'rlGetShaderIdDefault :: IO CInt
