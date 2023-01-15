{-# OPTIONS -Wall #-}
module Raylib.Util (c'free, pop, popCArray, withArray2D, configsToBitflag, withMaybe, withMaybeCString) where

import Foreign (Ptr, Storable (peek), castPtr, newArray, free, peekArray, with, nullPtr)
import Control.Monad (forM_)
import Data.Bits ((.|.))
import Foreign.C (CString, withCString)
-- Internal utility functions

foreign import ccall "stdlib.h free" c'free :: Ptr () -> IO ()

pop :: (Storable a) => Ptr a -> IO a
pop ptr = do
    val <- peek ptr
    c'free $ castPtr ptr
    return val

popCArray :: (Storable a) => Int -> Ptr a -> IO [a]
popCArray count ptr = do
    str <- peekArray count ptr
    c'free $ castPtr ptr
    return str

withArray2D :: (Storable a) => [[a]] -> (Ptr (Ptr a) -> IO b) -> IO b
withArray2D arr func = do
    arrays <- mapM newArray arr
    ptr <- newArray arrays
    res <- func ptr
    forM_ arrays free
    free ptr
    return res

configsToBitflag :: (Enum a) => [a] -> Integer
configsToBitflag = fromIntegral . foldr folder (toEnum 0)
    where folder a b = fromEnum a .|. b

withMaybe :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe a f = case a of
    (Just val) -> with val f
    Nothing    -> f nullPtr

withMaybeCString :: Maybe String -> (CString -> IO b) -> IO b
withMaybeCString a f = case a of
    (Just val) -> withCString val f
    Nothing    -> f nullPtr