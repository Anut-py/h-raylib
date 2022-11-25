{-# OPTIONS -Wall #-}
module Raylib.Util (c'free, pop, popCArray, withArray2D, configsToBitflag) where

import Raylib.Types (ConfigFlag)
import Foreign (Ptr, Storable (peek), castPtr, newArray, free, peekArray)
import Control.Monad (forM_)
import Data.Bits ((.|.))
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

configsToBitflag :: [ConfigFlag] -> Integer
configsToBitflag = fromIntegral . foldr folder (toEnum 0) 
    where folder a b = (fromEnum a) .|. b
