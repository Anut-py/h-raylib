module Raylib.Util (c'free, pop, withArray2D) where
import Foreign (Ptr, Storable (peek), castPtr, mallocArray, newArray, free)

-- Internal utility functions

foreign import ccall "stdlib.h free" c'free :: Ptr () -> IO ()

pop :: (Storable a) => Ptr a -> IO a
pop ptr = do
    val <- peek ptr
    c'free $ castPtr ptr
    return val

withArray2D :: (Storable a) => [[a]] -> (Ptr (Ptr a) -> IO b) -> IO b
withArray2D arr func = do
    arrays <- mapM newArray arr
    ptr <- newArray arrays
    res <- func ptr
    free ptr
    return res