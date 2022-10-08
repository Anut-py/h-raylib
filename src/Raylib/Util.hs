module Raylib.Util (c'free, pop, popCArray, withArray2D) where
import Foreign (Ptr, Storable (peek), castPtr, mallocArray, newArray, free, peekArray)
import Control.Monad (forM, forM_)
import Foreign.C (CChar, peekCString)

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