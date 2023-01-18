{-# OPTIONS -Wall #-}
module Raylib.Util (c'free, pop, popCArray, withArray2D, configsToBitflag, withMaybe, withMaybeCString, peekMaybeArray, newMaybeArray, peekStaticArray, peekStaticArrayOff, pokeStaticArray, pokeStaticArrayOff, rightPad) where

import Control.Monad (forM_)
import Data.Bits ((.|.))
import Foreign (Ptr, Storable (peek, sizeOf, poke), castPtr, free, newArray, nullPtr, peekArray, plusPtr, with)
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
  where
    folder a b = fromEnum a .|. b

withMaybe :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe a f = case a of
  (Just val) -> with val f
  Nothing -> f nullPtr

withMaybeCString :: Maybe String -> (CString -> IO b) -> IO b
withMaybeCString a f = case a of
  (Just val) -> withCString val f
  Nothing -> f nullPtr

peekMaybeArray :: (Storable a) => Int -> Ptr a -> IO (Maybe [a])
peekMaybeArray size ptr = if ptr == nullPtr then return Nothing else Just <$> peekArray size ptr

newMaybeArray :: (Storable a) => Maybe [a] -> IO (Ptr a)
newMaybeArray a = case a of
  (Just arr) -> newArray arr
  Nothing -> return nullPtr

peekStaticArray :: (Storable a) => Int -> Ptr a -> IO [a]
peekStaticArray size ptr = reverse <$> helper size ptr []
  where
    helper s p a =
      if s == 0
        then return a
        else do
          val <- peek p
          helper (s - 1) (plusPtr p (sizeOf val)) (val : a)

peekStaticArrayOff :: (Storable a) => Int -> Ptr a -> Int -> IO [a]
peekStaticArrayOff size ptr off = peekStaticArray size (plusPtr ptr off)

pokeStaticArray :: (Storable a) => Ptr a -> [a] -> IO ()
pokeStaticArray _ [] = return ()
pokeStaticArray ptr (x:xs) = poke ptr x >> pokeStaticArray (plusPtr ptr $ sizeOf x) xs

pokeStaticArrayOff :: (Storable a) => Ptr a -> Int -> [a] -> IO ()
pokeStaticArrayOff ptr off = pokeStaticArray (plusPtr ptr off)

rightPad :: Int -> a -> [a] -> [a]
rightPad size val arr = take size $ arr ++ repeat val
