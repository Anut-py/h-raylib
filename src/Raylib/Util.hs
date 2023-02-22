{-# OPTIONS -Wall #-}

module Raylib.Util (c'free, p'free, freeMaybePtr, Freeable (..), rlFreeArray, rlFreeMaybeArray, pop, popCArray, popCString, withFreeable, withArray2D, configsToBitflag, withMaybe, withMaybeCString, peekMaybe, peekMaybeOff, pokeMaybe, pokeMaybeOff, peekMaybeArray, newMaybeArray, peekStaticArray, peekStaticArrayOff, pokeStaticArray, pokeStaticArrayOff, rightPad) where

import Control.Monad (forM_, unless)
import Data.Bits ((.|.))
import Foreign (FunPtr, Ptr, Storable (peek, peekByteOff, poke, sizeOf), castPtr, free, malloc, newArray, nullPtr, peekArray, plusPtr, with)
import Foreign.C (CInt, CString, CUInt, withCString, CUChar, peekCString, CFloat)

-- Internal utility functions

foreign import ccall "stdlib.h free" c'free :: Ptr () -> IO ()

foreign import ccall "stdlib.h &free" p'free :: FunPtr (Ptr a -> IO ())

freeMaybePtr :: Ptr () -> IO ()
freeMaybePtr ptr = unless (ptr == nullPtr) (c'free ptr)

class Freeable a where
  rlFreeDependents :: a -> Ptr a -> IO ()
  rlFreeDependents _ _ = return ()

  rlFree :: a -> Ptr a -> IO ()
  rlFree val ptr = rlFreeDependents val ptr >> c'free (castPtr ptr)

instance Freeable CInt

instance Freeable CUInt

instance Freeable CUChar

instance Freeable CFloat

rlFreeArray :: (Freeable a, Storable a) => [a] -> Ptr a -> IO ()
rlFreeArray arr ptr = do
  forM_
    [0 .. length arr - 1]
    ( \i -> do
        let val = arr !! i in rlFreeDependents val (plusPtr ptr (i * sizeOf val))
    )
  c'free $ castPtr ptr

rlFreeMaybeArray :: (Freeable a, Storable a) => Maybe [a] -> Ptr a -> IO ()
rlFreeMaybeArray Nothing _ = return ()
rlFreeMaybeArray (Just arr) ptr = rlFreeArray arr ptr

pop :: (Freeable a, Storable a) => Ptr a -> IO a
pop ptr = do
  val <- peek ptr
  rlFree val ptr
  return val

popCArray :: (Freeable a, Storable a) => Int -> Ptr a -> IO [a]
popCArray count ptr = do
  str <- peekArray count ptr
  c'free $ castPtr ptr
  return str

popCString :: CString -> IO String
popCString ptr = do
  str <- peekCString ptr
  c'free $ castPtr ptr
  return str

withFreeable :: (Freeable a, Storable a) => a -> (Ptr a -> IO b) -> IO b
withFreeable val f = do
  ptr <- malloc
  poke ptr val
  result <- f ptr
  rlFree val ptr
  return result

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

peekMaybe :: (Storable a) => Ptr (Ptr a) -> IO (Maybe a)
peekMaybe ptr = do
  ref <- peek ptr
  if ref == nullPtr then return Nothing else Just <$> peek ref

peekMaybeOff :: (Storable a) => Ptr (Ptr a) -> Int -> IO (Maybe a)
peekMaybeOff ptr off = do
  ref <- peekByteOff ptr off
  if ref == nullPtr then return Nothing else Just <$> peek ref

pokeMaybe :: (Storable a) => Ptr (Ptr a) -> Maybe a -> IO ()
pokeMaybe ptr val = case val of
  Nothing -> poke ptr nullPtr
  Just a -> with a $ poke ptr

pokeMaybeOff :: (Storable a) => Ptr (Ptr a) -> Int -> Maybe a -> IO ()
pokeMaybeOff ptr off = pokeMaybe (plusPtr ptr off)

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
pokeStaticArray ptr (x : xs) = poke ptr x >> pokeStaticArray (plusPtr ptr $ sizeOf x) xs

pokeStaticArrayOff :: (Storable a) => Ptr a -> Int -> [a] -> IO ()
pokeStaticArrayOff ptr off = pokeStaticArray (plusPtr ptr off)

rightPad :: Int -> a -> [a] -> [a]
rightPad size val arr = take size $ arr ++ repeat val
