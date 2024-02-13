{-# OPTIONS -Wall #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Miscellaneous utility functions for marshalling values to/from C. The most
notable thing in this module is the `Freeable` typeclass.
-}
module Raylib.Internal.Foreign
  ( c'free,
    p'free,
    freeMaybePtr,
    Freeable (..),
    rlFreeArray,
    rlFreeMaybeArray,
    pop,
    popCArray,
    popCString,
    withFreeable,
    withFreeableArray,
    withFreeableArrayLen,
    withFreeableArray2D,
    configsToBitflag,
    withMaybe,
    withMaybeCString,
    withCStringBuffer,
    peekMaybe,
    peekMaybeOff,
    pokeMaybe,
    pokeMaybeOff,
    peekMaybeArray,
    newMaybeArray,
    peekStaticArray,
    peekStaticArrayOff,
    pokeStaticArray,
    pokeStaticArrayOff,
    rightPad,
  )
where

import Control.Monad (forM_, unless)
import Data.Bits ((.|.))
import Foreign (FunPtr, Ptr, Storable (peek, peekByteOff, poke, pokeByteOff, sizeOf), castPtr, malloc, newArray, nullPtr, peekArray, plusPtr, with, callocBytes)
import Foreign.C (CFloat, CInt, CString, CUChar, CUInt, peekCString, withCString)
import Foreign.C.Types (CBool, CChar, CShort, CUShort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

-- Internal utility functions

#ifdef WEB_FFI

import Raylib.Internal.Web.Native (jsfree, p'jsfree)

c'free :: Ptr () -> IO ()
c'free = jsfree

p'free :: FunPtr (Ptr a -> IO ())
p'free = p'jsfree

#else

foreign import ccall "stdlib.h free" c'free :: Ptr () -> IO ()

foreign import ccall "stdlib.h &free" p'free :: FunPtr (Ptr a -> IO ())

#endif

freeMaybePtr :: Ptr () -> IO ()
freeMaybePtr ptr = unless (ptr == nullPtr) (c'free ptr)

-- | A typeclass used internally to free complex data types. You will most
--   likely not have to use this directly. If you do need to implement it, you
--   can probably just stick with the default definitions of `rlFree` and
--   `rlFreeDependents`.
class Freeable a where
  -- | Frees the data \"dependent\" on a pointer, which usually means dynamic
  --   C arrays, i.e. more pointers
  rlFreeDependents :: a -> Ptr a -> IO ()
  rlFreeDependents _ _ = return ()

  -- | Receives a pointer and frees all of the data associated with it,
  --   including the pointer itself
  rlFree :: a -> Ptr a -> IO ()
  rlFree val ptr = rlFreeDependents val ptr >> c'free (castPtr ptr)

instance Freeable CBool

instance Freeable CChar

instance Freeable CFloat

instance Freeable CInt

instance Freeable CShort

instance Freeable CUChar

instance Freeable CUInt

instance Freeable CUShort

instance Freeable (Ptr CChar) where
  rlFreeDependents str _ = c'free (castPtr str)

instance (Freeable a, Storable a) => Freeable [a] where
  rlFreeDependents arr ptr =
    forM_
      [0 .. length arr - 1]
      ( \i -> do
          let val = arr !! i in rlFreeDependents val (plusPtr ptr (i * sizeOf val))
      )

rlFreeArray :: (Freeable a, Storable a) => [a] -> Ptr a -> IO ()
rlFreeArray arr ptr = rlFree arr (castPtr ptr)

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

withFreeableArray :: (Freeable a, Storable a) => [a] -> (Ptr a -> IO b) -> IO b
withFreeableArray arr f = do
  ptr <- newArray arr
  result <- f ptr
  rlFreeArray arr ptr
  return result

withFreeableArrayLen :: (Freeable a, Storable a) => [a] -> (Int -> Ptr a -> IO b) -> IO b
withFreeableArrayLen arr f = do
  ptr <- newArray arr
  result <- f (length arr) ptr
  rlFreeArray arr ptr
  return result

withFreeableArray2D :: (Freeable a, Storable a) => [[a]] -> (Ptr (Ptr a) -> IO b) -> IO b
withFreeableArray2D arr func = do
  arrays <- mapM newArray arr
  ptr <- newArray arrays
  res <- func ptr
  forM_ (zip [0 ..] arrays) (\(i, a) -> rlFreeArray (arr !! i) a)
  c'free $ castPtr ptr
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

withCStringBuffer :: String -> Maybe Int -> (Int -> CString -> IO b) -> IO (b, String)
withCStringBuffer str bufferSize f = do
  let bytes = BS.unpack (TE.encodeUtf8 (T.pack str))
      bufferSize' = fromMaybe (length bytes + 8) bufferSize
  buffer <- callocBytes bufferSize'
  mapM_ (uncurry (pokeByteOff buffer)) (zip [0 ..] bytes)
  res <- f bufferSize' buffer
  str' <- peekCString buffer
  c'free $ castPtr buffer
  return (res, str')

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
