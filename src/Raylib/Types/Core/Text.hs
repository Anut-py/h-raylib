{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}

module Raylib.Types.Core.Text
  ( FontType (..),
    GlyphInfo (..),
    Font (..),
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    castPtr,
    newArray,
    peekArray,
  )
import Foreign.C
  ( CInt (..),
    CUChar,
  )
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, rlFreeArray)
import Raylib.Types.Core (Rectangle)
import Raylib.Types.Core.Textures (Image, Texture)

---------------------------------------
-- text enums -------------------------
---------------------------------------

data FontType = FontDefault | FontBitmap | FontSDF deriving (Enum)

---------------------------------------
-- text structures --------------------
---------------------------------------

data GlyphInfo = GlyphInfo
  { glyphInfo'value :: Int,
    glyphInfo'offsetX :: Int,
    glyphInfo'offsetY :: Int,
    glyphInfo'advanceX :: Int,
    glyphInfo'image :: Image
  }
  deriving (Eq, Show)

instance Storable GlyphInfo where
  sizeOf _ = 40
  alignment _ = 4
  peek _p = do
    value <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    offsetX <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    offsetY <- fromIntegral <$> (peekByteOff _p 8 :: IO CInt)
    advanceX <- fromIntegral <$> (peekByteOff _p 12 :: IO CInt)
    image <- peekByteOff _p 16
    return $ GlyphInfo value offsetX offsetY advanceX image
  poke _p (GlyphInfo value offsetX offsetY advanceX image) = do
    pokeByteOff _p 0 (fromIntegral value :: CInt)
    pokeByteOff _p 4 (fromIntegral offsetX :: CInt)
    pokeByteOff _p 8 (fromIntegral offsetY :: CInt)
    pokeByteOff _p 12 (fromIntegral advanceX :: CInt)
    pokeByteOff _p 16 image
    return ()

instance Freeable GlyphInfo where
  rlFreeDependents _ ptr = do
    dataPtr <- (peekByteOff ptr 16 :: IO (Ptr CUChar))
    c'free $ castPtr dataPtr

data Font = Font
  { font'baseSize :: Int,
    font'glyphCount :: Int,
    font'glyphPadding :: Int,
    font'texture :: Texture,
    font'recs :: [Rectangle],
    font'glyphs :: [GlyphInfo]
  }
  deriving (Eq, Show)

instance Storable Font where
  sizeOf _ = 48
  alignment _ = 4
  peek _p = do
    baseSize <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    glyphCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    glyphPadding <- fromIntegral <$> (peekByteOff _p 8 :: IO CInt)
    texture <- peekByteOff _p 12
    recPtr <- (peekByteOff _p 32 :: IO (Ptr Rectangle))
    recs <- peekArray glyphCount recPtr
    glyphPtr <- (peekByteOff _p 40 :: IO (Ptr GlyphInfo))
    glyphs <- peekArray glyphCount glyphPtr
    return $ Font baseSize glyphCount glyphPadding texture recs glyphs
  poke _p (Font baseSize glyphCount glyphPadding texture recs glyphs) = do
    pokeByteOff _p 0 (fromIntegral baseSize :: CInt)
    pokeByteOff _p 4 (fromIntegral glyphCount :: CInt)
    pokeByteOff _p 8 (fromIntegral glyphPadding :: CInt)
    pokeByteOff _p 12 texture
    pokeByteOff _p 32 =<< newArray recs
    pokeByteOff _p 40 =<< newArray glyphs
    return ()

instance Freeable Font where
  rlFreeDependents val ptr = do
    recsPtr <- (peekByteOff ptr 32 :: IO (Ptr Rectangle))
    c'free $ castPtr recsPtr
    glyphsPtr <- (peekByteOff ptr 40 :: IO (Ptr GlyphInfo))
    rlFreeArray (font'glyphs val) glyphsPtr
