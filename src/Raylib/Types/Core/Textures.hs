{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS -Wall #-}

-- | Bindings for types used mainly in @rtextures@
module Raylib.Types.Core.Textures
  ( -- * Enumerations
    PixelFormat (..),
    TextureFilter (..),
    TextureWrap (..),
    CubemapLayout (..),
    NPatchLayout (..),
    -- * Structures
    Image (..),
    Texture (..),
    RenderTexture (..),
    NPatchInfo (..),
    Texture2D,
    TextureCubemap,
    RenderTexture2D,
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    Word8,
    castPtr,
    newArray,
    peekArray,
  )
import Foreign.C
  ( CInt (..),
    CUChar,
    CUInt,
  )
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free)
import Raylib.Internal (getPixelDataSize)
import Raylib.Types.Core (Rectangle)

---------------------------------------
-- textures enums ---------------------
---------------------------------------

data PixelFormat
  = PixelFormatUnset
  | PixelFormatUncompressedGrayscale
  | PixelFormatUncompressedGrayAlpha
  | PixelFormatUncompressedR5G6B5
  | PixelFormatUncompressedR8G8B8
  | PixelFormatUncompressedR5G5B5A1
  | PixelFormatUncompressedR4G4B4A4
  | PixelFormatUncompressedR8G8B8A8
  | PixelFormatUncompressedR32
  | PixelFormatUncompressedR32G32B32
  | PixelFormatUncompressedR32G32B32A32
  | PixelFormatUncompressedR16
  | PixelFormatUncompressedR16G16B16
  | PixelFormatUncompressedR16G16B16A16
  | PixelFormatCompressedDxt1Rgb
  | PixelFormatCompressedDxt1Rgba
  | PixelFormatCompressedDxt3Rgba
  | PixelFormatCompressedDxt5Rgba
  | PixelFormatCompressedEtc1Rgb
  | PixelFormatCompressedEtc2Rgb
  | PixelFormatCompressedEtc2EacRgba
  | PixelFormatCompressedPvrtRgb
  | PixelFormatCompressedPvrtRgba
  | PixelFormatCompressedAstc4x4Rgba
  | PixelFormatCompressedAstc8x8Rgba
  deriving (Eq, Show)

instance Storable PixelFormat where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return (toEnum $ fromIntegral val)
  poke ptr v = do
    poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

instance Enum PixelFormat where
  fromEnum n = case n of
    PixelFormatUnset -> 0
    PixelFormatUncompressedGrayscale -> 1
    PixelFormatUncompressedGrayAlpha -> 2
    PixelFormatUncompressedR5G6B5 -> 3
    PixelFormatUncompressedR8G8B8 -> 4
    PixelFormatUncompressedR5G5B5A1 -> 5
    PixelFormatUncompressedR4G4B4A4 -> 6
    PixelFormatUncompressedR8G8B8A8 -> 7
    PixelFormatUncompressedR32 -> 8
    PixelFormatUncompressedR32G32B32 -> 9
    PixelFormatUncompressedR32G32B32A32 -> 10
    PixelFormatUncompressedR16 -> 11
    PixelFormatUncompressedR16G16B16 -> 12
    PixelFormatUncompressedR16G16B16A16 -> 13
    PixelFormatCompressedDxt1Rgb -> 14
    PixelFormatCompressedDxt1Rgba -> 15
    PixelFormatCompressedDxt3Rgba -> 16
    PixelFormatCompressedDxt5Rgba -> 17
    PixelFormatCompressedEtc1Rgb -> 18
    PixelFormatCompressedEtc2Rgb -> 19
    PixelFormatCompressedEtc2EacRgba -> 20
    PixelFormatCompressedPvrtRgb -> 21
    PixelFormatCompressedPvrtRgba -> 22
    PixelFormatCompressedAstc4x4Rgba -> 23
    PixelFormatCompressedAstc8x8Rgba -> 24

  toEnum n = case n of
    0 -> PixelFormatUnset
    1 -> PixelFormatUncompressedGrayscale
    2 -> PixelFormatUncompressedGrayAlpha
    3 -> PixelFormatUncompressedR5G6B5
    4 -> PixelFormatUncompressedR8G8B8
    5 -> PixelFormatUncompressedR5G5B5A1
    6 -> PixelFormatUncompressedR4G4B4A4
    7 -> PixelFormatUncompressedR8G8B8A8
    8 -> PixelFormatUncompressedR32
    9 -> PixelFormatUncompressedR32G32B32
    10 -> PixelFormatUncompressedR32G32B32A32
    11 -> PixelFormatUncompressedR16
    12 -> PixelFormatUncompressedR16G16B16
    13 -> PixelFormatUncompressedR16G16B16A16
    14 -> PixelFormatCompressedDxt1Rgb
    15 -> PixelFormatCompressedDxt1Rgba
    16 -> PixelFormatCompressedDxt3Rgba
    17 -> PixelFormatCompressedDxt5Rgba
    18 -> PixelFormatCompressedEtc1Rgb
    19 -> PixelFormatCompressedEtc2Rgb
    20 -> PixelFormatCompressedEtc2EacRgba
    21 -> PixelFormatCompressedPvrtRgb
    22 -> PixelFormatCompressedPvrtRgba
    23 -> PixelFormatCompressedAstc4x4Rgba
    24 -> PixelFormatCompressedAstc8x8Rgba
    _ -> error $ "(PixelFormat.toEnum) Invalid value: " ++ show n

data TextureFilter
  = TextureFilterPoint
  | TextureFilterBilinear
  | TextureFilterTrilinear
  | TextureFilterAnisotropic4x
  | TextureFilterAnisotropic8x
  | TextureFilterAnisotropic16x
  deriving (Enum)

data TextureWrap
  = TextureWrapRepeat
  | TextureWrapClamp
  | TextureWrapMirrorRepeat
  | TextureWrapMirrorClamp
  deriving (Enum)

data CubemapLayout
  = CubemapLayoutAutoDetect
  | CubemapLayoutLineVertical
  | CubemapLayoutLineHorizontal
  | CubemapLayoutCrossThreeByFour
  | CubemapLayoutCrossThreeByThree
  | CubemapLayoutPanorama
  deriving (Enum)

data NPatchLayout = NPatchNinePatch | NPatchThreePatchVertical | NPatchThreePatchHorizontal deriving (Eq, Show, Enum)

instance Storable NPatchLayout where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

---------------------------------------
-- textures structures ----------------
---------------------------------------

data Image = Image
  { image'data :: [Word8],
    image'width :: Int,
    image'height :: Int,
    image'mipmaps :: Int,
    image'format :: PixelFormat
  }
  deriving (Eq, Show)

instance Storable Image where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    width <- fromIntegral <$> (peekByteOff _p 8 :: IO CInt)
    height <- fromIntegral <$> (peekByteOff _p 12 :: IO CInt)
    mipmaps <- fromIntegral <$> (peekByteOff _p 16 :: IO CInt)
    format <- peekByteOff _p 20
    ptr <- (peekByteOff _p 0 :: IO (Ptr CUChar))
    arr <- peekArray (getPixelDataSize width height (fromEnum format)) ptr
    return $ Image (map fromIntegral arr) width height mipmaps format
  poke _p (Image arr width height mipmaps format) = do
    pokeByteOff _p 0 =<< newArray (map fromIntegral arr :: [CUChar])
    pokeByteOff _p 8 (fromIntegral width :: CInt)
    pokeByteOff _p 12 (fromIntegral height :: CInt)
    pokeByteOff _p 16 (fromIntegral mipmaps :: CInt)
    pokeByteOff _p 20 format
    return ()

instance Freeable Image where
  rlFreeDependents _ ptr = do
    dataPtr <- (peekByteOff ptr 0 :: IO (Ptr CUChar))
    c'free $ castPtr dataPtr

data Texture = Texture
  { texture'id :: Integer,
    texture'width :: Int,
    texture'height :: Int,
    texture'mipmaps :: Int,
    texture'format :: PixelFormat
  }
  deriving (Eq, Show, Freeable)

instance Storable Texture where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    tId <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    width <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    height <- fromIntegral <$> (peekByteOff _p 8 :: IO CInt)
    mipmaps <- fromIntegral <$> (peekByteOff _p 12 :: IO CInt)
    format <- peekByteOff _p 16
    return $ Texture tId width height mipmaps format
  poke _p (Texture tId width height mipmaps format) = do
    pokeByteOff _p 0 (fromIntegral tId :: CUInt)
    pokeByteOff _p 4 (fromIntegral width :: CInt)
    pokeByteOff _p 8 (fromIntegral height :: CInt)
    pokeByteOff _p 12 (fromIntegral mipmaps :: CInt)
    pokeByteOff _p 16 format
    return ()

type Texture2D = Texture

type TextureCubemap = Texture

data RenderTexture = RenderTexture
  { renderTexture'id :: Integer,
    renderTexture'texture :: Texture,
    renderTexture'depth :: Texture
  }
  deriving (Eq, Show, Freeable)

instance Storable RenderTexture where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    rtId <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    texture <- peekByteOff _p 4
    depth <- peekByteOff _p 24
    return $ RenderTexture rtId texture depth
  poke _p (RenderTexture rtId texture depth) = do
    pokeByteOff _p 0 (fromIntegral rtId :: CUInt)
    pokeByteOff _p 4 texture
    pokeByteOff _p 24 depth
    return ()

type RenderTexture2D = RenderTexture

data NPatchInfo = NPatchInfo
  { nPatchInfo'source :: Rectangle,
    nPatchInfo'left :: Int,
    nPatchInfo'top :: Int,
    nPatchInfo'right :: Int,
    nPatchInfo'bottom :: Int,
    nPatchInfo'layout :: NPatchLayout
  }
  deriving (Eq, Show, Freeable)

instance Storable NPatchInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    source <- peekByteOff _p 0
    left <- fromIntegral <$> (peekByteOff _p 16 :: IO CInt)
    top <- fromIntegral <$> (peekByteOff _p 20 :: IO CInt)
    right <- fromIntegral <$> (peekByteOff _p 24 :: IO CInt)
    bottom <- fromIntegral <$> (peekByteOff _p 28 :: IO CInt)
    layout <- peekByteOff _p 32
    return $ NPatchInfo source left right top bottom layout
  poke _p (NPatchInfo source left right top bottom layout) = do
    pokeByteOff _p 0 source
    pokeByteOff _p 16 (fromIntegral left :: CInt)
    pokeByteOff _p 20 (fromIntegral right :: CInt)
    pokeByteOff _p 24 (fromIntegral top :: CInt)
    pokeByteOff _p 28 (fromIntegral bottom :: CInt)
    pokeByteOff _p 32 layout
    return ()
