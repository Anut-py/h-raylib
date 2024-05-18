{-# LANGUAGE DeriveAnyClass #-}

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

    -- * Pointer utilities
    p'image'data,
    p'image'width,
    p'image'height,
    p'image'mipmaps,
    p'image'format,
    p'texture'id,
    p'texture'width,
    p'texture'height,
    p'texture'mipmaps,
    p'texture'format,
    p'renderTexture'id,
    p'renderTexture'texture,
    p'renderTexture'depth,
    p'nPatchInfo'source,
    p'nPatchInfo'left,
    p'nPatchInfo'top,
    p'nPatchInfo'right,
    p'nPatchInfo'bottom,
    p'nPatchInfo'layout,
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, poke, sizeOf),
    Word8,
    castPtr,
    newArray,
    peekArray,
    plusPtr,
  )
import Foreign.C
  ( CInt (..),
    CUChar,
    CUInt,
  )
import Raylib.Internal (getPixelDataSize)
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free)
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
    width <- fromIntegral <$> peek (p'image'width _p)
    height <- fromIntegral <$> peek (p'image'height _p)
    mipmaps <- fromIntegral <$> peek (p'image'mipmaps _p)
    format <- peek (p'image'format _p)
    iData <- map fromIntegral <$> (peekArray (getPixelDataSize width height (fromEnum format)) =<< peek (p'image'data _p))
    return $ Image iData width height mipmaps format
  poke _p (Image arr width height mipmaps format) = do
    poke (p'image'data _p) =<< newArray (map fromIntegral arr)
    poke (p'image'width _p) (fromIntegral width)
    poke (p'image'height _p) (fromIntegral height)
    poke (p'image'mipmaps _p) (fromIntegral mipmaps)
    poke (p'image'format _p) format
    return ()

-- array (getPixelDataSize image'width image'height (fromEnum image'format))
p'image'data :: Ptr Image -> Ptr (Ptr CUChar)
p'image'data = (`plusPtr` 0)

p'image'width :: Ptr Image -> Ptr CInt
p'image'width = (`plusPtr` 8)

p'image'height :: Ptr Image -> Ptr CInt
p'image'height = (`plusPtr` 12)

p'image'mipmaps :: Ptr Image -> Ptr CInt
p'image'mipmaps = (`plusPtr` 16)

p'image'format :: Ptr Image -> Ptr PixelFormat
p'image'format = (`plusPtr` 20)

instance Freeable Image where
  rlFreeDependents _ ptr = do
    c'free . castPtr =<< peek (p'image'data ptr)

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
    tId <- fromIntegral <$> peek (p'texture'id _p)
    width <- fromIntegral <$> peek (p'texture'width _p)
    height <- fromIntegral <$> peek (p'texture'height _p)
    mipmaps <- fromIntegral <$> peek (p'texture'mipmaps _p)
    format <- peek (p'texture'format _p)
    return $ Texture tId width height mipmaps format
  poke _p (Texture tId width height mipmaps format) = do
    poke (p'texture'id _p) (fromIntegral tId)
    poke (p'texture'width _p) (fromIntegral width)
    poke (p'texture'height _p) (fromIntegral height)
    poke (p'texture'mipmaps _p) (fromIntegral mipmaps)
    poke (p'texture'format _p) format
    return ()

p'texture'id :: Ptr Texture -> Ptr CUInt
p'texture'id = (`plusPtr` 0)

p'texture'width :: Ptr Texture -> Ptr CInt
p'texture'width = (`plusPtr` 4)

p'texture'height :: Ptr Texture -> Ptr CInt
p'texture'height = (`plusPtr` 8)

p'texture'mipmaps :: Ptr Texture -> Ptr CInt
p'texture'mipmaps = (`plusPtr` 12)

p'texture'format :: Ptr Texture -> Ptr PixelFormat
p'texture'format = (`plusPtr` 16)

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
    rtId <- fromIntegral <$> peek (p'renderTexture'id _p)
    texture <- peek (p'renderTexture'texture _p)
    depth <- peek (p'renderTexture'depth _p)
    return $ RenderTexture rtId texture depth
  poke _p (RenderTexture rtId texture depth) = do
    poke (p'renderTexture'id _p) (fromIntegral rtId)
    poke (p'renderTexture'texture _p) texture
    poke (p'renderTexture'depth _p) depth
    return ()

p'renderTexture'id :: Ptr RenderTexture -> Ptr CUInt
p'renderTexture'id = (`plusPtr` 0)

p'renderTexture'texture :: Ptr RenderTexture -> Ptr Texture
p'renderTexture'texture = (`plusPtr` 4)

p'renderTexture'depth :: Ptr RenderTexture -> Ptr Texture
p'renderTexture'depth = (`plusPtr` 24)

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
    source <- peek (p'nPatchInfo'source _p)
    left <- fromIntegral <$> peek (p'nPatchInfo'left _p)
    top <- fromIntegral <$> peek (p'nPatchInfo'top _p)
    right <- fromIntegral <$> peek (p'nPatchInfo'right _p)
    bottom <- fromIntegral <$> peek (p'nPatchInfo'bottom _p)
    layout <- peek (p'nPatchInfo'layout _p)
    return $ NPatchInfo source left right top bottom layout
  poke _p (NPatchInfo source left right top bottom layout) = do
    poke (p'nPatchInfo'source _p) source
    poke (p'nPatchInfo'left _p) (fromIntegral left)
    poke (p'nPatchInfo'right _p) (fromIntegral right)
    poke (p'nPatchInfo'top _p) (fromIntegral top)
    poke (p'nPatchInfo'bottom _p) (fromIntegral bottom)
    poke (p'nPatchInfo'layout _p) layout
    return ()

p'nPatchInfo'source :: Ptr NPatchInfo -> Ptr Rectangle
p'nPatchInfo'source = (`plusPtr` 0)

p'nPatchInfo'left :: Ptr NPatchInfo -> Ptr CInt
p'nPatchInfo'left = (`plusPtr` 16)

p'nPatchInfo'top :: Ptr NPatchInfo -> Ptr CInt
p'nPatchInfo'top = (`plusPtr` 20)

p'nPatchInfo'right :: Ptr NPatchInfo -> Ptr CInt
p'nPatchInfo'right = (`plusPtr` 24)

p'nPatchInfo'bottom :: Ptr NPatchInfo -> Ptr CInt
p'nPatchInfo'bottom = (`plusPtr` 28)

p'nPatchInfo'layout :: Ptr NPatchInfo -> Ptr NPatchLayout
p'nPatchInfo'layout = (`plusPtr` 32)
