-- | Bindings for types used in @rtext@
module Raylib.Types.Core.Text
  ( -- * Enumerations
    FontType (..),

    -- * Structures
    GlyphInfo (..),
    Font (..),

    -- * Pointer utilities
    p'glyphInfo'value,
    p'glyphInfo'offsetX,
    p'glyphInfo'offsetY,
    p'glyphInfo'advanceX,
    p'glyphInfo'image,
    p'font'baseSize,
    p'font'glyphCount,
    p'font'glyphPadding,
    p'font'texture,
    p'font'recs,
    p'font'glyphs,
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, poke, sizeOf),
    castPtr,
    newArray,
    peekArray,
    plusPtr,
  )
import Foreign.C
  ( CInt (..),
  )
import Raylib.Internal (Closeable(..))
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, rlFree)
import Raylib.Types.Core (Rectangle)
import Raylib.Types.Core.Textures (Image, Texture, p'image'data)

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
    value <- fromIntegral <$> peek (p'glyphInfo'value _p)
    offsetX <- fromIntegral <$> peek (p'glyphInfo'offsetX _p)
    offsetY <- fromIntegral <$> peek (p'glyphInfo'offsetY _p)
    advanceX <- fromIntegral <$> peek (p'glyphInfo'advanceX _p)
    image <- peek (p'glyphInfo'image _p)
    return $ GlyphInfo value offsetX offsetY advanceX image
  poke _p (GlyphInfo value offsetX offsetY advanceX image) = do
    poke (p'glyphInfo'value _p) (fromIntegral value)
    poke (p'glyphInfo'offsetX _p) (fromIntegral offsetX)
    poke (p'glyphInfo'offsetY _p) (fromIntegral offsetY)
    poke (p'glyphInfo'advanceX _p) (fromIntegral advanceX)
    poke (p'glyphInfo'image _p) image
    return ()

p'glyphInfo'value :: Ptr GlyphInfo -> Ptr CInt
p'glyphInfo'value = (`plusPtr` 0)

p'glyphInfo'offsetX :: Ptr GlyphInfo -> Ptr CInt
p'glyphInfo'offsetX = (`plusPtr` 4)

p'glyphInfo'offsetY :: Ptr GlyphInfo -> Ptr CInt
p'glyphInfo'offsetY = (`plusPtr` 8)

p'glyphInfo'advanceX :: Ptr GlyphInfo -> Ptr CInt
p'glyphInfo'advanceX = (`plusPtr` 12)

p'glyphInfo'image :: Ptr GlyphInfo -> Ptr Image
p'glyphInfo'image = (`plusPtr` 16)

instance Freeable GlyphInfo where
  rlFreeDependents _ ptr = c'free . castPtr =<< peek (p'image'data (p'glyphInfo'image ptr))

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
    baseSize <- fromIntegral <$> peek (p'font'baseSize _p)
    glyphCount <- fromIntegral <$> peek (p'font'glyphCount _p)
    glyphPadding <- fromIntegral <$> peek (p'font'glyphPadding _p)
    texture <- peek (p'font'texture _p)
    recs <- peekArray glyphCount =<< peek (p'font'recs _p)
    glyphs <- peekArray glyphCount =<< peek (p'font'glyphs _p)
    return $ Font baseSize glyphCount glyphPadding texture recs glyphs
  poke _p (Font baseSize glyphCount glyphPadding texture recs glyphs) = do
    poke (p'font'baseSize _p) (fromIntegral baseSize)
    poke (p'font'glyphCount _p) (fromIntegral glyphCount)
    poke (p'font'glyphPadding _p) (fromIntegral glyphPadding)
    poke (p'font'texture _p) texture
    poke (p'font'recs _p) =<< newArray recs
    poke (p'font'glyphs _p) =<< newArray glyphs
    return ()

instance Closeable Font where
  close font = close (font'texture font)
  addToWindowResources window font = addToWindowResources window (font'texture font)

p'font'baseSize :: Ptr Font -> Ptr CInt
p'font'baseSize = (`plusPtr` 0)

p'font'glyphCount :: Ptr Font -> Ptr CInt
p'font'glyphCount = (`plusPtr` 4)

p'font'glyphPadding :: Ptr Font -> Ptr CInt
p'font'glyphPadding = (`plusPtr` 8)

p'font'texture :: Ptr Font -> Ptr Texture
p'font'texture = (`plusPtr` 12)

-- array (font'glyphCount)
p'font'recs :: Ptr Font -> Ptr (Ptr Rectangle)
p'font'recs = (`plusPtr` 32)

-- array (font'glyphCount)
p'font'glyphs :: Ptr Font -> Ptr (Ptr GlyphInfo)
p'font'glyphs = (`plusPtr` 40)

instance Freeable Font where
  rlFreeDependents val ptr = do
    c'free . castPtr =<< peek (p'font'recs ptr)
    rlFree (font'glyphs val) . castPtr =<< peek (p'font'glyphs ptr)
