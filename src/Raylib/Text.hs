{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS -Wall #-}

module Raylib.Text where

import Foreign
  ( Storable (peek, sizeOf),
    toBool,
    withArray,
    withArrayLen,
  )
import Foreign.C
  ( CUChar,
    peekCString,
    withCString,
  )
import Raylib.Internal (addTextureId)
import Raylib.Native
  ( c'codepointToUTF8,
    c'drawFPS,
    c'drawText,
    c'drawTextCodepoint,
    c'drawTextCodepoints,
    c'drawTextEx,
    c'drawTextPro,
    c'exportFontAsCode,
    c'genImageFontAtlas,
    c'getCodepointCount,
    c'getCodepointNext,
    c'getCodepointPrevious,
    c'getFontDefault,
    c'getGlyphAtlasRec,
    c'getGlyphIndex,
    c'getGlyphInfo,
    c'isFontReady,
    c'loadCodepoints,
    c'loadFont,
    c'loadFontData,
    c'loadFontEx,
    c'loadFontFromImage,
    c'loadFontFromMemory,
    c'loadUTF8,
    c'measureText,
    c'measureTextEx,
  )
import Raylib.Types
  ( Color,
    Font (font'texture),
    FontType,
    GlyphInfo,
    Image,
    Rectangle,
    Texture (texture'id),
    Vector2,
  )
import Raylib.Util
  ( pop,
    popCArray,
    popCString,
    withArray2D,
    withFreeable,
  )

getFontDefault :: IO Raylib.Types.Font
getFontDefault = c'getFontDefault >>= pop

loadFont :: String -> IO Raylib.Types.Font
loadFont fileName = do
  font <- withCString fileName c'loadFont >>= pop
  addTextureId $ texture'id $ font'texture font
  return font

loadFontEx :: String -> Int -> [Int] -> Int -> IO Raylib.Types.Font
loadFontEx fileName fontSize fontChars glyphCount = do
  font <- withCString fileName (\f -> withArray (map fromIntegral fontChars) (\c -> c'loadFontEx f (fromIntegral fontSize) c (fromIntegral glyphCount))) >>= pop
  addTextureId $ texture'id $ font'texture font
  return font

loadFontFromImage :: Raylib.Types.Image -> Raylib.Types.Color -> Int -> IO Raylib.Types.Font
loadFontFromImage image key firstChar = do
  font <- withFreeable image (\i -> withFreeable key (\k -> c'loadFontFromImage i k (fromIntegral firstChar))) >>= pop
  addTextureId $ texture'id $ font'texture font
  return font

loadFontFromMemory :: String -> [Integer] -> Int -> [Int] -> Int -> IO Raylib.Types.Font
loadFontFromMemory fileType fileData fontSize fontChars glyphCount = do
  font <- withCString fileType (\t -> withArrayLen (map fromIntegral fileData) (\size d -> withArray (map fromIntegral fontChars) (\c -> c'loadFontFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)) (fromIntegral fontSize) c (fromIntegral glyphCount)))) >>= pop
  addTextureId $ texture'id $ font'texture font
  return font

loadFontData :: [Integer] -> Int -> [Int] -> Int -> FontType -> IO Raylib.Types.GlyphInfo
loadFontData fileData fontSize fontChars glyphCount fontType = withArrayLen (map fromIntegral fileData) (\size d -> withArray (map fromIntegral fontChars) (\c -> c'loadFontData d (fromIntegral $ size * sizeOf (0 :: CUChar)) (fromIntegral fontSize) c (fromIntegral glyphCount) (fromIntegral $ fromEnum fontType))) >>= pop

genImageFontAtlas :: [Raylib.Types.GlyphInfo] -> [[Raylib.Types.Rectangle]] -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
genImageFontAtlas chars recs glyphCount fontSize padding packMethod = withArray chars (\c -> withArray2D recs (\r -> c'genImageFontAtlas c r (fromIntegral glyphCount) (fromIntegral fontSize) (fromIntegral padding) (fromIntegral packMethod))) >>= pop

isFontReady :: Raylib.Types.Font -> IO Bool
isFontReady font = toBool <$> withFreeable font c'isFontReady

exportFontAsCode :: Raylib.Types.Font -> String -> IO Bool
exportFontAsCode font fileName = toBool <$> withFreeable font (withCString fileName . c'exportFontAsCode)

drawFPS :: Int -> Int -> IO ()
drawFPS x y = c'drawFPS (fromIntegral x) (fromIntegral y)

drawText :: String -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawText text x y fontSize color = withCString text (\t -> withFreeable color (c'drawText t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize)))

drawTextEx :: Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextEx font text position fontSize spacing tint = withFreeable font (\f -> withCString text (\t -> withFreeable position (\p -> withFreeable tint (c'drawTextEx f t p (realToFrac fontSize) (realToFrac spacing)))))

drawTextPro :: Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextPro font text position origin rotation fontSize spacing tint = withFreeable font (\f -> withCString text (\t -> withFreeable position (\p -> withFreeable origin (\o -> withFreeable tint (c'drawTextPro f t p o (realToFrac rotation) (realToFrac fontSize) (realToFrac spacing))))))

drawTextCodepoint :: Raylib.Types.Font -> Int -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTextCodepoint font codepoint position fontSize tint = withFreeable font (\f -> withFreeable position (\p -> withFreeable tint (c'drawTextCodepoint f (fromIntegral codepoint) p (realToFrac fontSize))))

drawTextCodepoints :: Raylib.Types.Font -> [Int] -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextCodepoints font codepoints position fontSize spacing tint = withFreeable font (\f -> withArrayLen (map fromIntegral codepoints) (\count cp -> withFreeable position (\p -> withFreeable tint (c'drawTextCodepoints f cp (fromIntegral count) p (realToFrac fontSize) (realToFrac spacing)))))

measureText :: String -> Int -> IO Int
measureText text fontSize = fromIntegral <$> withCString text (\t -> c'measureText t (fromIntegral fontSize))

measureTextEx :: Raylib.Types.Font -> String -> Float -> Float -> IO Raylib.Types.Vector2
measureTextEx font text fontSize spacing = withFreeable font (\f -> withCString text (\t -> c'measureTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

getGlyphIndex :: Raylib.Types.Font -> Int -> IO Int
getGlyphIndex font codepoint = fromIntegral <$> withFreeable font (\f -> c'getGlyphIndex f (fromIntegral codepoint))

getGlyphInfo :: Raylib.Types.Font -> Int -> IO Raylib.Types.GlyphInfo
getGlyphInfo font codepoint = withFreeable font (\f -> c'getGlyphInfo f (fromIntegral codepoint)) >>= pop

getGlyphAtlasRec :: Raylib.Types.Font -> Int -> IO Raylib.Types.Rectangle
getGlyphAtlasRec font codepoint = withFreeable font (\f -> c'getGlyphAtlasRec f (fromIntegral codepoint)) >>= pop

loadUTF8 :: [Integer] -> IO String
loadUTF8 codepoints =
  withArrayLen
    (map fromIntegral codepoints)
    ( \size c ->
        c'loadUTF8 c (fromIntegral size)
    )
    >>= popCString

loadCodepoints :: String -> IO [Int]
loadCodepoints text =
  withCString
    text
    ( \t ->
        withFreeable
          0
          ( \n -> do
              res <- c'loadCodepoints t n
              num <- peek n
              map fromIntegral <$> popCArray (fromIntegral num) res
          )
    )

getCodepointCount :: String -> IO Int
getCodepointCount text = fromIntegral <$> withCString text c'getCodepointCount

-- | Deprecated, use `getCodepointNext`
getCodepointNext :: String -> IO (Int, Int)
getCodepointNext text =
  withCString
    text
    ( \t ->
        withFreeable
          0
          ( \n ->
              do
                res <- c'getCodepointNext t n
                num <- peek n
                return (fromIntegral res, fromIntegral num)
          )
    )

getCodepointPrevious :: String -> IO (Int, Int)
getCodepointPrevious text =
  withCString
    text
    ( \t ->
        withFreeable
          0
          ( \n ->
              do
                res <- c'getCodepointPrevious t n
                num <- peek n
                return (fromIntegral res, fromIntegral num)
          )
    )

codepointToUTF8 :: Int -> IO String
codepointToUTF8 codepoint = withFreeable 0 (c'codepointToUTF8 $ fromIntegral codepoint) >>= peekCString
