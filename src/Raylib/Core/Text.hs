{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings to @rtext@
module Raylib.Core.Text
  ( -- * High level
    getFontDefault,
    loadFont,
    loadFontEx,
    loadFontFromImage,
    loadFontFromMemory,
    loadFontData,
    genImageFontAtlas,
    unloadFont,
    isFontValid,
    exportFontAsCode,
    drawFPS,
    drawText,
    drawTextEx,
    drawTextPro,
    drawTextCodepoint,
    drawTextCodepoints,
    setTextLineSpacing,
    measureText,
    measureTextEx,
    getGlyphIndex,
    getGlyphInfo,
    getGlyphAtlasRec,
    loadUTF8,
    loadCodepoints,
    getCodepointCount,
    getCodepointNext,
    getCodepointPrevious,
    codepointToUTF8,

    -- * Native
    c'getFontDefault,
    c'loadFont,
    c'loadFontEx,
    c'loadFontFromImage,
    c'loadFontFromMemory,
    c'loadFontData,
    c'genImageFontAtlas,
    c'unloadFontData,
    c'isFontValid,
    c'unloadFont,
    c'exportFontAsCode,
    c'drawFPS,
    c'drawText,
    c'drawTextEx,
    c'drawTextPro,
    c'drawTextCodepoint,
    c'drawTextCodepoints,
    c'setTextLineSpacing,
    c'measureText,
    c'measureTextEx,
    c'getGlyphIndex,
    c'getGlyphInfo,
    c'getGlyphAtlasRec,
    c'loadUTF8,
    c'loadCodepoints,
    c'getCodepointCount,
    c'getCodepointNext,
    c'getCodepointPrevious,
    c'codepointToUTF8,
  )
where

import Control.Monad ((>=>))
import Foreign (Ptr, Storable (peek, sizeOf), nullPtr, toBool)
import Foreign.C
  ( CBool (..),
    CFloat (..),
    CInt (..),
    CString,
    CUChar,
    withCString,
  )
import Raylib.Internal (WindowResources, unloadSingleTexture)
import Raylib.Internal.Foreign
  ( ALike (popALike, withALike, withALikeLen),
    PALike,
    PLike,
    StringLike,
    TLike (peekTLike, popTLike, withTLike),
    pop,
    withFreeable,
    withFreeableArray2D,
    withFreeableArrayLen,
  )
import Raylib.Internal.TH (genNative)
import Raylib.Types
  ( Color,
    Font,
    FontType,
    GlyphInfo,
    Image,
    Rectangle,
    Vector2,
    p'font'texture,
    p'texture'id,
  )

$( genNative
     [ ("c'getFontDefault", "GetFontDefault_", "rl_bindings.h", [t|IO (Ptr Font)|]),
       ("c'loadFont", "LoadFont_", "rl_bindings.h", [t|CString -> IO (Ptr Font)|]),
       ("c'loadFontEx", "LoadFontEx_", "rl_bindings.h", [t|CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)|]),
       ("c'loadFontFromImage", "LoadFontFromImage_", "rl_bindings.h", [t|Ptr Image -> Ptr Color -> CInt -> IO (Ptr Font)|]),
       ("c'loadFontFromMemory", "LoadFontFromMemory_", "rl_bindings.h", [t|CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)|]),
       ("c'loadFontData", "LoadFontData_", "rl_bindings.h", [t|Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)|]),
       ("c'genImageFontAtlas", "GenImageFontAtlas_", "rl_bindings.h", [t|Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)|]),
       ("c'unloadFontData", "UnloadFontData_", "rl_bindings.h", [t|Ptr GlyphInfo -> CInt -> IO ()|]),
       ("c'isFontValid", "IsFontValid_", "rl_bindings.h", [t|Ptr Font -> IO CBool|]),
       ("c'unloadFont", "UnloadFont_", "rl_bindings.h", [t|Ptr Font -> IO ()|]),
       ("c'exportFontAsCode", "ExportFontAsCode_", "rl_bindings.h", [t|Ptr Font -> CString -> IO CBool|]),
       ("c'drawFPS", "DrawFPS_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|]),
       ("c'drawText", "DrawText_", "rl_bindings.h", [t|CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawTextEx", "DrawTextEx_", "rl_bindings.h", [t|Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTextPro", "DrawTextPro_", "rl_bindings.h", [t|Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTextCodepoint", "DrawTextCodepoint_", "rl_bindings.h", [t|Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTextCodepoints", "DrawTextCodepoints_", "rl_bindings.h", [t|Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'setTextLineSpacing", "SetTextLineSpacing_", "rl_bindings.h", [t|CInt -> IO ()|]),
       ("c'measureText", "MeasureText_", "rl_bindings.h", [t|CString -> CInt -> IO CInt|]),
       ("c'measureTextEx", "MeasureTextEx_", "rl_bindings.h", [t|Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)|]),
       ("c'getGlyphIndex", "GetGlyphIndex_", "rl_bindings.h", [t|Ptr Font -> CInt -> IO CInt|]),
       ("c'getGlyphInfo", "GetGlyphInfo_", "rl_bindings.h", [t|Ptr Font -> CInt -> IO (Ptr GlyphInfo)|]),
       ("c'getGlyphAtlasRec", "GetGlyphAtlasRec_", "rl_bindings.h", [t|Ptr Font -> CInt -> IO (Ptr Rectangle)|]),
       ("c'loadUTF8", "LoadUTF8_", "rl_bindings.h", [t|Ptr CInt -> CInt -> IO CString|]),
       ("c'loadCodepoints", "LoadCodepoints_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO (Ptr CInt)|]),
       ("c'getCodepointCount", "GetCodepointCount_", "rl_bindings.h", [t|CString -> IO CInt|]),
       ("c'getCodepointNext", "GetCodepointNext_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO CInt|]),
       ("c'getCodepointPrevious", "GetCodepointPrevious_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO CInt|]),
       ("c'codepointToUTF8", "CodepointToUTF8_", "rl_bindings.h", [t|CInt -> Ptr CInt -> IO CString|])
     ]
 )

getFontDefault :: (PLike Font font) => IO font
getFontDefault = c'getFontDefault >>= popTLike

loadFont :: (StringLike string, PLike Font font) => string -> IO font
loadFont fileName = withTLike fileName c'loadFont >>= popTLike

loadFontEx :: (StringLike string, PLike Font font) => string -> Int -> Maybe [Int] -> IO font
loadFontEx fileName fontSize codepoints =
  withTLike
    fileName
    ( \f -> case codepoints of
        Just codepoints' -> withFreeableArrayLen (map fromIntegral codepoints') (\l c -> c'loadFontEx f (fromIntegral fontSize) c (fromIntegral l))
        Nothing -> c'loadFontEx f (fromIntegral fontSize) nullPtr 0
    )
    >>= popTLike

loadFontFromImage :: (PLike Image image, PLike Font font) => image -> Color -> Int -> IO font
loadFontFromImage image key firstChar = withTLike image (\i -> withFreeable key (\k -> c'loadFontFromImage i k (fromIntegral firstChar))) >>= popTLike

loadFontFromMemory :: (PALike CUChar contents, PLike Font font) => String -> contents -> Int -> Maybe [Int] -> IO font
loadFontFromMemory fileType fileData fontSize codepoints =
  withCString
    fileType
    ( \t ->
        withALikeLen
          fileData
          ( \size d ->
              case codepoints of
                Just codepoints' ->
                  withFreeableArrayLen
                    (map fromIntegral codepoints')
                    (\l c -> c'loadFontFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)) (fromIntegral fontSize) c (fromIntegral l))
                Nothing -> c'loadFontFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)) (fromIntegral fontSize) nullPtr 0
          )
    )
    >>= popTLike

loadFontData :: (PALike CUChar contents, PLike GlyphInfo glyphInfo) => contents -> Int -> Maybe [Int] -> FontType -> IO glyphInfo
loadFontData fileData fontSize codepoints fontType =
  withALikeLen
    fileData
    ( \size d ->
        case codepoints of
          Just codepoints' ->
            withFreeableArrayLen (map fromIntegral codepoints') (\l c -> c'loadFontData d (fromIntegral (size * sizeOf (0 :: CUChar))) (fromIntegral fontSize) c (fromIntegral l) (fromIntegral (fromEnum fontType)))
          Nothing -> c'loadFontData d (fromIntegral (size * sizeOf (0 :: CUChar))) (fromIntegral fontSize) nullPtr 0 (fromIntegral (fromEnum fontType))
    )
    >>= popTLike

genImageFontAtlas :: (PALike GlyphInfo glyphInfos, PLike Image image) => glyphInfos -> [[Rectangle]] -> Int -> Int -> Int -> Int -> IO image
genImageFontAtlas chars recs glyphCount fontSize padding packMethod = withALike chars (\c -> withFreeableArray2D recs (\r -> c'genImageFontAtlas c r (fromIntegral glyphCount) (fromIntegral fontSize) (fromIntegral padding) (fromIntegral packMethod))) >>= popTLike

-- | Unloads a `managed` font from GPU memory (VRAM)
unloadFont :: (PLike Font font) => font -> WindowResources -> IO ()
unloadFont font wr =
  withTLike
    font
    ( \fontPtr -> do
        tId <- peek (p'texture'id (p'font'texture fontPtr))
        unloadSingleTexture tId wr
    )

isFontValid :: (PLike Font font) => font -> IO Bool
isFontValid font = toBool <$> withTLike font c'isFontValid

exportFontAsCode :: (PLike Font font, StringLike string) => font -> string -> IO Bool
exportFontAsCode font fileName = toBool <$> withTLike font (withTLike fileName . c'exportFontAsCode)

drawFPS :: Int -> Int -> IO ()
drawFPS x y = c'drawFPS (fromIntegral x) (fromIntegral y)

drawText :: (StringLike string) => string -> Int -> Int -> Int -> Color -> IO ()
drawText text x y fontSize color = withTLike text (\t -> withFreeable color (c'drawText t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize)))

drawTextEx :: (PLike Font font, StringLike string) => font -> string -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextEx font text position fontSize spacing tint = withTLike font (\f -> withTLike text (\t -> withFreeable position (\p -> withFreeable tint (c'drawTextEx f t p (realToFrac fontSize) (realToFrac spacing)))))

drawTextPro :: (PLike Font font, StringLike string) => font -> string -> Vector2 -> Vector2 -> Float -> Float -> Float -> Color -> IO ()
drawTextPro font text position origin rotation fontSize spacing tint = withTLike font (\f -> withTLike text (\t -> withFreeable position (\p -> withFreeable origin (\o -> withFreeable tint (c'drawTextPro f t p o (realToFrac rotation) (realToFrac fontSize) (realToFrac spacing))))))

drawTextCodepoint :: (PLike Font font) => font -> Int -> Vector2 -> Float -> Color -> IO ()
drawTextCodepoint font codepoint position fontSize tint = withTLike font (\f -> withFreeable position (\p -> withFreeable tint (c'drawTextCodepoint f (fromIntegral codepoint) p (realToFrac fontSize))))

drawTextCodepoints :: (PLike Font font, PALike CInt codepoints) => font -> codepoints -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextCodepoints font codepoints position fontSize spacing tint = withTLike font (\f -> withALikeLen codepoints (\count cp -> withFreeable position (\p -> withFreeable tint (c'drawTextCodepoints f cp (fromIntegral count) p (realToFrac fontSize) (realToFrac spacing)))))

setTextLineSpacing :: Int -> IO ()
setTextLineSpacing = c'setTextLineSpacing . fromIntegral

measureText :: (StringLike string) => string -> Int -> IO Int
measureText text fontSize = fromIntegral <$> withTLike text (\t -> c'measureText t (fromIntegral fontSize))

measureTextEx :: (PLike Font font, StringLike string) => font -> string -> Float -> Float -> IO Vector2
measureTextEx font text fontSize spacing = withTLike font (\f -> withTLike text (\t -> c'measureTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

getGlyphIndex :: (PLike Font font) => font -> Int -> IO Int
getGlyphIndex font codepoint = fromIntegral <$> withTLike font (\f -> c'getGlyphIndex f (fromIntegral codepoint))

getGlyphInfo :: (PLike Font font) => font -> Int -> IO GlyphInfo
getGlyphInfo font codepoint = withTLike font (\f -> c'getGlyphInfo f (fromIntegral codepoint)) >>= pop

getGlyphAtlasRec :: (PLike Font font) => font -> Int -> IO Rectangle
getGlyphAtlasRec font codepoint = withTLike font (\f -> c'getGlyphAtlasRec f (fromIntegral codepoint)) >>= pop

loadUTF8 :: (PALike CInt codepoints, StringLike string) => codepoints -> IO string
loadUTF8 codepoints =
  withALikeLen
    codepoints
    ( \size c ->
        c'loadUTF8 c (fromIntegral size)
    )
    >>= popTLike

loadCodepoints :: (StringLike string, PALike CInt codepoints) => string -> IO codepoints
loadCodepoints text =
  withTLike
    text
    ( \t ->
        withFreeable
          0
          ( \n -> do
              res <- c'loadCodepoints t n
              num <- peek n
              popALike (fromIntegral num) res
          )
    )

getCodepointCount :: (StringLike string) => string -> IO Int
getCodepointCount text = fromIntegral <$> withTLike text c'getCodepointCount

getCodepointNext :: (StringLike string) => string -> IO (Int, Int)
getCodepointNext text =
  withTLike
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

getCodepointPrevious :: (StringLike string) => string -> IO (Int, Int)
getCodepointPrevious text =
  withTLike
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

codepointToUTF8 :: (StringLike string) => Int -> IO string
codepointToUTF8 codepoint = withFreeable 0 (c'codepointToUTF8 (fromIntegral codepoint) >=> peekTLike)
