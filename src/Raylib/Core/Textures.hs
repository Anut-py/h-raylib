{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings to @rtextures@
module Raylib.Core.Textures
  ( -- * High level
    loadImage,
    loadImageRaw,
    loadImageAnim,
    loadImageAnimFromMemory,
    loadImageFromMemory,
    loadImageFromTexture,
    loadImageFromScreen,
    isImageValid,
    exportImage,
    exportImageToMemory,
    exportImageAsCode,

    -- ** Image generation

    --

    -- | WARNING: When these functions return `Ptr`, you must manually free
    --   the pointer. Using `ForeignPtr` is recommended as the pointer will
    --   automatically be freed.
    genImageColor,
    genImageGradientLinear,
    genImageGradientRadial,
    genImageGradientSquare,
    genImageChecked,
    genImageWhiteNoise,
    genImagePerlinNoise,
    genImageCellular,
    genImageText,
    imageFromImage,
    imageFromChannel,
    imageText,
    imageTextEx,
    imageFormat,
    imageToPOT,
    imageCrop,
    imageAlphaCrop,
    imageAlphaClear,
    imageAlphaMask,
    imageAlphaPremultiply,
    imageBlurGaussian,
    imageKernelConvolution,
    imageResize,
    imageResizeNN,
    imageResizeCanvas,
    imageMipmaps,
    imageDither,
    imageFlipVertical,
    imageFlipHorizontal,
    imageRotate,
    imageRotateCW,
    imageRotateCCW,
    imageColorTint,
    imageColorInvert,
    imageColorGrayscale,
    imageColorContrast,
    imageColorBrightness,
    imageColorReplace,
    loadImageColors,
    loadImagePalette,
    getImageAlphaBorder,
    getImageColor,
    imageClearBackground,
    imageDrawPixel,
    imageDrawPixelV,
    imageDrawLine,
    imageDrawLineV,
    imageDrawCircle,
    imageDrawCircleV,
    imageDrawCircleLines,
    imageDrawCircleLinesV,
    imageDrawRectangle,
    imageDrawRectangleV,
    imageDrawRectangleRec,
    imageDrawRectangleLines,
    imageDrawTriangle,
    imageDrawTriangleEx,
    imageDrawTriangleLines,
    imageDrawTriangleFan,
    imageDrawTriangleStrip,
    imageDraw,
    imageDrawText,
    imageDrawTextEx,
    loadTexture,
    loadTextureFromImage,
    loadTextureCubemap,
    loadRenderTexture,
    isTextureValid,
    isRenderTextureValid,
    unloadTexture,
    unloadRenderTexture,
    updateTexture,
    updateTextureRec,
    genTextureMipmaps,
    setTextureFilter,
    setTextureWrap,
    drawTexture,
    drawTextureV,
    drawTextureEx,
    drawTextureRec,
    drawTexturePro,
    drawTextureNPatch,
    fade,
    colorToInt,
    colorNormalize,
    colorFromNormalized,
    colorToHSV,
    colorFromHSV,
    colorTint,
    colorBrightness,
    colorContrast,
    colorAlpha,
    colorAlphaBlend,
    colorLerp,
    getColor,
    getPixelColor,
    setPixelColor,
    getPixelDataSize,

    -- * Native
    c'loadImage,
    c'loadImageRaw,
    c'loadImageAnim,
    c'loadImageAnimFromMemory,
    c'loadImageFromMemory,
    c'loadImageFromTexture,
    c'loadImageFromScreen,
    c'isImageValid,
    c'unloadImage,
    c'exportImage,
    c'exportImageToMemory,
    c'exportImageAsCode,
    c'genImageColor,
    c'genImageGradientLinear,
    c'genImageGradientRadial,
    c'genImageGradientSquare,
    c'genImageChecked,
    c'genImageWhiteNoise,
    c'genImagePerlinNoise,
    c'genImageCellular,
    c'genImageText,
    c'imageCopy,
    c'imageFromImage,
    c'imageFromChannel,
    c'imageText,
    c'imageTextEx,
    c'imageFormat,
    c'imageToPOT,
    c'imageCrop,
    c'imageAlphaCrop,
    c'imageAlphaClear,
    c'imageAlphaMask,
    c'imageAlphaPremultiply,
    c'imageBlurGaussian,
    c'imageKernelConvolution,
    c'imageResize,
    c'imageResizeNN,
    c'imageResizeCanvas,
    c'imageMipmaps,
    c'imageDither,
    c'imageFlipVertical,
    c'imageFlipHorizontal,
    c'imageRotate,
    c'imageRotateCW,
    c'imageRotateCCW,
    c'imageColorTint,
    c'imageColorInvert,
    c'imageColorGrayscale,
    c'imageColorContrast,
    c'imageColorBrightness,
    c'imageColorReplace,
    c'loadImageColors,
    c'loadImagePalette,
    c'getImageAlphaBorder,
    c'getImageColor,
    c'imageClearBackground,
    c'imageDrawPixel,
    c'imageDrawPixelV,
    c'imageDrawLine,
    c'imageDrawLineV,
    c'imageDrawCircle,
    c'imageDrawCircleV,
    c'imageDrawCircleLines,
    c'imageDrawCircleLinesV,
    c'imageDrawRectangle,
    c'imageDrawRectangleV,
    c'imageDrawRectangleRec,
    c'imageDrawRectangleLines,
    c'imageDrawTriangle,
    c'imageDrawTriangleEx,
    c'imageDrawTriangleLines,
    c'imageDrawTriangleFan,
    c'imageDrawTriangleStrip,
    c'imageDraw,
    c'imageDrawText,
    c'imageDrawTextEx,
    c'loadTexture,
    c'loadTextureFromImage,
    c'loadTextureCubemap,
    c'loadRenderTexture,
    c'isTextureValid,
    c'unloadTexture,
    c'isRenderTextureValid,
    c'unloadRenderTexture,
    c'updateTexture,
    c'updateTextureRec,
    c'genTextureMipmaps,
    c'setTextureFilter,
    c'setTextureWrap,
    c'drawTexture,
    c'drawTextureV,
    c'drawTextureEx,
    c'drawTextureRec,
    c'drawTexturePro,
    c'drawTextureNPatch,
    c'fade,
    c'colorToInt,
    c'colorNormalize,
    c'colorFromNormalized,
    c'colorToHSV,
    c'colorFromHSV,
    c'colorTint,
    c'colorBrightness,
    c'colorContrast,
    c'colorAlpha,
    c'colorAlphaBlend,
    c'colorLerp,
    c'getColor,
    c'getPixelColor,
    c'setPixelColor,
  )
where

import Foreign
  ( Ptr,
    Storable (peek, sizeOf),
    toBool,
  )
import Foreign.C
  ( CBool (..),
    CFloat (..),
    CInt (..),
    CString,
    CUChar (..),
    CUInt (..),
    withCString,
  )
import GHC.IO (unsafePerformIO)
import Raylib.Internal (WindowResources, unloadSingleFrameBuffer, unloadSingleTexture)
import qualified Raylib.Internal as I
import Raylib.Internal.Foreign
  ( ALike (popALike, withALikeLen),
    Mutable (peekMutated),
    PALike,
    PLike,
    StringLike,
    TLike (popTLike, withTLike),
    pop,
    withFreeable,
  )
import Raylib.Internal.TH (genNative)
import Raylib.Types
  ( Color,
    CubemapLayout,
    Font,
    Image,
    NPatchInfo,
    PixelFormat,
    Rectangle,
    RenderTexture,
    Texture,
    TextureFilter,
    TextureWrap,
    Vector2,
    Vector3,
    Vector4,
    p'image'height,
    p'image'width,
    p'renderTexture'id,
    p'renderTexture'texture,
    p'texture'id,
  )

$( genNative
     [ ("c'loadImage", "LoadImage_", "rl_bindings.h", [t|CString -> IO (Ptr Image)|]),
       ("c'loadImageRaw", "LoadImageRaw_", "rl_bindings.h", [t|CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)|]),
       ("c'loadImageAnim", "LoadImageAnim_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO (Ptr Image)|]),
       ("c'loadImageAnimFromMemory", "LoadImageAnimFromMemory_", "rl_bindings.h", [t|CString -> Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr Image)|]),
       ("c'loadImageFromMemory", "LoadImageFromMemory_", "rl_bindings.h", [t|CString -> Ptr CUChar -> CInt -> IO (Ptr Image)|]),
       ("c'loadImageFromTexture", "LoadImageFromTexture_", "rl_bindings.h", [t|Ptr Texture -> IO (Ptr Image)|]),
       ("c'loadImageFromScreen", "LoadImageFromScreen_", "rl_bindings.h", [t|IO (Ptr Image)|]),
       ("c'isImageValid", "IsImageValid_", "rl_bindings.h", [t|Ptr Image -> IO CBool|]),
       ("c'unloadImage", "UnloadImage_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'exportImage", "ExportImage_", "rl_bindings.h", [t|Ptr Image -> CString -> IO CBool|]),
       ("c'exportImageToMemory", "ExportImageToMemory_", "rl_bindings.h", [t|Ptr Image -> CString -> Ptr CInt -> IO (Ptr CUChar)|]),
       ("c'exportImageAsCode", "ExportImageAsCode_", "rl_bindings.h", [t|Ptr Image -> CString -> IO CBool|]),
       ("c'genImageColor", "GenImageColor_", "rl_bindings.h", [t|CInt -> CInt -> Ptr Color -> IO (Ptr Image)|]),
       ("c'genImageGradientLinear", "GenImageGradientLinear_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)|]),
       ("c'genImageGradientRadial", "GenImageGradientRadial_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)|]),
       ("c'genImageGradientSquare", "GenImageGradientSquare_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)|]),
       ("c'genImageChecked", "GenImageChecked_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)|]),
       ("c'genImageWhiteNoise", "GenImageWhiteNoise_", "rl_bindings.h", [t|CInt -> CInt -> CFloat -> IO (Ptr Image)|]),
       ("c'genImagePerlinNoise", "GenImagePerlinNoise_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Image)|]),
       ("c'genImageCellular", "GenImageCellular_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> IO (Ptr Image)|]),
       ("c'genImageText", "GenImageText_", "rl_bindings.h", [t|CInt -> CInt -> CString -> IO (Ptr Image)|]),
       ("c'imageCopy", "ImageCopy_", "rl_bindings.h", [t|Ptr Image -> IO (Ptr Image)|]),
       ("c'imageFromImage", "ImageFromImage_", "rl_bindings.h", [t|Ptr Image -> Ptr Rectangle -> IO (Ptr Image)|]),
       ("c'imageFromChannel", "ImageFromChannel_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO (Ptr Image)|]),
       ("c'imageText", "ImageText_", "rl_bindings.h", [t|CString -> CInt -> Ptr Color -> IO (Ptr Image)|]),
       ("c'imageTextEx", "ImageTextEx_", "rl_bindings.h", [t|Ptr Font -> CString -> CFloat -> CFloat -> Ptr Color -> IO (Ptr Image)|]),
       ("c'imageFormat", "ImageFormat_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO ()|]),
       ("c'imageToPOT", "ImageToPOT_", "rl_bindings.h", [t|Ptr Image -> Ptr Color -> IO ()|]),
       ("c'imageCrop", "ImageCrop_", "rl_bindings.h", [t|Ptr Image -> Ptr Rectangle -> IO ()|]),
       ("c'imageAlphaCrop", "ImageAlphaCrop_", "rl_bindings.h", [t|Ptr Image -> CFloat -> IO ()|]),
       ("c'imageAlphaClear", "ImageAlphaClear_", "rl_bindings.h", [t|Ptr Image -> Ptr Color -> CFloat -> IO ()|]),
       ("c'imageAlphaMask", "ImageAlphaMask_", "rl_bindings.h", [t|Ptr Image -> Ptr Image -> IO ()|]),
       ("c'imageAlphaPremultiply", "ImageAlphaPremultiply_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageBlurGaussian", "ImageBlurGaussian_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO ()|]),
       ("c'imageKernelConvolution", "ImageKernelConvolution_", "rl_bindings.h", [t|Ptr Image -> Ptr CFloat -> CInt -> IO ()|]),
       ("c'imageResize", "ImageResize_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> IO ()|]),
       ("c'imageResizeNN", "ImageResizeNN_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> IO ()|]),
       ("c'imageResizeCanvas", "ImageResizeCanvas_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageMipmaps", "ImageMipmaps_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageDither", "ImageDither_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()|]),
       ("c'imageFlipVertical", "ImageFlipVertical_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageFlipHorizontal", "ImageFlipHorizontal_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageRotate", "ImageRotate_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO ()|]),
       ("c'imageRotateCW", "ImageRotateCW_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageRotateCCW", "ImageRotateCCW_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageColorTint", "ImageColorTint_", "rl_bindings.h", [t|Ptr Image -> Ptr Color -> IO ()|]),
       ("c'imageColorInvert", "ImageColorInvert_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageColorGrayscale", "ImageColorGrayscale_", "rl_bindings.h", [t|Ptr Image -> IO ()|]),
       ("c'imageColorContrast", "ImageColorContrast_", "rl_bindings.h", [t|Ptr Image -> CFloat -> IO ()|]),
       ("c'imageColorBrightness", "ImageColorBrightness_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO ()|]),
       ("c'imageColorReplace", "ImageColorReplace_", "rl_bindings.h", [t|Ptr Image -> Ptr Color -> Ptr Color -> IO ()|]),
       ("c'loadImageColors", "LoadImageColors_", "rl_bindings.h", [t|Ptr Image -> IO (Ptr Color)|]),
       ("c'loadImagePalette", "LoadImagePalette_", "rl_bindings.h", [t|Ptr Image -> CInt -> Ptr CInt -> IO (Ptr Color)|]),
       ("c'getImageAlphaBorder", "GetImageAlphaBorder_", "rl_bindings.h", [t|Ptr Image -> CFloat -> IO (Ptr Rectangle)|]),
       ("c'getImageColor", "GetImageColor_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> IO (Ptr Color)|]),
       ("c'imageClearBackground", "ImageClearBackground_", "rl_bindings.h", [t|Ptr Image -> Ptr Color -> IO ()|]),
       ("c'imageDrawPixel", "ImageDrawPixel_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawPixelV", "ImageDrawPixelV_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'imageDrawLine", "ImageDrawLine_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawLineV", "ImageDrawLineV_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'imageDrawCircle", "ImageDrawCircle_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawCircleV", "ImageDrawCircleV_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawCircleLines", "ImageDrawCircleLines_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawCircleLinesV", "ImageDrawCircleLinesV_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawRectangle", "ImageDrawRectangle_", "rl_bindings.h", [t|Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawRectangleV", "ImageDrawRectangleV_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'imageDrawRectangleRec", "ImageDrawRectangleRec_", "rl_bindings.h", [t|Ptr Image -> Ptr Rectangle -> Ptr Color -> IO ()|]),
       ("c'imageDrawRectangleLines", "ImageDrawRectangleLines_", "rl_bindings.h", [t|Ptr Image -> Ptr Rectangle -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawTriangle", "ImageDrawTriangle_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'imageDrawTriangleEx", "ImageDrawTriangleEx_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()|]),
       ("c'imageDrawTriangleLines", "ImageDrawTriangleLines_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'imageDrawTriangleFan", "ImageDrawTriangleFan_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawTriangleStrip", "ImageDrawTriangleStrip_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDraw", "ImageDraw_", "rl_bindings.h", [t|Ptr Image -> Ptr Image -> Ptr Rectangle -> Ptr Rectangle -> Ptr Color -> IO ()|]),
       ("c'imageDrawText", "ImageDrawText_", "rl_bindings.h", [t|Ptr Image -> CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'imageDrawTextEx", "ImageDrawTextEx_", "rl_bindings.h", [t|Ptr Image -> Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'loadTexture", "LoadTexture_", "rl_bindings.h", [t|CString -> IO (Ptr Texture)|]),
       ("c'loadTextureFromImage", "LoadTextureFromImage_", "rl_bindings.h", [t|Ptr Image -> IO (Ptr Texture)|]),
       ("c'loadTextureCubemap", "LoadTextureCubemap_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO (Ptr Texture)|]),
       ("c'loadRenderTexture", "LoadRenderTexture_", "rl_bindings.h", [t|CInt -> CInt -> IO (Ptr RenderTexture)|]),
       ("c'isTextureValid", "IsTextureValid_", "rl_bindings.h", [t|Ptr Texture -> IO CBool|]),
       ("c'unloadTexture", "UnloadTexture_", "rl_bindings.h", [t|Ptr Texture -> IO ()|]),
       ("c'isRenderTextureValid", "IsRenderTextureValid_", "rl_bindings.h", [t|Ptr RenderTexture -> IO CBool|]),
       ("c'unloadRenderTexture", "UnloadRenderTexture_", "rl_bindings.h", [t|Ptr RenderTexture -> IO ()|]),
       ("c'updateTexture", "UpdateTexture_", "rl_bindings.h", [t|Ptr Texture -> Ptr () -> IO ()|]),
       ("c'updateTextureRec", "UpdateTextureRec_", "rl_bindings.h", [t|Ptr Texture -> Ptr Rectangle -> Ptr () -> IO ()|]),
       ("c'genTextureMipmaps", "GenTextureMipmaps_", "rl_bindings.h", [t|Ptr Texture -> IO ()|]),
       ("c'setTextureFilter", "SetTextureFilter_", "rl_bindings.h", [t|Ptr Texture -> CInt -> IO ()|]),
       ("c'setTextureWrap", "SetTextureWrap_", "rl_bindings.h", [t|Ptr Texture -> CInt -> IO ()|]),
       ("c'drawTexture", "DrawTexture_", "rl_bindings.h", [t|Ptr Texture -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawTextureV", "DrawTextureV_", "rl_bindings.h", [t|Ptr Texture -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawTextureEx", "DrawTextureEx_", "rl_bindings.h", [t|Ptr Texture -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTextureRec", "DrawTextureRec_", "rl_bindings.h", [t|Ptr Texture -> Ptr Rectangle -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawTexturePro", "DrawTexturePro_", "rl_bindings.h", [t|Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTextureNPatch", "DrawTextureNPatch_", "rl_bindings.h", [t|Ptr Texture -> Ptr NPatchInfo -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'fade", "Fade_", "rl_bindings.h", [t|Ptr Color -> CFloat -> IO (Ptr Color)|]),
       ("c'colorToInt", "ColorToInt_", "rl_bindings.h", [t|Ptr Color -> IO CInt|]),
       ("c'colorNormalize", "ColorNormalize_", "rl_bindings.h", [t|Ptr Color -> IO (Ptr Vector4)|]),
       ("c'colorFromNormalized", "ColorFromNormalized_", "rl_bindings.h", [t|Ptr Vector4 -> IO (Ptr Color)|]),
       ("c'colorToHSV", "ColorToHSV_", "rl_bindings.h", [t|Ptr Color -> IO (Ptr Vector3)|]),
       ("c'colorFromHSV", "ColorFromHSV_", "rl_bindings.h", [t|CFloat -> CFloat -> CFloat -> IO (Ptr Color)|]),
       ("c'colorTint", "ColorTint_", "rl_bindings.h", [t|Ptr Color -> Ptr Color -> IO (Ptr Color)|]),
       ("c'colorBrightness", "ColorBrightness_", "rl_bindings.h", [t|Ptr Color -> CFloat -> IO (Ptr Color)|]),
       ("c'colorContrast", "ColorContrast_", "rl_bindings.h", [t|Ptr Color -> CFloat -> IO (Ptr Color)|]),
       ("c'colorAlpha", "ColorAlpha_", "rl_bindings.h", [t|Ptr Color -> CFloat -> IO (Ptr Color)|]),
       ("c'colorAlphaBlend", "ColorAlphaBlend_", "rl_bindings.h", [t|Ptr Color -> Ptr Color -> Ptr Color -> IO (Ptr Color)|]),
       ("c'colorLerp", "ColorLerp_", "rl_bindings.h", [t|Ptr Color -> Ptr Color -> CFloat -> IO (Ptr Color)|]),
       ("c'getColor", "GetColor_", "rl_bindings.h", [t|CUInt -> IO (Ptr Color)|]),
       ("c'getPixelColor", "GetPixelColor_", "rl_bindings.h", [t|Ptr () -> CInt -> IO (Ptr Color)|]),
       ("c'setPixelColor", "SetPixelColor_", "rl_bindings.h", [t|Ptr () -> Ptr Color -> CInt -> IO ()|])
     ]
 )

loadImage :: (StringLike string, PLike Image image) => string -> IO image
loadImage fileName = withTLike fileName c'loadImage >>= popTLike

loadImageRaw :: (StringLike string, PLike Image image) => string -> Int -> Int -> Int -> Int -> IO image
loadImageRaw fileName width height format headerSize =
  withTLike fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) (fromIntegral headerSize)) >>= popTLike

-- | Returns the animation and the number of frames in a tuple
loadImageAnim :: (StringLike string, PLike Image image) => string -> IO (image, Int)
loadImageAnim fileName =
  withFreeable
    0
    ( \frames ->
        withTLike
          fileName
          ( \fn -> do
              img <- c'loadImageAnim fn frames >>= popTLike
              frameNum <- fromIntegral <$> peek frames
              return (img, frameNum)
          )
    )

loadImageAnimFromMemory :: (PALike CUChar contents, PLike Image image) => String -> contents -> IO (image, Int)
loadImageAnimFromMemory fileType fileData =
  withCString
    fileType
    ( \ft ->
        withALikeLen
          fileData
          ( \size fd ->
              withFreeable
                (0 :: CInt)
                ( \frames -> do
                    img <- c'loadImageAnimFromMemory ft fd (fromIntegral $ size * sizeOf (0 :: CUChar)) frames >>= popTLike
                    frameNum <- fromIntegral <$> peek frames
                    return (img, frameNum)
                )
          )
    )

loadImageFromMemory :: (PALike CUChar contents, PLike Image image) => String -> contents -> IO image
loadImageFromMemory fileType fileData =
  withCString fileType (\ft -> withALikeLen fileData (\size fd -> c'loadImageFromMemory ft fd (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= popTLike

loadImageFromTexture :: (PLike Texture texture, PLike Image image) => texture -> IO image
loadImageFromTexture tex = withTLike tex c'loadImageFromTexture >>= popTLike

loadImageFromScreen :: (PLike Image image) => IO image
loadImageFromScreen = c'loadImageFromScreen >>= popTLike

isImageValid :: (PLike Image image) => image -> IO Bool
isImageValid image = toBool <$> withTLike image c'isImageValid

exportImage :: (PLike Image image, StringLike string) => image -> string -> IO Bool
exportImage image fileName = toBool <$> withTLike image (withTLike fileName . c'exportImage)

exportImageToMemory :: (PLike Image image, PALike CUChar contents) => image -> String -> IO contents
exportImageToMemory image fileType =
  withTLike
    image
    ( \i ->
        withCString
          fileType
          ( \t ->
              withFreeable
                0
                ( \s -> do
                    bytes <- c'exportImageToMemory i t s
                    size <- fromIntegral <$> peek s
                    popALike size bytes
                )
          )
    )

exportImageAsCode :: (PLike Image image, StringLike string) => image -> string -> IO Bool
exportImageAsCode image fileName =
  toBool <$> withTLike image (withTLike fileName . c'exportImageAsCode)

genImageColor :: (PLike Image image) => Int -> Int -> Color -> IO image
genImageColor width height color =
  withFreeable color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= popTLike

genImageGradientLinear :: (PLike Image image) => Int -> Int -> Int -> Color -> Color -> IO image
genImageGradientLinear width height direction start end =
  withFreeable start (withFreeable end . c'genImageGradientLinear (fromIntegral width) (fromIntegral height) (fromIntegral direction)) >>= popTLike

genImageGradientRadial :: (PLike Image image) => Int -> Int -> Float -> Color -> Color -> IO image
genImageGradientRadial width height density inner outer =
  withFreeable inner (withFreeable outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= popTLike

genImageGradientSquare :: (PLike Image image) => Int -> Int -> Float -> Color -> Color -> IO image
genImageGradientSquare width height density inner outer =
  withFreeable inner (withFreeable outer . c'genImageGradientSquare (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= popTLike

genImageChecked :: (PLike Image image) => Int -> Int -> Int -> Int -> Color -> Color -> IO image
genImageChecked width height checksX checksY col1 col2 =
  withFreeable col1 (withFreeable col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= popTLike

genImageWhiteNoise :: (PLike Image image) => Int -> Int -> Float -> IO image
genImageWhiteNoise width height factor =
  c'genImageWhiteNoise (fromIntegral width) (fromIntegral height) (realToFrac factor) >>= popTLike

genImagePerlinNoise :: (PLike Image image) => Int -> Int -> Int -> Int -> Float -> IO image
genImagePerlinNoise width height offsetX offsetY scale = c'genImagePerlinNoise (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY) (realToFrac scale) >>= popTLike

genImageCellular :: (PLike Image image) => Int -> Int -> Int -> IO image
genImageCellular width height tileSize =
  c'genImageCellular (fromIntegral width) (fromIntegral height) (fromIntegral tileSize) >>= popTLike

genImageText :: (PLike Image image) => Int -> Int -> String -> IO image
genImageText width height text =
  withCString text (c'genImageText (fromIntegral width) (fromIntegral height)) >>= popTLike

imageFromImage :: (PLike Image image) => image -> Rectangle -> IO image
imageFromImage image rect = withTLike image (withFreeable rect . c'imageFromImage) >>= popTLike

imageFromChannel :: (PLike Image image) => image -> Int -> IO image
imageFromChannel image channel = withTLike image (\i -> c'imageFromChannel i (fromIntegral channel)) >>= popTLike

imageText :: (StringLike string, PLike Image image) => string -> Int -> Color -> IO image
imageText text fontSize color =
  withTLike text (\t -> withFreeable color (c'imageText t (fromIntegral fontSize))) >>= popTLike

imageTextEx :: (PLike Font font, StringLike string, PLike Image image) => font -> string -> Float -> Float -> Color -> IO image
imageTextEx font text fontSize spacing tint =
  withTLike font (\f -> withTLike text (\t -> withFreeable tint (c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing)))) >>= popTLike

imageFormat :: (PLike Image image, Mutable image mut) => image -> PixelFormat -> IO mut
imageFormat image newFormat =
  withTLike image (\i -> c'imageFormat i (fromIntegral $ fromEnum newFormat) >> peekMutated image i)

imageToPOT :: (PLike Image image, Mutable image mut) => image -> Color -> IO mut
imageToPOT image color = withTLike image (\i -> withFreeable color (c'imageToPOT i) >> peekMutated image i)

imageCrop :: (PLike Image image, Mutable image mut) => image -> Rectangle -> IO mut
imageCrop image crop = withTLike image (\i -> withFreeable crop (c'imageCrop i) >> peekMutated image i)

imageAlphaCrop :: (PLike Image image, Mutable image mut) => image -> Float -> IO mut
imageAlphaCrop image threshold = withTLike image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peekMutated image i)

imageAlphaClear :: (PLike Image image, Mutable image mut) => image -> Color -> Float -> IO mut
imageAlphaClear image color threshold = withTLike image (\i -> withFreeable color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peekMutated image i))

imageAlphaMask :: (PLike Image image1, PLike Image image2, Mutable image1 mut) => image1 -> image2 -> IO mut
imageAlphaMask image alphaMask = withTLike image (\i -> withTLike alphaMask (c'imageAlphaMask i) >> peekMutated image i)

imageAlphaPremultiply :: (PLike Image image, Mutable image mut) => image -> IO mut
imageAlphaPremultiply image = withTLike image (\i -> c'imageAlphaPremultiply i >> peekMutated image i)

imageBlurGaussian :: (PLike Image image, Mutable image mut) => image -> Int -> IO mut
imageBlurGaussian image blurSize = withTLike image (\i -> c'imageBlurGaussian i (fromIntegral blurSize) >> peekMutated image i)

imageKernelConvolution :: (PLike Image image, PALike CFloat kernel, Mutable image mut) => image -> kernel -> IO mut
imageKernelConvolution image kernel = withTLike image (\i -> withALikeLen kernel (\l k -> c'imageKernelConvolution i k (fromIntegral l) >> peekMutated image i))

imageResize :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> IO mut
imageResize image newWidth newHeight = withTLike image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peekMutated image i)

imageResizeNN :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> IO mut
imageResizeNN image newWidth newHeight = withTLike image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peekMutated image i)

imageResizeCanvas :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Int -> Int -> Color -> IO mut
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = withTLike image (\i -> withFreeable fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peekMutated image i)

imageMipmaps :: (PLike Image image, Mutable image mut) => image -> IO mut
imageMipmaps image = withTLike image (\i -> c'imageMipmaps i >> peekMutated image i)

imageDither :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Int -> Int -> IO mut
imageDither image rBpp gBpp bBpp aBpp = withTLike image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peekMutated image i)

imageFlipVertical :: (PLike Image image, Mutable image mut) => image -> IO mut
imageFlipVertical image = withTLike image (\i -> c'imageFlipVertical i >> peekMutated image i)

imageFlipHorizontal :: (PLike Image image, Mutable image mut) => image -> IO mut
imageFlipHorizontal image = withTLike image (\i -> c'imageFlipHorizontal i >> peekMutated image i)

imageRotate :: (PLike Image image, Mutable image mut) => image -> Int -> IO mut
imageRotate image degrees = withTLike image (\i -> c'imageRotate i (fromIntegral degrees) >> peekMutated image i)

imageRotateCW :: (PLike Image image, Mutable image mut) => image -> IO mut
imageRotateCW image = withTLike image (\i -> c'imageRotateCW i >> peekMutated image i)

imageRotateCCW :: (PLike Image image, Mutable image mut) => image -> IO mut
imageRotateCCW image = withTLike image (\i -> c'imageRotateCCW i >> peekMutated image i)

imageColorTint :: (PLike Image image, Mutable image mut) => image -> Color -> IO mut
imageColorTint image color = withTLike image (\i -> withFreeable color (c'imageColorTint i) >> peekMutated image i)

imageColorInvert :: (PLike Image image, Mutable image mut) => image -> IO mut
imageColorInvert image = withTLike image (\i -> c'imageColorInvert i >> peekMutated image i)

imageColorGrayscale :: (PLike Image image, Mutable image mut) => image -> IO mut
imageColorGrayscale image = withTLike image (\i -> c'imageColorGrayscale i >> peekMutated image i)

imageColorContrast :: (PLike Image image, Mutable image mut) => image -> Float -> IO mut
imageColorContrast image contrast = withTLike image (\i -> c'imageColorContrast i (realToFrac contrast) >> peekMutated image i)

imageColorBrightness :: (PLike Image image, Mutable image mut) => image -> Int -> IO mut
imageColorBrightness image brightness = withTLike image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peekMutated image i)

imageColorReplace :: (PLike Image image, Mutable image mut) => image -> Color -> Color -> IO mut
imageColorReplace image color replace = withTLike image (\i -> withFreeable color (withFreeable replace . c'imageColorReplace i) >> peekMutated image i)

loadImageColors :: (PLike Image image, PALike Color colors) => image -> IO colors
loadImageColors image =
  withTLike
    image
    ( \i -> do
        w <- fromIntegral <$> peek (p'image'width i)
        h <- fromIntegral <$> peek (p'image'height i)
        popALike (w * h) =<< c'loadImageColors i
    )

loadImagePalette :: (PLike Image image, PALike Color colors) => image -> Int -> IO colors
loadImagePalette image maxPaletteSize =
  withTLike
    image
    ( \i -> do
        (palette, num) <-
          withFreeable
            0
            ( \size -> do
                cols <- c'loadImagePalette i (fromIntegral maxPaletteSize) size
                s <- peek size
                return (cols, s)
            )
        popALike (fromIntegral num) palette
    )

getImageAlphaBorder :: (PLike Image image) => image -> Float -> IO Rectangle
getImageAlphaBorder image threshold = withTLike image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

getImageColor :: (PLike Image image) => image -> Int -> Int -> IO Color
getImageColor image x y = withTLike image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

imageClearBackground :: (PLike Image image, Mutable image mut) => image -> Color -> IO mut
imageClearBackground image color = withTLike image (\i -> withFreeable color (c'imageClearBackground i) >> peekMutated image i)

imageDrawPixel :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Color -> IO mut
imageDrawPixel image x y color = withTLike image (\i -> withFreeable color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peekMutated image i)

imageDrawPixelV :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Color -> IO mut
imageDrawPixelV image position color = withTLike image (\i -> withFreeable position (withFreeable color . c'imageDrawPixelV i) >> peekMutated image i)

imageDrawLine :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Int -> Int -> Color -> IO mut
imageDrawLine image startPosX startPosY endPosX endPosY color = withTLike image (\i -> withFreeable color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peekMutated image i)

imageDrawLineV :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Vector2 -> Color -> IO mut
imageDrawLineV image start end color = withTLike image (\i -> withFreeable start (\s -> withFreeable end (withFreeable color . c'imageDrawLineV i s)) >> peekMutated image i)

imageDrawCircle :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Int -> Color -> IO mut
imageDrawCircle image centerX centerY radius color = withTLike image (\i -> withFreeable color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peekMutated image i)

imageDrawCircleV :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Int -> Color -> IO mut
imageDrawCircleV image center radius color = withTLike image (\i -> withFreeable center (\c -> withFreeable color (c'imageDrawCircleV i c (fromIntegral radius))) >> peekMutated image i)

imageDrawCircleLines :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Int -> Color -> IO mut
imageDrawCircleLines image centerX centerY radius color = withTLike image (\i -> withFreeable color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peekMutated image i)

imageDrawCircleLinesV :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Int -> Color -> IO mut
imageDrawCircleLinesV image center radius color = withTLike image (\i -> withFreeable center (\c -> withFreeable color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peekMutated image i)

imageDrawRectangle :: (PLike Image image, Mutable image mut) => image -> Int -> Int -> Int -> Int -> Color -> IO mut
imageDrawRectangle image posX posY width height color = withTLike image (\i -> withFreeable color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peekMutated image i)

imageDrawRectangleV :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Vector2 -> Color -> IO mut
imageDrawRectangleV image position size color = withTLike image (\i -> withFreeable position (\p -> withFreeable size (withFreeable color . c'imageDrawRectangleV i p)) >> peekMutated image i)

imageDrawRectangleRec :: (PLike Image image, Mutable image mut) => image -> Rectangle -> Color -> IO mut
imageDrawRectangleRec image rectangle color = withTLike image (\i -> withFreeable rectangle (withFreeable color . c'imageDrawRectangleRec i) >> peekMutated image i)

imageDrawRectangleLines :: (PLike Image image, Mutable image mut) => image -> Rectangle -> Int -> Color -> IO mut
imageDrawRectangleLines image rectangle thickness color = withTLike image (\i -> withFreeable rectangle (\r -> withFreeable color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peekMutated image i)

imageDrawTriangle :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Vector2 -> Vector2 -> Color -> IO mut
imageDrawTriangle image v1 v2 v3 color = withTLike image (\i -> withFreeable v1 (\p1 -> withFreeable v2 (\p2 -> withFreeable v3 (\p3 -> withFreeable color (\c -> c'imageDrawTriangle i p1 p2 p3 c)))) >> peekMutated image i)

imageDrawTriangleEx :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Vector2 -> Vector2 -> Color -> Color -> Color -> IO mut
imageDrawTriangleEx image v1 v2 v3 c1 c2 c3 = withTLike image (\i -> withFreeable v1 (\p1 -> withFreeable v2 (\p2 -> withFreeable v3 (\p3 -> withFreeable c1 (\q1 -> withFreeable c2 (\q2 -> withFreeable c3 (\q3 -> c'imageDrawTriangleEx i p1 p2 p3 q1 q2 q3)))))) >> peekMutated image i)

imageDrawTriangleLines :: (PLike Image image, Mutable image mut) => image -> Vector2 -> Vector2 -> Vector2 -> Color -> IO mut
imageDrawTriangleLines image v1 v2 v3 color = withTLike image (\i -> withFreeable v1 (\p1 -> withFreeable v2 (\p2 -> withFreeable v3 (\p3 -> withFreeable color (\c -> c'imageDrawTriangleLines i p1 p2 p3 c)))) >> peekMutated image i)

imageDrawTriangleFan :: (PLike Image image, PALike Vector2 points, Mutable image mut) => image -> points -> Color -> IO mut
imageDrawTriangleFan image points color = withTLike image (\i -> withALikeLen points (\l p -> withFreeable color (c'imageDrawTriangleFan i p (fromIntegral l))) >> peekMutated image i)

imageDrawTriangleStrip :: (PLike Image image, PALike Vector2 points, Mutable image mut) => image -> points -> Color -> IO mut
imageDrawTriangleStrip image points color = withTLike image (\i -> withALikeLen points (\l p -> withFreeable color (c'imageDrawTriangleStrip i p (fromIntegral l))) >> peekMutated image i)

imageDraw :: (PLike Image image1, PLike Image image2, Mutable image1 mut) => image1 -> image2 -> Rectangle -> Rectangle -> Color -> IO mut
imageDraw image source srcRec dstRec tint = withTLike image (\i -> withTLike source (\s -> withFreeable srcRec (\sr -> withFreeable dstRec (withFreeable tint . c'imageDraw i s sr))) >> peekMutated image i)

imageDrawText :: (PLike Image image, StringLike string, Mutable image mut) => image -> string -> Int -> Int -> Int -> Color -> IO mut
imageDrawText image text x y fontSize color = withTLike image (\i -> withTLike text (\t -> withFreeable color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peekMutated image i)

imageDrawTextEx :: (PLike Image image, PLike Font font, StringLike string, Mutable image mut) => image -> font -> string -> Vector2 -> Float -> Float -> Color -> IO mut
imageDrawTextEx image font text position fontSize spacing tint = withTLike image (\i -> withTLike font (\f -> withTLike text (\t -> withFreeable position (\p -> withFreeable tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peekMutated image i)

loadTexture :: (StringLike string, PLike Texture texture) => string -> IO texture
loadTexture fileName = withTLike fileName c'loadTexture >>= popTLike

loadTextureFromImage :: (PLike Image image, PLike Texture texture) => image -> IO texture
loadTextureFromImage image = withTLike image c'loadTextureFromImage >>= popTLike

loadTextureCubemap :: (PLike Image image, PLike Texture texture) => image -> CubemapLayout -> IO texture
loadTextureCubemap image layout = withTLike image (\i -> c'loadTextureCubemap i (fromIntegral $ fromEnum layout)) >>= popTLike

loadRenderTexture :: (PLike RenderTexture renderTexture) => Int -> Int -> IO renderTexture
loadRenderTexture width height = c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= popTLike

isTextureValid :: (PLike Texture texture) => texture -> IO Bool
isTextureValid texture = toBool <$> withTLike texture c'isTextureValid

isRenderTextureValid :: (PLike RenderTexture renderTexture) => renderTexture -> IO Bool
isRenderTextureValid renderTexture = toBool <$> withTLike renderTexture c'isRenderTextureValid

-- | Unloads a `managed` texture from GPU memory (VRAM)
unloadTexture :: (PLike Texture texture) => texture -> WindowResources -> IO ()
unloadTexture texture wr =
  withTLike
    texture
    ( \t -> do
        tId <- peek (p'texture'id t)
        unloadSingleTexture tId wr
    )

-- | Unloads a `managed` render texture from GPU memory (VRAM)
unloadRenderTexture :: (PLike RenderTexture renderTexture) => renderTexture -> WindowResources -> IO ()
unloadRenderTexture renderTexture wr =
  withTLike
    renderTexture
    ( \rt -> do
        tId <- peek (p'texture'id (p'renderTexture'texture rt))
        rtId <- peek (p'renderTexture'id rt)
        unloadSingleTexture tId wr
        unloadSingleFrameBuffer rtId wr
    )

updateTexture :: (PLike Texture texture) => texture -> Ptr () -> IO ()
updateTexture texture pixels = withTLike texture (\t -> c'updateTexture t pixels)

updateTextureRec :: (PLike Texture texture) => texture -> Rectangle -> Ptr () -> IO ()
updateTextureRec texture rect pixels = withTLike texture (\t -> withFreeable rect (\r -> c'updateTextureRec t r pixels))

genTextureMipmaps :: (PLike Texture texture, Mutable texture mut) => texture -> IO mut
genTextureMipmaps texture = withTLike texture (\t -> c'genTextureMipmaps t >> peekMutated texture t)

setTextureFilter :: (PLike Texture texture, Mutable texture mut) => texture -> TextureFilter -> IO mut
setTextureFilter texture filterType = withTLike texture (\t -> c'setTextureFilter t (fromIntegral $ fromEnum filterType) >> peekMutated texture t)

setTextureWrap :: (PLike Texture texture, Mutable texture mut) => texture -> TextureWrap -> IO mut
setTextureWrap texture wrap = withTLike texture (\t -> c'setTextureWrap t (fromIntegral $ fromEnum wrap) >> peekMutated texture t)

drawTexture :: (PLike Texture texture) => texture -> Int -> Int -> Color -> IO ()
drawTexture texture x y tint = withTLike texture (\t -> withFreeable tint (c'drawTexture t (fromIntegral x) (fromIntegral y)))

drawTextureV :: (PLike Texture texture) => texture -> Vector2 -> Color -> IO ()
drawTextureV texture position color = withTLike texture (\t -> withFreeable position (withFreeable color . c'drawTextureV t))

drawTextureEx :: (PLike Texture texture) => texture -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextureEx texture position rotation scale tint = withTLike texture (\t -> withFreeable position (\p -> withFreeable tint (c'drawTextureEx t p (realToFrac rotation) (realToFrac scale))))

drawTextureRec :: (PLike Texture texture) => texture -> Rectangle -> Vector2 -> Color -> IO ()
drawTextureRec texture source position tint = withTLike texture (\t -> withFreeable source (\s -> withFreeable position (withFreeable tint . c'drawTextureRec t s)))

drawTexturePro :: (PLike Texture texture) => texture -> Rectangle -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTexturePro texture source dest origin rotation tint = withTLike texture (\t -> withFreeable source (\s -> withFreeable dest (\d -> withFreeable origin (\o -> withFreeable tint (c'drawTexturePro t s d o (realToFrac rotation))))))

drawTextureNPatch :: (PLike Texture texture) => texture -> NPatchInfo -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTextureNPatch texture nPatchInfo dest origin rotation tint = withTLike texture (\t -> withFreeable nPatchInfo (\n -> withFreeable dest (\d -> withFreeable origin (\o -> withFreeable tint (c'drawTextureNPatch t n d o (realToFrac rotation))))))

fade :: Color -> Float -> Color
fade color alpha = unsafePerformIO $ withFreeable color (\c -> c'fade c (realToFrac alpha)) >>= pop

colorToInt :: Color -> Int
colorToInt color = unsafePerformIO $ fromIntegral <$> withFreeable color c'colorToInt

colorNormalize :: Color -> Vector4
colorNormalize color = unsafePerformIO $ withFreeable color c'colorNormalize >>= pop

colorFromNormalized :: Vector4 -> Color
colorFromNormalized normalized = unsafePerformIO $ withFreeable normalized c'colorFromNormalized >>= pop

colorToHSV :: Color -> Vector3
colorToHSV color = unsafePerformIO $ withFreeable color c'colorToHSV >>= pop

colorFromHSV :: Float -> Float -> Float -> Color
colorFromHSV hue saturation value = unsafePerformIO $ c'colorFromHSV (realToFrac hue) (realToFrac saturation) (realToFrac value) >>= pop

colorTint :: Color -> Color -> Color
colorTint color tint = unsafePerformIO $ withFreeable color (withFreeable tint . c'colorTint) >>= pop

colorBrightness :: Color -> Float -> Color
colorBrightness color brightness = unsafePerformIO $ withFreeable color (\c -> c'colorBrightness c (realToFrac brightness)) >>= pop

colorContrast :: Color -> Float -> Color
colorContrast color contrast = unsafePerformIO $ withFreeable color (\c -> c'colorContrast c (realToFrac contrast)) >>= pop

colorAlpha :: Color -> Float -> Color
colorAlpha color alpha = unsafePerformIO $ withFreeable color (\c -> c'colorAlpha c (realToFrac alpha)) >>= pop

colorAlphaBlend :: Color -> Color -> Color -> Color
colorAlphaBlend dst src tint = unsafePerformIO $ withFreeable dst (\d -> withFreeable src (withFreeable tint . c'colorAlphaBlend d)) >>= pop

colorLerp :: Color -> Color -> Float -> Color
colorLerp color1 color2 factor = unsafePerformIO $ withFreeable color1 (\c1 -> withFreeable color2 (\c2 -> c'colorLerp c1 c2 (realToFrac factor))) >>= pop

getColor :: Integer -> Color
getColor hexValue = unsafePerformIO $ c'getColor (fromIntegral hexValue) >>= pop

getPixelColor :: Ptr () -> PixelFormat -> IO Color
getPixelColor srcPtr format = c'getPixelColor srcPtr (fromIntegral $ fromEnum format) >>= pop

setPixelColor :: Ptr () -> Color -> PixelFormat -> IO ()
setPixelColor dstPtr color format = withFreeable color (\c -> c'setPixelColor dstPtr c (fromIntegral $ fromEnum format))

getPixelDataSize :: Int -> Int -> PixelFormat -> Int
getPixelDataSize width height format = I.getPixelDataSize width height (fromEnum format)
