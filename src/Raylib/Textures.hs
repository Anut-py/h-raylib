{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS -Wall #-}

module Raylib.Textures where

import Foreign
  ( Ptr,
    Storable (peek, sizeOf),
    peekArray,
    toBool,
    with,
    withArrayLen,
  )
import Foreign.C (CUChar, withCString)
import GHC.IO (unsafePerformIO)
import Raylib.Native
  ( c'colorAlpha,
    c'colorAlphaBlend,
    c'colorBrightness,
    c'colorContrast,
    c'colorFromHSV,
    c'colorFromNormalized,
    c'colorNormalize,
    c'colorTint,
    c'colorToHSV,
    c'colorToInt,
    c'drawTexture,
    c'drawTextureEx,
    c'drawTextureNPatch,
    c'drawTexturePro,
    c'drawTextureRec,
    c'drawTextureV,
    c'exportImage,
    c'exportImageAsCode,
    c'fade,
    c'genImageCellular,
    c'genImageChecked,
    c'genImageColor,
    c'genImageGradientH,
    c'genImageGradientRadial,
    c'genImageGradientV,
    c'genImagePerlinNoise,
    c'genImageText,
    c'genImageWhiteNoise,
    c'genTextureMipmaps,
    c'getColor,
    c'getImageAlphaBorder,
    c'getImageColor,
    c'getPixelColor,
    c'imageAlphaClear,
    c'imageAlphaCrop,
    c'imageAlphaMask,
    c'imageAlphaPremultiply,
    c'imageBlurGaussian,
    c'imageClearBackground,
    c'imageColorBrightness,
    c'imageColorContrast,
    c'imageColorGrayscale,
    c'imageColorInvert,
    c'imageColorReplace,
    c'imageColorTint,
    c'imageCopy,
    c'imageCrop,
    c'imageDither,
    c'imageDraw,
    c'imageDrawCircle,
    c'imageDrawCircleLines,
    c'imageDrawCircleLinesV,
    c'imageDrawCircleV,
    c'imageDrawLine,
    c'imageDrawLineV,
    c'imageDrawPixel,
    c'imageDrawPixelV,
    c'imageDrawRectangle,
    c'imageDrawRectangleLines,
    c'imageDrawRectangleRec,
    c'imageDrawRectangleV,
    c'imageDrawText,
    c'imageDrawTextEx,
    c'imageFlipHorizontal,
    c'imageFlipVertical,
    c'imageFormat,
    c'imageFromImage,
    c'imageMipmaps,
    c'imageResize,
    c'imageResizeCanvas,
    c'imageResizeNN,
    c'imageRotateCCW,
    c'imageRotateCW,
    c'imageText,
    c'imageTextEx,
    c'imageToPOT,
    c'loadImage,
    c'loadImageAnim,
    c'loadImageColors,
    c'loadImageFromMemory,
    c'loadImageFromScreen,
    c'loadImageFromTexture,
    c'loadImagePalette,
    c'loadImageRaw,
    c'loadRenderTexture,
    c'loadTexture,
    c'loadTextureCubemap,
    c'loadTextureFromImage,
    c'setPixelColor,
    c'setTextureFilter,
    c'setTextureWrap,
    c'unloadImage,
    c'unloadRenderTexture,
    c'unloadTexture,
    c'updateTexture,
    c'updateTextureRec,
  )
import Raylib.Types
  ( Color,
    CubemapLayout,
    Font,
    Image (image'height, image'width),
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
  )
import Raylib.Util (pop)

loadImage :: String -> IO Raylib.Types.Image
loadImage fileName = withCString fileName c'loadImage >>= pop

loadImageRaw :: String -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
loadImageRaw fileName width height format headerSize =
  withCString fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) (fromIntegral headerSize)) >>= pop

-- | Returns the final image and the framees in a tuple, e.g. @(img, 18)@
loadImageAnim :: String -> IO (Raylib.Types.Image, Int)
loadImageAnim fileName =
  with
    0
    ( \frames ->
        withCString
          fileName
          ( \fn -> do
              img <- c'loadImageAnim fn frames >>= pop
              frameNum <- fromIntegral <$> peek frames
              return (img, frameNum)
          )
    )

loadImageFromMemory :: String -> [Integer] -> IO Raylib.Types.Image
loadImageFromMemory fileType fileData =
  withCString fileType (\ft -> withArrayLen (map fromIntegral fileData) (\size fd -> c'loadImageFromMemory ft fd (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

loadImageFromTexture :: Raylib.Types.Texture -> IO Raylib.Types.Image
loadImageFromTexture tex = with tex c'loadImageFromTexture >>= pop

loadImageFromScreen :: IO Raylib.Types.Image
loadImageFromScreen = c'loadImageFromScreen >>= pop

unloadImage :: Raylib.Types.Image -> IO ()
unloadImage image = with image c'unloadImage

exportImage :: Raylib.Types.Image -> String -> IO Bool
exportImage image fileName = toBool <$> with image (withCString fileName . c'exportImage)

exportImageAsCode :: Raylib.Types.Image -> String -> IO Bool
exportImageAsCode image fileName =
  toBool <$> with image (withCString fileName . c'exportImageAsCode)

genImageColor :: Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageColor width height color =
  with color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientV :: Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientV width height top bottom =
  with top (with bottom . c'genImageGradientV (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientH :: Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientH width height left right =
  with left (with right . c'genImageGradientH (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientRadial :: Int -> Int -> Float -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientRadial width height density inner outer =
  with inner (with outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

genImageChecked :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageChecked width height checksX checksY col1 col2 =
  with col1 (with col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= pop

genImageWhiteNoise :: Int -> Int -> Float -> IO Raylib.Types.Image
genImageWhiteNoise width height factor =
  c'genImageWhiteNoise (fromIntegral width) (fromIntegral height) (realToFrac factor) >>= pop

genImagePerlinNoise :: Int -> Int -> Int -> Int -> Float -> IO Raylib.Types.Image
genImagePerlinNoise width height offsetX offsetY scale = c'genImagePerlinNoise (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY) (realToFrac scale) >>= pop

genImageCellular :: Int -> Int -> Int -> IO Raylib.Types.Image
genImageCellular width height tileSize =
  c'genImageCellular (fromIntegral width) (fromIntegral height) (fromIntegral tileSize) >>= pop

genImageText :: Int -> Int -> String -> IO Raylib.Types.Image
genImageText width height text =
  withCString text (c'genImageText (fromIntegral width) (fromIntegral height)) >>= pop

imageCopy :: Raylib.Types.Image -> IO Raylib.Types.Image
imageCopy image = with image c'imageCopy >>= pop

imageFromImage :: Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image
imageFromImage image rect = with image (with rect . c'imageFromImage) >>= pop

imageText :: String -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageText text fontSize color =
  withCString text (\t -> with color $ c'imageText t (fromIntegral fontSize)) >>= pop

imageTextEx :: Raylib.Types.Font -> String -> Float -> Float -> Raylib.Types.Color -> IO Raylib.Types.Image
imageTextEx font text fontSize spacing tint =
  with font (\f -> withCString text (\t -> with tint $ c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

imageFormat :: Raylib.Types.Image -> PixelFormat -> IO Raylib.Types.Image
imageFormat image newFormat =
  with image (\i -> c'imageFormat i (fromIntegral $ fromEnum newFormat) >> peek i)

imageToPOT :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageToPOT image color = with image (\i -> with color (c'imageToPOT i) >> peek i)

imageCrop :: Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image
imageCrop image crop = with image (\i -> with crop (c'imageCrop i) >> peek i)

imageAlphaCrop :: Raylib.Types.Image -> Float -> IO Raylib.Types.Image
imageAlphaCrop image threshold = with image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peek i)

imageAlphaClear :: Raylib.Types.Image -> Raylib.Types.Color -> Float -> IO Raylib.Types.Image
imageAlphaClear image color threshold = with image (\i -> with color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peek i))

imageAlphaMask :: Raylib.Types.Image -> Raylib.Types.Image -> IO Raylib.Types.Image
imageAlphaMask image alphaMask = with image (\i -> with alphaMask (c'imageAlphaMask i) >> peek i)

imageAlphaPremultiply :: Raylib.Types.Image -> IO Raylib.Types.Image
imageAlphaPremultiply image = with image (\i -> c'imageAlphaPremultiply i >> peek i)

imageBlurGaussian :: Raylib.Types.Image -> Int -> IO Raylib.Types.Image
imageBlurGaussian image blurSize = with image (\i -> c'imageBlurGaussian i (fromIntegral blurSize) >> peek i)

imageResize :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Image
imageResize image newWidth newHeight = with image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

imageResizeNN :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Image
imageResizeNN image newWidth newHeight = with image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

imageResizeCanvas :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = with image (\i -> with fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peek i)

imageMipmaps :: Raylib.Types.Image -> IO Raylib.Types.Image
imageMipmaps image = with image (\i -> c'imageMipmaps i >> peek i)

imageDither :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
imageDither image rBpp gBpp bBpp aBpp = with image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peek i)

imageFlipVertical :: Raylib.Types.Image -> IO Raylib.Types.Image
imageFlipVertical image = with image (\i -> c'imageFlipVertical i >> peek i)

imageFlipHorizontal :: Raylib.Types.Image -> IO Raylib.Types.Image
imageFlipHorizontal image = with image (\i -> c'imageFlipHorizontal i >> peek i)

imageRotateCW :: Raylib.Types.Image -> IO Raylib.Types.Image
imageRotateCW image = with image (\i -> c'imageRotateCW i >> peek i)

imageRotateCCW :: Raylib.Types.Image -> IO Raylib.Types.Image
imageRotateCCW image = with image (\i -> c'imageRotateCCW i >> peek i)

imageColorTint :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageColorTint image color = with image (\i -> with color (c'imageColorTint i) >> peek i)

imageColorInvert :: Raylib.Types.Image -> IO Raylib.Types.Image
imageColorInvert image = with image (\i -> c'imageColorInvert i >> peek i)

imageColorGrayscale :: Raylib.Types.Image -> IO Raylib.Types.Image
imageColorGrayscale image = with image (\i -> c'imageColorGrayscale i >> peek i)

imageColorContrast :: Raylib.Types.Image -> Float -> IO Raylib.Types.Image
imageColorContrast image contrast = with image (\i -> c'imageColorContrast i (realToFrac contrast) >> peek i)

imageColorBrightness :: Raylib.Types.Image -> Int -> IO Raylib.Types.Image
imageColorBrightness image brightness = with image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peek i)

imageColorReplace :: Raylib.Types.Image -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
imageColorReplace image color replace = with image (\i -> with color (with replace . c'imageColorReplace i) >> peek i)

loadImageColors :: Raylib.Types.Image -> IO [Raylib.Types.Color]
loadImageColors image =
  with
    image
    ( \i -> do
        colors <- c'loadImageColors i
        colArray <- peekArray (fromIntegral $ Raylib.Types.image'width image * Raylib.Types.image'height image) colors
        unloadImageColors colors
        return colArray
    )

loadImagePalette :: Raylib.Types.Image -> Int -> IO [Raylib.Types.Color]
loadImagePalette image maxPaletteSize =
  with
    image
    ( \i -> do
        (palette, num) <-
          with
            0
            ( \size -> do
                cols <- c'loadImagePalette i (fromIntegral maxPaletteSize) size
                s <- peek size
                return (cols, s)
            )
        colArray <- peekArray (fromIntegral num) palette
        unloadImagePalette palette
        return colArray
    )

foreign import ccall safe "raylib.h UnloadImageColors"
  unloadImageColors ::
    Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h UnloadImagePalette"
  unloadImagePalette ::
    Ptr Raylib.Types.Color -> IO ()

getImageAlphaBorder :: Raylib.Types.Image -> Float -> IO Raylib.Types.Rectangle
getImageAlphaBorder image threshold = with image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

getImageColor :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Color
getImageColor image x y = with image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

imageClearBackground :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageClearBackground image color = with image (\i -> with color (c'imageClearBackground i) >> peek i)

imageDrawPixel :: Raylib.Types.Image -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawPixel image x y color = with image (\i -> with color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peek i)

imageDrawPixelV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawPixelV image position color = with image (\i -> with position (with color . c'imageDrawPixelV i) >> peek i)

imageDrawLine :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawLine image startPosX startPosY endPosX endPosY color = with image (\i -> with color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peek i)

imageDrawLineV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawLineV image start end color = with image (\i -> with start (\s -> with end (with color . c'imageDrawLineV i s)) >> peek i)

imageDrawCircle :: Raylib.Types.Image -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircle image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

imageDrawCircleV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleV i c (fromIntegral radius))) >> peek i)

imageDrawCircleLines :: Raylib.Types.Image -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleLines image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

imageDrawCircleLinesV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleLinesV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peek i)

imageDrawRectangle :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangle image posX posY width height color = with image (\i -> with color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peek i)

imageDrawRectangleV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleV image position size color = with image (\i -> with position (\p -> with size (with color . c'imageDrawRectangleV i p)) >> peek i)

imageDrawRectangleRec :: Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleRec image rectangle color = with image (\i -> with rectangle (with color . c'imageDrawRectangleRec i) >> peek i)

imageDrawRectangleLines :: Raylib.Types.Image -> Raylib.Types.Rectangle -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleLines image rectangle thickness color = with image (\i -> with rectangle (\r -> with color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peek i)

imageDraw :: Raylib.Types.Image -> Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDraw image source srcRec dstRec tint = with image (\i -> with source (\s -> with srcRec (\sr -> with dstRec (with tint . c'imageDraw i s sr))) >> peek i)

imageDrawText :: Raylib.Types.Image -> String -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawText image text x y fontSize color = with image (\i -> withCString text (\t -> with color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peek i)

imageDrawTextEx :: Raylib.Types.Image -> Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawTextEx image font text position fontSize spacing tint = with image (\i -> with font (\f -> withCString text (\t -> with position (\p -> with tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peek i)

loadTexture :: String -> IO Raylib.Types.Texture
loadTexture fileName = withCString fileName c'loadTexture >>= pop

loadTextureFromImage :: Raylib.Types.Image -> IO Raylib.Types.Texture
loadTextureFromImage image = with image c'loadTextureFromImage >>= pop

loadTextureCubemap :: Raylib.Types.Image -> CubemapLayout -> IO Raylib.Types.Texture
loadTextureCubemap image layout = with image (\i -> c'loadTextureCubemap i (fromIntegral $ fromEnum layout)) >>= pop

loadRenderTexture :: Int -> Int -> IO Raylib.Types.RenderTexture
loadRenderTexture width height = c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= pop

unloadTexture :: Raylib.Types.Texture -> IO ()
unloadTexture texture = with texture c'unloadTexture

unloadRenderTexture :: Raylib.Types.RenderTexture -> IO ()
unloadRenderTexture target = with target c'unloadRenderTexture

updateTexture :: Raylib.Types.Texture -> Ptr () -> IO Raylib.Types.Texture
updateTexture texture pixels = with texture (\t -> c'updateTexture t pixels >> peek t)

updateTextureRec :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Ptr () -> IO Raylib.Types.Texture
updateTextureRec texture rect pixels = with texture (\t -> with rect (\r -> c'updateTextureRec t r pixels) >> peek t)

genTextureMipmaps :: Raylib.Types.Texture -> IO Raylib.Types.Texture
genTextureMipmaps texture = with texture (\t -> c'genTextureMipmaps t >> peek t)

setTextureFilter :: Raylib.Types.Texture -> TextureFilter -> IO Raylib.Types.Texture
setTextureFilter texture filterType = with texture (\t -> c'setTextureFilter t (fromIntegral $ fromEnum filterType) >> peek t)

setTextureWrap :: Raylib.Types.Texture -> TextureWrap -> IO Raylib.Types.Texture
setTextureWrap texture wrap = with texture (\t -> c'setTextureWrap t (fromIntegral $ fromEnum wrap) >> peek t)

drawTexture :: Raylib.Types.Texture -> Int -> Int -> Raylib.Types.Color -> IO ()
drawTexture texture x y tint = with texture (\t -> with tint (c'drawTexture t (fromIntegral x) (fromIntegral y)))

drawTextureV :: Raylib.Types.Texture -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTextureV texture position color = with texture (\t -> with position (with color . c'drawTextureV t))

drawTextureEx :: Raylib.Types.Texture -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextureEx texture position rotation scale tint = with texture (\t -> with position (\p -> with tint (c'drawTextureEx t p (realToFrac rotation) (realToFrac scale))))

drawTextureRec :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTextureRec texture source position tint = with texture (\t -> with source (\s -> with position (with tint . c'drawTextureRec t s)))

drawTexturePro :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTexturePro texture source dest origin rotation tint = with texture (\t -> with source (\s -> with dest (\d -> with origin (\o -> with tint (c'drawTexturePro t s d o (realToFrac rotation))))))

drawTextureNPatch :: Raylib.Types.Texture -> Raylib.Types.NPatchInfo -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTextureNPatch texture nPatchInfo dest origin rotation tint = with texture (\t -> with nPatchInfo (\n -> with dest (\d -> with origin (\o -> with tint (c'drawTextureNPatch t n d o (realToFrac rotation))))))

fade :: Raylib.Types.Color -> Float -> Raylib.Types.Color
fade color alpha = unsafePerformIO $ with color (\c -> c'fade c (realToFrac alpha)) >>= pop

colorToInt :: Raylib.Types.Color -> Int
colorToInt color = unsafePerformIO $ fromIntegral <$> with color c'colorToInt

colorNormalize :: Raylib.Types.Color -> Raylib.Types.Vector4
colorNormalize color = unsafePerformIO $ with color c'colorNormalize >>= pop

colorFromNormalized :: Raylib.Types.Vector4 -> Raylib.Types.Color
colorFromNormalized normalized = unsafePerformIO $ with normalized c'colorFromNormalized >>= pop

colorToHSV :: Raylib.Types.Color -> Raylib.Types.Vector3
colorToHSV color = unsafePerformIO $ with color c'colorToHSV >>= pop

colorFromHSV :: Float -> Float -> Float -> Raylib.Types.Color
colorFromHSV hue saturation value = unsafePerformIO $ c'colorFromHSV (realToFrac hue) (realToFrac saturation) (realToFrac value) >>= pop

colorTint :: Color -> Color -> Raylib.Types.Color
colorTint color tint = unsafePerformIO $ with color (with tint . c'colorTint) >>= pop

colorBrightness :: Color -> Float -> Raylib.Types.Color
colorBrightness color brightness = unsafePerformIO $ with color (\c -> c'colorBrightness c (realToFrac brightness)) >>= pop

colorContrast :: Color -> Float -> Raylib.Types.Color
colorContrast color contrast = unsafePerformIO $ with color (\c -> c'colorContrast c (realToFrac contrast)) >>= pop

colorAlpha :: Raylib.Types.Color -> Float -> Raylib.Types.Color
colorAlpha color alpha = unsafePerformIO $ with color (\c -> c'colorAlpha c (realToFrac alpha)) >>= pop

colorAlphaBlend :: Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color
colorAlphaBlend dst src tint = unsafePerformIO $ with dst (\d -> with src (with tint . c'colorAlphaBlend d)) >>= pop

getColor :: Integer -> Raylib.Types.Color
getColor hexValue = unsafePerformIO $ c'getColor (fromIntegral hexValue) >>= pop

getPixelColor :: Ptr () -> PixelFormat -> IO Raylib.Types.Color
getPixelColor srcPtr format = c'getPixelColor srcPtr (fromIntegral $ fromEnum format) >>= pop

setPixelColor :: Ptr () -> Raylib.Types.Color -> PixelFormat -> IO ()
setPixelColor dstPtr color format = with color (\c -> c'setPixelColor dstPtr c (fromIntegral $ fromEnum format))
