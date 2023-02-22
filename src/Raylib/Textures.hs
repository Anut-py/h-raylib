{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS -Wall #-}

module Raylib.Textures where

import Control.Monad ((<=<))
import Foreign
  ( Ptr,
    Storable (peek, sizeOf),
    toBool,
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
    c'isImageReady,
    c'isRenderTextureReady,
    c'isTextureReady,
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
    RenderTexture (renderTexture'id, renderTexture'texture),
    Texture (texture'id),
    TextureFilter,
    TextureWrap,
    Vector2,
    Vector3,
    Vector4,
  )
import Raylib.Util
  ( pop,
    popCArray,
    withFreeable,
  )
import Raylib.Internal (addTextureId, addFrameBuffer)

loadImage :: String -> IO Raylib.Types.Image
loadImage fileName = withCString fileName c'loadImage >>= pop

loadImageRaw :: String -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
loadImageRaw fileName width height format headerSize =
  withCString fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) (fromIntegral headerSize)) >>= pop

-- | Returns the animation and the frames in a tuple
loadImageAnim :: String -> IO (Raylib.Types.Image, Int)
loadImageAnim fileName =
  withFreeable
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
loadImageFromTexture tex = withFreeable tex c'loadImageFromTexture >>= pop

loadImageFromScreen :: IO Raylib.Types.Image
loadImageFromScreen = c'loadImageFromScreen >>= pop

isImageReady :: Image -> IO Bool
isImageReady image = toBool <$> withFreeable image c'isImageReady

exportImage :: Raylib.Types.Image -> String -> IO Bool
exportImage image fileName = toBool <$> withFreeable image (withCString fileName . c'exportImage)

exportImageAsCode :: Raylib.Types.Image -> String -> IO Bool
exportImageAsCode image fileName =
  toBool <$> withFreeable image (withCString fileName . c'exportImageAsCode)

genImageColor :: Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageColor width height color =
  withFreeable color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientV :: Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientV width height top bottom =
  withFreeable top (withFreeable bottom . c'genImageGradientV (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientH :: Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientH width height left right =
  withFreeable left (withFreeable right . c'genImageGradientH (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientRadial :: Int -> Int -> Float -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientRadial width height density inner outer =
  withFreeable inner (withFreeable outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

genImageChecked :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageChecked width height checksX checksY col1 col2 =
  withFreeable col1 (withFreeable col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= pop

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
imageCopy image = withFreeable image c'imageCopy >>= pop

imageFromImage :: Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image
imageFromImage image rect = withFreeable image (withFreeable rect . c'imageFromImage) >>= pop

imageText :: String -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageText text fontSize color =
  withCString text (\t -> withFreeable color $ c'imageText t (fromIntegral fontSize)) >>= pop

imageTextEx :: Raylib.Types.Font -> String -> Float -> Float -> Raylib.Types.Color -> IO Raylib.Types.Image
imageTextEx font text fontSize spacing tint =
  withFreeable font (\f -> withCString text (\t -> withFreeable tint $ c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

imageFormat :: Raylib.Types.Image -> PixelFormat -> IO Raylib.Types.Image
imageFormat image newFormat =
  withFreeable image (\i -> c'imageFormat i (fromIntegral $ fromEnum newFormat) >> peek i)

imageToPOT :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageToPOT image color = withFreeable image (\i -> withFreeable color (c'imageToPOT i) >> peek i)

imageCrop :: Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image
imageCrop image crop = withFreeable image (\i -> withFreeable crop (c'imageCrop i) >> peek i)

imageAlphaCrop :: Raylib.Types.Image -> Float -> IO Raylib.Types.Image
imageAlphaCrop image threshold = withFreeable image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peek i)

imageAlphaClear :: Raylib.Types.Image -> Raylib.Types.Color -> Float -> IO Raylib.Types.Image
imageAlphaClear image color threshold = withFreeable image (\i -> withFreeable color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peek i))

imageAlphaMask :: Raylib.Types.Image -> Raylib.Types.Image -> IO Raylib.Types.Image
imageAlphaMask image alphaMask = withFreeable image (\i -> withFreeable alphaMask (c'imageAlphaMask i) >> peek i)

imageAlphaPremultiply :: Raylib.Types.Image -> IO Raylib.Types.Image
imageAlphaPremultiply image = withFreeable image (\i -> c'imageAlphaPremultiply i >> peek i)

imageBlurGaussian :: Raylib.Types.Image -> Int -> IO Raylib.Types.Image
imageBlurGaussian image blurSize = withFreeable image (\i -> c'imageBlurGaussian i (fromIntegral blurSize) >> peek i)

imageResize :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Image
imageResize image newWidth newHeight = withFreeable image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

imageResizeNN :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Image
imageResizeNN image newWidth newHeight = withFreeable image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

imageResizeCanvas :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = withFreeable image (\i -> withFreeable fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peek i)

imageMipmaps :: Raylib.Types.Image -> IO Raylib.Types.Image
imageMipmaps image = withFreeable image (\i -> c'imageMipmaps i >> peek i)

imageDither :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
imageDither image rBpp gBpp bBpp aBpp = withFreeable image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peek i)

imageFlipVertical :: Raylib.Types.Image -> IO Raylib.Types.Image
imageFlipVertical image = withFreeable image (\i -> c'imageFlipVertical i >> peek i)

imageFlipHorizontal :: Raylib.Types.Image -> IO Raylib.Types.Image
imageFlipHorizontal image = withFreeable image (\i -> c'imageFlipHorizontal i >> peek i)

imageRotateCW :: Raylib.Types.Image -> IO Raylib.Types.Image
imageRotateCW image = withFreeable image (\i -> c'imageRotateCW i >> peek i)

imageRotateCCW :: Raylib.Types.Image -> IO Raylib.Types.Image
imageRotateCCW image = withFreeable image (\i -> c'imageRotateCCW i >> peek i)

imageColorTint :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageColorTint image color = withFreeable image (\i -> withFreeable color (c'imageColorTint i) >> peek i)

imageColorInvert :: Raylib.Types.Image -> IO Raylib.Types.Image
imageColorInvert image = withFreeable image (\i -> c'imageColorInvert i >> peek i)

imageColorGrayscale :: Raylib.Types.Image -> IO Raylib.Types.Image
imageColorGrayscale image = withFreeable image (\i -> c'imageColorGrayscale i >> peek i)

imageColorContrast :: Raylib.Types.Image -> Float -> IO Raylib.Types.Image
imageColorContrast image contrast = withFreeable image (\i -> c'imageColorContrast i (realToFrac contrast) >> peek i)

imageColorBrightness :: Raylib.Types.Image -> Int -> IO Raylib.Types.Image
imageColorBrightness image brightness = withFreeable image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peek i)

imageColorReplace :: Raylib.Types.Image -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
imageColorReplace image color replace = withFreeable image (\i -> withFreeable color (withFreeable replace . c'imageColorReplace i) >> peek i)

loadImageColors :: Raylib.Types.Image -> IO [Raylib.Types.Color]
loadImageColors image =
  withFreeable
    image
    (popCArray (fromIntegral $ Raylib.Types.image'width image * Raylib.Types.image'height image) <=< c'loadImageColors)

loadImagePalette :: Raylib.Types.Image -> Int -> IO [Raylib.Types.Color]
loadImagePalette image maxPaletteSize =
  withFreeable
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
        popCArray (fromIntegral num) palette
    )

getImageAlphaBorder :: Raylib.Types.Image -> Float -> IO Raylib.Types.Rectangle
getImageAlphaBorder image threshold = withFreeable image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

getImageColor :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Color
getImageColor image x y = withFreeable image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

imageClearBackground :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageClearBackground image color = withFreeable image (\i -> withFreeable color (c'imageClearBackground i) >> peek i)

imageDrawPixel :: Raylib.Types.Image -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawPixel image x y color = withFreeable image (\i -> withFreeable color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peek i)

imageDrawPixelV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawPixelV image position color = withFreeable image (\i -> withFreeable position (withFreeable color . c'imageDrawPixelV i) >> peek i)

imageDrawLine :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawLine image startPosX startPosY endPosX endPosY color = withFreeable image (\i -> withFreeable color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peek i)

imageDrawLineV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawLineV image start end color = withFreeable image (\i -> withFreeable start (\s -> withFreeable end (withFreeable color . c'imageDrawLineV i s)) >> peek i)

imageDrawCircle :: Raylib.Types.Image -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircle image centerX centerY radius color = withFreeable image (\i -> withFreeable color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

imageDrawCircleV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleV image center radius color = withFreeable image (\i -> withFreeable center (\c -> withFreeable color (c'imageDrawCircleV i c (fromIntegral radius))) >> peek i)

imageDrawCircleLines :: Raylib.Types.Image -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleLines image centerX centerY radius color = withFreeable image (\i -> withFreeable color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

imageDrawCircleLinesV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleLinesV image center radius color = withFreeable image (\i -> withFreeable center (\c -> withFreeable color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peek i)

imageDrawRectangle :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangle image posX posY width height color = withFreeable image (\i -> withFreeable color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peek i)

imageDrawRectangleV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleV image position size color = withFreeable image (\i -> withFreeable position (\p -> withFreeable size (withFreeable color . c'imageDrawRectangleV i p)) >> peek i)

imageDrawRectangleRec :: Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleRec image rectangle color = withFreeable image (\i -> withFreeable rectangle (withFreeable color . c'imageDrawRectangleRec i) >> peek i)

imageDrawRectangleLines :: Raylib.Types.Image -> Raylib.Types.Rectangle -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleLines image rectangle thickness color = withFreeable image (\i -> withFreeable rectangle (\r -> withFreeable color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peek i)

imageDraw :: Raylib.Types.Image -> Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDraw image source srcRec dstRec tint = withFreeable image (\i -> withFreeable source (\s -> withFreeable srcRec (\sr -> withFreeable dstRec (withFreeable tint . c'imageDraw i s sr))) >> peek i)

imageDrawText :: Raylib.Types.Image -> String -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawText image text x y fontSize color = withFreeable image (\i -> withCString text (\t -> withFreeable color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peek i)

imageDrawTextEx :: Raylib.Types.Image -> Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawTextEx image font text position fontSize spacing tint = withFreeable image (\i -> withFreeable font (\f -> withCString text (\t -> withFreeable position (\p -> withFreeable tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peek i)

loadTexture :: String -> IO Raylib.Types.Texture
loadTexture fileName = do
  texture <- withCString fileName c'loadTexture >>= pop
  addTextureId $ texture'id texture
  return texture

loadTextureFromImage :: Raylib.Types.Image -> IO Raylib.Types.Texture
loadTextureFromImage image = do
  texture <- withFreeable image c'loadTextureFromImage >>= pop
  addTextureId $ texture'id texture
  return texture

loadTextureCubemap :: Raylib.Types.Image -> CubemapLayout -> IO Raylib.Types.Texture
loadTextureCubemap image layout = do
  texture <- withFreeable image (\i -> c'loadTextureCubemap i (fromIntegral $ fromEnum layout)) >>= pop
  addTextureId $ texture'id texture
  return texture

loadRenderTexture :: Int -> Int -> IO Raylib.Types.RenderTexture
loadRenderTexture width height = do
  renderTexture <-  c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= pop
  addFrameBuffer $ renderTexture'id renderTexture
  addTextureId $ texture'id $ renderTexture'texture renderTexture
  return renderTexture

isTextureReady :: Texture -> IO Bool
isTextureReady texture = toBool <$> withFreeable texture c'isTextureReady

isRenderTextureReady :: RenderTexture -> IO Bool
isRenderTextureReady renderTexture = toBool <$> withFreeable renderTexture c'isRenderTextureReady

updateTexture :: Raylib.Types.Texture -> Ptr () -> IO Raylib.Types.Texture
updateTexture texture pixels = withFreeable texture (\t -> c'updateTexture t pixels >> peek t)

updateTextureRec :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Ptr () -> IO Raylib.Types.Texture
updateTextureRec texture rect pixels = withFreeable texture (\t -> withFreeable rect (\r -> c'updateTextureRec t r pixels) >> peek t)

genTextureMipmaps :: Raylib.Types.Texture -> IO Raylib.Types.Texture
genTextureMipmaps texture = withFreeable texture (\t -> c'genTextureMipmaps t >> peek t)

setTextureFilter :: Raylib.Types.Texture -> TextureFilter -> IO Raylib.Types.Texture
setTextureFilter texture filterType = withFreeable texture (\t -> c'setTextureFilter t (fromIntegral $ fromEnum filterType) >> peek t)

setTextureWrap :: Raylib.Types.Texture -> TextureWrap -> IO Raylib.Types.Texture
setTextureWrap texture wrap = withFreeable texture (\t -> c'setTextureWrap t (fromIntegral $ fromEnum wrap) >> peek t)

drawTexture :: Raylib.Types.Texture -> Int -> Int -> Raylib.Types.Color -> IO ()
drawTexture texture x y tint = withFreeable texture (\t -> withFreeable tint (c'drawTexture t (fromIntegral x) (fromIntegral y)))

drawTextureV :: Raylib.Types.Texture -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTextureV texture position color = withFreeable texture (\t -> withFreeable position (withFreeable color . c'drawTextureV t))

drawTextureEx :: Raylib.Types.Texture -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextureEx texture position rotation scale tint = withFreeable texture (\t -> withFreeable position (\p -> withFreeable tint (c'drawTextureEx t p (realToFrac rotation) (realToFrac scale))))

drawTextureRec :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTextureRec texture source position tint = withFreeable texture (\t -> withFreeable source (\s -> withFreeable position (withFreeable tint . c'drawTextureRec t s)))

drawTexturePro :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTexturePro texture source dest origin rotation tint = withFreeable texture (\t -> withFreeable source (\s -> withFreeable dest (\d -> withFreeable origin (\o -> withFreeable tint (c'drawTexturePro t s d o (realToFrac rotation))))))

drawTextureNPatch :: Raylib.Types.Texture -> Raylib.Types.NPatchInfo -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTextureNPatch texture nPatchInfo dest origin rotation tint = withFreeable texture (\t -> withFreeable nPatchInfo (\n -> withFreeable dest (\d -> withFreeable origin (\o -> withFreeable tint (c'drawTextureNPatch t n d o (realToFrac rotation))))))

fade :: Raylib.Types.Color -> Float -> Raylib.Types.Color
fade color alpha = unsafePerformIO $ withFreeable color (\c -> c'fade c (realToFrac alpha)) >>= pop

colorToInt :: Raylib.Types.Color -> Int
colorToInt color = unsafePerformIO $ fromIntegral <$> withFreeable color c'colorToInt

colorNormalize :: Raylib.Types.Color -> Raylib.Types.Vector4
colorNormalize color = unsafePerformIO $ withFreeable color c'colorNormalize >>= pop

colorFromNormalized :: Raylib.Types.Vector4 -> Raylib.Types.Color
colorFromNormalized normalized = unsafePerformIO $ withFreeable normalized c'colorFromNormalized >>= pop

colorToHSV :: Raylib.Types.Color -> Raylib.Types.Vector3
colorToHSV color = unsafePerformIO $ withFreeable color c'colorToHSV >>= pop

colorFromHSV :: Float -> Float -> Float -> Raylib.Types.Color
colorFromHSV hue saturation value = unsafePerformIO $ c'colorFromHSV (realToFrac hue) (realToFrac saturation) (realToFrac value) >>= pop

colorTint :: Color -> Color -> Raylib.Types.Color
colorTint color tint = unsafePerformIO $ withFreeable color (withFreeable tint . c'colorTint) >>= pop

colorBrightness :: Color -> Float -> Raylib.Types.Color
colorBrightness color brightness = unsafePerformIO $ withFreeable color (\c -> c'colorBrightness c (realToFrac brightness)) >>= pop

colorContrast :: Color -> Float -> Raylib.Types.Color
colorContrast color contrast = unsafePerformIO $ withFreeable color (\c -> c'colorContrast c (realToFrac contrast)) >>= pop

colorAlpha :: Raylib.Types.Color -> Float -> Raylib.Types.Color
colorAlpha color alpha = unsafePerformIO $ withFreeable color (\c -> c'colorAlpha c (realToFrac alpha)) >>= pop

colorAlphaBlend :: Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color
colorAlphaBlend dst src tint = unsafePerformIO $ withFreeable dst (\d -> withFreeable src (withFreeable tint . c'colorAlphaBlend d)) >>= pop

getColor :: Integer -> Raylib.Types.Color
getColor hexValue = unsafePerformIO $ c'getColor (fromIntegral hexValue) >>= pop

getPixelColor :: Ptr () -> PixelFormat -> IO Raylib.Types.Color
getPixelColor srcPtr format = c'getPixelColor srcPtr (fromIntegral $ fromEnum format) >>= pop

setPixelColor :: Ptr () -> Raylib.Types.Color -> PixelFormat -> IO ()
setPixelColor dstPtr color format = withFreeable color (\c -> c'setPixelColor dstPtr c (fromIntegral $ fromEnum format))
