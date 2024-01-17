{-# OPTIONS -Wall #-}

module Raylib.Core.Textures where

import Control.Monad ((<=<))
import Data.Word (Word8)
import Foreign
  ( Ptr,
    Storable (peek, sizeOf),
    toBool,
  )
import Foreign.C (CUChar (CUChar), withCString, CInt, CFloat)
import GHC.IO (unsafePerformIO)
import Raylib.ForeignUtil
  ( pop,
    popCArray,
    withFreeable,
    withFreeableArrayLen, withFreeableArray,
  )
import Raylib.Internal (WindowResources, addFrameBuffer, addTextureId, unloadSingleFrameBuffer, unloadSingleTexture)
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
    c'exportImageToMemory,
    c'fade,
    c'genImageCellular,
    c'genImageChecked,
    c'genImageColor,
    c'genImageGradientLinear,
    c'genImageGradientRadial,
    c'genImageGradientSquare,
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
    c'imageRotate,
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
    c'loadImageSvg,
    c'loadRenderTexture,
    c'loadTexture,
    c'loadTextureCubemap,
    c'loadTextureFromImage,
    c'setPixelColor,
    c'setTextureFilter,
    c'setTextureWrap,
    c'updateTexture,
    c'updateTextureRec, c'loadImageAnimFromMemory, c'imageKernelConvolution,
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

loadImage :: String -> IO Image
loadImage fileName = withCString fileName c'loadImage >>= pop

loadImageRaw :: String -> Int -> Int -> Int -> Int -> IO Image
loadImageRaw fileName width height format headerSize =
  withCString fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) (fromIntegral headerSize)) >>= pop

loadImageSvg :: String -> Int -> Int -> IO Image
loadImageSvg fileNameOrString width height = withCString fileNameOrString (\s -> c'loadImageSvg s (fromIntegral width) (fromIntegral height)) >>= pop

-- | Returns the animation and the number of frames in a tuple
loadImageAnim :: String -> IO (Image, Int)
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


loadImageAnimFromMemory :: String -> [Integer] -> IO (Image, Int)
loadImageAnimFromMemory fileType fileData =
  withCString fileType
    (\ft ->
      withFreeableArrayLen (map fromIntegral fileData)
        (\size fd ->
          withFreeable (0 :: CInt)
            (\frames -> do
                img <- c'loadImageAnimFromMemory ft fd (fromIntegral $ size * sizeOf (0 :: CUChar)) frames >>= pop
                frameNum <- fromIntegral <$> peek frames
                return (img, frameNum)
            )
        )
    )

loadImageFromMemory :: String -> [Integer] -> IO Image
loadImageFromMemory fileType fileData =
  withCString fileType (\ft -> withFreeableArrayLen (map fromIntegral fileData) (\size fd -> c'loadImageFromMemory ft fd (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

loadImageFromTexture :: Texture -> IO Image
loadImageFromTexture tex = withFreeable tex c'loadImageFromTexture >>= pop

loadImageFromScreen :: IO Image
loadImageFromScreen = c'loadImageFromScreen >>= pop

isImageReady :: Image -> IO Bool
isImageReady image = toBool <$> withFreeable image c'isImageReady

exportImage :: Image -> String -> IO Bool
exportImage image fileName = toBool <$> withFreeable image (withCString fileName . c'exportImage)

exportImageToMemory :: Image -> String -> IO [Word8]
exportImageToMemory image fileType =
  withFreeable
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
                    map (\(CUChar x) -> x) <$> popCArray size bytes
                )
          )
    )

exportImageAsCode :: Image -> String -> IO Bool
exportImageAsCode image fileName =
  toBool <$> withFreeable image (withCString fileName . c'exportImageAsCode)

genImageColor :: Int -> Int -> Color -> IO Image
genImageColor width height color =
  withFreeable color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= pop

genImageGradientLinear :: Int -> Int -> Int -> Color -> Color -> IO Image
genImageGradientLinear width height direction start end =
  withFreeable start (withFreeable end . c'genImageGradientLinear (fromIntegral width) (fromIntegral height) (fromIntegral direction)) >>= pop

genImageGradientRadial :: Int -> Int -> Float -> Color -> Color -> IO Image
genImageGradientRadial width height density inner outer =
  withFreeable inner (withFreeable outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

genImageGradientSquare :: Int -> Int -> Float -> Color -> Color -> IO Image
genImageGradientSquare width height density inner outer =
  withFreeable inner (withFreeable outer . c'genImageGradientSquare (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

genImageChecked :: Int -> Int -> Int -> Int -> Color -> Color -> IO Image
genImageChecked width height checksX checksY col1 col2 =
  withFreeable col1 (withFreeable col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= pop

genImageWhiteNoise :: Int -> Int -> Float -> IO Image
genImageWhiteNoise width height factor =
  c'genImageWhiteNoise (fromIntegral width) (fromIntegral height) (realToFrac factor) >>= pop

genImagePerlinNoise :: Int -> Int -> Int -> Int -> Float -> IO Image
genImagePerlinNoise width height offsetX offsetY scale = c'genImagePerlinNoise (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY) (realToFrac scale) >>= pop

genImageCellular :: Int -> Int -> Int -> IO Image
genImageCellular width height tileSize =
  c'genImageCellular (fromIntegral width) (fromIntegral height) (fromIntegral tileSize) >>= pop

genImageText :: Int -> Int -> String -> IO Image
genImageText width height text =
  withCString text (c'genImageText (fromIntegral width) (fromIntegral height)) >>= pop

imageCopy :: Image -> IO Image
imageCopy image = withFreeable image c'imageCopy >>= pop

imageFromImage :: Image -> Rectangle -> IO Image
imageFromImage image rect = withFreeable image (withFreeable rect . c'imageFromImage) >>= pop

imageText :: String -> Int -> Color -> IO Image
imageText text fontSize color =
  withCString text (\t -> withFreeable color $ c'imageText t (fromIntegral fontSize)) >>= pop

imageTextEx :: Font -> String -> Float -> Float -> Color -> IO Image
imageTextEx font text fontSize spacing tint =
  withFreeable font (\f -> withCString text (\t -> withFreeable tint $ c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

imageFormat :: Image -> PixelFormat -> IO Image
imageFormat image newFormat =
  withFreeable image (\i -> c'imageFormat i (fromIntegral $ fromEnum newFormat) >> peek i)

imageToPOT :: Image -> Color -> IO Image
imageToPOT image color = withFreeable image (\i -> withFreeable color (c'imageToPOT i) >> peek i)

imageCrop :: Image -> Rectangle -> IO Image
imageCrop image crop = withFreeable image (\i -> withFreeable crop (c'imageCrop i) >> peek i)

imageAlphaCrop :: Image -> Float -> IO Image
imageAlphaCrop image threshold = withFreeable image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peek i)

imageAlphaClear :: Image -> Color -> Float -> IO Image
imageAlphaClear image color threshold = withFreeable image (\i -> withFreeable color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peek i))

imageAlphaMask :: Image -> Image -> IO Image
imageAlphaMask image alphaMask = withFreeable image (\i -> withFreeable alphaMask (c'imageAlphaMask i) >> peek i)

imageAlphaPremultiply :: Image -> IO Image
imageAlphaPremultiply image = withFreeable image (\i -> c'imageAlphaPremultiply i >> peek i)

imageBlurGaussian :: Image -> Int -> IO Image
imageBlurGaussian image blurSize = withFreeable image (\i -> c'imageBlurGaussian i (fromIntegral blurSize) >> peek i)

imageKernelConvolution :: Image -> [Float] -> IO Image
imageKernelConvolution image kernel = withFreeable image (\i -> withFreeableArray (map realToFrac kernel :: [CFloat]) (\k -> c'imageKernelConvolution i k (fromIntegral $ length kernel) >> peek i))

imageResize :: Image -> Int -> Int -> IO Image
imageResize image newWidth newHeight = withFreeable image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

imageResizeNN :: Image -> Int -> Int -> IO Image
imageResizeNN image newWidth newHeight = withFreeable image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

imageResizeCanvas :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = withFreeable image (\i -> withFreeable fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peek i)

imageMipmaps :: Image -> IO Image
imageMipmaps image = withFreeable image (\i -> c'imageMipmaps i >> peek i)

imageDither :: Image -> Int -> Int -> Int -> Int -> IO Image
imageDither image rBpp gBpp bBpp aBpp = withFreeable image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peek i)

imageFlipVertical :: Image -> IO Image
imageFlipVertical image = withFreeable image (\i -> c'imageFlipVertical i >> peek i)

imageFlipHorizontal :: Image -> IO Image
imageFlipHorizontal image = withFreeable image (\i -> c'imageFlipHorizontal i >> peek i)

imageRotate :: Image -> Int -> IO Image
imageRotate image degrees = withFreeable image (\i -> c'imageRotate i (fromIntegral degrees) >> peek i)

imageRotateCW :: Image -> IO Image
imageRotateCW image = withFreeable image (\i -> c'imageRotateCW i >> peek i)

imageRotateCCW :: Image -> IO Image
imageRotateCCW image = withFreeable image (\i -> c'imageRotateCCW i >> peek i)

imageColorTint :: Image -> Color -> IO Image
imageColorTint image color = withFreeable image (\i -> withFreeable color (c'imageColorTint i) >> peek i)

imageColorInvert :: Image -> IO Image
imageColorInvert image = withFreeable image (\i -> c'imageColorInvert i >> peek i)

imageColorGrayscale :: Image -> IO Image
imageColorGrayscale image = withFreeable image (\i -> c'imageColorGrayscale i >> peek i)

imageColorContrast :: Image -> Float -> IO Image
imageColorContrast image contrast = withFreeable image (\i -> c'imageColorContrast i (realToFrac contrast) >> peek i)

imageColorBrightness :: Image -> Int -> IO Image
imageColorBrightness image brightness = withFreeable image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peek i)

imageColorReplace :: Image -> Color -> Color -> IO Image
imageColorReplace image color replace = withFreeable image (\i -> withFreeable color (withFreeable replace . c'imageColorReplace i) >> peek i)

loadImageColors :: Image -> IO [Color]
loadImageColors image =
  withFreeable
    image
    (popCArray (fromIntegral $ image'width image * image'height image) <=< c'loadImageColors)

loadImagePalette :: Image -> Int -> IO [Color]
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

getImageAlphaBorder :: Image -> Float -> IO Rectangle
getImageAlphaBorder image threshold = withFreeable image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

getImageColor :: Image -> Int -> Int -> IO Color
getImageColor image x y = withFreeable image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

imageClearBackground :: Image -> Color -> IO Image
imageClearBackground image color = withFreeable image (\i -> withFreeable color (c'imageClearBackground i) >> peek i)

imageDrawPixel :: Image -> Int -> Int -> Color -> IO Image
imageDrawPixel image x y color = withFreeable image (\i -> withFreeable color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peek i)

imageDrawPixelV :: Image -> Vector2 -> Color -> IO Image
imageDrawPixelV image position color = withFreeable image (\i -> withFreeable position (withFreeable color . c'imageDrawPixelV i) >> peek i)

imageDrawLine :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageDrawLine image startPosX startPosY endPosX endPosY color = withFreeable image (\i -> withFreeable color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peek i)

imageDrawLineV :: Image -> Vector2 -> Vector2 -> Color -> IO Image
imageDrawLineV image start end color = withFreeable image (\i -> withFreeable start (\s -> withFreeable end (withFreeable color . c'imageDrawLineV i s)) >> peek i)

imageDrawCircle :: Image -> Int -> Int -> Int -> Color -> IO Image
imageDrawCircle image centerX centerY radius color = withFreeable image (\i -> withFreeable color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

imageDrawCircleV :: Image -> Vector2 -> Int -> Color -> IO Image
imageDrawCircleV image center radius color = withFreeable image (\i -> withFreeable center (\c -> withFreeable color (c'imageDrawCircleV i c (fromIntegral radius))) >> peek i)

imageDrawCircleLines :: Image -> Int -> Int -> Int -> Color -> IO Image
imageDrawCircleLines image centerX centerY radius color = withFreeable image (\i -> withFreeable color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

imageDrawCircleLinesV :: Image -> Vector2 -> Int -> Color -> IO Image
imageDrawCircleLinesV image center radius color = withFreeable image (\i -> withFreeable center (\c -> withFreeable color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peek i)

imageDrawRectangle :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageDrawRectangle image posX posY width height color = withFreeable image (\i -> withFreeable color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peek i)

imageDrawRectangleV :: Image -> Vector2 -> Vector2 -> Color -> IO Image
imageDrawRectangleV image position size color = withFreeable image (\i -> withFreeable position (\p -> withFreeable size (withFreeable color . c'imageDrawRectangleV i p)) >> peek i)

imageDrawRectangleRec :: Image -> Rectangle -> Color -> IO Image
imageDrawRectangleRec image rectangle color = withFreeable image (\i -> withFreeable rectangle (withFreeable color . c'imageDrawRectangleRec i) >> peek i)

imageDrawRectangleLines :: Image -> Rectangle -> Int -> Color -> IO Image
imageDrawRectangleLines image rectangle thickness color = withFreeable image (\i -> withFreeable rectangle (\r -> withFreeable color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peek i)

imageDraw :: Image -> Image -> Rectangle -> Rectangle -> Color -> IO Image
imageDraw image source srcRec dstRec tint = withFreeable image (\i -> withFreeable source (\s -> withFreeable srcRec (\sr -> withFreeable dstRec (withFreeable tint . c'imageDraw i s sr))) >> peek i)

imageDrawText :: Image -> String -> Int -> Int -> Int -> Color -> IO Image
imageDrawText image text x y fontSize color = withFreeable image (\i -> withCString text (\t -> withFreeable color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peek i)

imageDrawTextEx :: Image -> Font -> String -> Vector2 -> Float -> Float -> Color -> IO Image
imageDrawTextEx image font text position fontSize spacing tint = withFreeable image (\i -> withFreeable font (\f -> withCString text (\t -> withFreeable position (\p -> withFreeable tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peek i)

loadTexture :: String -> WindowResources -> IO Texture
loadTexture fileName wr = do
  texture <- withCString fileName c'loadTexture >>= pop
  addTextureId (texture'id texture) wr
  return texture

loadTextureFromImage :: Image -> WindowResources -> IO Texture
loadTextureFromImage image wr = do
  texture <- withFreeable image c'loadTextureFromImage >>= pop
  addTextureId (texture'id texture) wr
  return texture

loadTextureCubemap :: Image -> CubemapLayout -> WindowResources -> IO Texture
loadTextureCubemap image layout wr = do
  texture <- withFreeable image (\i -> c'loadTextureCubemap i (fromIntegral $ fromEnum layout)) >>= pop
  addTextureId (texture'id texture) wr
  return texture

loadRenderTexture :: Int -> Int -> WindowResources -> IO RenderTexture
loadRenderTexture width height wr = do
  renderTexture <- c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= pop
  addFrameBuffer (renderTexture'id renderTexture) wr
  addTextureId (texture'id $ renderTexture'texture renderTexture) wr
  return renderTexture

isTextureReady :: Texture -> IO Bool
isTextureReady texture = toBool <$> withFreeable texture c'isTextureReady

isRenderTextureReady :: RenderTexture -> IO Bool
isRenderTextureReady renderTexture = toBool <$> withFreeable renderTexture c'isRenderTextureReady

-- | Unloads a texture from GPU memory (VRAM). Textures are automatically unloaded
-- when `closeWindow` is called, so manually unloading textures is not required.
-- In larger projects, you may want to manually unload textures to avoid having
-- them in VRAM for too long.
unloadTexture :: Texture -> WindowResources -> IO ()
unloadTexture texture = unloadSingleTexture (texture'id texture)

-- | Unloads a render texture from GPU memory (VRAM). Render textures are
-- automatically unloaded when `closeWindow` is called, so manually unloading
-- render textures is not required. In larger projects, you may want to
-- manually unload render textures to avoid having them in VRAM for too long.
unloadRenderTexture :: RenderTexture -> WindowResources -> IO ()
unloadRenderTexture renderTexture wr = do
  unloadSingleTexture (texture'id $ renderTexture'texture renderTexture) wr
  unloadSingleFrameBuffer (renderTexture'id renderTexture) wr

updateTexture :: Texture -> Ptr () -> IO Texture
updateTexture texture pixels = withFreeable texture (\t -> c'updateTexture t pixels >> peek t)

updateTextureRec :: Texture -> Rectangle -> Ptr () -> IO Texture
updateTextureRec texture rect pixels = withFreeable texture (\t -> withFreeable rect (\r -> c'updateTextureRec t r pixels) >> peek t)

genTextureMipmaps :: Texture -> IO Texture
genTextureMipmaps texture = withFreeable texture (\t -> c'genTextureMipmaps t >> peek t)

setTextureFilter :: Texture -> TextureFilter -> IO Texture
setTextureFilter texture filterType = withFreeable texture (\t -> c'setTextureFilter t (fromIntegral $ fromEnum filterType) >> peek t)

setTextureWrap :: Texture -> TextureWrap -> IO Texture
setTextureWrap texture wrap = withFreeable texture (\t -> c'setTextureWrap t (fromIntegral $ fromEnum wrap) >> peek t)

drawTexture :: Texture -> Int -> Int -> Color -> IO ()
drawTexture texture x y tint = withFreeable texture (\t -> withFreeable tint (c'drawTexture t (fromIntegral x) (fromIntegral y)))

drawTextureV :: Texture -> Vector2 -> Color -> IO ()
drawTextureV texture position color = withFreeable texture (\t -> withFreeable position (withFreeable color . c'drawTextureV t))

drawTextureEx :: Texture -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextureEx texture position rotation scale tint = withFreeable texture (\t -> withFreeable position (\p -> withFreeable tint (c'drawTextureEx t p (realToFrac rotation) (realToFrac scale))))

drawTextureRec :: Texture -> Rectangle -> Vector2 -> Color -> IO ()
drawTextureRec texture source position tint = withFreeable texture (\t -> withFreeable source (\s -> withFreeable position (withFreeable tint . c'drawTextureRec t s)))

drawTexturePro :: Texture -> Rectangle -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTexturePro texture source dest origin rotation tint = withFreeable texture (\t -> withFreeable source (\s -> withFreeable dest (\d -> withFreeable origin (\o -> withFreeable tint (c'drawTexturePro t s d o (realToFrac rotation))))))

drawTextureNPatch :: Texture -> NPatchInfo -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTextureNPatch texture nPatchInfo dest origin rotation tint = withFreeable texture (\t -> withFreeable nPatchInfo (\n -> withFreeable dest (\d -> withFreeable origin (\o -> withFreeable tint (c'drawTextureNPatch t n d o (realToFrac rotation))))))

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

getColor :: Integer -> Color
getColor hexValue = unsafePerformIO $ c'getColor (fromIntegral hexValue) >>= pop

getPixelColor :: Ptr () -> PixelFormat -> IO Color
getPixelColor srcPtr format = c'getPixelColor srcPtr (fromIntegral $ fromEnum format) >>= pop

setPixelColor :: Ptr () -> Color -> PixelFormat -> IO ()
setPixelColor dstPtr color format = withFreeable color (\c -> c'setPixelColor dstPtr c (fromIntegral $ fromEnum format))
