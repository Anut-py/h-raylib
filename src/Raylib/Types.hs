{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveAnyClass #-}

module Raylib.Types where

import Control.Monad (forM_, unless)
import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    Word16,
    Word8,
    castPtr,
    fromBool,
    malloc,
    newArray,
    newForeignPtr,
    nullFunPtr,
    nullPtr,
    peekArray,
    toBool,
    withForeignPtr,
  )
import Foreign.C
  ( CBool,
    CChar,
    CFloat,
    CInt (..),
    CShort,
    CString,
    CUChar,
    CUInt,
    CUShort,
    castCharToCChar,
    newCString,
    peekCString,
  )
import Foreign.C.String (castCCharToChar)
import Raylib.ForeignUtil (Freeable (rlFreeDependents), c'free, freeMaybePtr, newMaybeArray, p'free, peekMaybeArray, peekStaticArray, peekStaticArrayOff, pokeMaybeOff, pokeStaticArray, pokeStaticArrayOff, rightPad, rlFreeArray, rlFreeMaybeArray)
import Raylib.Internal (c'rlGetShaderIdDefault, getPixelDataSize)

------------------------------------------------
-- Raylib enumerations -------------------------
------------------------------------------------

---- raylib.h

data ConfigFlag
  = VsyncHint
  | FullscreenMode
  | WindowResizable
  | WindowUndecorated
  | WindowHidden
  | WindowMinimized
  | WindowMaximized
  | WindowUnfocused
  | WindowTopmost
  | WindowAlwaysRun
  | WindowTransparent
  | WindowHighdpi
  | WindowMousePassthrough
  | BorderlessWindowedMode
  | Msaa4xHint
  | InterlacedHint
  deriving (Eq, Show, Freeable)

instance Enum ConfigFlag where
  fromEnum g = case g of
    VsyncHint -> 64
    FullscreenMode -> 2
    WindowResizable -> 4
    WindowUndecorated -> 8
    WindowHidden -> 128
    WindowMinimized -> 512
    WindowMaximized -> 1024
    WindowUnfocused -> 2048
    WindowTopmost -> 4096
    WindowAlwaysRun -> 256
    WindowTransparent -> 16
    WindowHighdpi -> 8192
    WindowMousePassthrough -> 16384
    BorderlessWindowedMode -> 32768
    Msaa4xHint -> 32
    InterlacedHint -> 65536
  toEnum x = case x of
    64 -> VsyncHint
    2 -> FullscreenMode
    4 -> WindowResizable
    8 -> WindowUndecorated
    128 -> WindowHidden
    512 -> WindowMinimized
    1024 -> WindowMaximized
    2048 -> WindowUnfocused
    4096 -> WindowTopmost
    256 -> WindowAlwaysRun
    16 -> WindowTransparent
    8192 -> WindowHighdpi
    16384 -> WindowMousePassthrough
    32768 -> BorderlessWindowedMode
    32 -> Msaa4xHint
    65536 -> InterlacedHint
    n -> error $ "(ConfigFlag.toEnum) Invalid value: " ++ show n

data TraceLogLevel = LogAll | LogTrace | LogDebug | LogInfo | LogWarning | LogError | LogFatal | LogNone
  deriving (Eq, Show, Enum)

data KeyboardKey
  = KeyNull
  | KeyApostrophe
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeySlash
  | KeyZero
  | KeyOne
  | KeyTwo
  | KeyThree
  | KeyFour
  | KeyFive
  | KeySix
  | KeySeven
  | KeyEight
  | KeyNine
  | KeySemicolon
  | KeyEqual
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeyLeftBracket
  | KeyBackslash
  | KeyRightBracket
  | KeyGrave
  | KeySpace
  | KeyEscape
  | KeyEnter
  | KeyTab
  | KeyBackspace
  | KeyInsert
  | KeyDelete
  | KeyRight
  | KeyLeft
  | KeyDown
  | KeyUp
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeyCapsLock
  | KeyScrollLock
  | KeyNumLock
  | KeyPrintScreen
  | KeyPause
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyLeftShift
  | KeyLeftControl
  | KeyLeftAlt
  | KeyLeftSuper
  | KeyRightShift
  | KeyRightControl
  | KeyRightAlt
  | KeyRightSuper
  | KeyKbMenu
  | KeyKp0
  | KeyKp1
  | KeyKp2
  | KeyKp3
  | KeyKp4
  | KeyKp5
  | KeyKp6
  | KeyKp7
  | KeyKp8
  | KeyKp9
  | KeyKpDecimal
  | KeyKpDivide
  | KeyKpMultiply
  | KeyKpSubtract
  | KeyKpAdd
  | KeyKpEnter
  | KeyKpEqual
  | KeyBack
  | KeyMenu
  | KeyVolumeUp
  | KeyVolumeDown
  deriving (Eq, Show)

instance Enum KeyboardKey where
  fromEnum k = case k of
    KeyNull -> 0
    KeyApostrophe -> 39
    KeyComma -> 44
    KeyMinus -> 45
    KeyPeriod -> 46
    KeySlash -> 47
    KeyZero -> 48
    KeyOne -> 49
    KeyTwo -> 50
    KeyThree -> 51
    KeyFour -> 52
    KeyFive -> 53
    KeySix -> 54
    KeySeven -> 55
    KeyEight -> 56
    KeyNine -> 57
    KeySemicolon -> 59
    KeyEqual -> 61
    KeyA -> 65
    KeyB -> 66
    KeyC -> 67
    KeyD -> 68
    KeyE -> 69
    KeyF -> 70
    KeyG -> 71
    KeyH -> 72
    KeyI -> 73
    KeyJ -> 74
    KeyK -> 75
    KeyL -> 76
    KeyM -> 77
    KeyN -> 78
    KeyO -> 79
    KeyP -> 80
    KeyQ -> 81
    KeyR -> 82
    KeyS -> 83
    KeyT -> 84
    KeyU -> 85
    KeyV -> 86
    KeyW -> 87
    KeyX -> 88
    KeyY -> 89
    KeyZ -> 90
    KeyLeftBracket -> 91
    KeyBackslash -> 92
    KeyRightBracket -> 93
    KeyGrave -> 96
    KeySpace -> 32
    KeyEscape -> 256
    KeyEnter -> 257
    KeyTab -> 258
    KeyBackspace -> 259
    KeyInsert -> 260
    KeyDelete -> 261
    KeyRight -> 262
    KeyLeft -> 263
    KeyDown -> 264
    KeyUp -> 265
    KeyPageUp -> 266
    KeyPageDown -> 267
    KeyHome -> 268
    KeyEnd -> 269
    KeyCapsLock -> 280
    KeyScrollLock -> 281
    KeyNumLock -> 282
    KeyPrintScreen -> 283
    KeyPause -> 284
    KeyF1 -> 290
    KeyF2 -> 291
    KeyF3 -> 292
    KeyF4 -> 293
    KeyF5 -> 294
    KeyF6 -> 295
    KeyF7 -> 296
    KeyF8 -> 297
    KeyF9 -> 298
    KeyF10 -> 299
    KeyF11 -> 300
    KeyF12 -> 301
    KeyLeftShift -> 340
    KeyLeftControl -> 341
    KeyLeftAlt -> 342
    KeyLeftSuper -> 343
    KeyRightShift -> 344
    KeyRightControl -> 345
    KeyRightAlt -> 346
    KeyRightSuper -> 347
    KeyKbMenu -> 348
    KeyKp0 -> 320
    KeyKp1 -> 321
    KeyKp2 -> 322
    KeyKp3 -> 323
    KeyKp4 -> 324
    KeyKp5 -> 325
    KeyKp6 -> 326
    KeyKp7 -> 327
    KeyKp8 -> 328
    KeyKp9 -> 329
    KeyKpDecimal -> 330
    KeyKpDivide -> 331
    KeyKpMultiply -> 332
    KeyKpSubtract -> 333
    KeyKpAdd -> 334
    KeyKpEnter -> 335
    KeyKpEqual -> 336
    -- Android buttons
    KeyBack -> 4
    KeyMenu -> 82
    KeyVolumeUp -> 24
    KeyVolumeDown -> 25

  toEnum n = case n of
    0 -> KeyNull
    39 -> KeyApostrophe
    44 -> KeyComma
    45 -> KeyMinus
    46 -> KeyPeriod
    47 -> KeySlash
    48 -> KeyZero
    49 -> KeyOne
    50 -> KeyTwo
    51 -> KeyThree
    52 -> KeyFour
    53 -> KeyFive
    54 -> KeySix
    55 -> KeySeven
    56 -> KeyEight
    57 -> KeyNine
    59 -> KeySemicolon
    61 -> KeyEqual
    65 -> KeyA
    66 -> KeyB
    67 -> KeyC
    68 -> KeyD
    69 -> KeyE
    70 -> KeyF
    71 -> KeyG
    72 -> KeyH
    73 -> KeyI
    74 -> KeyJ
    75 -> KeyK
    76 -> KeyL
    77 -> KeyM
    78 -> KeyN
    79 -> KeyO
    80 -> KeyP
    81 -> KeyQ
    82 -> KeyR
    83 -> KeyS
    84 -> KeyT
    85 -> KeyU
    86 -> KeyV
    87 -> KeyW
    88 -> KeyX
    89 -> KeyY
    90 -> KeyZ
    91 -> KeyLeftBracket
    92 -> KeyBackslash
    93 -> KeyRightBracket
    96 -> KeyGrave
    32 -> KeySpace
    256 -> KeyEscape
    257 -> KeyEnter
    258 -> KeyTab
    259 -> KeyBackspace
    260 -> KeyInsert
    261 -> KeyDelete
    262 -> KeyRight
    263 -> KeyLeft
    264 -> KeyDown
    265 -> KeyUp
    266 -> KeyPageUp
    267 -> KeyPageDown
    268 -> KeyHome
    269 -> KeyEnd
    280 -> KeyCapsLock
    281 -> KeyScrollLock
    282 -> KeyNumLock
    283 -> KeyPrintScreen
    284 -> KeyPause
    290 -> KeyF1
    291 -> KeyF2
    292 -> KeyF3
    293 -> KeyF4
    294 -> KeyF5
    295 -> KeyF6
    296 -> KeyF7
    297 -> KeyF8
    298 -> KeyF9
    299 -> KeyF10
    300 -> KeyF11
    301 -> KeyF12
    340 -> KeyLeftShift
    341 -> KeyLeftControl
    342 -> KeyLeftAlt
    343 -> KeyLeftSuper
    344 -> KeyRightShift
    345 -> KeyRightControl
    346 -> KeyRightAlt
    347 -> KeyRightSuper
    348 -> KeyKbMenu
    320 -> KeyKp0
    321 -> KeyKp1
    322 -> KeyKp2
    323 -> KeyKp3
    324 -> KeyKp4
    325 -> KeyKp5
    326 -> KeyKp6
    327 -> KeyKp7
    328 -> KeyKp8
    329 -> KeyKp9
    330 -> KeyKpDecimal
    331 -> KeyKpDivide
    332 -> KeyKpMultiply
    333 -> KeyKpSubtract
    334 -> KeyKpAdd
    335 -> KeyKpEnter
    336 -> KeyKpEqual
    -- Android buttons
    4 -> KeyBack
    --  82  -> KeyMenu
    24 -> KeyVolumeUp
    25 -> KeyVolumeDown
    x -> error $ "(KeyboardKey.toEnum) Invalid value: " ++ show x

data MouseButton
  = MouseButtonLeft
  | MouseButtonRight
  | MouseButtonMiddle
  | MouseButtonSide
  | MouseButtonExtra
  | MouseButtonForward
  | MouseButtonBack
  deriving (Eq, Show, Enum, Bounded)

data MouseCursor
  = MouseCursorDefault
  | MouseCursorArrow
  | MouseCursorIbeam
  | MouseCursorCrosshair
  | MouseCursorPointingHand
  | MouseCursorResizeEW
  | MouseCursorResizeNS
  | MouseCursorResizeNWSE
  | MouseCursorResizeNESW
  | MouseCursorResizeAll
  | MouseCursorNotAllowed
  deriving (Eq, Show, Enum, Bounded)

data GamepadButton
  = GamepadButtonUnknown
  | GamepadButtonUnknownLeftFaceUp
  | GamepadButtonLeftFaceRight
  | GamepadButtonLeftFaceDown
  | GamepadButtonLeftFaceLeft
  | GamepadButtonRightFaceUp
  | GamepadButtonRightFaceRight
  | GamepadButtonRightFaceDown
  | GamepadButtonRightFaceLeft
  | GamepadButtonLeftTrigger1
  | GamepadButtonLeftTrigger2
  | GamepadButtonRightTrigger1
  | GamepadButtonRightTrigger2
  | GamepadButtonMiddleLeft
  | GamepadButtonMiddle
  | GamepadButtonMiddleRight
  | GamepadButtonLeftThumb
  | GamepadButtonRightThumb
  deriving (Eq, Show, Enum, Bounded)

data GamepadAxis
  = GamepadAxisLeftX
  | GamepadAxisLeftY
  | GamepadAxisRightX
  | GamepadAxisRightY
  | GamepadAxisLeftTrigger
  | GamepadAxisRightTrigger
  deriving (Eq, Show, Enum, Bounded)

data MaterialMapIndex
  = MaterialMapAlbedo
  | MaterialMapMetalness
  | MaterialMapNormal
  | MaterialMapRoughness
  | MaterialMapOcclusion
  | MaterialMapEmission
  | MaterialMapHeight
  | MaterialMapCubemap
  | MaterialMapIrradiance
  | MaterialMapPrefilter
  | MaterialMapBrdf
  deriving (Eq, Show, Enum)

data ShaderLocationIndex
  = ShaderLocVertexPosition
  | ShaderLocVertexTexcoord01
  | ShaderLocVertexTexcoord02
  | ShaderLocVertexNormal
  | ShaderLocVertexTangent
  | ShaderLocVertexColor
  | ShaderLocMatrixMvp
  | ShaderLocMatrixView
  | ShaderLocMatrixProjection
  | ShaderLocMatrixModel
  | ShaderLocMatrixNormal
  | ShaderLocVectorView
  | ShaderLocColorDiffuse
  | ShaderLocColorSpecular
  | ShaderLocColorAmbient
  | ShaderLocMapAlbedo
  | ShaderLocMapMetalness
  | ShaderLocMapNormal
  | ShaderLocMapRoughness
  | ShaderLocMapOcclusion
  | ShaderLocMapEmission
  | ShaderLocMapHeight
  | ShaderLocMapCubemap
  | ShaderLocMapIrradiance
  | ShaderLocMapPrefilter
  | ShaderLocMapBrdf
  deriving (Eq, Show, Enum)

data ShaderUniformDataType
  = ShaderUniformFloatType
  | ShaderUniformVec2Type
  | ShaderUniformVec3Type
  | ShaderUniformVec4Type
  | ShaderUniformIntType
  | ShaderUniformIVec2Type
  | ShaderUniformIVec3Type
  | ShaderUniformIVec4Type
  | ShaderUniformSampler2DType
  deriving (Eq, Show, Enum)

data ShaderUniformData
  = ShaderUniformFloat Float
  | ShaderUniformVec2 Vector2
  | ShaderUniformVec3 Vector3
  | ShaderUniformVec4 Vector4
  | ShaderUniformInt Int
  | ShaderUniformIVec2 (Int, Int)
  | ShaderUniformIVec3 (Int, Int, Int)
  | ShaderUniformIVec4 (Int, Int, Int, Int)
  | ShaderUniformSampler2D Texture
  deriving (Eq, Show)

data ShaderUniformDataV
  = ShaderUniformFloatV [Float]
  | ShaderUniformVec2V [Vector2]
  | ShaderUniformVec3V [Vector3]
  | ShaderUniformVec4V [Vector4]
  | ShaderUniformIntV [Int]
  | ShaderUniformIVec2V [(Int, Int)]
  | ShaderUniformIVec3V [(Int, Int, Int)]
  | ShaderUniformIVec4V [(Int, Int, Int, Int)]
  | ShaderUniformSampler2DV [Texture]
  deriving (Eq, Show)

-- I don't know if there's a cleaner way to do this
unpackShaderUniformData :: ShaderUniformData -> IO (ShaderUniformDataType, Ptr ())
unpackShaderUniformData u = do
  case u of
    (ShaderUniformFloat f) ->
      do
        ptr <- malloc
        poke ptr (realToFrac f :: CFloat)
        return (ShaderUniformFloatType, castPtr ptr)
    (ShaderUniformVec2 (Vector2 x y)) ->
      do
        ptr <- newArray (map realToFrac [x, y] :: [CFloat])
        return (ShaderUniformVec2Type, castPtr ptr)
    (ShaderUniformVec3 (Vector3 x y z)) ->
      do
        ptr <- newArray (map realToFrac [x, y, z] :: [CFloat])
        return (ShaderUniformVec3Type, castPtr ptr)
    (ShaderUniformVec4 (Vector4 x y z w)) ->
      do
        ptr <- newArray (map realToFrac [x, y, z, w] :: [CFloat])
        return (ShaderUniformVec4Type, castPtr ptr)
    (ShaderUniformInt i) ->
      do
        ptr <- malloc
        poke ptr (fromIntegral i :: CInt)
        return (ShaderUniformIntType, castPtr ptr)
    (ShaderUniformIVec2 (i1, i2)) ->
      do
        ptr <- newArray (map fromIntegral [i1, i2] :: [CInt])
        return (ShaderUniformIVec2Type, castPtr ptr)
    (ShaderUniformIVec3 (i1, i2, i3)) ->
      do
        ptr <- newArray (map fromIntegral [i1, i2, i3] :: [CInt])
        return (ShaderUniformIVec3Type, castPtr ptr)
    (ShaderUniformIVec4 (i1, i2, i3, i4)) ->
      do
        ptr <- newArray (map fromIntegral [i1, i2, i3, i4] :: [CInt])
        return (ShaderUniformIVec4Type, castPtr ptr)
    (ShaderUniformSampler2D texture) ->
      do
        ptr <- malloc
        poke ptr (fromIntegral $ texture'id texture :: CInt)
        return (ShaderUniformSampler2DType, castPtr ptr)

unpackShaderUniformDataV :: ShaderUniformDataV -> IO (ShaderUniformDataType, Ptr (), Int)
unpackShaderUniformDataV xs = do
  case xs of
    (ShaderUniformFloatV fs) ->
      do
        ptr <- newArray (map realToFrac fs :: [CFloat])
        return (ShaderUniformFloatType, castPtr ptr, length fs)
    (ShaderUniformVec2V vs) ->
      do
        ptr <- newArray (map realToFrac $ concatMap (\(Vector2 x y) -> [x, y]) vs :: [CFloat])
        return (ShaderUniformVec2Type, castPtr ptr, length vs)
    (ShaderUniformVec3V vs) ->
      do
        ptr <- newArray (map realToFrac $ concatMap (\(Vector3 x y z) -> [x, y, z]) vs :: [CFloat])
        return (ShaderUniformVec3Type, castPtr ptr, length vs)
    (ShaderUniformVec4V vs) ->
      do
        ptr <- newArray (map realToFrac $ concatMap (\(Vector4 x y z w) -> [x, y, z, w]) vs :: [CFloat])
        return (ShaderUniformVec4Type, castPtr ptr, length vs)
    (ShaderUniformIntV is) ->
      do
        ptr <- newArray (map fromIntegral is :: [CInt])
        return (ShaderUniformIntType, castPtr ptr, length is)
    (ShaderUniformIVec2V is) ->
      do
        ptr <- newArray (map fromIntegral $ concatMap (\(x, y) -> [x, y]) is :: [CInt])
        return (ShaderUniformIVec2Type, castPtr ptr, length is)
    (ShaderUniformIVec3V is) ->
      do
        ptr <- newArray (map fromIntegral $ concatMap (\(x, y, z) -> [x, y, z]) is :: [CInt])
        return (ShaderUniformIVec3Type, castPtr ptr, length is)
    (ShaderUniformIVec4V is) ->
      do
        ptr <- newArray (map fromIntegral $ concatMap (\(x, y, z, w) -> [x, y, z, w]) is :: [CInt])
        return (ShaderUniformIVec4Type, castPtr ptr, length is)
    (ShaderUniformSampler2DV textures) ->
      do
        ptr <- newArray (map (fromIntegral . texture'id) textures :: [CInt])
        return (ShaderUniformSampler2DType, castPtr ptr, length textures)

-- I genuinely have no idea where this is used.
data ShaderAttributeDataType
  = ShaderAttribFloat
  | ShaderAttribVec2
  | ShaderAttribVec3
  | ShaderAttribVec4
  deriving (Eq, Show, Enum)

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
    PixelFormatUncompressedR16  -> 11
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

data FontType = FontDefault | FontBitmap | FontSDF deriving (Enum)

data BlendMode
  = BlendAlpha
  | BlendAdditive
  | BlendMultiplied
  | BlendAddColors
  | BlendSubtractColors
  | BlendAlphaPremultiply
  | BlendCustom
  | BlendCustomSeparate
  deriving (Enum)

data Gesture
  = GestureNone
  | GestureTap
  | GestureDoubleTap
  | GestureHold
  | GestureDrag
  | GestureSwipeRight
  | GestureSwipeLeft
  | GestureSwipeUp
  | GestureSwipeDown
  | GesturePinchIn
  | GesturePinchOut
  deriving (Show, Eq)

-- NOTE: This is not the ideal solution, I need to make this unjanky
instance Enum Gesture where
  fromEnum n = case n of
    GestureNone -> 0
    GestureTap -> 1
    GestureDoubleTap -> 2
    GestureHold -> 4
    GestureDrag -> 8
    GestureSwipeRight -> 16
    GestureSwipeLeft -> 32
    GestureSwipeUp -> 64
    GestureSwipeDown -> 128
    GesturePinchIn -> 256
    GesturePinchOut -> 512
  toEnum n = case n of
    0 -> GestureNone
    1 -> GestureTap
    2 -> GestureDoubleTap
    4 -> GestureHold
    8 -> GestureDrag
    16 -> GestureSwipeRight
    32 -> GestureSwipeLeft
    64 -> GestureSwipeUp
    128 -> GestureSwipeDown
    256 -> GesturePinchIn
    512 -> GesturePinchOut
    _ -> error $ "(Gesture.toEnum) Invalid value: " ++ show n

data CameraMode
  = CameraModeCustom
  | CameraModeFree
  | CameraModeOrbital
  | CameraModeFirstPerson
  | CameraModeThirdPerson
  deriving (Enum)

data CameraProjection = CameraPerspective | CameraOrthographic deriving (Eq, Show, Enum)

instance Storable CameraProjection where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return (toEnum $ fromEnum (val :: CInt))
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

data NPatchLayout = NPatchNinePatch | NPatchThreePatchVertical | NPatchThreePatchHorizontal deriving (Eq, Show, Enum)

instance Storable NPatchLayout where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

data MusicContextType
  = MusicAudioNone
  | MusicAudioWAV
  | MusicAudioOGG
  | MusicAudioFLAC
  | MusicAudioMP3
  | MusicAudioQOA
  | MusicModuleXM
  | MusicModuleMOD
  deriving (Eq, Show, Enum)

instance Storable MusicContextType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

---- rlgl.h

-- | OpenGL version
data RLGLVersion
  = -- | OpenGL 1.1
    RLOpenGL11
  | -- | OpenGL 2.1 (GLSL 120)
    RLOpenGL21
  | -- | OpenGL 3.3 (GLSL 330)
    RLOpenGL33
  | -- | OpenGL 4.3 (using GLSL 330)
    RLOpenGL43
  | -- | OpenGL ES 2.0 (GLSL 100)
    RLOpenGLES20
  deriving (Eq, Show)

instance Enum RLGLVersion where
  fromEnum n = case n of
    RLOpenGL11 -> 0
    RLOpenGL21 -> 1
    RLOpenGL33 -> 2
    RLOpenGL43 -> 3
    RLOpenGLES20 -> 4
  toEnum n = case n of
    0 -> RLOpenGL11
    1 -> RLOpenGL21
    2 -> RLOpenGL33
    3 -> RLOpenGL43
    4 -> RLOpenGLES20
    _ -> error $ "(RLGLVersion.toEnum) Invalid value: " ++ show n

instance Storable RLGLVersion where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Trace log level.
-- NOTE: Organized by priority level
data RLTraceLogLevel
  = -- | Display all logs
    RLLogAll
  | -- | Trace logging, intended for internal use only
    RLLogTrace
  | -- | Debug logging, used for internal debugging, it should be disabled on release builds
    RLLogDebug
  | -- | Info logging, used for program execution info
    RLLogInfo
  | -- | Warning logging, used on recoverable failures
    RLLogWarning
  | -- | Error logging, used on unrecoverable failures
    RLLogError
  | -- | Fatal logging, used to abort program: exit(EXIT_FAILURE)
    RLLogFatal
  | -- | Disable logging
    RLLogNone
  deriving (Eq, Show, Enum)

instance Storable RLTraceLogLevel where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Texture pixel formats.
-- NOTE: Support depends on OpenGL version
data RLPixelFormat
  = -- | 8 bit per pixel (no alpha)
    RLPixelFormatUncompressedGrayscale
  | -- | 8*2 bpp (2 channels)
    RLPixelFormatUncompressedGrayAlpha
  | -- | 16 bpp
    RLPixelFormatUncompressedR5G6B5
  | -- | 24 bpp
    RLPixelFormatUncompressedR8G8B8
  | -- | 16 bpp (1 bit alpha)
    RLPixelFormatUncompressedR5G5B5A1
  | -- | 16 bpp (4 bit alpha)
    RLPixelFormatUncompressedR4G4B4A4
  | -- | 32 bpp
    RLPixelFormatUncompressedR8G8B8A8
  | -- | 32 bpp (1 channel - float)
    RLPixelFormatUncompressedR32
  | -- | 32*3 bpp (3 channels - float)
    RLPixelFormatUncompressedR32G32B32
  | -- | 32*4 bpp (4 channels - float)
    RLPixelFormatUncompressedR32G32B32A32
  | -- | 16 bpp (1 channel - half float)
    RLPixelFormatUncompressedR16
  | -- | 16*3 bpp (3 channels - half float)
    RLPixelFormatUncompressedR16G16B16
  | -- | 16*4 bpp (4 channels - half float)
    RLPixelFormatUncompressedR16G16B16A16
  | -- | 4 bpp (no alpha)
    RLPixelFormatCompressedDxt1Rgb
  | -- | 4 bpp (1 bit alpha)
    RLPixelFormatCompressedDxt1Rgba
  | -- | 8 bpp
    RLPixelFormatCompressedDxt3Rgba
  | -- | 8 bpp
    RLPixelFormatCompressedDxt5Rgba
  | -- | 4 bpp
    RLPixelFormatCompressedEtc1Rgb
  | -- | 4 bpp
    RLPixelFormatCompressedEtc2Rgb
  | -- | 8 bpp
    RLPixelFormatCompressedEtc2EacRgba
  | -- | 4 bpp
    RLPixelFormatCompressedPvrtRgb
  | -- | 4 bpp
    RLPixelFormatCompressedPvrtRgba
  | -- | 8 bpp
    RLPixelFormatCompressedAstc4x4Rgba
  | -- | 2 bpp
    RLPixelFormatCompressedAstc8x8Rgba
  deriving (Eq, Show)

instance Enum RLPixelFormat where
  fromEnum n = case n of
    RLPixelFormatUncompressedGrayscale -> 1
    RLPixelFormatUncompressedGrayAlpha -> 2
    RLPixelFormatUncompressedR5G6B5 -> 3
    RLPixelFormatUncompressedR8G8B8 -> 4
    RLPixelFormatUncompressedR5G5B5A1 -> 5
    RLPixelFormatUncompressedR4G4B4A4 -> 6
    RLPixelFormatUncompressedR8G8B8A8 -> 7
    RLPixelFormatUncompressedR32 -> 8
    RLPixelFormatUncompressedR32G32B32 -> 9
    RLPixelFormatUncompressedR32G32B32A32 -> 10
    RLPixelFormatUncompressedR16 -> 11
    RLPixelFormatUncompressedR16G16B16 -> 12
    RLPixelFormatUncompressedR16G16B16A16 -> 13
    RLPixelFormatCompressedDxt1Rgb -> 14
    RLPixelFormatCompressedDxt1Rgba -> 15
    RLPixelFormatCompressedDxt3Rgba -> 16
    RLPixelFormatCompressedDxt5Rgba -> 17
    RLPixelFormatCompressedEtc1Rgb -> 18
    RLPixelFormatCompressedEtc2Rgb -> 19
    RLPixelFormatCompressedEtc2EacRgba -> 20
    RLPixelFormatCompressedPvrtRgb -> 21
    RLPixelFormatCompressedPvrtRgba -> 22
    RLPixelFormatCompressedAstc4x4Rgba -> 23
    RLPixelFormatCompressedAstc8x8Rgba -> 24

  toEnum n = case n of
    1 -> RLPixelFormatUncompressedGrayscale
    2 -> RLPixelFormatUncompressedGrayAlpha
    3 -> RLPixelFormatUncompressedR5G6B5
    4 -> RLPixelFormatUncompressedR8G8B8
    5 -> RLPixelFormatUncompressedR5G5B5A1
    6 -> RLPixelFormatUncompressedR4G4B4A4
    7 -> RLPixelFormatUncompressedR8G8B8A8
    8 -> RLPixelFormatUncompressedR32
    9 -> RLPixelFormatUncompressedR32G32B32
    10 -> RLPixelFormatUncompressedR32G32B32A32
    11 -> RLPixelFormatUncompressedR16
    12 -> RLPixelFormatUncompressedR16G16B16
    13 -> RLPixelFormatUncompressedR16G16B16A16
    14 -> RLPixelFormatCompressedDxt1Rgb
    15 -> RLPixelFormatCompressedDxt1Rgba
    16 -> RLPixelFormatCompressedDxt3Rgba
    17 -> RLPixelFormatCompressedDxt5Rgba
    18 -> RLPixelFormatCompressedEtc1Rgb
    19 -> RLPixelFormatCompressedEtc2Rgb
    20 -> RLPixelFormatCompressedEtc2EacRgba
    21 -> RLPixelFormatCompressedPvrtRgb
    22 -> RLPixelFormatCompressedPvrtRgba
    23 -> RLPixelFormatCompressedAstc4x4Rgba
    24 -> RLPixelFormatCompressedAstc8x8Rgba
    _ -> error $ "(RLPixelFormat.toEnum) Invalid value: " ++ show n

instance Storable RLPixelFormat where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Texture parameters: filter mode.
-- NOTE 1: Filtering considers mipmaps if available in the texture.
-- NOTE 2: Filter is accordingly set for minification and magnification.
data RLTextureFilter
  = -- | No filter, just pixel approximation
    RLTextureFilterPoint
  | -- | Linear filtering
    RLTextureFilterBilinear
  | -- | Trilinear filtering (linear with mipmaps)
    RLTextureFilterTrilinear
  | -- | Anisotropic filtering 4x
    RLTextureFilterAnisotropic4x
  | -- | Anisotropic filtering 8x
    RLTextureFilterAnisotropic8x
  | -- | Anisotropic filtering 16x
    RLTextureFilterAnisotropic16x
  deriving (Eq, Show, Enum)

instance Storable RLTextureFilter where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Color blending modes (pre-defined)
data RLBlendMode
  = -- | Blend textures considering alpha (default)
    RlBlendAlpha
  | -- | Blend textures adding colors
    RlBlendAdditive
  | -- | Blend textures multiplying colors
    RlBlendMultiplied
  | -- | Blend textures adding colors (alternative)
    RlBlendAddColors
  | -- | Blend textures subtracting colors (alternative)
    RlBlendSubtractColors
  | -- | Blend premultiplied textures considering alpha
    RlBlendAlphaPremultiply
  | -- | Blend textures using custom src/dst factors (use rlSetBlendFactors())
    RlBlendCustom
  | -- | Blend textures using custom src/dst factors (use rlSetBlendFactorsSeparate())
    RlBlendCustomSeparate
  deriving (Eq, Show, Enum)

instance Storable RLBlendMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Shader location point type
data RLShaderLocationIndex
  = -- | Shader location: vertex attribute: position
    RLShaderLocVertexPosition
  | -- | Shader location: vertex attribute: texcoord01
    RLShaderLocVertexTexcoord01
  | -- | Shader location: vertex attribute: texcoord02
    RLShaderLocVertexTexcoord02
  | -- | Shader location: vertex attribute: normal
    RLShaderLocVertexNormal
  | -- | Shader location: vertex attribute: tangent
    RLShaderLocVertexTangent
  | -- | Shader location: vertex attribute: color
    RLShaderLocVertexColor
  | -- | Shader location: matrix uniform: model-view-projection
    RLShaderLocMatrixMVP
  | -- | Shader location: matrix uniform: view (camera transform)
    RLShaderLocMatrixView
  | -- | Shader location: matrix uniform: projection
    RLShaderLocMatrixProjection
  | -- | Shader location: matrix uniform: model (transform)
    RLShaderLocMatrixModel
  | -- | Shader location: matrix uniform: normal
    RLShaderLocMatrixNormal
  | -- | Shader location: vector uniform: view
    RLShaderLocVectorView
  | -- | Shader location: vector uniform: diffuse color
    RLShaderLocColorDiffuse
  | -- | Shader location: vector uniform: specular color
    RLShaderLocColorSpecular
  | -- | Shader location: vector uniform: ambient color
    RLShaderLocColorAmbient
  | -- | Shader location: sampler2d texture: albedo (same as: RL_SHADER_LOC_MAP_DIFFUSE)
    RLShaderLocMapAlbedo
  | -- | Shader location: sampler2d texture: metalness (same as: RL_SHADER_LOC_MAP_SPECULAR)
    RLShaderLocMapMetalness
  | -- | Shader location: sampler2d texture: normal
    RLShaderLocMapNormal
  | -- | Shader location: sampler2d texture: roughness
    RLShaderLocMapRoughness
  | -- | Shader location: sampler2d texture: occlusion
    RLShaderLocMapOcclusion
  | -- | Shader location: sampler2d texture: emission
    RLShaderLocMapEmission
  | -- | Shader location: sampler2d texture: height
    RLShaderLocMapHeight
  | -- | Shader location: samplerCube texture: cubemap
    RLShaderLocMapCubemap
  | -- | Shader location: samplerCube texture: irradiance
    RLShaderLocMapIrradiance
  | -- | Shader location: samplerCube texture: prefilter
    RLShaderLocMapPrefilter
  | -- | Shader location: sampler2d texture: brdf
    RLShaderLocMapBRDF
  deriving (Eq, Show, Enum)

instance Storable RLShaderLocationIndex where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Shader uniform data type
data RLShaderUniformDataType
  = -- | Shader uniform type: float
    RLShaderUniformFloat
  | -- | Shader uniform type: vec2 (2 float)
    RLShaderUniformVec2
  | -- | Shader uniform type: vec3 (3 float)
    RLShaderUniformVec3
  | -- | Shader uniform type: vec4 (4 float)
    RLShaderUniformVec4
  | -- | Shader uniform type: int
    RLShaderUniformInt
  | -- | Shader uniform type: ivec2 (2 int)
    RLShaderUniformIVec2
  | -- | Shader uniform type: ivec3 (3 int)
    RLShaderUniformIVec3
  | -- | Shader uniform type: ivec4 (4 int)
    RLShaderUniformIVec4
  | -- | Shader uniform type: sampler2d
    RLShaderUniformSampler2D
  deriving (Eq, Show, Enum)

instance Storable RLShaderUniformDataType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Shader attribute data types
data RLShaderAttributeDataType
  = -- | Shader attribute type: float
    RLShaderAttribFloat
  | -- | Shader attribute type: vec2 (2 float)
    RLShaderAttribVec2
  | -- | Shader attribute type: vec3 (3 float)
    RLShaderAttribVec3
  | -- | Shader attribute type: vec4 (4 float)
    RLShaderAttribVec4
  deriving (Eq, Show, Enum)

instance Storable RLShaderAttributeDataType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Framebuffer attachment type.
-- NOTE: By default up to 8 color channels are defined, but it can be more
data RLFramebufferAttachType
  = -- | Framebuffer attachment type: color 0
    RLAttachmentColorChannel0
  | -- | Framebuffer attachment type: color 1
    RLAttachmentColorChannel1
  | -- | Framebuffer attachment type: color 2
    RLAttachmentColorChannel2
  | -- | Framebuffer attachment type: color 3
    RLAttachmentColorChannel3
  | -- | Framebuffer attachment type: color 4
    RLAttachmentColorChannel4
  | -- | Framebuffer attachment type: color 5
    RLAttachmentColorChannel5
  | -- | Framebuffer attachment type: color 6
    RLAttachmentColorChannel6
  | -- | Framebuffer attachment type: color 7
    RLAttachmentColorChannel7
  | -- | Framebuffer attachment type: depth
    RLAttachmentDepth
  | -- | Framebuffer attachment type: stencil
    RLAttachmentStencil
  deriving (Eq, Show)

instance Enum RLFramebufferAttachType where
  fromEnum n = case n of
    RLAttachmentColorChannel0 -> 0
    RLAttachmentColorChannel1 -> 1
    RLAttachmentColorChannel2 -> 2
    RLAttachmentColorChannel3 -> 3
    RLAttachmentColorChannel4 -> 4
    RLAttachmentColorChannel5 -> 5
    RLAttachmentColorChannel6 -> 6
    RLAttachmentColorChannel7 -> 7
    RLAttachmentDepth -> 100
    RLAttachmentStencil -> 200

  toEnum n = case n of
    0 -> RLAttachmentColorChannel0
    1 -> RLAttachmentColorChannel1
    2 -> RLAttachmentColorChannel2
    3 -> RLAttachmentColorChannel3
    4 -> RLAttachmentColorChannel4
    5 -> RLAttachmentColorChannel5
    6 -> RLAttachmentColorChannel6
    7 -> RLAttachmentColorChannel7
    100 -> RLAttachmentDepth
    200 -> RLAttachmentStencil
    _ -> error $ "(RLFramebufferAttachType.toEnum) Invalid value: " ++ show n

instance Storable RLFramebufferAttachType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Framebuffer texture attachment type
data RLFramebufferAttachTextureType
  = -- | Framebuffer texture attachment type: cubemap, +X side
    RLAttachmentCubemapPositiveX
  | -- | Framebuffer texture attachment type: cubemap, -X side
    RLAttachmentCubemapNegativeX
  | -- | Framebuffer texture attachment type: cubemap, +Y side
    RLAttachmentCubemapPositiveY
  | -- | Framebuffer texture attachment type: cubemap, -Y side
    RLAttachmentCubemapNegativeY
  | -- | Framebuffer texture attachment type: cubemap, +Z side
    RLAttachmentCubemapPositiveZ
  | -- | Framebuffer texture attachment type: cubemap, -Z side
    RLAttachmentCubemapNegativeZ
  | -- | Framebuffer texture attachment type: texture2d
    RLAttachmentTexture2D
  | -- | Framebuffer texture attachment type: renderbuffer
    RLAttachmentRenderBuffer
  deriving (Eq, Show)

instance Enum RLFramebufferAttachTextureType where
  fromEnum n = case n of
    RLAttachmentCubemapPositiveX -> 0
    RLAttachmentCubemapNegativeX -> 1
    RLAttachmentCubemapPositiveY -> 2
    RLAttachmentCubemapNegativeY -> 3
    RLAttachmentCubemapPositiveZ -> 4
    RLAttachmentCubemapNegativeZ -> 5
    RLAttachmentTexture2D -> 100
    RLAttachmentRenderBuffer -> 200

  toEnum n = case n of
    0 -> RLAttachmentCubemapPositiveX
    1 -> RLAttachmentCubemapNegativeX
    2 -> RLAttachmentCubemapPositiveY
    3 -> RLAttachmentCubemapNegativeY
    4 -> RLAttachmentCubemapPositiveZ
    5 -> RLAttachmentCubemapNegativeZ
    100 -> RLAttachmentTexture2D
    200 -> RLAttachmentRenderBuffer
    _ -> error $ "(RLFramebufferAttachTextureType.toEnum) Invalid value: " ++ show n

instance Storable RLFramebufferAttachTextureType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Face culling mode
data RLCullMode
  = RLCullFaceFront
  | RLCullFaceBack
  deriving (Eq, Show, Enum)

instance Storable RLCullMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Matrix modes (equivalent to OpenGL)
data RLMatrixMode
  -- | GL_MODELVIEW
  = RLModelView
  -- | GL_PROJECTION
  | RLProjection
  -- | GL_TEXTURE
  | RLTexture
  deriving (Eq, Show)

instance Enum RLMatrixMode where
  fromEnum n = case n of
    RLModelView -> 0x1700
    RLProjection -> 0x1701
    RLTexture -> 0x1702

  toEnum n = case n of
    0x1700 -> RLModelView
    0x1701 -> RLProjection
    0x1702 -> RLTexture
    _ -> error $ "(RLMatrixMode.toEnum) Invalid value: " ++ show n

instance Storable RLMatrixMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Primitive assembly draw modes
data RLDrawMode
  -- | GL_LINES
  = RLLines
  -- | GL_TRIANGLES
  | RLTriangles
  -- | GL_QUADS
  | RLQuads
  deriving (Eq, Show)

instance Enum RLDrawMode where
  fromEnum n = case n of
    RLLines -> 0x0001
    RLTriangles -> 0x0004
    RLQuads -> 0x0007

  toEnum n = case n of
    0x0001 -> RLLines
    0x0004 -> RLTriangles
    0x0007 -> RLQuads
    _ -> error $ "(RLDrawMode.toEnum) Invalid value: " ++ show n

instance Storable RLDrawMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Texture parameters (equivalent to OpenGL defines)
data RLTextureParam
  -- | GL_TEXTURE_WRAP_S
  = RLTextureParamWrapS
  -- | GL_TEXTURE_WRAP_T
  | RLTextureParamWrapT
  -- | GL_TEXTURE_MAG_FILTER
  | RLTextureParamMagFilter
  -- | GL_TEXTURE_MIN_FILTER
  | RLTextureParamMinFilter
  -- | GL_NEAREST
  | RLTextureParamFilterNearest
  -- | GL_LINEAR
  | RLTextureParamFilterLinear
  -- | GL_NEAREST_MIPMAP_NEAREST
  | RLTextureParamFilterMipNearest
  -- | GL_NEAREST_MIPMAP_LINEAR
  | RLTextureParamFilterNearestMipLinear
  -- | GL_LINEAR_MIPMAP_NEAREST
  | RLTextureParamFilterLinearMipNearest
  -- | GL_LINEAR_MIPMAP_LINEAR
  | RLTextureParamFilterMipLinear
  -- | Anisotropic filter (custom identifier)
  | RLTextureParamFilterAnisotropic
  -- | Texture mipmap bias, percentage ratio (custom identifier)
  | RLTextureParamMipmapBiasRatio
  deriving (Eq, Show)

instance Enum RLTextureParam where
  fromEnum n = case n of
    RLTextureParamWrapS -> 0x2802
    RLTextureParamWrapT -> 0x2803
    RLTextureParamMagFilter -> 0x2800
    RLTextureParamMinFilter -> 0x2801
    RLTextureParamFilterNearest -> 0x2600
    RLTextureParamFilterLinear -> 0x2601
    RLTextureParamFilterMipNearest -> 0x2700
    RLTextureParamFilterNearestMipLinear -> 0x2702
    RLTextureParamFilterLinearMipNearest -> 0x2701
    RLTextureParamFilterMipLinear -> 0x2703
    RLTextureParamFilterAnisotropic -> 0x3000
    RLTextureParamMipmapBiasRatio -> 0x4000

  toEnum n = case n of
    0x2802 -> RLTextureParamWrapS
    0x2803 -> RLTextureParamWrapT
    0x2800 -> RLTextureParamMagFilter
    0x2801 -> RLTextureParamMinFilter
    0x2600 -> RLTextureParamFilterNearest
    0x2601 -> RLTextureParamFilterLinear
    0x2700 -> RLTextureParamFilterMipNearest
    0x2702 -> RLTextureParamFilterNearestMipLinear
    0x2701 -> RLTextureParamFilterLinearMipNearest
    0x2703 -> RLTextureParamFilterMipLinear
    0x3000 -> RLTextureParamFilterAnisotropic
    0x4000 -> RLTextureParamMipmapBiasRatio
    _ -> error $ "(RLTextureParam.toEnum) Invalid value: " ++ show n

instance Storable RLTextureParam where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | OpenGL shader type
data RLShaderType
  -- | GL_FRAGMENT_SHADER
  = RLFragmentShader
  -- | GL_VERTEX_SHADER
  | RLVertexShader
  -- | GL_COMPUTE_SHADER
  | RLComputeShader
  deriving (Eq, Show)

instance Enum RLShaderType where
  fromEnum n = case n of
    RLFragmentShader -> 0x8B30
    RLVertexShader -> 0x8B31
    RLComputeShader -> 0x91B9

  toEnum n = case n of
    0x8B30 -> RLFragmentShader
    0x8B31 -> RLVertexShader
    0x91B9 -> RLComputeShader
    _ -> error $ "(RLShaderType.toEnum) Invalid value: " ++ show n

instance Storable RLShaderType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | GL buffer usage hint
data RLBufferHint
  -- | GL_STREAM_DRAW
  = RLBufferHintStreamDraw
  -- | GL_STREAM_READ
  | RLBufferHintStreamRead
  -- | GL_STREAM_COPY
  | RLBufferHintStreamCopy
  -- | GL_STATIC_DRAW
  | RLBufferHintStaticDraw
  -- | GL_STATIC_READ
  | RLBufferHintStaticRead
  -- | GL_STATIC_COPY
  | RLBufferHintStaticCopy
  -- | GL_DYNAMIC_DRAW
  | RLBufferHintDynamicDraw
  -- | GL_DYNAMIC_READ
  | RLBufferHintDynamicRead
  -- | GL_DYNAMIC_COPY
  | RLBufferHintDynamicCopy
  deriving (Eq, Show)

instance Enum RLBufferHint where
  fromEnum n = case n of
    RLBufferHintStreamDraw -> 0x88E0
    RLBufferHintStreamRead -> 0x88E1
    RLBufferHintStreamCopy -> 0x88E2
    RLBufferHintStaticDraw -> 0x88E4
    RLBufferHintStaticRead -> 0x88E5
    RLBufferHintStaticCopy -> 0x88E6
    RLBufferHintDynamicDraw -> 0x88E8
    RLBufferHintDynamicRead -> 0x88E9
    RLBufferHintDynamicCopy -> 0x88EA

  toEnum n = case n of
    0x88E0 -> RLBufferHintStreamDraw
    0x88E1 -> RLBufferHintStreamRead
    0x88E2 -> RLBufferHintStreamCopy
    0x88E4 -> RLBufferHintStaticDraw
    0x88E5 -> RLBufferHintStaticRead
    0x88E6 -> RLBufferHintStaticCopy
    0x88E8 -> RLBufferHintDynamicDraw
    0x88E9 -> RLBufferHintDynamicRead
    0x88EA -> RLBufferHintDynamicCopy
    _ -> error $ "(RLBufferHint.toEnum) Invalid value: " ++ show n

instance Storable RLBufferHint where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

------------------------------------------------
-- Raylib structures ---------------------------
------------------------------------------------

---- raylib.h

data Vector2 = Vector2
  { vector2'x :: Float,
    vector2'y :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Vector2 where
  sizeOf _ = 8
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> (peekByteOff _p 0 :: IO CFloat)
    y <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    return $ Vector2 x y
  poke _p (Vector2 x y) = do
    pokeByteOff _p 0 (realToFrac x :: CFloat)
    pokeByteOff _p 4 (realToFrac y :: CFloat)
    return ()

data Vector3 = Vector3
  { vector3'x :: Float,
    vector3'y :: Float,
    vector3'z :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Vector3 where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> (peekByteOff _p 0 :: IO CFloat)
    y <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    z <- realToFrac <$> (peekByteOff _p 8 :: IO CFloat)
    return $ Vector3 x y z
  poke _p (Vector3 x y z) = do
    pokeByteOff _p 0 (realToFrac x :: CFloat)
    pokeByteOff _p 4 (realToFrac y :: CFloat)
    pokeByteOff _p 8 (realToFrac z :: CFloat)
    return ()

data Vector4 = Vector4
  { vector4'x :: Float,
    vector4'y :: Float,
    vector4'z :: Float,
    vector4'w :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Vector4 where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> (peekByteOff _p 0 :: IO CFloat)
    y <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    z <- realToFrac <$> (peekByteOff _p 8 :: IO CFloat)
    w <- realToFrac <$> (peekByteOff _p 12 :: IO CFloat)
    return $ Vector4 x y z w
  poke _p (Vector4 x y z w) = do
    pokeByteOff _p 0 (realToFrac x :: CFloat)
    pokeByteOff _p 4 (realToFrac y :: CFloat)
    pokeByteOff _p 8 (realToFrac z :: CFloat)
    pokeByteOff _p 12 (realToFrac w :: CFloat)
    return ()

type Quaternion = Vector4

data Matrix = Matrix
  { matrix'm0 :: Float,
    matrix'm4 :: Float,
    matrix'm8 :: Float,
    matrix'm12 :: Float,
    matrix'm1 :: Float,
    matrix'm5 :: Float,
    matrix'm9 :: Float,
    matrix'm13 :: Float,
    matrix'm2 :: Float,
    matrix'm6 :: Float,
    matrix'm10 :: Float,
    matrix'm14 :: Float,
    matrix'm3 :: Float,
    matrix'm7 :: Float,
    matrix'm11 :: Float,
    matrix'm15 :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Matrix where
  sizeOf _ = 64
  alignment _ = 4
  peek _p = do
    m0 <- realToFrac <$> (peekByteOff _p 0 :: IO CFloat)
    m4 <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    m8 <- realToFrac <$> (peekByteOff _p 8 :: IO CFloat)
    m12 <- realToFrac <$> (peekByteOff _p 12 :: IO CFloat)
    m1 <- realToFrac <$> (peekByteOff _p 16 :: IO CFloat)
    m5 <- realToFrac <$> (peekByteOff _p 20 :: IO CFloat)
    m9 <- realToFrac <$> (peekByteOff _p 24 :: IO CFloat)
    m13 <- realToFrac <$> (peekByteOff _p 28 :: IO CFloat)
    m2 <- realToFrac <$> (peekByteOff _p 32 :: IO CFloat)
    m6 <- realToFrac <$> (peekByteOff _p 36 :: IO CFloat)
    m10 <- realToFrac <$> (peekByteOff _p 40 :: IO CFloat)
    m14 <- realToFrac <$> (peekByteOff _p 44 :: IO CFloat)
    m3 <- realToFrac <$> (peekByteOff _p 48 :: IO CFloat)
    m7 <- realToFrac <$> (peekByteOff _p 52 :: IO CFloat)
    m11 <- realToFrac <$> (peekByteOff _p 56 :: IO CFloat)
    m15 <- realToFrac <$> (peekByteOff _p 60 :: IO CFloat)
    return $ Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15
  poke _p (Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) = do
    pokeByteOff _p 0 (realToFrac m0 :: CFloat)
    pokeByteOff _p 4 (realToFrac m4 :: CFloat)
    pokeByteOff _p 8 (realToFrac m8 :: CFloat)
    pokeByteOff _p 12 (realToFrac m12 :: CFloat)
    pokeByteOff _p 16 (realToFrac m1 :: CFloat)
    pokeByteOff _p 20 (realToFrac m5 :: CFloat)
    pokeByteOff _p 24 (realToFrac m9 :: CFloat)
    pokeByteOff _p 28 (realToFrac m13 :: CFloat)
    pokeByteOff _p 32 (realToFrac m2 :: CFloat)
    pokeByteOff _p 36 (realToFrac m6 :: CFloat)
    pokeByteOff _p 40 (realToFrac m10 :: CFloat)
    pokeByteOff _p 44 (realToFrac m14 :: CFloat)
    pokeByteOff _p 48 (realToFrac m3 :: CFloat)
    pokeByteOff _p 52 (realToFrac m7 :: CFloat)
    pokeByteOff _p 56 (realToFrac m11 :: CFloat)
    pokeByteOff _p 60 (realToFrac m15 :: CFloat)
    return ()

vectorToColor :: Vector4 -> Color
vectorToColor (Vector4 x y z w) = Color (round $ x * 255) (round $ y * 255) (round $ z * 255) (round $ w * 255)

data Color = Color
  { color'r :: Word8,
    color'g :: Word8,
    color'b :: Word8,
    color'a :: Word8
  }
  deriving (Eq, Show, Freeable)

instance Storable Color where
  sizeOf _ = 4
  alignment _ = 1
  peek _p = do
    r <- fromIntegral <$> (peekByteOff _p 0 :: IO CUChar)
    g <- fromIntegral <$> (peekByteOff _p 1 :: IO CUChar)
    b <- fromIntegral <$> (peekByteOff _p 2 :: IO CUChar)
    a <- fromIntegral <$> (peekByteOff _p 3 :: IO CUChar)
    return $ Color r g b a
  poke _p (Color r g b a) = do
    pokeByteOff _p 0 (fromIntegral r :: CUChar)
    pokeByteOff _p 1 (fromIntegral g :: CUChar)
    pokeByteOff _p 2 (fromIntegral b :: CUChar)
    pokeByteOff _p 3 (fromIntegral a :: CUChar)
    return ()

data Rectangle = Rectangle
  { rectangle'x :: Float,
    rectangle'y :: Float,
    rectangle'width :: Float,
    rectangle'height :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Rectangle where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> (peekByteOff _p 0 :: IO CFloat)
    y <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    width <- realToFrac <$> (peekByteOff _p 8 :: IO CFloat)
    height <- realToFrac <$> (peekByteOff _p 12 :: IO CFloat)
    return $ Rectangle x y width height
  poke _p (Rectangle x y width height) = do
    pokeByteOff _p 0 (realToFrac x :: CFloat)
    pokeByteOff _p 4 (realToFrac y :: CFloat)
    pokeByteOff _p 8 (realToFrac width :: CFloat)
    pokeByteOff _p 12 (realToFrac height :: CFloat)
    return ()

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

data Camera3D = Camera3D
  { camera3D'position :: Vector3,
    camera3D'target :: Vector3,
    camera3D'up :: Vector3,
    camera3D'fovy :: Float,
    camera3D'projection :: CameraProjection
  }
  deriving (Eq, Show, Freeable)

instance Storable Camera3D where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    position <- peekByteOff _p 0
    target <- peekByteOff _p 12
    up <- peekByteOff _p 24
    fovy <- realToFrac <$> (peekByteOff _p 36 :: IO CFloat)
    projection <- peekByteOff _p 40
    return $ Camera3D position target up fovy projection
  poke _p (Camera3D position target up fovy projection) = do
    pokeByteOff _p 0 position
    pokeByteOff _p 12 target
    pokeByteOff _p 24 up
    pokeByteOff _p 36 (realToFrac fovy :: CFloat)
    pokeByteOff _p 40 projection
    return ()

type Camera = Camera3D

data Camera2D = Camera2D
  { camera2D'offset :: Vector2,
    camera2D'target :: Vector2,
    camera2D'rotation :: Float,
    camera2D'zoom :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Camera2D where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    offset <- peekByteOff _p 0
    target <- peekByteOff _p 8
    rotation <- realToFrac <$> (peekByteOff _p 16 :: IO CFloat)
    zoom <- realToFrac <$> (peekByteOff _p 20 :: IO CFloat)
    return $ Camera2D offset target rotation zoom
  poke _p (Camera2D offset target rotation zoom) = do
    pokeByteOff _p 0 offset
    pokeByteOff _p 8 target
    pokeByteOff _p 16 (realToFrac rotation :: CFloat)
    pokeByteOff _p 20 (realToFrac zoom :: CFloat)
    return ()

data Mesh = Mesh
  { mesh'vertexCount :: Int,
    mesh'triangleCount :: Int,
    mesh'vertices :: [Vector3],
    mesh'texcoords :: [Vector2],
    mesh'texcoords2 :: Maybe [Vector2],
    mesh'normals :: [Vector3],
    mesh'tangents :: Maybe [Vector4],
    mesh'colors :: Maybe [Color],
    mesh'indices :: Maybe [Word16],
    mesh'animVertices :: Maybe [Vector3],
    mesh'animNormals :: Maybe [Vector3],
    mesh'boneIds :: Maybe [Word8],
    mesh'boneWeights :: Maybe [Float],
    mesh'vaoId :: Integer,
    mesh'vboId :: Maybe [Integer]
  }
  deriving (Eq, Show)

instance Storable Mesh where
  sizeOf _ = 112
  alignment _ = 8
  peek _p = do
    vertexCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    triangleCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    verticesPtr <- (peekByteOff _p 8 :: IO (Ptr Vector3))
    vertices <- peekArray vertexCount verticesPtr
    texcoordsPtr <- (peekByteOff _p 16 :: IO (Ptr Vector2))
    texcoords <- peekArray vertexCount texcoordsPtr
    texcoords2Ptr <- (peekByteOff _p 24 :: IO (Ptr Vector2))
    texcoords2 <- peekMaybeArray vertexCount texcoords2Ptr
    normalsPtr <- (peekByteOff _p 32 :: IO (Ptr Vector3))
    normals <- peekArray vertexCount normalsPtr
    tangentsPtr <- (peekByteOff _p 40 :: IO (Ptr Vector4))
    tangents <- peekMaybeArray vertexCount tangentsPtr
    colorsPtr <- (peekByteOff _p 48 :: IO (Ptr Color))
    colors <- peekMaybeArray vertexCount colorsPtr
    indicesPtr <- (peekByteOff _p 56 :: IO (Ptr CUShort))
    indices <- (\m -> map fromIntegral <$> m) <$> peekMaybeArray vertexCount indicesPtr
    animVerticesPtr <- (peekByteOff _p 64 :: IO (Ptr Vector3))
    animVertices <- peekMaybeArray vertexCount animVerticesPtr
    animNormalsPtr <- (peekByteOff _p 72 :: IO (Ptr Vector3))
    animNormals <- peekMaybeArray vertexCount animNormalsPtr
    boneIdsPtr <- (peekByteOff _p 80 :: IO (Ptr CUChar))
    boneIds <- (\m -> map fromIntegral <$> m) <$> peekMaybeArray (vertexCount * 4) boneIdsPtr
    boneWeightsPtr <- (peekByteOff _p 88 :: IO (Ptr CFloat))
    boneWeights <- (map realToFrac <$>) <$> peekMaybeArray (vertexCount * 4) boneWeightsPtr
    vaoId <- fromIntegral <$> (peekByteOff _p 96 :: IO CUInt)
    vboIdPtr <- (peekByteOff _p 104 :: IO (Ptr CUInt))
    vboId <- (\m -> map fromIntegral <$> m) <$> peekMaybeArray 7 vboIdPtr
    return $ Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices animVertices animNormals boneIds boneWeights vaoId vboId
  poke _p (Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices animVertices animNormals boneIds boneWeights vaoId vboId) = do
    pokeByteOff _p 0 (fromIntegral vertexCount :: CInt)
    pokeByteOff _p 4 (fromIntegral triangleCount :: CInt)
    pokeByteOff _p 8 =<< newArray vertices
    pokeByteOff _p 16 =<< newArray texcoords
    newMaybeArray texcoords2 >>= pokeByteOff _p 24
    pokeByteOff _p 32 =<< newArray normals
    newMaybeArray tangents >>= pokeByteOff _p 40
    newMaybeArray colors >>= pokeByteOff _p 48
    newMaybeArray (map fromIntegral <$> indices :: Maybe [CUShort]) >>= pokeByteOff _p 56
    newMaybeArray animVertices >>= pokeByteOff _p 64
    newMaybeArray animNormals >>= pokeByteOff _p 72
    newMaybeArray (map fromIntegral <$> boneIds :: Maybe [CUChar]) >>= pokeByteOff _p 80
    newMaybeArray (map realToFrac <$> boneWeights :: Maybe [CFloat]) >>= pokeByteOff _p 88
    pokeByteOff _p 96 (fromIntegral vaoId :: CUInt)
    newMaybeArray (map fromIntegral <$> vboId :: Maybe [CUInt]) >>= pokeByteOff _p 104
    return ()

instance Freeable Mesh where
  rlFreeDependents _ ptr = do
    verticesPtr <- (peekByteOff ptr 8 :: IO (Ptr Float))
    c'free $ castPtr verticesPtr
    texcoordsPtr <- (peekByteOff ptr 16 :: IO (Ptr Vector2))
    c'free $ castPtr texcoordsPtr
    texcoords2Ptr <- (peekByteOff ptr 24 :: IO (Ptr Vector2))
    freeMaybePtr $ castPtr texcoords2Ptr
    normalsPtr <- (peekByteOff ptr 32 :: IO (Ptr Vector3))
    c'free $ castPtr normalsPtr
    tangentsPtr <- (peekByteOff ptr 40 :: IO (Ptr Vector4))
    freeMaybePtr $ castPtr tangentsPtr
    colorsPtr <- (peekByteOff ptr 48 :: IO (Ptr Color))
    freeMaybePtr $ castPtr colorsPtr
    indicesPtr <- (peekByteOff ptr 56 :: IO (Ptr CUShort))
    freeMaybePtr $ castPtr indicesPtr
    animVerticesPtr <- (peekByteOff ptr 64 :: IO (Ptr Vector3))
    freeMaybePtr $ castPtr animVerticesPtr
    animNormalsPtr <- (peekByteOff ptr 72 :: IO (Ptr Vector3))
    freeMaybePtr $ castPtr animNormalsPtr
    boneIdsPtr <- (peekByteOff ptr 80 :: IO (Ptr CUChar))
    freeMaybePtr $ castPtr boneIdsPtr
    boneWeightsPtr <- (peekByteOff ptr 88 :: IO (Ptr CFloat))
    freeMaybePtr $ castPtr boneWeightsPtr
    vboIdPtr <- (peekByteOff ptr 104 :: IO (Ptr CUInt))
    c'free $ castPtr vboIdPtr

data Shader = Shader
  { shader'id :: Integer,
    shader'locs :: [Int]
  }
  deriving (Eq, Show)

instance Storable Shader where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    sId <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    locsPtr <- (peekByteOff _p 8 :: IO (Ptr CInt))
    locs <- map fromIntegral <$> peekArray 32 locsPtr
    return $ Shader sId locs
  poke _p (Shader sId locs) = do
    pokeByteOff _p 0 (fromIntegral sId :: CUInt)
    defaultShaderId <- c'rlGetShaderIdDefault
    locsArr <- newArray (map fromIntegral locs :: [CInt])
    if sId == fromIntegral defaultShaderId
      then do
        locsPtr <- newForeignPtr p'free locsArr
        withForeignPtr locsPtr $ pokeByteOff _p 8
      else pokeByteOff _p 8 locsArr
    return ()

instance Freeable Shader where
  rlFreeDependents val ptr = do
    defaultShaderId <- c'rlGetShaderIdDefault
    unless
      (shader'id val == fromIntegral defaultShaderId)
      ( do
          locsPtr <- (peekByteOff ptr 8 :: IO (Ptr CInt))
          c'free $ castPtr locsPtr
      )

data MaterialMap = MaterialMap
  { materialMap'texture :: Texture,
    materialMap'color :: Color,
    materialMap'value :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable MaterialMap where
  sizeOf _ = 28
  alignment _ = 4
  peek _p = do
    texture <- peekByteOff _p 0
    color <- peekByteOff _p 20
    value <- realToFrac <$> (peekByteOff _p 24 :: IO CFloat)
    return $ MaterialMap texture color value
  poke _p (MaterialMap texture color value) = do
    pokeByteOff _p 0 texture
    pokeByteOff _p 20 color
    pokeByteOff _p 24 (realToFrac value :: CFloat)
    return ()

data Material = Material
  { material'shader :: Shader,
    material'maps :: Maybe [MaterialMap],
    material'params :: [Float]
  }
  deriving (Eq, Show)

instance Storable Material where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    shader <- peekByteOff _p 0
    mapsPtr <- (peekByteOff _p 16 :: IO (Ptr MaterialMap))
    maps <- peekMaybeArray 12 mapsPtr
    params <- map realToFrac <$> peekStaticArrayOff 4 (castPtr _p :: Ptr CFloat) 24
    return $ Material shader maps params
  poke _p (Material shader maps params) = do
    pokeByteOff _p 0 shader
    pokeByteOff _p 16 =<< newMaybeArray maps
    pokeStaticArrayOff (castPtr _p :: Ptr CFloat) 24 (map realToFrac params :: [CFloat])
    return ()

instance Freeable Material where
  rlFreeDependents val ptr = do
    rlFreeDependents (material'shader val) (castPtr ptr :: Ptr Shader)
    mapsPtr <- (peekByteOff ptr 16 :: IO (Ptr MaterialMap))
    rlFreeMaybeArray (material'maps val) mapsPtr

data Transform = Transform
  { transform'translation :: Vector3,
    transform'rotation :: Quaternion,
    transform'scale :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable Transform where
  sizeOf _ = 40
  alignment _ = 4
  peek _p = do
    translation <- peekByteOff _p 0
    rotation <- peekByteOff _p 12
    scale <- peekByteOff _p 28
    return $ Transform translation rotation scale
  poke _p (Transform translation rotation scale) = do
    pokeByteOff _p 0 translation
    pokeByteOff _p 12 rotation
    pokeByteOff _p 28 scale
    return ()

data BoneInfo = BoneInfo
  { boneInfo'name :: String,
    boneinfo'parent :: Int
  }
  deriving (Eq, Show, Freeable)

instance Storable BoneInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    name <- map castCCharToChar . takeWhile (/= 0) <$> peekStaticArray 32 (castPtr _p :: Ptr CChar)
    parent <- fromIntegral <$> (peekByteOff _p 32 :: IO CInt)
    return $ BoneInfo name parent
  poke _p (BoneInfo name parent) = do
    pokeStaticArray (castPtr _p :: Ptr CChar) (rightPad 32 0 $ map castCharToCChar name)
    pokeByteOff _p 32 (fromIntegral parent :: CInt)
    return ()

data Model = Model
  { model'transform :: Matrix,
    model'meshes :: [Mesh],
    model'materials :: [Material],
    model'meshMaterial :: [Int],
    model'boneCount :: Int,
    model'bones :: Maybe [BoneInfo],
    model'bindPose :: Maybe [Transform]
  }
  deriving (Eq, Show)

instance Storable Model where
  sizeOf _ = 120
  alignment _ = 4
  peek _p = do
    transform <- peekByteOff _p 0
    meshCount <- fromIntegral <$> (peekByteOff _p 64 :: IO CInt)
    materialCount <- fromIntegral <$> (peekByteOff _p 68 :: IO CInt)
    meshesPtr <- (peekByteOff _p 72 :: IO (Ptr Mesh))
    meshes <- peekArray meshCount meshesPtr
    materialsPtr <- (peekByteOff _p 80 :: IO (Ptr Material))
    materials <- peekArray materialCount materialsPtr
    meshMaterialPtr <- (peekByteOff _p 88 :: IO (Ptr CInt))
    meshMaterial <- map fromIntegral <$> peekArray meshCount meshMaterialPtr
    boneCount <- fromIntegral <$> (peekByteOff _p 96 :: IO CInt)
    bonesPtr <- (peekByteOff _p 104 :: IO (Ptr BoneInfo))
    bones <- peekMaybeArray boneCount bonesPtr
    bindPosePtr <- (peekByteOff _p 112 :: IO (Ptr Transform))
    bindPose <- peekMaybeArray boneCount bindPosePtr
    return $ Model transform meshes materials meshMaterial boneCount bones bindPose
  poke _p (Model transform meshes materials meshMaterial boneCount bones bindPose) = do
    pokeByteOff _p 0 transform
    pokeByteOff _p 64 (fromIntegral $ length meshes :: CInt)
    pokeByteOff _p 68 (fromIntegral $ length materials :: CInt)
    pokeByteOff _p 72 =<< newArray meshes
    pokeByteOff _p 80 =<< newArray materials
    pokeByteOff _p 88 =<< newArray (map fromIntegral meshMaterial :: [CInt])
    pokeByteOff _p 96 (fromIntegral boneCount :: CInt)
    newMaybeArray bones >>= pokeByteOff _p 104
    newMaybeArray bindPose >>= pokeByteOff _p 112
    return ()

instance Freeable Model where
  rlFreeDependents val ptr = do
    meshesPtr <- (peekByteOff ptr 72 :: IO (Ptr Mesh))
    rlFreeArray (model'meshes val) meshesPtr
    materialsPtr <- (peekByteOff ptr 80 :: IO (Ptr Material))
    rlFreeArray (model'materials val) materialsPtr
    meshMaterialPtr <- (peekByteOff ptr 88 :: IO (Ptr CInt))
    c'free $ castPtr meshMaterialPtr
    bonesPtr <- (peekByteOff ptr 104 :: IO (Ptr BoneInfo))
    freeMaybePtr $ castPtr bonesPtr
    bindPosePtr <- (peekByteOff ptr 112 :: IO (Ptr Transform))
    freeMaybePtr $ castPtr bindPosePtr

data ModelAnimation = ModelAnimation
  { modelAnimation'boneCount :: Int,
    modelAnimation'frameCount :: Int,
    modelAnimation'bones :: [BoneInfo],
    modelAnimation'framePoses :: [[Transform]],
    modelAnimation'name :: String
  }
  deriving (Eq, Show)

instance Storable ModelAnimation where
  sizeOf _ = 56
  alignment _ = 4
  peek _p = do
    boneCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    frameCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    bonesPtr <- (peekByteOff _p 8 :: IO (Ptr BoneInfo))
    bones <- peekArray boneCount bonesPtr
    framePosesPtr <- (peekByteOff _p 16 :: IO (Ptr (Ptr Transform)))
    framePosesPtrArr <- peekArray frameCount framePosesPtr
    framePoses <- mapM (peekArray boneCount) framePosesPtrArr
    name <- map castCCharToChar <$> peekStaticArrayOff 32 (castPtr _p) 24
    return $ ModelAnimation boneCount frameCount bones framePoses name
  poke _p (ModelAnimation boneCount frameCount bones framePoses name) = do
    pokeByteOff _p 0 (fromIntegral boneCount :: CInt)
    pokeByteOff _p 4 (fromIntegral frameCount :: CInt)
    pokeByteOff _p 8 =<< newArray bones
    mapM newArray framePoses >>= newArray >>= pokeByteOff _p 16
    pokeStaticArrayOff (castPtr _p) 24 (map castCharToCChar name)
    return ()

instance Freeable ModelAnimation where
  rlFreeDependents val ptr = do
    bonesPtr <- (peekByteOff ptr 8 :: IO (Ptr BoneInfo))
    c'free $ castPtr bonesPtr
    framePosesPtr <- (peekByteOff ptr 16 :: IO (Ptr (Ptr Transform)))
    framePosesPtrArr <- peekArray (modelAnimation'frameCount val) framePosesPtr
    forM_ framePosesPtrArr (c'free . castPtr)
    c'free $ castPtr framePosesPtr

data Ray = Ray
  { ray'position :: Vector3,
    ray'direction :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable Ray where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    position <- peekByteOff _p 0
    direction <- peekByteOff _p 12
    return $ Ray position direction
  poke _p (Ray position direction) = do
    pokeByteOff _p 0 position
    pokeByteOff _p 12 direction
    return ()

data RayCollision = RayCollision
  { rayCollision'hit :: Bool,
    rayCollision'distance :: Float,
    rayCollision'point :: Vector3,
    rayCollision'normal :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable RayCollision where
  sizeOf _ = 32
  alignment _ = 4
  peek _p = do
    hit <- toBool <$> (peekByteOff _p 0 :: IO CBool)
    distance <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    point <- peekByteOff _p 8
    normal <- peekByteOff _p 20
    return $ RayCollision hit distance point normal
  poke _p (RayCollision hit distance point normal) = do
    pokeByteOff _p 0 (fromBool hit :: CInt)
    pokeByteOff _p 4 (realToFrac distance :: CFloat)
    pokeByteOff _p 8 point
    pokeByteOff _p 20 normal
    return ()

data BoundingBox = BoundingBox
  { boundingBox'min :: Vector3,
    boundingBox'max :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable BoundingBox where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    bMin <- peekByteOff _p 0
    bMax <- peekByteOff _p 12
    return $ BoundingBox bMin bMax
  poke _p (BoundingBox bMin bMax) = do
    pokeByteOff _p 0 bMin
    pokeByteOff _p 12 bMax
    return ()

data Wave = Wave
  { wave'frameCount :: Integer,
    wave'sampleRate :: Integer,
    wave'sampleSize :: Integer,
    wave'channels :: Integer,
    wave'data :: [Int]
  }
  deriving (Eq, Show)

instance Storable Wave where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    frameCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    sampleRate <- fromIntegral <$> (peekByteOff _p 4 :: IO CUInt)
    sampleSize <- fromIntegral <$> (peekByteOff _p 8 :: IO CUInt)
    channels <- fromIntegral <$> (peekByteOff _p 12 :: IO CUInt)
    wDataPtr <- (peekByteOff _p 16 :: IO (Ptr CShort))
    wData <- map fromIntegral <$> peekArray (fromInteger $ frameCount * channels) wDataPtr
    return $ Wave frameCount sampleRate sampleSize channels wData
  poke _p (Wave frameCount sampleRate sampleSize channels wData) = do
    pokeByteOff _p 0 (fromIntegral frameCount :: CUInt)
    pokeByteOff _p 4 (fromIntegral sampleRate :: CUInt)
    pokeByteOff _p 8 (fromIntegral sampleSize :: CUInt)
    pokeByteOff _p 12 (fromIntegral channels :: CUInt)
    pokeByteOff _p 16 =<< newArray (map fromIntegral wData :: [CShort])
    return ()

instance Freeable Wave where
  rlFreeDependents _ ptr = do
    dataPtr <- peekByteOff ptr 16 :: IO (Ptr CShort)
    c'free $ castPtr dataPtr

-- RAudioBuffer/Processor don't work perfectly right now, I need to fix them later on.
-- They are currently used as `Ptr`s because peeking/poking them every time
-- an audio function is called doesn't work properly (they are stored in a
-- linked list in C, which makes it very difficult to properly marshal them)
data RAudioBuffer = RAudioBuffer
  { rAudioBuffer'converter :: [Int], -- Implemented as an array of 39 integers because binding the entire `ma_data_converter` type is too painful
    rAudioBuffer'callback :: AudioCallback,
    rAudioBuffer'processor :: Maybe RAudioProcessor,
    rAudioBuffer'volume :: Float,
    rAudioBuffer'pitch :: Float,
    rAudioBuffer'pan :: Float,
    rAudioBuffer'playing :: Bool,
    rAudioBuffer'paused :: Bool,
    rAudioBuffer'looping :: Bool,
    rAudioBuffer'usage :: Int,
    rAudioBuffer'isSubBufferProcessed :: [Bool],
    rAudioBuffer'sizeInFrames :: Integer,
    rAudioBuffer'frameCursorPos :: Integer,
    rAudioBuffer'framesProcessed :: Integer,
    rAudioBuffer'data :: [Word8],
    rAudioBuffer'next :: Maybe RAudioBuffer,
    rAudioBuffer'prev :: Maybe RAudioBuffer
  }
  deriving (Eq, Show, Freeable)

instance Storable RAudioBuffer where
  sizeOf _ = 392
  alignment _ = 8
  peek _p = do
    base <- loadBase _p
    nextPtr <- peekByteOff _p 376
    next <- loadNext nextPtr
    prevPtr <- peekByteOff _p 384
    prev <- loadPrev prevPtr
    return $
      let p =
            base
              ((\a -> a {rAudioBuffer'prev = Just p}) <$> next)
              ((\a -> a {rAudioBuffer'next = Just p}) <$> prev)
       in p
    where
      getBytesPerSample = ([0, 1, 2, 3, 4, 4] !!)
      loadBase ptr = do
        converter <- map fromIntegral <$> (peekStaticArray 39 (castPtr ptr) :: IO [CInt])
        callback <- peekByteOff ptr 312
        pPtr <- peekByteOff ptr 320 :: IO (Ptr RAudioProcessor)
        processor <- if pPtr == nullPtr then return Nothing else Just <$> peek pPtr

        volume <- realToFrac <$> (peekByteOff ptr 328 :: IO CFloat)
        pitch <- realToFrac <$> (peekByteOff ptr 332 :: IO CFloat)
        pan <- realToFrac <$> (peekByteOff ptr 336 :: IO CFloat)

        playing <- toBool <$> (peekByteOff ptr 340 :: IO CBool)
        paused <- toBool <$> (peekByteOff ptr 341 :: IO CBool)
        looping <- toBool <$> (peekByteOff ptr 342 :: IO CBool)
        usage <- fromIntegral <$> (peekByteOff ptr 344 :: IO CInt)

        isSubBufferProcessed <- map toBool <$> peekStaticArrayOff 2 (castPtr ptr :: Ptr CBool) 348
        sizeInFrames <- fromIntegral <$> (peekByteOff ptr 352 :: IO CUInt)
        frameCursorPos <- fromIntegral <$> (peekByteOff ptr 356 :: IO CUInt)
        framesProcessed <- fromIntegral <$> (peekByteOff ptr 360 :: IO CUInt)

        bData <- map fromIntegral <$> (peekArray (fromIntegral $ sizeInFrames * 2 * getBytesPerSample (head converter)) =<< (peekByteOff ptr 368 :: IO (Ptr CUChar)))

        return $ RAudioBuffer converter callback processor volume pitch pan playing paused looping usage isSubBufferProcessed sizeInFrames frameCursorPos framesProcessed bData
      loadNext ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            base <- loadBase ptr
            nextPtr <- peekByteOff ptr 376
            next <- loadNext nextPtr
            let p = base ((\a -> a {rAudioBuffer'prev = Just p}) <$> next) Nothing
             in return (Just p)

      loadPrev ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            base <- loadBase ptr
            prevPtr <- peekByteOff ptr 384
            prev <- loadPrev prevPtr
            let p = base Nothing ((\a -> a {rAudioBuffer'next = Just p}) <$> prev)
             in return (Just p)
  poke _p a = do
    pokeBase _p a
    pokeNext _p $ rAudioBuffer'next a
    pokePrev _p $ rAudioBuffer'prev a
    return ()
    where
      pokeBase ptr (RAudioBuffer converter callback processor volume pitch pan playing paused looping usage isSubBufferProcessed sizeInFrames frameCursorPos framesProcessed bData _ _) = do
        pokeStaticArray (castPtr ptr) (map fromIntegral converter :: [CInt])
        pokeByteOff ptr 312 callback
        pokeMaybeOff (castPtr ptr) 320 processor

        pokeByteOff ptr 328 (realToFrac volume :: CFloat)
        pokeByteOff ptr 332 (realToFrac pitch :: CFloat)
        pokeByteOff ptr 336 (realToFrac pan :: CFloat)

        pokeByteOff ptr 340 (fromBool playing :: CBool)
        pokeByteOff ptr 341 (fromBool paused :: CBool)
        pokeByteOff ptr 342 (fromBool looping :: CBool)
        pokeByteOff ptr 344 (fromIntegral usage :: CInt)

        pokeStaticArrayOff (castPtr ptr) 348 (map fromBool isSubBufferProcessed :: [CBool])
        pokeByteOff ptr 352 (fromIntegral sizeInFrames :: CUInt)
        pokeByteOff ptr 356 (fromIntegral frameCursorPos :: CUInt)
        pokeByteOff ptr 360 (fromIntegral framesProcessed :: CUInt)

        pokeByteOff ptr 368 =<< newArray (map fromIntegral bData :: [CUChar])

        return ()
      pokeNext basePtr pNext =
        case pNext of
          Nothing -> pokeByteOff basePtr 376 nullPtr
          Just val -> do
            nextPtr <- malloc
            pokeBase nextPtr val
            pokeNext nextPtr (rAudioBuffer'next val)
            pokeByteOff nextPtr 384 basePtr
            pokeByteOff basePtr 376 nextPtr
      pokePrev basePtr pPrev =
        case pPrev of
          Nothing -> pokeByteOff basePtr 384 nullPtr
          Just val -> do
            prevPtr <- malloc
            pokeBase prevPtr val
            pokeByteOff prevPtr 376 basePtr
            pokePrev prevPtr (rAudioBuffer'prev val)
            pokeByteOff basePtr 384 prevPtr

data RAudioProcessor = RAudioProcessor
  { rAudioProcessor'process :: Maybe AudioCallback,
    rAudioProcessor'next :: Maybe RAudioProcessor,
    rAudioProcessor'prev :: Maybe RAudioProcessor
  }
  deriving (Eq, Show, Freeable)

instance Storable RAudioProcessor where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    process <- loadProcess _p
    nextPtr <- peekByteOff _p 8
    next <- loadNext nextPtr
    prevPtr <- peekByteOff _p 16
    prev <- loadPrev prevPtr
    return $ let p = RAudioProcessor process ((\a -> a {rAudioProcessor'prev = Just p}) <$> next) ((\a -> a {rAudioProcessor'next = Just p}) <$> prev) in p
    where
      loadProcess ptr = do
        funPtr <- peekByteOff ptr 0
        if funPtr == nullFunPtr then return Nothing else return (Just funPtr)
      loadNext ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            process <- loadProcess ptr
            nextPtr <- peekByteOff ptr 8
            next <- loadNext nextPtr
            let p = RAudioProcessor process ((\a -> a {rAudioProcessor'prev = Just p}) <$> next) Nothing
             in return (Just p)

      loadPrev ptr =
        if ptr == nullPtr
          then return Nothing
          else do
            process <- loadProcess ptr
            prevPtr <- peekByteOff ptr 16
            prev <- loadPrev prevPtr
            let p = RAudioProcessor process Nothing ((\a -> a {rAudioProcessor'next = Just p}) <$> prev)
             in return (Just p)
  poke _p (RAudioProcessor process next prev) = do
    pokeMaybeOff (castPtr _p) 0 process
    pokeNext (castPtr _p) next
    pokePrev (castPtr _p) prev
    return ()
    where
      pokeNext basePtr pNext =
        case pNext of
          Nothing -> pokeByteOff basePtr 8 nullPtr
          Just val -> do
            nextPtr <- malloc
            pokeMaybeOff nextPtr 0 (rAudioProcessor'process val)
            pokeNext nextPtr (rAudioProcessor'next val)
            pokeByteOff nextPtr 16 basePtr
            pokeByteOff basePtr 8 nextPtr
      pokePrev basePtr pPrev =
        case pPrev of
          Nothing -> pokeByteOff basePtr 16 nullPtr
          Just val -> do
            prevPtr <- malloc
            pokeMaybeOff prevPtr 0 (rAudioProcessor'process val)
            pokeByteOff prevPtr 8 basePtr
            pokePrev prevPtr (rAudioProcessor'prev val)
            pokeByteOff basePtr 16 prevPtr

data AudioStream = AudioStream
  { audioStream'buffer :: Ptr RAudioBuffer,
    audioStream'processor :: Ptr RAudioProcessor,
    audioStream'sampleRate :: Integer,
    audioStream'sampleSize :: Integer,
    audiostream'channels :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable AudioStream where
  sizeOf _ = 32
  alignment _ = 8
  peek _p = do
    buffer <- peekByteOff _p 0
    processor <- peekByteOff _p 8
    sampleRate <- fromIntegral <$> (peekByteOff _p 16 :: IO CUInt)
    sampleSize <- fromIntegral <$> (peekByteOff _p 20 :: IO CUInt)
    channels <- fromIntegral <$> (peekByteOff _p 24 :: IO CUInt)
    return $ AudioStream buffer processor sampleRate sampleSize channels
  poke _p (AudioStream buffer processor sampleRate sampleSize channels) = do
    pokeByteOff _p 0 buffer
    pokeByteOff _p 8 processor
    pokeByteOff _p 16 (fromIntegral sampleRate :: CUInt)
    pokeByteOff _p 20 (fromIntegral sampleSize :: CUInt)
    pokeByteOff _p 24 (fromIntegral channels :: CUInt)
    return ()

data Sound = Sound
  { sound'stream :: AudioStream,
    sound'frameCount :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable Sound where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    stream <- peekByteOff _p 0
    frameCount <- fromIntegral <$> (peekByteOff _p 32 :: IO CUInt)
    return $ Sound stream frameCount
  poke _p (Sound stream frameCount) = do
    pokeByteOff _p 0 stream
    pokeByteOff _p 32 (fromIntegral frameCount :: CUInt)
    return ()

data Music = Music
  { music'stream :: AudioStream,
    music'frameCount :: Integer,
    music'looping :: Bool,
    music'ctxType :: MusicContextType,
    music'ctxData :: Ptr ()
  }
  deriving (Eq, Show, Freeable)

instance Storable Music where
  sizeOf _ = 56
  alignment _ = 4
  peek _p = do
    stream <- peekByteOff _p 0
    frameCount <- fromIntegral <$> (peekByteOff _p 32 :: IO CUInt)
    looping <- toBool <$> (peekByteOff _p 36 :: IO CBool)
    ctxType <- peekByteOff _p 40
    ctxData <- peekByteOff _p 48
    return $ Music stream frameCount looping ctxType ctxData
  poke _p (Music stream frameCount looping ctxType ctxData) = do
    pokeByteOff _p 0 stream
    pokeByteOff _p 32 (fromIntegral frameCount :: CUInt)
    pokeByteOff _p 36 (fromBool looping :: CInt)
    pokeByteOff _p 40 ctxType
    pokeByteOff _p 48 ctxData
    return ()

data VrDeviceInfo = VrDeviceInfo
  { vrDeviceInfo'hResolution :: Int,
    vrDeviceInfo'vResolution :: Int,
    vrDeviceInfo'hScreenSize :: Float,
    vrDeviceInfo'vScreenSize :: Float,
    vrDeviceInfo'vScreenCenter :: Float,
    vrDeviceInfo'eyeToScreenDistance :: Float,
    vrDeviceInfo'lensSeparationDistance :: Float,
    vrDeviceInfo'interpupillaryDistance :: Float,
    vrDeviceInfo'lensDistortionValues :: [Float],
    vrDeviceInfo'chromaAbCorrection :: [Float]
  }
  deriving (Eq, Show, Freeable)

instance Storable VrDeviceInfo where
  sizeOf _ = 64
  alignment _ = 4
  peek _p = do
    hResolution <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    vResolution <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    hScreenSize <- realToFrac <$> (peekByteOff _p 8 :: IO CFloat)
    vScreenSize <- realToFrac <$> (peekByteOff _p 12 :: IO CFloat)
    vScreenCenter <- realToFrac <$> (peekByteOff _p 16 :: IO CFloat)
    eyeToScreenDistance <- realToFrac <$> (peekByteOff _p 20 :: IO CFloat)
    lensSeparationDistance <- realToFrac <$> (peekByteOff _p 24 :: IO CFloat)
    interpupillaryDistance <- realToFrac <$> (peekByteOff _p 28 :: IO CFloat)
    lensDistortionValues <- map realToFrac <$> (peekStaticArrayOff 4 (castPtr _p) 32 :: IO [CFloat])
    chromaAbCorrection <- map realToFrac <$> (peekStaticArrayOff 4 (castPtr _p) 48 :: IO [CFloat])
    return $ VrDeviceInfo hResolution vResolution hScreenSize vScreenSize vScreenCenter eyeToScreenDistance lensSeparationDistance interpupillaryDistance lensDistortionValues chromaAbCorrection
  poke _p (VrDeviceInfo hResolution vResolution hScreenSize vScreenSize vScreenCenter eyeToScreenDistance lensSeparationDistance interpupillaryDistance lensDistortionValues chromaAbCorrection) = do
    pokeByteOff _p 0 (fromIntegral hResolution :: CInt)
    pokeByteOff _p 4 (fromIntegral vResolution :: CInt)
    pokeByteOff _p 8 (realToFrac hScreenSize :: CFloat)
    pokeByteOff _p 12 (realToFrac vScreenSize :: CFloat)
    pokeByteOff _p 16 (realToFrac vScreenCenter :: CFloat)
    pokeByteOff _p 20 (realToFrac eyeToScreenDistance :: CFloat)
    pokeByteOff _p 24 (realToFrac lensSeparationDistance :: CFloat)
    pokeByteOff _p 28 (realToFrac interpupillaryDistance :: CFloat)
    pokeStaticArrayOff (castPtr _p) 32 (map realToFrac lensDistortionValues :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 48 (map realToFrac chromaAbCorrection :: [CFloat])
    return ()

data VrStereoConfig = VrStereoConfig
  { vrStereoConfig'projection :: [Matrix],
    vrStereoConfig'viewOffset :: [Matrix],
    vrStereoConfig'leftLensCenter :: [Float],
    vrStereoConfig'rightLensCenter :: [Float],
    vrStereoConfig'leftScreenCenter :: [Float],
    vrStereoConfig'rightScreenCenter :: [Float],
    vrStereoConfig'scale :: [Float],
    vrStereoConfig'scaleIn :: [Float]
  }
  deriving (Eq, Show, Freeable)

instance Storable VrStereoConfig where
  sizeOf _ = 304
  alignment _ = 4
  peek _p = do
    projection <- peekStaticArrayOff 2 (castPtr _p) 0
    viewOffset <- peekStaticArrayOff 2 (castPtr _p) 128
    leftLensCenter <- map realToFrac <$> (peekStaticArrayOff 2 (castPtr _p) 256 :: IO [CFloat])
    rightLensCenter <- map realToFrac <$> (peekStaticArrayOff 2 (castPtr _p) 264 :: IO [CFloat])
    leftScreenCenter <- map realToFrac <$> (peekStaticArrayOff 2 (castPtr _p) 272 :: IO [CFloat])
    rightScreenCenter <- map realToFrac <$> (peekStaticArrayOff 2 (castPtr _p) 280 :: IO [CFloat])
    scale <- map realToFrac <$> (peekStaticArrayOff 2 (castPtr _p) 288 :: IO [CFloat])
    scaleIn <- map realToFrac <$> (peekStaticArrayOff 2 (castPtr _p) 296 :: IO [CFloat])
    return $ VrStereoConfig projection viewOffset leftLensCenter rightLensCenter leftScreenCenter rightScreenCenter scale scaleIn
  poke _p (VrStereoConfig projection viewOffset leftLensCenter rightLensCenter leftScreenCenter rightScreenCenter scale scaleIn) = do
    pokeStaticArrayOff (castPtr _p) 0 projection
    pokeStaticArrayOff (castPtr _p) 128 viewOffset
    pokeStaticArrayOff (castPtr _p) 256 (map realToFrac leftLensCenter :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 264 (map realToFrac rightLensCenter :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 272 (map realToFrac leftScreenCenter :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 280 (map realToFrac rightScreenCenter :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 288 (map realToFrac scale :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 296 (map realToFrac scaleIn :: [CFloat])
    return ()

data FilePathList = FilePathList
  { filePathlist'capacity :: Integer,
    filePathList'paths :: [String]
  }
  deriving (Eq, Show)

instance Storable FilePathList where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    capacity <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    count <- fromIntegral <$> (peekByteOff _p 4 :: IO CUInt)
    pathsPtr <- (peekByteOff _p 8 :: IO (Ptr CString))
    pathsCStrings <- peekArray count pathsPtr
    paths <- mapM peekCString pathsCStrings
    return $ FilePathList capacity paths
  poke _p (FilePathList capacity paths) = do
    pokeByteOff _p 0 (fromIntegral capacity :: CUInt)
    pokeByteOff _p 4 (fromIntegral (length paths) :: CUInt)
    pathsCStrings <- mapM newCString paths
    pokeByteOff _p 8 =<< newArray pathsCStrings
    return ()

instance Freeable FilePathList where
  rlFreeDependents val ptr = do
    pathsPtr <- (peekByteOff ptr 8 :: IO (Ptr CString))
    pathsCStrings <- peekArray (length $ filePathList'paths val) pathsPtr
    mapM_ (c'free . castPtr) pathsCStrings
    c'free $ castPtr pathsPtr

---- rlgl.h

-- | Dynamic vertex buffers (position + texcoords + colors + indices arrays)
data RLVertexBuffer = RLVertexBuffer
  { -- | Number of elements in the buffer (QUADS)
    rlVertexBuffer'elementCount :: Int,
    -- | Vertex position (shader-location = 0)
    rlVertexBuffer'vertices :: [Vector3],
    -- | Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
    rlVertexBuffer'texcoords :: [Vector2],
    -- | Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
    rlVertexBuffer'colors :: [Color],
    -- | Vertex indices (in case vertex data comes indexed) (6 indices per quad)
    rlVertexBuffer'indices :: [Integer],
    -- | OpenGL Vertex Array Object id
    rlVertexBuffer'vaoId :: Integer,
    -- | OpenGL Vertex Buffer Objects id (4 types of vertex data)
    rlVertexBuffer'vboId :: [Integer]
  }
  deriving (Eq, Show)

instance Storable RLVertexBuffer where
  sizeOf _ = 64
  alignment _ = 8
  peek _p = do
    elementCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    verticesPtr <- (peekByteOff _p 8 :: IO (Ptr Vector3))
    vertices <- peekArray elementCount verticesPtr
    texcoordsPtr <- (peekByteOff _p 16 :: IO (Ptr Vector2))
    texcoords <- peekArray elementCount texcoordsPtr
    colorsPtr <- (peekByteOff _p 24 :: IO (Ptr Color))
    colors <- peekArray elementCount colorsPtr
    indicesPtr <- (peekByteOff _p 32 :: IO (Ptr CUInt))
    indices <- map fromIntegral <$> peekArray elementCount indicesPtr
    vaoId <- fromIntegral <$> (peekByteOff _p 40 :: IO CUInt)
    vboId <- map fromIntegral <$> peekStaticArrayOff 4 (castPtr _p :: Ptr CUInt) 44
    return $ RLVertexBuffer elementCount vertices texcoords colors indices vaoId vboId
  poke _p (RLVertexBuffer elementCount vertices texcoords colors indices vaoId vboId) = do
    pokeByteOff _p 0 (fromIntegral elementCount :: CInt)
    pokeByteOff _p 8 =<< newArray vertices
    pokeByteOff _p 16 =<< newArray texcoords
    pokeByteOff _p 24 =<< newArray colors
    pokeByteOff _p 32 =<< newArray (map fromIntegral indices :: [CUInt])
    pokeByteOff _p 40 (fromIntegral vaoId :: CUInt)
    pokeStaticArrayOff (castPtr _p) 44 (map fromIntegral vboId :: [CUInt])
    return ()

instance Freeable RLVertexBuffer where
  rlFreeDependents _ ptr = do
    verticesPtr <- (peekByteOff ptr 8 :: IO (Ptr Vector3))
    c'free $ castPtr verticesPtr
    texcoordsPtr <- (peekByteOff ptr 16 :: IO (Ptr Vector2))
    c'free $ castPtr texcoordsPtr
    colorsPtr <- (peekByteOff ptr 24 :: IO (Ptr Color))
    c'free $ castPtr colorsPtr
    indicesPtr <- (peekByteOff ptr 32 :: IO (Ptr CUInt))
    c'free $ castPtr indicesPtr

-- | Draw call type.
-- NOTE: Only texture changes register a new draw, other state-change-related elements are not
-- used at this moment (vaoId, shaderId, matrices), raylib just forces a batch draw call if any
-- of those state changes happen (this is done in the core module).
data RLDrawCall = RLDrawCall
  { -- | Drawing mode: LINES, TRIANGLES, QUADS
    rlDrawCall'mode :: RLDrawMode,
    -- | Number of vertices of the draw
    rlDrawCall'vertexCount :: Int,
    -- | Number of vertices required for index alignment (LINES, TRIANGLES)
    rlDrawCall'vertexAlignment :: Int,
    -- | Texture id to be used on the draw -> Used to create new draw call if changed
    rlDrawCall'textureId :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable RLDrawCall where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    mode <- peekByteOff _p 0
    vertexCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    vertexAlignment <- fromIntegral <$> (peekByteOff _p 8 :: IO CInt)
    textureId <- fromIntegral <$> (peekByteOff _p 12 :: IO CUInt)
    return $ RLDrawCall mode vertexCount vertexAlignment textureId
  poke _p (RLDrawCall mode vertexCount vertexAlignment textureId) = do
    pokeByteOff _p 0 mode
    pokeByteOff _p 4 (fromIntegral vertexCount :: CInt)
    pokeByteOff _p 8 (fromIntegral vertexAlignment :: CInt)
    pokeByteOff _p 12 (fromIntegral textureId :: CUInt)
    return ()

-- rlRenderBatch type
data RLRenderBatch = RLRenderBatch
  { -- | Number of vertex buffers (multi-buffering support)
    rlRenderBatch'bufferCount :: Int,
    -- | Current buffer tracking in case of multi-buffering
    rlRenderBatch'currentBuffer :: Int,
    -- | Dynamic buffer(s) for vertex data
    rlRenderBatch'vertexBuffers :: [RLVertexBuffer],
    -- | Draw calls array, depends on textureId
    rlRenderBatch'draws :: [RLDrawCall],
    -- | Draw calls counter
    rlRenderBatch'drawCounter :: Int,
    -- | Current depth value for next draw
    rlRenderBatch'currentDepth :: Float
  }
  deriving (Eq, Show)

instance Storable RLRenderBatch where
  sizeOf _ = 32
  alignment _ = 8
  peek _p = do
    bufferCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    currentBuffer <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    vertexBuffersPtr <- (peekByteOff _p 8 :: IO (Ptr RLVertexBuffer))
    vertexBuffers <- peekArray bufferCount vertexBuffersPtr
    drawsPtr <- (peekByteOff _p 16 :: IO (Ptr RLDrawCall))
    draws <- peekArray 256 drawsPtr
    drawCounter <- fromIntegral <$> (peekByteOff _p 24 :: IO CInt)
    currentDepth <- realToFrac <$> (peekByteOff _p 28 :: IO CFloat)
    return $ RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth
  poke _p (RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) = do
    pokeByteOff _p 0 (fromIntegral bufferCount :: CInt)
    pokeByteOff _p 4 (fromIntegral currentBuffer :: CInt)
    pokeByteOff _p 8 =<< newArray vertexBuffers
    pokeByteOff _p 16 =<< newArray draws
    pokeByteOff _p 24 (fromIntegral drawCounter :: CInt)
    pokeByteOff _p 28 (realToFrac currentDepth :: CFloat)
    return ()

instance Freeable RLRenderBatch where
  rlFreeDependents val ptr = do
    vertexBuffersPtr <- (peekByteOff ptr 8 :: IO (Ptr RLVertexBuffer))
    rlFreeArray (rlRenderBatch'vertexBuffers val) vertexBuffersPtr
    drawsPtr <- (peekByteOff ptr 16 :: IO (Ptr RLDrawCall))
    c'free $ castPtr drawsPtr

------------------------------------------------
-- Raylib callbacks ----------------------------
------------------------------------------------

type LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

type SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

type LoadFileTextCallback = FunPtr (CString -> IO CString)

type SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())
