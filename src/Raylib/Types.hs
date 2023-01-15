{-# OPTIONS -Wall #-}
module Raylib.Types where

-- This file includes Haskell counterparts to the structs defined in raylib

import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    Word8,
    castPtr,
    newArray,
    peekArray,
    plusPtr,
    pokeArray,
  )
import Foreign.C
  ( CBool,
    CChar,
    CFloat,
    CInt (..),
    CString,
    CUChar,
    CUInt,
    CUShort,
  )
import GHC.IO (unsafePerformIO)

-- Necessary functions
foreign import ccall safe "raylib.h GetPixelDataSize"
  c'getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

getPixelDataSize :: Int -> Int -> PixelFormat -> Int
getPixelDataSize width height format = unsafePerformIO (fromIntegral <$> c'getPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format))

foreign import ccall safe "raylib.h &GetPixelDataSize"
  p'getPixelDataSize ::
    FunPtr (CInt -> CInt -> CInt -> IO CInt)

------------------------------------------------
-- Raylib enumerations -------------------------
------------------------------------------------

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
  | Msaa4xHint
  | InterlacedHint
  deriving (Eq, Show)

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
    32 -> Msaa4xHint
    65536 -> InterlacedHint
    n -> error $ "Invalid value " ++ show n ++ " for `toEnum`."

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
    x -> error $ "Invalid value " ++ show x ++ " for `fromEnum`."

data MouseButton
  = MouseButtonLeft
  | MouseButtonRight
  | MouseButtonMiddle
  | MouseButtonSide
  | MouseButtonExtra
  | MouseButtonForward
  | MouseButtonBack
  deriving (Eq, Show, Enum)

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
  deriving (Eq, Show, Enum)

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
  deriving (Eq, Show, Enum)

data GamepadAxis
  = GamepadAxisLeftX
  | GamepadAxisLeftY
  | GamepadAxisRightX
  | GamepadAxisRightY
  | GamepadAxisLeftTrigger
  | GamepadAxisRightTrigger
  deriving (Eq, Show, Enum)

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
  = ShaderUniformFloat
  | ShaderUniformVec2
  | ShaderUniformVec3
  | ShaderUniformVec4
  | ShaderUniformInt
  | ShaderUniformIvec2
  | ShaderUniformIvec3
  | ShaderUniformIvec4
  | ShaderUniformSampler2d
  deriving (Eq, Show, Enum)

-- I genuinely have no idea where this is used.
data ShaderAttributeDataType
  = ShaderAttribFloat
  | ShaderAttribVec2
  | ShaderAttribVec3
  | ShaderAttribVec4
  deriving (Eq, Show, Enum)

data PixelFormat
  = PixelFormatUncompressedGrayscale
  | PixelFormatUncompressedGrayAlpha
  | PixelFormatUncompressedR5G6B5
  | PixelFormatUncompressedR8G8B8
  | PixelFormatUncompressedR5G5B5A1
  | PixelFormatUncompressedR4G4B4A4
  | PixelFormatUncompressedR8G8B8A8
  | PixelFormatUncompressedR32
  | PixelFormatUncompressedR32G32B32
  | PixelFormatUncompressedR32G32B32A32
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
    PixelFormatCompressedDxt1Rgb -> 11
    PixelFormatCompressedDxt1Rgba -> 12
    PixelFormatCompressedDxt3Rgba -> 13
    PixelFormatCompressedDxt5Rgba -> 14
    PixelFormatCompressedEtc1Rgb -> 15
    PixelFormatCompressedEtc2Rgb -> 16
    PixelFormatCompressedEtc2EacRgba -> 17
    PixelFormatCompressedPvrtRgb -> 18
    PixelFormatCompressedPvrtRgba -> 19
    PixelFormatCompressedAstc4x4Rgba -> 20
    PixelFormatCompressedAstc8x8Rgba -> 21

  toEnum n = case n of
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
    11 -> PixelFormatCompressedDxt1Rgb
    12 -> PixelFormatCompressedDxt1Rgba
    13 -> PixelFormatCompressedDxt3Rgba
    14 -> PixelFormatCompressedDxt5Rgba
    15 -> PixelFormatCompressedEtc1Rgb
    16 -> PixelFormatCompressedEtc2Rgb
    17 -> PixelFormatCompressedEtc2EacRgba
    18 -> PixelFormatCompressedPvrtRgb
    19 -> PixelFormatCompressedPvrtRgba
    20 -> PixelFormatCompressedAstc4x4Rgba
    21 -> PixelFormatCompressedAstc8x8Rgba
    _ -> error "Invalid input"

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

data BlendMode = BlendAlpha | BlendAdditive | BlendMultiplied | BlendAddColors | BlendSubtractColors | BlendAlphaPremultiply | BlendCustom | BlendCustomSeparate deriving (Enum)

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
  deriving (Show)

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
    _ -> error "Invalid input"

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
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (toEnum $ fromEnum v :: CInt)

data NPatchLayout = NPatchNinePatch | NPatchThreePatchVertical | NPatchThreePatchHorizontal deriving (Eq, Show, Enum)

instance Storable NPatchLayout where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (toEnum $ fromEnum v :: CInt)

------------------------------------------------
-- Raylib structures ---------------------------
------------------------------------------------

data Vector2 = Vector2
  { vector2'x :: Float,
    vector2'y :: Float
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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

data Color = Color
  { color'r :: Word8,
    color'g :: Word8,
    color'b :: Word8,
    color'a :: Word8
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
    let size = getPixelDataSize width height format
    arr <- peekArray size ptr
    return $ Image (map fromIntegral arr) width height mipmaps format
  poke _p (Image arr width height mipmaps format) = do
    ptr <- newArray (map fromIntegral arr :: [CUChar])
    pokeByteOff _p 0 ptr
    pokeByteOff _p 8 (fromIntegral width :: CInt)
    pokeByteOff _p 12 (fromIntegral height :: CInt)
    pokeByteOff _p 16 (fromIntegral mipmaps :: CInt)
    pokeByteOff _p 20 format
    return ()

{- typedef struct Texture {
            unsigned int id; int width; int height; int mipmaps; int format;
        } Texture; -}
data Texture = Texture
  { texture'id :: Integer,
    texture'width :: Int,
    texture'height :: Int,
    texture'mipmaps :: Int,
    texture'format :: PixelFormat
  }
  deriving (Eq, Show)

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

{- typedef struct RenderTexture {
            unsigned int id; Texture texture; Texture depth;
        } RenderTexture; -}
data RenderTexture = RenderTexture
  { rendertexture'id :: Integer,
    rendertexture'texture :: Texture,
    rendertexture'depth :: Texture
  }
  deriving (Eq, Show)

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

{- typedef RenderTexture RenderTexture2D; -}
type RenderTexture2D = RenderTexture

{- typedef struct NPatchInfo {
            Rectangle source;
            int left;
            int top;
            int right;
            int bottom;
            int layout;
        } NPatchInfo; -}
data NPatchInfo = NPatchInfo
  { nPatchinfo'source :: Rectangle,
    nPatchinfo'left :: CInt,
    nPatchinfo'top :: CInt,
    nPatchinfo'right :: CInt,
    nPatchinfo'bottom :: CInt,
    nPatchinfo'layout :: NPatchLayout
  }
  deriving (Eq, Show)

p'NPatchInfo'source p = plusPtr p 0

p'NPatchInfo'source :: Ptr NPatchInfo -> Ptr Rectangle

p'NPatchInfo'left p = plusPtr p 16

p'NPatchInfo'left :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'top p = plusPtr p 20

p'NPatchInfo'top :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'right p = plusPtr p 24

p'NPatchInfo'right :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'bottom p = plusPtr p 28

p'NPatchInfo'bottom :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'layout p = plusPtr p 32

p'NPatchInfo'layout :: Ptr NPatchInfo -> Ptr CInt

instance Storable NPatchInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 20
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 28
    v5 <- peekByteOff _p 32
    return $ NPatchInfo v0 v1 v2 v3 v4 v5
  poke _p (NPatchInfo v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 20 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 28 v4
    pokeByteOff _p 32 v5
    return ()

{- typedef struct GlyphInfo {
            int value; int offsetX; int offsetY; int advanceX; Image image;
        } GlyphInfo; -}
data GlyphInfo = GlyphInfo
  { glyphinfo'value :: CInt,
    glyphInfo'offsetX :: CInt,
    glyphInfo'offsetY :: CInt,
    glyphInfo'advanceX :: CInt,
    glyphinfo'image :: Image
  }
  deriving (Eq, Show)

p'GlyphInfo'value p = plusPtr p 0

p'GlyphInfo'value :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'offsetX p = plusPtr p 4

p'GlyphInfo'offsetX :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'offsetY p = plusPtr p 8

p'GlyphInfo'offsetY :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'advanceX p = plusPtr p 12

p'GlyphInfo'advanceX :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'image p = plusPtr p 16

p'GlyphInfo'image :: Ptr GlyphInfo -> Ptr Image

instance Storable GlyphInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ GlyphInfo v0 v1 v2 v3 v4
  poke _p (GlyphInfo v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct Font {
            int baseSize;
            int glyphCount;
            int glyphPadding;
            Texture2D texture;
            Rectangle * recs;
            GlyphInfo * glyphs;
        } Font; -}
data Font = Font
  { font'baseSize :: CInt,
    font'glyphCount :: CInt,
    font'glyphPadding :: CInt,
    font'texture :: Texture,
    font'recs :: Ptr Rectangle,
    font'glyphs :: Ptr GlyphInfo
  }
  deriving (Eq, Show)

p'Font'baseSize p = plusPtr p 0

p'Font'baseSize :: Ptr Font -> Ptr CInt

p'Font'glyphCount p = plusPtr p 4

p'Font'glyphCount :: Ptr Font -> Ptr CInt

p'Font'glyphPadding p = plusPtr p 8

p'Font'glyphPadding :: Ptr Font -> Ptr CInt

p'Font'texture p = plusPtr p 12

p'Font'texture :: Ptr Font -> Ptr Texture

p'Font'recs p = plusPtr p 32

p'Font'recs :: Ptr Font -> Ptr (Ptr Rectangle)

p'Font'glyphs p = plusPtr p 40

p'Font'glyphs :: Ptr Font -> Ptr (Ptr GlyphInfo)

instance Storable Font where
  sizeOf _ = 48
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 32
    v5 <- peekByteOff _p 40
    return $ Font v0 v1 v2 v3 v4 v5
  poke _p (Font v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 32 v4
    pokeByteOff _p 40 v5
    return ()

{- typedef struct Camera3D {
            Vector3 position;
            Vector3 target;
            Vector3 up;
            float fovy;
            int projection;
        } Camera3D; -}
data Camera3D = Camera3D
  { camera3D'position :: Vector3,
    camera3D'target :: Vector3,
    camera3D'up :: Vector3,
    camera3D'fovy :: CFloat,
    camera3D'projection :: CameraProjection
  }
  deriving (Eq, Show)

p'Camera3D'position p = plusPtr p 0

p'Camera3D'position :: Ptr Camera3D -> Ptr Vector3

p'Camera3D'target p = plusPtr p 12

p'Camera3D'target :: Ptr Camera3D -> Ptr Vector3

p'Camera3D'up p = plusPtr p 24

p'Camera3D'up :: Ptr Camera3D -> Ptr Vector3

p'Camera3D'fovy p = plusPtr p 36

p'Camera3D'fovy :: Ptr Camera3D -> Ptr CFloat

p'Camera3D'projection p = plusPtr p 40

p'Camera3D'projection :: Ptr Camera3D -> Ptr CInt

instance Storable Camera3D where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 36
    v4 <- peekByteOff _p 40
    return $ Camera3D v0 v1 v2 v3 v4
  poke _p (Camera3D v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 36 v3
    pokeByteOff _p 40 v4
    return ()

{- typedef Camera3D Camera; -}
type Camera = Camera3D

{- typedef struct Camera2D {
            Vector2 offset; Vector2 target; float rotation; float zoom;
        } Camera2D; -}
data Camera2D = Camera2D
  { camera2D'offset :: Vector2,
    camera2D'target :: Vector2,
    camera2d'rotation :: CFloat,
    camera2d'zoom :: CFloat
  }
  deriving (Eq, Show)

p'Camera2D'offset p = plusPtr p 0

p'Camera2D'offset :: Ptr Camera2D -> Ptr Vector2

p'Camera2D'target p = plusPtr p 8

p'Camera2D'target :: Ptr Camera2D -> Ptr Vector2

p'Camera2D'rotation p = plusPtr p 16

p'Camera2D'rotation :: Ptr Camera2D -> Ptr CFloat

p'Camera2D'zoom p = plusPtr p 20

p'Camera2D'zoom :: Ptr Camera2D -> Ptr CFloat

instance Storable Camera2D where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 20
    return $ Camera2D v0 v1 v2 v3
  poke _p (Camera2D v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 20 v3
    return ()

{- typedef struct Mesh {
            int vertexCount;
            int triangleCount;
            float * vertices;
            float * texcoords;
            float * texcoords2;
            float * normals;
            float * tangents;
            unsigned char * colors;
            unsigned short * indices;
            float * animVertices;
            float * animNormals;
            unsigned char * boneIds;
            float * boneWeights;
            unsigned int vaoId;
            unsigned int * vboId;
        } Mesh; -}
data Mesh = Mesh
  { mesh'vertexCount :: CInt,
    mesh'triangleCount :: CInt,
    mesh'vertices :: Ptr CFloat,
    mesh'texcoords :: Ptr CFloat,
    mesh'texcoords2 :: Ptr CFloat,
    mesh'normals :: Ptr CFloat,
    mesh'tangents :: Ptr CFloat,
    mesh'colors :: Ptr CUChar,
    mesh'indices :: Ptr CUShort,
    mesh'animVertices :: Ptr CFloat,
    mesh'animNormals :: Ptr CFloat,
    mesh'boneIds :: Ptr CUChar,
    mesh'boneWeights :: Ptr CFloat,
    mesh'vaoId :: CUInt,
    mesh'vboId :: Ptr CUInt
  }
  deriving (Eq, Show)

p'Mesh'vertexCount p = plusPtr p 0

p'Mesh'vertexCount :: Ptr Mesh -> Ptr CInt

p'Mesh'triangleCount p = plusPtr p 4

p'Mesh'triangleCount :: Ptr Mesh -> Ptr CInt

p'Mesh'vertices p = plusPtr p 8

p'Mesh'vertices :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'texcoords p = plusPtr p 12

p'Mesh'texcoords :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'texcoords2 p = plusPtr p 16

p'Mesh'texcoords2 :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'normals p = plusPtr p 20

p'Mesh'normals :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'tangents p = plusPtr p 24

p'Mesh'tangents :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'colors p = plusPtr p 28

p'Mesh'colors :: Ptr Mesh -> Ptr (Ptr CUChar)

p'Mesh'indices p = plusPtr p 32

p'Mesh'indices :: Ptr Mesh -> Ptr (Ptr CUShort)

p'Mesh'animVertices p = plusPtr p 36

p'Mesh'animVertices :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'animNormals p = plusPtr p 40

p'Mesh'animNormals :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'boneIds p = plusPtr p 44

p'Mesh'boneIds :: Ptr Mesh -> Ptr (Ptr CUChar)

p'Mesh'boneWeights p = plusPtr p 48

p'Mesh'boneWeights :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'vaoId p = plusPtr p 52

p'Mesh'vaoId :: Ptr Mesh -> Ptr CUInt

p'Mesh'vboId p = plusPtr p 56

p'Mesh'vboId :: Ptr Mesh -> Ptr (Ptr CUInt)

instance Storable Mesh where
  sizeOf _ = 60
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    v5 <- peekByteOff _p 20
    v6 <- peekByteOff _p 24
    v7 <- peekByteOff _p 28
    v8 <- peekByteOff _p 32
    v9 <- peekByteOff _p 36
    v10 <- peekByteOff _p 40
    v11 <- peekByteOff _p 44
    v12 <- peekByteOff _p 48
    v13 <- peekByteOff _p 52
    v14 <- peekByteOff _p 56
    return $ Mesh v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  poke _p (Mesh v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    pokeByteOff _p 20 v5
    pokeByteOff _p 24 v6
    pokeByteOff _p 28 v7
    pokeByteOff _p 32 v8
    pokeByteOff _p 36 v9
    pokeByteOff _p 40 v10
    pokeByteOff _p 44 v11
    pokeByteOff _p 48 v12
    pokeByteOff _p 52 v13
    pokeByteOff _p 56 v14
    return ()

{- typedef struct Shader {
            unsigned int id; int * locs;
        } Shader; -}
data Shader = Shader
  { shader'id :: CUInt,
    shader'locs :: Ptr CInt
  }
  deriving (Eq, Show)

p'Shader'id p = plusPtr p 0

p'Shader'id :: Ptr Shader -> Ptr CUInt

p'Shader'locs p = plusPtr p 4

p'Shader'locs :: Ptr Shader -> Ptr (Ptr CInt)

instance Storable Shader where
  sizeOf _ = 8
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    return $ Shader v0 v1
  poke _p (Shader v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    return ()

{- typedef struct MaterialMap {
            Texture2D texture; Color color; float value;
        } MaterialMap; -}
data MaterialMap = MaterialMap
  { materialmap'texture :: Texture,
    materialmap'color :: Color,
    materialmap'value :: CFloat
  }
  deriving (Eq, Show)

p'MaterialMap'texture p = plusPtr p 0

p'MaterialMap'texture :: Ptr MaterialMap -> Ptr Texture

p'MaterialMap'color p = plusPtr p 20

p'MaterialMap'color :: Ptr MaterialMap -> Ptr Color

p'MaterialMap'value p = plusPtr p 24

p'MaterialMap'value :: Ptr MaterialMap -> Ptr CFloat

instance Storable MaterialMap where
  sizeOf _ = 28
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 20
    v2 <- peekByteOff _p 24
    return $ MaterialMap v0 v1 v2
  poke _p (MaterialMap v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 20 v1
    pokeByteOff _p 24 v2
    return ()

{- typedef struct Material {
            Shader shader; MaterialMap * maps; float params[4];
        } Material; -}
data Material = Material
  { material'shader :: Shader,
    material'maps :: Ptr MaterialMap,
    material'params :: [CFloat]
  }
  deriving (Eq, Show)

p'Material'shader p = plusPtr p 0

p'Material'shader :: Ptr Material -> Ptr Shader

p'Material'maps p = plusPtr p 8

p'Material'maps :: Ptr Material -> Ptr (Ptr MaterialMap)

p'Material'params p = plusPtr p 12

p'Material'params :: Ptr Material -> Ptr CFloat

instance Storable Material where
  sizeOf _ = 28
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- let s2 = div 16 $ sizeOf (undefined :: CFloat) in peekArray s2 (plusPtr _p 12)
    return $ Material v0 v1 v2
  poke _p (Material v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    let s2 = div 16 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 12) (take s2 v2)
    return ()

{- typedef struct Transform {
            Vector3 translation; Quaternion rotation; Vector3 scale;
        } Transform; -}
data Transform = Transform
  { transform'translation :: Vector3,
    transform'rotation :: Vector4,
    transform'scale :: Vector3
  }
  deriving (Eq, Show)

p'Transform'translation p = plusPtr p 0

p'Transform'translation :: Ptr Transform -> Ptr Vector3

p'Transform'rotation p = plusPtr p 12

p'Transform'rotation :: Ptr Transform -> Ptr Vector4

p'Transform'scale p = plusPtr p 28

p'Transform'scale :: Ptr Transform -> Ptr Vector3

instance Storable Transform where
  sizeOf _ = 40
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    v2 <- peekByteOff _p 28
    return $ Transform v0 v1 v2
  poke _p (Transform v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    pokeByteOff _p 28 v2
    return ()

{- typedef struct BoneInfo {
            char name[32]; int parent;
        } BoneInfo; -}
data BoneInfo = BoneInfo
  { boneInfo'name :: [CChar],
    boneinfo'parent :: CInt
  }
  deriving (Eq, Show)

p'BoneInfo'name p = plusPtr p 0

p'BoneInfo'name :: Ptr BoneInfo -> Ptr CChar

p'BoneInfo'parent p = plusPtr p 32

p'BoneInfo'parent :: Ptr BoneInfo -> Ptr CInt

instance Storable BoneInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- let s0 = div 32 $ sizeOf (undefined :: CChar) in peekArray s0 (plusPtr _p 0)
    v1 <- peekByteOff _p 32
    return $ BoneInfo v0 v1
  poke _p (BoneInfo v0 v1) = do
    let s0 = div 32 $ sizeOf (undefined :: CChar)
    pokeArray (plusPtr _p 0) (take s0 v0)
    pokeByteOff _p 32 v1
    return ()

{- typedef struct Model {
            Matrix transform;
            int meshCount;
            int materialCount;
            Mesh * meshes;
            Material * materials;
            int * meshMaterial;
            int boneCount;
            BoneInfo * bones;
            Transform * bindPose;
        } Model; -}
data Model = Model
  { model'transform :: Matrix,
    model'meshCount :: CInt,
    model'materialCount :: CInt,
    model'meshes :: Ptr Mesh,
    model'materials :: Ptr Material,
    model'meshMaterial :: Ptr CInt,
    model'boneCount :: CInt,
    model'bones :: Ptr BoneInfo,
    model'bindPose :: Ptr Transform
  }
  deriving (Eq, Show)

p'Model'transform p = plusPtr p 0

p'Model'transform :: Ptr Model -> Ptr Matrix

p'Model'meshCount p = plusPtr p 64

p'Model'meshCount :: Ptr Model -> Ptr CInt

p'Model'materialCount p = plusPtr p 68

p'Model'materialCount :: Ptr Model -> Ptr CInt

p'Model'meshes p = plusPtr p 72

p'Model'meshes :: Ptr Model -> Ptr (Ptr Mesh)

p'Model'materials p = plusPtr p 76

p'Model'materials :: Ptr Model -> Ptr (Ptr Material)

p'Model'meshMaterial p = plusPtr p 80

p'Model'meshMaterial :: Ptr Model -> Ptr (Ptr CInt)

p'Model'boneCount p = plusPtr p 84

p'Model'boneCount :: Ptr Model -> Ptr CInt

p'Model'bones p = plusPtr p 88

p'Model'bones :: Ptr Model -> Ptr (Ptr BoneInfo)

p'Model'bindPose p = plusPtr p 92

p'Model'bindPose :: Ptr Model -> Ptr (Ptr Transform)

instance Storable Model where
  sizeOf _ = 96
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 64
    v2 <- peekByteOff _p 68
    v3 <- peekByteOff _p 72
    v4 <- peekByteOff _p 76
    v5 <- peekByteOff _p 80
    v6 <- peekByteOff _p 84
    v7 <- peekByteOff _p 88
    v8 <- peekByteOff _p 92
    return $ Model v0 v1 v2 v3 v4 v5 v6 v7 v8
  poke _p (Model v0 v1 v2 v3 v4 v5 v6 v7 v8) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 64 v1
    pokeByteOff _p 68 v2
    pokeByteOff _p 72 v3
    pokeByteOff _p 76 v4
    pokeByteOff _p 80 v5
    pokeByteOff _p 84 v6
    pokeByteOff _p 88 v7
    pokeByteOff _p 92 v8
    return ()

{- typedef struct ModelAnimation {
            int boneCount;
            int frameCount;
            BoneInfo * bones;
            Transform * * framePoses;
        } ModelAnimation; -}
data ModelAnimation = ModelAnimation
  { modelAnimation'boneCount :: CInt,
    modelAnimation'frameCount :: CInt,
    modelAnimation'bones :: Ptr BoneInfo,
    modelAnimation'framePoses :: Ptr (Ptr Transform)
  }
  deriving (Eq, Show)

p'ModelAnimation'boneCount p = plusPtr p 0

p'ModelAnimation'boneCount :: Ptr ModelAnimation -> Ptr CInt

p'ModelAnimation'frameCount p = plusPtr p 4

p'ModelAnimation'frameCount :: Ptr ModelAnimation -> Ptr CInt

p'ModelAnimation'bones p = plusPtr p 8

p'ModelAnimation'bones :: Ptr ModelAnimation -> Ptr (Ptr BoneInfo)

p'ModelAnimation'framePoses p = plusPtr p 12

p'ModelAnimation'framePoses :: Ptr ModelAnimation -> Ptr (Ptr (Ptr Transform))

instance Storable ModelAnimation where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    return $ ModelAnimation v0 v1 v2 v3
  poke _p (ModelAnimation v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    return ()

{- typedef struct Ray {
            Vector3 position; Vector3 direction;
        } Ray; -}
data Ray = Ray
  { ray'position :: Vector3,
    ray'direction :: Vector3
  }
  deriving (Eq, Show)

p'Ray'position p = plusPtr p 0

p'Ray'position :: Ptr Ray -> Ptr Vector3

p'Ray'direction p = plusPtr p 12

p'Ray'direction :: Ptr Ray -> Ptr Vector3

instance Storable Ray where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    return $ Ray v0 v1
  poke _p (Ray v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    return ()

{- typedef struct RayCollision {
            _Bool hit; float distance; Vector3 point; Vector3 normal;
        } RayCollision; -}
data RayCollision = RayCollision
  { rayCollision'hit :: CBool,
    rayCollision'distance :: CFloat,
    rayCollision'point :: Vector3,
    rayCollision'normal :: Vector3
  }
  deriving (Eq, Show)

p'RayCollision'hit p = plusPtr p 0

p'RayCollision'hit :: Ptr RayCollision -> Ptr CBool

p'RayCollision'distance p = plusPtr p 4

p'RayCollision'distance :: Ptr RayCollision -> Ptr CFloat

p'RayCollision'point p = plusPtr p 8

p'RayCollision'point :: Ptr RayCollision -> Ptr Vector3

p'RayCollision'normal p = plusPtr p 20

p'RayCollision'normal :: Ptr RayCollision -> Ptr Vector3

instance Storable RayCollision where
  sizeOf _ = 32
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 20
    return $ RayCollision v0 v1 v2 v3
  poke _p (RayCollision v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 20 v3
    return ()

{- typedef struct BoundingBox {
            Vector3 min; Vector3 max;
        } BoundingBox; -}
data BoundingBox = BoundingBox
  { boundingBox'min :: Vector3,
    boundingBox'max :: Vector3
  }
  deriving (Eq, Show)

p'BoundingBox'min p = plusPtr p 0

p'BoundingBox'min :: Ptr BoundingBox -> Ptr Vector3

p'BoundingBox'max p = plusPtr p 12

p'BoundingBox'max :: Ptr BoundingBox -> Ptr Vector3

instance Storable BoundingBox where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    return $ BoundingBox v0 v1
  poke _p (BoundingBox v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    return ()

{- typedef struct Wave {
            unsigned int frameCount;
            unsigned int sampleRate;
            unsigned int sampleSize;
            unsigned int channels;
            void * data;
        } Wave; -}
data Wave = Wave
  { wave'frameCount :: CUInt,
    wave'sampleRate :: CUInt,
    wave'sampleSize :: CUInt,
    wave'channels :: CUInt,
    wave'data :: Ptr ()
  }
  deriving (Eq, Show)

p'Wave'frameCount p = plusPtr p 0

p'Wave'frameCount :: Ptr Wave -> Ptr CUInt

p'Wave'sampleRate p = plusPtr p 4

p'Wave'sampleRate :: Ptr Wave -> Ptr CUInt

p'Wave'sampleSize p = plusPtr p 8

p'Wave'sampleSize :: Ptr Wave -> Ptr CUInt

p'Wave'channels p = plusPtr p 12

p'Wave'channels :: Ptr Wave -> Ptr CUInt

p'Wave'data p = plusPtr p 16

p'Wave'data :: Ptr Wave -> Ptr (Ptr ())

instance Storable Wave where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ Wave v0 v1 v2 v3 v4
  poke _p (Wave v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct rAudioBuffer rAudioBuffer; -}
data RAudioBuffer = RAudioBuffer

{- typedef struct rAudioProcessor rAudioProcessor; -}
data RAudioProcessor = RAudioProcessor

{- typedef struct AudioStream {
            rAudioBuffer * buffer;
            rAudioProcessor * processor;
            unsigned int sampleRate;
            unsigned int sampleSize;
            unsigned int channels;
        } AudioStream; -}
data AudioStream = AudioStream
  { audioStream'buffer :: Ptr RAudioBuffer,
    audioStream'processor :: Ptr RAudioProcessor,
    audioStream'sampleRate :: CUInt,
    audioStream'sampleSize :: CUInt,
    audiostream'channels :: CUInt
  }
  deriving (Eq, Show)

p'AudioStream'buffer p = plusPtr p 0

p'AudioStream'buffer :: Ptr AudioStream -> Ptr (Ptr RAudioBuffer)

p'AudioStream'processor p = plusPtr p 4

p'AudioStream'processor :: Ptr AudioStream -> Ptr (Ptr rAudioProcessor)

p'AudioStream'sampleRate p = plusPtr p 8

p'AudioStream'sampleRate :: Ptr AudioStream -> Ptr CUInt

p'AudioStream'sampleSize p = plusPtr p 12

p'AudioStream'sampleSize :: Ptr AudioStream -> Ptr CUInt

p'AudioStream'channels p = plusPtr p 16

p'AudioStream'channels :: Ptr AudioStream -> Ptr CUInt

instance Storable AudioStream where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ AudioStream v0 v1 v2 v3 v4
  poke _p (AudioStream v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct Sound {
            AudioStream stream; unsigned int frameCount;
        } Sound; -}
data Sound = Sound
  { sound'stream :: AudioStream,
    sound'frameCount :: CUInt
  }
  deriving (Eq, Show)

p'Sound'stream p = plusPtr p 0

p'Sound'stream :: Ptr Sound -> Ptr AudioStream

p'Sound'frameCount p = plusPtr p 20

p'Sound'frameCount :: Ptr Sound -> Ptr CUInt

instance Storable Sound where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 20
    return $ Sound v0 v1
  poke _p (Sound v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 20 v1
    return ()

{- typedef struct Music {
            AudioStream stream;
            unsigned int frameCount;
            _Bool looping;
            int ctxType;
            void * ctxData;
        } Music; -}
data Music = Music
  { musistream :: AudioStream,
    musiframeCount :: CUInt,
    musilooping :: CInt,
    musictxType :: CInt,
    musictxData :: Ptr ()
  }
  deriving (Eq, Show)

p'Musistream p = plusPtr p 0

p'Musistream :: Ptr Music -> Ptr AudioStream

p'MusiframeCount p = plusPtr p 20

p'MusiframeCount :: Ptr Music -> Ptr CUInt

p'Musilooping p = plusPtr p 24

p'Musilooping :: Ptr Music -> Ptr CInt

p'MusictxType p = plusPtr p 28

p'MusictxType :: Ptr Music -> Ptr CInt

p'MusictxData p = plusPtr p 32

p'MusictxData :: Ptr Music -> Ptr (Ptr ())

instance Storable Music where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 20
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 28
    v4 <- peekByteOff _p 32
    return $ Music v0 v1 v2 v3 v4
  poke _p (Music v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 20 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 28 v3
    pokeByteOff _p 32 v4
    return ()

{- typedef struct VrDeviceInfo {
            int hResolution;
            int vResolution;
            float hScreenSize;
            float vScreenSize;
            float vScreenCenter;
            float eyeToScreenDistance;
            float lensSeparationDistance;
            float interpupillaryDistance;
            float lensDistortionValues[4];
            float chromaAbCorrection[4];
        } VrDeviceInfo; -}
data VrDeviceInfo = VrDeviceInfo
  { vrDeviceInfo'hResolution :: CInt,
    vrDeviceInfo'vResolution :: CInt,
    vrDeviceInfo'hScreenSize :: CFloat,
    vrDeviceInfo'vScreenSize :: CFloat,
    vrDeviceInfo'vScreenCenter :: CFloat,
    vrDeviceInfo'eyeToScreenDistance :: CFloat,
    vrDeviceInfo'lensSeparationDistance :: CFloat,
    vrDeviceInfo'interpupillaryDistance :: CFloat,
    vrDeviceInfo'lensDistortionValues :: [CFloat],
    vrDeviceInfo'chromaAbCorrection :: [CFloat]
  }
  deriving (Eq, Show)

p'VrDeviceInfo'hResolution p = plusPtr p 0

p'VrDeviceInfo'hResolution :: Ptr VrDeviceInfo -> Ptr CInt

p'VrDeviceInfo'vResolution p = plusPtr p 4

p'VrDeviceInfo'vResolution :: Ptr VrDeviceInfo -> Ptr CInt

p'VrDeviceInfo'hScreenSize p = plusPtr p 8

p'VrDeviceInfo'hScreenSize :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'vScreenSize p = plusPtr p 12

p'VrDeviceInfo'vScreenSize :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'vScreenCenter p = plusPtr p 16

p'VrDeviceInfo'vScreenCenter :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'eyeToScreenDistance p = plusPtr p 20

p'VrDeviceInfo'eyeToScreenDistance :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'lensSeparationDistance p = plusPtr p 24

p'VrDeviceInfo'lensSeparationDistance :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'interpupillaryDistance p = plusPtr p 28

p'VrDeviceInfo'interpupillaryDistance :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'lensDistortionValues p = plusPtr p 32

p'VrDeviceInfo'lensDistortionValues :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'chromaAbCorrection p = plusPtr p 48

p'VrDeviceInfo'chromaAbCorrection :: Ptr VrDeviceInfo -> Ptr CFloat

instance Storable VrDeviceInfo where
  sizeOf _ = 64
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    v5 <- peekByteOff _p 20
    v6 <- peekByteOff _p 24
    v7 <- peekByteOff _p 28
    v8 <- let s8 = div 16 $ sizeOf (undefined :: CFloat) in peekArray s8 (plusPtr _p 32)
    v9 <- let s9 = div 16 $ sizeOf (undefined :: CFloat) in peekArray s9 (plusPtr _p 48)
    return $ VrDeviceInfo v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  poke _p (VrDeviceInfo v0 v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    pokeByteOff _p 20 v5
    pokeByteOff _p 24 v6
    pokeByteOff _p 28 v7
    let s8 = div 16 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 32) (take s8 v8)
    let s9 = div 16 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 48) (take s9 v9)
    return ()

{- typedef struct VrStereoConfig {
            Matrix projection[2];
            Matrix viewOffset[2];
            float leftLensCenter[2];
            float rightLensCenter[2];
            float leftScreenCenter[2];
            float rightScreenCenter[2];
            float scale[2];
            float scaleIn[2];
        } VrStereoConfig; -}
data VrStereoConfig = VrStereoConfig
  { vrStereoConfig'projection :: [Matrix],
    vrStereoConfig'viewOffset :: [Matrix],
    vrStereoConfig'leftLensCenter :: [CFloat],
    vrStereoConfig'rightLensCenter :: [CFloat],
    vrStereoConfig'leftScreenCenter :: [CFloat],
    vrStereoConfig'rightScreenCenter :: [CFloat],
    vrStereoConfig'scale :: [CFloat],
    vrStereoConfig'scaleIn :: [CFloat]
  }
  deriving (Eq, Show)

p'VrStereoConfig'projection p = plusPtr p 0

p'VrStereoConfig'projection :: Ptr VrStereoConfig -> Ptr Matrix

p'VrStereoConfig'viewOffset p = plusPtr p 128

p'VrStereoConfig'viewOffset :: Ptr VrStereoConfig -> Ptr Matrix

p'VrStereoConfig'leftLensCenter p = plusPtr p 256

p'VrStereoConfig'leftLensCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'rightLensCenter p = plusPtr p 264

p'VrStereoConfig'rightLensCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'leftScreenCenter p = plusPtr p 272

p'VrStereoConfig'leftScreenCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'rightScreenCenter p = plusPtr p 280

p'VrStereoConfig'rightScreenCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'scale p = plusPtr p 288

p'VrStereoConfig'scale :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'scaleIn p = plusPtr p 296

p'VrStereoConfig'scaleIn :: Ptr VrStereoConfig -> Ptr CFloat

instance Storable VrStereoConfig where
  sizeOf _ = 304
  alignment _ = 4
  peek _p = do
    v0 <- let s0 = div 128 $ sizeOf (undefined :: Matrix) in peekArray s0 (plusPtr _p 0)
    v1 <- let s1 = div 128 $ sizeOf (undefined :: Matrix) in peekArray s1 (plusPtr _p 128)
    v2 <- let s2 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s2 (plusPtr _p 256)
    v3 <- let s3 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s3 (plusPtr _p 264)
    v4 <- let s4 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s4 (plusPtr _p 272)
    v5 <- let s5 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s5 (plusPtr _p 280)
    v6 <- let s6 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s6 (plusPtr _p 288)
    v7 <- let s7 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s7 (plusPtr _p 296)
    return $ VrStereoConfig v0 v1 v2 v3 v4 v5 v6 v7
  poke _p (VrStereoConfig v0 v1 v2 v3 v4 v5 v6 v7) = do
    let s0 = div 128 $ sizeOf (undefined :: Matrix)
    pokeArray (plusPtr _p 0) (take s0 v0)
    let s1 = div 128 $ sizeOf (undefined :: Matrix)
    pokeArray (plusPtr _p 128) (take s1 v1)
    let s2 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 256) (take s2 v2)
    let s3 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 264) (take s3 v3)
    let s4 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 272) (take s4 v4)
    let s5 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 280) (take s5 v5)
    let s6 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 288) (take s6 v6)
    let s7 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 296) (take s7 v7)
    return ()

{- typedef struct FilePathList {
            unsigned int capacity; unsigned int count; char * * paths;
        } FilePathList; -}
data FilePathList = FilePathList
  { filePathlist'capacity :: CUInt,
    filePathlist'count :: CUInt,
    filePathList'paths :: Ptr CString
  }
  deriving (Eq, Show)

p'FilePathList'capacity p = plusPtr p 0

p'FilePathList'capacity :: Ptr FilePathList -> Ptr CUInt

p'FilePathList'count p = plusPtr p 4

p'FilePathList'count :: Ptr FilePathList -> Ptr CUInt

p'FilePathList'paths p = plusPtr p 8

p'FilePathList'paths :: Ptr FilePathList -> Ptr (Ptr CString)

instance Storable FilePathList where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    return $ FilePathList v0 v1 v2
  poke _p (FilePathList v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    return ()
