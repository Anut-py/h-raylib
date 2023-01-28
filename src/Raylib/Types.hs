{-# OPTIONS -Wall #-}
module Raylib.Types where

-- This file includes Haskell counterparts to the structs defined in raylib

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
    nullPtr,
    peekArray,
    plusPtr,
    pokeArray,
    toBool, nullFunPtr
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
  )
import Foreign.C.String (castCCharToChar)
import GHC.IO (unsafePerformIO)
import Raylib.Util (newMaybeArray, peekMaybeArray, peekStaticArray, peekStaticArrayOff, pokeMaybeOff, pokeStaticArray, pokeStaticArrayOff, rightPad)

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
    arr <- peekArray (getPixelDataSize width height format) ptr
    return $ Image (map fromIntegral arr) width height mipmaps format
  poke _p (Image arr width height mipmaps format) = do
    pokeByteOff _p 0 =<< newArray (map fromIntegral arr :: [CUChar])
    pokeByteOff _p 8 (fromIntegral width :: CInt)
    pokeByteOff _p 12 (fromIntegral height :: CInt)
    pokeByteOff _p 16 (fromIntegral mipmaps :: CInt)
    pokeByteOff _p 20 format
    return ()

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

type RenderTexture2D = RenderTexture

data NPatchInfo = NPatchInfo
  { nPatchinfo'source :: Rectangle,
    nPatchinfo'left :: Int,
    nPatchinfo'top :: Int,
    nPatchinfo'right :: Int,
    nPatchinfo'bottom :: Int,
    nPatchinfo'layout :: NPatchLayout
  }
  deriving (Eq, Show)

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
  { glyphinfo'value :: Int,
    glyphInfo'offsetX :: Int,
    glyphInfo'offsetY :: Int,
    glyphInfo'advanceX :: Int,
    glyphinfo'image :: Image
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

data Camera3D = Camera3D
  { camera3D'position :: Vector3,
    camera3D'target :: Vector3,
    camera3D'up :: Vector3,
    camera3D'fovy :: Float,
    camera3D'projection :: CameraProjection
  }
  deriving (Eq, Show)

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
    camera2d'rotation :: Float,
    camera2d'zoom :: Float
  }
  deriving (Eq, Show)

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
    mesh'vboId :: [Integer]
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
    boneWeightsPtr <- (peekByteOff _p 88 :: IO (Ptr Float))
    boneWeights <- peekMaybeArray (vertexCount * 4) boneWeightsPtr
    vaoId <- fromIntegral <$> (peekByteOff _p 96 :: IO CUInt)
    vboIdPtr <- (peekByteOff _p 104 :: IO (Ptr CUInt))
    vboId <- map fromIntegral <$> peekArray 7 vboIdPtr
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
    newMaybeArray ((\m -> map fromIntegral m :: [CUShort]) <$> indices) >>= pokeByteOff _p 56
    newMaybeArray animVertices >>= pokeByteOff _p 64
    newMaybeArray animNormals >>= pokeByteOff _p 72
    newMaybeArray ((\m -> map fromIntegral m :: [CUChar]) <$> boneIds) >>= pokeByteOff _p 80
    newMaybeArray boneWeights >>= pokeByteOff _p 88
    pokeByteOff _p 96 (fromIntegral vaoId :: CUInt)
    pokeByteOff _p 104 =<< newArray (map fromIntegral vboId :: [CUInt])
    return ()

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
    pokeByteOff _p 8 =<< newArray (map fromIntegral locs :: [CInt])
    return ()

data MaterialMap = MaterialMap
  { materialmap'texture :: Texture,
    materialmap'color :: Color,
    materialmap'value :: Float
  }
  deriving (Eq, Show)

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
    material'maps :: [MaterialMap],
    material'params :: [Float]
  }
  deriving (Eq, Show)

instance Storable Material where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    shader <- peekByteOff _p 0
    mapsPtr <- (peekByteOff _p 16 :: IO (Ptr MaterialMap))
    maps <- peekArray 12 mapsPtr
    params <- map realToFrac <$> peekStaticArrayOff 4 (castPtr _p :: Ptr CFloat) 24
    return $ Material shader maps params
  poke _p (Material shader maps params) = do
    pokeByteOff _p 0 shader
    pokeByteOff _p 16 =<< newArray maps
    pokeStaticArrayOff (castPtr _p :: Ptr CFloat) 24 (map realToFrac params :: [CFloat])
    return ()

data Transform = Transform
  { transform'translation :: Vector3,
    transform'rotation :: Quaternion,
    transform'scale :: Vector3
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
    model'meshCount :: Int,
    model'materialCount :: Int,
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
    return $ Model transform meshCount materialCount meshes materials meshMaterial boneCount bones bindPose
  poke _p (Model transform meshCount materialCount meshes materials meshMaterial boneCount bones bindPose) = do
    pokeByteOff _p 0 transform
    pokeByteOff _p 64 (fromIntegral meshCount :: CInt)
    pokeByteOff _p 68 (fromIntegral materialCount :: CInt)
    pokeByteOff _p 72 =<< newArray meshes
    pokeByteOff _p 80 =<< newArray materials
    pokeByteOff _p 88 =<< newArray (map fromIntegral meshMaterial :: [CInt])
    pokeByteOff _p 96 (fromIntegral boneCount :: CInt)
    newMaybeArray bones >>= pokeByteOff _p 104
    newMaybeArray bindPose >>= pokeByteOff _p 112
    return ()

data ModelAnimation = ModelAnimation
  { modelAnimation'boneCount :: Int,
    modelAnimation'frameCount :: Int,
    modelAnimation'bones :: [BoneInfo],
    modelAnimation'framePoses :: [[Transform]]
  }
  deriving (Eq, Show)

instance Storable ModelAnimation where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    boneCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    frameCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    bonesPtr <- (peekByteOff _p 8 :: IO (Ptr BoneInfo))
    bones <- peekArray boneCount bonesPtr
    framePosesPtr <- (peekByteOff _p 16 :: IO (Ptr (Ptr Transform)))
    framePosesPtrArr <- peekArray frameCount framePosesPtr
    framePoses <- mapM (peekArray boneCount) framePosesPtrArr
    return $ ModelAnimation boneCount frameCount bones framePoses
  poke _p (ModelAnimation boneCount frameCount bones framePoses) = do
    pokeByteOff _p 0 (fromIntegral boneCount :: CInt)
    pokeByteOff _p 4 (fromIntegral frameCount :: CInt)
    pokeByteOff _p 8 =<< newArray bones
    mapM newArray framePoses >>= newArray >>= pokeByteOff _p 16
    return ()

data Ray = Ray
  { ray'position :: Vector3,
    ray'direction :: Vector3
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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

-- These types don't work perfectly right now, I need to fix them later on
-- They are currently used as `Ptr`s because peeking/poking them every time
-- an audio function is called doesn't work properly (they are stored in a
-- linked list in C, which makes it very difficult to properly peek/poke them)
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  { audioStream'buffer :: Ptr RAudioBuffer, -- TODO: Convert these into ForeignPtrs to make them automatically unload
    audioStream'processor :: Ptr RAudioProcessor,
    audioStream'sampleRate :: Integer,
    audioStream'sampleSize :: Integer,
    audiostream'channels :: Integer
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
    music'ctxData :: Ptr () -- TODO: Convert this into a ForeignPtr to make it automatically unload
  }
  deriving (Eq, Show)

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

type LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

type SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

type LoadFileTextCallback = FunPtr (CString -> IO CString)

type SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())
