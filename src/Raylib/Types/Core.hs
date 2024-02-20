{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Bindings for types used in all raylib modules
module Raylib.Types.Core
  ( -- * Enumerations
    ConfigFlag (..),
    TraceLogLevel (..),
    KeyboardKey (..),
    MouseButton (..),
    MouseCursor (..),
    GamepadButton (..),
    GamepadAxis (..),
    BlendMode (..),
    Gesture (..),

    -- * Structures
    Vector2 (..),
    Vector3 (..),
    Vector4 (..),
    Matrix (..),
    vectorToColor,
    Color (..),
    Rectangle (..),
    VrDeviceInfo (..),
    VrStereoConfig (..),
    FilePathList (..),
    AutomationEvent (..),
    AutomationEventList (..),
    Quaternion,
    AutomationEventListRef,

    -- * Callbacks
    LoadFileDataCallback,
    SaveFileDataCallback,
    LoadFileTextCallback,
    SaveFileTextCallback,
    C'LoadFileDataCallback,
    C'SaveFileDataCallback,
    C'LoadFileTextCallback,
    C'SaveFileTextCallback,
  )
where

import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    Word8,
    callocBytes,
    castPtr,
    newArray,
    peekArray,
  )
import Foreign.C
  ( CFloat,
    CInt (..),
    CString,
    CUChar,
    CUInt,
    newCString,
    peekCString,
  )
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, peekStaticArrayOff, pokeStaticArrayOff)

---------------------------------------
-- core enums -------------------------
---------------------------------------

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
    KeyMenu -> 5
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
    5 -> KeyMenu
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

---------------------------------------
-- core structures --------------------
---------------------------------------

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

data VrDeviceInfo = VrDeviceInfo
  { vrDeviceInfo'hResolution :: Int,
    vrDeviceInfo'vResolution :: Int,
    vrDeviceInfo'hScreenSize :: Float,
    vrDeviceInfo'vScreenSize :: Float,
    vrDeviceInfo'eyeToScreenDistance :: Float,
    vrDeviceInfo'lensSeparationDistance :: Float,
    vrDeviceInfo'interpupillaryDistance :: Float,
    vrDeviceInfo'lensDistortionValues :: [Float],
    vrDeviceInfo'chromaAbCorrection :: [Float]
  }
  deriving (Eq, Show, Freeable)

instance Storable VrDeviceInfo where
  sizeOf _ = 60
  alignment _ = 4
  peek _p = do
    hResolution <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    vResolution <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    hScreenSize <- realToFrac <$> (peekByteOff _p 8 :: IO CFloat)
    vScreenSize <- realToFrac <$> (peekByteOff _p 12 :: IO CFloat)
    eyeToScreenDistance <- realToFrac <$> (peekByteOff _p 16 :: IO CFloat)
    lensSeparationDistance <- realToFrac <$> (peekByteOff _p 20 :: IO CFloat)
    interpupillaryDistance <- realToFrac <$> (peekByteOff _p 24 :: IO CFloat)
    lensDistortionValues <- map realToFrac <$> (peekStaticArrayOff 4 (castPtr _p) 28 :: IO [CFloat])
    chromaAbCorrection <- map realToFrac <$> (peekStaticArrayOff 4 (castPtr _p) 44 :: IO [CFloat])
    return $ VrDeviceInfo hResolution vResolution hScreenSize vScreenSize eyeToScreenDistance lensSeparationDistance interpupillaryDistance lensDistortionValues chromaAbCorrection
  poke _p (VrDeviceInfo hResolution vResolution hScreenSize vScreenSize eyeToScreenDistance lensSeparationDistance interpupillaryDistance lensDistortionValues chromaAbCorrection) = do
    pokeByteOff _p 0 (fromIntegral hResolution :: CInt)
    pokeByteOff _p 4 (fromIntegral vResolution :: CInt)
    pokeByteOff _p 8 (realToFrac hScreenSize :: CFloat)
    pokeByteOff _p 12 (realToFrac vScreenSize :: CFloat)
    pokeByteOff _p 16 (realToFrac eyeToScreenDistance :: CFloat)
    pokeByteOff _p 20 (realToFrac lensSeparationDistance :: CFloat)
    pokeByteOff _p 24 (realToFrac interpupillaryDistance :: CFloat)
    pokeStaticArrayOff (castPtr _p) 28 (map realToFrac lensDistortionValues :: [CFloat])
    pokeStaticArrayOff (castPtr _p) 44 (map realToFrac chromaAbCorrection :: [CFloat])
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

data AutomationEvent = AutomationEvent
  { automationEvent'frame :: Integer,
    automationEvent'type :: Integer,
    automationEvent'params :: [Int]
  }
  deriving (Eq, Show, Freeable)

instance Storable AutomationEvent where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    frame <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    aeType <- fromIntegral <$> (peekByteOff _p 4 :: IO CUInt)
    params <- map fromIntegral <$> peekStaticArrayOff 4 (castPtr _p :: Ptr CInt) 8
    return $ AutomationEvent frame aeType params
  poke _p (AutomationEvent frame aeType params) = do
    pokeByteOff _p 0 (fromIntegral frame :: CUInt)
    pokeByteOff _p 4 (fromIntegral aeType :: CUInt)
    pokeStaticArrayOff (castPtr _p :: Ptr CInt) 8 (map fromIntegral params)
    return ()

data AutomationEventList = AutomationEventList
  { automationEventList'capacity :: Integer,
    automationEventList'events :: [AutomationEvent]
  }
  deriving (Eq, Show)

instance Storable AutomationEventList where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    capacity <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    count <- fromIntegral <$> (peekByteOff _p 4 :: IO CUInt)
    eventsPtr <- (peekByteOff _p 8 :: IO (Ptr AutomationEvent))
    events <- peekArray count eventsPtr
    return $ AutomationEventList capacity events
  poke _p (AutomationEventList capacity events) = do
    pokeByteOff _p 0 (fromIntegral capacity :: CUInt)
    pokeByteOff _p 4 (fromIntegral (length events) :: CUInt)
    ptr <- callocBytes $ case events of
      [] -> 0
      (x:_) -> fromIntegral capacity * sizeOf x
    pokeByteOff _p 8 ptr
    return ()

instance Freeable AutomationEventList where
  rlFreeDependents _ ptr = do
    eventsPtr <- (peekByteOff ptr 8 :: IO (Ptr AutomationEvent))
    c'free $ castPtr eventsPtr

type AutomationEventListRef = Ptr AutomationEventList

---------------------------------------
-- core callbacks ---------------------
---------------------------------------

type LoadFileDataCallback = String -> IO [Integer]

type SaveFileDataCallback a = String -> Ptr a -> Integer -> IO Bool

type LoadFileTextCallback = String -> IO String

type SaveFileTextCallback = String -> String -> IO Bool

-- TODO: Add FunPtrs to WindowResources for automatic memory management

type C'LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

type C'SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

type C'LoadFileTextCallback = FunPtr (CString -> IO CString)

type C'SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)
