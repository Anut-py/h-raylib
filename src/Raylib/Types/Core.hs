{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

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
    Vector2,
    Vector3,
    Vector4,
    pattern Vector2,
    vector2'x,
    vector2'y,
    pattern Vector3,
    vector3'x,
    vector3'y,
    vector3'z,
    pattern Vector4,
    vector4'x,
    vector4'y,
    vector4'z,
    vector4'w,
    vectorToColor,
    Matrix (..),
    Color (..),
    Rectangle (..),
    VrDeviceInfo (..),
    VrStereoConfig (..),
    FilePathList (..),
    AutomationEvent (..),
    AutomationEventList (..),
    Quaternion,
    AutomationEventListRef,

    -- * Pointer utilities
    p'vector2'x,
    p'vector2'y,
    p'vector3'x,
    p'vector3'y,
    p'vector3'z,
    p'vector4'x,
    p'vector4'y,
    p'vector4'z,
    p'vector4'w,
    p'matrix'm0,
    p'matrix'm4,
    p'matrix'm8,
    p'matrix'm12,
    p'matrix'm1,
    p'matrix'm5,
    p'matrix'm9,
    p'matrix'm13,
    p'matrix'm2,
    p'matrix'm6,
    p'matrix'm10,
    p'matrix'm14,
    p'matrix'm3,
    p'matrix'm7,
    p'matrix'm11,
    p'matrix'm15,
    p'color'r,
    p'color'g,
    p'color'b,
    p'color'a,
    p'rectangle'x,
    p'rectangle'y,
    p'rectangle'width,
    p'rectangle'height,
    p'vrDeviceInfo'hResolution,
    p'vrDeviceInfo'vResolution,
    p'vrDeviceInfo'hScreenSize,
    p'vrDeviceInfo'vScreenSize,
    p'vrDeviceInfo'eyeToScreenDistance,
    p'vrDeviceInfo'lensSeparationDistance,
    p'vrDeviceInfo'interpupillaryDistance,
    p'vrDeviceInfo'lensDistortionValues,
    p'vrDeviceInfo'chromaAbCorrection,
    p'vrStereoConfig'projection,
    p'vrStereoConfig'viewOffset,
    p'vrStereoConfig'leftLensCenter,
    p'vrStereoConfig'rightLensCenter,
    p'vrStereoConfig'leftScreenCenter,
    p'vrStereoConfig'rightScreenCenter,
    p'vrStereoConfig'scale,
    p'vrStereoConfig'scaleIn,
    p'filePathList'capacity,
    p'filePathList'count,
    p'filePathList'paths,
    p'automationEvent'frame,
    p'automationEvent'type,
    p'automationEvent'params,
    p'automationEventList'capacity,
    p'automationEventList'count,
    p'automationEventList'events,

    -- * Callbacks
    TraceLogCallback,
    LoadFileDataCallback,
    SaveFileDataCallback,
    LoadFileTextCallback,
    SaveFileTextCallback,
    C'TraceLogCallback,
    C'LoadFileDataCallback,
    C'SaveFileDataCallback,
    C'LoadFileTextCallback,
    C'SaveFileTextCallback,
  )
where

import Foreign
  ( FunPtr,
    Ptr,
    Storable (alignment, peek, poke, sizeOf),
    Word8,
    castPtr,
    mallocArray,
    newArray,
    peekArray,
    plusPtr,
    pokeArray,
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
import Raylib.Internal (Closeable(..), _unloadAutomationEventList, addAutomationEventList)
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, peekStaticArray, pokeStaticArray)

#ifndef DISABLE_LENS

import Linear (V2(V2), V3(V3), V4(V4))

#endif


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

pattern Vector2 :: Float -> Float -> Vector2
pattern Vector3 :: Float -> Float -> Float -> Vector3
pattern Vector4 :: Float -> Float -> Float -> Float -> Vector4

#ifdef DISABLE_LENS

data Vector2' = Vector2' Float Float deriving (Eq, Show, Freeable)
instance Storable Vector2' where
  sizeOf _ = 8
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> peek (p'vector2'x _p)
    y <- realToFrac <$> peek (p'vector2'y _p)
    return $ Vector2' x y
  poke _p (Vector2' x y) = do
    poke (p'vector2'x _p) (realToFrac x)
    poke (p'vector2'y _p) (realToFrac y)
    return ()
type Vector2 = Vector2'

pattern Vector2
  { vector2'x ,
    vector2'y
  } = Vector2' vector2'x vector2'y
{-# COMPLETE Vector2 :: Vector2' #-}

data Vector3' = Vector3' Float Float Float deriving (Eq, Show, Freeable)

instance Storable Vector3' where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> peek (p'vector3'x _p)
    y <- realToFrac <$> peek (p'vector3'y _p)
    z <- realToFrac <$> peek (p'vector3'z _p)
    return $ Vector3' x y z
  poke _p (Vector3' x y z) = do
    poke (p'vector3'x _p) (realToFrac x)
    poke (p'vector3'y _p) (realToFrac y)
    poke (p'vector3'z _p) (realToFrac z)
    return ()

type Vector3 = Vector3'

pattern Vector3
  { vector3'x ,
    vector3'y ,
    vector3'z
  } = Vector3' vector3'x vector3'y vector3'z
{-# COMPLETE Vector3 :: Vector3' #-}

data Vector4' = Vector4' Float Float Float Float deriving (Eq, Show, Freeable)

instance Storable Vector4' where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    x <- realToFrac <$> peek (p'vector4'x _p)
    y <- realToFrac <$> peek (p'vector4'y _p)
    z <- realToFrac <$> peek (p'vector4'z _p)
    w <- realToFrac <$> peek (p'vector4'w _p)
    return $ Vector4' x y z w
  poke _p (Vector4' x y z w) = do
    poke (p'vector4'x _p) (realToFrac x)
    poke (p'vector4'y _p) (realToFrac y)
    poke (p'vector4'z _p) (realToFrac z)
    poke (p'vector4'w _p) (realToFrac w)
    return ()

type Vector4 = Vector4'

pattern Vector4
  { vector4'x,
    vector4'y,
    vector4'z,
    vector4'w
  } = Vector4' vector4'x vector4'y vector4'z vector4'w
{-# COMPLETE Vector4 :: Vector4' #-}

#else

type Vector2 = V2 Float

pattern Vector2
  { vector2'x ,
    vector2'y
  } = V2 vector2'x vector2'y
{-# COMPLETE Vector2 :: V2 #-}

type Vector3 = V3 Float

pattern Vector3
  { vector3'x ,
    vector3'y ,
    vector3'z
  } = V3 vector3'x vector3'y vector3'z
{-# COMPLETE Vector3 :: V3 #-}

type Vector4 = V4 Float

pattern Vector4
  { vector4'x,
    vector4'y,
    vector4'z,
    vector4'w
  } = V4 vector4'x vector4'y vector4'z vector4'w
{-# COMPLETE Vector4 :: V4 #-}

#endif

p'vector2'x :: Ptr Vector2 -> Ptr CFloat
p'vector2'x = (`plusPtr` 0)

p'vector2'y :: Ptr Vector2 -> Ptr CFloat
p'vector2'y = (`plusPtr` 4)

p'vector3'x :: Ptr Vector3 -> Ptr CFloat
p'vector3'x = (`plusPtr` 0)

p'vector3'y :: Ptr Vector3 -> Ptr CFloat
p'vector3'y = (`plusPtr` 4)

p'vector3'z :: Ptr Vector3 -> Ptr CFloat
p'vector3'z = (`plusPtr` 8)

vectorToColor :: Vector4 -> Color
vectorToColor (Vector4 x y z w) = Color (round $ x * 255) (round $ y * 255) (round $ z * 255) (round $ w * 255)

p'vector4'x :: Ptr Vector4 -> Ptr CFloat
p'vector4'x = (`plusPtr` 0)

p'vector4'y :: Ptr Vector4 -> Ptr CFloat
p'vector4'y = (`plusPtr` 4)

p'vector4'z :: Ptr Vector4 -> Ptr CFloat
p'vector4'z = (`plusPtr` 8)

p'vector4'w :: Ptr Vector4 -> Ptr CFloat
p'vector4'w = (`plusPtr` 12)

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
    m0 <- realToFrac <$> peek (p'matrix'm0 _p)
    m4 <- realToFrac <$> peek (p'matrix'm4 _p)
    m8 <- realToFrac <$> peek (p'matrix'm8 _p)
    m12 <- realToFrac <$> peek (p'matrix'm12 _p)
    m1 <- realToFrac <$> peek (p'matrix'm1 _p)
    m5 <- realToFrac <$> peek (p'matrix'm5 _p)
    m9 <- realToFrac <$> peek (p'matrix'm9 _p)
    m13 <- realToFrac <$> peek (p'matrix'm13 _p)
    m2 <- realToFrac <$> peek (p'matrix'm2 _p)
    m6 <- realToFrac <$> peek (p'matrix'm6 _p)
    m10 <- realToFrac <$> peek (p'matrix'm10 _p)
    m14 <- realToFrac <$> peek (p'matrix'm14 _p)
    m3 <- realToFrac <$> peek (p'matrix'm3 _p)
    m7 <- realToFrac <$> peek (p'matrix'm7 _p)
    m11 <- realToFrac <$> peek (p'matrix'm11 _p)
    m15 <- realToFrac <$> peek (p'matrix'm15 _p)
    return $ Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15
  poke _p (Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) = do
    poke (p'matrix'm0 _p) (realToFrac m0)
    poke (p'matrix'm4 _p) (realToFrac m4)
    poke (p'matrix'm8 _p) (realToFrac m8)
    poke (p'matrix'm12 _p) (realToFrac m12)
    poke (p'matrix'm1 _p) (realToFrac m1)
    poke (p'matrix'm5 _p) (realToFrac m5)
    poke (p'matrix'm9 _p) (realToFrac m9)
    poke (p'matrix'm13 _p) (realToFrac m13)
    poke (p'matrix'm2 _p) (realToFrac m2)
    poke (p'matrix'm6 _p) (realToFrac m6)
    poke (p'matrix'm10 _p) (realToFrac m10)
    poke (p'matrix'm14 _p) (realToFrac m14)
    poke (p'matrix'm3 _p) (realToFrac m3)
    poke (p'matrix'm7 _p) (realToFrac m7)
    poke (p'matrix'm11 _p) (realToFrac m11)
    poke (p'matrix'm15 _p) (realToFrac m15)
    return ()

p'matrix'm0 :: Ptr Matrix -> Ptr CFloat
p'matrix'm0 = (`plusPtr` 0)

p'matrix'm4 :: Ptr Matrix -> Ptr CFloat
p'matrix'm4 = (`plusPtr` 4)

p'matrix'm8 :: Ptr Matrix -> Ptr CFloat
p'matrix'm8 = (`plusPtr` 8)

p'matrix'm12 :: Ptr Matrix -> Ptr CFloat
p'matrix'm12 = (`plusPtr` 12)

p'matrix'm1 :: Ptr Matrix -> Ptr CFloat
p'matrix'm1 = (`plusPtr` 16)

p'matrix'm5 :: Ptr Matrix -> Ptr CFloat
p'matrix'm5 = (`plusPtr` 20)

p'matrix'm9 :: Ptr Matrix -> Ptr CFloat
p'matrix'm9 = (`plusPtr` 24)

p'matrix'm13 :: Ptr Matrix -> Ptr CFloat
p'matrix'm13 = (`plusPtr` 28)

p'matrix'm2 :: Ptr Matrix -> Ptr CFloat
p'matrix'm2 = (`plusPtr` 32)

p'matrix'm6 :: Ptr Matrix -> Ptr CFloat
p'matrix'm6 = (`plusPtr` 36)

p'matrix'm10 :: Ptr Matrix -> Ptr CFloat
p'matrix'm10 = (`plusPtr` 40)

p'matrix'm14 :: Ptr Matrix -> Ptr CFloat
p'matrix'm14 = (`plusPtr` 44)

p'matrix'm3 :: Ptr Matrix -> Ptr CFloat
p'matrix'm3 = (`plusPtr` 48)

p'matrix'm7 :: Ptr Matrix -> Ptr CFloat
p'matrix'm7 = (`plusPtr` 52)

p'matrix'm11 :: Ptr Matrix -> Ptr CFloat
p'matrix'm11 = (`plusPtr` 56)

p'matrix'm15 :: Ptr Matrix -> Ptr CFloat
p'matrix'm15 = (`plusPtr` 60)

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
    r <- fromIntegral <$> peek (p'color'r _p)
    g <- fromIntegral <$> peek (p'color'g _p)
    b <- fromIntegral <$> peek (p'color'b _p)
    a <- fromIntegral <$> peek (p'color'a _p)
    return $ Color r g b a
  poke _p (Color r g b a) = do
    poke (p'color'r _p) (fromIntegral r)
    poke (p'color'g _p) (fromIntegral g)
    poke (p'color'b _p) (fromIntegral b)
    poke (p'color'a _p) (fromIntegral a)
    return ()

p'color'r :: Ptr Color -> Ptr CUChar
p'color'r = (`plusPtr` 0)

p'color'g :: Ptr Color -> Ptr CUChar
p'color'g = (`plusPtr` 1)

p'color'b :: Ptr Color -> Ptr CUChar
p'color'b = (`plusPtr` 2)

p'color'a :: Ptr Color -> Ptr CUChar
p'color'a = (`plusPtr` 3)

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
    x <- realToFrac <$> peek (p'rectangle'x _p)
    y <- realToFrac <$> peek (p'rectangle'y _p)
    width <- realToFrac <$> peek (p'rectangle'width _p)
    height <- realToFrac <$> peek (p'rectangle'height _p)
    return $ Rectangle x y width height
  poke _p (Rectangle x y width height) = do
    poke (p'rectangle'x _p) (realToFrac x)
    poke (p'rectangle'y _p) (realToFrac y)
    poke (p'rectangle'width _p) (realToFrac width)
    poke (p'rectangle'height _p) (realToFrac height)
    return ()

p'rectangle'x :: Ptr Rectangle -> Ptr CFloat
p'rectangle'x = (`plusPtr` 0)

p'rectangle'y :: Ptr Rectangle -> Ptr CFloat
p'rectangle'y = (`plusPtr` 4)

p'rectangle'width :: Ptr Rectangle -> Ptr CFloat
p'rectangle'width = (`plusPtr` 8)

p'rectangle'height :: Ptr Rectangle -> Ptr CFloat
p'rectangle'height = (`plusPtr` 12)

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
    hResolution <- fromIntegral <$> peek (p'vrDeviceInfo'hResolution _p)
    vResolution <- fromIntegral <$> peek (p'vrDeviceInfo'vResolution _p)
    hScreenSize <- realToFrac <$> peek (p'vrDeviceInfo'hScreenSize _p)
    vScreenSize <- realToFrac <$> peek (p'vrDeviceInfo'vScreenSize _p)
    eyeToScreenDistance <- realToFrac <$> peek (p'vrDeviceInfo'eyeToScreenDistance _p)
    lensSeparationDistance <- realToFrac <$> peek (p'vrDeviceInfo'lensSeparationDistance _p)
    interpupillaryDistance <- realToFrac <$> peek (p'vrDeviceInfo'interpupillaryDistance _p)
    lensDistortionValues <- map realToFrac <$> peekStaticArray 4 (p'vrDeviceInfo'lensDistortionValues _p)
    chromaAbCorrection <- map realToFrac <$> peekStaticArray 4 (p'vrDeviceInfo'chromaAbCorrection _p)
    return $ VrDeviceInfo hResolution vResolution hScreenSize vScreenSize eyeToScreenDistance lensSeparationDistance interpupillaryDistance lensDistortionValues chromaAbCorrection
  poke _p (VrDeviceInfo hResolution vResolution hScreenSize vScreenSize eyeToScreenDistance lensSeparationDistance interpupillaryDistance lensDistortionValues chromaAbCorrection) = do
    poke (p'vrDeviceInfo'hResolution _p) (fromIntegral hResolution)
    poke (p'vrDeviceInfo'vResolution _p) (fromIntegral vResolution)
    poke (p'vrDeviceInfo'hScreenSize _p) (realToFrac hScreenSize)
    poke (p'vrDeviceInfo'vScreenSize _p) (realToFrac vScreenSize)
    poke (p'vrDeviceInfo'eyeToScreenDistance _p) (realToFrac eyeToScreenDistance)
    poke (p'vrDeviceInfo'lensSeparationDistance _p) (realToFrac lensSeparationDistance)
    poke (p'vrDeviceInfo'interpupillaryDistance _p) (realToFrac interpupillaryDistance)
    pokeStaticArray (p'vrDeviceInfo'lensDistortionValues _p) (map realToFrac lensDistortionValues)
    pokeStaticArray (p'vrDeviceInfo'chromaAbCorrection _p) (map realToFrac chromaAbCorrection)
    return ()

p'vrDeviceInfo'hResolution :: Ptr VrDeviceInfo -> Ptr CInt
p'vrDeviceInfo'hResolution = (`plusPtr` 0)

p'vrDeviceInfo'vResolution :: Ptr VrDeviceInfo -> Ptr CInt
p'vrDeviceInfo'vResolution = (`plusPtr` 4)

p'vrDeviceInfo'hScreenSize :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'hScreenSize = (`plusPtr` 8)

p'vrDeviceInfo'vScreenSize :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'vScreenSize = (`plusPtr` 12)

p'vrDeviceInfo'eyeToScreenDistance :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'eyeToScreenDistance = (`plusPtr` 16)

p'vrDeviceInfo'lensSeparationDistance :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'lensSeparationDistance = (`plusPtr` 20)

p'vrDeviceInfo'interpupillaryDistance :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'interpupillaryDistance = (`plusPtr` 24)

-- static array (4)
p'vrDeviceInfo'lensDistortionValues :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'lensDistortionValues = (`plusPtr` 28)

-- static array (4)
p'vrDeviceInfo'chromaAbCorrection :: Ptr VrDeviceInfo -> Ptr CFloat
p'vrDeviceInfo'chromaAbCorrection = (`plusPtr` 44)

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
    projection <- peekStaticArray 2 (p'vrStereoConfig'projection _p)
    viewOffset <- peekStaticArray 2 (p'vrStereoConfig'viewOffset _p)
    leftLensCenter <- map realToFrac <$> peekStaticArray 2 (p'vrStereoConfig'leftLensCenter _p)
    rightLensCenter <- map realToFrac <$> peekStaticArray 2 (p'vrStereoConfig'rightLensCenter _p)
    leftScreenCenter <- map realToFrac <$> peekStaticArray 2 (p'vrStereoConfig'leftScreenCenter _p)
    rightScreenCenter <- map realToFrac <$> peekStaticArray 2 (p'vrStereoConfig'rightScreenCenter _p)
    scale <- map realToFrac <$> peekStaticArray 2 (p'vrStereoConfig'scale _p)
    scaleIn <- map realToFrac <$> peekStaticArray 2 (p'vrStereoConfig'scaleIn _p)
    return $ VrStereoConfig projection viewOffset leftLensCenter rightLensCenter leftScreenCenter rightScreenCenter scale scaleIn
  poke _p (VrStereoConfig projection viewOffset leftLensCenter rightLensCenter leftScreenCenter rightScreenCenter scale scaleIn) = do
    pokeStaticArray (p'vrStereoConfig'projection _p) projection
    pokeStaticArray (p'vrStereoConfig'viewOffset _p) viewOffset
    pokeStaticArray (p'vrStereoConfig'leftLensCenter _p) (map realToFrac leftLensCenter)
    pokeStaticArray (p'vrStereoConfig'rightLensCenter _p) (map realToFrac rightLensCenter)
    pokeStaticArray (p'vrStereoConfig'leftScreenCenter _p) (map realToFrac leftScreenCenter)
    pokeStaticArray (p'vrStereoConfig'rightScreenCenter _p) (map realToFrac rightScreenCenter)
    pokeStaticArray (p'vrStereoConfig'scale _p) (map realToFrac scale)
    pokeStaticArray (p'vrStereoConfig'scaleIn _p) (map realToFrac scaleIn)
    return ()

-- static array (2)
p'vrStereoConfig'projection :: Ptr VrStereoConfig -> Ptr Matrix
p'vrStereoConfig'projection = (`plusPtr` 0)

-- static array (2)
p'vrStereoConfig'viewOffset :: Ptr VrStereoConfig -> Ptr Matrix
p'vrStereoConfig'viewOffset = (`plusPtr` 128)

-- static array (2)
p'vrStereoConfig'leftLensCenter :: Ptr VrStereoConfig -> Ptr CFloat
p'vrStereoConfig'leftLensCenter = (`plusPtr` 256)

-- static array (2)
p'vrStereoConfig'rightLensCenter :: Ptr VrStereoConfig -> Ptr CFloat
p'vrStereoConfig'rightLensCenter = (`plusPtr` 264)

-- static array (2)
p'vrStereoConfig'leftScreenCenter :: Ptr VrStereoConfig -> Ptr CFloat
p'vrStereoConfig'leftScreenCenter = (`plusPtr` 272)

-- static array (2)
p'vrStereoConfig'rightScreenCenter :: Ptr VrStereoConfig -> Ptr CFloat
p'vrStereoConfig'rightScreenCenter = (`plusPtr` 280)

-- static array (2)
p'vrStereoConfig'scale :: Ptr VrStereoConfig -> Ptr CFloat
p'vrStereoConfig'scale = (`plusPtr` 288)

-- static array (2)
p'vrStereoConfig'scaleIn :: Ptr VrStereoConfig -> Ptr CFloat
p'vrStereoConfig'scaleIn = (`plusPtr` 296)

data FilePathList = FilePathList
  { filePathList'capacity :: Integer,
    filePathList'paths :: [String]
  }
  deriving (Eq, Show)

instance Storable FilePathList where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    capacity <- fromIntegral <$> peek (p'filePathList'capacity _p)
    count <- fromIntegral <$> peek (p'filePathList'count _p)
    pathsPtr <- peek (p'filePathList'paths _p)
    pathsCStrings <- peekArray count pathsPtr
    paths <- mapM peekCString pathsCStrings
    return $ FilePathList capacity paths
  poke _p (FilePathList capacity paths) = do
    poke (p'filePathList'capacity _p) (fromIntegral capacity)
    poke (p'filePathList'count _p) (fromIntegral (length paths))
    pathsCStrings <- mapM newCString paths
    poke (p'filePathList'paths _p) =<< newArray pathsCStrings
    return ()

p'filePathList'capacity :: Ptr FilePathList -> Ptr CUInt
p'filePathList'capacity = (`plusPtr` 0)

p'filePathList'count :: Ptr FilePathList -> Ptr CUInt
p'filePathList'count = (`plusPtr` 4)

-- array (filePathList'count)
p'filePathList'paths :: Ptr FilePathList -> Ptr (Ptr CString)
p'filePathList'paths = (`plusPtr` 8)

instance Freeable FilePathList where
  rlFreeDependents val ptr = do
    pathsPtr <- peek (p'filePathList'paths ptr)
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
    frame <- fromIntegral <$> peek (p'automationEvent'frame _p)
    aeType <- fromIntegral <$> peek (p'automationEvent'type _p)
    params <- map fromIntegral <$> peekStaticArray 4 (p'automationEvent'params _p)
    return $ AutomationEvent frame aeType params
  poke _p (AutomationEvent frame aeType params) = do
    poke (p'automationEvent'frame _p) (fromIntegral frame)
    poke (p'automationEvent'type _p) (fromIntegral aeType)
    pokeStaticArray (p'automationEvent'params _p) (map fromIntegral params)
    return ()

p'automationEvent'frame :: Ptr AutomationEvent -> Ptr CUInt
p'automationEvent'frame = (`plusPtr` 0)

p'automationEvent'type :: Ptr AutomationEvent -> Ptr CUInt
p'automationEvent'type = (`plusPtr` 4)

-- static array (4)
p'automationEvent'params :: Ptr AutomationEvent -> Ptr CInt
p'automationEvent'params = (`plusPtr` 8)

data AutomationEventList = AutomationEventList
  { automationEventList'capacity :: Integer,
    automationEventList'events :: [AutomationEvent]
  }
  deriving (Eq, Show)

instance Storable AutomationEventList where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    capacity <- fromIntegral <$> peek (p'automationEventList'capacity _p)
    count <- fromIntegral <$> peek (p'automationEventList'count _p)
    events <- peekArray count =<< peek (p'automationEventList'events _p)
    return $ AutomationEventList capacity events
  poke _p (AutomationEventList capacity events) = do
    poke (p'automationEventList'capacity _p) (fromIntegral capacity)
    poke (p'automationEventList'count _p) (fromIntegral (length events))
    eventsPtr <- mallocArray (fromIntegral capacity)
    pokeArray eventsPtr events
    poke (p'automationEventList'events _p) eventsPtr
    return ()

p'automationEventList'capacity :: Ptr AutomationEventList -> Ptr CUInt
p'automationEventList'capacity = (`plusPtr` 0)

p'automationEventList'count :: Ptr AutomationEventList -> Ptr CUInt
p'automationEventList'count = (`plusPtr` 4)

-- array (automationEventList'count)
p'automationEventList'events :: Ptr AutomationEventList -> Ptr (Ptr AutomationEvent)
p'automationEventList'events = (`plusPtr` 8)

instance Freeable AutomationEventList where
  rlFreeDependents _ ptr = do
    c'free . castPtr =<< peek (p'automationEventList'events ptr)

type AutomationEventListRef = Ptr AutomationEventList

instance Closeable AutomationEventListRef where
  close = _unloadAutomationEventList . castPtr
  addToWindowResources window listRef = addAutomationEventList (castPtr listRef) window

---------------------------------------
-- core callbacks ---------------------
---------------------------------------

type TraceLogCallback = TraceLogLevel -> String -> IO ()

type LoadFileDataCallback = String -> IO [Integer]

type SaveFileDataCallback a = String -> Ptr a -> Integer -> IO Bool

type LoadFileTextCallback = String -> IO String

type SaveFileTextCallback = String -> String -> IO Bool

type C'TraceLogCallback = FunPtr (CInt -> CString -> IO ())

type C'LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

type C'SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

type C'LoadFileTextCallback = FunPtr (CString -> IO CString)

type C'SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)
