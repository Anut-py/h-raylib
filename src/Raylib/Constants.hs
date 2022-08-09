module Raylib.Constants where

-- This file includes constants and enums defined in raylib

import Foreign.C (CUInt)

type ConfigFlags = CUInt

flag'vsyncHint = 64

flag'vsyncHint :: (Num a) => a

flag'fullscreenMode = 2

flag'fullscreenMode :: (Num a) => a

flag'windowResizable = 4

flag'windowResizable :: (Num a) => a

flag'windowUndecorated = 8

flag'windowUndecorated :: (Num a) => a

flag'windowHidden = 128

flag'windowHidden :: (Num a) => a

flag'windowMinimized = 512

flag'windowMinimized :: (Num a) => a

flag'windowMaximized = 1024

flag'windowMaximized :: (Num a) => a

flag'windowUnfocused = 2048

flag'windowUnfocused :: (Num a) => a

flag'windowTopmost = 4096

flag'windowTopmost :: (Num a) => a

flag'windowAlwaysRun = 256

flag'windowAlwaysRun :: (Num a) => a

flag'windowTransparent = 16

flag'windowTransparent :: (Num a) => a

flag'windowHighdpi = 8192

flag'windowHighdpi :: (Num a) => a

flag'windowMousePassthrough = 16384

flag'windowMousePassthrough :: (Num a) => a

flag'msaa4xHint = 32

flag'msaa4xHint :: (Num a) => a

flag'interlacedHint = 65536

flag'interlacedHint :: (Num a) => a

type TraceLogLevel = CUInt

log'all = 0

log'all :: (Num a) => a

log'trace = 1

log'trace :: (Num a) => a

log'debug = 2

log'debug :: (Num a) => a

log'info = 3

log'info :: (Num a) => a

log'warning = 4

log'warning :: (Num a) => a

log'error = 5

log'error :: (Num a) => a

log'fatal = 6

log'fatal :: (Num a) => a

log'none = 7

log'none :: (Num a) => a

type KeyboardKey = CUInt

key'null = 0

key'null :: (Num a) => a

key'apostrophe = 39

key'apostrophe :: (Num a) => a

key'comma = 44

key'comma :: (Num a) => a

key'minus = 45

key'minus :: (Num a) => a

key'period = 46

key'period :: (Num a) => a

key'slash = 47

key'slash :: (Num a) => a

key'zero = 48

key'zero :: (Num a) => a

key'one = 49

key'one :: (Num a) => a

key'two = 50

key'two :: (Num a) => a

key'three = 51

key'three :: (Num a) => a

key'four = 52

key'four :: (Num a) => a

key'five = 53

key'five :: (Num a) => a

key'six = 54

key'six :: (Num a) => a

key'seven = 55

key'seven :: (Num a) => a

key'eight = 56

key'eight :: (Num a) => a

key'nine = 57

key'nine :: (Num a) => a

key'semicolon = 59

key'semicolon :: (Num a) => a

key'equal = 61

key'equal :: (Num a) => a

key'a = 65

key'a :: (Num a) => a

key'b = 66

key'b :: (Num a) => a

key'c = 67

key'c :: (Num a) => a

key'd = 68

key'd :: (Num a) => a

key'e = 69

key'e :: (Num a) => a

key'f = 70

key'f :: (Num a) => a

key'g = 71

key'g :: (Num a) => a

key'h = 72

key'h :: (Num a) => a

key'i = 73

key'i :: (Num a) => a

key'j = 74

key'j :: (Num a) => a

key'k = 75

key'k :: (Num a) => a

key'l = 76

key'l :: (Num a) => a

key'm = 77

key'm :: (Num a) => a

key'n = 78

key'n :: (Num a) => a

key'o = 79

key'o :: (Num a) => a

key'p = 80

key'p :: (Num a) => a

key'q = 81

key'q :: (Num a) => a

key'r = 82

key'r :: (Num a) => a

key's = 83

key's :: (Num a) => a

key't = 84

key't :: (Num a) => a

key'u = 85

key'u :: (Num a) => a

key'v = 86

key'v :: (Num a) => a

key'w = 87

key'w :: (Num a) => a

key'x = 88

key'x :: (Num a) => a

key'y = 89

key'y :: (Num a) => a

key'z = 90

key'z :: (Num a) => a

key'leftBracket = 91

key'leftBracket :: (Num a) => a

key'backslash = 92

key'backslash :: (Num a) => a

key'rightBracket = 93

key'rightBracket :: (Num a) => a

key'grave = 96

key'grave :: (Num a) => a

key'space = 32

key'space :: (Num a) => a

key'escape = 256

key'escape :: (Num a) => a

key'enter = 257

key'enter :: (Num a) => a

key'tab = 258

key'tab :: (Num a) => a

key'backspace = 259

key'backspace :: (Num a) => a

key'insert = 260

key'insert :: (Num a) => a

key'delete = 261

key'delete :: (Num a) => a

key'right = 262

key'right :: (Num a) => a

key'left = 263

key'left :: (Num a) => a

key'down = 264

key'down :: (Num a) => a

key'up = 265

key'up :: (Num a) => a

key'pageUp = 266

key'pageUp :: (Num a) => a

key'pageDown = 267

key'pageDown :: (Num a) => a

key'home = 268

key'home :: (Num a) => a

key'end = 269

key'end :: (Num a) => a

key'capsLock = 280

key'capsLock :: (Num a) => a

key'scrollLock = 281

key'scrollLock :: (Num a) => a

key'numLock = 282

key'numLock :: (Num a) => a

key'printScreen = 283

key'printScreen :: (Num a) => a

key'pause = 284

key'pause :: (Num a) => a

key'f1 = 290

key'f1 :: (Num a) => a

key'f2 = 291

key'f2 :: (Num a) => a

key'f3 = 292

key'f3 :: (Num a) => a

key'f4 = 293

key'f4 :: (Num a) => a

key'f5 = 294

key'f5 :: (Num a) => a

key'f6 = 295

key'f6 :: (Num a) => a

key'f7 = 296

key'f7 :: (Num a) => a

key'f8 = 297

key'f8 :: (Num a) => a

key'f9 = 298

key'f9 :: (Num a) => a

key'f10 = 299

key'f10 :: (Num a) => a

key'f11 = 300

key'f11 :: (Num a) => a

key'f12 = 301

key'f12 :: (Num a) => a

key'leftShift = 340

key'leftShift :: (Num a) => a

key'leftControl = 341

key'leftControl :: (Num a) => a

key'leftAlt = 342

key'leftAlt :: (Num a) => a

key'leftSuper = 343

key'leftSuper :: (Num a) => a

key'rightShift = 344

key'rightShift :: (Num a) => a

key'rightControl = 345

key'rightControl :: (Num a) => a

key'rightAlt = 346

key'rightAlt :: (Num a) => a

key'rightSuper = 347

key'rightSuper :: (Num a) => a

key'kbMenu = 348

key'kbMenu :: (Num a) => a

key'kp0 = 320

key'kp0 :: (Num a) => a

key'kp1 = 321

key'kp1 :: (Num a) => a

key'kp2 = 322

key'kp2 :: (Num a) => a

key'kp3 = 323

key'kp3 :: (Num a) => a

key'kp4 = 324

key'kp4 :: (Num a) => a

key'kp5 = 325

key'kp5 :: (Num a) => a

key'kp6 = 326

key'kp6 :: (Num a) => a

key'kp7 = 327

key'kp7 :: (Num a) => a

key'kp8 = 328

key'kp8 :: (Num a) => a

key'kp9 = 329

key'kp9 :: (Num a) => a

key'kpDecimal = 330

key'kpDecimal :: (Num a) => a

key'kpDivide = 331

key'kpDivide :: (Num a) => a

key'kpMultiply = 332

key'kpMultiply :: (Num a) => a

key'kpSubtract = 333

key'kpSubtract :: (Num a) => a

key'kpAdd = 334

key'kpAdd :: (Num a) => a

key'kpEnter = 335

key'kpEnter :: (Num a) => a

key'kpEqual = 336

key'kpEqual :: (Num a) => a

key'back = 4

key'back :: (Num a) => a

key'menu = 82

key'menu :: (Num a) => a

key'volumeUp = 24

key'volumeUp :: (Num a) => a

key'volumeDown = 25

key'volumeDown :: (Num a) => a

type MouseButton = CUInt

mouseButton'left = 0

mouseButton'left :: (Num a) => a

mouseButton'right = 1

mouseButton'right :: (Num a) => a

mouseButton'middle = 2

mouseButton'middle :: (Num a) => a

mouseButton'side = 3

mouseButton'side :: (Num a) => a

mouseButton'extra = 4

mouseButton'extra :: (Num a) => a

mouseButton'forward = 5

mouseButton'forward :: (Num a) => a

mouseButton'back = 6

mouseButton'back :: (Num a) => a

type MouseCursor = CUInt

mouseCursor'default = 0

mouseCursor'default :: (Num a) => a

mouseCursor'arrow = 1

mouseCursor'arrow :: (Num a) => a

mouseCursor'ibeam = 2

mouseCursor'ibeam :: (Num a) => a

mouseCursor'crosshair = 3

mouseCursor'crosshair :: (Num a) => a

mouseCursor'pointingHand = 4

mouseCursor'pointingHand :: (Num a) => a

mouseCursor'resizeEW = 5

mouseCursor'resizeEW :: (Num a) => a

mouseCursor'resizeNS = 6

mouseCursor'resizeNS :: (Num a) => a

mouseCursor'resizeNWSE = 7

mouseCursor'resizeNWSE :: (Num a) => a

mouseCursor'resizeNESW = 8

mouseCursor'resizeNESW :: (Num a) => a

mouseCursor'resizeAll = 9

mouseCursor'resizeAll :: (Num a) => a

mouseCursor'notAllowed = 10

mouseCursor'notAllowed :: (Num a) => a

type GamepadButton = CUInt

gamepadButton'unknown = 0

gamepadButton'unknown :: (Num a) => a

gamepadButton'leftFaceUp = 1

gamepadButton'leftFaceUp :: (Num a) => a

gamepadButton'leftFaceRight = 2

gamepadButton'leftFaceRight :: (Num a) => a

gamepadButton'leftFaceDown = 3

gamepadButton'leftFaceDown :: (Num a) => a

gamepadButton'leftFaceLeft = 4

gamepadButton'leftFaceLeft :: (Num a) => a

gamepadButton'rightFaceUp = 5

gamepadButton'rightFaceUp :: (Num a) => a

gamepadButton'rightFaceRight = 6

gamepadButton'rightFaceRight :: (Num a) => a

gamepadButton'rightFaceDown = 7

gamepadButton'rightFaceDown :: (Num a) => a

gamepadButton'rightFaceLeft = 8

gamepadButton'rightFaceLeft :: (Num a) => a

gamepadButton'leftTrigger1 = 9

gamepadButton'leftTrigger1 :: (Num a) => a

gamepadButton'leftTrigger2 = 10

gamepadButton'leftTrigger2 :: (Num a) => a

gamepadButton'rightTrigger1 = 11

gamepadButton'rightTrigger1 :: (Num a) => a

gamepadButton'rightTrigger2 = 12

gamepadButton'rightTrigger2 :: (Num a) => a

gamepadButton'middleLeft = 13

gamepadButton'middleLeft :: (Num a) => a

gamepadButton'middle = 14

gamepadButton'middle :: (Num a) => a

gamepadButton'middleRight = 15

gamepadButton'middleRight :: (Num a) => a

gamepadButton'leftThumb = 16

gamepadButton'leftThumb :: (Num a) => a

gamepadButton'rightThumb = 17

gamepadButton'rightThumb :: (Num a) => a

type GamepadAxis = CUInt

gamepadAxisLeftX = 0

gamepadAxisLeftX :: (Num a) => a

gamepadAxisLeftY = 1

gamepadAxisLeftY :: (Num a) => a

gamepadAxisRightX = 2

gamepadAxisRightX :: (Num a) => a

gamepadAxisRightY = 3

gamepadAxisRightY :: (Num a) => a

gamepadAxisLeftTrigger = 4

gamepadAxisLeftTrigger :: (Num a) => a

gamepadAxisRightTrigger = 5

gamepadAxisRightTrigger :: (Num a) => a

type MaterialMapIndex = CUInt

materialMap'albedo = 0

materialMap'albedo :: (Num a) => a

materialMap'metalness = 1

materialMap'metalness :: (Num a) => a

materialMap'normal = 2

materialMap'normal :: (Num a) => a

materialMap'roughness = 3

materialMap'roughness :: (Num a) => a

materialMap'occlusion = 4

materialMap'occlusion :: (Num a) => a

materialMap'emission = 5

materialMap'emission :: (Num a) => a

materialMap'height = 6

materialMap'height :: (Num a) => a

materialMap'cubemap = 7

materialMap'cubemap :: (Num a) => a

materialMap'irradiance = 8

materialMap'irradiance :: (Num a) => a

materialMap'prefilter = 9

materialMap'prefilter :: (Num a) => a

materialMap'brdf = 10

materialMap'brdf :: (Num a) => a

type ShaderLocationIndex = CUInt

shaderLoc'vertexPosition = 0

shaderLoc'vertexPosition :: (Num a) => a

shaderLoc'vertexTexcoord01 = 1

shaderLoc'vertexTexcoord01 :: (Num a) => a

shaderLoc'vertexTexcoord02 = 2

shaderLoc'vertexTexcoord02 :: (Num a) => a

shaderLoc'vertexNormal = 3

shaderLoc'vertexNormal :: (Num a) => a

shaderLoc'vertexTangent = 4

shaderLoc'vertexTangent :: (Num a) => a

shaderLoc'vertexColor = 5

shaderLoc'vertexColor :: (Num a) => a

shaderLoc'matrixMvp = 6

shaderLoc'matrixMvp :: (Num a) => a

shaderLoc'matrixView = 7

shaderLoc'matrixView :: (Num a) => a

shaderLoc'matrixProjection = 8

shaderLoc'matrixProjection :: (Num a) => a

shaderLoc'matrixModel = 9

shaderLoc'matrixModel :: (Num a) => a

shaderLoc'matrixNormal = 10

shaderLoc'matrixNormal :: (Num a) => a

shaderLoc'vectorView = 11

shaderLoc'vectorView :: (Num a) => a

shaderLoc'colorDiffuse = 12

shaderLoc'colorDiffuse :: (Num a) => a

shaderLoc'colorSpecular = 13

shaderLoc'colorSpecular :: (Num a) => a

shaderLoc'colorAmbient = 14

shaderLoc'colorAmbient :: (Num a) => a

shaderLoc'mapAlbedo = 15

shaderLoc'mapAlbedo :: (Num a) => a

shaderLoc'mapMetalness = 16

shaderLoc'mapMetalness :: (Num a) => a

shaderLoc'mapNormal = 17

shaderLoc'mapNormal :: (Num a) => a

shaderLoc'mapRoughness = 18

shaderLoc'mapRoughness :: (Num a) => a

shaderLoc'mapOcclusion = 19

shaderLoc'mapOcclusion :: (Num a) => a

shaderLoc'mapEmission = 20

shaderLoc'mapEmission :: (Num a) => a

shaderLoc'mapHeight = 21

shaderLoc'mapHeight :: (Num a) => a

shaderLoc'mapCubemap = 22

shaderLoc'mapCubemap :: (Num a) => a

shaderLoc'mapIrradiance = 23

shaderLoc'mapIrradiance :: (Num a) => a

shaderLoc'mapPrefilter = 24

shaderLoc'mapPrefilter :: (Num a) => a

shaderLoc'mapBrdf = 25

shaderLoc'mapBrdf :: (Num a) => a

type ShaderUniformDataType = CUInt

shaderUniform'float = 0

shaderUniform'float :: (Num a) => a

shaderUniform'vec2 = 1

shaderUniform'vec2 :: (Num a) => a

shaderUniform'vec3 = 2

shaderUniform'vec3 :: (Num a) => a

shaderUniform'vec4 = 3

shaderUniform'vec4 :: (Num a) => a

shaderUniform'int = 4

shaderUniform'int :: (Num a) => a

shaderUniform'ivec2 = 5

shaderUniform'ivec2 :: (Num a) => a

shaderUniform'ivec3 = 6

shaderUniform'ivec3 :: (Num a) => a

shaderUniform'ivec4 = 7

shaderUniform'ivec4 :: (Num a) => a

shaderUniform'sampler2d = 8

shaderUniform'sampler2d :: (Num a) => a

type ShaderAttributeDataType = CUInt

shaderAttrib'float = 0

shaderAttrib'float :: (Num a) => a

shaderAttrib'vec2 = 1

shaderAttrib'vec2 :: (Num a) => a

shaderAttrib'vec3 = 2

shaderAttrib'vec3 :: (Num a) => a

shaderAttrib'vec4 = 3

shaderAttrib'vec4 :: (Num a) => a

type PixelFormat = CUInt

pixelFormat'uncompressedGrayscale = 1

pixelFormat'uncompressedGrayscale :: (Num a) => a

pixelFormat'uncompressedGrayAlpha = 2

pixelFormat'uncompressedGrayAlpha :: (Num a) => a

pixelFormat'uncompressedR5G6B5 = 3

pixelFormat'uncompressedR5G6B5 :: (Num a) => a

pixelFormat'uncompressedR8G8B8 = 4

pixelFormat'uncompressedR8G8B8 :: (Num a) => a

pixelFormat'uncompressedR5G5B5A1 = 5

pixelFormat'uncompressedR5G5B5A1 :: (Num a) => a

pixelFormat'uncompressedR4G4B4A4 = 6

pixelFormat'uncompressedR4G4B4A4 :: (Num a) => a

pixelFormat'uncompressedR8G8B8A8 = 7

pixelFormat'uncompressedR8G8B8A8 :: (Num a) => a

pixelFormat'uncompressedR32 = 8

pixelFormat'uncompressedR32 :: (Num a) => a

pixelFormat'uncompressedR32G32B32 = 9

pixelFormat'uncompressedR32G32B32 :: (Num a) => a

pixelFormat'uncompressedR32G32B32A32 = 10

pixelFormat'uncompressedR32G32B32A32 :: (Num a) => a

pixelFormat'compressedDxt1Rgb = 11

pixelFormat'compressedDxt1Rgb :: (Num a) => a

pixelFormat'compressedDxt1Rgba = 12

pixelFormat'compressedDxt1Rgba :: (Num a) => a

pixelFormat'compressedDxt3Rgba = 13

pixelFormat'compressedDxt3Rgba :: (Num a) => a

pixelFormat'compressedDxt5Rgba = 14

pixelFormat'compressedDxt5Rgba :: (Num a) => a

pixelFormat'compressedEtc1Rgb = 15

pixelFormat'compressedEtc1Rgb :: (Num a) => a

pixelFormat'compressedEtc2Rgb = 16

pixelFormat'compressedEtc2Rgb :: (Num a) => a

pixelFormat'compressedEtc2EacRgba = 17

pixelFormat'compressedEtc2EacRgba :: (Num a) => a

pixelFormat'compressedPvrtRgb = 18

pixelFormat'compressedPvrtRgb :: (Num a) => a

pixelFormat'compressedPvrtRgba = 19

pixelFormat'compressedPvrtRgba :: (Num a) => a

pixelFormat'compressedAstc4x4Rgba = 20

pixelFormat'compressedAstc4x4Rgba :: (Num a) => a

pixelFormat'compressedAstc8x8Rgba = 21

pixelFormat'compressedAstc8x8Rgba :: (Num a) => a

type TextureFilter = CUInt

textureFilter'point = 0

textureFilter'point :: (Num a) => a

textureFilter'bilinear = 1

textureFilter'bilinear :: (Num a) => a

textureFilter'trilinear = 2

textureFilter'trilinear :: (Num a) => a

textureFilter'anisotropic4x = 3

textureFilter'anisotropic4x :: (Num a) => a

textureFilter'anisotropic8x = 4

textureFilter'anisotropic8x :: (Num a) => a

textureFilter'anisotropic16x = 5

textureFilter'anisotropic16x :: (Num a) => a

type TextureWrap = CUInt

textureWrap'repeat = 0

textureWrap'repeat :: (Num a) => a

textureWrap'clamp = 1

textureWrap'clamp :: (Num a) => a

textureWrap'mirrorRepeat = 2

textureWrap'mirrorRepeat :: (Num a) => a

textureWrap'mirrorClamp = 3

textureWrap'mirrorClamp :: (Num a) => a

type CubemapLayout = CUInt

cubemapLayout'autoDetect = 0

cubemapLayout'autoDetect :: (Num a) => a

cubemapLayout'lineVertical = 1

cubemapLayout'lineVertical :: (Num a) => a

cubemapLayout'lineHorizontal = 2

cubemapLayout'lineHorizontal :: (Num a) => a

cubemapLayout'crossThreeByFour = 3

cubemapLayout'crossThreeByFour :: (Num a) => a

cubemapLayout'crossThreeByThree = 4

cubemapLayout'crossThreeByThree :: (Num a) => a

cubemapLayout'panorama = 5

cubemapLayout'panorama :: (Num a) => a

type FontType = CUInt

font'default = 0

font'default :: (Num a) => a

font'bitmap = 1

font'bitmap :: (Num a) => a

font'sdf = 2

font'sdf :: (Num a) => a

type BlendMode = CUInt

blend'alpha = 0

blend'alpha :: (Num a) => a

blend'additive = 1

blend'additive :: (Num a) => a

blend'multiplied = 2

blend'multiplied :: (Num a) => a

blend'addColors = 3

blend'addColors :: (Num a) => a

blend'subtractColors = 4

blend'subtractColors :: (Num a) => a

blend'alphaPremultiply = 5

blend'alphaPremultiply :: (Num a) => a

blend'custom = 6

blend'custom :: (Num a) => a

type Gesture = CUInt

gesture'none = 0

gesture'none :: (Num a) => a

gesture'tap = 1

gesture'tap :: (Num a) => a

gesture'doubletap = 2

gesture'doubletap :: (Num a) => a

gesture'hold = 4

gesture'hold :: (Num a) => a

gesture'drag = 8

gesture'drag :: (Num a) => a

gesture'swipeRight = 16

gesture'swipeRight :: (Num a) => a

gesture'swipeLeft = 32

gesture'swipeLeft :: (Num a) => a

gesture'swipeUp = 64

gesture'swipeUp :: (Num a) => a

gesture'swipeDown = 128

gesture'swipeDown :: (Num a) => a

gesture'pinchIn = 256

gesture'pinchIn :: (Num a) => a

gesture'pinchOut = 512

gesture'pinchOut :: (Num a) => a

type CameraMode = CUInt

cameraMode'custom = 0

cameraMode'custom :: (Num a) => a

cameraMode'free = 1

cameraMode'free :: (Num a) => a

cameraMode'orbital = 2

cameraMode'orbital :: (Num a) => a

cameraMode'firstPerson = 3

cameraMode'firstPerson :: (Num a) => a

cameraMode'thirdPerson = 4

cameraMode'thirdPerson :: (Num a) => a

type CameraProjection = CUInt

cameraProjection'perspective = 0

cameraProjection'perspective :: (Num a) => a

cameraProjection'orthographic = 1

cameraProjection'orthographic :: (Num a) => a

type NPatchLayout = CUInt

npatch'ninePatch = 0

npatch'ninePatch :: (Num a) => a

npatch'threePatchVertical = 1

npatch'threePatchVertical :: (Num a) => a

npatch'threePatchHorizontal = 2

npatch'threePatchHorizontal :: (Num a) => a