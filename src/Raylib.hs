{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS -Wall #-}

module Raylib where

-- Haskell bindings to raylib

import Data.List (genericLength)
import Foreign
  ( FunPtr,
    Ptr,
    Storable (peek, sizeOf),
    castPtr,
    fromBool,
    peekArray,
    toBool,
    with,
    withArray,
    withArrayLen,
  )
import Foreign.C
  ( CBool (..),
    CChar (..),
    CDouble (..),
    CFloat (..),
    CInt (..),
    CLong (..),
    CString,
    CUChar,
    CUInt (..),
    peekCString,
    withCString,
  )
import GHC.IO (unsafePerformIO)
import Raylib.Types
  ( AudioStream,
    BoundingBox,
    Camera2D,
    Camera3D,
    Color,
    FilePathList,
    Font,
    GlyphInfo,
    Image (image'height, image'width),
    Material,
    Matrix,
    Mesh,
    Model,
    ModelAnimation,
    Music,
    NPatchInfo,
    Ray,
    RayCollision,
    Rectangle,
    RenderTexture,
    Shader,
    Sound,
    Texture,
    Vector2 (Vector2),
    Vector3,
    Vector4,
    VrDeviceInfo,
    VrStereoConfig,
    Wave (wave'channels, wave'frameCount),
    MouseButton,
    MouseCursor,
    TraceLogLevel,
    CameraMode,
    Gesture,
    BlendMode,
    CubemapLayout,
    FontType,
    TextureWrap,
    TextureFilter,
    ConfigFlag,
    KeyboardKey,
    GamepadButton,
    GamepadAxis,
    ShaderUniformDataType,
    PixelFormat
  )
import Raylib.Util (pop, withArray2D, configsToBitflag, withMaybeCString)
import Prelude hiding (length)

-- Haskell doesn't support varargs in foreign calls, so these functions are impossible to call from FFI
-- type TraceLogCallback = FunPtr (CInt -> CString -> __builtin_va_list -> IO ())
-- foreign import ccall safe "wrapper"
--   mk'TraceLogCallback ::
--     (CInt -> CString -> __builtin_va_list -> IO ()) -> IO TraceLogCallback
-- foreign import ccall safe "dynamic"
--   mK'TraceLogCallback ::
--     TraceLogCallback -> (CInt -> CString -> __builtin_va_list -> IO ())
type LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall safe "wrapper"
  mk'loadFileDataCallback ::
    (CString -> Ptr CUInt -> IO (Ptr CUChar)) -> IO LoadFileDataCallback

foreign import ccall safe "dynamic"
  mK'loadFileDataCallback ::
    LoadFileDataCallback -> (CString -> Ptr CUInt -> IO (Ptr CUChar))

type SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall safe "wrapper"
  mk'saveFileDataCallback ::
    (CString -> Ptr () -> CUInt -> IO CInt) -> IO SaveFileDataCallback

foreign import ccall safe "dynamic"
  mK'saveFileDataCallback ::
    SaveFileDataCallback -> (CString -> Ptr () -> CUInt -> IO CInt)

type LoadFileTextCallback = FunPtr (CString -> IO CString)

foreign import ccall safe "wrapper"
  mk'loadFileTextCallback ::
    (CString -> IO CString) -> IO LoadFileTextCallback

foreign import ccall safe "dynamic"
  mK'loadFileTextCallback ::
    LoadFileTextCallback -> (CString -> IO CString)

type SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)

foreign import ccall safe "wrapper"
  mk'saveFileTextCallback ::
    (CString -> CString -> IO CInt) -> IO SaveFileTextCallback

foreign import ccall safe "dynamic"
  mK'saveFileTextCallback ::
    SaveFileTextCallback -> (CString -> CString -> IO CInt)

foreign import ccall safe "raylib.h InitWindow"
  c'initWindow ::
    CInt -> CInt -> CString -> IO ()

initWindow :: Int -> Int -> String -> IO ()
initWindow width height title = withCString title $ c'initWindow (fromIntegral width) (fromIntegral height)

foreign import ccall safe "raylib.h &InitWindow"
  p'initWindow ::
    FunPtr (CInt -> CInt -> CString -> IO ())

foreign import ccall safe "raylib.h WindowShouldClose"
  c'windowShouldClose ::
    IO CBool

windowShouldClose :: IO Bool
windowShouldClose = toBool <$> c'windowShouldClose

foreign import ccall safe "raylib.h &WindowShouldClose"
  p'windowShouldClose ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h CloseWindow"
  closeWindow ::
    IO ()

foreign import ccall safe "raylib.h &CloseWindow"
  p'closeWindow ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h IsWindowReady"
  c'isWindowReady ::
    IO CBool

isWindowReady :: IO Bool
isWindowReady = toBool <$> c'isWindowReady

foreign import ccall safe "raylib.h &IsWindowReady"
  p'isWindowReady ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowFullscreen"
  c'isWindowFullscreen ::
    IO CBool

isWindowFullscreen :: IO Bool
isWindowFullscreen = toBool <$> c'isWindowFullscreen

foreign import ccall safe "raylib.h &IsWindowFullscreen"
  p'isWindowFullscreen ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowHidden"
  c'isWindowHidden ::
    IO CBool

isWindowHidden :: IO Bool
isWindowHidden = toBool <$> c'isWindowHidden

foreign import ccall safe "raylib.h &IsWindowHidden"
  p'isWindowHidden ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowMinimized"
  c'isWindowMinimized ::
    IO CBool

isWindowMinimized :: IO Bool
isWindowMinimized = toBool <$> c'isWindowMinimized

foreign import ccall safe "raylib.h &IsWindowMinimized"
  p'isWindowMinimized ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowMaximized"
  c'isWindowMaximized ::
    IO CBool

isWindowMaximized :: IO Bool
isWindowMaximized = toBool <$> c'isWindowMaximized

foreign import ccall safe "raylib.h &IsWindowMaximized"
  p'isWindowMaximized ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowFocused"
  c'isWindowFocused ::
    IO CBool

isWindowFocused :: IO Bool
isWindowFocused = toBool <$> c'isWindowFocused

foreign import ccall safe "raylib.h &IsWindowFocused"
  p'isWindowFocused ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowResized"
  c'isWindowResized ::
    IO CBool

isWindowResized :: IO Bool
isWindowResized = toBool <$> c'isWindowResized

foreign import ccall safe "raylib.h &IsWindowResized"
  p'isWindowResized ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowState"
  c'isWindowState ::
    CUInt -> IO CBool

isWindowState :: [ConfigFlag] -> IO Bool
isWindowState flags = toBool <$> c'isWindowState (fromIntegral $ configsToBitflag flags)

foreign import ccall safe "raylib.h &IsWindowState"
  p'isWindowState ::
    FunPtr (CUInt -> IO CInt)

foreign import ccall safe "raylib.h SetWindowState"
  c'setWindowState ::
    CUInt -> IO ()

setWindowState :: [ConfigFlag] -> IO ()
setWindowState = c'setWindowState . fromIntegral . configsToBitflag

foreign import ccall safe "raylib.h &SetWindowState"
  p'setWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h ClearWindowState"
  c'clearWindowState ::
    CUInt -> IO ()

clearWindowState :: [ConfigFlag] -> IO ()
clearWindowState = c'clearWindowState . fromIntegral . configsToBitflag

foreign import ccall safe "raylib.h &ClearWindowState"
  p'clearWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h ToggleFullscreen"
  toggleFullscreen ::
    IO ()

foreign import ccall safe "raylib.h &ToggleFullscreen"
  p'toggleFullscreen ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h MaximizeWindow"
  maximizeWindow ::
    IO ()

foreign import ccall safe "raylib.h &MaximizeWindow"
  p'maximizeWindow ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h MinimizeWindow"
  minimizeWindow ::
    IO ()

foreign import ccall safe "raylib.h &MinimizeWindow"
  p'minimizeWindow ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h RestoreWindow"
  restoreWindow ::
    IO ()

foreign import ccall safe "raylib.h &RestoreWindow"
  p'restoreWindow ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h SetWindowIcon_" c'setWindowIcon :: Ptr Raylib.Types.Image -> IO ()

setWindowIcon :: Raylib.Types.Image -> IO ()
setWindowIcon image = with image c'setWindowIcon

foreign import ccall safe "raylib.h &SetWindowIcon"
  p'setWindowIcon ::
    FunPtr (Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h SetWindowTitle"
  c'setWindowTitle ::
    CString -> IO ()

setWindowTitle :: String -> IO ()
setWindowTitle title = withCString title c'setWindowTitle

foreign import ccall safe "raylib.h &SetWindowTitle"
  p'setWindowTitle ::
    FunPtr (CString -> IO ())

foreign import ccall safe "raylib.h SetWindowPosition"
  c'setWindowPosition ::
    CInt -> CInt -> IO ()

setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = c'setWindowPosition (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &SetWindowPosition"
  p'setWindowPosition ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h SetWindowMonitor"
  c'setWindowMonitor ::
    CInt -> IO ()

setWindowMonitor :: Int -> IO ()
setWindowMonitor = c'setWindowMonitor . fromIntegral

foreign import ccall safe "raylib.h &SetWindowMonitor"
  p'setWindowMonitor ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h SetWindowMinSize"
  c'setWindowMinSize ::
    CInt -> CInt -> IO ()

setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize x y = c'setWindowMinSize (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &SetWindowMinSize"
  p'setWindowMinSize ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h SetWindowSize"
  c'setWindowSize ::
    CInt -> CInt -> IO ()

setWindowSize :: Int -> Int -> IO ()
setWindowSize x y = c'setWindowSize (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &SetWindowSize"
  p'setWindowSize ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h SetWindowOpacity"
  c'setWindowOpacity ::
    CFloat -> IO ()

setWindowOpacity :: Float -> IO ()
setWindowOpacity opacity = c'setWindowOpacity $ realToFrac opacity

foreign import ccall safe "raylib.h &SetWindowOpacity"
  p'setWindowOpacity ::
    FunPtr (CFloat -> IO ())

foreign import ccall safe "raylib.h GetWindowHandle"
  getWindowHandle ::
    IO (Ptr ())

foreign import ccall safe "raylib.h &GetWindowHandle"
  p'getWindowHandle ::
    FunPtr (IO (Ptr ()))

foreign import ccall safe "raylib.h GetScreenWidth"
  c'getScreenWidth ::
    IO CInt

getScreenWidth :: IO Int
getScreenWidth = fromIntegral <$> c'getScreenWidth

foreign import ccall safe "raylib.h &GetScreenWidth"
  p'getScreenWidth ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetScreenHeight"
  c'getScreenHeight ::
    IO CInt

getScreenHeight :: IO Int
getScreenHeight = fromIntegral <$> c'getScreenHeight

foreign import ccall safe "raylib.h &GetScreenHeight"
  p'getScreenHeight ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetRenderWidth"
  c'getRenderWidth ::
    IO CInt

getRenderWidth :: IO Int
getRenderWidth = fromIntegral <$> c'getRenderWidth

foreign import ccall safe "raylib.h &GetRenderWidth"
  p'getRenderWidth ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetRenderHeight"
  c'getRenderHeight ::
    IO CInt

getRenderHeight :: IO Int
getRenderHeight = fromIntegral <$> c'getRenderHeight

foreign import ccall safe "raylib.h &GetRenderHeight"
  p'getRenderHeight ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetMonitorCount"
  c'getMonitorCount ::
    IO CInt

getMonitorCount :: IO Int
getMonitorCount = fromIntegral <$> c'getMonitorCount

foreign import ccall safe "raylib.h &GetMonitorCount"
  p'getMonitorCount ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetCurrentMonitor"
  c'getCurrentMonitor ::
    IO CInt

getCurrentMonitor :: IO Int
getCurrentMonitor = fromIntegral <$> c'getCurrentMonitor

foreign import ccall safe "raylib.h &GetCurrentMonitor"
  p'getCurrentMonitor ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h GetMonitorPosition_" c'getMonitorPosition :: CInt -> IO (Ptr Raylib.Types.Vector2)

getMonitorPosition :: Int -> IO Raylib.Types.Vector2
getMonitorPosition monitor = c'getMonitorPosition (fromIntegral monitor) >>= pop

foreign import ccall safe "raylib.h &GetMonitorPosition"
  p'getMonitorPosition ::
    FunPtr (CInt -> IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetMonitorWidth"
  c'getMonitorWidth ::
    CInt -> IO CInt

getMonitorWidth :: Int -> IO Int
getMonitorWidth monitor = fromIntegral <$> c'getMonitorWidth (fromIntegral monitor)

foreign import ccall safe "raylib.h &GetMonitorWidth"
  p'getMonitorWidth ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetMonitorHeight"
  c'getMonitorHeight ::
    CInt -> IO CInt

getMonitorHeight :: Int -> IO Int
getMonitorHeight monitor = fromIntegral <$> c'getMonitorHeight (fromIntegral monitor)

foreign import ccall safe "raylib.h &GetMonitorHeight"
  p'getMonitorHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetMonitorPhysicalWidth"
  c'getMonitorPhysicalWidth ::
    CInt -> IO CInt

getMonitorPhysicalWidth :: Int -> IO Int
getMonitorPhysicalWidth monitor = fromIntegral <$> c'getMonitorPhysicalWidth (fromIntegral monitor)

foreign import ccall safe "raylib.h &GetMonitorPhysicalWidth"
  p'getMonitorPhysicalWidth ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetMonitorPhysicalHeight"
  c'getMonitorPhysicalHeight ::
    CInt -> IO CInt

getMonitorPhysicalHeight :: Int -> IO Int
getMonitorPhysicalHeight monitor = fromIntegral <$> c'getMonitorPhysicalHeight (fromIntegral monitor)

foreign import ccall safe "raylib.h &GetMonitorPhysicalHeight"
  p'getMonitorPhysicalHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetMonitorRefreshRate"
  c'getMonitorRefreshRate ::
    CInt -> IO CInt

getMonitorRefreshRate :: Int -> IO Int
getMonitorRefreshRate monitor = fromIntegral <$> c'getMonitorRefreshRate (fromIntegral monitor)

foreign import ccall safe "raylib.h &GetMonitorRefreshRate"
  p'getMonitorRefreshRate ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "bindings.h GetWindowPosition_" c'getWindowPosition :: IO (Ptr Raylib.Types.Vector2)

getWindowPosition :: IO Raylib.Types.Vector2
getWindowPosition = c'getWindowPosition >>= pop

foreign import ccall safe "raylib.h &GetWindowPosition"
  p'getWindowPosition ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetWindowScaleDPI_" c'getWindowScaleDPI :: IO (Ptr Raylib.Types.Vector2)

getWindowScaleDPI :: IO Raylib.Types.Vector2
getWindowScaleDPI = c'getWindowScaleDPI >>= pop

foreign import ccall safe "raylib.h &GetWindowScaleDPI"
  p'getWindowScaleDPI ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetMonitorName"
  c'getMonitorName ::
    CInt -> IO CString

getMonitorName :: Int -> IO String
getMonitorName monitor = c'getMonitorName (fromIntegral monitor) >>= peekCString

foreign import ccall safe "raylib.h &GetMonitorName"
  p'getMonitorName ::
    FunPtr (CInt -> IO CString)

foreign import ccall safe "raylib.h SetClipboardText"
  c'setClipboardText ::
    CString -> IO ()

setClipboardText :: String -> IO ()
setClipboardText text = withCString text c'setClipboardText

foreign import ccall safe "raylib.h &SetClipboardText"
  p'setClipboardText ::
    FunPtr (CString -> IO ())

foreign import ccall safe "raylib.h GetClipboardText"
  c'getClipboardText ::
    IO CString

getClipboardText :: IO String
getClipboardText = c'getClipboardText >>= peekCString

foreign import ccall safe "raylib.h &GetClipboardText"
  p'getClipboardText ::
    FunPtr (IO CString)

foreign import ccall safe "raylib.h EnableEventWaiting"
  enableEventWaiting ::
    IO ()

foreign import ccall safe "raylib.h &EnableEventWaiting"
  p'enableEventWaiting ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h DisableEventWaiting"
  disableEventWaiting ::
    IO ()

foreign import ccall safe "raylib.h &DisableEventWaiting"
  p'disableEventWaiting ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h SwapScreenBuffer"
  swapScreenBuffer ::
    IO ()

foreign import ccall safe "raylib.h &SwapScreenBuffer"
  p'swapScreenBuffer ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h PollInputEvents"
  pollInputEvents ::
    IO ()

foreign import ccall safe "raylib.h &PollInputEvents"
  p'pollInputEvents ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h WaitTime"
  c'waitTime ::
    CDouble -> IO ()

waitTime :: Double -> IO ()
waitTime seconds = c'waitTime $ realToFrac seconds

foreign import ccall safe "raylib.h &WaitTime"
  p'waitTime ::
    FunPtr (CDouble -> IO ())

foreign import ccall safe "raylib.h ShowCursor"
  showCursor ::
    IO ()

foreign import ccall safe "raylib.h &ShowCursor"
  p'showCursor ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h HideCursor"
  hideCursor ::
    IO ()

foreign import ccall safe "raylib.h &HideCursor"
  p'hideCursor ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h IsCursorHidden"
  c'isCursorHidden ::
    IO CBool

isCursorHidden :: IO Bool
isCursorHidden = toBool <$> c'isCursorHidden

foreign import ccall safe "raylib.h &IsCursorHidden"
  p'isCursorHidden ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h EnableCursor"
  enableCursor ::
    IO ()

foreign import ccall safe "raylib.h &EnableCursor"
  p'enableCursor ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h DisableCursor"
  disableCursor ::
    IO ()

foreign import ccall safe "raylib.h &DisableCursor"
  p'disableCursor ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h IsCursorOnScreen"
  c'isCursorOnScreen ::
    IO CBool

isCursorOnScreen :: IO Bool
isCursorOnScreen = toBool <$> c'isCursorOnScreen

foreign import ccall safe "raylib.h &IsCursorOnScreen"
  p'isCursorOnScreen ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h ClearBackground_" c'clearBackground :: Ptr Raylib.Types.Color -> IO ()

clearBackground :: Raylib.Types.Color -> IO ()
clearBackground color = with color c'clearBackground

foreign import ccall safe "raylib.h &ClearBackground"
  p'clearBackground ::
    FunPtr (Raylib.Types.Color -> IO ())

foreign import ccall safe "raylib.h BeginDrawing"
  beginDrawing ::
    IO ()

foreign import ccall safe "raylib.h &BeginDrawing"
  p'beginDrawing ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h EndDrawing"
  endDrawing ::
    IO ()

foreign import ccall safe "raylib.h &EndDrawing"
  p'endDrawing ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginMode2D_" c'beginMode2D :: Ptr Raylib.Types.Camera2D -> IO ()

beginMode2D :: Raylib.Types.Camera2D -> IO ()
beginMode2D camera = with camera c'beginMode2D

foreign import ccall safe "raylib.h &BeginMode2D"
  p'beginMode2D ::
    FunPtr (Raylib.Types.Camera2D -> IO ())

foreign import ccall safe "raylib.h EndMode2D"
  endMode2D ::
    IO ()

foreign import ccall safe "raylib.h &EndMode2D"
  p'endMode2D ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginMode3D_" c'beginMode3D :: Ptr Raylib.Types.Camera3D -> IO ()

beginMode3D :: Raylib.Types.Camera3D -> IO ()
beginMode3D camera = with camera c'beginMode3D

foreign import ccall safe "raylib.h &BeginMode3D"
  p'beginMode3D ::
    FunPtr (Raylib.Types.Camera3D -> IO ())

foreign import ccall safe "raylib.h EndMode3D"
  endMode3D ::
    IO ()

foreign import ccall safe "raylib.h &EndMode3D"
  p'endMode3D ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginTextureMode_" c'beginTextureMode :: Ptr Raylib.Types.RenderTexture -> IO ()

beginTextureMode :: Raylib.Types.RenderTexture -> IO ()
beginTextureMode renderTexture = with renderTexture c'beginTextureMode

foreign import ccall safe "raylib.h &BeginTextureMode"
  p'beginTextureMode ::
    FunPtr (Raylib.Types.RenderTexture -> IO ())

foreign import ccall safe "raylib.h EndTextureMode"
  endTextureMode ::
    IO ()

foreign import ccall safe "raylib.h &EndTextureMode"
  p'endTextureMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginShaderMode_" c'beginShaderMode :: Ptr Raylib.Types.Shader -> IO ()

beginShaderMode :: Raylib.Types.Shader -> IO ()
beginShaderMode shader = with shader c'beginShaderMode

foreign import ccall safe "raylib.h &BeginShaderMode"
  p'beginShaderMode ::
    FunPtr (Raylib.Types.Shader -> IO ())

foreign import ccall safe "raylib.h EndShaderMode"
  endShaderMode ::
    IO ()

foreign import ccall safe "raylib.h &EndShaderMode"
  p'endShaderMode ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h BeginBlendMode"
  c'beginBlendMode ::
    CInt -> IO ()

beginBlendMode :: BlendMode -> IO ()
beginBlendMode = c'beginBlendMode . fromIntegral . fromEnum

foreign import ccall safe "raylib.h &BeginBlendMode"
  p'beginBlendMode ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h EndBlendMode"
  endBlendMode ::
    IO ()

foreign import ccall safe "raylib.h &EndBlendMode"
  p'endBlendMode ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h BeginScissorMode"
  c'beginScissorMode ::
    CInt -> CInt -> CInt -> CInt -> IO ()

beginScissorMode :: Int -> Int -> Int -> Int -> IO ()
beginScissorMode x y width height = c'beginScissorMode (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

foreign import ccall safe "raylib.h &BeginScissorMode"
  p'beginScissorMode ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h EndScissorMode"
  endScissorMode ::
    IO ()

foreign import ccall safe "raylib.h &EndScissorMode"
  p'endScissorMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginVrStereoMode_" c'beginVrStereoMode :: Ptr Raylib.Types.VrStereoConfig -> IO ()

beginVrStereoMode :: Raylib.Types.VrStereoConfig -> IO ()
beginVrStereoMode config = with config c'beginVrStereoMode

foreign import ccall safe "raylib.h &BeginVrStereoMode"
  p'beginVrStereoMode ::
    FunPtr (Raylib.Types.VrStereoConfig -> IO ())

foreign import ccall safe "raylib.h EndVrStereoMode"
  endVrStereoMode ::
    IO ()

foreign import ccall safe "raylib.h &EndVrStereoMode"
  p'endVrStereoMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h LoadVrStereoConfig_" c'loadVrStereoConfig :: Ptr Raylib.Types.VrDeviceInfo -> IO (Ptr Raylib.Types.VrStereoConfig)

loadVrStereoConfig :: Raylib.Types.VrDeviceInfo -> IO Raylib.Types.VrStereoConfig
loadVrStereoConfig deviceInfo = with deviceInfo c'loadVrStereoConfig >>= pop

foreign import ccall safe "raylib.h &LoadVrStereoConfig"
  p'loadVrStereoConfig ::
    FunPtr (Raylib.Types.VrDeviceInfo -> IO Raylib.Types.VrStereoConfig)

foreign import ccall safe "bindings.h UnloadVrStereoConfig_" c'unloadVrStereoConfig :: Ptr Raylib.Types.VrStereoConfig -> IO ()

unloadVrStereoConfig :: Raylib.Types.VrStereoConfig -> IO ()
unloadVrStereoConfig config = with config c'unloadVrStereoConfig

foreign import ccall safe "raylib.h &UnloadVrStereoConfig"
  p'unloadVrStereoConfig ::
    FunPtr (Raylib.Types.VrStereoConfig -> IO ())

foreign import ccall safe "bindings.h LoadShader_" c'loadShader :: CString -> CString -> IO (Ptr Raylib.Types.Shader)

loadShader :: Maybe String -> Maybe String -> IO Raylib.Types.Shader
loadShader vsFileName fsFileName = 
  withMaybeCString vsFileName (withMaybeCString fsFileName . c'loadShader) >>= pop

foreign import ccall safe "raylib.h &LoadShader"
  p'loadShader ::
    FunPtr (CString -> CString -> IO Raylib.Types.Shader)

foreign import ccall safe "bindings.h LoadShaderFromMemory_" c'loadShaderFromMemory :: CString -> CString -> IO (Ptr Raylib.Types.Shader)

loadShaderFromMemory :: Maybe String -> Maybe String -> IO Raylib.Types.Shader
loadShaderFromMemory vsCode fsCode = withMaybeCString vsCode (withMaybeCString fsCode . c'loadShaderFromMemory) >>= pop

foreign import ccall safe "raylib.h &LoadShaderFromMemory"
  p'loadShaderFromMemory ::
    FunPtr (CString -> CString -> IO Raylib.Types.Shader)

foreign import ccall safe "bindings.h GetShaderLocation_" c'getShaderLocation :: Ptr Raylib.Types.Shader -> CString -> IO CInt

getShaderLocation :: Raylib.Types.Shader -> String -> IO Int
getShaderLocation shader uniformName = fromIntegral <$> with shader (withCString uniformName . c'getShaderLocation)

foreign import ccall safe "raylib.h &GetShaderLocation"
  p'getShaderLocation ::
    FunPtr (Raylib.Types.Shader -> CString -> IO CInt)

foreign import ccall safe "bindings.h GetShaderLocationAttrib_" c'getShaderLocationAttrib :: Ptr Raylib.Types.Shader -> CString -> IO CInt

getShaderLocationAttrib :: Raylib.Types.Shader -> String -> IO Int
getShaderLocationAttrib shader attribName = fromIntegral <$> with shader (withCString attribName . c'getShaderLocationAttrib)

foreign import ccall safe "raylib.h &GetShaderLocationAttrib"
  p'getShaderLocationAttrib ::
    FunPtr (Raylib.Types.Shader -> CString -> IO CInt)

foreign import ccall safe "bindings.h SetShaderValue_" c'setShaderValue :: Ptr Raylib.Types.Shader -> CInt -> Ptr () -> CInt -> IO ()

setShaderValue :: Raylib.Types.Shader -> Int -> Ptr () -> ShaderUniformDataType -> IO ()
setShaderValue shader locIndex value uniformType = with shader (\s -> c'setShaderValue s (fromIntegral locIndex) value (fromIntegral $ fromEnum uniformType))

foreign import ccall safe "raylib.h &SetShaderValue"
  p'setShaderValue ::
    FunPtr (Raylib.Types.Shader -> CInt -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShaderValueV_" c'setShaderValueV :: Ptr Raylib.Types.Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()

setShaderValueV :: Raylib.Types.Shader -> Int -> Ptr () -> ShaderUniformDataType -> Int -> IO ()
setShaderValueV shader locIndex value uniformType count = with shader (\s -> c'setShaderValueV s (fromIntegral locIndex) value (fromIntegral $ fromEnum uniformType) (fromIntegral count))

foreign import ccall safe "raylib.h &SetShaderValueV"
  p'setShaderValueV ::
    FunPtr (Raylib.Types.Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShaderValueMatrix_" c'setShaderValueMatrix :: Ptr Raylib.Types.Shader -> CInt -> Ptr Raylib.Types.Matrix -> IO ()

setShaderValueMatrix :: Raylib.Types.Shader -> Int -> Raylib.Types.Matrix -> IO ()
setShaderValueMatrix shader locIndex mat = with shader (\s -> with mat (c'setShaderValueMatrix s (fromIntegral locIndex)))

foreign import ccall safe "raylib.h &SetShaderValueMatrix"
  p'setShaderValueMatrix ::
    FunPtr (Raylib.Types.Shader -> CInt -> Raylib.Types.Matrix -> IO ())

foreign import ccall safe "bindings.h SetShaderValueTexture_" c'setShaderValueTexture :: Ptr Raylib.Types.Shader -> CInt -> Ptr Raylib.Types.Texture -> IO ()

setShaderValueTexture :: Raylib.Types.Shader -> Int -> Raylib.Types.Texture -> IO ()
setShaderValueTexture shader locIndex tex = with shader (\s -> with tex (c'setShaderValueTexture s (fromIntegral locIndex)))

foreign import ccall safe "raylib.h &SetShaderValueTexture"
  p'setShaderValueTexture ::
    FunPtr (Raylib.Types.Shader -> CInt -> Raylib.Types.Texture -> IO ())

foreign import ccall safe "bindings.h UnloadShader_" c'unloadShader :: Ptr Raylib.Types.Shader -> IO ()

unloadShader :: Raylib.Types.Shader -> IO ()
unloadShader shader = with shader c'unloadShader

foreign import ccall safe "raylib.h &UnloadShader"
  p'unloadShader ::
    FunPtr (Raylib.Types.Shader -> IO ())

foreign import ccall safe "bindings.h GetMouseRay_" c'getMouseRay :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Camera3D -> IO (Ptr Raylib.Types.Ray)

getMouseRay :: Raylib.Types.Vector2 -> Raylib.Types.Camera3D -> IO Raylib.Types.Ray
getMouseRay mousePosition camera = with mousePosition (with camera . c'getMouseRay) >>= pop

foreign import ccall safe "raylib.h &GetMouseRay"
  p'getMouseRay ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Camera3D -> IO Raylib.Types.Ray)

foreign import ccall safe "bindings.h GetCameraMatrix_" c'getCameraMatrix :: Ptr Raylib.Types.Camera3D -> IO (Ptr Raylib.Types.Matrix)

getCameraMatrix :: Raylib.Types.Camera3D -> IO Raylib.Types.Matrix
getCameraMatrix camera = with camera c'getCameraMatrix >>= pop

foreign import ccall safe "raylib.h &GetCameraMatrix"
  p'getCameraMatrix ::
    FunPtr (Raylib.Types.Camera3D -> IO Raylib.Types.Matrix)

foreign import ccall safe "bindings.h GetCameraMatrix2D_" c'getCameraMatrix2D :: Ptr Raylib.Types.Camera2D -> IO (Ptr Raylib.Types.Matrix)

getCameraMatrix2D :: Raylib.Types.Camera2D -> IO Raylib.Types.Matrix
getCameraMatrix2D camera = with camera c'getCameraMatrix2D >>= pop

foreign import ccall safe "raylib.h &GetCameraMatrix2D"
  p'getCameraMatrix2D ::
    FunPtr (Raylib.Types.Camera2D -> IO Raylib.Types.Matrix)

foreign import ccall safe "bindings.h GetWorldToScreen_" c'getWorldToScreen :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Camera3D -> IO (Ptr Raylib.Types.Vector2)

getWorldToScreen :: Raylib.Types.Vector3 -> Raylib.Types.Camera3D -> IO Raylib.Types.Vector2
getWorldToScreen position camera = with position (with camera . c'getWorldToScreen) >>= pop

foreign import ccall safe "raylib.h &GetWorldToScreen"
  p'getWorldToScreen ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Camera3D -> IO Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetScreenToWorld2D_" c'getScreenToWorld2D :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Camera2D -> IO (Ptr Raylib.Types.Vector2)

getScreenToWorld2D :: Raylib.Types.Vector2 -> Raylib.Types.Camera2D -> IO Raylib.Types.Vector2
getScreenToWorld2D position camera = with position (with camera . c'getScreenToWorld2D) >>= pop

foreign import ccall safe "raylib.h &GetScreenToWorld2D"
  p'getScreenToWorld2D ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Camera2D -> IO Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetWorldToScreenEx_" c'getWorldToScreenEx :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Camera3D -> CInt -> CInt -> IO (Ptr Raylib.Types.Vector2)

getWorldToScreenEx :: Raylib.Types.Vector3 -> Raylib.Types.Camera3D -> Int -> Int -> IO Raylib.Types.Vector2
getWorldToScreenEx position camera width height = with position (\p -> with camera (\c -> c'getWorldToScreenEx p c (fromIntegral width) (fromIntegral height))) >>= pop

foreign import ccall safe "raylib.h &GetWorldToScreenEx"
  p'getWorldToScreenEx ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Camera3D -> CInt -> CInt -> IO Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetWorldToScreen2D_" c'getWorldToScreen2D :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Camera2D -> IO (Ptr Raylib.Types.Vector2)

getWorldToScreen2D :: Raylib.Types.Vector2 -> Raylib.Types.Camera2D -> IO Raylib.Types.Vector2
getWorldToScreen2D position camera = with position (with camera . c'getWorldToScreen2D) >>= pop

foreign import ccall safe "raylib.h &GetWorldToScreen2D"
  p'getWorldToScreen2D ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Camera2D -> IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h SetTargetFPS"
  c'setTargetFPS ::
    CInt -> IO ()

setTargetFPS :: Int -> IO ()
setTargetFPS fps = c'setTargetFPS $ fromIntegral fps

foreign import ccall safe "raylib.h &SetTargetFPS"
  p'setTargetFPS ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h GetFPS"
  c'getFPS ::
    IO CInt

getFPS :: IO Int
getFPS = fromIntegral <$> c'getFPS

foreign import ccall safe "raylib.h &GetFPS"
  p'getFPS ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetFrameTime"
  c'getFrameTime ::
    IO CFloat

getFrameTime :: IO Float
getFrameTime = realToFrac <$> c'getFrameTime

foreign import ccall safe "raylib.h &GetFrameTime"
  p'getFrameTime ::
    FunPtr (IO CFloat)

foreign import ccall safe "raylib.h GetTime"
  c'getTime ::
    IO CDouble

getTime :: IO Double
getTime = realToFrac <$> c'getTime

foreign import ccall safe "raylib.h &GetTime"
  p'getTime ::
    FunPtr (IO CDouble)

foreign import ccall safe "raylib.h GetRandomValue"
  c'getRandomValue ::
    CInt -> CInt -> IO CInt

getRandomValue :: Int -> Int -> IO Int
getRandomValue minVal maxVal = fromIntegral <$> c'getRandomValue (fromIntegral minVal) (fromIntegral maxVal)

foreign import ccall safe "raylib.h &GetRandomValue"
  p'getRandomValue ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h SetRandomSeed"
  c'setRandomSeed ::
    CUInt -> IO ()

setRandomSeed :: Integer -> IO ()
setRandomSeed seed = c'setRandomSeed $ fromIntegral seed

foreign import ccall safe "raylib.h &SetRandomSeed"
  p'setRandomSeed ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h TakeScreenshot"
  c'takeScreenshot ::
    CString -> IO ()

takeScreenshot :: String -> IO ()
takeScreenshot fileName = withCString fileName c'takeScreenshot

foreign import ccall safe "raylib.h &TakeScreenshot"
  p'takeScreenshot ::
    FunPtr (CString -> IO ())

foreign import ccall safe "raylib.h SetConfigFlags"
  c'setConfigFlags ::
    CUInt -> IO ()

setConfigFlags :: [ConfigFlag] -> IO ()
setConfigFlags flags = c'setConfigFlags $ fromIntegral $ configsToBitflag flags

foreign import ccall safe "raylib.h &SetConfigFlags"
  p'setConfigFlags ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h TraceLog"
  c'traceLog ::
    CInt -> CString -> IO () -- Uses varags, can't implement complete functionality

traceLog :: TraceLogLevel -> String -> IO ()
traceLog logLevel text = withCString text $ c'traceLog $ fromIntegral $ fromEnum logLevel

foreign import ccall safe "raylib.h &TraceLog"
  p'traceLog ::
    FunPtr (CInt -> CString -> IO ())

foreign import ccall safe "raylib.h SetTraceLogLevel"
  c'setTraceLogLevel ::
    CInt -> IO ()

setTraceLogLevel :: TraceLogLevel -> IO ()
setTraceLogLevel = c'setTraceLogLevel . fromIntegral . fromEnum

foreign import ccall safe "raylib.h &SetTraceLogLevel"
  p'setTraceLogLevel ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h MemAlloc"
  c'memAlloc ::
    CInt -> IO (Ptr ())

memAlloc :: (Storable a) => Int -> IO (Ptr a)
memAlloc size = castPtr <$> c'memAlloc (fromIntegral size)

foreign import ccall safe "raylib.h &MemAlloc"
  p'memAlloc ::
    FunPtr (CInt -> IO (Ptr ()))

foreign import ccall safe "raylib.h MemRealloc"
  c'memRealloc ::
    Ptr () -> CInt -> IO (Ptr ())

memRealloc :: (Storable a, Storable b) => Ptr a -> Int -> IO (Ptr b)
memRealloc ptr size = castPtr <$> c'memRealloc (castPtr ptr) (fromIntegral size)

foreign import ccall safe "raylib.h &MemRealloc"
  p'memRealloc ::
    FunPtr (Ptr () -> CInt -> IO (Ptr ()))

foreign import ccall safe "raylib.h MemFree"
  c'memFree ::
    Ptr () -> IO ()

memFree :: (Storable a) => Ptr a -> IO ()
memFree = c'memFree . castPtr

foreign import ccall safe "raylib.h &MemFree"
  p'memFree ::
    FunPtr (Ptr () -> IO ())

foreign import ccall safe "raylib.h OpenURL"
  c'openURL ::
    CString -> IO ()

openURL :: String -> IO ()
openURL url = withCString url c'openURL

foreign import ccall safe "raylib.h &OpenURL"
  p'openURL ::
    FunPtr (CString -> IO ())

-- These functions use varargs so they can't be implemented through FFI
-- foreign import ccall safe "raylib.h SetTraceLogCallback"
--   SetTraceLogCallback ::
--     TraceLogCallback -> IO ()
-- foreign import ccall safe "raylib.h &SetTraceLogCallback"
--   p'SetTraceLogCallback ::
--     FunPtr (TraceLogCallback -> IO ())

foreign import ccall safe "raylib.h SetLoadFileDataCallback"
  setLoadFileDataCallback ::
    LoadFileDataCallback -> IO ()

foreign import ccall safe "raylib.h &SetLoadFileDataCallback"
  p'setLoadFileDataCallback ::
    FunPtr (LoadFileDataCallback -> IO ())

foreign import ccall safe "raylib.h SetSaveFileDataCallback"
  setSaveFileDataCallback ::
    SaveFileDataCallback -> IO ()

foreign import ccall safe "raylib.h &SetSaveFileDataCallback"
  p'setSaveFileDataCallback ::
    FunPtr (SaveFileDataCallback -> IO ())

foreign import ccall safe "raylib.h SetLoadFileTextCallback"
  setLoadFileTextCallback ::
    LoadFileTextCallback -> IO ()

foreign import ccall safe "raylib.h &SetLoadFileTextCallback"
  p'setLoadFileTextCallback ::
    FunPtr (LoadFileTextCallback -> IO ())

foreign import ccall safe "raylib.h SetSaveFileTextCallback"
  setSaveFileTextCallback ::
    SaveFileTextCallback -> IO ()

foreign import ccall safe "raylib.h &SetSaveFileTextCallback"
  p'setSaveFileTextCallback ::
    FunPtr (SaveFileTextCallback -> IO ())

foreign import ccall safe "raylib.h LoadFileData"
  c'loadFileData ::
    CString -> Ptr CUInt -> IO (Ptr CUChar)

loadFileData :: String -> IO [Integer]
loadFileData fileName =
  with
    0
    ( \size -> do
        withCString
          fileName
          ( \path -> do
              ptr <- c'loadFileData path size
              arrSize <- fromIntegral <$> peek size
              arr <- peekArray arrSize ptr
              c'unloadFileData ptr
              return $ map fromIntegral arr
          )
    )

foreign import ccall safe "raylib.h &LoadFileData"
  p'loadFileData ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h UnloadFileData"
  c'unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall safe "raylib.h &UnloadFileData"
  p'unloadFileData ::
    FunPtr (Ptr CUChar -> IO ())

foreign import ccall safe "raylib.h SaveFileData"
  c'saveFileData ::
    CString -> Ptr () -> CUInt -> IO CBool

saveFileData :: (Storable a) => String -> Ptr a -> Integer -> IO Bool
saveFileData fileName contents bytesToWrite =
  toBool <$> withCString fileName (\s -> c'saveFileData s (castPtr contents) (fromIntegral bytesToWrite))

foreign import ccall safe "raylib.h &SaveFileData"
  p'saveFileData ::
    FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall safe "raylib.h ExportDataAsCode"
  c'exportDataAsCode ::
    Ptr CUChar -> CUInt -> CString -> IO CBool

exportDataAsCode :: [Integer] -> Integer -> String -> IO Bool
exportDataAsCode contents size fileName =
  toBool <$> withArray (map fromInteger contents) (\c -> withCString fileName (c'exportDataAsCode c (fromIntegral size)))

foreign import ccall safe "raylib.h &ExportDataAsCode"
  p'exportDataAsCode ::
    FunPtr (CString -> CUInt -> CString -> IO CInt)

foreign import ccall safe "raylib.h LoadFileText"
  c'loadFileText ::
    CString -> IO CString

loadFileText :: String -> IO String
loadFileText fileName = withCString fileName c'loadFileText >>= peekCString

foreign import ccall safe "raylib.h &LoadFileText"
  p'loadFileText ::
    FunPtr (CString -> IO CString)

foreign import ccall safe "raylib.h UnloadFileText"
  c'unloadFileText ::
    CString -> IO ()

unloadFileText :: String -> IO ()
unloadFileText text = withCString text c'unloadFileText

foreign import ccall safe "raylib.h &UnloadFileText"
  p'unloadFileText ::
    FunPtr (CString -> IO ())

foreign import ccall safe "raylib.h SaveFileText"
  c'saveFileText ::
    CString -> CString -> IO CBool

saveFileText :: String -> String -> IO Bool
saveFileText fileName text = toBool <$> withCString fileName (withCString text . c'saveFileText)

foreign import ccall safe "raylib.h &SaveFileText"
  p'saveFileText ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall safe "raylib.h FileExists"
  c'fileExists ::
    CString -> IO CBool

fileExists :: String -> IO Bool
fileExists fileName = toBool <$> withCString fileName c'fileExists

foreign import ccall safe "raylib.h &FileExists"
  p'fileExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h DirectoryExists"
  c'directoryExists ::
    CString -> IO CBool

directoryExists :: String -> IO Bool
directoryExists dirPath = toBool <$> withCString dirPath c'directoryExists

foreign import ccall safe "raylib.h &DirectoryExists"
  p'directoryExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h IsFileExtension"
  c'isFileExtension ::
    CString -> CString -> IO CBool

isFileExtension :: String -> String -> IO Bool
isFileExtension fileName ext = toBool <$> withCString fileName (withCString ext . c'isFileExtension)

foreign import ccall safe "raylib.h &IsFileExtension"
  p'isFileExtension ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall safe "raylib.h GetFileLength"
  c'getFileLength ::
    CString -> IO CBool

getFileLength :: String -> IO Bool
getFileLength fileName = toBool <$> withCString fileName c'getFileLength

foreign import ccall safe "raylib.h &GetFileLength"
  p'getFileLength ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h GetFileExtension"
  c'getFileExtension ::
    CString -> IO CString

getFileExtension :: String -> IO String
getFileExtension fileName = withCString fileName c'getFileExtension >>= peekCString

foreign import ccall safe "raylib.h &GetFileExtension"
  p'getFileExtension ::
    FunPtr (CString -> IO CString)

foreign import ccall safe "raylib.h GetFileName"
  c'getFileName ::
    CString -> IO CString

getFileName :: String -> IO String
getFileName filePath = withCString filePath c'getFileName >>= peekCString

foreign import ccall safe "raylib.h &GetFileName"
  p'getFileName ::
    FunPtr (CString -> IO CString)

foreign import ccall safe "raylib.h GetFileNameWithoutExt"
  c'getFileNameWithoutExt ::
    CString -> IO CString

getFileNameWithoutExt :: String -> IO String
getFileNameWithoutExt fileName = withCString fileName c'getFileNameWithoutExt >>= peekCString

foreign import ccall safe "raylib.h &GetFileNameWithoutExt"
  p'getFileNameWithoutExt ::
    FunPtr (CString -> IO CString)

foreign import ccall safe "raylib.h GetDirectoryPath"
  c'getDirectoryPath ::
    CString -> IO CString

getDirectoryPath :: String -> IO String
getDirectoryPath filePath = withCString filePath c'getDirectoryPath >>= peekCString

foreign import ccall safe "raylib.h &GetDirectoryPath"
  p'getDirectoryPath ::
    FunPtr (CString -> IO CString)

foreign import ccall safe "raylib.h GetPrevDirectoryPath"
  c'getPrevDirectoryPath ::
    CString -> IO CString

getPrevDirectoryPath :: String -> IO String
getPrevDirectoryPath dirPath = withCString dirPath c'getPrevDirectoryPath >>= peekCString

foreign import ccall safe "raylib.h &GetPrevDirectoryPath"
  p'getPrevDirectoryPath ::
    FunPtr (CString -> IO CString)

foreign import ccall safe "raylib.h GetWorkingDirectory"
  c'getWorkingDirectory ::
    IO CString

getWorkingDirectory :: IO String
getWorkingDirectory = c'getWorkingDirectory >>= peekCString

foreign import ccall safe "raylib.h &GetWorkingDirectory"
  p'getWorkingDirectory ::
    FunPtr (IO CString)

foreign import ccall safe "raylib.h GetApplicationDirectory"
  c'getApplicationDirectory ::
    IO CString

getApplicationDirectory :: IO String
getApplicationDirectory = c'getApplicationDirectory >>= peekCString

foreign import ccall safe "raylib.h &GetApplicationDirectory"
  p'getApplicationDirectory ::
    FunPtr (IO CString)

foreign import ccall safe "raylib.h ChangeDirectory"
  c'changeDirectory ::
    CString -> IO CBool

changeDirectory :: String -> IO Bool
changeDirectory dir = toBool <$> withCString dir c'changeDirectory

foreign import ccall safe "raylib.h &ChangeDirectory"
  p'changeDirectory ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h IsPathFile"
  c'isPathFile ::
    CString -> IO CBool

isPathFile :: String -> IO Bool
isPathFile path = toBool <$> withCString path c'isPathFile

foreign import ccall safe "raylib.h &IsPathFile"
  p'isPathFile ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "bindings.h LoadDirectoryFiles_" c'loadDirectoryFiles :: CString -> IO (Ptr Raylib.Types.FilePathList)

loadDirectoryFiles :: String -> IO Raylib.Types.FilePathList
loadDirectoryFiles dirPath = withCString dirPath c'loadDirectoryFiles >>= pop

foreign import ccall safe "raylib.h &LoadDirectoryFiles"
  p'loadDirectoryFiles ::
    FunPtr (CString -> IO Raylib.Types.FilePathList)

foreign import ccall safe "bindings.h LoadDirectoryFilesEx_" c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr Raylib.Types.FilePathList)

loadDirectoryFilesEx :: String -> String -> Bool -> IO Raylib.Types.FilePathList
loadDirectoryFilesEx basePath filterStr scanSubdirs =
  withCString basePath (\b -> withCString filterStr (\f -> c'loadDirectoryFilesEx b f (fromBool scanSubdirs))) >>= pop

foreign import ccall safe "raylib.h &LoadDirectoryFilesEx"
  p'loadDirectoryFilesEx ::
    FunPtr (CString -> CString -> CInt -> IO Raylib.Types.FilePathList)

foreign import ccall safe "bindings.h UnloadDirectoryFiles_" c'unloadDirectoryFiles :: Ptr Raylib.Types.FilePathList -> IO ()

unloadDirectoryFiles :: Raylib.Types.FilePathList -> IO ()
unloadDirectoryFiles files = with files c'unloadDirectoryFiles

foreign import ccall safe "raylib.h &UnloadDirectoryFiles"
  p'unloadDirectoryFiles ::
    FunPtr (Raylib.Types.FilePathList -> IO ())

foreign import ccall safe "raylib.h IsFileDropped"
  c'isFileDropped ::
    IO CBool

isFileDropped :: IO Bool
isFileDropped = toBool <$> c'isFileDropped

foreign import ccall safe "raylib.h &IsFileDropped"
  p'isFileDropped ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h LoadDroppedFiles_" c'loadDroppedFiles :: IO (Ptr Raylib.Types.FilePathList)

loadDroppedFiles :: IO Raylib.Types.FilePathList
loadDroppedFiles = c'loadDroppedFiles >>= pop

foreign import ccall safe "raylib.h &LoadDroppedFiles"
  p'loadDroppedFiles ::
    FunPtr (IO Raylib.Types.FilePathList)

foreign import ccall safe "bindings.h UnloadDroppedFiles_" c'unloadDroppedFiles :: Ptr Raylib.Types.FilePathList -> IO ()

unloadDroppedFiles :: Raylib.Types.FilePathList -> IO ()
unloadDroppedFiles files = with files c'unloadDroppedFiles

foreign import ccall safe "raylib.h &UnloadDroppedFiles"
  p'unloadDroppedFiles ::
    FunPtr (Raylib.Types.FilePathList -> IO ())

foreign import ccall safe "raylib.h GetFileModTime"
  c'getFileModTime ::
    CString -> IO CLong

getFileModTime :: String -> IO Integer
getFileModTime fileName = fromIntegral <$> withCString fileName c'getFileModTime

foreign import ccall safe "raylib.h &GetFileModTime"
  p'getFileModTime ::
    FunPtr (CString -> IO CLong)

foreign import ccall safe "raylib.h CompressData"
  c'compressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

compressData :: [Integer] -> IO [Integer]
compressData contents = do
  withArrayLen
    (map fromIntegral contents)
    ( \size c -> do
        with
          0
          ( \ptr -> do
              compressed <- c'compressData c (fromIntegral $ size * sizeOf (0 :: CUChar)) ptr
              compressedSize <- fromIntegral <$> peek ptr
              arr <- peekArray compressedSize compressed
              return $ map fromIntegral arr
          )
    )

foreign import ccall safe "raylib.h &CompressData"
  p'compressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h DecompressData"
  c'decompressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

decompressData :: [Integer] -> IO [Integer]
decompressData compressedData = do
  withArrayLen
    (map fromIntegral compressedData)
    ( \size c -> do
        with
          0
          ( \ptr -> do
              decompressed <- c'decompressData c (fromIntegral $ size * sizeOf (0 :: CUChar)) ptr
              decompressedSize <- fromIntegral <$> peek ptr
              arr <- peekArray decompressedSize decompressed
              return $ map fromIntegral arr
          )
    )

foreign import ccall safe "raylib.h &DecompressData"
  p'decompressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h EncodeDataBase64"
  c'encodeDataBase64 ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO CString

encodeDataBase64 :: [Integer] -> IO [Integer]
encodeDataBase64 contents = do
  withArrayLen
    (map fromIntegral contents)
    ( \size c -> do
        with
          0
          ( \ptr -> do
              encoded <- c'encodeDataBase64 c (fromIntegral $ size * sizeOf (0 :: CUChar)) ptr
              encodedSize <- fromIntegral <$> peek ptr
              arr <- peekArray encodedSize encoded
              return $ map fromIntegral arr
          )
    )

foreign import ccall safe "raylib.h &EncodeDataBase64"
  p'encodeDataBase64 ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO CString)

foreign import ccall safe "raylib.h DecodeDataBase64"
  c'decodeDataBase64 ::
    Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)

decodeDataBase64 :: [Integer] -> IO [Integer]
decodeDataBase64 encodedData = do
  withArray
    (map fromIntegral encodedData)
    ( \c -> do
        with
          0
          ( \ptr -> do
              decoded <- c'decodeDataBase64 c ptr
              decodedSize <- fromIntegral <$> peek ptr
              arr <- peekArray decodedSize decoded
              return $ map fromIntegral arr
          )
    )

foreign import ccall safe "raylib.h &DecodeDataBase64"
  p'decodeDataBase64 ::
    FunPtr (Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h IsKeyPressed"
  c'isKeyPressed ::
    CInt -> IO CBool

isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed key = toBool <$> c'isKeyPressed (fromIntegral $ fromEnum key)

foreign import ccall safe "raylib.h &IsKeyPressed"
  p'isKeyPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsKeyDown"
  c'isKeyDown ::
    CInt -> IO CBool

isKeyDown :: KeyboardKey -> IO Bool
isKeyDown key = toBool <$> c'isKeyDown (fromIntegral $ fromEnum key)

foreign import ccall safe "raylib.h &IsKeyDown"
  p'isKeyDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsKeyReleased"
  c'isKeyReleased ::
    CInt -> IO CBool

isKeyReleased :: KeyboardKey -> IO Bool
isKeyReleased key = toBool <$> c'isKeyReleased (fromIntegral $ fromEnum key)

foreign import ccall safe "raylib.h &IsKeyReleased"
  p'isKeyReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsKeyUp"
  c'isKeyUp ::
    CInt -> IO CBool

isKeyUp :: KeyboardKey -> IO Bool
isKeyUp key = toBool <$> c'isKeyUp (fromIntegral $ fromEnum key)

foreign import ccall safe "raylib.h &IsKeyUp"
  p'isKeyUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h SetExitKey"
  c'setExitKey ::
    CInt -> IO ()

setExitKey :: KeyboardKey -> IO ()
setExitKey = c'setExitKey . fromIntegral . fromEnum

foreign import ccall safe "raylib.h &SetExitKey"
  p'setExitKey ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h GetKeyPressed"
  c'getKeyPressed ::
    IO CInt

getKeyPressed :: IO KeyboardKey
getKeyPressed = toEnum . fromIntegral <$> c'getKeyPressed

foreign import ccall safe "raylib.h &GetKeyPressed"
  p'getKeyPressed ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetCharPressed"
  c'getCharPressed ::
    IO CInt

getCharPressed :: IO Int
getCharPressed = fromIntegral <$> c'getCharPressed

foreign import ccall safe "raylib.h &GetCharPressed"
  p'getCharPressed ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsGamepadAvailable"
  c'isGamepadAvailable ::
    CInt -> IO CBool

isGamepadAvailable :: Int -> IO Bool
isGamepadAvailable gamepad = toBool <$> c'isGamepadAvailable (fromIntegral gamepad)

foreign import ccall safe "raylib.h &IsGamepadAvailable"
  p'isGamepadAvailable ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetGamepadName"
  c'getGamepadName ::
    CInt -> IO CString

getGamepadName :: Int -> IO String
getGamepadName gamepad = c'getGamepadName (fromIntegral gamepad) >>= peekCString

foreign import ccall safe "raylib.h &GetGamepadName"
  p'getGamepadName ::
    FunPtr (CInt -> IO CString)

foreign import ccall safe "raylib.h IsGamepadButtonPressed"
  c'isGamepadButtonPressed ::
    CInt -> CInt -> IO CBool

isGamepadButtonPressed :: Int -> GamepadButton -> IO Bool
isGamepadButtonPressed gamepad button = toBool <$> c'isGamepadButtonPressed (fromIntegral gamepad) (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsGamepadButtonPressed"
  p'isGamepadButtonPressed ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h IsGamepadButtonDown"
  c'isGamepadButtonDown ::
    CInt -> CInt -> IO CBool

isGamepadButtonDown :: Int -> GamepadButton -> IO Bool
isGamepadButtonDown gamepad button = toBool <$> c'isGamepadButtonDown (fromIntegral gamepad) (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsGamepadButtonDown"
  p'isGamepadButtonDown ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h IsGamepadButtonReleased"
  c'isGamepadButtonReleased ::
    CInt -> CInt -> IO CBool

isGamepadButtonReleased :: Int -> GamepadButton -> IO Bool
isGamepadButtonReleased gamepad button = toBool <$> c'isGamepadButtonReleased (fromIntegral gamepad) (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsGamepadButtonReleased"
  p'isGamepadButtonReleased ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h IsGamepadButtonUp"
  c'isGamepadButtonUp ::
    CInt -> CInt -> IO CBool

isGamepadButtonUp :: Int -> GamepadButton -> IO Bool
isGamepadButtonUp gamepad button = toBool <$> c'isGamepadButtonUp (fromIntegral gamepad) (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsGamepadButtonUp"
  p'isGamepadButtonUp ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h GetGamepadButtonPressed"
  c'getGamepadButtonPressed ::
    IO CInt

getGamepadButtonPressed :: IO GamepadButton
getGamepadButtonPressed = toEnum . fromIntegral <$> c'getGamepadButtonPressed

foreign import ccall safe "raylib.h &GetGamepadButtonPressed"
  p'getGamepadButtonPressed ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetGamepadAxisCount"
  c'getGamepadAxisCount ::
    CInt -> IO CInt

getGamepadAxisCount :: Int -> IO Int
getGamepadAxisCount gamepad = fromIntegral <$> c'getGamepadAxisCount (fromIntegral gamepad)

foreign import ccall safe "raylib.h &GetGamepadAxisCount"
  p'getGamepadAxisCount ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetGamepadAxisMovement"
  c'getGamepadAxisMovement ::
    CInt -> CInt -> IO CFloat

getGamepadAxisMovement :: Int -> GamepadAxis -> IO Float
getGamepadAxisMovement gamepad axis = realToFrac <$> c'getGamepadAxisMovement (fromIntegral gamepad) (fromIntegral $ fromEnum axis)

foreign import ccall safe "raylib.h &GetGamepadAxisMovement"
  p'getGamepadAxisMovement ::
    FunPtr (CInt -> CInt -> IO CFloat)

foreign import ccall safe "raylib.h SetGamepadMappings"
  c'setGamepadMappings ::
    CString -> IO CInt

setGamepadMappings :: String -> IO Int
setGamepadMappings mappings = fromIntegral <$> withCString mappings c'setGamepadMappings

foreign import ccall safe "raylib.h &SetGamepadMappings"
  p'setGamepadMappings ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonPressed"
  c'isMouseButtonPressed ::
    CInt -> IO CBool

isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed button = toBool <$> c'isMouseButtonPressed (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsMouseButtonPressed"
  p'isMouseButtonPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonDown"
  c'isMouseButtonDown ::
    CInt -> IO CBool

isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown button = toBool <$> c'isMouseButtonDown (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsMouseButtonDown"
  p'isMouseButtonDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonReleased"
  c'isMouseButtonReleased ::
    CInt -> IO CBool

isMouseButtonReleased :: MouseButton -> IO Bool
isMouseButtonReleased button = toBool <$> c'isMouseButtonReleased (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsMouseButtonReleased"
  p'isMouseButtonReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonUp"
  c'isMouseButtonUp ::
    CInt -> IO CBool

isMouseButtonUp :: MouseButton -> IO Bool
isMouseButtonUp button = toBool <$> c'isMouseButtonUp (fromIntegral $ fromEnum button)

foreign import ccall safe "raylib.h &IsMouseButtonUp"
  p'isMouseButtonUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetMouseX"
  c'getMouseX ::
    IO CInt

getMouseX :: IO Int
getMouseX = fromIntegral <$> c'getMouseX

foreign import ccall safe "raylib.h &GetMouseX"
  p'getMouseX ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetMouseY"
  c'getMouseY ::
    IO CInt

getMouseY :: IO Int
getMouseY = fromIntegral <$> c'getMouseY

foreign import ccall safe "raylib.h &GetMouseY"
  p'getMouseY ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h GetMousePosition_" c'getMousePosition :: IO (Ptr Raylib.Types.Vector2)

getMousePosition :: IO Raylib.Types.Vector2
getMousePosition = c'getMousePosition >>= pop

foreign import ccall safe "raylib.h &GetMousePosition"
  p'getMousePosition ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetMouseDelta_" c'getMouseDelta :: IO (Ptr Raylib.Types.Vector2)

getMouseDelta :: IO Raylib.Types.Vector2
getMouseDelta = c'getMouseDelta >>= pop

foreign import ccall safe "raylib.h &GetMouseDelta"
  p'getMouseDelta ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h SetMousePosition"
  c'setMousePosition ::
    CInt -> CInt -> IO ()

setMousePosition :: Int -> Int -> IO ()
setMousePosition x y = c'setMousePosition (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &SetMousePosition"
  p'setMousePosition ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h SetMouseOffset"
  c'setMouseOffset ::
    CInt -> CInt -> IO ()

setMouseOffset :: Int -> Int -> IO ()
setMouseOffset x y = c'setMouseOffset (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &SetMouseOffset"
  p'setMouseOffset ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h SetMouseScale"
  c'setMouseScale ::
    CFloat -> CFloat -> IO ()

setMouseScale :: Float -> Float -> IO ()
setMouseScale x y = c'setMouseScale (realToFrac x) (realToFrac y)

foreign import ccall safe "raylib.h &SetMouseScale"
  p'setMouseScale ::
    FunPtr (CFloat -> CFloat -> IO ())

foreign import ccall safe "raylib.h GetMouseWheelMove"
  c'getMouseWheelMove ::
    IO CFloat

getMouseWheelMove :: IO Float
getMouseWheelMove = realToFrac <$> c'getMouseWheelMove

foreign import ccall safe "raylib.h &GetMouseWheelMove"
  p'getMouseWheelMove ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetMouseWheelMoveV_" c'getMouseWheelMoveV :: IO (Ptr Raylib.Types.Vector2)

getMouseWheelMoveV :: IO Raylib.Types.Vector2
getMouseWheelMoveV = c'getMouseWheelMoveV >>= pop

foreign import ccall safe "raylib.h &GetMouseWheelMoveV"
  p'getMouseWheelMoveV ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h SetMouseCursor"
  c'setMouseCursor ::
    CInt -> IO ()

setMouseCursor :: MouseCursor -> IO ()
setMouseCursor cursor = c'setMouseCursor . fromIntegral $ fromEnum cursor

foreign import ccall safe "raylib.h &SetMouseCursor"
  p'setMouseCursor ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h GetTouchX"
  c'getTouchX ::
    IO CInt

getTouchX :: IO Int
getTouchX = fromIntegral <$> c'getTouchX

foreign import ccall safe "raylib.h &GetTouchX"
  p'getTouchX ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetTouchY"
  c'getTouchY ::
    IO CInt

getTouchY :: IO Int
getTouchY = fromIntegral <$> c'getTouchY

foreign import ccall safe "raylib.h &GetTouchY"
  p'getTouchY ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h GetTouchPosition_" c'getTouchPosition :: CInt -> IO (Ptr Raylib.Types.Vector2)

getTouchPosition :: Int -> IO Raylib.Types.Vector2
getTouchPosition index = c'getTouchPosition (fromIntegral index) >>= pop

foreign import ccall safe "raylib.h &GetTouchPosition"
  p'getTouchPosition ::
    FunPtr (CInt -> IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetTouchPointId"
  c'getTouchPointId ::
    CInt -> IO CInt

getTouchPointId :: Int -> IO Int
getTouchPointId index = fromIntegral <$> c'getTouchPointId (fromIntegral index)

foreign import ccall safe "raylib.h &GetTouchPointId"
  p'getTouchPointId ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetTouchPointCount"
  c'getTouchPointCount ::
    IO CInt

getTouchPointCount :: IO Int
getTouchPointCount = fromIntegral <$> c'getTouchPointCount

foreign import ccall safe "raylib.h &GetTouchPointCount"
  p'getTouchPointCount ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h SetGesturesEnabled"
  c'setGesturesEnabled ::
    CUInt -> IO ()

setGesturesEnabled :: [Gesture] -> IO ()
setGesturesEnabled flags = c'setGesturesEnabled (fromIntegral $ configsToBitflag flags)

foreign import ccall safe "raylib.h &SetGesturesEnabled"
  p'setGesturesEnabled ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h IsGestureDetected"
  c'isGestureDetected ::
    CInt -> IO CBool

isGestureDetected :: Gesture -> IO Bool
isGestureDetected gesture = toBool <$> c'isGestureDetected (fromIntegral $ fromEnum gesture)

foreign import ccall safe "raylib.h &IsGestureDetected"
  p'isGestureDetected ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetGestureDetected"
  c'getGestureDetected ::
    IO CInt

getGestureDetected :: IO Gesture
getGestureDetected = toEnum . fromIntegral <$> c'getGestureDetected

foreign import ccall safe "raylib.h &GetGestureDetected"
  p'getGestureDetected ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h GetGestureHoldDuration"
  c'getGestureHoldDuration ::
    IO CFloat

getGestureHoldDuration :: IO Float
getGestureHoldDuration = realToFrac <$> c'getGestureHoldDuration

foreign import ccall safe "raylib.h &GetGestureHoldDuration"
  p'getGestureHoldDuration ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetGestureDragVector_" c'getGestureDragVector :: IO (Ptr Raylib.Types.Vector2)

getGestureDragVector :: IO Raylib.Types.Vector2
getGestureDragVector = c'getGestureDragVector >>= pop

foreign import ccall safe "raylib.h &GetGestureDragVector"
  p'getGestureDragVector ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetGestureDragAngle"
  c'getGestureDragAngle ::
    IO CFloat

getGestureDragAngle :: IO Float
getGestureDragAngle = realToFrac <$> c'getGestureDragAngle

foreign import ccall safe "raylib.h &GetGestureDragAngle"
  p'getGestureDragAngle ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetGesturePinchVector_" c'getGesturePinchVector :: IO (Ptr Raylib.Types.Vector2)

getGesturePinchVector :: IO Raylib.Types.Vector2
getGesturePinchVector = c'getGesturePinchVector >>= pop

foreign import ccall safe "raylib.h &GetGesturePinchVector"
  p'getGesturePinchVector ::
    FunPtr (IO Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetGesturePinchAngle"
  c'getGesturePinchAngle ::
    IO CFloat

getGesturePinchAngle :: IO Float
getGesturePinchAngle = realToFrac <$> c'getGesturePinchAngle

foreign import ccall safe "raylib.h &GetGesturePinchAngle"
  p'getGesturePinchAngle ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h SetCameraMode_" c'setCameraMode :: Ptr Raylib.Types.Camera3D -> CInt -> IO ()

setCameraMode :: Raylib.Types.Camera3D -> CameraMode -> IO ()
setCameraMode camera mode = with camera (\c -> c'setCameraMode c (fromIntegral $ fromEnum mode))

foreign import ccall safe "raylib.h &SetCameraMode"
  p'setCameraMode ::
    FunPtr (Raylib.Types.Camera3D -> CInt -> IO ())

foreign import ccall safe "raylib.h UpdateCamera"
  c'updateCamera ::
    Ptr Raylib.Types.Camera3D -> IO ()

updateCamera :: Raylib.Types.Camera3D -> IO Raylib.Types.Camera3D
updateCamera camera =
  with
    camera
    ( \c -> do
        c'updateCamera c
        peek c
    )

foreign import ccall safe "raylib.h &UpdateCamera"
  p'updateCamera ::
    FunPtr (Ptr Raylib.Types.Camera3D -> IO ())

foreign import ccall safe "raylib.h SetCameraPanControl"
  c'setCameraPanControl ::
    CInt -> IO ()

setCameraPanControl :: Int -> IO ()
setCameraPanControl keyPan = c'setCameraPanControl $ fromIntegral keyPan

foreign import ccall safe "raylib.h &SetCameraPanControl"
  p'setCameraPanControl ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h SetCameraAltControl"
  c'setCameraAltControl ::
    CInt -> IO ()

setCameraAltControl :: Int -> IO ()
setCameraAltControl keyAlt = c'setCameraAltControl $ fromIntegral keyAlt

foreign import ccall safe "raylib.h &SetCameraAltControl"
  p'setCameraAltControl ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h SetCameraSmoothZoomControl"
  c'setCameraSmoothZoomControl ::
    CInt -> IO ()

setCameraSmoothZoomControl :: Int -> IO ()
setCameraSmoothZoomControl keySmoothZoom = c'setCameraSmoothZoomControl $ fromIntegral keySmoothZoom

foreign import ccall safe "raylib.h &SetCameraSmoothZoomControl"
  p'setCameraSmoothZoomControl ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h SetCameraMoveControls"
  c'setCameraMoveControls ::
    CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

setCameraMoveControls :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
setCameraMoveControls keyFront keyBack keyRight keyLeft keyUp keyDown =
  c'setCameraMoveControls
    (fromIntegral keyFront)
    (fromIntegral keyBack)
    (fromIntegral keyRight)
    (fromIntegral keyLeft)
    (fromIntegral keyUp)
    (fromIntegral keyDown)

foreign import ccall safe "raylib.h &SetCameraMoveControls"
  p'setCameraMoveControls ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShapesTexture_" c'setShapesTexture :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> IO ()

setShapesTexture :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> IO ()
setShapesTexture tex source = with tex (with source . c'setShapesTexture)

foreign import ccall safe "raylib.h &SetShapesTexture"
  p'setShapesTexture ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.Rectangle -> IO ())

foreign import ccall safe "bindings.h DrawPixel_" c'drawPixel :: CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawPixel :: Int -> Int -> Raylib.Types.Color -> IO ()
drawPixel x y color = with color $ c'drawPixel (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &DrawPixel"
  p'drawPixel ::
    FunPtr (CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawPixelV_" c'drawPixelV :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawPixelV :: Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawPixelV position color = with position (with color . c'drawPixelV)

foreign import ccall safe "raylib.h &DrawPixelV"
  p'drawPixelV ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLine_" c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawLine :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawLine startX startY endX endY color =
  with color $ c'drawLine (fromIntegral startX) (fromIntegral startY) (fromIntegral endX) (fromIntegral endY)

foreign import ccall safe "raylib.h &DrawLine"
  p'drawLine ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLineV_" c'drawLineV :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawLineV :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawLineV start end color = with start (\s -> with end (with color . c'drawLineV s))

foreign import ccall safe "raylib.h &DrawLineV"
  p'drawLineV ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLineEx_" c'drawLineEx :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawLineEx :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineEx start end thickness color =
  with start (\s -> with end (\e -> with color (c'drawLineEx s e (realToFrac thickness))))

foreign import ccall safe "raylib.h &DrawLineEx"
  p'drawLineEx ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezier_" c'drawLineBezier :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawLineBezier :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineBezier start end thickness color =
  with start (\s -> with end (\e -> with color (c'drawLineBezier s e (realToFrac thickness))))

foreign import ccall safe "raylib.h &DrawLineBezier"
  p'drawLineBezier ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezierQuad_" c'drawLineBezierQuad :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawLineBezierQuad :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineBezierQuad start end control thickness color =
  with start (\s -> with end (\e -> with control (\c -> with color (c'drawLineBezierQuad s e c (realToFrac thickness)))))

foreign import ccall safe "raylib.h &DrawLineBezierQuad"
  p'drawLineBezierQuad ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezierCubic_" c'drawLineBezierCubic :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawLineBezierCubic :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawLineBezierCubic start end startControl endControl thickness color =
  with
    start
    ( \s ->
        with
          end
          ( \e ->
              with
                startControl
                ( \sc ->
                    with
                      endControl
                      ( \ec ->
                          with
                            color
                            ( c'drawLineBezierCubic s e sc ec (realToFrac thickness)
                            )
                      )
                )
          )
    )

foreign import ccall safe "raylib.h &DrawLineBezierCubic"
  p'drawLineBezierCubic ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawLineStrip_" c'drawLineStrip :: Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawLineStrip :: [Raylib.Types.Vector2] -> Raylib.Types.Color -> IO ()
drawLineStrip points color = withArray points (\p -> with color $ c'drawLineStrip p (genericLength points))

foreign import ccall safe "raylib.h &DrawLineStrip"
  p'drawLineStrip ::
    FunPtr (Ptr Raylib.Types.Vector2 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircle_" c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawCircle :: Int -> Int -> Float -> Raylib.Types.Color -> IO ()
drawCircle centerX centerY radius color = with color (c'drawCircle (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall safe "raylib.h &DrawCircle"
  p'drawCircle ::
    FunPtr (CInt -> CInt -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleSector_" c'drawCircleSector :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawCircleSector :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCircleSector center radius startAngle endAngle segments color =
  with
    center
    ( \c ->
        with
          color
          ( c'drawCircleSector c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

foreign import ccall safe "raylib.h &DrawCircleSector"
  p'drawCircleSector ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleSectorLines_" c'drawCircleSectorLines :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawCircleSectorLines :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCircleSectorLines center radius startAngle endAngle segments color =
  with
    center
    ( \c ->
        with
          color
          ( c'drawCircleSectorLines c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

foreign import ccall safe "raylib.h &DrawCircleSectorLines"
  p'drawCircleSectorLines ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleGradient_" c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

drawCircleGradient :: Int -> Int -> Float -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 =
  with color1 (with color2 . c'drawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall safe "raylib.h &DrawCircleGradient"
  p'drawCircleGradient ::
    FunPtr (CInt -> CInt -> CFloat -> Raylib.Types.Color -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleV_" c'drawCircleV :: Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawCircleV :: Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawCircleV center radius color =
  with center (\c -> with color (c'drawCircleV c (realToFrac radius)))

foreign import ccall safe "raylib.h &DrawCircleV"
  p'drawCircleV ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleLines_" c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawCircleLines :: Int -> Int -> Float -> Raylib.Types.Color -> IO ()
drawCircleLines centerX centerY radius color =
  with color (c'drawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall safe "raylib.h &DrawCircleLines"
  p'drawCircleLines ::
    FunPtr (CInt -> CInt -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawEllipse_" c'drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawEllipse :: Int -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawEllipse centerX centerY radiusH radiusV color =
  with color (c'drawEllipse (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

foreign import ccall safe "raylib.h &DrawEllipse"
  p'drawEllipse ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawEllipseLines_" c'drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawEllipseLines :: Int -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawEllipseLines centerX centerY radiusH radiusV color =
  with color (c'drawEllipseLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

foreign import ccall safe "raylib.h &DrawEllipseLines"
  p'drawEllipseLines ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRing_" c'drawRing :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawRing :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawRing center innerRadius outerRadius startAngle endAngle segments color =
  with
    center
    ( \c ->
        with
          color
          ( c'drawRing
              c
              (realToFrac innerRadius)
              (realToFrac outerRadius)
              (realToFrac startAngle)
              (realToFrac endAngle)
              (fromIntegral segments)
          )
    )

foreign import ccall safe "raylib.h &DrawRing"
  p'drawRing ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRingLines_" c'drawRingLines :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawRingLines :: Raylib.Types.Vector2 -> Float -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawRingLines center innerRadius outerRadius startAngle endAngle segments color =
  with
    center
    ( \c ->
        with
          color
          ( c'drawRingLines
              c
              (realToFrac innerRadius)
              (realToFrac outerRadius)
              (realToFrac startAngle)
              (realToFrac endAngle)
              (fromIntegral segments)
          )
    )

foreign import ccall safe "raylib.h &DrawRingLines"
  p'drawRingLines ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangle_" c'drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawRectangle :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawRectangle posX posY width height color =
  with color (c'drawRectangle (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

foreign import ccall safe "raylib.h &DrawRectangle"
  p'drawRectangle ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleV_" c'drawRectangleV :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawRectangleV :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawRectangleV position size color = with position (\p -> with size (with color . c'drawRectangleV p))

foreign import ccall safe "raylib.h &DrawRectangleV"
  p'drawRectangleV ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRec_" c'drawRectangleRec :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> IO ()

drawRectangleRec :: Raylib.Types.Rectangle -> Raylib.Types.Color -> IO ()
drawRectangleRec rect color = with rect (with color . c'drawRectangleRec)

foreign import ccall safe "raylib.h &DrawRectangleRec"
  p'drawRectangleRec ::
    FunPtr (Raylib.Types.Rectangle -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectanglePro_" c'drawRectanglePro :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawRectanglePro :: Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawRectanglePro rect origin rotation color =
  with color (\c -> with rect (\r -> with origin (\o -> c'drawRectanglePro r o (realToFrac rotation) c)))

foreign import ccall safe "raylib.h &DrawRectanglePro"
  p'drawRectanglePro ::
    FunPtr (Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientV_" c'drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

drawRectangleGradientV :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
drawRectangleGradientV posX posY width height color1 color2 =
  with
    color1
    ( with color2
        . c'drawRectangleGradientV
          (fromIntegral posX)
          (fromIntegral posY)
          (fromIntegral width)
          (fromIntegral height)
    )

foreign import ccall safe "raylib.h &DrawRectangleGradientV"
  p'drawRectangleGradientV ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientH_" c'drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

drawRectangleGradientH :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
drawRectangleGradientH posX posY width height color1 color2 =
  with
    color1
    ( with color2
        . c'drawRectangleGradientH
          (fromIntegral posX)
          (fromIntegral posY)
          (fromIntegral width)
          (fromIntegral height)
    )

foreign import ccall safe "raylib.h &DrawRectangleGradientH"
  p'drawRectangleGradientH ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientEx_" c'drawRectangleGradientEx :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

drawRectangleGradientEx :: Raylib.Types.Rectangle -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> IO ()
drawRectangleGradientEx rect col1 col2 col3 col4 =
  with
    rect
    ( \r ->
        with
          col1
          ( \c1 ->
              with
                col2
                ( \c2 ->
                    with col3 (with col4 . c'drawRectangleGradientEx r c1 c2)
                )
          )
    )

foreign import ccall safe "raylib.h &DrawRectangleGradientEx"
  p'drawRectangleGradientEx ::
    FunPtr (Raylib.Types.Rectangle -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleLines_" c'drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawRectangleLines :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawRectangleLines posX posY width height color =
  with color (c'drawRectangleLines (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

foreign import ccall safe "raylib.h &DrawRectangleLines"
  p'drawRectangleLines ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleLinesEx_" c'drawRectangleLinesEx :: Ptr Raylib.Types.Rectangle -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawRectangleLinesEx :: Raylib.Types.Rectangle -> Float -> Raylib.Types.Color -> IO ()
drawRectangleLinesEx rect thickness color =
  with color (\c -> with rect (\r -> c'drawRectangleLinesEx r (realToFrac thickness) c))

foreign import ccall safe "raylib.h &DrawRectangleLinesEx"
  p'drawRectangleLinesEx ::
    FunPtr (Raylib.Types.Rectangle -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRounded_" c'drawRectangleRounded :: Ptr Raylib.Types.Rectangle -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawRectangleRounded :: Raylib.Types.Rectangle -> Float -> Int -> Raylib.Types.Color -> IO ()
drawRectangleRounded rect roundness segments color =
  with rect (\r -> with color $ c'drawRectangleRounded r (realToFrac roundness) (fromIntegral segments))

foreign import ccall safe "raylib.h &DrawRectangleRounded"
  p'drawRectangleRounded ::
    FunPtr (Raylib.Types.Rectangle -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRoundedLines_" c'drawRectangleRoundedLines :: Ptr Raylib.Types.Rectangle -> CFloat -> CInt -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawRectangleRoundedLines :: Raylib.Types.Rectangle -> Float -> Int -> Float -> Raylib.Types.Color -> IO ()
drawRectangleRoundedLines rect roundness segments thickness color =
  with rect (\r -> with color $ c'drawRectangleRoundedLines r (realToFrac roundness) (fromIntegral segments) (realToFrac thickness))

foreign import ccall safe "raylib.h &DrawRectangleRoundedLines"
  p'drawRectangleRoundedLines ::
    FunPtr (Raylib.Types.Rectangle -> CFloat -> CInt -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangle_" c'drawTriangle :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawTriangle :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTriangle v1 v2 v3 color =
  with
    v1
    ( \p1 ->
        with
          v2
          ( \p2 -> with v3 (with color . c'drawTriangle p1 p2)
          )
    )

foreign import ccall safe "raylib.h &DrawTriangle"
  p'drawTriangle ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleLines_" c'drawTriangleLines :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawTriangleLines :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTriangleLines v1 v2 v3 color =
  with
    v1
    ( \p1 ->
        with
          v2
          ( \p2 -> with v3 (with color . c'drawTriangleLines p1 p2)
          )
    )

foreign import ccall safe "raylib.h &DrawTriangleLines"
  p'drawTriangleLines ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleFan_" c'drawTriangleFan :: Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawTriangleFan :: [Raylib.Types.Vector2] -> Raylib.Types.Color -> IO ()
drawTriangleFan points color = withArray points (\p -> with color $ c'drawTriangleFan p (genericLength points))

foreign import ccall safe "raylib.h &DrawTriangleFan"
  p'drawTriangleFan ::
    FunPtr (Ptr Raylib.Types.Vector2 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleStrip_" c'drawTriangleStrip :: Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawTriangleStrip :: [Raylib.Types.Vector2] -> Raylib.Types.Color -> IO ()
drawTriangleStrip points color =
  withArray points (\p -> with color $ c'drawTriangleStrip p (genericLength points))

foreign import ccall safe "raylib.h &DrawTriangleStrip"
  p'drawTriangleStrip ::
    FunPtr (Ptr Raylib.Types.Vector2 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawPoly_" c'drawPoly :: Ptr Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawPoly :: Raylib.Types.Vector2 -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawPoly center sides radius rotation color =
  with center (\c -> with color $ c'drawPoly c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

foreign import ccall safe "raylib.h &DrawPoly"
  p'drawPoly ::
    FunPtr (Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawPolyLines_" c'drawPolyLines :: Ptr Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawPolyLines :: Raylib.Types.Vector2 -> Int -> Float -> Float -> Raylib.Types.Color -> IO ()
drawPolyLines center sides radius rotation color =
  with center (\c -> with color $ c'drawPolyLines c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

foreign import ccall safe "raylib.h &DrawPolyLines"
  p'drawPolyLines ::
    FunPtr (Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawPolyLinesEx_" c'drawPolyLinesEx :: Ptr Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawPolyLinesEx :: Raylib.Types.Vector2 -> Int -> Float -> Float -> Float -> Raylib.Types.Color -> IO ()
drawPolyLinesEx center sides radius rotation thickness color =
  with
    center
    ( \c ->
        with color $
          c'drawPolyLinesEx
            c
            (fromIntegral sides)
            (realToFrac radius)
            (realToFrac rotation)
            (realToFrac thickness)
    )

foreign import ccall safe "raylib.h &DrawPolyLinesEx"
  p'drawPolyLinesEx ::
    FunPtr (Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h CheckCollisionRecs_" c'checkCollisionRecs :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> IO CBool

checkCollisionRecs :: Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Bool
checkCollisionRecs rec1 rec2 = unsafePerformIO $ toBool <$> with rec1 (with rec2 . c'checkCollisionRecs)

foreign import ccall safe "raylib.h &CheckCollisionRecs"
  p'checkCollisionRecs ::
    FunPtr (Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionCircles_" c'checkCollisionCircles :: Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Vector2 -> CFloat -> IO CBool

checkCollisionCircles :: Raylib.Types.Vector2 -> Float -> Raylib.Types.Vector2 -> Float -> Bool
checkCollisionCircles center1 radius1 center2 radius2 =
  unsafePerformIO $ toBool <$> with center1 (\c1 -> with center2 (\c2 -> c'checkCollisionCircles c1 (realToFrac radius1) c2 (realToFrac radius2)))

foreign import ccall safe "raylib.h &CheckCollisionCircles"
  p'checkCollisionCircles ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Vector2 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionCircleRec_" c'checkCollisionCircleRec :: Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Rectangle -> IO CBool

checkCollisionCircleRec :: Raylib.Types.Vector2 -> Float -> Raylib.Types.Rectangle -> Bool
checkCollisionCircleRec center radius rect =
  unsafePerformIO $ toBool <$> with center (\c -> with rect $ c'checkCollisionCircleRec c (realToFrac radius))

foreign import ccall safe "raylib.h &CheckCollisionCircleRec"
  p'checkCollisionCircleRec ::
    FunPtr (Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointRec_" c'checkCollisionPointRec :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Rectangle -> IO CBool

checkCollisionPointRec :: Raylib.Types.Vector2 -> Raylib.Types.Rectangle -> Bool
checkCollisionPointRec point rect =
  unsafePerformIO $ toBool <$> with point (with rect . c'checkCollisionPointRec)

foreign import ccall safe "raylib.h &CheckCollisionPointRec"
  p'checkCollisionPointRec ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointCircle_" c'checkCollisionPointCircle :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> IO CBool

checkCollisionPointCircle :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Bool
checkCollisionPointCircle point center radius =
  unsafePerformIO $ toBool <$> with point (\p -> with center (\c -> c'checkCollisionPointCircle p c (realToFrac radius)))

foreign import ccall safe "raylib.h &CheckCollisionPointCircle"
  p'checkCollisionPointCircle ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointTriangle_" c'checkCollisionPointTriangle :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> IO CBool

checkCollisionPointTriangle :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Bool
checkCollisionPointTriangle point p1 p2 p3 =
  unsafePerformIO $ toBool <$> with point (\p -> with p1 (\ptr1 -> with p2 (with p3 . c'checkCollisionPointTriangle p ptr1)))

foreign import ccall safe "raylib.h &CheckCollisionPointTriangle"
  p'checkCollisionPointTriangle ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionLines_" c'checkCollisionLines :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> IO CBool

-- | If a collision is found, returns @Just collisionPoint@, otherwise returns @Nothing@
checkCollisionLines :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Maybe Raylib.Types.Vector2
checkCollisionLines start1 end1 start2 end2 =
  unsafePerformIO $
    with
      (Raylib.Types.Vector2 0 0)
      ( \res -> do
          foundCollision <- toBool <$> with start1 (\s1 -> with end1 (\e1 -> with start2 (\s2 -> with end2 (\e2 -> c'checkCollisionLines s1 e1 s2 e2 res))))
          if foundCollision then Just <$> peek res else return Nothing
      )

foreign import ccall safe "raylib.h &CheckCollisionLines"
  p'checkCollisionLines ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointLine_" c'checkCollisionPointLine :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CInt -> IO CBool

checkCollisionPointLine :: Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Int -> Bool
checkCollisionPointLine point p1 p2 threshold =
  unsafePerformIO $ toBool <$> with point (\p -> with p1 (\ptr1 -> with p2 (\ptr2 -> c'checkCollisionPointLine p ptr1 ptr2 (fromIntegral threshold))))

foreign import ccall safe "raylib.h &CheckCollisionPointLine"
  p'checkCollisionPointLine ::
    FunPtr (Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetCollisionRec_" c'getCollisionRec :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> IO (Ptr Raylib.Types.Rectangle)

getCollisionRec :: Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle
getCollisionRec rec1 rec2 =
  unsafePerformIO $ with rec1 (with rec2 . c'getCollisionRec) >>= pop

foreign import ccall safe "raylib.h &GetCollisionRec"
  p'getCollisionRec ::
    FunPtr (Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> IO Raylib.Types.Rectangle)

foreign import ccall safe "bindings.h LoadImage_" c'loadImage :: CString -> IO (Ptr Raylib.Types.Image)

loadImage :: String -> IO Raylib.Types.Image
loadImage fileName = withCString fileName c'loadImage >>= pop

foreign import ccall safe "raylib.h &LoadImage"
  p'loadImage ::
    FunPtr (CString -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageRaw_" c'loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.Image)

loadImageRaw :: String -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
loadImageRaw fileName width height format headerSize =
  withCString fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) (fromIntegral headerSize)) >>= pop

foreign import ccall safe "raylib.h &LoadImageRaw"
  p'loadImageRaw ::
    FunPtr (CString -> CInt -> CInt -> CInt -> CInt -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageAnim_" c'loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Raylib.Types.Image)

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

foreign import ccall safe "raylib.h &LoadImageAnim"
  p'loadImageAnim ::
    FunPtr (CString -> Ptr CInt -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageFromMemory_" c'loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Raylib.Types.Image)

loadImageFromMemory :: String -> [Integer] -> IO Raylib.Types.Image
loadImageFromMemory fileType fileData =
  withCString fileType (\ft -> withArrayLen (map fromIntegral fileData) (\size fd -> c'loadImageFromMemory ft fd (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

foreign import ccall safe "raylib.h &LoadImageFromMemory"
  p'loadImageFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageFromTexture_" c'loadImageFromTexture :: Ptr Raylib.Types.Texture -> IO (Ptr Raylib.Types.Image)

loadImageFromTexture :: Raylib.Types.Texture -> IO Raylib.Types.Image
loadImageFromTexture tex = with tex c'loadImageFromTexture >>= pop

foreign import ccall safe "raylib.h &LoadImageFromTexture"
  p'loadImageFromTexture ::
    FunPtr (Raylib.Types.Texture -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageFromScreen_" c'loadImageFromScreen :: IO (Ptr Raylib.Types.Image)

loadImageFromScreen :: IO Raylib.Types.Image
loadImageFromScreen = c'loadImageFromScreen >>= pop

foreign import ccall safe "raylib.h &LoadImageFromScreen"
  p'loadImageFromScreen ::
    FunPtr (IO Raylib.Types.Image)

foreign import ccall safe "bindings.h UnloadImage_" c'unloadImage :: Ptr Raylib.Types.Image -> IO ()

unloadImage :: Raylib.Types.Image -> IO ()
unloadImage image = with image c'unloadImage

foreign import ccall safe "raylib.h &UnloadImage"
  p'unloadImage ::
    FunPtr (Raylib.Types.Image -> IO ())

foreign import ccall safe "bindings.h ExportImage_" c'exportImage :: Ptr Raylib.Types.Image -> CString -> IO CBool

exportImage :: Raylib.Types.Image -> String -> IO Bool
exportImage image fileName = toBool <$> with image (withCString fileName . c'exportImage)

foreign import ccall safe "raylib.h &ExportImage"
  p'exportImage ::
    FunPtr (Raylib.Types.Image -> CString -> IO CInt)

foreign import ccall safe "bindings.h ExportImageAsCode_" c'exportImageAsCode :: Ptr Raylib.Types.Image -> CString -> IO CBool

exportImageAsCode :: Raylib.Types.Image -> String -> IO Bool
exportImageAsCode image fileName =
  toBool <$> with image (withCString fileName . c'exportImageAsCode)

foreign import ccall safe "raylib.h &ExportImageAsCode"
  p'exportImageAsCode ::
    FunPtr (Raylib.Types.Image -> CString -> IO CInt)

foreign import ccall safe "bindings.h GenImageColor_" c'genImageColor :: CInt -> CInt -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

genImageColor :: Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageColor width height color =
  with color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageColor"
  p'genImageColor ::
    FunPtr (CInt -> CInt -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageGradientV_" c'genImageGradientV :: CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

genImageGradientV :: Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientV width height top bottom =
  with top (with bottom . c'genImageGradientV (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageGradientV"
  p'genImageGradientV ::
    FunPtr (CInt -> CInt -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageGradientH_" c'genImageGradientH :: CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

genImageGradientH :: Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientH width height left right =
  with left (with right . c'genImageGradientH (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageGradientH"
  p'genImageGradientH ::
    FunPtr (CInt -> CInt -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageGradientRadial_" c'genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

genImageGradientRadial :: Int -> Int -> Float -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageGradientRadial width height density inner outer =
  with inner (with outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

foreign import ccall safe "raylib.h &GenImageGradientRadial"
  p'genImageGradientRadial ::
    FunPtr (CInt -> CInt -> CFloat -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageChecked_" c'genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

genImageChecked :: Int -> Int -> Int -> Int -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
genImageChecked width height checksX checksY col1 col2 =
  with col1 (with col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= pop

foreign import ccall safe "raylib.h &GenImageChecked"
  p'genImageChecked ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageWhiteNoise_" c'genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Raylib.Types.Image)

genImageWhiteNoise :: Int -> Int -> Float -> IO Raylib.Types.Image
genImageWhiteNoise width height factor =
  c'genImageWhiteNoise (fromIntegral width) (fromIntegral height) (realToFrac factor) >>= pop

foreign import ccall safe "raylib.h &GenImageWhiteNoise"
  p'genImageWhiteNoise ::
    FunPtr (CInt -> CInt -> CFloat -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImagePerlinNoise_" c'genImagePerlinNoise :: CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Raylib.Types.Image)

genImagePerlinNoise :: Int -> Int -> Int -> Int -> Float -> IO Raylib.Types.Image
genImagePerlinNoise width height offsetX offsetY scale = c'genImagePerlinNoise (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY) (realToFrac scale) >>= pop

foreign import ccall safe "raylib.h &GenImagePerlinNoise" p'genImagePerlinNoise :: FunPtr (CInt -> CInt -> CInt -> CInt -> CFloat -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageCellular_" c'genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.Image)

genImageCellular :: Int -> Int -> Int -> IO Raylib.Types.Image
genImageCellular width height tileSize =
  c'genImageCellular (fromIntegral width) (fromIntegral height) (fromIntegral tileSize) >>= pop

foreign import ccall safe "raylib.h &GenImageCellular"
  p'genImageCellular ::
    FunPtr (CInt -> CInt -> CInt -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageText_" c'genImageText :: CInt -> CInt -> CString -> IO (Ptr Raylib.Types.Image)

genImageText :: Int -> Int -> String -> IO Raylib.Types.Image
genImageText width height text =
  withCString text (c'genImageText (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageText"
  p'genImageText ::
    FunPtr (CInt -> CInt -> CString -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageCopy_" c'imageCopy :: Ptr Raylib.Types.Image -> IO (Ptr Raylib.Types.Image)

imageCopy :: Raylib.Types.Image -> IO Raylib.Types.Image
imageCopy image = with image c'imageCopy >>= pop

foreign import ccall safe "raylib.h &ImageCopy"
  p'imageCopy ::
    FunPtr (Raylib.Types.Image -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageFromImage_" c'imageFromImage :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> IO (Ptr Raylib.Types.Image)

imageFromImage :: Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image
imageFromImage image rect = with image (with rect . c'imageFromImage) >>= pop

foreign import ccall safe "raylib.h &ImageFromImage"
  p'imageFromImage ::
    FunPtr (Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageText_" c'imageText :: CString -> CInt -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

imageText :: String -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageText text fontSize color =
  withCString text (\t -> with color $ c'imageText t (fromIntegral fontSize)) >>= pop

foreign import ccall safe "raylib.h &ImageText"
  p'imageText ::
    FunPtr (CString -> CInt -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageTextEx_" c'imageTextEx :: Ptr Raylib.Types.Font -> CString -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

imageTextEx :: Raylib.Types.Font -> String -> Float -> Float -> Raylib.Types.Color -> IO Raylib.Types.Image
imageTextEx font text fontSize spacing tint =
  with font (\f -> withCString text (\t -> with tint $ c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

foreign import ccall safe "raylib.h &ImageTextEx"
  p'imageTextEx ::
    FunPtr (Raylib.Types.Font -> CString -> CFloat -> CFloat -> Raylib.Types.Color -> IO Raylib.Types.Image)

foreign import ccall safe "raylib.h ImageFormat"
  c'imageFormat ::
    Ptr Raylib.Types.Image -> CInt -> IO ()

imageFormat :: Raylib.Types.Image -> PixelFormat -> IO Raylib.Types.Image
imageFormat image newFormat =
  with image (\i -> c'imageFormat i (fromIntegral $ fromEnum newFormat) >> peek i)

foreign import ccall safe "raylib.h &ImageFormat"
  p'imageFormat ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageToPOT_" c'imageToPOT :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> IO ()

imageToPOT :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageToPOT image color = with image (\i -> with color (c'imageToPOT i) >> peek i)

foreign import ccall safe "raylib.h &ImageToPOT"
  p'imageToPOT ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageCrop_" c'imageCrop :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> IO ()

imageCrop :: Raylib.Types.Image -> Raylib.Types.Rectangle -> IO Raylib.Types.Image
imageCrop image crop = with image (\i -> with crop (c'imageCrop i) >> peek i)

foreign import ccall safe "raylib.h &ImageCrop"
  p'imageCrop ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Rectangle -> IO ())

foreign import ccall safe "raylib.h ImageAlphaCrop"
  c'imageAlphaCrop ::
    Ptr Raylib.Types.Image -> CFloat -> IO ()

imageAlphaCrop :: Raylib.Types.Image -> Float -> IO Raylib.Types.Image
imageAlphaCrop image threshold = with image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peek i)

foreign import ccall safe "raylib.h &ImageAlphaCrop"
  p'imageAlphaCrop ::
    FunPtr (Ptr Raylib.Types.Image -> CFloat -> IO ())

foreign import ccall safe "bindings.h ImageAlphaClear_" c'imageAlphaClear :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> CFloat -> IO ()

imageAlphaClear :: Raylib.Types.Image -> Raylib.Types.Color -> Float -> IO Raylib.Types.Image
imageAlphaClear image color threshold = with image (\i -> with color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peek i))

foreign import ccall safe "raylib.h &ImageAlphaClear"
  p'imageAlphaClear ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Color -> CFloat -> IO ())

foreign import ccall safe "bindings.h ImageAlphaMask_" c'imageAlphaMask :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Image -> IO ()

imageAlphaMask :: Raylib.Types.Image -> Raylib.Types.Image -> IO Raylib.Types.Image
imageAlphaMask image alphaMask = with image (\i -> with alphaMask (c'imageAlphaMask i) >> peek i)

foreign import ccall safe "raylib.h &ImageAlphaMask"
  p'imageAlphaMask ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageAlphaPremultiply"
  c'imageAlphaPremultiply ::
    Ptr Raylib.Types.Image -> IO ()

imageAlphaPremultiply :: Raylib.Types.Image -> IO Raylib.Types.Image
imageAlphaPremultiply image = with image (\i -> c'imageAlphaPremultiply i >> peek i)

foreign import ccall safe "raylib.h &ImageAlphaPremultiply"
  p'imageAlphaPremultiply ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())


foreign import ccall safe "raylib.h ImageBlurGaussian"
  c'imageBlurGaussian ::
    Ptr Raylib.Types.Image -> CInt -> IO ()

imageBlurGaussian :: Raylib.Types.Image -> Int -> IO Raylib.Types.Image
imageBlurGaussian image blurSize = with image (\i -> c'imageBlurGaussian i (fromIntegral blurSize) >> peek i)

foreign import ccall safe "raylib.h &ImageBlurGaussian"
  p'imageBlurGaussian ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> IO ())

foreign import ccall safe "raylib.h ImageResize"
  c'imageResize ::
    Ptr Raylib.Types.Image -> CInt -> CInt -> IO ()

imageResize :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Image
imageResize image newWidth newHeight = with image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

foreign import ccall safe "raylib.h &ImageResize"
  p'imageResize ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h ImageResizeNN"
  c'imageResizeNN ::
    Ptr Raylib.Types.Image -> CInt -> CInt -> IO ()

imageResizeNN :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Image
imageResizeNN image newWidth newHeight = with image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

foreign import ccall safe "raylib.h &ImageResizeNN"
  p'imageResizeNN ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageResizeCanvas_" c'imageResizeCanvas :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageResizeCanvas :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = with image (\i -> with fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peek i)

foreign import ccall safe "raylib.h &ImageResizeCanvas"
  p'imageResizeCanvas ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "raylib.h ImageMipmaps"
  c'imageMipmaps ::
    Ptr Raylib.Types.Image -> IO ()

imageMipmaps :: Raylib.Types.Image -> IO Raylib.Types.Image
imageMipmaps image = with image (\i -> c'imageMipmaps i >> peek i)

foreign import ccall safe "raylib.h &ImageMipmaps"
  p'imageMipmaps ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageDither"
  c'imageDither ::
    Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> IO ()

imageDither :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
imageDither image rBpp gBpp bBpp aBpp = with image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peek i)

foreign import ccall safe "raylib.h &ImageDither"
  p'imageDither ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h ImageFlipVertical"
  c'imageFlipVertical ::
    Ptr Raylib.Types.Image -> IO ()

imageFlipVertical :: Raylib.Types.Image -> IO Raylib.Types.Image
imageFlipVertical image = with image (\i -> c'imageFlipVertical i >> peek i)

foreign import ccall safe "raylib.h &ImageFlipVertical"
  p'imageFlipVertical ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageFlipHorizontal"
  c'imageFlipHorizontal ::
    Ptr Raylib.Types.Image -> IO ()

imageFlipHorizontal :: Raylib.Types.Image -> IO Raylib.Types.Image
imageFlipHorizontal image = with image (\i -> c'imageFlipHorizontal i >> peek i)

foreign import ccall safe "raylib.h &ImageFlipHorizontal"
  p'imageFlipHorizontal ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageRotateCW"
  c'imageRotateCW ::
    Ptr Raylib.Types.Image -> IO ()

imageRotateCW :: Raylib.Types.Image -> IO Raylib.Types.Image
imageRotateCW image = with image (\i -> c'imageRotateCW i >> peek i)

foreign import ccall safe "raylib.h &ImageRotateCW"
  p'imageRotateCW ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageRotateCCW"
  c'imageRotateCCW ::
    Ptr Raylib.Types.Image -> IO ()

imageRotateCCW :: Raylib.Types.Image -> IO Raylib.Types.Image
imageRotateCCW image = with image (\i -> c'imageRotateCCW i >> peek i)

foreign import ccall safe "raylib.h &ImageRotateCCW"
  p'imageRotateCCW ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "bindings.h ImageColorTint_" c'imageColorTint :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> IO ()

imageColorTint :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageColorTint image color = with image (\i -> with color (c'imageColorTint i) >> peek i)

foreign import ccall safe "raylib.h &ImageColorTint"
  p'imageColorTint ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Color -> IO ())

foreign import ccall safe "raylib.h ImageColorInvert"
  c'imageColorInvert ::
    Ptr Raylib.Types.Image -> IO ()

imageColorInvert :: Raylib.Types.Image -> IO Raylib.Types.Image
imageColorInvert image = with image (\i -> c'imageColorInvert i >> peek i)

foreign import ccall safe "raylib.h &ImageColorInvert"
  p'imageColorInvert ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageColorGrayscale"
  c'imageColorGrayscale ::
    Ptr Raylib.Types.Image -> IO ()

imageColorGrayscale :: Raylib.Types.Image -> IO Raylib.Types.Image
imageColorGrayscale image = with image (\i -> c'imageColorGrayscale i >> peek i)

foreign import ccall safe "raylib.h &ImageColorGrayscale"
  p'imageColorGrayscale ::
    FunPtr (Ptr Raylib.Types.Image -> IO ())

foreign import ccall safe "raylib.h ImageColorContrast"
  c'imageColorContrast ::
    Ptr Raylib.Types.Image -> CFloat -> IO ()

imageColorContrast :: Raylib.Types.Image -> Float -> IO Raylib.Types.Image
imageColorContrast image contrast = with image (\i -> c'imageColorContrast i (realToFrac contrast) >> peek i)

foreign import ccall safe "raylib.h &ImageColorContrast"
  p'imageColorContrast ::
    FunPtr (Ptr Raylib.Types.Image -> CFloat -> IO ())

foreign import ccall safe "raylib.h ImageColorBrightness"
  c'imageColorBrightness ::
    Ptr Raylib.Types.Image -> CInt -> IO ()

imageColorBrightness :: Raylib.Types.Image -> Int -> IO Raylib.Types.Image
imageColorBrightness image brightness = with image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peek i)

foreign import ccall safe "raylib.h &ImageColorBrightness"
  p'imageColorBrightness ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageColorReplace_" c'imageColorReplace :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

imageColorReplace :: Raylib.Types.Image -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Image
imageColorReplace image color replace = with image (\i -> with color (with replace . c'imageColorReplace i) >> peek i)

foreign import ccall safe "raylib.h &ImageColorReplace"
  p'imageColorReplace ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Color -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h LoadImageColors_" c'loadImageColors :: Ptr Raylib.Types.Image -> IO (Ptr Raylib.Types.Color)

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

foreign import ccall safe "raylib.h &LoadImageColors"
  p'loadImageColors ::
    FunPtr (Raylib.Types.Image -> IO (Ptr Raylib.Types.Color))

foreign import ccall safe "bindings.h LoadImagePalette_" c'loadImagePalette :: Ptr Raylib.Types.Image -> CInt -> Ptr CInt -> IO (Ptr Raylib.Types.Color)

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

foreign import ccall safe "raylib.h &LoadImagePalette"
  p'loadImagePalette ::
    FunPtr (Raylib.Types.Image -> CInt -> Ptr CInt -> IO (Ptr Raylib.Types.Color))

-- | NOTE: You usually won't need to use this. `loadImageColors` unloads the colors automatically. Only use this when you are using `c'loadImageColors` to load the colors.
foreign import ccall safe "raylib.h UnloadImageColors"
  unloadImageColors ::
    Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h &UnloadImageColors"
  p'unloadImageColors ::
    FunPtr (Ptr Raylib.Types.Color -> IO ())

-- | NOTE: You usually won't need to use this. `loadImagePalette` unloads the colors automatically. Only use this when you are using `c'loadImagePalette` to load the colors.
foreign import ccall safe "raylib.h UnloadImagePalette"
  unloadImagePalette ::
    Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h &UnloadImagePalette"
  p'unloadImagePalette ::
    FunPtr (Ptr Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h GetImageAlphaBorder_" c'getImageAlphaBorder :: Ptr Raylib.Types.Image -> CFloat -> IO (Ptr Raylib.Types.Rectangle)

getImageAlphaBorder :: Raylib.Types.Image -> Float -> IO Raylib.Types.Rectangle
getImageAlphaBorder image threshold = with image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

foreign import ccall safe "raylib.h &GetImageAlphaBorder"
  p'getImageAlphaBorder ::
    FunPtr (Raylib.Types.Image -> CFloat -> IO Raylib.Types.Rectangle)

foreign import ccall safe "bindings.h GetImageColor_" c'getImageColor :: Ptr Raylib.Types.Image -> CInt -> CInt -> IO (Ptr Raylib.Types.Color)

getImageColor :: Raylib.Types.Image -> Int -> Int -> IO Raylib.Types.Color
getImageColor image x y = with image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

foreign import ccall safe "raylib.h &GetImageColor"
  p'getImageColor ::
    FunPtr (Raylib.Types.Image -> CInt -> CInt -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ImageClearBackground_" c'imageClearBackground :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> IO ()

imageClearBackground :: Raylib.Types.Image -> Raylib.Types.Color -> IO Raylib.Types.Image
imageClearBackground image color = with image (\i -> with color (c'imageClearBackground i) >> peek i)

foreign import ccall safe "raylib.h &ImageClearBackground"
  p'imageClearBackground ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawPixel_" c'imageDrawPixel :: Ptr Raylib.Types.Image -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawPixel :: Raylib.Types.Image -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawPixel image x y color = with image (\i -> with color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawPixel"
  p'imageDrawPixel ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawPixelV_" c'imageDrawPixelV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

imageDrawPixelV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawPixelV image position color = with image (\i -> with position (with color . c'imageDrawPixelV i) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawPixelV"
  p'imageDrawPixelV ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawLine_" c'imageDrawLine :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawLine :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawLine image startPosX startPosY endPosX endPosY color = with image (\i -> with color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawLine"
  p'imageDrawLine ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawLineV_" c'imageDrawLineV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

imageDrawLineV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawLineV image start end color = with image (\i -> with start (\s -> with end (with color . c'imageDrawLineV i s)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawLineV"
  p'imageDrawLineV ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircle_" c'imageDrawCircle :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawCircle :: Raylib.Types.Image -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircle image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircle"
  p'imageDrawCircle ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleV_" c'imageDrawCircleV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawCircleV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleV i c (fromIntegral radius))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircleV"
  p'imageDrawCircleV ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Vector2 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleLines_" c'imageDrawCircleLines :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawCircleLines :: Raylib.Types.Image -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleLines image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircleLines"
  p'imageDrawCircleLines ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Vector2 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleLinesV_" c'imageDrawCircleLinesV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawCircleLinesV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawCircleLinesV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircleLinesV"
  p'imageDrawCircleLinesV ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Vector2 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangle_" c'imageDrawRectangle :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawRectangle :: Raylib.Types.Image -> Int -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangle image posX posY width height color = with image (\i -> with color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangle"
  p'imageDrawRectangle ::
    FunPtr (Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleV_" c'imageDrawRectangleV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

imageDrawRectangleV :: Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleV image position size color = with image (\i -> with position (\p -> with size (with color . c'imageDrawRectangleV i p)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangleV"
  p'imageDrawRectangleV ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleRec_" c'imageDrawRectangleRec :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> IO ()

imageDrawRectangleRec :: Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleRec image rectangle color = with image (\i -> with rectangle (with color . c'imageDrawRectangleRec i) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangleRec"
  p'imageDrawRectangleRec ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleLines_" c'imageDrawRectangleLines :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawRectangleLines :: Raylib.Types.Image -> Raylib.Types.Rectangle -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawRectangleLines image rectangle thickness color = with image (\i -> with rectangle (\r -> with color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangleLines"
  p'imageDrawRectangleLines ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Rectangle -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDraw_" c'imageDraw :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> IO ()

imageDraw :: Raylib.Types.Image -> Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDraw image source srcRec dstRec tint = with image (\i -> with source (\s -> with srcRec (\sr -> with dstRec (with tint . c'imageDraw i s sr))) >> peek i)

foreign import ccall safe "raylib.h &ImageDraw"
  p'imageDraw ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Image -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawText_" c'imageDrawText :: Ptr Raylib.Types.Image -> CString -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

imageDrawText :: Raylib.Types.Image -> String -> Int -> Int -> Int -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawText image text x y fontSize color = with image (\i -> withCString text (\t -> with color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawText"
  p'imageDrawText ::
    FunPtr (Ptr Raylib.Types.Image -> CString -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawTextEx_" c'imageDrawTextEx :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Font -> CString -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

imageDrawTextEx :: Raylib.Types.Image -> Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO Raylib.Types.Image
imageDrawTextEx image font text position fontSize spacing tint = with image (\i -> with font (\f -> withCString text (\t -> with position (\p -> with tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawTextEx"
  p'imageDrawTextEx ::
    FunPtr (Ptr Raylib.Types.Image -> Raylib.Types.Font -> CString -> Raylib.Types.Vector2 -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h LoadTexture_" c'loadTexture :: CString -> IO (Ptr Raylib.Types.Texture)

loadTexture :: String -> IO Raylib.Types.Texture
loadTexture fileName = withCString fileName c'loadTexture >>= pop

foreign import ccall safe "raylib.h &LoadTexture"
  p'loadTexture ::
    FunPtr (CString -> IO Raylib.Types.Texture)

foreign import ccall safe "bindings.h LoadTextureFromImage_" c'loadTextureFromImage :: Ptr Raylib.Types.Image -> IO (Ptr Raylib.Types.Texture)

loadTextureFromImage :: Raylib.Types.Image -> IO Raylib.Types.Texture
loadTextureFromImage image = with image c'loadTextureFromImage >>= pop

foreign import ccall safe "raylib.h &LoadTextureFromImage"
  p'loadTextureFromImage ::
    FunPtr (Raylib.Types.Image -> IO Raylib.Types.Texture)

foreign import ccall safe "bindings.h LoadTextureCubemap_" c'loadTextureCubemap :: Ptr Raylib.Types.Image -> CInt -> IO (Ptr Raylib.Types.Texture)

loadTextureCubemap :: Raylib.Types.Image -> CubemapLayout -> IO Raylib.Types.Texture
loadTextureCubemap image layout = with image (\i -> c'loadTextureCubemap i (fromIntegral $ fromEnum layout)) >>= pop

foreign import ccall safe "raylib.h &LoadTextureCubemap"
  p'loadTextureCubemap ::
    FunPtr (Raylib.Types.Image -> CInt -> IO Raylib.Types.Texture)

foreign import ccall safe "bindings.h LoadRenderTexture_" c'loadRenderTexture :: CInt -> CInt -> IO (Ptr Raylib.Types.RenderTexture)

loadRenderTexture :: Int -> Int -> IO Raylib.Types.RenderTexture
loadRenderTexture width height = c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= pop

foreign import ccall safe "raylib.h &LoadRenderTexture"
  p'loadRenderTexture ::
    FunPtr (CInt -> CInt -> IO Raylib.Types.RenderTexture)

foreign import ccall safe "bindings.h UnloadTexture_" c'unloadTexture :: Ptr Raylib.Types.Texture -> IO ()

unloadTexture :: Raylib.Types.Texture -> IO ()
unloadTexture texture = with texture c'unloadTexture

foreign import ccall safe "raylib.h &UnloadTexture"
  p'unloadTexture ::
    FunPtr (Raylib.Types.Texture -> IO ())

foreign import ccall safe "bindings.h UnloadRenderTexture_" c'unloadRenderTexture :: Ptr Raylib.Types.RenderTexture -> IO ()

unloadRenderTexture :: Raylib.Types.RenderTexture -> IO ()
unloadRenderTexture target = with target c'unloadRenderTexture

foreign import ccall safe "raylib.h &UnloadRenderTexture"
  p'unloadRenderTexture ::
    FunPtr (Raylib.Types.RenderTexture -> IO ())

foreign import ccall safe "bindings.h UpdateTexture_" c'updateTexture :: Ptr Raylib.Types.Texture -> Ptr () -> IO ()

updateTexture :: Raylib.Types.Texture -> Ptr () -> IO Raylib.Types.Texture
updateTexture texture pixels = with texture (\t -> c'updateTexture t pixels >> peek t)

foreign import ccall safe "raylib.h &UpdateTexture"
  p'updateTexture ::
    FunPtr (Raylib.Types.Texture -> Ptr () -> IO ())

foreign import ccall safe "bindings.h UpdateTextureRec_" c'updateTextureRec :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr () -> IO ()

updateTextureRec :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Ptr () -> IO Raylib.Types.Texture
updateTextureRec texture rect pixels = with texture (\t -> with rect (\r -> c'updateTextureRec t r pixels) >> peek t)

foreign import ccall safe "raylib.h &UpdateTextureRec"
  p'updateTextureRec ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.Rectangle -> Ptr () -> IO ())

foreign import ccall safe "raylib.h GenTextureMipmaps"
  c'genTextureMipmaps ::
    Ptr Raylib.Types.Texture -> IO ()

genTextureMipmaps :: Raylib.Types.Texture -> IO Raylib.Types.Texture
genTextureMipmaps texture = with texture (\t -> c'genTextureMipmaps t >> peek t)

foreign import ccall safe "raylib.h &GenTextureMipmaps"
  p'genTextureMipmaps ::
    FunPtr (Ptr Raylib.Types.Texture -> IO ())

foreign import ccall safe "bindings.h SetTextureFilter_" c'setTextureFilter :: Ptr Raylib.Types.Texture -> CInt -> IO ()

setTextureFilter :: Raylib.Types.Texture -> TextureFilter -> IO Raylib.Types.Texture
setTextureFilter texture filterType = with texture (\t -> c'setTextureFilter t (fromIntegral $ fromEnum filterType) >> peek t)

foreign import ccall safe "raylib.h &SetTextureFilter"
  p'setTextureFilter ::
    FunPtr (Raylib.Types.Texture -> CInt -> IO ())

foreign import ccall safe "bindings.h SetTextureWrap_" c'setTextureWrap :: Ptr Raylib.Types.Texture -> CInt -> IO ()

setTextureWrap :: Raylib.Types.Texture -> TextureWrap -> IO Raylib.Types.Texture
setTextureWrap texture wrap = with texture (\t -> c'setTextureWrap t (fromIntegral $ fromEnum wrap) >> peek t)

foreign import ccall safe "raylib.h &SetTextureWrap"
  p'setTextureWrap ::
    FunPtr (Raylib.Types.Texture -> CInt -> IO ())

foreign import ccall safe "bindings.h DrawTexture_" c'drawTexture :: Ptr Raylib.Types.Texture -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawTexture :: Raylib.Types.Texture -> Int -> Int -> Raylib.Types.Color -> IO ()
drawTexture texture x y tint = with texture (\t -> with tint (c'drawTexture t (fromIntegral x) (fromIntegral y)))

foreign import ccall safe "raylib.h &DrawTexture"
  p'drawTexture ::
    FunPtr (Raylib.Types.Texture -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureV_" c'drawTextureV :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawTextureV :: Raylib.Types.Texture -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTextureV texture position color = with texture (\t -> with position (with color . c'drawTextureV t))

foreign import ccall safe "raylib.h &DrawTextureV"
  p'drawTextureV ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureEx_" c'drawTextureEx :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTextureEx :: Raylib.Types.Texture -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextureEx texture position rotation scale tint = with texture (\t -> with position (\p -> with tint (c'drawTextureEx t p (realToFrac rotation) (realToFrac scale))))

foreign import ccall safe "raylib.h &DrawTextureEx"
  p'drawTextureEx ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.Vector2 -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureRec_" c'drawTextureRec :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawTextureRec :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawTextureRec texture source position tint = with texture (\t -> with source (\s -> with position (with tint . c'drawTextureRec t s)))

foreign import ccall safe "raylib.h &DrawTextureRec"
  p'drawTextureRec ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTexturePro_" c'drawTexturePro :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTexturePro :: Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTexturePro texture source dest origin rotation tint = with texture (\t -> with source (\s -> with dest (\d -> with origin (\o -> with tint (c'drawTexturePro t s d o (realToFrac rotation))))))

foreign import ccall safe "raylib.h &DrawTexturePro"
  p'drawTexturePro ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureNPatch_" c'drawTextureNPatch :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.NPatchInfo -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTextureNPatch :: Raylib.Types.Texture -> Raylib.Types.NPatchInfo -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTextureNPatch texture nPatchInfo dest origin rotation tint = with texture (\t -> with nPatchInfo (\n -> with dest (\d -> with origin (\o -> with tint (c'drawTextureNPatch t n d o (realToFrac rotation))))))

foreign import ccall safe "raylib.h &DrawTextureNPatch"
  p'drawTextureNPatch ::
    FunPtr (Raylib.Types.Texture -> Raylib.Types.NPatchInfo -> Raylib.Types.Rectangle -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h Fade_" c'fade :: Ptr Raylib.Types.Color -> CFloat -> IO (Ptr Raylib.Types.Color)

fade :: Raylib.Types.Color -> Float -> Raylib.Types.Color
fade color alpha = unsafePerformIO $ with color (\c -> c'fade c (realToFrac alpha)) >>= pop

foreign import ccall safe "raylib.h &Fade"
  p'fade ::
    FunPtr (Raylib.Types.Color -> CFloat -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorToInt_" c'colorToInt :: Ptr Raylib.Types.Color -> IO CInt

colorToInt :: Raylib.Types.Color -> Int
colorToInt color = unsafePerformIO $ fromIntegral <$> with color c'colorToInt

foreign import ccall safe "raylib.h &ColorToInt"
  p'colorToInt ::
    FunPtr (Raylib.Types.Color -> IO CInt)

foreign import ccall safe "bindings.h ColorNormalize_" c'colorNormalize :: Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Vector4)

colorNormalize :: Raylib.Types.Color -> Raylib.Types.Vector4
colorNormalize color = unsafePerformIO $ with color c'colorNormalize >>= pop

foreign import ccall safe "raylib.h &ColorNormalize"
  p'colorNormalize ::
    FunPtr (Raylib.Types.Color -> IO Raylib.Types.Vector4)

foreign import ccall safe "bindings.h ColorFromNormalized_" c'colorFromNormalized :: Ptr Raylib.Types.Vector4 -> IO (Ptr Raylib.Types.Color)

colorFromNormalized :: Raylib.Types.Vector4 -> Raylib.Types.Color
colorFromNormalized normalized = unsafePerformIO $ with normalized c'colorFromNormalized >>= pop

foreign import ccall safe "raylib.h &ColorFromNormalized"
  p'colorFromNormalized ::
    FunPtr (Raylib.Types.Vector4 -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorToHSV_" c'colorToHSV :: Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Vector3)

colorToHSV :: Raylib.Types.Color -> Raylib.Types.Vector3
colorToHSV color = unsafePerformIO $ with color c'colorToHSV >>= pop

foreign import ccall safe "raylib.h &ColorToHSV"
  p'colorToHSV ::
    FunPtr (Raylib.Types.Color -> IO Raylib.Types.Vector3)

foreign import ccall safe "bindings.h ColorFromHSV_" c'colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Raylib.Types.Color)

colorFromHSV :: Float -> Float -> Float -> Raylib.Types.Color
colorFromHSV hue saturation value = unsafePerformIO $ c'colorFromHSV (realToFrac hue) (realToFrac saturation) (realToFrac value) >>= pop

foreign import ccall safe "raylib.h &ColorFromHSV"
  p'colorFromHSV ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorTint_" c'colorTint :: Ptr Color -> Ptr Color -> IO (Ptr Raylib.Types.Color)

colorTint :: Color -> Color -> Raylib.Types.Color
colorTint color tint = unsafePerformIO $ with color (with tint . c'colorTint) >>= pop

foreign import ccall safe "raylib.h &ColorTint"
  p'colorTint ::
    FunPtr (Color -> Color -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorBrightness_" c'colorBrightness :: Ptr Color -> CFloat -> IO (Ptr Raylib.Types.Color)

colorBrightness :: Color -> Float -> Raylib.Types.Color
colorBrightness color brightness = unsafePerformIO $ with color (\c -> c'colorBrightness c (realToFrac brightness)) >>= pop

foreign import ccall safe "raylib.h &ColorBrightness"
  p'colorBrightness ::
    FunPtr (Color -> CFloat -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorContrast_" c'colorContrast :: Ptr Color -> CFloat -> IO (Ptr Raylib.Types.Color)

colorContrast :: Color -> Float -> Raylib.Types.Color
colorContrast color contrast = unsafePerformIO $ with color (\c -> c'colorContrast c (realToFrac contrast)) >>= pop

foreign import ccall safe "raylib.h &ColorContrast"
  p'colorContrast ::
    FunPtr (Color -> CFloat -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorAlpha_" c'colorAlpha :: Ptr Raylib.Types.Color -> CFloat -> IO (Ptr Raylib.Types.Color)

colorAlpha :: Raylib.Types.Color -> Float -> Raylib.Types.Color
colorAlpha color alpha = unsafePerformIO $ with color (\c -> c'colorAlpha c (realToFrac alpha)) >>= pop

foreign import ccall safe "raylib.h &ColorAlpha"
  p'colorAlpha ::
    FunPtr (Raylib.Types.Color -> CFloat -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorAlphaBlend_" c'colorAlphaBlend :: Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Color)

colorAlphaBlend :: Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color
colorAlphaBlend dst src tint = unsafePerformIO $ with dst (\d -> with src (with tint . c'colorAlphaBlend d)) >>= pop

foreign import ccall safe "raylib.h &ColorAlphaBlend"
  p'colorAlphaBlend ::
    FunPtr (Raylib.Types.Color -> Raylib.Types.Color -> Raylib.Types.Color -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h GetColor_" c'getColor :: CUInt -> IO (Ptr Raylib.Types.Color)

getColor :: Integer -> Raylib.Types.Color
getColor hexValue = unsafePerformIO $ c'getColor (fromIntegral hexValue) >>= pop

foreign import ccall safe "raylib.h &GetColor"
  p'getColor ::
    FunPtr (CUInt -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h GetPixelColor_" c'getPixelColor :: Ptr () -> CInt -> IO (Ptr Raylib.Types.Color)

getPixelColor :: Ptr () -> PixelFormat -> IO Raylib.Types.Color
getPixelColor srcPtr format = c'getPixelColor srcPtr (fromIntegral $ fromEnum format) >>= pop

foreign import ccall safe "raylib.h &GetPixelColor"
  p'getPixelColor ::
    FunPtr (Ptr () -> CInt -> IO Raylib.Types.Color)

foreign import ccall safe "bindings.h SetPixelColor_" c'setPixelColor :: Ptr () -> Ptr Raylib.Types.Color -> CInt -> IO ()

setPixelColor :: Ptr () -> Raylib.Types.Color -> PixelFormat -> IO ()
setPixelColor dstPtr color format = with color (\c -> c'setPixelColor dstPtr c (fromIntegral $ fromEnum format))

foreign import ccall safe "raylib.h &SetPixelColor"
  p'setPixelColor ::
    FunPtr (Ptr () -> Raylib.Types.Color -> CInt -> IO ())

foreign import ccall safe "raylib.h GetPixelDataSize"
  c'getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

getPixelDataSize :: Int -> Int -> PixelFormat -> Int
getPixelDataSize width height format = unsafePerformIO (fromIntegral <$> c'getPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format))

foreign import ccall safe "raylib.h &GetPixelDataSize"
  p'getPixelDataSize ::
    FunPtr (CInt -> CInt -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetFontDefault_" c'getFontDefault :: IO (Ptr Raylib.Types.Font)

getFontDefault :: IO Raylib.Types.Font
getFontDefault = c'getFontDefault >>= pop

foreign import ccall safe "raylib.h &GetFontDefault"
  p'getFontDefault ::
    FunPtr (IO Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFont_" c'loadFont :: CString -> IO (Ptr Raylib.Types.Font)

loadFont :: String -> IO Raylib.Types.Font
loadFont fileName = withCString fileName c'loadFont >>= pop

foreign import ccall safe "raylib.h &LoadFont"
  p'loadFont ::
    FunPtr (CString -> IO Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFontEx_" c'loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Raylib.Types.Font)

loadFontEx :: String -> Int -> [Int] -> Int -> IO Raylib.Types.Font
loadFontEx fileName fontSize fontChars glyphCount = withCString fileName (\f -> withArray (map fromIntegral fontChars) (\c -> c'loadFontEx f (fromIntegral fontSize) c (fromIntegral glyphCount))) >>= pop

foreign import ccall safe "raylib.h &LoadFontEx"
  p'loadFontEx ::
    FunPtr (CString -> CInt -> Ptr CInt -> CInt -> IO Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFontFromImage_" c'loadFontFromImage :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> CInt -> IO (Ptr Raylib.Types.Font)

loadFontFromImage :: Raylib.Types.Image -> Raylib.Types.Color -> Int -> IO Raylib.Types.Font
loadFontFromImage image key firstChar = with image (\i -> with key (\k -> c'loadFontFromImage i k (fromIntegral firstChar))) >>= pop

foreign import ccall safe "raylib.h &LoadFontFromImage"
  p'loadFontFromImage ::
    FunPtr (Raylib.Types.Image -> Raylib.Types.Color -> CInt -> IO Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFontFromMemory_" c'loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Raylib.Types.Font)

loadFontFromMemory :: String -> [Integer] -> Int -> [Int] -> Int -> IO Raylib.Types.Font
loadFontFromMemory fileType fileData fontSize fontChars glyphCount = withCString fileType (\t -> withArrayLen (map fromIntegral fileData) (\size d -> withArray (map fromIntegral fontChars) (\c -> c'loadFontFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)) (fromIntegral fontSize) c (fromIntegral glyphCount)))) >>= pop

foreign import ccall safe "raylib.h &LoadFontFromMemory"
  p'loadFontFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO Raylib.Types.Font)

foreign import ccall safe "raylib.h LoadFontData"
  c'loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.GlyphInfo)

loadFontData :: [Integer] -> Int -> [Int] -> Int -> FontType -> IO Raylib.Types.GlyphInfo
loadFontData fileData fontSize fontChars glyphCount fontType = withArrayLen (map fromIntegral fileData) (\size d -> withArray (map fromIntegral fontChars) (\c -> c'loadFontData d (fromIntegral $ size * sizeOf (0 :: CUChar)) (fromIntegral fontSize) c (fromIntegral glyphCount) (fromIntegral $ fromEnum fontType))) >>= pop

foreign import ccall safe "raylib.h &LoadFontData"
  p'loadFontData ::
    FunPtr (Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.GlyphInfo))

foreign import ccall safe "bindings.h GenImageFontAtlas_" c'genImageFontAtlas :: Ptr Raylib.Types.GlyphInfo -> Ptr (Ptr Raylib.Types.Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.Image)
genImageFontAtlas :: [Raylib.Types.GlyphInfo] -> [[Raylib.Types.Rectangle]] -> Int -> Int -> Int -> Int -> IO Raylib.Types.Image
genImageFontAtlas chars recs glyphCount fontSize padding packMethod = withArray chars (\c -> withArray2D recs (\r -> c'genImageFontAtlas c r (fromIntegral glyphCount) (fromIntegral fontSize) (fromIntegral padding) (fromIntegral packMethod))) >>= pop

foreign import ccall safe "raylib.h &GenImageFontAtlas"
  p'genImageFontAtlas ::
    FunPtr (Ptr Raylib.Types.GlyphInfo -> Ptr (Ptr Raylib.Types.Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO Raylib.Types.Image)

foreign import ccall safe "raylib.h UnloadFontData"
  c'unloadFontData ::
    Ptr Raylib.Types.GlyphInfo -> CInt -> IO ()

unloadFontData :: [Raylib.Types.GlyphInfo] -> IO ()
unloadFontData glyphs = withArrayLen glyphs (\size g -> c'unloadFontData g (fromIntegral size))

foreign import ccall safe "raylib.h &UnloadFontData"
  p'unloadFontData ::
    FunPtr (Ptr Raylib.Types.GlyphInfo -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadFont_" c'unloadFont :: Ptr Raylib.Types.Font -> IO ()

unloadFont :: Raylib.Types.Font -> IO ()
unloadFont font = with font c'unloadFont

foreign import ccall safe "raylib.h &UnloadFont"
  p'unloadFont ::
    FunPtr (Raylib.Types.Font -> IO ())

foreign import ccall safe "bindings.h ExportFontAsCode_" c'exportFontAsCode :: Ptr Raylib.Types.Font -> CString -> IO CBool

exportFontAsCode :: Raylib.Types.Font -> String -> IO Bool
exportFontAsCode font fileName = toBool <$> with font (withCString fileName . c'exportFontAsCode)

foreign import ccall safe "raylib.h &ExportFontAsCode"
  p'exportFontAsCode ::
    FunPtr (Raylib.Types.Font -> CString -> IO CInt)

foreign import ccall safe "raylib.h DrawFPS"
  c'drawFPS ::
    CInt -> CInt -> IO ()

drawFPS :: Int -> Int -> IO ()
drawFPS x y = c'drawFPS (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &DrawFPS"
  p'drawFPS ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h DrawText_" c'drawText :: CString -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawText :: String -> Int -> Int -> Int -> Raylib.Types.Color -> IO ()
drawText text x y fontSize color = withCString text (\t -> with color (c'drawText t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize)))

foreign import ccall safe "raylib.h &DrawText"
  p'drawText ::
    FunPtr (CString -> CInt -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextEx_" c'drawTextEx :: Ptr Raylib.Types.Font -> CString -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTextEx :: Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextEx font text position fontSize spacing tint = with font (\f -> withCString text (\t -> with position (\p -> with tint (c'drawTextEx f t p (realToFrac fontSize) (realToFrac spacing)))))

foreign import ccall safe "raylib.h &DrawTextEx"
  p'drawTextEx ::
    FunPtr (Raylib.Types.Font -> CString -> Raylib.Types.Vector2 -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextPro_" c'drawTextPro :: Ptr Raylib.Types.Font -> CString -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTextPro :: Raylib.Types.Font -> String -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextPro font text position origin rotation fontSize spacing tint = with font (\f -> withCString text (\t -> with position (\p -> with origin (\o -> with tint (c'drawTextPro f t p o (realToFrac rotation) (realToFrac fontSize) (realToFrac spacing))))))

foreign import ccall safe "raylib.h &DrawTextPro"
  p'drawTextPro ::
    FunPtr (Raylib.Types.Font -> CString -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextCodepoint_" c'drawTextCodepoint :: Ptr Raylib.Types.Font -> CInt -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTextCodepoint :: Raylib.Types.Font -> Int -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawTextCodepoint font codepoint position fontSize tint = with font (\f -> with position (\p -> with tint (c'drawTextCodepoint f (fromIntegral codepoint) p (realToFrac fontSize))))

foreign import ccall safe "raylib.h &DrawTextCodepoint"
  p'drawTextCodepoint ::
    FunPtr (Raylib.Types.Font -> CInt -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTextCodepoints_" c'drawTextCodepoints :: Ptr Raylib.Types.Font -> Ptr CInt -> CInt -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawTextCodepoints :: Raylib.Types.Font -> [Int] -> Raylib.Types.Vector2 -> Float -> Float -> Raylib.Types.Color -> IO ()
drawTextCodepoints font codepoints position fontSize spacing tint = with font (\f -> withArrayLen (map fromIntegral codepoints) (\count cp -> with position (\p -> with tint (c'drawTextCodepoints f cp (fromIntegral count) p (realToFrac fontSize) (realToFrac spacing)))))

foreign import ccall safe "raylib.h &DrawTextCodepoints"
  p'drawTextCodepoints ::
    FunPtr (Raylib.Types.Font -> Ptr CInt -> CInt -> Raylib.Types.Vector2 -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "raylib.h MeasureText"
  c'measureText ::
    CString -> CInt -> IO CInt

measureText :: String -> Int -> IO Int
measureText text fontSize = fromIntegral <$> withCString text (\t -> c'measureText t (fromIntegral fontSize))

foreign import ccall safe "raylib.h &MeasureText"
  p'measureText ::
    FunPtr (CString -> CInt -> IO CInt)

foreign import ccall safe "bindings.h MeasureTextEx_" c'measureTextEx :: Ptr Raylib.Types.Font -> CString -> CFloat -> CFloat -> IO (Ptr Raylib.Types.Vector2)

measureTextEx :: Raylib.Types.Font -> String -> Float -> Float -> IO Raylib.Types.Vector2
measureTextEx font text fontSize spacing = with font (\f -> withCString text (\t -> c'measureTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

foreign import ccall safe "raylib.h &MeasureTextEx"
  p'measureTextEx ::
    FunPtr (Raylib.Types.Font -> CString -> CFloat -> CFloat -> IO Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetGlyphIndex_" c'getGlyphIndex :: Ptr Raylib.Types.Font -> CInt -> IO CInt

getGlyphIndex :: Raylib.Types.Font -> Int -> IO Int
getGlyphIndex font codepoint = fromIntegral <$> with font (\f -> c'getGlyphIndex f (fromIntegral codepoint))

foreign import ccall safe "raylib.h &GetGlyphIndex"
  p'getGlyphIndex ::
    FunPtr (Raylib.Types.Font -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetGlyphInfo_" c'getGlyphInfo :: Ptr Raylib.Types.Font -> CInt -> IO (Ptr Raylib.Types.GlyphInfo)

getGlyphInfo :: Raylib.Types.Font -> Int -> IO Raylib.Types.GlyphInfo
getGlyphInfo font codepoint = with font (\f -> c'getGlyphInfo f (fromIntegral codepoint)) >>= pop

foreign import ccall safe "raylib.h &GetGlyphInfo"
  p'getGlyphInfo ::
    FunPtr (Raylib.Types.Font -> CInt -> IO Raylib.Types.GlyphInfo)

foreign import ccall safe "bindings.h GetGlyphAtlasRec_" c'getGlyphAtlasRec :: Ptr Raylib.Types.Font -> CInt -> IO (Ptr Raylib.Types.Rectangle)

getGlyphAtlasRec :: Raylib.Types.Font -> Int -> IO Raylib.Types.Rectangle
getGlyphAtlasRec font codepoint = with font (\f -> c'getGlyphAtlasRec f (fromIntegral codepoint)) >>= pop

foreign import ccall safe "raylib.h &GetGlyphAtlasRec"
  p'getGlyphAtlasRec ::
    FunPtr (Raylib.Types.Font -> CInt -> IO Raylib.Types.Rectangle)

foreign import ccall safe "raylib.h LoadUTF8"
  c'loadUTF8 ::
    Ptr CInt -> CInt -> IO CString

loadUTF8 :: [Integer] -> IO String
loadUTF8 codepoints =
  withArrayLen
    (map fromIntegral codepoints)
    ( \size c ->
        c'loadUTF8 c (fromIntegral size)
    )
    >>= ( \s -> do
            val <- peekCString s
            unloadUTF8 s
            return val
        )

foreign import ccall safe "raylib.h &LoadUTF8"
  p'loadUTF8 ::
    FunPtr (Ptr CInt -> CInt -> IO CString)

foreign import ccall safe "raylib.h UnloadUTF8"
  unloadUTF8 ::
    CString -> IO ()

foreign import ccall safe "raylib.h &UnloadUTF8"
  p'unloadUTF8 ::
    FunPtr (CString -> IO ())

foreign import ccall safe "raylib.h LoadCodepoints"
  c'loadCodepoints ::
    CString -> Ptr CInt -> IO (Ptr CInt)

loadCodepoints :: String -> IO [Int]
loadCodepoints text =
  withCString
    text
    ( \t ->
        with
          0
          ( \n -> do
              res <- c'loadCodepoints t n
              num <- peek n
              arr <- peekArray (fromIntegral num) res
              unloadCodepoints res
              return $ fromIntegral <$> arr
          )
    )

foreign import ccall safe "raylib.h &LoadCodepoints"
  p'loadCodepoints ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr CInt))

foreign import ccall safe "raylib.h UnloadCodepoints"
  unloadCodepoints ::
    Ptr CInt -> IO ()

foreign import ccall safe "raylib.h &UnloadCodepoints"
  p'unloadCodepoints ::
    FunPtr (Ptr CInt -> IO ())

foreign import ccall safe "raylib.h GetCodepointCount"
  c'getCodepointCount ::
    CString -> IO CInt

getCodepointCount :: String -> IO Int
getCodepointCount text = fromIntegral <$> withCString text c'getCodepointCount

foreign import ccall safe "raylib.h &GetCodepointCount"
  p'getCodepointCount ::
    FunPtr (CString -> IO CInt)

-- | Deprecated, use `getCodepointNext`
foreign import ccall safe "raylib.h GetCodepoint"
  getCodepoint ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "raylib.h &GetCodepoint"
  p'getCodepoint ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall safe "raylib.h GetCodepointNext"
  c'getCodepointNext ::
    CString -> Ptr CInt -> IO CInt

getCodepointNext :: String -> IO (Int, Int)
getCodepointNext text =
  withCString
    text
    ( \t ->
        with
          0
          ( \n ->
              do
                res <- c'getCodepointNext t n
                num <- peek n
                return (fromIntegral res, fromIntegral num)
          )
    )

foreign import ccall safe "raylib.h &GetCodepointNext"
  p'getCodepointNext ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall safe "raylib.h GetCodepointPrevious"
  c'getCodepointPrevious ::
    CString -> Ptr CInt -> IO CInt

getCodepointPrevious :: String -> IO (Int, Int)
getCodepointPrevious text =
  withCString
    text
    ( \t ->
        with
          0
          ( \n ->
              do
                res <- c'getCodepointPrevious t n
                num <- peek n
                return (fromIntegral res, fromIntegral num)
          )
    )

foreign import ccall safe "raylib.h &GetCodepointPrevious"
  p'getCodepointPrevious ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall safe "raylib.h CodepointToUTF8"
  c'codepointToUTF8 ::
    CInt -> Ptr CInt -> IO CString

codepointToUTF8 :: Int -> IO String
codepointToUTF8 codepoint = with 0 (c'codepointToUTF8 $ fromIntegral codepoint) >>= peekCString

foreign import ccall safe "raylib.h &CodepointToUTF8"
  p'codepointToUTF8 ::
    FunPtr (CInt -> Ptr CInt -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextCopy"
  textCopy ::
    CString -> CString -> IO CInt

foreign import ccall safe "raylib.h &TextCopy"
  p'textCopy ::
    FunPtr (CString -> CString -> IO CInt)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextIsEqual"
  textIsEqual ::
    CString -> CString -> IO CInt

foreign import ccall safe "raylib.h &TextIsEqual"
  p'textIsEqual ::
    FunPtr (CString -> CString -> IO CInt)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextLength"
  textLength ::
    CString -> IO CUInt

foreign import ccall safe "raylib.h &TextLength"
  p'textLength ::
    FunPtr (CString -> IO CUInt)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextFormat"
  textFormat ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextFormat"
  p'textFormat ::
    FunPtr (CString -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextSubtext"
  textSubtext ::
    CString -> CInt -> CInt -> IO CString

foreign import ccall safe "raylib.h &TextSubtext"
  p'textSubtext ::
    FunPtr (CString -> CInt -> CInt -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextReplace"
  textReplace ::
    CString -> CString -> CString -> IO CString

foreign import ccall safe "raylib.h &TextReplace"
  p'textReplace ::
    FunPtr (CString -> CString -> CString -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextInsert"
  textInsert ::
    CString -> CString -> CInt -> IO CString

foreign import ccall safe "raylib.h &TextInsert"
  p'textInsert ::
    FunPtr (CString -> CString -> CInt -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextJoin"
  textJoin ::
    Ptr CString -> CInt -> CString -> IO CString

foreign import ccall safe "raylib.h &TextJoin"
  p'textJoin ::
    FunPtr (Ptr CString -> CInt -> CString -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextSplit"
  textSplit ::
    CString -> CChar -> Ptr CInt -> IO (Ptr CString)

foreign import ccall safe "raylib.h &TextSplit"
  p'textSplit ::
    FunPtr (CString -> CChar -> Ptr CInt -> IO (Ptr CString))

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextAppend"
  textAppend ::
    CString -> CString -> Ptr CInt -> IO ()

foreign import ccall safe "raylib.h &TextAppend"
  p'textAppend ::
    FunPtr (CString -> CString -> Ptr CInt -> IO ())

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextFindIndex"
  textFindIndex ::
    CString -> CString -> IO CInt

foreign import ccall safe "raylib.h &TextFindIndex"
  p'textFindIndex ::
    FunPtr (CString -> CString -> IO CInt)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextToUpper"
  textToUpper ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextToUpper"
  p'textToUpper ::
    FunPtr (CString -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextToLower"
  textToLower ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextToLower"
  p'textToLower ::
    FunPtr (CString -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextToPascal"
  textToPascal ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextToPascal"
  p'textToPascal ::
    FunPtr (CString -> IO CString)

-- | Not required in Haskell
foreign import ccall safe "raylib.h TextToInteger"
  textToInteger ::
    CString -> IO CInt

foreign import ccall safe "raylib.h &TextToInteger"
  p'textToInteger ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "bindings.h DrawLine3D_" c'drawLine3D :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawLine3D :: Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawLine3D start end color = with start (\s -> with end (with color . c'drawLine3D s))

foreign import ccall safe "raylib.h &DrawLine3D"
  p'drawLine3D ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawPoint3D_" c'drawPoint3D :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawPoint3D :: Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawPoint3D point color = with point (with color . c'drawPoint3D)

foreign import ccall safe "raylib.h &DrawPoint3D"
  p'drawPoint3D ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCircle3D_" c'drawCircle3D :: Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawCircle3D :: Raylib.Types.Vector3 -> Float -> Raylib.Types.Vector3 -> Float -> Raylib.Types.Color -> IO ()
drawCircle3D center radius rotationAxis rotationAngle color = with center (\c -> with rotationAxis (\r -> with color (c'drawCircle3D c (realToFrac radius) r (realToFrac rotationAngle))))

foreign import ccall safe "raylib.h &DrawCircle3D"
  p'drawCircle3D ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangle3D_" c'drawTriangle3D :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawTriangle3D :: Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawTriangle3D v1 v2 v3 color = with v1 (\p1 -> with v2 (\p2 -> with v3 (with color . c'drawTriangle3D p1 p2)))

foreign import ccall safe "raylib.h &DrawTriangle3D"
  p'drawTriangle3D ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleStrip3D_" c'drawTriangleStrip3D :: Ptr Raylib.Types.Vector3 -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawTriangleStrip3D :: [Raylib.Types.Vector3] -> Int -> Raylib.Types.Color -> IO ()
drawTriangleStrip3D points pointCount color = withArray points (\p -> with color (c'drawTriangleStrip3D p (fromIntegral pointCount)))

foreign import ccall safe "raylib.h &DrawTriangleStrip3D"
  p'drawTriangleStrip3D ::
    FunPtr (Ptr Raylib.Types.Vector3 -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCube_" c'drawCube :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawCube :: Raylib.Types.Vector3 -> Float -> Float -> Float -> Raylib.Types.Color -> IO ()
drawCube position width height length color = with position (\p -> with color (c'drawCube p (realToFrac width) (realToFrac height) (realToFrac length)))

foreign import ccall safe "raylib.h &DrawCube"
  p'drawCube ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeV_" c'drawCubeV :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawCubeV :: Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawCubeV position size color = with position (\p -> with size (with color . c'drawCubeV p))

foreign import ccall safe "raylib.h &DrawCubeV"
  p'drawCubeV ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeWires_" c'drawCubeWires :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawCubeWires :: Raylib.Types.Vector3 -> Float -> Float -> Float -> Raylib.Types.Color -> IO ()
drawCubeWires position width height length color = with position (\p -> with color (c'drawCubeWires p (realToFrac width) (realToFrac height) (realToFrac length)))

foreign import ccall safe "raylib.h &DrawCubeWires"
  p'drawCubeWires ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeWiresV_" c'drawCubeWiresV :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawCubeWiresV :: Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawCubeWiresV position size color = with position (\p -> with size (with color . c'drawCubeWiresV p))

foreign import ccall safe "raylib.h &DrawCubeWiresV"
  p'drawCubeWiresV ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawSphere_" c'drawSphere :: Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawSphere :: Raylib.Types.Vector3 -> Float -> Raylib.Types.Color -> IO ()
drawSphere position radius color = with position (\p -> with color (c'drawSphere p (realToFrac radius)))

foreign import ccall safe "raylib.h &DrawSphere"
  p'drawSphere ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawSphereEx_" c'drawSphereEx :: Ptr Raylib.Types.Vector3 -> CFloat -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawSphereEx :: Raylib.Types.Vector3 -> Float -> Int -> Int -> Raylib.Types.Color -> IO ()
drawSphereEx position radius rings slices color = with position (\p -> with color (c'drawSphereEx p (realToFrac radius) (fromIntegral rings) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawSphereEx"
  p'drawSphereEx ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawSphereWires_" c'drawSphereWires :: Ptr Raylib.Types.Vector3 -> CFloat -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawSphereWires :: Raylib.Types.Vector3 -> Float -> Int -> Int -> Raylib.Types.Color -> IO ()
drawSphereWires position radius rings slices color = with position (\p -> with color (c'drawSphereWires p (realToFrac radius) (fromIntegral rings) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawSphereWires"
  p'drawSphereWires ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> CInt -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinder_" c'drawCylinder :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawCylinder :: Raylib.Types.Vector3 -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCylinder position radiusTop radiusBottom height slices color = with position (\p -> with color (c'drawCylinder p (realToFrac radiusTop) (realToFrac radiusBottom) (realToFrac height) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawCylinder"
  p'drawCylinder ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderEx_" c'drawCylinderEx :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawCylinderEx :: Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCylinderEx start end startRadius endRadius sides color = with start (\s -> with end (\e -> with color (c'drawCylinderEx s e (realToFrac startRadius) (realToFrac endRadius) (fromIntegral sides))))

foreign import ccall safe "raylib.h &DrawCylinderEx"
  p'drawCylinderEx ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderWires_" c'drawCylinderWires :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawCylinderWires :: Raylib.Types.Vector3 -> Float -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCylinderWires position radiusTop radiusBottom height slices color = with position (\p -> with color (c'drawCylinderWires p (realToFrac radiusTop) (realToFrac radiusBottom) (realToFrac height) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawCylinderWires"
  p'drawCylinderWires ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderWiresEx_" c'drawCylinderWiresEx :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

drawCylinderWiresEx :: Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Float -> Float -> Int -> Raylib.Types.Color -> IO ()
drawCylinderWiresEx start end startRadius endRadius sides color = with start (\s -> with end (\e -> with color (c'drawCylinderWiresEx s e (realToFrac startRadius) (realToFrac endRadius) (fromIntegral sides))))

foreign import ccall safe "raylib.h &DrawCylinderWiresEx"
  p'drawCylinderWiresEx ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> CFloat -> CFloat -> CInt -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawCapsule_" c'drawCapsule :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

drawCapsule :: Vector3 -> Vector3 -> CFloat -> Int -> Int -> Color -> IO ()
drawCapsule start end radius slices rings color = with start (\s -> with end (\e -> with color (c'drawCapsule s e (realToFrac radius) (fromIntegral slices) (fromIntegral rings))))

foreign import ccall safe "raylib.h &DrawCapsule"
  p'drawCapsule ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCapsuleWires_" c'drawCapsuleWires :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

drawCapsuleWires :: Vector3 -> Vector3 -> CFloat -> Int -> Int -> Color -> IO ()
drawCapsuleWires start end radius slices rings color = with start (\s -> with end (\e -> with color (c'drawCapsuleWires s e (realToFrac radius) (fromIntegral slices) (fromIntegral rings))))

foreign import ccall safe "raylib.h &DrawCapsuleWires"
  p'drawCapsuleWires ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPlane_" c'drawPlane :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawPlane :: Raylib.Types.Vector3 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawPlane center size color = with center (\c -> with size (with color . c'drawPlane c))

foreign import ccall safe "raylib.h &DrawPlane"
  p'drawPlane ::
    FunPtr (Raylib.Types.Vector3 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawRay_" c'drawRay :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Color -> IO ()

drawRay :: Raylib.Types.Ray -> Raylib.Types.Color -> IO ()
drawRay ray color = with ray (with color . c'drawRay)

foreign import ccall safe "raylib.h &DrawRay"
  p'drawRay ::
    FunPtr (Raylib.Types.Ray -> Raylib.Types.Color -> IO ())

foreign import ccall safe "raylib.h DrawGrid"
  c'drawGrid ::
    CInt -> CFloat -> IO ()

drawGrid :: Int -> Float -> IO ()
drawGrid slices spacing = c'drawGrid (fromIntegral slices) (realToFrac spacing)

foreign import ccall safe "raylib.h &DrawGrid"
  p'drawGrid ::
    FunPtr (CInt -> CFloat -> IO ())

foreign import ccall safe "bindings.h LoadModel_" c'loadModel :: CString -> IO (Ptr Raylib.Types.Model)

loadModel :: String -> IO Raylib.Types.Model
loadModel fileName = withCString fileName c'loadModel >>= pop

foreign import ccall safe "raylib.h &LoadModel"
  p'loadModel ::
    FunPtr (CString -> IO Raylib.Types.Model)

foreign import ccall safe "bindings.h LoadModelFromMesh_" c'loadModelFromMesh :: Ptr Raylib.Types.Mesh -> IO (Ptr Raylib.Types.Model)

loadModelFromMesh :: Raylib.Types.Mesh -> IO Raylib.Types.Model
loadModelFromMesh mesh = with mesh c'loadModelFromMesh >>= pop

foreign import ccall safe "raylib.h &LoadModelFromMesh"
  p'loadModelFromMesh ::
    FunPtr (Raylib.Types.Mesh -> IO Raylib.Types.Model)

foreign import ccall safe "bindings.h UnloadModel_" c'unloadModel :: Ptr Raylib.Types.Model -> IO ()

unloadModel :: Raylib.Types.Model -> IO ()
unloadModel model = with model c'unloadModel

foreign import ccall safe "raylib.h &UnloadModel"
  p'unloadModel ::
    FunPtr (Raylib.Types.Model -> IO ())

foreign import ccall safe "bindings.h UnloadModelKeepMeshes_" c'unloadModelKeepMeshes :: Ptr Raylib.Types.Model -> IO ()

unloadModelKeepMeshes :: Raylib.Types.Model -> IO ()
unloadModelKeepMeshes model = with model c'unloadModelKeepMeshes

foreign import ccall safe "raylib.h &UnloadModelKeepMeshes"
  p'unloadModelKeepMeshes ::
    FunPtr (Raylib.Types.Model -> IO ())

foreign import ccall safe "bindings.h GetModelBoundingBox_" c'getModelBoundingBox :: Ptr Raylib.Types.Model -> IO (Ptr Raylib.Types.BoundingBox)

getModelBoundingBox :: Raylib.Types.Model -> IO Raylib.Types.BoundingBox
getModelBoundingBox model = with model c'getModelBoundingBox >>= pop

foreign import ccall safe "raylib.h &GetModelBoundingBox"
  p'getModelBoundingBox ::
    FunPtr (Raylib.Types.Model -> IO Raylib.Types.BoundingBox)

foreign import ccall safe "bindings.h DrawModel_" c'drawModel :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawModel :: Raylib.Types.Model -> Raylib.Types.Vector3 -> Float -> Raylib.Types.Color -> IO ()
drawModel model position scale tint = with model (\m -> with position (\p -> with tint (c'drawModel m p (realToFrac scale))))

foreign import ccall safe "raylib.h &DrawModel"
  p'drawModel ::
    FunPtr (Raylib.Types.Model -> Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawModelEx_" c'drawModelEx :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawModelEx :: Raylib.Types.Model -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Float -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawModelEx model position rotationAxis rotationAngle scale tint = with model (\m -> with position (\p -> with rotationAxis (\r -> with scale (with tint . c'drawModelEx m p r (realToFrac rotationAngle)))))

foreign import ccall safe "raylib.h &DrawModelEx"
  p'drawModelEx ::
    FunPtr (Raylib.Types.Model -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawModelWires_" c'drawModelWires :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawModelWires :: Raylib.Types.Model -> Raylib.Types.Vector3 -> Float -> Raylib.Types.Color -> IO ()
drawModelWires model position scale tint = with model (\m -> with position (\p -> with tint (c'drawModelWires m p (realToFrac scale))))

foreign import ccall safe "raylib.h &DrawModelWires"
  p'drawModelWires ::
    FunPtr (Raylib.Types.Model -> Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawModelWiresEx_" c'drawModelWiresEx :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

drawModelWiresEx :: Raylib.Types.Model -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Float -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ()
drawModelWiresEx model position rotationAxis rotationAngle scale tint = with model (\m -> with position (\p -> with rotationAxis (\r -> with scale (with tint . c'drawModelWiresEx m p r (realToFrac rotationAngle)))))

foreign import ccall safe "raylib.h &DrawModelWiresEx"
  p'drawModelWiresEx ::
    FunPtr (Raylib.Types.Model -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Vector3 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawBoundingBox_" c'drawBoundingBox :: Ptr Raylib.Types.BoundingBox -> Ptr Raylib.Types.Color -> IO ()

drawBoundingBox :: Raylib.Types.BoundingBox -> Raylib.Types.Color -> IO ()
drawBoundingBox box color = with box (with color . c'drawBoundingBox)

foreign import ccall safe "raylib.h &DrawBoundingBox"
  p'drawBoundingBox ::
    FunPtr (Raylib.Types.BoundingBox -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboard_" c'drawBillboard :: Ptr Raylib.Types.Camera3D -> Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawBillboard :: Raylib.Types.Camera3D -> Raylib.Types.Texture -> Raylib.Types.Vector3 -> Float -> Raylib.Types.Color -> IO ()
drawBillboard camera texture position size tint = with camera (\c -> with texture (\t -> with position (\p -> with tint (c'drawBillboard c t p (realToFrac size)))))

foreign import ccall safe "raylib.h &DrawBillboard"
  p'drawBillboard ::
    FunPtr (Raylib.Types.Camera3D -> Raylib.Types.Texture -> Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboardRec_" c'drawBillboardRec :: Ptr Raylib.Types.Camera3D -> Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

drawBillboardRec :: Raylib.Types.Camera3D -> Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector3 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ()
drawBillboardRec camera texture source position size tint = with camera (\c -> with texture (\t -> with source (\s -> with position (\p -> with size (with tint . c'drawBillboardRec c t s p)))))

foreign import ccall safe "raylib.h &DrawBillboardRec"
  p'drawBillboardRec ::
    FunPtr (Raylib.Types.Camera3D -> Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector3 -> Raylib.Types.Vector2 -> Raylib.Types.Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboardPro_" c'drawBillboardPro :: Ptr Raylib.Types.Camera3D -> Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

drawBillboardPro :: Raylib.Types.Camera3D -> Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> Float -> Raylib.Types.Color -> IO ()
drawBillboardPro camera texture source position up size origin rotation tint = with camera (\c -> with texture (\t -> with source (\s -> with position (\p -> with up (\u -> with size (\sz -> with origin (\o -> with tint (c'drawBillboardPro c t s p u sz o (realToFrac rotation)))))))))

foreign import ccall safe "raylib.h &DrawBillboardPro"
  p'drawBillboardPro ::
    FunPtr (Raylib.Types.Camera3D -> Raylib.Types.Texture -> Raylib.Types.Rectangle -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector2 -> Raylib.Types.Vector2 -> CFloat -> Raylib.Types.Color -> IO ())

foreign import ccall safe "raylib.h UploadMesh"
  c'uploadMesh ::
    Ptr Raylib.Types.Mesh -> CInt -> IO ()

uploadMesh :: Raylib.Types.Mesh -> Bool -> IO Raylib.Types.Mesh
uploadMesh mesh dynamic = with mesh (\m -> c'uploadMesh m (fromBool dynamic) >> peek m)

foreign import ccall safe "raylib.h &UploadMesh"
  p'uploadMesh ::
    FunPtr (Ptr Raylib.Types.Mesh -> CInt -> IO ())

foreign import ccall safe "bindings.h UpdateMeshBuffer_" c'updateMeshBuffer :: Ptr Raylib.Types.Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()

updateMeshBuffer :: Raylib.Types.Mesh -> Int -> Ptr () -> Int -> Int -> IO ()
updateMeshBuffer mesh index dataValue dataSize offset = with mesh (\m -> c'updateMeshBuffer m (fromIntegral index) dataValue (fromIntegral dataSize) (fromIntegral offset))

foreign import ccall safe "raylib.h &UpdateMeshBuffer"
  p'updateMeshBuffer ::
    FunPtr (Raylib.Types.Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadMesh_" c'unloadMesh :: Ptr Raylib.Types.Mesh -> IO ()

unloadMesh :: Raylib.Types.Mesh -> IO ()
unloadMesh mesh = with mesh c'unloadMesh

foreign import ccall safe "raylib.h &UnloadMesh"
  p'unloadMesh ::
    FunPtr (Raylib.Types.Mesh -> IO ())

foreign import ccall safe "bindings.h DrawMesh_" c'drawMesh :: Ptr Raylib.Types.Mesh -> Ptr Raylib.Types.Material -> Ptr Raylib.Types.Matrix -> IO ()

drawMesh :: Raylib.Types.Mesh -> Raylib.Types.Material -> Raylib.Types.Matrix -> IO ()
drawMesh mesh material transform = with mesh (\m -> with material (with transform . c'drawMesh m))

foreign import ccall safe "raylib.h &DrawMesh"
  p'drawMesh ::
    FunPtr (Raylib.Types.Mesh -> Raylib.Types.Material -> Raylib.Types.Matrix -> IO ())

foreign import ccall safe "bindings.h DrawMeshInstanced_" c'drawMeshInstanced :: Ptr Raylib.Types.Mesh -> Ptr Raylib.Types.Material -> Ptr Raylib.Types.Matrix -> CInt -> IO ()

drawMeshInstanced :: Raylib.Types.Mesh -> Raylib.Types.Material -> [Raylib.Types.Matrix] -> IO ()
drawMeshInstanced mesh material transforms = with mesh (\m -> with material (\mat -> withArrayLen transforms (\size t -> c'drawMeshInstanced m mat t (fromIntegral size))))

foreign import ccall safe "raylib.h &DrawMeshInstanced"
  p'drawMeshInstanced ::
    FunPtr (Raylib.Types.Mesh -> Raylib.Types.Material -> Ptr Raylib.Types.Matrix -> CInt -> IO ())

foreign import ccall safe "bindings.h ExportMesh_" c'exportMesh :: Ptr Raylib.Types.Mesh -> CString -> IO CBool

exportMesh :: Raylib.Types.Mesh -> String -> IO Bool
exportMesh mesh fileName = toBool <$> with mesh (withCString fileName . c'exportMesh)

foreign import ccall safe "raylib.h &ExportMesh"
  p'exportMesh ::
    FunPtr (Raylib.Types.Mesh -> CString -> IO CInt)

foreign import ccall safe "bindings.h GetMeshBoundingBox_" c'getMeshBoundingBox :: Ptr Raylib.Types.Mesh -> IO (Ptr Raylib.Types.BoundingBox)

getMeshBoundingBox :: Raylib.Types.Mesh -> IO Raylib.Types.BoundingBox
getMeshBoundingBox mesh = with mesh c'getMeshBoundingBox >>= pop

foreign import ccall safe "raylib.h &GetMeshBoundingBox"
  p'getMeshBoundingBox ::
    FunPtr (Raylib.Types.Mesh -> IO Raylib.Types.BoundingBox)

foreign import ccall safe "raylib.h GenMeshTangents"
  c'genMeshTangents ::
    Ptr Raylib.Types.Mesh -> IO ()

genMeshTangents :: Raylib.Types.Mesh -> IO Raylib.Types.Mesh
genMeshTangents mesh = with mesh (\m -> c'genMeshTangents m >> peek m)

foreign import ccall safe "raylib.h &GenMeshTangents"
  p'genMeshTangents ::
    FunPtr (Ptr Raylib.Types.Mesh -> IO ())

foreign import ccall safe "bindings.h GenMeshPoly_" c'genMeshPoly :: CInt -> CFloat -> IO (Ptr Raylib.Types.Mesh)

genMeshPoly :: Int -> Float -> IO Raylib.Types.Mesh
genMeshPoly sides radius = c'genMeshPoly (fromIntegral sides) (realToFrac radius) >>= pop

foreign import ccall safe "raylib.h &GenMeshPoly"
  p'genMeshPoly ::
    FunPtr (CInt -> CFloat -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshPlane_" c'genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshPlane :: Float -> Float -> Int -> Int -> IO Raylib.Types.Mesh
genMeshPlane width length resX resZ = c'genMeshPlane (realToFrac width) (realToFrac length) (fromIntegral resX) (fromIntegral resZ) >>= pop

foreign import ccall safe "raylib.h &GenMeshPlane"
  p'genMeshPlane ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCube_" c'genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Raylib.Types.Mesh)

genMeshCube :: Float -> Float -> Float -> IO Raylib.Types.Mesh
genMeshCube width height length = c'genMeshCube (realToFrac width) (realToFrac height) (realToFrac length) >>= pop

foreign import ccall safe "raylib.h &GenMeshCube"
  p'genMeshCube ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshSphere_" c'genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshSphere :: Float -> Int -> Int -> IO Raylib.Types.Mesh
genMeshSphere radius rings slices = c'genMeshSphere (realToFrac radius) (fromIntegral rings) (fromIntegral slices) >>= pop

foreign import ccall safe "raylib.h &GenMeshSphere"
  p'genMeshSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshHemiSphere_" c'genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshHemiSphere :: Float -> Int -> Int -> IO Raylib.Types.Mesh
genMeshHemiSphere radius rings slices = c'genMeshHemiSphere (realToFrac radius) (fromIntegral rings) (fromIntegral slices) >>= pop

foreign import ccall safe "raylib.h &GenMeshHemiSphere"
  p'genMeshHemiSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCylinder_" c'genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshCylinder :: Float -> Float -> Int -> IO Raylib.Types.Mesh
genMeshCylinder radius height slices = c'genMeshCylinder (realToFrac radius) (realToFrac height) (fromIntegral slices) >>= pop

foreign import ccall safe "raylib.h &GenMeshCylinder"
  p'genMeshCylinder ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCone_" c'genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshCone :: Float -> Float -> Int -> IO Raylib.Types.Mesh
genMeshCone radius height slices = c'genMeshCone (realToFrac radius) (realToFrac height) (fromIntegral slices) >>= pop

foreign import ccall safe "raylib.h &GenMeshCone"
  p'genMeshCone ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshTorus_" c'genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshTorus :: Float -> Float -> Int -> Int -> IO Raylib.Types.Mesh
genMeshTorus radius size radSeg sides = c'genMeshTorus (realToFrac radius) (realToFrac size) (fromIntegral radSeg) (fromIntegral sides) >>= pop

foreign import ccall safe "raylib.h &GenMeshTorus"
  p'genMeshTorus ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshKnot_" c'genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

genMeshKnot :: Float -> Float -> Int -> Int -> IO Raylib.Types.Mesh
genMeshKnot radius size radSeg sides = c'genMeshKnot (realToFrac radius) (realToFrac size) (fromIntegral radSeg) (fromIntegral sides) >>= pop

foreign import ccall safe "raylib.h &GenMeshKnot"
  p'genMeshKnot ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshHeightmap_" c'genMeshHeightmap :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.Mesh)

genMeshHeightmap :: Raylib.Types.Image -> Raylib.Types.Vector3 -> IO Raylib.Types.Mesh
genMeshHeightmap heightmap size = with heightmap (with size . c'genMeshHeightmap) >>= pop

foreign import ccall safe "raylib.h &GenMeshHeightmap"
  p'genMeshHeightmap ::
    FunPtr (Raylib.Types.Image -> Raylib.Types.Vector3 -> IO Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCubicmap_" c'genMeshCubicmap :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.Mesh)

genMeshCubicmap :: Raylib.Types.Image -> Raylib.Types.Vector3 -> IO Raylib.Types.Mesh
genMeshCubicmap cubicmap cubeSize = with cubicmap (with cubeSize . c'genMeshCubicmap) >>= pop

foreign import ccall safe "raylib.h &GenMeshCubicmap"
  p'genMeshCubicmap ::
    FunPtr (Raylib.Types.Image -> Raylib.Types.Vector3 -> IO Raylib.Types.Mesh)

foreign import ccall safe "raylib.h LoadMaterials"
  c'loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Raylib.Types.Material)

loadMaterials :: String -> IO [Raylib.Types.Material]
loadMaterials fileName =
  withCString
    fileName
    ( \f ->
        with
          0
          ( \n -> do
              ptr <- c'loadMaterials f n
              num <- peek n
              peekArray (fromIntegral num) ptr
          )
    )

foreign import ccall safe "raylib.h &LoadMaterials"
  p'loadMaterials ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr Raylib.Types.Material))

foreign import ccall safe "bindings.h LoadMaterialDefault_" c'loadMaterialDefault :: IO (Ptr Raylib.Types.Material)

loadMaterialDefault :: IO Raylib.Types.Material
loadMaterialDefault = c'loadMaterialDefault >>= pop

foreign import ccall safe "raylib.h &LoadMaterialDefault"
  p'loadMaterialDefault ::
    FunPtr (IO Raylib.Types.Material)

foreign import ccall safe "bindings.h UnloadMaterial_" c'unloadMaterial :: Ptr Raylib.Types.Material -> IO ()

unloadMaterial :: Raylib.Types.Material -> IO ()
unloadMaterial material = with material c'unloadMaterial

foreign import ccall safe "raylib.h &UnloadMaterial"
  p'unloadMaterial ::
    FunPtr (Raylib.Types.Material -> IO ())

foreign import ccall safe "bindings.h SetMaterialTexture_" c'setMaterialTexture :: Ptr Raylib.Types.Material -> CInt -> Ptr Raylib.Types.Texture -> IO ()

setMaterialTexture :: Raylib.Types.Material -> Int -> Raylib.Types.Texture -> IO Raylib.Types.Material
setMaterialTexture material mapType texture = with material (\m -> with texture (c'setMaterialTexture m (fromIntegral mapType)) >> peek m)

foreign import ccall safe "raylib.h &SetMaterialTexture"
  p'setMaterialTexture ::
    FunPtr (Ptr Raylib.Types.Material -> CInt -> Raylib.Types.Texture -> IO ())

foreign import ccall safe "raylib.h SetModelMeshMaterial"
  c'setModelMeshMaterial ::
    Ptr Raylib.Types.Model -> CInt -> CInt -> IO ()

setModelMeshMaterial :: Raylib.Types.Model -> Int -> Int -> IO Raylib.Types.Model
setModelMeshMaterial model meshId materialId = with model (\m -> c'setModelMeshMaterial m (fromIntegral meshId) (fromIntegral materialId) >> peek m)

foreign import ccall safe "raylib.h &SetModelMeshMaterial"
  p'setModelMeshMaterial ::
    FunPtr (Ptr Raylib.Types.Model -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h LoadModelAnimations"
  c'loadModelAnimations ::
    CString -> Ptr CUInt -> IO (Ptr Raylib.Types.ModelAnimation)

loadModelAnimations :: String -> IO [Raylib.Types.ModelAnimation]
loadModelAnimations fileName =
  withCString
    fileName
    ( \f ->
        with
          0
          ( \n -> do
              ptr <- c'loadModelAnimations f n
              num <- peek n
              peekArray (fromIntegral num) ptr
          )
    )

foreign import ccall safe "raylib.h &LoadModelAnimations"
  p'loadModelAnimations ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr Raylib.Types.ModelAnimation))

foreign import ccall safe "bindings.h UpdateModelAnimation_" c'updateModelAnimation :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.ModelAnimation -> CInt -> IO ()

updateModelAnimation :: Raylib.Types.Model -> Raylib.Types.ModelAnimation -> Int -> IO ()
updateModelAnimation model animation frame = with model (\m -> with animation (\a -> c'updateModelAnimation m a (fromIntegral frame)))

foreign import ccall safe "raylib.h &UpdateModelAnimation"
  p'updateModelAnimation ::
    FunPtr (Raylib.Types.Model -> Raylib.Types.ModelAnimation -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadModelAnimation_" c'unloadModelAnimation :: Ptr Raylib.Types.ModelAnimation -> IO ()

unloadModelAnimation :: ModelAnimation -> IO ()
unloadModelAnimation animation = with animation c'unloadModelAnimation

foreign import ccall safe "raylib.h &UnloadModelAnimation"
  p'unloadModelAnimation ::
    FunPtr (Raylib.Types.ModelAnimation -> IO ())

foreign import ccall safe "raylib.h UnloadModelAnimations"
  c'unloadModelAnimations ::
    Ptr Raylib.Types.ModelAnimation -> CUInt -> IO ()

unloadModelAnimations :: [ModelAnimation] -> IO ()
unloadModelAnimations animations = withArrayLen animations (\num a -> c'unloadModelAnimations a (fromIntegral num))

foreign import ccall safe "raylib.h &UnloadModelAnimations"
  p'unloadModelAnimations ::
    FunPtr (Ptr Raylib.Types.ModelAnimation -> CUInt -> IO ())

foreign import ccall safe "bindings.h IsModelAnimationValid_" c'isModelAnimationValid :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.ModelAnimation -> IO CBool

isModelAnimationValid :: Model -> ModelAnimation -> IO Bool
isModelAnimationValid model animation = toBool <$> with model (with animation . c'isModelAnimationValid)

foreign import ccall safe "raylib.h &IsModelAnimationValid"
  p'isModelAnimationValid ::
    FunPtr (Raylib.Types.Model -> Raylib.Types.ModelAnimation -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionSpheres_" c'checkCollisionSpheres :: Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> CFloat -> IO CBool

checkCollisionSpheres :: Vector3 -> Float -> Vector3 -> Float -> Bool
checkCollisionSpheres center1 radius1 center2 radius2 = toBool $ unsafePerformIO (with center1 (\c1 -> with center2 (\c2 -> c'checkCollisionSpheres c1 (realToFrac radius1) c2 (realToFrac radius2))))

foreign import ccall safe "raylib.h &CheckCollisionSpheres"
  p'checkCollisionSpheres ::
    FunPtr (Raylib.Types.Vector3 -> CFloat -> Raylib.Types.Vector3 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionBoxes_" c'checkCollisionBoxes :: Ptr Raylib.Types.BoundingBox -> Ptr Raylib.Types.BoundingBox -> IO CBool

checkCollisionBoxes :: BoundingBox -> BoundingBox -> Bool
checkCollisionBoxes box1 box2 = toBool $ unsafePerformIO (with box1 (with box2 . c'checkCollisionBoxes))

foreign import ccall safe "raylib.h &CheckCollisionBoxes"
  p'checkCollisionBoxes ::
    FunPtr (Raylib.Types.BoundingBox -> Raylib.Types.BoundingBox -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionBoxSphere_" c'checkCollisionBoxSphere :: Ptr Raylib.Types.BoundingBox -> Ptr Raylib.Types.Vector3 -> CFloat -> IO CBool

checkCollisionBoxSphere :: BoundingBox -> Vector3 -> Float -> Bool
checkCollisionBoxSphere box center radius = toBool $ unsafePerformIO (with box (\b -> with center (\c -> c'checkCollisionBoxSphere b c (realToFrac radius))))

foreign import ccall safe "raylib.h &CheckCollisionBoxSphere"
  p'checkCollisionBoxSphere ::
    FunPtr (Raylib.Types.BoundingBox -> Raylib.Types.Vector3 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h GetRayCollisionSphere_" c'getRayCollisionSphere :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Vector3 -> CFloat -> IO (Ptr Raylib.Types.RayCollision)

getRayCollisionSphere :: Ray -> Vector3 -> Float -> RayCollision
getRayCollisionSphere ray center radius = unsafePerformIO $ with ray (\r -> with center (\c -> c'getRayCollisionSphere r c (realToFrac radius))) >>= pop

foreign import ccall safe "raylib.h &GetRayCollisionSphere"
  p'getRayCollisionSphere ::
    FunPtr (Raylib.Types.Ray -> Raylib.Types.Vector3 -> CFloat -> IO Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionBox_" c'getRayCollisionBox :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.BoundingBox -> IO (Ptr Raylib.Types.RayCollision)

getRayCollisionBox :: Ray -> BoundingBox -> RayCollision
getRayCollisionBox ray box = unsafePerformIO $ with ray (with box . c'getRayCollisionBox) >>= pop

foreign import ccall safe "raylib.h &GetRayCollisionBox"
  p'getRayCollisionBox ::
    FunPtr (Raylib.Types.Ray -> Raylib.Types.BoundingBox -> IO Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionMesh_" c'getRayCollisionMesh :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Mesh -> Ptr Raylib.Types.Matrix -> IO (Ptr Raylib.Types.RayCollision)

getRayCollisionMesh :: Ray -> Mesh -> Matrix -> RayCollision
getRayCollisionMesh ray mesh transform = unsafePerformIO $ with ray (\r -> with mesh (with transform . c'getRayCollisionMesh r)) >>= pop

foreign import ccall safe "raylib.h &GetRayCollisionMesh"
  p'getRayCollisionMesh ::
    FunPtr (Raylib.Types.Ray -> Raylib.Types.Mesh -> Raylib.Types.Matrix -> IO Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionTriangle_" c'getRayCollisionTriangle :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.RayCollision)

getRayCollisionTriangle :: Ray -> Vector3 -> Vector3 -> Vector3 -> RayCollision
getRayCollisionTriangle ray v1 v2 v3 = unsafePerformIO $ with ray (\r -> with v1 (\p1 -> with v2 (with v3 . c'getRayCollisionTriangle r p1))) >>= pop

foreign import ccall safe "raylib.h &GetRayCollisionTriangle"
  p'getRayCollisionTriangle ::
    FunPtr (Raylib.Types.Ray -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> IO Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionQuad_" c'getRayCollisionQuad :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.RayCollision)

getRayCollisionQuad :: Ray -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> RayCollision
getRayCollisionQuad ray v1 v2 v3 v4 = unsafePerformIO $ with ray (\r -> with v1 (\p1 -> with v2 (\p2 -> with v3 (with v4 . c'getRayCollisionQuad r p1 p2)))) >>= pop

foreign import ccall safe "raylib.h &GetRayCollisionQuad"
  p'getRayCollisionQuad ::
    FunPtr (Raylib.Types.Ray -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> Raylib.Types.Vector3 -> IO Raylib.Types.RayCollision)

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())

foreign import ccall safe "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO AudioCallback

foreign import ccall safe "dynamic"
  mK'audioCallback ::
    AudioCallback -> (Ptr () -> CUInt -> IO ())

foreign import ccall safe "raylib.h InitAudioDevice"
  initAudioDevice ::
    IO ()

foreign import ccall safe "raylib.h &InitAudioDevice"
  p'initAudioDevice ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h CloseAudioDevice"
  closeAudioDevice ::
    IO ()

foreign import ccall safe "raylib.h &CloseAudioDevice"
  p'closeAudioDevice ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h IsAudioDeviceReady"
  c'isAudioDeviceReady ::
    IO CBool

isAudioDeviceReady :: IO Bool
isAudioDeviceReady = toBool <$> c'isAudioDeviceReady

foreign import ccall safe "raylib.h &IsAudioDeviceReady"
  p'isAudioDeviceReady ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h SetMasterVolume"
  c'setMasterVolume ::
    CFloat -> IO ()

setMasterVolume :: Float -> IO ()
setMasterVolume volume = c'setMasterVolume (realToFrac volume)

foreign import ccall safe "raylib.h &SetMasterVolume"
  p'setMasterVolume ::
    FunPtr (CFloat -> IO ())

foreign import ccall safe "bindings.h LoadWave_" c'loadWave :: CString -> IO (Ptr Raylib.Types.Wave)

loadWave :: String -> IO Wave
loadWave fileName = withCString fileName c'loadWave >>= pop

foreign import ccall safe "raylib.h &LoadWave"
  p'loadWave ::
    FunPtr (CString -> IO Raylib.Types.Wave)

foreign import ccall safe "bindings.h LoadWaveFromMemory_" c'loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Raylib.Types.Wave)

loadWaveFromMemory :: String -> [Integer] -> IO Wave
loadWaveFromMemory fileType fileData = withCString fileType (\f -> withArrayLen (map fromIntegral fileData) (\size d -> c'loadWaveFromMemory f d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

foreign import ccall safe "raylib.h &LoadWaveFromMemory"
  p'loadWaveFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Raylib.Types.Wave)

foreign import ccall safe "bindings.h LoadSound_" c'loadSound :: CString -> IO (Ptr Raylib.Types.Sound)

loadSound :: String -> IO Sound
loadSound fileName = withCString fileName c'loadSound >>= pop

foreign import ccall safe "raylib.h &LoadSound"
  p'loadSound ::
    FunPtr (CString -> IO Raylib.Types.Sound)

foreign import ccall safe "bindings.h LoadSoundFromWave_" c'loadSoundFromWave :: Ptr Raylib.Types.Wave -> IO (Ptr Raylib.Types.Sound)

loadSoundFromWave :: Wave -> IO Sound
loadSoundFromWave wave = with wave c'loadSoundFromWave >>= pop

foreign import ccall safe "raylib.h &LoadSoundFromWave"
  p'loadSoundFromWave ::
    FunPtr (Raylib.Types.Wave -> IO Raylib.Types.Sound)

foreign import ccall safe "bindings.h UpdateSound_" c'updateSound :: Ptr Raylib.Types.Sound -> Ptr () -> CInt -> IO ()

updateSound :: Sound -> Ptr () -> Int -> IO ()
updateSound sound dataValue sampleCount = with sound (\s -> c'updateSound s dataValue (fromIntegral sampleCount))

foreign import ccall safe "raylib.h &UpdateSound"
  p'updateSound ::
    FunPtr (Raylib.Types.Sound -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadWave_" c'unloadWave :: Ptr Raylib.Types.Wave -> IO ()

unloadWave :: Wave -> IO ()
unloadWave wave = with wave c'unloadWave

foreign import ccall safe "raylib.h &UnloadWave"
  p'unloadWave ::
    FunPtr (Raylib.Types.Wave -> IO ())

foreign import ccall safe "bindings.h UnloadSound_" c'unloadSound :: Ptr Raylib.Types.Sound -> IO ()

unloadSound :: Sound -> IO ()
unloadSound sound = with sound c'unloadSound

foreign import ccall safe "raylib.h &UnloadSound"
  p'unloadSound ::
    FunPtr (Raylib.Types.Sound -> IO ())

foreign import ccall safe "bindings.h ExportWave_" c'exportWave :: Ptr Raylib.Types.Wave -> CString -> IO CBool

exportWave :: Wave -> String -> IO Bool
exportWave wave fileName = toBool <$> with wave (withCString fileName . c'exportWave)

foreign import ccall safe "raylib.h &ExportWave"
  p'exportWave ::
    FunPtr (Raylib.Types.Wave -> CString -> IO CInt)

foreign import ccall safe "bindings.h ExportWaveAsCode_" c'exportWaveAsCode :: Ptr Raylib.Types.Wave -> CString -> IO CBool

exportWaveAsCode :: Wave -> String -> IO Bool
exportWaveAsCode wave fileName = toBool <$> with wave (withCString fileName . c'exportWaveAsCode)

foreign import ccall safe "raylib.h &ExportWaveAsCode"
  p'exportWaveAsCode ::
    FunPtr (Raylib.Types.Wave -> CString -> IO CInt)

foreign import ccall safe "bindings.h PlaySound_" c'playSound :: Ptr Raylib.Types.Sound -> IO ()

playSound :: Sound -> IO ()
playSound sound = with sound c'playSound

foreign import ccall safe "raylib.h &PlaySound"
  p'playSound ::
    FunPtr (Raylib.Types.Sound -> IO ())

foreign import ccall safe "bindings.h StopSound_" c'stopSound :: Ptr Raylib.Types.Sound -> IO ()

stopSound :: Sound -> IO ()
stopSound sound = with sound c'stopSound

foreign import ccall safe "raylib.h &StopSound"
  p'stopSound ::
    FunPtr (Raylib.Types.Sound -> IO ())

foreign import ccall safe "bindings.h PauseSound_" c'pauseSound :: Ptr Raylib.Types.Sound -> IO ()

pauseSound :: Sound -> IO ()
pauseSound sound = with sound c'pauseSound

foreign import ccall safe "raylib.h &PauseSound"
  p'pauseSound ::
    FunPtr (Raylib.Types.Sound -> IO ())

foreign import ccall safe "bindings.h ResumeSound_" c'resumeSound :: Ptr Raylib.Types.Sound -> IO ()

resumeSound :: Sound -> IO ()
resumeSound sound = with sound c'resumeSound

foreign import ccall safe "raylib.h &ResumeSound"
  p'resumeSound ::
    FunPtr (Raylib.Types.Sound -> IO ())

foreign import ccall safe "bindings.h PlaySoundMulti_" c'playSoundMulti :: Ptr Raylib.Types.Sound -> IO ()

playSoundMulti :: Sound -> IO ()
playSoundMulti sound = with sound c'playSoundMulti

foreign import ccall safe "raylib.h &PlaySoundMulti"
  p'playSoundMulti ::
    FunPtr (Raylib.Types.Sound -> IO ())

foreign import ccall safe "raylib.h StopSoundMulti"
  stopSoundMulti ::
    IO ()

foreign import ccall safe "raylib.h &StopSoundMulti"
  p'stopSoundMulti ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h GetSoundsPlaying"
  c'getSoundsPlaying ::
    IO CInt

getSoundsPlaying :: IO Int
getSoundsPlaying = fromIntegral <$> c'getSoundsPlaying

foreign import ccall safe "raylib.h &GetSoundsPlaying"
  p'getSoundsPlaying ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h IsSoundPlaying_" c'isSoundPlaying :: Ptr Raylib.Types.Sound -> IO CBool

isSoundPlaying :: Sound -> IO Bool
isSoundPlaying sound = toBool <$> with sound c'isSoundPlaying

foreign import ccall safe "raylib.h &IsSoundPlaying"
  p'isSoundPlaying ::
    FunPtr (Raylib.Types.Sound -> IO CInt)

foreign import ccall safe "bindings.h SetSoundVolume_" c'setSoundVolume :: Ptr Raylib.Types.Sound -> CFloat -> IO ()

setSoundVolume :: Sound -> Float -> IO ()
setSoundVolume sound volume = with sound (\s -> c'setSoundVolume s (realToFrac volume))

foreign import ccall safe "raylib.h &SetSoundVolume"
  p'setSoundVolume ::
    FunPtr (Raylib.Types.Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetSoundPitch_" c'setSoundPitch :: Ptr Raylib.Types.Sound -> CFloat -> IO ()

setSoundPitch :: Sound -> Float -> IO ()
setSoundPitch sound pitch = with sound (\s -> c'setSoundPitch s (realToFrac pitch))

foreign import ccall safe "raylib.h &SetSoundPitch"
  p'setSoundPitch ::
    FunPtr (Raylib.Types.Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetSoundPan_" c'setSoundPan :: Ptr Raylib.Types.Sound -> CFloat -> IO ()

setSoundPan :: Sound -> Float -> IO ()
setSoundPan sound pan = with sound (\s -> c'setSoundPan s (realToFrac pan))

foreign import ccall safe "raylib.h &SetSoundPan"
  p'setSoundPan ::
    FunPtr (Raylib.Types.Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h WaveCopy_" c'waveCopy :: Ptr Raylib.Types.Wave -> IO (Ptr Raylib.Types.Wave)

waveCopy :: Wave -> IO Wave
waveCopy wave = with wave c'waveCopy >>= pop

foreign import ccall safe "raylib.h &WaveCopy"
  p'waveCopy ::
    FunPtr (Raylib.Types.Wave -> IO Raylib.Types.Wave)

foreign import ccall safe "raylib.h WaveCrop"
  c'waveCrop ::
    Ptr Raylib.Types.Wave -> CInt -> CInt -> IO ()

waveCrop :: Wave -> Int -> Int -> IO Wave
waveCrop wave initSample finalSample = do
  new <- waveCopy wave
  with new (\w -> c'waveCrop w (fromIntegral initSample) (fromIntegral finalSample) >> peek w)

foreign import ccall safe "raylib.h &WaveCrop"
  p'waveCrop ::
    FunPtr (Ptr Raylib.Types.Wave -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h WaveFormat"
  c'waveFormat ::
    Ptr Raylib.Types.Wave -> CInt -> CInt -> CInt -> IO ()

waveFormat :: Wave -> Int -> Int -> Int -> IO ()
waveFormat wave sampleRate sampleSize channels = do
  new <- waveCopy wave
  with new (\n -> c'waveFormat n (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels))

foreign import ccall safe "raylib.h &WaveFormat"
  p'waveFormat ::
    FunPtr (Ptr Raylib.Types.Wave -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h LoadWaveSamples_" c'loadWaveSamples :: Ptr Raylib.Types.Wave -> IO (Ptr CFloat)

loadWaveSamples :: Wave -> IO [Float]
loadWaveSamples wave =
  with
    wave
    ( \w -> do
        ptr <- c'loadWaveSamples w
        arr <- peekArray (fromIntegral $ wave'frameCount wave * wave'channels wave) ptr
        c'unloadWaveSamples ptr
        return $ map realToFrac arr
    )

foreign import ccall safe "raylib.h &LoadWaveSamples"
  p'loadWaveSamples ::
    FunPtr (Raylib.Types.Wave -> IO (Ptr CFloat))

foreign import ccall safe "raylib.h UnloadWaveSamples"
  c'unloadWaveSamples ::
    Ptr CFloat -> IO ()

foreign import ccall safe "raylib.h &UnloadWaveSamples"
  p'unloadWaveSamples ::
    FunPtr (Ptr CFloat -> IO ())

foreign import ccall safe "bindings.h LoadMusicStream_" c'loadMusicStream :: CString -> IO (Ptr Raylib.Types.Music)

loadMusicStream :: String -> IO Music
loadMusicStream fileName = withCString fileName c'loadMusicStream >>= pop

foreign import ccall safe "raylib.h &LoadMusicStream"
  p'loadMusicStream ::
    FunPtr (CString -> IO Raylib.Types.Music)

foreign import ccall safe "bindings.h LoadMusicStreamFromMemory_" c'loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Raylib.Types.Music)

loadMusicStreamFromMemory :: String -> [Integer] -> IO Music
loadMusicStreamFromMemory fileType streamData = withCString fileType (\t -> withArrayLen (map fromIntegral streamData) (\size d -> c'loadMusicStreamFromMemory t d (fromIntegral $ size * sizeOf (0 :: CUChar)))) >>= pop

foreign import ccall safe "raylib.h &LoadMusicStreamFromMemory"
  p'loadMusicStreamFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Raylib.Types.Music)

foreign import ccall safe "bindings.h UnloadMusicStream_" c'unloadMusicStream :: Ptr Raylib.Types.Music -> IO ()

unloadMusicStream :: Music -> IO ()
unloadMusicStream music = with music c'unloadMusicStream

foreign import ccall safe "raylib.h &UnloadMusicStream"
  p'unloadMusicStream ::
    FunPtr (Raylib.Types.Music -> IO ())

foreign import ccall safe "bindings.h PlayMusicStream_" c'playMusicStream :: Ptr Raylib.Types.Music -> IO ()

playMusicStream :: Music -> IO ()
playMusicStream music = with music c'playMusicStream

foreign import ccall safe "raylib.h &PlayMusicStream"
  p'playMusicStream ::
    FunPtr (Raylib.Types.Music -> IO ())

foreign import ccall safe "bindings.h IsMusicStreamPlaying_" c'isMusicStreamPlaying :: Ptr Raylib.Types.Music -> IO CBool

isMusicStreamPlaying :: Music -> IO Bool
isMusicStreamPlaying music = toBool <$> with music c'isMusicStreamPlaying

foreign import ccall safe "raylib.h &IsMusicStreamPlaying"
  p'isMusicStreamPlaying ::
    FunPtr (Raylib.Types.Music -> IO CInt)

foreign import ccall safe "bindings.h UpdateMusicStream_" c'updateMusicStream :: Ptr Raylib.Types.Music -> IO ()

updateMusicStream :: Music -> IO ()
updateMusicStream music = with music c'updateMusicStream

foreign import ccall safe "raylib.h &UpdateMusicStream"
  p'updateMusicStream ::
    FunPtr (Raylib.Types.Music -> IO ())

foreign import ccall safe "bindings.h StopMusicStream_" c'stopMusicStream :: Ptr Raylib.Types.Music -> IO ()

stopMusicStream :: Music -> IO ()
stopMusicStream music = with music c'stopMusicStream

foreign import ccall safe "raylib.h &StopMusicStream"
  p'stopMusicStream ::
    FunPtr (Raylib.Types.Music -> IO ())

foreign import ccall safe "bindings.h PauseMusicStream_" c'pauseMusicStream :: Ptr Raylib.Types.Music -> IO ()

pauseMusicStream :: Music -> IO ()
pauseMusicStream music = with music c'pauseMusicStream

foreign import ccall safe "raylib.h &PauseMusicStream"
  p'pauseMusicStream ::
    FunPtr (Raylib.Types.Music -> IO ())

foreign import ccall safe "bindings.h ResumeMusicStream_" c'resumeMusicStream :: Ptr Raylib.Types.Music -> IO ()

resumeMusicStream :: Music -> IO ()
resumeMusicStream music = with music c'resumeMusicStream

foreign import ccall safe "raylib.h &ResumeMusicStream"
  p'resumeMusicStream ::
    FunPtr (Raylib.Types.Music -> IO ())

foreign import ccall safe "bindings.h SeekMusicStream_" c'seekMusicStream :: Ptr Raylib.Types.Music -> CFloat -> IO ()

seekMusicStream :: Music -> Float -> IO ()
seekMusicStream music position = with music (\m -> c'seekMusicStream m (realToFrac position))

foreign import ccall safe "raylib.h &SeekMusicStream"
  p'seekMusicStream ::
    FunPtr (Raylib.Types.Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicVolume_" c'setMusicVolume :: Ptr Raylib.Types.Music -> CFloat -> IO ()

setMusicVolume :: Music -> Float -> IO ()
setMusicVolume music volume = with music (\m -> c'setMusicVolume m (realToFrac volume))

foreign import ccall safe "raylib.h &SetMusicVolume"
  p'setMusicVolume ::
    FunPtr (Raylib.Types.Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicPitch_" c'setMusicPitch :: Ptr Raylib.Types.Music -> CFloat -> IO ()

setMusicPitch :: Music -> Float -> IO ()
setMusicPitch music pitch = with music (\m -> c'setMusicPitch m (realToFrac pitch))

foreign import ccall safe "raylib.h &SetMusicPitch"
  p'setMusicPitch ::
    FunPtr (Raylib.Types.Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicPan_" c'setMusicPan :: Ptr Raylib.Types.Music -> CFloat -> IO ()

setMusicPan :: Music -> Float -> IO ()
setMusicPan music pan = with music (\m -> c'setMusicPan m (realToFrac pan))

foreign import ccall safe "raylib.h &SetMusicPan"
  p'setMusicPan ::
    FunPtr (Raylib.Types.Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h GetMusicTimeLength_" c'getMusicTimeLength :: Ptr Raylib.Types.Music -> IO CFloat

getMusicTimeLength :: Music -> IO Float
getMusicTimeLength music = realToFrac <$> with music c'getMusicTimeLength

foreign import ccall safe "raylib.h &GetMusicTimeLength"
  p'getMusicTimeLength ::
    FunPtr (Raylib.Types.Music -> IO CFloat)

foreign import ccall safe "bindings.h GetMusicTimePlayed_" c'getMusicTimePlayed :: Ptr Raylib.Types.Music -> IO CFloat

getMusicTimePlayed :: Music -> IO Float
getMusicTimePlayed music = realToFrac <$> with music c'getMusicTimePlayed

foreign import ccall safe "raylib.h &GetMusicTimePlayed"
  p'getMusicTimePlayed ::
    FunPtr (Raylib.Types.Music -> IO CFloat)

foreign import ccall safe "bindings.h LoadAudioStream_" c'loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr Raylib.Types.AudioStream)

loadAudioStream :: Integer -> Integer -> Integer -> IO AudioStream
loadAudioStream sampleRate sampleSize channels = c'loadAudioStream (fromIntegral sampleRate) (fromIntegral sampleSize) (fromIntegral channels) >>= pop

foreign import ccall safe "raylib.h &LoadAudioStream"
  p'loadAudioStream ::
    FunPtr (CUInt -> CUInt -> CUInt -> IO Raylib.Types.AudioStream)

foreign import ccall safe "bindings.h UnloadAudioStream_" c'unloadAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

unloadAudioStream :: AudioStream -> IO ()
unloadAudioStream stream = with stream c'unloadAudioStream

foreign import ccall safe "raylib.h &UnloadAudioStream"
  p'unloadAudioStream ::
    FunPtr (Raylib.Types.AudioStream -> IO ())

foreign import ccall safe "bindings.h UpdateAudioStream_" c'updateAudioStream :: Ptr Raylib.Types.AudioStream -> Ptr () -> CInt -> IO ()

updateAudioStream :: AudioStream -> Ptr () -> Int -> IO ()
updateAudioStream stream value frameCount = with stream (\s -> c'updateAudioStream s value (fromIntegral frameCount))

foreign import ccall safe "raylib.h &UpdateAudioStream"
  p'updateAudioStream ::
    FunPtr (Raylib.Types.AudioStream -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h IsAudioStreamProcessed_" c'isAudioStreamProcessed :: Ptr Raylib.Types.AudioStream -> IO CBool

isAudioStreamProcessed :: AudioStream -> IO Bool
isAudioStreamProcessed stream = toBool <$> with stream c'isAudioStreamProcessed

foreign import ccall safe "raylib.h &IsAudioStreamProcessed"
  p'isAudioStreamProcessed ::
    FunPtr (Raylib.Types.AudioStream -> IO CInt)

foreign import ccall safe "bindings.h PlayAudioStream_" c'playAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

playAudioStream :: AudioStream -> IO ()
playAudioStream stream = with stream c'playAudioStream

foreign import ccall safe "raylib.h &PlayAudioStream"
  p'playAudioStream ::
    FunPtr (Raylib.Types.AudioStream -> IO ())

foreign import ccall safe "bindings.h PauseAudioStream_" c'pauseAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

pauseAudioStream :: AudioStream -> IO ()
pauseAudioStream stream = with stream c'pauseAudioStream

foreign import ccall safe "raylib.h &PauseAudioStream"
  p'pauseAudioStream ::
    FunPtr (Raylib.Types.AudioStream -> IO ())

foreign import ccall safe "bindings.h ResumeAudioStream_" c'resumeAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

resumeAudioStream :: AudioStream -> IO ()
resumeAudioStream stream = with stream c'resumeAudioStream

foreign import ccall safe "raylib.h &ResumeAudioStream"
  p'resumeAudioStream ::
    FunPtr (Raylib.Types.AudioStream -> IO ())

foreign import ccall safe "bindings.h IsAudioStreamPlaying_" c'isAudioStreamPlaying :: Ptr Raylib.Types.AudioStream -> IO CBool

isAudioStreamPlaying :: AudioStream -> IO Bool
isAudioStreamPlaying stream = toBool <$> with stream c'isAudioStreamPlaying

foreign import ccall safe "raylib.h &IsAudioStreamPlaying"
  p'isAudioStreamPlaying ::
    FunPtr (Raylib.Types.AudioStream -> IO CInt)

foreign import ccall safe "bindings.h StopAudioStream_" c'stopAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

stopAudioStream :: AudioStream -> IO ()
stopAudioStream stream = with stream c'stopAudioStream

foreign import ccall safe "raylib.h &StopAudioStream"
  p'stopAudioStream ::
    FunPtr (Raylib.Types.AudioStream -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamVolume_" c'setAudioStreamVolume :: Ptr Raylib.Types.AudioStream -> CFloat -> IO ()

setAudioStreamVolume :: AudioStream -> Float -> IO ()
setAudioStreamVolume stream volume = with stream (\s -> c'setAudioStreamVolume s (realToFrac volume))

foreign import ccall safe "raylib.h &SetAudioStreamVolume"
  p'setAudioStreamVolume ::
    FunPtr (Raylib.Types.AudioStream -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamPitch_" c'setAudioStreamPitch :: Ptr Raylib.Types.AudioStream -> CFloat -> IO ()

setAudioStreamPitch :: AudioStream -> Float -> IO ()
setAudioStreamPitch stream pitch = with stream (\s -> c'setAudioStreamPitch s (realToFrac pitch))

foreign import ccall safe "raylib.h &SetAudioStreamPitch"
  p'setAudioStreamPitch ::
    FunPtr (Raylib.Types.AudioStream -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamPan_" c'setAudioStreamPan :: Ptr Raylib.Types.AudioStream -> CFloat -> IO ()

setAudioStreamPan :: AudioStream -> Float -> IO ()
setAudioStreamPan stream pan = with stream (\s -> c'setAudioStreamPan s (realToFrac pan))

foreign import ccall safe "raylib.h &SetAudioStreamPan"
  p'setAudioStreamPan ::
    FunPtr (Raylib.Types.AudioStream -> CFloat -> IO ())

foreign import ccall safe "raylib.h SetAudioStreamBufferSizeDefault"
  c'setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

setAudioStreamBufferSizeDefault :: Int -> IO ()
setAudioStreamBufferSizeDefault = setAudioStreamBufferSizeDefault . fromIntegral

foreign import ccall safe "raylib.h &SetAudioStreamBufferSizeDefault"
  p'setAudioStreamBufferSizeDefault ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamCallback_" c'setAudioStreamCallback :: Ptr Raylib.Types.AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "raylib.h &SetAudioStreamCallback"
  p'setAudioStreamCallback ::
    FunPtr (Raylib.Types.AudioStream -> AudioCallback -> IO ())

foreign import ccall safe "bindings.h AttachAudioStreamProcessor_" c'attachAudioStreamProcessor :: Ptr Raylib.Types.AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "raylib.h &AttachAudioStreamProcessor"
  p'attachAudioStreamProcessor ::
    FunPtr (Raylib.Types.AudioStream -> AudioCallback -> IO ())

foreign import ccall safe "bindings.h DetachAudioStreamProcessor_" c'detachAudioStreamProcessor :: Ptr Raylib.Types.AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "raylib.h &DetachAudioStreamProcessor"
  p'detachAudioStreamProcessor ::
    FunPtr (Raylib.Types.AudioStream -> AudioCallback -> IO ())
