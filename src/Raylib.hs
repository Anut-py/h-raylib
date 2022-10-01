{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib where

-- Haskell bindings to raylib

import Data.List (genericLength)
import Foreign
  ( FunPtr,
    Ptr,
    Storable (peek),
    castPtr,
    fromBool,
    peekArray,
    toBool,
    with,
    withArray,
  )
import Foreign.C
  ( CChar (..),
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
    Wave,
  )
import Raylib.Util (pop)

-- Haskell doesn't support varargs in foreign calls, so these functions are impossible to call from FFI
-- type TraceLogCallback = FunPtr (CInt -> CString -> __builtin_va_list -> IO ())
-- foreign import ccall unsafe "wrapper"
--   mk'TraceLogCallback ::
--     (CInt -> CString -> __builtin_va_list -> IO ()) -> IO TraceLogCallback
-- foreign import ccall unsafe "dynamic"
--   mK'TraceLogCallback ::
--     TraceLogCallback -> (CInt -> CString -> __builtin_va_list -> IO ())
type LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall unsafe "wrapper"
  mk'loadFileDataCallback ::
    (CString -> Ptr CUInt -> IO (Ptr CUChar)) -> IO LoadFileDataCallback

foreign import ccall unsafe "dynamic"
  mK'loadFileDataCallback ::
    LoadFileDataCallback -> (CString -> Ptr CUInt -> IO (Ptr CUChar))

type SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall unsafe "wrapper"
  mk'saveFileDataCallback ::
    (CString -> Ptr () -> CUInt -> IO CInt) -> IO SaveFileDataCallback

foreign import ccall unsafe "dynamic"
  mK'saveFileDataCallback ::
    SaveFileDataCallback -> (CString -> Ptr () -> CUInt -> IO CInt)

type LoadFileTextCallback = FunPtr (CString -> IO CString)

foreign import ccall unsafe "wrapper"
  mk'loadFileTextCallback ::
    (CString -> IO CString) -> IO LoadFileTextCallback

foreign import ccall unsafe "dynamic"
  mK'loadFileTextCallback ::
    LoadFileTextCallback -> (CString -> IO CString)

type SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)

foreign import ccall unsafe "wrapper"
  mk'saveFileTextCallback ::
    (CString -> CString -> IO CInt) -> IO SaveFileTextCallback

foreign import ccall unsafe "dynamic"
  mK'saveFileTextCallback ::
    SaveFileTextCallback -> (CString -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h InitWindow"
  c'initWindow ::
    CInt -> CInt -> CString -> IO ()

initWindow :: Int -> Int -> String -> IO ()
initWindow width height title = withCString title $ c'initWindow (fromIntegral width) (fromIntegral height)

foreign import ccall unsafe "raylib.h &InitWindow"
  p'initWindow ::
    FunPtr (CInt -> CInt -> CString -> IO ())

foreign import ccall unsafe "raylib.h WindowShouldClose"
  c'windowShouldClose ::
    IO CInt

windowShouldClose :: IO Bool
windowShouldClose = toBool <$> c'windowShouldClose

foreign import ccall unsafe "raylib.h &WindowShouldClose"
  p'windowShouldClose ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h CloseWindow"
  closeWindow ::
    IO ()

foreign import ccall unsafe "raylib.h &CloseWindow"
  p'closeWindow ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h IsWindowReady"
  c'isWindowReady ::
    IO CInt

isWindowReady :: IO Bool
isWindowReady = toBool <$> c'isWindowReady

foreign import ccall unsafe "raylib.h &IsWindowReady"
  p'isWindowReady ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowFullscreen"
  c'isWindowFullscreen ::
    IO CInt

isWindowFullscreen :: IO Bool
isWindowFullscreen = toBool <$> c'isWindowFullscreen

foreign import ccall unsafe "raylib.h &IsWindowFullscreen"
  p'isWindowFullscreen ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowHidden"
  c'isWindowHidden ::
    IO CInt

isWindowHidden :: IO Bool
isWindowHidden = toBool <$> c'isWindowHidden

foreign import ccall unsafe "raylib.h &IsWindowHidden"
  p'isWindowHidden ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowMinimized"
  c'isWindowMinimized ::
    IO CInt

isWindowMinimized :: IO Bool
isWindowMinimized = toBool <$> c'isWindowMinimized

foreign import ccall unsafe "raylib.h &IsWindowMinimized"
  p'isWindowMinimized ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowMaximized"
  c'isWindowMaximized ::
    IO CInt

isWindowMaximized :: IO Bool
isWindowMaximized = toBool <$> c'isWindowMaximized

foreign import ccall unsafe "raylib.h &IsWindowMaximized"
  p'isWindowMaximized ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowFocused"
  c'isWindowFocused ::
    IO CInt

isWindowFocused :: IO Bool
isWindowFocused = toBool <$> c'isWindowFocused

foreign import ccall unsafe "raylib.h &IsWindowFocused"
  p'isWindowFocused ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowResized"
  c'isWindowResized ::
    IO CInt

isWindowResized :: IO Bool
isWindowResized = toBool <$> c'isWindowResized

foreign import ccall unsafe "raylib.h &IsWindowResized"
  p'isWindowResized ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsWindowState"
  c'isWindowState ::
    CUInt -> IO CInt

isWindowState :: Integer -> IO Bool
isWindowState flag = toBool <$> c'isWindowState (fromIntegral flag)

foreign import ccall unsafe "raylib.h &IsWindowState"
  p'isWindowState ::
    FunPtr (CUInt -> IO CInt)

foreign import ccall unsafe "raylib.h SetWindowState"
  c'setWindowState ::
    CUInt -> IO ()

setWindowState :: Integer -> IO ()
setWindowState = c'setWindowState . fromIntegral

foreign import ccall unsafe "raylib.h &SetWindowState"
  p'setWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall unsafe "raylib.h ClearWindowState"
  c'clearWindowState ::
    CUInt -> IO ()

clearWindowState :: Integer -> IO ()
clearWindowState = c'clearWindowState . fromIntegral

foreign import ccall unsafe "raylib.h &ClearWindowState"
  p'clearWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall unsafe "raylib.h ToggleFullscreen"
  toggleFullscreen ::
    IO ()

foreign import ccall unsafe "raylib.h &ToggleFullscreen"
  p'toggleFullscreen ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h MaximizeWindow"
  maximizeWindow ::
    IO ()

foreign import ccall unsafe "raylib.h &MaximizeWindow"
  p'maximizeWindow ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h MinimizeWindow"
  minimizeWindow ::
    IO ()

foreign import ccall unsafe "raylib.h &MinimizeWindow"
  p'minimizeWindow ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h RestoreWindow"
  restoreWindow ::
    IO ()

foreign import ccall unsafe "raylib.h &RestoreWindow"
  p'restoreWindow ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h SetWindowIcon_" c'setWindowIcon :: Ptr Image -> IO ()

setWindowIcon :: Image -> IO ()
setWindowIcon image = with image c'setWindowIcon

foreign import ccall unsafe "raylib.h &SetWindowIcon"
  p'setWindowIcon ::
    FunPtr (Image -> IO ())

foreign import ccall unsafe "raylib.h SetWindowTitle"
  c'setWindowTitle ::
    CString -> IO ()

setWindowTitle :: String -> IO ()
setWindowTitle title = withCString title c'setWindowTitle

foreign import ccall unsafe "raylib.h &SetWindowTitle"
  p'setWindowTitle ::
    FunPtr (CString -> IO ())

foreign import ccall unsafe "raylib.h SetWindowPosition"
  c'setWindowPosition ::
    CInt -> CInt -> IO ()

setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = c'setWindowPosition (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h &SetWindowPosition"
  p'setWindowPosition ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h SetWindowMonitor"
  c'setWindowMonitor ::
    CInt -> IO ()

setWindowMonitor :: Int -> IO ()
setWindowMonitor = c'setWindowMonitor . fromIntegral

foreign import ccall unsafe "raylib.h &SetWindowMonitor"
  p'setWindowMonitor ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h SetWindowMinSize"
  c'setWindowMinSize ::
    CInt -> CInt -> IO ()

setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize x y = c'setWindowMinSize (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h &SetWindowMinSize"
  p'setWindowMinSize ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h SetWindowSize"
  c'setWindowSize ::
    CInt -> CInt -> IO ()

setWindowSize :: Int -> Int -> IO ()
setWindowSize x y = c'setWindowSize (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h &SetWindowSize"
  p'setWindowSize ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h SetWindowOpacity"
  c'setWindowOpacity ::
    CFloat -> IO ()

setWindowOpacity :: Float -> IO ()
setWindowOpacity opacity = c'setWindowOpacity $ realToFrac opacity

foreign import ccall unsafe "raylib.h &SetWindowOpacity"
  p'setWindowOpacity ::
    FunPtr (CFloat -> IO ())

foreign import ccall unsafe "raylib.h GetWindowHandle"
  getWindowHandle ::
    IO (Ptr ())

foreign import ccall unsafe "raylib.h &GetWindowHandle"
  p'getWindowHandle ::
    FunPtr (IO (Ptr ()))

foreign import ccall unsafe "raylib.h GetScreenWidth"
  c'getScreenWidth ::
    IO CInt

getScreenWidth :: IO Int
getScreenWidth = fromIntegral <$> c'getScreenWidth

foreign import ccall unsafe "raylib.h &GetScreenWidth"
  p'getScreenWidth ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetScreenHeight"
  c'getScreenHeight ::
    IO CInt

getScreenHeight :: IO Int
getScreenHeight = fromIntegral <$> c'getScreenHeight

foreign import ccall unsafe "raylib.h &GetScreenHeight"
  p'getScreenHeight ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetRenderWidth"
  c'getRenderWidth ::
    IO CInt

getRenderWidth :: IO Int
getRenderWidth = fromIntegral <$> c'getRenderWidth

foreign import ccall unsafe "raylib.h &GetRenderWidth"
  p'getRenderWidth ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetRenderHeight"
  c'getRenderHeight ::
    IO CInt

getRenderHeight :: IO Int
getRenderHeight = fromIntegral <$> c'getRenderHeight

foreign import ccall unsafe "raylib.h &GetRenderHeight"
  p'getRenderHeight ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetMonitorCount"
  c'getMonitorCount ::
    IO CInt

getMonitorCount :: IO Int
getMonitorCount = fromIntegral <$> c'getMonitorCount

foreign import ccall unsafe "raylib.h &GetMonitorCount"
  p'getMonitorCount ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetCurrentMonitor"
  c'getCurrentMonitor ::
    IO CInt

getCurrentMonitor :: IO Int
getCurrentMonitor = fromIntegral <$> c'getCurrentMonitor

foreign import ccall unsafe "raylib.h &GetCurrentMonitor"
  p'getCurrentMonitor ::
    FunPtr (IO CInt)

foreign import ccall unsafe "bindings.h GetMonitorPosition_" c'getMonitorPosition :: CInt -> IO (Ptr Vector2)

getMonitorPosition :: Int -> IO Vector2
getMonitorPosition monitor = c'getMonitorPosition (fromIntegral monitor) >>= pop

foreign import ccall unsafe "raylib.h &GetMonitorPosition"
  p'getMonitorPosition ::
    FunPtr (CInt -> IO Vector2)

foreign import ccall unsafe "raylib.h GetMonitorWidth"
  c'getMonitorWidth ::
    CInt -> IO CInt

getMonitorWidth :: Int -> IO Int
getMonitorWidth monitor = fromIntegral <$> c'getMonitorWidth (fromIntegral monitor)

foreign import ccall unsafe "raylib.h &GetMonitorWidth"
  p'getMonitorWidth ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetMonitorHeight"
  c'getMonitorHeight ::
    CInt -> IO CInt

getMonitorHeight :: Int -> IO CInt
getMonitorHeight monitor = fromIntegral <$> c'getMonitorHeight (fromIntegral monitor)

foreign import ccall unsafe "raylib.h &GetMonitorHeight"
  p'getMonitorHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetMonitorPhysicalWidth"
  c'getMonitorPhysicalWidth ::
    CInt -> IO CInt

getMonitorPhysicalWidth :: Int -> IO CInt
getMonitorPhysicalWidth monitor = fromIntegral <$> c'getMonitorPhysicalWidth (fromIntegral monitor)

foreign import ccall unsafe "raylib.h &GetMonitorPhysicalWidth"
  p'getMonitorPhysicalWidth ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetMonitorPhysicalHeight"
  c'getMonitorPhysicalHeight ::
    CInt -> IO CInt

getMonitorPhysicalHeight :: Int -> IO Int
getMonitorPhysicalHeight monitor = fromIntegral <$> c'getMonitorPhysicalHeight (fromIntegral monitor)

foreign import ccall unsafe "raylib.h &GetMonitorPhysicalHeight"
  p'getMonitorPhysicalHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetMonitorRefreshRate"
  c'getMonitorRefreshRate ::
    CInt -> IO CInt

getMonitorRefreshRate :: Int -> IO Int
getMonitorRefreshRate monitor = fromIntegral <$> c'getMonitorRefreshRate (fromIntegral monitor)

foreign import ccall unsafe "raylib.h &GetMonitorRefreshRate"
  p'getMonitorRefreshRate ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "bindings.h GetWindowPosition_" c'getWindowPosition :: IO (Ptr Vector2)

getWindowPosition :: IO Vector2
getWindowPosition = c'getWindowPosition >>= pop

foreign import ccall unsafe "raylib.h &GetWindowPosition"
  p'getWindowPosition ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "bindings.h GetWindowScaleDPI_" c'getWindowScaleDPI :: IO (Ptr Vector2)

getWindowScaleDPI :: IO Vector2
getWindowScaleDPI = c'getWindowScaleDPI >>= pop

foreign import ccall unsafe "raylib.h &GetWindowScaleDPI"
  p'getWindowScaleDPI ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "raylib.h GetMonitorName"
  c'getMonitorName ::
    CInt -> IO CString

getMonitorName :: Int -> IO String
getMonitorName monitor = c'getMonitorName (fromIntegral monitor) >>= peekCString

foreign import ccall unsafe "raylib.h &GetMonitorName"
  p'getMonitorName ::
    FunPtr (CInt -> IO CString)

foreign import ccall unsafe "raylib.h SetClipboardText"
  c'setClipboardText ::
    CString -> IO ()

setClipboardText :: String -> IO ()
setClipboardText text = withCString text c'setClipboardText

foreign import ccall unsafe "raylib.h &SetClipboardText"
  p'setClipboardText ::
    FunPtr (CString -> IO ())

foreign import ccall unsafe "raylib.h GetClipboardText"
  c'getClipboardText ::
    IO CString

getClipboardText :: IO String
getClipboardText = c'getClipboardText >>= peekCString

foreign import ccall unsafe "raylib.h &GetClipboardText"
  p'getClipboardText ::
    FunPtr (IO CString)

foreign import ccall unsafe "raylib.h EnableEventWaiting"
  enableEventWaiting ::
    IO ()

foreign import ccall unsafe "raylib.h &EnableEventWaiting"
  p'enableEventWaiting ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h DisableEventWaiting"
  disableEventWaiting ::
    IO ()

foreign import ccall unsafe "raylib.h &DisableEventWaiting"
  p'disableEventWaiting ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h SwapScreenBuffer"
  swapScreenBuffer ::
    IO ()

foreign import ccall unsafe "raylib.h &SwapScreenBuffer"
  p'swapScreenBuffer ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h PollInputEvents"
  pollInputEvents ::
    IO ()

foreign import ccall unsafe "raylib.h &PollInputEvents"
  p'pollInputEvents ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h WaitTime"
  c'waitTime ::
    CDouble -> IO ()

waitTime :: Double -> IO ()
waitTime seconds = c'waitTime $ realToFrac seconds

foreign import ccall unsafe "raylib.h &WaitTime"
  p'waitTime ::
    FunPtr (CDouble -> IO ())

foreign import ccall unsafe "raylib.h ShowCursor"
  showCursor ::
    IO ()

foreign import ccall unsafe "raylib.h &ShowCursor"
  p'showCursor ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h HideCursor"
  hideCursor ::
    IO ()

foreign import ccall unsafe "raylib.h &HideCursor"
  p'hideCursor ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h IsCursorHidden"
  c'isCursorHidden ::
    IO CInt

isCursorHidden :: IO Bool
isCursorHidden = toBool <$> c'isCursorHidden

foreign import ccall unsafe "raylib.h &IsCursorHidden"
  p'isCursorHidden ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h EnableCursor"
  enableCursor ::
    IO ()

foreign import ccall unsafe "raylib.h &EnableCursor"
  p'enableCursor ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h DisableCursor"
  disableCursor ::
    IO ()

foreign import ccall unsafe "raylib.h &DisableCursor"
  p'disableCursor ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h IsCursorOnScreen"
  c'isCursorOnScreen ::
    IO CInt

isCursorOnScreen :: IO Bool
isCursorOnScreen = toBool <$> c'isCursorOnScreen

foreign import ccall unsafe "raylib.h &IsCursorOnScreen"
  p'isCursorOnScreen ::
    FunPtr (IO CInt)

foreign import ccall unsafe "bindings.h ClearBackground_" c'clearBackground :: Ptr Color -> IO ()

clearBackground :: Color -> IO ()
clearBackground color = with color c'clearBackground

foreign import ccall unsafe "raylib.h &ClearBackground"
  p'clearBackground ::
    FunPtr (Color -> IO ())

foreign import ccall unsafe "raylib.h BeginDrawing"
  beginDrawing ::
    IO ()

foreign import ccall unsafe "raylib.h &BeginDrawing"
  p'beginDrawing ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h EndDrawing"
  endDrawing ::
    IO ()

foreign import ccall unsafe "raylib.h &EndDrawing"
  p'endDrawing ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h BeginMode2D_" c'beginMode2D :: Ptr Camera2D -> IO ()

beginMode2D :: Camera2D -> IO ()
beginMode2D camera = with camera c'beginMode2D

foreign import ccall unsafe "raylib.h &BeginMode2D"
  p'beginMode2D ::
    FunPtr (Camera2D -> IO ())

foreign import ccall unsafe "raylib.h EndMode2D"
  endMode2D ::
    IO ()

foreign import ccall unsafe "raylib.h &EndMode2D"
  p'endMode2D ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h BeginMode3D_" c'beginMode3D :: Ptr Camera3D -> IO ()

beginMode3D :: Camera3D -> IO ()
beginMode3D camera = with camera c'beginMode3D

foreign import ccall unsafe "raylib.h &BeginMode3D"
  p'beginMode3D ::
    FunPtr (Camera3D -> IO ())

foreign import ccall unsafe "raylib.h EndMode3D"
  endMode3D ::
    IO ()

foreign import ccall unsafe "raylib.h &EndMode3D"
  p'endMode3D ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h BeginTextureMode_" c'beginTextureMode :: Ptr RenderTexture -> IO ()

beginTextureMode :: RenderTexture -> IO ()
beginTextureMode renderTexture = with renderTexture c'beginTextureMode

foreign import ccall unsafe "raylib.h &BeginTextureMode"
  p'beginTextureMode ::
    FunPtr (RenderTexture -> IO ())

foreign import ccall unsafe "raylib.h EndTextureMode"
  endTextureMode ::
    IO ()

foreign import ccall unsafe "raylib.h &EndTextureMode"
  p'endTextureMode ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h BeginShaderMode_" c'beginShaderMode :: Ptr Shader -> IO ()

beginShaderMode :: Shader -> IO ()
beginShaderMode shader = with shader c'beginShaderMode

foreign import ccall unsafe "raylib.h &BeginShaderMode"
  p'beginShaderMode ::
    FunPtr (Shader -> IO ())

foreign import ccall unsafe "raylib.h EndShaderMode"
  endShaderMode ::
    IO ()

foreign import ccall unsafe "raylib.h &EndShaderMode"
  p'endShaderMode ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h BeginBlendMode"
  c'beginBlendMode ::
    CInt -> IO ()

beginBlendMode :: Int -> IO ()
beginBlendMode = c'beginBlendMode . fromIntegral

foreign import ccall unsafe "raylib.h &BeginBlendMode"
  p'beginBlendMode ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h EndBlendMode"
  endBlendMode ::
    IO ()

foreign import ccall unsafe "raylib.h &EndBlendMode"
  p'endBlendMode ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h BeginScissorMode"
  c'beginScissorMode ::
    CInt -> CInt -> CInt -> CInt -> IO ()

beginScissorMode :: Int -> Int -> Int -> Int -> IO ()
beginScissorMode x y width height = c'beginScissorMode (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

foreign import ccall unsafe "raylib.h &BeginScissorMode"
  p'beginScissorMode ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h EndScissorMode"
  endScissorMode ::
    IO ()

foreign import ccall unsafe "raylib.h &EndScissorMode"
  p'endScissorMode ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h BeginVrStereoMode_" c'beginVrStereoMode :: Ptr VrStereoConfig -> IO ()

beginVrStereoMode :: VrStereoConfig -> IO ()
beginVrStereoMode config = with config c'beginVrStereoMode

foreign import ccall unsafe "raylib.h &BeginVrStereoMode"
  p'beginVrStereoMode ::
    FunPtr (VrStereoConfig -> IO ())

foreign import ccall unsafe "raylib.h EndVrStereoMode"
  endVrStereoMode ::
    IO ()

foreign import ccall unsafe "raylib.h &EndVrStereoMode"
  p'endVrStereoMode ::
    FunPtr (IO ())

foreign import ccall unsafe "bindings.h LoadVrStereoConfig_" c'loadVrStereoConfig :: Ptr VrDeviceInfo -> IO (Ptr VrStereoConfig)

loadVrStereoConfig :: VrDeviceInfo -> IO VrStereoConfig
loadVrStereoConfig deviceInfo = with deviceInfo c'loadVrStereoConfig >>= pop

foreign import ccall unsafe "raylib.h &LoadVrStereoConfig"
  p'loadVrStereoConfig ::
    FunPtr (VrDeviceInfo -> IO VrStereoConfig)

foreign import ccall unsafe "bindings.h UnloadVrStereoConfig_" c'unloadVrStereoConfig :: Ptr VrStereoConfig -> IO ()

unloadVrStereoConfig :: VrStereoConfig -> IO ()
unloadVrStereoConfig config = with config c'unloadVrStereoConfig

foreign import ccall unsafe "raylib.h &UnloadVrStereoConfig"
  p'unloadVrStereoConfig ::
    FunPtr (VrStereoConfig -> IO ())

foreign import ccall unsafe "bindings.h LoadShader_" c'loadShader :: CString -> CString -> IO (Ptr Shader)

loadShader :: String -> String -> IO Shader
loadShader vsFileName fsFileName = withCString vsFileName (withCString fsFileName . c'loadShader) >>= pop

foreign import ccall unsafe "raylib.h &LoadShader"
  p'loadShader ::
    FunPtr (CString -> CString -> IO Shader)

foreign import ccall unsafe "bindings.h LoadShaderFromMemory_" c'loadShaderFromMemory :: CString -> CString -> IO (Ptr Shader)

loadShaderFromMemory :: String -> String -> IO Shader
loadShaderFromMemory vsCode fsCode = withCString vsCode (withCString fsCode . c'loadShaderFromMemory) >>= pop

foreign import ccall unsafe "raylib.h &LoadShaderFromMemory"
  p'loadShaderFromMemory ::
    FunPtr (CString -> CString -> IO Shader)

foreign import ccall unsafe "bindings.h GetShaderLocation_" c'getShaderLocation :: Ptr Shader -> CString -> IO CInt

getShaderLocation :: Shader -> String -> IO Int
getShaderLocation shader uniformName = fromIntegral <$> with shader (withCString uniformName . c'getShaderLocation)

foreign import ccall unsafe "raylib.h &GetShaderLocation"
  p'getShaderLocation ::
    FunPtr (Shader -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h GetShaderLocationAttrib_" c'getShaderLocationAttrib :: Ptr Shader -> CString -> IO CInt

getShaderLocationAttrib :: Shader -> String -> IO Int
getShaderLocationAttrib shader attribName = fromIntegral <$> with shader (withCString attribName . c'getShaderLocationAttrib)

foreign import ccall unsafe "raylib.h &GetShaderLocationAttrib"
  p'getShaderLocationAttrib ::
    FunPtr (Shader -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h SetShaderValue_" c'setShaderValue :: Ptr Shader -> CInt -> Ptr () -> CInt -> IO ()

setShaderValue :: (Storable a) => Shader -> Int -> a -> Int -> IO ()
setShaderValue shader locIndex value uniformType = with value (\v -> with shader (\s -> c'setShaderValue s (fromIntegral locIndex) (castPtr v) (fromIntegral uniformType)))

foreign import ccall unsafe "raylib.h &SetShaderValue"
  p'setShaderValue ::
    FunPtr (Shader -> CInt -> Ptr () -> CInt -> IO ())

foreign import ccall unsafe "bindings.h SetShaderValueV_" c'setShaderValueV :: Ptr Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()

setShaderValueV :: (Storable a) => Shader -> Int -> a -> Int -> Int -> IO ()
setShaderValueV shader locIndex value uniformType count = with value (\v -> with shader (\s -> c'setShaderValueV s (fromIntegral locIndex) (castPtr v) (fromIntegral uniformType) (fromIntegral count)))

foreign import ccall unsafe "raylib.h &SetShaderValueV"
  p'setShaderValueV ::
    FunPtr (Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall unsafe "bindings.h SetShaderValueMatrix_" c'setShaderValueMatrix :: Ptr Shader -> CInt -> Ptr Matrix -> IO ()

setShaderValueMatrix :: Shader -> Int -> Matrix -> IO ()
setShaderValueMatrix shader locIndex mat = with shader (\s -> with mat (c'setShaderValueMatrix s (fromIntegral locIndex)))

foreign import ccall unsafe "raylib.h &SetShaderValueMatrix"
  p'setShaderValueMatrix ::
    FunPtr (Shader -> CInt -> Matrix -> IO ())

foreign import ccall unsafe "bindings.h SetShaderValueTexture_" c'setShaderValueTexture :: Ptr Shader -> CInt -> Ptr Texture -> IO ()

setShaderValueTexture :: Shader -> Int -> Texture -> IO ()
setShaderValueTexture shader locIndex tex = with shader (\s -> with tex (c'setShaderValueTexture s (fromIntegral locIndex)))

foreign import ccall unsafe "raylib.h &SetShaderValueTexture"
  p'setShaderValueTexture ::
    FunPtr (Shader -> CInt -> Texture -> IO ())

foreign import ccall unsafe "bindings.h UnloadShader_" c'unloadShader :: Ptr Shader -> IO ()

unloadShader :: Shader -> IO ()
unloadShader shader = with shader c'unloadShader

foreign import ccall unsafe "raylib.h &UnloadShader"
  p'unloadShader ::
    FunPtr (Shader -> IO ())

foreign import ccall unsafe "bindings.h GetMouseRay_" c'getMouseRay :: Ptr Vector2 -> Ptr Camera3D -> IO (Ptr Ray)

getMouseRay :: Vector2 -> Camera3D -> IO Ray
getMouseRay mousePosition camera = with mousePosition (with camera . c'getMouseRay) >>= pop

foreign import ccall unsafe "raylib.h &GetMouseRay"
  p'getMouseRay ::
    FunPtr (Vector2 -> Camera3D -> IO Ray)

foreign import ccall unsafe "bindings.h GetCameraMatrix_" c'getCameraMatrix :: Ptr Camera3D -> IO (Ptr Matrix)

getCameraMatrix :: Camera3D -> IO Matrix
getCameraMatrix camera = with camera c'getCameraMatrix >>= pop

foreign import ccall unsafe "raylib.h &GetCameraMatrix"
  p'getCameraMatrix ::
    FunPtr (Camera3D -> IO Matrix)

foreign import ccall unsafe "bindings.h GetCameraMatrix2D_" c'getCameraMatrix2D :: Ptr Camera2D -> IO (Ptr Matrix)

getCameraMatrix2D :: Camera2D -> IO Matrix
getCameraMatrix2D camera = with camera c'getCameraMatrix2D >>= pop

foreign import ccall unsafe "raylib.h &GetCameraMatrix2D"
  p'getCameraMatrix2D ::
    FunPtr (Camera2D -> IO Matrix)

foreign import ccall unsafe "bindings.h GetWorldToScreen_" c'getWorldToScreen :: Ptr Vector3 -> Ptr Camera3D -> IO (Ptr Vector2)

getWorldToScreen :: Vector3 -> Camera3D -> IO Vector2
getWorldToScreen position camera = with position (with camera . c'getWorldToScreen) >>= pop

foreign import ccall unsafe "raylib.h &GetWorldToScreen"
  p'getWorldToScreen ::
    FunPtr (Vector3 -> Camera3D -> IO Vector2)

foreign import ccall unsafe "bindings.h GetScreenToWorld2D_" c'getScreenToWorld2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)

getScreenToWorld2D :: Vector2 -> Camera2D -> IO Vector2
getScreenToWorld2D position camera = with position (with camera . c'getScreenToWorld2D) >>= pop

foreign import ccall unsafe "raylib.h &GetScreenToWorld2D"
  p'getScreenToWorld2D ::
    FunPtr (Vector2 -> Camera2D -> IO Vector2)

foreign import ccall unsafe "bindings.h GetWorldToScreenEx_" c'getWorldToScreenEx :: Ptr Vector3 -> Ptr Camera3D -> CInt -> CInt -> IO (Ptr Vector2)

getWorldToScreenEx :: Vector3 -> Camera3D -> Int -> Int -> IO Vector2
getWorldToScreenEx position camera width height = with position (\p -> with camera (\c -> c'getWorldToScreenEx p c (fromIntegral width) (fromIntegral height))) >>= pop

foreign import ccall unsafe "raylib.h &GetWorldToScreenEx"
  p'getWorldToScreenEx ::
    FunPtr (Vector3 -> Camera3D -> CInt -> CInt -> IO Vector2)

foreign import ccall unsafe "bindings.h GetWorldToScreen2D_" c'getWorldToScreen2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)

getWorldToScreen2D :: Vector2 -> Camera2D -> IO Vector2
getWorldToScreen2D position camera = with position (with camera . c'getWorldToScreen2D) >>= pop

foreign import ccall unsafe "raylib.h &GetWorldToScreen2D"
  p'getWorldToScreen2D ::
    FunPtr (Vector2 -> Camera2D -> IO Vector2)

foreign import ccall unsafe "raylib.h SetTargetFPS"
  c'setTargetFPS ::
    CInt -> IO ()

setTargetFPS :: Int -> IO ()
setTargetFPS fps = c'setTargetFPS $ fromIntegral fps

foreign import ccall unsafe "raylib.h &SetTargetFPS"
  p'setTargetFPS ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h GetFPS"
  c'getFPS ::
    IO CInt

getFPS :: IO Int
getFPS = fromIntegral <$> c'getFPS

foreign import ccall unsafe "raylib.h &GetFPS"
  p'getFPS ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetFrameTime"
  c'getFrameTime ::
    IO CFloat

getFrameTime :: IO Float
getFrameTime = realToFrac <$> c'getFrameTime

foreign import ccall unsafe "raylib.h &GetFrameTime"
  p'getFrameTime ::
    FunPtr (IO CFloat)

foreign import ccall unsafe "raylib.h GetTime"
  c'getTime ::
    IO CDouble

getTime :: IO Double
getTime = realToFrac <$> c'getTime

foreign import ccall unsafe "raylib.h &GetTime"
  p'getTime ::
    FunPtr (IO CDouble)

foreign import ccall unsafe "raylib.h GetRandomValue"
  c'getRandomValue ::
    CInt -> CInt -> IO CInt

getRandomValue :: Int -> Int -> IO Int
getRandomValue min max = fromIntegral <$> c'getRandomValue (fromIntegral min) (fromIntegral max)

foreign import ccall unsafe "raylib.h &GetRandomValue"
  p'getRandomValue ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall unsafe "raylib.h SetRandomSeed"
  c'setRandomSeed ::
    CUInt -> IO ()

setRandomSeed :: Integer -> IO ()
setRandomSeed seed = c'setRandomSeed $ fromIntegral seed

foreign import ccall unsafe "raylib.h &SetRandomSeed"
  p'setRandomSeed ::
    FunPtr (CUInt -> IO ())

foreign import ccall unsafe "raylib.h TakeScreenshot"
  c'takeScreenshot ::
    CString -> IO ()

takeScreenshot :: String -> IO ()
takeScreenshot fileName = withCString fileName c'takeScreenshot

foreign import ccall unsafe "raylib.h &TakeScreenshot"
  p'takeScreenshot ::
    FunPtr (CString -> IO ())

foreign import ccall unsafe "raylib.h SetConfigFlags"
  c'setConfigFlags ::
    CUInt -> IO ()

setConfigFlags :: Integer -> IO ()
setConfigFlags flags = c'setConfigFlags $ fromIntegral flags

foreign import ccall unsafe "raylib.h &SetConfigFlags"
  p'setConfigFlags ::
    FunPtr (CUInt -> IO ())

foreign import ccall unsafe "raylib.h TraceLog"
  c'traceLog ::
    CInt -> CString -> IO () -- Uses varags, can't implement complete functionality

traceLog :: Int -> String -> IO ()
traceLog logLevel text = withCString text $ c'traceLog $ fromIntegral logLevel

foreign import ccall unsafe "raylib.h &TraceLog"
  p'traceLog ::
    FunPtr (CInt -> CString -> IO ())

foreign import ccall unsafe "raylib.h SetTraceLogLevel"
  c'setTraceLogLevel ::
    CInt -> IO ()

setTraceLogLevel :: Int -> IO ()
setTraceLogLevel logLevel = c'setTraceLogLevel $ fromIntegral logLevel

foreign import ccall unsafe "raylib.h &SetTraceLogLevel"
  p'setTraceLogLevel ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h MemAlloc"
  c'memAlloc ::
    CInt -> IO (Ptr ())

memAlloc :: (Storable a) => Int -> IO (Ptr a)
memAlloc size = castPtr <$> c'memAlloc (fromIntegral size)

foreign import ccall unsafe "raylib.h &MemAlloc"
  p'memAlloc ::
    FunPtr (CInt -> IO (Ptr ()))

foreign import ccall unsafe "raylib.h MemRealloc"
  c'memRealloc ::
    Ptr () -> CInt -> IO (Ptr ())

memRealloc :: (Storable a, Storable b) => Ptr a -> Int -> IO (Ptr b)
memRealloc ptr size = castPtr <$> c'memRealloc (castPtr ptr) (fromIntegral size)

foreign import ccall unsafe "raylib.h &MemRealloc"
  p'memRealloc ::
    FunPtr (Ptr () -> CInt -> IO (Ptr ()))

foreign import ccall unsafe "raylib.h MemFree"
  c'memFree ::
    Ptr () -> IO ()

memFree :: (Storable a) => Ptr a -> IO ()
memFree = c'memFree . castPtr

foreign import ccall unsafe "raylib.h &MemFree"
  p'memFree ::
    FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "raylib.h OpenURL"
  c'openURL ::
    CString -> IO ()

openURL :: String -> IO ()
openURL url = withCString url c'openURL

foreign import ccall unsafe "raylib.h &OpenURL"
  p'openURL ::
    FunPtr (CString -> IO ())

-- These functions use varargs so they can't be implemented through FFI
-- foreign import ccall unsafe "raylib.h SetTraceLogCallback"
--   SetTraceLogCallback ::
--     TraceLogCallback -> IO ()
-- foreign import ccall unsafe "raylib.h &SetTraceLogCallback"
--   p'SetTraceLogCallback ::
--     FunPtr (TraceLogCallback -> IO ())

foreign import ccall unsafe "raylib.h SetLoadFileDataCallback"
  setLoadFileDataCallback ::
    LoadFileDataCallback -> IO ()

foreign import ccall unsafe "raylib.h &SetLoadFileDataCallback"
  p'setLoadFileDataCallback ::
    FunPtr (LoadFileDataCallback -> IO ())

foreign import ccall unsafe "raylib.h SetSaveFileDataCallback"
  setSaveFileDataCallback ::
    SaveFileDataCallback -> IO ()

foreign import ccall unsafe "raylib.h &SetSaveFileDataCallback"
  p'setSaveFileDataCallback ::
    FunPtr (SaveFileDataCallback -> IO ())

foreign import ccall unsafe "raylib.h SetLoadFileTextCallback"
  setLoadFileTextCallback ::
    LoadFileTextCallback -> IO ()

foreign import ccall unsafe "raylib.h &SetLoadFileTextCallback"
  p'setLoadFileTextCallback ::
    FunPtr (LoadFileTextCallback -> IO ())

foreign import ccall unsafe "raylib.h SetSaveFileTextCallback"
  setSaveFileTextCallback ::
    SaveFileTextCallback -> IO ()

foreign import ccall unsafe "raylib.h &SetSaveFileTextCallback"
  p'setSaveFileTextCallback ::
    FunPtr (SaveFileTextCallback -> IO ())

foreign import ccall unsafe "raylib.h LoadFileData"
  c'loadFileData ::
    CString -> Ptr CUInt -> IO (Ptr CUChar)

-- | Returns the file data and size in bytes in a tuple, e.g. @(\"Contents\", 8)@
loadFileData :: String -> IO (String, Integer)
loadFileData fileName =
  with
    0
    ( \size -> do
        withCString
          fileName
          ( \path -> do
              contents <- c'loadFileData path size >>= peekCString . castPtr
              bytesRead <- fromIntegral <$> peek size
              return (contents, bytesRead)
          )
    )

foreign import ccall unsafe "raylib.h &LoadFileData"
  p'loadFileData ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall unsafe "raylib.h UnloadFileData"
  unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall unsafe "raylib.h &UnloadFileData"
  p'unloadFileData ::
    FunPtr (Ptr CUChar -> IO ())

foreign import ccall unsafe "raylib.h SaveFileData"
  c'saveFileData ::
    CString -> Ptr () -> CUInt -> IO CInt

saveFileData :: (Storable a) => String -> Ptr a -> Integer -> IO Bool
saveFileData fileName contents bytesToWrite =
  toBool <$> withCString fileName (\s -> c'saveFileData s (castPtr contents) (fromIntegral bytesToWrite))

foreign import ccall unsafe "raylib.h &SaveFileData"
  p'saveFileData ::
    FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall unsafe "raylib.h ExportDataAsCode"
  c'exportDataAsCode ::
    CString -> CUInt -> CString -> IO CInt

exportDataAsCode :: String -> Integer -> String -> IO Bool
exportDataAsCode contents size fileName =
  toBool <$> withCString contents (\c -> withCString fileName (c'exportDataAsCode c (fromIntegral size)))

foreign import ccall unsafe "raylib.h &ExportDataAsCode"
  p'exportDataAsCode ::
    FunPtr (CString -> CUInt -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h LoadFileText"
  c'loadFileText ::
    CString -> IO CString

loadFileText :: String -> IO String
loadFileText fileName = withCString fileName c'loadFileText >>= peekCString

foreign import ccall unsafe "raylib.h &LoadFileText"
  p'loadFileText ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h UnloadFileText"
  c'unloadFileText ::
    CString -> IO ()

unloadFileText :: String -> IO ()
unloadFileText text = withCString text c'unloadFileText

foreign import ccall unsafe "raylib.h &UnloadFileText"
  p'unloadFileText ::
    FunPtr (CString -> IO ())

foreign import ccall unsafe "raylib.h SaveFileText"
  c'saveFileText ::
    CString -> CString -> IO CInt

saveFileText :: String -> String -> IO Bool
saveFileText fileName text = toBool <$> withCString fileName (withCString text . c'saveFileText)

foreign import ccall unsafe "raylib.h &SaveFileText"
  p'saveFileText ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h FileExists"
  c'fileExists ::
    CString -> IO CInt

fileExists :: String -> IO Bool
fileExists fileName = toBool <$> withCString fileName c'fileExists

foreign import ccall unsafe "raylib.h &FileExists"
  p'fileExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "raylib.h DirectoryExists"
  c'directoryExists ::
    CString -> IO CInt

directoryExists :: String -> IO Bool
directoryExists dirPath = toBool <$> withCString dirPath c'directoryExists

foreign import ccall unsafe "raylib.h &DirectoryExists"
  p'directoryExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "raylib.h IsFileExtension"
  c'isFileExtension ::
    CString -> CString -> IO CInt

isFileExtension :: String -> String -> IO Bool
isFileExtension fileName ext = toBool <$> withCString fileName (withCString ext . c'isFileExtension)

foreign import ccall unsafe "raylib.h &IsFileExtension"
  p'isFileExtension ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h GetFileLength"
  c'getFileLength ::
    CString -> IO CInt

getFileLength :: String -> IO Bool
getFileLength fileName = toBool <$> withCString fileName c'getFileLength

foreign import ccall unsafe "raylib.h &GetFileLength"
  p'getFileLength ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "raylib.h GetFileExtension"
  c'getFileExtension ::
    CString -> IO CString

getFileExtension :: String -> IO String
getFileExtension fileName = withCString fileName c'getFileExtension >>= peekCString

foreign import ccall unsafe "raylib.h &GetFileExtension"
  p'getFileExtension ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h GetFileName"
  c'getFileName ::
    CString -> IO CString

getFileName :: String -> IO String
getFileName filePath = withCString filePath c'getFileName >>= peekCString

foreign import ccall unsafe "raylib.h &GetFileName"
  p'getFileName ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h GetFileNameWithoutExt"
  c'getFileNameWithoutExt ::
    CString -> IO CString

getFileNameWithoutExt :: String -> IO String
getFileNameWithoutExt fileName = withCString fileName c'getFileNameWithoutExt >>= peekCString

foreign import ccall unsafe "raylib.h &GetFileNameWithoutExt"
  p'getFileNameWithoutExt ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h GetDirectoryPath"
  c'getDirectoryPath ::
    CString -> IO CString

getDirectoryPath :: String -> IO String
getDirectoryPath filePath = withCString filePath c'getDirectoryPath >>= peekCString

foreign import ccall unsafe "raylib.h &GetDirectoryPath"
  p'getDirectoryPath ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h GetPrevDirectoryPath"
  c'getPrevDirectoryPath ::
    CString -> IO CString

getPrevDirectoryPath :: String -> IO String
getPrevDirectoryPath dirPath = withCString dirPath c'getPrevDirectoryPath >>= peekCString

foreign import ccall unsafe "raylib.h &GetPrevDirectoryPath"
  p'getPrevDirectoryPath ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h GetWorkingDirectory"
  c'getWorkingDirectory ::
    IO CString

getWorkingDirectory :: IO String
getWorkingDirectory = c'getWorkingDirectory >>= peekCString

foreign import ccall unsafe "raylib.h &GetWorkingDirectory"
  p'getWorkingDirectory ::
    FunPtr (IO CString)

foreign import ccall unsafe "raylib.h GetApplicationDirectory"
  c'getApplicationDirectory ::
    IO CString

getApplicationDirectory :: IO String
getApplicationDirectory = c'getApplicationDirectory >>= peekCString

foreign import ccall unsafe "raylib.h &GetApplicationDirectory"
  p'getApplicationDirectory ::
    FunPtr (IO CString)

foreign import ccall unsafe "raylib.h ChangeDirectory"
  c'changeDirectory ::
    CString -> IO CInt

changeDirectory :: String -> IO Bool
changeDirectory dir = toBool <$> withCString dir c'changeDirectory

foreign import ccall unsafe "raylib.h &ChangeDirectory"
  p'changeDirectory ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "raylib.h IsPathFile"
  c'isPathFile ::
    CString -> IO CInt

isPathFile :: String -> IO Bool
isPathFile path = toBool <$> withCString path c'isPathFile

foreign import ccall unsafe "raylib.h &IsPathFile"
  p'isPathFile ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "bindings.h LoadDirectoryFiles_" c'loadDirectoryFiles :: CString -> IO (Ptr FilePathList)

loadDirectoryFiles :: String -> IO FilePathList
loadDirectoryFiles dirPath = withCString dirPath c'loadDirectoryFiles >>= pop

foreign import ccall unsafe "raylib.h &LoadDirectoryFiles"
  p'loadDirectoryFiles ::
    FunPtr (CString -> IO FilePathList)

foreign import ccall unsafe "bindings.h LoadDirectoryFilesEx_" c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr FilePathList)

loadDirectoryFilesEx :: String -> String -> Bool -> IO FilePathList
loadDirectoryFilesEx basePath filter scanSubdirs =
  withCString basePath (\b -> withCString filter (\f -> c'loadDirectoryFilesEx b f (fromBool scanSubdirs))) >>= pop

foreign import ccall unsafe "raylib.h &LoadDirectoryFilesEx"
  p'loadDirectoryFilesEx ::
    FunPtr (CString -> CString -> CInt -> IO FilePathList)

foreign import ccall unsafe "bindings.h UnloadDirectoryFiles_" c'unloadDirectoryFiles :: Ptr FilePathList -> IO ()

unloadDirectoryFiles :: FilePathList -> IO ()
unloadDirectoryFiles files = with files c'unloadDirectoryFiles

foreign import ccall unsafe "raylib.h &UnloadDirectoryFiles"
  p'unloadDirectoryFiles ::
    FunPtr (FilePathList -> IO ())

foreign import ccall unsafe "raylib.h IsFileDropped"
  c'isFileDropped ::
    IO CInt

isFileDropped :: IO Bool
isFileDropped = toBool <$> c'isFileDropped

foreign import ccall unsafe "raylib.h &IsFileDropped"
  p'isFileDropped ::
    FunPtr (IO CInt)

foreign import ccall unsafe "bindings.h LoadDroppedFiles_" c'loadDroppedFiles :: IO (Ptr FilePathList)

loadDroppedFiles :: IO FilePathList
loadDroppedFiles = c'loadDroppedFiles >>= pop

foreign import ccall unsafe "raylib.h &LoadDroppedFiles"
  p'loadDroppedFiles ::
    FunPtr (IO FilePathList)

foreign import ccall unsafe "bindings.h UnloadDroppedFiles_" c'unloadDroppedFiles :: Ptr FilePathList -> IO ()

unloadDroppedFiles :: FilePathList -> IO ()
unloadDroppedFiles files = with files c'unloadDroppedFiles

foreign import ccall unsafe "raylib.h &UnloadDroppedFiles"
  p'unloadDroppedFiles ::
    FunPtr (FilePathList -> IO ())

foreign import ccall unsafe "raylib.h GetFileModTime"
  c'getFileModTime ::
    CString -> IO CLong

getFileModTime :: String -> IO Integer
getFileModTime fileName = fromIntegral <$> withCString fileName c'getFileModTime

foreign import ccall unsafe "raylib.h &GetFileModTime"
  p'getFileModTime ::
    FunPtr (CString -> IO CLong)

foreign import ccall unsafe "raylib.h CompressData"
  c'compressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

compressData :: String -> Int -> IO (String, Integer)
compressData contents size = do
  withCString
    contents
    ( \c -> do
        with
          0
          ( \ptr -> do
              compressed <- c'compressData (castPtr c) (fromIntegral size) ptr >>= peekCString . castPtr
              compressedSize <- fromIntegral <$> peek ptr
              return (compressed, compressedSize)
          )
    )

foreign import ccall unsafe "raylib.h &CompressData"
  p'compressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall unsafe "raylib.h DecompressData"
  c'decompressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

decompressData :: String -> Int -> IO (String, Integer)
decompressData compressedData size = do
  withCString
    compressedData
    ( \c -> do
        with
          0
          ( \ptr -> do
              decompressed <- c'decompressData (castPtr c) (fromIntegral size) ptr >>= peekCString . castPtr
              decompressedSize <- fromIntegral <$> peek ptr
              return (decompressed, decompressedSize)
          )
    )

foreign import ccall unsafe "raylib.h &DecompressData"
  p'decompressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall unsafe "raylib.h EncodeDataBase64"
  c'encodeDataBase64 ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO CString

encodeDataBase64 :: String -> Int -> IO (String, Integer)
encodeDataBase64 contents size = do
  withCString
    contents
    ( \c -> do
        with
          0
          ( \ptr -> do
              encoded <- c'encodeDataBase64 (castPtr c) (fromIntegral size) ptr >>= peekCString . castPtr
              encodedSize <- fromIntegral <$> peek ptr
              return (encoded, encodedSize)
          )
    )

foreign import ccall unsafe "raylib.h &EncodeDataBase64"
  p'encodeDataBase64 ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO CString)

foreign import ccall unsafe "raylib.h DecodeDataBase64"
  c'decodeDataBase64 ::
    Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)

decodeDataBase64 :: String -> IO (String, Integer)
decodeDataBase64 encodedData = do
  withCString
    encodedData
    ( \c -> do
        with
          0
          ( \ptr -> do
              decoded <- c'decodeDataBase64 (castPtr c) ptr >>= peekCString . castPtr
              decodedSize <- fromIntegral <$> peek ptr
              return (decoded, decodedSize)
          )
    )

foreign import ccall unsafe "raylib.h &DecodeDataBase64"
  p'decodeDataBase64 ::
    FunPtr (Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall unsafe "raylib.h IsKeyPressed"
  c'isKeyPressed ::
    CInt -> IO CInt

isKeyPressed :: Int -> IO Bool
isKeyPressed key = toBool <$> c'isKeyPressed (fromIntegral key)

foreign import ccall unsafe "raylib.h &IsKeyPressed"
  p'isKeyPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsKeyDown"
  c'isKeyDown ::
    CInt -> IO CInt

isKeyDown :: Int -> IO Bool
isKeyDown key = toBool <$> c'isKeyDown (fromIntegral key)

foreign import ccall unsafe "raylib.h &IsKeyDown"
  p'isKeyDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsKeyReleased"
  c'isKeyReleased ::
    CInt -> IO CInt

isKeyReleased :: Int -> IO Bool
isKeyReleased key = toBool <$> c'isKeyReleased (fromIntegral key)

foreign import ccall unsafe "raylib.h &IsKeyReleased"
  p'isKeyReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsKeyUp"
  c'isKeyUp ::
    CInt -> IO CInt

isKeyUp :: Int -> IO Bool
isKeyUp key = toBool <$> c'isKeyUp (fromIntegral key)

foreign import ccall unsafe "raylib.h &IsKeyUp"
  p'isKeyUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h SetExitKey"
  c'setExitKey ::
    CInt -> IO ()

setExitKey :: Int -> IO ()
setExitKey = c'setExitKey . fromIntegral

foreign import ccall unsafe "raylib.h &SetExitKey"
  p'setExitKey ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h GetKeyPressed"
  c'getKeyPressed ::
    IO CInt

getKeyPressed :: IO Int
getKeyPressed = fromIntegral <$> c'getKeyPressed

foreign import ccall unsafe "raylib.h &GetKeyPressed"
  p'getKeyPressed ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetCharPressed"
  c'getCharPressed ::
    IO CInt

getCharPressed :: IO Int
getCharPressed = fromIntegral <$> c'getCharPressed

foreign import ccall unsafe "raylib.h &GetCharPressed"
  p'getCharPressed ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h IsGamepadAvailable"
  c'isGamepadAvailable ::
    CInt -> IO CInt

isGamepadAvailable :: Int -> IO Bool
isGamepadAvailable gamepad = toBool <$> c'isGamepadAvailable (fromIntegral gamepad)

foreign import ccall unsafe "raylib.h &IsGamepadAvailable"
  p'isGamepadAvailable ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetGamepadName"
  c'getGamepadName ::
    CInt -> IO CString

getGamepadName :: Int -> IO String
getGamepadName gamepad = c'getGamepadName (fromIntegral gamepad) >>= peekCString

foreign import ccall unsafe "raylib.h &GetGamepadName"
  p'getGamepadName ::
    FunPtr (CInt -> IO CString)

foreign import ccall unsafe "raylib.h IsGamepadButtonPressed"
  c'isGamepadButtonPressed ::
    CInt -> CInt -> IO CInt

isGamepadButtonPressed :: Int -> Int -> IO Bool
isGamepadButtonPressed gamepad button = toBool <$> c'isGamepadButtonPressed (fromIntegral gamepad) (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsGamepadButtonPressed"
  p'isGamepadButtonPressed ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsGamepadButtonDown"
  c'isGamepadButtonDown ::
    CInt -> CInt -> IO CInt

isGamepadButtonDown :: Int -> Int -> IO Bool
isGamepadButtonDown gamepad button = toBool <$> c'isGamepadButtonDown (fromIntegral gamepad) (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsGamepadButtonDown"
  p'isGamepadButtonDown ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsGamepadButtonReleased"
  c'isGamepadButtonReleased ::
    CInt -> CInt -> IO CInt

isGamepadButtonReleased :: Int -> Int -> IO Bool
isGamepadButtonReleased gamepad button = toBool <$> c'isGamepadButtonReleased (fromIntegral gamepad) (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsGamepadButtonReleased"
  p'isGamepadButtonReleased ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsGamepadButtonUp"
  c'isGamepadButtonUp ::
    CInt -> CInt -> IO CInt

isGamepadButtonUp :: Int -> Int -> IO Bool
isGamepadButtonUp gamepad button = toBool <$> c'isGamepadButtonUp (fromIntegral gamepad) (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsGamepadButtonUp"
  p'isGamepadButtonUp ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetGamepadButtonPressed"
  c'getGamepadButtonPressed ::
    IO CInt

getGamepadButtonPressed :: IO Int
getGamepadButtonPressed = fromIntegral <$> c'getGamepadButtonPressed

foreign import ccall unsafe "raylib.h &GetGamepadButtonPressed"
  p'getGamepadButtonPressed ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetGamepadAxisCount"
  c'getGamepadAxisCount ::
    CInt -> IO CInt

getGamepadAxisCount :: Int -> IO Int
getGamepadAxisCount gamepad = fromIntegral <$> c'getGamepadAxisCount (fromIntegral gamepad)

foreign import ccall unsafe "raylib.h &GetGamepadAxisCount"
  p'getGamepadAxisCount ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetGamepadAxisMovement"
  c'getGamepadAxisMovement ::
    CInt -> CInt -> IO CFloat

getGamepadAxisMovement :: Int -> Int -> IO Float
getGamepadAxisMovement gamepad axis = realToFrac <$> c'getGamepadAxisMovement (fromIntegral gamepad) (fromIntegral axis)

foreign import ccall unsafe "raylib.h &GetGamepadAxisMovement"
  p'getGamepadAxisMovement ::
    FunPtr (CInt -> CInt -> IO CFloat)

foreign import ccall unsafe "raylib.h SetGamepadMappings"
  c'setGamepadMappings ::
    CString -> IO CInt

setGamepadMappings :: String -> IO Int
setGamepadMappings mappings = fromIntegral <$> withCString mappings c'setGamepadMappings

foreign import ccall unsafe "raylib.h &SetGamepadMappings"
  p'setGamepadMappings ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "raylib.h IsMouseButtonPressed"
  c'isMouseButtonPressed ::
    CInt -> IO CInt

isMouseButtonPressed :: Int -> IO Bool
isMouseButtonPressed button = toBool <$> c'isMouseButtonPressed (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsMouseButtonPressed"
  p'isMouseButtonPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsMouseButtonDown"
  c'isMouseButtonDown ::
    CInt -> IO CInt

isMouseButtonDown :: Int -> IO Bool
isMouseButtonDown button = toBool <$> c'isMouseButtonDown (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsMouseButtonDown"
  p'isMouseButtonDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsMouseButtonReleased"
  c'isMouseButtonReleased ::
    CInt -> IO CInt

isMouseButtonReleased :: Int -> IO Bool
isMouseButtonReleased button = toBool <$> c'isMouseButtonReleased (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsMouseButtonReleased"
  p'isMouseButtonReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h IsMouseButtonUp"
  c'isMouseButtonUp ::
    CInt -> IO CInt

isMouseButtonUp :: Int -> IO Bool
isMouseButtonUp button = toBool <$> c'isMouseButtonUp (fromIntegral button)

foreign import ccall unsafe "raylib.h &IsMouseButtonUp"
  p'isMouseButtonUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetMouseX"
  c'getMouseX ::
    IO CInt

getMouseX :: IO Int
getMouseX = fromIntegral <$> c'getMouseX

foreign import ccall unsafe "raylib.h &GetMouseX"
  p'getMouseX ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetMouseY"
  c'getMouseY ::
    IO CInt

getMouseY :: IO Int
getMouseY = fromIntegral <$> c'getMouseY

foreign import ccall unsafe "raylib.h &GetMouseY"
  p'getMouseY ::
    FunPtr (IO CInt)

foreign import ccall unsafe "bindings.h GetMousePosition_" c'getMousePosition :: IO (Ptr Vector2)

getMousePosition :: IO Vector2
getMousePosition = c'getMousePosition >>= pop

foreign import ccall unsafe "raylib.h &GetMousePosition"
  p'getMousePosition ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "bindings.h GetMouseDelta_" c'getMouseDelta :: IO (Ptr Vector2)

getMouseDelta :: IO Vector2
getMouseDelta = c'getMouseDelta >>= pop

foreign import ccall unsafe "raylib.h &GetMouseDelta"
  p'getMouseDelta ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "raylib.h SetMousePosition"
  c'setMousePosition ::
    CInt -> CInt -> IO ()

setMousePosition :: Int -> Int -> IO ()
setMousePosition x y = c'setMousePosition (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h &SetMousePosition"
  p'setMousePosition ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h SetMouseOffset"
  c'setMouseOffset ::
    CInt -> CInt -> IO ()

setMouseOffset :: Int -> Int -> IO ()
setMouseOffset x y = c'setMouseOffset (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h &SetMouseOffset"
  p'setMouseOffset ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h SetMouseScale"
  c'setMouseScale ::
    CFloat -> CFloat -> IO ()

setMouseScale :: Float -> Float -> IO ()
setMouseScale x y = c'setMouseScale (realToFrac x) (realToFrac y)

foreign import ccall unsafe "raylib.h &SetMouseScale"
  p'setMouseScale ::
    FunPtr (CFloat -> CFloat -> IO ())

foreign import ccall unsafe "raylib.h GetMouseWheelMove"
  c'getMouseWheelMove ::
    IO CFloat

getMouseWheelMove :: IO Float
getMouseWheelMove = realToFrac <$> c'getMouseWheelMove

foreign import ccall unsafe "raylib.h &GetMouseWheelMove"
  p'getMouseWheelMove ::
    FunPtr (IO CFloat)

foreign import ccall unsafe "bindings.h GetMouseWheelMoveV_" c'getMouseWheelMoveV :: IO (Ptr Vector2)

getMouseWheelMoveV :: IO Vector2
getMouseWheelMoveV = c'getMouseWheelMoveV >>= pop

foreign import ccall unsafe "raylib.h &GetMouseWheelMoveV"
  p'getMouseWheelMoveV ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "raylib.h SetMouseCursor"
  c'setMouseCursor ::
    CInt -> IO ()

setMouseCursor :: Int -> IO ()
setMouseCursor cursor = c'setMouseCursor $ fromIntegral cursor

foreign import ccall unsafe "raylib.h &SetMouseCursor"
  p'setMouseCursor ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h GetTouchX"
  c'getTouchX ::
    IO CInt

getTouchX :: IO Int
getTouchX = fromIntegral <$> c'getTouchX

foreign import ccall unsafe "raylib.h &GetTouchX"
  p'getTouchX ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetTouchY"
  c'getTouchY ::
    IO CInt

getTouchY :: IO Int
getTouchY = fromIntegral <$> c'getTouchY

foreign import ccall unsafe "raylib.h &GetTouchY"
  p'getTouchY ::
    FunPtr (IO CInt)

foreign import ccall unsafe "bindings.h GetTouchPosition_" c'getTouchPosition :: CInt -> IO (Ptr Vector2)

getTouchPosition :: Int -> IO Vector2
getTouchPosition index = c'getTouchPosition (fromIntegral index) >>= pop

foreign import ccall unsafe "raylib.h &GetTouchPosition"
  p'getTouchPosition ::
    FunPtr (CInt -> IO Vector2)

foreign import ccall unsafe "raylib.h GetTouchPointId"
  c'getTouchPointId ::
    CInt -> IO CInt

getTouchPointId :: Int -> IO Int
getTouchPointId index = fromIntegral <$> c'getTouchPointId (fromIntegral index)

foreign import ccall unsafe "raylib.h &GetTouchPointId"
  p'getTouchPointId ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetTouchPointCount"
  c'getTouchPointCount ::
    IO CInt

getTouchPointCount :: IO Int
getTouchPointCount = fromIntegral <$> c'getTouchPointCount

foreign import ccall unsafe "raylib.h &GetTouchPointCount"
  p'getTouchPointCount ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h SetGesturesEnabled"
  c'setGesturesEnabled ::
    CUInt -> IO ()

setGesturesEnabled :: Integer -> IO ()
setGesturesEnabled flags = c'setGesturesEnabled (fromIntegral flags)

foreign import ccall unsafe "raylib.h &SetGesturesEnabled"
  p'setGesturesEnabled ::
    FunPtr (CUInt -> IO ())

foreign import ccall unsafe "raylib.h IsGestureDetected"
  c'isGestureDetected ::
    CInt -> IO CInt

isGestureDetected :: Int -> IO Bool
isGestureDetected gesture = toBool <$> c'isGestureDetected (fromIntegral gesture)

foreign import ccall unsafe "raylib.h &IsGestureDetected"
  p'isGestureDetected ::
    FunPtr (CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetGestureDetected"
  c'getGestureDetected ::
    IO CInt

getGestureDetected :: IO Int
getGestureDetected = fromIntegral <$> c'getGestureDetected

foreign import ccall unsafe "raylib.h &GetGestureDetected"
  p'getGestureDetected ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h GetGestureHoldDuration"
  c'getGestureHoldDuration ::
    IO CFloat

getGestureHoldDuration :: IO Float
getGestureHoldDuration = realToFrac <$> c'getGestureHoldDuration

foreign import ccall unsafe "raylib.h &GetGestureHoldDuration"
  p'getGestureHoldDuration ::
    FunPtr (IO CFloat)

foreign import ccall unsafe "bindings.h GetGestureDragVector_" c'getGestureDragVector :: IO (Ptr Vector2)

getGestureDragVector :: IO Vector2
getGestureDragVector = c'getGestureDragVector >>= pop

foreign import ccall unsafe "raylib.h &GetGestureDragVector"
  p'getGestureDragVector ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "raylib.h GetGestureDragAngle"
  c'getGestureDragAngle ::
    IO CFloat

getGestureDragAngle :: IO Float
getGestureDragAngle = realToFrac <$> c'getGestureDragAngle

foreign import ccall unsafe "raylib.h &GetGestureDragAngle"
  p'getGestureDragAngle ::
    FunPtr (IO CFloat)

foreign import ccall unsafe "bindings.h GetGesturePinchVector_" c'getGesturePinchVector :: IO (Ptr Vector2)

getGesturePinchVector :: IO Vector2
getGesturePinchVector = c'getGesturePinchVector >>= pop

foreign import ccall unsafe "raylib.h &GetGesturePinchVector"
  p'getGesturePinchVector ::
    FunPtr (IO Vector2)

foreign import ccall unsafe "raylib.h GetGesturePinchAngle"
  c'getGesturePinchAngle ::
    IO CFloat

getGesturePinchAngle :: IO Float
getGesturePinchAngle = realToFrac <$> c'getGesturePinchAngle

foreign import ccall unsafe "raylib.h &GetGesturePinchAngle"
  p'getGesturePinchAngle ::
    FunPtr (IO CFloat)

foreign import ccall unsafe "bindings.h SetCameraMode_" c'setCameraMode :: Ptr Camera3D -> CInt -> IO ()

setCameraMode :: Camera3D -> Int -> IO ()
setCameraMode camera mode = with camera (\c -> c'setCameraMode c (fromIntegral mode))

foreign import ccall unsafe "raylib.h &SetCameraMode"
  p'setCameraMode ::
    FunPtr (Camera3D -> CInt -> IO ())

foreign import ccall unsafe "raylib.h UpdateCamera"
  c'updateCamera ::
    Ptr Camera3D -> IO ()

updateCamera :: Camera3D -> IO Camera3D
updateCamera camera =
  with
    camera
    ( \c -> do
        c'updateCamera c
        peek c
    )

foreign import ccall unsafe "raylib.h &UpdateCamera"
  p'updateCamera ::
    FunPtr (Ptr Camera3D -> IO ())

foreign import ccall unsafe "raylib.h SetCameraPanControl"
  c'setCameraPanControl ::
    CInt -> IO ()

setCameraPanControl :: Int -> IO ()
setCameraPanControl keyPan = c'setCameraPanControl $ fromIntegral keyPan

foreign import ccall unsafe "raylib.h &SetCameraPanControl"
  p'setCameraPanControl ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h SetCameraAltControl"
  c'setCameraAltControl ::
    CInt -> IO ()

setCameraAltControl :: Int -> IO ()
setCameraAltControl keyAlt = c'setCameraAltControl $ fromIntegral keyAlt

foreign import ccall unsafe "raylib.h &SetCameraAltControl"
  p'setCameraAltControl ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h SetCameraSmoothZoomControl"
  c'setCameraSmoothZoomControl ::
    CInt -> IO ()

setCameraSmoothZoomControl :: Int -> IO ()
setCameraSmoothZoomControl keySmoothZoom = c'setCameraSmoothZoomControl $ fromIntegral keySmoothZoom

foreign import ccall unsafe "raylib.h &SetCameraSmoothZoomControl"
  p'setCameraSmoothZoomControl ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "raylib.h SetCameraMoveControls"
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

foreign import ccall unsafe "raylib.h &SetCameraMoveControls"
  p'setCameraMoveControls ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall unsafe "bindings.h SetShapesTexture_" c'setShapesTexture :: Ptr Texture -> Ptr Rectangle -> IO ()

setShapesTexture :: Texture -> Rectangle -> IO ()
setShapesTexture tex source = with tex (with source . c'setShapesTexture)

foreign import ccall unsafe "raylib.h &SetShapesTexture"
  p'setShapesTexture ::
    FunPtr (Texture -> Rectangle -> IO ())

foreign import ccall unsafe "bindings.h DrawPixel_" c'drawPixel :: CInt -> CInt -> Ptr Color -> IO ()

drawPixel :: Int -> Int -> Color -> IO ()
drawPixel x y color = with color $ c'drawPixel (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h &DrawPixel"
  p'drawPixel ::
    FunPtr (CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawPixelV_" c'drawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()

drawPixelV :: Vector2 -> Color -> IO ()
drawPixelV position color = with position (with color . c'drawPixelV)

foreign import ccall unsafe "raylib.h &DrawPixelV"
  p'drawPixelV ::
    FunPtr (Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLine_" c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawLine :: Int -> Int -> Int -> Int -> Color -> IO ()
drawLine startX startY endX endY color =
  with color $ c'drawLine (fromIntegral startX) (fromIntegral startY) (fromIntegral endX) (fromIntegral endY)

foreign import ccall unsafe "raylib.h &DrawLine"
  p'drawLine ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLineV_" c'drawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

drawLineV :: Vector2 -> Vector2 -> Color -> IO ()
drawLineV start end color = with start (\s -> with end (with color . c'drawLineV s))

foreign import ccall unsafe "raylib.h &DrawLineV"
  p'drawLineV ::
    FunPtr (Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLineEx_" c'drawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineEx :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineEx start end thickness color =
  with start (\s -> with end (\e -> with color (c'drawLineEx s e (realToFrac thickness))))

foreign import ccall unsafe "raylib.h &DrawLineEx"
  p'drawLineEx ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLineBezier_" c'drawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineBezier :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezier start end thickness color =
  with start (\s -> with end (\e -> with color (c'drawLineBezier s e (realToFrac thickness))))

foreign import ccall unsafe "raylib.h &DrawLineBezier"
  p'drawLineBezier ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLineBezierQuad_" c'drawLineBezierQuad :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineBezierQuad :: Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezierQuad start end control thickness color =
  with start (\s -> with end (\e -> with control (\c -> with color (c'drawLineBezierQuad s e c (realToFrac thickness)))))

foreign import ccall unsafe "raylib.h &DrawLineBezierQuad"
  p'drawLineBezierQuad ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLineBezierCubic_" c'drawLineBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineBezierCubic :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawLineBezierCubic"
  p'drawLineBezierCubic ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawLineStrip_" c'drawLineStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawLineStrip :: [Vector2] -> Color -> IO ()
drawLineStrip points color = withArray points (\p -> with color $ c'drawLineStrip p (genericLength points))

foreign import ccall unsafe "raylib.h &DrawLineStrip"
  p'drawLineStrip ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircle_" c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

drawCircle :: Int -> Int -> Float -> Color -> IO ()
drawCircle centerX centerY radius color = with color (c'drawCircle (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall unsafe "raylib.h &DrawCircle"
  p'drawCircle ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircleSector_" c'drawCircleSector :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawCircleSector :: Vector2 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCircleSector center radius startAngle endAngle segments color =
  with
    center
    ( \c ->
        with
          color
          ( c'drawCircleSector c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

foreign import ccall unsafe "raylib.h &DrawCircleSector"
  p'drawCircleSector ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircleSectorLines_" c'drawCircleSectorLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawCircleSectorLines :: Vector2 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCircleSectorLines center radius startAngle endAngle segments color =
  with
    center
    ( \c ->
        with
          color
          ( c'drawCircleSectorLines c (realToFrac radius) (realToFrac startAngle) (realToFrac endAngle) (fromIntegral segments)
          )
    )

foreign import ccall unsafe "raylib.h &DrawCircleSectorLines"
  p'drawCircleSectorLines ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircleGradient_" c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()

drawCircleGradient :: Int -> Int -> Float -> Color -> Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 =
  with color1 (with color2 . c'drawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall unsafe "raylib.h &DrawCircleGradient"
  p'drawCircleGradient ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircleV_" c'drawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawCircleV :: Vector2 -> Float -> Color -> IO ()
drawCircleV center radius color =
  with center (\c -> with color (c'drawCircleV c (realToFrac radius)))

foreign import ccall unsafe "raylib.h &DrawCircleV"
  p'drawCircleV ::
    FunPtr (Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircleLines_" c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

drawCircleLines :: Int -> Int -> Float -> Color -> IO ()
drawCircleLines centerX centerY radius color =
  with color (c'drawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall unsafe "raylib.h &DrawCircleLines"
  p'drawCircleLines ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawEllipse_" c'drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawEllipse :: Int -> Int -> Float -> Float -> Color -> IO ()
drawEllipse centerX centerY radiusH radiusV color =
  with color (c'drawEllipse (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

foreign import ccall unsafe "raylib.h &DrawEllipse"
  p'drawEllipse ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawEllipseLines_" c'drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawEllipseLines :: Int -> Int -> Float -> Float -> Color -> IO ()
drawEllipseLines centerX centerY radiusH radiusV color =
  with color (c'drawEllipseLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

foreign import ccall unsafe "raylib.h &DrawEllipseLines"
  p'drawEllipseLines ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRing_" c'drawRing :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawRing :: Vector2 -> Float -> Float -> Float -> Float -> Int -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawRing"
  p'drawRing ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRingLines_" c'drawRingLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawRingLines :: Vector2 -> Float -> Float -> Float -> Float -> Int -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawRingLines"
  p'drawRingLines ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangle_" c'drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangle posX posY width height color =
  with color (c'drawRectangle (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

foreign import ccall unsafe "raylib.h &DrawRectangle"
  p'drawRectangle ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleV_" c'drawRectangleV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

drawRectangleV :: Vector2 -> Vector2 -> Color -> IO ()
drawRectangleV position size color = with position (\p -> with size (with color . c'drawRectangleV p))

foreign import ccall unsafe "raylib.h &DrawRectangleV"
  p'drawRectangleV ::
    FunPtr (Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleRec_" c'drawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()

drawRectangleRec :: Rectangle -> Color -> IO ()
drawRectangleRec rect color = with rect (with color . c'drawRectangleRec)

foreign import ccall unsafe "raylib.h &DrawRectangleRec"
  p'drawRectangleRec ::
    FunPtr (Rectangle -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectanglePro_" c'drawRectanglePro :: Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawRectanglePro :: Rectangle -> Vector2 -> Float -> Color -> IO ()
drawRectanglePro rect origin rotation color =
  with color (\c -> with rect (\r -> with origin (\o -> c'drawRectanglePro r o (realToFrac rotation) c)))

foreign import ccall unsafe "raylib.h &DrawRectanglePro"
  p'drawRectanglePro ::
    FunPtr (Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleGradientV_" c'drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()

drawRectangleGradientV :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawRectangleGradientV"
  p'drawRectangleGradientV ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleGradientH_" c'drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()

drawRectangleGradientH :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawRectangleGradientH"
  p'drawRectangleGradientH ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleGradientEx_" c'drawRectangleGradientEx :: Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()

drawRectangleGradientEx :: Rectangle -> Color -> Color -> Color -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawRectangleGradientEx"
  p'drawRectangleGradientEx ::
    FunPtr (Rectangle -> Color -> Color -> Color -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleLines_" c'drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawRectangleLines :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangleLines posX posY width height color =
  with color (c'drawRectangleLines (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

foreign import ccall unsafe "raylib.h &DrawRectangleLines"
  p'drawRectangleLines ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleLinesEx_" c'drawRectangleLinesEx :: Ptr Rectangle -> CFloat -> Ptr Color -> IO ()

drawRectangleLinesEx :: Rectangle -> Float -> Color -> IO ()
drawRectangleLinesEx rect thickness color =
  with color (\c -> with rect (\r -> c'drawRectangleLinesEx r (realToFrac thickness) c))

foreign import ccall unsafe "raylib.h &DrawRectangleLinesEx"
  p'drawRectangleLinesEx ::
    FunPtr (Rectangle -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleRounded_" c'drawRectangleRounded :: Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()

drawRectangleRounded :: Rectangle -> Float -> Int -> Color -> IO ()
drawRectangleRounded rect roundness segments color =
  with rect (\r -> with color $ c'drawRectangleRounded r (realToFrac roundness) (fromIntegral segments))

foreign import ccall unsafe "raylib.h &DrawRectangleRounded"
  p'drawRectangleRounded ::
    FunPtr (Rectangle -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRectangleRoundedLines_" c'drawRectangleRoundedLines :: Ptr Rectangle -> CFloat -> CInt -> CFloat -> Ptr Color -> IO ()

drawRectangleRoundedLines :: Rectangle -> Float -> Int -> Float -> Color -> IO ()
drawRectangleRoundedLines rect roundness segments thickness color =
  with rect (\r -> with color $ c'drawRectangleRoundedLines r (realToFrac roundness) (fromIntegral segments) (realToFrac thickness))

foreign import ccall unsafe "raylib.h &DrawRectangleRoundedLines"
  p'drawRectangleRoundedLines ::
    FunPtr (Rectangle -> CFloat -> CInt -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTriangle_" c'drawTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

drawTriangle :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangle v1 v2 v3 color =
  with
    v1
    ( \p1 ->
        with
          v2
          ( \p2 -> with v3 (with color . c'drawTriangle p1 p2)
          )
    )

foreign import ccall unsafe "raylib.h &DrawTriangle"
  p'drawTriangle ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTriangleLines_" c'drawTriangleLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

drawTriangleLines :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangleLines v1 v2 v3 color =
  with
    v1
    ( \p1 ->
        with
          v2
          ( \p2 -> with v3 (with color . c'drawTriangleLines p1 p2)
          )
    )

foreign import ccall unsafe "raylib.h &DrawTriangleLines"
  p'drawTriangleLines ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTriangleFan_" c'drawTriangleFan :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawTriangleFan :: [Vector2] -> Color -> IO ()
drawTriangleFan points color = withArray points (\p -> with color $ c'drawTriangleFan p (genericLength points))

foreign import ccall unsafe "raylib.h &DrawTriangleFan"
  p'drawTriangleFan ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTriangleStrip_" c'drawTriangleStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawTriangleStrip :: [Vector2] -> Color -> IO ()
drawTriangleStrip points color =
  withArray points (\p -> with color $ c'drawTriangleStrip p (genericLength points))

foreign import ccall unsafe "raylib.h &DrawTriangleStrip"
  p'drawTriangleStrip ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawPoly_" c'drawPoly :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawPoly :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPoly center sides radius rotation color =
  with center (\c -> with color $ c'drawPoly c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

foreign import ccall unsafe "raylib.h &DrawPoly"
  p'drawPoly ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawPolyLines_" c'drawPolyLines :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawPolyLines :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPolyLines center sides radius rotation color =
  with center (\c -> with color $ c'drawPolyLines c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

foreign import ccall unsafe "raylib.h &DrawPolyLines"
  p'drawPolyLines ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawPolyLinesEx_" c'drawPolyLinesEx :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

drawPolyLinesEx :: Vector2 -> Int -> Float -> Float -> Float -> Color -> IO ()
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

foreign import ccall unsafe "raylib.h &DrawPolyLinesEx"
  p'drawPolyLinesEx ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Color -> IO ())

-- unsafePerformIO has been used with these collision functions
-- It is OK to use to here because even though we are passing
-- pointers as arguments, they are created through the `with`
-- function, which ensures that all pointers are destroyed.
-- See https://stackoverflow.com/a/10530919/17907758

foreign import ccall unsafe "bindings.h CheckCollisionRecs_" c'checkCollisionRecs :: Ptr Rectangle -> Ptr Rectangle -> IO CInt

checkCollisionRecs :: Rectangle -> Rectangle -> Bool
checkCollisionRecs rec1 rec2 = unsafePerformIO $ toBool <$> with rec1 (with rec2 . c'checkCollisionRecs)

foreign import ccall unsafe "raylib.h &CheckCollisionRecs"
  p'checkCollisionRecs ::
    FunPtr (Rectangle -> Rectangle -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionCircles_" c'checkCollisionCircles :: Ptr Vector2 -> CFloat -> Ptr Vector2 -> CFloat -> IO CInt

checkCollisionCircles :: Vector2 -> Float -> Vector2 -> Float -> Bool
checkCollisionCircles center1 radius1 center2 radius2 =
  unsafePerformIO $ toBool <$> with center1 (\c1 -> with center2 (\c2 -> c'checkCollisionCircles c1 (realToFrac radius1) c2 (realToFrac radius2)))

foreign import ccall unsafe "raylib.h &CheckCollisionCircles"
  p'checkCollisionCircles ::
    FunPtr (Vector2 -> CFloat -> Vector2 -> CFloat -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionCircleRec_" c'checkCollisionCircleRec :: Ptr Vector2 -> CFloat -> Ptr Rectangle -> IO CInt

checkCollisionCircleRec :: Vector2 -> Float -> Rectangle -> Bool
checkCollisionCircleRec center radius rect =
  unsafePerformIO $ toBool <$> with center (\c -> with rect $ c'checkCollisionCircleRec c (realToFrac radius))

foreign import ccall unsafe "raylib.h &CheckCollisionCircleRec"
  p'checkCollisionCircleRec ::
    FunPtr (Vector2 -> CFloat -> Rectangle -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionPointRec_" c'checkCollisionPointRec :: Ptr Vector2 -> Ptr Rectangle -> IO CInt

checkCollisionPointRec :: Vector2 -> Rectangle -> Bool
checkCollisionPointRec point rect =
  unsafePerformIO $ toBool <$> with point (with rect . c'checkCollisionPointRec)

foreign import ccall unsafe "raylib.h &CheckCollisionPointRec"
  p'checkCollisionPointRec ::
    FunPtr (Vector2 -> Rectangle -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionPointCircle_" c'checkCollisionPointCircle :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO CInt

checkCollisionPointCircle :: Vector2 -> Vector2 -> Float -> Bool
checkCollisionPointCircle point center radius =
  unsafePerformIO $ toBool <$> with point (\p -> with center (\c -> c'checkCollisionPointCircle p c (realToFrac radius)))

foreign import ccall unsafe "raylib.h &CheckCollisionPointCircle"
  p'checkCollisionPointCircle ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionPointTriangle_" c'checkCollisionPointTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CInt

checkCollisionPointTriangle :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Bool
checkCollisionPointTriangle point p1 p2 p3 =
  unsafePerformIO $ toBool <$> with point (\p -> with p1 (\ptr1 -> with p2 (with p3 . c'checkCollisionPointTriangle p ptr1)))

foreign import ccall unsafe "raylib.h &CheckCollisionPointTriangle"
  p'checkCollisionPointTriangle ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionLines_" c'checkCollisionLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CInt

-- | If a collision is found, returns @Just collisionPoint@, otherwise returns @Nothing@
checkCollisionLines :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Maybe Vector2
checkCollisionLines start1 end1 start2 end2 =
  unsafePerformIO $
    with
      (Vector2 0 0)
      ( \res -> do
          foundCollision <- toBool <$> with start1 (\s1 -> with end1 (\e1 -> with start2 (\s2 -> with end2 (\e2 -> c'checkCollisionLines s1 e1 s2 e2 res))))
          if foundCollision then Just <$> peek res else return Nothing
      )

foreign import ccall unsafe "raylib.h &CheckCollisionLines"
  p'checkCollisionLines ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> Ptr Vector2 -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionPointLine_" c'checkCollisionPointLine :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CInt

checkCollisionPointLine :: Vector2 -> Vector2 -> Vector2 -> Int -> Bool
checkCollisionPointLine point p1 p2 threshold =
  unsafePerformIO $ toBool <$> with point (\p -> with p1 (\ptr1 -> with p2 (\ptr2 -> c'checkCollisionPointLine p ptr1 ptr2 (fromIntegral threshold))))

foreign import ccall unsafe "raylib.h &CheckCollisionPointLine"
  p'checkCollisionPointLine ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> CInt -> IO CInt)

foreign import ccall unsafe "bindings.h GetCollisionRec_" c'getCollisionRec :: Ptr Rectangle -> Ptr Rectangle -> IO (Ptr Rectangle)

getCollisionRec :: Rectangle -> Rectangle -> Rectangle
getCollisionRec rec1 rec2 =
  unsafePerformIO $ with rec1 (with rec2 . c'getCollisionRec) >>= pop

foreign import ccall unsafe "raylib.h &GetCollisionRec"
  p'getCollisionRec ::
    FunPtr (Rectangle -> Rectangle -> IO Rectangle)

foreign import ccall unsafe "bindings.h LoadImage_" c'loadImage :: CString -> IO (Ptr Image)

loadImage :: String -> IO Image
loadImage fileName = withCString fileName c'loadImage >>= pop

foreign import ccall unsafe "raylib.h &LoadImage"
  p'loadImage ::
    FunPtr (CString -> IO Image)

foreign import ccall unsafe "bindings.h LoadImageRaw_" c'loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

loadImageRaw :: String -> Int -> Int -> Int -> Int -> IO Image
loadImageRaw fileName width height format headerSize =
  withCString fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral format) (fromIntegral headerSize)) >>= pop

foreign import ccall unsafe "raylib.h &LoadImageRaw"
  p'loadImageRaw ::
    FunPtr (CString -> CInt -> CInt -> CInt -> CInt -> IO Image)

foreign import ccall unsafe "bindings.h LoadImageAnim_" c'loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Image)

-- | Returns the final image and the framees in a tuple, e.g. @(img, 18)@
loadImageAnim :: String -> IO (Image, Int)
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

foreign import ccall unsafe "raylib.h &LoadImageAnim"
  p'loadImageAnim ::
    FunPtr (CString -> Ptr CInt -> IO Image)

foreign import ccall unsafe "bindings.h LoadImageFromMemory_" c'loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Image)

loadImageFromMemory :: String -> String -> Int -> IO Image
loadImageFromMemory fileType fileData fileSize =
  withCString fileType (\ft -> withCString fileData (\fd -> c'loadImageFromMemory ft (castPtr fd) (fromIntegral fileSize))) >>= pop

foreign import ccall unsafe "raylib.h &LoadImageFromMemory"
  p'loadImageFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Image)

foreign import ccall unsafe "bindings.h LoadImageFromTexture_" c'loadImageFromTexture :: Ptr Texture -> IO (Ptr Image)

loadImageFromTexture :: Texture -> IO Image
loadImageFromTexture tex = with tex c'loadImageFromTexture >>= pop

foreign import ccall unsafe "raylib.h &LoadImageFromTexture"
  p'loadImageFromTexture ::
    FunPtr (Texture -> IO Image)

foreign import ccall unsafe "bindings.h LoadImageFromScreen_" c'loadImageFromScreen :: IO (Ptr Image)

loadImageFromScreen :: IO Image
loadImageFromScreen = c'loadImageFromScreen >>= pop

foreign import ccall unsafe "raylib.h &LoadImageFromScreen"
  p'loadImageFromScreen ::
    FunPtr (IO Image)

foreign import ccall unsafe "bindings.h UnloadImage_" c'unloadImage :: Ptr Image -> IO ()

unloadImage :: Image -> IO ()
unloadImage image = with image c'unloadImage

foreign import ccall unsafe "raylib.h &UnloadImage"
  p'unloadImage ::
    FunPtr (Image -> IO ())

foreign import ccall unsafe "bindings.h ExportImage_" c'exportImage :: Ptr Image -> CString -> IO CInt

exportImage :: Image -> String -> IO Bool
exportImage image fileName = toBool <$> with image (withCString fileName . c'exportImage)

foreign import ccall unsafe "raylib.h &ExportImage"
  p'exportImage ::
    FunPtr (Image -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h ExportImageAsCode_" c'exportImageAsCode :: Ptr Image -> CString -> IO CInt

exportImageAsCode :: Image -> String -> IO Bool
exportImageAsCode image fileName =
  toBool <$> with image (withCString fileName . c'exportImageAsCode)

foreign import ccall unsafe "raylib.h &ExportImageAsCode"
  p'exportImageAsCode ::
    FunPtr (Image -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h GenImageColor_" c'genImageColor :: CInt -> CInt -> Ptr Color -> IO (Ptr Image)

genImageColor :: Int -> Int -> Color -> IO Image
genImageColor width height color =
  with color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall unsafe "raylib.h &GenImageColor"
  p'genImageColor ::
    FunPtr (CInt -> CInt -> Color -> IO Image)

foreign import ccall unsafe "bindings.h GenImageGradientV_" c'genImageGradientV :: CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageGradientV :: Int -> Int -> Color -> Color -> IO Image
genImageGradientV width height top bottom =
  with top (with bottom . c'genImageGradientV (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall unsafe "raylib.h &GenImageGradientV"
  p'genImageGradientV ::
    FunPtr (CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall unsafe "bindings.h GenImageGradientH_" c'genImageGradientH :: CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageGradientH :: Int -> Int -> Color -> Color -> IO Image
genImageGradientH width height left right =
  with left (with right . c'genImageGradientH (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall unsafe "raylib.h &GenImageGradientH"
  p'genImageGradientH ::
    FunPtr (CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall unsafe "bindings.h GenImageGradientRadial_" c'genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageGradientRadial :: Int -> Int -> Float -> Color -> Color -> IO Image
genImageGradientRadial width height density inner outer =
  with inner (with outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

foreign import ccall unsafe "raylib.h &GenImageGradientRadial"
  p'genImageGradientRadial ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> Color -> IO Image)

foreign import ccall unsafe "bindings.h GenImageChecked_" c'genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageChecked :: Int -> Int -> Int -> Int -> Color -> Color -> IO Image
genImageChecked width height checksX checksY col1 col2 =
  with col1 (with col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= pop

foreign import ccall unsafe "raylib.h &GenImageChecked"
  p'genImageChecked ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall unsafe "bindings.h GenImageWhiteNoise_" c'genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Image)

genImageWhiteNoise :: Int -> Int -> Float -> IO Image
genImageWhiteNoise width height factor =
  c'genImageWhiteNoise (fromIntegral width) (fromIntegral height) (realToFrac factor) >>= pop

foreign import ccall unsafe "raylib.h &GenImageWhiteNoise"
  p'genImageWhiteNoise ::
    FunPtr (CInt -> CInt -> CFloat -> IO Image)

foreign import ccall unsafe "bindings.h GenImagePerlinNoise_" c'genImagePerlinNoise :: CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Image)

genImagePerlinNoise :: Int -> Int -> Int -> Int -> Float -> IO Image
genImagePerlinNoise width height offsetX offsetY scale = c'genImagePerlinNoise (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY) (realToFrac scale) >>= pop

foreign import ccall unsafe "raylib.h &GenImagePerlinNoise" p'genImagePerlinNoise :: FunPtr (CInt -> CInt -> CInt -> CInt -> CFloat -> IO Image)

foreign import ccall unsafe "bindings.h GenImageCellular_" c'genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Image)

genImageCellular :: Int -> Int -> Int -> IO Image
genImageCellular width height tileSize =
  c'genImageCellular (fromIntegral width) (fromIntegral height) (fromIntegral tileSize) >>= pop

foreign import ccall unsafe "raylib.h &GenImageCellular"
  p'genImageCellular ::
    FunPtr (CInt -> CInt -> CInt -> IO Image)

foreign import ccall unsafe "bindings.h ImageCopy_" c'imageCopy :: Ptr Image -> IO (Ptr Image)

imageCopy :: Image -> IO Image
imageCopy image = with image c'imageCopy >>= pop

foreign import ccall unsafe "raylib.h &ImageCopy"
  p'imageCopy ::
    FunPtr (Image -> IO Image)

foreign import ccall unsafe "bindings.h ImageFromImage_" c'imageFromImage :: Ptr Image -> Ptr Rectangle -> IO (Ptr Image)

imageFromImage :: Image -> Rectangle -> IO Image
imageFromImage image rect = with image (with rect . c'imageFromImage) >>= pop

foreign import ccall unsafe "raylib.h &ImageFromImage"
  p'imageFromImage ::
    FunPtr (Image -> Rectangle -> IO Image)

foreign import ccall unsafe "bindings.h ImageText_" c'imageText :: CString -> CInt -> Ptr Color -> IO (Ptr Image)

imageText :: String -> Int -> Color -> IO Image
imageText text fontSize color =
  withCString text (\t -> with color $ c'imageText t (fromIntegral fontSize)) >>= pop

foreign import ccall unsafe "raylib.h &ImageText"
  p'imageText ::
    FunPtr (CString -> CInt -> Color -> IO Image)

foreign import ccall unsafe "bindings.h ImageTextEx_" c'imageTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> Ptr Color -> IO (Ptr Image)

imageTextEx :: Font -> String -> Float -> Float -> Color -> IO Image
imageTextEx font text fontSize spacing tint =
  with font (\f -> withCString text (\t -> with tint $ c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

foreign import ccall unsafe "raylib.h &ImageTextEx"
  p'imageTextEx ::
    FunPtr (Font -> CString -> CFloat -> CFloat -> Color -> IO Image)

foreign import ccall unsafe "raylib.h ImageFormat"
  c'imageFormat ::
    Ptr Image -> CInt -> IO ()

imageFormat :: Image -> Int -> IO Image
imageFormat image newFormat =
  with image (\i -> c'imageFormat i (fromIntegral newFormat) >> peek i)

foreign import ccall unsafe "raylib.h &ImageFormat"
  p'imageFormat ::
    FunPtr (Ptr Image -> CInt -> IO ())

foreign import ccall unsafe "bindings.h ImageToPOT_" c'imageToPOT :: Ptr Image -> Ptr Color -> IO ()

imageToPOT :: Image -> Color -> IO Image
imageToPOT image color = with image (\i -> with color (c'imageToPOT i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageToPOT"
  p'imageToPOT ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageCrop_" c'imageCrop :: Ptr Image -> Ptr Rectangle -> IO ()

imageCrop :: Image -> Rectangle -> IO Image
imageCrop image crop = with image (\i -> with crop (c'imageCrop i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageCrop"
  p'imageCrop ::
    FunPtr (Ptr Image -> Rectangle -> IO ())

foreign import ccall unsafe "raylib.h ImageAlphaCrop"
  c'imageAlphaCrop ::
    Ptr Image -> CFloat -> IO ()

imageAlphaCrop :: Image -> Float -> IO Image
imageAlphaCrop image threshold = with image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peek i)

foreign import ccall unsafe "raylib.h &ImageAlphaCrop"
  p'imageAlphaCrop ::
    FunPtr (Ptr Image -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h ImageAlphaClear_" c'imageAlphaClear :: Ptr Image -> Ptr Color -> CFloat -> IO ()

imageAlphaClear :: Image -> Color -> Float -> IO Image
imageAlphaClear image color threshold = with image (\i -> with color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peek i))

foreign import ccall unsafe "raylib.h &ImageAlphaClear"
  p'imageAlphaClear ::
    FunPtr (Ptr Image -> Color -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h ImageAlphaMask_" c'imageAlphaMask :: Ptr Image -> Ptr Image -> IO ()

imageAlphaMask :: Image -> Image -> IO Image
imageAlphaMask image alphaMask = with image (\i -> with alphaMask (c'imageAlphaMask i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageAlphaMask"
  p'imageAlphaMask ::
    FunPtr (Ptr Image -> Image -> IO ())

foreign import ccall unsafe "raylib.h ImageAlphaPremultiply"
  c'imageAlphaPremultiply ::
    Ptr Image -> IO ()

imageAlphaPremultiply :: Image -> IO Image
imageAlphaPremultiply image = with image (\i -> c'imageAlphaPremultiply i >> peek i)

foreign import ccall unsafe "raylib.h &ImageAlphaPremultiply"
  p'imageAlphaPremultiply ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageResize"
  c'imageResize ::
    Ptr Image -> CInt -> CInt -> IO ()

imageResize :: Image -> Int -> Int -> IO Image
imageResize image newWidth newHeight = with image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

foreign import ccall unsafe "raylib.h &ImageResize"
  p'imageResize ::
    FunPtr (Ptr Image -> CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h ImageResizeNN"
  c'imageResizeNN ::
    Ptr Image -> CInt -> CInt -> IO ()

imageResizeNN :: Image -> Int -> Int -> IO Image
imageResizeNN image newWidth newHeight = with image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

foreign import ccall unsafe "raylib.h &ImageResizeNN"
  p'imageResizeNN ::
    FunPtr (Ptr Image -> CInt -> CInt -> IO ())

foreign import ccall unsafe "bindings.h ImageResizeCanvas_" c'imageResizeCanvas :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageResizeCanvas :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = with image (\i -> with fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageResizeCanvas"
  p'imageResizeCanvas ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "raylib.h ImageMipmaps"
  c'imageMipmaps ::
    Ptr Image -> IO ()

imageMipmaps :: Image -> IO Image
imageMipmaps image = with image (\i -> c'imageMipmaps i >> peek i)

foreign import ccall unsafe "raylib.h &ImageMipmaps"
  p'imageMipmaps ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageDither"
  c'imageDither ::
    Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()

imageDither :: Image -> Int -> Int -> Int -> Int -> IO Image
imageDither image rBpp gBpp bBpp aBpp = with image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDither"
  p'imageDither ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h ImageFlipVertical"
  c'imageFlipVertical ::
    Ptr Image -> IO ()

imageFlipVertical :: Image -> IO Image
imageFlipVertical image = with image (\i -> c'imageFlipVertical i >> peek i)

foreign import ccall unsafe "raylib.h &ImageFlipVertical"
  p'imageFlipVertical ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageFlipHorizontal"
  c'imageFlipHorizontal ::
    Ptr Image -> IO ()

imageFlipHorizontal :: Image -> IO Image
imageFlipHorizontal image = with image (\i -> c'imageFlipHorizontal i >> peek i)

foreign import ccall unsafe "raylib.h &ImageFlipHorizontal"
  p'imageFlipHorizontal ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageRotateCW"
  c'imageRotateCW ::
    Ptr Image -> IO ()

imageRotateCW :: Image -> IO Image
imageRotateCW image = with image (\i -> c'imageRotateCW i >> peek i)

foreign import ccall unsafe "raylib.h &ImageRotateCW"
  p'imageRotateCW ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageRotateCCW"
  c'imageRotateCCW ::
    Ptr Image -> IO ()

imageRotateCCW :: Image -> IO Image
imageRotateCCW image = with image (\i -> c'imageRotateCCW i >> peek i)

foreign import ccall unsafe "raylib.h &ImageRotateCCW"
  p'imageRotateCCW ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "bindings.h ImageColorTint_" c'imageColorTint :: Ptr Image -> Ptr Color -> IO ()

imageColorTint :: Image -> Color -> IO Image
imageColorTint image color = with image (\i -> with color (c'imageColorTint i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageColorTint"
  p'imageColorTint ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall unsafe "raylib.h ImageColorInvert"
  c'imageColorInvert ::
    Ptr Image -> IO ()

imageColorInvert :: Image -> IO Image
imageColorInvert image = with image (\i -> c'imageColorInvert i >> peek i)

foreign import ccall unsafe "raylib.h &ImageColorInvert"
  p'imageColorInvert ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageColorGrayscale"
  c'imageColorGrayscale ::
    Ptr Image -> IO ()

imageColorGrayscale :: Image -> IO Image
imageColorGrayscale image = with image (\i -> c'imageColorGrayscale i >> peek i)

foreign import ccall unsafe "raylib.h &ImageColorGrayscale"
  p'imageColorGrayscale ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall unsafe "raylib.h ImageColorContrast"
  c'imageColorContrast ::
    Ptr Image -> CFloat -> IO ()

imageColorContrast :: Image -> Float -> IO Image
imageColorContrast image contrast = with image (\i -> c'imageColorContrast i (realToFrac contrast) >> peek i)

foreign import ccall unsafe "raylib.h &ImageColorContrast"
  p'imageColorContrast ::
    FunPtr (Ptr Image -> CFloat -> IO ())

foreign import ccall unsafe "raylib.h ImageColorBrightness"
  c'imageColorBrightness ::
    Ptr Image -> CInt -> IO ()

imageColorBrightness :: Image -> Int -> IO Image
imageColorBrightness image brightness = with image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peek i)

foreign import ccall unsafe "raylib.h &ImageColorBrightness"
  p'imageColorBrightness ::
    FunPtr (Ptr Image -> CInt -> IO ())

foreign import ccall unsafe "bindings.h ImageColorReplace_" c'imageColorReplace :: Ptr Image -> Ptr Color -> Ptr Color -> IO ()

imageColorReplace :: Image -> Color -> Color -> IO Image
imageColorReplace image color replace = with image (\i -> with color (with replace . c'imageColorReplace i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageColorReplace"
  p'imageColorReplace ::
    FunPtr (Ptr Image -> Color -> Color -> IO ())

foreign import ccall unsafe "bindings.h LoadImageColors_" c'loadImageColors :: Ptr Image -> IO (Ptr Color)

loadImageColors :: Image -> IO [Color]
loadImageColors image =
  with
    image
    ( \i -> do
        colors <- c'loadImageColors i
        colArray <- peekArray (fromIntegral $ image'width image * image'height image) colors
        unloadImageColors colors
        return colArray
    )

foreign import ccall unsafe "raylib.h &LoadImageColors"
  p'loadImageColors ::
    FunPtr (Image -> IO (Ptr Color))

foreign import ccall unsafe "bindings.h LoadImagePalette_" c'loadImagePalette :: Ptr Image -> CInt -> Ptr CInt -> IO (Ptr Color)

loadImagePalette :: Image -> Int -> IO [Color]
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

foreign import ccall unsafe "raylib.h &LoadImagePalette"
  p'loadImagePalette ::
    FunPtr (Image -> CInt -> Ptr CInt -> IO (Ptr Color))

{-|NOTE: You usually won't need to use this. `loadImageColors` unloads the colors automatically. Only use this when you are using `c'loadImageColors` to load the colors.-}
foreign import ccall unsafe "raylib.h UnloadImageColors"
  unloadImageColors ::
    Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &UnloadImageColors"
  p'unloadImageColors ::
    FunPtr (Ptr Color -> IO ())

{-|NOTE: You usually won't need to use this. `loadImagePalette` unloads the colors automatically. Only use this when you are using `c'loadImagePalette` to load the colors.-}
foreign import ccall unsafe "raylib.h UnloadImagePalette"
  unloadImagePalette ::
    Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &UnloadImagePalette"
  p'unloadImagePalette ::
    FunPtr (Ptr Color -> IO ())

foreign import ccall unsafe "bindings.h GetImageAlphaBorder_" c'getImageAlphaBorder :: Ptr Image -> CFloat -> IO (Ptr Rectangle)

getImageAlphaBorder :: Image -> Float -> IO Rectangle
getImageAlphaBorder image threshold = with image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

foreign import ccall unsafe "raylib.h &GetImageAlphaBorder"
  p'getImageAlphaBorder ::
    FunPtr (Image -> CFloat -> IO Rectangle)

foreign import ccall unsafe "bindings.h GetImageColor_" c'getImageColor :: Ptr Image -> CInt -> CInt -> IO (Ptr Color)

getImageColor :: Image -> Int -> Int -> IO Color
getImageColor image x y = with image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

foreign import ccall unsafe "raylib.h &GetImageColor"
  p'getImageColor ::
    FunPtr (Image -> CInt -> CInt -> IO Color)

foreign import ccall unsafe "bindings.h ImageClearBackground_" c'imageClearBackground :: Ptr Image -> Ptr Color -> IO ()

imageClearBackground :: Image -> Color -> IO Image
imageClearBackground image color = with image (\i -> with color (c'imageClearBackground i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageClearBackground"
  p'imageClearBackground ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawPixel_" c'imageDrawPixel :: Ptr Image -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawPixel :: Image -> Int -> Int -> Color -> IO Image
imageDrawPixel image x y color = with image (\i -> with color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawPixel"
  p'imageDrawPixel ::
    FunPtr (Ptr Image -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawPixelV_" c'imageDrawPixelV :: Ptr Image -> Ptr Vector2 -> Ptr Color -> IO ()

imageDrawPixelV :: Image -> Vector2 -> Color -> IO Image
imageDrawPixelV image position color = with image (\i -> with position (with color . c'imageDrawPixelV i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawPixelV"
  p'imageDrawPixelV ::
    FunPtr (Ptr Image -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawLine_" c'imageDrawLine :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawLine :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageDrawLine image startPosX startPosY endPosX endPosY color = with image (\i -> with color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawLine"
  p'imageDrawLine ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawLineV_" c'imageDrawLineV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

imageDrawLineV :: Image -> Vector2 -> Vector2 -> Color -> IO Image
imageDrawLineV image start end color = with image (\i -> with start (\s -> with end (with color . c'imageDrawLineV i s)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawLineV"
  p'imageDrawLineV ::
    FunPtr (Ptr Image -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawCircle_" c'imageDrawCircle :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawCircle :: Image -> Int -> Int -> Int -> Color -> IO Image
imageDrawCircle image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawCircle"
  p'imageDrawCircle ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawCircleV_" c'imageDrawCircleV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

imageDrawCircleV :: Image -> Vector2 -> Int -> Color -> IO Image
imageDrawCircleV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleV i c (fromIntegral radius))) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawCircleV"
  p'imageDrawCircleV ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawCircleLines_" c'imageDrawCircleLines :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawCircleLines :: Image -> Int -> Int -> Int -> Color -> IO Image
imageDrawCircleLines image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawCircleLines"
  p'imageDrawCircleLines ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawCircleLinesV_" c'imageDrawCircleLinesV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

imageDrawCircleLinesV :: Image -> Vector2 -> Int -> Color -> IO Image
imageDrawCircleLinesV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawCircleLinesV"
  p'imageDrawCircleLinesV ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawRectangle_" c'imageDrawRectangle :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawRectangle :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageDrawRectangle image posX posY width height color = with image (\i -> with color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawRectangle"
  p'imageDrawRectangle ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawRectangleV_" c'imageDrawRectangleV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

imageDrawRectangleV :: Image -> Vector2 -> Vector2 -> Color -> IO Image
imageDrawRectangleV image position size color = with image (\i -> with position (\p -> with size (with color . c'imageDrawRectangleV i p)) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawRectangleV"
  p'imageDrawRectangleV ::
    FunPtr (Ptr Image -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawRectangleRec_" c'imageDrawRectangleRec :: Ptr Image -> Ptr Rectangle -> Ptr Color -> IO ()

imageDrawRectangleRec :: Image -> Rectangle -> Color -> IO Image
imageDrawRectangleRec image rectangle color = with image (\i -> with rectangle (with color . c'imageDrawRectangleRec i) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawRectangleRec"
  p'imageDrawRectangleRec ::
    FunPtr (Ptr Image -> Rectangle -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawRectangleLines_" c'imageDrawRectangleLines :: Ptr Image -> Ptr Rectangle -> CInt -> Ptr Color -> IO ()

imageDrawRectangleLines :: Image -> Rectangle -> Int -> Color -> IO Image
imageDrawRectangleLines image rectangle thickness color = with image (\i -> with rectangle (\r -> with color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawRectangleLines"
  p'imageDrawRectangleLines ::
    FunPtr (Ptr Image -> Rectangle -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDraw_" c'imageDraw :: Ptr Image -> Ptr Image -> Ptr Rectangle -> Ptr Rectangle -> Ptr Color -> IO ()

imageDraw :: Image -> Image -> Rectangle -> Rectangle -> Color -> IO Image
imageDraw image source srcRec dstRec tint = with image (\i -> with source (\s -> with srcRec (\sr -> with dstRec (with tint . c'imageDraw i s sr))) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDraw"
  p'imageDraw ::
    FunPtr (Ptr Image -> Image -> Rectangle -> Rectangle -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawText_" c'imageDrawText :: Ptr Image -> CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawText :: Image -> String -> Int -> Int -> Int -> Color -> IO Image
imageDrawText image text x y fontSize color = with image (\i -> withCString text (\t -> with color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawText"
  p'imageDrawText ::
    FunPtr (Ptr Image -> CString -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h ImageDrawTextEx_" c'imageDrawTextEx :: Ptr Image -> Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

imageDrawTextEx :: Image -> Font -> String -> Vector2 -> Float -> Float -> Color -> IO Image
imageDrawTextEx image font text position fontSize spacing tint = with image (\i -> with font (\f -> withCString text (\t -> with position (\p -> with tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peek i)

foreign import ccall unsafe "raylib.h &ImageDrawTextEx"
  p'imageDrawTextEx ::
    FunPtr (Ptr Image -> Font -> CString -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h LoadTexture_" c'loadTexture :: CString -> IO (Ptr Texture)

loadTexture :: String -> IO Texture
loadTexture fileName = withCString fileName c'loadTexture >>= pop

foreign import ccall unsafe "raylib.h &LoadTexture"
  p'loadTexture ::
    FunPtr (CString -> IO Texture)

foreign import ccall unsafe "bindings.h LoadTextureFromImage_" c'loadTextureFromImage :: Ptr Image -> IO (Ptr Texture)

loadTextureFromImage :: Image -> IO Texture
loadTextureFromImage image = with image c'loadTextureFromImage >>= pop

foreign import ccall unsafe "raylib.h &LoadTextureFromImage"
  p'loadTextureFromImage ::
    FunPtr (Image -> IO Texture)

foreign import ccall unsafe "bindings.h LoadTextureCubemap_" c'loadTextureCubemap :: Ptr Image -> CInt -> IO (Ptr Texture)

loadTextureCubemap :: Image -> Int -> IO Texture
loadTextureCubemap image layout = with image (\i -> c'loadTextureCubemap i (fromIntegral layout)) >>= pop

foreign import ccall unsafe "raylib.h &LoadTextureCubemap"
  p'loadTextureCubemap ::
    FunPtr (Image -> CInt -> IO Texture)

foreign import ccall unsafe "bindings.h LoadRenderTexture_" c'loadRenderTexture :: CInt -> CInt -> IO (Ptr RenderTexture)

loadRenderTexture :: Int -> Int -> IO RenderTexture
loadRenderTexture width height = c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= pop

foreign import ccall unsafe "raylib.h &LoadRenderTexture"
  p'loadRenderTexture ::
    FunPtr (CInt -> CInt -> IO RenderTexture)

foreign import ccall unsafe "bindings.h UnloadTexture_" c'unloadTexture :: Ptr Texture -> IO ()

unloadTexture :: Texture -> IO ()
unloadTexture texture = with texture c'unloadTexture

foreign import ccall unsafe "raylib.h &UnloadTexture"
  p'unloadTexture ::
    FunPtr (Texture -> IO ())

foreign import ccall unsafe "bindings.h UnloadRenderTexture_" c'unloadRenderTexture :: Ptr RenderTexture -> IO ()

unloadRenderTexture :: RenderTexture -> IO ()
unloadRenderTexture target = with target c'unloadRenderTexture

foreign import ccall unsafe "raylib.h &UnloadRenderTexture"
  p'unloadRenderTexture ::
    FunPtr (RenderTexture -> IO ())

foreign import ccall unsafe "bindings.h UpdateTexture_" c'updateTexture :: Ptr Texture -> Ptr () -> IO ()

updateTexture :: Texture -> Ptr () -> IO Texture
updateTexture texture pixels = with texture (\t -> c'updateTexture t pixels >> peek t)

foreign import ccall unsafe "raylib.h &UpdateTexture"
  p'updateTexture ::
    FunPtr (Texture -> Ptr () -> IO ())

foreign import ccall unsafe "bindings.h UpdateTextureRec_" c'updateTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr () -> IO ()

updateTextureRec :: Texture -> Rectangle -> Ptr () -> IO Texture
updateTextureRec texture rect pixels = with texture (\t -> with rect (\r -> c'updateTextureRec t r pixels) >> peek t)

foreign import ccall unsafe "raylib.h &UpdateTextureRec"
  p'updateTextureRec ::
    FunPtr (Texture -> Rectangle -> Ptr () -> IO ())

foreign import ccall unsafe "raylib.h GenTextureMipmaps"
  c'genTextureMipmaps ::
    Ptr Texture -> IO ()

genTextureMipmaps :: Texture -> IO Texture
genTextureMipmaps texture = with texture (\t -> c'genTextureMipmaps t >> peek t)

foreign import ccall unsafe "raylib.h &GenTextureMipmaps"
  p'genTextureMipmaps ::
    FunPtr (Ptr Texture -> IO ())

foreign import ccall unsafe "bindings.h SetTextureFilter_" c'setTextureFilter :: Ptr Texture -> CInt -> IO ()

setTextureFilter :: Texture -> Int -> IO Texture
setTextureFilter texture filter = with texture (\t -> c'setTextureFilter t (fromIntegral filter) >> peek t)

foreign import ccall unsafe "raylib.h &SetTextureFilter"
  p'setTextureFilter ::
    FunPtr (Texture -> CInt -> IO ())

foreign import ccall unsafe "bindings.h SetTextureWrap_" c'setTextureWrap :: Ptr Texture -> CInt -> IO ()

setTextureWrap :: Texture -> Int -> IO Texture
setTextureWrap texture wrap = with texture (\t -> c'setTextureWrap t (fromIntegral wrap) >> peek t)

foreign import ccall unsafe "raylib.h &SetTextureWrap"
  p'setTextureWrap ::
    FunPtr (Texture -> CInt -> IO ())

foreign import ccall unsafe "bindings.h DrawTexture_" drawTexture :: Ptr Texture -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTexture"
  p'drawTexture ::
    FunPtr (Texture -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextureV_" drawTextureV :: Ptr Texture -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextureV"
  p'drawTextureV ::
    FunPtr (Texture -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextureEx_" drawTextureEx :: Ptr Texture -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextureEx"
  p'drawTextureEx ::
    FunPtr (Texture -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextureRec_" drawTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextureRec"
  p'drawTextureRec ::
    FunPtr (Texture -> Rectangle -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextureQuad_" drawTextureQuad :: Ptr Texture -> Ptr Vector2 -> Ptr Vector2 -> Ptr Rectangle -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextureQuad"
  p'drawTextureQuad ::
    FunPtr (Texture -> Vector2 -> Vector2 -> Rectangle -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextureTiled_" drawTextureTiled :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextureTiled"
  p'drawTextureTiled ::
    FunPtr (Texture -> Rectangle -> Rectangle -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTexturePro_" drawTexturePro :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTexturePro"
  p'drawTexturePro ::
    FunPtr (Texture -> Rectangle -> Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextureNPatch_" drawTextureNPatch :: Ptr Texture -> Ptr NPatchInfo -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextureNPatch"
  p'drawTextureNPatch ::
    FunPtr (Texture -> NPatchInfo -> Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTexturePoly_" drawTexturePoly :: Ptr Texture -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTexturePoly"
  p'drawTexturePoly ::
    FunPtr (Texture -> Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h Fade_" fade :: Ptr Color -> CFloat -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &Fade"
  p'fade ::
    FunPtr (Color -> CFloat -> IO Color)

foreign import ccall unsafe "bindings.h ColorToInt_" colorToInt :: Ptr Color -> IO CInt

foreign import ccall unsafe "raylib.h &ColorToInt"
  p'colorToInt ::
    FunPtr (Color -> IO CInt)

foreign import ccall unsafe "bindings.h ColorNormalize_" colorNormalize :: Ptr Color -> IO (Ptr Vector4)

foreign import ccall unsafe "raylib.h &ColorNormalize"
  p'colorNormalize ::
    FunPtr (Color -> IO Vector4)

foreign import ccall unsafe "bindings.h ColorFromNormalized_" colorFromNormalized :: Ptr Vector4 -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &ColorFromNormalized"
  p'colorFromNormalized ::
    FunPtr (Vector4 -> IO Color)

foreign import ccall unsafe "bindings.h ColorToHSV_" colorToHSV :: Ptr Color -> IO (Ptr Vector3)

foreign import ccall unsafe "raylib.h &ColorToHSV"
  p'colorToHSV ::
    FunPtr (Color -> IO Vector3)

foreign import ccall unsafe "bindings.h ColorFromHSV_" colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &ColorFromHSV"
  p'colorFromHSV ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Color)

foreign import ccall unsafe "bindings.h ColorAlpha_" colorAlpha :: Ptr Color -> CFloat -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &ColorAlpha"
  p'colorAlpha ::
    FunPtr (Color -> CFloat -> IO Color)

foreign import ccall unsafe "bindings.h ColorAlphaBlend_" colorAlphaBlend :: Ptr Color -> Ptr Color -> Ptr Color -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &ColorAlphaBlend"
  p'colorAlphaBlend ::
    FunPtr (Color -> Color -> Color -> IO Color)

foreign import ccall unsafe "bindings.h GetColor_" getColor :: CUInt -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &GetColor"
  p'getColor ::
    FunPtr (CUInt -> IO Color)

foreign import ccall unsafe "bindings.h GetPixelColor_" getPixelColor :: Ptr () -> CInt -> IO (Ptr Color)

foreign import ccall unsafe "raylib.h &GetPixelColor"
  p'getPixelColor ::
    FunPtr (Ptr () -> CInt -> IO Color)

foreign import ccall unsafe "bindings.h SetPixelColor_" setPixelColor :: Ptr () -> Ptr Color -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &SetPixelColor"
  p'setPixelColor ::
    FunPtr (Ptr () -> Color -> CInt -> IO ())

foreign import ccall unsafe "raylib.h GetPixelDataSize"
  getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "raylib.h &GetPixelDataSize"
  p'getPixelDataSize ::
    FunPtr (CInt -> CInt -> CInt -> IO CInt)

foreign import ccall unsafe "bindings.h GetFontDefault_" getFontDefault :: IO (Ptr Font)

foreign import ccall unsafe "raylib.h &GetFontDefault"
  p'getFontDefault ::
    FunPtr (IO Font)

foreign import ccall unsafe "bindings.h LoadFont_" loadFont :: CString -> IO (Ptr Font)

foreign import ccall unsafe "raylib.h &LoadFont"
  p'loadFont ::
    FunPtr (CString -> IO Font)

foreign import ccall unsafe "bindings.h LoadFontEx_" loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)

foreign import ccall unsafe "raylib.h &LoadFontEx"
  p'loadFontEx ::
    FunPtr (CString -> CInt -> Ptr CInt -> CInt -> IO Font)

foreign import ccall unsafe "bindings.h LoadFontFromImage_" loadFontFromImage :: Ptr Image -> Ptr Color -> CInt -> IO (Ptr Font)

foreign import ccall unsafe "raylib.h &LoadFontFromImage"
  p'loadFontFromImage ::
    FunPtr (Image -> Color -> CInt -> IO Font)

foreign import ccall unsafe "bindings.h LoadFontFromMemory_" loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)

foreign import ccall unsafe "raylib.h &LoadFontFromMemory"
  p'loadFontFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO Font)

foreign import ccall unsafe "raylib.h LoadFontData"
  loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall unsafe "raylib.h &LoadFontData"
  p'loadFontData ::
    FunPtr (Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo))

foreign import ccall unsafe "bindings.h GenImageFontAtlas_" genImageFontAtlas :: Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

foreign import ccall unsafe "raylib.h &GenImageFontAtlas"
  p'genImageFontAtlas ::
    FunPtr (Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO Image)

foreign import ccall unsafe "raylib.h UnloadFontData"
  unloadFontData ::
    Ptr GlyphInfo -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &UnloadFontData"
  p'unloadFontData ::
    FunPtr (Ptr GlyphInfo -> CInt -> IO ())

foreign import ccall unsafe "bindings.h UnloadFont_" unloadFont :: Ptr Font -> IO ()

foreign import ccall unsafe "raylib.h &UnloadFont"
  p'unloadFont ::
    FunPtr (Font -> IO ())

foreign import ccall unsafe "bindings.h ExportFontAsCode_" exportFontAsCode :: Ptr Font -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &ExportFontAsCode"
  p'exportFontAsCode ::
    FunPtr (Font -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h DrawFPS"
  drawFPS ::
    CInt -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &DrawFPS"
  p'drawFPS ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall unsafe "bindings.h DrawText_" drawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawText"
  p'drawText ::
    FunPtr (CString -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextEx_" drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextEx"
  p'drawTextEx ::
    FunPtr (Font -> CString -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextPro_" drawTextPro :: Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextPro"
  p'drawTextPro ::
    FunPtr (Font -> CString -> Vector2 -> Vector2 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextCodepoint_" drawTextCodepoint :: Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextCodepoint"
  p'drawTextCodepoint ::
    FunPtr (Font -> CInt -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTextCodepoints_" drawTextCodepoints :: Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTextCodepoints"
  p'drawTextCodepoints ::
    FunPtr (Font -> Ptr CInt -> CInt -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "raylib.h MeasureText"
  measureText ::
    CString -> CInt -> IO CInt

foreign import ccall unsafe "raylib.h &MeasureText"
  p'measureText ::
    FunPtr (CString -> CInt -> IO CInt)

foreign import ccall unsafe "bindings.h MeasureTextEx_" measureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)

foreign import ccall unsafe "raylib.h &MeasureTextEx"
  p'measureTextEx ::
    FunPtr (Font -> CString -> CFloat -> CFloat -> IO Vector2)

foreign import ccall unsafe "bindings.h GetGlyphIndex_" getGlyphIndex :: Ptr Font -> CInt -> IO CInt

foreign import ccall unsafe "raylib.h &GetGlyphIndex"
  p'getGlyphIndex ::
    FunPtr (Font -> CInt -> IO CInt)

foreign import ccall unsafe "bindings.h GetGlyphInfo_" getGlyphInfo :: Ptr Font -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall unsafe "raylib.h &GetGlyphInfo"
  p'getGlyphInfo ::
    FunPtr (Font -> CInt -> IO GlyphInfo)

foreign import ccall unsafe "bindings.h GetGlyphAtlasRec_" getGlyphAtlasRec :: Ptr Font -> CInt -> IO (Ptr Rectangle)

foreign import ccall unsafe "raylib.h &GetGlyphAtlasRec"
  p'getGlyphAtlasRec ::
    FunPtr (Font -> CInt -> IO Rectangle)

foreign import ccall unsafe "raylib.h LoadUTF8"
  loadUTF8 ::
    Ptr CInt -> CInt -> IO CString

foreign import ccall unsafe "raylib.h &LoadUTF8"
  p'loadUTF8 ::
    FunPtr (Ptr CInt -> CInt -> IO CString)

foreign import ccall unsafe "raylib.h UnloadUTF8"
  unloadUTF8 ::
    CString -> IO ()

foreign import ccall unsafe "raylib.h &UnloadUTF8"
  p'unloadUTF8 ::
    FunPtr (CString -> IO ())

foreign import ccall unsafe "raylib.h LoadCodepoints"
  loadCodepoints ::
    CString -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall unsafe "raylib.h &LoadCodepoints"
  p'loadCodepoints ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr CInt))

foreign import ccall unsafe "raylib.h UnloadCodepoints"
  unloadCodepoints ::
    Ptr CInt -> IO ()

foreign import ccall unsafe "raylib.h &UnloadCodepoints"
  p'unloadCodepoints ::
    FunPtr (Ptr CInt -> IO ())

foreign import ccall unsafe "raylib.h GetCodepointCount"
  getCodepointCount ::
    CString -> IO CInt

foreign import ccall unsafe "raylib.h &GetCodepointCount"
  p'getCodepointCount ::
    FunPtr (CString -> IO CInt)

{-|Deprecated, use `getCodepointNext` -}
foreign import ccall unsafe "raylib.h GetCodepoint"
  getCodepoint ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall unsafe "raylib.h &GetCodepoint"
  p'getCodepoint ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetCodepointNext"
  getCodepointNext ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall unsafe "raylib.h &GetCodepointNext"
  p'getCodepointNext ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall unsafe "raylib.h GetCodepointPrevious"
  getCodepointPrevious ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall unsafe "raylib.h &GetCodepointPrevious"
  p'getCodepointPrevious ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall unsafe "raylib.h CodepointToUTF8"
  codepointToUTF8 ::
    CInt -> Ptr CInt -> IO CString

foreign import ccall unsafe "raylib.h &CodepointToUTF8"
  p'codepointToUTF8 ::
    FunPtr (CInt -> Ptr CInt -> IO CString)

foreign import ccall unsafe "raylib.h TextCopy"
  textCopy ::
    CString -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &TextCopy"
  p'textCopy ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h TextIsEqual"
  textIsEqual ::
    CString -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &TextIsEqual"
  p'textIsEqual ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h TextLength"
  textLength ::
    CString -> IO CUInt

foreign import ccall unsafe "raylib.h &TextLength"
  p'textLength ::
    FunPtr (CString -> IO CUInt)

foreign import ccall unsafe "raylib.h TextFormat"
  textFormat ::
    CString -> IO CString

foreign import ccall unsafe "raylib.h &TextFormat"
  p'textFormat ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h TextSubtext"
  textSubtext ::
    CString -> CInt -> CInt -> IO CString

foreign import ccall unsafe "raylib.h &TextSubtext"
  p'textSubtext ::
    FunPtr (CString -> CInt -> CInt -> IO CString)

foreign import ccall unsafe "raylib.h TextReplace"
  textReplace ::
    CString -> CString -> CString -> IO CString

foreign import ccall unsafe "raylib.h &TextReplace"
  p'textReplace ::
    FunPtr (CString -> CString -> CString -> IO CString)

foreign import ccall unsafe "raylib.h TextInsert"
  textInsert ::
    CString -> CString -> CInt -> IO CString

foreign import ccall unsafe "raylib.h &TextInsert"
  p'textInsert ::
    FunPtr (CString -> CString -> CInt -> IO CString)

foreign import ccall unsafe "raylib.h TextJoin"
  textJoin ::
    Ptr CString -> CInt -> CString -> IO CString

foreign import ccall unsafe "raylib.h &TextJoin"
  p'textJoin ::
    FunPtr (Ptr CString -> CInt -> CString -> IO CString)

foreign import ccall unsafe "raylib.h TextSplit"
  textSplit ::
    CString -> CChar -> Ptr CInt -> IO (Ptr CString)

foreign import ccall unsafe "raylib.h &TextSplit"
  p'textSplit ::
    FunPtr (CString -> CChar -> Ptr CInt -> IO (Ptr CString))

foreign import ccall unsafe "raylib.h TextAppend"
  textAppend ::
    CString -> CString -> Ptr CInt -> IO ()

foreign import ccall unsafe "raylib.h &TextAppend"
  p'textAppend ::
    FunPtr (CString -> CString -> Ptr CInt -> IO ())

foreign import ccall unsafe "raylib.h TextFindIndex"
  textFindIndex ::
    CString -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &TextFindIndex"
  p'textFindIndex ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall unsafe "raylib.h TextToUpper"
  textToUpper ::
    CString -> IO CString

foreign import ccall unsafe "raylib.h &TextToUpper"
  p'textToUpper ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h TextToLower"
  textToLower ::
    CString -> IO CString

foreign import ccall unsafe "raylib.h &TextToLower"
  p'textToLower ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h TextToPascal"
  textToPascal ::
    CString -> IO CString

foreign import ccall unsafe "raylib.h &TextToPascal"
  p'textToPascal ::
    FunPtr (CString -> IO CString)

foreign import ccall unsafe "raylib.h TextToInteger"
  textToInteger ::
    CString -> IO CInt

foreign import ccall unsafe "raylib.h &TextToInteger"
  p'textToInteger ::
    FunPtr (CString -> IO CInt)

foreign import ccall unsafe "bindings.h DrawLine3D_" drawLine3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawLine3D"
  p'drawLine3D ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawPoint3D_" drawPoint3D :: Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawPoint3D"
  p'drawPoint3D ::
    FunPtr (Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCircle3D_" drawCircle3D :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCircle3D"
  p'drawCircle3D ::
    FunPtr (Vector3 -> CFloat -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTriangle3D_" drawTriangle3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTriangle3D"
  p'drawTriangle3D ::
    FunPtr (Vector3 -> Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawTriangleStrip3D_" drawTriangleStrip3D :: Ptr Vector3 -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawTriangleStrip3D"
  p'drawTriangleStrip3D ::
    FunPtr (Ptr Vector3 -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCube_" drawCube :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCube"
  p'drawCube ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCubeV_" drawCubeV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCubeV"
  p'drawCubeV ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCubeWires_" drawCubeWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCubeWires"
  p'drawCubeWires ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCubeWiresV_" drawCubeWiresV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCubeWiresV"
  p'drawCubeWiresV ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCubeTexture_" drawCubeTexture :: Ptr Texture -> Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCubeTexture"
  p'drawCubeTexture ::
    FunPtr (Texture -> Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCubeTextureRec_" drawCubeTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCubeTextureRec"
  p'drawCubeTextureRec ::
    FunPtr (Texture -> Rectangle -> Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawSphere_" drawSphere :: Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawSphere"
  p'drawSphere ::
    FunPtr (Vector3 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawSphereEx_" drawSphereEx :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawSphereEx"
  p'drawSphereEx ::
    FunPtr (Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawSphereWires_" drawSphereWires :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawSphereWires"
  p'drawSphereWires ::
    FunPtr (Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCylinder_" drawCylinder :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCylinder"
  p'drawCylinder ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCylinderEx_" drawCylinderEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCylinderEx"
  p'drawCylinderEx ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCylinderWires_" drawCylinderWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCylinderWires"
  p'drawCylinderWires ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawCylinderWiresEx_" drawCylinderWiresEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawCylinderWiresEx"
  p'drawCylinderWiresEx ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawPlane_" drawPlane :: Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawPlane"
  p'drawPlane ::
    FunPtr (Vector3 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawRay_" drawRay :: Ptr Ray -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawRay"
  p'drawRay ::
    FunPtr (Ray -> Color -> IO ())

foreign import ccall unsafe "raylib.h DrawGrid"
  drawGrid ::
    CInt -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &DrawGrid"
  p'drawGrid ::
    FunPtr (CInt -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h LoadModel_" loadModel :: CString -> IO (Ptr Model)

foreign import ccall unsafe "raylib.h &LoadModel"
  p'loadModel ::
    FunPtr (CString -> IO Model)

foreign import ccall unsafe "bindings.h LoadModelFromMesh_" loadModelFromMesh :: Ptr Mesh -> IO (Ptr Model)

foreign import ccall unsafe "raylib.h &LoadModelFromMesh"
  p'loadModelFromMesh ::
    FunPtr (Mesh -> IO Model)

foreign import ccall unsafe "bindings.h UnloadModel_" unloadModel :: Ptr Model -> IO ()

foreign import ccall unsafe "raylib.h &UnloadModel"
  p'unloadModel ::
    FunPtr (Model -> IO ())

foreign import ccall unsafe "bindings.h UnloadModelKeepMeshes_" unloadModelKeepMeshes :: Ptr Model -> IO ()

foreign import ccall unsafe "raylib.h &UnloadModelKeepMeshes"
  p'unloadModelKeepMeshes ::
    FunPtr (Model -> IO ())

foreign import ccall unsafe "bindings.h GetModelBoundingBox_" getModelBoundingBox :: Ptr Model -> IO (Ptr BoundingBox)

foreign import ccall unsafe "raylib.h &GetModelBoundingBox"
  p'getModelBoundingBox ::
    FunPtr (Model -> IO BoundingBox)

foreign import ccall unsafe "bindings.h DrawModel_" drawModel :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawModel"
  p'drawModel ::
    FunPtr (Model -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawModelEx_" drawModelEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawModelEx"
  p'drawModelEx ::
    FunPtr (Model -> Vector3 -> Vector3 -> CFloat -> Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawModelWires_" drawModelWires :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawModelWires"
  p'drawModelWires ::
    FunPtr (Model -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawModelWiresEx_" drawModelWiresEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawModelWiresEx"
  p'drawModelWiresEx ::
    FunPtr (Model -> Vector3 -> Vector3 -> CFloat -> Vector3 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawBoundingBox_" drawBoundingBox :: Ptr BoundingBox -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawBoundingBox"
  p'drawBoundingBox ::
    FunPtr (BoundingBox -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawBillboard_" drawBillboard :: Ptr Camera3D -> Ptr Texture -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawBillboard"
  p'drawBillboard ::
    FunPtr (Camera3D -> Texture -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawBillboardRec_" drawBillboardRec :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawBillboardRec"
  p'drawBillboardRec ::
    FunPtr (Camera3D -> Texture -> Rectangle -> Vector3 -> Vector2 -> Color -> IO ())

foreign import ccall unsafe "bindings.h DrawBillboardPro_" drawBillboardPro :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall unsafe "raylib.h &DrawBillboardPro"
  p'drawBillboardPro ::
    FunPtr (Camera3D -> Texture -> Rectangle -> Vector3 -> Vector3 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall unsafe "raylib.h UploadMesh"
  uploadMesh ::
    Ptr Mesh -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &UploadMesh"
  p'uploadMesh ::
    FunPtr (Ptr Mesh -> CInt -> IO ())

foreign import ccall unsafe "bindings.h UpdateMeshBuffer_" updateMeshBuffer :: Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &UpdateMeshBuffer"
  p'updateMeshBuffer ::
    FunPtr (Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall unsafe "bindings.h UnloadMesh_" unloadMesh :: Ptr Mesh -> IO ()

foreign import ccall unsafe "raylib.h &UnloadMesh"
  p'unloadMesh ::
    FunPtr (Mesh -> IO ())

foreign import ccall unsafe "bindings.h DrawMesh_" drawMesh :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()

foreign import ccall unsafe "raylib.h &DrawMesh"
  p'drawMesh ::
    FunPtr (Mesh -> Material -> Matrix -> IO ())

foreign import ccall unsafe "bindings.h DrawMeshInstanced_" drawMeshInstanced :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &DrawMeshInstanced"
  p'drawMeshInstanced ::
    FunPtr (Mesh -> Material -> Ptr Matrix -> CInt -> IO ())

foreign import ccall unsafe "bindings.h ExportMesh_" exportMesh :: Ptr Mesh -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &ExportMesh"
  p'exportMesh ::
    FunPtr (Mesh -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h GetMeshBoundingBox_" getMeshBoundingBox :: Ptr Mesh -> IO (Ptr BoundingBox)

foreign import ccall unsafe "raylib.h &GetMeshBoundingBox"
  p'getMeshBoundingBox ::
    FunPtr (Mesh -> IO BoundingBox)

foreign import ccall unsafe "raylib.h GenMeshTangents"
  genMeshTangents ::
    Ptr Mesh -> IO ()

foreign import ccall unsafe "raylib.h &GenMeshTangents"
  p'genMeshTangents ::
    FunPtr (Ptr Mesh -> IO ())

foreign import ccall unsafe "bindings.h GenMeshPoly_" genMeshPoly :: CInt -> CFloat -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshPoly"
  p'genMeshPoly ::
    FunPtr (CInt -> CFloat -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshPlane_" genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshPlane"
  p'genMeshPlane ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshCube_" genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshCube"
  p'genMeshCube ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshSphere_" genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshSphere"
  p'genMeshSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshHemiSphere_" genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshHemiSphere"
  p'genMeshHemiSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshCylinder_" genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshCylinder"
  p'genMeshCylinder ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshCone_" genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshCone"
  p'genMeshCone ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshTorus_" genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshTorus"
  p'genMeshTorus ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshKnot_" genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshKnot"
  p'genMeshKnot ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshHeightmap_" genMeshHeightmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshHeightmap"
  p'genMeshHeightmap ::
    FunPtr (Image -> Vector3 -> IO Mesh)

foreign import ccall unsafe "bindings.h GenMeshCubicmap_" genMeshCubicmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)

foreign import ccall unsafe "raylib.h &GenMeshCubicmap"
  p'genMeshCubicmap ::
    FunPtr (Image -> Vector3 -> IO Mesh)

foreign import ccall unsafe "raylib.h LoadMaterials"
  loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Material)

foreign import ccall unsafe "raylib.h &LoadMaterials"
  p'loadMaterials ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr Material))

foreign import ccall unsafe "bindings.h LoadMaterialDefault_" loadMaterialDefault :: IO (Ptr Material)

foreign import ccall unsafe "raylib.h &LoadMaterialDefault"
  p'loadMaterialDefault ::
    FunPtr (IO Material)

foreign import ccall unsafe "bindings.h UnloadMaterial_" unloadMaterial :: Ptr Material -> IO ()

foreign import ccall unsafe "raylib.h &UnloadMaterial"
  p'unloadMaterial ::
    FunPtr (Material -> IO ())

foreign import ccall unsafe "bindings.h SetMaterialTexture_" setMaterialTexture :: Ptr Material -> CInt -> Ptr Texture -> IO ()

foreign import ccall unsafe "raylib.h &SetMaterialTexture"
  p'setMaterialTexture ::
    FunPtr (Ptr Material -> CInt -> Texture -> IO ())

foreign import ccall unsafe "raylib.h SetModelMeshMaterial"
  setModelMeshMaterial ::
    Ptr Model -> CInt -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &SetModelMeshMaterial"
  p'setModelMeshMaterial ::
    FunPtr (Ptr Model -> CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h LoadModelAnimations"
  loadModelAnimations ::
    CString -> Ptr CUInt -> IO (Ptr ModelAnimation)

foreign import ccall unsafe "raylib.h &LoadModelAnimations"
  p'loadModelAnimations ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr ModelAnimation))

foreign import ccall unsafe "bindings.h UpdateModelAnimation_" updateModelAnimation :: Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &UpdateModelAnimation"
  p'updateModelAnimation ::
    FunPtr (Model -> ModelAnimation -> CInt -> IO ())

foreign import ccall unsafe "bindings.h UnloadModelAnimation_" unloadModelAnimation :: Ptr ModelAnimation -> IO ()

foreign import ccall unsafe "raylib.h &UnloadModelAnimation"
  p'unloadModelAnimation ::
    FunPtr (ModelAnimation -> IO ())

foreign import ccall unsafe "raylib.h UnloadModelAnimations"
  unloadModelAnimations ::
    Ptr ModelAnimation -> CUInt -> IO ()

foreign import ccall unsafe "raylib.h &UnloadModelAnimations"
  p'unloadModelAnimations ::
    FunPtr (Ptr ModelAnimation -> CUInt -> IO ())

foreign import ccall unsafe "bindings.h IsModelAnimationValid_" isModelAnimationValid :: Ptr Model -> Ptr ModelAnimation -> IO CInt

foreign import ccall unsafe "raylib.h &IsModelAnimationValid"
  p'isModelAnimationValid ::
    FunPtr (Model -> ModelAnimation -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionSpheres_" checkCollisionSpheres :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> IO CInt

foreign import ccall unsafe "raylib.h &CheckCollisionSpheres"
  p'checkCollisionSpheres ::
    FunPtr (Vector3 -> CFloat -> Vector3 -> CFloat -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionBoxes_" checkCollisionBoxes :: Ptr BoundingBox -> Ptr BoundingBox -> IO CInt

foreign import ccall unsafe "raylib.h &CheckCollisionBoxes"
  p'checkCollisionBoxes ::
    FunPtr (BoundingBox -> BoundingBox -> IO CInt)

foreign import ccall unsafe "bindings.h CheckCollisionBoxSphere_" checkCollisionBoxSphere :: Ptr BoundingBox -> Ptr Vector3 -> CFloat -> IO CInt

foreign import ccall unsafe "raylib.h &CheckCollisionBoxSphere"
  p'checkCollisionBoxSphere ::
    FunPtr (BoundingBox -> Vector3 -> CFloat -> IO CInt)

foreign import ccall unsafe "bindings.h GetRayCollisionSphere_" getRayCollisionSphere :: Ptr Ray -> Ptr Vector3 -> CFloat -> IO (Ptr RayCollision)

foreign import ccall unsafe "raylib.h &GetRayCollisionSphere"
  p'getRayCollisionSphere ::
    FunPtr (Ray -> Vector3 -> CFloat -> IO RayCollision)

foreign import ccall unsafe "bindings.h GetRayCollisionBox_" getRayCollisionBox :: Ptr Ray -> Ptr BoundingBox -> IO (Ptr RayCollision)

foreign import ccall unsafe "raylib.h &GetRayCollisionBox"
  p'getRayCollisionBox ::
    FunPtr (Ray -> BoundingBox -> IO RayCollision)

foreign import ccall unsafe "bindings.h GetRayCollisionMesh_" getRayCollisionMesh :: Ptr Ray -> Ptr Mesh -> Ptr Matrix -> IO (Ptr RayCollision)

foreign import ccall unsafe "raylib.h &GetRayCollisionMesh"
  p'getRayCollisionMesh ::
    FunPtr (Ray -> Mesh -> Matrix -> IO RayCollision)

foreign import ccall unsafe "bindings.h GetRayCollisionTriangle_" getRayCollisionTriangle :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)

foreign import ccall unsafe "raylib.h &GetRayCollisionTriangle"
  p'getRayCollisionTriangle ::
    FunPtr (Ray -> Vector3 -> Vector3 -> Vector3 -> IO RayCollision)

foreign import ccall unsafe "bindings.h GetRayCollisionQuad_" getRayCollisionQuad :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)

foreign import ccall unsafe "raylib.h &GetRayCollisionQuad"
  p'getRayCollisionQuad ::
    FunPtr (Ray -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> IO RayCollision)

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())

foreign import ccall unsafe "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO AudioCallback

foreign import ccall unsafe "dynamic"
  mK'audioCallback ::
    AudioCallback -> (Ptr () -> CUInt -> IO ())

foreign import ccall unsafe "raylib.h InitAudioDevice"
  initAudioDevice ::
    IO ()

foreign import ccall unsafe "raylib.h &InitAudioDevice"
  p'initAudioDevice ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h CloseAudioDevice"
  closeAudioDevice ::
    IO ()

foreign import ccall unsafe "raylib.h &CloseAudioDevice"
  p'closeAudioDevice ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h IsAudioDeviceReady"
  isAudioDeviceReady ::
    IO CInt

foreign import ccall unsafe "raylib.h &IsAudioDeviceReady"
  p'isAudioDeviceReady ::
    FunPtr (IO CInt)

foreign import ccall unsafe "raylib.h SetMasterVolume"
  setMasterVolume ::
    CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetMasterVolume"
  p'setMasterVolume ::
    FunPtr (CFloat -> IO ())

foreign import ccall unsafe "bindings.h LoadWave_" loadWave :: CString -> IO (Ptr Wave)

foreign import ccall unsafe "raylib.h &LoadWave"
  p'loadWave ::
    FunPtr (CString -> IO Wave)

foreign import ccall unsafe "bindings.h LoadWaveFromMemory_" loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Wave)

foreign import ccall unsafe "raylib.h &LoadWaveFromMemory"
  p'loadWaveFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Wave)

foreign import ccall unsafe "bindings.h LoadSound_" loadSound :: CString -> IO (Ptr Sound)

foreign import ccall unsafe "raylib.h &LoadSound"
  p'loadSound ::
    FunPtr (CString -> IO Sound)

foreign import ccall unsafe "bindings.h LoadSoundFromWave_" loadSoundFromWave :: Ptr Wave -> IO (Ptr Sound)

foreign import ccall unsafe "raylib.h &LoadSoundFromWave"
  p'loadSoundFromWave ::
    FunPtr (Wave -> IO Sound)

foreign import ccall unsafe "bindings.h UpdateSound_" updateSound :: Ptr Sound -> Ptr () -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &UpdateSound"
  p'updateSound ::
    FunPtr (Sound -> Ptr () -> CInt -> IO ())

foreign import ccall unsafe "bindings.h UnloadWave_" unloadWave :: Ptr Wave -> IO ()

foreign import ccall unsafe "raylib.h &UnloadWave"
  p'unloadWave ::
    FunPtr (Wave -> IO ())

foreign import ccall unsafe "bindings.h UnloadSound_" unloadSound :: Ptr Sound -> IO ()

foreign import ccall unsafe "raylib.h &UnloadSound"
  p'unloadSound ::
    FunPtr (Sound -> IO ())

foreign import ccall unsafe "bindings.h ExportWave_" exportWave :: Ptr Wave -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &ExportWave"
  p'exportWave ::
    FunPtr (Wave -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h ExportWaveAsCode_" exportWaveAsCode :: Ptr Wave -> CString -> IO CInt

foreign import ccall unsafe "raylib.h &ExportWaveAsCode"
  p'exportWaveAsCode ::
    FunPtr (Wave -> CString -> IO CInt)

foreign import ccall unsafe "bindings.h PlaySound_" playSound :: Ptr Sound -> IO ()

foreign import ccall unsafe "raylib.h &PlaySound"
  p'playSound ::
    FunPtr (Sound -> IO ())

foreign import ccall unsafe "bindings.h StopSound_" stopSound :: Ptr Sound -> IO ()

foreign import ccall unsafe "raylib.h &StopSound"
  p'stopSound ::
    FunPtr (Sound -> IO ())

foreign import ccall unsafe "bindings.h PauseSound_" pauseSound :: Ptr Sound -> IO ()

foreign import ccall unsafe "raylib.h &PauseSound"
  p'pauseSound ::
    FunPtr (Sound -> IO ())

foreign import ccall unsafe "bindings.h ResumeSound_" resumeSound :: Ptr Sound -> IO ()

foreign import ccall unsafe "raylib.h &ResumeSound"
  p'resumeSound ::
    FunPtr (Sound -> IO ())

foreign import ccall unsafe "bindings.h PlaySoundMulti_" playSoundMulti :: Ptr Sound -> IO ()

foreign import ccall unsafe "raylib.h &PlaySoundMulti"
  p'playSoundMulti ::
    FunPtr (Sound -> IO ())

foreign import ccall unsafe "raylib.h StopSoundMulti"
  stopSoundMulti ::
    IO ()

foreign import ccall unsafe "raylib.h &StopSoundMulti"
  p'stopSoundMulti ::
    FunPtr (IO ())

foreign import ccall unsafe "raylib.h GetSoundsPlaying"
  getSoundsPlaying ::
    IO CInt

foreign import ccall unsafe "raylib.h &GetSoundsPlaying"
  p'getSoundsPlaying ::
    FunPtr (IO CInt)

foreign import ccall unsafe "bindings.h IsSoundPlaying_" isSoundPlaying :: Ptr Sound -> IO CInt

foreign import ccall unsafe "raylib.h &IsSoundPlaying"
  p'isSoundPlaying ::
    FunPtr (Sound -> IO CInt)

foreign import ccall unsafe "bindings.h SetSoundVolume_" setSoundVolume :: Ptr Sound -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetSoundVolume"
  p'setSoundVolume ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetSoundPitch_" setSoundPitch :: Ptr Sound -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetSoundPitch"
  p'setSoundPitch ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetSoundPan_" setSoundPan :: Ptr Sound -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetSoundPan"
  p'setSoundPan ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h WaveCopy_" waveCopy :: Ptr Wave -> IO (Ptr Wave)

foreign import ccall unsafe "raylib.h &WaveCopy"
  p'waveCopy ::
    FunPtr (Wave -> IO Wave)

foreign import ccall unsafe "raylib.h WaveCrop"
  waveCrop ::
    Ptr Wave -> CInt -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &WaveCrop"
  p'waveCrop ::
    FunPtr (Ptr Wave -> CInt -> CInt -> IO ())

foreign import ccall unsafe "raylib.h WaveFormat"
  waveFormat ::
    Ptr Wave -> CInt -> CInt -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &WaveFormat"
  p'waveFormat ::
    FunPtr (Ptr Wave -> CInt -> CInt -> CInt -> IO ())

foreign import ccall unsafe "bindings.h LoadWaveSamples_" loadWaveSamples :: Ptr Wave -> IO (Ptr CFloat)

foreign import ccall unsafe "raylib.h &LoadWaveSamples"
  p'loadWaveSamples ::
    FunPtr (Wave -> IO (Ptr CFloat))

foreign import ccall unsafe "raylib.h UnloadWaveSamples"
  unloadWaveSamples ::
    Ptr CFloat -> IO ()

foreign import ccall unsafe "raylib.h &UnloadWaveSamples"
  p'unloadWaveSamples ::
    FunPtr (Ptr CFloat -> IO ())

foreign import ccall unsafe "bindings.h LoadMusicStream_" loadMusicStream :: CString -> IO (Ptr Music)

foreign import ccall unsafe "raylib.h &LoadMusicStream"
  p'loadMusicStream ::
    FunPtr (CString -> IO Music)

foreign import ccall unsafe "bindings.h LoadMusicStreamFromMemory_" loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Music)

foreign import ccall unsafe "raylib.h &LoadMusicStreamFromMemory"
  p'loadMusicStreamFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Music)

foreign import ccall unsafe "bindings.h UnloadMusicStream_" unloadMusicStream :: Ptr Music -> IO ()

foreign import ccall unsafe "raylib.h &UnloadMusicStream"
  p'unloadMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall unsafe "bindings.h PlayMusicStream_" playMusicStream :: Ptr Music -> IO ()

foreign import ccall unsafe "raylib.h &PlayMusicStream"
  p'playMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall unsafe "bindings.h IsMusicStreamPlaying_" isMusicStreamPlaying :: Ptr Music -> IO CInt

foreign import ccall unsafe "raylib.h &IsMusicStreamPlaying"
  p'isMusicStreamPlaying ::
    FunPtr (Music -> IO CInt)

foreign import ccall unsafe "bindings.h UpdateMusicStream_" updateMusicStream :: Ptr Music -> IO ()

foreign import ccall unsafe "raylib.h &UpdateMusicStream"
  p'updateMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall unsafe "bindings.h StopMusicStream_" stopMusicStream :: Ptr Music -> IO ()

foreign import ccall unsafe "raylib.h &StopMusicStream"
  p'stopMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall unsafe "bindings.h PauseMusicStream_" pauseMusicStream :: Ptr Music -> IO ()

foreign import ccall unsafe "raylib.h &PauseMusicStream"
  p'pauseMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall unsafe "bindings.h ResumeMusicStream_" resumeMusicStream :: Ptr Music -> IO ()

foreign import ccall unsafe "raylib.h &ResumeMusicStream"
  p'resumeMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall unsafe "bindings.h SeekMusicStream_" seekMusicStream :: Ptr Music -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SeekMusicStream"
  p'seekMusicStream ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetMusicVolume_" setMusicVolume :: Ptr Music -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetMusicVolume"
  p'setMusicVolume ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetMusicPitch_" setMusicPitch :: Ptr Music -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetMusicPitch"
  p'setMusicPitch ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetMusicPan_" setMusicPan :: Ptr Music -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetMusicPan"
  p'setMusicPan ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h GetMusicTimeLength_" getMusicTimeLength :: Ptr Music -> IO CFloat

foreign import ccall unsafe "raylib.h &GetMusicTimeLength"
  p'getMusicTimeLength ::
    FunPtr (Music -> IO CFloat)

foreign import ccall unsafe "bindings.h GetMusicTimePlayed_" getMusicTimePlayed :: Ptr Music -> IO CFloat

foreign import ccall unsafe "raylib.h &GetMusicTimePlayed"
  p'getMusicTimePlayed ::
    FunPtr (Music -> IO CFloat)

foreign import ccall unsafe "bindings.h LoadAudioStream_" loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr AudioStream)

foreign import ccall unsafe "raylib.h &LoadAudioStream"
  p'loadAudioStream ::
    FunPtr (CUInt -> CUInt -> CUInt -> IO AudioStream)

foreign import ccall unsafe "bindings.h UnloadAudioStream_" unloadAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall unsafe "raylib.h &UnloadAudioStream"
  p'unloadAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall unsafe "bindings.h UpdateAudioStream_" updateAudioStream :: Ptr AudioStream -> Ptr () -> CInt -> IO ()

foreign import ccall unsafe "raylib.h &UpdateAudioStream"
  p'updateAudioStream ::
    FunPtr (AudioStream -> Ptr () -> CInt -> IO ())

foreign import ccall unsafe "bindings.h IsAudioStreamProcessed_" isAudioStreamProcessed :: Ptr AudioStream -> IO CInt

foreign import ccall unsafe "raylib.h &IsAudioStreamProcessed"
  p'isAudioStreamProcessed ::
    FunPtr (AudioStream -> IO CInt)

foreign import ccall unsafe "bindings.h PlayAudioStream_" playAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall unsafe "raylib.h &PlayAudioStream"
  p'playAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall unsafe "bindings.h PauseAudioStream_" pauseAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall unsafe "raylib.h &PauseAudioStream"
  p'pauseAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall unsafe "bindings.h ResumeAudioStream_" resumeAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall unsafe "raylib.h &ResumeAudioStream"
  p'resumeAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall unsafe "bindings.h IsAudioStreamPlaying_" isAudioStreamPlaying :: Ptr AudioStream -> IO CInt

foreign import ccall unsafe "raylib.h &IsAudioStreamPlaying"
  p'isAudioStreamPlaying ::
    FunPtr (AudioStream -> IO CInt)

foreign import ccall unsafe "bindings.h StopAudioStream_" stopAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall unsafe "raylib.h &StopAudioStream"
  p'stopAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall unsafe "bindings.h SetAudioStreamVolume_" setAudioStreamVolume :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetAudioStreamVolume"
  p'setAudioStreamVolume ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetAudioStreamPitch_" setAudioStreamPitch :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetAudioStreamPitch"
  p'setAudioStreamPitch ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall unsafe "bindings.h SetAudioStreamPan_" setAudioStreamPan :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall unsafe "raylib.h &SetAudioStreamPan"
  p'setAudioStreamPan ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall unsafe "raylib.h SetAudioStreamBufferSizeDefault"
  setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

foreign import ccall unsafe "raylib.h &SetAudioStreamBufferSizeDefault"
  p'setAudioStreamBufferSizeDefault ::
    FunPtr (CInt -> IO ())

foreign import ccall unsafe "bindings.h SetAudioStreamCallback_" setAudioStreamCallback :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall unsafe "raylib.h &SetAudioStreamCallback"
  p'setAudioStreamCallback ::
    FunPtr (AudioStream -> AudioCallback -> IO ())
foreign import ccall unsafe "bindings.h AttachAudioStreamProcessor_" attachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall unsafe "raylib.h &AttachAudioStreamProcessor"
  p'attachAudioStreamProcessor ::
    FunPtr (AudioStream -> AudioCallback -> IO ())

foreign import ccall unsafe "bindings.h DetachAudioStreamProcessor_" detachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall unsafe "raylib.h &DetachAudioStreamProcessor"
  p'detachAudioStreamProcessor ::
    FunPtr (AudioStream -> AudioCallback -> IO ())
