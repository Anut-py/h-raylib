{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -Wall #-}

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
    withArrayLen,
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
import Raylib.Util (pop, withArray2D)
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
    IO CInt

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
    IO CInt

isWindowReady :: IO Bool
isWindowReady = toBool <$> c'isWindowReady

foreign import ccall safe "raylib.h &IsWindowReady"
  p'isWindowReady ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowFullscreen"
  c'isWindowFullscreen ::
    IO CInt

isWindowFullscreen :: IO Bool
isWindowFullscreen = toBool <$> c'isWindowFullscreen

foreign import ccall safe "raylib.h &IsWindowFullscreen"
  p'isWindowFullscreen ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowHidden"
  c'isWindowHidden ::
    IO CInt

isWindowHidden :: IO Bool
isWindowHidden = toBool <$> c'isWindowHidden

foreign import ccall safe "raylib.h &IsWindowHidden"
  p'isWindowHidden ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowMinimized"
  c'isWindowMinimized ::
    IO CInt

isWindowMinimized :: IO Bool
isWindowMinimized = toBool <$> c'isWindowMinimized

foreign import ccall safe "raylib.h &IsWindowMinimized"
  p'isWindowMinimized ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowMaximized"
  c'isWindowMaximized ::
    IO CInt

isWindowMaximized :: IO Bool
isWindowMaximized = toBool <$> c'isWindowMaximized

foreign import ccall safe "raylib.h &IsWindowMaximized"
  p'isWindowMaximized ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowFocused"
  c'isWindowFocused ::
    IO CInt

isWindowFocused :: IO Bool
isWindowFocused = toBool <$> c'isWindowFocused

foreign import ccall safe "raylib.h &IsWindowFocused"
  p'isWindowFocused ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowResized"
  c'isWindowResized ::
    IO CInt

isWindowResized :: IO Bool
isWindowResized = toBool <$> c'isWindowResized

foreign import ccall safe "raylib.h &IsWindowResized"
  p'isWindowResized ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h IsWindowState"
  c'isWindowState ::
    CUInt -> IO CInt

isWindowState :: Integer -> IO Bool
isWindowState flag = toBool <$> c'isWindowState (fromIntegral flag)

foreign import ccall safe "raylib.h &IsWindowState"
  p'isWindowState ::
    FunPtr (CUInt -> IO CInt)

foreign import ccall safe "raylib.h SetWindowState"
  c'setWindowState ::
    CUInt -> IO ()

setWindowState :: Integer -> IO ()
setWindowState = c'setWindowState . fromIntegral

foreign import ccall safe "raylib.h &SetWindowState"
  p'setWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h ClearWindowState"
  c'clearWindowState ::
    CUInt -> IO ()

clearWindowState :: Integer -> IO ()
clearWindowState = c'clearWindowState . fromIntegral

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

foreign import ccall safe "bindings.h SetWindowIcon_" c'setWindowIcon :: Ptr Image -> IO ()

setWindowIcon :: Image -> IO ()
setWindowIcon image = with image c'setWindowIcon

foreign import ccall safe "raylib.h &SetWindowIcon"
  p'setWindowIcon ::
    FunPtr (Image -> IO ())

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

foreign import ccall safe "bindings.h GetMonitorPosition_" c'getMonitorPosition :: CInt -> IO (Ptr Vector2)

getMonitorPosition :: Int -> IO Vector2
getMonitorPosition monitor = c'getMonitorPosition (fromIntegral monitor) >>= pop

foreign import ccall safe "raylib.h &GetMonitorPosition"
  p'getMonitorPosition ::
    FunPtr (CInt -> IO Vector2)

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

getMonitorHeight :: Int -> IO CInt
getMonitorHeight monitor = fromIntegral <$> c'getMonitorHeight (fromIntegral monitor)

foreign import ccall safe "raylib.h &GetMonitorHeight"
  p'getMonitorHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetMonitorPhysicalWidth"
  c'getMonitorPhysicalWidth ::
    CInt -> IO CInt

getMonitorPhysicalWidth :: Int -> IO CInt
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

foreign import ccall safe "bindings.h GetWindowPosition_" c'getWindowPosition :: IO (Ptr Vector2)

getWindowPosition :: IO Vector2
getWindowPosition = c'getWindowPosition >>= pop

foreign import ccall safe "raylib.h &GetWindowPosition"
  p'getWindowPosition ::
    FunPtr (IO Vector2)

foreign import ccall safe "bindings.h GetWindowScaleDPI_" c'getWindowScaleDPI :: IO (Ptr Vector2)

getWindowScaleDPI :: IO Vector2
getWindowScaleDPI = c'getWindowScaleDPI >>= pop

foreign import ccall safe "raylib.h &GetWindowScaleDPI"
  p'getWindowScaleDPI ::
    FunPtr (IO Vector2)

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
    IO CInt

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
    IO CInt

isCursorOnScreen :: IO Bool
isCursorOnScreen = toBool <$> c'isCursorOnScreen

foreign import ccall safe "raylib.h &IsCursorOnScreen"
  p'isCursorOnScreen ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h ClearBackground_" c'clearBackground :: Ptr Color -> IO ()

clearBackground :: Color -> IO ()
clearBackground color = with color c'clearBackground

foreign import ccall safe "raylib.h &ClearBackground"
  p'clearBackground ::
    FunPtr (Color -> IO ())

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

foreign import ccall safe "bindings.h BeginMode2D_" c'beginMode2D :: Ptr Camera2D -> IO ()

beginMode2D :: Camera2D -> IO ()
beginMode2D camera = with camera c'beginMode2D

foreign import ccall safe "raylib.h &BeginMode2D"
  p'beginMode2D ::
    FunPtr (Camera2D -> IO ())

foreign import ccall safe "raylib.h EndMode2D"
  endMode2D ::
    IO ()

foreign import ccall safe "raylib.h &EndMode2D"
  p'endMode2D ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginMode3D_" c'beginMode3D :: Ptr Camera3D -> IO ()

beginMode3D :: Camera3D -> IO ()
beginMode3D camera = with camera c'beginMode3D

foreign import ccall safe "raylib.h &BeginMode3D"
  p'beginMode3D ::
    FunPtr (Camera3D -> IO ())

foreign import ccall safe "raylib.h EndMode3D"
  endMode3D ::
    IO ()

foreign import ccall safe "raylib.h &EndMode3D"
  p'endMode3D ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginTextureMode_" c'beginTextureMode :: Ptr RenderTexture -> IO ()

beginTextureMode :: RenderTexture -> IO ()
beginTextureMode renderTexture = with renderTexture c'beginTextureMode

foreign import ccall safe "raylib.h &BeginTextureMode"
  p'beginTextureMode ::
    FunPtr (RenderTexture -> IO ())

foreign import ccall safe "raylib.h EndTextureMode"
  endTextureMode ::
    IO ()

foreign import ccall safe "raylib.h &EndTextureMode"
  p'endTextureMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginShaderMode_" c'beginShaderMode :: Ptr Shader -> IO ()

beginShaderMode :: Shader -> IO ()
beginShaderMode shader = with shader c'beginShaderMode

foreign import ccall safe "raylib.h &BeginShaderMode"
  p'beginShaderMode ::
    FunPtr (Shader -> IO ())

foreign import ccall safe "raylib.h EndShaderMode"
  endShaderMode ::
    IO ()

foreign import ccall safe "raylib.h &EndShaderMode"
  p'endShaderMode ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h BeginBlendMode"
  c'beginBlendMode ::
    CInt -> IO ()

beginBlendMode :: Int -> IO ()
beginBlendMode = c'beginBlendMode . fromIntegral

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

foreign import ccall safe "bindings.h BeginVrStereoMode_" c'beginVrStereoMode :: Ptr VrStereoConfig -> IO ()

beginVrStereoMode :: VrStereoConfig -> IO ()
beginVrStereoMode config = with config c'beginVrStereoMode

foreign import ccall safe "raylib.h &BeginVrStereoMode"
  p'beginVrStereoMode ::
    FunPtr (VrStereoConfig -> IO ())

foreign import ccall safe "raylib.h EndVrStereoMode"
  endVrStereoMode ::
    IO ()

foreign import ccall safe "raylib.h &EndVrStereoMode"
  p'endVrStereoMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h LoadVrStereoConfig_" c'loadVrStereoConfig :: Ptr VrDeviceInfo -> IO (Ptr VrStereoConfig)

loadVrStereoConfig :: VrDeviceInfo -> IO VrStereoConfig
loadVrStereoConfig deviceInfo = with deviceInfo c'loadVrStereoConfig >>= pop

foreign import ccall safe "raylib.h &LoadVrStereoConfig"
  p'loadVrStereoConfig ::
    FunPtr (VrDeviceInfo -> IO VrStereoConfig)

foreign import ccall safe "bindings.h UnloadVrStereoConfig_" c'unloadVrStereoConfig :: Ptr VrStereoConfig -> IO ()

unloadVrStereoConfig :: VrStereoConfig -> IO ()
unloadVrStereoConfig config = with config c'unloadVrStereoConfig

foreign import ccall safe "raylib.h &UnloadVrStereoConfig"
  p'unloadVrStereoConfig ::
    FunPtr (VrStereoConfig -> IO ())

foreign import ccall safe "bindings.h LoadShader_" c'loadShader :: CString -> CString -> IO (Ptr Shader)

loadShader :: String -> String -> IO Shader
loadShader vsFileName fsFileName = withCString vsFileName (withCString fsFileName . c'loadShader) >>= pop

foreign import ccall safe "raylib.h &LoadShader"
  p'loadShader ::
    FunPtr (CString -> CString -> IO Shader)

foreign import ccall safe "bindings.h LoadShaderFromMemory_" c'loadShaderFromMemory :: CString -> CString -> IO (Ptr Shader)

loadShaderFromMemory :: String -> String -> IO Shader
loadShaderFromMemory vsCode fsCode = withCString vsCode (withCString fsCode . c'loadShaderFromMemory) >>= pop

foreign import ccall safe "raylib.h &LoadShaderFromMemory"
  p'loadShaderFromMemory ::
    FunPtr (CString -> CString -> IO Shader)

foreign import ccall safe "bindings.h GetShaderLocation_" c'getShaderLocation :: Ptr Shader -> CString -> IO CInt

getShaderLocation :: Shader -> String -> IO Int
getShaderLocation shader uniformName = fromIntegral <$> with shader (withCString uniformName . c'getShaderLocation)

foreign import ccall safe "raylib.h &GetShaderLocation"
  p'getShaderLocation ::
    FunPtr (Shader -> CString -> IO CInt)

foreign import ccall safe "bindings.h GetShaderLocationAttrib_" c'getShaderLocationAttrib :: Ptr Shader -> CString -> IO CInt

getShaderLocationAttrib :: Shader -> String -> IO Int
getShaderLocationAttrib shader attribName = fromIntegral <$> with shader (withCString attribName . c'getShaderLocationAttrib)

foreign import ccall safe "raylib.h &GetShaderLocationAttrib"
  p'getShaderLocationAttrib ::
    FunPtr (Shader -> CString -> IO CInt)

foreign import ccall safe "bindings.h SetShaderValue_" c'setShaderValue :: Ptr Shader -> CInt -> Ptr () -> CInt -> IO ()

setShaderValue :: (Storable a) => Shader -> Int -> a -> Int -> IO ()
setShaderValue shader locIndex value uniformType = with value (\v -> with shader (\s -> c'setShaderValue s (fromIntegral locIndex) (castPtr v) (fromIntegral uniformType)))

foreign import ccall safe "raylib.h &SetShaderValue"
  p'setShaderValue ::
    FunPtr (Shader -> CInt -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShaderValueV_" c'setShaderValueV :: Ptr Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()

setShaderValueV :: (Storable a) => Shader -> Int -> a -> Int -> Int -> IO ()
setShaderValueV shader locIndex value uniformType count = with value (\v -> with shader (\s -> c'setShaderValueV s (fromIntegral locIndex) (castPtr v) (fromIntegral uniformType) (fromIntegral count)))

foreign import ccall safe "raylib.h &SetShaderValueV"
  p'setShaderValueV ::
    FunPtr (Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShaderValueMatrix_" c'setShaderValueMatrix :: Ptr Shader -> CInt -> Ptr Matrix -> IO ()

setShaderValueMatrix :: Shader -> Int -> Matrix -> IO ()
setShaderValueMatrix shader locIndex mat = with shader (\s -> with mat (c'setShaderValueMatrix s (fromIntegral locIndex)))

foreign import ccall safe "raylib.h &SetShaderValueMatrix"
  p'setShaderValueMatrix ::
    FunPtr (Shader -> CInt -> Matrix -> IO ())

foreign import ccall safe "bindings.h SetShaderValueTexture_" c'setShaderValueTexture :: Ptr Shader -> CInt -> Ptr Texture -> IO ()

setShaderValueTexture :: Shader -> Int -> Texture -> IO ()
setShaderValueTexture shader locIndex tex = with shader (\s -> with tex (c'setShaderValueTexture s (fromIntegral locIndex)))

foreign import ccall safe "raylib.h &SetShaderValueTexture"
  p'setShaderValueTexture ::
    FunPtr (Shader -> CInt -> Texture -> IO ())

foreign import ccall safe "bindings.h UnloadShader_" c'unloadShader :: Ptr Shader -> IO ()

unloadShader :: Shader -> IO ()
unloadShader shader = with shader c'unloadShader

foreign import ccall safe "raylib.h &UnloadShader"
  p'unloadShader ::
    FunPtr (Shader -> IO ())

foreign import ccall safe "bindings.h GetMouseRay_" c'getMouseRay :: Ptr Vector2 -> Ptr Camera3D -> IO (Ptr Ray)

getMouseRay :: Vector2 -> Camera3D -> IO Ray
getMouseRay mousePosition camera = with mousePosition (with camera . c'getMouseRay) >>= pop

foreign import ccall safe "raylib.h &GetMouseRay"
  p'getMouseRay ::
    FunPtr (Vector2 -> Camera3D -> IO Ray)

foreign import ccall safe "bindings.h GetCameraMatrix_" c'getCameraMatrix :: Ptr Camera3D -> IO (Ptr Matrix)

getCameraMatrix :: Camera3D -> IO Matrix
getCameraMatrix camera = with camera c'getCameraMatrix >>= pop

foreign import ccall safe "raylib.h &GetCameraMatrix"
  p'getCameraMatrix ::
    FunPtr (Camera3D -> IO Matrix)

foreign import ccall safe "bindings.h GetCameraMatrix2D_" c'getCameraMatrix2D :: Ptr Camera2D -> IO (Ptr Matrix)

getCameraMatrix2D :: Camera2D -> IO Matrix
getCameraMatrix2D camera = with camera c'getCameraMatrix2D >>= pop

foreign import ccall safe "raylib.h &GetCameraMatrix2D"
  p'getCameraMatrix2D ::
    FunPtr (Camera2D -> IO Matrix)

foreign import ccall safe "bindings.h GetWorldToScreen_" c'getWorldToScreen :: Ptr Vector3 -> Ptr Camera3D -> IO (Ptr Vector2)

getWorldToScreen :: Vector3 -> Camera3D -> IO Vector2
getWorldToScreen position camera = with position (with camera . c'getWorldToScreen) >>= pop

foreign import ccall safe "raylib.h &GetWorldToScreen"
  p'getWorldToScreen ::
    FunPtr (Vector3 -> Camera3D -> IO Vector2)

foreign import ccall safe "bindings.h GetScreenToWorld2D_" c'getScreenToWorld2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)

getScreenToWorld2D :: Vector2 -> Camera2D -> IO Vector2
getScreenToWorld2D position camera = with position (with camera . c'getScreenToWorld2D) >>= pop

foreign import ccall safe "raylib.h &GetScreenToWorld2D"
  p'getScreenToWorld2D ::
    FunPtr (Vector2 -> Camera2D -> IO Vector2)

foreign import ccall safe "bindings.h GetWorldToScreenEx_" c'getWorldToScreenEx :: Ptr Vector3 -> Ptr Camera3D -> CInt -> CInt -> IO (Ptr Vector2)

getWorldToScreenEx :: Vector3 -> Camera3D -> Int -> Int -> IO Vector2
getWorldToScreenEx position camera width height = with position (\p -> with camera (\c -> c'getWorldToScreenEx p c (fromIntegral width) (fromIntegral height))) >>= pop

foreign import ccall safe "raylib.h &GetWorldToScreenEx"
  p'getWorldToScreenEx ::
    FunPtr (Vector3 -> Camera3D -> CInt -> CInt -> IO Vector2)

foreign import ccall safe "bindings.h GetWorldToScreen2D_" c'getWorldToScreen2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)

getWorldToScreen2D :: Vector2 -> Camera2D -> IO Vector2
getWorldToScreen2D position camera = with position (with camera . c'getWorldToScreen2D) >>= pop

foreign import ccall safe "raylib.h &GetWorldToScreen2D"
  p'getWorldToScreen2D ::
    FunPtr (Vector2 -> Camera2D -> IO Vector2)

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

setConfigFlags :: Integer -> IO ()
setConfigFlags flags = c'setConfigFlags $ fromIntegral flags

foreign import ccall safe "raylib.h &SetConfigFlags"
  p'setConfigFlags ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h TraceLog"
  c'traceLog ::
    CInt -> CString -> IO () -- Uses varags, can't implement complete functionality

traceLog :: Int -> String -> IO ()
traceLog logLevel text = withCString text $ c'traceLog $ fromIntegral logLevel

foreign import ccall safe "raylib.h &TraceLog"
  p'traceLog ::
    FunPtr (CInt -> CString -> IO ())

foreign import ccall safe "raylib.h SetTraceLogLevel"
  c'setTraceLogLevel ::
    CInt -> IO ()

setTraceLogLevel :: Int -> IO ()
setTraceLogLevel logLevel = c'setTraceLogLevel $ fromIntegral logLevel

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

foreign import ccall safe "raylib.h &LoadFileData"
  p'loadFileData ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h UnloadFileData"
  unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall safe "raylib.h &UnloadFileData"
  p'unloadFileData ::
    FunPtr (Ptr CUChar -> IO ())

foreign import ccall safe "raylib.h SaveFileData"
  c'saveFileData ::
    CString -> Ptr () -> CUInt -> IO CInt

saveFileData :: (Storable a) => String -> Ptr a -> Integer -> IO Bool
saveFileData fileName contents bytesToWrite =
  toBool <$> withCString fileName (\s -> c'saveFileData s (castPtr contents) (fromIntegral bytesToWrite))

foreign import ccall safe "raylib.h &SaveFileData"
  p'saveFileData ::
    FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall safe "raylib.h ExportDataAsCode"
  c'exportDataAsCode ::
    CString -> CUInt -> CString -> IO CInt

exportDataAsCode :: String -> Integer -> String -> IO Bool
exportDataAsCode contents size fileName =
  toBool <$> withCString contents (\c -> withCString fileName (c'exportDataAsCode c (fromIntegral size)))

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
    CString -> CString -> IO CInt

saveFileText :: String -> String -> IO Bool
saveFileText fileName text = toBool <$> withCString fileName (withCString text . c'saveFileText)

foreign import ccall safe "raylib.h &SaveFileText"
  p'saveFileText ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall safe "raylib.h FileExists"
  c'fileExists ::
    CString -> IO CInt

fileExists :: String -> IO Bool
fileExists fileName = toBool <$> withCString fileName c'fileExists

foreign import ccall safe "raylib.h &FileExists"
  p'fileExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h DirectoryExists"
  c'directoryExists ::
    CString -> IO CInt

directoryExists :: String -> IO Bool
directoryExists dirPath = toBool <$> withCString dirPath c'directoryExists

foreign import ccall safe "raylib.h &DirectoryExists"
  p'directoryExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h IsFileExtension"
  c'isFileExtension ::
    CString -> CString -> IO CInt

isFileExtension :: String -> String -> IO Bool
isFileExtension fileName ext = toBool <$> withCString fileName (withCString ext . c'isFileExtension)

foreign import ccall safe "raylib.h &IsFileExtension"
  p'isFileExtension ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall safe "raylib.h GetFileLength"
  c'getFileLength ::
    CString -> IO CInt

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
    CString -> IO CInt

changeDirectory :: String -> IO Bool
changeDirectory dir = toBool <$> withCString dir c'changeDirectory

foreign import ccall safe "raylib.h &ChangeDirectory"
  p'changeDirectory ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "raylib.h IsPathFile"
  c'isPathFile ::
    CString -> IO CInt

isPathFile :: String -> IO Bool
isPathFile path = toBool <$> withCString path c'isPathFile

foreign import ccall safe "raylib.h &IsPathFile"
  p'isPathFile ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "bindings.h LoadDirectoryFiles_" c'loadDirectoryFiles :: CString -> IO (Ptr FilePathList)

loadDirectoryFiles :: String -> IO FilePathList
loadDirectoryFiles dirPath = withCString dirPath c'loadDirectoryFiles >>= pop

foreign import ccall safe "raylib.h &LoadDirectoryFiles"
  p'loadDirectoryFiles ::
    FunPtr (CString -> IO FilePathList)

foreign import ccall safe "bindings.h LoadDirectoryFilesEx_" c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr FilePathList)

loadDirectoryFilesEx :: String -> String -> Bool -> IO FilePathList
loadDirectoryFilesEx basePath filterStr scanSubdirs =
  withCString basePath (\b -> withCString filterStr (\f -> c'loadDirectoryFilesEx b f (fromBool scanSubdirs))) >>= pop

foreign import ccall safe "raylib.h &LoadDirectoryFilesEx"
  p'loadDirectoryFilesEx ::
    FunPtr (CString -> CString -> CInt -> IO FilePathList)

foreign import ccall safe "bindings.h UnloadDirectoryFiles_" c'unloadDirectoryFiles :: Ptr FilePathList -> IO ()

unloadDirectoryFiles :: FilePathList -> IO ()
unloadDirectoryFiles files = with files c'unloadDirectoryFiles

foreign import ccall safe "raylib.h &UnloadDirectoryFiles"
  p'unloadDirectoryFiles ::
    FunPtr (FilePathList -> IO ())

foreign import ccall safe "raylib.h IsFileDropped"
  c'isFileDropped ::
    IO CInt

isFileDropped :: IO Bool
isFileDropped = toBool <$> c'isFileDropped

foreign import ccall safe "raylib.h &IsFileDropped"
  p'isFileDropped ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h LoadDroppedFiles_" c'loadDroppedFiles :: IO (Ptr FilePathList)

loadDroppedFiles :: IO FilePathList
loadDroppedFiles = c'loadDroppedFiles >>= pop

foreign import ccall safe "raylib.h &LoadDroppedFiles"
  p'loadDroppedFiles ::
    FunPtr (IO FilePathList)

foreign import ccall safe "bindings.h UnloadDroppedFiles_" c'unloadDroppedFiles :: Ptr FilePathList -> IO ()

unloadDroppedFiles :: FilePathList -> IO ()
unloadDroppedFiles files = with files c'unloadDroppedFiles

foreign import ccall safe "raylib.h &UnloadDroppedFiles"
  p'unloadDroppedFiles ::
    FunPtr (FilePathList -> IO ())

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

foreign import ccall safe "raylib.h &CompressData"
  p'compressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h DecompressData"
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

foreign import ccall safe "raylib.h &DecompressData"
  p'decompressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h EncodeDataBase64"
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

foreign import ccall safe "raylib.h &EncodeDataBase64"
  p'encodeDataBase64 ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO CString)

foreign import ccall safe "raylib.h DecodeDataBase64"
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

foreign import ccall safe "raylib.h &DecodeDataBase64"
  p'decodeDataBase64 ::
    FunPtr (Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall safe "raylib.h IsKeyPressed"
  c'isKeyPressed ::
    CInt -> IO CInt

isKeyPressed :: Int -> IO Bool
isKeyPressed key = toBool <$> c'isKeyPressed (fromIntegral key)

foreign import ccall safe "raylib.h &IsKeyPressed"
  p'isKeyPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsKeyDown"
  c'isKeyDown ::
    CInt -> IO CInt

isKeyDown :: Int -> IO Bool
isKeyDown key = toBool <$> c'isKeyDown (fromIntegral key)

foreign import ccall safe "raylib.h &IsKeyDown"
  p'isKeyDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsKeyReleased"
  c'isKeyReleased ::
    CInt -> IO CInt

isKeyReleased :: Int -> IO Bool
isKeyReleased key = toBool <$> c'isKeyReleased (fromIntegral key)

foreign import ccall safe "raylib.h &IsKeyReleased"
  p'isKeyReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsKeyUp"
  c'isKeyUp ::
    CInt -> IO CInt

isKeyUp :: Int -> IO Bool
isKeyUp key = toBool <$> c'isKeyUp (fromIntegral key)

foreign import ccall safe "raylib.h &IsKeyUp"
  p'isKeyUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h SetExitKey"
  c'setExitKey ::
    CInt -> IO ()

setExitKey :: Int -> IO ()
setExitKey = c'setExitKey . fromIntegral

foreign import ccall safe "raylib.h &SetExitKey"
  p'setExitKey ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "raylib.h GetKeyPressed"
  c'getKeyPressed ::
    IO CInt

getKeyPressed :: IO Int
getKeyPressed = fromIntegral <$> c'getKeyPressed

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
    CInt -> IO CInt

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
    CInt -> CInt -> IO CInt

isGamepadButtonPressed :: Int -> Int -> IO Bool
isGamepadButtonPressed gamepad button = toBool <$> c'isGamepadButtonPressed (fromIntegral gamepad) (fromIntegral button)

foreign import ccall safe "raylib.h &IsGamepadButtonPressed"
  p'isGamepadButtonPressed ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h IsGamepadButtonDown"
  c'isGamepadButtonDown ::
    CInt -> CInt -> IO CInt

isGamepadButtonDown :: Int -> Int -> IO Bool
isGamepadButtonDown gamepad button = toBool <$> c'isGamepadButtonDown (fromIntegral gamepad) (fromIntegral button)

foreign import ccall safe "raylib.h &IsGamepadButtonDown"
  p'isGamepadButtonDown ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h IsGamepadButtonReleased"
  c'isGamepadButtonReleased ::
    CInt -> CInt -> IO CInt

isGamepadButtonReleased :: Int -> Int -> IO Bool
isGamepadButtonReleased gamepad button = toBool <$> c'isGamepadButtonReleased (fromIntegral gamepad) (fromIntegral button)

foreign import ccall safe "raylib.h &IsGamepadButtonReleased"
  p'isGamepadButtonReleased ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h IsGamepadButtonUp"
  c'isGamepadButtonUp ::
    CInt -> CInt -> IO CInt

isGamepadButtonUp :: Int -> Int -> IO Bool
isGamepadButtonUp gamepad button = toBool <$> c'isGamepadButtonUp (fromIntegral gamepad) (fromIntegral button)

foreign import ccall safe "raylib.h &IsGamepadButtonUp"
  p'isGamepadButtonUp ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall safe "raylib.h GetGamepadButtonPressed"
  c'getGamepadButtonPressed ::
    IO CInt

getGamepadButtonPressed :: IO Int
getGamepadButtonPressed = fromIntegral <$> c'getGamepadButtonPressed

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

getGamepadAxisMovement :: Int -> Int -> IO Float
getGamepadAxisMovement gamepad axis = realToFrac <$> c'getGamepadAxisMovement (fromIntegral gamepad) (fromIntegral axis)

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
    CInt -> IO CInt

isMouseButtonPressed :: Int -> IO Bool
isMouseButtonPressed button = toBool <$> c'isMouseButtonPressed (fromIntegral button)

foreign import ccall safe "raylib.h &IsMouseButtonPressed"
  p'isMouseButtonPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonDown"
  c'isMouseButtonDown ::
    CInt -> IO CInt

isMouseButtonDown :: Int -> IO Bool
isMouseButtonDown button = toBool <$> c'isMouseButtonDown (fromIntegral button)

foreign import ccall safe "raylib.h &IsMouseButtonDown"
  p'isMouseButtonDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonReleased"
  c'isMouseButtonReleased ::
    CInt -> IO CInt

isMouseButtonReleased :: Int -> IO Bool
isMouseButtonReleased button = toBool <$> c'isMouseButtonReleased (fromIntegral button)

foreign import ccall safe "raylib.h &IsMouseButtonReleased"
  p'isMouseButtonReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h IsMouseButtonUp"
  c'isMouseButtonUp ::
    CInt -> IO CInt

isMouseButtonUp :: Int -> IO Bool
isMouseButtonUp button = toBool <$> c'isMouseButtonUp (fromIntegral button)

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

foreign import ccall safe "bindings.h GetMousePosition_" c'getMousePosition :: IO (Ptr Vector2)

getMousePosition :: IO Vector2
getMousePosition = c'getMousePosition >>= pop

foreign import ccall safe "raylib.h &GetMousePosition"
  p'getMousePosition ::
    FunPtr (IO Vector2)

foreign import ccall safe "bindings.h GetMouseDelta_" c'getMouseDelta :: IO (Ptr Vector2)

getMouseDelta :: IO Vector2
getMouseDelta = c'getMouseDelta >>= pop

foreign import ccall safe "raylib.h &GetMouseDelta"
  p'getMouseDelta ::
    FunPtr (IO Vector2)

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

foreign import ccall safe "bindings.h GetMouseWheelMoveV_" c'getMouseWheelMoveV :: IO (Ptr Vector2)

getMouseWheelMoveV :: IO Vector2
getMouseWheelMoveV = c'getMouseWheelMoveV >>= pop

foreign import ccall safe "raylib.h &GetMouseWheelMoveV"
  p'getMouseWheelMoveV ::
    FunPtr (IO Vector2)

foreign import ccall safe "raylib.h SetMouseCursor"
  c'setMouseCursor ::
    CInt -> IO ()

setMouseCursor :: Int -> IO ()
setMouseCursor cursor = c'setMouseCursor $ fromIntegral cursor

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

foreign import ccall safe "bindings.h GetTouchPosition_" c'getTouchPosition :: CInt -> IO (Ptr Vector2)

getTouchPosition :: Int -> IO Vector2
getTouchPosition index = c'getTouchPosition (fromIntegral index) >>= pop

foreign import ccall safe "raylib.h &GetTouchPosition"
  p'getTouchPosition ::
    FunPtr (CInt -> IO Vector2)

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

setGesturesEnabled :: Integer -> IO ()
setGesturesEnabled flags = c'setGesturesEnabled (fromIntegral flags)

foreign import ccall safe "raylib.h &SetGesturesEnabled"
  p'setGesturesEnabled ::
    FunPtr (CUInt -> IO ())

foreign import ccall safe "raylib.h IsGestureDetected"
  c'isGestureDetected ::
    CInt -> IO CInt

isGestureDetected :: Int -> IO Bool
isGestureDetected gesture = toBool <$> c'isGestureDetected (fromIntegral gesture)

foreign import ccall safe "raylib.h &IsGestureDetected"
  p'isGestureDetected ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "raylib.h GetGestureDetected"
  c'getGestureDetected ::
    IO CInt

getGestureDetected :: IO Int
getGestureDetected = fromIntegral <$> c'getGestureDetected

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

foreign import ccall safe "bindings.h GetGestureDragVector_" c'getGestureDragVector :: IO (Ptr Vector2)

getGestureDragVector :: IO Vector2
getGestureDragVector = c'getGestureDragVector >>= pop

foreign import ccall safe "raylib.h &GetGestureDragVector"
  p'getGestureDragVector ::
    FunPtr (IO Vector2)

foreign import ccall safe "raylib.h GetGestureDragAngle"
  c'getGestureDragAngle ::
    IO CFloat

getGestureDragAngle :: IO Float
getGestureDragAngle = realToFrac <$> c'getGestureDragAngle

foreign import ccall safe "raylib.h &GetGestureDragAngle"
  p'getGestureDragAngle ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetGesturePinchVector_" c'getGesturePinchVector :: IO (Ptr Vector2)

getGesturePinchVector :: IO Vector2
getGesturePinchVector = c'getGesturePinchVector >>= pop

foreign import ccall safe "raylib.h &GetGesturePinchVector"
  p'getGesturePinchVector ::
    FunPtr (IO Vector2)

foreign import ccall safe "raylib.h GetGesturePinchAngle"
  c'getGesturePinchAngle ::
    IO CFloat

getGesturePinchAngle :: IO Float
getGesturePinchAngle = realToFrac <$> c'getGesturePinchAngle

foreign import ccall safe "raylib.h &GetGesturePinchAngle"
  p'getGesturePinchAngle ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h SetCameraMode_" c'setCameraMode :: Ptr Camera3D -> CInt -> IO ()

setCameraMode :: Camera3D -> Int -> IO ()
setCameraMode camera mode = with camera (\c -> c'setCameraMode c (fromIntegral mode))

foreign import ccall safe "raylib.h &SetCameraMode"
  p'setCameraMode ::
    FunPtr (Camera3D -> CInt -> IO ())

foreign import ccall safe "raylib.h UpdateCamera"
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

foreign import ccall safe "raylib.h &UpdateCamera"
  p'updateCamera ::
    FunPtr (Ptr Camera3D -> IO ())

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

foreign import ccall safe "bindings.h SetShapesTexture_" c'setShapesTexture :: Ptr Texture -> Ptr Rectangle -> IO ()

setShapesTexture :: Texture -> Rectangle -> IO ()
setShapesTexture tex source = with tex (with source . c'setShapesTexture)

foreign import ccall safe "raylib.h &SetShapesTexture"
  p'setShapesTexture ::
    FunPtr (Texture -> Rectangle -> IO ())

foreign import ccall safe "bindings.h DrawPixel_" c'drawPixel :: CInt -> CInt -> Ptr Color -> IO ()

drawPixel :: Int -> Int -> Color -> IO ()
drawPixel x y color = with color $ c'drawPixel (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &DrawPixel"
  p'drawPixel ::
    FunPtr (CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPixelV_" c'drawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()

drawPixelV :: Vector2 -> Color -> IO ()
drawPixelV position color = with position (with color . c'drawPixelV)

foreign import ccall safe "raylib.h &DrawPixelV"
  p'drawPixelV ::
    FunPtr (Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLine_" c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawLine :: Int -> Int -> Int -> Int -> Color -> IO ()
drawLine startX startY endX endY color =
  with color $ c'drawLine (fromIntegral startX) (fromIntegral startY) (fromIntegral endX) (fromIntegral endY)

foreign import ccall safe "raylib.h &DrawLine"
  p'drawLine ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineV_" c'drawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

drawLineV :: Vector2 -> Vector2 -> Color -> IO ()
drawLineV start end color = with start (\s -> with end (with color . c'drawLineV s))

foreign import ccall safe "raylib.h &DrawLineV"
  p'drawLineV ::
    FunPtr (Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineEx_" c'drawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineEx :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineEx start end thickness color =
  with start (\s -> with end (\e -> with color (c'drawLineEx s e (realToFrac thickness))))

foreign import ccall safe "raylib.h &DrawLineEx"
  p'drawLineEx ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezier_" c'drawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineBezier :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezier start end thickness color =
  with start (\s -> with end (\e -> with color (c'drawLineBezier s e (realToFrac thickness))))

foreign import ccall safe "raylib.h &DrawLineBezier"
  p'drawLineBezier ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezierQuad_" c'drawLineBezierQuad :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawLineBezierQuad :: Vector2 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezierQuad start end control thickness color =
  with start (\s -> with end (\e -> with control (\c -> with color (c'drawLineBezierQuad s e c (realToFrac thickness)))))

foreign import ccall safe "raylib.h &DrawLineBezierQuad"
  p'drawLineBezierQuad ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezierCubic_" c'drawLineBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawLineBezierCubic"
  p'drawLineBezierCubic ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineStrip_" c'drawLineStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawLineStrip :: [Vector2] -> Color -> IO ()
drawLineStrip points color = withArray points (\p -> with color $ c'drawLineStrip p (genericLength points))

foreign import ccall safe "raylib.h &DrawLineStrip"
  p'drawLineStrip ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircle_" c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

drawCircle :: Int -> Int -> Float -> Color -> IO ()
drawCircle centerX centerY radius color = with color (c'drawCircle (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall safe "raylib.h &DrawCircle"
  p'drawCircle ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleSector_" c'drawCircleSector :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawCircleSector"
  p'drawCircleSector ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleSectorLines_" c'drawCircleSectorLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawCircleSectorLines"
  p'drawCircleSectorLines ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleGradient_" c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()

drawCircleGradient :: Int -> Int -> Float -> Color -> Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 =
  with color1 (with color2 . c'drawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall safe "raylib.h &DrawCircleGradient"
  p'drawCircleGradient ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleV_" c'drawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawCircleV :: Vector2 -> Float -> Color -> IO ()
drawCircleV center radius color =
  with center (\c -> with color (c'drawCircleV c (realToFrac radius)))

foreign import ccall safe "raylib.h &DrawCircleV"
  p'drawCircleV ::
    FunPtr (Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleLines_" c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

drawCircleLines :: Int -> Int -> Float -> Color -> IO ()
drawCircleLines centerX centerY radius color =
  with color (c'drawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius))

foreign import ccall safe "raylib.h &DrawCircleLines"
  p'drawCircleLines ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawEllipse_" c'drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawEllipse :: Int -> Int -> Float -> Float -> Color -> IO ()
drawEllipse centerX centerY radiusH radiusV color =
  with color (c'drawEllipse (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

foreign import ccall safe "raylib.h &DrawEllipse"
  p'drawEllipse ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawEllipseLines_" c'drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawEllipseLines :: Int -> Int -> Float -> Float -> Color -> IO ()
drawEllipseLines centerX centerY radiusH radiusV color =
  with color (c'drawEllipseLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radiusH) (realToFrac radiusV))

foreign import ccall safe "raylib.h &DrawEllipseLines"
  p'drawEllipseLines ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRing_" c'drawRing :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawRing"
  p'drawRing ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRingLines_" c'drawRingLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawRingLines"
  p'drawRingLines ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangle_" c'drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangle posX posY width height color =
  with color (c'drawRectangle (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

foreign import ccall safe "raylib.h &DrawRectangle"
  p'drawRectangle ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleV_" c'drawRectangleV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

drawRectangleV :: Vector2 -> Vector2 -> Color -> IO ()
drawRectangleV position size color = with position (\p -> with size (with color . c'drawRectangleV p))

foreign import ccall safe "raylib.h &DrawRectangleV"
  p'drawRectangleV ::
    FunPtr (Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRec_" c'drawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()

drawRectangleRec :: Rectangle -> Color -> IO ()
drawRectangleRec rect color = with rect (with color . c'drawRectangleRec)

foreign import ccall safe "raylib.h &DrawRectangleRec"
  p'drawRectangleRec ::
    FunPtr (Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectanglePro_" c'drawRectanglePro :: Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawRectanglePro :: Rectangle -> Vector2 -> Float -> Color -> IO ()
drawRectanglePro rect origin rotation color =
  with color (\c -> with rect (\r -> with origin (\o -> c'drawRectanglePro r o (realToFrac rotation) c)))

foreign import ccall safe "raylib.h &DrawRectanglePro"
  p'drawRectanglePro ::
    FunPtr (Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientV_" c'drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawRectangleGradientV"
  p'drawRectangleGradientV ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientH_" c'drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawRectangleGradientH"
  p'drawRectangleGradientH ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientEx_" c'drawRectangleGradientEx :: Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawRectangleGradientEx"
  p'drawRectangleGradientEx ::
    FunPtr (Rectangle -> Color -> Color -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleLines_" c'drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawRectangleLines :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangleLines posX posY width height color =
  with color (c'drawRectangleLines (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height))

foreign import ccall safe "raylib.h &DrawRectangleLines"
  p'drawRectangleLines ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleLinesEx_" c'drawRectangleLinesEx :: Ptr Rectangle -> CFloat -> Ptr Color -> IO ()

drawRectangleLinesEx :: Rectangle -> Float -> Color -> IO ()
drawRectangleLinesEx rect thickness color =
  with color (\c -> with rect (\r -> c'drawRectangleLinesEx r (realToFrac thickness) c))

foreign import ccall safe "raylib.h &DrawRectangleLinesEx"
  p'drawRectangleLinesEx ::
    FunPtr (Rectangle -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRounded_" c'drawRectangleRounded :: Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()

drawRectangleRounded :: Rectangle -> Float -> Int -> Color -> IO ()
drawRectangleRounded rect roundness segments color =
  with rect (\r -> with color $ c'drawRectangleRounded r (realToFrac roundness) (fromIntegral segments))

foreign import ccall safe "raylib.h &DrawRectangleRounded"
  p'drawRectangleRounded ::
    FunPtr (Rectangle -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRoundedLines_" c'drawRectangleRoundedLines :: Ptr Rectangle -> CFloat -> CInt -> CFloat -> Ptr Color -> IO ()

drawRectangleRoundedLines :: Rectangle -> Float -> Int -> Float -> Color -> IO ()
drawRectangleRoundedLines rect roundness segments thickness color =
  with rect (\r -> with color $ c'drawRectangleRoundedLines r (realToFrac roundness) (fromIntegral segments) (realToFrac thickness))

foreign import ccall safe "raylib.h &DrawRectangleRoundedLines"
  p'drawRectangleRoundedLines ::
    FunPtr (Rectangle -> CFloat -> CInt -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangle_" c'drawTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawTriangle"
  p'drawTriangle ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleLines_" c'drawTriangleLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawTriangleLines"
  p'drawTriangleLines ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleFan_" c'drawTriangleFan :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawTriangleFan :: [Vector2] -> Color -> IO ()
drawTriangleFan points color = withArray points (\p -> with color $ c'drawTriangleFan p (genericLength points))

foreign import ccall safe "raylib.h &DrawTriangleFan"
  p'drawTriangleFan ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleStrip_" c'drawTriangleStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawTriangleStrip :: [Vector2] -> Color -> IO ()
drawTriangleStrip points color =
  withArray points (\p -> with color $ c'drawTriangleStrip p (genericLength points))

foreign import ccall safe "raylib.h &DrawTriangleStrip"
  p'drawTriangleStrip ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPoly_" c'drawPoly :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawPoly :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPoly center sides radius rotation color =
  with center (\c -> with color $ c'drawPoly c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

foreign import ccall safe "raylib.h &DrawPoly"
  p'drawPoly ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPolyLines_" c'drawPolyLines :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

drawPolyLines :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPolyLines center sides radius rotation color =
  with center (\c -> with color $ c'drawPolyLines c (fromIntegral sides) (realToFrac radius) (realToFrac rotation))

foreign import ccall safe "raylib.h &DrawPolyLines"
  p'drawPolyLines ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPolyLinesEx_" c'drawPolyLinesEx :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

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

foreign import ccall safe "raylib.h &DrawPolyLinesEx"
  p'drawPolyLinesEx ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Color -> IO ())

-- unsafePerformIO has been used with these collision functions
-- It is OK to use to here because even though we are passing
-- pointers as arguments, they are created through the `with`
-- function, which ensures that all pointers are destroyed.
-- See https://stackoverflow.com/a/10530919/17907758

foreign import ccall safe "bindings.h CheckCollisionRecs_" c'checkCollisionRecs :: Ptr Rectangle -> Ptr Rectangle -> IO CInt

checkCollisionRecs :: Rectangle -> Rectangle -> Bool
checkCollisionRecs rec1 rec2 = unsafePerformIO $ toBool <$> with rec1 (with rec2 . c'checkCollisionRecs)

foreign import ccall safe "raylib.h &CheckCollisionRecs"
  p'checkCollisionRecs ::
    FunPtr (Rectangle -> Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionCircles_" c'checkCollisionCircles :: Ptr Vector2 -> CFloat -> Ptr Vector2 -> CFloat -> IO CInt

checkCollisionCircles :: Vector2 -> Float -> Vector2 -> Float -> Bool
checkCollisionCircles center1 radius1 center2 radius2 =
  unsafePerformIO $ toBool <$> with center1 (\c1 -> with center2 (\c2 -> c'checkCollisionCircles c1 (realToFrac radius1) c2 (realToFrac radius2)))

foreign import ccall safe "raylib.h &CheckCollisionCircles"
  p'checkCollisionCircles ::
    FunPtr (Vector2 -> CFloat -> Vector2 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionCircleRec_" c'checkCollisionCircleRec :: Ptr Vector2 -> CFloat -> Ptr Rectangle -> IO CInt

checkCollisionCircleRec :: Vector2 -> Float -> Rectangle -> Bool
checkCollisionCircleRec center radius rect =
  unsafePerformIO $ toBool <$> with center (\c -> with rect $ c'checkCollisionCircleRec c (realToFrac radius))

foreign import ccall safe "raylib.h &CheckCollisionCircleRec"
  p'checkCollisionCircleRec ::
    FunPtr (Vector2 -> CFloat -> Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointRec_" c'checkCollisionPointRec :: Ptr Vector2 -> Ptr Rectangle -> IO CInt

checkCollisionPointRec :: Vector2 -> Rectangle -> Bool
checkCollisionPointRec point rect =
  unsafePerformIO $ toBool <$> with point (with rect . c'checkCollisionPointRec)

foreign import ccall safe "raylib.h &CheckCollisionPointRec"
  p'checkCollisionPointRec ::
    FunPtr (Vector2 -> Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointCircle_" c'checkCollisionPointCircle :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO CInt

checkCollisionPointCircle :: Vector2 -> Vector2 -> Float -> Bool
checkCollisionPointCircle point center radius =
  unsafePerformIO $ toBool <$> with point (\p -> with center (\c -> c'checkCollisionPointCircle p c (realToFrac radius)))

foreign import ccall safe "raylib.h &CheckCollisionPointCircle"
  p'checkCollisionPointCircle ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointTriangle_" c'checkCollisionPointTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CInt

checkCollisionPointTriangle :: Vector2 -> Vector2 -> Vector2 -> Vector2 -> Bool
checkCollisionPointTriangle point p1 p2 p3 =
  unsafePerformIO $ toBool <$> with point (\p -> with p1 (\ptr1 -> with p2 (with p3 . c'checkCollisionPointTriangle p ptr1)))

foreign import ccall safe "raylib.h &CheckCollisionPointTriangle"
  p'checkCollisionPointTriangle ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionLines_" c'checkCollisionLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CInt

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

foreign import ccall safe "raylib.h &CheckCollisionLines"
  p'checkCollisionLines ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> Ptr Vector2 -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointLine_" c'checkCollisionPointLine :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CInt

checkCollisionPointLine :: Vector2 -> Vector2 -> Vector2 -> Int -> Bool
checkCollisionPointLine point p1 p2 threshold =
  unsafePerformIO $ toBool <$> with point (\p -> with p1 (\ptr1 -> with p2 (\ptr2 -> c'checkCollisionPointLine p ptr1 ptr2 (fromIntegral threshold))))

foreign import ccall safe "raylib.h &CheckCollisionPointLine"
  p'checkCollisionPointLine ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetCollisionRec_" c'getCollisionRec :: Ptr Rectangle -> Ptr Rectangle -> IO (Ptr Rectangle)

getCollisionRec :: Rectangle -> Rectangle -> Rectangle
getCollisionRec rec1 rec2 =
  unsafePerformIO $ with rec1 (with rec2 . c'getCollisionRec) >>= pop

foreign import ccall safe "raylib.h &GetCollisionRec"
  p'getCollisionRec ::
    FunPtr (Rectangle -> Rectangle -> IO Rectangle)

foreign import ccall safe "bindings.h LoadImage_" c'loadImage :: CString -> IO (Ptr Image)

loadImage :: String -> IO Image
loadImage fileName = withCString fileName c'loadImage >>= pop

foreign import ccall safe "raylib.h &LoadImage"
  p'loadImage ::
    FunPtr (CString -> IO Image)

foreign import ccall safe "bindings.h LoadImageRaw_" c'loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

loadImageRaw :: String -> Int -> Int -> Int -> Int -> IO Image
loadImageRaw fileName width height format headerSize =
  withCString fileName (\str -> c'loadImageRaw str (fromIntegral width) (fromIntegral height) (fromIntegral format) (fromIntegral headerSize)) >>= pop

foreign import ccall safe "raylib.h &LoadImageRaw"
  p'loadImageRaw ::
    FunPtr (CString -> CInt -> CInt -> CInt -> CInt -> IO Image)

foreign import ccall safe "bindings.h LoadImageAnim_" c'loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Image)

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

foreign import ccall safe "raylib.h &LoadImageAnim"
  p'loadImageAnim ::
    FunPtr (CString -> Ptr CInt -> IO Image)

foreign import ccall safe "bindings.h LoadImageFromMemory_" c'loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Image)

loadImageFromMemory :: String -> String -> Int -> IO Image
loadImageFromMemory fileType fileData fileSize =
  withCString fileType (\ft -> withCString fileData (\fd -> c'loadImageFromMemory ft (castPtr fd) (fromIntegral fileSize))) >>= pop

foreign import ccall safe "raylib.h &LoadImageFromMemory"
  p'loadImageFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Image)

foreign import ccall safe "bindings.h LoadImageFromTexture_" c'loadImageFromTexture :: Ptr Texture -> IO (Ptr Image)

loadImageFromTexture :: Texture -> IO Image
loadImageFromTexture tex = with tex c'loadImageFromTexture >>= pop

foreign import ccall safe "raylib.h &LoadImageFromTexture"
  p'loadImageFromTexture ::
    FunPtr (Texture -> IO Image)

foreign import ccall safe "bindings.h LoadImageFromScreen_" c'loadImageFromScreen :: IO (Ptr Image)

loadImageFromScreen :: IO Image
loadImageFromScreen = c'loadImageFromScreen >>= pop

foreign import ccall safe "raylib.h &LoadImageFromScreen"
  p'loadImageFromScreen ::
    FunPtr (IO Image)

foreign import ccall safe "bindings.h UnloadImage_" c'unloadImage :: Ptr Image -> IO ()

unloadImage :: Image -> IO ()
unloadImage image = with image c'unloadImage

foreign import ccall safe "raylib.h &UnloadImage"
  p'unloadImage ::
    FunPtr (Image -> IO ())

foreign import ccall safe "bindings.h ExportImage_" c'exportImage :: Ptr Image -> CString -> IO CInt

exportImage :: Image -> String -> IO Bool
exportImage image fileName = toBool <$> with image (withCString fileName . c'exportImage)

foreign import ccall safe "raylib.h &ExportImage"
  p'exportImage ::
    FunPtr (Image -> CString -> IO CInt)

foreign import ccall safe "bindings.h ExportImageAsCode_" c'exportImageAsCode :: Ptr Image -> CString -> IO CInt

exportImageAsCode :: Image -> String -> IO Bool
exportImageAsCode image fileName =
  toBool <$> with image (withCString fileName . c'exportImageAsCode)

foreign import ccall safe "raylib.h &ExportImageAsCode"
  p'exportImageAsCode ::
    FunPtr (Image -> CString -> IO CInt)

foreign import ccall safe "bindings.h GenImageColor_" c'genImageColor :: CInt -> CInt -> Ptr Color -> IO (Ptr Image)

genImageColor :: Int -> Int -> Color -> IO Image
genImageColor width height color =
  with color (c'genImageColor (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageColor"
  p'genImageColor ::
    FunPtr (CInt -> CInt -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageGradientV_" c'genImageGradientV :: CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageGradientV :: Int -> Int -> Color -> Color -> IO Image
genImageGradientV width height top bottom =
  with top (with bottom . c'genImageGradientV (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageGradientV"
  p'genImageGradientV ::
    FunPtr (CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageGradientH_" c'genImageGradientH :: CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageGradientH :: Int -> Int -> Color -> Color -> IO Image
genImageGradientH width height left right =
  with left (with right . c'genImageGradientH (fromIntegral width) (fromIntegral height)) >>= pop

foreign import ccall safe "raylib.h &GenImageGradientH"
  p'genImageGradientH ::
    FunPtr (CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageGradientRadial_" c'genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageGradientRadial :: Int -> Int -> Float -> Color -> Color -> IO Image
genImageGradientRadial width height density inner outer =
  with inner (with outer . c'genImageGradientRadial (fromIntegral width) (fromIntegral height) (realToFrac density)) >>= pop

foreign import ccall safe "raylib.h &GenImageGradientRadial"
  p'genImageGradientRadial ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageChecked_" c'genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

genImageChecked :: Int -> Int -> Int -> Int -> Color -> Color -> IO Image
genImageChecked width height checksX checksY col1 col2 =
  with col1 (with col2 . c'genImageChecked (fromIntegral width) (fromIntegral height) (fromIntegral checksX) (fromIntegral checksY)) >>= pop

foreign import ccall safe "raylib.h &GenImageChecked"
  p'genImageChecked ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageWhiteNoise_" c'genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Image)

genImageWhiteNoise :: Int -> Int -> Float -> IO Image
genImageWhiteNoise width height factor =
  c'genImageWhiteNoise (fromIntegral width) (fromIntegral height) (realToFrac factor) >>= pop

foreign import ccall safe "raylib.h &GenImageWhiteNoise"
  p'genImageWhiteNoise ::
    FunPtr (CInt -> CInt -> CFloat -> IO Image)

foreign import ccall safe "bindings.h GenImagePerlinNoise_" c'genImagePerlinNoise :: CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Image)

genImagePerlinNoise :: Int -> Int -> Int -> Int -> Float -> IO Image
genImagePerlinNoise width height offsetX offsetY scale = c'genImagePerlinNoise (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY) (realToFrac scale) >>= pop

foreign import ccall safe "raylib.h &GenImagePerlinNoise" p'genImagePerlinNoise :: FunPtr (CInt -> CInt -> CInt -> CInt -> CFloat -> IO Image)

foreign import ccall safe "bindings.h GenImageCellular_" c'genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Image)

genImageCellular :: Int -> Int -> Int -> IO Image
genImageCellular width height tileSize =
  c'genImageCellular (fromIntegral width) (fromIntegral height) (fromIntegral tileSize) >>= pop

foreign import ccall safe "raylib.h &GenImageCellular"
  p'genImageCellular ::
    FunPtr (CInt -> CInt -> CInt -> IO Image)

foreign import ccall safe "bindings.h ImageCopy_" c'imageCopy :: Ptr Image -> IO (Ptr Image)

imageCopy :: Image -> IO Image
imageCopy image = with image c'imageCopy >>= pop

foreign import ccall safe "raylib.h &ImageCopy"
  p'imageCopy ::
    FunPtr (Image -> IO Image)

foreign import ccall safe "bindings.h ImageFromImage_" c'imageFromImage :: Ptr Image -> Ptr Rectangle -> IO (Ptr Image)

imageFromImage :: Image -> Rectangle -> IO Image
imageFromImage image rect = with image (with rect . c'imageFromImage) >>= pop

foreign import ccall safe "raylib.h &ImageFromImage"
  p'imageFromImage ::
    FunPtr (Image -> Rectangle -> IO Image)

foreign import ccall safe "bindings.h ImageText_" c'imageText :: CString -> CInt -> Ptr Color -> IO (Ptr Image)

imageText :: String -> Int -> Color -> IO Image
imageText text fontSize color =
  withCString text (\t -> with color $ c'imageText t (fromIntegral fontSize)) >>= pop

foreign import ccall safe "raylib.h &ImageText"
  p'imageText ::
    FunPtr (CString -> CInt -> Color -> IO Image)

foreign import ccall safe "bindings.h ImageTextEx_" c'imageTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> Ptr Color -> IO (Ptr Image)

imageTextEx :: Font -> String -> Float -> Float -> Color -> IO Image
imageTextEx font text fontSize spacing tint =
  with font (\f -> withCString text (\t -> with tint $ c'imageTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

foreign import ccall safe "raylib.h &ImageTextEx"
  p'imageTextEx ::
    FunPtr (Font -> CString -> CFloat -> CFloat -> Color -> IO Image)

foreign import ccall safe "raylib.h ImageFormat"
  c'imageFormat ::
    Ptr Image -> CInt -> IO ()

imageFormat :: Image -> Int -> IO Image
imageFormat image newFormat =
  with image (\i -> c'imageFormat i (fromIntegral newFormat) >> peek i)

foreign import ccall safe "raylib.h &ImageFormat"
  p'imageFormat ::
    FunPtr (Ptr Image -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageToPOT_" c'imageToPOT :: Ptr Image -> Ptr Color -> IO ()

imageToPOT :: Image -> Color -> IO Image
imageToPOT image color = with image (\i -> with color (c'imageToPOT i) >> peek i)

foreign import ccall safe "raylib.h &ImageToPOT"
  p'imageToPOT ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall safe "bindings.h ImageCrop_" c'imageCrop :: Ptr Image -> Ptr Rectangle -> IO ()

imageCrop :: Image -> Rectangle -> IO Image
imageCrop image crop = with image (\i -> with crop (c'imageCrop i) >> peek i)

foreign import ccall safe "raylib.h &ImageCrop"
  p'imageCrop ::
    FunPtr (Ptr Image -> Rectangle -> IO ())

foreign import ccall safe "raylib.h ImageAlphaCrop"
  c'imageAlphaCrop ::
    Ptr Image -> CFloat -> IO ()

imageAlphaCrop :: Image -> Float -> IO Image
imageAlphaCrop image threshold = with image (\i -> c'imageAlphaCrop i (realToFrac threshold) >> peek i)

foreign import ccall safe "raylib.h &ImageAlphaCrop"
  p'imageAlphaCrop ::
    FunPtr (Ptr Image -> CFloat -> IO ())

foreign import ccall safe "bindings.h ImageAlphaClear_" c'imageAlphaClear :: Ptr Image -> Ptr Color -> CFloat -> IO ()

imageAlphaClear :: Image -> Color -> Float -> IO Image
imageAlphaClear image color threshold = with image (\i -> with color (\c -> c'imageAlphaClear i c (realToFrac threshold) >> peek i))

foreign import ccall safe "raylib.h &ImageAlphaClear"
  p'imageAlphaClear ::
    FunPtr (Ptr Image -> Color -> CFloat -> IO ())

foreign import ccall safe "bindings.h ImageAlphaMask_" c'imageAlphaMask :: Ptr Image -> Ptr Image -> IO ()

imageAlphaMask :: Image -> Image -> IO Image
imageAlphaMask image alphaMask = with image (\i -> with alphaMask (c'imageAlphaMask i) >> peek i)

foreign import ccall safe "raylib.h &ImageAlphaMask"
  p'imageAlphaMask ::
    FunPtr (Ptr Image -> Image -> IO ())

foreign import ccall safe "raylib.h ImageAlphaPremultiply"
  c'imageAlphaPremultiply ::
    Ptr Image -> IO ()

imageAlphaPremultiply :: Image -> IO Image
imageAlphaPremultiply image = with image (\i -> c'imageAlphaPremultiply i >> peek i)

foreign import ccall safe "raylib.h &ImageAlphaPremultiply"
  p'imageAlphaPremultiply ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageResize"
  c'imageResize ::
    Ptr Image -> CInt -> CInt -> IO ()

imageResize :: Image -> Int -> Int -> IO Image
imageResize image newWidth newHeight = with image (\i -> c'imageResize i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

foreign import ccall safe "raylib.h &ImageResize"
  p'imageResize ::
    FunPtr (Ptr Image -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h ImageResizeNN"
  c'imageResizeNN ::
    Ptr Image -> CInt -> CInt -> IO ()

imageResizeNN :: Image -> Int -> Int -> IO Image
imageResizeNN image newWidth newHeight = with image (\i -> c'imageResizeNN i (fromIntegral newWidth) (fromIntegral newHeight) >> peek i)

foreign import ccall safe "raylib.h &ImageResizeNN"
  p'imageResizeNN ::
    FunPtr (Ptr Image -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageResizeCanvas_" c'imageResizeCanvas :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageResizeCanvas :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageResizeCanvas image newWidth newHeight offsetX offsetY fill = with image (\i -> with fill (c'imageResizeCanvas i (fromIntegral newWidth) (fromIntegral newHeight) (fromIntegral offsetX) (fromIntegral offsetY)) >> peek i)

foreign import ccall safe "raylib.h &ImageResizeCanvas"
  p'imageResizeCanvas ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "raylib.h ImageMipmaps"
  c'imageMipmaps ::
    Ptr Image -> IO ()

imageMipmaps :: Image -> IO Image
imageMipmaps image = with image (\i -> c'imageMipmaps i >> peek i)

foreign import ccall safe "raylib.h &ImageMipmaps"
  p'imageMipmaps ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageDither"
  c'imageDither ::
    Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()

imageDither :: Image -> Int -> Int -> Int -> Int -> IO Image
imageDither image rBpp gBpp bBpp aBpp = with image (\i -> c'imageDither i (fromIntegral rBpp) (fromIntegral gBpp) (fromIntegral bBpp) (fromIntegral aBpp) >> peek i)

foreign import ccall safe "raylib.h &ImageDither"
  p'imageDither ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h ImageFlipVertical"
  c'imageFlipVertical ::
    Ptr Image -> IO ()

imageFlipVertical :: Image -> IO Image
imageFlipVertical image = with image (\i -> c'imageFlipVertical i >> peek i)

foreign import ccall safe "raylib.h &ImageFlipVertical"
  p'imageFlipVertical ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageFlipHorizontal"
  c'imageFlipHorizontal ::
    Ptr Image -> IO ()

imageFlipHorizontal :: Image -> IO Image
imageFlipHorizontal image = with image (\i -> c'imageFlipHorizontal i >> peek i)

foreign import ccall safe "raylib.h &ImageFlipHorizontal"
  p'imageFlipHorizontal ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageRotateCW"
  c'imageRotateCW ::
    Ptr Image -> IO ()

imageRotateCW :: Image -> IO Image
imageRotateCW image = with image (\i -> c'imageRotateCW i >> peek i)

foreign import ccall safe "raylib.h &ImageRotateCW"
  p'imageRotateCW ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageRotateCCW"
  c'imageRotateCCW ::
    Ptr Image -> IO ()

imageRotateCCW :: Image -> IO Image
imageRotateCCW image = with image (\i -> c'imageRotateCCW i >> peek i)

foreign import ccall safe "raylib.h &ImageRotateCCW"
  p'imageRotateCCW ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "bindings.h ImageColorTint_" c'imageColorTint :: Ptr Image -> Ptr Color -> IO ()

imageColorTint :: Image -> Color -> IO Image
imageColorTint image color = with image (\i -> with color (c'imageColorTint i) >> peek i)

foreign import ccall safe "raylib.h &ImageColorTint"
  p'imageColorTint ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall safe "raylib.h ImageColorInvert"
  c'imageColorInvert ::
    Ptr Image -> IO ()

imageColorInvert :: Image -> IO Image
imageColorInvert image = with image (\i -> c'imageColorInvert i >> peek i)

foreign import ccall safe "raylib.h &ImageColorInvert"
  p'imageColorInvert ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageColorGrayscale"
  c'imageColorGrayscale ::
    Ptr Image -> IO ()

imageColorGrayscale :: Image -> IO Image
imageColorGrayscale image = with image (\i -> c'imageColorGrayscale i >> peek i)

foreign import ccall safe "raylib.h &ImageColorGrayscale"
  p'imageColorGrayscale ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "raylib.h ImageColorContrast"
  c'imageColorContrast ::
    Ptr Image -> CFloat -> IO ()

imageColorContrast :: Image -> Float -> IO Image
imageColorContrast image contrast = with image (\i -> c'imageColorContrast i (realToFrac contrast) >> peek i)

foreign import ccall safe "raylib.h &ImageColorContrast"
  p'imageColorContrast ::
    FunPtr (Ptr Image -> CFloat -> IO ())

foreign import ccall safe "raylib.h ImageColorBrightness"
  c'imageColorBrightness ::
    Ptr Image -> CInt -> IO ()

imageColorBrightness :: Image -> Int -> IO Image
imageColorBrightness image brightness = with image (\i -> c'imageColorBrightness i (fromIntegral brightness) >> peek i)

foreign import ccall safe "raylib.h &ImageColorBrightness"
  p'imageColorBrightness ::
    FunPtr (Ptr Image -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageColorReplace_" c'imageColorReplace :: Ptr Image -> Ptr Color -> Ptr Color -> IO ()

imageColorReplace :: Image -> Color -> Color -> IO Image
imageColorReplace image color replace = with image (\i -> with color (with replace . c'imageColorReplace i) >> peek i)

foreign import ccall safe "raylib.h &ImageColorReplace"
  p'imageColorReplace ::
    FunPtr (Ptr Image -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h LoadImageColors_" c'loadImageColors :: Ptr Image -> IO (Ptr Color)

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

foreign import ccall safe "raylib.h &LoadImageColors"
  p'loadImageColors ::
    FunPtr (Image -> IO (Ptr Color))

foreign import ccall safe "bindings.h LoadImagePalette_" c'loadImagePalette :: Ptr Image -> CInt -> Ptr CInt -> IO (Ptr Color)

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

foreign import ccall safe "raylib.h &LoadImagePalette"
  p'loadImagePalette ::
    FunPtr (Image -> CInt -> Ptr CInt -> IO (Ptr Color))

-- | NOTE: You usually won't need to use this. `loadImageColors` unloads the colors automatically. Only use this when you are using `c'loadImageColors` to load the colors.
foreign import ccall safe "raylib.h UnloadImageColors"
  unloadImageColors ::
    Ptr Color -> IO ()

foreign import ccall safe "raylib.h &UnloadImageColors"
  p'unloadImageColors ::
    FunPtr (Ptr Color -> IO ())

-- | NOTE: You usually won't need to use this. `loadImagePalette` unloads the colors automatically. Only use this when you are using `c'loadImagePalette` to load the colors.
foreign import ccall safe "raylib.h UnloadImagePalette"
  unloadImagePalette ::
    Ptr Color -> IO ()

foreign import ccall safe "raylib.h &UnloadImagePalette"
  p'unloadImagePalette ::
    FunPtr (Ptr Color -> IO ())

foreign import ccall safe "bindings.h GetImageAlphaBorder_" c'getImageAlphaBorder :: Ptr Image -> CFloat -> IO (Ptr Rectangle)

getImageAlphaBorder :: Image -> Float -> IO Rectangle
getImageAlphaBorder image threshold = with image (\i -> c'getImageAlphaBorder i (realToFrac threshold)) >>= pop

foreign import ccall safe "raylib.h &GetImageAlphaBorder"
  p'getImageAlphaBorder ::
    FunPtr (Image -> CFloat -> IO Rectangle)

foreign import ccall safe "bindings.h GetImageColor_" c'getImageColor :: Ptr Image -> CInt -> CInt -> IO (Ptr Color)

getImageColor :: Image -> Int -> Int -> IO Color
getImageColor image x y = with image (\i -> c'getImageColor i (fromIntegral x) (fromIntegral y)) >>= pop

foreign import ccall safe "raylib.h &GetImageColor"
  p'getImageColor ::
    FunPtr (Image -> CInt -> CInt -> IO Color)

foreign import ccall safe "bindings.h ImageClearBackground_" c'imageClearBackground :: Ptr Image -> Ptr Color -> IO ()

imageClearBackground :: Image -> Color -> IO Image
imageClearBackground image color = with image (\i -> with color (c'imageClearBackground i) >> peek i)

foreign import ccall safe "raylib.h &ImageClearBackground"
  p'imageClearBackground ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawPixel_" c'imageDrawPixel :: Ptr Image -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawPixel :: Image -> Int -> Int -> Color -> IO Image
imageDrawPixel image x y color = with image (\i -> with color (c'imageDrawPixel i (fromIntegral x) (fromIntegral y)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawPixel"
  p'imageDrawPixel ::
    FunPtr (Ptr Image -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawPixelV_" c'imageDrawPixelV :: Ptr Image -> Ptr Vector2 -> Ptr Color -> IO ()

imageDrawPixelV :: Image -> Vector2 -> Color -> IO Image
imageDrawPixelV image position color = with image (\i -> with position (with color . c'imageDrawPixelV i) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawPixelV"
  p'imageDrawPixelV ::
    FunPtr (Ptr Image -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawLine_" c'imageDrawLine :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawLine :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageDrawLine image startPosX startPosY endPosX endPosY color = with image (\i -> with color (c'imageDrawLine i (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawLine"
  p'imageDrawLine ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawLineV_" c'imageDrawLineV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

imageDrawLineV :: Image -> Vector2 -> Vector2 -> Color -> IO Image
imageDrawLineV image start end color = with image (\i -> with start (\s -> with end (with color . c'imageDrawLineV i s)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawLineV"
  p'imageDrawLineV ::
    FunPtr (Ptr Image -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircle_" c'imageDrawCircle :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawCircle :: Image -> Int -> Int -> Int -> Color -> IO Image
imageDrawCircle image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircle i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircle"
  p'imageDrawCircle ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleV_" c'imageDrawCircleV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

imageDrawCircleV :: Image -> Vector2 -> Int -> Color -> IO Image
imageDrawCircleV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleV i c (fromIntegral radius))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircleV"
  p'imageDrawCircleV ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleLines_" c'imageDrawCircleLines :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawCircleLines :: Image -> Int -> Int -> Int -> Color -> IO Image
imageDrawCircleLines image centerX centerY radius color = with image (\i -> with color (c'imageDrawCircleLines i (fromIntegral centerX) (fromIntegral centerY) (fromIntegral radius)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircleLines"
  p'imageDrawCircleLines ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleLinesV_" c'imageDrawCircleLinesV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

imageDrawCircleLinesV :: Image -> Vector2 -> Int -> Color -> IO Image
imageDrawCircleLinesV image center radius color = with image (\i -> with center (\c -> with color (c'imageDrawCircleLinesV i c (fromIntegral radius))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawCircleLinesV"
  p'imageDrawCircleLinesV ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangle_" c'imageDrawRectangle :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawRectangle :: Image -> Int -> Int -> Int -> Int -> Color -> IO Image
imageDrawRectangle image posX posY width height color = with image (\i -> with color (c'imageDrawRectangle i (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangle"
  p'imageDrawRectangle ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleV_" c'imageDrawRectangleV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

imageDrawRectangleV :: Image -> Vector2 -> Vector2 -> Color -> IO Image
imageDrawRectangleV image position size color = with image (\i -> with position (\p -> with size (with color . c'imageDrawRectangleV i p)) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangleV"
  p'imageDrawRectangleV ::
    FunPtr (Ptr Image -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleRec_" c'imageDrawRectangleRec :: Ptr Image -> Ptr Rectangle -> Ptr Color -> IO ()

imageDrawRectangleRec :: Image -> Rectangle -> Color -> IO Image
imageDrawRectangleRec image rectangle color = with image (\i -> with rectangle (with color . c'imageDrawRectangleRec i) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangleRec"
  p'imageDrawRectangleRec ::
    FunPtr (Ptr Image -> Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleLines_" c'imageDrawRectangleLines :: Ptr Image -> Ptr Rectangle -> CInt -> Ptr Color -> IO ()

imageDrawRectangleLines :: Image -> Rectangle -> Int -> Color -> IO Image
imageDrawRectangleLines image rectangle thickness color = with image (\i -> with rectangle (\r -> with color (c'imageDrawRectangleLines i r (fromIntegral thickness))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawRectangleLines"
  p'imageDrawRectangleLines ::
    FunPtr (Ptr Image -> Rectangle -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDraw_" c'imageDraw :: Ptr Image -> Ptr Image -> Ptr Rectangle -> Ptr Rectangle -> Ptr Color -> IO ()

imageDraw :: Image -> Image -> Rectangle -> Rectangle -> Color -> IO Image
imageDraw image source srcRec dstRec tint = with image (\i -> with source (\s -> with srcRec (\sr -> with dstRec (with tint . c'imageDraw i s sr))) >> peek i)

foreign import ccall safe "raylib.h &ImageDraw"
  p'imageDraw ::
    FunPtr (Ptr Image -> Image -> Rectangle -> Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawText_" c'imageDrawText :: Ptr Image -> CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

imageDrawText :: Image -> String -> Int -> Int -> Int -> Color -> IO Image
imageDrawText image text x y fontSize color = with image (\i -> withCString text (\t -> with color (c'imageDrawText i t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawText"
  p'imageDrawText ::
    FunPtr (Ptr Image -> CString -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawTextEx_" c'imageDrawTextEx :: Ptr Image -> Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

imageDrawTextEx :: Image -> Font -> String -> Vector2 -> Float -> Float -> Color -> IO Image
imageDrawTextEx image font text position fontSize spacing tint = with image (\i -> with font (\f -> withCString text (\t -> with position (\p -> with tint (c'imageDrawTextEx i f t p (realToFrac fontSize) (realToFrac spacing))))) >> peek i)

foreign import ccall safe "raylib.h &ImageDrawTextEx"
  p'imageDrawTextEx ::
    FunPtr (Ptr Image -> Font -> CString -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h LoadTexture_" c'loadTexture :: CString -> IO (Ptr Texture)

loadTexture :: String -> IO Texture
loadTexture fileName = withCString fileName c'loadTexture >>= pop

foreign import ccall safe "raylib.h &LoadTexture"
  p'loadTexture ::
    FunPtr (CString -> IO Texture)

foreign import ccall safe "bindings.h LoadTextureFromImage_" c'loadTextureFromImage :: Ptr Image -> IO (Ptr Texture)

loadTextureFromImage :: Image -> IO Texture
loadTextureFromImage image = with image c'loadTextureFromImage >>= pop

foreign import ccall safe "raylib.h &LoadTextureFromImage"
  p'loadTextureFromImage ::
    FunPtr (Image -> IO Texture)

foreign import ccall safe "bindings.h LoadTextureCubemap_" c'loadTextureCubemap :: Ptr Image -> CInt -> IO (Ptr Texture)

loadTextureCubemap :: Image -> Int -> IO Texture
loadTextureCubemap image layout = with image (\i -> c'loadTextureCubemap i (fromIntegral layout)) >>= pop

foreign import ccall safe "raylib.h &LoadTextureCubemap"
  p'loadTextureCubemap ::
    FunPtr (Image -> CInt -> IO Texture)

foreign import ccall safe "bindings.h LoadRenderTexture_" c'loadRenderTexture :: CInt -> CInt -> IO (Ptr RenderTexture)

loadRenderTexture :: Int -> Int -> IO RenderTexture
loadRenderTexture width height = c'loadRenderTexture (fromIntegral width) (fromIntegral height) >>= pop

foreign import ccall safe "raylib.h &LoadRenderTexture"
  p'loadRenderTexture ::
    FunPtr (CInt -> CInt -> IO RenderTexture)

foreign import ccall safe "bindings.h UnloadTexture_" c'unloadTexture :: Ptr Texture -> IO ()

unloadTexture :: Texture -> IO ()
unloadTexture texture = with texture c'unloadTexture

foreign import ccall safe "raylib.h &UnloadTexture"
  p'unloadTexture ::
    FunPtr (Texture -> IO ())

foreign import ccall safe "bindings.h UnloadRenderTexture_" c'unloadRenderTexture :: Ptr RenderTexture -> IO ()

unloadRenderTexture :: RenderTexture -> IO ()
unloadRenderTexture target = with target c'unloadRenderTexture

foreign import ccall safe "raylib.h &UnloadRenderTexture"
  p'unloadRenderTexture ::
    FunPtr (RenderTexture -> IO ())

foreign import ccall safe "bindings.h UpdateTexture_" c'updateTexture :: Ptr Texture -> Ptr () -> IO ()

updateTexture :: Texture -> Ptr () -> IO Texture
updateTexture texture pixels = with texture (\t -> c'updateTexture t pixels >> peek t)

foreign import ccall safe "raylib.h &UpdateTexture"
  p'updateTexture ::
    FunPtr (Texture -> Ptr () -> IO ())

foreign import ccall safe "bindings.h UpdateTextureRec_" c'updateTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr () -> IO ()

updateTextureRec :: Texture -> Rectangle -> Ptr () -> IO Texture
updateTextureRec texture rect pixels = with texture (\t -> with rect (\r -> c'updateTextureRec t r pixels) >> peek t)

foreign import ccall safe "raylib.h &UpdateTextureRec"
  p'updateTextureRec ::
    FunPtr (Texture -> Rectangle -> Ptr () -> IO ())

foreign import ccall safe "raylib.h GenTextureMipmaps"
  c'genTextureMipmaps ::
    Ptr Texture -> IO ()

genTextureMipmaps :: Texture -> IO Texture
genTextureMipmaps texture = with texture (\t -> c'genTextureMipmaps t >> peek t)

foreign import ccall safe "raylib.h &GenTextureMipmaps"
  p'genTextureMipmaps ::
    FunPtr (Ptr Texture -> IO ())

foreign import ccall safe "bindings.h SetTextureFilter_" c'setTextureFilter :: Ptr Texture -> CInt -> IO ()

setTextureFilter :: Texture -> Int -> IO Texture
setTextureFilter texture filterType = with texture (\t -> c'setTextureFilter t (fromIntegral filterType) >> peek t)

foreign import ccall safe "raylib.h &SetTextureFilter"
  p'setTextureFilter ::
    FunPtr (Texture -> CInt -> IO ())

foreign import ccall safe "bindings.h SetTextureWrap_" c'setTextureWrap :: Ptr Texture -> CInt -> IO ()

setTextureWrap :: Texture -> Int -> IO Texture
setTextureWrap texture wrap = with texture (\t -> c'setTextureWrap t (fromIntegral wrap) >> peek t)

foreign import ccall safe "raylib.h &SetTextureWrap"
  p'setTextureWrap ::
    FunPtr (Texture -> CInt -> IO ())

foreign import ccall safe "bindings.h DrawTexture_" c'drawTexture :: Ptr Texture -> CInt -> CInt -> Ptr Color -> IO ()

drawTexture :: Texture -> CInt -> CInt -> Color -> IO ()
drawTexture texture x y tint = with texture (\t -> with tint (c'drawTexture t (fromIntegral x) (fromIntegral y)))

foreign import ccall safe "raylib.h &DrawTexture"
  p'drawTexture ::
    FunPtr (Texture -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureV_" c'drawTextureV :: Ptr Texture -> Ptr Vector2 -> Ptr Color -> IO ()

drawTextureV :: Texture -> Vector2 -> Color -> IO ()
drawTextureV texture position color = with texture (\t -> with position (with color . c'drawTextureV t))

foreign import ccall safe "raylib.h &DrawTextureV"
  p'drawTextureV ::
    FunPtr (Texture -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureEx_" c'drawTextureEx :: Ptr Texture -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

drawTextureEx :: Texture -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextureEx texture position rotation scale tint = with texture (\t -> with position (\p -> with tint (c'drawTextureEx t p (realToFrac rotation) (realToFrac scale))))

foreign import ccall safe "raylib.h &DrawTextureEx"
  p'drawTextureEx ::
    FunPtr (Texture -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureRec_" c'drawTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector2 -> Ptr Color -> IO ()

drawTextureRec :: Texture -> Rectangle -> Vector2 -> Color -> IO ()
drawTextureRec texture source position tint = with texture (\t -> with source (\s -> with position (with tint . c'drawTextureRec t s)))

foreign import ccall safe "raylib.h &DrawTextureRec"
  p'drawTextureRec ::
    FunPtr (Texture -> Rectangle -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureQuad_" c'drawTextureQuad :: Ptr Texture -> Ptr Vector2 -> Ptr Vector2 -> Ptr Rectangle -> Ptr Color -> IO ()

drawTextureQuad :: Texture -> Vector2 -> Vector2 -> Rectangle -> Color -> IO ()
drawTextureQuad texture tiling offset quad tint = with texture (\t -> with tiling (\ti -> with offset (\o -> with quad (with tint . c'drawTextureQuad t ti o))))

foreign import ccall safe "raylib.h &DrawTextureQuad"
  p'drawTextureQuad ::
    FunPtr (Texture -> Vector2 -> Vector2 -> Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureTiled_" c'drawTextureTiled :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

drawTextureTiled :: Texture -> Rectangle -> Rectangle -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextureTiled texture source dest origin rotation scale tint = with texture (\t -> with source (\s -> with dest (\d -> with origin (\o -> with tint (c'drawTextureTiled t s d o (realToFrac rotation) (realToFrac scale))))))

foreign import ccall safe "raylib.h &DrawTextureTiled"
  p'drawTextureTiled ::
    FunPtr (Texture -> Rectangle -> Rectangle -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTexturePro_" c'drawTexturePro :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawTexturePro :: Texture -> Rectangle -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTexturePro texture source dest origin rotation tint = with texture (\t -> with source (\s -> with dest (\d -> with origin (\o -> with tint (c'drawTexturePro t s d o (realToFrac rotation))))))

foreign import ccall safe "raylib.h &DrawTexturePro"
  p'drawTexturePro ::
    FunPtr (Texture -> Rectangle -> Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureNPatch_" c'drawTextureNPatch :: Ptr Texture -> Ptr NPatchInfo -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawTextureNPatch :: Texture -> NPatchInfo -> Rectangle -> Vector2 -> Float -> Color -> IO ()
drawTextureNPatch texture nPatchInfo dest origin rotation tint = with texture (\t -> with nPatchInfo (\n -> with dest (\d -> with origin (\o -> with tint (c'drawTextureNPatch t n d o (realToFrac rotation))))))

foreign import ccall safe "raylib.h &DrawTextureNPatch"
  p'drawTextureNPatch ::
    FunPtr (Texture -> NPatchInfo -> Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTexturePoly_" c'drawTexturePoly :: Ptr Texture -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

drawTexturePoly :: Texture -> Vector2 -> [Vector2] -> [Vector2] -> Color -> IO ()
drawTexturePoly texture center points texcoords tint = with texture (\t -> with center (\c -> withArrayLen points (\numPoints pArr -> withArray texcoords (\tc -> with tint (c'drawTexturePoly t c pArr tc (fromIntegral numPoints))))))

foreign import ccall safe "raylib.h &DrawTexturePoly"
  p'drawTexturePoly ::
    FunPtr (Texture -> Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h Fade_" c'fade :: Ptr Color -> CFloat -> IO (Ptr Color)

fade :: Color -> Float -> Color
fade color alpha = unsafePerformIO $ with color (\c -> c'fade c (realToFrac alpha)) >>= pop

foreign import ccall safe "raylib.h &Fade"
  p'fade ::
    FunPtr (Color -> CFloat -> IO Color)

foreign import ccall safe "bindings.h ColorToInt_" c'colorToInt :: Ptr Color -> IO CInt

colorToInt :: Color -> Int
colorToInt color = unsafePerformIO $ fromIntegral <$> with color c'colorToInt

foreign import ccall safe "raylib.h &ColorToInt"
  p'colorToInt ::
    FunPtr (Color -> IO CInt)

foreign import ccall safe "bindings.h ColorNormalize_" c'colorNormalize :: Ptr Color -> IO (Ptr Vector4)

colorNormalize :: Color -> Vector4
colorNormalize color = unsafePerformIO $ with color c'colorNormalize >>= pop

foreign import ccall safe "raylib.h &ColorNormalize"
  p'colorNormalize ::
    FunPtr (Color -> IO Vector4)

foreign import ccall safe "bindings.h ColorFromNormalized_" c'colorFromNormalized :: Ptr Vector4 -> IO (Ptr Color)

colorFromNormalized :: Vector4 -> Color
colorFromNormalized normalized = unsafePerformIO $ with normalized c'colorFromNormalized >>= pop

foreign import ccall safe "raylib.h &ColorFromNormalized"
  p'colorFromNormalized ::
    FunPtr (Vector4 -> IO Color)

foreign import ccall safe "bindings.h ColorToHSV_" c'colorToHSV :: Ptr Color -> IO (Ptr Vector3)

colorToHSV :: Color -> Vector3
colorToHSV color = unsafePerformIO $ with color c'colorToHSV >>= pop

foreign import ccall safe "raylib.h &ColorToHSV"
  p'colorToHSV ::
    FunPtr (Color -> IO Vector3)

foreign import ccall safe "bindings.h ColorFromHSV_" c'colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Color)

colorFromHSV :: Float -> Float -> Float -> Color
colorFromHSV hue saturation value = unsafePerformIO $ c'colorFromHSV (realToFrac hue) (realToFrac saturation) (realToFrac value) >>= pop

foreign import ccall safe "raylib.h &ColorFromHSV"
  p'colorFromHSV ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Color)

foreign import ccall safe "bindings.h ColorAlpha_" c'colorAlpha :: Ptr Color -> CFloat -> IO (Ptr Color)

colorAlpha :: Color -> Float -> Color
colorAlpha color alpha = unsafePerformIO $ with color (\c -> c'colorAlpha c (realToFrac alpha)) >>= pop

foreign import ccall safe "raylib.h &ColorAlpha"
  p'colorAlpha ::
    FunPtr (Color -> CFloat -> IO Color)

foreign import ccall safe "bindings.h ColorAlphaBlend_" c'colorAlphaBlend :: Ptr Color -> Ptr Color -> Ptr Color -> IO (Ptr Color)

colorAlphaBlend :: Color -> Color -> Color -> Color
colorAlphaBlend dst src tint = unsafePerformIO $ with dst (\d -> with src (with tint . c'colorAlphaBlend d)) >>= pop

foreign import ccall safe "raylib.h &ColorAlphaBlend"
  p'colorAlphaBlend ::
    FunPtr (Color -> Color -> Color -> IO Color)

foreign import ccall safe "bindings.h GetColor_" c'getColor :: CUInt -> IO (Ptr Color)

getColor :: Integer -> Color
getColor hexValue = unsafePerformIO $ c'getColor (fromIntegral hexValue) >>= pop

foreign import ccall safe "raylib.h &GetColor"
  p'getColor ::
    FunPtr (CUInt -> IO Color)

foreign import ccall safe "bindings.h GetPixelColor_" c'getPixelColor :: Ptr () -> CInt -> IO (Ptr Color)

getPixelColor :: Ptr () -> Int -> IO Color
getPixelColor srcPtr format = c'getPixelColor srcPtr (fromIntegral format) >>= pop

foreign import ccall safe "raylib.h &GetPixelColor"
  p'getPixelColor ::
    FunPtr (Ptr () -> CInt -> IO Color)

foreign import ccall safe "bindings.h SetPixelColor_" c'setPixelColor :: Ptr () -> Ptr Color -> CInt -> IO ()

setPixelColor :: Ptr () -> Color -> Int -> IO ()
setPixelColor dstPtr color format = with color (\c -> c'setPixelColor dstPtr c (fromIntegral format))

foreign import ccall safe "raylib.h &SetPixelColor"
  p'setPixelColor ::
    FunPtr (Ptr () -> Color -> CInt -> IO ())

foreign import ccall safe "raylib.h GetPixelDataSize"
  c'getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

getPixelDataSize :: Int -> Int -> Int -> Int
getPixelDataSize width height format = unsafePerformIO (fromIntegral <$> c'getPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral format))

foreign import ccall safe "raylib.h &GetPixelDataSize"
  p'getPixelDataSize ::
    FunPtr (CInt -> CInt -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetFontDefault_" c'getFontDefault :: IO (Ptr Font)

getFontDefault :: IO Font
getFontDefault = c'getFontDefault >>= pop

foreign import ccall safe "raylib.h &GetFontDefault"
  p'getFontDefault ::
    FunPtr (IO Font)

foreign import ccall safe "bindings.h LoadFont_" c'loadFont :: CString -> IO (Ptr Font)

loadFont :: String -> IO Font
loadFont fileName = withCString fileName c'loadFont >>= pop

foreign import ccall safe "raylib.h &LoadFont"
  p'loadFont ::
    FunPtr (CString -> IO Font)

foreign import ccall safe "bindings.h LoadFontEx_" c'loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)

loadFontEx :: String -> Int -> [Int] -> Int -> IO Font
loadFontEx fileName fontSize fontChars glyphCount = withCString fileName (\f -> withArray fontChars (\c -> c'loadFontEx f (fromIntegral fontSize) (castPtr c) (fromIntegral glyphCount))) >>= pop

foreign import ccall safe "raylib.h &LoadFontEx"
  p'loadFontEx ::
    FunPtr (CString -> CInt -> Ptr CInt -> CInt -> IO Font)

foreign import ccall safe "bindings.h LoadFontFromImage_" c'loadFontFromImage :: Ptr Image -> Ptr Color -> CInt -> IO (Ptr Font)

loadFontFromImage :: Image -> Color -> Int -> IO Font
loadFontFromImage image key firstChar = with image (\i -> with key (\k -> c'loadFontFromImage i k (fromIntegral firstChar))) >>= pop

foreign import ccall safe "raylib.h &LoadFontFromImage"
  p'loadFontFromImage ::
    FunPtr (Image -> Color -> CInt -> IO Font)

foreign import ccall safe "bindings.h LoadFontFromMemory_" c'loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)

loadFontFromMemory :: String -> [Int] -> Int -> [Int] -> Int -> IO Font
loadFontFromMemory fileType fileData fontSize fontChars glyphCount = withCString fileType (\t -> withArrayLen fileData (\size d -> withArray fontChars (\c -> c'loadFontFromMemory t (castPtr d) (fromIntegral size) (fromIntegral fontSize) (castPtr c) (fromIntegral glyphCount)))) >>= pop

foreign import ccall safe "raylib.h &LoadFontFromMemory"
  p'loadFontFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO Font)

foreign import ccall safe "raylib.h LoadFontData"
  c'loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)

loadFontData :: [Int] -> Int -> [Int] -> Int -> Int -> IO GlyphInfo
loadFontData fileData fontSize fontChars glyphCount fontType = withArrayLen fileData (\size d -> withArray fontChars (\c -> c'loadFontData (castPtr d) (fromIntegral size) (fromIntegral fontSize) (castPtr c) (fromIntegral glyphCount) (fromIntegral fontType))) >>= pop

foreign import ccall safe "raylib.h &LoadFontData"
  p'loadFontData ::
    FunPtr (Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo))

foreign import ccall safe "bindings.h GenImageFontAtlas_" c'genImageFontAtlas :: Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

genImageFontAtlas :: [GlyphInfo] -> [[Rectangle]] -> Int -> Int -> Int -> Int -> IO Image
genImageFontAtlas chars recs glyphCount fontSize padding packMethod = withArray chars (\c -> withArray2D recs (\r -> c'genImageFontAtlas c r (fromIntegral glyphCount) (fromIntegral fontSize) (fromIntegral padding) (fromIntegral packMethod))) >>= pop

foreign import ccall safe "raylib.h &GenImageFontAtlas"
  p'genImageFontAtlas ::
    FunPtr (Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO Image)

foreign import ccall safe "raylib.h UnloadFontData"
  c'unloadFontData ::
    Ptr GlyphInfo -> CInt -> IO ()

unloadFontData :: [GlyphInfo] -> IO ()
unloadFontData glyphs = withArrayLen glyphs (\size g -> c'unloadFontData g (fromIntegral size))

foreign import ccall safe "raylib.h &UnloadFontData"
  p'unloadFontData ::
    FunPtr (Ptr GlyphInfo -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadFont_" c'unloadFont :: Ptr Font -> IO ()

unloadFont :: Font -> IO ()
unloadFont font = with font c'unloadFont

foreign import ccall safe "raylib.h &UnloadFont"
  p'unloadFont ::
    FunPtr (Font -> IO ())

foreign import ccall safe "bindings.h ExportFontAsCode_" c'exportFontAsCode :: Ptr Font -> CString -> IO CInt

exportFontAsCode :: Font -> String -> IO Bool
exportFontAsCode font fileName = toBool <$> with font (withCString fileName . c'exportFontAsCode)

foreign import ccall safe "raylib.h &ExportFontAsCode"
  p'exportFontAsCode ::
    FunPtr (Font -> CString -> IO CInt)

foreign import ccall safe "raylib.h DrawFPS"
  c'drawFPS ::
    CInt -> CInt -> IO ()

drawFPS :: Int -> Int -> IO ()
drawFPS x y = c'drawFPS (fromIntegral x) (fromIntegral y)

foreign import ccall safe "raylib.h &DrawFPS"
  p'drawFPS ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h DrawText_" c'drawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

drawText :: String -> Int -> Int -> Int -> Color -> IO ()
drawText text x y fontSize color = withCString text (\t -> with color (c'drawText t (fromIntegral x) (fromIntegral y) (fromIntegral fontSize)))

foreign import ccall safe "raylib.h &DrawText"
  p'drawText ::
    FunPtr (CString -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextEx_" c'drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

drawTextEx :: Font -> String -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextEx font text position fontSize spacing tint = with font (\f -> withCString text (\t -> with position (\p -> with tint (c'drawTextEx f t p (realToFrac fontSize) (realToFrac spacing)))))

foreign import ccall safe "raylib.h &DrawTextEx"
  p'drawTextEx ::
    FunPtr (Font -> CString -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextPro_" c'drawTextPro :: Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

drawTextPro :: Font -> String -> Vector2 -> Vector2 -> Float -> Float -> Float -> Color -> IO ()
drawTextPro font text position origin rotation fontSize spacing tint = with font (\f -> withCString text (\t -> with position (\p -> with origin (\o -> with tint (c'drawTextPro f t p o (realToFrac rotation) (realToFrac fontSize) (realToFrac spacing))))))

foreign import ccall safe "raylib.h &DrawTextPro"
  p'drawTextPro ::
    FunPtr (Font -> CString -> Vector2 -> Vector2 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextCodepoint_" c'drawTextCodepoint :: Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawTextCodepoint :: Font -> Int -> Vector2 -> Float -> Color -> IO ()
drawTextCodepoint font codepoint position fontSize tint = with font (\f -> with position (\p -> with tint (c'drawTextCodepoint f (fromIntegral codepoint) p (realToFrac fontSize))))

foreign import ccall safe "raylib.h &DrawTextCodepoint"
  p'drawTextCodepoint ::
    FunPtr (Font -> CInt -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextCodepoints_" c'drawTextCodepoints :: Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

drawTextCodepoints :: Font -> [Int] -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextCodepoints font codepoints position fontSize spacing tint = with font (\f -> withArrayLen codepoints (\count cp -> with position (\p -> with tint (c'drawTextCodepoints f (castPtr cp) (fromIntegral count) p (realToFrac fontSize) (realToFrac spacing)))))

foreign import ccall safe "raylib.h &DrawTextCodepoints"
  p'drawTextCodepoints ::
    FunPtr (Font -> Ptr CInt -> CInt -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "raylib.h MeasureText"
  c'measureText ::
    CString -> CInt -> IO CInt

measureText :: String -> Int -> IO Int
measureText text fontSize = fromIntegral <$> withCString text (\t -> c'measureText t (fromIntegral fontSize))

foreign import ccall safe "raylib.h &MeasureText"
  p'measureText ::
    FunPtr (CString -> CInt -> IO CInt)

foreign import ccall safe "bindings.h MeasureTextEx_" c'measureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)

measureTextEx :: Font -> String -> Float -> Float -> IO Vector2
measureTextEx font text fontSize spacing = with font (\f -> withCString text (\t -> c'measureTextEx f t (realToFrac fontSize) (realToFrac spacing))) >>= pop

foreign import ccall safe "raylib.h &MeasureTextEx"
  p'measureTextEx ::
    FunPtr (Font -> CString -> CFloat -> CFloat -> IO Vector2)

foreign import ccall safe "bindings.h GetGlyphIndex_" c'getGlyphIndex :: Ptr Font -> CInt -> IO CInt

getGlyphIndex :: Font -> Int -> IO Int
getGlyphIndex font codepoint = fromIntegral <$> with font (\f -> c'getGlyphIndex f (fromIntegral codepoint))

foreign import ccall safe "raylib.h &GetGlyphIndex"
  p'getGlyphIndex ::
    FunPtr (Font -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetGlyphInfo_" c'getGlyphInfo :: Ptr Font -> CInt -> IO (Ptr GlyphInfo)

getGlyphInfo :: Font -> Int -> IO GlyphInfo
getGlyphInfo font codepoint = with font (\f -> c'getGlyphInfo f (fromIntegral codepoint)) >>= pop

foreign import ccall safe "raylib.h &GetGlyphInfo"
  p'getGlyphInfo ::
    FunPtr (Font -> CInt -> IO GlyphInfo)

foreign import ccall safe "bindings.h GetGlyphAtlasRec_" c'getGlyphAtlasRec :: Ptr Font -> CInt -> IO (Ptr Rectangle)

getGlyphAtlasRec :: Font -> Int -> IO Rectangle
getGlyphAtlasRec font codepoint = with font (\f -> c'getGlyphAtlasRec f (fromIntegral codepoint)) >>= pop

foreign import ccall safe "raylib.h &GetGlyphAtlasRec"
  p'getGlyphAtlasRec ::
    FunPtr (Font -> CInt -> IO Rectangle)

foreign import ccall safe "raylib.h LoadUTF8"
  c'loadUTF8 ::
    Ptr CInt -> CInt -> IO CString

loadUTF8 :: [Int] -> IO String
loadUTF8 codepoints = withArrayLen codepoints (\size c -> c'loadUTF8 (castPtr c) (fromIntegral size)) >>= (\s -> do val <- peekCString s; unloadUTF8 s; return val)

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
getCodepointNext text = withCString text (\t -> with 0 (\n -> do res <- c'getCodepointNext t n; num <- peek n; return (fromIntegral res, fromIntegral num)))

foreign import ccall safe "raylib.h &GetCodepointNext"
  p'getCodepointNext ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall safe "raylib.h GetCodepointPrevious"
  c'getCodepointPrevious ::
    CString -> Ptr CInt -> IO CInt

getCodepointPrevious :: String -> IO (Int, Int)
getCodepointPrevious text = withCString text(\t -> with 0 (\n -> do res <- c'getCodepointPrevious t n; num <- peek n; return (fromIntegral res, fromIntegral num)))

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

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextCopy"
  textCopy ::
    CString -> CString -> IO CInt

foreign import ccall safe "raylib.h &TextCopy"
  p'textCopy ::
    FunPtr (CString -> CString -> IO CInt)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextIsEqual"
  textIsEqual ::
    CString -> CString -> IO CInt

foreign import ccall safe "raylib.h &TextIsEqual"
  p'textIsEqual ::
    FunPtr (CString -> CString -> IO CInt)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextLength"
  textLength ::
    CString -> IO CUInt

foreign import ccall safe "raylib.h &TextLength"
  p'textLength ::
    FunPtr (CString -> IO CUInt)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextFormat"
  textFormat ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextFormat"
  p'textFormat ::
    FunPtr (CString -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextSubtext"
  textSubtext ::
    CString -> CInt -> CInt -> IO CString

foreign import ccall safe "raylib.h &TextSubtext"
  p'textSubtext ::
    FunPtr (CString -> CInt -> CInt -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextReplace"
  textReplace ::
    CString -> CString -> CString -> IO CString

foreign import ccall safe "raylib.h &TextReplace"
  p'textReplace ::
    FunPtr (CString -> CString -> CString -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextInsert"
  textInsert ::
    CString -> CString -> CInt -> IO CString

foreign import ccall safe "raylib.h &TextInsert"
  p'textInsert ::
    FunPtr (CString -> CString -> CInt -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextJoin"
  textJoin ::
    Ptr CString -> CInt -> CString -> IO CString

foreign import ccall safe "raylib.h &TextJoin"
  p'textJoin ::
    FunPtr (Ptr CString -> CInt -> CString -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextSplit"
  textSplit ::
    CString -> CChar -> Ptr CInt -> IO (Ptr CString)

foreign import ccall safe "raylib.h &TextSplit"
  p'textSplit ::
    FunPtr (CString -> CChar -> Ptr CInt -> IO (Ptr CString))

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextAppend"
  textAppend ::
    CString -> CString -> Ptr CInt -> IO ()

foreign import ccall safe "raylib.h &TextAppend"
  p'textAppend ::
    FunPtr (CString -> CString -> Ptr CInt -> IO ())

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextFindIndex"
  textFindIndex ::
    CString -> CString -> IO CInt

foreign import ccall safe "raylib.h &TextFindIndex"
  p'textFindIndex ::
    FunPtr (CString -> CString -> IO CInt)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextToUpper"
  textToUpper ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextToUpper"
  p'textToUpper ::
    FunPtr (CString -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextToLower"
  textToLower ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextToLower"
  p'textToLower ::
    FunPtr (CString -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextToPascal"
  textToPascal ::
    CString -> IO CString

foreign import ccall safe "raylib.h &TextToPascal"
  p'textToPascal ::
    FunPtr (CString -> IO CString)

{-| Not required in Haskell -}
foreign import ccall safe "raylib.h TextToInteger"
  textToInteger ::
    CString -> IO CInt

foreign import ccall safe "raylib.h &TextToInteger"
  p'textToInteger ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "bindings.h DrawLine3D_" c'drawLine3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

drawLine3D :: Vector3 -> Vector3 -> Color -> IO ()
drawLine3D start end color = with start (\s -> with end (with color . c'drawLine3D s))

foreign import ccall safe "raylib.h &DrawLine3D"
  p'drawLine3D ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPoint3D_" c'drawPoint3D :: Ptr Vector3 -> Ptr Color -> IO ()

drawPoint3D :: Vector3 -> Color -> IO ()
drawPoint3D point color = with point (with color . c'drawPoint3D)

foreign import ccall safe "raylib.h &DrawPoint3D"
  p'drawPoint3D ::
    FunPtr (Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircle3D_" c'drawCircle3D :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

drawCircle3D :: Vector3 -> Float -> Vector3 -> Float -> Color -> IO ()
drawCircle3D center radius rotationAxis rotationAngle color = with center (\c -> with rotationAxis (\r -> with color (c'drawCircle3D c (realToFrac radius) r (realToFrac rotationAngle))))

foreign import ccall safe "raylib.h &DrawCircle3D"
  p'drawCircle3D ::
    FunPtr (Vector3 -> CFloat -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangle3D_" c'drawTriangle3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

drawTriangle3D :: Vector3 -> Vector3 -> Vector3 -> Color -> IO ()
drawTriangle3D v1 v2 v3 color = with v1 (\p1 -> with v2 (\p2 -> with v3 (with color . c'drawTriangle3D p1 p2)))

foreign import ccall safe "raylib.h &DrawTriangle3D"
  p'drawTriangle3D ::
    FunPtr (Vector3 -> Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleStrip3D_" c'drawTriangleStrip3D :: Ptr Vector3 -> CInt -> Ptr Color -> IO ()

drawTriangleStrip3D :: [Vector3] -> Int -> Color -> IO ()
drawTriangleStrip3D points pointCount color = withArray points (\p -> with color (c'drawTriangleStrip3D p (fromIntegral pointCount)))

foreign import ccall safe "raylib.h &DrawTriangleStrip3D"
  p'drawTriangleStrip3D ::
    FunPtr (Ptr Vector3 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCube_" c'drawCube :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

drawCube :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCube position width height length color = with position (\p -> with color (c'drawCube p (realToFrac width) (realToFrac height) (realToFrac length)))

foreign import ccall safe "raylib.h &DrawCube"
  p'drawCube ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeV_" c'drawCubeV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

drawCubeV :: Vector3 -> Vector3 -> Color -> IO ()
drawCubeV position size color = with position (\p -> with size (with color . c'drawCubeV p))

foreign import ccall safe "raylib.h &DrawCubeV"
  p'drawCubeV ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeWires_" c'drawCubeWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

drawCubeWires :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeWires position width height length color = with position (\p -> with color (c'drawCubeWires p (realToFrac width) (realToFrac height) (realToFrac length)))

foreign import ccall safe "raylib.h &DrawCubeWires"
  p'drawCubeWires ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeWiresV_" c'drawCubeWiresV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

drawCubeWiresV :: Vector3 -> Vector3 -> Color -> IO ()
drawCubeWiresV position size color = with position (\p -> with size (with color . c'drawCubeWiresV p))

foreign import ccall safe "raylib.h &DrawCubeWiresV"
  p'drawCubeWiresV ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeTexture_" c'drawCubeTexture :: Ptr Texture -> Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

drawCubeTexture :: Texture -> Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeTexture texture position width height length color = with texture (\t -> with position (\p -> with color (c'drawCubeTexture t p (realToFrac width) (realToFrac height) (realToFrac length))))

foreign import ccall safe "raylib.h &DrawCubeTexture"
  p'drawCubeTexture ::
    FunPtr (Texture -> Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeTextureRec_" c'drawCubeTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

drawCubeTextureRec :: Texture -> Rectangle -> Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeTextureRec texture source position width height length color = with texture (\t -> with source (\s -> with position (\p -> with color (c'drawCubeTextureRec t s p (realToFrac width) (realToFrac height) (realToFrac length)))))

foreign import ccall safe "raylib.h &DrawCubeTextureRec"
  p'drawCubeTextureRec ::
    FunPtr (Texture -> Rectangle -> Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawSphere_" c'drawSphere :: Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

drawSphere :: Vector3 -> Float -> Color -> IO ()
drawSphere position radius color = with position (\p -> with color (c'drawSphere p (realToFrac radius)))

foreign import ccall safe "raylib.h &DrawSphere"
  p'drawSphere ::
    FunPtr (Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawSphereEx_" c'drawSphereEx :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

drawSphereEx :: Vector3 -> Float -> Int -> Int -> Color -> IO ()
drawSphereEx position radius rings slices color = with position (\p -> with color (c'drawSphereEx p (realToFrac radius) (fromIntegral rings) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawSphereEx"
  p'drawSphereEx ::
    FunPtr (Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawSphereWires_" c'drawSphereWires :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

drawSphereWires :: Vector3 -> Float -> Int -> Int -> Color -> IO ()
drawSphereWires position radius rings slices color = with position (\p -> with color (c'drawSphereWires p (realToFrac radius) (fromIntegral rings) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawSphereWires"
  p'drawSphereWires ::
    FunPtr (Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinder_" c'drawCylinder :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawCylinder :: Vector3 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCylinder position radiusTop radiusBottom height slices color = with position (\p -> with color (c'drawCylinder p (realToFrac radiusTop) (realToFrac radiusBottom) (realToFrac height) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawCylinder"
  p'drawCylinder ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderEx_" c'drawCylinderEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawCylinderEx :: Vector3 -> Vector3 -> Float -> Float -> Int -> Color -> IO ()
drawCylinderEx start end startRadius endRadius sides color = with start (\s -> with end (\e -> with color (c'drawCylinderEx s e (realToFrac startRadius) (realToFrac endRadius) (fromIntegral sides))))

foreign import ccall safe "raylib.h &DrawCylinderEx"
  p'drawCylinderEx ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderWires_" c'drawCylinderWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawCylinderWires :: Vector3 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCylinderWires position radiusTop radiusBottom height slices color = with position (\p -> with color (c'drawCylinderWires p (realToFrac radiusTop) (realToFrac radiusBottom) (realToFrac height) (fromIntegral slices)))

foreign import ccall safe "raylib.h &DrawCylinderWires"
  p'drawCylinderWires ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderWiresEx_" c'drawCylinderWiresEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

drawCylinderWiresEx :: Vector3 -> Vector3 -> Float -> Float -> Int -> Color -> IO ()
drawCylinderWiresEx start end startRadius endRadius sides color = with start (\s -> with end (\e -> with color (c'drawCylinderWiresEx s e (realToFrac startRadius) (realToFrac endRadius) (fromIntegral sides))))

foreign import ccall safe "raylib.h &DrawCylinderWiresEx"
  p'drawCylinderWiresEx ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPlane_" c'drawPlane :: Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()

drawPlane :: Vector3 -> Vector2 -> Color -> IO ()
drawPlane center size color = with center (\c -> with size (with color . c'drawPlane c))

foreign import ccall safe "raylib.h &DrawPlane"
  p'drawPlane ::
    FunPtr (Vector3 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRay_" c'drawRay :: Ptr Ray -> Ptr Color -> IO ()

drawRay :: Ray -> Color -> IO ()
drawRay ray color = with ray (with color . c'drawRay)

foreign import ccall safe "raylib.h &DrawRay"
  p'drawRay ::
    FunPtr (Ray -> Color -> IO ())

foreign import ccall safe "raylib.h DrawGrid"
  c'drawGrid ::
    CInt -> CFloat -> IO ()

drawGrid :: Int -> Float -> IO ()
drawGrid slices spacing = c'drawGrid (fromIntegral slices) (realToFrac spacing)

foreign import ccall safe "raylib.h &DrawGrid"
  p'drawGrid ::
    FunPtr (CInt -> CFloat -> IO ())

foreign import ccall safe "bindings.h LoadModel_" c'loadModel :: CString -> IO (Ptr Model)

loadModel :: String -> IO Model
loadModel fileName = withCString fileName c'loadModel >>= pop

foreign import ccall safe "raylib.h &LoadModel"
  p'loadModel ::
    FunPtr (CString -> IO Model)

foreign import ccall safe "bindings.h LoadModelFromMesh_" c'loadModelFromMesh :: Ptr Mesh -> IO (Ptr Model)

loadModelFromMesh :: Mesh -> IO Model
loadModelFromMesh mesh = with mesh c'loadModelFromMesh >>= pop

foreign import ccall safe "raylib.h &LoadModelFromMesh"
  p'loadModelFromMesh ::
    FunPtr (Mesh -> IO Model)

foreign import ccall safe "bindings.h UnloadModel_" c'unloadModel :: Ptr Model -> IO ()

unloadModel :: Model -> IO ()
unloadModel model = with model c'unloadModel

foreign import ccall safe "raylib.h &UnloadModel"
  p'unloadModel ::
    FunPtr (Model -> IO ())

foreign import ccall safe "bindings.h UnloadModelKeepMeshes_" c'unloadModelKeepMeshes :: Ptr Model -> IO ()

unloadModelKeepMeshes :: Model -> IO ()
unloadModelKeepMeshes model = with model c'unloadModelKeepMeshes

foreign import ccall safe "raylib.h &UnloadModelKeepMeshes"
  p'unloadModelKeepMeshes ::
    FunPtr (Model -> IO ())

foreign import ccall safe "bindings.h GetModelBoundingBox_" c'getModelBoundingBox :: Ptr Model -> IO (Ptr BoundingBox)

getModelBoundingBox :: Model -> IO BoundingBox
getModelBoundingBox model = with model c'getModelBoundingBox >>= pop

foreign import ccall safe "raylib.h &GetModelBoundingBox"
  p'getModelBoundingBox ::
    FunPtr (Model -> IO BoundingBox)

foreign import ccall safe "bindings.h DrawModel_" c'drawModel :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

drawModel :: Model -> Vector3 -> Float -> Color -> IO ()
drawModel model position scale tint = with model (\m -> with position (\p -> with tint (c'drawModel m p (realToFrac scale))))

foreign import ccall safe "raylib.h &DrawModel"
  p'drawModel ::
    FunPtr (Model -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawModelEx_" c'drawModelEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()

drawModelEx :: Model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelEx model position rotationAxis rotationAngle scale tint = with model (\m -> with position (\p -> with rotationAxis (\r -> with scale (with tint . c'drawModelEx m p r (realToFrac rotationAngle)))))

foreign import ccall safe "raylib.h &DrawModelEx"
  p'drawModelEx ::
    FunPtr (Model -> Vector3 -> Vector3 -> CFloat -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawModelWires_" c'drawModelWires :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

drawModelWires :: Model -> Vector3 -> Float -> Color -> IO ()
drawModelWires model position scale tint = with model (\m -> with position (\p -> with tint (c'drawModelWires m p (realToFrac scale))))

foreign import ccall safe "raylib.h &DrawModelWires"
  p'drawModelWires ::
    FunPtr (Model -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawModelWiresEx_" c'drawModelWiresEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()

drawModelWiresEx :: Model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelWiresEx model position rotationAxis rotationAngle scale tint = with model (\m -> with position (\p -> with rotationAxis (\r -> with scale (with tint . c'drawModelWiresEx m p r (realToFrac rotationAngle)))))

foreign import ccall safe "raylib.h &DrawModelWiresEx"
  p'drawModelWiresEx ::
    FunPtr (Model -> Vector3 -> Vector3 -> CFloat -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBoundingBox_" c'drawBoundingBox :: Ptr BoundingBox -> Ptr Color -> IO ()

drawBoundingBox :: BoundingBox -> Color -> IO ()
drawBoundingBox box color = with box (with color . c'drawBoundingBox)

foreign import ccall safe "raylib.h &DrawBoundingBox"
  p'drawBoundingBox ::
    FunPtr (BoundingBox -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboard_" c'drawBillboard :: Ptr Camera3D -> Ptr Texture -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

drawBillboard :: Camera3D -> Texture -> Vector3 -> Float -> Color -> IO ()
drawBillboard camera texture position size tint = with camera (\c -> with texture (\t -> with position (\p -> with tint (c'drawBillboard c t p (realToFrac size)))))

foreign import ccall safe "raylib.h &DrawBillboard"
  p'drawBillboard ::
    FunPtr (Camera3D -> Texture -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboardRec_" c'drawBillboardRec :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()

drawBillboardRec :: Camera3D -> Texture -> Rectangle -> Vector3 -> Vector2 -> Color -> IO ()
drawBillboardRec camera texture source position size tint = with camera (\c -> with texture (\t -> with source (\s -> with position (\p -> with size (with tint . c'drawBillboardRec c t s p)))))

foreign import ccall safe "raylib.h &DrawBillboardRec"
  p'drawBillboardRec ::
    FunPtr (Camera3D -> Texture -> Rectangle -> Vector3 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboardPro_" c'drawBillboardPro :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

drawBillboardPro :: Camera3D -> Texture -> Rectangle -> Vector3 -> Vector3 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawBillboardPro camera texture source position up size origin rotation tint = with camera (\c -> with texture (\t -> with source (\s -> with position (\p -> with up (\u -> with size (\sz -> with origin (\o -> with tint (c'drawBillboardPro c t s p u sz o (realToFrac rotation)))))))))

foreign import ccall safe "raylib.h &DrawBillboardPro"
  p'drawBillboardPro ::
    FunPtr (Camera3D -> Texture -> Rectangle -> Vector3 -> Vector3 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "raylib.h UploadMesh"
  c'uploadMesh ::
    Ptr Mesh -> CInt -> IO ()

uploadMesh :: Mesh -> Bool -> IO Mesh
uploadMesh mesh dynamic = with mesh (\m -> c'uploadMesh m (fromBool dynamic) >> peek m)

foreign import ccall safe "raylib.h &UploadMesh"
  p'uploadMesh ::
    FunPtr (Ptr Mesh -> CInt -> IO ())

foreign import ccall safe "bindings.h UpdateMeshBuffer_" c'updateMeshBuffer :: Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()

updateMeshBuffer :: Mesh -> Int -> Ptr () -> Int -> Int -> IO ()
updateMeshBuffer mesh index dataValue dataSize offset = with mesh (\m -> c'updateMeshBuffer m (fromIntegral index) dataValue (fromIntegral dataSize) (fromIntegral offset))

foreign import ccall safe "raylib.h &UpdateMeshBuffer"
  p'updateMeshBuffer ::
    FunPtr (Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadMesh_" c'unloadMesh :: Ptr Mesh -> IO ()

unloadMesh :: Mesh -> IO ()
unloadMesh mesh = with mesh c'unloadMesh

foreign import ccall safe "raylib.h &UnloadMesh"
  p'unloadMesh ::
    FunPtr (Mesh -> IO ())

foreign import ccall safe "bindings.h DrawMesh_" c'drawMesh :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()

drawMesh :: Mesh -> Material -> Matrix -> IO ()
drawMesh mesh material transform = with mesh (\m -> with material (with transform . c'drawMesh m))

foreign import ccall safe "raylib.h &DrawMesh"
  p'drawMesh ::
    FunPtr (Mesh -> Material -> Matrix -> IO ())

foreign import ccall safe "bindings.h DrawMeshInstanced_" drawMeshInstanced :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()

foreign import ccall safe "raylib.h &DrawMeshInstanced"
  p'drawMeshInstanced ::
    FunPtr (Mesh -> Material -> Ptr Matrix -> CInt -> IO ())

foreign import ccall safe "bindings.h ExportMesh_" exportMesh :: Ptr Mesh -> CString -> IO CInt

foreign import ccall safe "raylib.h &ExportMesh"
  p'exportMesh ::
    FunPtr (Mesh -> CString -> IO CInt)

foreign import ccall safe "bindings.h GetMeshBoundingBox_" getMeshBoundingBox :: Ptr Mesh -> IO (Ptr BoundingBox)

foreign import ccall safe "raylib.h &GetMeshBoundingBox"
  p'getMeshBoundingBox ::
    FunPtr (Mesh -> IO BoundingBox)

foreign import ccall safe "raylib.h GenMeshTangents"
  genMeshTangents ::
    Ptr Mesh -> IO ()

foreign import ccall safe "raylib.h &GenMeshTangents"
  p'genMeshTangents ::
    FunPtr (Ptr Mesh -> IO ())

foreign import ccall safe "bindings.h GenMeshPoly_" genMeshPoly :: CInt -> CFloat -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshPoly"
  p'genMeshPoly ::
    FunPtr (CInt -> CFloat -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshPlane_" genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshPlane"
  p'genMeshPlane ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCube_" genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshCube"
  p'genMeshCube ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshSphere_" genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshSphere"
  p'genMeshSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshHemiSphere_" genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshHemiSphere"
  p'genMeshHemiSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCylinder_" genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshCylinder"
  p'genMeshCylinder ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCone_" genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshCone"
  p'genMeshCone ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshTorus_" genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshTorus"
  p'genMeshTorus ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshKnot_" genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshKnot"
  p'genMeshKnot ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshHeightmap_" genMeshHeightmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshHeightmap"
  p'genMeshHeightmap ::
    FunPtr (Image -> Vector3 -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCubicmap_" genMeshCubicmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h &GenMeshCubicmap"
  p'genMeshCubicmap ::
    FunPtr (Image -> Vector3 -> IO Mesh)

foreign import ccall safe "raylib.h LoadMaterials"
  loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Material)

foreign import ccall safe "raylib.h &LoadMaterials"
  p'loadMaterials ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr Material))

foreign import ccall safe "bindings.h LoadMaterialDefault_" loadMaterialDefault :: IO (Ptr Material)

foreign import ccall safe "raylib.h &LoadMaterialDefault"
  p'loadMaterialDefault ::
    FunPtr (IO Material)

foreign import ccall safe "bindings.h UnloadMaterial_" unloadMaterial :: Ptr Material -> IO ()

foreign import ccall safe "raylib.h &UnloadMaterial"
  p'unloadMaterial ::
    FunPtr (Material -> IO ())

foreign import ccall safe "bindings.h SetMaterialTexture_" setMaterialTexture :: Ptr Material -> CInt -> Ptr Texture -> IO ()

foreign import ccall safe "raylib.h &SetMaterialTexture"
  p'setMaterialTexture ::
    FunPtr (Ptr Material -> CInt -> Texture -> IO ())

foreign import ccall safe "raylib.h SetModelMeshMaterial"
  setModelMeshMaterial ::
    Ptr Model -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h &SetModelMeshMaterial"
  p'setModelMeshMaterial ::
    FunPtr (Ptr Model -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h LoadModelAnimations"
  loadModelAnimations ::
    CString -> Ptr CUInt -> IO (Ptr ModelAnimation)

foreign import ccall safe "raylib.h &LoadModelAnimations"
  p'loadModelAnimations ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr ModelAnimation))

foreign import ccall safe "bindings.h UpdateModelAnimation_" updateModelAnimation :: Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()

foreign import ccall safe "raylib.h &UpdateModelAnimation"
  p'updateModelAnimation ::
    FunPtr (Model -> ModelAnimation -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadModelAnimation_" unloadModelAnimation :: Ptr ModelAnimation -> IO ()

foreign import ccall safe "raylib.h &UnloadModelAnimation"
  p'unloadModelAnimation ::
    FunPtr (ModelAnimation -> IO ())

foreign import ccall safe "raylib.h UnloadModelAnimations"
  unloadModelAnimations ::
    Ptr ModelAnimation -> CUInt -> IO ()

foreign import ccall safe "raylib.h &UnloadModelAnimations"
  p'unloadModelAnimations ::
    FunPtr (Ptr ModelAnimation -> CUInt -> IO ())

foreign import ccall safe "bindings.h IsModelAnimationValid_" isModelAnimationValid :: Ptr Model -> Ptr ModelAnimation -> IO CInt

foreign import ccall safe "raylib.h &IsModelAnimationValid"
  p'isModelAnimationValid ::
    FunPtr (Model -> ModelAnimation -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionSpheres_" checkCollisionSpheres :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> IO CInt

foreign import ccall safe "raylib.h &CheckCollisionSpheres"
  p'checkCollisionSpheres ::
    FunPtr (Vector3 -> CFloat -> Vector3 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionBoxes_" checkCollisionBoxes :: Ptr BoundingBox -> Ptr BoundingBox -> IO CInt

foreign import ccall safe "raylib.h &CheckCollisionBoxes"
  p'checkCollisionBoxes ::
    FunPtr (BoundingBox -> BoundingBox -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionBoxSphere_" checkCollisionBoxSphere :: Ptr BoundingBox -> Ptr Vector3 -> CFloat -> IO CInt

foreign import ccall safe "raylib.h &CheckCollisionBoxSphere"
  p'checkCollisionBoxSphere ::
    FunPtr (BoundingBox -> Vector3 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h GetRayCollisionSphere_" getRayCollisionSphere :: Ptr Ray -> Ptr Vector3 -> CFloat -> IO (Ptr RayCollision)

foreign import ccall safe "raylib.h &GetRayCollisionSphere"
  p'getRayCollisionSphere ::
    FunPtr (Ray -> Vector3 -> CFloat -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionBox_" getRayCollisionBox :: Ptr Ray -> Ptr BoundingBox -> IO (Ptr RayCollision)

foreign import ccall safe "raylib.h &GetRayCollisionBox"
  p'getRayCollisionBox ::
    FunPtr (Ray -> BoundingBox -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionMesh_" getRayCollisionMesh :: Ptr Ray -> Ptr Mesh -> Ptr Matrix -> IO (Ptr RayCollision)

foreign import ccall safe "raylib.h &GetRayCollisionMesh"
  p'getRayCollisionMesh ::
    FunPtr (Ray -> Mesh -> Matrix -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionTriangle_" getRayCollisionTriangle :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)

foreign import ccall safe "raylib.h &GetRayCollisionTriangle"
  p'getRayCollisionTriangle ::
    FunPtr (Ray -> Vector3 -> Vector3 -> Vector3 -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionQuad_" getRayCollisionQuad :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)

foreign import ccall safe "raylib.h &GetRayCollisionQuad"
  p'getRayCollisionQuad ::
    FunPtr (Ray -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> IO RayCollision)

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
  isAudioDeviceReady ::
    IO CInt

foreign import ccall safe "raylib.h &IsAudioDeviceReady"
  p'isAudioDeviceReady ::
    FunPtr (IO CInt)

foreign import ccall safe "raylib.h SetMasterVolume"
  setMasterVolume ::
    CFloat -> IO ()

foreign import ccall safe "raylib.h &SetMasterVolume"
  p'setMasterVolume ::
    FunPtr (CFloat -> IO ())

foreign import ccall safe "bindings.h LoadWave_" loadWave :: CString -> IO (Ptr Wave)

foreign import ccall safe "raylib.h &LoadWave"
  p'loadWave ::
    FunPtr (CString -> IO Wave)

foreign import ccall safe "bindings.h LoadWaveFromMemory_" loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Wave)

foreign import ccall safe "raylib.h &LoadWaveFromMemory"
  p'loadWaveFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Wave)

foreign import ccall safe "bindings.h LoadSound_" loadSound :: CString -> IO (Ptr Sound)

foreign import ccall safe "raylib.h &LoadSound"
  p'loadSound ::
    FunPtr (CString -> IO Sound)

foreign import ccall safe "bindings.h LoadSoundFromWave_" loadSoundFromWave :: Ptr Wave -> IO (Ptr Sound)

foreign import ccall safe "raylib.h &LoadSoundFromWave"
  p'loadSoundFromWave ::
    FunPtr (Wave -> IO Sound)

foreign import ccall safe "bindings.h UpdateSound_" updateSound :: Ptr Sound -> Ptr () -> CInt -> IO ()

foreign import ccall safe "raylib.h &UpdateSound"
  p'updateSound ::
    FunPtr (Sound -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadWave_" unloadWave :: Ptr Wave -> IO ()

foreign import ccall safe "raylib.h &UnloadWave"
  p'unloadWave ::
    FunPtr (Wave -> IO ())

foreign import ccall safe "bindings.h UnloadSound_" unloadSound :: Ptr Sound -> IO ()

foreign import ccall safe "raylib.h &UnloadSound"
  p'unloadSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h ExportWave_" exportWave :: Ptr Wave -> CString -> IO CInt

foreign import ccall safe "raylib.h &ExportWave"
  p'exportWave ::
    FunPtr (Wave -> CString -> IO CInt)

foreign import ccall safe "bindings.h ExportWaveAsCode_" exportWaveAsCode :: Ptr Wave -> CString -> IO CInt

foreign import ccall safe "raylib.h &ExportWaveAsCode"
  p'exportWaveAsCode ::
    FunPtr (Wave -> CString -> IO CInt)

foreign import ccall safe "bindings.h PlaySound_" playSound :: Ptr Sound -> IO ()

foreign import ccall safe "raylib.h &PlaySound"
  p'playSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h StopSound_" stopSound :: Ptr Sound -> IO ()

foreign import ccall safe "raylib.h &StopSound"
  p'stopSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h PauseSound_" pauseSound :: Ptr Sound -> IO ()

foreign import ccall safe "raylib.h &PauseSound"
  p'pauseSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h ResumeSound_" resumeSound :: Ptr Sound -> IO ()

foreign import ccall safe "raylib.h &ResumeSound"
  p'resumeSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h PlaySoundMulti_" playSoundMulti :: Ptr Sound -> IO ()

foreign import ccall safe "raylib.h &PlaySoundMulti"
  p'playSoundMulti ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "raylib.h StopSoundMulti"
  stopSoundMulti ::
    IO ()

foreign import ccall safe "raylib.h &StopSoundMulti"
  p'stopSoundMulti ::
    FunPtr (IO ())

foreign import ccall safe "raylib.h GetSoundsPlaying"
  getSoundsPlaying ::
    IO CInt

foreign import ccall safe "raylib.h &GetSoundsPlaying"
  p'getSoundsPlaying ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h IsSoundPlaying_" isSoundPlaying :: Ptr Sound -> IO CInt

foreign import ccall safe "raylib.h &IsSoundPlaying"
  p'isSoundPlaying ::
    FunPtr (Sound -> IO CInt)

foreign import ccall safe "bindings.h SetSoundVolume_" setSoundVolume :: Ptr Sound -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetSoundVolume"
  p'setSoundVolume ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetSoundPitch_" setSoundPitch :: Ptr Sound -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetSoundPitch"
  p'setSoundPitch ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetSoundPan_" setSoundPan :: Ptr Sound -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetSoundPan"
  p'setSoundPan ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h WaveCopy_" waveCopy :: Ptr Wave -> IO (Ptr Wave)

foreign import ccall safe "raylib.h &WaveCopy"
  p'waveCopy ::
    FunPtr (Wave -> IO Wave)

foreign import ccall safe "raylib.h WaveCrop"
  waveCrop ::
    Ptr Wave -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h &WaveCrop"
  p'waveCrop ::
    FunPtr (Ptr Wave -> CInt -> CInt -> IO ())

foreign import ccall safe "raylib.h WaveFormat"
  waveFormat ::
    Ptr Wave -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h &WaveFormat"
  p'waveFormat ::
    FunPtr (Ptr Wave -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h LoadWaveSamples_" loadWaveSamples :: Ptr Wave -> IO (Ptr CFloat)

foreign import ccall safe "raylib.h &LoadWaveSamples"
  p'loadWaveSamples ::
    FunPtr (Wave -> IO (Ptr CFloat))

foreign import ccall safe "raylib.h UnloadWaveSamples"
  unloadWaveSamples ::
    Ptr CFloat -> IO ()

foreign import ccall safe "raylib.h &UnloadWaveSamples"
  p'unloadWaveSamples ::
    FunPtr (Ptr CFloat -> IO ())

foreign import ccall safe "bindings.h LoadMusicStream_" loadMusicStream :: CString -> IO (Ptr Music)

foreign import ccall safe "raylib.h &LoadMusicStream"
  p'loadMusicStream ::
    FunPtr (CString -> IO Music)

foreign import ccall safe "bindings.h LoadMusicStreamFromMemory_" loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Music)

foreign import ccall safe "raylib.h &LoadMusicStreamFromMemory"
  p'loadMusicStreamFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Music)

foreign import ccall safe "bindings.h UnloadMusicStream_" unloadMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "raylib.h &UnloadMusicStream"
  p'unloadMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h PlayMusicStream_" playMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "raylib.h &PlayMusicStream"
  p'playMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h IsMusicStreamPlaying_" isMusicStreamPlaying :: Ptr Music -> IO CInt

foreign import ccall safe "raylib.h &IsMusicStreamPlaying"
  p'isMusicStreamPlaying ::
    FunPtr (Music -> IO CInt)

foreign import ccall safe "bindings.h UpdateMusicStream_" updateMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "raylib.h &UpdateMusicStream"
  p'updateMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h StopMusicStream_" stopMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "raylib.h &StopMusicStream"
  p'stopMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h PauseMusicStream_" pauseMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "raylib.h &PauseMusicStream"
  p'pauseMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h ResumeMusicStream_" resumeMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "raylib.h &ResumeMusicStream"
  p'resumeMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h SeekMusicStream_" seekMusicStream :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SeekMusicStream"
  p'seekMusicStream ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicVolume_" setMusicVolume :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetMusicVolume"
  p'setMusicVolume ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicPitch_" setMusicPitch :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetMusicPitch"
  p'setMusicPitch ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicPan_" setMusicPan :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetMusicPan"
  p'setMusicPan ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h GetMusicTimeLength_" getMusicTimeLength :: Ptr Music -> IO CFloat

foreign import ccall safe "raylib.h &GetMusicTimeLength"
  p'getMusicTimeLength ::
    FunPtr (Music -> IO CFloat)

foreign import ccall safe "bindings.h GetMusicTimePlayed_" getMusicTimePlayed :: Ptr Music -> IO CFloat

foreign import ccall safe "raylib.h &GetMusicTimePlayed"
  p'getMusicTimePlayed ::
    FunPtr (Music -> IO CFloat)

foreign import ccall safe "bindings.h LoadAudioStream_" loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr AudioStream)

foreign import ccall safe "raylib.h &LoadAudioStream"
  p'loadAudioStream ::
    FunPtr (CUInt -> CUInt -> CUInt -> IO AudioStream)

foreign import ccall safe "bindings.h UnloadAudioStream_" unloadAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "raylib.h &UnloadAudioStream"
  p'unloadAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h UpdateAudioStream_" updateAudioStream :: Ptr AudioStream -> Ptr () -> CInt -> IO ()

foreign import ccall safe "raylib.h &UpdateAudioStream"
  p'updateAudioStream ::
    FunPtr (AudioStream -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h IsAudioStreamProcessed_" isAudioStreamProcessed :: Ptr AudioStream -> IO CInt

foreign import ccall safe "raylib.h &IsAudioStreamProcessed"
  p'isAudioStreamProcessed ::
    FunPtr (AudioStream -> IO CInt)

foreign import ccall safe "bindings.h PlayAudioStream_" playAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "raylib.h &PlayAudioStream"
  p'playAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h PauseAudioStream_" pauseAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "raylib.h &PauseAudioStream"
  p'pauseAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h ResumeAudioStream_" resumeAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "raylib.h &ResumeAudioStream"
  p'resumeAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h IsAudioStreamPlaying_" isAudioStreamPlaying :: Ptr AudioStream -> IO CInt

foreign import ccall safe "raylib.h &IsAudioStreamPlaying"
  p'isAudioStreamPlaying ::
    FunPtr (AudioStream -> IO CInt)

foreign import ccall safe "bindings.h StopAudioStream_" stopAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "raylib.h &StopAudioStream"
  p'stopAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamVolume_" setAudioStreamVolume :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetAudioStreamVolume"
  p'setAudioStreamVolume ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamPitch_" setAudioStreamPitch :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetAudioStreamPitch"
  p'setAudioStreamPitch ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamPan_" setAudioStreamPan :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall safe "raylib.h &SetAudioStreamPan"
  p'setAudioStreamPan ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall safe "raylib.h SetAudioStreamBufferSizeDefault"
  setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

foreign import ccall safe "raylib.h &SetAudioStreamBufferSizeDefault"
  p'setAudioStreamBufferSizeDefault ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamCallback_" setAudioStreamCallback :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "raylib.h &SetAudioStreamCallback"
  p'setAudioStreamCallback ::
    FunPtr (AudioStream -> AudioCallback -> IO ())

foreign import ccall safe "bindings.h AttachAudioStreamProcessor_" attachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "raylib.h &AttachAudioStreamProcessor"
  p'attachAudioStreamProcessor ::
    FunPtr (AudioStream -> AudioCallback -> IO ())

foreign import ccall safe "bindings.h DetachAudioStreamProcessor_" detachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "raylib.h &DetachAudioStreamProcessor"
  p'detachAudioStreamProcessor ::
    FunPtr (AudioStream -> AudioCallback -> IO ())
