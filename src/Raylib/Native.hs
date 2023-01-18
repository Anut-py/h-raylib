{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS -Wall #-}

module Raylib.Native where

import Foreign
  ( FunPtr,
    Ptr,
  )
import Foreign.C
  ( CBool (..),
    CDouble (..),
    CFloat (..),
    CInt (..),
    CLong (..),
    CString,
    CUChar,
    CUInt (..),
  )
import Raylib.Types
  ( AudioStream,
    BoundingBox,
    Camera2D,
    Camera3D,
    Color,
    FilePathList,
    Font,
    GlyphInfo,
    Image,
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
    Vector2,
    Vector3,
    Vector4,
    VrDeviceInfo,
    VrStereoConfig,
    Wave,
  )
import Prelude hiding (length)

type LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

type SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

type LoadFileTextCallback = FunPtr (CString -> IO CString)

type SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())

-- foreign import ccall safe "wrapper"
--   mk'TraceLogCallback ::
--     (CInt -> CString -> __builtin_va_list -> IO ()) -> IO TraceLogCallback
-- foreign import ccall safe "dynamic"
--   mK'TraceLogCallback ::
--     TraceLogCallback -> (CInt -> CString -> __builtin_va_list -> IO ())
-- TODO: redesign this
foreign import ccall safe "wrapper"
  mk'loadFileDataCallback ::
    (CString -> Ptr CUInt -> IO (Ptr CUChar)) -> IO LoadFileDataCallback

foreign import ccall safe "dynamic"
  mK'loadFileDataCallback ::
    LoadFileDataCallback -> (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall safe "wrapper"
  mk'saveFileDataCallback ::
    (CString -> Ptr () -> CUInt -> IO CInt) -> IO SaveFileDataCallback

foreign import ccall safe "dynamic"
  mK'saveFileDataCallback ::
    SaveFileDataCallback -> (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall safe "wrapper"
  mk'loadFileTextCallback ::
    (CString -> IO CString) -> IO LoadFileTextCallback

foreign import ccall safe "dynamic"
  mK'loadFileTextCallback ::
    LoadFileTextCallback -> (CString -> IO CString)

foreign import ccall safe "wrapper"
  mk'saveFileTextCallback ::
    (CString -> CString -> IO CInt) -> IO SaveFileTextCallback

foreign import ccall safe "dynamic"
  mK'saveFileTextCallback ::
    SaveFileTextCallback -> (CString -> CString -> IO CInt)

foreign import ccall safe "raylib.h InitWindow"
  c'initWindow ::
    CInt -> CInt -> CString -> IO ()

foreign import ccall safe "raylib.h WindowShouldClose"
  c'windowShouldClose ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowReady"
  c'isWindowReady ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowFullscreen"
  c'isWindowFullscreen ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowHidden"
  c'isWindowHidden ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowMinimized"
  c'isWindowMinimized ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowMaximized"
  c'isWindowMaximized ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowFocused"
  c'isWindowFocused ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowResized"
  c'isWindowResized ::
    IO CBool

foreign import ccall safe "raylib.h IsWindowState"
  c'isWindowState ::
    CUInt -> IO CBool

foreign import ccall safe "raylib.h SetWindowState"
  c'setWindowState ::
    CUInt -> IO ()

foreign import ccall safe "raylib.h ClearWindowState"
  c'clearWindowState ::
    CUInt -> IO ()

foreign import ccall safe "bindings.h SetWindowIcon_" c'setWindowIcon :: Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h SetWindowTitle"
  c'setWindowTitle ::
    CString -> IO ()

foreign import ccall safe "raylib.h SetWindowPosition"
  c'setWindowPosition ::
    CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h SetWindowMonitor"
  c'setWindowMonitor ::
    CInt -> IO ()

foreign import ccall safe "raylib.h SetWindowMinSize"
  c'setWindowMinSize ::
    CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h SetWindowSize"
  c'setWindowSize ::
    CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h SetWindowOpacity"
  c'setWindowOpacity ::
    CFloat -> IO ()

foreign import ccall safe "raylib.h GetScreenWidth"
  c'getScreenWidth ::
    IO CInt

foreign import ccall safe "raylib.h GetScreenHeight"
  c'getScreenHeight ::
    IO CInt

foreign import ccall safe "raylib.h GetRenderWidth"
  c'getRenderWidth ::
    IO CInt

foreign import ccall safe "raylib.h GetRenderHeight"
  c'getRenderHeight ::
    IO CInt

foreign import ccall safe "raylib.h GetMonitorCount"
  c'getMonitorCount ::
    IO CInt

foreign import ccall safe "raylib.h GetCurrentMonitor"
  c'getCurrentMonitor ::
    IO CInt

foreign import ccall safe "bindings.h GetMonitorPosition_" c'getMonitorPosition :: CInt -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetMonitorWidth"
  c'getMonitorWidth ::
    CInt -> IO CInt

foreign import ccall safe "raylib.h GetMonitorHeight"
  c'getMonitorHeight ::
    CInt -> IO CInt

foreign import ccall safe "raylib.h GetMonitorPhysicalWidth"
  c'getMonitorPhysicalWidth ::
    CInt -> IO CInt

foreign import ccall safe "raylib.h GetMonitorPhysicalHeight"
  c'getMonitorPhysicalHeight ::
    CInt -> IO CInt

foreign import ccall safe "raylib.h GetMonitorRefreshRate"
  c'getMonitorRefreshRate ::
    CInt -> IO CInt

foreign import ccall safe "bindings.h GetWindowPosition_" c'getWindowPosition :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetWindowScaleDPI_" c'getWindowScaleDPI :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetMonitorName"
  c'getMonitorName ::
    CInt -> IO CString

foreign import ccall safe "raylib.h SetClipboardText"
  c'setClipboardText ::
    CString -> IO ()

foreign import ccall safe "raylib.h GetClipboardText"
  c'getClipboardText ::
    IO CString

foreign import ccall safe "raylib.h WaitTime"
  c'waitTime ::
    CDouble -> IO ()

foreign import ccall safe "raylib.h IsCursorHidden"
  c'isCursorHidden ::
    IO CBool

foreign import ccall safe "raylib.h IsCursorOnScreen"
  c'isCursorOnScreen ::
    IO CBool

foreign import ccall safe "bindings.h ClearBackground_" c'clearBackground :: Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h BeginMode2D_" c'beginMode2D :: Ptr Raylib.Types.Camera2D -> IO ()

foreign import ccall safe "bindings.h BeginMode3D_" c'beginMode3D :: Ptr Raylib.Types.Camera3D -> IO ()

foreign import ccall safe "bindings.h BeginTextureMode_" c'beginTextureMode :: Ptr Raylib.Types.RenderTexture -> IO ()

foreign import ccall safe "bindings.h BeginShaderMode_" c'beginShaderMode :: Ptr Raylib.Types.Shader -> IO ()

foreign import ccall safe "raylib.h BeginBlendMode"
  c'beginBlendMode ::
    CInt -> IO ()

foreign import ccall safe "raylib.h BeginScissorMode"
  c'beginScissorMode ::
    CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h BeginVrStereoMode_" c'beginVrStereoMode :: Ptr Raylib.Types.VrStereoConfig -> IO ()

foreign import ccall safe "bindings.h LoadVrStereoConfig_" c'loadVrStereoConfig :: Ptr Raylib.Types.VrDeviceInfo -> IO (Ptr Raylib.Types.VrStereoConfig)

foreign import ccall safe "bindings.h UnloadVrStereoConfig_" c'unloadVrStereoConfig :: Ptr Raylib.Types.VrStereoConfig -> IO ()

foreign import ccall safe "bindings.h LoadShader_" c'loadShader :: CString -> CString -> IO (Ptr Raylib.Types.Shader)

foreign import ccall safe "bindings.h LoadShaderFromMemory_" c'loadShaderFromMemory :: CString -> CString -> IO (Ptr Raylib.Types.Shader)

foreign import ccall safe "bindings.h GetShaderLocation_" c'getShaderLocation :: Ptr Raylib.Types.Shader -> CString -> IO CInt

foreign import ccall safe "bindings.h GetShaderLocationAttrib_" c'getShaderLocationAttrib :: Ptr Raylib.Types.Shader -> CString -> IO CInt

foreign import ccall safe "bindings.h SetShaderValue_" c'setShaderValue :: Ptr Raylib.Types.Shader -> CInt -> Ptr () -> CInt -> IO ()

foreign import ccall safe "bindings.h SetShaderValueV_" c'setShaderValueV :: Ptr Raylib.Types.Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h SetShaderValueMatrix_" c'setShaderValueMatrix :: Ptr Raylib.Types.Shader -> CInt -> Ptr Raylib.Types.Matrix -> IO ()

foreign import ccall safe "bindings.h SetShaderValueTexture_" c'setShaderValueTexture :: Ptr Raylib.Types.Shader -> CInt -> Ptr Raylib.Types.Texture -> IO ()

foreign import ccall safe "bindings.h UnloadShader_" c'unloadShader :: Ptr Raylib.Types.Shader -> IO ()

foreign import ccall safe "bindings.h GetMouseRay_" c'getMouseRay :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Camera3D -> IO (Ptr Raylib.Types.Ray)

foreign import ccall safe "bindings.h GetCameraMatrix_" c'getCameraMatrix :: Ptr Raylib.Types.Camera3D -> IO (Ptr Raylib.Types.Matrix)

foreign import ccall safe "bindings.h GetCameraMatrix2D_" c'getCameraMatrix2D :: Ptr Raylib.Types.Camera2D -> IO (Ptr Raylib.Types.Matrix)

foreign import ccall safe "bindings.h GetWorldToScreen_" c'getWorldToScreen :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Camera3D -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetScreenToWorld2D_" c'getScreenToWorld2D :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Camera2D -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetWorldToScreenEx_" c'getWorldToScreenEx :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Camera3D -> CInt -> CInt -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetWorldToScreen2D_" c'getWorldToScreen2D :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Camera2D -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h SetTargetFPS"
  c'setTargetFPS ::
    CInt -> IO ()

foreign import ccall safe "raylib.h GetFPS"
  c'getFPS ::
    IO CInt

foreign import ccall safe "raylib.h GetFrameTime"
  c'getFrameTime ::
    IO CFloat

foreign import ccall safe "raylib.h GetTime"
  c'getTime ::
    IO CDouble

foreign import ccall safe "raylib.h GetRandomValue"
  c'getRandomValue ::
    CInt -> CInt -> IO CInt

foreign import ccall safe "raylib.h SetRandomSeed"
  c'setRandomSeed ::
    CUInt -> IO ()

foreign import ccall safe "raylib.h TakeScreenshot"
  c'takeScreenshot ::
    CString -> IO ()

foreign import ccall safe "raylib.h SetConfigFlags"
  c'setConfigFlags ::
    CUInt -> IO ()

foreign import ccall safe "raylib.h TraceLog"
  c'traceLog ::
    CInt -> CString -> IO () -- Uses varags, can't implement complete functionality

foreign import ccall safe "raylib.h SetTraceLogLevel"
  c'setTraceLogLevel ::
    CInt -> IO ()

foreign import ccall safe "raylib.h MemAlloc"
  c'memAlloc ::
    CInt -> IO (Ptr ())

foreign import ccall safe "raylib.h MemRealloc"
  c'memRealloc ::
    Ptr () -> CInt -> IO (Ptr ())

foreign import ccall safe "raylib.h MemFree"
  c'memFree ::
    Ptr () -> IO ()

foreign import ccall safe "raylib.h OpenURL"
  c'openURL ::
    CString -> IO ()

foreign import ccall safe "raylib.h LoadFileData"
  c'loadFileData ::
    CString -> Ptr CUInt -> IO (Ptr CUChar)

foreign import ccall safe "raylib.h UnloadFileData"
  c'unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall safe "raylib.h SaveFileData"
  c'saveFileData ::
    CString -> Ptr () -> CUInt -> IO CBool

foreign import ccall safe "raylib.h ExportDataAsCode"
  c'exportDataAsCode ::
    Ptr CUChar -> CUInt -> CString -> IO CBool

foreign import ccall safe "raylib.h LoadFileText"
  c'loadFileText ::
    CString -> IO CString

foreign import ccall safe "raylib.h UnloadFileText"
  c'unloadFileText ::
    CString -> IO ()

foreign import ccall safe "raylib.h SaveFileText"
  c'saveFileText ::
    CString -> CString -> IO CBool

foreign import ccall safe "raylib.h FileExists"
  c'fileExists ::
    CString -> IO CBool

foreign import ccall safe "raylib.h DirectoryExists"
  c'directoryExists ::
    CString -> IO CBool

foreign import ccall safe "raylib.h IsFileExtension"
  c'isFileExtension ::
    CString -> CString -> IO CBool

foreign import ccall safe "raylib.h GetFileLength"
  c'getFileLength ::
    CString -> IO CBool

foreign import ccall safe "raylib.h GetFileExtension"
  c'getFileExtension ::
    CString -> IO CString

foreign import ccall safe "raylib.h GetFileName"
  c'getFileName ::
    CString -> IO CString

foreign import ccall safe "raylib.h GetFileNameWithoutExt"
  c'getFileNameWithoutExt ::
    CString -> IO CString

foreign import ccall safe "raylib.h GetDirectoryPath"
  c'getDirectoryPath ::
    CString -> IO CString

foreign import ccall safe "raylib.h GetPrevDirectoryPath"
  c'getPrevDirectoryPath ::
    CString -> IO CString

foreign import ccall safe "raylib.h GetWorkingDirectory"
  c'getWorkingDirectory ::
    IO CString

foreign import ccall safe "raylib.h GetApplicationDirectory"
  c'getApplicationDirectory ::
    IO CString

foreign import ccall safe "raylib.h ChangeDirectory"
  c'changeDirectory ::
    CString -> IO CBool

foreign import ccall safe "raylib.h IsPathFile"
  c'isPathFile ::
    CString -> IO CBool

foreign import ccall safe "bindings.h LoadDirectoryFiles_" c'loadDirectoryFiles :: CString -> IO (Ptr Raylib.Types.FilePathList)

foreign import ccall safe "bindings.h LoadDirectoryFilesEx_" c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr Raylib.Types.FilePathList)

foreign import ccall safe "bindings.h UnloadDirectoryFiles_" c'unloadDirectoryFiles :: Ptr Raylib.Types.FilePathList -> IO ()

foreign import ccall safe "raylib.h IsFileDropped"
  c'isFileDropped ::
    IO CBool

foreign import ccall safe "bindings.h LoadDroppedFiles_" c'loadDroppedFiles :: IO (Ptr Raylib.Types.FilePathList)

foreign import ccall safe "bindings.h UnloadDroppedFiles_" c'unloadDroppedFiles :: Ptr Raylib.Types.FilePathList -> IO ()

foreign import ccall safe "raylib.h GetFileModTime"
  c'getFileModTime ::
    CString -> IO CLong

foreign import ccall safe "raylib.h CompressData"
  c'compressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "raylib.h DecompressData"
  c'decompressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "raylib.h EncodeDataBase64"
  c'encodeDataBase64 ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO CString

foreign import ccall safe "raylib.h DecodeDataBase64"
  c'decodeDataBase64 ::
    Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "raylib.h IsKeyPressed"
  c'isKeyPressed ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h IsKeyDown"
  c'isKeyDown ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h IsKeyReleased"
  c'isKeyReleased ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h IsKeyUp"
  c'isKeyUp ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h SetExitKey"
  c'setExitKey ::
    CInt -> IO ()

foreign import ccall safe "raylib.h GetKeyPressed"
  c'getKeyPressed ::
    IO CInt

foreign import ccall safe "raylib.h GetCharPressed"
  c'getCharPressed ::
    IO CInt

foreign import ccall safe "raylib.h IsGamepadAvailable"
  c'isGamepadAvailable ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h GetGamepadName"
  c'getGamepadName ::
    CInt -> IO CString

foreign import ccall safe "raylib.h IsGamepadButtonPressed"
  c'isGamepadButtonPressed ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "raylib.h IsGamepadButtonDown"
  c'isGamepadButtonDown ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "raylib.h IsGamepadButtonReleased"
  c'isGamepadButtonReleased ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "raylib.h IsGamepadButtonUp"
  c'isGamepadButtonUp ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "raylib.h GetGamepadButtonPressed"
  c'getGamepadButtonPressed ::
    IO CInt

foreign import ccall safe "raylib.h GetGamepadAxisCount"
  c'getGamepadAxisCount ::
    CInt -> IO CInt

foreign import ccall safe "raylib.h GetGamepadAxisMovement"
  c'getGamepadAxisMovement ::
    CInt -> CInt -> IO CFloat

foreign import ccall safe "raylib.h SetGamepadMappings"
  c'setGamepadMappings ::
    CString -> IO CInt

foreign import ccall safe "raylib.h IsMouseButtonPressed"
  c'isMouseButtonPressed ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h IsMouseButtonDown"
  c'isMouseButtonDown ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h IsMouseButtonReleased"
  c'isMouseButtonReleased ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h IsMouseButtonUp"
  c'isMouseButtonUp ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h GetMouseX"
  c'getMouseX ::
    IO CInt

foreign import ccall safe "raylib.h GetMouseY"
  c'getMouseY ::
    IO CInt

foreign import ccall safe "bindings.h GetMousePosition_" c'getMousePosition :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetMouseDelta_" c'getMouseDelta :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h SetMousePosition"
  c'setMousePosition ::
    CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h SetMouseOffset"
  c'setMouseOffset ::
    CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h SetMouseScale"
  c'setMouseScale ::
    CFloat -> CFloat -> IO ()

foreign import ccall safe "raylib.h GetMouseWheelMove"
  c'getMouseWheelMove ::
    IO CFloat

foreign import ccall safe "bindings.h GetMouseWheelMoveV_" c'getMouseWheelMoveV :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h SetMouseCursor"
  c'setMouseCursor ::
    CInt -> IO ()

foreign import ccall safe "raylib.h GetTouchX"
  c'getTouchX ::
    IO CInt

foreign import ccall safe "raylib.h GetTouchY"
  c'getTouchY ::
    IO CInt

foreign import ccall safe "bindings.h GetTouchPosition_" c'getTouchPosition :: CInt -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetTouchPointId"
  c'getTouchPointId ::
    CInt -> IO CInt

foreign import ccall safe "raylib.h GetTouchPointCount"
  c'getTouchPointCount ::
    IO CInt

foreign import ccall safe "raylib.h SetGesturesEnabled"
  c'setGesturesEnabled ::
    CUInt -> IO ()

foreign import ccall safe "raylib.h IsGestureDetected"
  c'isGestureDetected ::
    CInt -> IO CBool

foreign import ccall safe "raylib.h GetGestureDetected"
  c'getGestureDetected ::
    IO CInt

foreign import ccall safe "raylib.h GetGestureHoldDuration"
  c'getGestureHoldDuration ::
    IO CFloat

foreign import ccall safe "bindings.h GetGestureDragVector_" c'getGestureDragVector :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetGestureDragAngle"
  c'getGestureDragAngle ::
    IO CFloat

foreign import ccall safe "bindings.h GetGesturePinchVector_" c'getGesturePinchVector :: IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "raylib.h GetGesturePinchAngle"
  c'getGesturePinchAngle ::
    IO CFloat

foreign import ccall safe "bindings.h SetCameraMode_" c'setCameraMode :: Ptr Raylib.Types.Camera3D -> CInt -> IO ()

foreign import ccall safe "raylib.h UpdateCamera"
  c'updateCamera ::
    Ptr Raylib.Types.Camera3D -> IO ()

foreign import ccall safe "raylib.h SetCameraPanControl"
  c'setCameraPanControl ::
    CInt -> IO ()

foreign import ccall safe "raylib.h SetCameraAltControl"
  c'setCameraAltControl ::
    CInt -> IO ()

foreign import ccall safe "raylib.h SetCameraSmoothZoomControl"
  c'setCameraSmoothZoomControl ::
    CInt -> IO ()

foreign import ccall safe "raylib.h SetCameraMoveControls"
  c'setCameraMoveControls ::
    CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h SetShapesTexture_" c'setShapesTexture :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> IO ()

foreign import ccall safe "bindings.h DrawPixel_" c'drawPixel :: CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawPixelV_" c'drawPixelV :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLine_" c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLineV_" c'drawLineV :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLineEx_" c'drawLineEx :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLineBezier_" c'drawLineBezier :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLineBezierQuad_" c'drawLineBezierQuad :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLineBezierCubic_" c'drawLineBezierCubic :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawLineStrip_" c'drawLineStrip :: Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircle_" c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircleSector_" c'drawCircleSector :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircleSectorLines_" c'drawCircleSectorLines :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircleGradient_" c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircleV_" c'drawCircleV :: Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircleLines_" c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawEllipse_" c'drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawEllipseLines_" c'drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRing_" c'drawRing :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRingLines_" c'drawRingLines :: Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangle_" c'drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleV_" c'drawRectangleV :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleRec_" c'drawRectangleRec :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectanglePro_" c'drawRectanglePro :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleGradientV_" c'drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleGradientH_" c'drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleGradientEx_" c'drawRectangleGradientEx :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleLines_" c'drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleLinesEx_" c'drawRectangleLinesEx :: Ptr Raylib.Types.Rectangle -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleRounded_" c'drawRectangleRounded :: Ptr Raylib.Types.Rectangle -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRectangleRoundedLines_" c'drawRectangleRoundedLines :: Ptr Raylib.Types.Rectangle -> CFloat -> CInt -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTriangle_" c'drawTriangle :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTriangleLines_" c'drawTriangleLines :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTriangleFan_" c'drawTriangleFan :: Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTriangleStrip_" c'drawTriangleStrip :: Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawPoly_" c'drawPoly :: Ptr Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawPolyLines_" c'drawPolyLines :: Ptr Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawPolyLinesEx_" c'drawPolyLinesEx :: Ptr Raylib.Types.Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h CheckCollisionRecs_" c'checkCollisionRecs :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionCircles_" c'checkCollisionCircles :: Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Vector2 -> CFloat -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionCircleRec_" c'checkCollisionCircleRec :: Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Rectangle -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionPointRec_" c'checkCollisionPointRec :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Rectangle -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionPointCircle_" c'checkCollisionPointCircle :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionPointTriangle_" c'checkCollisionPointTriangle :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionLines_" c'checkCollisionLines :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> IO CBool

-- | If a collision is found, returns @Just collisionPoint@, otherwise returns @Nothing@
foreign import ccall safe "bindings.h CheckCollisionPointLine_" c'checkCollisionPointLine :: Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CInt -> IO CBool

foreign import ccall safe "bindings.h GetCollisionRec_" c'getCollisionRec :: Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> IO (Ptr Raylib.Types.Rectangle)

foreign import ccall safe "bindings.h LoadImage_" c'loadImage :: CString -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageRaw_" c'loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageAnim_" c'loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Raylib.Types.Image)

-- | Returns the final image and the framees in a tuple, e.g. @(img, 18)@
foreign import ccall safe "bindings.h LoadImageFromMemory_" c'loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageFromTexture_" c'loadImageFromTexture :: Ptr Raylib.Types.Texture -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h LoadImageFromScreen_" c'loadImageFromScreen :: IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h UnloadImage_" c'unloadImage :: Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "bindings.h ExportImage_" c'exportImage :: Ptr Raylib.Types.Image -> CString -> IO CBool

foreign import ccall safe "bindings.h ExportImageAsCode_" c'exportImageAsCode :: Ptr Raylib.Types.Image -> CString -> IO CBool

foreign import ccall safe "bindings.h GenImageColor_" c'genImageColor :: CInt -> CInt -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageGradientV_" c'genImageGradientV :: CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageGradientH_" c'genImageGradientH :: CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageGradientRadial_" c'genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageChecked_" c'genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageWhiteNoise_" c'genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImagePerlinNoise_" c'genImagePerlinNoise :: CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageCellular_" c'genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h GenImageText_" c'genImageText :: CInt -> CInt -> CString -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageCopy_" c'imageCopy :: Ptr Raylib.Types.Image -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageFromImage_" c'imageFromImage :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageText_" c'imageText :: CString -> CInt -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "bindings.h ImageTextEx_" c'imageTextEx :: Ptr Raylib.Types.Font -> CString -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "raylib.h ImageFormat"
  c'imageFormat ::
    Ptr Raylib.Types.Image -> CInt -> IO ()

foreign import ccall safe "bindings.h ImageToPOT_" c'imageToPOT :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageCrop_" c'imageCrop :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> IO ()

foreign import ccall safe "raylib.h ImageAlphaCrop"
  c'imageAlphaCrop ::
    Ptr Raylib.Types.Image -> CFloat -> IO ()

foreign import ccall safe "bindings.h ImageAlphaClear_" c'imageAlphaClear :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> CFloat -> IO ()

foreign import ccall safe "bindings.h ImageAlphaMask_" c'imageAlphaMask :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageAlphaPremultiply"
  c'imageAlphaPremultiply ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageBlurGaussian"
  c'imageBlurGaussian ::
    Ptr Raylib.Types.Image -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageResize"
  c'imageResize ::
    Ptr Raylib.Types.Image -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageResizeNN"
  c'imageResizeNN ::
    Ptr Raylib.Types.Image -> CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h ImageResizeCanvas_" c'imageResizeCanvas :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h ImageMipmaps"
  c'imageMipmaps ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageDither"
  c'imageDither ::
    Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageFlipVertical"
  c'imageFlipVertical ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageFlipHorizontal"
  c'imageFlipHorizontal ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageRotateCW"
  c'imageRotateCW ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageRotateCCW"
  c'imageRotateCCW ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "bindings.h ImageColorTint_" c'imageColorTint :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h ImageColorInvert"
  c'imageColorInvert ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageColorGrayscale"
  c'imageColorGrayscale ::
    Ptr Raylib.Types.Image -> IO ()

foreign import ccall safe "raylib.h ImageColorContrast"
  c'imageColorContrast ::
    Ptr Raylib.Types.Image -> CFloat -> IO ()

foreign import ccall safe "raylib.h ImageColorBrightness"
  c'imageColorBrightness ::
    Ptr Raylib.Types.Image -> CInt -> IO ()

foreign import ccall safe "bindings.h ImageColorReplace_" c'imageColorReplace :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h LoadImageColors_" c'loadImageColors :: Ptr Raylib.Types.Image -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h LoadImagePalette_" c'loadImagePalette :: Ptr Raylib.Types.Image -> CInt -> Ptr CInt -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h GetImageAlphaBorder_" c'getImageAlphaBorder :: Ptr Raylib.Types.Image -> CFloat -> IO (Ptr Raylib.Types.Rectangle)

foreign import ccall safe "bindings.h GetImageColor_" c'getImageColor :: Ptr Raylib.Types.Image -> CInt -> CInt -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ImageClearBackground_" c'imageClearBackground :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawPixel_" c'imageDrawPixel :: Ptr Raylib.Types.Image -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawPixelV_" c'imageDrawPixelV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawLine_" c'imageDrawLine :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawLineV_" c'imageDrawLineV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawCircle_" c'imageDrawCircle :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawCircleV_" c'imageDrawCircleV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawCircleLines_" c'imageDrawCircleLines :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawCircleLinesV_" c'imageDrawCircleLinesV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawRectangle_" c'imageDrawRectangle :: Ptr Raylib.Types.Image -> CInt -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawRectangleV_" c'imageDrawRectangleV :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawRectangleRec_" c'imageDrawRectangleRec :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawRectangleLines_" c'imageDrawRectangleLines :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDraw_" c'imageDraw :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Image -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawText_" c'imageDrawText :: Ptr Raylib.Types.Image -> CString -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h ImageDrawTextEx_" c'imageDrawTextEx :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Font -> CString -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h LoadTexture_" c'loadTexture :: CString -> IO (Ptr Raylib.Types.Texture)

foreign import ccall safe "bindings.h LoadTextureFromImage_" c'loadTextureFromImage :: Ptr Raylib.Types.Image -> IO (Ptr Raylib.Types.Texture)

foreign import ccall safe "bindings.h LoadTextureCubemap_" c'loadTextureCubemap :: Ptr Raylib.Types.Image -> CInt -> IO (Ptr Raylib.Types.Texture)

foreign import ccall safe "bindings.h LoadRenderTexture_" c'loadRenderTexture :: CInt -> CInt -> IO (Ptr Raylib.Types.RenderTexture)

foreign import ccall safe "bindings.h UnloadTexture_" c'unloadTexture :: Ptr Raylib.Types.Texture -> IO ()

foreign import ccall safe "bindings.h UnloadRenderTexture_" c'unloadRenderTexture :: Ptr Raylib.Types.RenderTexture -> IO ()

foreign import ccall safe "bindings.h UpdateTexture_" c'updateTexture :: Ptr Raylib.Types.Texture -> Ptr () -> IO ()

foreign import ccall safe "bindings.h UpdateTextureRec_" c'updateTextureRec :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr () -> IO ()

foreign import ccall safe "raylib.h GenTextureMipmaps"
  c'genTextureMipmaps ::
    Ptr Raylib.Types.Texture -> IO ()

foreign import ccall safe "bindings.h SetTextureFilter_" c'setTextureFilter :: Ptr Raylib.Types.Texture -> CInt -> IO ()

foreign import ccall safe "bindings.h SetTextureWrap_" c'setTextureWrap :: Ptr Raylib.Types.Texture -> CInt -> IO ()

foreign import ccall safe "bindings.h DrawTexture_" c'drawTexture :: Ptr Raylib.Types.Texture -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextureV_" c'drawTextureV :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextureEx_" c'drawTextureEx :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextureRec_" c'drawTextureRec :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTexturePro_" c'drawTexturePro :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextureNPatch_" c'drawTextureNPatch :: Ptr Raylib.Types.Texture -> Ptr Raylib.Types.NPatchInfo -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h Fade_" c'fade :: Ptr Raylib.Types.Color -> CFloat -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorToInt_" c'colorToInt :: Ptr Raylib.Types.Color -> IO CInt

foreign import ccall safe "bindings.h ColorNormalize_" c'colorNormalize :: Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Vector4)

foreign import ccall safe "bindings.h ColorFromNormalized_" c'colorFromNormalized :: Ptr Raylib.Types.Vector4 -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorToHSV_" c'colorToHSV :: Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Vector3)

foreign import ccall safe "bindings.h ColorFromHSV_" c'colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorTint_" c'colorTint :: Ptr Color -> Ptr Color -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorBrightness_" c'colorBrightness :: Ptr Color -> CFloat -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorContrast_" c'colorContrast :: Ptr Color -> CFloat -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorAlpha_" c'colorAlpha :: Ptr Raylib.Types.Color -> CFloat -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h ColorAlphaBlend_" c'colorAlphaBlend :: Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> Ptr Raylib.Types.Color -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h GetColor_" c'getColor :: CUInt -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h GetPixelColor_" c'getPixelColor :: Ptr () -> CInt -> IO (Ptr Raylib.Types.Color)

foreign import ccall safe "bindings.h SetPixelColor_" c'setPixelColor :: Ptr () -> Ptr Raylib.Types.Color -> CInt -> IO ()

foreign import ccall safe "bindings.h GetFontDefault_" c'getFontDefault :: IO (Ptr Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFont_" c'loadFont :: CString -> IO (Ptr Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFontEx_" c'loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFontFromImage_" c'loadFontFromImage :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Color -> CInt -> IO (Ptr Raylib.Types.Font)

foreign import ccall safe "bindings.h LoadFontFromMemory_" c'loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Raylib.Types.Font)

foreign import ccall safe "raylib.h LoadFontData"
  c'loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.GlyphInfo)

foreign import ccall safe "bindings.h GenImageFontAtlas_" c'genImageFontAtlas :: Ptr Raylib.Types.GlyphInfo -> Ptr (Ptr Raylib.Types.Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Raylib.Types.Image)

foreign import ccall safe "raylib.h UnloadFontData"
  c'unloadFontData ::
    Ptr Raylib.Types.GlyphInfo -> CInt -> IO ()

foreign import ccall safe "bindings.h UnloadFont_" c'unloadFont :: Ptr Raylib.Types.Font -> IO ()

foreign import ccall safe "bindings.h ExportFontAsCode_" c'exportFontAsCode :: Ptr Raylib.Types.Font -> CString -> IO CBool

foreign import ccall safe "raylib.h DrawFPS"
  c'drawFPS ::
    CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h DrawText_" c'drawText :: CString -> CInt -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextEx_" c'drawTextEx :: Ptr Raylib.Types.Font -> CString -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextPro_" c'drawTextPro :: Ptr Raylib.Types.Font -> CString -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextCodepoint_" c'drawTextCodepoint :: Ptr Raylib.Types.Font -> CInt -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTextCodepoints_" c'drawTextCodepoints :: Ptr Raylib.Types.Font -> Ptr CInt -> CInt -> Ptr Raylib.Types.Vector2 -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h MeasureText"
  c'measureText ::
    CString -> CInt -> IO CInt

foreign import ccall safe "bindings.h MeasureTextEx_" c'measureTextEx :: Ptr Raylib.Types.Font -> CString -> CFloat -> CFloat -> IO (Ptr Raylib.Types.Vector2)

foreign import ccall safe "bindings.h GetGlyphIndex_" c'getGlyphIndex :: Ptr Raylib.Types.Font -> CInt -> IO CInt

foreign import ccall safe "bindings.h GetGlyphInfo_" c'getGlyphInfo :: Ptr Raylib.Types.Font -> CInt -> IO (Ptr Raylib.Types.GlyphInfo)

foreign import ccall safe "bindings.h GetGlyphAtlasRec_" c'getGlyphAtlasRec :: Ptr Raylib.Types.Font -> CInt -> IO (Ptr Raylib.Types.Rectangle)

foreign import ccall safe "raylib.h LoadUTF8"
  c'loadUTF8 ::
    Ptr CInt -> CInt -> IO CString

foreign import ccall safe "raylib.h LoadCodepoints"
  c'loadCodepoints ::
    CString -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall safe "raylib.h GetCodepointCount"
  c'getCodepointCount ::
    CString -> IO CInt

-- | Deprecated, use `getCodepointNext`
-- foreign import ccall safe "raylib.h GetCodepoint"
--   getCodepoint ::
--     CString -> Ptr CInt -> IO CInt
foreign import ccall safe "raylib.h GetCodepointNext"
  c'getCodepointNext ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "raylib.h GetCodepointPrevious"
  c'getCodepointPrevious ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "raylib.h CodepointToUTF8"
  c'codepointToUTF8 ::
    CInt -> Ptr CInt -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextCopy"
--   textCopy ::
--     CString -> CString -> IO CInt

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextIsEqual"
--   textIsEqual ::
--     CString -> CString -> IO CInt

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextLength"
--   textLength ::
--     CString -> IO CUInt

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextFormat"
--   textFormat ::
--     CString -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextSubtext"
--   textSubtext ::
--     CString -> CInt -> CInt -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextReplace"
--   textReplace ::
--     CString -> CString -> CString -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextInsert"
--   textInsert ::
--     CString -> CString -> CInt -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextJoin"
--   textJoin ::
--     Ptr CString -> CInt -> CString -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextSplit"
--   textSplit ::
--     CString -> CChar -> Ptr CInt -> IO (Ptr CString)

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextAppend"
--   textAppend ::
--     CString -> CString -> Ptr CInt -> IO ()

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextFindIndex"
--   textFindIndex ::
--     CString -> CString -> IO CInt

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextToUpper"
--   textToUpper ::
--     CString -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextToLower"
--   textToLower ::
--     CString -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextToPascal"
--   textToPascal ::
--     CString -> IO CString

-- | Not required in Haskell
-- foreign import ccall safe "raylib.h TextToInteger"
--   textToInteger ::
--     CString -> IO CInt
foreign import ccall safe "bindings.h DrawLine3D_" c'drawLine3D :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawPoint3D_" c'drawPoint3D :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCircle3D_" c'drawCircle3D :: Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTriangle3D_" c'drawTriangle3D :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawTriangleStrip3D_" c'drawTriangleStrip3D :: Ptr Raylib.Types.Vector3 -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCube_" c'drawCube :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCubeV_" c'drawCubeV :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCubeWires_" c'drawCubeWires :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCubeWiresV_" c'drawCubeWiresV :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawSphere_" c'drawSphere :: Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawSphereEx_" c'drawSphereEx :: Ptr Raylib.Types.Vector3 -> CFloat -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawSphereWires_" c'drawSphereWires :: Ptr Raylib.Types.Vector3 -> CFloat -> CInt -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCylinder_" c'drawCylinder :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCylinderEx_" c'drawCylinderEx :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCylinderWires_" c'drawCylinderWires :: Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCylinderWiresEx_" c'drawCylinderWiresEx :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> CFloat -> CInt -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawCapsule_" c'drawCapsule :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "bindings.h DrawCapsuleWires_" c'drawCapsuleWires :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "bindings.h DrawPlane_" c'drawPlane :: Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawRay_" c'drawRay :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h DrawGrid"
  c'drawGrid ::
    CInt -> CFloat -> IO ()

foreign import ccall safe "bindings.h LoadModel_" c'loadModel :: CString -> IO (Ptr Raylib.Types.Model)

foreign import ccall safe "bindings.h LoadModelFromMesh_" c'loadModelFromMesh :: Ptr Raylib.Types.Mesh -> IO (Ptr Raylib.Types.Model)

foreign import ccall safe "bindings.h UnloadModel_" c'unloadModel :: Ptr Raylib.Types.Model -> IO ()

foreign import ccall safe "bindings.h UnloadModelKeepMeshes_" c'unloadModelKeepMeshes :: Ptr Raylib.Types.Model -> IO ()

foreign import ccall safe "bindings.h GetModelBoundingBox_" c'getModelBoundingBox :: Ptr Raylib.Types.Model -> IO (Ptr Raylib.Types.BoundingBox)

foreign import ccall safe "bindings.h DrawModel_" c'drawModel :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawModelEx_" c'drawModelEx :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawModelWires_" c'drawModelWires :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawModelWiresEx_" c'drawModelWiresEx :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawBoundingBox_" c'drawBoundingBox :: Ptr Raylib.Types.BoundingBox -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawBillboard_" c'drawBillboard :: Ptr Raylib.Types.Camera3D -> Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawBillboardRec_" c'drawBillboardRec :: Ptr Raylib.Types.Camera3D -> Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "bindings.h DrawBillboardPro_" c'drawBillboardPro :: Ptr Raylib.Types.Camera3D -> Ptr Raylib.Types.Texture -> Ptr Raylib.Types.Rectangle -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector2 -> Ptr Raylib.Types.Vector2 -> CFloat -> Ptr Raylib.Types.Color -> IO ()

foreign import ccall safe "raylib.h UploadMesh"
  c'uploadMesh ::
    Ptr Raylib.Types.Mesh -> CInt -> IO ()

foreign import ccall safe "bindings.h UpdateMeshBuffer_" c'updateMeshBuffer :: Ptr Raylib.Types.Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h UnloadMesh_" c'unloadMesh :: Ptr Raylib.Types.Mesh -> IO ()

foreign import ccall safe "bindings.h DrawMesh_" c'drawMesh :: Ptr Raylib.Types.Mesh -> Ptr Raylib.Types.Material -> Ptr Raylib.Types.Matrix -> IO ()

foreign import ccall safe "bindings.h DrawMeshInstanced_" c'drawMeshInstanced :: Ptr Raylib.Types.Mesh -> Ptr Raylib.Types.Material -> Ptr Raylib.Types.Matrix -> CInt -> IO ()

foreign import ccall safe "bindings.h ExportMesh_" c'exportMesh :: Ptr Raylib.Types.Mesh -> CString -> IO CBool

foreign import ccall safe "bindings.h GetMeshBoundingBox_" c'getMeshBoundingBox :: Ptr Raylib.Types.Mesh -> IO (Ptr Raylib.Types.BoundingBox)

foreign import ccall safe "raylib.h GenMeshTangents"
  c'genMeshTangents ::
    Ptr Raylib.Types.Mesh -> IO ()

foreign import ccall safe "bindings.h GenMeshPoly_" c'genMeshPoly :: CInt -> CFloat -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshPlane_" c'genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCube_" c'genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshSphere_" c'genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshHemiSphere_" c'genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCylinder_" c'genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCone_" c'genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshTorus_" c'genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshKnot_" c'genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshHeightmap_" c'genMeshHeightmap :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "bindings.h GenMeshCubicmap_" c'genMeshCubicmap :: Ptr Raylib.Types.Image -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.Mesh)

foreign import ccall safe "raylib.h LoadMaterials"
  c'loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Raylib.Types.Material)

foreign import ccall safe "bindings.h LoadMaterialDefault_" c'loadMaterialDefault :: IO (Ptr Raylib.Types.Material)

foreign import ccall safe "bindings.h UnloadMaterial_" c'unloadMaterial :: Ptr Raylib.Types.Material -> IO ()

foreign import ccall safe "bindings.h SetMaterialTexture_" c'setMaterialTexture :: Ptr Raylib.Types.Material -> CInt -> Ptr Raylib.Types.Texture -> IO ()

foreign import ccall safe "raylib.h SetModelMeshMaterial"
  c'setModelMeshMaterial ::
    Ptr Raylib.Types.Model -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h LoadModelAnimations"
  c'loadModelAnimations ::
    CString -> Ptr CUInt -> IO (Ptr Raylib.Types.ModelAnimation)

foreign import ccall safe "bindings.h UpdateModelAnimation_" c'updateModelAnimation :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.ModelAnimation -> CInt -> IO ()

foreign import ccall safe "bindings.h UnloadModelAnimation_" c'unloadModelAnimation :: Ptr Raylib.Types.ModelAnimation -> IO ()

foreign import ccall safe "raylib.h UnloadModelAnimations"
  c'unloadModelAnimations ::
    Ptr Raylib.Types.ModelAnimation -> CUInt -> IO ()

foreign import ccall safe "bindings.h IsModelAnimationValid_" c'isModelAnimationValid :: Ptr Raylib.Types.Model -> Ptr Raylib.Types.ModelAnimation -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionSpheres_" c'checkCollisionSpheres :: Ptr Raylib.Types.Vector3 -> CFloat -> Ptr Raylib.Types.Vector3 -> CFloat -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionBoxes_" c'checkCollisionBoxes :: Ptr Raylib.Types.BoundingBox -> Ptr Raylib.Types.BoundingBox -> IO CBool

foreign import ccall safe "bindings.h CheckCollisionBoxSphere_" c'checkCollisionBoxSphere :: Ptr Raylib.Types.BoundingBox -> Ptr Raylib.Types.Vector3 -> CFloat -> IO CBool

foreign import ccall safe "bindings.h GetRayCollisionSphere_" c'getRayCollisionSphere :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Vector3 -> CFloat -> IO (Ptr Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionBox_" c'getRayCollisionBox :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.BoundingBox -> IO (Ptr Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionMesh_" c'getRayCollisionMesh :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Mesh -> Ptr Raylib.Types.Matrix -> IO (Ptr Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionTriangle_" c'getRayCollisionTriangle :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionQuad_" c'getRayCollisionQuad :: Ptr Raylib.Types.Ray -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> Ptr Raylib.Types.Vector3 -> IO (Ptr Raylib.Types.RayCollision)

-- TODO: redesign this
foreign import ccall safe "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO AudioCallback

foreign import ccall safe "dynamic"
  mK'audioCallback ::
    AudioCallback -> (Ptr () -> CUInt -> IO ())

foreign import ccall safe "raylib.h IsAudioDeviceReady"
  c'isAudioDeviceReady ::
    IO CBool

foreign import ccall safe "raylib.h SetMasterVolume"
  c'setMasterVolume ::
    CFloat -> IO ()

foreign import ccall safe "bindings.h LoadWave_" c'loadWave :: CString -> IO (Ptr Raylib.Types.Wave)

foreign import ccall safe "bindings.h LoadWaveFromMemory_" c'loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Raylib.Types.Wave)

foreign import ccall safe "bindings.h LoadSound_" c'loadSound :: CString -> IO (Ptr Raylib.Types.Sound)

foreign import ccall safe "bindings.h LoadSoundFromWave_" c'loadSoundFromWave :: Ptr Raylib.Types.Wave -> IO (Ptr Raylib.Types.Sound)

foreign import ccall safe "bindings.h UpdateSound_" c'updateSound :: Ptr Raylib.Types.Sound -> Ptr () -> CInt -> IO ()

foreign import ccall safe "bindings.h UnloadWave_" c'unloadWave :: Ptr Raylib.Types.Wave -> IO ()

foreign import ccall safe "bindings.h UnloadSound_" c'unloadSound :: Ptr Raylib.Types.Sound -> IO ()

foreign import ccall safe "bindings.h ExportWave_" c'exportWave :: Ptr Raylib.Types.Wave -> CString -> IO CBool

foreign import ccall safe "bindings.h ExportWaveAsCode_" c'exportWaveAsCode :: Ptr Raylib.Types.Wave -> CString -> IO CBool

foreign import ccall safe "bindings.h PlaySound_" c'playSound :: Ptr Raylib.Types.Sound -> IO ()

foreign import ccall safe "bindings.h StopSound_" c'stopSound :: Ptr Raylib.Types.Sound -> IO ()

foreign import ccall safe "bindings.h PauseSound_" c'pauseSound :: Ptr Raylib.Types.Sound -> IO ()

foreign import ccall safe "bindings.h ResumeSound_" c'resumeSound :: Ptr Raylib.Types.Sound -> IO ()

foreign import ccall safe "bindings.h PlaySoundMulti_" c'playSoundMulti :: Ptr Raylib.Types.Sound -> IO ()

foreign import ccall safe "raylib.h GetSoundsPlaying"
  c'getSoundsPlaying ::
    IO CInt

foreign import ccall safe "bindings.h IsSoundPlaying_" c'isSoundPlaying :: Ptr Raylib.Types.Sound -> IO CBool

foreign import ccall safe "bindings.h SetSoundVolume_" c'setSoundVolume :: Ptr Raylib.Types.Sound -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetSoundPitch_" c'setSoundPitch :: Ptr Raylib.Types.Sound -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetSoundPan_" c'setSoundPan :: Ptr Raylib.Types.Sound -> CFloat -> IO ()

foreign import ccall safe "bindings.h WaveCopy_" c'waveCopy :: Ptr Raylib.Types.Wave -> IO (Ptr Raylib.Types.Wave)

foreign import ccall safe "raylib.h WaveCrop"
  c'waveCrop ::
    Ptr Raylib.Types.Wave -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h WaveFormat"
  c'waveFormat ::
    Ptr Raylib.Types.Wave -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "bindings.h LoadWaveSamples_" c'loadWaveSamples :: Ptr Raylib.Types.Wave -> IO (Ptr CFloat)

foreign import ccall safe "raylib.h UnloadWaveSamples"
  c'unloadWaveSamples ::
    Ptr CFloat -> IO ()

foreign import ccall safe "bindings.h LoadMusicStream_" c'loadMusicStream :: CString -> IO (Ptr Raylib.Types.Music)

foreign import ccall safe "bindings.h LoadMusicStreamFromMemory_" c'loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Raylib.Types.Music)

foreign import ccall safe "bindings.h UnloadMusicStream_" c'unloadMusicStream :: Ptr Raylib.Types.Music -> IO ()

foreign import ccall safe "bindings.h PlayMusicStream_" c'playMusicStream :: Ptr Raylib.Types.Music -> IO ()

foreign import ccall safe "bindings.h IsMusicStreamPlaying_" c'isMusicStreamPlaying :: Ptr Raylib.Types.Music -> IO CBool

foreign import ccall safe "bindings.h UpdateMusicStream_" c'updateMusicStream :: Ptr Raylib.Types.Music -> IO ()

foreign import ccall safe "bindings.h StopMusicStream_" c'stopMusicStream :: Ptr Raylib.Types.Music -> IO ()

foreign import ccall safe "bindings.h PauseMusicStream_" c'pauseMusicStream :: Ptr Raylib.Types.Music -> IO ()

foreign import ccall safe "bindings.h ResumeMusicStream_" c'resumeMusicStream :: Ptr Raylib.Types.Music -> IO ()

foreign import ccall safe "bindings.h SeekMusicStream_" c'seekMusicStream :: Ptr Raylib.Types.Music -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetMusicVolume_" c'setMusicVolume :: Ptr Raylib.Types.Music -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetMusicPitch_" c'setMusicPitch :: Ptr Raylib.Types.Music -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetMusicPan_" c'setMusicPan :: Ptr Raylib.Types.Music -> CFloat -> IO ()

foreign import ccall safe "bindings.h GetMusicTimeLength_" c'getMusicTimeLength :: Ptr Raylib.Types.Music -> IO CFloat

foreign import ccall safe "bindings.h GetMusicTimePlayed_" c'getMusicTimePlayed :: Ptr Raylib.Types.Music -> IO CFloat

foreign import ccall safe "bindings.h LoadAudioStream_" c'loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr Raylib.Types.AudioStream)

foreign import ccall safe "bindings.h UnloadAudioStream_" c'unloadAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

foreign import ccall safe "bindings.h UpdateAudioStream_" c'updateAudioStream :: Ptr Raylib.Types.AudioStream -> Ptr () -> CInt -> IO ()

foreign import ccall safe "bindings.h IsAudioStreamProcessed_" c'isAudioStreamProcessed :: Ptr Raylib.Types.AudioStream -> IO CBool

foreign import ccall safe "bindings.h PlayAudioStream_" c'playAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

foreign import ccall safe "bindings.h PauseAudioStream_" c'pauseAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

foreign import ccall safe "bindings.h ResumeAudioStream_" c'resumeAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

foreign import ccall safe "bindings.h IsAudioStreamPlaying_" c'isAudioStreamPlaying :: Ptr Raylib.Types.AudioStream -> IO CBool

foreign import ccall safe "bindings.h StopAudioStream_" c'stopAudioStream :: Ptr Raylib.Types.AudioStream -> IO ()

foreign import ccall safe "bindings.h SetAudioStreamVolume_" c'setAudioStreamVolume :: Ptr Raylib.Types.AudioStream -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetAudioStreamPitch_" c'setAudioStreamPitch :: Ptr Raylib.Types.AudioStream -> CFloat -> IO ()

foreign import ccall safe "bindings.h SetAudioStreamPan_" c'setAudioStreamPan :: Ptr Raylib.Types.AudioStream -> CFloat -> IO ()

foreign import ccall safe "raylib.h SetAudioStreamBufferSizeDefault"
  c'setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

foreign import ccall safe "bindings.h SetAudioStreamCallback_" c'setAudioStreamCallback :: Ptr Raylib.Types.AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "bindings.h AttachAudioStreamProcessor_" c'attachAudioStreamProcessor :: Ptr Raylib.Types.AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "bindings.h DetachAudioStreamProcessor_" c'detachAudioStreamProcessor :: Ptr Raylib.Types.AudioStream -> Ptr AudioCallback -> IO ()
