{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS -Wall #-}

module Raylib.Native where

import Foreign (Ptr)
import Foreign.C
  ( CBool (..),
    CDouble (..),
    CFloat (..),
    CInt (..),
    CLong (..),
    CString,
    CUChar (..),
    CUInt (..),
  )
import Raylib.Types
  ( AudioCallback,
    AudioStream,
    BoundingBox,
    Camera2D,
    Camera3D,
    Color,
    FilePathList,
    Font,
    GlyphInfo,
    Image,
    LoadFileDataCallback,
    LoadFileTextCallback,
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
    SaveFileDataCallback,
    SaveFileTextCallback,
    Shader,
    Sound,
    Texture,
    Vector2,
    Vector3,
    Vector4,
    VrDeviceInfo,
    VrStereoConfig,
    Wave, RLRenderBatch
  )
import Prelude hiding (length)

---- raylib.h

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

foreign import ccall safe "raylib.h CloseWindow"
  c'closeWindow ::
    IO ()

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

foreign import ccall safe "rl_bindings.h SetWindowIcon_" c'setWindowIcon :: Ptr Image -> IO ()

foreign import ccall safe "raylib.h SetWindowIcons" c'setWindowIcons :: Ptr Image -> CInt -> IO ()

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

foreign import ccall safe "rl_bindings.h GetMonitorPosition_" c'getMonitorPosition :: CInt -> IO (Ptr Vector2)

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

foreign import ccall safe "rl_bindings.h GetWindowPosition_" c'getWindowPosition :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetWindowScaleDPI_" c'getWindowScaleDPI :: IO (Ptr Vector2)

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

foreign import ccall safe "rl_bindings.h ClearBackground_" c'clearBackground :: Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h BeginMode2D_" c'beginMode2D :: Ptr Camera2D -> IO ()

foreign import ccall safe "rl_bindings.h BeginMode3D_" c'beginMode3D :: Ptr Camera3D -> IO ()

foreign import ccall safe "rl_bindings.h BeginTextureMode_" c'beginTextureMode :: Ptr RenderTexture -> IO ()

foreign import ccall safe "rl_bindings.h BeginShaderMode_" c'beginShaderMode :: Ptr Shader -> IO ()

foreign import ccall safe "raylib.h BeginBlendMode"
  c'beginBlendMode ::
    CInt -> IO ()

foreign import ccall safe "raylib.h BeginScissorMode"
  c'beginScissorMode ::
    CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h BeginVrStereoMode_" c'beginVrStereoMode :: Ptr VrStereoConfig -> IO ()

foreign import ccall safe "rl_bindings.h LoadVrStereoConfig_" c'loadVrStereoConfig :: Ptr VrDeviceInfo -> IO (Ptr VrStereoConfig)

foreign import ccall safe "rl_bindings.h UnloadVrStereoConfig_" c'unloadVrStereoConfig :: Ptr VrStereoConfig -> IO ()

foreign import ccall safe "rl_bindings.h LoadShader_" c'loadShader :: CString -> CString -> IO (Ptr Shader)

foreign import ccall safe "rl_bindings.h LoadShaderFromMemory_" c'loadShaderFromMemory :: CString -> CString -> IO (Ptr Shader)

foreign import ccall safe "rl_bindings.h IsShaderReady_" c'isShaderReady :: Ptr Shader -> IO CBool

foreign import ccall safe "rl_bindings.h GetShaderLocation_" c'getShaderLocation :: Ptr Shader -> CString -> IO CInt

foreign import ccall safe "rl_bindings.h GetShaderLocationAttrib_" c'getShaderLocationAttrib :: Ptr Shader -> CString -> IO CInt

foreign import ccall safe "rl_bindings.h SetShaderValue_" c'setShaderValue :: Ptr Shader -> CInt -> Ptr () -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetShaderValueV_" c'setShaderValueV :: Ptr Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetShaderValueMatrix_" c'setShaderValueMatrix :: Ptr Shader -> CInt -> Ptr Matrix -> IO ()

foreign import ccall safe "rl_bindings.h SetShaderValueTexture_" c'setShaderValueTexture :: Ptr Shader -> CInt -> Ptr Texture -> IO ()

foreign import ccall safe "rl_bindings.h UnloadShader_" c'unloadShader :: Ptr Shader -> IO ()

foreign import ccall safe "rl_bindings.h GetMouseRay_" c'getMouseRay :: Ptr Vector2 -> Ptr Camera3D -> IO (Ptr Ray)

foreign import ccall safe "rl_bindings.h GetCameraMatrix_" c'getCameraMatrix :: Ptr Camera3D -> IO (Ptr Matrix)

foreign import ccall safe "rl_bindings.h GetCameraMatrix2D_" c'getCameraMatrix2D :: Ptr Camera2D -> IO (Ptr Matrix)

foreign import ccall safe "rl_bindings.h GetWorldToScreen_" c'getWorldToScreen :: Ptr Vector3 -> Ptr Camera3D -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetScreenToWorld2D_" c'getScreenToWorld2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetWorldToScreenEx_" c'getWorldToScreenEx :: Ptr Vector3 -> Ptr Camera3D -> CInt -> CInt -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetWorldToScreen2D_" c'getWorldToScreen2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)

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
    CString -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "raylib.h UnloadFileData"
  c'unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall safe "raylib.h SaveFileData"
  c'saveFileData ::
    CString -> Ptr () -> CInt -> IO CBool

foreign import ccall safe "raylib.h ExportDataAsCode"
  c'exportDataAsCode ::
    Ptr CUChar -> CInt -> CString -> IO CBool

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

foreign import ccall safe "rl_bindings.h LoadDirectoryFiles_" c'loadDirectoryFiles :: CString -> IO (Ptr FilePathList)

foreign import ccall safe "rl_bindings.h LoadDirectoryFilesEx_" c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr FilePathList)

foreign import ccall safe "rl_bindings.h UnloadDirectoryFiles_" c'unloadDirectoryFiles :: Ptr FilePathList -> IO ()

foreign import ccall safe "raylib.h IsFileDropped"
  c'isFileDropped ::
    IO CBool

foreign import ccall safe "rl_bindings.h LoadDroppedFiles_" c'loadDroppedFiles :: IO (Ptr FilePathList)

foreign import ccall safe "rl_bindings.h UnloadDroppedFiles_" c'unloadDroppedFiles :: Ptr FilePathList -> IO ()

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

foreign import ccall safe "raylib.h IsKeyPressedRepeat"
  c'isKeyPressedRepeat ::
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

foreign import ccall safe "rl_bindings.h GetMousePosition_" c'getMousePosition :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetMouseDelta_" c'getMouseDelta :: IO (Ptr Vector2)

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

foreign import ccall safe "rl_bindings.h GetMouseWheelMoveV_" c'getMouseWheelMoveV :: IO (Ptr Vector2)

foreign import ccall safe "raylib.h SetMouseCursor"
  c'setMouseCursor ::
    CInt -> IO ()

foreign import ccall safe "raylib.h GetTouchX"
  c'getTouchX ::
    IO CInt

foreign import ccall safe "raylib.h GetTouchY"
  c'getTouchY ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetTouchPosition_" c'getTouchPosition :: CInt -> IO (Ptr Vector2)

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

foreign import ccall safe "rl_bindings.h GetGestureDragVector_" c'getGestureDragVector :: IO (Ptr Vector2)

foreign import ccall safe "raylib.h GetGestureDragAngle"
  c'getGestureDragAngle ::
    IO CFloat

foreign import ccall safe "rl_bindings.h GetGesturePinchVector_" c'getGesturePinchVector :: IO (Ptr Vector2)

foreign import ccall safe "raylib.h GetGesturePinchAngle"
  c'getGesturePinchAngle ::
    IO CFloat

foreign import ccall safe "raylib.h UpdateCamera"
  c'updateCamera ::
    Ptr Camera3D -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UpdateCameraPro_"
  c'updateCameraPro ::
    Ptr Camera3D -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetShapesTexture_" c'setShapesTexture :: Ptr Texture -> Ptr Rectangle -> IO ()

foreign import ccall safe "rl_bindings.h DrawPixel_" c'drawPixel :: CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPixelV_" c'drawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLine_" c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineV_" c'drawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineEx_" c'drawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineBezier_" c'drawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineBezierQuad_" c'drawLineBezierQuad :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineBezierCubic_" c'drawLineBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineBSpline_" c'drawLineBSpline :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineCatmullRom_" c'drawLineCatmullRom :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineStrip_" c'drawLineStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircle_" c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleSector_" c'drawCircleSector :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleSectorLines_" c'drawCircleSectorLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleGradient_" c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleV_" c'drawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleLines_" c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawEllipse_" c'drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawEllipseLines_" c'drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRing_" c'drawRing :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRingLines_" c'drawRingLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangle_" c'drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleV_" c'drawRectangleV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleRec_" c'drawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectanglePro_" c'drawRectanglePro :: Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleGradientV_" c'drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleGradientH_" c'drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleGradientEx_" c'drawRectangleGradientEx :: Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleLines_" c'drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleLinesEx_" c'drawRectangleLinesEx :: Ptr Rectangle -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleRounded_" c'drawRectangleRounded :: Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRectangleRoundedLines_" c'drawRectangleRoundedLines :: Ptr Rectangle -> CFloat -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTriangle_" c'drawTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTriangleLines_" c'drawTriangleLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTriangleFan_" c'drawTriangleFan :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTriangleStrip_" c'drawTriangleStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPoly_" c'drawPoly :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPolyLines_" c'drawPolyLines :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPolyLinesEx_" c'drawPolyLinesEx :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h CheckCollisionRecs_" c'checkCollisionRecs :: Ptr Rectangle -> Ptr Rectangle -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionCircles_" c'checkCollisionCircles :: Ptr Vector2 -> CFloat -> Ptr Vector2 -> CFloat -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionCircleRec_" c'checkCollisionCircleRec :: Ptr Vector2 -> CFloat -> Ptr Rectangle -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionPointRec_" c'checkCollisionPointRec :: Ptr Vector2 -> Ptr Rectangle -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionPointCircle_" c'checkCollisionPointCircle :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionPointTriangle_" c'checkCollisionPointTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionPointPoly_" c'checkCollisionPointPoly :: Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionLines_" c'checkCollisionLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionPointLine_" c'checkCollisionPointLine :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h GetCollisionRec_" c'getCollisionRec :: Ptr Rectangle -> Ptr Rectangle -> IO (Ptr Rectangle)

foreign import ccall safe "rl_bindings.h LoadImage_" c'loadImage :: CString -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h LoadImageRaw_" c'loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h LoadImageSvg_" c'loadImageSvg :: CString -> CInt -> CInt -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h LoadImageAnim_" c'loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h LoadImageFromMemory_" c'loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h LoadImageFromTexture_" c'loadImageFromTexture :: Ptr Texture -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h LoadImageFromScreen_" c'loadImageFromScreen :: IO (Ptr Image)

foreign import ccall safe "rl_bindings.h IsImageReady_" c'isImageReady :: Ptr Image -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadImage_" c'unloadImage :: Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ExportImage_" c'exportImage :: Ptr Image -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h ExportImageToMemory_" c'exportImageToMemory :: Ptr Image -> CString -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "rl_bindings.h ExportImageAsCode_" c'exportImageAsCode :: Ptr Image -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h GenImageColor_" c'genImageColor :: CInt -> CInt -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageGradientLinear_" c'genImageGradientLinear :: CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageGradientRadial_" c'genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageGradientSquare_" c'genImageGradientSquare :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageChecked_" c'genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageWhiteNoise_" c'genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImagePerlinNoise_" c'genImagePerlinNoise :: CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageCellular_" c'genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h GenImageText_" c'genImageText :: CInt -> CInt -> CString -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h ImageCopy_" c'imageCopy :: Ptr Image -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h ImageFromImage_" c'imageFromImage :: Ptr Image -> Ptr Rectangle -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h ImageText_" c'imageText :: CString -> CInt -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h ImageTextEx_" c'imageTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> Ptr Color -> IO (Ptr Image)

foreign import ccall safe "raylib.h ImageFormat"
  c'imageFormat ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageToPOT_" c'imageToPOT :: Ptr Image -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageCrop_" c'imageCrop :: Ptr Image -> Ptr Rectangle -> IO ()

foreign import ccall safe "raylib.h ImageAlphaCrop"
  c'imageAlphaCrop ::
    Ptr Image -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h ImageAlphaClear_" c'imageAlphaClear :: Ptr Image -> Ptr Color -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h ImageAlphaMask_" c'imageAlphaMask :: Ptr Image -> Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageAlphaPremultiply"
  c'imageAlphaPremultiply ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageBlurGaussian"
  c'imageBlurGaussian ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageResize"
  c'imageResize ::
    Ptr Image -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageResizeNN"
  c'imageResizeNN ::
    Ptr Image -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageResizeCanvas_" c'imageResizeCanvas :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "raylib.h ImageMipmaps"
  c'imageMipmaps ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageDither"
  c'imageDither ::
    Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageFlipVertical"
  c'imageFlipVertical ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageFlipHorizontal"
  c'imageFlipHorizontal ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageRotate"
  c'imageRotate ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "raylib.h ImageRotateCW"
  c'imageRotateCW ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageRotateCCW"
  c'imageRotateCCW ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorTint_" c'imageColorTint :: Ptr Image -> Ptr Color -> IO ()

foreign import ccall safe "raylib.h ImageColorInvert"
  c'imageColorInvert ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageColorGrayscale"
  c'imageColorGrayscale ::
    Ptr Image -> IO ()

foreign import ccall safe "raylib.h ImageColorContrast"
  c'imageColorContrast ::
    Ptr Image -> CFloat -> IO ()

foreign import ccall safe "raylib.h ImageColorBrightness"
  c'imageColorBrightness ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorReplace_" c'imageColorReplace :: Ptr Image -> Ptr Color -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h LoadImageColors_" c'loadImageColors :: Ptr Image -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h LoadImagePalette_" c'loadImagePalette :: Ptr Image -> CInt -> Ptr CInt -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h GetImageAlphaBorder_" c'getImageAlphaBorder :: Ptr Image -> CFloat -> IO (Ptr Rectangle)

foreign import ccall safe "rl_bindings.h GetImageColor_" c'getImageColor :: Ptr Image -> CInt -> CInt -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ImageClearBackground_" c'imageClearBackground :: Ptr Image -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawPixel_" c'imageDrawPixel :: Ptr Image -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawPixelV_" c'imageDrawPixelV :: Ptr Image -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawLine_" c'imageDrawLine :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawLineV_" c'imageDrawLineV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawCircle_" c'imageDrawCircle :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawCircleV_" c'imageDrawCircleV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawCircleLines_" c'imageDrawCircleLines :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawCircleLinesV_" c'imageDrawCircleLinesV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawRectangle_" c'imageDrawRectangle :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawRectangleV_" c'imageDrawRectangleV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawRectangleRec_" c'imageDrawRectangleRec :: Ptr Image -> Ptr Rectangle -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawRectangleLines_" c'imageDrawRectangleLines :: Ptr Image -> Ptr Rectangle -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDraw_" c'imageDraw :: Ptr Image -> Ptr Image -> Ptr Rectangle -> Ptr Rectangle -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawText_" c'imageDrawText :: Ptr Image -> CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageDrawTextEx_" c'imageDrawTextEx :: Ptr Image -> Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h LoadTexture_" c'loadTexture :: CString -> IO (Ptr Texture)

foreign import ccall safe "rl_bindings.h LoadTextureFromImage_" c'loadTextureFromImage :: Ptr Image -> IO (Ptr Texture)

foreign import ccall safe "rl_bindings.h LoadTextureCubemap_" c'loadTextureCubemap :: Ptr Image -> CInt -> IO (Ptr Texture)

foreign import ccall safe "rl_bindings.h LoadRenderTexture_" c'loadRenderTexture :: CInt -> CInt -> IO (Ptr RenderTexture)

foreign import ccall safe "rl_bindings.h IsTextureReady_" c'isTextureReady :: Ptr Texture -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadTexture_" c'unloadTexture :: Ptr Texture -> IO ()

foreign import ccall safe "rl_bindings.h IsRenderTextureReady_" c'isRenderTextureReady :: Ptr RenderTexture -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadRenderTexture_" c'unloadRenderTexture :: Ptr RenderTexture -> IO ()

foreign import ccall safe "rl_bindings.h UpdateTexture_" c'updateTexture :: Ptr Texture -> Ptr () -> IO ()

foreign import ccall safe "rl_bindings.h UpdateTextureRec_" c'updateTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr () -> IO ()

foreign import ccall safe "raylib.h GenTextureMipmaps"
  c'genTextureMipmaps ::
    Ptr Texture -> IO ()

foreign import ccall safe "rl_bindings.h SetTextureFilter_" c'setTextureFilter :: Ptr Texture -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetTextureWrap_" c'setTextureWrap :: Ptr Texture -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h DrawTexture_" c'drawTexture :: Ptr Texture -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextureV_" c'drawTextureV :: Ptr Texture -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextureEx_" c'drawTextureEx :: Ptr Texture -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextureRec_" c'drawTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTexturePro_" c'drawTexturePro :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextureNPatch_" c'drawTextureNPatch :: Ptr Texture -> Ptr NPatchInfo -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h Fade_" c'fade :: Ptr Color -> CFloat -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorToInt_" c'colorToInt :: Ptr Color -> IO CInt

foreign import ccall safe "rl_bindings.h ColorNormalize_" c'colorNormalize :: Ptr Color -> IO (Ptr Vector4)

foreign import ccall safe "rl_bindings.h ColorFromNormalized_" c'colorFromNormalized :: Ptr Vector4 -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorToHSV_" c'colorToHSV :: Ptr Color -> IO (Ptr Vector3)

foreign import ccall safe "rl_bindings.h ColorFromHSV_" c'colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorTint_" c'colorTint :: Ptr Color -> Ptr Color -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorBrightness_" c'colorBrightness :: Ptr Color -> CFloat -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorContrast_" c'colorContrast :: Ptr Color -> CFloat -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorAlpha_" c'colorAlpha :: Ptr Color -> CFloat -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h ColorAlphaBlend_" c'colorAlphaBlend :: Ptr Color -> Ptr Color -> Ptr Color -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h GetColor_" c'getColor :: CUInt -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h GetPixelColor_" c'getPixelColor :: Ptr () -> CInt -> IO (Ptr Color)

foreign import ccall safe "rl_bindings.h SetPixelColor_" c'setPixelColor :: Ptr () -> Ptr Color -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h GetFontDefault_" c'getFontDefault :: IO (Ptr Font)

foreign import ccall safe "rl_bindings.h LoadFont_" c'loadFont :: CString -> IO (Ptr Font)

foreign import ccall safe "rl_bindings.h LoadFontEx_" c'loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)

foreign import ccall safe "rl_bindings.h LoadFontFromImage_" c'loadFontFromImage :: Ptr Image -> Ptr Color -> CInt -> IO (Ptr Font)

foreign import ccall safe "rl_bindings.h LoadFontFromMemory_" c'loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)

foreign import ccall safe "raylib.h LoadFontData"
  c'loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall safe "rl_bindings.h GenImageFontAtlas_" c'genImageFontAtlas :: Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

foreign import ccall safe "raylib.h UnloadFontData"
  c'unloadFontData ::
    Ptr GlyphInfo -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h IsFontReady_" c'isFontReady :: Ptr Font -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadFont_" c'unloadFont :: Ptr Font -> IO ()

foreign import ccall safe "rl_bindings.h ExportFontAsCode_" c'exportFontAsCode :: Ptr Font -> CString -> IO CBool

foreign import ccall safe "raylib.h DrawFPS"
  c'drawFPS ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h DrawText_" c'drawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextEx_" c'drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextPro_" c'drawTextPro :: Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextCodepoint_" c'drawTextCodepoint :: Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextCodepoints_" c'drawTextCodepoints :: Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "raylib.h SetTextLineSpacing"
  c'setTextLineSpacing ::
    CInt -> IO ()

foreign import ccall safe "raylib.h MeasureText"
  c'measureText ::
    CString -> CInt -> IO CInt

foreign import ccall safe "rl_bindings.h MeasureTextEx_" c'measureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetGlyphIndex_" c'getGlyphIndex :: Ptr Font -> CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetGlyphInfo_" c'getGlyphInfo :: Ptr Font -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall safe "rl_bindings.h GetGlyphAtlasRec_" c'getGlyphAtlasRec :: Ptr Font -> CInt -> IO (Ptr Rectangle)

foreign import ccall safe "raylib.h LoadUTF8"
  c'loadUTF8 ::
    Ptr CInt -> CInt -> IO CString

foreign import ccall safe "raylib.h LoadCodepoints"
  c'loadCodepoints ::
    CString -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall safe "raylib.h GetCodepointCount"
  c'getCodepointCount ::
    CString -> IO CInt

foreign import ccall safe "raylib.h GetCodepointNext"
  c'getCodepointNext ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "raylib.h GetCodepointPrevious"
  c'getCodepointPrevious ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "raylib.h CodepointToUTF8"
  c'codepointToUTF8 ::
    CInt -> Ptr CInt -> IO CString

foreign import ccall safe "rl_bindings.h DrawLine3D_" c'drawLine3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPoint3D_" c'drawPoint3D :: Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircle3D_" c'drawCircle3D :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTriangle3D_" c'drawTriangle3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTriangleStrip3D_" c'drawTriangleStrip3D :: Ptr Vector3 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCube_" c'drawCube :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCubeV_" c'drawCubeV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCubeWires_" c'drawCubeWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCubeWiresV_" c'drawCubeWiresV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSphere_" c'drawSphere :: Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSphereEx_" c'drawSphereEx :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSphereWires_" c'drawSphereWires :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCylinder_" c'drawCylinder :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCylinderEx_" c'drawCylinderEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCylinderWires_" c'drawCylinderWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCylinderWiresEx_" c'drawCylinderWiresEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCapsule_" c'drawCapsule :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCapsuleWires_" c'drawCapsuleWires :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPlane_" c'drawPlane :: Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawRay_" c'drawRay :: Ptr Ray -> Ptr Color -> IO ()

foreign import ccall safe "raylib.h DrawGrid"
  c'drawGrid ::
    CInt -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h LoadModel_" c'loadModel :: CString -> IO (Ptr Model)

foreign import ccall safe "rl_bindings.h LoadModelFromMesh_" c'loadModelFromMesh :: Ptr Mesh -> IO (Ptr Model)

foreign import ccall safe "rl_bindings.h IsModelReady_" c'isModelReady :: Ptr Model -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadModel_" c'unloadModel :: Ptr Model -> IO ()

foreign import ccall safe "rl_bindings.h GetModelBoundingBox_" c'getModelBoundingBox :: Ptr Model -> IO (Ptr BoundingBox)

foreign import ccall safe "rl_bindings.h DrawModel_" c'drawModel :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawModelEx_" c'drawModelEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawModelWires_" c'drawModelWires :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawModelWiresEx_" c'drawModelWiresEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawBoundingBox_" c'drawBoundingBox :: Ptr BoundingBox -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawBillboard_" c'drawBillboard :: Ptr Camera3D -> Ptr Texture -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawBillboardRec_" c'drawBillboardRec :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawBillboardPro_" c'drawBillboardPro :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "raylib.h UploadMesh"
  c'uploadMesh ::
    Ptr Mesh -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UpdateMeshBuffer_" c'updateMeshBuffer :: Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UnloadMesh_" c'unloadMesh :: Ptr Mesh -> IO ()

foreign import ccall safe "rl_bindings.h DrawMesh_" c'drawMesh :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()

foreign import ccall safe "rl_bindings.h DrawMeshInstanced_" c'drawMeshInstanced :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ExportMesh_" c'exportMesh :: Ptr Mesh -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h GetMeshBoundingBox_" c'getMeshBoundingBox :: Ptr Mesh -> IO (Ptr BoundingBox)

foreign import ccall safe "raylib.h GenMeshTangents"
  c'genMeshTangents ::
    Ptr Mesh -> IO ()

foreign import ccall safe "rl_bindings.h GenMeshPoly_" c'genMeshPoly :: CInt -> CFloat -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshPlane_" c'genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshCube_" c'genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshSphere_" c'genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshHemiSphere_" c'genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshCylinder_" c'genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshCone_" c'genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshTorus_" c'genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshKnot_" c'genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshHeightmap_" c'genMeshHeightmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)

foreign import ccall safe "rl_bindings.h GenMeshCubicmap_" c'genMeshCubicmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)

foreign import ccall safe "raylib.h LoadMaterials"
  c'loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Material)

foreign import ccall safe "rl_bindings.h LoadMaterialDefault_" c'loadMaterialDefault :: IO (Ptr Material)

foreign import ccall safe "rl_bindings.h IsMaterialReady_" c'isMaterialReady :: Ptr Material -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadMaterial_" c'unloadMaterial :: Ptr Material -> IO ()

foreign import ccall safe "rl_bindings.h SetMaterialTexture_" c'setMaterialTexture :: Ptr Material -> CInt -> Ptr Texture -> IO ()

foreign import ccall safe "raylib.h SetModelMeshMaterial"
  c'setModelMeshMaterial ::
    Ptr Model -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h LoadModelAnimations"
  c'loadModelAnimations ::
    CString -> Ptr CInt -> IO (Ptr ModelAnimation)

foreign import ccall safe "rl_bindings.h UpdateModelAnimation_" c'updateModelAnimation :: Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UnloadModelAnimation_" c'unloadModelAnimation :: Ptr ModelAnimation -> IO ()

foreign import ccall safe "raylib.h UnloadModelAnimations"
  c'unloadModelAnimations ::
    Ptr ModelAnimation -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h IsModelAnimationValid_" c'isModelAnimationValid :: Ptr Model -> Ptr ModelAnimation -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionSpheres_" c'checkCollisionSpheres :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionBoxes_" c'checkCollisionBoxes :: Ptr BoundingBox -> Ptr BoundingBox -> IO CBool

foreign import ccall safe "rl_bindings.h CheckCollisionBoxSphere_" c'checkCollisionBoxSphere :: Ptr BoundingBox -> Ptr Vector3 -> CFloat -> IO CBool

foreign import ccall safe "rl_bindings.h GetRayCollisionSphere_" c'getRayCollisionSphere :: Ptr Ray -> Ptr Vector3 -> CFloat -> IO (Ptr RayCollision)

foreign import ccall safe "rl_bindings.h GetRayCollisionBox_" c'getRayCollisionBox :: Ptr Ray -> Ptr BoundingBox -> IO (Ptr RayCollision)

foreign import ccall safe "rl_bindings.h GetRayCollisionMesh_" c'getRayCollisionMesh :: Ptr Ray -> Ptr Mesh -> Ptr Matrix -> IO (Ptr RayCollision)

foreign import ccall safe "rl_bindings.h GetRayCollisionTriangle_" c'getRayCollisionTriangle :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)

foreign import ccall safe "rl_bindings.h GetRayCollisionQuad_" c'getRayCollisionQuad :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)

-- TODO: redesign this
foreign import ccall safe "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO AudioCallback

foreign import ccall safe "dynamic"
  mK'audioCallback ::
    AudioCallback -> (Ptr () -> CUInt -> IO ())

foreign import ccall safe "raylib.h CloseAudioDevice"
  c'closeAudioDevice ::
    IO ()

foreign import ccall safe "raylib.h IsAudioDeviceReady"
  c'isAudioDeviceReady ::
    IO CBool

foreign import ccall safe "raylib.h SetMasterVolume"
  c'setMasterVolume ::
    CFloat -> IO ()

foreign import ccall safe "rl_bindings.h LoadWave_" c'loadWave :: CString -> IO (Ptr Wave)

foreign import ccall safe "rl_bindings.h LoadWaveFromMemory_" c'loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Wave)

foreign import ccall safe "rl_bindings.h LoadSound_" c'loadSound :: CString -> IO (Ptr Sound)

foreign import ccall safe "rl_bindings.h LoadSoundFromWave_" c'loadSoundFromWave :: Ptr Wave -> IO (Ptr Sound)

foreign import ccall safe "rl_bindings.h LoadSoundAlias_" c'loadSoundAlias :: Ptr Sound -> IO (Ptr Sound)

foreign import ccall safe "rl_bindings.h UpdateSound_" c'updateSound :: Ptr Sound -> Ptr () -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h IsWaveReady_" c'isWaveReady :: Ptr Wave -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadWave_" c'unloadWave :: Ptr Wave -> IO ()

foreign import ccall safe "rl_bindings.h IsSoundReady_" c'isSoundReady :: Ptr Sound -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadSound_" c'unloadSound :: Ptr Sound -> IO ()

foreign import ccall safe "rl_bindings.h UnloadSoundAlias_" c'unloadSoundAlias :: Ptr Sound -> IO ()

foreign import ccall safe "rl_bindings.h ExportWave_" c'exportWave :: Ptr Wave -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h ExportWaveAsCode_" c'exportWaveAsCode :: Ptr Wave -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h PlaySound_" c'playSound :: Ptr Sound -> IO ()

foreign import ccall safe "rl_bindings.h StopSound_" c'stopSound :: Ptr Sound -> IO ()

foreign import ccall safe "rl_bindings.h PauseSound_" c'pauseSound :: Ptr Sound -> IO ()

foreign import ccall safe "rl_bindings.h ResumeSound_" c'resumeSound :: Ptr Sound -> IO ()

foreign import ccall safe "rl_bindings.h IsSoundPlaying_" c'isSoundPlaying :: Ptr Sound -> IO CBool

foreign import ccall safe "rl_bindings.h SetSoundVolume_" c'setSoundVolume :: Ptr Sound -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetSoundPitch_" c'setSoundPitch :: Ptr Sound -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetSoundPan_" c'setSoundPan :: Ptr Sound -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h WaveCopy_" c'waveCopy :: Ptr Wave -> IO (Ptr Wave)

foreign import ccall safe "raylib.h WaveCrop"
  c'waveCrop ::
    Ptr Wave -> CInt -> CInt -> IO ()

foreign import ccall safe "raylib.h WaveFormat"
  c'waveFormat ::
    Ptr Wave -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h LoadWaveSamples_" c'loadWaveSamples :: Ptr Wave -> IO (Ptr CFloat)

foreign import ccall safe "raylib.h UnloadWaveSamples"
  c'unloadWaveSamples ::
    Ptr CFloat -> IO ()

foreign import ccall safe "rl_bindings.h LoadMusicStream_" c'loadMusicStream :: CString -> IO (Ptr Music)

foreign import ccall safe "rl_bindings.h LoadMusicStreamFromMemory_" c'loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Music)

foreign import ccall safe "rl_bindings.h IsMusicReady_" c'isMusicReady :: Ptr Music -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadMusicStream_" c'unloadMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "rl_bindings.h PlayMusicStream_" c'playMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "rl_bindings.h IsMusicStreamPlaying_" c'isMusicStreamPlaying :: Ptr Music -> IO CBool

foreign import ccall safe "rl_bindings.h UpdateMusicStream_" c'updateMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "rl_bindings.h StopMusicStream_" c'stopMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "rl_bindings.h PauseMusicStream_" c'pauseMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "rl_bindings.h ResumeMusicStream_" c'resumeMusicStream :: Ptr Music -> IO ()

foreign import ccall safe "rl_bindings.h SeekMusicStream_" c'seekMusicStream :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetMusicVolume_" c'setMusicVolume :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetMusicPitch_" c'setMusicPitch :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetMusicPan_" c'setMusicPan :: Ptr Music -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h GetMusicTimeLength_" c'getMusicTimeLength :: Ptr Music -> IO CFloat

foreign import ccall safe "rl_bindings.h GetMusicTimePlayed_" c'getMusicTimePlayed :: Ptr Music -> IO CFloat

foreign import ccall safe "rl_bindings.h LoadAudioStream_" c'loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr AudioStream)

foreign import ccall safe "rl_bindings.h IsAudioStreamReady_" c'isAudioStreamReady :: Ptr AudioStream -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadAudioStream_" c'unloadAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "rl_bindings.h UpdateAudioStream_" c'updateAudioStream :: Ptr AudioStream -> Ptr () -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h IsAudioStreamProcessed_" c'isAudioStreamProcessed :: Ptr AudioStream -> IO CBool

foreign import ccall safe "rl_bindings.h PlayAudioStream_" c'playAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "rl_bindings.h PauseAudioStream_" c'pauseAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "rl_bindings.h ResumeAudioStream_" c'resumeAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "rl_bindings.h IsAudioStreamPlaying_" c'isAudioStreamPlaying :: Ptr AudioStream -> IO CBool

foreign import ccall safe "rl_bindings.h StopAudioStream_" c'stopAudioStream :: Ptr AudioStream -> IO ()

foreign import ccall safe "rl_bindings.h SetAudioStreamVolume_" c'setAudioStreamVolume :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetAudioStreamPitch_" c'setAudioStreamPitch :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetAudioStreamPan_" c'setAudioStreamPan :: Ptr AudioStream -> CFloat -> IO ()

foreign import ccall safe "raylib.h SetAudioStreamBufferSizeDefault"
  c'setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetAudioStreamCallback_" c'setAudioStreamCallback :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h AttachAudioStreamProcessor_" c'attachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h DetachAudioStreamProcessor_" c'detachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h AttachAudioMixedProcessor_" c'attachAudioMixedProcessor :: Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h DetachAudioMixedProcessor_" c'detachAudioMixedProcessor :: Ptr AudioCallback -> IO ()

---- rlgl.h

foreign import ccall safe "rlgl.h rlMatrixMode" c'rlMatrixMode :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlTranslatef" c'rlTranslatef :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlRotatef" c'rlRotatef :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlScalef" c'rlScalef :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlMultMatrixf" c'rlMultMatrixf :: Ptr CFloat -> IO ()

foreign import ccall safe "rlgl.h rlFrustum" c'rlFrustum :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall safe "rlgl.h rlOrtho" c'rlOrtho :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall safe "rlgl.h rlViewport" c'rlViewport :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlBegin" c'rlBegin :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlVertex2i" c'rlVertex2i :: CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlVertex2f" c'rlVertex2f :: CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlVertex3f" c'rlVertex3f :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlTexCoord2f" c'rlTexCoord2f :: CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlNormal3f" c'rlNormal3f :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlColor4ub" c'rlColor4ub :: CUChar -> CUChar -> CUChar -> CUChar -> IO ()

foreign import ccall safe "rlgl.h rlColor3f" c'rlColor3f :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlColor4f" c'rlColor4f :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl.h rlEnableVertexArray" c'rlEnableVertexArray :: CUInt -> IO CBool

foreign import ccall safe "rlgl.h rlEnableVertexBuffer" c'rlEnableVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlEnableVertexBufferElement" c'rlEnableVertexBufferElement :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlEnableVertexAttribute" c'rlEnableVertexAttribute :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlDisableVertexAttribute" c'rlDisableVertexAttribute :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlActiveTextureSlot" c'rlActiveTextureSlot :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlEnableTexture" c'rlEnableTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlEnableTextureCubemap" c'rlEnableTextureCubemap :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlTextureParameters" c'rlTextureParameters :: CUInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlCubemapParameters" c'rlCubemapParameters :: CUInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlEnableShader" c'rlEnableShader :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlEnableFramebuffer" c'rlEnableFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlActiveDrawBuffers" c'rlActiveDrawBuffers :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlSetCullFace" c'rlSetCullFace :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlScissor" c'rlScissor :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlSetLineWidth" c'rlSetLineWidth :: CFloat -> IO ()

foreign import ccall safe "rlgl.h rlGetLineWidth" c'rlGetLineWidth :: IO CFloat

foreign import ccall safe "rlgl.h rlIsStereoRenderEnabled" c'rlIsStereoRenderEnabled :: IO CBool

foreign import ccall safe "rlgl.h rlClearColor" c'rlClearColor :: CUChar -> CUChar -> CUChar -> CUChar -> IO ()

foreign import ccall safe "rlgl.h rlSetBlendMode" c'rlSetBlendMode :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlSetBlendFactors" c'rlSetBlendFactors :: CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlSetBlendFactorsSeparate" c'rlSetBlendFactorsSeparate :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlglInit" c'rlglInit :: CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlLoadExtensions" c'rlLoadExtensions :: Ptr () -> IO ()

foreign import ccall safe "rlgl.h rlGetVersion" c'rlGetVersion :: IO CInt

foreign import ccall safe "rlgl.h rlSetFramebufferWidth" c'rlSetFramebufferWidth :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlGetFramebufferWidth" c'rlGetFramebufferWidth :: IO CInt

foreign import ccall safe "rlgl.h rlSetFramebufferHeight" c'rlSetFramebufferHeight :: CInt -> IO ()

foreign import ccall safe "rlgl.h rlGetFramebufferHeight" c'rlGetFramebufferHeight :: IO CInt

foreign import ccall safe "rlgl.h rlGetTextureIdDefault" c'rlGetTextureIdDefault :: IO CUInt

foreign import ccall safe "rlgl.h rlGetShaderIdDefault" c'rlGetShaderIdDefault :: IO CUInt

foreign import ccall safe "rlgl.h rlGetShaderLocsDefault" c'rlGetShaderLocsDefault :: IO (Ptr CInt)

foreign import ccall safe "rlgl_bindings.h rlLoadRenderBatch_" c'rlLoadRenderBatch :: CInt -> CInt -> IO (Ptr RLRenderBatch)

foreign import ccall safe "rlgl_bindings.h rlUnloadRenderBatch_" c'rlUnloadRenderBatch :: Ptr RLRenderBatch -> IO ()

foreign import ccall safe "rlgl.h rlDrawRenderBatch" c'rlDrawRenderBatch :: Ptr RLRenderBatch -> IO ()

foreign import ccall safe "rlgl.h rlSetRenderBatchActive" c'rlSetRenderBatchActive :: Ptr RLRenderBatch -> IO ()

foreign import ccall safe "rlgl.h rlCheckRenderBatchLimit" c'rlCheckRenderBatchLimit :: CInt -> IO CBool

foreign import ccall safe "rlgl.h rlSetTexture" c'rlSetTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlLoadVertexArray" c'rlLoadVertexArray :: IO CUInt

foreign import ccall safe "rlgl.h rlLoadVertexBuffer" c'rlLoadVertexBuffer :: Ptr () -> CInt -> CBool -> IO CUInt

foreign import ccall safe "rlgl.h rlLoadVertexBufferElement" c'rlLoadVertexBufferElement :: Ptr () -> CInt -> CBool -> IO CUInt

foreign import ccall safe "rlgl.h rlUpdateVertexBuffer" c'rlUpdateVertexBuffer :: CUInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlUpdateVertexBufferElements" c'rlUpdateVertexBufferElements :: CUInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexArray" c'rlUnloadVertexArray :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUnloadVertexBuffer" c'rlUnloadVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlSetVertexAttribute" c'rlSetVertexAttribute :: CUInt -> CInt -> CInt -> CBool -> CInt -> Ptr () -> IO ()

foreign import ccall safe "rlgl.h rlSetVertexAttributeDivisor" c'rlSetVertexAttributeDivisor :: CUInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlSetVertexAttributeDefault" c'rlSetVertexAttributeDefault :: CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlDrawVertexArray" c'rlDrawVertexArray :: CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlDrawVertexArrayElements" c'rlDrawVertexArrayElements :: CInt -> CInt -> Ptr () -> IO ()

foreign import ccall safe "rlgl.h rlDrawVertexArrayInstanced" c'rlDrawVertexArrayInstanced :: CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlDrawVertexArrayElementsInstanced" c'rlDrawVertexArrayElementsInstanced :: CInt -> CInt -> Ptr () -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlLoadTexture" c'rlLoadTexture :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CUInt

foreign import ccall safe "rlgl.h rlLoadTextureDepth" c'rlLoadTextureDepth :: CInt -> CInt -> CBool -> IO CUInt

foreign import ccall safe "rlgl.h rlLoadTextureCubemap" c'rlLoadTextureCubemap :: Ptr () -> CInt -> CInt -> IO CUInt

foreign import ccall safe "rlgl.h rlUpdateTexture" c'rlUpdateTexture :: CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ()

foreign import ccall safe "rlgl.h rlGetGlTextureFormats" c'rlGetGlTextureFormats :: CInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()

foreign import ccall safe "rlgl.h rlGetPixelFormatName" c'rlGetPixelFormatName :: CUInt -> IO CString

foreign import ccall safe "rlgl.h rlUnloadTexture" c'rlUnloadTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlGenTextureMipmaps" c'rlGenTextureMipmaps :: CUInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO ()

foreign import ccall safe "rlgl.h rlReadTexturePixels" c'rlReadTexturePixels :: CUInt -> CInt -> CInt -> CInt -> IO (Ptr ())

foreign import ccall safe "rlgl.h rlReadScreenPixels" c'rlReadScreenPixels :: CInt -> CInt -> IO (Ptr CUChar)

foreign import ccall safe "rlgl.h rlLoadFramebuffer" c'rlLoadFramebuffer :: CInt -> CInt -> IO CUInt

foreign import ccall safe "rlgl.h rlFramebufferAttach" c'rlFramebufferAttach :: CUInt -> CUInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl.h rlFramebufferComplete" c'rlFramebufferComplete :: CUInt -> IO CBool

foreign import ccall safe "rlgl.h rlUnloadFramebuffer" c'rlUnloadFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlLoadShaderCode" c'rlLoadShaderCode :: CString -> CString -> IO CUInt

foreign import ccall safe "rlgl.h rlCompileShader" c'rlCompileShader :: CString -> CInt -> IO CUInt

foreign import ccall safe "rlgl.h rlLoadShaderProgram" c'rlLoadShaderProgram :: CUInt -> CUInt -> IO CUInt

foreign import ccall safe "rlgl.h rlUnloadShaderProgram" c'rlUnloadShaderProgram :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlGetLocationUniform" c'rlGetLocationUniform :: CUInt -> CString -> IO CInt

foreign import ccall safe "rlgl.h rlGetLocationAttrib" c'rlGetLocationAttrib :: CUInt -> CString -> IO CInt

foreign import ccall safe "rlgl.h rlSetUniform" c'rlSetUniform :: CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetUniformMatrix_" c'rlSetUniformMatrix :: CInt -> Ptr Matrix -> IO ()

foreign import ccall safe "rlgl.h rlSetUniformSampler" c'rlSetUniformSampler :: CInt -> CUInt -> IO ()

foreign import ccall safe "rlgl.h rlSetShader" c'rlSetShader :: CUInt -> Ptr CInt -> IO ()

foreign import ccall safe "rlgl.h rlLoadComputeShaderProgram" c'rlLoadComputeShaderProgram :: CUInt -> IO CUInt

foreign import ccall safe "rlgl.h rlComputeShaderDispatch" c'rlComputeShaderDispatch :: CUInt -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl.h rlLoadShaderBuffer" c'rlLoadShaderBuffer :: CUInt -> Ptr () -> CInt -> IO CUInt

foreign import ccall safe "rlgl.h rlUnloadShaderBuffer" c'rlUnloadShaderBuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl.h rlUpdateShaderBuffer" c'rlUpdateShaderBuffer :: CUInt -> Ptr () -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl.h rlBindShaderBuffer" c'rlBindShaderBuffer :: CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl.h rlReadShaderBuffer" c'rlReadShaderBuffer :: CUInt -> Ptr () -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl.h rlCopyShaderBuffer" c'rlCopyShaderBuffer :: CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl.h rlGetShaderBufferSize" c'rlGetShaderBufferSize :: CUInt -> IO CUInt

foreign import ccall safe "rlgl.h rlBindImageTexture" c'rlBindImageTexture :: CUInt -> CUInt -> CInt -> CBool -> IO ()

foreign import ccall safe "rlgl.h rlGetMatrixModelview" c'rlGetMatrixModelview :: IO (Ptr Matrix)

foreign import ccall safe "rlgl.h rlGetMatrixProjection" c'rlGetMatrixProjection :: IO (Ptr Matrix)

foreign import ccall safe "rlgl.h rlGetMatrixTransform" c'rlGetMatrixTransform :: IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlGetMatrixProjectionStereo_" c'rlGetMatrixProjectionStereo :: CInt -> IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlGetMatrixViewOffsetStereo_" c'rlGetMatrixViewOffsetStereo :: CInt -> IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlSetMatrixProjection_" c'rlSetMatrixProjection :: Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetMatrixModelview_" c'rlSetMatrixModelview :: Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetMatrixProjectionStereo_" c'rlSetMatrixProjectionStereo :: Ptr Matrix -> Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetMatrixViewOffsetStereo_" c'rlSetMatrixViewOffsetStereo :: Ptr Matrix -> Ptr Matrix -> IO ()

foreign import ccall safe "rl_internal.h rlGetPixelDataSize" c'rlGetPixelDataSize :: CInt -> CInt -> CInt -> IO CInt
