{-# LANGUAGE CPP #-}
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
    AutomationEvent,
    AutomationEventList,
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
    RLRenderBatch,
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
    Wave,
  )

#ifdef WEB_FFI

import Raylib.Web.Native (callRaylibFunction)

c'initWindow :: CInt -> CInt -> CString -> IO ()
c'initWindow = callRaylibFunction "_InitWindow_"

c'windowShouldClose :: IO CBool
c'windowShouldClose = callRaylibFunction "_WindowShouldClose_"

c'closeWindow :: IO ()
c'closeWindow = callRaylibFunction "_CloseWindow_"

c'isWindowReady :: IO CBool
c'isWindowReady = callRaylibFunction "_IsWindowReady_"

c'isWindowFullscreen :: IO CBool
c'isWindowFullscreen = callRaylibFunction "_IsWindowFullscreen_"

c'isWindowHidden :: IO CBool
c'isWindowHidden = callRaylibFunction "_IsWindowHidden_"

c'isWindowMinimized :: IO CBool
c'isWindowMinimized = callRaylibFunction "_IsWindowMinimized_"

c'isWindowMaximized :: IO CBool
c'isWindowMaximized = callRaylibFunction "_IsWindowMaximized_"

c'isWindowFocused :: IO CBool
c'isWindowFocused = callRaylibFunction "_IsWindowFocused_"

c'isWindowResized :: IO CBool
c'isWindowResized = callRaylibFunction "_IsWindowResized_"

c'isWindowState :: CUInt -> IO CBool
c'isWindowState = callRaylibFunction "_IsWindowState_"

c'setWindowState :: CUInt -> IO ()
c'setWindowState = callRaylibFunction "_SetWindowState_"

c'clearWindowState :: CUInt -> IO ()
c'clearWindowState = callRaylibFunction "_ClearWindowState_"

c'toggleFullscreen :: IO ()
c'toggleFullscreen = callRaylibFunction "_ToggleFullscreen_"

c'toggleBorderlessWindowed :: IO ()
c'toggleBorderlessWindowed = callRaylibFunction "_ToggleBorderlessWindowed_"

c'maximizeWindow :: IO ()
c'maximizeWindow = callRaylibFunction "_MaximizeWindow_"

c'minimizeWindow :: IO ()
c'minimizeWindow = callRaylibFunction "_MinimizeWindow_"

c'restoreWindow :: IO ()
c'restoreWindow = callRaylibFunction "_RestoreWindow_"

c'setWindowIcon :: Ptr Image -> IO ()
c'setWindowIcon = callRaylibFunction "_SetWindowIcon_"

c'setWindowIcons :: Ptr Image -> CInt -> IO ()
c'setWindowIcons = callRaylibFunction "_SetWindowIcons_"

c'setWindowTitle :: CString -> IO ()
c'setWindowTitle = callRaylibFunction "_SetWindowTitle_"

c'setWindowPosition :: CInt -> CInt -> IO ()
c'setWindowPosition = callRaylibFunction "_SetWindowPosition_"

c'setWindowMonitor :: CInt -> IO ()
c'setWindowMonitor = callRaylibFunction "_SetWindowMonitor_"

c'setWindowMinSize :: CInt -> CInt -> IO ()
c'setWindowMinSize = callRaylibFunction "_SetWindowMinSize_"

c'setWindowMaxSize :: CInt -> CInt -> IO ()
c'setWindowMaxSize = callRaylibFunction "_SetWindowMaxSize_"

c'setWindowSize :: CInt -> CInt -> IO ()
c'setWindowSize = callRaylibFunction "_SetWindowSize_"

c'setWindowOpacity :: CFloat -> IO ()
c'setWindowOpacity = callRaylibFunction "_SetWindowOpacity_"

c'setWindowFocused :: IO ()
c'setWindowFocused = callRaylibFunction "_SetWindowFocused_"

c'getWindowHandle :: IO (Ptr ())
c'getWindowHandle = callRaylibFunction "_GetWindowHandle_"

c'getScreenWidth :: IO CInt
c'getScreenWidth = callRaylibFunction "_GetScreenWidth_"

c'getScreenHeight :: IO CInt
c'getScreenHeight = callRaylibFunction "_GetScreenHeight_"

c'getRenderWidth :: IO CInt
c'getRenderWidth = callRaylibFunction "_GetRenderWidth_"

c'getRenderHeight :: IO CInt
c'getRenderHeight = callRaylibFunction "_GetRenderHeight_"

c'getMonitorCount :: IO CInt
c'getMonitorCount = callRaylibFunction "_GetMonitorCount_"

c'getCurrentMonitor :: IO CInt
c'getCurrentMonitor = callRaylibFunction "_GetCurrentMonitor_"

c'getMonitorPosition :: CInt -> IO (Ptr Vector2)
c'getMonitorPosition = callRaylibFunction "_GetMonitorPosition_"

c'getMonitorWidth :: CInt -> IO CInt
c'getMonitorWidth = callRaylibFunction "_GetMonitorWidth_"

c'getMonitorHeight :: CInt -> IO CInt
c'getMonitorHeight = callRaylibFunction "_GetMonitorHeight_"

c'getMonitorPhysicalWidth :: CInt -> IO CInt
c'getMonitorPhysicalWidth = callRaylibFunction "_GetMonitorPhysicalWidth_"

c'getMonitorPhysicalHeight :: CInt -> IO CInt
c'getMonitorPhysicalHeight = callRaylibFunction "_GetMonitorPhysicalHeight_"

c'getMonitorRefreshRate :: CInt -> IO CInt
c'getMonitorRefreshRate = callRaylibFunction "_GetMonitorRefreshRate_"

c'getWindowPosition :: IO (Ptr Vector2)
c'getWindowPosition = callRaylibFunction "_GetWindowPosition_"

c'getWindowScaleDPI :: IO (Ptr Vector2)
c'getWindowScaleDPI = callRaylibFunction "_GetWindowScaleDPI_"

c'getMonitorName :: CInt -> IO CString
c'getMonitorName = callRaylibFunction "_GetMonitorName_"

c'setClipboardText :: CString -> IO ()
c'setClipboardText = callRaylibFunction "_SetClipboardText_"

c'getClipboardText :: IO CString
c'getClipboardText = callRaylibFunction "_GetClipboardText_"

c'enableEventWaiting :: IO ()
c'enableEventWaiting = callRaylibFunction "_EnableEventWaiting_"

c'disableEventWaiting :: IO ()
c'disableEventWaiting = callRaylibFunction "_DisableEventWaiting_"

c'swapScreenBuffer :: IO ()
c'swapScreenBuffer = callRaylibFunction "_SwapScreenBuffer_"

c'pollInputEvents :: IO ()
c'pollInputEvents = callRaylibFunction "_PollInputEvents_"

c'waitTime :: CDouble -> IO ()
c'waitTime = callRaylibFunction "_WaitTime_"

c'showCursor :: IO ()
c'showCursor = callRaylibFunction "_ShowCursor_"

c'hideCursor :: IO ()
c'hideCursor = callRaylibFunction "_HideCursor_"

c'isCursorHidden :: IO CBool
c'isCursorHidden = callRaylibFunction "_IsCursorHidden_"

c'enableCursor :: IO ()
c'enableCursor = callRaylibFunction "_EnableCursor_"

c'disableCursor :: IO ()
c'disableCursor = callRaylibFunction "_DisableCursor_"

c'isCursorOnScreen :: IO CBool
c'isCursorOnScreen = callRaylibFunction "_IsCursorOnScreen_"

c'clearBackground :: Ptr Color -> IO ()
c'clearBackground = callRaylibFunction "_ClearBackground_"

c'beginDrawing :: IO ()
c'beginDrawing = callRaylibFunction "_BeginDrawing_"

c'endDrawing :: IO ()
c'endDrawing = callRaylibFunction "_EndDrawing_"

c'beginMode2D :: Ptr Camera2D -> IO ()
c'beginMode2D = callRaylibFunction "_BeginMode2D_"

c'endMode2D :: IO ()
c'endMode2D = callRaylibFunction "_EndMode2D_"

c'beginMode3D :: Ptr Camera3D -> IO ()
c'beginMode3D = callRaylibFunction "_BeginMode3D_"

c'endMode3D :: IO ()
c'endMode3D = callRaylibFunction "_EndMode3D_"

c'beginTextureMode :: Ptr RenderTexture -> IO ()
c'beginTextureMode = callRaylibFunction "_BeginTextureMode_"

c'endTextureMode :: IO ()
c'endTextureMode = callRaylibFunction "_EndTextureMode_"

c'beginShaderMode :: Ptr Shader -> IO ()
c'beginShaderMode = callRaylibFunction "_BeginShaderMode_"

c'endShaderMode :: IO ()
c'endShaderMode = callRaylibFunction "_EndShaderMode_"

c'beginBlendMode :: CInt -> IO ()
c'beginBlendMode = callRaylibFunction "_BeginBlendMode_"

c'endBlendMode :: IO ()
c'endBlendMode = callRaylibFunction "_EndBlendMode_"

c'beginScissorMode :: CInt -> CInt -> CInt -> CInt -> IO ()
c'beginScissorMode = callRaylibFunction "_BeginScissorMode_"

c'endScissorMode :: IO ()
c'endScissorMode = callRaylibFunction "_EndScissorMode_"

c'beginVrStereoMode :: Ptr VrStereoConfig -> IO ()
c'beginVrStereoMode = callRaylibFunction "_BeginVrStereoMode_"

c'endVrStereoMode :: IO ()
c'endVrStereoMode = callRaylibFunction "_EndVrStereoMode_"

c'loadVrStereoConfig :: Ptr VrDeviceInfo -> IO (Ptr VrStereoConfig)
c'loadVrStereoConfig = callRaylibFunction "_LoadVrStereoConfig_"

c'unloadVrStereoConfig :: Ptr VrStereoConfig -> IO ()
c'unloadVrStereoConfig = callRaylibFunction "_UnloadVrStereoConfig_"

c'loadShader :: CString -> CString -> IO (Ptr Shader)
c'loadShader = callRaylibFunction "_LoadShader_"

c'loadShaderFromMemory :: CString -> CString -> IO (Ptr Shader)
c'loadShaderFromMemory = callRaylibFunction "_LoadShaderFromMemory_"

c'isShaderReady :: Ptr Shader -> IO CBool
c'isShaderReady = callRaylibFunction "_IsShaderReady_"

c'getShaderLocation :: Ptr Shader -> CString -> IO CInt
c'getShaderLocation = callRaylibFunction "_GetShaderLocation_"

c'getShaderLocationAttrib :: Ptr Shader -> CString -> IO CInt
c'getShaderLocationAttrib = callRaylibFunction "_GetShaderLocationAttrib_"

c'setShaderValue :: Ptr Shader -> CInt -> Ptr () -> CInt -> IO ()
c'setShaderValue = callRaylibFunction "_SetShaderValue_"

c'setShaderValueV :: Ptr Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()
c'setShaderValueV = callRaylibFunction "_SetShaderValueV_"

c'setShaderValueMatrix :: Ptr Shader -> CInt -> Ptr Matrix -> IO ()
c'setShaderValueMatrix = callRaylibFunction "_SetShaderValueMatrix_"

c'setShaderValueTexture :: Ptr Shader -> CInt -> Ptr Texture -> IO ()
c'setShaderValueTexture = callRaylibFunction "_SetShaderValueTexture_"

c'unloadShader :: Ptr Shader -> IO ()
c'unloadShader = callRaylibFunction "_UnloadShader_"

c'getMouseRay :: Ptr Vector2 -> Ptr Camera3D -> IO (Ptr Ray)
c'getMouseRay = callRaylibFunction "_GetMouseRay_"

c'getCameraMatrix :: Ptr Camera3D -> IO (Ptr Matrix)
c'getCameraMatrix = callRaylibFunction "_GetCameraMatrix_"

c'getCameraMatrix2D :: Ptr Camera2D -> IO (Ptr Matrix)
c'getCameraMatrix2D = callRaylibFunction "_GetCameraMatrix2D_"

c'getWorldToScreen :: Ptr Vector3 -> Ptr Camera3D -> IO (Ptr Vector2)
c'getWorldToScreen = callRaylibFunction "_GetWorldToScreen_"

c'getScreenToWorld2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)
c'getScreenToWorld2D = callRaylibFunction "_GetScreenToWorld2D_"

c'getWorldToScreenEx :: Ptr Vector3 -> Ptr Camera3D -> CInt -> CInt -> IO (Ptr Vector2)
c'getWorldToScreenEx = callRaylibFunction "_GetWorldToScreenEx_"

c'getWorldToScreen2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)
c'getWorldToScreen2D = callRaylibFunction "_GetWorldToScreen2D_"

c'setTargetFPS :: CInt -> IO ()
c'setTargetFPS = callRaylibFunction "_SetTargetFPS_"

c'getFPS :: IO CInt
c'getFPS = callRaylibFunction "_GetFPS_"

c'getFrameTime :: IO CFloat
c'getFrameTime = callRaylibFunction "_GetFrameTime_"

c'getTime :: IO CDouble
c'getTime = callRaylibFunction "_GetTime_"

c'setRandomSeed :: CUInt -> IO ()
c'setRandomSeed = callRaylibFunction "_SetRandomSeed_"

c'getRandomValue :: CInt -> CInt -> IO CInt
c'getRandomValue = callRaylibFunction "_GetRandomValue_"

c'loadRandomSequence :: CUInt -> CInt -> CInt -> IO (Ptr CInt)
c'loadRandomSequence = callRaylibFunction "_LoadRandomSequence_"

c'takeScreenshot :: CString -> IO ()
c'takeScreenshot = callRaylibFunction "_TakeScreenshot_"

c'setConfigFlags :: CUInt -> IO ()
c'setConfigFlags = callRaylibFunction "_SetConfigFlags_"

c'traceLog :: CInt -> CString -> IO () -- Uses varags, can't implement complete functionality
c'traceLog = callRaylibFunction "_TraceLog_"

c'setTraceLogLevel :: CInt -> IO ()
c'setTraceLogLevel = callRaylibFunction "_SetTraceLogLevel_"

c'memAlloc :: CInt -> IO (Ptr ())
c'memAlloc = callRaylibFunction "_MemAlloc_"

c'memRealloc :: Ptr () -> CInt -> IO (Ptr ())
c'memRealloc = callRaylibFunction "_MemRealloc_"

c'memFree :: Ptr () -> IO ()
c'memFree = callRaylibFunction "_MemFree_"

c'openURL :: CString -> IO ()
c'openURL = callRaylibFunction "_OpenURL_"

c'setLoadFileDataCallback :: LoadFileDataCallback -> IO ()
c'setLoadFileDataCallback = callRaylibFunction "_SetLoadFileDataCallback_"

c'setSaveFileDataCallback :: SaveFileDataCallback -> IO ()
c'setSaveFileDataCallback = callRaylibFunction "_SetSaveFileDataCallback_"

c'setLoadFileTextCallback :: LoadFileTextCallback -> IO ()
c'setLoadFileTextCallback = callRaylibFunction "_SetLoadFileTextCallback_"

c'setSaveFileTextCallback :: SaveFileTextCallback -> IO ()
c'setSaveFileTextCallback = callRaylibFunction "_SetSaveFileTextCallback_"

c'loadFileData :: CString -> Ptr CInt -> IO (Ptr CUChar)
c'loadFileData = callRaylibFunction "_LoadFileData_"

c'unloadFileData :: Ptr CUChar -> IO ()
c'unloadFileData = callRaylibFunction "_UnloadFileData_"

c'saveFileData :: CString -> Ptr () -> CInt -> IO CBool
c'saveFileData = callRaylibFunction "_SaveFileData_"

c'exportDataAsCode :: Ptr CUChar -> CInt -> CString -> IO CBool
c'exportDataAsCode = callRaylibFunction "_ExportDataAsCode_"

c'loadFileText :: CString -> IO CString
c'loadFileText = callRaylibFunction "_LoadFileText_"

c'unloadFileText :: CString -> IO ()
c'unloadFileText = callRaylibFunction "_UnloadFileText_"

c'saveFileText :: CString -> CString -> IO CBool
c'saveFileText = callRaylibFunction "_SaveFileText_"

c'fileExists :: CString -> IO CBool
c'fileExists = callRaylibFunction "_FileExists_"

c'directoryExists :: CString -> IO CBool
c'directoryExists = callRaylibFunction "_DirectoryExists_"

c'isFileExtension :: CString -> CString -> IO CBool
c'isFileExtension = callRaylibFunction "_IsFileExtension_"

c'getFileLength :: CString -> IO CBool
c'getFileLength = callRaylibFunction "_GetFileLength_"

c'getFileExtension :: CString -> IO CString
c'getFileExtension = callRaylibFunction "_GetFileExtension_"

c'getFileName :: CString -> IO CString
c'getFileName = callRaylibFunction "_GetFileName_"

c'getFileNameWithoutExt :: CString -> IO CString
c'getFileNameWithoutExt = callRaylibFunction "_GetFileNameWithoutExt_"

c'getDirectoryPath :: CString -> IO CString
c'getDirectoryPath = callRaylibFunction "_GetDirectoryPath_"

c'getPrevDirectoryPath :: CString -> IO CString
c'getPrevDirectoryPath = callRaylibFunction "_GetPrevDirectoryPath_"

c'getWorkingDirectory :: IO CString
c'getWorkingDirectory = callRaylibFunction "_GetWorkingDirectory_"

c'getApplicationDirectory :: IO CString
c'getApplicationDirectory = callRaylibFunction "_GetApplicationDirectory_"

c'changeDirectory :: CString -> IO CBool
c'changeDirectory = callRaylibFunction "_ChangeDirectory_"

c'isPathFile :: CString -> IO CBool
c'isPathFile = callRaylibFunction "_IsPathFile_"

c'loadDirectoryFiles :: CString -> IO (Ptr FilePathList)
c'loadDirectoryFiles = callRaylibFunction "_LoadDirectoryFiles_"

c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr FilePathList)
c'loadDirectoryFilesEx = callRaylibFunction "_LoadDirectoryFilesEx_"

c'unloadDirectoryFiles :: Ptr FilePathList -> IO ()
c'unloadDirectoryFiles = callRaylibFunction "_UnloadDirectoryFiles_"

c'isFileDropped :: IO CBool
c'isFileDropped = callRaylibFunction "_IsFileDropped_"

c'loadDroppedFiles :: IO (Ptr FilePathList)
c'loadDroppedFiles = callRaylibFunction "_LoadDroppedFiles_"

c'unloadDroppedFiles :: Ptr FilePathList -> IO ()
c'unloadDroppedFiles = callRaylibFunction "_UnloadDroppedFiles_"

c'getFileModTime :: CString -> IO CLong
c'getFileModTime = callRaylibFunction "_GetFileModTime_"

c'compressData :: Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)
c'compressData = callRaylibFunction "_CompressData_"

c'decompressData :: Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)
c'decompressData = callRaylibFunction "_DecompressData_"

c'encodeDataBase64 :: Ptr CUChar -> CInt -> Ptr CInt -> IO CString
c'encodeDataBase64 = callRaylibFunction "_EncodeDataBase64_"

c'decodeDataBase64 :: Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)
c'decodeDataBase64 = callRaylibFunction "_DecodeDataBase64_"

c'loadAutomationEventList :: CString -> IO (Ptr AutomationEventList)
c'loadAutomationEventList = callRaylibFunction "_LoadAutomationEventList_"

c'exportAutomationEventList :: Ptr AutomationEventList -> CString -> IO CBool
c'exportAutomationEventList = callRaylibFunction "_ExportAutomationEventList_"

c'setAutomationEventList :: Ptr AutomationEventList -> IO ()
c'setAutomationEventList = callRaylibFunction "_SetAutomationEventList_"

c'setAutomationEventBaseFrame :: CInt -> IO ()
c'setAutomationEventBaseFrame = callRaylibFunction "_SetAutomationEventBaseFrame_"

c'startAutomationEventRecording :: IO ()
c'startAutomationEventRecording = callRaylibFunction "_StartAutomationEventRecording_"

c'stopAutomationEventRecording :: IO ()
c'stopAutomationEventRecording = callRaylibFunction "_StopAutomationEventRecording_"

c'playAutomationEvent :: Ptr AutomationEvent -> IO ()
c'playAutomationEvent = callRaylibFunction "_PlayAutomationEvent"

c'isKeyPressed :: CInt -> IO CBool
c'isKeyPressed = callRaylibFunction "_IsKeyPressed_"

c'isKeyPressedRepeat :: CInt -> IO CBool
c'isKeyPressedRepeat = callRaylibFunction "_IsKeyPressedRepeat_"

c'isKeyDown :: CInt -> IO CBool
c'isKeyDown = callRaylibFunction "_IsKeyDown_"

c'isKeyReleased :: CInt -> IO CBool
c'isKeyReleased = callRaylibFunction "_IsKeyReleased_"

c'isKeyUp :: CInt -> IO CBool
c'isKeyUp = callRaylibFunction "_IsKeyUp_"

c'setExitKey :: CInt -> IO ()
c'setExitKey = callRaylibFunction "_SetExitKey_"

c'getKeyPressed :: IO CInt
c'getKeyPressed = callRaylibFunction "_GetKeyPressed_"

c'getCharPressed :: IO CInt
c'getCharPressed = callRaylibFunction "_GetCharPressed_"

c'isGamepadAvailable :: CInt -> IO CBool
c'isGamepadAvailable = callRaylibFunction "_IsGamepadAvailable_"

c'getGamepadName :: CInt -> IO CString
c'getGamepadName = callRaylibFunction "_GetGamepadName_"

c'isGamepadButtonPressed :: CInt -> CInt -> IO CBool
c'isGamepadButtonPressed = callRaylibFunction "_IsGamepadButtonPressed_"

c'isGamepadButtonDown :: CInt -> CInt -> IO CBool
c'isGamepadButtonDown = callRaylibFunction "_IsGamepadButtonDown_"

c'isGamepadButtonReleased :: CInt -> CInt -> IO CBool
c'isGamepadButtonReleased = callRaylibFunction "_IsGamepadButtonReleased_"

c'isGamepadButtonUp :: CInt -> CInt -> IO CBool
c'isGamepadButtonUp = callRaylibFunction "_IsGamepadButtonUp_"

c'getGamepadButtonPressed :: IO CInt
c'getGamepadButtonPressed = callRaylibFunction "_GetGamepadButtonPressed_"

c'getGamepadAxisCount :: CInt -> IO CInt
c'getGamepadAxisCount = callRaylibFunction "_GetGamepadAxisCount_"

c'getGamepadAxisMovement :: CInt -> CInt -> IO CFloat
c'getGamepadAxisMovement = callRaylibFunction "_GetGamepadAxisMovement_"

c'setGamepadMappings :: CString -> IO CInt
c'setGamepadMappings = callRaylibFunction "_SetGamepadMappings_"

c'isMouseButtonPressed :: CInt -> IO CBool
c'isMouseButtonPressed = callRaylibFunction "_IsMouseButtonPressed_"

c'isMouseButtonDown :: CInt -> IO CBool
c'isMouseButtonDown = callRaylibFunction "_IsMouseButtonDown_"

c'isMouseButtonReleased :: CInt -> IO CBool
c'isMouseButtonReleased = callRaylibFunction "_IsMouseButtonReleased_"

c'isMouseButtonUp :: CInt -> IO CBool
c'isMouseButtonUp = callRaylibFunction "_IsMouseButtonUp_"

c'getMouseX :: IO CInt
c'getMouseX = callRaylibFunction "_GetMouseX_"

c'getMouseY :: IO CInt
c'getMouseY = callRaylibFunction "_GetMouseY_"

c'getMousePosition :: IO (Ptr Vector2)
c'getMousePosition = callRaylibFunction "_GetMousePosition_"

c'getMouseDelta :: IO (Ptr Vector2)
c'getMouseDelta = callRaylibFunction "_GetMouseDelta_"

c'setMousePosition :: CInt -> CInt -> IO ()
c'setMousePosition = callRaylibFunction "_SetMousePosition_"

c'setMouseOffset :: CInt -> CInt -> IO ()
c'setMouseOffset = callRaylibFunction "_SetMouseOffset_"

c'setMouseScale :: CFloat -> CFloat -> IO ()
c'setMouseScale = callRaylibFunction "_SetMouseScale_"

c'getMouseWheelMove :: IO CFloat
c'getMouseWheelMove = callRaylibFunction "_GetMouseWheelMove_"

c'getMouseWheelMoveV :: IO (Ptr Vector2)
c'getMouseWheelMoveV = callRaylibFunction "_GetMouseWheelMoveV_"

c'setMouseCursor :: CInt -> IO ()
c'setMouseCursor = callRaylibFunction "_SetMouseCursor_"

c'getTouchX :: IO CInt
c'getTouchX = callRaylibFunction "_GetTouchX_"

c'getTouchY :: IO CInt
c'getTouchY = callRaylibFunction "_GetTouchY_"

c'getTouchPosition :: CInt -> IO (Ptr Vector2)
c'getTouchPosition = callRaylibFunction "_GetTouchPosition_"

c'getTouchPointId :: CInt -> IO CInt
c'getTouchPointId = callRaylibFunction "_GetTouchPointId_"

c'getTouchPointCount :: IO CInt
c'getTouchPointCount = callRaylibFunction "_GetTouchPointCount_"

c'setGesturesEnabled :: CUInt -> IO ()
c'setGesturesEnabled = callRaylibFunction "_SetGesturesEnabled_"

c'isGestureDetected :: CUInt -> IO CBool
c'isGestureDetected = callRaylibFunction "_IsGestureDetected_"

c'getGestureDetected :: IO CInt
c'getGestureDetected = callRaylibFunction "_GetGestureDetected_"

c'getGestureHoldDuration :: IO CFloat
c'getGestureHoldDuration = callRaylibFunction "_GetGestureHoldDuration_"

c'getGestureDragVector :: IO (Ptr Vector2)
c'getGestureDragVector = callRaylibFunction "_GetGestureDragVector_"

c'getGestureDragAngle :: IO CFloat
c'getGestureDragAngle = callRaylibFunction "_GetGestureDragAngle_"

c'getGesturePinchVector :: IO (Ptr Vector2)
c'getGesturePinchVector = callRaylibFunction "_GetGesturePinchVector_"

c'getGesturePinchAngle :: IO CFloat
c'getGesturePinchAngle = callRaylibFunction "_GetGesturePinchAngle_"

c'updateCamera :: Ptr Camera3D -> CInt -> IO ()
c'updateCamera = callRaylibFunction "_UpdateCamera_"

c'updateCameraPro :: Ptr Camera3D -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> IO ()
c'updateCameraPro = callRaylibFunction "_UpdateCameraPro_"

c'setShapesTexture :: Ptr Texture -> Ptr Rectangle -> IO ()
c'setShapesTexture = callRaylibFunction "_SetShapesTexture_"

c'getShapesTexture :: IO (Ptr Texture)
c'getShapesTexture = callRaylibFunction "_GetShapesTexture_"

c'getShapesTextureRectangle :: IO (Ptr Rectangle)
c'getShapesTextureRectangle = callRaylibFunction "_GetShapesTextureRectangle_"

c'drawPixel :: CInt -> CInt -> Ptr Color -> IO ()
c'drawPixel = callRaylibFunction "_DrawPixel_"

c'drawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()
c'drawPixelV = callRaylibFunction "_DrawPixelV_"

c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'drawLine = callRaylibFunction "_DrawLine_"

c'drawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawLineV = callRaylibFunction "_DrawLineV_"

c'drawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawLineEx = callRaylibFunction "_DrawLineEx_"

c'drawLineStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()
c'drawLineStrip = callRaylibFunction "_DrawLineStrip_"

c'drawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawLineBezier = callRaylibFunction "_DrawLineBezier_"

c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawCircle = callRaylibFunction "_DrawCircle_"

c'drawCircleSector :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawCircleSector = callRaylibFunction "_DrawCircleSector_"

c'drawCircleSectorLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawCircleSectorLines = callRaylibFunction "_DrawCircleSectorLines_"

c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()
c'drawCircleGradient = callRaylibFunction "_DrawCircleGradient_"

c'drawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawCircleV = callRaylibFunction "_DrawCircleV_"

c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawCircleLines = callRaylibFunction "_DrawCircleLines_"

c'drawCircleLinesV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawCircleLinesV = callRaylibFunction "_DrawCircleLinesV_"

c'drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawEllipse = callRaylibFunction "_DrawEllipse_"

c'drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawEllipseLines = callRaylibFunction "_DrawEllipseLines_"

c'drawRing :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawRing = callRaylibFunction "_DrawRing_"

c'drawRingLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawRingLines = callRaylibFunction "_DrawRingLines_"

c'drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'drawRectangle = callRaylibFunction "_DrawRectangle_"

c'drawRectangleV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawRectangleV = callRaylibFunction "_DrawRectangleV_"

c'drawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()
c'drawRectangleRec = callRaylibFunction "_DrawRectangleRec_"

c'drawRectanglePro :: Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawRectanglePro = callRaylibFunction "_DrawRectanglePro_"

c'drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()
c'drawRectangleGradientV = callRaylibFunction "_DrawRectangleGradientV_"

c'drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()
c'drawRectangleGradientH = callRaylibFunction "_DrawRectangleGradientH_"

c'drawRectangleGradientEx :: Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()
c'drawRectangleGradientEx = callRaylibFunction "_DrawRectangleGradientEx_"

c'drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'drawRectangleLines = callRaylibFunction "_DrawRectangleLines_"

c'drawRectangleLinesEx :: Ptr Rectangle -> CFloat -> Ptr Color -> IO ()
c'drawRectangleLinesEx = callRaylibFunction "_DrawRectangleLinesEx_"

c'drawRectangleRounded :: Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawRectangleRounded = callRaylibFunction "_DrawRectangleRounded_"

c'drawRectangleRoundedLines :: Ptr Rectangle -> CFloat -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawRectangleRoundedLines = callRaylibFunction "_DrawRectangleRoundedLines_"

c'drawTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawTriangle = callRaylibFunction "_DrawTriangle_"

c'drawTriangleLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawTriangleLines = callRaylibFunction "_DrawTriangleLines_"

c'drawTriangleFan :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()
c'drawTriangleFan = callRaylibFunction "_DrawTriangleFan_"

c'drawTriangleStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()
c'drawTriangleStrip = callRaylibFunction "_DrawTriangleStrip_"

c'drawPoly :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawPoly = callRaylibFunction "_DrawPoly_"

c'drawPolyLines :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawPolyLines = callRaylibFunction "_DrawPolyLines_"

c'drawPolyLinesEx :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawPolyLinesEx = callRaylibFunction "_DrawPolyLinesEx_"

c'drawSplineLinear :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawSplineLinear = callRaylibFunction "_DrawSplineLinear_"

c'drawSplineBasis :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawSplineBasis = callRaylibFunction "_DrawSplineBasis_"

c'drawSplineCatmullRom :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawSplineCatmullRom = callRaylibFunction "_DrawSplineCatmullRom_"

c'drawSplineBezierQuadratic :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawSplineBezierQuadratic = callRaylibFunction "_DrawSplineBezierQuadratic_"

c'drawSplineBezierCubic :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()
c'drawSplineBezierCubic = callRaylibFunction "_DrawSplineBezierCubic_"

c'drawSplineSegmentLinear :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawSplineSegmentLinear = callRaylibFunction "_DrawSplineSegmentLinear_"

c'drawSplineSegmentBasis :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawSplineSegmentBasis = callRaylibFunction "_DrawSplineSegmentBasis_"

c'drawSplineSegmentCatmullRom :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawSplineSegmentCatmullRom = callRaylibFunction "_DrawSplineSegmentCatmullRom_"

c'drawSplineSegmentBezierQuadratic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawSplineSegmentBezierQuadratic = callRaylibFunction "_DrawSplineSegmentBezierQuadratic_"

c'drawSplineSegmentBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawSplineSegmentBezierCubic = callRaylibFunction "_DrawSplineSegmentBezierCubic_"

c'getSplinePointLinear :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)
c'getSplinePointLinear = callRaylibFunction "_GetSplinePointLinear_"

c'getSplinePointBasis :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)
c'getSplinePointBasis = callRaylibFunction "_GetSplinePointBasis_"

c'getSplinePointCatmullRom :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)
c'getSplinePointCatmullRom = callRaylibFunction "_GetSplinePointCatmullRom_"

c'getSplinePointBezierQuad :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)
c'getSplinePointBezierQuad = callRaylibFunction "_GetSplinePointBezierQuad_"

c'getSplinePointBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)
c'getSplinePointBezierCubic = callRaylibFunction "_GetSplinePointBezierCubic_"

c'checkCollisionRecs :: Ptr Rectangle -> Ptr Rectangle -> IO CBool
c'checkCollisionRecs = callRaylibFunction "_CheckCollisionRecs_"

c'checkCollisionCircles :: Ptr Vector2 -> CFloat -> Ptr Vector2 -> CFloat -> IO CBool
c'checkCollisionCircles = callRaylibFunction "_CheckCollisionCircles_"

c'checkCollisionCircleRec :: Ptr Vector2 -> CFloat -> Ptr Rectangle -> IO CBool
c'checkCollisionCircleRec = callRaylibFunction "_CheckCollisionCircleRec_"

c'checkCollisionPointRec :: Ptr Vector2 -> Ptr Rectangle -> IO CBool
c'checkCollisionPointRec = callRaylibFunction "_CheckCollisionPointRec_"

c'checkCollisionPointCircle :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO CBool
c'checkCollisionPointCircle = callRaylibFunction "_CheckCollisionPointCircle_"

c'checkCollisionPointTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CBool
c'checkCollisionPointTriangle = callRaylibFunction "_CheckCollisionPointTriangle_"

c'checkCollisionPointPoly :: Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CBool
c'checkCollisionPointPoly = callRaylibFunction "_CheckCollisionPointPoly_"

c'checkCollisionLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CBool
c'checkCollisionLines = callRaylibFunction "_CheckCollisionLines_"

c'checkCollisionPointLine :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CBool
c'checkCollisionPointLine = callRaylibFunction "_CheckCollisionPointLine_"

c'getCollisionRec :: Ptr Rectangle -> Ptr Rectangle -> IO (Ptr Rectangle)
c'getCollisionRec = callRaylibFunction "_GetCollisionRec_"

c'loadImage :: CString -> IO (Ptr Image)
c'loadImage = callRaylibFunction "_LoadImage_"

c'loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)
c'loadImageRaw = callRaylibFunction "_LoadImageRaw_"

c'loadImageSvg :: CString -> CInt -> CInt -> IO (Ptr Image)
c'loadImageSvg = callRaylibFunction "_LoadImageSvg_"

c'loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Image)
c'loadImageAnim = callRaylibFunction "_LoadImageAnim_"

c'loadImageAnimFromMemory :: CString -> Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr Image)
c'loadImageAnimFromMemory = callRaylibFunction "_LoadImageAnimFromMemory_"

c'loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Image)
c'loadImageFromMemory = callRaylibFunction "_LoadImageFromMemory_"

c'loadImageFromTexture :: Ptr Texture -> IO (Ptr Image)
c'loadImageFromTexture = callRaylibFunction "_LoadImageFromTexture_"

c'loadImageFromScreen :: IO (Ptr Image)
c'loadImageFromScreen = callRaylibFunction "_LoadImageFromScreen_"

c'isImageReady :: Ptr Image -> IO CBool
c'isImageReady = callRaylibFunction "_IsImageReady_"

c'unloadImage :: Ptr Image -> IO ()
c'unloadImage = callRaylibFunction "_UnloadImage_"

c'exportImage :: Ptr Image -> CString -> IO CBool
c'exportImage = callRaylibFunction "_ExportImage_"

c'exportImageToMemory :: Ptr Image -> CString -> Ptr CInt -> IO (Ptr CUChar)
c'exportImageToMemory = callRaylibFunction "_ExportImageToMemory_"

c'exportImageAsCode :: Ptr Image -> CString -> IO CBool
c'exportImageAsCode = callRaylibFunction "_ExportImageAsCode_"

c'genImageColor :: CInt -> CInt -> Ptr Color -> IO (Ptr Image)
c'genImageColor = callRaylibFunction "_GenImageColor_"

c'genImageGradientLinear :: CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)
c'genImageGradientLinear = callRaylibFunction "_GenImageGradientLinear_"

c'genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)
c'genImageGradientRadial = callRaylibFunction "_GenImageGradientRadial_"

c'genImageGradientSquare :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)
c'genImageGradientSquare = callRaylibFunction "_GenImageGradientSquare_"

c'genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)
c'genImageChecked = callRaylibFunction "_GenImageChecked_"

c'genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Image)
c'genImageWhiteNoise = callRaylibFunction "_GenImageWhiteNoise_"

c'genImagePerlinNoise :: CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr Image)
c'genImagePerlinNoise = callRaylibFunction "_GenImagePerlinNoise_"

c'genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Image)
c'genImageCellular = callRaylibFunction "_GenImageCellular_"

c'genImageText :: CInt -> CInt -> CString -> IO (Ptr Image)
c'genImageText = callRaylibFunction "_GenImageText_"

c'imageCopy :: Ptr Image -> IO (Ptr Image)
c'imageCopy = callRaylibFunction "_ImageCopy_"

c'imageFromImage :: Ptr Image -> Ptr Rectangle -> IO (Ptr Image)
c'imageFromImage = callRaylibFunction "_ImageFromImage_"

c'imageText :: CString -> CInt -> Ptr Color -> IO (Ptr Image)
c'imageText = callRaylibFunction "_ImageText_"

c'imageTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> Ptr Color -> IO (Ptr Image)
c'imageTextEx = callRaylibFunction "_ImageTextEx_"

c'imageFormat :: Ptr Image -> CInt -> IO ()
c'imageFormat = callRaylibFunction "_ImageFormat_"

c'imageToPOT :: Ptr Image -> Ptr Color -> IO ()
c'imageToPOT = callRaylibFunction "_ImageToPOT_"

c'imageCrop :: Ptr Image -> Ptr Rectangle -> IO ()
c'imageCrop = callRaylibFunction "_ImageCrop_"

c'imageAlphaCrop :: Ptr Image -> CFloat -> IO ()
c'imageAlphaCrop = callRaylibFunction "_ImageAlphaCrop_"

c'imageAlphaClear :: Ptr Image -> Ptr Color -> CFloat -> IO ()
c'imageAlphaClear = callRaylibFunction "_ImageAlphaClear_"

c'imageAlphaMask :: Ptr Image -> Ptr Image -> IO ()
c'imageAlphaMask = callRaylibFunction "_ImageAlphaMask_"

c'imageAlphaPremultiply :: Ptr Image -> IO ()
c'imageAlphaPremultiply = callRaylibFunction "_ImageAlphaPremultiply_"

c'imageBlurGaussian :: Ptr Image -> CInt -> IO ()
c'imageBlurGaussian = callRaylibFunction "_ImageBlurGaussian_"

c'imageKernelConvolution :: Ptr Image -> Ptr CFloat -> CInt -> IO ()
c'imageKernelConvolution = callRaylibFunction "_ImageKernelConvolution_"

c'imageResize :: Ptr Image -> CInt -> CInt -> IO ()
c'imageResize = callRaylibFunction "_ImageResize_"

c'imageResizeNN :: Ptr Image -> CInt -> CInt -> IO ()
c'imageResizeNN = callRaylibFunction "_ImageResizeNN_"

c'imageResizeCanvas :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'imageResizeCanvas = callRaylibFunction "_ImageResizeCanvas_"

c'imageMipmaps :: Ptr Image -> IO ()
c'imageMipmaps = callRaylibFunction "_ImageMipmaps_"

c'imageDither :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()
c'imageDither = callRaylibFunction "_ImageDither_"

c'imageFlipVertical :: Ptr Image -> IO ()
c'imageFlipVertical = callRaylibFunction "_ImageFlipVertical_"

c'imageFlipHorizontal :: Ptr Image -> IO ()
c'imageFlipHorizontal = callRaylibFunction "_ImageFlipHorizontal_"

c'imageRotate :: Ptr Image -> CInt -> IO ()
c'imageRotate = callRaylibFunction "_ImageRotate_"

c'imageRotateCW :: Ptr Image -> IO ()
c'imageRotateCW = callRaylibFunction "_ImageRotateCW_"

c'imageRotateCCW :: Ptr Image -> IO ()
c'imageRotateCCW = callRaylibFunction "_ImageRotateCCW_"

c'imageColorTint :: Ptr Image -> Ptr Color -> IO ()
c'imageColorTint = callRaylibFunction "_ImageColorTint_"

c'imageColorInvert :: Ptr Image -> IO ()
c'imageColorInvert = callRaylibFunction "_ImageColorInvert_"

c'imageColorGrayscale :: Ptr Image -> IO ()
c'imageColorGrayscale = callRaylibFunction "_ImageColorGrayscale_"

c'imageColorContrast :: Ptr Image -> CFloat -> IO ()
c'imageColorContrast = callRaylibFunction "_ImageColorContrast_"

c'imageColorBrightness :: Ptr Image -> CInt -> IO ()
c'imageColorBrightness = callRaylibFunction "_ImageColorBrightness_"

c'imageColorReplace :: Ptr Image -> Ptr Color -> Ptr Color -> IO ()
c'imageColorReplace = callRaylibFunction "_ImageColorReplace_"

c'loadImageColors :: Ptr Image -> IO (Ptr Color)
c'loadImageColors = callRaylibFunction "_LoadImageColors_"

c'loadImagePalette :: Ptr Image -> CInt -> Ptr CInt -> IO (Ptr Color)
c'loadImagePalette = callRaylibFunction "_LoadImagePalette_"

c'getImageAlphaBorder :: Ptr Image -> CFloat -> IO (Ptr Rectangle)
c'getImageAlphaBorder = callRaylibFunction "_GetImageAlphaBorder_"

c'getImageColor :: Ptr Image -> CInt -> CInt -> IO (Ptr Color)
c'getImageColor = callRaylibFunction "_GetImageColor_"

c'imageClearBackground :: Ptr Image -> Ptr Color -> IO ()
c'imageClearBackground = callRaylibFunction "_ImageClearBackground_"

c'imageDrawPixel :: Ptr Image -> CInt -> CInt -> Ptr Color -> IO ()
c'imageDrawPixel = callRaylibFunction "_ImageDrawPixel_"

c'imageDrawPixelV :: Ptr Image -> Ptr Vector2 -> Ptr Color -> IO ()
c'imageDrawPixelV = callRaylibFunction "_ImageDrawPixelV_"

c'imageDrawLine :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'imageDrawLine = callRaylibFunction "_ImageDrawLine_"

c'imageDrawLineV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
c'imageDrawLineV = callRaylibFunction "_ImageDrawLineV_"

c'imageDrawCircle :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'imageDrawCircle = callRaylibFunction "_ImageDrawCircle_"

c'imageDrawCircleV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()
c'imageDrawCircleV = callRaylibFunction "_ImageDrawCircleV_"

c'imageDrawCircleLines :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'imageDrawCircleLines = callRaylibFunction "_ImageDrawCircleLines_"

c'imageDrawCircleLinesV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()
c'imageDrawCircleLinesV = callRaylibFunction "_ImageDrawCircleLinesV_"

c'imageDrawRectangle :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'imageDrawRectangle = callRaylibFunction "_ImageDrawRectangle_"

c'imageDrawRectangleV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
c'imageDrawRectangleV = callRaylibFunction "_ImageDrawRectangleV_"

c'imageDrawRectangleRec :: Ptr Image -> Ptr Rectangle -> Ptr Color -> IO ()
c'imageDrawRectangleRec = callRaylibFunction "_ImageDrawRectangleRec_"

c'imageDrawRectangleLines :: Ptr Image -> Ptr Rectangle -> CInt -> Ptr Color -> IO ()
c'imageDrawRectangleLines = callRaylibFunction "_ImageDrawRectangleLines_"

c'imageDraw :: Ptr Image -> Ptr Image -> Ptr Rectangle -> Ptr Rectangle -> Ptr Color -> IO ()
c'imageDraw = callRaylibFunction "_ImageDraw_"

c'imageDrawText :: Ptr Image -> CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'imageDrawText = callRaylibFunction "_ImageDrawText_"

c'imageDrawTextEx :: Ptr Image -> Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
c'imageDrawTextEx = callRaylibFunction "_ImageDrawTextEx_"

c'loadTexture :: CString -> IO (Ptr Texture)
c'loadTexture = callRaylibFunction "_LoadTexture_"

c'loadTextureFromImage :: Ptr Image -> IO (Ptr Texture)
c'loadTextureFromImage = callRaylibFunction "_LoadTextureFromImage_"

c'loadTextureCubemap :: Ptr Image -> CInt -> IO (Ptr Texture)
c'loadTextureCubemap = callRaylibFunction "_LoadTextureCubemap_"

c'loadRenderTexture :: CInt -> CInt -> IO (Ptr RenderTexture)
c'loadRenderTexture = callRaylibFunction "_LoadRenderTexture_"

c'isTextureReady :: Ptr Texture -> IO CBool
c'isTextureReady = callRaylibFunction "_IsTextureReady_"

c'unloadTexture :: Ptr Texture -> IO ()
c'unloadTexture = callRaylibFunction "_UnloadTexture_"

c'isRenderTextureReady :: Ptr RenderTexture -> IO CBool
c'isRenderTextureReady = callRaylibFunction "_IsRenderTextureReady_"

c'unloadRenderTexture :: Ptr RenderTexture -> IO ()
c'unloadRenderTexture = callRaylibFunction "_UnloadRenderTexture_"

c'updateTexture :: Ptr Texture -> Ptr () -> IO ()
c'updateTexture = callRaylibFunction "_UpdateTexture_"

c'updateTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr () -> IO ()
c'updateTextureRec = callRaylibFunction "_UpdateTextureRec_"

c'genTextureMipmaps :: Ptr Texture -> IO ()
c'genTextureMipmaps = callRaylibFunction "_GenTextureMipmaps_"

c'setTextureFilter :: Ptr Texture -> CInt -> IO ()
c'setTextureFilter = callRaylibFunction "_SetTextureFilter_"

c'setTextureWrap :: Ptr Texture -> CInt -> IO ()
c'setTextureWrap = callRaylibFunction "_SetTextureWrap_"

c'drawTexture :: Ptr Texture -> CInt -> CInt -> Ptr Color -> IO ()
c'drawTexture = callRaylibFunction "_DrawTexture_"

c'drawTextureV :: Ptr Texture -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawTextureV = callRaylibFunction "_DrawTextureV_"

c'drawTextureEx :: Ptr Texture -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawTextureEx = callRaylibFunction "_DrawTextureEx_"

c'drawTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawTextureRec = callRaylibFunction "_DrawTextureRec_"

c'drawTexturePro :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawTexturePro = callRaylibFunction "_DrawTexturePro_"

c'drawTextureNPatch :: Ptr Texture -> Ptr NPatchInfo -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawTextureNPatch = callRaylibFunction "_DrawTextureNPatch_"

c'fade :: Ptr Color -> CFloat -> IO (Ptr Color)
c'fade = callRaylibFunction "_Fade_"

c'colorToInt :: Ptr Color -> IO CInt
c'colorToInt = callRaylibFunction "_ColorToInt_"

c'colorNormalize :: Ptr Color -> IO (Ptr Vector4)
c'colorNormalize = callRaylibFunction "_ColorNormalize_"

c'colorFromNormalized :: Ptr Vector4 -> IO (Ptr Color)
c'colorFromNormalized = callRaylibFunction "_ColorFromNormalized_"

c'colorToHSV :: Ptr Color -> IO (Ptr Vector3)
c'colorToHSV = callRaylibFunction "_ColorToHSV_"

c'colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Color)
c'colorFromHSV = callRaylibFunction "_ColorFromHSV_"

c'colorTint :: Ptr Color -> Ptr Color -> IO (Ptr Color)
c'colorTint = callRaylibFunction "_ColorTint_"

c'colorBrightness :: Ptr Color -> CFloat -> IO (Ptr Color)
c'colorBrightness = callRaylibFunction "_ColorBrightness_"

c'colorContrast :: Ptr Color -> CFloat -> IO (Ptr Color)
c'colorContrast = callRaylibFunction "_ColorContrast_"

c'colorAlpha :: Ptr Color -> CFloat -> IO (Ptr Color)
c'colorAlpha = callRaylibFunction "_ColorAlpha_"

c'colorAlphaBlend :: Ptr Color -> Ptr Color -> Ptr Color -> IO (Ptr Color)
c'colorAlphaBlend = callRaylibFunction "_ColorAlphaBlend_"

c'getColor :: CUInt -> IO (Ptr Color)
c'getColor = callRaylibFunction "_GetColor_"

c'getPixelColor :: Ptr () -> CInt -> IO (Ptr Color)
c'getPixelColor = callRaylibFunction "_GetPixelColor_"

c'setPixelColor :: Ptr () -> Ptr Color -> CInt -> IO ()
c'setPixelColor = callRaylibFunction "_SetPixelColor_"

c'getFontDefault :: IO (Ptr Font)
c'getFontDefault = callRaylibFunction "_GetFontDefault_"

c'loadFont :: CString -> IO (Ptr Font)
c'loadFont = callRaylibFunction "_LoadFont_"

c'loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)
c'loadFontEx = callRaylibFunction "_LoadFontEx_"

c'loadFontFromImage :: Ptr Image -> Ptr Color -> CInt -> IO (Ptr Font)
c'loadFontFromImage = callRaylibFunction "_LoadFontFromImage_"

c'loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)
c'loadFontFromMemory = callRaylibFunction "_LoadFontFromMemory_"

c'loadFontData :: Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)
c'loadFontData = callRaylibFunction "_LoadFontData_"

c'genImageFontAtlas :: Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)
c'genImageFontAtlas = callRaylibFunction "_GenImageFontAtlas_"

c'unloadFontData :: Ptr GlyphInfo -> CInt -> IO ()
c'unloadFontData = callRaylibFunction "_UnloadFontData_"

c'isFontReady :: Ptr Font -> IO CBool
c'isFontReady = callRaylibFunction "_IsFontReady_"

c'unloadFont :: Ptr Font -> IO ()
c'unloadFont = callRaylibFunction "_UnloadFont_"

c'exportFontAsCode :: Ptr Font -> CString -> IO CBool
c'exportFontAsCode = callRaylibFunction "_ExportFontAsCode_"

c'drawFPS :: CInt -> CInt -> IO ()
c'drawFPS = callRaylibFunction "_DrawFPS_"

c'drawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
c'drawText = callRaylibFunction "_DrawText_"

c'drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawTextEx = callRaylibFunction "_DrawTextEx_"

c'drawTextPro :: Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawTextPro = callRaylibFunction "_DrawTextPro_"

c'drawTextCodepoint :: Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawTextCodepoint = callRaylibFunction "_DrawTextCodepoint_"

c'drawTextCodepoints :: Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawTextCodepoints = callRaylibFunction "_DrawTextCodepoints_"

c'setTextLineSpacing :: CInt -> IO ()
c'setTextLineSpacing = callRaylibFunction "_SetTextLineSpacing_"

c'measureText :: CString -> CInt -> IO CInt
c'measureText = callRaylibFunction "_MeasureText_"

c'measureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)
c'measureTextEx = callRaylibFunction "_MeasureTextEx_"

c'getGlyphIndex :: Ptr Font -> CInt -> IO CInt
c'getGlyphIndex = callRaylibFunction "_GetGlyphIndex_"

c'getGlyphInfo :: Ptr Font -> CInt -> IO (Ptr GlyphInfo)
c'getGlyphInfo = callRaylibFunction "_GetGlyphInfo_"

c'getGlyphAtlasRec :: Ptr Font -> CInt -> IO (Ptr Rectangle)
c'getGlyphAtlasRec = callRaylibFunction "_GetGlyphAtlasRec_"

c'loadUTF8 :: Ptr CInt -> CInt -> IO CString
c'loadUTF8 = callRaylibFunction "_LoadUTF8_"

c'loadCodepoints :: CString -> Ptr CInt -> IO (Ptr CInt)
c'loadCodepoints = callRaylibFunction "_LoadCodepoints_"

c'getCodepointCount :: CString -> IO CInt
c'getCodepointCount = callRaylibFunction "_GetCodepointCount_"

c'getCodepointNext :: CString -> Ptr CInt -> IO CInt
c'getCodepointNext = callRaylibFunction "_GetCodepointNext_"

c'getCodepointPrevious :: CString -> Ptr CInt -> IO CInt
c'getCodepointPrevious = callRaylibFunction "_GetCodepointPrevious_"

c'codepointToUTF8 :: CInt -> Ptr CInt -> IO CString
c'codepointToUTF8 = callRaylibFunction "_CodepointToUTF8_"

c'drawLine3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()
c'drawLine3D = callRaylibFunction "_DrawLine3D_"

c'drawPoint3D :: Ptr Vector3 -> Ptr Color -> IO ()
c'drawPoint3D = callRaylibFunction "_DrawPoint3D_"

c'drawCircle3D :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
c'drawCircle3D = callRaylibFunction "_DrawCircle3D_"

c'drawTriangle3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()
c'drawTriangle3D = callRaylibFunction "_DrawTriangle3D_"

c'drawTriangleStrip3D :: Ptr Vector3 -> CInt -> Ptr Color -> IO ()
c'drawTriangleStrip3D = callRaylibFunction "_DrawTriangleStrip3D_"

c'drawCube :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawCube = callRaylibFunction "_DrawCube_"

c'drawCubeV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()
c'drawCubeV = callRaylibFunction "_DrawCubeV_"

c'drawCubeWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()
c'drawCubeWires = callRaylibFunction "_DrawCubeWires_"

c'drawCubeWiresV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()
c'drawCubeWiresV = callRaylibFunction "_DrawCubeWiresV_"

c'drawSphere :: Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
c'drawSphere = callRaylibFunction "_DrawSphere_"

c'drawSphereEx :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()
c'drawSphereEx = callRaylibFunction "_DrawSphereEx_"

c'drawSphereWires :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()
c'drawSphereWires = callRaylibFunction "_DrawSphereWires_"

c'drawCylinder :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawCylinder = callRaylibFunction "_DrawCylinder_"

c'drawCylinderEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawCylinderEx = callRaylibFunction "_DrawCylinderEx_"

c'drawCylinderWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawCylinderWires = callRaylibFunction "_DrawCylinderWires_"

c'drawCylinderWiresEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()
c'drawCylinderWiresEx = callRaylibFunction "_DrawCylinderWiresEx_"

c'drawCapsule :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()
c'drawCapsule = callRaylibFunction "_DrawCapsule_"

c'drawCapsuleWires :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()
c'drawCapsuleWires = callRaylibFunction "_DrawCapsuleWires_"

c'drawPlane :: Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawPlane = callRaylibFunction "_DrawPlane_"

c'drawRay :: Ptr Ray -> Ptr Color -> IO ()
c'drawRay = callRaylibFunction "_DrawRay_"

c'drawGrid :: CInt -> CFloat -> IO ()
c'drawGrid = callRaylibFunction "_DrawGrid_"

c'loadModel :: CString -> IO (Ptr Model)
c'loadModel = callRaylibFunction "_LoadModel_"

c'loadModelFromMesh :: Ptr Mesh -> IO (Ptr Model)
c'loadModelFromMesh = callRaylibFunction "_LoadModelFromMesh_"

c'isModelReady :: Ptr Model -> IO CBool
c'isModelReady = callRaylibFunction "_IsModelReady_"

c'unloadModel :: Ptr Model -> IO ()
c'unloadModel = callRaylibFunction "_UnloadModel_"

c'getModelBoundingBox :: Ptr Model -> IO (Ptr BoundingBox)
c'getModelBoundingBox = callRaylibFunction "_GetModelBoundingBox_"

c'drawModel :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
c'drawModel = callRaylibFunction "_DrawModel_"

c'drawModelEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()
c'drawModelEx = callRaylibFunction "_DrawModelEx_"

c'drawModelWires :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
c'drawModelWires = callRaylibFunction "_DrawModelWires_"

c'drawModelWiresEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()
c'drawModelWiresEx = callRaylibFunction "_DrawModelWiresEx_"

c'drawBoundingBox :: Ptr BoundingBox -> Ptr Color -> IO ()
c'drawBoundingBox = callRaylibFunction "_DrawBoundingBox_"

c'drawBillboard :: Ptr Camera3D -> Ptr Texture -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
c'drawBillboard = callRaylibFunction "_DrawBillboard_"

c'drawBillboardRec :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()
c'drawBillboardRec = callRaylibFunction "_DrawBillboardRec_"

c'drawBillboardPro :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
c'drawBillboardPro = callRaylibFunction "_DrawBillboardPro_"

c'uploadMesh :: Ptr Mesh -> CInt -> IO ()
c'uploadMesh = callRaylibFunction "_UploadMesh_"

c'updateMeshBuffer :: Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()
c'updateMeshBuffer = callRaylibFunction "_UpdateMeshBuffer_"

c'unloadMesh :: Ptr Mesh -> IO ()
c'unloadMesh = callRaylibFunction "_UnloadMesh_"

c'drawMesh :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()
c'drawMesh = callRaylibFunction "_DrawMesh_"

c'drawMeshInstanced :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()
c'drawMeshInstanced = callRaylibFunction "_DrawMeshInstanced_"

c'exportMesh :: Ptr Mesh -> CString -> IO CBool
c'exportMesh = callRaylibFunction "_ExportMesh_"

c'exportMeshAsCode :: Ptr Mesh -> CString -> IO CBool
c'exportMeshAsCode = callRaylibFunction "_ExportMeshAsCode_"

c'getMeshBoundingBox :: Ptr Mesh -> IO (Ptr BoundingBox)
c'getMeshBoundingBox = callRaylibFunction "_GetMeshBoundingBox_"

c'genMeshTangents :: Ptr Mesh -> IO ()
c'genMeshTangents = callRaylibFunction "_GenMeshTangents_"

c'genMeshPoly :: CInt -> CFloat -> IO (Ptr Mesh)
c'genMeshPoly = callRaylibFunction "_GenMeshPoly_"

c'genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)
c'genMeshPlane = callRaylibFunction "_GenMeshPlane_"

c'genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)
c'genMeshCube = callRaylibFunction "_GenMeshCube_"

c'genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)
c'genMeshSphere = callRaylibFunction "_GenMeshSphere_"

c'genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)
c'genMeshHemiSphere = callRaylibFunction "_GenMeshHemiSphere_"

c'genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)
c'genMeshCylinder = callRaylibFunction "_GenMeshCylinder_"

c'genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)
c'genMeshCone = callRaylibFunction "_GenMeshCone_"

c'genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)
c'genMeshTorus = callRaylibFunction "_GenMeshTorus_"

c'genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)
c'genMeshKnot = callRaylibFunction "_GenMeshKnot_"

c'genMeshHeightmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)
c'genMeshHeightmap = callRaylibFunction "_GenMeshHeightmap_"

c'genMeshCubicmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)
c'genMeshCubicmap = callRaylibFunction "_GenMeshCubicmap_"

c'loadMaterials :: CString -> Ptr CInt -> IO (Ptr Material)
c'loadMaterials = callRaylibFunction "_LoadMaterials_"

c'loadMaterialDefault :: IO (Ptr Material)
c'loadMaterialDefault = callRaylibFunction "_LoadMaterialDefault_"

c'isMaterialReady :: Ptr Material -> IO CBool
c'isMaterialReady = callRaylibFunction "_IsMaterialReady_"

c'unloadMaterial :: Ptr Material -> IO ()
c'unloadMaterial = callRaylibFunction "_UnloadMaterial_"

c'setMaterialTexture :: Ptr Material -> CInt -> Ptr Texture -> IO ()
c'setMaterialTexture = callRaylibFunction "_SetMaterialTexture_"

c'setModelMeshMaterial :: Ptr Model -> CInt -> CInt -> IO ()
c'setModelMeshMaterial = callRaylibFunction "_SetModelMeshMaterial_"

c'loadModelAnimations :: CString -> Ptr CInt -> IO (Ptr ModelAnimation)
c'loadModelAnimations = callRaylibFunction "_LoadModelAnimations_"

c'updateModelAnimation :: Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()
c'updateModelAnimation = callRaylibFunction "_UpdateModelAnimation_"

c'unloadModelAnimation :: Ptr ModelAnimation -> IO ()
c'unloadModelAnimation = callRaylibFunction "_UnloadModelAnimation_"

c'unloadModelAnimations :: Ptr ModelAnimation -> CInt -> IO ()
c'unloadModelAnimations = callRaylibFunction "_UnloadModelAnimations_"

c'isModelAnimationValid :: Ptr Model -> Ptr ModelAnimation -> IO CBool
c'isModelAnimationValid = callRaylibFunction "_IsModelAnimationValid_"

c'checkCollisionSpheres :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> IO CBool
c'checkCollisionSpheres = callRaylibFunction "_CheckCollisionSpheres_"

c'checkCollisionBoxes :: Ptr BoundingBox -> Ptr BoundingBox -> IO CBool
c'checkCollisionBoxes = callRaylibFunction "_CheckCollisionBoxes_"

c'checkCollisionBoxSphere :: Ptr BoundingBox -> Ptr Vector3 -> CFloat -> IO CBool
c'checkCollisionBoxSphere = callRaylibFunction "_CheckCollisionBoxSphere_"

c'getRayCollisionSphere :: Ptr Ray -> Ptr Vector3 -> CFloat -> IO (Ptr RayCollision)
c'getRayCollisionSphere = callRaylibFunction "_GetRayCollisionSphere_"

c'getRayCollisionBox :: Ptr Ray -> Ptr BoundingBox -> IO (Ptr RayCollision)
c'getRayCollisionBox = callRaylibFunction "_GetRayCollisionBox_"

c'getRayCollisionMesh :: Ptr Ray -> Ptr Mesh -> Ptr Matrix -> IO (Ptr RayCollision)
c'getRayCollisionMesh = callRaylibFunction "_GetRayCollisionMesh_"

c'getRayCollisionTriangle :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)
c'getRayCollisionTriangle = callRaylibFunction "_GetRayCollisionTriangle_"

c'getRayCollisionQuad :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)
c'getRayCollisionQuad = callRaylibFunction "_GetRayCollisionQuad_"


c'initAudioDevice :: IO ()
c'initAudioDevice = callRaylibFunction "_InitAudioDevice_"

c'closeAudioDevice :: IO ()
c'closeAudioDevice = callRaylibFunction "_CloseAudioDevice_"

c'isAudioDeviceReady :: IO CBool
c'isAudioDeviceReady = callRaylibFunction "_IsAudioDeviceReady_"

c'setMasterVolume :: CFloat -> IO ()
c'setMasterVolume = callRaylibFunction "_SetMasterVolume_"

c'getMasterVolume :: IO CFloat
c'getMasterVolume = callRaylibFunction "_GetMasterVolume_"

c'loadWave :: CString -> IO (Ptr Wave)
c'loadWave = callRaylibFunction "_LoadWave_"

c'loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Wave)
c'loadWaveFromMemory = callRaylibFunction "_LoadWaveFromMemory_"

c'loadSound :: CString -> IO (Ptr Sound)
c'loadSound = callRaylibFunction "_LoadSound_"

c'loadSoundFromWave :: Ptr Wave -> IO (Ptr Sound)
c'loadSoundFromWave = callRaylibFunction "_LoadSoundFromWave_"

c'loadSoundAlias :: Ptr Sound -> IO (Ptr Sound)
c'loadSoundAlias = callRaylibFunction "_LoadSoundAlias_"

c'updateSound :: Ptr Sound -> Ptr () -> CInt -> IO ()
c'updateSound = callRaylibFunction "_UpdateSound_"

c'isWaveReady :: Ptr Wave -> IO CBool
c'isWaveReady = callRaylibFunction "_IsWaveReady_"

c'unloadWave :: Ptr Wave -> IO ()
c'unloadWave = callRaylibFunction "_UnloadWave_"

c'isSoundReady :: Ptr Sound -> IO CBool
c'isSoundReady = callRaylibFunction "_IsSoundReady_"

c'unloadSound :: Ptr Sound -> IO ()
c'unloadSound = callRaylibFunction "_UnloadSound_"

c'unloadSoundAlias :: Ptr Sound -> IO ()
c'unloadSoundAlias = callRaylibFunction "_UnloadSoundAlias_"

c'exportWave :: Ptr Wave -> CString -> IO CBool
c'exportWave = callRaylibFunction "_ExportWave_"

c'exportWaveAsCode :: Ptr Wave -> CString -> IO CBool
c'exportWaveAsCode = callRaylibFunction "_ExportWaveAsCode_"

c'playSound :: Ptr Sound -> IO ()
c'playSound = callRaylibFunction "_PlaySound_"

c'stopSound :: Ptr Sound -> IO ()
c'stopSound = callRaylibFunction "_StopSound_"

c'pauseSound :: Ptr Sound -> IO ()
c'pauseSound = callRaylibFunction "_PauseSound_"

c'resumeSound :: Ptr Sound -> IO ()
c'resumeSound = callRaylibFunction "_ResumeSound_"

c'isSoundPlaying :: Ptr Sound -> IO CBool
c'isSoundPlaying = callRaylibFunction "_IsSoundPlaying_"

c'setSoundVolume :: Ptr Sound -> CFloat -> IO ()
c'setSoundVolume = callRaylibFunction "_SetSoundVolume_"

c'setSoundPitch :: Ptr Sound -> CFloat -> IO ()
c'setSoundPitch = callRaylibFunction "_SetSoundPitch_"

c'setSoundPan :: Ptr Sound -> CFloat -> IO ()
c'setSoundPan = callRaylibFunction "_SetSoundPan_"

c'waveCopy :: Ptr Wave -> IO (Ptr Wave)
c'waveCopy = callRaylibFunction "_WaveCopy_"

c'waveCrop :: Ptr Wave -> CInt -> CInt -> IO ()
c'waveCrop = callRaylibFunction "_WaveCrop_"

c'waveFormat :: Ptr Wave -> CInt -> CInt -> CInt -> IO ()
c'waveFormat = callRaylibFunction "_WaveFormat_"

c'loadWaveSamples :: Ptr Wave -> IO (Ptr CFloat)
c'loadWaveSamples = callRaylibFunction "_LoadWaveSamples_"

c'unloadWaveSamples :: Ptr CFloat -> IO ()
c'unloadWaveSamples = callRaylibFunction "_UnloadWaveSamples_"

c'loadMusicStream :: CString -> IO (Ptr Music)
c'loadMusicStream = callRaylibFunction "_LoadMusicStream_"

c'loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Music)
c'loadMusicStreamFromMemory = callRaylibFunction "_LoadMusicStreamFromMemory_"

c'isMusicReady :: Ptr Music -> IO CBool
c'isMusicReady = callRaylibFunction "_IsMusicReady_"

c'unloadMusicStream :: Ptr Music -> IO ()
c'unloadMusicStream = callRaylibFunction "_UnloadMusicStream_"

c'playMusicStream :: Ptr Music -> IO ()
c'playMusicStream = callRaylibFunction "_PlayMusicStream_"

c'isMusicStreamPlaying :: Ptr Music -> IO CBool
c'isMusicStreamPlaying = callRaylibFunction "_IsMusicStreamPlaying_"

c'updateMusicStream :: Ptr Music -> IO ()
c'updateMusicStream = callRaylibFunction "_UpdateMusicStream_"

c'stopMusicStream :: Ptr Music -> IO ()
c'stopMusicStream = callRaylibFunction "_StopMusicStream_"

c'pauseMusicStream :: Ptr Music -> IO ()
c'pauseMusicStream = callRaylibFunction "_PauseMusicStream_"

c'resumeMusicStream :: Ptr Music -> IO ()
c'resumeMusicStream = callRaylibFunction "_ResumeMusicStream_"

c'seekMusicStream :: Ptr Music -> CFloat -> IO ()
c'seekMusicStream = callRaylibFunction "_SeekMusicStream_"

c'setMusicVolume :: Ptr Music -> CFloat -> IO ()
c'setMusicVolume = callRaylibFunction "_SetMusicVolume_"

c'setMusicPitch :: Ptr Music -> CFloat -> IO ()
c'setMusicPitch = callRaylibFunction "_SetMusicPitch_"

c'setMusicPan :: Ptr Music -> CFloat -> IO ()
c'setMusicPan = callRaylibFunction "_SetMusicPan_"

c'getMusicTimeLength :: Ptr Music -> IO CFloat
c'getMusicTimeLength = callRaylibFunction "_GetMusicTimeLength_"

c'getMusicTimePlayed :: Ptr Music -> IO CFloat
c'getMusicTimePlayed = callRaylibFunction "_GetMusicTimePlayed_"

c'loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr AudioStream)
c'loadAudioStream = callRaylibFunction "_LoadAudioStream_"

c'isAudioStreamReady :: Ptr AudioStream -> IO CBool
c'isAudioStreamReady = callRaylibFunction "_IsAudioStreamReady_"

c'unloadAudioStream :: Ptr AudioStream -> IO ()
c'unloadAudioStream = callRaylibFunction "_UnloadAudioStream_"

c'updateAudioStream :: Ptr AudioStream -> Ptr () -> CInt -> IO ()
c'updateAudioStream = callRaylibFunction "_UpdateAudioStream_"

c'isAudioStreamProcessed :: Ptr AudioStream -> IO CBool
c'isAudioStreamProcessed = callRaylibFunction "_IsAudioStreamProcessed_"

c'playAudioStream :: Ptr AudioStream -> IO ()
c'playAudioStream = callRaylibFunction "_PlayAudioStream_"

c'pauseAudioStream :: Ptr AudioStream -> IO ()
c'pauseAudioStream = callRaylibFunction "_PauseAudioStream_"

c'resumeAudioStream :: Ptr AudioStream -> IO ()
c'resumeAudioStream = callRaylibFunction "_ResumeAudioStream_"

c'isAudioStreamPlaying :: Ptr AudioStream -> IO CBool
c'isAudioStreamPlaying = callRaylibFunction "_IsAudioStreamPlaying_"

c'stopAudioStream :: Ptr AudioStream -> IO ()
c'stopAudioStream = callRaylibFunction "_StopAudioStream_"

c'setAudioStreamVolume :: Ptr AudioStream -> CFloat -> IO ()
c'setAudioStreamVolume = callRaylibFunction "_SetAudioStreamVolume_"

c'setAudioStreamPitch :: Ptr AudioStream -> CFloat -> IO ()
c'setAudioStreamPitch = callRaylibFunction "_SetAudioStreamPitch_"

c'setAudioStreamPan :: Ptr AudioStream -> CFloat -> IO ()
c'setAudioStreamPan = callRaylibFunction "_SetAudioStreamPan_"

c'setAudioStreamBufferSizeDefault :: CInt -> IO ()
c'setAudioStreamBufferSizeDefault = callRaylibFunction "_SetAudioStreamBufferSizeDefault_"

c'setAudioStreamCallback :: Ptr AudioStream -> Ptr AudioCallback -> IO ()
c'setAudioStreamCallback = callRaylibFunction "_SetAudioStreamCallback_"

c'attachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()
c'attachAudioStreamProcessor = callRaylibFunction "_AttachAudioStreamProcessor_"

c'detachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()
c'detachAudioStreamProcessor = callRaylibFunction "_DetachAudioStreamProcessor_"

c'attachAudioMixedProcessor :: Ptr AudioCallback -> IO ()
c'attachAudioMixedProcessor = callRaylibFunction "_AttachAudioMixedProcessor_"

c'detachAudioMixedProcessor :: Ptr AudioCallback -> IO ()
c'detachAudioMixedProcessor = callRaylibFunction "_DetachAudioMixedProcessor_"

c'rlMatrixMode :: CInt -> IO ()
c'rlMatrixMode = callRaylibFunction "_rlMatrixMode_"

c'rlTranslatef :: CFloat -> CFloat -> CFloat -> IO ()
c'rlTranslatef = callRaylibFunction "_rlTranslatef_"

c'rlRotatef :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()
c'rlRotatef = callRaylibFunction "_rlRotatef_"

c'rlScalef :: CFloat -> CFloat -> CFloat -> IO ()
c'rlScalef = callRaylibFunction "_rlScalef_"

c'rlMultMatrixf :: Ptr CFloat -> IO ()
c'rlMultMatrixf = callRaylibFunction "_rlMultMatrixf_"

c'rlFrustum :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
c'rlFrustum = callRaylibFunction "_rlFrustum_"

c'rlOrtho :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
c'rlOrtho = callRaylibFunction "_rlOrtho_"

c'rlViewport :: CInt -> CInt -> CInt -> CInt -> IO ()
c'rlViewport = callRaylibFunction "_rlViewport_"

c'rlBegin :: CInt -> IO ()
c'rlBegin = callRaylibFunction "_rlBegin_"

c'rlVertex2i :: CInt -> CInt -> IO ()
c'rlVertex2i = callRaylibFunction "_rlVertex2i_"

c'rlVertex2f :: CFloat -> CFloat -> IO ()
c'rlVertex2f = callRaylibFunction "_rlVertex2f_"

c'rlVertex3f :: CFloat -> CFloat -> CFloat -> IO ()
c'rlVertex3f = callRaylibFunction "_rlVertex3f_"

c'rlTexCoord2f :: CFloat -> CFloat -> IO ()
c'rlTexCoord2f = callRaylibFunction "_rlTexCoord2f_"

c'rlNormal3f :: CFloat -> CFloat -> CFloat -> IO ()
c'rlNormal3f = callRaylibFunction "_rlNormal3f_"

c'rlColor4ub :: CUChar -> CUChar -> CUChar -> CUChar -> IO ()
c'rlColor4ub = callRaylibFunction "_rlColor4ub_"

c'rlColor3f :: CFloat -> CFloat -> CFloat -> IO ()
c'rlColor3f = callRaylibFunction "_rlColor3f_"

c'rlColor4f :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()
c'rlColor4f = callRaylibFunction "_rlColor4f_"

c'rlEnableVertexArray :: CUInt -> IO CBool
c'rlEnableVertexArray = callRaylibFunction "_rlEnableVertexArray_"

c'rlEnableVertexBuffer :: CUInt -> IO ()
c'rlEnableVertexBuffer = callRaylibFunction "_rlEnableVertexBuffer_"

c'rlEnableVertexBufferElement :: CUInt -> IO ()
c'rlEnableVertexBufferElement = callRaylibFunction "_rlEnableVertexBufferElement_"

c'rlEnableVertexAttribute :: CUInt -> IO ()
c'rlEnableVertexAttribute = callRaylibFunction "_rlEnableVertexAttribute_"

c'rlDisableVertexAttribute :: CUInt -> IO ()
c'rlDisableVertexAttribute = callRaylibFunction "_rlDisableVertexAttribute_"

c'rlActiveTextureSlot :: CInt -> IO ()
c'rlActiveTextureSlot = callRaylibFunction "_rlActiveTextureSlot_"

c'rlEnableTexture :: CUInt -> IO ()
c'rlEnableTexture = callRaylibFunction "_rlEnableTexture_"

c'rlEnableTextureCubemap :: CUInt -> IO ()
c'rlEnableTextureCubemap = callRaylibFunction "_rlEnableTextureCubemap_"

c'rlTextureParameters :: CUInt -> CInt -> CInt -> IO ()
c'rlTextureParameters = callRaylibFunction "_rlTextureParameters_"

c'rlCubemapParameters :: CUInt -> CInt -> CInt -> IO ()
c'rlCubemapParameters = callRaylibFunction "_rlCubemapParameters_"

c'rlEnableShader :: CUInt -> IO ()
c'rlEnableShader = callRaylibFunction "_rlEnableShader_"

c'rlEnableFramebuffer :: CUInt -> IO ()
c'rlEnableFramebuffer = callRaylibFunction "_rlEnableFramebuffer_"

c'rlActiveDrawBuffers :: CInt -> IO ()
c'rlActiveDrawBuffers = callRaylibFunction "_rlActiveDrawBuffers_"

c'rlBlitFramebuffer :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
c'rlBlitFramebuffer = callRaylibFunction "_rlBlitFramebuffer_"

c'rlBindFramebuffer :: CUInt -> CUInt -> IO ()
c'rlBindFramebuffer = callRaylibFunction "_rlBindFramebuffer_"

c'rlColorMask :: CBool -> CBool -> CBool -> CBool -> IO ()
c'rlColorMask = callRaylibFunction "_rlColorMask_"

c'rlSetCullFace :: CInt -> IO ()
c'rlSetCullFace = callRaylibFunction "_rlSetCullFace_"

c'rlScissor :: CInt -> CInt -> CInt -> CInt -> IO ()
c'rlScissor = callRaylibFunction "_rlScissor_"

c'rlSetLineWidth :: CFloat -> IO ()
c'rlSetLineWidth = callRaylibFunction "_rlSetLineWidth_"

c'rlGetLineWidth :: IO CFloat
c'rlGetLineWidth = callRaylibFunction "_rlGetLineWidth_"

c'rlIsStereoRenderEnabled :: IO CBool
c'rlIsStereoRenderEnabled = callRaylibFunction "_rlIsStereoRenderEnabled_"

c'rlClearColor :: CUChar -> CUChar -> CUChar -> CUChar -> IO ()
c'rlClearColor = callRaylibFunction "_rlClearColor_"

c'rlSetBlendMode :: CInt -> IO ()
c'rlSetBlendMode = callRaylibFunction "_rlSetBlendMode_"

c'rlSetBlendFactors :: CInt -> CInt -> CInt -> IO ()
c'rlSetBlendFactors = callRaylibFunction "_rlSetBlendFactors_"

c'rlSetBlendFactorsSeparate :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
c'rlSetBlendFactorsSeparate = callRaylibFunction "_rlSetBlendFactorsSeparate_"

c'rlglInit :: CInt -> CInt -> IO ()
c'rlglInit = callRaylibFunction "_rlglInit_"

c'rlLoadExtensions :: Ptr () -> IO ()
c'rlLoadExtensions = callRaylibFunction "_rlLoadExtensions_"

c'rlGetVersion :: IO CInt
c'rlGetVersion = callRaylibFunction "_rlGetVersion_"

c'rlSetFramebufferWidth :: CInt -> IO ()
c'rlSetFramebufferWidth = callRaylibFunction "_rlSetFramebufferWidth_"

c'rlGetFramebufferWidth :: IO CInt
c'rlGetFramebufferWidth = callRaylibFunction "_rlGetFramebufferWidth_"

c'rlSetFramebufferHeight :: CInt -> IO ()
c'rlSetFramebufferHeight = callRaylibFunction "_rlSetFramebufferHeight_"

c'rlGetFramebufferHeight :: IO CInt
c'rlGetFramebufferHeight = callRaylibFunction "_rlGetFramebufferHeight_"

c'rlGetTextureIdDefault :: IO CUInt
c'rlGetTextureIdDefault = callRaylibFunction "_rlGetTextureIdDefault_"

c'rlGetShaderIdDefault :: IO CUInt
c'rlGetShaderIdDefault = callRaylibFunction "_rlGetShaderIdDefault_"

c'rlGetShaderLocsDefault :: IO (Ptr CInt)
c'rlGetShaderLocsDefault = callRaylibFunction "_rlGetShaderLocsDefault_"

c'rlLoadRenderBatch :: CInt -> CInt -> IO (Ptr RLRenderBatch)
c'rlLoadRenderBatch = callRaylibFunction "_rlLoadRenderBatch_"

c'rlUnloadRenderBatch :: Ptr RLRenderBatch -> IO ()
c'rlUnloadRenderBatch = callRaylibFunction "_rlUnloadRenderBatch_"

c'rlDrawRenderBatch :: Ptr RLRenderBatch -> IO ()
c'rlDrawRenderBatch = callRaylibFunction "_rlDrawRenderBatch_"

c'rlSetRenderBatchActive :: Ptr RLRenderBatch -> IO ()
c'rlSetRenderBatchActive = callRaylibFunction "_rlSetRenderBatchActive_"

c'rlCheckRenderBatchLimit :: CInt -> IO CBool
c'rlCheckRenderBatchLimit = callRaylibFunction "_rlCheckRenderBatchLimit_"

c'rlSetTexture :: CUInt -> IO ()
c'rlSetTexture = callRaylibFunction "_rlSetTexture_"

c'rlLoadVertexArray :: IO CUInt
c'rlLoadVertexArray = callRaylibFunction "_rlLoadVertexArray_"

c'rlLoadVertexBuffer :: Ptr () -> CInt -> CBool -> IO CUInt
c'rlLoadVertexBuffer = callRaylibFunction "_rlLoadVertexBuffer_"

c'rlLoadVertexBufferElement :: Ptr () -> CInt -> CBool -> IO CUInt
c'rlLoadVertexBufferElement = callRaylibFunction "_rlLoadVertexBufferElement_"

c'rlUpdateVertexBuffer :: CUInt -> Ptr () -> CInt -> CInt -> IO ()
c'rlUpdateVertexBuffer = callRaylibFunction "_rlUpdateVertexBuffer_"

c'rlUpdateVertexBufferElements :: CUInt -> Ptr () -> CInt -> CInt -> IO ()
c'rlUpdateVertexBufferElements = callRaylibFunction "_rlUpdateVertexBufferElements_"

c'rlUnloadVertexArray :: CUInt -> IO ()
c'rlUnloadVertexArray = callRaylibFunction "_rlUnloadVertexArray_"

c'rlUnloadVertexBuffer :: CUInt -> IO ()
c'rlUnloadVertexBuffer = callRaylibFunction "_rlUnloadVertexBuffer_"

c'rlSetVertexAttribute :: CUInt -> CInt -> CInt -> CBool -> CInt -> Ptr () -> IO ()
c'rlSetVertexAttribute = callRaylibFunction "_rlSetVertexAttribute_"

c'rlSetVertexAttributeDivisor :: CUInt -> CInt -> IO ()
c'rlSetVertexAttributeDivisor = callRaylibFunction "_rlSetVertexAttributeDivisor_"

c'rlSetVertexAttributeDefault :: CInt -> Ptr () -> CInt -> CInt -> IO ()
c'rlSetVertexAttributeDefault = callRaylibFunction "_rlSetVertexAttributeDefault_"

c'rlDrawVertexArray :: CInt -> CInt -> IO ()
c'rlDrawVertexArray = callRaylibFunction "_rlDrawVertexArray_"

c'rlDrawVertexArrayElements :: CInt -> CInt -> Ptr () -> IO ()
c'rlDrawVertexArrayElements = callRaylibFunction "_rlDrawVertexArrayElements_"

c'rlDrawVertexArrayInstanced :: CInt -> CInt -> CInt -> IO ()
c'rlDrawVertexArrayInstanced = callRaylibFunction "_rlDrawVertexArrayInstanced_"

c'rlDrawVertexArrayElementsInstanced :: CInt -> CInt -> Ptr () -> CInt -> IO ()
c'rlDrawVertexArrayElementsInstanced = callRaylibFunction "_rlDrawVertexArrayElementsInstanced_"

c'rlLoadTexture :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CUInt
c'rlLoadTexture = callRaylibFunction "_rlLoadTexture_"

c'rlLoadTextureDepth :: CInt -> CInt -> CBool -> IO CUInt
c'rlLoadTextureDepth = callRaylibFunction "_rlLoadTextureDepth_"

c'rlLoadTextureCubemap :: Ptr () -> CInt -> CInt -> IO CUInt
c'rlLoadTextureCubemap = callRaylibFunction "_rlLoadTextureCubemap_"

c'rlUpdateTexture :: CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ()
c'rlUpdateTexture = callRaylibFunction "_rlUpdateTexture_"

c'rlGetGlTextureFormats :: CInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()
c'rlGetGlTextureFormats = callRaylibFunction "_rlGetGlTextureFormats_"

c'rlGetPixelFormatName :: CUInt -> IO CString
c'rlGetPixelFormatName = callRaylibFunction "_rlGetPixelFormatName_"

c'rlUnloadTexture :: CUInt -> IO ()
c'rlUnloadTexture = callRaylibFunction "_rlUnloadTexture_"

c'rlGenTextureMipmaps :: CUInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO ()
c'rlGenTextureMipmaps = callRaylibFunction "_rlGenTextureMipmaps_"

c'rlReadTexturePixels :: CUInt -> CInt -> CInt -> CInt -> IO (Ptr ())
c'rlReadTexturePixels = callRaylibFunction "_rlReadTexturePixels_"

c'rlReadScreenPixels :: CInt -> CInt -> IO (Ptr CUChar)
c'rlReadScreenPixels = callRaylibFunction "_rlReadScreenPixels_"

c'rlLoadFramebuffer :: CInt -> CInt -> IO CUInt
c'rlLoadFramebuffer = callRaylibFunction "_rlLoadFramebuffer_"

c'rlFramebufferAttach :: CUInt -> CUInt -> CInt -> CInt -> CInt -> IO ()
c'rlFramebufferAttach = callRaylibFunction "_rlFramebufferAttach_"

c'rlFramebufferComplete :: CUInt -> IO CBool
c'rlFramebufferComplete = callRaylibFunction "_rlFramebufferComplete_"

c'rlUnloadFramebuffer :: CUInt -> IO ()
c'rlUnloadFramebuffer = callRaylibFunction "_rlUnloadFramebuffer_"

c'rlLoadShaderCode :: CString -> CString -> IO CUInt
c'rlLoadShaderCode = callRaylibFunction "_rlLoadShaderCode_"

c'rlCompileShader :: CString -> CInt -> IO CUInt
c'rlCompileShader = callRaylibFunction "_rlCompileShader_"

c'rlLoadShaderProgram :: CUInt -> CUInt -> IO CUInt
c'rlLoadShaderProgram = callRaylibFunction "_rlLoadShaderProgram_"

c'rlUnloadShaderProgram :: CUInt -> IO ()
c'rlUnloadShaderProgram = callRaylibFunction "_rlUnloadShaderProgram_"

c'rlGetLocationUniform :: CUInt -> CString -> IO CInt
c'rlGetLocationUniform = callRaylibFunction "_rlGetLocationUniform_"

c'rlGetLocationAttrib :: CUInt -> CString -> IO CInt
c'rlGetLocationAttrib = callRaylibFunction "_rlGetLocationAttrib_"

c'rlSetUniform :: CInt -> Ptr () -> CInt -> CInt -> IO ()
c'rlSetUniform = callRaylibFunction "_rlSetUniform_"

c'rlSetUniformMatrix :: CInt -> Ptr Matrix -> IO ()
c'rlSetUniformMatrix = callRaylibFunction "_rlSetUniformMatrix_"

c'rlSetUniformSampler :: CInt -> CUInt -> IO ()
c'rlSetUniformSampler = callRaylibFunction "_rlSetUniformSampler_"

c'rlSetShader :: CUInt -> Ptr CInt -> IO ()
c'rlSetShader = callRaylibFunction "_rlSetShader_"

c'rlLoadComputeShaderProgram :: CUInt -> IO CUInt
c'rlLoadComputeShaderProgram = callRaylibFunction "_rlLoadComputeShaderProgram_"

c'rlComputeShaderDispatch :: CUInt -> CUInt -> CUInt -> IO ()
c'rlComputeShaderDispatch = callRaylibFunction "_rlComputeShaderDispatch_"

c'rlLoadShaderBuffer :: CUInt -> Ptr () -> CInt -> IO CUInt
c'rlLoadShaderBuffer = callRaylibFunction "_rlLoadShaderBuffer_"

c'rlUnloadShaderBuffer :: CUInt -> IO ()
c'rlUnloadShaderBuffer = callRaylibFunction "_rlUnloadShaderBuffer_"

c'rlUpdateShaderBuffer :: CUInt -> Ptr () -> CUInt -> CUInt -> IO ()
c'rlUpdateShaderBuffer = callRaylibFunction "_rlUpdateShaderBuffer_"

c'rlBindShaderBuffer :: CUInt -> CUInt -> IO ()
c'rlBindShaderBuffer = callRaylibFunction "_rlBindShaderBuffer_"

c'rlReadShaderBuffer :: CUInt -> Ptr () -> CUInt -> CUInt -> IO ()
c'rlReadShaderBuffer = callRaylibFunction "_rlReadShaderBuffer_"

c'rlCopyShaderBuffer :: CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
c'rlCopyShaderBuffer = callRaylibFunction "_rlCopyShaderBuffer_"

c'rlGetShaderBufferSize :: CUInt -> IO CUInt
c'rlGetShaderBufferSize = callRaylibFunction "_rlGetShaderBufferSize_"

c'rlBindImageTexture :: CUInt -> CUInt -> CInt -> CBool -> IO ()
c'rlBindImageTexture = callRaylibFunction "_rlBindImageTexture_"

c'rlGetMatrixModelview :: IO (Ptr Matrix)
c'rlGetMatrixModelview = callRaylibFunction "_rlGetMatrixModelview_"

c'rlGetMatrixProjection :: IO (Ptr Matrix)
c'rlGetMatrixProjection = callRaylibFunction "_rlGetMatrixProjection_"

c'rlGetMatrixTransform :: IO (Ptr Matrix)
c'rlGetMatrixTransform = callRaylibFunction "_rlGetMatrixTransform_"

c'rlGetMatrixProjectionStereo :: CInt -> IO (Ptr Matrix)
c'rlGetMatrixProjectionStereo = callRaylibFunction "_rlGetMatrixProjectionStereo_"

c'rlGetMatrixViewOffsetStereo :: CInt -> IO (Ptr Matrix)
c'rlGetMatrixViewOffsetStereo = callRaylibFunction "_rlGetMatrixViewOffsetStereo_"

c'rlSetMatrixProjection :: Ptr Matrix -> IO ()
c'rlSetMatrixProjection = callRaylibFunction "_rlSetMatrixProjection_"

c'rlSetMatrixModelview :: Ptr Matrix -> IO ()
c'rlSetMatrixModelview = callRaylibFunction "_rlSetMatrixModelview_"

c'rlSetMatrixProjectionStereo :: Ptr Matrix -> Ptr Matrix -> IO ()
c'rlSetMatrixProjectionStereo = callRaylibFunction "_rlSetMatrixProjectionStereo_"

c'rlSetMatrixViewOffsetStereo :: Ptr Matrix -> Ptr Matrix -> IO ()
c'rlSetMatrixViewOffsetStereo = callRaylibFunction "_rlSetMatrixViewOffsetStereo_"

c'rlGetPixelDataSize :: CInt -> CInt -> CInt -> IO CInt
c'rlGetPixelDataSize = callRaylibFunction "_rlGetPixelDataSize"

c'rlPushMatrix :: IO ()
c'rlPushMatrix = callRaylibFunction "_rlPushMatrix_"

c'rlPopMatrix :: IO ()
c'rlPopMatrix = callRaylibFunction "_rlPopMatrix_"

c'rlLoadIdentity :: IO ()
c'rlLoadIdentity = callRaylibFunction "_rlLoadIdentity_"

c'rlEnd :: IO ()
c'rlEnd = callRaylibFunction "_rlEnd_"

c'rlDisableVertexArray :: IO ()
c'rlDisableVertexArray = callRaylibFunction "_rlDisableVertexArray_"

c'rlDisableVertexBuffer :: IO ()
c'rlDisableVertexBuffer = callRaylibFunction "_rlDisableVertexBuffer_"

c'rlDisableVertexBufferElement :: IO ()
c'rlDisableVertexBufferElement = callRaylibFunction "_rlDisableVertexBufferElement_"

c'rlDisableTexture :: IO ()
c'rlDisableTexture = callRaylibFunction "_rlDisableTexture_"

c'rlDisableTextureCubemap :: IO ()
c'rlDisableTextureCubemap = callRaylibFunction "_rlDisableTextureCubemap_"

c'rlDisableShader :: IO ()
c'rlDisableShader = callRaylibFunction "_rlDisableShader_"

c'rlDisableFramebuffer :: IO ()
c'rlDisableFramebuffer = callRaylibFunction "_rlDisableFramebuffer_"

c'rlEnableColorBlend :: IO ()
c'rlEnableColorBlend = callRaylibFunction "_rlEnableColorBlend_"

c'rlDisableColorBlend :: IO ()
c'rlDisableColorBlend = callRaylibFunction "_rlDisableColorBlend_"

c'rlEnableDepthTest :: IO ()
c'rlEnableDepthTest = callRaylibFunction "_rlEnableDepthTest_"

c'rlDisableDepthTest :: IO ()
c'rlDisableDepthTest = callRaylibFunction "_rlDisableDepthTest_"

c'rlEnableDepthMask :: IO ()
c'rlEnableDepthMask = callRaylibFunction "_rlEnableDepthMask_"

c'rlDisableDepthMask :: IO ()
c'rlDisableDepthMask = callRaylibFunction "_rlDisableDepthMask_"

c'rlEnableBackfaceCulling :: IO ()
c'rlEnableBackfaceCulling = callRaylibFunction "_rlEnableBackfaceCulling_"

c'rlDisableBackfaceCulling :: IO ()
c'rlDisableBackfaceCulling = callRaylibFunction "_rlDisableBackfaceCulling_"

c'rlEnableScissorTest :: IO ()
c'rlEnableScissorTest = callRaylibFunction "_rlEnableScissorTest_"

c'rlDisableScissorTest :: IO ()
c'rlDisableScissorTest = callRaylibFunction "_rlDisableScissorTest_"

c'rlEnableWireMode :: IO ()
c'rlEnableWireMode = callRaylibFunction "_rlEnableWireMode_"

c'rlEnablePointMode :: IO ()
c'rlEnablePointMode = callRaylibFunction "_rlEnablePointMode_"

c'rlDisableWireMode :: IO ()
c'rlDisableWireMode = callRaylibFunction "_rlDisableWireMode_"

c'rlEnableSmoothLines :: IO ()
c'rlEnableSmoothLines = callRaylibFunction "_rlEnableSmoothLines_"

c'rlDisableSmoothLines :: IO ()
c'rlDisableSmoothLines = callRaylibFunction "_rlDisableSmoothLines_"

c'rlEnableStereoRender :: IO ()
c'rlEnableStereoRender = callRaylibFunction "_rlEnableStereoRender_"

c'rlDisableStereoRender :: IO ()
c'rlDisableStereoRender = callRaylibFunction "_rlDisableStereoRender_"

c'rlClearScreenBuffers :: IO ()
c'rlClearScreenBuffers = callRaylibFunction "_rlClearScreenBuffers_"

c'rlCheckErrors :: IO ()
c'rlCheckErrors = callRaylibFunction "_rlCheckErrors_"

c'rlglClose :: IO ()
c'rlglClose = callRaylibFunction "_rlglClose_"

c'rlDrawRenderBatchActive :: IO ()
c'rlDrawRenderBatchActive = callRaylibFunction "_rlDrawRenderBatchActive_"

c'rlLoadDrawCube :: IO ()
c'rlLoadDrawCube = callRaylibFunction "_rlLoadDrawCube_"

c'rlLoadDrawQuad :: IO ()
c'rlLoadDrawQuad = callRaylibFunction "_rlLoadDrawQuad_"

#else

---- raylib.h

foreign import ccall safe "rl_bindings.h InitWindow_"
  c'initWindow ::
    CInt -> CInt -> CString -> IO ()

foreign import ccall safe "rl_bindings.h WindowShouldClose_"
  c'windowShouldClose ::
    IO CBool

foreign import ccall safe "rl_bindings.h CloseWindow_"
  c'closeWindow ::
    IO ()

foreign import ccall safe "rl_bindings.h IsWindowReady_"
  c'isWindowReady ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowFullscreen_"
  c'isWindowFullscreen ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowHidden_"
  c'isWindowHidden ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowMinimized_"
  c'isWindowMinimized ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowMaximized_"
  c'isWindowMaximized ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowFocused_"
  c'isWindowFocused ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowResized_"
  c'isWindowResized ::
    IO CBool

foreign import ccall safe "rl_bindings.h IsWindowState_"
  c'isWindowState ::
    CUInt -> IO CBool

foreign import ccall safe "rl_bindings.h SetWindowState_"
  c'setWindowState ::
    CUInt -> IO ()

foreign import ccall safe "rl_bindings.h ClearWindowState_"
  c'clearWindowState ::
    CUInt -> IO ()

foreign import ccall safe "rl_bindings.h ToggleFullscreen_"
  c'toggleFullscreen ::
    IO ()

foreign import ccall safe "rl_bindings.h ToggleBorderlessWindowed_"
  c'toggleBorderlessWindowed ::
    IO ()

foreign import ccall safe "rl_bindings.h MaximizeWindow_"
  c'maximizeWindow ::
    IO ()

foreign import ccall safe "rl_bindings.h MinimizeWindow_"
  c'minimizeWindow ::
    IO ()

foreign import ccall safe "rl_bindings.h RestoreWindow_"
  c'restoreWindow ::
    IO ()

foreign import ccall safe "rl_bindings.h SetWindowIcon_" c'setWindowIcon :: Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowIcons_" c'setWindowIcons :: Ptr Image -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowTitle_"
  c'setWindowTitle ::
    CString -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowPosition_"
  c'setWindowPosition ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowMonitor_"
  c'setWindowMonitor ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowMinSize_"
  c'setWindowMinSize ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowMaxSize_"
  c'setWindowMaxSize ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowSize_"
  c'setWindowSize ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowOpacity_"
  c'setWindowOpacity ::
    CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetWindowFocused_"
  c'setWindowFocused ::
    IO ()

foreign import ccall safe "rl_bindings.h GetWindowHandle_"
  c'getWindowHandle ::
    IO (Ptr ())

foreign import ccall safe "rl_bindings.h GetScreenWidth_"
  c'getScreenWidth ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetScreenHeight_"
  c'getScreenHeight ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetRenderWidth_"
  c'getRenderWidth ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetRenderHeight_"
  c'getRenderHeight ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetMonitorCount_"
  c'getMonitorCount ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetCurrentMonitor_"
  c'getCurrentMonitor ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetMonitorPosition_" c'getMonitorPosition :: CInt -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetMonitorWidth_"
  c'getMonitorWidth ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetMonitorHeight_"
  c'getMonitorHeight ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetMonitorPhysicalWidth_"
  c'getMonitorPhysicalWidth ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetMonitorPhysicalHeight_"
  c'getMonitorPhysicalHeight ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetMonitorRefreshRate_"
  c'getMonitorRefreshRate ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetWindowPosition_" c'getWindowPosition :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetWindowScaleDPI_" c'getWindowScaleDPI :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetMonitorName_"
  c'getMonitorName ::
    CInt -> IO CString

foreign import ccall safe "rl_bindings.h SetClipboardText_"
  c'setClipboardText ::
    CString -> IO ()

foreign import ccall safe "rl_bindings.h GetClipboardText_"
  c'getClipboardText ::
    IO CString

foreign import ccall safe "rl_bindings.h EnableEventWaiting_"
  c'enableEventWaiting ::
    IO ()

foreign import ccall safe "rl_bindings.h DisableEventWaiting_"
  c'disableEventWaiting ::
    IO ()

foreign import ccall safe "rl_bindings.h SwapScreenBuffer_"
  c'swapScreenBuffer ::
    IO ()

foreign import ccall safe "rl_bindings.h PollInputEvents_"
  c'pollInputEvents ::
    IO ()

foreign import ccall safe "rl_bindings.h WaitTime_"
  c'waitTime ::
    CDouble -> IO ()

foreign import ccall safe "rl_bindings.h ShowCursor_"
  c'showCursor ::
    IO ()

foreign import ccall safe "rl_bindings.h HideCursor_"
  c'hideCursor ::
    IO ()

foreign import ccall safe "rl_bindings.h IsCursorHidden_"
  c'isCursorHidden ::
    IO CBool

foreign import ccall safe "rl_bindings.h EnableCursor_"
  c'enableCursor ::
    IO ()

foreign import ccall safe "rl_bindings.h DisableCursor_"
  c'disableCursor ::
    IO ()

foreign import ccall safe "rl_bindings.h IsCursorOnScreen_"
  c'isCursorOnScreen ::
    IO CBool

foreign import ccall safe "rl_bindings.h ClearBackground_" c'clearBackground :: Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h BeginDrawing_"
  c'beginDrawing ::
    IO ()

foreign import ccall safe "rl_bindings.h EndDrawing_"
  c'endDrawing ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginMode2D_" c'beginMode2D :: Ptr Camera2D -> IO ()

foreign import ccall safe "rl_bindings.h EndMode2D_"
  c'endMode2D ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginMode3D_" c'beginMode3D :: Ptr Camera3D -> IO ()

foreign import ccall safe "rl_bindings.h EndMode3D_"
  c'endMode3D ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginTextureMode_" c'beginTextureMode :: Ptr RenderTexture -> IO ()

foreign import ccall safe "rl_bindings.h EndTextureMode_"
  c'endTextureMode ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginShaderMode_" c'beginShaderMode :: Ptr Shader -> IO ()

foreign import ccall safe "rl_bindings.h EndShaderMode_"
  c'endShaderMode ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginBlendMode_"
  c'beginBlendMode ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h EndBlendMode_"
  c'endBlendMode ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginScissorMode_"
  c'beginScissorMode ::
    CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h EndScissorMode_"
  c'endScissorMode ::
    IO ()

foreign import ccall safe "rl_bindings.h BeginVrStereoMode_" c'beginVrStereoMode :: Ptr VrStereoConfig -> IO ()

foreign import ccall safe "rl_bindings.h EndVrStereoMode_"
  c'endVrStereoMode ::
    IO ()

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

foreign import ccall safe "rl_bindings.h SetTargetFPS_"
  c'setTargetFPS ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h GetFPS_"
  c'getFPS ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetFrameTime_"
  c'getFrameTime ::
    IO CFloat

foreign import ccall safe "rl_bindings.h GetTime_"
  c'getTime ::
    IO CDouble

foreign import ccall safe "rl_bindings.h SetRandomSeed_"
  c'setRandomSeed ::
    CUInt -> IO ()

foreign import ccall safe "rl_bindings.h GetRandomValue_"
  c'getRandomValue ::
    CInt -> CInt -> IO CInt

foreign import ccall safe "rl_bindings.h LoadRandomSequence_"
  c'loadRandomSequence ::
    CUInt -> CInt -> CInt -> IO (Ptr CInt)

foreign import ccall safe "rl_bindings.h TakeScreenshot_"
  c'takeScreenshot ::
    CString -> IO ()

foreign import ccall safe "rl_bindings.h SetConfigFlags_"
  c'setConfigFlags ::
    CUInt -> IO ()

foreign import ccall safe "rl_bindings.h TraceLog_"
  c'traceLog ::
    CInt -> CString -> IO () -- Uses varags, can't implement complete functionality

foreign import ccall safe "rl_bindings.h SetTraceLogLevel_"
  c'setTraceLogLevel ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h MemAlloc_"
  c'memAlloc ::
    CInt -> IO (Ptr ())

foreign import ccall safe "rl_bindings.h MemRealloc_"
  c'memRealloc ::
    Ptr () -> CInt -> IO (Ptr ())

foreign import ccall safe "rl_bindings.h MemFree_"
  c'memFree ::
    Ptr () -> IO ()

foreign import ccall safe "rl_bindings.h OpenURL_"
  c'openURL ::
    CString -> IO ()

foreign import ccall safe "rl_bindings.h SetLoadFileDataCallback_"
  c'setLoadFileDataCallback ::
    LoadFileDataCallback -> IO ()

foreign import ccall safe "rl_bindings.h SetSaveFileDataCallback_"
  c'setSaveFileDataCallback ::
    SaveFileDataCallback -> IO ()

foreign import ccall safe "rl_bindings.h SetLoadFileTextCallback_"
  c'setLoadFileTextCallback ::
    LoadFileTextCallback -> IO ()

foreign import ccall safe "rl_bindings.h SetSaveFileTextCallback_"
  c'setSaveFileTextCallback ::
    SaveFileTextCallback -> IO ()

foreign import ccall safe "rl_bindings.h LoadFileData_"
  c'loadFileData ::
    CString -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "rl_bindings.h UnloadFileData_"
  c'unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall safe "rl_bindings.h SaveFileData_"
  c'saveFileData ::
    CString -> Ptr () -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h ExportDataAsCode_"
  c'exportDataAsCode ::
    Ptr CUChar -> CInt -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h LoadFileText_"
  c'loadFileText ::
    CString -> IO CString

foreign import ccall safe "rl_bindings.h UnloadFileText_"
  c'unloadFileText ::
    CString -> IO ()

foreign import ccall safe "rl_bindings.h SaveFileText_"
  c'saveFileText ::
    CString -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h FileExists_"
  c'fileExists ::
    CString -> IO CBool

foreign import ccall safe "rl_bindings.h DirectoryExists_"
  c'directoryExists ::
    CString -> IO CBool

foreign import ccall safe "rl_bindings.h IsFileExtension_"
  c'isFileExtension ::
    CString -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h GetFileLength_"
  c'getFileLength ::
    CString -> IO CBool

foreign import ccall safe "rl_bindings.h GetFileExtension_"
  c'getFileExtension ::
    CString -> IO CString

foreign import ccall safe "rl_bindings.h GetFileName_"
  c'getFileName ::
    CString -> IO CString

foreign import ccall safe "rl_bindings.h GetFileNameWithoutExt_"
  c'getFileNameWithoutExt ::
    CString -> IO CString

foreign import ccall safe "rl_bindings.h GetDirectoryPath_"
  c'getDirectoryPath ::
    CString -> IO CString

foreign import ccall safe "rl_bindings.h GetPrevDirectoryPath_"
  c'getPrevDirectoryPath ::
    CString -> IO CString

foreign import ccall safe "rl_bindings.h GetWorkingDirectory_"
  c'getWorkingDirectory ::
    IO CString

foreign import ccall safe "rl_bindings.h GetApplicationDirectory_"
  c'getApplicationDirectory ::
    IO CString

foreign import ccall safe "rl_bindings.h ChangeDirectory_"
  c'changeDirectory ::
    CString -> IO CBool

foreign import ccall safe "rl_bindings.h IsPathFile_"
  c'isPathFile ::
    CString -> IO CBool

foreign import ccall safe "rl_bindings.h LoadDirectoryFiles_" c'loadDirectoryFiles :: CString -> IO (Ptr FilePathList)

foreign import ccall safe "rl_bindings.h LoadDirectoryFilesEx_" c'loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr FilePathList)

foreign import ccall safe "rl_bindings.h UnloadDirectoryFiles_" c'unloadDirectoryFiles :: Ptr FilePathList -> IO ()

foreign import ccall safe "rl_bindings.h IsFileDropped_"
  c'isFileDropped ::
    IO CBool

foreign import ccall safe "rl_bindings.h LoadDroppedFiles_" c'loadDroppedFiles :: IO (Ptr FilePathList)

foreign import ccall safe "rl_bindings.h UnloadDroppedFiles_" c'unloadDroppedFiles :: Ptr FilePathList -> IO ()

foreign import ccall safe "rl_bindings.h GetFileModTime_"
  c'getFileModTime ::
    CString -> IO CLong

foreign import ccall safe "rl_bindings.h CompressData_"
  c'compressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "rl_bindings.h DecompressData_"
  c'decompressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "rl_bindings.h EncodeDataBase64_"
  c'encodeDataBase64 ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO CString

foreign import ccall safe "rl_bindings.h DecodeDataBase64_"
  c'decodeDataBase64 ::
    Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall safe "rl_bindings.h LoadAutomationEventList_"
  c'loadAutomationEventList ::
    CString -> IO (Ptr AutomationEventList)

foreign import ccall safe "rl_bindings.h ExportAutomationEventList_"
  c'exportAutomationEventList ::
    Ptr AutomationEventList -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h SetAutomationEventList_"
  c'setAutomationEventList ::
    Ptr AutomationEventList -> IO ()

foreign import ccall safe "rl_bindings.h SetAutomationEventBaseFrame_"
  c'setAutomationEventBaseFrame ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h StartAutomationEventRecording_"
  c'startAutomationEventRecording ::
    IO ()

foreign import ccall safe "rl_bindings.h StopAutomationEventRecording_"
  c'stopAutomationEventRecording ::
    IO ()

foreign import ccall safe "rl_bindings.h PlayAutomationEvent"
  c'playAutomationEvent ::
    Ptr AutomationEvent -> IO ()

foreign import ccall safe "rl_bindings.h IsKeyPressed_"
  c'isKeyPressed ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsKeyPressedRepeat_"
  c'isKeyPressedRepeat ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsKeyDown_"
  c'isKeyDown ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsKeyReleased_"
  c'isKeyReleased ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsKeyUp_"
  c'isKeyUp ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h SetExitKey_"
  c'setExitKey ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h GetKeyPressed_"
  c'getKeyPressed ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetCharPressed_"
  c'getCharPressed ::
    IO CInt

foreign import ccall safe "rl_bindings.h IsGamepadAvailable_"
  c'isGamepadAvailable ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h GetGamepadName_"
  c'getGamepadName ::
    CInt -> IO CString

foreign import ccall safe "rl_bindings.h IsGamepadButtonPressed_"
  c'isGamepadButtonPressed ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsGamepadButtonDown_"
  c'isGamepadButtonDown ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsGamepadButtonReleased_"
  c'isGamepadButtonReleased ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsGamepadButtonUp_"
  c'isGamepadButtonUp ::
    CInt -> CInt -> IO CBool

foreign import ccall safe "rl_bindings.h GetGamepadButtonPressed_"
  c'getGamepadButtonPressed ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetGamepadAxisCount_"
  c'getGamepadAxisCount ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetGamepadAxisMovement_"
  c'getGamepadAxisMovement ::
    CInt -> CInt -> IO CFloat

foreign import ccall safe "rl_bindings.h SetGamepadMappings_"
  c'setGamepadMappings ::
    CString -> IO CInt

foreign import ccall safe "rl_bindings.h IsMouseButtonPressed_"
  c'isMouseButtonPressed ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsMouseButtonDown_"
  c'isMouseButtonDown ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsMouseButtonReleased_"
  c'isMouseButtonReleased ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h IsMouseButtonUp_"
  c'isMouseButtonUp ::
    CInt -> IO CBool

foreign import ccall safe "rl_bindings.h GetMouseX_"
  c'getMouseX ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetMouseY_"
  c'getMouseY ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetMousePosition_" c'getMousePosition :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetMouseDelta_" c'getMouseDelta :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h SetMousePosition_"
  c'setMousePosition ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetMouseOffset_"
  c'setMouseOffset ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetMouseScale_"
  c'setMouseScale ::
    CFloat -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h GetMouseWheelMove_"
  c'getMouseWheelMove ::
    IO CFloat

foreign import ccall safe "rl_bindings.h GetMouseWheelMoveV_" c'getMouseWheelMoveV :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h SetMouseCursor_"
  c'setMouseCursor ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h GetTouchX_"
  c'getTouchX ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetTouchY_"
  c'getTouchY ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetTouchPosition_" c'getTouchPosition :: CInt -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetTouchPointId_"
  c'getTouchPointId ::
    CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetTouchPointCount_"
  c'getTouchPointCount ::
    IO CInt

foreign import ccall safe "rl_bindings.h SetGesturesEnabled_"
  c'setGesturesEnabled ::
    CUInt -> IO ()

foreign import ccall safe "rl_bindings.h IsGestureDetected_"
  c'isGestureDetected ::
    CUInt -> IO CBool

foreign import ccall safe "rl_bindings.h GetGestureDetected_"
  c'getGestureDetected ::
    IO CInt

foreign import ccall safe "rl_bindings.h GetGestureHoldDuration_"
  c'getGestureHoldDuration ::
    IO CFloat

foreign import ccall safe "rl_bindings.h GetGestureDragVector_" c'getGestureDragVector :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetGestureDragAngle_"
  c'getGestureDragAngle ::
    IO CFloat

foreign import ccall safe "rl_bindings.h GetGesturePinchVector_" c'getGesturePinchVector :: IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetGesturePinchAngle_"
  c'getGesturePinchAngle ::
    IO CFloat

foreign import ccall safe "rl_bindings.h UpdateCamera_"
  c'updateCamera ::
    Ptr Camera3D -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UpdateCameraPro_"
  c'updateCameraPro ::
    Ptr Camera3D -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h SetShapesTexture_" c'setShapesTexture :: Ptr Texture -> Ptr Rectangle -> IO ()

foreign import ccall safe "rl_bindings.h GetShapesTexture_" c'getShapesTexture :: IO (Ptr Texture)

foreign import ccall safe "rl_bindings.h GetShapesTextureRectangle_" c'getShapesTextureRectangle :: IO (Ptr Rectangle)

foreign import ccall safe "rl_bindings.h DrawPixel_" c'drawPixel :: CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawPixelV_" c'drawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLine_" c'drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineV_" c'drawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineEx_" c'drawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineStrip_" c'drawLineStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawLineBezier_" c'drawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircle_" c'drawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleSector_" c'drawCircleSector :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleSectorLines_" c'drawCircleSectorLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleGradient_" c'drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleV_" c'drawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleLines_" c'drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawCircleLinesV_" c'drawCircleLinesV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

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

foreign import ccall safe "rl_bindings.h DrawSplineLinear_" c'drawSplineLinear :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineBasis_" c'drawSplineBasis :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineCatmullRom_" c'drawSplineCatmullRom :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineBezierQuadratic_" c'drawSplineBezierQuadratic :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineBezierCubic_" c'drawSplineBezierCubic :: Ptr Vector2 -> CInt -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineSegmentLinear_" c'drawSplineSegmentLinear :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineSegmentBasis_" c'drawSplineSegmentBasis :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineSegmentCatmullRom_" c'drawSplineSegmentCatmullRom :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineSegmentBezierQuadratic_" c'drawSplineSegmentBezierQuadratic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawSplineSegmentBezierCubic_" c'drawSplineSegmentBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h GetSplinePointLinear_" c'getSplinePointLinear :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetSplinePointBasis_" c'getSplinePointBasis :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetSplinePointCatmullRom_" c'getSplinePointCatmullRom :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetSplinePointBezierQuad_" c'getSplinePointBezierQuad :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetSplinePointBezierCubic_" c'getSplinePointBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO (Ptr Vector2)

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

foreign import ccall safe "rl_bindings.h LoadImageAnimFromMemory_" c'loadImageAnimFromMemory :: CString -> Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr Image)

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

foreign import ccall safe "rl_bindings.h ImageFormat_"
  c'imageFormat ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageToPOT_" c'imageToPOT :: Ptr Image -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageCrop_" c'imageCrop :: Ptr Image -> Ptr Rectangle -> IO ()

foreign import ccall safe "rl_bindings.h ImageAlphaCrop_"
  c'imageAlphaCrop ::
    Ptr Image -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h ImageAlphaClear_" c'imageAlphaClear :: Ptr Image -> Ptr Color -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h ImageAlphaMask_" c'imageAlphaMask :: Ptr Image -> Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageAlphaPremultiply_"
  c'imageAlphaPremultiply ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageBlurGaussian_"
  c'imageBlurGaussian ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageKernelConvolution_"
  c'imageKernelConvolution ::
    Ptr Image -> Ptr CFloat -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageResize_"
  c'imageResize ::
    Ptr Image -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageResizeNN_"
  c'imageResizeNN ::
    Ptr Image -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageResizeCanvas_" c'imageResizeCanvas :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageMipmaps_"
  c'imageMipmaps ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageDither_"
  c'imageDither ::
    Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageFlipVertical_"
  c'imageFlipVertical ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageFlipHorizontal_"
  c'imageFlipHorizontal ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageRotate_"
  c'imageRotate ::
    Ptr Image -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ImageRotateCW_"
  c'imageRotateCW ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageRotateCCW_"
  c'imageRotateCCW ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorTint_" c'imageColorTint :: Ptr Image -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorInvert_"
  c'imageColorInvert ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorGrayscale_"
  c'imageColorGrayscale ::
    Ptr Image -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorContrast_"
  c'imageColorContrast ::
    Ptr Image -> CFloat -> IO ()

foreign import ccall safe "rl_bindings.h ImageColorBrightness_"
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

foreign import ccall safe "rl_bindings.h GenTextureMipmaps_"
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

foreign import ccall safe "rl_bindings.h LoadFontData_"
  c'loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall safe "rl_bindings.h GenImageFontAtlas_" c'genImageFontAtlas :: Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)

foreign import ccall safe "rl_bindings.h UnloadFontData_"
  c'unloadFontData ::
    Ptr GlyphInfo -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h IsFontReady_" c'isFontReady :: Ptr Font -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadFont_" c'unloadFont :: Ptr Font -> IO ()

foreign import ccall safe "rl_bindings.h ExportFontAsCode_" c'exportFontAsCode :: Ptr Font -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h DrawFPS_"
  c'drawFPS ::
    CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h DrawText_" c'drawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextEx_" c'drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextPro_" c'drawTextPro :: Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextCodepoint_" c'drawTextCodepoint :: Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h DrawTextCodepoints_" c'drawTextCodepoints :: Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()

foreign import ccall safe "rl_bindings.h SetTextLineSpacing_"
  c'setTextLineSpacing ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h MeasureText_"
  c'measureText ::
    CString -> CInt -> IO CInt

foreign import ccall safe "rl_bindings.h MeasureTextEx_" c'measureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)

foreign import ccall safe "rl_bindings.h GetGlyphIndex_" c'getGlyphIndex :: Ptr Font -> CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetGlyphInfo_" c'getGlyphInfo :: Ptr Font -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall safe "rl_bindings.h GetGlyphAtlasRec_" c'getGlyphAtlasRec :: Ptr Font -> CInt -> IO (Ptr Rectangle)

foreign import ccall safe "rl_bindings.h LoadUTF8_"
  c'loadUTF8 ::
    Ptr CInt -> CInt -> IO CString

foreign import ccall safe "rl_bindings.h LoadCodepoints_"
  c'loadCodepoints ::
    CString -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall safe "rl_bindings.h GetCodepointCount_"
  c'getCodepointCount ::
    CString -> IO CInt

foreign import ccall safe "rl_bindings.h GetCodepointNext_"
  c'getCodepointNext ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "rl_bindings.h GetCodepointPrevious_"
  c'getCodepointPrevious ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall safe "rl_bindings.h CodepointToUTF8_"
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

foreign import ccall safe "rl_bindings.h DrawGrid_"
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

foreign import ccall safe "rl_bindings.h UploadMesh_"
  c'uploadMesh ::
    Ptr Mesh -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UpdateMeshBuffer_" c'updateMeshBuffer :: Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UnloadMesh_" c'unloadMesh :: Ptr Mesh -> IO ()

foreign import ccall safe "rl_bindings.h DrawMesh_" c'drawMesh :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()

foreign import ccall safe "rl_bindings.h DrawMeshInstanced_" c'drawMeshInstanced :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h ExportMesh_" c'exportMesh :: Ptr Mesh -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h ExportMeshAsCode_" c'exportMeshAsCode :: Ptr Mesh -> CString -> IO CBool

foreign import ccall safe "rl_bindings.h GetMeshBoundingBox_" c'getMeshBoundingBox :: Ptr Mesh -> IO (Ptr BoundingBox)

foreign import ccall safe "rl_bindings.h GenMeshTangents_"
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

foreign import ccall safe "rl_bindings.h LoadMaterials_"
  c'loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Material)

foreign import ccall safe "rl_bindings.h LoadMaterialDefault_" c'loadMaterialDefault :: IO (Ptr Material)

foreign import ccall safe "rl_bindings.h IsMaterialReady_" c'isMaterialReady :: Ptr Material -> IO CBool

foreign import ccall safe "rl_bindings.h UnloadMaterial_" c'unloadMaterial :: Ptr Material -> IO ()

foreign import ccall safe "rl_bindings.h SetMaterialTexture_" c'setMaterialTexture :: Ptr Material -> CInt -> Ptr Texture -> IO ()

foreign import ccall safe "rl_bindings.h SetModelMeshMaterial_"
  c'setModelMeshMaterial ::
    Ptr Model -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h LoadModelAnimations_"
  c'loadModelAnimations ::
    CString -> Ptr CInt -> IO (Ptr ModelAnimation)

foreign import ccall safe "rl_bindings.h UpdateModelAnimation_" c'updateModelAnimation :: Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h UnloadModelAnimation_" c'unloadModelAnimation :: Ptr ModelAnimation -> IO ()

foreign import ccall safe "rl_bindings.h UnloadModelAnimations_"
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

foreign import ccall safe "rl_bindings.h InitAudioDevice_"
  c'initAudioDevice ::
    IO ()

foreign import ccall safe "rl_bindings.h CloseAudioDevice_"
  c'closeAudioDevice ::
    IO ()

foreign import ccall safe "rl_bindings.h IsAudioDeviceReady_"
  c'isAudioDeviceReady ::
    IO CBool

foreign import ccall safe "rl_bindings.h SetMasterVolume_"
  c'setMasterVolume ::
    CFloat -> IO ()

foreign import ccall safe "rl_bindings.h GetMasterVolume_"
  c'getMasterVolume ::
    IO CFloat

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

foreign import ccall safe "rl_bindings.h WaveCrop_"
  c'waveCrop ::
    Ptr Wave -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h WaveFormat_"
  c'waveFormat ::
    Ptr Wave -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rl_bindings.h LoadWaveSamples_" c'loadWaveSamples :: Ptr Wave -> IO (Ptr CFloat)

foreign import ccall safe "rl_bindings.h UnloadWaveSamples_"
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

foreign import ccall safe "rl_bindings.h SetAudioStreamBufferSizeDefault_"
  c'setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

foreign import ccall safe "rl_bindings.h SetAudioStreamCallback_" c'setAudioStreamCallback :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h AttachAudioStreamProcessor_" c'attachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h DetachAudioStreamProcessor_" c'detachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h AttachAudioMixedProcessor_" c'attachAudioMixedProcessor :: Ptr AudioCallback -> IO ()

foreign import ccall safe "rl_bindings.h DetachAudioMixedProcessor_" c'detachAudioMixedProcessor :: Ptr AudioCallback -> IO ()

---- rlgl.h

foreign import ccall safe "rlgl_bindings.h rlMatrixMode_" c'rlMatrixMode :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlTranslatef_" c'rlTranslatef :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlRotatef_" c'rlRotatef :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlScalef_" c'rlScalef :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlMultMatrixf_" c'rlMultMatrixf :: Ptr CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlFrustum_" c'rlFrustum :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall safe "rlgl_bindings.h rlOrtho_" c'rlOrtho :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall safe "rlgl_bindings.h rlViewport_" c'rlViewport :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlBegin_" c'rlBegin :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlVertex2i_" c'rlVertex2i :: CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlVertex2f_" c'rlVertex2f :: CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlVertex3f_" c'rlVertex3f :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlTexCoord2f_" c'rlTexCoord2f :: CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlNormal3f_" c'rlNormal3f :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlColor4ub_" c'rlColor4ub :: CUChar -> CUChar -> CUChar -> CUChar -> IO ()

foreign import ccall safe "rlgl_bindings.h rlColor3f_" c'rlColor3f :: CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlColor4f_" c'rlColor4f :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableVertexArray_" c'rlEnableVertexArray :: CUInt -> IO CBool

foreign import ccall safe "rlgl_bindings.h rlEnableVertexBuffer_" c'rlEnableVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableVertexBufferElement_" c'rlEnableVertexBufferElement :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableVertexAttribute_" c'rlEnableVertexAttribute :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableVertexAttribute_" c'rlDisableVertexAttribute :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlActiveTextureSlot_" c'rlActiveTextureSlot :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableTexture_" c'rlEnableTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableTextureCubemap_" c'rlEnableTextureCubemap :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlTextureParameters_" c'rlTextureParameters :: CUInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlCubemapParameters_" c'rlCubemapParameters :: CUInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableShader_" c'rlEnableShader :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableFramebuffer_" c'rlEnableFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlActiveDrawBuffers_" c'rlActiveDrawBuffers :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlBlitFramebuffer_" c'rlBlitFramebuffer :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlBindFramebuffer_" c'rlBindFramebuffer :: CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlColorMask_" c'rlColorMask :: CBool -> CBool -> CBool -> CBool -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetCullFace_" c'rlSetCullFace :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlScissor_" c'rlScissor :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetLineWidth_" c'rlSetLineWidth :: CFloat -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetLineWidth_" c'rlGetLineWidth :: IO CFloat

foreign import ccall safe "rlgl_bindings.h rlIsStereoRenderEnabled_" c'rlIsStereoRenderEnabled :: IO CBool

foreign import ccall safe "rlgl_bindings.h rlClearColor_" c'rlClearColor :: CUChar -> CUChar -> CUChar -> CUChar -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetBlendMode_" c'rlSetBlendMode :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetBlendFactors_" c'rlSetBlendFactors :: CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetBlendFactorsSeparate_" c'rlSetBlendFactorsSeparate :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlglInit_" c'rlglInit :: CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadExtensions_" c'rlLoadExtensions :: Ptr () -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetVersion_" c'rlGetVersion :: IO CInt

foreign import ccall safe "rlgl_bindings.h rlSetFramebufferWidth_" c'rlSetFramebufferWidth :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetFramebufferWidth_" c'rlGetFramebufferWidth :: IO CInt

foreign import ccall safe "rlgl_bindings.h rlSetFramebufferHeight_" c'rlSetFramebufferHeight :: CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetFramebufferHeight_" c'rlGetFramebufferHeight :: IO CInt

foreign import ccall safe "rlgl_bindings.h rlGetTextureIdDefault_" c'rlGetTextureIdDefault :: IO CUInt

foreign import ccall safe "rlgl_bindings.h rlGetShaderIdDefault_" c'rlGetShaderIdDefault :: IO CUInt

foreign import ccall safe "rlgl_bindings.h rlGetShaderLocsDefault_" c'rlGetShaderLocsDefault :: IO (Ptr CInt)

foreign import ccall safe "rlgl_bindings.h rlLoadRenderBatch_" c'rlLoadRenderBatch :: CInt -> CInt -> IO (Ptr RLRenderBatch)

foreign import ccall safe "rlgl_bindings.h rlUnloadRenderBatch_" c'rlUnloadRenderBatch :: Ptr RLRenderBatch -> IO ()

foreign import ccall safe "rlgl_bindings.h rlDrawRenderBatch_" c'rlDrawRenderBatch :: Ptr RLRenderBatch -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetRenderBatchActive_" c'rlSetRenderBatchActive :: Ptr RLRenderBatch -> IO ()

foreign import ccall safe "rlgl_bindings.h rlCheckRenderBatchLimit_" c'rlCheckRenderBatchLimit :: CInt -> IO CBool

foreign import ccall safe "rlgl_bindings.h rlSetTexture_" c'rlSetTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadVertexArray_" c'rlLoadVertexArray :: IO CUInt

foreign import ccall safe "rlgl_bindings.h rlLoadVertexBuffer_" c'rlLoadVertexBuffer :: Ptr () -> CInt -> CBool -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlLoadVertexBufferElement_" c'rlLoadVertexBufferElement :: Ptr () -> CInt -> CBool -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlUpdateVertexBuffer_" c'rlUpdateVertexBuffer :: CUInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlUpdateVertexBufferElements_" c'rlUpdateVertexBufferElements :: CUInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlUnloadVertexArray_" c'rlUnloadVertexArray :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlUnloadVertexBuffer_" c'rlUnloadVertexBuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetVertexAttribute_" c'rlSetVertexAttribute :: CUInt -> CInt -> CInt -> CBool -> CInt -> Ptr () -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetVertexAttributeDivisor_" c'rlSetVertexAttributeDivisor :: CUInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetVertexAttributeDefault_" c'rlSetVertexAttributeDefault :: CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlDrawVertexArray_" c'rlDrawVertexArray :: CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlDrawVertexArrayElements_" c'rlDrawVertexArrayElements :: CInt -> CInt -> Ptr () -> IO ()

foreign import ccall safe "rlgl_bindings.h rlDrawVertexArrayInstanced_" c'rlDrawVertexArrayInstanced :: CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlDrawVertexArrayElementsInstanced_" c'rlDrawVertexArrayElementsInstanced :: CInt -> CInt -> Ptr () -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadTexture_" c'rlLoadTexture :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlLoadTextureDepth_" c'rlLoadTextureDepth :: CInt -> CInt -> CBool -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlLoadTextureCubemap_" c'rlLoadTextureCubemap :: Ptr () -> CInt -> CInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlUpdateTexture_" c'rlUpdateTexture :: CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetGlTextureFormats_" c'rlGetGlTextureFormats :: CInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetPixelFormatName_" c'rlGetPixelFormatName :: CUInt -> IO CString

foreign import ccall safe "rlgl_bindings.h rlUnloadTexture_" c'rlUnloadTexture :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGenTextureMipmaps_" c'rlGenTextureMipmaps :: CUInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlReadTexturePixels_" c'rlReadTexturePixels :: CUInt -> CInt -> CInt -> CInt -> IO (Ptr ())

foreign import ccall safe "rlgl_bindings.h rlReadScreenPixels_" c'rlReadScreenPixels :: CInt -> CInt -> IO (Ptr CUChar)

foreign import ccall safe "rlgl_bindings.h rlLoadFramebuffer_" c'rlLoadFramebuffer :: CInt -> CInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlFramebufferAttach_" c'rlFramebufferAttach :: CUInt -> CUInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlFramebufferComplete_" c'rlFramebufferComplete :: CUInt -> IO CBool

foreign import ccall safe "rlgl_bindings.h rlUnloadFramebuffer_" c'rlUnloadFramebuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadShaderCode_" c'rlLoadShaderCode :: CString -> CString -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlCompileShader_" c'rlCompileShader :: CString -> CInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlLoadShaderProgram_" c'rlLoadShaderProgram :: CUInt -> CUInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlUnloadShaderProgram_" c'rlUnloadShaderProgram :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetLocationUniform_" c'rlGetLocationUniform :: CUInt -> CString -> IO CInt

foreign import ccall safe "rlgl_bindings.h rlGetLocationAttrib_" c'rlGetLocationAttrib :: CUInt -> CString -> IO CInt

foreign import ccall safe "rlgl_bindings.h rlSetUniform_" c'rlSetUniform :: CInt -> Ptr () -> CInt -> CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetUniformMatrix_" c'rlSetUniformMatrix :: CInt -> Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetUniformSampler_" c'rlSetUniformSampler :: CInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetShader_" c'rlSetShader :: CUInt -> Ptr CInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadComputeShaderProgram_" c'rlLoadComputeShaderProgram :: CUInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlComputeShaderDispatch_" c'rlComputeShaderDispatch :: CUInt -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadShaderBuffer_" c'rlLoadShaderBuffer :: CUInt -> Ptr () -> CInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlUnloadShaderBuffer_" c'rlUnloadShaderBuffer :: CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlUpdateShaderBuffer_" c'rlUpdateShaderBuffer :: CUInt -> Ptr () -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlBindShaderBuffer_" c'rlBindShaderBuffer :: CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlReadShaderBuffer_" c'rlReadShaderBuffer :: CUInt -> Ptr () -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlCopyShaderBuffer_" c'rlCopyShaderBuffer :: CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetShaderBufferSize_" c'rlGetShaderBufferSize :: CUInt -> IO CUInt

foreign import ccall safe "rlgl_bindings.h rlBindImageTexture_" c'rlBindImageTexture :: CUInt -> CUInt -> CInt -> CBool -> IO ()

foreign import ccall safe "rlgl_bindings.h rlGetMatrixModelview_" c'rlGetMatrixModelview :: IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlGetMatrixProjection_" c'rlGetMatrixProjection :: IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlGetMatrixTransform_" c'rlGetMatrixTransform :: IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlGetMatrixProjectionStereo_" c'rlGetMatrixProjectionStereo :: CInt -> IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlGetMatrixViewOffsetStereo_" c'rlGetMatrixViewOffsetStereo :: CInt -> IO (Ptr Matrix)

foreign import ccall safe "rlgl_bindings.h rlSetMatrixProjection_" c'rlSetMatrixProjection :: Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetMatrixModelview_" c'rlSetMatrixModelview :: Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetMatrixProjectionStereo_" c'rlSetMatrixProjectionStereo :: Ptr Matrix -> Ptr Matrix -> IO ()

foreign import ccall safe "rlgl_bindings.h rlSetMatrixViewOffsetStereo_" c'rlSetMatrixViewOffsetStereo :: Ptr Matrix -> Ptr Matrix -> IO ()

foreign import ccall safe "rl_internal.h rlGetPixelDataSize" c'rlGetPixelDataSize :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall safe "rlgl_bindings.h rlPushMatrix_" c'rlPushMatrix :: IO ()

foreign import ccall safe "rlgl_bindings.h rlPopMatrix_" c'rlPopMatrix :: IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadIdentity_" c'rlLoadIdentity :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnd_" c'rlEnd :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableVertexArray_" c'rlDisableVertexArray :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableVertexBuffer_" c'rlDisableVertexBuffer :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableVertexBufferElement_" c'rlDisableVertexBufferElement :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableTexture_" c'rlDisableTexture :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableTextureCubemap_" c'rlDisableTextureCubemap :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableShader_" c'rlDisableShader :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableFramebuffer_" c'rlDisableFramebuffer :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableColorBlend_" c'rlEnableColorBlend :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableColorBlend_" c'rlDisableColorBlend :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableDepthTest_" c'rlEnableDepthTest :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableDepthTest_" c'rlDisableDepthTest :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableDepthMask_" c'rlEnableDepthMask :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableDepthMask_" c'rlDisableDepthMask :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableBackfaceCulling_" c'rlEnableBackfaceCulling :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableBackfaceCulling_" c'rlDisableBackfaceCulling :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableScissorTest_" c'rlEnableScissorTest :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableScissorTest_" c'rlDisableScissorTest :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableWireMode_" c'rlEnableWireMode :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnablePointMode_" c'rlEnablePointMode :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableWireMode_" c'rlDisableWireMode :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableSmoothLines_" c'rlEnableSmoothLines :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableSmoothLines_" c'rlDisableSmoothLines :: IO ()

foreign import ccall safe "rlgl_bindings.h rlEnableStereoRender_" c'rlEnableStereoRender :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDisableStereoRender_" c'rlDisableStereoRender :: IO ()

foreign import ccall safe "rlgl_bindings.h rlClearScreenBuffers_" c'rlClearScreenBuffers :: IO ()

foreign import ccall safe "rlgl_bindings.h rlCheckErrors_" c'rlCheckErrors :: IO ()

foreign import ccall safe "rlgl_bindings.h rlglClose_" c'rlglClose :: IO ()

foreign import ccall safe "rlgl_bindings.h rlDrawRenderBatchActive_" c'rlDrawRenderBatchActive :: IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadDrawCube_" c'rlLoadDrawCube :: IO ()

foreign import ccall safe "rlgl_bindings.h rlLoadDrawQuad_" c'rlLoadDrawQuad :: IO ()

#endif

-- TODO: redesign this

-- foreign import ccall safe "wrapper"
--   mk'TraceLogCallback ::
--     (CInt -> CString -> __builtin_va_list -> IO ()) -> IO TraceLogCallback
-- foreign import ccall safe "dynamic"
--   mK'TraceLogCallback ::
--     TraceLogCallback -> (CInt -> CString -> __builtin_va_list -> IO ())

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

foreign import ccall safe "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO AudioCallback

foreign import ccall safe "dynamic"
  mK'audioCallback ::
    AudioCallback -> (Ptr () -> CUInt -> IO ())
