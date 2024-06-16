{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings to @rcore@
module Raylib.Core
  ( -- * High level
    initWindow,
    windowShouldClose,
    closeWindow,
    isWindowReady,
    isWindowFullscreen,
    isWindowHidden,
    isWindowMinimized,
    isWindowMaximized,
    isWindowFocused,
    isWindowResized,
    isWindowState,
    setWindowState,
    clearWindowState,
    toggleFullscreen,
    toggleBorderlessWindowed,
    maximizeWindow,
    minimizeWindow,
    restoreWindow,
    setWindowIcon,
    setWindowIcons,
    setWindowTitle,
    setWindowPosition,
    setWindowMonitor,
    setWindowMinSize,
    setWindowMaxSize,
    setWindowSize,
    setWindowOpacity,
    setWindowFocused,
    getWindowHandle,
    getScreenWidth,
    getScreenHeight,
    getRenderWidth,
    getRenderHeight,
    getMonitorCount,
    getCurrentMonitor,
    getMonitorPosition,
    getMonitorWidth,
    getMonitorHeight,
    getMonitorPhysicalWidth,
    getMonitorPhysicalHeight,
    getMonitorRefreshRate,
    getWindowPosition,
    getWindowScaleDPI,
    getMonitorName,
    setClipboardText,
    getClipboardText,
    enableEventWaiting,
    disableEventWaiting,
    swapScreenBuffer,
    pollInputEvents,
    waitTime,
    showCursor,
    hideCursor,
    isCursorHidden,
    enableCursor,
    disableCursor,
    isCursorOnScreen,
    clearBackground,
    beginDrawing,
    endDrawing,
    beginMode2D,
    endMode2D,
    beginMode3D,
    endMode3D,
    beginTextureMode,
    endTextureMode,
    beginShaderMode,
    endShaderMode,
    beginBlendMode,
    endBlendMode,
    beginScissorMode,
    endScissorMode,
    beginVrStereoMode,
    endVrStereoMode,
    loadVrStereoConfig,
    loadShader,
    loadShaderFromMemory,
    isShaderReady,
    getShaderLocation,
    getShaderLocationAttrib,
    setShaderValue,
    setShaderValueV,
    nativeSetShaderValue,
    nativeSetShaderValueV,
    setShaderValueMatrix,
    setShaderValueTexture,
    unloadShader,
    getScreenToWorldRay,
    getScreenToWorldRayEx,
    getCameraMatrix,
    getCameraMatrix2D,
    getWorldToScreen,
    getWorldToScreenEx,
    getWorldToScreen2D,
    getScreenToWorld2D,
    setTargetFPS,
    getFPS,
    getFrameTime,
    getTime,
    setRandomSeed,
    getRandomValue,
    loadRandomSequence,
    takeScreenshot,
    setConfigFlags,
    traceLog,
    setTraceLogLevel,
    openURL,
    setLoadFileDataCallback,
    setSaveFileDataCallback,
    setLoadFileTextCallback,
    setSaveFileTextCallback,
    loadFileData,
    saveFileData,
    exportDataAsCode,
    loadFileText,
    saveFileText,
    fileExists,
    directoryExists,
    isFileExtension,
    getFileLength,
    getFileExtension,
    getFileName,
    getFileNameWithoutExt,
    getDirectoryPath,
    getPrevDirectoryPath,
    getWorkingDirectory,
    getApplicationDirectory,
    changeDirectory,
    isPathFile,
    isFileNameValid,
    loadDirectoryFiles,
    loadDirectoryFilesEx,
    isFileDropped,
    loadDroppedFiles,
    getFileModTime,
    compressData,
    decompressData,
    encodeDataBase64,
    decodeDataBase64,
    loadAutomationEventList,
    newAutomationEventList,
    exportAutomationEventList,
    setAutomationEventList,
    setAutomationEventBaseFrame,
    startAutomationEventRecording,
    stopAutomationEventRecording,
    playAutomationEvent,
    peekAutomationEventList,
    freeAutomationEventList,
    isKeyPressed,
    isKeyPressedRepeat,
    isKeyDown,
    isKeyReleased,
    isKeyUp,
    setExitKey,
    getKeyPressed,
    getCharPressed,
    isGamepadAvailable,
    getGamepadName,
    isGamepadButtonPressed,
    isGamepadButtonDown,
    isGamepadButtonReleased,
    isGamepadButtonUp,
    getGamepadButtonPressed,
    getGamepadAxisCount,
    getGamepadAxisMovement,
    setGamepadMappings,
    setGamepadVibration,
    isMouseButtonPressed,
    isMouseButtonDown,
    isMouseButtonReleased,
    isMouseButtonUp,
    getMouseX,
    getMouseY,
    getMousePosition,
    getMouseDelta,
    setMousePosition,
    setMouseOffset,
    setMouseScale,
    getMouseWheelMove,
    getMouseWheelMoveV,
    setMouseCursor,
    getTouchX,
    getTouchY,
    getTouchPosition,
    getTouchPointId,
    getTouchPointCount,
    setGesturesEnabled,
    isGestureDetected,
    getGestureDetected,
    getGestureHoldDuration,
    getGestureDragVector,
    getGestureDragAngle,
    getGesturePinchVector,
    getGesturePinchAngle,

    -- * Native
    c'initWindow,
    c'windowShouldClose,
    c'closeWindow,
    c'isWindowReady,
    c'isWindowFullscreen,
    c'isWindowHidden,
    c'isWindowMinimized,
    c'isWindowMaximized,
    c'isWindowFocused,
    c'isWindowResized,
    c'isWindowState,
    c'setWindowState,
    c'clearWindowState,
    c'toggleFullscreen,
    c'toggleBorderlessWindowed,
    c'maximizeWindow,
    c'minimizeWindow,
    c'restoreWindow,
    c'setWindowIcon,
    c'setWindowIcons,
    c'setWindowTitle,
    c'setWindowPosition,
    c'setWindowMonitor,
    c'setWindowMinSize,
    c'setWindowMaxSize,
    c'setWindowSize,
    c'setWindowOpacity,
    c'setWindowFocused,
    c'getWindowHandle,
    c'getScreenWidth,
    c'getScreenHeight,
    c'getRenderWidth,
    c'getRenderHeight,
    c'getMonitorCount,
    c'getCurrentMonitor,
    c'getMonitorPosition,
    c'getMonitorWidth,
    c'getMonitorHeight,
    c'getMonitorPhysicalWidth,
    c'getMonitorPhysicalHeight,
    c'getMonitorRefreshRate,
    c'getWindowPosition,
    c'getWindowScaleDPI,
    c'getMonitorName,
    c'setClipboardText,
    c'getClipboardText,
    c'enableEventWaiting,
    c'disableEventWaiting,
    c'swapScreenBuffer,
    c'pollInputEvents,
    c'waitTime,
    c'showCursor,
    c'hideCursor,
    c'isCursorHidden,
    c'enableCursor,
    c'disableCursor,
    c'isCursorOnScreen,
    c'clearBackground,
    c'beginDrawing,
    c'endDrawing,
    c'beginMode2D,
    c'endMode2D,
    c'beginMode3D,
    c'endMode3D,
    c'beginTextureMode,
    c'endTextureMode,
    c'beginShaderMode,
    c'endShaderMode,
    c'beginBlendMode,
    c'endBlendMode,
    c'beginScissorMode,
    c'endScissorMode,
    c'beginVrStereoMode,
    c'endVrStereoMode,
    c'loadVrStereoConfig,
    c'unloadVrStereoConfig,
    c'loadShader,
    c'loadShaderFromMemory,
    c'isShaderReady,
    c'getShaderLocation,
    c'getShaderLocationAttrib,
    c'setShaderValue,
    c'setShaderValueV,
    c'setShaderValueMatrix,
    c'setShaderValueTexture,
    c'unloadShader,
    c'getScreenToWorldRay,
    c'getScreenToWorldRayEx,
    c'getCameraMatrix,
    c'getCameraMatrix2D,
    c'getWorldToScreen,
    c'getScreenToWorld2D,
    c'getWorldToScreenEx,
    c'getWorldToScreen2D,
    c'setTargetFPS,
    c'getFPS,
    c'getFrameTime,
    c'getTime,
    c'setRandomSeed,
    c'getRandomValue,
    c'loadRandomSequence,
    c'takeScreenshot,
    c'setConfigFlags,
    c'traceLog,
    c'setTraceLogLevel,
    c'memAlloc,
    c'memRealloc,
    c'memFree,
    c'openURL,
    c'setLoadFileDataCallback,
    c'setSaveFileDataCallback,
    c'setLoadFileTextCallback,
    c'setSaveFileTextCallback,
    c'loadFileData,
    c'unloadFileData,
    c'saveFileData,
    c'exportDataAsCode,
    c'loadFileText,
    c'unloadFileText,
    c'saveFileText,
    c'fileExists,
    c'directoryExists,
    c'isFileExtension,
    c'getFileLength,
    c'getFileExtension,
    c'getFileName,
    c'getFileNameWithoutExt,
    c'getDirectoryPath,
    c'getPrevDirectoryPath,
    c'getWorkingDirectory,
    c'getApplicationDirectory,
    c'changeDirectory,
    c'isPathFile,
    c'isFileNameValid,
    c'loadDirectoryFiles,
    c'loadDirectoryFilesEx,
    c'unloadDirectoryFiles,
    c'isFileDropped,
    c'loadDroppedFiles,
    c'unloadDroppedFiles,
    c'getFileModTime,
    c'compressData,
    c'decompressData,
    c'encodeDataBase64,
    c'decodeDataBase64,
    c'loadAutomationEventList,
    c'exportAutomationEventList,
    c'setAutomationEventList,
    c'setAutomationEventBaseFrame,
    c'startAutomationEventRecording,
    c'stopAutomationEventRecording,
    c'playAutomationEvent,
    c'isKeyPressed,
    c'isKeyPressedRepeat,
    c'isKeyDown,
    c'isKeyReleased,
    c'isKeyUp,
    c'setExitKey,
    c'getKeyPressed,
    c'getCharPressed,
    c'isGamepadAvailable,
    c'getGamepadName,
    c'isGamepadButtonPressed,
    c'isGamepadButtonDown,
    c'isGamepadButtonReleased,
    c'isGamepadButtonUp,
    c'getGamepadButtonPressed,
    c'getGamepadAxisCount,
    c'getGamepadAxisMovement,
    c'setGamepadMappings,
    c'setGamepadVibration,
    c'isMouseButtonPressed,
    c'isMouseButtonDown,
    c'isMouseButtonReleased,
    c'isMouseButtonUp,
    c'getMouseX,
    c'getMouseY,
    c'getMousePosition,
    c'getMouseDelta,
    c'setMousePosition,
    c'setMouseOffset,
    c'setMouseScale,
    c'getMouseWheelMove,
    c'getMouseWheelMoveV,
    c'setMouseCursor,
    c'getTouchX,
    c'getTouchY,
    c'getTouchPosition,
    c'getTouchPointId,
    c'getTouchPointCount,
    c'setGesturesEnabled,
    c'isGestureDetected,
    c'getGestureDetected,
    c'getGestureHoldDuration,
    c'getGestureDragVector,
    c'getGestureDragAngle,
    c'getGesturePinchVector,
    c'getGesturePinchAngle,

    -- * Callbacks
    mk'loadFileDataCallback,
    mk'saveFileDataCallback,
    mk'loadFileTextCallback,
    mk'saveFileTextCallback,
    createLoadFileDataCallback,
    createSaveFileDataCallback,
    createLoadFileTextCallback,
    createSaveFileTextCallback,
  )
where

import Data.IORef (modifyIORef', readIORef)
import qualified Data.Map as Map
import Foreign
  ( Ptr,
    Storable (peek, poke, sizeOf),
    castFunPtr,
    castPtr,
    finalizeForeignPtr,
    fromBool,
    malloc,
    newArray,
    peekArray,
    toBool,
    withForeignPtr,
  )
import Foreign.C
  ( CBool (..),
    CDouble (..),
    CFloat (..),
    CInt (..),
    CLong (..),
    CString,
    CUChar (..),
    CUInt (..),
    newCString,
    peekCString,
    withCString,
  )
import Foreign.Ptr (nullPtr)
import Raylib.Internal (WindowResources, addAutomationEventList, addFunPtr, addShaderId, defaultWindowResources, shaderLocations, unloadAutomationEventLists, unloadFrameBuffers, unloadFunPtrs, unloadShaders, unloadSingleAutomationEventList, unloadSingleShader, unloadTextures, unloadVaoIds, unloadVboIds)
import Raylib.Internal.Foreign (configsToBitflag, pop, popCArray, popCString, withFreeable, withFreeableArray, withFreeableArrayLen, withMaybeCString)
import Raylib.Internal.TH (genNative)
import Raylib.Types
  ( AutomationEvent,
    AutomationEventList,
    AutomationEventListRef,
    BlendMode,
    C'LoadFileDataCallback,
    C'LoadFileTextCallback,
    C'SaveFileDataCallback,
    C'SaveFileTextCallback,
    Camera2D,
    Camera3D,
    Color,
    ConfigFlag,
    FilePathList,
    GamepadAxis,
    GamepadButton,
    Gesture,
    Image,
    KeyboardKey,
    LoadFileDataCallback,
    LoadFileTextCallback,
    Matrix,
    MouseButton,
    MouseCursor,
    Ray,
    RenderTexture,
    SaveFileDataCallback,
    SaveFileTextCallback,
    Shader (shader'id),
    ShaderUniformData,
    ShaderUniformDataV,
    Texture,
    TraceLogLevel,
    Vector2,
    Vector3,
    VrDeviceInfo,
    VrStereoConfig,
    unpackShaderUniformData,
    unpackShaderUniformDataV,
  )
import GHC.IO (unsafePerformIO)

$( genNative
     [ ("c'initWindow", "InitWindow_", "rl_bindings.h", [t|CInt -> CInt -> CString -> IO ()|], False),
       ("c'windowShouldClose", "WindowShouldClose_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'closeWindow", "CloseWindow_", "rl_bindings.h", [t|IO ()|], False),
       ("c'isWindowReady", "IsWindowReady_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowFullscreen", "IsWindowFullscreen_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowHidden", "IsWindowHidden_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowMinimized", "IsWindowMinimized_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowMaximized", "IsWindowMaximized_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowFocused", "IsWindowFocused_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowResized", "IsWindowResized_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'isWindowState", "IsWindowState_", "rl_bindings.h", [t|CUInt -> IO CBool|], False),
       ("c'setWindowState", "SetWindowState_", "rl_bindings.h", [t|CUInt -> IO ()|], False),
       ("c'clearWindowState", "ClearWindowState_", "rl_bindings.h", [t|CUInt -> IO ()|], False),
       ("c'toggleFullscreen", "ToggleFullscreen_", "rl_bindings.h", [t|IO ()|], False),
       ("c'toggleBorderlessWindowed", "ToggleBorderlessWindowed_", "rl_bindings.h", [t|IO ()|], False),
       ("c'maximizeWindow", "MaximizeWindow_", "rl_bindings.h", [t|IO ()|], False),
       ("c'minimizeWindow", "MinimizeWindow_", "rl_bindings.h", [t|IO ()|], False),
       ("c'restoreWindow", "RestoreWindow_", "rl_bindings.h", [t|IO ()|], False),
       ("c'setWindowIcon", "SetWindowIcon_", "rl_bindings.h", [t|Ptr Image -> IO ()|], False),
       ("c'setWindowIcons", "SetWindowIcons_", "rl_bindings.h", [t|Ptr Image -> CInt -> IO ()|], False),
       ("c'setWindowTitle", "SetWindowTitle_", "rl_bindings.h", [t|CString -> IO ()|], False),
       ("c'setWindowPosition", "SetWindowPosition_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|], False),
       ("c'setWindowMonitor", "SetWindowMonitor_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'setWindowMinSize", "SetWindowMinSize_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|], False),
       ("c'setWindowMaxSize", "SetWindowMaxSize_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|], False),
       ("c'setWindowSize", "SetWindowSize_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|], False),
       ("c'setWindowOpacity", "SetWindowOpacity_", "rl_bindings.h", [t|CFloat -> IO ()|], False),
       ("c'setWindowFocused", "SetWindowFocused_", "rl_bindings.h", [t|IO ()|], False),
       ("c'getWindowHandle", "GetWindowHandle_", "rl_bindings.h", [t|IO (Ptr ())|], False),
       ("c'getScreenWidth", "GetScreenWidth_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getScreenHeight", "GetScreenHeight_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getRenderWidth", "GetRenderWidth_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getRenderHeight", "GetRenderHeight_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getMonitorCount", "GetMonitorCount_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getCurrentMonitor", "GetCurrentMonitor_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getMonitorPosition", "GetMonitorPosition_", "rl_bindings.h", [t|CInt -> IO (Ptr Vector2)|], False),
       ("c'getMonitorWidth", "GetMonitorWidth_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getMonitorHeight", "GetMonitorHeight_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getMonitorPhysicalWidth", "GetMonitorPhysicalWidth_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getMonitorPhysicalHeight", "GetMonitorPhysicalHeight_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getMonitorRefreshRate", "GetMonitorRefreshRate_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getWindowPosition", "GetWindowPosition_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'getWindowScaleDPI", "GetWindowScaleDPI_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'getMonitorName", "GetMonitorName_", "rl_bindings.h", [t|CInt -> IO CString|], False),
       ("c'setClipboardText", "SetClipboardText_", "rl_bindings.h", [t|CString -> IO ()|], False),
       ("c'getClipboardText", "GetClipboardText_", "rl_bindings.h", [t|IO CString|], False),
       ("c'enableEventWaiting", "EnableEventWaiting_", "rl_bindings.h", [t|IO ()|], False),
       ("c'disableEventWaiting", "DisableEventWaiting_", "rl_bindings.h", [t|IO ()|], False),
       ("c'swapScreenBuffer", "SwapScreenBuffer_", "rl_bindings.h", [t|IO ()|], False),
       ("c'pollInputEvents", "PollInputEvents_", "rl_bindings.h", [t|IO ()|], False),
       ("c'waitTime", "WaitTime_", "rl_bindings.h", [t|CDouble -> IO ()|], False),
       ("c'showCursor", "ShowCursor_", "rl_bindings.h", [t|IO ()|], False),
       ("c'hideCursor", "HideCursor_", "rl_bindings.h", [t|IO ()|], False),
       ("c'isCursorHidden", "IsCursorHidden_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'enableCursor", "EnableCursor_", "rl_bindings.h", [t|IO ()|], False),
       ("c'disableCursor", "DisableCursor_", "rl_bindings.h", [t|IO ()|], False),
       ("c'isCursorOnScreen", "IsCursorOnScreen_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'clearBackground", "ClearBackground_", "rl_bindings.h", [t|Ptr Color -> IO ()|], False),
       ("c'beginDrawing", "BeginDrawing_", "rl_bindings.h", [t|IO ()|], False),
       ("c'endDrawing", "EndDrawing_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginMode2D", "BeginMode2D_", "rl_bindings.h", [t|Ptr Camera2D -> IO ()|], False),
       ("c'endMode2D", "EndMode2D_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginMode3D", "BeginMode3D_", "rl_bindings.h", [t|Ptr Camera3D -> IO ()|], False),
       ("c'endMode3D", "EndMode3D_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginTextureMode", "BeginTextureMode_", "rl_bindings.h", [t|Ptr RenderTexture -> IO ()|], False),
       ("c'endTextureMode", "EndTextureMode_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginShaderMode", "BeginShaderMode_", "rl_bindings.h", [t|Ptr Shader -> IO ()|], False),
       ("c'endShaderMode", "EndShaderMode_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginBlendMode", "BeginBlendMode_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'endBlendMode", "EndBlendMode_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginScissorMode", "BeginScissorMode_", "rl_bindings.h", [t|CInt -> CInt -> CInt -> CInt -> IO ()|], False),
       ("c'endScissorMode", "EndScissorMode_", "rl_bindings.h", [t|IO ()|], False),
       ("c'beginVrStereoMode", "BeginVrStereoMode_", "rl_bindings.h", [t|Ptr VrStereoConfig -> IO ()|], False),
       ("c'endVrStereoMode", "EndVrStereoMode_", "rl_bindings.h", [t|IO ()|], False),
       ("c'loadVrStereoConfig", "LoadVrStereoConfig_", "rl_bindings.h", [t|Ptr VrDeviceInfo -> IO (Ptr VrStereoConfig)|], False),
       ("c'unloadVrStereoConfig", "UnloadVrStereoConfig_", "rl_bindings.h", [t|Ptr VrStereoConfig -> IO ()|], False),
       ("c'loadShader", "LoadShader_", "rl_bindings.h", [t|CString -> CString -> IO (Ptr Shader)|], False),
       ("c'loadShaderFromMemory", "LoadShaderFromMemory_", "rl_bindings.h", [t|CString -> CString -> IO (Ptr Shader)|], False),
       ("c'isShaderReady", "IsShaderReady_", "rl_bindings.h", [t|Ptr Shader -> IO CBool|], False),
       ("c'getShaderLocation", "GetShaderLocation_", "rl_bindings.h", [t|Ptr Shader -> CString -> IO CInt|], False),
       ("c'getShaderLocationAttrib", "GetShaderLocationAttrib_", "rl_bindings.h", [t|Ptr Shader -> CString -> IO CInt|], False),
       ("c'setShaderValue", "SetShaderValue_", "rl_bindings.h", [t|Ptr Shader -> CInt -> Ptr () -> CInt -> IO ()|], False),
       ("c'setShaderValueV", "SetShaderValueV_", "rl_bindings.h", [t|Ptr Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()|], False),
       ("c'setShaderValueMatrix", "SetShaderValueMatrix_", "rl_bindings.h", [t|Ptr Shader -> CInt -> Ptr Matrix -> IO ()|], False),
       ("c'setShaderValueTexture", "SetShaderValueTexture_", "rl_bindings.h", [t|Ptr Shader -> CInt -> Ptr Texture -> IO ()|], False),
       ("c'unloadShader", "UnloadShader_", "rl_bindings.h", [t|Ptr Shader -> IO ()|], False),
       ("c'getScreenToWorldRay", "GetScreenToWorldRay_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Camera3D -> IO (Ptr Ray)|], False),
       ("c'getScreenToWorldRayEx", "GetScreenToWorldRayEx_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Camera3D -> CFloat -> CFloat -> IO (Ptr Ray)|], False),
       ("c'getCameraMatrix", "GetCameraMatrix_", "rl_bindings.h", [t|Ptr Camera3D -> IO (Ptr Matrix)|], False),
       ("c'getCameraMatrix2D", "GetCameraMatrix2D_", "rl_bindings.h", [t|Ptr Camera2D -> IO (Ptr Matrix)|], False),
       ("c'getWorldToScreen", "GetWorldToScreen_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Camera3D -> IO (Ptr Vector2)|], False),
       ("c'getScreenToWorld2D", "GetScreenToWorld2D_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)|], False),
       ("c'getWorldToScreenEx", "GetWorldToScreenEx_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Camera3D -> CInt -> CInt -> IO (Ptr Vector2)|], False),
       ("c'getWorldToScreen2D", "GetWorldToScreen2D_", "rl_bindings.h", [t|Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)|], False),
       ("c'setTargetFPS", "SetTargetFPS_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'getFPS", "GetFPS_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getFrameTime", "GetFrameTime_", "rl_bindings.h", [t|IO CFloat|], False),
       ("c'getTime", "GetTime_", "rl_bindings.h", [t|IO CDouble|], False),
       ("c'setRandomSeed", "SetRandomSeed_", "rl_bindings.h", [t|CUInt -> IO ()|], False),
       ("c'getRandomValue", "GetRandomValue_", "rl_bindings.h", [t|CInt -> CInt -> IO CInt|], False),
       ("c'loadRandomSequence", "LoadRandomSequence_", "rl_bindings.h", [t|CUInt -> CInt -> CInt -> IO (Ptr CInt)|], False),
       ("c'takeScreenshot", "TakeScreenshot_", "rl_bindings.h", [t|CString -> IO ()|], False),
       ("c'setConfigFlags", "SetConfigFlags_", "rl_bindings.h", [t|CUInt -> IO ()|], False),
       ("c'traceLog", "TraceLog_", "rl_bindings.h", [t|CInt -> CString -> IO ()|], False), -- Uses varags, can't implement complete functionality
       ("c'setTraceLogLevel", "SetTraceLogLevel_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'memAlloc", "MemAlloc_", "rl_bindings.h", [t|CInt -> IO (Ptr ())|], False),
       ("c'memRealloc", "MemRealloc_", "rl_bindings.h", [t|Ptr () -> CInt -> IO (Ptr ())|], False),
       ("c'memFree", "MemFree_", "rl_bindings.h", [t|Ptr () -> IO ()|], False),
       ("c'openURL", "OpenURL_", "rl_bindings.h", [t|CString -> IO ()|], False),
       ("c'setLoadFileDataCallback", "SetLoadFileDataCallback_", "rl_bindings.h", [t|C'LoadFileDataCallback -> IO ()|], False),
       ("c'setSaveFileDataCallback", "SetSaveFileDataCallback_", "rl_bindings.h", [t|C'SaveFileDataCallback -> IO ()|], False),
       ("c'setLoadFileTextCallback", "SetLoadFileTextCallback_", "rl_bindings.h", [t|C'LoadFileTextCallback -> IO ()|], False),
       ("c'setSaveFileTextCallback", "SetSaveFileTextCallback_", "rl_bindings.h", [t|C'SaveFileTextCallback -> IO ()|], False),
       ("c'loadFileData", "LoadFileData_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO (Ptr CUChar)|], True),
       ("c'unloadFileData", "UnloadFileData_", "rl_bindings.h", [t|Ptr CUChar -> IO ()|], False),
       ("c'saveFileData", "SaveFileData_", "rl_bindings.h", [t|CString -> Ptr () -> CInt -> IO CBool|], True),
       ("c'exportDataAsCode", "ExportDataAsCode_", "rl_bindings.h", [t|Ptr CUChar -> CInt -> CString -> IO CBool|], False),
       ("c'loadFileText", "LoadFileText_", "rl_bindings.h", [t|CString -> IO CString|], True),
       ("c'unloadFileText", "UnloadFileText_", "rl_bindings.h", [t|CString -> IO ()|], False),
       ("c'saveFileText", "SaveFileText_", "rl_bindings.h", [t|CString -> CString -> IO CBool|], True),
       ("c'fileExists", "FileExists_", "rl_bindings.h", [t|CString -> IO CBool|], False),
       ("c'directoryExists", "DirectoryExists_", "rl_bindings.h", [t|CString -> IO CBool|], False),
       ("c'isFileExtension", "IsFileExtension_", "rl_bindings.h", [t|CString -> CString -> IO CBool|], False),
       ("c'getFileLength", "GetFileLength_", "rl_bindings.h", [t|CString -> IO CBool|], False),
       ("c'getFileExtension", "GetFileExtension_", "rl_bindings.h", [t|CString -> IO CString|], False),
       ("c'getFileName", "GetFileName_", "rl_bindings.h", [t|CString -> IO CString|], False),
       ("c'getFileNameWithoutExt", "GetFileNameWithoutExt_", "rl_bindings.h", [t|CString -> IO CString|], False),
       ("c'getDirectoryPath", "GetDirectoryPath_", "rl_bindings.h", [t|CString -> IO CString|], False),
       ("c'getPrevDirectoryPath", "GetPrevDirectoryPath_", "rl_bindings.h", [t|CString -> IO CString|], False),
       ("c'getWorkingDirectory", "GetWorkingDirectory_", "rl_bindings.h", [t|IO CString|], False),
       ("c'getApplicationDirectory", "GetApplicationDirectory_", "rl_bindings.h", [t|IO CString|], False),
       ("c'changeDirectory", "ChangeDirectory_", "rl_bindings.h", [t|CString -> IO CBool|], False),
       ("c'isPathFile", "IsPathFile_", "rl_bindings.h", [t|CString -> IO CBool|], False),
       ("c'isFileNameValid", "IsFileNameValid_", "rl_bindings.h", [t|CString -> IO CBool|], False),
       ("c'loadDirectoryFiles", "LoadDirectoryFiles_", "rl_bindings.h", [t|CString -> IO (Ptr FilePathList)|], False),
       ("c'loadDirectoryFilesEx", "LoadDirectoryFilesEx_", "rl_bindings.h", [t|CString -> CString -> CInt -> IO (Ptr FilePathList)|], False),
       ("c'unloadDirectoryFiles", "UnloadDirectoryFiles_", "rl_bindings.h", [t|Ptr FilePathList -> IO ()|], False),
       ("c'isFileDropped", "IsFileDropped_", "rl_bindings.h", [t|IO CBool|], False),
       ("c'loadDroppedFiles", "LoadDroppedFiles_", "rl_bindings.h", [t|IO (Ptr FilePathList)|], False),
       ("c'unloadDroppedFiles", "UnloadDroppedFiles_", "rl_bindings.h", [t|Ptr FilePathList -> IO ()|], False),
       ("c'getFileModTime", "GetFileModTime_", "rl_bindings.h", [t|CString -> IO CLong|], False),
       ("c'compressData", "CompressData_", "rl_bindings.h", [t|Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)|], False),
       ("c'decompressData", "DecompressData_", "rl_bindings.h", [t|Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)|], False),
       ("c'encodeDataBase64", "EncodeDataBase64_", "rl_bindings.h", [t|Ptr CUChar -> CInt -> Ptr CInt -> IO CString|], False),
       ("c'decodeDataBase64", "DecodeDataBase64_", "rl_bindings.h", [t|Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)|], False),
       ("c'loadAutomationEventList", "LoadAutomationEventList_", "rl_bindings.h", [t|CString -> IO (Ptr AutomationEventList)|], False),
       ("c'exportAutomationEventList", "ExportAutomationEventList_", "rl_bindings.h", [t|Ptr AutomationEventList -> CString -> IO CBool|], False),
       ("c'setAutomationEventList", "SetAutomationEventList_", "rl_bindings.h", [t|Ptr AutomationEventList -> IO ()|], False),
       ("c'setAutomationEventBaseFrame", "SetAutomationEventBaseFrame_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'startAutomationEventRecording", "StartAutomationEventRecording_", "rl_bindings.h", [t|IO ()|], False),
       ("c'stopAutomationEventRecording", "StopAutomationEventRecording_", "rl_bindings.h", [t|IO ()|], False),
       ("c'playAutomationEvent", "PlayAutomationEvent", "rl_bindings.h", [t|Ptr AutomationEvent -> IO ()|], False),
       ("c'isKeyPressed", "IsKeyPressed_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isKeyPressedRepeat", "IsKeyPressedRepeat_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isKeyDown", "IsKeyDown_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isKeyReleased", "IsKeyReleased_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isKeyUp", "IsKeyUp_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'setExitKey", "SetExitKey_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'getKeyPressed", "GetKeyPressed_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getCharPressed", "GetCharPressed_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'isGamepadAvailable", "IsGamepadAvailable_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'getGamepadName", "GetGamepadName_", "rl_bindings.h", [t|CInt -> IO CString|], False),
       ("c'isGamepadButtonPressed", "IsGamepadButtonPressed_", "rl_bindings.h", [t|CInt -> CInt -> IO CBool|], False),
       ("c'isGamepadButtonDown", "IsGamepadButtonDown_", "rl_bindings.h", [t|CInt -> CInt -> IO CBool|], False),
       ("c'isGamepadButtonReleased", "IsGamepadButtonReleased_", "rl_bindings.h", [t|CInt -> CInt -> IO CBool|], False),
       ("c'isGamepadButtonUp", "IsGamepadButtonUp_", "rl_bindings.h", [t|CInt -> CInt -> IO CBool|], False),
       ("c'getGamepadButtonPressed", "GetGamepadButtonPressed_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getGamepadAxisCount", "GetGamepadAxisCount_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getGamepadAxisMovement", "GetGamepadAxisMovement_", "rl_bindings.h", [t|CInt -> CInt -> IO CFloat|], False),
       ("c'setGamepadMappings", "SetGamepadMappings_", "rl_bindings.h", [t|CString -> IO CInt|], False),
       ("c'setGamepadVibration", "SetGamepadVibration_", "rl_bindings.h", [t|CInt -> CFloat -> CFloat -> IO ()|], False),
       ("c'isMouseButtonPressed", "IsMouseButtonPressed_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isMouseButtonDown", "IsMouseButtonDown_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isMouseButtonReleased", "IsMouseButtonReleased_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'isMouseButtonUp", "IsMouseButtonUp_", "rl_bindings.h", [t|CInt -> IO CBool|], False),
       ("c'getMouseX", "GetMouseX_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getMouseY", "GetMouseY_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getMousePosition", "GetMousePosition_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'getMouseDelta", "GetMouseDelta_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'setMousePosition", "SetMousePosition_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|], False),
       ("c'setMouseOffset", "SetMouseOffset_", "rl_bindings.h", [t|CInt -> CInt -> IO ()|], False),
       ("c'setMouseScale", "SetMouseScale_", "rl_bindings.h", [t|CFloat -> CFloat -> IO ()|], False),
       ("c'getMouseWheelMove", "GetMouseWheelMove_", "rl_bindings.h", [t|IO CFloat|], False),
       ("c'getMouseWheelMoveV", "GetMouseWheelMoveV_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'setMouseCursor", "SetMouseCursor_", "rl_bindings.h", [t|CInt -> IO ()|], False),
       ("c'getTouchX", "GetTouchX_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getTouchY", "GetTouchY_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getTouchPosition", "GetTouchPosition_", "rl_bindings.h", [t|CInt -> IO (Ptr Vector2)|], False),
       ("c'getTouchPointId", "GetTouchPointId_", "rl_bindings.h", [t|CInt -> IO CInt|], False),
       ("c'getTouchPointCount", "GetTouchPointCount_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'setGesturesEnabled", "SetGesturesEnabled_", "rl_bindings.h", [t|CUInt -> IO ()|], False),
       ("c'isGestureDetected", "IsGestureDetected_", "rl_bindings.h", [t|CUInt -> IO CBool|], False),
       ("c'getGestureDetected", "GetGestureDetected_", "rl_bindings.h", [t|IO CInt|], False),
       ("c'getGestureHoldDuration", "GetGestureHoldDuration_", "rl_bindings.h", [t|IO CFloat|], False),
       ("c'getGestureDragVector", "GetGestureDragVector_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'getGestureDragAngle", "GetGestureDragAngle_", "rl_bindings.h", [t|IO CFloat|], False),
       ("c'getGesturePinchVector", "GetGesturePinchVector_", "rl_bindings.h", [t|IO (Ptr Vector2)|], False),
       ("c'getGesturePinchAngle", "GetGesturePinchAngle_", "rl_bindings.h", [t|IO CFloat|], False)
     ]
 )

initWindow ::
  Int ->
  Int ->
  String ->
  -- | This value must be passed to some @load*@ and @unload*@ functions for
  --   automatic memory management.
  IO WindowResources
initWindow width height title = withCString title (c'initWindow (fromIntegral width) (fromIntegral height)) >> defaultWindowResources

windowShouldClose :: IO Bool
windowShouldClose = toBool <$> c'windowShouldClose

closeWindow :: WindowResources -> IO ()
closeWindow wr = do
  unloadShaders wr
  unloadTextures wr
  unloadFrameBuffers wr
  unloadVaoIds wr
  unloadVboIds wr
  unloadAutomationEventLists wr
  unloadFunPtrs wr
  c'closeWindow

isWindowReady :: IO Bool
isWindowReady = toBool <$> c'isWindowReady

isWindowFullscreen :: IO Bool
isWindowFullscreen = toBool <$> c'isWindowFullscreen

isWindowHidden :: IO Bool
isWindowHidden = toBool <$> c'isWindowHidden

isWindowMinimized :: IO Bool
isWindowMinimized = toBool <$> c'isWindowMinimized

isWindowMaximized :: IO Bool
isWindowMaximized = toBool <$> c'isWindowMaximized

isWindowFocused :: IO Bool
isWindowFocused = toBool <$> c'isWindowFocused

isWindowResized :: IO Bool
isWindowResized = toBool <$> c'isWindowResized

isWindowState :: [ConfigFlag] -> IO Bool
isWindowState flags = toBool <$> c'isWindowState (fromIntegral $ configsToBitflag flags)

setWindowState :: [ConfigFlag] -> IO ()
setWindowState = c'setWindowState . fromIntegral . configsToBitflag

clearWindowState :: [ConfigFlag] -> IO ()
clearWindowState = c'clearWindowState . fromIntegral . configsToBitflag

toggleFullscreen :: IO ()
toggleFullscreen = c'toggleFullscreen

toggleBorderlessWindowed :: IO ()
toggleBorderlessWindowed = c'toggleBorderlessWindowed

maximizeWindow :: IO ()
maximizeWindow = c'maximizeWindow

minimizeWindow :: IO ()
minimizeWindow = c'minimizeWindow

restoreWindow :: IO ()
restoreWindow = c'restoreWindow

setWindowIcon :: Image -> IO ()
setWindowIcon image = withFreeable image c'setWindowIcon

setWindowIcons :: [Image] -> IO ()
setWindowIcons images = withFreeableArrayLen images (\l ptr -> c'setWindowIcons ptr (fromIntegral l))

setWindowTitle :: String -> IO ()
setWindowTitle title = withCString title c'setWindowTitle

setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = c'setWindowPosition (fromIntegral x) (fromIntegral y)

setWindowMonitor :: Int -> IO ()
setWindowMonitor = c'setWindowMonitor . fromIntegral

setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize x y = c'setWindowMinSize (fromIntegral x) (fromIntegral y)

setWindowMaxSize :: Int -> Int -> IO ()
setWindowMaxSize x y = c'setWindowMaxSize (fromIntegral x) (fromIntegral y)

setWindowSize :: Int -> Int -> IO ()
setWindowSize x y = c'setWindowSize (fromIntegral x) (fromIntegral y)

setWindowOpacity :: Float -> IO ()
setWindowOpacity opacity = c'setWindowOpacity $ realToFrac opacity

setWindowFocused :: IO ()
setWindowFocused = c'setWindowFocused

getWindowHandle :: IO (Ptr ())
getWindowHandle = c'getWindowHandle

getScreenWidth :: IO Int
getScreenWidth = fromIntegral <$> c'getScreenWidth

getScreenHeight :: IO Int
getScreenHeight = fromIntegral <$> c'getScreenHeight

getRenderWidth :: IO Int
getRenderWidth = fromIntegral <$> c'getRenderWidth

getRenderHeight :: IO Int
getRenderHeight = fromIntegral <$> c'getRenderHeight

getMonitorCount :: IO Int
getMonitorCount = fromIntegral <$> c'getMonitorCount

getCurrentMonitor :: IO Int
getCurrentMonitor = fromIntegral <$> c'getCurrentMonitor

getMonitorPosition :: Int -> IO Vector2
getMonitorPosition monitor = c'getMonitorPosition (fromIntegral monitor) >>= pop

getMonitorWidth :: Int -> IO Int
getMonitorWidth monitor = fromIntegral <$> c'getMonitorWidth (fromIntegral monitor)

getMonitorHeight :: Int -> IO Int
getMonitorHeight monitor = fromIntegral <$> c'getMonitorHeight (fromIntegral monitor)

getMonitorPhysicalWidth :: Int -> IO Int
getMonitorPhysicalWidth monitor = fromIntegral <$> c'getMonitorPhysicalWidth (fromIntegral monitor)

getMonitorPhysicalHeight :: Int -> IO Int
getMonitorPhysicalHeight monitor = fromIntegral <$> c'getMonitorPhysicalHeight (fromIntegral monitor)

getMonitorRefreshRate :: Int -> IO Int
getMonitorRefreshRate monitor = fromIntegral <$> c'getMonitorRefreshRate (fromIntegral monitor)

getWindowPosition :: IO Vector2
getWindowPosition = c'getWindowPosition >>= pop

getWindowScaleDPI :: IO Vector2
getWindowScaleDPI = c'getWindowScaleDPI >>= pop

getMonitorName :: Int -> IO String
getMonitorName monitor = c'getMonitorName (fromIntegral monitor) >>= peekCString

setClipboardText :: String -> IO ()
setClipboardText text = withCString text c'setClipboardText

getClipboardText :: IO String
getClipboardText = c'getClipboardText >>= peekCString

enableEventWaiting :: IO ()
enableEventWaiting = c'enableEventWaiting

disableEventWaiting :: IO ()
disableEventWaiting = c'disableEventWaiting

swapScreenBuffer :: IO ()
swapScreenBuffer = c'swapScreenBuffer

pollInputEvents :: IO ()
pollInputEvents = c'pollInputEvents

waitTime :: Double -> IO ()
waitTime seconds = c'waitTime $ realToFrac seconds

showCursor :: IO ()
showCursor = c'showCursor

hideCursor :: IO ()
hideCursor = c'hideCursor

isCursorHidden :: IO Bool
isCursorHidden = toBool <$> c'isCursorHidden

enableCursor :: IO ()
enableCursor = c'enableCursor

disableCursor :: IO ()
disableCursor = c'disableCursor

isCursorOnScreen :: IO Bool
isCursorOnScreen = toBool <$> c'isCursorOnScreen

clearBackground :: Color -> IO ()
clearBackground color = withFreeable color c'clearBackground

beginDrawing :: IO ()
beginDrawing = c'beginDrawing

endDrawing :: IO ()
endDrawing = c'endDrawing

beginMode2D :: Camera2D -> IO ()
beginMode2D camera = withFreeable camera c'beginMode2D

endMode2D :: IO ()
endMode2D = c'endMode2D

beginMode3D :: Camera3D -> IO ()
beginMode3D camera = withFreeable camera c'beginMode3D

endMode3D :: IO ()
endMode3D = c'endMode3D

beginTextureMode :: RenderTexture -> IO ()
beginTextureMode renderTexture = withFreeable renderTexture c'beginTextureMode

endTextureMode :: IO ()
endTextureMode = c'endTextureMode

beginShaderMode :: Shader -> IO ()
beginShaderMode shader = withFreeable shader c'beginShaderMode

endShaderMode :: IO ()
endShaderMode = c'endShaderMode

beginBlendMode :: BlendMode -> IO ()
beginBlendMode = c'beginBlendMode . fromIntegral . fromEnum

endBlendMode :: IO ()
endBlendMode = c'endBlendMode

beginScissorMode :: Int -> Int -> Int -> Int -> IO ()
beginScissorMode x y width height = c'beginScissorMode (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

endScissorMode :: IO ()
endScissorMode = c'endScissorMode

beginVrStereoMode :: VrStereoConfig -> IO ()
beginVrStereoMode config = withFreeable config c'beginVrStereoMode

endVrStereoMode :: IO ()
endVrStereoMode = c'endVrStereoMode

loadVrStereoConfig :: VrDeviceInfo -> IO VrStereoConfig
loadVrStereoConfig deviceInfo = withFreeable deviceInfo c'loadVrStereoConfig >>= pop

loadShader :: Maybe String -> Maybe String -> WindowResources -> IO Shader
loadShader vsFileName fsFileName wr = do
  shader <- withMaybeCString vsFileName (withMaybeCString fsFileName . c'loadShader) >>= pop
  addShaderId (shader'id shader) wr
  return shader

loadShaderFromMemory :: Maybe String -> Maybe String -> WindowResources -> IO Shader
loadShaderFromMemory vsCode fsCode wr = do
  shader <- withMaybeCString vsCode (withMaybeCString fsCode . c'loadShaderFromMemory) >>= pop
  addShaderId (shader'id shader) wr
  return shader

isShaderReady :: Shader -> IO Bool
isShaderReady shader = toBool <$> withFreeable shader c'isShaderReady

getShaderLocation :: Shader -> String -> WindowResources -> IO Int
getShaderLocation shader uniformName wr = do
  let sId = shader'id shader
  let sLocs = shaderLocations wr
  locs <- readIORef sLocs
  case Map.lookup sId locs of
    Nothing -> do
      idx <- locIdx
      let newMap = Map.fromList [(uniformName, idx)]
      modifyIORef' sLocs (Map.insert sId newMap)
      return idx
    Just m -> case Map.lookup uniformName m of
      Nothing -> do
        idx <- locIdx
        let newMap = Map.insert uniformName idx m
        modifyIORef' sLocs (Map.insert sId newMap)
        return idx
      Just val -> return val
  where
    locIdx = fromIntegral <$> withFreeable shader (withCString uniformName . c'getShaderLocation)

getShaderLocationAttrib :: Shader -> String -> IO Int
getShaderLocationAttrib shader attribName = fromIntegral <$> withFreeable shader (withCString attribName . c'getShaderLocationAttrib)

setShaderValue :: Shader -> String -> ShaderUniformData -> WindowResources -> IO ()
setShaderValue shader uniformName value wr = do
  idx <- getShaderLocation shader uniformName wr
  nativeSetShaderValue shader idx value

setShaderValueV :: Shader -> String -> ShaderUniformDataV -> WindowResources -> IO ()
setShaderValueV shader uniformName values wr = do
  idx <- getShaderLocation shader uniformName wr
  nativeSetShaderValueV shader idx values

nativeSetShaderValue :: Shader -> Int -> ShaderUniformData -> IO ()
nativeSetShaderValue shader locIndex value = do
  (uniformType, fptr) <- unpackShaderUniformData value
  withFreeable shader (\s -> withForeignPtr fptr (\ptr -> c'setShaderValue s (fromIntegral locIndex) ptr (fromIntegral $ fromEnum uniformType)))
  finalizeForeignPtr fptr

nativeSetShaderValueV :: Shader -> Int -> ShaderUniformDataV -> IO ()
nativeSetShaderValueV shader locIndex values = do
  (uniformType, fptr, l) <- unpackShaderUniformDataV values
  withFreeable shader (\s -> withForeignPtr fptr (\ptr -> c'setShaderValueV s (fromIntegral locIndex) ptr (fromIntegral $ fromEnum uniformType) (fromIntegral l)))
  finalizeForeignPtr fptr

setShaderValueMatrix :: Shader -> Int -> Matrix -> IO ()
setShaderValueMatrix shader locIndex mat = withFreeable shader (\s -> withFreeable mat (c'setShaderValueMatrix s (fromIntegral locIndex)))

setShaderValueTexture :: Shader -> Int -> Texture -> IO ()
setShaderValueTexture shader locIndex tex = withFreeable shader (\s -> withFreeable tex (c'setShaderValueTexture s (fromIntegral locIndex)))

-- | Unloads a shader from GPU memory (VRAM). Shaders are automatically unloaded
-- when `closeWindow` is called, so manually unloading shaders is not required.
-- In larger projects, you may want to manually unload shaders to avoid having
-- them in VRAM for too long.
unloadShader :: Shader -> WindowResources -> IO ()
unloadShader shader = unloadSingleShader (shader'id shader)

getScreenToWorldRay :: Vector2 -> Camera3D -> IO Ray
getScreenToWorldRay position camera = withFreeable position (withFreeable camera . c'getScreenToWorldRay) >>= pop

getScreenToWorldRayEx :: Vector2 -> Camera3D -> Float -> Float -> Ray
getScreenToWorldRayEx position camera width height = unsafePerformIO $ withFreeable position (\p -> withFreeable camera (\c -> c'getScreenToWorldRayEx p c (realToFrac width) (realToFrac height))) >>= pop

getCameraMatrix :: Camera3D -> Matrix
getCameraMatrix camera = unsafePerformIO $ withFreeable camera c'getCameraMatrix >>= pop

getCameraMatrix2D :: Camera2D -> Matrix
getCameraMatrix2D camera = unsafePerformIO $ withFreeable camera c'getCameraMatrix2D >>= pop

getWorldToScreen :: Vector3 -> Camera3D -> IO Vector2
getWorldToScreen position camera = withFreeable position (withFreeable camera . c'getWorldToScreen) >>= pop

getWorldToScreenEx :: Vector3 -> Camera3D -> Int -> Int -> Vector2
getWorldToScreenEx position camera width height = unsafePerformIO $ withFreeable position (\p -> withFreeable camera (\c -> c'getWorldToScreenEx p c (fromIntegral width) (fromIntegral height))) >>= pop

getWorldToScreen2D :: Vector2 -> Camera2D -> Vector2
getWorldToScreen2D position camera = unsafePerformIO $ withFreeable position (withFreeable camera . c'getWorldToScreen2D) >>= pop

getScreenToWorld2D :: Vector2 -> Camera2D -> Vector2
getScreenToWorld2D position camera = unsafePerformIO $ withFreeable position (withFreeable camera . c'getScreenToWorld2D) >>= pop

setTargetFPS :: Int -> IO ()
setTargetFPS fps = c'setTargetFPS $ fromIntegral fps

getFPS :: IO Int
getFPS = fromIntegral <$> c'getFPS

getFrameTime :: IO Float
getFrameTime = realToFrac <$> c'getFrameTime

getTime :: IO Double
getTime = realToFrac <$> c'getTime

setRandomSeed :: Integer -> IO ()
setRandomSeed seed = c'setRandomSeed $ fromIntegral seed

getRandomValue :: Int -> Int -> IO Int
getRandomValue minVal maxVal = fromIntegral <$> c'getRandomValue (fromIntegral minVal) (fromIntegral maxVal)

loadRandomSequence :: Integer -> Int -> Int -> IO [Int]
loadRandomSequence count rMin rMax = map fromIntegral <$> (popCArray (fromIntegral count) =<< c'loadRandomSequence (fromIntegral count) (fromIntegral rMin) (fromIntegral rMax))

takeScreenshot :: String -> IO ()
takeScreenshot fileName = withCString fileName c'takeScreenshot

setConfigFlags :: [ConfigFlag] -> IO ()
setConfigFlags flags = c'setConfigFlags $ fromIntegral $ configsToBitflag flags

traceLog :: TraceLogLevel -> String -> IO ()
traceLog logLevel text = withCString text $ c'traceLog $ fromIntegral $ fromEnum logLevel

setTraceLogLevel :: TraceLogLevel -> IO ()
setTraceLogLevel = c'setTraceLogLevel . fromIntegral . fromEnum

openURL :: String -> IO ()
openURL url = withCString url c'openURL

setLoadFileDataCallback :: LoadFileDataCallback -> WindowResources -> IO C'LoadFileDataCallback
setLoadFileDataCallback callback window = do
  c <- createLoadFileDataCallback callback
  addFunPtr (castFunPtr c) window
  c'setLoadFileDataCallback c
  return c

setSaveFileDataCallback :: (Storable a) => SaveFileDataCallback a -> WindowResources -> IO C'SaveFileDataCallback
setSaveFileDataCallback callback window = do
  c <- createSaveFileDataCallback callback
  addFunPtr (castFunPtr c) window
  c'setSaveFileDataCallback c
  return c

setLoadFileTextCallback :: LoadFileTextCallback -> WindowResources -> IO C'LoadFileTextCallback
setLoadFileTextCallback callback window = do
  c <- createLoadFileTextCallback callback
  addFunPtr (castFunPtr c) window
  c'setLoadFileTextCallback c
  return c

setSaveFileTextCallback :: SaveFileTextCallback -> WindowResources -> IO C'SaveFileTextCallback
setSaveFileTextCallback callback window = do
  c <- createSaveFileTextCallback callback
  addFunPtr (castFunPtr c) window
  c'setSaveFileTextCallback c
  return c

loadFileData :: String -> IO [Integer]
loadFileData fileName =
  withFreeable
    0
    ( \size -> do
        withCString
          fileName
          ( \path -> do
              ptr <- c'loadFileData path size
              arrSize <- fromIntegral <$> peek size
              map fromIntegral <$> popCArray arrSize ptr
          )
    )

saveFileData :: (Storable a) => String -> Ptr a -> Integer -> IO Bool
saveFileData fileName contents bytesToWrite =
  toBool <$> withCString fileName (\s -> c'saveFileData s (castPtr contents) (fromIntegral bytesToWrite))

exportDataAsCode :: [Integer] -> Integer -> String -> IO Bool
exportDataAsCode contents size fileName =
  toBool <$> withFreeableArray (map fromInteger contents) (\c -> withCString fileName (c'exportDataAsCode c (fromIntegral size)))

loadFileText :: String -> IO String
loadFileText fileName = withCString fileName c'loadFileText >>= popCString

saveFileText :: String -> String -> IO Bool
saveFileText fileName text = toBool <$> withCString fileName (withCString text . c'saveFileText)

fileExists :: String -> IO Bool
fileExists fileName = toBool <$> withCString fileName c'fileExists

directoryExists :: String -> IO Bool
directoryExists dirPath = toBool <$> withCString dirPath c'directoryExists

isFileExtension :: String -> String -> IO Bool
isFileExtension fileName ext = toBool <$> withCString fileName (withCString ext . c'isFileExtension)

getFileLength :: String -> IO Bool
getFileLength fileName = toBool <$> withCString fileName c'getFileLength

getFileExtension :: String -> IO String
getFileExtension fileName = withCString fileName c'getFileExtension >>= peekCString

getFileName :: String -> IO String
getFileName filePath = withCString filePath c'getFileName >>= peekCString

getFileNameWithoutExt :: String -> IO String
getFileNameWithoutExt fileName = withCString fileName c'getFileNameWithoutExt >>= peekCString

getDirectoryPath :: String -> IO String
getDirectoryPath filePath = withCString filePath c'getDirectoryPath >>= peekCString

getPrevDirectoryPath :: String -> IO String
getPrevDirectoryPath dirPath = withCString dirPath c'getPrevDirectoryPath >>= peekCString

getWorkingDirectory :: IO String
getWorkingDirectory = c'getWorkingDirectory >>= peekCString

getApplicationDirectory :: IO String
getApplicationDirectory = c'getApplicationDirectory >>= peekCString

changeDirectory :: String -> IO Bool
changeDirectory dir = toBool <$> withCString dir c'changeDirectory

isPathFile :: String -> IO Bool
isPathFile path = toBool <$> withCString path c'isPathFile

isFileNameValid :: String -> IO Bool
isFileNameValid path = toBool <$> withCString path c'isFileNameValid

loadDirectoryFiles :: String -> IO FilePathList
loadDirectoryFiles dirPath = withCString dirPath c'loadDirectoryFiles >>= pop

loadDirectoryFilesEx :: String -> String -> Bool -> IO FilePathList
loadDirectoryFilesEx basePath filterStr scanSubdirs =
  withCString basePath (\b -> withCString filterStr (\f -> c'loadDirectoryFilesEx b f (fromBool scanSubdirs))) >>= pop

isFileDropped :: IO Bool
isFileDropped = toBool <$> c'isFileDropped

loadDroppedFiles :: IO FilePathList
loadDroppedFiles = do
  ptr <- c'loadDroppedFiles
  val <- peek ptr
  c'unloadDroppedFiles ptr
  return val

getFileModTime :: String -> IO Integer
getFileModTime fileName = fromIntegral <$> withCString fileName c'getFileModTime

compressData :: [Integer] -> IO [Integer]
compressData contents = do
  withFreeableArrayLen
    (map fromIntegral contents)
    ( \size c -> do
        withFreeable
          0
          ( \ptr -> do
              compressed <- c'compressData c (fromIntegral $ size * sizeOf (0 :: CUChar)) ptr
              compressedSize <- fromIntegral <$> peek ptr
              arr <- peekArray compressedSize compressed
              return $ map fromIntegral arr
          )
    )

decompressData :: [Integer] -> IO [Integer]
decompressData compressedData = do
  withFreeableArrayLen
    (map fromIntegral compressedData)
    ( \size c -> do
        withFreeable
          0
          ( \ptr -> do
              decompressed <- c'decompressData c (fromIntegral $ size * sizeOf (0 :: CUChar)) ptr
              decompressedSize <- fromIntegral <$> peek ptr
              arr <- peekArray decompressedSize decompressed
              return $ map fromIntegral arr
          )
    )

encodeDataBase64 :: [Integer] -> IO [Integer]
encodeDataBase64 contents = do
  withFreeableArrayLen
    (map fromIntegral contents)
    ( \size c -> do
        withFreeable
          0
          ( \ptr -> do
              encoded <- c'encodeDataBase64 c (fromIntegral $ size * sizeOf (0 :: CUChar)) ptr
              encodedSize <- fromIntegral <$> peek ptr
              arr <- peekArray encodedSize encoded
              return $ map fromIntegral arr
          )
    )

decodeDataBase64 :: [Integer] -> IO [Integer]
decodeDataBase64 encodedData = do
  withFreeableArray
    (map fromIntegral encodedData)
    ( \c -> do
        withFreeable
          0
          ( \ptr -> do
              decoded <- c'decodeDataBase64 c ptr
              decodedSize <- fromIntegral <$> peek ptr
              arr <- peekArray decodedSize decoded
              return $ map fromIntegral arr
          )
    )

loadAutomationEventList :: String -> IO AutomationEventList
loadAutomationEventList fileName = withCString fileName c'loadAutomationEventList >>= pop

newAutomationEventList :: IO AutomationEventList
newAutomationEventList = c'loadAutomationEventList nullPtr >>= pop

exportAutomationEventList :: AutomationEventList -> String -> IO Bool
exportAutomationEventList list fileName = toBool <$> withFreeable list (withCString fileName . c'exportAutomationEventList)

setAutomationEventList :: AutomationEventList -> WindowResources -> IO AutomationEventListRef
setAutomationEventList list wr = do
  ptr <- malloc
  poke ptr list
  c'setAutomationEventList ptr
  addAutomationEventList (castPtr ptr) wr
  return ptr

setAutomationEventBaseFrame :: Int -> IO ()
setAutomationEventBaseFrame frame = c'setAutomationEventBaseFrame (fromIntegral frame)

startAutomationEventRecording :: IO ()
startAutomationEventRecording = c'startAutomationEventRecording

stopAutomationEventRecording :: IO ()
stopAutomationEventRecording = c'stopAutomationEventRecording

playAutomationEvent :: AutomationEvent -> IO ()
playAutomationEvent event = withFreeable event c'playAutomationEvent

peekAutomationEventList :: AutomationEventListRef -> IO AutomationEventList
peekAutomationEventList = peek

freeAutomationEventList :: AutomationEventListRef -> WindowResources -> IO ()
freeAutomationEventList list = unloadSingleAutomationEventList (castPtr list)

isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed key = toBool <$> c'isKeyPressed (fromIntegral $ fromEnum key)

isKeyPressedRepeat :: KeyboardKey -> IO Bool
isKeyPressedRepeat key = toBool <$> c'isKeyPressedRepeat (fromIntegral $ fromEnum key)

isKeyDown :: KeyboardKey -> IO Bool
isKeyDown key = toBool <$> c'isKeyDown (fromIntegral $ fromEnum key)

isKeyReleased :: KeyboardKey -> IO Bool
isKeyReleased key = toBool <$> c'isKeyReleased (fromIntegral $ fromEnum key)

isKeyUp :: KeyboardKey -> IO Bool
isKeyUp key = toBool <$> c'isKeyUp (fromIntegral $ fromEnum key)

setExitKey :: KeyboardKey -> IO ()
setExitKey = c'setExitKey . fromIntegral . fromEnum

getKeyPressed :: IO KeyboardKey
getKeyPressed = toEnum . fromIntegral <$> c'getKeyPressed

getCharPressed :: IO Int
getCharPressed = fromIntegral <$> c'getCharPressed

isGamepadAvailable :: Int -> IO Bool
isGamepadAvailable gamepad = toBool <$> c'isGamepadAvailable (fromIntegral gamepad)

getGamepadName :: Int -> IO String
getGamepadName gamepad = c'getGamepadName (fromIntegral gamepad) >>= peekCString

isGamepadButtonPressed :: Int -> GamepadButton -> IO Bool
isGamepadButtonPressed gamepad button = toBool <$> c'isGamepadButtonPressed (fromIntegral gamepad) (fromIntegral $ fromEnum button)

isGamepadButtonDown :: Int -> GamepadButton -> IO Bool
isGamepadButtonDown gamepad button = toBool <$> c'isGamepadButtonDown (fromIntegral gamepad) (fromIntegral $ fromEnum button)

isGamepadButtonReleased :: Int -> GamepadButton -> IO Bool
isGamepadButtonReleased gamepad button = toBool <$> c'isGamepadButtonReleased (fromIntegral gamepad) (fromIntegral $ fromEnum button)

isGamepadButtonUp :: Int -> GamepadButton -> IO Bool
isGamepadButtonUp gamepad button = toBool <$> c'isGamepadButtonUp (fromIntegral gamepad) (fromIntegral $ fromEnum button)

getGamepadButtonPressed :: IO GamepadButton
getGamepadButtonPressed = toEnum . fromIntegral <$> c'getGamepadButtonPressed

getGamepadAxisCount :: Int -> IO Int
getGamepadAxisCount gamepad = fromIntegral <$> c'getGamepadAxisCount (fromIntegral gamepad)

getGamepadAxisMovement :: Int -> GamepadAxis -> IO Float
getGamepadAxisMovement gamepad axis = realToFrac <$> c'getGamepadAxisMovement (fromIntegral gamepad) (fromIntegral $ fromEnum axis)

setGamepadMappings :: String -> IO Int
setGamepadMappings mappings = fromIntegral <$> withCString mappings c'setGamepadMappings

setGamepadVibration :: Int -> Float -> Float -> IO ()
setGamepadVibration gamepad leftMotor rightMotor = c'setGamepadVibration (fromIntegral gamepad) (realToFrac leftMotor) (realToFrac rightMotor)

isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed button = toBool <$> c'isMouseButtonPressed (fromIntegral $ fromEnum button)

isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown button = toBool <$> c'isMouseButtonDown (fromIntegral $ fromEnum button)

isMouseButtonReleased :: MouseButton -> IO Bool
isMouseButtonReleased button = toBool <$> c'isMouseButtonReleased (fromIntegral $ fromEnum button)

isMouseButtonUp :: MouseButton -> IO Bool
isMouseButtonUp button = toBool <$> c'isMouseButtonUp (fromIntegral $ fromEnum button)

getMouseX :: IO Int
getMouseX = fromIntegral <$> c'getMouseX

getMouseY :: IO Int
getMouseY = fromIntegral <$> c'getMouseY

getMousePosition :: IO Vector2
getMousePosition = c'getMousePosition >>= pop

getMouseDelta :: IO Vector2
getMouseDelta = c'getMouseDelta >>= pop

setMousePosition :: Int -> Int -> IO ()
setMousePosition x y = c'setMousePosition (fromIntegral x) (fromIntegral y)

setMouseOffset :: Int -> Int -> IO ()
setMouseOffset x y = c'setMouseOffset (fromIntegral x) (fromIntegral y)

setMouseScale :: Float -> Float -> IO ()
setMouseScale x y = c'setMouseScale (realToFrac x) (realToFrac y)

getMouseWheelMove :: IO Float
getMouseWheelMove = realToFrac <$> c'getMouseWheelMove

getMouseWheelMoveV :: IO Vector2
getMouseWheelMoveV = c'getMouseWheelMoveV >>= pop

setMouseCursor :: MouseCursor -> IO ()
setMouseCursor cursor = c'setMouseCursor . fromIntegral $ fromEnum cursor

getTouchX :: IO Int
getTouchX = fromIntegral <$> c'getTouchX

getTouchY :: IO Int
getTouchY = fromIntegral <$> c'getTouchY

getTouchPosition :: Int -> IO Vector2
getTouchPosition index = c'getTouchPosition (fromIntegral index) >>= pop

getTouchPointId :: Int -> IO Int
getTouchPointId index = fromIntegral <$> c'getTouchPointId (fromIntegral index)

getTouchPointCount :: IO Int
getTouchPointCount = fromIntegral <$> c'getTouchPointCount

setGesturesEnabled :: [Gesture] -> IO ()
setGesturesEnabled flags = c'setGesturesEnabled (fromIntegral $ configsToBitflag flags)

isGestureDetected :: Gesture -> IO Bool
isGestureDetected gesture = toBool <$> c'isGestureDetected (fromIntegral $ fromEnum gesture)

getGestureDetected :: IO Gesture
getGestureDetected = toEnum . fromIntegral <$> c'getGestureDetected

getGestureHoldDuration :: IO Float
getGestureHoldDuration = realToFrac <$> c'getGestureHoldDuration

getGestureDragVector :: IO Vector2
getGestureDragVector = c'getGestureDragVector >>= pop

getGestureDragAngle :: IO Float
getGestureDragAngle = realToFrac <$> c'getGestureDragAngle

getGesturePinchVector :: IO Vector2
getGesturePinchVector = c'getGesturePinchVector >>= pop

getGesturePinchAngle :: IO Float
getGesturePinchAngle = realToFrac <$> c'getGesturePinchAngle

foreign import ccall unsafe "wrapper"
  mk'loadFileDataCallback ::
    (CString -> Ptr CUInt -> IO (Ptr CUChar)) -> IO C'LoadFileDataCallback

-- foreign import ccall unsafe "dynamic"
--   mK'loadFileDataCallback ::
--     C'LoadFileDataCallback -> (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall unsafe "wrapper"
  mk'saveFileDataCallback ::
    (CString -> Ptr () -> CUInt -> IO CInt) -> IO C'SaveFileDataCallback

-- foreign import ccall unsafe "dynamic"
--   mK'saveFileDataCallback ::
--     C'SaveFileDataCallback -> (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall unsafe "wrapper"
  mk'loadFileTextCallback ::
    (CString -> IO CString) -> IO C'LoadFileTextCallback

-- foreign import ccall unsafe "dynamic"
--   mK'loadFileTextCallback ::
--     C'LoadFileTextCallback -> (CString -> IO CString)

foreign import ccall unsafe "wrapper"
  mk'saveFileTextCallback ::
    (CString -> CString -> IO CInt) -> IO C'SaveFileTextCallback

-- foreign import ccall unsafe "dynamic"
--   mK'saveFileTextCallback ::
--     C'SaveFileTextCallback -> (CString -> CString -> IO CInt)

createLoadFileDataCallback :: LoadFileDataCallback -> IO C'LoadFileDataCallback
createLoadFileDataCallback callback =
  mk'loadFileDataCallback
    ( \fileName dataSize ->
        do
          fn <- peekCString fileName
          arr <- callback fn
          poke dataSize (fromIntegral (length arr) :: CUInt)
          newArray (map fromIntegral arr :: [CUChar])
    )

createSaveFileDataCallback :: (Storable a) => SaveFileDataCallback a -> IO C'SaveFileDataCallback
createSaveFileDataCallback callback =
  mk'saveFileDataCallback
    ( \fileName contents bytesToWrite ->
        do
          fn <- peekCString fileName
          fromBool <$> callback fn (castPtr contents) (fromIntegral bytesToWrite)
    )

createLoadFileTextCallback :: LoadFileTextCallback -> IO C'LoadFileTextCallback
createLoadFileTextCallback callback =
  mk'loadFileTextCallback
    (\fileName -> peekCString fileName >>= callback >>= newCString)

createSaveFileTextCallback :: SaveFileTextCallback -> IO C'SaveFileTextCallback
createSaveFileTextCallback callback =
  mk'saveFileTextCallback
    ( \fileName content -> do
        fn <- peekCString fileName
        c <- peekCString content
        fromBool <$> callback fn c
    )
