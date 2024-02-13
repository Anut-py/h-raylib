{-# OPTIONS -Wall #-}

module Raylib.Core where

import Data.IORef (modifyIORef', readIORef)
import qualified Data.Map as Map
import Foreign
  ( Ptr,
    Storable (peek, poke, sizeOf),
    castPtr,
    fromBool,
    malloc,
    peekArray,
    toBool,
  )
import Foreign.C
  ( CUChar,
    peekCString,
    withCString,
  )
import Foreign.Ptr (nullPtr)
import Raylib.Internal.Foreign (c'free, configsToBitflag, pop, popCArray, popCString, withFreeable, withFreeableArray, withFreeableArrayLen, withMaybeCString)
import Raylib.Internal (WindowResources, addAutomationEventList, addShaderId, defaultWindowResources, shaderLocations, unloadAutomationEventLists, unloadFrameBuffers, unloadShaders, unloadSingleAutomationEventList, unloadSingleShader, unloadTextures, unloadVaoIds, unloadVboIds)
import Raylib.Internal.Native
  ( c'beginBlendMode,
    c'beginDrawing,
    c'beginMode2D,
    c'beginMode3D,
    c'beginScissorMode,
    c'beginShaderMode,
    c'beginTextureMode,
    c'beginVrStereoMode,
    c'changeDirectory,
    c'clearBackground,
    c'clearWindowState,
    c'closeWindow,
    c'compressData,
    c'decodeDataBase64,
    c'decompressData,
    c'directoryExists,
    c'disableCursor,
    c'disableEventWaiting,
    c'enableCursor,
    c'enableEventWaiting,
    c'encodeDataBase64,
    c'endBlendMode,
    c'endDrawing,
    c'endMode2D,
    c'endMode3D,
    c'endScissorMode,
    c'endShaderMode,
    c'endTextureMode,
    c'endVrStereoMode,
    c'exportAutomationEventList,
    c'exportDataAsCode,
    c'fileExists,
    c'getApplicationDirectory,
    c'getCameraMatrix,
    c'getCameraMatrix2D,
    c'getCharPressed,
    c'getClipboardText,
    c'getCurrentMonitor,
    c'getDirectoryPath,
    c'getFPS,
    c'getFileExtension,
    c'getFileLength,
    c'getFileModTime,
    c'getFileName,
    c'getFileNameWithoutExt,
    c'getFrameTime,
    c'getGamepadAxisCount,
    c'getGamepadAxisMovement,
    c'getGamepadButtonPressed,
    c'getGamepadName,
    c'getGestureDetected,
    c'getGestureDragAngle,
    c'getGestureDragVector,
    c'getGestureHoldDuration,
    c'getGesturePinchAngle,
    c'getGesturePinchVector,
    c'getKeyPressed,
    c'getMonitorCount,
    c'getMonitorHeight,
    c'getMonitorName,
    c'getMonitorPhysicalHeight,
    c'getMonitorPhysicalWidth,
    c'getMonitorPosition,
    c'getMonitorRefreshRate,
    c'getMonitorWidth,
    c'getMouseDelta,
    c'getMousePosition,
    c'getMouseRay,
    c'getMouseWheelMove,
    c'getMouseWheelMoveV,
    c'getMouseX,
    c'getMouseY,
    c'getPrevDirectoryPath,
    c'getRandomValue,
    c'getRenderHeight,
    c'getRenderWidth,
    c'getScreenHeight,
    c'getScreenToWorld2D,
    c'getScreenWidth,
    c'getShaderLocation,
    c'getShaderLocationAttrib,
    c'getTime,
    c'getTouchPointCount,
    c'getTouchPointId,
    c'getTouchPosition,
    c'getTouchX,
    c'getTouchY,
    c'getViewRay,
    c'getWindowHandle,
    c'getWindowPosition,
    c'getWindowScaleDPI,
    c'getWorkingDirectory,
    c'getWorldToScreen,
    c'getWorldToScreen2D,
    c'getWorldToScreenEx,
    c'hideCursor,
    c'initWindow,
    c'isCursorHidden,
    c'isCursorOnScreen,
    c'isFileDropped,
    c'isFileExtension,
    c'isGamepadAvailable,
    c'isGamepadButtonDown,
    c'isGamepadButtonPressed,
    c'isGamepadButtonReleased,
    c'isGamepadButtonUp,
    c'isGestureDetected,
    c'isKeyDown,
    c'isKeyPressed,
    c'isKeyPressedRepeat,
    c'isKeyReleased,
    c'isKeyUp,
    c'isMouseButtonDown,
    c'isMouseButtonPressed,
    c'isMouseButtonReleased,
    c'isMouseButtonUp,
    c'isPathFile,
    c'isShaderReady,
    c'isWindowFocused,
    c'isWindowFullscreen,
    c'isWindowHidden,
    c'isWindowMaximized,
    c'isWindowMinimized,
    c'isWindowReady,
    c'isWindowResized,
    c'isWindowState,
    c'loadAutomationEventList,
    c'loadDirectoryFiles,
    c'loadDirectoryFilesEx,
    c'loadDroppedFiles,
    c'loadFileData,
    c'loadFileText,
    c'loadRandomSequence,
    c'loadShader,
    c'loadShaderFromMemory,
    c'loadVrStereoConfig,
    c'maximizeWindow,
    c'minimizeWindow,
    c'openURL,
    c'playAutomationEvent,
    c'pollInputEvents,
    c'restoreWindow,
    c'saveFileData,
    c'saveFileText,
    c'setAutomationEventBaseFrame,
    c'setAutomationEventList,
    c'setClipboardText,
    c'setConfigFlags,
    c'setExitKey,
    c'setGamepadMappings,
    c'setGesturesEnabled,
    c'setLoadFileDataCallback,
    c'setLoadFileTextCallback,
    c'setMouseCursor,
    c'setMouseOffset,
    c'setMousePosition,
    c'setMouseScale,
    c'setRandomSeed,
    c'setSaveFileDataCallback,
    c'setSaveFileTextCallback,
    c'setShaderValue,
    c'setShaderValueMatrix,
    c'setShaderValueTexture,
    c'setShaderValueV,
    c'setTargetFPS,
    c'setTraceLogLevel,
    c'setWindowFocused,
    c'setWindowIcon,
    c'setWindowIcons,
    c'setWindowMaxSize,
    c'setWindowMinSize,
    c'setWindowMonitor,
    c'setWindowOpacity,
    c'setWindowPosition,
    c'setWindowSize,
    c'setWindowState,
    c'setWindowTitle,
    c'showCursor,
    c'startAutomationEventRecording,
    c'stopAutomationEventRecording,
    c'swapScreenBuffer,
    c'takeScreenshot,
    c'toggleBorderlessWindowed,
    c'toggleFullscreen,
    c'traceLog,
    c'waitTime,
    c'windowShouldClose,
    createLoadFileDataCallback,
    createLoadFileTextCallback,
    createSaveFileDataCallback,
    createSaveFileTextCallback,
  )
import Raylib.Types
  ( AutomationEvent,
    AutomationEventList,
    AutomationEventListRef,
    BlendMode,
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
    unpackShaderUniformDataV, C'LoadFileDataCallback, C'SaveFileDataCallback, C'LoadFileTextCallback, C'SaveFileTextCallback,
  )

initWindow :: Int -> Int -> String -> IO WindowResources
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
  (uniformType, ptr) <- unpackShaderUniformData value
  withFreeable shader (\s -> c'setShaderValue s (fromIntegral locIndex) ptr (fromIntegral $ fromEnum uniformType))
  c'free $ castPtr ptr

nativeSetShaderValueV :: Shader -> Int -> ShaderUniformDataV -> IO ()
nativeSetShaderValueV shader locIndex values = do
  (uniformType, ptr, l) <- unpackShaderUniformDataV values
  withFreeable shader (\s -> c'setShaderValueV s (fromIntegral locIndex) ptr (fromIntegral $ fromEnum uniformType) (fromIntegral l))
  c'free $ castPtr ptr

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

getMouseRay :: Vector2 -> Camera3D -> IO Ray
getMouseRay mousePosition camera = withFreeable mousePosition (withFreeable camera . c'getMouseRay) >>= pop

getViewRay :: Vector2 -> Camera3D -> Float -> Float -> IO Ray
getViewRay mousePosition camera width height = withFreeable mousePosition (\p -> withFreeable camera (\c -> c'getViewRay p c (realToFrac width) (realToFrac height))) >>= pop

getCameraMatrix :: Camera3D -> IO Matrix
getCameraMatrix camera = withFreeable camera c'getCameraMatrix >>= pop

getCameraMatrix2D :: Camera2D -> IO Matrix
getCameraMatrix2D camera = withFreeable camera c'getCameraMatrix2D >>= pop

getWorldToScreen :: Vector3 -> Camera3D -> IO Vector2
getWorldToScreen position camera = withFreeable position (withFreeable camera . c'getWorldToScreen) >>= pop

getScreenToWorld2D :: Vector2 -> Camera2D -> IO Vector2
getScreenToWorld2D position camera = withFreeable position (withFreeable camera . c'getScreenToWorld2D) >>= pop

getWorldToScreenEx :: Vector3 -> Camera3D -> Int -> Int -> IO Vector2
getWorldToScreenEx position camera width height = withFreeable position (\p -> withFreeable camera (\c -> c'getWorldToScreenEx p c (fromIntegral width) (fromIntegral height))) >>= pop

getWorldToScreen2D :: Vector2 -> Camera2D -> IO Vector2
getWorldToScreen2D position camera = withFreeable position (withFreeable camera . c'getWorldToScreen2D) >>= pop

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

setLoadFileDataCallback :: LoadFileDataCallback -> IO C'LoadFileDataCallback
setLoadFileDataCallback callback = do
  c <- createLoadFileDataCallback callback
  c'setLoadFileDataCallback c
  return c

setSaveFileDataCallback :: (Storable a) => SaveFileDataCallback a -> IO C'SaveFileDataCallback
setSaveFileDataCallback callback = do
  c <- createSaveFileDataCallback callback
  c'setSaveFileDataCallback c
  return c

setLoadFileTextCallback :: LoadFileTextCallback -> IO C'LoadFileTextCallback
setLoadFileTextCallback callback = do
  c <- createLoadFileTextCallback callback
  c'setLoadFileTextCallback c
  return c

setSaveFileTextCallback :: SaveFileTextCallback -> IO C'SaveFileTextCallback
setSaveFileTextCallback callback = do
  c <- createSaveFileTextCallback callback
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

loadDirectoryFiles :: String -> IO FilePathList
loadDirectoryFiles dirPath = withCString dirPath c'loadDirectoryFiles >>= pop

loadDirectoryFilesEx :: String -> String -> Bool -> IO FilePathList
loadDirectoryFilesEx basePath filterStr scanSubdirs =
  withCString basePath (\b -> withCString filterStr (\f -> c'loadDirectoryFilesEx b f (fromBool scanSubdirs))) >>= pop

isFileDropped :: IO Bool
isFileDropped = toBool <$> c'isFileDropped

loadDroppedFiles :: IO FilePathList
loadDroppedFiles = c'loadDroppedFiles >>= pop

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