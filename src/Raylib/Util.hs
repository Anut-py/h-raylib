{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Utility functions that may be useful for an h-raylib application
module Raylib.Util
  ( -- * Bracket functions
    withWindow,
    drawing,
    mode2D,
    mode3D,
    textureMode,
    shaderMode,
    blendMode,
    scissorMode,
    vrStereoMode,

    -- * Game loop functions
    raylibApplication,
    whileWindowOpen,
    whileWindowOpen_,
    whileWindowOpen0,

    -- * Resource management
    WindowResources,
    Closeable (close),
    managed,

    -- * Miscellaneous
    cameraDirectionRay,
    setMaterialShader,
    inGHCi,
    inWeb,
    Freeable (..),
  )
where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket, bracket_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Foreign (Storable (peek, poke), advancePtr)
import Language.Haskell.TH.Syntax (Name (Name), OccName (OccName))
import Raylib.Core (beginBlendMode, beginDrawing, beginMode2D, beginMode3D, beginScissorMode, beginShaderMode, beginTextureMode, beginVrStereoMode, closeWindow, endBlendMode, endDrawing, endMode2D, endMode3D, endScissorMode, endShaderMode, endTextureMode, endVrStereoMode, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Internal (Closeable (..), WindowResources, managed)
import Raylib.Internal.Foreign (Freeable (..), PLike, TLike (peekTLike, withTLike))
import Raylib.Types
  ( BlendMode,
    Camera2D,
    Camera3D (camera3D'position, camera3D'target),
    Model,
    Ray (Ray),
    RenderTexture,
    Shader,
    VrStereoConfig,
    p'material'shader,
    p'model'materials,
  )
import Raylib.Util.Math (Vector (vectorNormalize, (|-|)))

#ifdef WEB_FFI

import Foreign (Ptr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Language.Haskell.TH (Body (NormalB), Callconv (CCall), Clause (Clause), Dec (ForeignD, FunD, SigD), DecsQ, Exp (AppE, VarE), Foreign (ExportF), Name, Pat (VarP), Q, Type (AppT, ArrowT, ConT, TupleT), mkName, ppr, reifyType)

#else

import Language.Haskell.TH (DecsQ, Type (AppT, ConT, ArrowT, TupleT), Q, reifyType, mkName, ppr, Dec (SigD, FunD), Clause (Clause), Body (NormalB), Exp (VarE, AppE))

#endif

-- | NOTE: Only for native targets. If your program is intended to
--         run on the web, use `raylibApplication` instead.
withWindow ::
  (MonadIO m, MonadMask m) =>
  -- | Window width
  Int ->
  -- | Window height
  Int ->
  -- | Window title
  String ->
  -- | Target FPS
  Int ->
  (WindowResources -> m b) ->
  m b
withWindow w h title fps = bracket (liftIO $ initWindow w h title <* setTargetFPS fps) (liftIO . closeWindow . Just)

drawing :: (MonadIO m, MonadMask m) => m b -> m b
drawing = bracket_ (liftIO beginDrawing) (liftIO endDrawing)

mode2D :: (MonadIO m, MonadMask m, PLike Camera2D camera2D) => camera2D -> m b -> m b
mode2D camera = bracket_ (liftIO (beginMode2D camera)) (liftIO endMode2D)

mode3D :: (MonadIO m, MonadMask m, PLike Camera3D camera3D) => camera3D -> m b -> m b
mode3D camera = bracket_ (liftIO (beginMode3D camera)) (liftIO endMode3D)

textureMode :: (MonadIO m, MonadMask m, PLike RenderTexture renderTexture) => renderTexture -> m b -> m b
textureMode rt = bracket_ (liftIO (beginTextureMode rt)) (liftIO endTextureMode)

shaderMode :: (MonadIO m, MonadMask m, PLike Shader shader) => shader -> m b -> m b
shaderMode shader = bracket_ (liftIO (beginShaderMode shader)) (liftIO endShaderMode)

blendMode :: (MonadIO m, MonadMask m) => BlendMode -> m b -> m b
blendMode bm = bracket_ (liftIO (beginBlendMode bm)) (liftIO endBlendMode)

scissorMode :: (MonadIO m, MonadMask m) => Int -> Int -> Int -> Int -> m b -> m b
scissorMode x y width height = bracket_ (liftIO (beginScissorMode x y width height)) (liftIO endScissorMode)

vrStereoMode :: (MonadIO m, MonadMask m, PLike VrStereoConfig vrStereoConfig) => vrStereoConfig -> m b -> m b
vrStereoMode config = bracket_ (liftIO (beginVrStereoMode config)) (liftIO endVrStereoMode)

-- | Gets the direction of a camera as a ray.
cameraDirectionRay :: Camera3D -> Ray
cameraDirectionRay camera = Ray (camera3D'position camera) (vectorNormalize $ camera3D'target camera |-| camera3D'position camera)

-- | Creates a raylib application using the given program functions. Supports
--   both native and web targets, so it is recommended for all programs. If
--   your program is intended only for native use, you may manually write a
--   @main@ function.
--
--   On a native (non-web) target, it simply creates a @main@ function that
--   uses the startup, mainLoop, shouldClose, and teardown functions. When
--   building with @platform-web@ enabled, it creates four @foreign export@
--   statements (@startup@, @mainLoop@, @shouldClose@, and @teardown@), which
--   will be called through the browser.
--
--   See @raygui-suite@ for an example of how to use it.
raylibApplication ::
  -- | The startup function, should be of type @IO AppState@
  Name ->
  -- | The mainLoop function, should be of type @AppState -> IO AppState@
  Name ->
  -- | The shouldClose function, should be of type @AppState -> IO Bool@
  Name ->
  -- | The teardown function, should be of type @AppState -> IO ()@
  Name ->
  DecsQ

#ifdef WEB_FFI

raylibApplication
  startup@(Name (OccName sun) _)
  mainLoop@(Name (OccName mln) _)
  shouldClose@(Name (OccName scn) _)
  teardown@(Name (OccName tdn) _) =
    do
      assertTypes startup mainLoop shouldClose teardown

      let _startupN = mkName ('_' : sun)
          _startupT = ConT ''IO `AppT` (ConT ''Ptr `AppT` TupleT 0)
          _startupS = SigD _startupN _startupT -- _startup :: IO (Ptr ())
          _startupF =
            -- _startup = startup >>= createStablePtr
            FunD
              _startupN
              [Clause [] (NormalB ((VarE '(>>=) `AppE` VarE startup) `AppE` VarE 'createStablePtr)) []]

      let _mainLoopN = mkName ('_' : mln)
          _mainLoopPtrN = mkName "ptr"
          _mainLoopT = (ArrowT `AppT` (ConT ''Ptr `AppT` TupleT 0)) `AppT` (ConT ''IO `AppT` (ConT ''Ptr `AppT` TupleT 0))
          _mainLoopS = SigD _mainLoopN _mainLoopT -- _mainLoop :: Ptr () -> IO (Ptr ())
          _mainLoopF =
            -- _mainLoop ptr = popStablePtr ptr >>= mainLoop >>= createStablePtr
            FunD
              _mainLoopN
              [ Clause
                  [VarP _mainLoopPtrN]
                  (NormalB ((VarE '(>>=) `AppE` ((VarE '(>>=) `AppE` (VarE 'popStablePtr `AppE` VarE _mainLoopPtrN)) `AppE` VarE mainLoop)) `AppE` VarE 'createStablePtr))
                  []
              ]

      let _shouldCloseN = mkName ('_' : scn)
          _shouldClosePtrN = mkName "ptr"
          _shouldCloseT = (ArrowT `AppT` (ConT ''Ptr `AppT` TupleT 0)) `AppT` (ConT ''IO `AppT` ConT ''Bool)
          _shouldCloseS = SigD _shouldCloseN _shouldCloseT -- _shouldClose :: Ptr () -> IO Bool
          _shouldCloseF =
            -- _shouldClose ptr = readStablePtr ptr >>= P.shouldClose
            FunD
              _shouldCloseN
              [ Clause
                  [VarP _shouldClosePtrN]
                  (NormalB ((VarE '(>>=) `AppE` (VarE 'readStablePtr `AppE` VarE _shouldClosePtrN)) `AppE` VarE shouldClose))
                  []
              ]

      let _teardownN = mkName ('_' : tdn)
          _teardownPtrN = mkName "ptr"
          _teardownT = (ArrowT `AppT` (ConT ''Ptr `AppT` TupleT 0)) `AppT` (ConT ''IO `AppT` TupleT 0)
          _teardownS = SigD _teardownN _teardownT -- _teardown :: Ptr () -> IO ()
          _teardownF =
            -- _teardown ptr = popStablePtr ptr >>= teardown
            FunD
              _teardownN
              [ Clause
                  [VarP _teardownPtrN]
                  (NormalB ((VarE '(>>=) `AppE` (VarE 'popStablePtr `AppE` VarE _teardownPtrN)) `AppE` VarE teardown))
                  []
              ]
      
      return
        [ _startupS,
          _startupF,
          _mainLoopS,
          _mainLoopF,
          _shouldCloseS,
          _shouldCloseF,
          _teardownS,
          _teardownF,
          ForeignD (ExportF CCall "startup" _startupN _startupT),
          ForeignD (ExportF CCall "mainLoop" _mainLoopN _mainLoopT),
          ForeignD (ExportF CCall "shouldClose" _shouldCloseN _shouldCloseT),
          ForeignD (ExportF CCall "teardown" _teardownN _teardownT)
        ]

createStablePtr :: a -> IO (Ptr ())
createStablePtr val = castStablePtrToPtr <$> newStablePtr val

readStablePtr :: Ptr () -> IO a
readStablePtr ptr = deRefStablePtr $ castPtrToStablePtr ptr

popStablePtr :: Ptr () -> IO a
popStablePtr ptr = do
  let sptr = castPtrToStablePtr ptr
  val <- deRefStablePtr sptr
  freeStablePtr sptr
  return val

#else

raylibApplication startup mainLoop shouldClose teardown = do
  assertTypes startup mainLoop shouldClose teardown

  return
    [
      -- main :: IO ()
      SigD main (ConT ''IO `AppT` TupleT 0),
      -- main = runRaylibProgram startup mainLoop shouldClose teardown
      FunD main [Clause [] (NormalB ((((VarE 'runRaylibProgram `AppE` VarE startup) `AppE` VarE mainLoop) `AppE` VarE shouldClose) `AppE` VarE teardown)) []]
    ]
  where main = mkName "main"

runRaylibProgram :: IO a -> (a -> IO a) -> (a -> IO Bool) -> (a -> IO ()) -> IO ()
runRaylibProgram startup mainLoop shouldClose teardown = do
  st <- startup
  helper st
  where helper s = shouldClose s >>= (\toClose -> if toClose then teardown s else mainLoop s >>= helper)

#endif

assertTypes :: Name -> Name -> Name -> Name -> Q ()
assertTypes startup mainLoop shouldClose teardown = do
  sut <- reifyType startup
  state <-
    case sut of
      m `AppT` st ->
        if m == ConT ''IO
          then return st
          else typeErr startup (ConT ''IO `AppT` ConT (mkName "AppState")) sut
      _ -> typeErr startup (ConT ''IO `AppT` ConT (mkName "AppState")) sut

  mlt <- reifyType mainLoop
  assertType mainLoop ((ArrowT `AppT` state) `AppT` (ConT ''IO `AppT` state)) mlt

  sct <- reifyType shouldClose
  assertType shouldClose ((ArrowT `AppT` state) `AppT` (ConT ''IO `AppT` ConT ''Bool)) sct

  tdt <- reifyType teardown
  assertType teardown ((ArrowT `AppT` state) `AppT` (ConT ''IO `AppT` TupleT 0)) tdt

assertType :: Name -> Type -> Type -> Q ()
assertType n expected actual = if expected == actual then return () else typeErr n expected actual

typeErr :: Name -> Type -> Type -> a
typeErr (Name (OccName n) _) expected actual =
  error (n ++ " was not the expected type\n\nexpected " ++ show (ppr expected) ++ "\n\ngot " ++ show (ppr actual) ++ "\n")

-- | Calls the game loop every frame as long as the window is open.
--   For larger projects, instead of using this function, consider
--   making a custom game loop for flexibility.
--
--   NOTE: Only for native targets. If your program is intended to
--         run on the web, use `raylibApplication` instead.
whileWindowOpen ::
  (MonadIO m) =>
  -- | The game loop. Its only argument should be the current application state, and it should return a new state.
  (a -> m a) ->
  -- | The initial application state.
  a ->
  -- | The application state after the last frame.
  m a
whileWindowOpen f state = do
  newState <- f state
  shouldClose <- liftIO windowShouldClose
  if shouldClose
    then return newState
    else whileWindowOpen f newState

-- | Same as `whileWindowOpen`, but discards the final state.
whileWindowOpen_ ::
  (MonadIO m) =>
  (a -> m a) ->
  a ->
  m ()
whileWindowOpen_ f state = void (whileWindowOpen f state)

-- | Same as `whileWindowOpen`, but without application state.
whileWindowOpen0 ::
  (MonadIO m) =>
  m () ->
  m ()
whileWindowOpen0 f = whileWindowOpen (const f) ()

-- | Sets the shader of a material at a specific index
--
--   WARNING: This will fail if the index provided is out of bounds
setMaterialShader ::
  (PLike Model model, PLike Shader shader) =>
  -- | The model to operate on
  model ->
  -- | The index of the material
  Int ->
  -- | The shader to use
  shader ->
  -- | The modified model
  IO model
setMaterialShader model matIdx shader =
  withTLike
    model
    ( \modelPtr ->
        withTLike
          shader
          ( \shaderPtr -> do
              mats <- peek (p'model'materials modelPtr)
              poke (p'material'shader (advancePtr mats matIdx)) =<< peek shaderPtr
              peekTLike modelPtr
          )
    )

-- | True if the program is running in GHCi
inGHCi :: Bool

#ifdef GHCI
inGHCi = True
#else
inGHCi = False
#endif

-- | True if the program is running in the web
inWeb :: Bool

#ifdef WEB_FFI
inWeb = True
#else
inWeb = False
#endif
