{-# OPTIONS -Wall #-}
{-# LANGUAGE CPP #-}

module Raylib.Util
  ( 
    -- * Bracket functions
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
    whileWindowOpen,
    whileWindowOpen_,
    whileWindowOpen0,

    -- * Miscellaneous
    cameraDirectionRay,
    setMaterialShader,
    inGHCi,
    WindowResources,
    Freeable (..),
  )
where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket, bracket_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Raylib.Core (beginBlendMode, beginDrawing, beginMode2D, beginMode3D, beginScissorMode, beginShaderMode, beginTextureMode, beginVrStereoMode, closeWindow, endBlendMode, endDrawing, endMode2D, endMode3D, endScissorMode, endShaderMode, endTextureMode, endVrStereoMode, initWindow, setTargetFPS, windowShouldClose)
import Raylib.ForeignUtil (Freeable (..))
import Raylib.Internal (WindowResources)
import Raylib.Types
  ( BlendMode,
    Camera2D,
    Camera3D (camera3D'position, camera3D'target),
    Material (material'shader),
    Model (model'materials),
    Ray (Ray),
    RenderTexture,
    Shader,
    VrStereoConfig,
  )
import Raylib.Util.Math (Vector (vectorNormalize, (|-|)))

withWindow :: (MonadIO m, MonadMask m) => Int -> Int -> String -> Int -> (WindowResources -> m b) -> m b
withWindow w h title fps = bracket (liftIO $ initWindow w h title <* setTargetFPS fps) (liftIO . closeWindow)

drawing :: (MonadIO m, MonadMask m) => m b -> m b
drawing = bracket_ (liftIO beginDrawing) (liftIO endDrawing)

mode2D :: (MonadIO m, MonadMask m) => Camera2D -> m b -> m b
mode2D camera = bracket_ (liftIO (beginMode2D camera)) (liftIO endMode2D)

mode3D :: (MonadIO m, MonadMask m) => Camera3D -> m b -> m b
mode3D camera = bracket_ (liftIO (beginMode3D camera)) (liftIO endMode3D)

textureMode :: (MonadIO m, MonadMask m) => RenderTexture -> m b -> m b
textureMode rt = bracket_ (liftIO (beginTextureMode rt)) (liftIO endTextureMode)

shaderMode :: (MonadIO m, MonadMask m) => Shader -> m b -> m b
shaderMode shader = bracket_ (liftIO (beginShaderMode shader)) (liftIO endShaderMode)

blendMode :: (MonadIO m, MonadMask m) => BlendMode -> m b -> m b
blendMode bm = bracket_ (liftIO (beginBlendMode bm)) (liftIO endBlendMode)

scissorMode :: (MonadIO m, MonadMask m) => Int -> Int -> Int -> Int -> m b -> m b
scissorMode x y width height = bracket_ (liftIO (beginScissorMode x y width height)) (liftIO endScissorMode)

vrStereoMode :: (MonadIO m, MonadMask m) => VrStereoConfig -> m b -> m b
vrStereoMode config = bracket_ (liftIO (beginVrStereoMode config)) (liftIO endVrStereoMode)

-- | Gets the direction of a camera as a ray.
cameraDirectionRay :: Camera3D -> Ray
cameraDirectionRay camera = Ray (camera3D'position camera) (vectorNormalize $ camera3D'target camera |-| camera3D'position camera)

-- | Calls the game loop every frame as long as the window is open.
--  For larger projects, instead of using this function, consider
--  making a custom game loop for flexibility.
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

-- | Sets the shader of a material at a specific index (WARNING: This will fail
-- if the index provided is out of bounds).
setMaterialShader ::
  -- | The model to operate on
  Model ->
  -- | The index of the material
  Int ->
  -- | The shader to use
  Shader ->
  -- | The modified model
  Model
setMaterialShader model matIdx shader = model {model'materials = setIdx mats matIdx newMat}
  where
    mats = model'materials model
    newMat = (mats !! matIdx) {material'shader = shader}
    setIdx l i v = take i l ++ [v] ++ drop (i + 1) l

-- | True if the program is running in GHCi
inGHCi :: Bool

#ifdef GHCI
inGHCi = True
#else
inGHCi = False
#endif