{-# OPTIONS -Wall #-}
{-# LANGUAGE CPP #-}

module Raylib.Util (cameraDirectionRay, whileWindowOpen, whileWindowOpen_, whileWindowOpen0, setMaterialShader, inGHCi) where

import Control.Monad (void)
import Raylib.Core (windowShouldClose)
import Raylib.Types
  ( Camera3D (camera3D'position, camera3D'target),
    Material (material'shader),
    Model (model'materials),
    Ray (Ray),
    Shader,
    Vector (normalize, (|-|)),
  )

-- | Gets the direction of a camera as a ray.
cameraDirectionRay :: Camera3D -> Ray
cameraDirectionRay camera = Ray (camera3D'position camera) (normalize $ camera3D'target camera |-| camera3D'position camera)

-- | Calls the game loop every frame as long as the window is open.
--  For larger projects, instead of using this function, consider
--  making a custom game loop for flexibility.
whileWindowOpen ::
  -- | The game loop. Its only argument should be the current application state, and it should return a new state.
  (a -> IO a) ->
  -- | The initial application state.
  a ->
  -- | The application state after the last frame.
  IO a
whileWindowOpen f state = do
  newState <- f state
  shouldClose <- windowShouldClose
  if shouldClose
    then return newState
    else whileWindowOpen f newState

-- | Same as `whileWindowOpen`, but discards the final state.
whileWindowOpen_ ::
  (a -> IO a) ->
  a ->
  IO ()
whileWindowOpen_ f state = void (whileWindowOpen f state)

-- | Same as `whileWindowOpen`, but without application state.
whileWindowOpen0 ::
  IO () ->
  IO ()
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