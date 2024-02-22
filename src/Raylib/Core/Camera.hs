{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings to @rcamera@ (raylib.h)
module Raylib.Core.Camera
  ( -- * High level
    updateCamera,
    updateCameraPro,

    -- * Native
    c'updateCamera,
    c'updateCameraPro,
  )
where

import Foreign (Ptr, Storable (peek))
import Foreign.C (CFloat (..), CInt (..))
import GHC.IO (unsafePerformIO)
import Raylib.Internal.Foreign (withFreeable)
import Raylib.Internal.TH (genNative)
import Raylib.Types (Camera3D, CameraMode, Vector3)

$( genNative
     [ ("c'updateCamera", "UpdateCamera_", "rl_bindings.h", [t|Ptr Camera3D -> CInt -> IO ()|], False),
       ("c'updateCameraPro", "UpdateCameraPro_", "rl_bindings.h", [t|Ptr Camera3D -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> IO ()|], False)
     ]
 )

updateCamera :: Camera3D -> CameraMode -> IO Camera3D
updateCamera camera mode =
  withFreeable
    camera
    ( \c -> do
        c'updateCamera c (fromIntegral $ fromEnum mode)
        peek c
    )

updateCameraPro :: Camera3D -> Vector3 -> Vector3 -> Float -> Camera3D
updateCameraPro camera movement rotation zoom =
  unsafePerformIO $
    withFreeable
      camera
      ( \c -> do
          withFreeable movement (\m -> withFreeable rotation (\r -> c'updateCameraPro c m r (realToFrac zoom)))
          peek c
      )
