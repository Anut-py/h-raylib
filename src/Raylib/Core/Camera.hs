{-# LANGUAGE FlexibleContexts #-}
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

import Foreign (Ptr)
import Foreign.C (CFloat (..), CInt (..))
import Raylib.Internal.Foreign (Mutable (peekMutated), PLike, TLike (..), withFreeable)
import Raylib.Internal.TH (genNative)
import Raylib.Types (Camera3D, CameraMode, Vector3)

$( genNative
     [ ("c'updateCamera", "UpdateCamera_", "rl_bindings.h", [t|Ptr Camera3D -> CInt -> IO ()|]),
       ("c'updateCameraPro", "UpdateCameraPro_", "rl_bindings.h", [t|Ptr Camera3D -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> IO ()|])
     ]
 )

updateCamera :: (PLike Camera3D camera3D, Mutable camera3D mut) => camera3D -> CameraMode -> IO mut
updateCamera camera mode =
  withTLike
    camera
    ( \c -> do
        c'updateCamera c (fromIntegral $ fromEnum mode)
        peekMutated camera c
    )

updateCameraPro :: (PLike Camera3D camera3D, Mutable camera3D mut) => camera3D -> Vector3 -> Vector3 -> Float -> IO mut
updateCameraPro camera movement rotation zoom =
  withTLike
    camera
    ( \c -> do
        withFreeable movement (\m -> withFreeable rotation (\r -> c'updateCameraPro c m r (realToFrac zoom)))
        peekMutated camera c
    )
