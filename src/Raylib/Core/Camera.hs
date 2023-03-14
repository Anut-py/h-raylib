module Raylib.Core.Camera (updateCamera, updateCameraPro) where

import Foreign (Storable (peek))
import GHC.IO (unsafePerformIO)
import Raylib.ForeignUtil (withFreeable)
import Raylib.Native (c'updateCamera, c'updateCameraPro)
import Raylib.Types (Camera3D, CameraMode, Vector3)

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
