{-# LANGUAGE DeriveAnyClass #-}

-- | Bindings for camera-related types
module Raylib.Types.Core.Camera
  ( -- * Enumerations
    CameraMode (..),
    CameraProjection (..),

    -- * Structures
    Camera3D (..),
    Camera2D (..),
    Camera,

    -- * Pointer utilities
    p'camera3D'position,
    p'camera3D'target,
    p'camera3D'up,
    p'camera3D'fovy,
    p'camera3D'projection,
    p'camera2D'offset,
    p'camera2D'target,
    p'camera2D'rotation,
    p'camera2D'zoom,
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, poke, sizeOf),
    castPtr,
    plusPtr,
  )
import Foreign.C
  ( CFloat,
    CInt (..),
  )
import Raylib.Internal.Foreign (Freeable)
import Raylib.Types.Core (Vector2, Vector3)

---------------------------------------
-- camera enums -----------------------
---------------------------------------

data CameraMode
  = CameraModeCustom
  | CameraModeFree
  | CameraModeOrbital
  | CameraModeFirstPerson
  | CameraModeThirdPerson
  deriving (Enum)

data CameraProjection = CameraPerspective | CameraOrthographic deriving (Eq, Show, Read, Enum)

instance Storable CameraProjection where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return (toEnum $ fromEnum (val :: CInt))
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

---------------------------------------
-- camera structures ------------------
---------------------------------------

data Camera3D = Camera3D
  { camera3D'position :: Vector3,
    camera3D'target :: Vector3,
    camera3D'up :: Vector3,
    camera3D'fovy :: Float,
    camera3D'projection :: CameraProjection
  }
  deriving (Eq, Show, Read, Freeable)

instance Storable Camera3D where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    position <- peek (p'camera3D'position _p)
    target <- peek (p'camera3D'target _p)
    up <- peek (p'camera3D'up _p)
    fovy <- realToFrac <$> peek (p'camera3D'fovy _p)
    projection <- peek (p'camera3D'projection _p)
    return $ Camera3D position target up fovy projection
  poke _p (Camera3D position target up fovy projection) = do
    poke (p'camera3D'position _p) position
    poke (p'camera3D'target _p) target
    poke (p'camera3D'up _p) up
    poke (p'camera3D'fovy _p) (realToFrac fovy)
    poke (p'camera3D'projection _p) projection
    return ()

p'camera3D'position :: Ptr Camera3D -> Ptr Vector3
p'camera3D'position = (`plusPtr` 0)

p'camera3D'target :: Ptr Camera3D -> Ptr Vector3
p'camera3D'target = (`plusPtr` 12)

p'camera3D'up :: Ptr Camera3D -> Ptr Vector3
p'camera3D'up = (`plusPtr` 24)

p'camera3D'fovy :: Ptr Camera3D -> Ptr CFloat
p'camera3D'fovy = (`plusPtr` 36)

p'camera3D'projection :: Ptr Camera3D -> Ptr CameraProjection
p'camera3D'projection = (`plusPtr` 40)

type Camera = Camera3D

data Camera2D = Camera2D
  { camera2D'offset :: Vector2,
    camera2D'target :: Vector2,
    camera2D'rotation :: Float,
    camera2D'zoom :: Float
  }
  deriving (Eq, Show, Read, Freeable)

instance Storable Camera2D where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    offset <- peek (p'camera2D'offset _p)
    target <- peek (p'camera2D'target _p)
    rotation <- realToFrac <$> peek (p'camera2D'rotation _p)
    zoom <- realToFrac <$> peek (p'camera2D'zoom _p)
    return $ Camera2D offset target rotation zoom
  poke _p (Camera2D offset target rotation zoom) = do
    poke (p'camera2D'offset _p) offset
    poke (p'camera2D'target _p) target
    poke (p'camera2D'rotation _p) (realToFrac rotation)
    poke (p'camera2D'zoom _p) (realToFrac zoom)
    return ()

p'camera2D'offset :: Ptr Camera2D -> Ptr Vector2
p'camera2D'offset = (`plusPtr` 0)

p'camera2D'target :: Ptr Camera2D -> Ptr Vector2
p'camera2D'target = (`plusPtr` 8)

p'camera2D'rotation :: Ptr Camera2D -> Ptr CFloat
p'camera2D'rotation = (`plusPtr` 16)

p'camera2D'zoom :: Ptr Camera2D -> Ptr CFloat
p'camera2D'zoom = (`plusPtr` 20)
