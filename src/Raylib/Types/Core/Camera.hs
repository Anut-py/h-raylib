{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}

module Raylib.Types.Core.Camera
  ( CameraMode (..),
    CameraProjection (..),
    Camera3D (..),
    Camera2D (..),
    Camera,
  )
where

import Foreign
  ( Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    castPtr,
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

data CameraProjection = CameraPerspective | CameraOrthographic deriving (Eq, Show, Enum)

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
  deriving (Eq, Show, Freeable)

instance Storable Camera3D where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    position <- peekByteOff _p 0
    target <- peekByteOff _p 12
    up <- peekByteOff _p 24
    fovy <- realToFrac <$> (peekByteOff _p 36 :: IO CFloat)
    projection <- peekByteOff _p 40
    return $ Camera3D position target up fovy projection
  poke _p (Camera3D position target up fovy projection) = do
    pokeByteOff _p 0 position
    pokeByteOff _p 12 target
    pokeByteOff _p 24 up
    pokeByteOff _p 36 (realToFrac fovy :: CFloat)
    pokeByteOff _p 40 projection
    return ()

type Camera = Camera3D

data Camera2D = Camera2D
  { camera2D'offset :: Vector2,
    camera2D'target :: Vector2,
    camera2D'rotation :: Float,
    camera2D'zoom :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable Camera2D where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    offset <- peekByteOff _p 0
    target <- peekByteOff _p 8
    rotation <- realToFrac <$> (peekByteOff _p 16 :: IO CFloat)
    zoom <- realToFrac <$> (peekByteOff _p 20 :: IO CFloat)
    return $ Camera2D offset target rotation zoom
  poke _p (Camera2D offset target rotation zoom) = do
    pokeByteOff _p 0 offset
    pokeByteOff _p 8 target
    pokeByteOff _p 16 (realToFrac rotation :: CFloat)
    pokeByteOff _p 20 (realToFrac zoom :: CFloat)
    return ()
