{-# OPTIONS -Wall #-}

-- | Bindings to @rcamera@ (rcamera.h)
--
--   These were rewritten in Haskell instead of binding to C. All the functions
--   are pure.
module Raylib.Util.Camera
  ( getCameraForward,
    getCameraUp,
    getCameraRight,
    cameraMove,
    cameraMoveForward,
    cameraMoveUp,
    cameraMoveRight,
    cameraRotate,
    cameraYaw,
    cameraPitch,
    cameraRoll,
    getCameraViewMatrix,
    getCameraProjectionMatrix,
  )
where

import Raylib.Types (Camera, Camera3D (..), CameraProjection (CameraOrthographic, CameraPerspective), Matrix, Vector3 (..))
import Raylib.Util.Math (Vector (..), clamp, deg2Rad, matrixLookAt, matrixOrtho, matrixPerspective, vector3Angle, vector3CrossProduct, vector3RotateByAxisAngle)

-- | The camera's forward vector (normalized)
getCameraForward :: Camera -> Vector3
getCameraForward cam = vectorNormalize $ camera3D'target cam |-| camera3D'position cam

-- | The camera's up vector (normalized)
getCameraUp :: Camera -> Vector3
getCameraUp cam = vectorNormalize $ camera3D'up cam

-- | The camera's right vector (normalized)
getCameraRight :: Camera -> Vector3
getCameraRight cam = vector3CrossProduct (getCameraForward cam) (getCameraUp cam)

-- | Move the camera by a specific vector
cameraMove :: Camera -> Vector3 -> Camera
cameraMove cam dir =
  cam {camera3D'position = camera3D'position cam |+| dir, camera3D'target = camera3D'target cam |+| dir}

-- | Move the camera in its forward direction
cameraMoveForward ::
  Camera ->
  -- | Distance to move
  Float ->
  -- | Move in world plane (i.e. no vertical movement if enabled)
  Bool ->
  Camera
cameraMoveForward cam distance moveInWorldPlane =
  cameraMove cam (forward |* distance)
  where
    forward = if moveInWorldPlane then camForward {vector3'y = 0} else camForward
    camForward = getCameraForward cam

-- | Move the camera in its up direction
cameraMoveUp ::
  Camera ->
  -- | Distance to move
  Float ->
  Camera
cameraMoveUp cam distance =
  cameraMove cam (up |* distance)
  where
    up = getCameraUp cam

-- | Move the camera in its right direction
cameraMoveRight ::
  Camera ->
  -- | Distance to move
  Float ->
  -- | Move in world plane (i.e. no vertical movement if enabled)
  Bool ->
  Camera
cameraMoveRight cam distance moveInWorldPlane =
  cameraMove cam (right |* distance)
  where
    right = if moveInWorldPlane then camRight {vector3'y = 0} else camRight
    camRight = getCameraRight cam

-- | Rotate the camera using an axis and angle
cameraRotate ::
  Camera ->
  -- | Axis of rotation
  Vector3 ->
  -- | Angle to rotate by
  Float ->
  -- | Rotate around target (if false, the camera rotates around its position)
  Bool ->
  Camera
cameraRotate cam axis angle rotateAroundTarget =
  cam
    { camera3D'position = if rotateAroundTarget then target |-| viewRot else pos,
      camera3D'target = if rotateAroundTarget then target else pos |+| viewRot
    }
  where
    viewVec = target |-| pos
    viewRot = vector3RotateByAxisAngle viewVec axis angle
    pos = camera3D'position cam
    target = camera3D'target cam

-- | Rotate the camera around its up vector.
--   Yaw is "looking left and right".
cameraYaw ::
  Camera ->
  -- | Angle in radians
  Float ->
  -- | Rotate around target (if false, the camera rotates around its position)
  Bool ->
  Camera
cameraYaw cam angle rotateAroundTarget =
  cam
    { camera3D'position = if rotateAroundTarget then target |-| viewRot else pos,
      camera3D'target = if rotateAroundTarget then target else pos |+| viewRot
    }
  where
    viewVec = target |-| pos
    viewRot = vector3RotateByAxisAngle viewVec (getCameraUp cam) angle
    pos = camera3D'position cam
    target = camera3D'target cam

-- | Rotate the camera around its right vector.
--   Pitch is "looking up and down".
cameraPitch ::
  Camera ->
  -- | Angle in radians
  Float ->
  -- | Lock view (prevents camera overrotation, aka "somersaults")
  Bool ->
  -- | Rotate around target (if false, the camera rotates around its position)
  Bool ->
  -- | Rotate the camera's up vector to match the new pitch
  Bool ->
  Camera
cameraPitch cam angle lockView rotateAroundTarget rotateUp =
  cam
    { camera3D'position = if rotateAroundTarget then target |-| viewRot else pos,
      camera3D'target = if rotateAroundTarget then target else pos |+| viewRot,
      camera3D'up = if not rotateUp then up else vector3RotateByAxisAngle up right angle'
    }
  where
    angle' = if not lockView then angle else clamp angle maxAngleDown maxAngleUp
    maxAngleUp = vector3Angle up viewVec - 0.001
    maxAngleDown = (-vector3Angle (additiveInverse up) viewVec) + 0.001

    viewVec = target |-| pos
    viewRot = vector3RotateByAxisAngle viewVec right angle'

    pos = camera3D'position cam
    target = camera3D'target cam
    up = getCameraUp cam
    right = getCameraRight cam

-- | Rotates the camera around its forward vector.
--   Roll is "turning your head sideways to the left or right".
cameraRoll ::
  Camera ->
  -- | Angle in radians
  Float ->
  Camera
cameraRoll cam angle =
  cam
    { camera3D'up = vector3RotateByAxisAngle up forward angle
    }
  where
    forward = getCameraForward cam
    up = getCameraUp cam

-- | View matrix from camera
getCameraViewMatrix :: Camera -> Matrix
getCameraViewMatrix cam = matrixLookAt (camera3D'position cam) (camera3D'target cam) (camera3D'up cam)

-- | Projection matrix from camera
getCameraProjectionMatrix ::
  Camera ->
  -- | Aspect ratio
  Float ->
  -- | Near clipping plane distance (recommended: 0.01)
  Float ->
  -- | Far clipping plane distance (recommended: 1000)
  Float ->
  Matrix
getCameraProjectionMatrix cam aspect near far =
  case camera3D'projection cam of
    CameraPerspective -> matrixPerspective (camera3D'fovy cam * deg2Rad) aspect near far
    CameraOrthographic -> matrixOrtho (-right) right (-top) top near far
      where
        top = camera3D'fovy cam / 2
        right = top * aspect
