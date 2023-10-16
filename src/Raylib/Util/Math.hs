{-# OPTIONS -Wall #-}

module Raylib.Util.Math
  ( -- * Utility constants
    epsilon,
    deg2Rad,
    rad2Deg,

    -- * Float math
    clamp,
    lerp,
    normalize,
    remap,
    wrap,
    floatEquals,

    -- * General vector math
    Vector (..),

    -- * Vector2 math
    vector2Angle,
    vector2LineAngle,
    vector2Transform,
    vector2Reflect,
    vector2Rotate,

    -- * Vector3 math
    vector3CrossProduct,
    vector3Perpendicular,
    vector3Angle,
    vector3OrthoNormalize,
    vector3Transform,
    vector3RotateByQuaternion,
    vector3RotateByAxisAngle,
    vector3Reflect,
    vector3Barycenter,
    vector3Unproject,
    vector3Refract,

    -- * Matrix math
    matrixToList,
    matrixFromList,
    matrixConstant,
    matrixDeterminant,
    matrixTrace,
    matrixTranspose,
    matrixInvert,
    matrixIdentity,
    matrixAdd,
    (/+/),
    matrixSubtract,
    (/-/),
    matrixMultiply,
    (/*/),
    matrixTranslate,
    matrixRotate,
    matrixRotateX,
    matrixRotateY,
    matrixRotateZ,
    matrixRotateXYZ,
    matrixRotateZYX,
    matrixScale,
    matrixFrustum,
    matrixPerspective,
    matrixOrtho,
    matrixLookAt,

    -- * Quaternion math
    quaternionIdentity,
    quaternionInvert,
    quaternionMultiply,
    quaternionNormalize,
    quaternionLerp,
    quaternionNLerp,
    quaternionSLerp,
    quaternionFromVector3ToVector3,
    quaternionFromMatrix,
    quaternionToMatrix,
    quaternionFromAxisAngle,
    quaternionToAxisAngle,
    quaternionFromEuler,
    quaternionToEuler,
    quaternionTransform,
  )
where

import Raylib.Types (Matrix (..), Quaternion, Vector2 (Vector2), Vector3 (Vector3), Vector4 (Vector4))
import Data.Foldable (foldl')

epsilon :: Float
epsilon = 0.000001

deg2Rad :: Float
deg2Rad = pi / 180

rad2Deg :: Float
rad2Deg = 180 / pi

------------------------------------------------
-- Float math ----------------------------------
------------------------------------------------

-- | Clamp float to range
clamp ::
  -- | Value to clamp
  Float ->
  -- | Lower bound
  Float ->
  -- | Upper bound
  Float ->
  Float
clamp value low high
  | value < low = low
  | value > high = high
  | otherwise = value

-- | Calculate linear interpolation between two floats
lerp ::
  -- | Starting value
  Float ->
  -- | Ending value
  Float ->
  -- | Lerp amount
  Float ->
  Float
lerp start end amount = start + amount * (end - start)

-- | Normalize input value within input range
normalize ::
  -- | Value to normalize
  Float ->
  -- | Starting value of range
  Float ->
  -- | Ending value of range
  Float ->
  Float
normalize value start end = (value - start) / (end - start)

-- | Remap input value within input range to output range
remap ::
  -- | Input value
  Float ->
  -- | Input range start
  Float ->
  -- | Input range end
  Float ->
  -- | Output range start
  Float ->
  -- | Output range end
  Float ->
  Float
remap value inputStart inputEnd outputStart outputEnd = (value - inputStart) / (inputEnd - inputStart) * (outputEnd - outputStart) + outputStart

-- | Wrap input value from min to max
wrap ::
  -- | Input value
  Float ->
  -- | Min value
  Float ->
  -- | Max value
  Float ->
  Float
wrap value low high = value - (high - low) * fromIntegral (floor ((value - low) / (high - low)) :: Integer)

-- | Check if two floats are close to equal
floatEquals :: Float -> Float -> Bool
floatEquals x y = abs (x - y) <= (epsilon * max 1 (max (abs x) (abs y)))

------------------------------------------------
-- Vector math ---------------------------------
------------------------------------------------

class Vector a where
  -- | List representation of a vector
  asList :: a -> [Float]

  -- | Vector representation of a list
  fromList :: [Float] -> a

  -- | Vector-vector addition
  (|+|) :: a -> a -> a
  a |+| b = fromList $ zipWith (+) (asList a) (asList b)

  -- | Vector-vector subtraction
  (|-|) :: a -> a -> a
  a |-| b = fromList $ zipWith (-) (asList a) (asList b)

  -- | Vector-scalar addition
  (|+) :: a -> Float -> a
  a |+ b = a |+| constant b

  -- | Vector-scalar subtraction
  (|-) :: a -> Float -> a
  a |- b = a |-| constant b

  -- | Vector-vector multiplication
  (|*|) :: a -> a -> a
  a |*| b = fromList $ zipWith (*) (asList a) (asList b)

  -- | Vector-vector division
  (|/|) :: a -> a -> a
  a |/| b = fromList $ zipWith (/) (asList a) (asList b)

  -- | Vector-scalar multiplication
  (|*) :: a -> Float -> a
  a |* b = a |*| constant b

  -- | Vector-scalar division
  (|/) :: a -> Float -> a
  a |/ b = a |/| constant b

  -- | Vector-vector dot product
  (|.|) :: a -> a -> Float
  a |.| b = sum . asList $ a |*| b

  -- | Zero vector
  zero :: a
  zero = constant 0

  -- | One vector
  one :: a
  one = constant 1

  -- | Scalar to vector (all elements are set to the scalar)
  constant :: Float -> a
  constant val = fromList $ repeat val

  -- | Vector additive inverse
  additiveInverse :: a -> a
  additiveInverse v = fromList $ map negate (asList v)

  -- | Vector multiplicative inverse
  multiplicativeInverse :: a -> a
  multiplicativeInverse v = fromList $ map (1 /) (asList v)

  -- | Squared magnitude of a vector
  magnitudeSqr :: a -> Float
  magnitudeSqr v = v |.| v

  -- | Vector magnitude
  magnitude :: a -> Float
  magnitude v = if m == 1 then m else sqrt m
    where
      m = magnitudeSqr v

  -- | Squared distance between two vectors
  vectorDistanceSqr :: a -> a -> Float
  vectorDistanceSqr a b = magnitudeSqr $ a |-| b

  -- | Distance between two vectors
  vectorDistance :: a -> a -> Float
  vectorDistance a b = sqrt $ vectorDistanceSqr a b

  -- | Normalize vector (same direction, magnitude 1)
  vectorNormalize :: a -> a
  vectorNormalize v = v |/ magnitude v

  -- | Lerp between two vectors
  vectorLerp :: a -> a -> Float -> a
  vectorLerp a b amount = fromList $ zipWith (\v1 v2 -> lerp v1 v2 amount) (asList a) (asList b)

  -- | Move vector towards target
  vectorMoveTowards ::
    -- | Vector to move
    a ->
    -- | Target vector
    a ->
    -- | Max distance to move by
    Float ->
    a
  vectorMoveTowards v target maxDistance =
    if distSquared <= maxDistance * maxDistance
      then target
      else v |+| fromList (map (* (maxDistance / dist)) (asList diff))
    where
      diff = target |-| v
      distSquared = magnitudeSqr diff
      dist = sqrt distSquared

  -- | Clamp vector to range
  vectorClamp ::
    -- | Vector to clamp
    a ->
    -- | Lower bound
    a ->
    -- | Upper bound
    a ->
    a
  vectorClamp v low high = fromList $ zipWith3 clamp (asList v) (asList low) (asList high)

  -- | Clamp the magnitude of a vector to a range
  vectorClampValue ::
    -- | Vector to clamp
    a ->
    -- | Lower bound
    Float ->
    -- | Upper bound
    Float ->
    a
  vectorClampValue v low high = v |* (clamp size low high / size)
    where
      size = magnitude v

  -- | Min value for each pair of components
  vectorMin :: a -> a -> a
  vectorMin v1 v2 = fromList $ zipWith min (asList v1) (asList v2)

  -- | Max value for each pair of components
  vectorMax :: a -> a -> a
  vectorMax v1 v2 = fromList $ zipWith max (asList v1) (asList v2)

  -- | sum of vectors
  vectorSum :: Foldable t => t a -> a
  vectorSum = foldl' (|+|) zero

instance Vector Vector2 where
  asList (Vector2 x y) = [x, y]
  fromList (x : y : _) = Vector2 x y
  fromList _ = error "(Vector2.fromList) Input list must have at least two elements!"

instance Vector Vector3 where
  asList (Vector3 x y z) = [x, y, z]
  fromList (x : y : z : _) = Vector3 x y z
  fromList _ = error "(Vector3.fromList) Input list must have at least three elements!"

instance Vector Vector4 where
  asList (Vector4 x y z w) = [x, y, z, w]
  fromList (x : y : z : w : _) = Vector4 x y z w
  fromList _ = error "(Vector4.fromList) Input list must have at least four elements!"

------------------------------------------------
-- Vector2 math --------------------------------
------------------------------------------------

-- | Angle between two 2D vectors
vector2Angle :: Vector2 -> Vector2 -> Float
vector2Angle (Vector2 x1 y1) (Vector2 x2 y2) = - atan2 (x1 * x2 + y1 * y2) (x1 * y2 - y1 * x2)

-- | Angle created by the line between two 2D vectors (parameters must be normalized)
vector2LineAngle :: Vector2 -> Vector2 -> Float
vector2LineAngle (Vector2 sx sy) (Vector2 ex ey) = atan2 (ey - sy) (ex - sx)

-- | Transform a 2D vector by the given matrix
vector2Transform :: Vector2 -> Matrix -> Vector2
vector2Transform (Vector2 x y) mat = Vector2 ((x * matrix'm0 mat) + (y * matrix'm4 mat) + matrix'm12 mat) ((x * matrix'm1 mat) + (y * matrix'm5 mat) + matrix'm13 mat)

-- | Reflect 2D vector to normal
vector2Reflect ::
  -- | Input vector
  Vector2 ->
  -- | Normal vector
  Vector2 ->
  Vector2
vector2Reflect v normal = v |-| (normal |* (2 * (v |.| normal)))

-- | Rotate 2D vector by angle
vector2Rotate :: Vector2 -> Float -> Vector2
vector2Rotate (Vector2 x y) angle = Vector2 (x * c - y * s) (x * s + y * c)
  where
    c = cos angle
    s = sin angle

------------------------------------------------
-- Vector3 math --------------------------------
------------------------------------------------

-- | 3D vector cross-product
vector3CrossProduct :: Vector3 -> Vector3 -> Vector3
vector3CrossProduct (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

-- | Perpendicular to given 3D vector
vector3Perpendicular :: Vector3 -> Vector3
vector3Perpendicular v@(Vector3 x y z) = vector3CrossProduct v cardinalAxis
  where
    cardinalAxis
      | abs y < abs x = Vector3 0 1 0
      | abs z < abs x = Vector3 0 0 1
      | otherwise = Vector3 1 0 0

-- | Angle between two 3D vectors
vector3Angle :: Vector3 -> Vector3 -> Float
vector3Angle v1 v2 = atan2 len dot
  where
    cross = vector3CrossProduct v1 v2
    len = magnitude cross
    dot = v1 |.| v2

-- | Orthonormalize provided vectors.
--   Makes vectors normalized and orthogonal to each other.
--   Gram-Schmidt function implementation.
vector3OrthoNormalize :: Vector3 -> Vector3 -> (Vector3, Vector3)
vector3OrthoNormalize v1 v2 = (v1', v2')
  where
    v1' = vectorNormalize v1
    v2' = vector3CrossProduct vn1 v1'
    vn1 = vectorNormalize $ vector3CrossProduct v1' v2

-- | Transform a 3D vector by a matrix
vector3Transform :: Vector3 -> Matrix -> Vector3
vector3Transform
  (Vector3 x y z)
  (Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 _ _ _ _) =
    Vector3
      (x * m0 + y * m4 + z * m8 + m12)
      (x * m1 + y * m5 + z * m9 + m13)
      (x * m2 + y * m6 + z * m10 + m14)

-- | Transform a 3D vector by quaternion rotation
vector3RotateByQuaternion :: Vector3 -> Quaternion -> Vector3
vector3RotateByQuaternion (Vector3 a b c) (Vector4 x y z w) =
  Vector3
    (a * (x * x + w * w - y * y - z * z) + b * (2 * x * y - 2 * w * z) + c * (2 * x * z + 2 * w * y))
    (a * (2 * w * z + 2 * x * y) + b * (w * w - x * x + y * y - z * z) + c * ((-2) * w * x + 2 * y * z))
    (a * ((-2) * w * y + 2 * x * z) + b * (2 * w * x + 2 * y * z) + c * (w * w - x * x - y * y + z * z))

-- | Rotate a 3D vector around an axis
vector3RotateByAxisAngle ::
  -- | Vector to rotate
  Vector3 ->
  -- | Axis to rotate around
  Vector3 ->
  -- | Angle to rotate by
  Float ->
  Vector3
vector3RotateByAxisAngle v axis angle = v |+| (wv |* (2 * a)) |+| (wwv |* 2)
  where
    (Vector3 ax ay az) = vectorNormalize axis
    s = sin (angle / 2)
    a = cos (angle / 2)
    b = ax * s
    c = ay * s
    d = az * s
    w = Vector3 b c d
    wv = vector3CrossProduct w v
    wwv = vector3CrossProduct w wv

-- | Reflect 3D vector to normal
vector3Reflect ::
  -- | Input vector
  Vector3 ->
  -- | Normal vector
  Vector3 ->
  Vector3
vector3Reflect v normal = v |-| (normal |* (2 * v |.| normal))

-- | Compute barycenter coordinates (u, v, w) for a point with respect to a triangle.
--   NOTE: Assumes the point is on the plane of the triangle.
vector3Barycenter ::
  -- | Input point
  Vector3 ->
  -- | Triangle vertices
  (Vector3, Vector3, Vector3) ->
  Vector3
vector3Barycenter p (a, b, c) = Vector3 (1 - y - z) y z
  where
    v0 = b |-| a
    v1 = c |-| a
    v2 = p |-| a
    d00 = v0 |.| v0
    d01 = v0 |.| v1
    d11 = v1 |.| v1
    d20 = v2 |.| v0
    d21 = v2 |.| v1
    denom = d00 * d11 - d01 * d01
    y = (d11 * d20 - d01 * d21) / denom
    z = (d00 * d21 - d01 * d20) / denom

-- | Project a Vector3 from screen space into object space
vector3Unproject ::
  -- | Vector to unproject
  Vector3 ->
  -- | Projection matrix
  Matrix ->
  -- | View matrix
  Matrix ->
  Vector3
vector3Unproject (Vector3 x y z) projection view = Vector3 (rx / rw) (ry / rw) (rz / rw)
  where
    matViewProj = view /*/ projection
    a00 = matrix'm0 matViewProj
    a01 = matrix'm1 matViewProj
    a02 = matrix'm2 matViewProj
    a03 = matrix'm3 matViewProj
    a10 = matrix'm4 matViewProj
    a11 = matrix'm5 matViewProj
    a12 = matrix'm6 matViewProj
    a13 = matrix'm7 matViewProj
    a20 = matrix'm8 matViewProj
    a21 = matrix'm9 matViewProj
    a22 = matrix'm10 matViewProj
    a23 = matrix'm11 matViewProj
    a30 = matrix'm12 matViewProj
    a31 = matrix'm13 matViewProj
    a32 = matrix'm14 matViewProj
    a33 = matrix'm15 matViewProj
    b00 = a00 * a11 - a01 * a10
    b01 = a00 * a12 - a02 * a10
    b02 = a00 * a13 - a03 * a10
    b03 = a01 * a12 - a02 * a11
    b04 = a01 * a13 - a03 * a11
    b05 = a02 * a13 - a03 * a12
    b06 = a20 * a31 - a21 * a30
    b07 = a20 * a32 - a22 * a30
    b08 = a20 * a33 - a23 * a30
    b09 = a21 * a32 - a22 * a31
    b10 = a21 * a33 - a23 * a31
    b11 = a22 * a33 - a23 * a32
    invDet = 1 / (b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06)
    matViewProjInv =
      Matrix
        ((a11 * b11 - a12 * b10 + a13 * b09) * invDet)
        ((- a01 * b11 + a02 * b10 - a03 * b09) * invDet)
        ((a31 * b05 - a32 * b04 + a33 * b03) * invDet)
        ((- a21 * b05 + a22 * b04 - a23 * b03) * invDet)
        ((- a10 * b11 + a12 * b08 - a13 * b07) * invDet)
        ((a00 * b11 - a02 * b08 + a03 * b07) * invDet)
        ((- a30 * b05 + a32 * b02 - a33 * b01) * invDet)
        ((a20 * b05 - a22 * b02 + a23 * b01) * invDet)
        ((a10 * b10 - a11 * b08 + a13 * b06) * invDet)
        ((- a00 * b10 + a01 * b08 - a03 * b06) * invDet)
        ((a30 * b04 - a31 * b02 + a33 * b00) * invDet)
        ((- a20 * b04 + a21 * b02 - a23 * b00) * invDet)
        ((- a10 * b09 + a11 * b07 - a12 * b06) * invDet)
        ((a00 * b09 - a01 * b07 + a02 * b06) * invDet)
        ((- a30 * b03 + a31 * b01 - a32 * b00) * invDet)
        ((a20 * b03 - a21 * b01 + a22 * b00) * invDet)
    (Vector4 rx ry rz rw) = quaternionTransform (Vector4 x y z 1) matViewProjInv

-- | Compute the direction of a refracted ray
vector3Refract ::
  -- | Normalized direction of the incoming ray
  Vector3 ->
  -- | Normalized normal vector of the interface of two optical media
  Vector3 ->
  -- | Ratio of the refractive index of the medium from where the ray
  --   comes to the refractive index of the medium on the other side of
  --   the surface
  Float ->
  Vector3
vector3Refract (Vector3 x y z) (Vector3 nx ny nz) r = Vector3 (r * x - (r * dot + d) * nx) (r * y - (r * dot + d) * ny) (r * z - (r * dot + d) * nz)
  where
    dot = x * nx + y * ny + z * nz
    d = sqrt $ 1 - r * r * (1 - dot * dot)

------------------------------------------------
-- Matrix math ---------------------------------
------------------------------------------------

-- | Utility function
matrixToList :: Matrix -> [Float]
matrixToList
  (Matrix a00 a10 a20 a30 a01 a11 a21 a31 a02 a12 a22 a32 a03 a13 a23 a33) =
    [a00, a10, a20, a30, a01, a11, a21, a31, a02, a12, a22, a32, a03, a13, a23, a33]

-- | Utility function
matrixFromList :: [Float] -> Matrix
matrixFromList
  (a00 : a10 : a20 : a30 : a01 : a11 : a21 : a31 : a02 : a12 : a22 : a32 : a03 : a13 : a23 : a33 : _) =
    Matrix a00 a10 a20 a30 a01 a11 a21 a31 a02 a12 a22 a32 a03 a13 a23 a33
matrixFromList _ = error "matrixFromList expects a list of at least 16 elements!"

-- | Scalar to matrix (all elements are set to the scalar)
matrixConstant :: Float -> Matrix
matrixConstant n = matrixFromList $ repeat n

-- | Compute matrix determinant
matrixDeterminant :: Matrix -> Float
matrixDeterminant
  (Matrix a00 a10 a20 a30 a01 a11 a21 a31 a02 a12 a22 a32 a03 a13 a23 a33) =
    a30 * a21 * a12 * a03 - a20 * a31 * a12 * a03 - a30 * a11 * a22 * a03 + a10 * a31 * a22 * a03
      + a20 * a11 * a32 * a03 - a10 * a21 * a32 * a03 - a30 * a21 * a02 * a13
      + a20 * a31 * a02 * a13
      + a30 * a01 * a22 * a13 - a00 * a31 * a22 * a13 - a20 * a01 * a32 * a13
      + a00 * a21 * a32 * a13
      + a30 * a11 * a02 * a23 - a10 * a31 * a02 * a23 - a30 * a01 * a12 * a23
      + a00 * a31 * a12 * a23
      + a10 * a01 * a32 * a23 - a00 * a11 * a32 * a23 - a20 * a11 * a02 * a33
      + a10 * a21 * a02 * a33
      + a20 * a01 * a12 * a33 - a00 * a21 * a12 * a33 - a10 * a01 * a22 * a33
      + a00 * a11 * a22 * a33

-- | Trace of a matrix (sum of the values along the diagonal)
matrixTrace :: Matrix -> Float
matrixTrace mat = matrix'm0 mat + matrix'm5 mat + matrix'm10 mat + matrix'm15 mat

-- | Transpose a matrix
matrixTranspose :: Matrix -> Matrix
matrixTranspose
  (Matrix m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14 m3 m7 m11 m15) =
    Matrix m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15

-- | Invert a matrix
matrixInvert :: Matrix -> Matrix
matrixInvert
  (Matrix a00 a10 a20 a30 a01 a11 a21 a31 a02 a12 a22 a32 a03 a13 a23 a33) =
    Matrix
      ((a11 * b11 - a12 * b10 + a13 * b09) * invDet)
      ((- a10 * b11 + a12 * b08 - a13 * b07) * invDet)
      ((a10 * b10 - a11 * b08 + a13 * b06) * invDet)
      ((- a10 * b09 + a11 * b07 - a12 * b06) * invDet)
      ((- a01 * b11 + a02 * b10 - a03 * b09) * invDet)
      ((a00 * b11 - a02 * b08 + a03 * b07) * invDet)
      ((- a00 * b10 + a01 * b08 - a03 * b06) * invDet)
      ((a00 * b09 - a01 * b07 + a02 * b06) * invDet)
      ((a31 * b05 - a32 * b04 + a33 * b03) * invDet)
      ((- a30 * b05 + a32 * b02 - a33 * b01) * invDet)
      ((a30 * b04 - a31 * b02 + a33 * b00) * invDet)
      ((- a30 * b03 + a31 * b01 - a32 * b00) * invDet)
      ((- a21 * b05 + a22 * b04 - a23 * b03) * invDet)
      ((a20 * b05 - a22 * b02 + a23 * b01) * invDet)
      ((- a20 * b04 + a21 * b02 - a23 * b00) * invDet)
      ((a20 * b03 - a21 * b01 + a22 * b00) * invDet)
    where
      b00 = a00 * a11 - a01 * a10
      b01 = a00 * a12 - a02 * a10
      b02 = a00 * a13 - a03 * a10
      b03 = a01 * a12 - a02 * a11
      b04 = a01 * a13 - a03 * a11
      b05 = a02 * a13 - a03 * a12
      b06 = a20 * a31 - a21 * a30
      b07 = a20 * a32 - a22 * a30
      b08 = a20 * a33 - a23 * a30
      b09 = a21 * a32 - a22 * a31
      b10 = a21 * a33 - a23 * a31
      b11 = a22 * a33 - a23 * a32
      invDet = 1 / (b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06)

-- | Identity matrix
matrixIdentity :: Matrix
matrixIdentity = Matrix 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1

-- | Add two matrices
matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd left right = matrixFromList $ zipWith (+) (matrixToList left) (matrixToList right)

-- | Alias for 'matrixAdd'
(/+/) :: Matrix -> Matrix -> Matrix
(/+/) = matrixAdd

-- | Subtract two matrices
matrixSubtract :: Matrix -> Matrix -> Matrix
matrixSubtract left right = matrixFromList $ zipWith (-) (matrixToList left) (matrixToList right)

-- | Alias for 'matrixSubtract'
(/-/) :: Matrix -> Matrix -> Matrix
(/-/) = matrixSubtract

-- | Multiply two matrices (order matters!)
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply
  (Matrix l0 l4 l8 l12 l1 l5 l9 l13 l2 l6 l10 l14 l3 l7 l11 l15)
  (Matrix r0 r4 r8 r12 r1 r5 r9 r13 r2 r6 r10 r14 r3 r7 r11 r15) =
    Matrix
      (l0 * r0 + l1 * r4 + l2 * r8 + l3 * r12)
      (l4 * r0 + l5 * r4 + l6 * r8 + l7 * r12)
      (l8 * r0 + l9 * r4 + l10 * r8 + l11 * r12)
      (l12 * r0 + l13 * r4 + l14 * r8 + l15 * r12)
      (l0 * r1 + l1 * r5 + l2 * r9 + l3 * r13)
      (l4 * r1 + l5 * r5 + l6 * r9 + l7 * r13)
      (l8 * r1 + l9 * r5 + l10 * r9 + l11 * r13)
      (l12 * r1 + l13 * r5 + l14 * r9 + l15 * r13)
      (l0 * r2 + l1 * r6 + l2 * r10 + l3 * r14)
      (l4 * r2 + l5 * r6 + l6 * r10 + l7 * r14)
      (l8 * r2 + l9 * r6 + l10 * r10 + l11 * r14)
      (l12 * r2 + l13 * r6 + l14 * r10 + l15 * r14)
      (l0 * r3 + l1 * r7 + l2 * r11 + l3 * r15)
      (l4 * r3 + l5 * r7 + l6 * r11 + l7 * r15)
      (l8 * r3 + l9 * r7 + l10 * r11 + l11 * r15)
      (l12 * r3 + l13 * r7 + l14 * r11 + l15 * r15)

-- | Alias for 'matrixMultiply'
(/*/) :: Matrix -> Matrix -> Matrix
(/*/) = matrixMultiply

-- | Translation matrix
matrixTranslate ::
  -- | x translation
  Float ->
  -- | y translation
  Float ->
  -- | z translation
  Float ->
  Matrix
matrixTranslate x y z = Matrix 1 0 0 x 0 1 0 y 0 0 1 z 0 0 0 1

-- | Axis-angle rotation matrix (angle should be in radians)
matrixRotate ::
  -- | Axis to rotate around
  Vector3 ->
  -- | Angle to rotate by
  Float ->
  Matrix
matrixRotate axis angle = Matrix (x * x * t + c) (x * y * t - z * s) (x * z * t + y * s) 0 (y * x * t + z * s) (y * y * t + c) (y * z * t - x * s) 0 (z * x * t - y * s) (z * y * t + x * s) (z * z * t + c) 0 0 0 0 1
  where
    (Vector3 x y z) = vectorNormalize axis
    s = sin angle
    c = cos angle
    t = 1 - c

-- | x-rotation matrix (angle should be in radians)
matrixRotateX :: Float -> Matrix
matrixRotateX angle = Matrix 1 0 0 0 0 c (- s) 0 0 s c 0 0 0 0 1
  where
    s = sin angle
    c = cos angle

-- | y-rotation matrix (angle should be in radians)
matrixRotateY :: Float -> Matrix
matrixRotateY angle = Matrix c 0 s 0 0 1 0 0 (- s) 0 c 0 0 0 0 1
  where
    s = sin angle
    c = cos angle

-- | z-rotation matrix (angle should be in radians)
matrixRotateZ :: Float -> Matrix
matrixRotateZ angle = Matrix c (- s) 0 0 s c 0 0 0 0 1 0 0 0 0 1
  where
    s = sin angle
    c = cos angle

-- | Euler angle xyz rotation matrix (angles should be in radians)
matrixRotateXYZ :: Vector3 -> Matrix
matrixRotateXYZ (Vector3 x y z) = Matrix (cz - cy) (sz * cy) (- sy) 0 (cz * sy * sx - sz * cx) (sz * sy * sx + cz * cx) (cy * sx) 0 (cz * sy * cx + sz * sx) (sz * sy * cx - cz * sx) (cy * cx) 0 0 0 0 1
  where
    cx = cos (- x)
    sx = sin (- x)
    cy = cos (- y)
    sy = sin (- y)
    cz = cos (- z)
    sz = sin (- z)

-- | Euler angle zyx rotation matrix (angles should be in radians)
matrixRotateZYX :: Vector3 -> Matrix
matrixRotateZYX (Vector3 x y z) = Matrix (cz * cy) (cz * sy * sx - cx * sz) (sz * sx + cz * cx * sy) 0 (cy * sz) (cz * cx + sz * sy * sx) (cx * sz * sy - cz * sx) 0 (- sy) (cy * sx) (cy * cx) 0 0 0 0 1
  where
    cz = cos z
    sz = sin z
    cy = cos y
    sy = sin y
    cx = cos x
    sx = sin x

-- | Scaling matrix
matrixScale :: Vector3 -> Matrix
matrixScale (Vector3 x y z) = Matrix x 0 0 0 0 y 0 0 0 0 z 0 0 0 0 1

-- | Frustum projection matrix
matrixFrustum ::
  -- | Left edge distance
  Float ->
  -- | Right edge distance
  Float ->
  -- | Bottom edge distance
  Float ->
  -- | Top edge distance
  Float ->
  -- | Near clipping plane distance
  Float ->
  -- | Far clipping plane distance
  Float ->
  Matrix
matrixFrustum left right bottom top near far =
  Matrix
    (near * 2 / x)
    0
    ((right + left) / x)
    0
    0
    (near * 2 / y)
    ((top + bottom) / y)
    0
    0
    0
    (- (far + near) / z)
    (- far * near * 2 / z)
    0
    0
    (-1)
    0
  where
    x = right - left
    y = top - bottom
    z = far - near

-- | Perspective projection matrix
matrixPerspective ::
  -- | y-fov angle (should be in radians)
  Float ->
  -- | Aspect ratio
  Float ->
  -- | Near clipping plane distance
  Float ->
  -- | Far clipping plane distance
  Float ->
  Matrix
matrixPerspective fovy aspect near far = matrixFrustum left right bottom top near far
  where
    top = near * tan (fovy / 2)
    bottom = - top
    right = top * aspect
    left = - right

-- | Orthographic projection matrix
matrixOrtho ::
  -- | Left edge distance
  Float ->
  -- | Right edge distance
  Float ->
  -- | Bottom edge distance
  Float ->
  -- | Top edge distance
  Float ->
  -- | Near clipping plane distance
  Float ->
  -- | Far clipping plane distance
  Float ->
  Matrix
matrixOrtho left right bottom top near far =
  Matrix (2 / x) 0 0 (- (left + right) / x) 0 (2 / y) 0 (- (top + bottom) / y) 0 0 (-2 / z) (- (far + near) / z) 0 0 0 1
  where
    x = right - left
    y = top - bottom
    z = far - near

-- | Camera look-at matrix (view matrix)
matrixLookAt ::
  -- | Camera position
  Vector3 ->
  -- | Camera target
  Vector3 ->
  -- | World up vector
  Vector3 ->
  Matrix
matrixLookAt eye target up = Matrix xx xy xz (- vx |.| eye) yx yy yz (- vy |.| eye) zx zy zz (- vz |.| eye) 0 0 0 1
  where
    vz@(Vector3 zx zy zz) = vectorNormalize $ eye |-| target
    vx@(Vector3 xx xy xz) = vectorNormalize $ vector3CrossProduct up vz
    vy@(Vector3 yx yy yz) = vector3CrossProduct vz vx

------------------------------------------------
-- Quaternion math -----------------------------
------------------------------------------------

-- | Identity quaternion
quaternionIdentity :: Quaternion
quaternionIdentity = Vector4 0 0 0 1

-- | Invert a quaternion
quaternionInvert :: Quaternion -> Quaternion
quaternionInvert q@(Vector4 x y z w) = Vector4 (- x * invLength) (- y * invLength) (- z * invLength) (w * invLength)
  where
    invLength = 1 / magnitudeSqr q

-- | Multiply two quaternions
quaternionMultiply :: Quaternion -> Quaternion -> Quaternion
quaternionMultiply (Vector4 qax qay qaz qaw) (Vector4 qbx qby qbz qbw) = Vector4 (qax * qbw + qaw * qbx + qay * qbz - qaz * qby) (qay * qbw + qaw * qby + qaz * qbx - qax * qbz) (qaz * qbw + qaw * qbz + qax * qby - qay * qbx) (qaw * qbw - qax * qbx - qay * qby - qaz * qbz)

-- | Normalize a quaternion (alias for 'vectorNormalize')
quaternionNormalize :: Quaternion -> Quaternion
quaternionNormalize = vectorNormalize

-- | Lerp between two quaternions (alias for 'quaternionLerp')
quaternionLerp ::
  -- | Lerp start value
  Quaternion ->
  -- | Lerp end value
  Quaternion ->
  -- | Lerp amount
  Float ->
  Quaternion
quaternionLerp = vectorLerp

-- | Slerp-optimized interpolation between two quaternions
quaternionNLerp ::
  -- | Lerp start value
  Quaternion ->
  -- | Lerp end value
  Quaternion ->
  -- | Lerp amount
  Float ->
  Quaternion
quaternionNLerp q1 q2 amount = vectorNormalize $ quaternionLerp q1 q2 amount

-- | Spherical linear interpolation between two quaternions
quaternionSLerp ::
  -- | Lerp start value
  Quaternion ->
  -- | Lerp end value
  Quaternion ->
  -- | Lerp amount
  Float ->
  Quaternion
quaternionSLerp q1 q2 amount
  | cosHalfTheta >= 1 = q1
  | cosHalfTheta > 0.95 = quaternionNLerp q1 q2' amount
  | abs sinHalfTheta < 0.001 = (q1 |+| q2') |/ 2
  | otherwise = (q1 |* ratioA) |+| (q2 |* ratioB)
  where
    cosHalfTheta = if dot < 0 then - dot else dot
    sinHalfTheta = sqrt (1 - cosHalfTheta * cosHalfTheta)
    halfTheta = acos cosHalfTheta

    ratioA = sin ((1 - amount) * halfTheta) / sinHalfTheta
    ratioB = sin (amount * halfTheta) / sinHalfTheta

    q2' = if dot < 0 then additiveInverse q2 else q2
    dot = q1 |.| q2

-- | Quaternion based on the rotation between two vectors
quaternionFromVector3ToVector3 :: Vector3 -> Vector3 -> Quaternion
quaternionFromVector3ToVector3 from to = quaternionNormalize (Vector4 x y z (1 + cos2Theta))
  where
    cos2Theta = from |.| to
    (Vector3 x y z) = vector3CrossProduct from to

-- | Create a quaternion from a rotation matrix
quaternionFromMatrix :: Matrix -> Quaternion
quaternionFromMatrix mat
  | fourBiggestSquaredMinus1 == fourWSquaredMinus1 = Vector4 ((matrix'm6 mat - matrix'm9 mat) * mult) ((matrix'm8 mat - matrix'm2 mat) * mult) ((matrix'm1 mat - matrix'm4 mat) * mult) biggestVal
  | fourBiggestSquaredMinus1 == fourXSquaredMinus1 = Vector4 biggestVal ((matrix'm1 mat + matrix'm4 mat) * mult) ((matrix'm8 mat + matrix'm2 mat) * mult) ((matrix'm6 mat - matrix'm9 mat) * mult)
  | fourBiggestSquaredMinus1 == fourYSquaredMinus1 = Vector4 ((matrix'm1 mat + matrix'm4 mat) * mult) biggestVal ((matrix'm6 mat + matrix'm9 mat) * mult) ((matrix'm8 mat - matrix'm2 mat) * mult)
  | fourBiggestSquaredMinus1 == fourZSquaredMinus1 = Vector4 ((matrix'm8 mat + matrix'm2 mat) * mult) ((matrix'm6 mat + matrix'm9 mat) * mult) biggestVal ((matrix'm1 mat - matrix'm4 mat) * mult)
  | otherwise = error "(quaternionFromMatrix) This error should never happen"
  where
    fourWSquaredMinus1 = matrix'm0 mat + matrix'm5 mat + matrix'm10 mat
    fourXSquaredMinus1 = matrix'm0 mat - matrix'm5 mat - matrix'm10 mat
    fourYSquaredMinus1 = matrix'm5 mat - matrix'm0 mat - matrix'm10 mat
    fourZSquaredMinus1 = matrix'm10 mat - matrix'm0 mat - matrix'm5 mat
    fourBiggestSquaredMinus1 =
      maximum
        [ fourWSquaredMinus1,
          fourXSquaredMinus1,
          fourYSquaredMinus1,
          fourZSquaredMinus1
        ]
    biggestVal = sqrt (fourBiggestSquaredMinus1 + 1) / 2
    mult = 0.5 / biggestVal

-- | Create a rotation matrix from a quaternion
quaternionToMatrix :: Quaternion -> Matrix
quaternionToMatrix (Vector4 x y z w) = Matrix (1 - 2 * (b2 + c2)) (2 * (ab - cd)) (2 * (ac + bd)) 0 (2 * (ab + cd)) (1 - 2 * (a2 + c2)) (2 * (bc - ad)) 0 (2 * (ac - bd)) (2 * (bc + ad)) (1 - 2 * (a2 + b2)) 0 0 0 0 1
  where
    a2 = x * x
    b2 = y * y
    c2 = z * z
    ac = x * z
    ab = x * y
    bc = y * z
    ad = w * x
    bd = w * y
    cd = w * z

-- | Create a quaternion for an angle and axis
quaternionFromAxisAngle ::
  -- | Rotation axis
  Vector3 ->
  -- | Angle in radians
  Float ->
  Quaternion
quaternionFromAxisAngle axis angle = quaternionNormalize $ Vector4 (x * s) (y * s) (z * s) c -- I'm pretty sure normalizing the quaternion here is redundant
  where
    (Vector3 x y z) = vectorNormalize axis
    s = sin (angle / 2)
    c = cos (angle / 2)

-- | Convert a quaternion to axis-angle representation
quaternionToAxisAngle :: Quaternion -> (Vector3, Float)
quaternionToAxisAngle q = (axis, angle)
  where
    (Vector4 x y z w) = quaternionNormalize q
    s = sqrt (1 - w * w)
    axis = if w > 0.9999 then Vector3 1 0 0 else Vector3 (x / s) (y / s) (z / s)
    angle = acos w * 2

-- | Create a quaternion from Euler angles (ZYX rotation order, angles should be in radians)
quaternionFromEuler ::
  -- | Pitch
  Float ->
  -- | Yaw
  Float ->
  -- | Roll
  Float ->
  Quaternion
quaternionFromEuler pitch yaw roll = Vector4 (x1 * y0 * z0 - x0 * y1 * z1) (x0 * y1 * z0 + x1 * y0 * z1) (x0 * y0 * z1 - x1 * y1 * z0) (x0 * y0 * z0 + x1 * y1 * z1)
  where
    x0 = cos (pitch * 0.5)
    x1 = sin (pitch * 0.5)
    y0 = cos (yaw * 0.5)
    y1 = sin (yaw * 0.5)
    z0 = cos (roll * 0.5)
    z1 = sin (roll * 0.5)

-- | Convert a quaternion to Euler angle representation (Vector3 roll pitch yaw, all angles in radians)
quaternionToEuler :: Quaternion -> Vector3
quaternionToEuler (Vector4 x y z w) = Vector3 (atan2 x0 x1) (asin y0) (atan2 z0 z1)
  where
    x0 = 2 * (w * x + y * z)
    x1 = 1 - 2 * (x * x + y * y)
    y0 = clamp (2 * (w * y - z * x)) (-1) 1
    z0 = 2 * (w * z + x * y)
    z1 = 1 - 2 * (y * y + z * z)

-- | Transform a quaternion given a transformation matrix
quaternionTransform :: Quaternion -> Matrix -> Quaternion
quaternionTransform (Vector4 x y z w) mat =
  Vector4
    (matrix'm0 mat * x + matrix'm4 mat * y + matrix'm8 mat * z + matrix'm12 mat * w)
    (matrix'm1 mat * x + matrix'm5 mat * y + matrix'm9 mat * z + matrix'm13 mat * w)
    (matrix'm2 mat * x + matrix'm6 mat * y + matrix'm10 mat * z + matrix'm14 mat * w)
    (matrix'm3 mat * x + matrix'm7 mat * y + matrix'm11 mat * z + matrix'm15 mat * w)
