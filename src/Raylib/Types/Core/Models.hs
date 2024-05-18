{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Bindings for types used mainly in @rmodels@
module Raylib.Types.Core.Models
  ( -- * Enumerations
    MaterialMapIndex (..),
    ShaderLocationIndex (..),
    ShaderUniformDataType (..),
    ShaderUniformData (..),
    ShaderUniformDataV (..),
    unpackShaderUniformData,
    unpackShaderUniformDataV,
    ShaderAttributeDataType (..),

    -- * Structures
    Mesh (..),
    Shader (..),
    MaterialMap (..),
    Material (..),
    Transform (..),
    BoneInfo (..),
    Model (..),
    ModelAnimation (..),
    Ray (..),
    RayCollision (..),
    BoundingBox (..),

    -- * Pointer utilities
    p'mesh'vertexCount,
    p'mesh'triangleCount,
    p'mesh'vertices,
    p'mesh'texcoords,
    p'mesh'texcoords2,
    p'mesh'normals,
    p'mesh'tangents,
    p'mesh'colors,
    p'mesh'indices,
    p'mesh'animVertices,
    p'mesh'animNormals,
    p'mesh'boneIds,
    p'mesh'boneWeights,
    p'mesh'vaoId,
    p'mesh'vboId,
    p'shader'id,
    p'shader'locs,
    p'materialMap'texture,
    p'materialMap'color,
    p'materialMap'value,
    p'material'shader,
    p'material'maps,
    p'material'params,
    p'transform'translation,
    p'transform'rotation,
    p'transform'scale,
    p'boneInfo'name,
    p'boneInfo'parent,
    p'model'transform,
    p'model'meshCount,
    p'model'materialCount,
    p'model'meshes,
    p'model'materials,
    p'model'meshMaterial,
    p'model'boneCount,
    p'model'bones,
    p'model'bindPose,
    p'modelAnimation'boneCount,
    p'modelAnimation'frameCount,
    p'modelAnimation'bones,
    p'modelAnimation'framePoses,
    p'modelAnimation'name,
    p'ray'position,
    p'ray'direction,
    p'rayCollision'hit,
    p'rayCollision'distance,
    p'rayCollision'point,
    p'rayCollision'normal,
    p'boundingBox'min,
    p'boundingBox'max,
  )
where

import Control.Monad (forM_, unless)
import Foreign
  ( ForeignPtr,
    Ptr,
    Storable (alignment, peek, poke, sizeOf),
    Word16,
    Word8,
    castForeignPtr,
    castPtr,
    fromBool,
    mallocForeignPtr,
    mallocForeignPtrArray,
    newArray,
    newForeignPtr,
    peekArray,
    plusPtr,
    pokeArray,
    toBool,
    withForeignPtr,
  )
import Foreign.C
  ( CBool,
    CChar,
    CFloat,
    CInt (..),
    CUChar,
    CUInt,
    CUShort,
    castCharToCChar,
    peekCString,
  )
import Raylib.Internal (c'rlGetShaderIdDefault)
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, freeMaybePtr, newMaybeArray, p'free, peekMaybeArray, peekStaticArray, pokeStaticArray, rightPad, rlFree, rlFreeMaybeArray)
import Raylib.Types.Core (Color, Matrix, Quaternion, Vector2, pattern Vector2, Vector3, pattern Vector3, Vector4, pattern Vector4)
import Raylib.Types.Core.Textures (Texture (texture'id))

---------------------------------------
-- models enums -----------------------
---------------------------------------

data MaterialMapIndex
  = MaterialMapAlbedo
  | MaterialMapMetalness
  | MaterialMapNormal
  | MaterialMapRoughness
  | MaterialMapOcclusion
  | MaterialMapEmission
  | MaterialMapHeight
  | MaterialMapCubemap
  | MaterialMapIrradiance
  | MaterialMapPrefilter
  | MaterialMapBrdf
  deriving (Eq, Show, Enum)

data ShaderLocationIndex
  = ShaderLocVertexPosition
  | ShaderLocVertexTexcoord01
  | ShaderLocVertexTexcoord02
  | ShaderLocVertexNormal
  | ShaderLocVertexTangent
  | ShaderLocVertexColor
  | ShaderLocMatrixMvp
  | ShaderLocMatrixView
  | ShaderLocMatrixProjection
  | ShaderLocMatrixModel
  | ShaderLocMatrixNormal
  | ShaderLocVectorView
  | ShaderLocColorDiffuse
  | ShaderLocColorSpecular
  | ShaderLocColorAmbient
  | ShaderLocMapAlbedo
  | ShaderLocMapMetalness
  | ShaderLocMapNormal
  | ShaderLocMapRoughness
  | ShaderLocMapOcclusion
  | ShaderLocMapEmission
  | ShaderLocMapHeight
  | ShaderLocMapCubemap
  | ShaderLocMapIrradiance
  | ShaderLocMapPrefilter
  | ShaderLocMapBrdf
  deriving (Eq, Show, Enum)

data ShaderUniformDataType
  = ShaderUniformFloatType
  | ShaderUniformVec2Type
  | ShaderUniformVec3Type
  | ShaderUniformVec4Type
  | ShaderUniformIntType
  | ShaderUniformIVec2Type
  | ShaderUniformIVec3Type
  | ShaderUniformIVec4Type
  | ShaderUniformSampler2DType
  deriving (Eq, Show, Enum)

data ShaderUniformData
  = ShaderUniformFloat Float
  | ShaderUniformVec2 Vector2
  | ShaderUniformVec3 Vector3
  | ShaderUniformVec4 Vector4
  | ShaderUniformInt Int
  | ShaderUniformIVec2 (Int, Int)
  | ShaderUniformIVec3 (Int, Int, Int)
  | ShaderUniformIVec4 (Int, Int, Int, Int)
  | ShaderUniformSampler2D Texture
  deriving (Eq, Show)

data ShaderUniformDataV
  = ShaderUniformFloatV [Float]
  | ShaderUniformVec2V [Vector2]
  | ShaderUniformVec3V [Vector3]
  | ShaderUniformVec4V [Vector4]
  | ShaderUniformIntV [Int]
  | ShaderUniformIVec2V [(Int, Int)]
  | ShaderUniformIVec3V [(Int, Int, Int)]
  | ShaderUniformIVec4V [(Int, Int, Int, Int)]
  | ShaderUniformSampler2DV [Texture]
  deriving (Eq, Show)

-- | Internal use
unpackShaderUniformData :: ShaderUniformData -> IO (ShaderUniformDataType, ForeignPtr ())
unpackShaderUniformData u = do
  case u of
    (ShaderUniformFloat f) ->
      do
        fptr <- mallocForeignPtr
        withForeignPtr fptr (\ptr -> poke ptr (realToFrac f :: CFloat))
        return (ShaderUniformFloatType, castForeignPtr fptr)
    (ShaderUniformVec2 (Vector2 x y)) ->
      do
        fptr <- mallocForeignPtrArray 2
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac [x, y] :: [CFloat]))
        return (ShaderUniformVec2Type, castForeignPtr fptr)
    (ShaderUniformVec3 (Vector3 x y z)) ->
      do
        fptr <- mallocForeignPtrArray 3
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac [x, y, z] :: [CFloat]))
        return (ShaderUniformVec3Type, castForeignPtr fptr)
    (ShaderUniformVec4 (Vector4 x y z w)) ->
      do
        fptr <- mallocForeignPtrArray 3
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac [x, y, z, w] :: [CFloat]))
        return (ShaderUniformVec4Type, castForeignPtr fptr)
    (ShaderUniformInt i) ->
      do
        fptr <- mallocForeignPtr
        withForeignPtr fptr (\ptr -> poke ptr (fromIntegral i :: CInt))
        return (ShaderUniformIntType, castForeignPtr fptr)
    (ShaderUniformIVec2 (i1, i2)) ->
      do
        fptr <- mallocForeignPtrArray 2
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral [i1, i2] :: [CInt]))
        return (ShaderUniformIVec2Type, castForeignPtr fptr)
    (ShaderUniformIVec3 (i1, i2, i3)) ->
      do
        fptr <- mallocForeignPtrArray 3
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral [i1, i2, i3] :: [CInt]))
        return (ShaderUniformIVec3Type, castForeignPtr fptr)
    (ShaderUniformIVec4 (i1, i2, i3, i4)) ->
      do
        fptr <- mallocForeignPtrArray 4
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral [i1, i2, i3, i4] :: [CInt]))
        return (ShaderUniformIVec4Type, castForeignPtr fptr)
    (ShaderUniformSampler2D texture) ->
      do
        fptr <- mallocForeignPtr
        withForeignPtr fptr (\ptr -> poke ptr (fromIntegral $ texture'id texture :: CInt))
        return (ShaderUniformSampler2DType, castForeignPtr fptr)

-- | Internal use
unpackShaderUniformDataV :: ShaderUniformDataV -> IO (ShaderUniformDataType, ForeignPtr (), Int)
unpackShaderUniformDataV xs = do
  case xs of
    (ShaderUniformFloatV fs) ->
      do
        fptr <- mallocForeignPtrArray (length fs)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac fs :: [CFloat]))
        return (ShaderUniformFloatType, castForeignPtr fptr, length fs)
    (ShaderUniformVec2V vs) ->
      do
        fptr <- mallocForeignPtrArray (2 * length vs)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac $ concatMap (\(Vector2 x y) -> [x, y]) vs :: [CFloat]))
        return (ShaderUniformVec2Type, castForeignPtr fptr, length vs)
    (ShaderUniformVec3V vs) ->
      do
        fptr <- mallocForeignPtrArray (3 * length vs)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac $ concatMap (\(Vector3 x y z) -> [x, y, z]) vs :: [CFloat]))
        return (ShaderUniformVec3Type, castForeignPtr fptr, length vs)
    (ShaderUniformVec4V vs) ->
      do
        fptr <- mallocForeignPtrArray (4 * length vs)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map realToFrac $ concatMap (\(Vector4 x y z w) -> [x, y, z, w]) vs :: [CFloat]))
        return (ShaderUniformVec4Type, castForeignPtr fptr, length vs)
    (ShaderUniformIntV is) ->
      do
        fptr <- mallocForeignPtrArray (length is)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral is :: [CInt]))
        return (ShaderUniformFloatType, castForeignPtr fptr, length is)
    (ShaderUniformIVec2V is) ->
      do
        fptr <- mallocForeignPtrArray (2 * length is)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral $ concatMap (\(x, y) -> [x, y]) is :: [CInt]))
        return (ShaderUniformIVec2Type, castForeignPtr fptr, length is)
    (ShaderUniformIVec3V is) ->
      do
        fptr <- mallocForeignPtrArray (3 * length is)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral $ concatMap (\(x, y, z) -> [x, y, z]) is :: [CInt]))
        return (ShaderUniformIVec3Type, castForeignPtr fptr, length is)
    (ShaderUniformIVec4V is) ->
      do
        fptr <- mallocForeignPtrArray (4 * length is)
        withForeignPtr fptr (\ptr -> pokeArray ptr (map fromIntegral $ concatMap (\(x, y, z, w) -> [x, y, z, w]) is :: [CInt]))
        return (ShaderUniformIVec4Type, castForeignPtr fptr, length is)
    (ShaderUniformSampler2DV textures) ->
      do
        fptr <- mallocForeignPtr
        withForeignPtr fptr (\ptr -> pokeArray ptr (map (fromIntegral . texture'id) textures :: [CInt]))
        return (ShaderUniformSampler2DType, castForeignPtr fptr, length textures)

-- Unused
data ShaderAttributeDataType
  = ShaderAttribFloat
  | ShaderAttribVec2
  | ShaderAttribVec3
  | ShaderAttribVec4
  deriving (Eq, Show, Enum)

---------------------------------------
-- models structures ------------------
---------------------------------------

data Mesh = Mesh
  { mesh'vertexCount :: Int,
    mesh'triangleCount :: Int,
    mesh'vertices :: [Vector3],
    mesh'texcoords :: [Vector2],
    mesh'texcoords2 :: Maybe [Vector2],
    mesh'normals :: [Vector3],
    mesh'tangents :: Maybe [Vector4],
    mesh'colors :: Maybe [Color],
    mesh'indices :: Maybe [Word16],
    mesh'animVertices :: Maybe [Vector3],
    mesh'animNormals :: Maybe [Vector3],
    mesh'boneIds :: Maybe [Word8],
    mesh'boneWeights :: Maybe [Float],
    mesh'vaoId :: Integer,
    mesh'vboId :: Maybe [Integer]
  }
  deriving (Eq, Show)

instance Storable Mesh where
  sizeOf _ = 112
  alignment _ = 8
  peek _p = do
    vertexCount <- fromIntegral <$> peek (p'mesh'vertexCount _p)
    triangleCount <- fromIntegral <$> peek (p'mesh'triangleCount _p)
    vertices <- peekArray vertexCount =<< peek (p'mesh'vertices _p)
    texcoords <- peekArray vertexCount =<< peek (p'mesh'texcoords _p)
    texcoords2 <- peekMaybeArray vertexCount =<< peek (p'mesh'texcoords2 _p)
    normals <- peekArray vertexCount =<< peek (p'mesh'normals _p)
    tangents <- peekMaybeArray vertexCount =<< peek (p'mesh'tangents _p)
    colors <- peekMaybeArray vertexCount =<< peek (p'mesh'colors _p)
    indices <- (map fromIntegral <$>) <$> (peekMaybeArray vertexCount =<< peek (p'mesh'indices _p))
    animVertices <- peekMaybeArray vertexCount =<< peek (p'mesh'animVertices _p)
    animNormals <- peekMaybeArray vertexCount =<< peek (p'mesh'animNormals _p)
    boneIds <- (map fromIntegral <$>) <$> (peekMaybeArray (vertexCount * 4) =<< peek (p'mesh'boneIds _p))
    boneWeights <- (map realToFrac <$>) <$> (peekMaybeArray (vertexCount * 4) =<< peek (p'mesh'boneWeights _p))
    vaoId <- fromIntegral <$> peek (p'mesh'vaoId _p)
    vboId <- (map fromIntegral <$>) <$> (peekMaybeArray 7 =<< peek (p'mesh'vboId _p))
    return $ Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices animVertices animNormals boneIds boneWeights vaoId vboId
  poke _p (Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices animVertices animNormals boneIds boneWeights vaoId vboId) = do
    poke (p'mesh'vertexCount _p) (fromIntegral vertexCount)
    poke (p'mesh'triangleCount _p) (fromIntegral triangleCount)
    poke (p'mesh'vertices _p) =<< newArray vertices
    poke (p'mesh'texcoords _p) =<< newArray texcoords
    poke (p'mesh'texcoords2 _p) =<< newMaybeArray texcoords2
    poke (p'mesh'normals _p) =<< newArray normals
    poke (p'mesh'tangents _p) =<< newMaybeArray tangents
    poke (p'mesh'colors _p) =<< newMaybeArray colors
    poke (p'mesh'indices _p) =<< newMaybeArray (map fromIntegral <$> indices)
    poke (p'mesh'animVertices _p) =<< newMaybeArray animVertices
    poke (p'mesh'animNormals _p) =<< newMaybeArray animNormals
    poke (p'mesh'boneIds _p) =<< newMaybeArray (map fromIntegral <$> boneIds)
    poke (p'mesh'boneWeights _p) =<< newMaybeArray (map realToFrac <$> boneWeights)
    poke (p'mesh'vaoId _p) (fromIntegral vaoId)
    poke (p'mesh'vboId _p) =<< newMaybeArray (map fromIntegral <$> vboId)
    return ()

p'mesh'vertexCount :: Ptr Mesh -> Ptr CInt
p'mesh'vertexCount = (`plusPtr` 0)

p'mesh'triangleCount :: Ptr Mesh -> Ptr CInt
p'mesh'triangleCount = (`plusPtr` 4)

-- array (mesh'vertexCount)
p'mesh'vertices :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'vertices = (`plusPtr` 8)

-- array (mesh'vertexCount)
p'mesh'texcoords :: Ptr Mesh -> Ptr (Ptr Vector2)
p'mesh'texcoords = (`plusPtr` 16)

-- maybe array (mesh'vertexCount)
p'mesh'texcoords2 :: Ptr Mesh -> Ptr (Ptr Vector2)
p'mesh'texcoords2 = (`plusPtr` 24)

-- array (mesh'vertexCount)
p'mesh'normals :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'normals = (`plusPtr` 32)

-- maybe array (mesh'vertexCount)
p'mesh'tangents :: Ptr Mesh -> Ptr (Ptr Vector4)
p'mesh'tangents = (`plusPtr` 40)

-- maybe array (mesh'vertexCount)
p'mesh'colors :: Ptr Mesh -> Ptr (Ptr Color)
p'mesh'colors = (`plusPtr` 48)

-- maybe array (mesh'vertexCount)
p'mesh'indices :: Ptr Mesh -> Ptr (Ptr CUShort)
p'mesh'indices = (`plusPtr` 56)

-- maybe array (mesh'vertexCount)
p'mesh'animVertices :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'animVertices = (`plusPtr` 64)

-- maybe array (mesh'vertexCount)
p'mesh'animNormals :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'animNormals = (`plusPtr` 72)

-- maybe array (mesh'vertexCount * 4)
p'mesh'boneIds :: Ptr Mesh -> Ptr (Ptr CUChar)
p'mesh'boneIds = (`plusPtr` 80)

-- maybe array (mesh'vertexCount * 4)
p'mesh'boneWeights :: Ptr Mesh -> Ptr (Ptr CFloat)
p'mesh'boneWeights = (`plusPtr` 88)

p'mesh'vaoId :: Ptr Mesh -> Ptr CUInt
p'mesh'vaoId = (`plusPtr` 96)

-- maybe array (7)
p'mesh'vboId :: Ptr Mesh -> Ptr (Ptr CUInt)
p'mesh'vboId = (`plusPtr` 104)

instance Freeable Mesh where
  rlFreeDependents _ ptr = do
    verticesPtr <- peek (p'mesh'vertices ptr)
    c'free $ castPtr verticesPtr
    texcoordsPtr <- peek (p'mesh'texcoords ptr)
    c'free $ castPtr texcoordsPtr
    texcoords2Ptr <- peek (p'mesh'texcoords2 ptr)
    freeMaybePtr $ castPtr texcoords2Ptr
    normalsPtr <- peek (p'mesh'normals ptr)
    c'free $ castPtr normalsPtr
    tangentsPtr <- peek (p'mesh'tangents ptr)
    freeMaybePtr $ castPtr tangentsPtr
    colorsPtr <- peek (p'mesh'colors ptr)
    freeMaybePtr $ castPtr colorsPtr
    indicesPtr <- peek (p'mesh'indices ptr)
    freeMaybePtr $ castPtr indicesPtr
    animVerticesPtr <- peek (p'mesh'animVertices ptr)
    freeMaybePtr $ castPtr animVerticesPtr
    animNormalsPtr <- peek (p'mesh'animNormals ptr)
    freeMaybePtr $ castPtr animNormalsPtr
    boneIdsPtr <- peek (p'mesh'boneIds ptr)
    freeMaybePtr $ castPtr boneIdsPtr
    boneWeightsPtr <- peek (p'mesh'boneWeights ptr)
    freeMaybePtr $ castPtr boneWeightsPtr
    vboIdPtr <- peek (p'mesh'vboId ptr)
    c'free $ castPtr vboIdPtr

data Shader = Shader
  { shader'id :: Integer,
    shader'locs :: [Int]
  }
  deriving (Eq, Show)

instance Storable Shader where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    sId <- fromIntegral <$> peek (p'shader'id _p)
    locs <- map fromIntegral <$> (peekArray 32 =<< peek (p'shader'locs _p))
    return $ Shader sId locs
  poke _p (Shader sId locs) = do
    poke (p'shader'id _p) (fromIntegral sId)
    defaultShaderId <- c'rlGetShaderIdDefault
    locsArr <- newArray (map fromIntegral locs)
    if sId == fromIntegral defaultShaderId
      then do
        locsPtr <- newForeignPtr p'free locsArr
        withForeignPtr locsPtr $ poke (p'shader'locs _p)
      else poke (p'shader'locs _p) locsArr
    return ()

p'shader'id :: Ptr Shader -> Ptr CUInt
p'shader'id = (`plusPtr` 0)

-- array (32)
p'shader'locs :: Ptr Shader -> Ptr (Ptr CInt)
p'shader'locs = (`plusPtr` 8)

instance Freeable Shader where
  rlFreeDependents val ptr = do
    defaultShaderId <- c'rlGetShaderIdDefault
    unless
      (shader'id val == fromIntegral defaultShaderId)
      ( do
          locsPtr <- peek (p'shader'locs ptr)
          c'free $ castPtr locsPtr
      )

data MaterialMap = MaterialMap
  { materialMap'texture :: Texture,
    materialMap'color :: Color,
    materialMap'value :: Float
  }
  deriving (Eq, Show, Freeable)

instance Storable MaterialMap where
  sizeOf _ = 28
  alignment _ = 4
  peek _p = do
    texture <- peek (p'materialMap'texture _p)
    color <- peek (p'materialMap'color _p)
    value <- realToFrac <$> peek (p'materialMap'value _p)
    return $ MaterialMap texture color value
  poke _p (MaterialMap texture color value) = do
    poke (p'materialMap'texture _p) texture
    poke (p'materialMap'color _p) color
    poke (p'materialMap'value _p) (realToFrac value)
    return ()

p'materialMap'texture :: Ptr MaterialMap -> Ptr Texture
p'materialMap'texture = (`plusPtr` 0)

p'materialMap'color :: Ptr MaterialMap -> Ptr Color
p'materialMap'color = (`plusPtr` 20)

p'materialMap'value :: Ptr MaterialMap -> Ptr CFloat
p'materialMap'value = (`plusPtr` 24)

data Material = Material
  { material'shader :: Shader,
    material'maps :: Maybe [MaterialMap],
    material'params :: [Float]
  }
  deriving (Eq, Show)

instance Storable Material where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    shader <- peek (p'material'shader _p)
    maps <- peekMaybeArray 12 =<< peek (p'material'maps _p)
    params <- map realToFrac <$> peekStaticArray 4 (p'material'params _p)
    return $ Material shader maps params
  poke _p (Material shader maps params) = do
    poke (p'material'shader _p) shader
    poke (p'material'maps _p) =<< newMaybeArray maps
    pokeStaticArray (p'material'params _p) (map realToFrac params)
    return ()

p'material'shader :: Ptr Material -> Ptr Shader
p'material'shader = (`plusPtr` 0)

-- maybe array (12)
p'material'maps :: Ptr Material -> Ptr (Ptr MaterialMap)
p'material'maps = (`plusPtr` 16)

-- static array (4)
p'material'params :: Ptr Material -> Ptr CFloat
p'material'params = (`plusPtr` 24)

instance Freeable Material where
  rlFreeDependents val ptr = do
    rlFreeDependents (material'shader val) (castPtr ptr :: Ptr Shader)
    rlFreeMaybeArray (material'maps val) =<< peek (p'material'maps ptr)

data Transform = Transform
  { transform'translation :: Vector3,
    transform'rotation :: Quaternion,
    transform'scale :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable Transform where
  sizeOf _ = 40
  alignment _ = 4
  peek _p = do
    translation <- peek (p'transform'translation _p)
    rotation <- peek (p'transform'rotation _p)
    scale <- peek (p'transform'scale _p)
    return $ Transform translation rotation scale
  poke _p (Transform translation rotation scale) = do
    poke (p'transform'translation _p) translation
    poke (p'transform'rotation _p) rotation
    poke (p'transform'scale _p) scale
    return ()

p'transform'translation :: Ptr Transform -> Ptr Vector3
p'transform'translation = (`plusPtr` 0)

p'transform'rotation :: Ptr Transform -> Ptr Quaternion
p'transform'rotation = (`plusPtr` 12)

p'transform'scale :: Ptr Transform -> Ptr Vector3
p'transform'scale = (`plusPtr` 28)

data BoneInfo = BoneInfo
  { boneInfo'name :: String,
    boneInfo'parent :: Int
  }
  deriving (Eq, Show, Freeable)

instance Storable BoneInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    name <- peekCString (p'boneInfo'name _p)
    parent <- fromIntegral <$> peek (p'boneInfo'parent _p)
    return $ BoneInfo name parent
  poke _p (BoneInfo name parent) = do
    pokeStaticArray (p'boneInfo'name _p) (rightPad 32 0 $ map castCharToCChar name)
    poke (p'boneInfo'parent _p) (fromIntegral parent)
    return ()

-- static string (32)
p'boneInfo'name :: Ptr BoneInfo -> Ptr CChar
p'boneInfo'name = (`plusPtr` 0)

p'boneInfo'parent :: Ptr BoneInfo -> Ptr CInt
p'boneInfo'parent = (`plusPtr` 32)

data Model = Model
  { model'transform :: Matrix,
    model'meshes :: [Mesh],
    model'materials :: [Material],
    model'meshMaterial :: [Int],
    model'boneCount :: Int,
    model'bones :: Maybe [BoneInfo],
    model'bindPose :: Maybe [Transform]
  }
  deriving (Eq, Show)

instance Storable Model where
  sizeOf _ = 120
  alignment _ = 4
  peek _p = do
    transform <- peek (p'model'transform _p)
    meshCount <- fromIntegral <$> peek (p'model'meshCount _p)
    materialCount <- fromIntegral <$> peek (p'model'materialCount _p)
    meshes <- peekArray meshCount =<< peek (p'model'meshes _p)
    materials <- peekArray materialCount =<< peek (p'model'materials _p)
    meshMaterial <- map fromIntegral <$> (peekArray meshCount =<< peek (p'model'meshMaterial _p))
    boneCount <- fromIntegral <$> peek (p'model'boneCount _p)
    bones <- peekMaybeArray boneCount =<< peek (p'model'bones _p)
    bindPose <- peekMaybeArray boneCount =<< peek (p'model'bindPose _p)
    return $ Model transform meshes materials meshMaterial boneCount bones bindPose
  poke _p (Model transform meshes materials meshMaterial boneCount bones bindPose) = do
    poke (p'model'transform _p) transform
    poke (p'model'meshCount _p) (fromIntegral (length meshes))
    poke (p'model'materialCount _p) (fromIntegral (length materials))
    poke (p'model'meshes _p) =<< newArray meshes
    poke (p'model'materials _p) =<< newArray materials
    poke (p'model'meshMaterial _p) =<< newArray (map fromIntegral meshMaterial)
    poke (p'model'boneCount _p) (fromIntegral boneCount)
    poke (p'model'bones _p) =<< newMaybeArray bones
    poke (p'model'bindPose _p) =<< newMaybeArray bindPose
    return ()

p'model'transform :: Ptr Model -> Ptr Matrix
p'model'transform = (`plusPtr` 0)

p'model'meshCount :: Ptr Model -> Ptr CInt
p'model'meshCount = (`plusPtr` 64)

p'model'materialCount :: Ptr Model -> Ptr CInt
p'model'materialCount = (`plusPtr` 68)

-- array (model'meshCount)
p'model'meshes :: Ptr Model -> Ptr (Ptr Mesh)
p'model'meshes = (`plusPtr` 72)

-- array (model'materialCount)
p'model'materials :: Ptr Model -> Ptr (Ptr Material)
p'model'materials = (`plusPtr` 80)

-- array (model'meshCount)
p'model'meshMaterial :: Ptr Model -> Ptr (Ptr CInt)
p'model'meshMaterial = (`plusPtr` 88)

p'model'boneCount :: Ptr Model -> Ptr CInt
p'model'boneCount = (`plusPtr` 96)

-- maybe array (model'boneCount)
p'model'bones :: Ptr Model -> Ptr (Ptr BoneInfo)
p'model'bones = (`plusPtr` 104)

-- maybe array (model'boneCount)
p'model'bindPose :: Ptr Model -> Ptr (Ptr Transform)
p'model'bindPose = (`plusPtr` 112)

instance Freeable Model where
  rlFreeDependents val ptr = do
    rlFree (model'meshes val) . castPtr =<< peek (p'model'meshes ptr)
    rlFree (model'materials val) . castPtr =<< peek (p'model'materials ptr)
    c'free . castPtr =<< peek (p'model'meshMaterial ptr)
    freeMaybePtr . castPtr =<< peek (p'model'bones ptr)
    freeMaybePtr . castPtr =<< peek (p'model'bindPose ptr)

data ModelAnimation = ModelAnimation
  { modelAnimation'boneCount :: Int,
    modelAnimation'frameCount :: Int,
    modelAnimation'bones :: [BoneInfo],
    modelAnimation'framePoses :: [[Transform]],
    modelAnimation'name :: String
  }
  deriving (Eq, Show)

instance Storable ModelAnimation where
  sizeOf _ = 56
  alignment _ = 4
  peek _p = do
    boneCount <- fromIntegral <$> peek (p'modelAnimation'boneCount _p)
    frameCount <- fromIntegral <$> peek (p'modelAnimation'frameCount _p)
    bones <- peekArray boneCount =<< peek (p'modelAnimation'bones _p)
    framePosesPtr <- peek (p'modelAnimation'framePoses _p)
    framePosesPtrArr <- peekArray frameCount framePosesPtr
    framePoses <- mapM (peekArray boneCount) framePosesPtrArr
    name <- peekCString (p'modelAnimation'name _p)
    return $ ModelAnimation boneCount frameCount bones framePoses name
  poke _p (ModelAnimation boneCount frameCount bones framePoses name) = do
    poke (p'modelAnimation'boneCount _p) (fromIntegral boneCount)
    poke (p'modelAnimation'frameCount _p) (fromIntegral frameCount)
    poke (p'modelAnimation'bones _p) =<< newArray bones
    poke (p'modelAnimation'framePoses _p) =<< newArray =<< mapM newArray framePoses
    pokeStaticArray (p'modelAnimation'name _p) (rightPad 32 0 $ map castCharToCChar name)
    return ()

p'modelAnimation'boneCount :: Ptr ModelAnimation -> Ptr CInt
p'modelAnimation'boneCount = (`plusPtr` 0)

p'modelAnimation'frameCount :: Ptr ModelAnimation -> Ptr CInt
p'modelAnimation'frameCount = (`plusPtr` 4)

-- array (modelAnimation'boneCount)
p'modelAnimation'bones :: Ptr ModelAnimation -> Ptr (Ptr BoneInfo)
p'modelAnimation'bones = (`plusPtr` 8)

-- array 2d (rows: modelAnimation'frameCount, cols: modelAnimation'boneCount)
p'modelAnimation'framePoses :: Ptr ModelAnimation -> Ptr (Ptr (Ptr Transform))
p'modelAnimation'framePoses = (`plusPtr` 16)

-- static string (32)
p'modelAnimation'name :: Ptr ModelAnimation -> Ptr CChar
p'modelAnimation'name = (`plusPtr` 24)

instance Freeable ModelAnimation where
  rlFreeDependents val ptr = do
    c'free . castPtr =<< peek (p'modelAnimation'bones ptr)
    framePosesPtr <- peek (p'modelAnimation'framePoses ptr)
    framePosesPtrArr <- peekArray (modelAnimation'frameCount val) framePosesPtr
    forM_ framePosesPtrArr (c'free . castPtr)
    c'free $ castPtr framePosesPtr

data Ray = Ray
  { ray'position :: Vector3,
    ray'direction :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable Ray where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    position <- peek (p'ray'position _p)
    direction <- peek (p'ray'direction _p)
    return $ Ray position direction
  poke _p (Ray position direction) = do
    poke (p'ray'position _p) position
    poke (p'ray'direction _p) direction
    return ()

p'ray'position :: Ptr Ray -> Ptr Vector3
p'ray'position = (`plusPtr` 0)

p'ray'direction :: Ptr Ray -> Ptr Vector3
p'ray'direction = (`plusPtr` 12)

data RayCollision = RayCollision
  { rayCollision'hit :: Bool,
    rayCollision'distance :: Float,
    rayCollision'point :: Vector3,
    rayCollision'normal :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable RayCollision where
  sizeOf _ = 32
  alignment _ = 4
  peek _p = do
    hit <- toBool <$> peek (p'rayCollision'hit _p)
    distance <- realToFrac <$> peek (p'rayCollision'distance _p)
    point <- peek (p'rayCollision'point _p)
    normal <- peek (p'rayCollision'normal _p)
    return $ RayCollision hit distance point normal
  poke _p (RayCollision hit distance point normal) = do
    poke (p'rayCollision'hit _p) (fromBool hit)
    poke (p'rayCollision'distance _p) (realToFrac distance)
    poke (p'rayCollision'point _p) point
    poke (p'rayCollision'normal _p) normal
    return ()

p'rayCollision'hit :: Ptr RayCollision -> Ptr CBool
p'rayCollision'hit = (`plusPtr` 0)

p'rayCollision'distance :: Ptr RayCollision -> Ptr CFloat
p'rayCollision'distance = (`plusPtr` 4)

p'rayCollision'point :: Ptr RayCollision -> Ptr Vector3
p'rayCollision'point = (`plusPtr` 8)

p'rayCollision'normal :: Ptr RayCollision -> Ptr Vector3
p'rayCollision'normal = (`plusPtr` 20)

data BoundingBox = BoundingBox
  { boundingBox'min :: Vector3,
    boundingBox'max :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable BoundingBox where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    bMin <- peek (p'boundingBox'min _p)
    bMax <- peek (p'boundingBox'max _p)
    return $ BoundingBox bMin bMax
  poke _p (BoundingBox bMin bMax) = do
    poke (p'boundingBox'min _p) bMin
    poke (p'boundingBox'max _p) bMax
    return ()

p'boundingBox'min :: Ptr BoundingBox -> Ptr Vector3
p'boundingBox'min = (`plusPtr` 0)

p'boundingBox'max :: Ptr BoundingBox -> Ptr Vector3
p'boundingBox'max = (`plusPtr` 12)
