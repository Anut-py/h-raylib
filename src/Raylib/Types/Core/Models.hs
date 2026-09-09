{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Bindings for types used mainly in @rmodels@
module Raylib.Types.Core.Models
  ( -- * Enumerations
    MaterialMapIndex (..),
    DefaultShaderAttributeLocation (..),
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
    ModelAnimPose,
    BoneInfo (..),
    ModelSkeleton (..),
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
    p'mesh'boneCount,
    p'mesh'boneIndices,
    p'mesh'boneWeights,
    p'mesh'animVertices,
    p'mesh'animNormals,
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
    p'model'skeleton,
    p'model'currentPose,
    p'model'boneMatrices,
    p'modelAnimation'name,
    p'modelAnimation'boneCount,
    p'modelAnimation'keyframeCount,
    p'modelAnimation'keyframePoses,
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

import Control.Monad (forM_, unless, when)
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
import Raylib.Internal (Closeable (addToWindowResources, close), addShaderId, addTextureId, addVaoId, addVboIds, c'rlGetShaderIdDefault, c'rlGetShaderLocsDefault, c'rlUnloadShaderProgram, c'rlUnloadTexture, c'rlUnloadVertexArray, c'rlUnloadVertexBuffer)
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, freeMaybePtr, newMaybeArray, peekMaybeArray, peekStaticArray, pokeStaticArray, rightPad, rlFree, rlFreeMaybeArray)
import Raylib.Types.Core (Color, Matrix, Quaternion, Vector2, Vector3, Vector4, pattern Vector2, pattern Vector3, pattern Vector4)
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
  deriving (Eq, Show, Read, Enum)

data DefaultShaderAttributeLocation
  = DefaultShaderAttribLocationPosition
  | DefaultShaderAttribLocationTexcoord
  | DefaultShaderAttribLocationNormal
  | DefaultShaderAttribLocationColor
  | DefaultShaderAttribLocationTangent
  | DefaultShaderAttribLocationTexcoord2
  | DefaultShaderAttribLocationIndices
  | DefaultShaderAttribLocationBoneIds
  | DefaultShaderAttribLocationBoneWeights
  deriving (Eq, Show, Read, Enum)

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
  | ShaderLocVertexBoneIds
  | ShaderLocVertexBoneWeights
  | ShaderLocBoneWeights
  | ShaderLocVertexInstanceTransform
  deriving (Eq, Show, Read, Enum)

data ShaderUniformDataType
  = ShaderUniformFloatType
  | ShaderUniformVec2Type
  | ShaderUniformVec3Type
  | ShaderUniformVec4Type
  | ShaderUniformIntType
  | ShaderUniformIVec2Type
  | ShaderUniformIVec3Type
  | ShaderUniformIVec4Type
  | ShaderUniformUIntType
  | ShaderUniformUIVec2Type
  | ShaderUniformUIVec3Type
  | ShaderUniformUIVec4Type
  | ShaderUniformSampler2DType
  deriving (Eq, Show, Read, Enum)

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
  deriving (Eq, Show, Read)

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
  deriving (Eq, Show, Read)

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
  deriving (Eq, Show, Read, Enum)

---------------------------------------
-- models structures ------------------
---------------------------------------

data Mesh = Mesh
  { mesh'vertexCount :: Int,
    mesh'triangleCount :: Int,
    mesh'vertices :: [Vector3],
    mesh'texcoords :: Maybe [Vector2],
    mesh'texcoords2 :: Maybe [Vector2],
    mesh'normals :: [Vector3],
    mesh'tangents :: Maybe [Vector4],
    mesh'colors :: Maybe [Color],
    mesh'indices :: Maybe [Word16],
    mesh'boneCount :: Int,
    mesh'boneIndices :: Maybe [Word8],
    mesh'boneWeights :: Maybe [CFloat],
    mesh'animVertices :: Maybe [Vector3],
    mesh'animNormals :: Maybe [Vector3],
    mesh'vaoId :: Integer,
    -- | Use `toEnum` on `DefaultShaderAttributeLocation` for indices
    mesh'vboId :: Maybe [Integer]
  }
  deriving (Eq, Show, Read)

instance Storable Mesh where
  sizeOf _ = 120
  alignment _ = 8
  peek _p = do
    vertexCount <- fromIntegral <$> peek (p'mesh'vertexCount _p)
    triangleCount <- fromIntegral <$> peek (p'mesh'triangleCount _p)
    vertices <- peekArray vertexCount =<< peek (p'mesh'vertices _p)
    texcoords <- peekMaybeArray vertexCount =<< peek (p'mesh'texcoords _p)
    texcoords2 <- peekMaybeArray vertexCount =<< peek (p'mesh'texcoords2 _p)
    normals <- peekArray vertexCount =<< peek (p'mesh'normals _p)
    tangents <- peekMaybeArray vertexCount =<< peek (p'mesh'tangents _p)
    colors <- peekMaybeArray vertexCount =<< peek (p'mesh'colors _p)
    indices <- (map fromIntegral <$>) <$> (peekMaybeArray vertexCount =<< peek (p'mesh'indices _p))
    boneCount <- fromIntegral <$> peek (p'mesh'boneCount _p)
    boneIndices <- (map fromIntegral <$>) <$> (peekMaybeArray (4 * vertexCount) =<< peek (p'mesh'boneIndices _p))
    boneWeights <- (map realToFrac <$>) <$> (peekMaybeArray (4 * vertexCount) =<< peek (p'mesh'boneWeights _p))
    animVertices <- peekMaybeArray vertexCount =<< peek (p'mesh'animVertices _p)
    animNormals <- peekMaybeArray vertexCount =<< peek (p'mesh'animNormals _p)
    vaoId <- fromIntegral <$> peek (p'mesh'vaoId _p)
    vboId <- (map fromIntegral <$>) <$> (peekMaybeArray 9 =<< peek (p'mesh'vboId _p))
    return $ Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices boneCount boneIndices boneWeights animVertices animNormals vaoId vboId
  poke _p (Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices boneCount boneIndices boneWeights animVertices animNormals vaoId vboId) = do
    poke (p'mesh'vertexCount _p) (fromIntegral vertexCount)
    poke (p'mesh'triangleCount _p) (fromIntegral triangleCount)
    poke (p'mesh'vertices _p) =<< newArray vertices
    poke (p'mesh'texcoords _p) =<< newMaybeArray texcoords
    poke (p'mesh'texcoords2 _p) =<< newMaybeArray texcoords2
    poke (p'mesh'normals _p) =<< newArray normals
    poke (p'mesh'tangents _p) =<< newMaybeArray tangents
    poke (p'mesh'colors _p) =<< newMaybeArray colors
    poke (p'mesh'indices _p) =<< newMaybeArray (map fromIntegral <$> indices)
    poke (p'mesh'boneCount _p) (fromIntegral boneCount)
    poke (p'mesh'boneIndices _p) =<< newMaybeArray (map fromIntegral <$> boneIndices)
    poke (p'mesh'boneWeights _p) =<< newMaybeArray (map realToFrac <$> boneWeights)
    poke (p'mesh'animVertices _p) =<< newMaybeArray animVertices
    poke (p'mesh'animNormals _p) =<< newMaybeArray animNormals
    poke (p'mesh'vaoId _p) (fromIntegral vaoId)
    poke (p'mesh'vboId _p) =<< newMaybeArray (map fromIntegral <$> vboId)
    return ()

instance Closeable Mesh where
  close mesh = do
    c'rlUnloadVertexArray (fromIntegral (mesh'vaoId mesh))
    case mesh'vboId mesh of
      Nothing -> return ()
      Just vbos -> forM_ vbos (c'rlUnloadVertexBuffer . fromIntegral)
  addToWindowResources window mesh = do
    addVaoId (mesh'vaoId mesh) window
    addVboIds (mesh'vboId mesh) window

p'mesh'vertexCount :: Ptr Mesh -> Ptr CInt
p'mesh'vertexCount = (`plusPtr` 0)

p'mesh'triangleCount :: Ptr Mesh -> Ptr CInt
p'mesh'triangleCount = (`plusPtr` 4)

-- array (mesh'vertexCount)
p'mesh'vertices :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'vertices = (`plusPtr` 8)

-- maybe array (mesh'vertexCount)
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

p'mesh'boneCount :: Ptr Mesh -> Ptr CInt
p'mesh'boneCount = (`plusPtr` 64)

-- maybe array (4 * mesh'vertexCount)
p'mesh'boneIndices :: Ptr Mesh -> Ptr (Ptr CUChar)
p'mesh'boneIndices = (`plusPtr` 72)

-- maybe array (4 * mesh'vertexCount)
p'mesh'boneWeights :: Ptr Mesh -> Ptr (Ptr CFloat)
p'mesh'boneWeights = (`plusPtr` 80)

-- maybe array (mesh'vertexCount)
p'mesh'animVertices :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'animVertices = (`plusPtr` 88)

-- maybe array (mesh'vertexCount)
p'mesh'animNormals :: Ptr Mesh -> Ptr (Ptr Vector3)
p'mesh'animNormals = (`plusPtr` 96)

p'mesh'vaoId :: Ptr Mesh -> Ptr CUInt
p'mesh'vaoId = (`plusPtr` 104)

-- maybe array (9)
p'mesh'vboId :: Ptr Mesh -> Ptr (Ptr CUInt)
p'mesh'vboId = (`plusPtr` 112)

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
    boneIndicesPtr <- peek (p'mesh'boneIndices ptr)
    freeMaybePtr $ castPtr boneIndicesPtr
    boneWeightsPtr <- peek (p'mesh'boneWeights ptr)
    freeMaybePtr $ castPtr boneWeightsPtr
    freeMaybePtr $ castPtr indicesPtr
    animVerticesPtr <- peek (p'mesh'animVertices ptr)
    freeMaybePtr $ castPtr animVerticesPtr
    animNormalsPtr <- peek (p'mesh'animNormals ptr)
    freeMaybePtr $ castPtr animNormalsPtr
    vboIdPtr <- peek (p'mesh'vboId ptr)
    c'free $ castPtr vboIdPtr

data Shader = Shader
  { shader'id :: Integer,
    shader'locs :: [Int]
  }
  deriving (Eq, Show, Read)

instance Storable Shader where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    sId <- fromIntegral <$> peek (p'shader'id _p)
    locs <- map fromIntegral <$> (peekArray 32 =<< peek (p'shader'locs _p))
    return $ Shader sId locs
  poke _p (Shader sId locs) = do
    poke (p'shader'id _p) (fromIntegral sId)
    poke (p'shader'locs _p) =<< newArray (map fromIntegral locs)
    return ()

instance Closeable Shader where
  close shader = do
    shaderIdDefault <- c'rlGetShaderIdDefault
    unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId)
    where sId = fromIntegral (shader'id shader)
  addToWindowResources window shader = addShaderId (shader'id shader) window

p'shader'id :: Ptr Shader -> Ptr CUInt
p'shader'id = (`plusPtr` 0)

-- array (32)
p'shader'locs :: Ptr Shader -> Ptr (Ptr CInt)
p'shader'locs = (`plusPtr` 8)

instance Freeable Shader where
  rlFreeDependents _ ptr = do
    defaultShaderLocs <- c'rlGetShaderLocsDefault
    locsPtr <- peek (p'shader'locs ptr)
    unless
      (locsPtr == defaultShaderLocs)
      (c'free $ castPtr locsPtr)

data MaterialMap = MaterialMap
  { materialMap'texture :: Texture,
    materialMap'color :: Color,
    materialMap'value :: Float
  }
  deriving (Eq, Show, Read, Freeable)

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
  deriving (Eq, Show, Read)

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

instance Closeable Material where
  close mat = do
    let sId = fromIntegral (shader'id (material'shader mat))
    shaderIdDefault <- c'rlGetShaderIdDefault
    unless (sId == shaderIdDefault) (c'rlUnloadShaderProgram sId)
    case material'maps mat of
      Nothing -> return ()
      (Just maps) ->
        forM_
          maps
          ( \m ->
              let tId =
                    fromIntegral (texture'id (materialMap'texture m))
               in when (tId > 0) (c'rlUnloadTexture tId)
          )
  addToWindowResources window mat = do
    addShaderId (shader'id $ material'shader mat) window
    case material'maps mat of
      Nothing -> return ()
      (Just maps) -> forM_ maps (\m -> addTextureId (texture'id $ materialMap'texture m) window)

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
  deriving (Eq, Show, Read, Freeable)

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

type ModelAnimPose = [Transform]

data BoneInfo = BoneInfo
  { boneInfo'name :: String,
    boneInfo'parent :: Int
  }
  deriving (Eq, Show, Read, Freeable)

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

data ModelSkeleton = ModelSkeleton
  { modelSkeleton'boneCount :: Int,
    modelSkeleton'bones :: [BoneInfo],
    modelSkeleton'bindPose :: ModelAnimPose
  }
  deriving (Eq, Show, Read)

instance Storable ModelSkeleton where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    boneCount <- fromIntegral <$> peek (p'modelSkeleton'boneCount _p)
    bones <- peekArray boneCount =<< peek (p'modelSkeleton'bones _p)
    bindPose <- peekArray boneCount =<< peek (p'modelSkeleton'bindPose _p)
    return $ ModelSkeleton boneCount bones bindPose
  poke _p (ModelSkeleton boneCount bones bindPose) = do
    poke (p'modelSkeleton'boneCount _p) (fromIntegral boneCount)
    poke (p'modelSkeleton'bones _p) =<< newArray bones
    poke (p'modelSkeleton'bindPose _p) =<< newArray bindPose
    return ()

p'modelSkeleton'boneCount :: Ptr ModelSkeleton -> Ptr CUInt
p'modelSkeleton'boneCount = (`plusPtr` 0)

-- array (modelSkeleton'boneCount)
p'modelSkeleton'bones :: Ptr ModelSkeleton -> Ptr (Ptr BoneInfo)
p'modelSkeleton'bones = (`plusPtr` 8)

-- array (modelSkeleton'boneCount)
p'modelSkeleton'bindPose :: Ptr ModelSkeleton -> Ptr (Ptr Transform)
p'modelSkeleton'bindPose = (`plusPtr` 16)

instance Freeable ModelSkeleton where
  rlFreeDependents _ ptr = do
    (c'free . castPtr) =<< peek (p'modelSkeleton'bones ptr)
    (c'free . castPtr) =<< peek (p'modelSkeleton'bindPose ptr)

data Model = Model
  { model'transform :: Matrix,
    model'meshes :: [Mesh],
    model'materials :: [Material],
    model'meshMaterial :: [Int],
    model'skeleton :: ModelSkeleton,
    model'currentPose :: Maybe ModelAnimPose,
    model'boneMatrices :: Maybe [Matrix]
  }
  deriving (Eq, Show, Read)

instance Storable Model where
  sizeOf _ = 136
  alignment _ = 8
  peek _p = do
    transform <- peek (p'model'transform _p)
    meshCount <- fromIntegral <$> peek (p'model'meshCount _p)
    materialCount <- fromIntegral <$> peek (p'model'materialCount _p)
    meshes <- peekArray meshCount =<< peek (p'model'meshes _p)
    materials <- peekArray materialCount =<< peek (p'model'materials _p)
    meshMaterial <- map fromIntegral <$> (peekArray meshCount =<< peek (p'model'meshMaterial _p))
    skeleton <- peek (p'model'skeleton _p)
    let boneCount = modelSkeleton'boneCount skeleton
    currentPose <- peekMaybeArray boneCount =<< peek (p'model'currentPose _p)
    boneMatrices <- peekMaybeArray boneCount =<< peek (p'model'boneMatrices _p)
    return $ Model transform meshes materials meshMaterial skeleton currentPose boneMatrices
  poke _p (Model transform meshes materials meshMaterial skeleton currentPose boneMatrices) = do
    poke (p'model'transform _p) transform
    poke (p'model'meshCount _p) (fromIntegral (length meshes))
    poke (p'model'materialCount _p) (fromIntegral (length materials))
    poke (p'model'meshes _p) =<< newArray meshes
    poke (p'model'materials _p) =<< newArray materials
    poke (p'model'meshMaterial _p) =<< newArray (map fromIntegral meshMaterial)
    poke (p'model'skeleton _p) skeleton
    poke (p'model'currentPose _p) =<< newMaybeArray currentPose
    poke (p'model'boneMatrices _p) =<< newMaybeArray boneMatrices
    return ()

instance Closeable Model where
  close model = do
    forM_ (model'meshes model) close
    forM_ (model'materials model) close
  addToWindowResources window model = do
    forM_ (model'meshes model) (addToWindowResources window)
    forM_ (model'materials model) (addToWindowResources window)

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

p'model'skeleton :: Ptr Model -> Ptr ModelSkeleton
p'model'skeleton = (`plusPtr` 96)

-- maybe array (modelSkeleton'boneCount . model'skeleton)
p'model'currentPose :: Ptr Model -> Ptr (Ptr Transform)
p'model'currentPose = (`plusPtr` 120)

-- maybe array (modelSkeleton'boneCount . model'skeleton)
p'model'boneMatrices :: Ptr Model -> Ptr (Ptr Matrix)
p'model'boneMatrices = (`plusPtr` 128)

instance Freeable Model where
  rlFreeDependents val ptr = do
    rlFree (model'meshes val) . castPtr =<< peek (p'model'meshes ptr)
    rlFree (model'materials val) . castPtr =<< peek (p'model'materials ptr)
    c'free . castPtr =<< peek (p'model'meshMaterial ptr)
    rlFreeMaybeArray (model'currentPose val) =<< peek (p'model'currentPose ptr)
    rlFreeMaybeArray (model'boneMatrices val) =<< peek (p'model'boneMatrices ptr)
    rlFreeDependents (model'skeleton val) (p'model'skeleton ptr)

data ModelAnimation = ModelAnimation
  { modelAnimation'name :: String,
    modelAnimation'boneCount :: Int,
    modelAnimation'keyframeCount :: Int,
    modelAnimation'keyframePoses :: [ModelAnimPose]
  }
  deriving (Eq, Show, Read)

instance Storable ModelAnimation where
  sizeOf _ = 48
  alignment _ = 8
  peek _p = do
    name <- peekCString (p'modelAnimation'name _p)
    boneCount <- fromIntegral <$> peek (p'modelAnimation'boneCount _p)
    keyframeCount <- fromIntegral <$> peek (p'modelAnimation'keyframeCount _p)
    keyframePosesPtrs <- peekArray keyframeCount =<< peek (p'modelAnimation'keyframePoses _p)
    keyframePoses <- mapM (peekArray boneCount) keyframePosesPtrs
    return $ ModelAnimation name boneCount keyframeCount keyframePoses
  poke _p (ModelAnimation name boneCount keyframeCount keyframePoses) = do
    pokeStaticArray (p'modelAnimation'name _p) (rightPad 32 0 $ map castCharToCChar name)
    poke (p'modelAnimation'boneCount _p) (fromIntegral boneCount)
    poke (p'modelAnimation'keyframeCount _p) (fromIntegral keyframeCount)
    poke (p'modelAnimation'keyframePoses _p) =<< newArray =<< mapM newArray keyframePoses
    return ()

-- static string (32)
p'modelAnimation'name :: Ptr ModelAnimation -> Ptr CChar
p'modelAnimation'name = (`plusPtr` 0)

p'modelAnimation'boneCount :: Ptr ModelAnimation -> Ptr CUInt
p'modelAnimation'boneCount = (`plusPtr` 32)

p'modelAnimation'keyframeCount :: Ptr ModelAnimation -> Ptr CInt
p'modelAnimation'keyframeCount = (`plusPtr` 36)

-- array 2d (rows: modelAnimation'frameCount, cols: modelAnimation'boneCount)
p'modelAnimation'keyframePoses :: Ptr ModelAnimation -> Ptr (Ptr (Ptr Transform))
p'modelAnimation'keyframePoses = (`plusPtr` 40)

instance Freeable ModelAnimation where
  rlFreeDependents val ptr = do
    keyframePosesPtr <- peek (p'modelAnimation'keyframePoses ptr)
    keyframePosesPtrArr <- peekArray (modelAnimation'keyframeCount val) keyframePosesPtr
    forM_ keyframePosesPtrArr (c'free . castPtr)
    c'free $ castPtr keyframePosesPtr

data Ray = Ray
  { ray'position :: Vector3,
    ray'direction :: Vector3
  }
  deriving (Eq, Show, Read, Freeable)

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
  deriving (Eq, Show, Read, Freeable)

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
  deriving (Eq, Show, Read, Freeable)

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
