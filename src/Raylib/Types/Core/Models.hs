{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}

module Raylib.Types.Core.Models
  ( MaterialMapIndex (..),
    ShaderLocationIndex (..),
    ShaderUniformDataType (..),
    ShaderUniformData (..),
    ShaderUniformDataV (..),
    unpackShaderUniformData,
    unpackShaderUniformDataV,
    ShaderAttributeDataType (..),
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
  )
where

import Control.Monad (forM_, unless)
import Foreign
  ( Ptr,
    Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    Word16,
    Word8,
    castPtr,
    fromBool,
    malloc,
    newArray,
    newForeignPtr,
    peekArray,
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
  )
import Foreign.C.String (castCCharToChar)
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, freeMaybePtr, newMaybeArray, p'free, peekMaybeArray, peekStaticArray, peekStaticArrayOff, pokeStaticArray, pokeStaticArrayOff, rightPad, rlFreeArray, rlFreeMaybeArray)
import Raylib.Internal (c'rlGetShaderIdDefault)
import Raylib.Types.Core (Color, Matrix, Quaternion, Vector2 (Vector2), Vector3 (Vector3), Vector4 (Vector4))
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

unpackShaderUniformData :: ShaderUniformData -> IO (ShaderUniformDataType, Ptr ())
unpackShaderUniformData u = do
  case u of
    (ShaderUniformFloat f) ->
      do
        ptr <- malloc
        poke ptr (realToFrac f :: CFloat)
        return (ShaderUniformFloatType, castPtr ptr)
    (ShaderUniformVec2 (Vector2 x y)) ->
      do
        ptr <- newArray (map realToFrac [x, y] :: [CFloat])
        return (ShaderUniformVec2Type, castPtr ptr)
    (ShaderUniformVec3 (Vector3 x y z)) ->
      do
        ptr <- newArray (map realToFrac [x, y, z] :: [CFloat])
        return (ShaderUniformVec3Type, castPtr ptr)
    (ShaderUniformVec4 (Vector4 x y z w)) ->
      do
        ptr <- newArray (map realToFrac [x, y, z, w] :: [CFloat])
        return (ShaderUniformVec4Type, castPtr ptr)
    (ShaderUniformInt i) ->
      do
        ptr <- malloc
        poke ptr (fromIntegral i :: CInt)
        return (ShaderUniformIntType, castPtr ptr)
    (ShaderUniformIVec2 (i1, i2)) ->
      do
        ptr <- newArray (map fromIntegral [i1, i2] :: [CInt])
        return (ShaderUniformIVec2Type, castPtr ptr)
    (ShaderUniformIVec3 (i1, i2, i3)) ->
      do
        ptr <- newArray (map fromIntegral [i1, i2, i3] :: [CInt])
        return (ShaderUniformIVec3Type, castPtr ptr)
    (ShaderUniformIVec4 (i1, i2, i3, i4)) ->
      do
        ptr <- newArray (map fromIntegral [i1, i2, i3, i4] :: [CInt])
        return (ShaderUniformIVec4Type, castPtr ptr)
    (ShaderUniformSampler2D texture) ->
      do
        ptr <- malloc
        poke ptr (fromIntegral $ texture'id texture :: CInt)
        return (ShaderUniformSampler2DType, castPtr ptr)

unpackShaderUniformDataV :: ShaderUniformDataV -> IO (ShaderUniformDataType, Ptr (), Int)
unpackShaderUniformDataV xs = do
  case xs of
    (ShaderUniformFloatV fs) ->
      do
        ptr <- newArray (map realToFrac fs :: [CFloat])
        return (ShaderUniformFloatType, castPtr ptr, length fs)
    (ShaderUniformVec2V vs) ->
      do
        ptr <- newArray (map realToFrac $ concatMap (\(Vector2 x y) -> [x, y]) vs :: [CFloat])
        return (ShaderUniformVec2Type, castPtr ptr, length vs)
    (ShaderUniformVec3V vs) ->
      do
        ptr <- newArray (map realToFrac $ concatMap (\(Vector3 x y z) -> [x, y, z]) vs :: [CFloat])
        return (ShaderUniformVec3Type, castPtr ptr, length vs)
    (ShaderUniformVec4V vs) ->
      do
        ptr <- newArray (map realToFrac $ concatMap (\(Vector4 x y z w) -> [x, y, z, w]) vs :: [CFloat])
        return (ShaderUniformVec4Type, castPtr ptr, length vs)
    (ShaderUniformIntV is) ->
      do
        ptr <- newArray (map fromIntegral is :: [CInt])
        return (ShaderUniformIntType, castPtr ptr, length is)
    (ShaderUniformIVec2V is) ->
      do
        ptr <- newArray (map fromIntegral $ concatMap (\(x, y) -> [x, y]) is :: [CInt])
        return (ShaderUniformIVec2Type, castPtr ptr, length is)
    (ShaderUniformIVec3V is) ->
      do
        ptr <- newArray (map fromIntegral $ concatMap (\(x, y, z) -> [x, y, z]) is :: [CInt])
        return (ShaderUniformIVec3Type, castPtr ptr, length is)
    (ShaderUniformIVec4V is) ->
      do
        ptr <- newArray (map fromIntegral $ concatMap (\(x, y, z, w) -> [x, y, z, w]) is :: [CInt])
        return (ShaderUniformIVec4Type, castPtr ptr, length is)
    (ShaderUniformSampler2DV textures) ->
      do
        ptr <- newArray (map (fromIntegral . texture'id) textures :: [CInt])
        return (ShaderUniformSampler2DType, castPtr ptr, length textures)

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
    vertexCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    triangleCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    verticesPtr <- (peekByteOff _p 8 :: IO (Ptr Vector3))
    vertices <- peekArray vertexCount verticesPtr
    texcoordsPtr <- (peekByteOff _p 16 :: IO (Ptr Vector2))
    texcoords <- peekArray vertexCount texcoordsPtr
    texcoords2Ptr <- (peekByteOff _p 24 :: IO (Ptr Vector2))
    texcoords2 <- peekMaybeArray vertexCount texcoords2Ptr
    normalsPtr <- (peekByteOff _p 32 :: IO (Ptr Vector3))
    normals <- peekArray vertexCount normalsPtr
    tangentsPtr <- (peekByteOff _p 40 :: IO (Ptr Vector4))
    tangents <- peekMaybeArray vertexCount tangentsPtr
    colorsPtr <- (peekByteOff _p 48 :: IO (Ptr Color))
    colors <- peekMaybeArray vertexCount colorsPtr
    indicesPtr <- (peekByteOff _p 56 :: IO (Ptr CUShort))
    indices <- (\m -> map fromIntegral <$> m) <$> peekMaybeArray vertexCount indicesPtr
    animVerticesPtr <- (peekByteOff _p 64 :: IO (Ptr Vector3))
    animVertices <- peekMaybeArray vertexCount animVerticesPtr
    animNormalsPtr <- (peekByteOff _p 72 :: IO (Ptr Vector3))
    animNormals <- peekMaybeArray vertexCount animNormalsPtr
    boneIdsPtr <- (peekByteOff _p 80 :: IO (Ptr CUChar))
    boneIds <- (\m -> map fromIntegral <$> m) <$> peekMaybeArray (vertexCount * 4) boneIdsPtr
    boneWeightsPtr <- (peekByteOff _p 88 :: IO (Ptr CFloat))
    boneWeights <- (map realToFrac <$>) <$> peekMaybeArray (vertexCount * 4) boneWeightsPtr
    vaoId <- fromIntegral <$> (peekByteOff _p 96 :: IO CUInt)
    vboIdPtr <- (peekByteOff _p 104 :: IO (Ptr CUInt))
    vboId <- (\m -> map fromIntegral <$> m) <$> peekMaybeArray 7 vboIdPtr
    return $ Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices animVertices animNormals boneIds boneWeights vaoId vboId
  poke _p (Mesh vertexCount triangleCount vertices texcoords texcoords2 normals tangents colors indices animVertices animNormals boneIds boneWeights vaoId vboId) = do
    pokeByteOff _p 0 (fromIntegral vertexCount :: CInt)
    pokeByteOff _p 4 (fromIntegral triangleCount :: CInt)
    pokeByteOff _p 8 =<< newArray vertices
    pokeByteOff _p 16 =<< newArray texcoords
    newMaybeArray texcoords2 >>= pokeByteOff _p 24
    pokeByteOff _p 32 =<< newArray normals
    newMaybeArray tangents >>= pokeByteOff _p 40
    newMaybeArray colors >>= pokeByteOff _p 48
    newMaybeArray (map fromIntegral <$> indices :: Maybe [CUShort]) >>= pokeByteOff _p 56
    newMaybeArray animVertices >>= pokeByteOff _p 64
    newMaybeArray animNormals >>= pokeByteOff _p 72
    newMaybeArray (map fromIntegral <$> boneIds :: Maybe [CUChar]) >>= pokeByteOff _p 80
    newMaybeArray (map realToFrac <$> boneWeights :: Maybe [CFloat]) >>= pokeByteOff _p 88
    pokeByteOff _p 96 (fromIntegral vaoId :: CUInt)
    newMaybeArray (map fromIntegral <$> vboId :: Maybe [CUInt]) >>= pokeByteOff _p 104
    return ()

instance Freeable Mesh where
  rlFreeDependents _ ptr = do
    verticesPtr <- (peekByteOff ptr 8 :: IO (Ptr Float))
    c'free $ castPtr verticesPtr
    texcoordsPtr <- (peekByteOff ptr 16 :: IO (Ptr Vector2))
    c'free $ castPtr texcoordsPtr
    texcoords2Ptr <- (peekByteOff ptr 24 :: IO (Ptr Vector2))
    freeMaybePtr $ castPtr texcoords2Ptr
    normalsPtr <- (peekByteOff ptr 32 :: IO (Ptr Vector3))
    c'free $ castPtr normalsPtr
    tangentsPtr <- (peekByteOff ptr 40 :: IO (Ptr Vector4))
    freeMaybePtr $ castPtr tangentsPtr
    colorsPtr <- (peekByteOff ptr 48 :: IO (Ptr Color))
    freeMaybePtr $ castPtr colorsPtr
    indicesPtr <- (peekByteOff ptr 56 :: IO (Ptr CUShort))
    freeMaybePtr $ castPtr indicesPtr
    animVerticesPtr <- (peekByteOff ptr 64 :: IO (Ptr Vector3))
    freeMaybePtr $ castPtr animVerticesPtr
    animNormalsPtr <- (peekByteOff ptr 72 :: IO (Ptr Vector3))
    freeMaybePtr $ castPtr animNormalsPtr
    boneIdsPtr <- (peekByteOff ptr 80 :: IO (Ptr CUChar))
    freeMaybePtr $ castPtr boneIdsPtr
    boneWeightsPtr <- (peekByteOff ptr 88 :: IO (Ptr CFloat))
    freeMaybePtr $ castPtr boneWeightsPtr
    vboIdPtr <- (peekByteOff ptr 104 :: IO (Ptr CUInt))
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
    sId <- fromIntegral <$> (peekByteOff _p 0 :: IO CUInt)
    locsPtr <- (peekByteOff _p 8 :: IO (Ptr CInt))
    locs <- map fromIntegral <$> peekArray 32 locsPtr
    return $ Shader sId locs
  poke _p (Shader sId locs) = do
    pokeByteOff _p 0 (fromIntegral sId :: CUInt)
    defaultShaderId <- c'rlGetShaderIdDefault
    locsArr <- newArray (map fromIntegral locs :: [CInt])
    if sId == fromIntegral defaultShaderId
      then do
        locsPtr <- newForeignPtr p'free locsArr
        withForeignPtr locsPtr $ pokeByteOff _p 8
      else pokeByteOff _p 8 locsArr
    return ()

instance Freeable Shader where
  rlFreeDependents val ptr = do
    defaultShaderId <- c'rlGetShaderIdDefault
    unless
      (shader'id val == fromIntegral defaultShaderId)
      ( do
          locsPtr <- (peekByteOff ptr 8 :: IO (Ptr CInt))
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
    texture <- peekByteOff _p 0
    color <- peekByteOff _p 20
    value <- realToFrac <$> (peekByteOff _p 24 :: IO CFloat)
    return $ MaterialMap texture color value
  poke _p (MaterialMap texture color value) = do
    pokeByteOff _p 0 texture
    pokeByteOff _p 20 color
    pokeByteOff _p 24 (realToFrac value :: CFloat)
    return ()

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
    shader <- peekByteOff _p 0
    mapsPtr <- (peekByteOff _p 16 :: IO (Ptr MaterialMap))
    maps <- peekMaybeArray 12 mapsPtr
    params <- map realToFrac <$> peekStaticArrayOff 4 (castPtr _p :: Ptr CFloat) 24
    return $ Material shader maps params
  poke _p (Material shader maps params) = do
    pokeByteOff _p 0 shader
    pokeByteOff _p 16 =<< newMaybeArray maps
    pokeStaticArrayOff (castPtr _p :: Ptr CFloat) 24 (map realToFrac params :: [CFloat])
    return ()

instance Freeable Material where
  rlFreeDependents val ptr = do
    rlFreeDependents (material'shader val) (castPtr ptr :: Ptr Shader)
    mapsPtr <- (peekByteOff ptr 16 :: IO (Ptr MaterialMap))
    rlFreeMaybeArray (material'maps val) mapsPtr

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
    translation <- peekByteOff _p 0
    rotation <- peekByteOff _p 12
    scale <- peekByteOff _p 28
    return $ Transform translation rotation scale
  poke _p (Transform translation rotation scale) = do
    pokeByteOff _p 0 translation
    pokeByteOff _p 12 rotation
    pokeByteOff _p 28 scale
    return ()

data BoneInfo = BoneInfo
  { boneInfo'name :: String,
    boneinfo'parent :: Int
  }
  deriving (Eq, Show, Freeable)

instance Storable BoneInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    name <- map castCCharToChar . takeWhile (/= 0) <$> peekStaticArray 32 (castPtr _p :: Ptr CChar)
    parent <- fromIntegral <$> (peekByteOff _p 32 :: IO CInt)
    return $ BoneInfo name parent
  poke _p (BoneInfo name parent) = do
    pokeStaticArray (castPtr _p :: Ptr CChar) (rightPad 32 0 $ map castCharToCChar name)
    pokeByteOff _p 32 (fromIntegral parent :: CInt)
    return ()

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
    transform <- peekByteOff _p 0
    meshCount <- fromIntegral <$> (peekByteOff _p 64 :: IO CInt)
    materialCount <- fromIntegral <$> (peekByteOff _p 68 :: IO CInt)
    meshesPtr <- (peekByteOff _p 72 :: IO (Ptr Mesh))
    meshes <- peekArray meshCount meshesPtr
    materialsPtr <- (peekByteOff _p 80 :: IO (Ptr Material))
    materials <- peekArray materialCount materialsPtr
    meshMaterialPtr <- (peekByteOff _p 88 :: IO (Ptr CInt))
    meshMaterial <- map fromIntegral <$> peekArray meshCount meshMaterialPtr
    boneCount <- fromIntegral <$> (peekByteOff _p 96 :: IO CInt)
    bonesPtr <- (peekByteOff _p 104 :: IO (Ptr BoneInfo))
    bones <- peekMaybeArray boneCount bonesPtr
    bindPosePtr <- (peekByteOff _p 112 :: IO (Ptr Transform))
    bindPose <- peekMaybeArray boneCount bindPosePtr
    return $ Model transform meshes materials meshMaterial boneCount bones bindPose
  poke _p (Model transform meshes materials meshMaterial boneCount bones bindPose) = do
    pokeByteOff _p 0 transform
    pokeByteOff _p 64 (fromIntegral $ length meshes :: CInt)
    pokeByteOff _p 68 (fromIntegral $ length materials :: CInt)
    pokeByteOff _p 72 =<< newArray meshes
    pokeByteOff _p 80 =<< newArray materials
    pokeByteOff _p 88 =<< newArray (map fromIntegral meshMaterial :: [CInt])
    pokeByteOff _p 96 (fromIntegral boneCount :: CInt)
    newMaybeArray bones >>= pokeByteOff _p 104
    newMaybeArray bindPose >>= pokeByteOff _p 112
    return ()

instance Freeable Model where
  rlFreeDependents val ptr = do
    meshesPtr <- (peekByteOff ptr 72 :: IO (Ptr Mesh))
    rlFreeArray (model'meshes val) meshesPtr
    materialsPtr <- (peekByteOff ptr 80 :: IO (Ptr Material))
    rlFreeArray (model'materials val) materialsPtr
    meshMaterialPtr <- (peekByteOff ptr 88 :: IO (Ptr CInt))
    c'free $ castPtr meshMaterialPtr
    bonesPtr <- (peekByteOff ptr 104 :: IO (Ptr BoneInfo))
    freeMaybePtr $ castPtr bonesPtr
    bindPosePtr <- (peekByteOff ptr 112 :: IO (Ptr Transform))
    freeMaybePtr $ castPtr bindPosePtr

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
    boneCount <- fromIntegral <$> (peekByteOff _p 0 :: IO CInt)
    frameCount <- fromIntegral <$> (peekByteOff _p 4 :: IO CInt)
    bonesPtr <- (peekByteOff _p 8 :: IO (Ptr BoneInfo))
    bones <- peekArray boneCount bonesPtr
    framePosesPtr <- (peekByteOff _p 16 :: IO (Ptr (Ptr Transform)))
    framePosesPtrArr <- peekArray frameCount framePosesPtr
    framePoses <- mapM (peekArray boneCount) framePosesPtrArr
    name <- map castCCharToChar <$> peekStaticArrayOff 32 (castPtr _p) 24
    return $ ModelAnimation boneCount frameCount bones framePoses name
  poke _p (ModelAnimation boneCount frameCount bones framePoses name) = do
    pokeByteOff _p 0 (fromIntegral boneCount :: CInt)
    pokeByteOff _p 4 (fromIntegral frameCount :: CInt)
    pokeByteOff _p 8 =<< newArray bones
    mapM newArray framePoses >>= newArray >>= pokeByteOff _p 16
    pokeStaticArrayOff (castPtr _p) 24 (map castCharToCChar name)
    return ()

instance Freeable ModelAnimation where
  rlFreeDependents val ptr = do
    bonesPtr <- (peekByteOff ptr 8 :: IO (Ptr BoneInfo))
    c'free $ castPtr bonesPtr
    framePosesPtr <- (peekByteOff ptr 16 :: IO (Ptr (Ptr Transform)))
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
    position <- peekByteOff _p 0
    direction <- peekByteOff _p 12
    return $ Ray position direction
  poke _p (Ray position direction) = do
    pokeByteOff _p 0 position
    pokeByteOff _p 12 direction
    return ()

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
    hit <- toBool <$> (peekByteOff _p 0 :: IO CBool)
    distance <- realToFrac <$> (peekByteOff _p 4 :: IO CFloat)
    point <- peekByteOff _p 8
    normal <- peekByteOff _p 20
    return $ RayCollision hit distance point normal
  poke _p (RayCollision hit distance point normal) = do
    pokeByteOff _p 0 (fromBool hit :: CInt)
    pokeByteOff _p 4 (realToFrac distance :: CFloat)
    pokeByteOff _p 8 point
    pokeByteOff _p 20 normal
    return ()

data BoundingBox = BoundingBox
  { boundingBox'min :: Vector3,
    boundingBox'max :: Vector3
  }
  deriving (Eq, Show, Freeable)

instance Storable BoundingBox where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    bMin <- peekByteOff _p 0
    bMax <- peekByteOff _p 12
    return $ BoundingBox bMin bMax
  poke _p (BoundingBox bMin bMax) = do
    pokeByteOff _p 0 bMin
    pokeByteOff _p 12 bMax
    return ()
