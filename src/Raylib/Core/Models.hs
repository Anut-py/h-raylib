{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings to @rmodels@
module Raylib.Core.Models
  ( -- * High level
    drawLine3D,
    drawPoint3D,
    drawCircle3D,
    drawTriangle3D,
    drawTriangleStrip3D,
    drawCube,
    drawCubeV,
    drawCubeWires,
    drawCubeWiresV,
    drawSphere,
    drawSphereEx,
    drawSphereWires,
    drawCylinder,
    drawCylinderEx,
    drawCylinderWires,
    drawCylinderWiresEx,
    drawCapsule,
    drawCapsuleWires,
    drawPlane,
    drawRay,
    drawGrid,
    loadModel,
    loadModelFromMesh,
    loadModelFromMeshManaged,
    unloadModel,
    isModelValid,
    getModelBoundingBox,
    drawModel,
    drawModelEx,
    drawModelWires,
    drawModelWiresEx,
    drawModelPoints,
    drawModelPointsEx,
    drawBoundingBox,
    drawBillboard,
    drawBillboardRec,
    drawBillboardPro,
    uploadMesh,
    updateMeshBuffer,
    unloadMesh,
    drawMesh,
    drawMeshInstanced,
    exportMesh,
    exportMeshAsCode,
    getMeshBoundingBox,
    genMeshTangents,
    genMeshPoly,
    genMeshPlane,
    genMeshCube,
    genMeshSphere,
    genMeshHemiSphere,
    genMeshCylinder,
    genMeshCone,
    genMeshTorus,
    genMeshKnot,
    genMeshHeightmap,
    genMeshCubicmap,
    loadMaterials,
    unloadMaterial,
    loadMaterialDefault,
    isMaterialValid,
    setMaterialTexture,
    setModelMeshMaterial,
    loadModelAnimations,
    updateModelAnimation,
    isModelAnimationValid,
    updateModelAnimationBoneMatrices,
    checkCollisionSpheres,
    checkCollisionBoxes,
    checkCollisionBoxSphere,
    getRayCollisionSphere,
    getRayCollisionBox,
    getRayCollisionMesh,
    getRayCollisionTriangle,
    getRayCollisionQuad,

    -- * Native
    c'drawLine3D,
    c'drawPoint3D,
    c'drawCircle3D,
    c'drawTriangle3D,
    c'drawTriangleStrip3D,
    c'drawCube,
    c'drawCubeV,
    c'drawCubeWires,
    c'drawCubeWiresV,
    c'drawSphere,
    c'drawSphereEx,
    c'drawSphereWires,
    c'drawCylinder,
    c'drawCylinderEx,
    c'drawCylinderWires,
    c'drawCylinderWiresEx,
    c'drawCapsule,
    c'drawCapsuleWires,
    c'drawPlane,
    c'drawRay,
    c'drawGrid,
    c'loadModel,
    c'loadModelFromMesh,
    c'isModelValid,
    c'unloadModel,
    c'getModelBoundingBox,
    c'drawModel,
    c'drawModelEx,
    c'drawModelWires,
    c'drawModelWiresEx,
    c'drawModelPoints,
    c'drawModelPointsEx,
    c'drawBoundingBox,
    c'drawBillboard,
    c'drawBillboardRec,
    c'drawBillboardPro,
    c'uploadMesh,
    c'updateMeshBuffer,
    c'unloadMesh,
    c'drawMesh,
    c'drawMeshInstanced,
    c'exportMesh,
    c'exportMeshAsCode,
    c'getMeshBoundingBox,
    c'genMeshTangents,
    c'genMeshPoly,
    c'genMeshPlane,
    c'genMeshCube,
    c'genMeshSphere,
    c'genMeshHemiSphere,
    c'genMeshCylinder,
    c'genMeshCone,
    c'genMeshTorus,
    c'genMeshKnot,
    c'genMeshHeightmap,
    c'genMeshCubicmap,
    c'loadMaterials,
    c'loadMaterialDefault,
    c'isMaterialValid,
    c'unloadMaterial,
    c'setMaterialTexture,
    c'setModelMeshMaterial,
    c'loadModelAnimations,
    c'updateModelAnimation,
    c'unloadModelAnimation,
    c'unloadModelAnimations,
    c'isModelAnimationValid,
    c'updateModelAnimationBoneMatrices,
    c'checkCollisionSpheres,
    c'checkCollisionBoxes,
    c'checkCollisionBoxSphere,
    c'getRayCollisionSphere,
    c'getRayCollisionBox,
    c'getRayCollisionMesh,
    c'getRayCollisionTriangle,
    c'getRayCollisionQuad,
  )
where

import Control.Monad (forM_)
import Foreign (Ptr, Storable (peek), advancePtr, fromBool, nullPtr, toBool)
import Foreign.C
  ( CBool (..),
    CFloat (..),
    CInt (..),
    CString,
  )
import GHC.IO (unsafePerformIO)
import Raylib.Internal (WindowResources, addToWindowResources, unloadSingleShader, unloadSingleTexture, unloadSingleVaoId, unloadSingleVboIdList)
import Raylib.Internal.Foreign
  ( ALike (peekALike, popALike, withALike, withALikeLen),
    Mutable (peekMutated),
    PALike,
    PLike,
    StringLike,
    TLike (peekTLike, popTLike, withTLike),
    peekMaybeArray,
    pop,
    withFreeable,
  )
import Raylib.Internal.TH (genNative)
import Raylib.Types
  ( BoundingBox,
    Camera3D,
    Color,
    Image,
    Material,
    MaterialMapIndex,
    Matrix,
    Mesh,
    Model,
    ModelAnimation,
    Ray,
    RayCollision,
    Rectangle,
    Texture,
    Vector2,
    Vector3,
    p'material'maps,
    p'material'shader,
    p'materialMap'texture,
    p'mesh'vaoId,
    p'mesh'vboId,
    p'model'materialCount,
    p'model'materials,
    p'model'meshCount,
    p'model'meshes,
    p'shader'id,
    p'texture'id,
  )

$( genNative
     [ ("c'drawLine3D", "DrawLine3D_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawPoint3D", "DrawPoint3D_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawCircle3D", "DrawCircle3D_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawTriangle3D", "DrawTriangle3D_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawTriangleStrip3D", "DrawTriangleStrip3D_", "rl_bindings.h", [t|Ptr Vector3 -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCube", "DrawCube_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawCubeV", "DrawCubeV_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawCubeWires", "DrawCubeWires_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawCubeWiresV", "DrawCubeWiresV_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawSphere", "DrawSphere_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawSphereEx", "DrawSphereEx_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawSphereWires", "DrawSphereWires_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCylinder", "DrawCylinder_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCylinderEx", "DrawCylinderEx_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCylinderWires", "DrawCylinderWires_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCylinderWiresEx", "DrawCylinderWiresEx_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCapsule", "DrawCapsule_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawCapsuleWires", "DrawCapsuleWires_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()|]),
       ("c'drawPlane", "DrawPlane_", "rl_bindings.h", [t|Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawRay", "DrawRay_", "rl_bindings.h", [t|Ptr Ray -> Ptr Color -> IO ()|]),
       ("c'drawGrid", "DrawGrid_", "rl_bindings.h", [t|CInt -> CFloat -> IO ()|]),
       ("c'loadModel", "LoadModel_", "rl_bindings.h", [t|CString -> IO (Ptr Model)|]),
       ("c'loadModelFromMesh", "LoadModelFromMesh_", "rl_bindings.h", [t|Ptr Mesh -> IO (Ptr Model)|]),
       ("c'isModelValid", "IsModelValid_", "rl_bindings.h", [t|Ptr Model -> IO CBool|]),
       ("c'unloadModel", "UnloadModel_", "rl_bindings.h", [t|Ptr Model -> IO ()|]),
       ("c'getModelBoundingBox", "GetModelBoundingBox_", "rl_bindings.h", [t|Ptr Model -> IO (Ptr BoundingBox)|]),
       ("c'drawModel", "DrawModel_", "rl_bindings.h", [t|Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawModelEx", "DrawModelEx_", "rl_bindings.h", [t|Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawModelWires", "DrawModelWires_", "rl_bindings.h", [t|Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawModelWiresEx", "DrawModelWiresEx_", "rl_bindings.h", [t|Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawModelPoints", "DrawModelPoints_", "rl_bindings.h", [t|Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawModelPointsEx", "DrawModelPointsEx_", "rl_bindings.h", [t|Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()|]),
       ("c'drawBoundingBox", "DrawBoundingBox_", "rl_bindings.h", [t|Ptr BoundingBox -> Ptr Color -> IO ()|]),
       ("c'drawBillboard", "DrawBillboard_", "rl_bindings.h", [t|Ptr Camera3D -> Ptr Texture -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'drawBillboardRec", "DrawBillboardRec_", "rl_bindings.h", [t|Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()|]),
       ("c'drawBillboardPro", "DrawBillboardPro_", "rl_bindings.h", [t|Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()|]),
       ("c'uploadMesh", "UploadMesh_", "rl_bindings.h", [t|Ptr Mesh -> CInt -> IO ()|]),
       ("c'updateMeshBuffer", "UpdateMeshBuffer_", "rl_bindings.h", [t|Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()|]),
       ("c'unloadMesh", "UnloadMesh_", "rl_bindings.h", [t|Ptr Mesh -> IO ()|]),
       ("c'drawMesh", "DrawMesh_", "rl_bindings.h", [t|Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()|]),
       ("c'drawMeshInstanced", "DrawMeshInstanced_", "rl_bindings.h", [t|Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()|]),
       ("c'exportMesh", "ExportMesh_", "rl_bindings.h", [t|Ptr Mesh -> CString -> IO CBool|]),
       ("c'exportMeshAsCode", "ExportMeshAsCode_", "rl_bindings.h", [t|Ptr Mesh -> CString -> IO CBool|]),
       ("c'getMeshBoundingBox", "GetMeshBoundingBox_", "rl_bindings.h", [t|Ptr Mesh -> IO (Ptr BoundingBox)|]),
       ("c'genMeshTangents", "GenMeshTangents_", "rl_bindings.h", [t|Ptr Mesh -> IO ()|]),
       ("c'genMeshPoly", "GenMeshPoly_", "rl_bindings.h", [t|CInt -> CFloat -> IO (Ptr Mesh)|]),
       ("c'genMeshPlane", "GenMeshPlane_", "rl_bindings.h", [t|CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshCube", "GenMeshCube_", "rl_bindings.h", [t|CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)|]),
       ("c'genMeshSphere", "GenMeshSphere_", "rl_bindings.h", [t|CFloat -> CInt -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshHemiSphere", "GenMeshHemiSphere_", "rl_bindings.h", [t|CFloat -> CInt -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshCylinder", "GenMeshCylinder_", "rl_bindings.h", [t|CFloat -> CFloat -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshCone", "GenMeshCone_", "rl_bindings.h", [t|CFloat -> CFloat -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshTorus", "GenMeshTorus_", "rl_bindings.h", [t|CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshKnot", "GenMeshKnot_", "rl_bindings.h", [t|CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)|]),
       ("c'genMeshHeightmap", "GenMeshHeightmap_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)|]),
       ("c'genMeshCubicmap", "GenMeshCubicmap_", "rl_bindings.h", [t|Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)|]),
       ("c'loadMaterials", "LoadMaterials_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO (Ptr Material)|]),
       ("c'loadMaterialDefault", "LoadMaterialDefault_", "rl_bindings.h", [t|IO (Ptr Material)|]),
       ("c'isMaterialValid", "IsMaterialValid_", "rl_bindings.h", [t|Ptr Material -> IO CBool|]),
       ("c'unloadMaterial", "UnloadMaterial_", "rl_bindings.h", [t|Ptr Material -> IO ()|]),
       ("c'setMaterialTexture", "SetMaterialTexture_", "rl_bindings.h", [t|Ptr Material -> CInt -> Ptr Texture -> IO ()|]),
       ("c'setModelMeshMaterial", "SetModelMeshMaterial_", "rl_bindings.h", [t|Ptr Model -> CInt -> CInt -> IO ()|]),
       ("c'loadModelAnimations", "LoadModelAnimations_", "rl_bindings.h", [t|CString -> Ptr CInt -> IO (Ptr ModelAnimation)|]),
       ("c'updateModelAnimation", "UpdateModelAnimation_", "rl_bindings.h", [t|Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()|]),
       ("c'unloadModelAnimation", "UnloadModelAnimation_", "rl_bindings.h", [t|Ptr ModelAnimation -> IO ()|]),
       ("c'unloadModelAnimations", "UnloadModelAnimations_", "rl_bindings.h", [t|Ptr ModelAnimation -> CInt -> IO ()|]),
       ("c'isModelAnimationValid", "IsModelAnimationValid_", "rl_bindings.h", [t|Ptr Model -> Ptr ModelAnimation -> IO CBool|]),
       ("c'updateModelAnimationBoneMatrices", "UpdateModelAnimationBoneMatrices_", "rl_bindings.h", [t|Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()|]),
       ("c'checkCollisionSpheres", "CheckCollisionSpheres_", "rl_bindings.h", [t|Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> IO CBool|]),
       ("c'checkCollisionBoxes", "CheckCollisionBoxes_", "rl_bindings.h", [t|Ptr BoundingBox -> Ptr BoundingBox -> IO CBool|]),
       ("c'checkCollisionBoxSphere", "CheckCollisionBoxSphere_", "rl_bindings.h", [t|Ptr BoundingBox -> Ptr Vector3 -> CFloat -> IO CBool|]),
       ("c'getRayCollisionSphere", "GetRayCollisionSphere_", "rl_bindings.h", [t|Ptr Ray -> Ptr Vector3 -> CFloat -> IO (Ptr RayCollision)|]),
       ("c'getRayCollisionBox", "GetRayCollisionBox_", "rl_bindings.h", [t|Ptr Ray -> Ptr BoundingBox -> IO (Ptr RayCollision)|]),
       ("c'getRayCollisionMesh", "GetRayCollisionMesh_", "rl_bindings.h", [t|Ptr Ray -> Ptr Mesh -> Ptr Matrix -> IO (Ptr RayCollision)|]),
       ("c'getRayCollisionTriangle", "GetRayCollisionTriangle_", "rl_bindings.h", [t|Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)|]),
       ("c'getRayCollisionQuad", "GetRayCollisionQuad_", "rl_bindings.h", [t|Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)|])
     ]
 )

drawLine3D :: Vector3 -> Vector3 -> Color -> IO ()
drawLine3D start end color = withFreeable start (\s -> withFreeable end (withFreeable color . c'drawLine3D s))

drawPoint3D :: Vector3 -> Color -> IO ()
drawPoint3D point color = withFreeable point (withFreeable color . c'drawPoint3D)

drawCircle3D :: Vector3 -> Float -> Vector3 -> Float -> Color -> IO ()
drawCircle3D center radius rotationAxis rotationAngle color = withFreeable center (\c -> withFreeable rotationAxis (\r -> withFreeable color (c'drawCircle3D c (realToFrac radius) r (realToFrac rotationAngle))))

drawTriangle3D :: Vector3 -> Vector3 -> Vector3 -> Color -> IO ()
drawTriangle3D v1 v2 v3 color = withFreeable v1 (\p1 -> withFreeable v2 (\p2 -> withFreeable v3 (withFreeable color . c'drawTriangle3D p1 p2)))

drawTriangleStrip3D :: (PALike Vector3 points) => points -> Int -> Color -> IO ()
drawTriangleStrip3D points pointCount color = withALike points (\p -> withFreeable color (c'drawTriangleStrip3D p (fromIntegral pointCount)))

drawCube :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCube position width height _length color = withFreeable position (\p -> withFreeable color (c'drawCube p (realToFrac width) (realToFrac height) (realToFrac _length)))

drawCubeV :: Vector3 -> Vector3 -> Color -> IO ()
drawCubeV position size color = withFreeable position (\p -> withFreeable size (withFreeable color . c'drawCubeV p))

drawCubeWires :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeWires position width height _length color = withFreeable position (\p -> withFreeable color (c'drawCubeWires p (realToFrac width) (realToFrac height) (realToFrac _length)))

drawCubeWiresV :: Vector3 -> Vector3 -> Color -> IO ()
drawCubeWiresV position size color = withFreeable position (\p -> withFreeable size (withFreeable color . c'drawCubeWiresV p))

drawSphere :: Vector3 -> Float -> Color -> IO ()
drawSphere position radius color = withFreeable position (\p -> withFreeable color (c'drawSphere p (realToFrac radius)))

drawSphereEx :: Vector3 -> Float -> Int -> Int -> Color -> IO ()
drawSphereEx position radius rings slices color = withFreeable position (\p -> withFreeable color (c'drawSphereEx p (realToFrac radius) (fromIntegral rings) (fromIntegral slices)))

drawSphereWires :: Vector3 -> Float -> Int -> Int -> Color -> IO ()
drawSphereWires position radius rings slices color = withFreeable position (\p -> withFreeable color (c'drawSphereWires p (realToFrac radius) (fromIntegral rings) (fromIntegral slices)))

drawCylinder :: Vector3 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCylinder position radiusTop radiusBottom height slices color = withFreeable position (\p -> withFreeable color (c'drawCylinder p (realToFrac radiusTop) (realToFrac radiusBottom) (realToFrac height) (fromIntegral slices)))

drawCylinderEx :: Vector3 -> Vector3 -> Float -> Float -> Int -> Color -> IO ()
drawCylinderEx start end startRadius endRadius sides color = withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawCylinderEx s e (realToFrac startRadius) (realToFrac endRadius) (fromIntegral sides))))

drawCylinderWires :: Vector3 -> Float -> Float -> Float -> Int -> Color -> IO ()
drawCylinderWires position radiusTop radiusBottom height slices color = withFreeable position (\p -> withFreeable color (c'drawCylinderWires p (realToFrac radiusTop) (realToFrac radiusBottom) (realToFrac height) (fromIntegral slices)))

drawCylinderWiresEx :: Vector3 -> Vector3 -> Float -> Float -> Int -> Color -> IO ()
drawCylinderWiresEx start end startRadius endRadius sides color = withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawCylinderWiresEx s e (realToFrac startRadius) (realToFrac endRadius) (fromIntegral sides))))

drawCapsule :: Vector3 -> Vector3 -> Float -> Int -> Int -> Color -> IO ()
drawCapsule start end radius slices rings color = withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawCapsule s e (realToFrac radius) (fromIntegral slices) (fromIntegral rings))))

drawCapsuleWires :: Vector3 -> Vector3 -> Float -> Int -> Int -> Color -> IO ()
drawCapsuleWires start end radius slices rings color = withFreeable start (\s -> withFreeable end (\e -> withFreeable color (c'drawCapsuleWires s e (realToFrac radius) (fromIntegral slices) (fromIntegral rings))))

drawPlane :: Vector3 -> Vector2 -> Color -> IO ()
drawPlane center size color = withFreeable center (\c -> withFreeable size (withFreeable color . c'drawPlane c))

drawRay :: Ray -> Color -> IO ()
drawRay ray color = withFreeable ray (withFreeable color . c'drawRay)

drawGrid :: Int -> Float -> IO ()
drawGrid slices spacing = c'drawGrid (fromIntegral slices) (realToFrac spacing)

loadModel :: (StringLike string, PLike Model model) => string -> IO model
loadModel fileName = withTLike fileName c'loadModel >>= popTLike

-- | Use `loadModelFromMeshManaged` for a resource-managed version
--
--   WARNING: Do not use this with `managed`
loadModelFromMesh :: (PLike Mesh mesh, PLike Model model) => mesh -> IO model
loadModelFromMesh mesh = withTLike mesh c'loadModelFromMesh >>= popTLike

loadModelFromMeshManaged :: (PLike Mesh mesh, PLike Model model) => mesh -> WindowResources -> IO model
loadModelFromMeshManaged mesh wr = do
  model <- loadModelFromMesh mesh
  withTLike
    model
    ( \modelPtr -> do
        matCount :: Int <- fromIntegral <$> peek (p'model'materialCount modelPtr)
        mats <- peek (p'model'materials modelPtr)
        forM_ [0 .. matCount - 1] (addToWindowResources wr . advancePtr mats)
    )
  return model

-- | Unloads a `managed` model from GPU memory (VRAM)
unloadModel :: (PLike Model model) => model -> WindowResources -> IO ()
unloadModel model wr =
  withTLike
    model
    ( \modelPtr -> do
        meshCount :: Int <- fromIntegral <$> peek (p'model'meshCount modelPtr)
        matCount :: Int <- fromIntegral <$> peek (p'model'materialCount modelPtr)
        meshes <- peek (p'model'meshes modelPtr)
        mats <- peek (p'model'materials modelPtr)
        forM_ [0 .. meshCount - 1] (addToWindowResources wr . advancePtr meshes)
        forM_ [0 .. matCount - 1] (addToWindowResources wr . advancePtr mats)
    )

isModelValid :: (PLike Model model) => model -> IO Bool
isModelValid model = toBool <$> withTLike model c'isModelValid

getModelBoundingBox :: (PLike Model model) => model -> IO BoundingBox
getModelBoundingBox model = withTLike model c'getModelBoundingBox >>= pop

drawModel :: (PLike Model model) => model -> Vector3 -> Float -> Color -> IO ()
drawModel model position scale tint = withTLike model (\m -> withFreeable position (\p -> withFreeable tint (c'drawModel m p (realToFrac scale))))

drawModelEx :: (PLike Model model) => model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelEx model position rotationAxis rotationAngle scale tint = withTLike model (\m -> withFreeable position (\p -> withFreeable rotationAxis (\r -> withFreeable scale (withFreeable tint . c'drawModelEx m p r (realToFrac rotationAngle)))))

drawModelWires :: (PLike Model model) => model -> Vector3 -> Float -> Color -> IO ()
drawModelWires model position scale tint = withTLike model (\m -> withFreeable position (\p -> withFreeable tint (c'drawModelWires m p (realToFrac scale))))

drawModelWiresEx :: (PLike Model model) => model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelWiresEx model position rotationAxis rotationAngle scale tint = withTLike model (\m -> withFreeable position (\p -> withFreeable rotationAxis (\r -> withFreeable scale (withFreeable tint . c'drawModelWiresEx m p r (realToFrac rotationAngle)))))

drawModelPoints :: (PLike Model model) => model -> Vector3 -> Float -> Color -> IO ()
drawModelPoints model position scale tint = withTLike model (\m -> withFreeable position (\p -> withFreeable tint (c'drawModelPoints m p (realToFrac scale))))

drawModelPointsEx :: (PLike Model model) => model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelPointsEx model position rotationAxis rotationAngle scale tint = withTLike model (\m -> withFreeable position (\p -> withFreeable rotationAxis (\r -> withFreeable scale (withFreeable tint . c'drawModelPointsEx m p r (realToFrac rotationAngle)))))

drawBoundingBox :: BoundingBox -> Color -> IO ()
drawBoundingBox box color = withFreeable box (withFreeable color . c'drawBoundingBox)

drawBillboard :: (PLike Camera3D camera3D, PLike Texture texture) => camera3D -> texture -> Vector3 -> Float -> Color -> IO ()
drawBillboard camera texture position size tint = withTLike camera (\c -> withTLike texture (\t -> withFreeable position (\p -> withFreeable tint (c'drawBillboard c t p (realToFrac size)))))

drawBillboardRec :: (PLike Camera3D camera3D, PLike Texture texture) => camera3D -> texture -> Rectangle -> Vector3 -> Vector2 -> Color -> IO ()
drawBillboardRec camera texture source position size tint = withTLike camera (\c -> withTLike texture (\t -> withFreeable source (\s -> withFreeable position (\p -> withFreeable size (withFreeable tint . c'drawBillboardRec c t s p)))))

drawBillboardPro :: (PLike Camera3D camera3D, PLike Texture texture) => camera3D -> texture -> Rectangle -> Vector3 -> Vector3 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawBillboardPro camera texture source position up size origin rotation tint = withTLike camera (\c -> withTLike texture (\t -> withFreeable source (\s -> withFreeable position (\p -> withFreeable up (\u -> withFreeable size (\sz -> withFreeable origin (\o -> withFreeable tint (c'drawBillboardPro c t s p u sz o (realToFrac rotation)))))))))

uploadMesh :: (PLike Mesh mesh) => mesh -> Bool -> IO mesh
uploadMesh mesh dynamic = withTLike mesh (\m -> c'uploadMesh m (fromBool dynamic) >> peekTLike m)

updateMeshBuffer :: (PLike Mesh mesh) => mesh -> Int -> Ptr () -> Int -> Int -> IO ()
updateMeshBuffer mesh index dataValue dataSize offset = withTLike mesh (\m -> c'updateMeshBuffer m (fromIntegral index) dataValue (fromIntegral dataSize) (fromIntegral offset))

-- | Unloads a `managed` mesh from GPU memory (VRAM)
unloadMesh :: (PLike Mesh mesh) => mesh -> WindowResources -> IO ()
unloadMesh mesh wr =
  withTLike
    mesh
    ( \meshPtr -> do
        vao <- peek (p'mesh'vaoId meshPtr)
        vbo <- peekMaybeArray 9 =<< peek (p'mesh'vboId meshPtr)
        unloadSingleVaoId vao wr
        unloadSingleVboIdList vbo wr
    )

drawMesh :: (PLike Mesh mesh, PLike Material material, PLike Matrix matrix) => mesh -> material -> matrix -> IO ()
drawMesh mesh material transform = withTLike mesh (\m -> withTLike material (withTLike transform . c'drawMesh m))

drawMeshInstanced :: (PLike Mesh mesh, PLike Material material, PALike Matrix matrices) => mesh -> material -> matrices -> IO ()
drawMeshInstanced mesh material transforms = withTLike mesh (\m -> withTLike material (\mat -> withALikeLen transforms (\size t -> c'drawMeshInstanced m mat t (fromIntegral size))))

exportMesh :: (PLike Mesh mesh, StringLike string) => mesh -> string -> IO Bool
exportMesh mesh fileName = toBool <$> withTLike mesh (withTLike fileName . c'exportMesh)

exportMeshAsCode :: (PLike Mesh mesh, StringLike string) => mesh -> string -> IO Bool
exportMeshAsCode mesh fileName = toBool <$> withTLike mesh (withTLike fileName . c'exportMeshAsCode)

getMeshBoundingBox :: (PLike Mesh mesh) => mesh -> IO BoundingBox
getMeshBoundingBox mesh = withTLike mesh c'getMeshBoundingBox >>= pop

genMeshTangents :: (PLike Mesh mesh, Mutable mesh mut) => mesh -> IO mut
genMeshTangents mesh = withTLike mesh (\m -> c'genMeshTangents m >> peekMutated mesh m)

genMeshPoly :: (PLike Mesh mesh) => Int -> Float -> IO mesh
genMeshPoly sides radius = c'genMeshPoly (fromIntegral sides) (realToFrac radius) >>= popTLike

genMeshPlane :: (PLike Mesh mesh) => Float -> Float -> Int -> Int -> IO mesh
genMeshPlane width _length resX resZ = c'genMeshPlane (realToFrac width) (realToFrac _length) (fromIntegral resX) (fromIntegral resZ) >>= popTLike

genMeshCube :: (PLike Mesh mesh) => Float -> Float -> Float -> IO mesh
genMeshCube width height _length = c'genMeshCube (realToFrac width) (realToFrac height) (realToFrac _length) >>= popTLike

genMeshSphere :: (PLike Mesh mesh) => Float -> Int -> Int -> IO mesh
genMeshSphere radius rings slices = c'genMeshSphere (realToFrac radius) (fromIntegral rings) (fromIntegral slices) >>= popTLike

genMeshHemiSphere :: (PLike Mesh mesh) => Float -> Int -> Int -> IO mesh
genMeshHemiSphere radius rings slices = c'genMeshHemiSphere (realToFrac radius) (fromIntegral rings) (fromIntegral slices) >>= popTLike

genMeshCylinder :: (PLike Mesh mesh) => Float -> Float -> Int -> IO mesh
genMeshCylinder radius height slices = c'genMeshCylinder (realToFrac radius) (realToFrac height) (fromIntegral slices) >>= popTLike

genMeshCone :: (PLike Mesh mesh) => Float -> Float -> Int -> IO mesh
genMeshCone radius height slices = c'genMeshCone (realToFrac radius) (realToFrac height) (fromIntegral slices) >>= popTLike

genMeshTorus :: (PLike Mesh mesh) => Float -> Float -> Int -> Int -> IO mesh
genMeshTorus radius size radSeg sides = c'genMeshTorus (realToFrac radius) (realToFrac size) (fromIntegral radSeg) (fromIntegral sides) >>= popTLike

genMeshKnot :: (PLike Mesh mesh) => Float -> Float -> Int -> Int -> IO mesh
genMeshKnot radius size radSeg sides = c'genMeshKnot (realToFrac radius) (realToFrac size) (fromIntegral radSeg) (fromIntegral sides) >>= popTLike

genMeshHeightmap :: (PLike Image image, PLike Mesh mesh) => image -> Vector3 -> IO mesh
genMeshHeightmap heightmap size = withTLike heightmap (withFreeable size . c'genMeshHeightmap) >>= popTLike

genMeshCubicmap :: (PLike Image image, PLike Mesh mesh) => image -> Vector3 -> IO mesh
genMeshCubicmap cubicmap cubeSize = withTLike cubicmap (withFreeable cubeSize . c'genMeshCubicmap) >>= popTLike

loadMaterials :: (StringLike string, PALike Material materials) => string -> IO materials
loadMaterials fileName =
  withTLike
    fileName
    ( \f ->
        withFreeable
          0
          ( \n -> do
              ptr <- c'loadMaterials f n
              num <- peek n
              popALike (fromIntegral num) ptr
          )
    )

-- | Unloads a `managed` material from GPU memory (VRAM)
unloadMaterial :: (PLike Material material) => material -> WindowResources -> IO ()
unloadMaterial material wr =
  withTLike
    material
    ( \materialPtr -> do
        sId <- peek (p'shader'id (p'material'shader materialPtr))
        unloadSingleShader sId wr

        maps <- peek (p'material'maps materialPtr)
        if maps == nullPtr
          then return ()
          else
            forM_
              [0 .. 11]
              ( \i -> do
                  tId <- peek (p'texture'id (p'materialMap'texture (advancePtr maps i)))
                  unloadSingleTexture tId wr
              )
    )

loadMaterialDefault :: (PLike Material material) => IO material
loadMaterialDefault = c'loadMaterialDefault >>= popTLike

isMaterialValid :: (PLike Material material) => material -> IO Bool
isMaterialValid material = toBool <$> withTLike material c'isMaterialValid

setMaterialTexture :: (PLike Material material, PLike Texture texture) => material -> MaterialMapIndex -> texture -> IO material
setMaterialTexture material mapType texture = withTLike material (\m -> withTLike texture (c'setMaterialTexture m (fromIntegral (fromEnum mapType))) >> peekTLike m)

setModelMeshMaterial :: (PLike Model model) => model -> Int -> Int -> IO model
setModelMeshMaterial model meshId materialId = withTLike model (\m -> c'setModelMeshMaterial m (fromIntegral meshId) (fromIntegral materialId) >> peekTLike m)

loadModelAnimations :: (StringLike string, PALike ModelAnimation modelAnimations) => string -> IO modelAnimations
loadModelAnimations fileName =
  withTLike
    fileName
    ( \f ->
        withFreeable
          0
          ( \n -> do
              ptr <- c'loadModelAnimations f n
              num <- peek n
              peekALike (fromIntegral num) ptr
          )
    )

updateModelAnimation :: (PLike Model model, PLike ModelAnimation modelAnimation) => model -> modelAnimation -> Int -> IO ()
updateModelAnimation model animation frame = withTLike model (\m -> withTLike animation (\a -> c'updateModelAnimation m a (fromIntegral frame)))

isModelAnimationValid :: (PLike Model model, PLike ModelAnimation modelAnimation) => model -> modelAnimation -> IO Bool
isModelAnimationValid model animation = toBool <$> withTLike model (withTLike animation . c'isModelAnimationValid)

updateModelAnimationBoneMatrices :: (PLike Model model, PLike ModelAnimation modelAnimation) => model -> modelAnimation -> Int -> IO ()
updateModelAnimationBoneMatrices model animation frame = withTLike model (\m -> withTLike animation (\a -> c'updateModelAnimationBoneMatrices m a (fromIntegral frame)))

checkCollisionSpheres :: Vector3 -> Float -> Vector3 -> Float -> Bool
checkCollisionSpheres center1 radius1 center2 radius2 = toBool $ unsafePerformIO (withFreeable center1 (\c1 -> withFreeable center2 (\c2 -> c'checkCollisionSpheres c1 (realToFrac radius1) c2 (realToFrac radius2))))

checkCollisionBoxes :: BoundingBox -> BoundingBox -> Bool
checkCollisionBoxes box1 box2 = toBool $ unsafePerformIO (withFreeable box1 (withFreeable box2 . c'checkCollisionBoxes))

checkCollisionBoxSphere :: BoundingBox -> Vector3 -> Float -> Bool
checkCollisionBoxSphere box center radius = toBool $ unsafePerformIO (withFreeable box (\b -> withFreeable center (\c -> c'checkCollisionBoxSphere b c (realToFrac radius))))

getRayCollisionSphere :: Ray -> Vector3 -> Float -> RayCollision
getRayCollisionSphere ray center radius = unsafePerformIO $ withFreeable ray (\r -> withFreeable center (\c -> c'getRayCollisionSphere r c (realToFrac radius))) >>= pop

getRayCollisionBox :: Ray -> BoundingBox -> RayCollision
getRayCollisionBox ray box = unsafePerformIO $ withFreeable ray (withFreeable box . c'getRayCollisionBox) >>= pop

getRayCollisionMesh :: Ray -> Mesh -> Matrix -> RayCollision
getRayCollisionMesh ray mesh transform = unsafePerformIO $ withFreeable ray (\r -> withFreeable mesh (withFreeable transform . c'getRayCollisionMesh r)) >>= pop

getRayCollisionTriangle :: Ray -> Vector3 -> Vector3 -> Vector3 -> RayCollision
getRayCollisionTriangle ray v1 v2 v3 = unsafePerformIO $ withFreeable ray (\r -> withFreeable v1 (\p1 -> withFreeable v2 (withFreeable v3 . c'getRayCollisionTriangle r p1))) >>= pop

getRayCollisionQuad :: Ray -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> RayCollision
getRayCollisionQuad ray v1 v2 v3 v4 = unsafePerformIO $ withFreeable ray (\r -> withFreeable v1 (\p1 -> withFreeable v2 (\p2 -> withFreeable v3 (withFreeable v4 . c'getRayCollisionQuad r p1 p2)))) >>= pop
