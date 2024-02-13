{-# OPTIONS -Wall #-}

module Raylib.Core.Models where

import Control.Monad (forM_)
import Foreign
  ( Ptr,
    Storable (peek, poke),
    castPtr,
    fromBool,
    malloc,
    peekArray,
    toBool,
  )
import Foreign.C (withCString)
import GHC.IO (unsafePerformIO)
import Raylib.Internal.Foreign
  ( c'free,
    pop,
    popCArray,
    withFreeable,
    withFreeableArray,
    withFreeableArrayLen,
  )
import Raylib.Internal (WindowResources, addShaderId, addTextureId, addVaoId, addVboIds, unloadSingleShader, unloadSingleTexture, unloadSingleVaoId, unloadSingleVboIdList)
import Raylib.Internal.Native
  ( c'checkCollisionBoxSphere,
    c'checkCollisionBoxes,
    c'checkCollisionSpheres,
    c'drawBillboard,
    c'drawBillboardPro,
    c'drawBillboardRec,
    c'drawBoundingBox,
    c'drawCapsule,
    c'drawCapsuleWires,
    c'drawCircle3D,
    c'drawCube,
    c'drawCubeV,
    c'drawCubeWires,
    c'drawCubeWiresV,
    c'drawCylinder,
    c'drawCylinderEx,
    c'drawCylinderWires,
    c'drawCylinderWiresEx,
    c'drawGrid,
    c'drawLine3D,
    c'drawMesh,
    c'drawMeshInstanced,
    c'drawModel,
    c'drawModelEx,
    c'drawModelWires,
    c'drawModelWiresEx,
    c'drawPlane,
    c'drawPoint3D,
    c'drawRay,
    c'drawSphere,
    c'drawSphereEx,
    c'drawSphereWires,
    c'drawTriangle3D,
    c'drawTriangleStrip3D,
    c'exportMesh,
    c'genMeshCone,
    c'genMeshCube,
    c'genMeshCubicmap,
    c'genMeshCylinder,
    c'genMeshHeightmap,
    c'genMeshHemiSphere,
    c'genMeshKnot,
    c'genMeshPlane,
    c'genMeshPoly,
    c'genMeshSphere,
    c'genMeshTangents,
    c'genMeshTorus,
    c'getMeshBoundingBox,
    c'getModelBoundingBox,
    c'getRayCollisionBox,
    c'getRayCollisionMesh,
    c'getRayCollisionQuad,
    c'getRayCollisionSphere,
    c'getRayCollisionTriangle,
    c'isMaterialReady,
    c'isModelAnimationValid,
    c'isModelReady,
    c'loadMaterialDefault,
    c'loadMaterials,
    c'loadModel,
    c'loadModelAnimations,
    c'loadModelFromMesh,
    c'setMaterialTexture,
    c'setModelMeshMaterial,
    c'updateMeshBuffer,
    c'updateModelAnimation,
    c'uploadMesh, c'exportMeshAsCode,
  )
import Raylib.Types
  ( BoundingBox,
    Camera3D,
    Color,
    Image,
    Material (material'maps, material'shader),
    MaterialMap (materialMap'texture),
    Matrix,
    Mesh (mesh'vaoId, mesh'vboId),
    Model (model'materials, model'meshes),
    ModelAnimation,
    Ray,
    RayCollision,
    Rectangle,
    Shader (shader'id),
    Texture (texture'id),
    Vector2,
    Vector3,
  )
import Prelude hiding (length)

drawLine3D :: Vector3 -> Vector3 -> Color -> IO ()
drawLine3D start end color = withFreeable start (\s -> withFreeable end (withFreeable color . c'drawLine3D s))

drawPoint3D :: Vector3 -> Color -> IO ()
drawPoint3D point color = withFreeable point (withFreeable color . c'drawPoint3D)

drawCircle3D :: Vector3 -> Float -> Vector3 -> Float -> Color -> IO ()
drawCircle3D center radius rotationAxis rotationAngle color = withFreeable center (\c -> withFreeable rotationAxis (\r -> withFreeable color (c'drawCircle3D c (realToFrac radius) r (realToFrac rotationAngle))))

drawTriangle3D :: Vector3 -> Vector3 -> Vector3 -> Color -> IO ()
drawTriangle3D v1 v2 v3 color = withFreeable v1 (\p1 -> withFreeable v2 (\p2 -> withFreeable v3 (withFreeable color . c'drawTriangle3D p1 p2)))

drawTriangleStrip3D :: [Vector3] -> Int -> Color -> IO ()
drawTriangleStrip3D points pointCount color = withFreeableArray points (\p -> withFreeable color (c'drawTriangleStrip3D p (fromIntegral pointCount)))

drawCube :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCube position width height length color = withFreeable position (\p -> withFreeable color (c'drawCube p (realToFrac width) (realToFrac height) (realToFrac length)))

drawCubeV :: Vector3 -> Vector3 -> Color -> IO ()
drawCubeV position size color = withFreeable position (\p -> withFreeable size (withFreeable color . c'drawCubeV p))

drawCubeWires :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeWires position width height length color = withFreeable position (\p -> withFreeable color (c'drawCubeWires p (realToFrac width) (realToFrac height) (realToFrac length)))

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

loadModel :: String -> WindowResources -> IO Model
loadModel fileName wr = do
  model <- withCString fileName c'loadModel >>= pop
  forM_ (model'meshes model) (`storeMeshData` wr)
  storeMaterialData (model'materials model) wr
  return model

loadModelFromMesh :: Mesh -> WindowResources -> IO Model
loadModelFromMesh mesh wr = do
  meshPtr <- malloc
  poke meshPtr mesh
  model <- c'loadModelFromMesh meshPtr >>= pop
  c'free $ castPtr meshPtr
  storeMaterialData (model'materials model) wr
  return model

-- | Unloads a model from GPU memory (VRAM). This unloads its associated
-- meshes and materials. Models are automatically unloaded when `Raylib.Core.closeWindow`
-- is called, so manually unloading models is not required. In larger projects,
-- you may want to manually unload models to avoid having them in VRAM for too
-- long.
unloadModel :: Model -> WindowResources -> IO ()
unloadModel model wr = do
  forM_ (model'meshes model) (`unloadMesh` wr)
  forM_ (model'materials model) (`unloadMaterial` wr)

isModelReady :: Model -> IO Bool
isModelReady model = toBool <$> withFreeable model c'isModelReady

getModelBoundingBox :: Model -> IO BoundingBox
getModelBoundingBox model = withFreeable model c'getModelBoundingBox >>= pop

drawModel :: Model -> Vector3 -> Float -> Color -> IO ()
drawModel model position scale tint = withFreeable model (\m -> withFreeable position (\p -> withFreeable tint (c'drawModel m p (realToFrac scale))))

drawModelEx :: Model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelEx model position rotationAxis rotationAngle scale tint = withFreeable model (\m -> withFreeable position (\p -> withFreeable rotationAxis (\r -> withFreeable scale (withFreeable tint . c'drawModelEx m p r (realToFrac rotationAngle)))))

drawModelWires :: Model -> Vector3 -> Float -> Color -> IO ()
drawModelWires model position scale tint = withFreeable model (\m -> withFreeable position (\p -> withFreeable tint (c'drawModelWires m p (realToFrac scale))))

drawModelWiresEx :: Model -> Vector3 -> Vector3 -> Float -> Vector3 -> Color -> IO ()
drawModelWiresEx model position rotationAxis rotationAngle scale tint = withFreeable model (\m -> withFreeable position (\p -> withFreeable rotationAxis (\r -> withFreeable scale (withFreeable tint . c'drawModelWiresEx m p r (realToFrac rotationAngle)))))

drawBoundingBox :: BoundingBox -> Color -> IO ()
drawBoundingBox box color = withFreeable box (withFreeable color . c'drawBoundingBox)

drawBillboard :: Camera3D -> Texture -> Vector3 -> Float -> Color -> IO ()
drawBillboard camera texture position size tint = withFreeable camera (\c -> withFreeable texture (\t -> withFreeable position (\p -> withFreeable tint (c'drawBillboard c t p (realToFrac size)))))

drawBillboardRec :: Camera3D -> Texture -> Rectangle -> Vector3 -> Vector2 -> Color -> IO ()
drawBillboardRec camera texture source position size tint = withFreeable camera (\c -> withFreeable texture (\t -> withFreeable source (\s -> withFreeable position (\p -> withFreeable size (withFreeable tint . c'drawBillboardRec c t s p)))))

drawBillboardPro :: Camera3D -> Texture -> Rectangle -> Vector3 -> Vector3 -> Vector2 -> Vector2 -> Float -> Color -> IO ()
drawBillboardPro camera texture source position up size origin rotation tint = withFreeable camera (\c -> withFreeable texture (\t -> withFreeable source (\s -> withFreeable position (\p -> withFreeable up (\u -> withFreeable size (\sz -> withFreeable origin (\o -> withFreeable tint (c'drawBillboardPro c t s p u sz o (realToFrac rotation)))))))))

uploadMesh :: Mesh -> Bool -> WindowResources -> IO Mesh
uploadMesh mesh dynamic wr = withFreeable mesh (\m -> c'uploadMesh m (fromBool dynamic) >> peek m >>= (`storeMeshData` wr))

updateMeshBuffer :: Mesh -> Int -> Ptr () -> Int -> Int -> IO ()
updateMeshBuffer mesh index dataValue dataSize offset = withFreeable mesh (\m -> c'updateMeshBuffer m (fromIntegral index) dataValue (fromIntegral dataSize) (fromIntegral offset))

-- | Unloads a mesh from GPU memory (VRAM). Meshes are
-- automatically unloaded when `Raylib.Core.closeWindow` is called, so manually unloading
-- meshes is not required. In larger projects, you may want to
-- manually unload meshes to avoid having them in VRAM for too long.
unloadMesh :: Mesh -> WindowResources -> IO ()
unloadMesh mesh wr = do
  unloadSingleVaoId (mesh'vaoId mesh) wr
  unloadSingleVboIdList (mesh'vboId mesh) wr

-- Internal
storeMeshData :: Mesh -> WindowResources -> IO Mesh
storeMeshData mesh wr = do
  addVaoId (mesh'vaoId mesh) wr
  addVboIds (mesh'vboId mesh) wr
  return mesh

drawMesh :: Mesh -> Material -> Matrix -> IO ()
drawMesh mesh material transform = withFreeable mesh (\m -> withFreeable material (withFreeable transform . c'drawMesh m))

drawMeshInstanced :: Mesh -> Material -> [Matrix] -> IO ()
drawMeshInstanced mesh material transforms = withFreeable mesh (\m -> withFreeable material (\mat -> withFreeableArrayLen transforms (\size t -> c'drawMeshInstanced m mat t (fromIntegral size))))

exportMesh :: Mesh -> String -> IO Bool
exportMesh mesh fileName = toBool <$> withFreeable mesh (withCString fileName . c'exportMesh)

exportMeshAsCode :: Mesh -> String -> IO Bool
exportMeshAsCode mesh fileName = toBool <$> withFreeable mesh (withCString fileName . c'exportMeshAsCode)

getMeshBoundingBox :: Mesh -> IO BoundingBox
getMeshBoundingBox mesh = withFreeable mesh c'getMeshBoundingBox >>= pop

genMeshTangents :: Mesh -> IO Mesh
genMeshTangents mesh = withFreeable mesh (\m -> c'genMeshTangents m >> peek m)

genMeshPoly :: Int -> Float -> WindowResources -> IO Mesh
genMeshPoly sides radius wr = c'genMeshPoly (fromIntegral sides) (realToFrac radius) >>= pop >>= (`storeMeshData` wr)

genMeshPlane :: Float -> Float -> Int -> Int -> WindowResources -> IO Mesh
genMeshPlane width length resX resZ wr = c'genMeshPlane (realToFrac width) (realToFrac length) (fromIntegral resX) (fromIntegral resZ) >>= pop >>= (`storeMeshData` wr)

genMeshCube :: Float -> Float -> Float -> WindowResources -> IO Mesh
genMeshCube width height length wr = c'genMeshCube (realToFrac width) (realToFrac height) (realToFrac length) >>= pop >>= (`storeMeshData` wr)

genMeshSphere :: Float -> Int -> Int -> WindowResources -> IO Mesh
genMeshSphere radius rings slices wr = c'genMeshSphere (realToFrac radius) (fromIntegral rings) (fromIntegral slices) >>= pop >>= (`storeMeshData` wr)

genMeshHemiSphere :: Float -> Int -> Int -> WindowResources -> IO Mesh
genMeshHemiSphere radius rings slices wr = c'genMeshHemiSphere (realToFrac radius) (fromIntegral rings) (fromIntegral slices) >>= pop >>= (`storeMeshData` wr)

genMeshCylinder :: Float -> Float -> Int -> WindowResources -> IO Mesh
genMeshCylinder radius height slices wr = c'genMeshCylinder (realToFrac radius) (realToFrac height) (fromIntegral slices) >>= pop >>= (`storeMeshData` wr)

genMeshCone :: Float -> Float -> Int -> WindowResources -> IO Mesh
genMeshCone radius height slices wr = c'genMeshCone (realToFrac radius) (realToFrac height) (fromIntegral slices) >>= pop >>= (`storeMeshData` wr)

genMeshTorus :: Float -> Float -> Int -> Int -> WindowResources -> IO Mesh
genMeshTorus radius size radSeg sides wr = c'genMeshTorus (realToFrac radius) (realToFrac size) (fromIntegral radSeg) (fromIntegral sides) >>= pop >>= (`storeMeshData` wr)

genMeshKnot :: Float -> Float -> Int -> Int -> WindowResources -> IO Mesh
genMeshKnot radius size radSeg sides wr = c'genMeshKnot (realToFrac radius) (realToFrac size) (fromIntegral radSeg) (fromIntegral sides) >>= pop >>= (`storeMeshData` wr)

genMeshHeightmap :: Image -> Vector3 -> WindowResources -> IO Mesh
genMeshHeightmap heightmap size wr = withFreeable heightmap (withFreeable size . c'genMeshHeightmap) >>= pop >>= (`storeMeshData` wr)

genMeshCubicmap :: Image -> Vector3 -> WindowResources -> IO Mesh
genMeshCubicmap cubicmap cubeSize wr = withFreeable cubicmap (withFreeable cubeSize . c'genMeshCubicmap) >>= pop >>= (`storeMeshData` wr)

loadMaterials :: String -> WindowResources -> IO [Material]
loadMaterials fileName wr =
  withCString
    fileName
    ( \f ->
        withFreeable
          0
          ( \n -> do
              ptr <- c'loadMaterials f n
              num <- peek n
              materials <- popCArray (fromIntegral num) ptr
              storeMaterialData materials wr
              return materials
          )
    )

-- Internal
storeMaterialData :: [Material] -> WindowResources -> IO ()
storeMaterialData materials wr =
  forM_
    materials
    ( \mat -> do
        addShaderId (shader'id $ material'shader mat) wr
        case material'maps mat of
          Nothing -> return ()
          (Just maps) -> forM_ maps (\m -> addTextureId (texture'id $ materialMap'texture m) wr)
    )

-- | Unloads a material from GPU memory (VRAM). Materials are
-- automatically unloaded when `Raylib.Core.closeWindow` is called, so manually unloading
-- materials is not required. In larger projects, you may want to
-- manually unload materials to avoid having them in VRAM for too long.
unloadMaterial :: Material -> WindowResources -> IO ()
unloadMaterial material wr = do
  unloadSingleShader (shader'id $ material'shader material) wr
  case material'maps material of
    Nothing -> return ()
    (Just maps) -> forM_ maps (\m -> unloadSingleTexture (texture'id $ materialMap'texture m) wr)

loadMaterialDefault :: IO Material
loadMaterialDefault = c'loadMaterialDefault >>= pop

isMaterialReady :: Material -> IO Bool
isMaterialReady material = toBool <$> withFreeable material c'isMaterialReady

setMaterialTexture :: Material -> Int -> Texture -> IO Material
setMaterialTexture material mapType texture = withFreeable material (\m -> withFreeable texture (c'setMaterialTexture m (fromIntegral mapType)) >> peek m)

setModelMeshMaterial :: Model -> Int -> Int -> IO Model
setModelMeshMaterial model meshId materialId = withFreeable model (\m -> c'setModelMeshMaterial m (fromIntegral meshId) (fromIntegral materialId) >> peek m)

loadModelAnimations :: String -> IO [ModelAnimation]
loadModelAnimations fileName =
  withCString
    fileName
    ( \f ->
        withFreeable
          0
          ( \n -> do
              ptr <- c'loadModelAnimations f n
              num <- peek n
              peekArray (fromIntegral num) ptr
          )
    )

updateModelAnimation :: Model -> ModelAnimation -> Int -> IO ()
updateModelAnimation model animation frame = withFreeable model (\m -> withFreeable animation (\a -> c'updateModelAnimation m a (fromIntegral frame)))

isModelAnimationValid :: Model -> ModelAnimation -> IO Bool
isModelAnimationValid model animation = toBool <$> withFreeable model (withFreeable animation . c'isModelAnimationValid)

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
