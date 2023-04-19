{-# OPTIONS -Wall #-}

module Raylib.Util.Lenses where

import Control.Lens (Lens')
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr)
import qualified Raylib.Types as RL



_vector2'x :: Lens' RL.Vector2 Float
_vector2'x f (RL.Vector2 x y) = (\x' -> RL.Vector2 x' y) <$> f x
_vector2'y :: Lens' RL.Vector2 Float
_vector2'y f (RL.Vector2 x y) = (\y' -> RL.Vector2 x y') <$> f y


_vector3'x :: Lens' RL.Vector3 Float
_vector3'x f (RL.Vector3 x y z) = (\x' -> RL.Vector3 x' y z) <$> f x
_vector3'y :: Lens' RL.Vector3 Float
_vector3'y f (RL.Vector3 x y z) = (\y' -> RL.Vector3 x y' z) <$> f y
_vector3'z :: Lens' RL.Vector3 Float
_vector3'z f (RL.Vector3 x y z) = (\z' -> RL.Vector3 x y z') <$> f z

_vector4'x :: Lens' RL.Vector4 Float
_vector4'x f (RL.Vector4 x y z w) = (\x' -> RL.Vector4 x' y z w) <$> f x
_vector4'y :: Lens' RL.Vector4 Float
_vector4'y f (RL.Vector4 x y z w) = (\y' -> RL.Vector4 x y' z w) <$> f y
_vector4'z :: Lens' RL.Vector4 Float
_vector4'z f (RL.Vector4 x y z w) = (\z' -> RL.Vector4 x y z' w) <$> f z
_vector4'w :: Lens' RL.Vector4 Float
_vector4'w f (RL.Vector4 x y z w) = (\w' -> RL.Vector4 x y z w') <$> f w


_matrix'm0 :: Lens' RL.Matrix Float
_matrix'm0 f matrix = (\m' -> matrix { RL.matrix'm0 = m' }) <$> f (RL.matrix'm0 matrix)
_matrix'm1 :: Lens' RL.Matrix Float
_matrix'm1 f matrix = (\m' -> matrix { RL.matrix'm1 = m' }) <$> f (RL.matrix'm1 matrix)
_matrix'm2 :: Lens' RL.Matrix Float
_matrix'm2 f matrix = (\m' -> matrix { RL.matrix'm2 = m' }) <$> f (RL.matrix'm2 matrix)
_matrix'm3 :: Lens' RL.Matrix Float
_matrix'm3 f matrix = (\m' -> matrix { RL.matrix'm3 = m' }) <$> f (RL.matrix'm3 matrix)
_matrix'm4 :: Lens' RL.Matrix Float
_matrix'm4 f matrix = (\m' -> matrix { RL.matrix'm4 = m' }) <$> f (RL.matrix'm4 matrix)
_matrix'm5 :: Lens' RL.Matrix Float
_matrix'm5 f matrix = (\m' -> matrix { RL.matrix'm5 = m' }) <$> f (RL.matrix'm5 matrix)
_matrix'm6 :: Lens' RL.Matrix Float
_matrix'm6 f matrix = (\m' -> matrix { RL.matrix'm6 = m' }) <$> f (RL.matrix'm6 matrix)
_matrix'm7 :: Lens' RL.Matrix Float
_matrix'm7 f matrix = (\m' -> matrix { RL.matrix'm7 = m' }) <$> f (RL.matrix'm7 matrix)
_matrix'm8 :: Lens' RL.Matrix Float
_matrix'm8 f matrix = (\m' -> matrix { RL.matrix'm8 = m' }) <$> f (RL.matrix'm8 matrix)
_matrix'm9 :: Lens' RL.Matrix Float
_matrix'm9 f matrix = (\m' -> matrix { RL.matrix'm9 = m' }) <$> f (RL.matrix'm9 matrix)
_matrix'm10 :: Lens' RL.Matrix Float
_matrix'm10 f matrix = (\m' -> matrix { RL.matrix'm10 = m' }) <$> f (RL.matrix'm10 matrix)
_matrix'm11 :: Lens' RL.Matrix Float
_matrix'm11 f matrix = (\m' -> matrix { RL.matrix'm11 = m' }) <$> f (RL.matrix'm11 matrix)
_matrix'm12 :: Lens' RL.Matrix Float
_matrix'm12 f matrix = (\m' -> matrix { RL.matrix'm12 = m' }) <$> f (RL.matrix'm12 matrix)
_matrix'm13 :: Lens' RL.Matrix Float
_matrix'm13 f matrix = (\m' -> matrix { RL.matrix'm13 = m' }) <$> f (RL.matrix'm13 matrix)
_matrix'm14 :: Lens' RL.Matrix Float
_matrix'm14 f matrix = (\m' -> matrix { RL.matrix'm14 = m' }) <$> f (RL.matrix'm14 matrix)
_matrix'm15 :: Lens' RL.Matrix Float
_matrix'm15 f matrix = (\m' -> matrix { RL.matrix'm15 = m' }) <$> f (RL.matrix'm15 matrix)


_color'r :: Lens' RL.Color Word8
_color'r f (RL.Color r g b a) = (\r' -> RL.Color r' g b a) <$> f r
_color'g :: Lens' RL.Color Word8
_color'g f (RL.Color r g b a) = (\g' -> RL.Color r g' b a) <$> f g
_color'b :: Lens' RL.Color Word8
_color'b f (RL.Color r g b a) = (\b' -> RL.Color r g b' a) <$> f b
_color'a :: Lens' RL.Color Word8
_color'a f (RL.Color r g b a) = (\a' -> RL.Color r g b a') <$> f a


_rectangle'x :: Lens' RL.Rectangle Float
_rectangle'x f (RL.Rectangle x y width height) = (\x' -> RL.Rectangle x' y width height) <$> f x
_rectangle'y :: Lens' RL.Rectangle Float
_rectangle'y f (RL.Rectangle x y width height) = (\y' -> RL.Rectangle x y' width height) <$> f y
_rectangle'width :: Lens' RL.Rectangle Float
_rectangle'width f (RL.Rectangle x y width height) = (\width' -> RL.Rectangle x y width' height) <$> f width
_rectangle'height :: Lens' RL.Rectangle Float
_rectangle'height f (RL.Rectangle x y width height) = (\height' -> RL.Rectangle x y width height') <$> f height


_image'data :: Lens' RL.Image [Word8]
_image'data f (RL.Image imgData width height mipmaps format) =
    (\imgData' -> RL.Image imgData' width height mipmaps format) <$> f imgData
_image'width :: Lens' RL.Image Int
_image'width f (RL.Image imgData width height mipmaps format) =
    (\width' -> RL.Image imgData width' height mipmaps format) <$> f width
_image'height :: Lens' RL.Image Int
_image'height f (RL.Image imgData width height mipmaps format) =
    (\height' -> RL.Image imgData width height' mipmaps format) <$> f height
_image'mipmaps :: Lens' RL.Image Int
_image'mipmaps f (RL.Image imgData width height mipmaps format) =
    (\mipmaps' -> RL.Image imgData width height mipmaps' format) <$> f mipmaps
_image'format :: Lens' RL.Image RL.PixelFormat
_image'format f (RL.Image imgData width height mipmaps format) =
    (\format' -> RL.Image imgData width height mipmaps format') <$> f format


_texture'id :: Lens' RL.Texture Integer
_texture'id f (RL.Texture ident width height mipmaps format) =
    (\ident' -> RL.Texture ident' width height mipmaps format) <$> f ident
_texture'width :: Lens' RL.Texture Int
_texture'width f (RL.Texture ident width height mipmaps format) =
    (\width' -> RL.Texture ident width' height mipmaps format) <$> f width
_texture'height :: Lens' RL.Texture Int
_texture'height f (RL.Texture ident width height mipmaps format) =
    (\height' -> RL.Texture ident width height' mipmaps format) <$> f height
_texture'mipmaps :: Lens' RL.Texture Int
_texture'mipmaps f (RL.Texture ident width height mipmaps format) =
    (\mipmaps' -> RL.Texture ident width height mipmaps' format) <$> f mipmaps
_texture'format :: Lens' RL.Texture RL.PixelFormat
_texture'format f (RL.Texture ident width height mipmaps format) =
    (\format' -> RL.Texture ident width height mipmaps format') <$> f format


_renderTexture'id :: Lens' RL.RenderTexture Integer
_renderTexture'id f (RL.RenderTexture ident texture depth) =
    (\ident' -> RL.RenderTexture ident' texture depth) <$> f ident
_renderTexture'texture :: Lens' RL.RenderTexture RL.Texture
_renderTexture'texture f (RL.RenderTexture ident texture depth) =
    (\texture' -> RL.RenderTexture ident texture' depth) <$> f texture
_renderTexture'depth :: Lens' RL.RenderTexture RL.Texture
_renderTexture'depth f (RL.RenderTexture ident texture depth) =
    (\depth' -> RL.RenderTexture ident texture depth') <$> f depth


_nPatchInfor'source :: Lens' RL.NPatchInfo RL.Rectangle
_nPatchInfor'source f (RL.NPatchInfo source left top right bottom layout) =
    (\source' -> RL.NPatchInfo source' left top right bottom layout) <$> f source
_nPatchInfor'left :: Lens' RL.NPatchInfo Int
_nPatchInfor'left f (RL.NPatchInfo source left top right bottom layout) =
    (\left' -> RL.NPatchInfo source left' top right bottom layout) <$> f left
_nPatchInfor'top :: Lens' RL.NPatchInfo Int
_nPatchInfor'top f (RL.NPatchInfo source left top right bottom layout) =
    (\top' -> RL.NPatchInfo source left top' right bottom layout) <$> f top
_nPatchInfor'right :: Lens' RL.NPatchInfo Int
_nPatchInfor'right f (RL.NPatchInfo source left top right bottom layout) =
    (\right' -> RL.NPatchInfo source left top right' bottom layout) <$> f right
_nPatchInfor'bottom :: Lens' RL.NPatchInfo Int
_nPatchInfor'bottom f (RL.NPatchInfo source left top right bottom layout) =
    (\bottom' -> RL.NPatchInfo source left top right bottom' layout) <$> f bottom
_nPatchInfor'layout :: Lens' RL.NPatchInfo RL.NPatchLayout
_nPatchInfor'layout f (RL.NPatchInfo source left top right bottom layout) =
    (\layout' -> RL.NPatchInfo source left top right bottom layout') <$> f layout


_glyphInfo'value :: Lens' RL.GlyphInfo Int
_glyphInfo'value f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\value' -> RL.GlyphInfo value' offsetX offsetY advanceX image) <$> f value
_glyphInfo'offsetX :: Lens' RL.GlyphInfo Int
_glyphInfo'offsetX f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\offsetX' -> RL.GlyphInfo value offsetX' offsetY advanceX image) <$> f offsetX
_glyphInfo'offsetY :: Lens' RL.GlyphInfo Int
_glyphInfo'offsetY f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\offsetY' -> RL.GlyphInfo value offsetX offsetY' advanceX image) <$> f offsetY
_glyphInfo'advanceX :: Lens' RL.GlyphInfo Int
_glyphInfo'advanceX f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\advanceX' -> RL.GlyphInfo value offsetX offsetY advanceX' image) <$> f advanceX
_glyphInfo'image :: Lens' RL.GlyphInfo RL.Image
_glyphInfo'image f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\image' -> RL.GlyphInfo value offsetX offsetY advanceX image') <$> f image


_font'baseSize :: Lens' RL.Font Int
_font'baseSize f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\baseSize' -> RL.Font baseSize' glyphCount glyphPadding texture recs glyphs) <$> f baseSize
_font'glyphCount :: Lens' RL.Font Int
_font'glyphCount f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\glyphCount' -> RL.Font baseSize glyphCount' glyphPadding texture recs glyphs) <$> f glyphCount
_font'glyphPadding :: Lens' RL.Font Int
_font'glyphPadding f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\glyphPadding' -> RL.Font baseSize glyphCount glyphPadding' texture recs glyphs) <$> f glyphPadding
_font'texture :: Lens' RL.Font RL.Texture
_font'texture f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\texture' -> RL.Font baseSize glyphCount glyphPadding texture' recs glyphs) <$> f texture
_font'recs :: Lens' RL.Font [RL.Rectangle]
_font'recs f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\recs' -> RL.Font baseSize glyphCount glyphPadding texture recs' glyphs) <$> f recs
_font'glyphs :: Lens' RL.Font [RL.GlyphInfo]
_font'glyphs f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\glyphs' -> RL.Font baseSize glyphCount glyphPadding texture recs glyphs') <$> f glyphs


_camera3D'position :: Lens' RL.Camera3D RL.Vector3
_camera3D'position f (RL.Camera3D position target up fovy projection) =
    (\position' -> RL.Camera3D position' target up fovy projection) <$> f position
_camera3D'target :: Lens' RL.Camera3D RL.Vector3
_camera3D'target f (RL.Camera3D position target up fovy projection) =
    (\target' -> RL.Camera3D position target' up fovy projection) <$> f target
_camera3Dup' :: Lens' RL.Camera3D RL.Vector3
_camera3Dup' f (RL.Camera3D position target up fovy projection) =
    (\up' -> RL.Camera3D position target up' fovy projection) <$> f up
_camera3D'fovy :: Lens' RL.Camera3D Float
_camera3D'fovy f (RL.Camera3D position target up fovy projection) =
    (\fovy' -> RL.Camera3D position target up fovy' projection) <$> f fovy
_camera3D'projection :: Lens' RL.Camera3D RL.CameraProjection
_camera3D'projection f (RL.Camera3D position target up fovy projection) =
    (\projection' -> RL.Camera3D position target up fovy projection') <$> f projection


_camera2D'offset :: Lens' RL.Camera2D RL.Vector2
_camera2D'offset f (RL.Camera2D offset target rotation zoom) =
    (\offset' -> RL.Camera2D offset' target rotation zoom) <$> f offset
_camera2D'target :: Lens' RL.Camera2D RL.Vector2
_camera2D'target f (RL.Camera2D offset target rotation zoom) =
    (\target' -> RL.Camera2D offset target' rotation zoom) <$> f target
_camera2D'rotation :: Lens' RL.Camera2D Float
_camera2D'rotation f (RL.Camera2D offset target rotation zoom) =
    (\rotation' -> RL.Camera2D offset target rotation' zoom) <$> f rotation
_camera2D'zoom :: Lens' RL.Camera2D Float
_camera2D'zoom f (RL.Camera2D offset target rotation zoom) =
    (\zoom' -> RL.Camera2D offset target rotation zoom') <$> f zoom


_mesh'vertexCount :: Lens' RL.Mesh Int
_mesh'vertexCount f mesh =
    (\vertexCount' -> mesh { RL.mesh'vertexCount = vertexCount' }) <$>
    f (RL.mesh'vertexCount mesh)
_mesh'triangleCount :: Lens' RL.Mesh Int
_mesh'triangleCount f mesh =
    (\triangleCount' -> mesh { RL.mesh'triangleCount = triangleCount' }) <$>
    f (RL.mesh'triangleCount mesh)
_mesh'vertices :: Lens' RL.Mesh [RL.Vector3]
_mesh'vertices f mesh =
    (\vertices' -> mesh { RL.mesh'vertices = vertices' }) <$>
    f (RL.mesh'vertices mesh)
_mesh'texcoords :: Lens' RL.Mesh [RL.Vector2]
_mesh'texcoords f mesh =
    (\texcoords' -> mesh { RL.mesh'texcoords = texcoords' }) <$>
    f (RL.mesh'texcoords mesh)
_mesh'texcoords2 :: Lens' RL.Mesh (Maybe [RL.Vector2])
_mesh'texcoords2 f mesh =
    (\texcoords2' -> mesh { RL.mesh'texcoords2 = texcoords2' }) <$>
    f (RL.mesh'texcoords2 mesh)
_mesh'normals :: Lens' RL.Mesh [RL.Vector3]
_mesh'normals f mesh =
    (\normals' -> mesh { RL.mesh'normals = normals' }) <$>
    f (RL.mesh'normals mesh)
_mesh'tangents :: Lens' RL.Mesh (Maybe [RL.Vector4])
_mesh'tangents f mesh =
    (\tangents' -> mesh { RL.mesh'tangents = tangents' }) <$>
    f (RL.mesh'tangents mesh)
_mesh'colors :: Lens' RL.Mesh (Maybe [RL.Color])
_mesh'colors f mesh =
    (\colors' -> mesh { RL.mesh'colors = colors' }) <$>
    f (RL.mesh'colors mesh)
_mesh'indices :: Lens' RL.Mesh (Maybe [Word16])
_mesh'indices f mesh =
    (\indices' -> mesh { RL.mesh'indices = indices' }) <$>
    f (RL.mesh'indices mesh)
_mesh'animVertices :: Lens' RL.Mesh (Maybe [RL.Vector3])
_mesh'animVertices f mesh =
    (\animVertices' -> mesh { RL.mesh'animVertices = animVertices' }) <$>
    f (RL.mesh'animVertices mesh)
_mesh'animNormals :: Lens' RL.Mesh (Maybe [RL.Vector3])
_mesh'animNormals f mesh =
    (\animNormals' -> mesh { RL.mesh'animNormals = animNormals' }) <$>
    f (RL.mesh'animNormals mesh)
_mesh'boneIds :: Lens' RL.Mesh (Maybe [Word8])
_mesh'boneIds f mesh =
    (\boneIds' -> mesh { RL.mesh'boneIds = boneIds' }) <$>
    f (RL.mesh'boneIds mesh)
_mesh'boneWeights :: Lens' RL.Mesh (Maybe [Float])
_mesh'boneWeights f mesh =
    (\boneWeights' -> mesh { RL.mesh'boneWeights = boneWeights' }) <$>
    f (RL.mesh'boneWeights mesh)
_mesh'vaoId :: Lens' RL.Mesh Integer
_mesh'vaoId f mesh =
    (\vaoId' -> mesh { RL.mesh'vaoId = vaoId' }) <$>
    f (RL.mesh'vaoId mesh)
_mesh'vboId :: Lens' RL.Mesh (Maybe [Integer])
_mesh'vboId f mesh =
    (\vboId' -> mesh { RL.mesh'vboId = vboId' }) <$>
    f (RL.mesh'vboId mesh)


_shader'id :: Lens' RL.Shader Integer
_shader'id f (RL.Shader ident locs) = (\ident' -> RL.Shader ident' locs) <$> f ident
_shader'locs :: Lens' RL.Shader [Int]
_shader'locs f (RL.Shader ident locs) = (\locs' -> RL.Shader ident locs') <$> f locs


_materialMap'texture :: Lens' RL.MaterialMap RL.Texture
_materialMap'texture f (RL.MaterialMap texture color value) =
    (\texture' -> RL.MaterialMap texture' color value) <$> f texture
_materialMap'color :: Lens' RL.MaterialMap RL.Color
_materialMap'color f (RL.MaterialMap texture color value) =
    (\color' -> RL.MaterialMap texture color' value) <$> f color
_materialMap'value :: Lens' RL.MaterialMap Float
_materialMap'value f (RL.MaterialMap texture color value) =
    (\value' -> RL.MaterialMap texture color value') <$> f value


_material'shader :: Lens' RL.Material RL.Shader
_material'shader f (RL.Material shader maps params) =
    (\shader' -> RL.Material shader' maps params) <$> f shader
_material'maps :: Lens' RL.Material (Maybe [RL.MaterialMap])
_material'maps f (RL.Material shader maps params) =
    (\maps' -> RL.Material shader maps' params) <$> f maps
_material'params :: Lens' RL.Material [Float]
_material'params f (RL.Material shader maps params) =
    (\params' -> RL.Material shader maps params') <$> f params


_transform'translation :: Lens' RL.Transform RL.Vector3
_transform'translation f (RL.Transform translation rotation scale) =
    (\translation' -> RL.Transform translation' rotation scale) <$> f translation
_transform'rotation :: Lens' RL.Transform RL.Quaternion
_transform'rotation f (RL.Transform translation rotation scale) =
    (\rotation' -> RL.Transform translation rotation' scale) <$> f rotation
_transform'scale :: Lens' RL.Transform RL.Vector3
_transform'scale f (RL.Transform translation rotation scale) =
    (\scale' -> RL.Transform translation rotation scale') <$> f scale


_boneInfo'name :: Lens' RL.BoneInfo String
_boneInfo'name f (RL.BoneInfo name parent) = (\name' -> RL.BoneInfo name' parent) <$> f name
_boneInfo'parent :: Lens' RL.BoneInfo Int
_boneInfo'parent f (RL.BoneInfo name parent) = (\parent' -> RL.BoneInfo name parent') <$> f parent


_model'transform :: Lens' RL.Model RL.Matrix
_model'transform f model =
    (\transform' -> model { RL.model'transform = transform' }) <$> f (RL.model'transform model)
_model'meshes :: Lens' RL.Model [RL.Mesh]
_model'meshes f model =
    (\meshes' -> model { RL.model'meshes = meshes' }) <$> f (RL.model'meshes model)
_model'materials :: Lens' RL.Model [RL.Material]
_model'materials f model =
    (\materials' -> model { RL.model'materials = materials' }) <$> f (RL.model'materials model)
_model'meshMaterial :: Lens' RL.Model [Int]
_model'meshMaterial f model =
    (\meshMaterial' -> model { RL.model'meshMaterial = meshMaterial' }) <$> f (RL.model'meshMaterial model)
_model'boneCount :: Lens' RL.Model Int
_model'boneCount f model =
    (\boneCount' -> model { RL.model'boneCount = boneCount' }) <$> f (RL.model'boneCount model)
_model'bones :: Lens' RL.Model (Maybe [RL.BoneInfo])
_model'bones f model =
    (\bones' -> model { RL.model'bones = bones' }) <$> f (RL.model'bones model)
_model'bindPose :: Lens' RL.Model (Maybe [RL.Transform])
_model'bindPose f model =
    (\bindPose' -> model { RL.model'bindPose = bindPose' }) <$> f (RL.model'bindPose model)


_modelAnimation'boneCount :: Lens' RL.ModelAnimation Int
_modelAnimation'boneCount f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\boneCount' -> RL.ModelAnimation boneCount' frameCount bones framePoses) <$> f boneCount
_modelAnimation'frameCount :: Lens' RL.ModelAnimation Int
_modelAnimation'frameCount f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\frameCount' -> RL.ModelAnimation boneCount frameCount' bones framePoses) <$> f frameCount
_modelAnimation'bones :: Lens' RL.ModelAnimation [RL.BoneInfo]
_modelAnimation'bones f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\bones' -> RL.ModelAnimation boneCount frameCount bones' framePoses) <$> f bones
_modelAnimation'framePoses :: Lens' RL.ModelAnimation [[RL.Transform]]
_modelAnimation'framePoses f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\framePoses' -> RL.ModelAnimation boneCount frameCount bones framePoses') <$> f framePoses


_ray'position :: Lens' RL.Ray RL.Vector3
_ray'position f (RL.Ray position direction) = (\position' -> RL.Ray position' direction) <$> f position
_ray'direction :: Lens' RL.Ray RL.Vector3
_ray'direction f (RL.Ray position direction) = (\direction' -> RL.Ray position direction') <$> f direction


_rayCollision'hit :: Lens' RL.RayCollision Bool
_rayCollision'hit f (RL.RayCollision hit distance point normal) =
    (\hit' -> RL.RayCollision hit' distance point normal) <$> f hit
_rayCollision'distance :: Lens' RL.RayCollision Float
_rayCollision'distance f (RL.RayCollision hit distance point normal) =
    (\distance' -> RL.RayCollision hit distance' point normal) <$> f distance
_rayCollision'point :: Lens' RL.RayCollision RL.Vector3
_rayCollision'point f (RL.RayCollision hit distance point normal) =
    (\point' -> RL.RayCollision hit distance point' normal) <$> f point
_rayCollision'normal :: Lens' RL.RayCollision RL.Vector3
_rayCollision'normal f (RL.RayCollision hit distance point normal) =
    (\normal' -> RL.RayCollision hit distance point normal') <$> f normal


_boundingBox'min :: Lens' RL.BoundingBox RL.Vector3
_boundingBox'min f (RL.BoundingBox bbMin bbMax) = (\bbMin' -> RL.BoundingBox bbMin' bbMax) <$> f bbMin
_boundingBox'max :: Lens' RL.BoundingBox RL.Vector3
_boundingBox'max f (RL.BoundingBox bbMin bbMax) = (\bbMax' -> RL.BoundingBox bbMin bbMax') <$> f bbMax


_wave'frameCount :: Lens' RL.Wave Integer
_wave'frameCount f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\frameCount' -> RL.Wave frameCount' sampleRate sampleSize channels waveData) <$> f frameCount
_wave'sampleRate :: Lens' RL.Wave Integer
_wave'sampleRate f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\sampleRate' -> RL.Wave frameCount sampleRate' sampleSize channels waveData) <$> f sampleRate
_wave'sampleSize :: Lens' RL.Wave Integer
_wave'sampleSize f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\sampleSize' -> RL.Wave frameCount sampleRate sampleSize' channels waveData) <$> f sampleSize
_wave'channels :: Lens' RL.Wave Integer
_wave'channels f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\channels' -> RL.Wave frameCount sampleRate sampleSize channels' waveData) <$> f channels
_wave'data :: Lens' RL.Wave [Int]
_wave'data f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\waveData' -> RL.Wave frameCount sampleRate sampleSize channels waveData') <$> f waveData


_rAudioBuffer'converter :: Lens' RL.RAudioBuffer [Int]
_rAudioBuffer'converter f buffer =
    (\converter' -> buffer { RL.rAudioBuffer'converter = converter' }) <$>
    f (RL.rAudioBuffer'converter buffer)
_rAudioBuffer'callback :: Lens' RL.RAudioBuffer RL.AudioCallback
_rAudioBuffer'callback f buffer =
    (\callback' -> buffer { RL.rAudioBuffer'callback = callback' }) <$>
    f (RL.rAudioBuffer'callback buffer)
_rAudioBuffer'processor :: Lens' RL.RAudioBuffer (Maybe RL.RAudioProcessor)
_rAudioBuffer'processor f buffer =
    (\processor' -> buffer { RL.rAudioBuffer'processor = processor' }) <$>
    f (RL.rAudioBuffer'processor buffer)
_rAudioBuffer'volume :: Lens' RL.RAudioBuffer Float
_rAudioBuffer'volume f buffer =
    (\volume' -> buffer { RL.rAudioBuffer'volume = volume' }) <$>
    f (RL.rAudioBuffer'volume buffer)
_rAudioBuffer'pitch :: Lens' RL.RAudioBuffer Float
_rAudioBuffer'pitch f buffer =
    (\pitch' -> buffer { RL.rAudioBuffer'pitch = pitch' }) <$>
    f (RL.rAudioBuffer'pitch buffer)
_rAudioBuffer'pan :: Lens' RL.RAudioBuffer Float
_rAudioBuffer'pan f buffer =
    (\pan' -> buffer { RL.rAudioBuffer'pan = pan' }) <$>
    f (RL.rAudioBuffer'pan buffer)
_rAudioBuffer'playing :: Lens' RL.RAudioBuffer Bool
_rAudioBuffer'playing f buffer =
    (\playing' -> buffer { RL.rAudioBuffer'playing = playing' }) <$>
    f (RL.rAudioBuffer'playing buffer)
_rAudioBuffer'paused :: Lens' RL.RAudioBuffer Bool
_rAudioBuffer'paused f buffer =
    (\paused' -> buffer { RL.rAudioBuffer'paused = paused' }) <$>
    f (RL.rAudioBuffer'paused buffer)
_rAudioBuffer'looping :: Lens' RL.RAudioBuffer Bool
_rAudioBuffer'looping f buffer =
    (\looping' -> buffer { RL.rAudioBuffer'looping = looping' }) <$>
    f (RL.rAudioBuffer'looping buffer)
_rAudioBuffer'usage :: Lens' RL.RAudioBuffer Int
_rAudioBuffer'usage f buffer =
    (\usage' -> buffer { RL.rAudioBuffer'usage = usage' }) <$>
    f (RL.rAudioBuffer'usage buffer)
_rAudioBuffer'isSubBufferProcessed :: Lens' RL.RAudioBuffer [Bool]
_rAudioBuffer'isSubBufferProcessed f buffer =
    (\isSubBufferProcessed' -> buffer { RL.rAudioBuffer'isSubBufferProcessed = isSubBufferProcessed' }) <$>
    f (RL.rAudioBuffer'isSubBufferProcessed buffer)
_rAudioBuffer'sizeInFrames :: Lens' RL.RAudioBuffer Integer
_rAudioBuffer'sizeInFrames f buffer =
    (\sizeInFrames' -> buffer { RL.rAudioBuffer'sizeInFrames = sizeInFrames' }) <$>
    f (RL.rAudioBuffer'sizeInFrames buffer)
_rAudioBuffer'frameCursorPos :: Lens' RL.RAudioBuffer Integer
_rAudioBuffer'frameCursorPos f buffer =
    (\frameCursorPos' -> buffer { RL.rAudioBuffer'frameCursorPos = frameCursorPos' }) <$>
    f (RL.rAudioBuffer'frameCursorPos buffer)
_rAudioBuffer'framesProcessed :: Lens' RL.RAudioBuffer Integer
_rAudioBuffer'framesProcessed f buffer =
    (\framesProcessed' -> buffer { RL.rAudioBuffer'framesProcessed = framesProcessed' }) <$>
    f (RL.rAudioBuffer'framesProcessed buffer)
_rAudioBuffer'data :: Lens' RL.RAudioBuffer [Word8]
_rAudioBuffer'data f buffer =
    (\data' -> buffer { RL.rAudioBuffer'data = data' }) <$>
    f (RL.rAudioBuffer'data buffer)
_rAudioBuffer'next :: Lens' RL.RAudioBuffer (Maybe RL.RAudioBuffer)
_rAudioBuffer'next f buffer =
    (\next' -> buffer { RL.rAudioBuffer'next = next' }) <$>
    f (RL.rAudioBuffer'next buffer)
_rAudioBuffer'prev :: Lens' RL.RAudioBuffer (Maybe RL.RAudioBuffer)
_rAudioBuffer'prev f buffer =
    (\prev' -> buffer { RL.rAudioBuffer'prev = prev' }) <$>
    f (RL.rAudioBuffer'prev buffer)


_rAudioProcessor'process :: Lens' RL.RAudioProcessor (Maybe RL.AudioCallback)
_rAudioProcessor'process f (RL.RAudioProcessor process next prev) = (\process' -> RL.RAudioProcessor process' next prev) <$> f process
_rAudioProcessor'next :: Lens' RL.RAudioProcessor (Maybe RL.RAudioProcessor)
_rAudioProcessor'next f (RL.RAudioProcessor process next prev) = (\next' -> RL.RAudioProcessor process next' prev) <$> f next
_rAudioProcessor'prev :: Lens' RL.RAudioProcessor (Maybe RL.RAudioProcessor)
_rAudioProcessor'prev f (RL.RAudioProcessor process next prev) = (\prev' -> RL.RAudioProcessor process next prev') <$> f prev


_rAudioStream'buffer :: Lens' RL.AudioStream (Ptr RL.RAudioBuffer)
_rAudioStream'buffer f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\buffer' -> RL.AudioStream buffer' processor sampleRate sampleSize channels) <$>
    f buffer
_rAudioStream'processor :: Lens' RL.AudioStream (Ptr RL.RAudioProcessor)
_rAudioStream'processor f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\processor' -> RL.AudioStream buffer processor' sampleRate sampleSize channels) <$>
    f processor
_rAudioStream'sampleRate :: Lens' RL.AudioStream Integer
_rAudioStream'sampleRate f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\sampleRate' -> RL.AudioStream buffer processor sampleRate' sampleSize channels) <$>
    f sampleRate
_rAudioStream'sampleSize :: Lens' RL.AudioStream Integer
_rAudioStream'sampleSize f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\sampleSize' -> RL.AudioStream buffer processor sampleRate sampleSize' channels) <$>
    f sampleSize
_rAudioStream'channels :: Lens' RL.AudioStream Integer
_rAudioStream'channels f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\channels' -> RL.AudioStream buffer processor sampleRate sampleSize channels') <$>
    f channels


_sound'stream :: Lens' RL.Sound RL.AudioStream
_sound'stream f (RL.Sound stream frameCount) = (\stream' -> RL.Sound stream' frameCount) <$> f stream
_sound'frameCount :: Lens' RL.Sound Integer
_sound'frameCount f (RL.Sound stream frameCount) = (\frameCount' -> RL.Sound stream frameCount') <$> f frameCount


_music'stream :: Lens' RL.Music RL.AudioStream
_music'stream f (RL.Music stream frameCount looping ctxType ctxData) =
    (\stream' -> RL.Music stream' frameCount looping ctxType ctxData) <$>
    f stream
_music'frameCount :: Lens' RL.Music Integer
_music'frameCount f (RL.Music stream frameCount looping ctxType ctxData) =
    (\frameCount' -> RL.Music stream frameCount' looping ctxType ctxData) <$>
    f frameCount
_music'looping :: Lens' RL.Music Bool
_music'looping f (RL.Music stream frameCount looping ctxType ctxData) =
    (\looping' -> RL.Music stream frameCount looping' ctxType ctxData) <$>
    f looping
_music'ctxType :: Lens' RL.Music RL.MusicContextType
_music'ctxType f (RL.Music stream frameCount looping ctxType ctxData) =
    (\ctxType' -> RL.Music stream frameCount looping ctxType' ctxData) <$>
    f ctxType
_music'ctxData :: Lens' RL.Music (Ptr ())
_music'ctxData f (RL.Music stream frameCount looping ctxType ctxData) =
    (\ctxData' -> RL.Music stream frameCount looping ctxType ctxData') <$>
    f ctxData


_vrDeviceInfo'hResolution :: Lens' RL.VrDeviceInfo Int
_vrDeviceInfo'hResolution f device =
    (\hResolution' -> device { RL.vrDeviceInfo'hResolution = hResolution' }) <$>
    f (RL.vrDeviceInfo'hResolution device)
_vrDeviceInfo'vResolution :: Lens' RL.VrDeviceInfo Int
_vrDeviceInfo'vResolution f device =
    (\vResolution' -> device { RL.vrDeviceInfo'vResolution = vResolution' }) <$>
    f (RL.vrDeviceInfo'vResolution device)
_vrDeviceInfo'hScreenSize :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'hScreenSize f device =
    (\hScreenSize' -> device { RL.vrDeviceInfo'hScreenSize = hScreenSize' }) <$>
    f (RL.vrDeviceInfo'hScreenSize device)
_vrDeviceInfo'vScreenSize :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'vScreenSize f device =
    (\vScreenSize' -> device { RL.vrDeviceInfo'vScreenSize = vScreenSize' }) <$>
    f (RL.vrDeviceInfo'vScreenSize device)
_vrDeviceInfo'vScreenCenter :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'vScreenCenter f device =
    (\vScreenCenter' -> device { RL.vrDeviceInfo'vScreenCenter = vScreenCenter' }) <$>
    f (RL.vrDeviceInfo'vScreenCenter device)
_vrDeviceInfo'eyeToScreenDistance :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'eyeToScreenDistance f device =
    (\eyeToScreenDistance' -> device { RL.vrDeviceInfo'eyeToScreenDistance = eyeToScreenDistance' }) <$>
    f (RL.vrDeviceInfo'eyeToScreenDistance device)
_vrDeviceInfo'lensSeparationDistance :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'lensSeparationDistance f device =
    (\lensSeparationDistance' -> device { RL.vrDeviceInfo'lensSeparationDistance = lensSeparationDistance' }) <$>
    f (RL.vrDeviceInfo'lensSeparationDistance device)
_vrDeviceInfo'interpupillaryDistance :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'interpupillaryDistance f device =
    (\interpupillaryDistance' -> device { RL.vrDeviceInfo'interpupillaryDistance = interpupillaryDistance' }) <$>
    f (RL.vrDeviceInfo'interpupillaryDistance device)
_vrDeviceInfo'lensDistortionValues :: Lens' RL.VrDeviceInfo [Float]
_vrDeviceInfo'lensDistortionValues f device =
    (\lensDistortionValues' -> device { RL.vrDeviceInfo'lensDistortionValues = lensDistortionValues' }) <$>
    f (RL.vrDeviceInfo'lensDistortionValues device)
_vrDeviceInfo'chromaAbCorrection :: Lens' RL.VrDeviceInfo [Float]
_vrDeviceInfo'chromaAbCorrection f device =
    (\chromaAbCorrection' -> device { RL.vrDeviceInfo'chromaAbCorrection = chromaAbCorrection' }) <$>
    f (RL.vrDeviceInfo'chromaAbCorrection device)


_vrStereoConfig'projection :: Lens' RL.VrStereoConfig [RL.Matrix]
_vrStereoConfig'projection f config =
    (\projection' -> config { RL.vrStereoConfig'projection = projection' }) <$>
    f (RL.vrStereoConfig'projection config)
_vrStereoConfig'viewOffset :: Lens' RL.VrStereoConfig [RL.Matrix]
_vrStereoConfig'viewOffset f config =
    (\viewOffset' -> config { RL.vrStereoConfig'viewOffset = viewOffset' }) <$>
    f (RL.vrStereoConfig'viewOffset config)
_vrStereoConfig'leftLensCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'leftLensCenter f config =
    (\leftLensCenter' -> config { RL.vrStereoConfig'leftLensCenter = leftLensCenter' }) <$>
    f (RL.vrStereoConfig'leftLensCenter config)
_vrStereoConfig'rightLensCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'rightLensCenter f config =
    (\rightLensCenter' -> config { RL.vrStereoConfig'rightLensCenter = rightLensCenter' }) <$>
    f (RL.vrStereoConfig'rightLensCenter config)
_vrStereoConfig'leftScreenCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'leftScreenCenter f config =
    (\leftScreenCenter' -> config { RL.vrStereoConfig'leftScreenCenter = leftScreenCenter' }) <$>
    f (RL.vrStereoConfig'leftScreenCenter config)
_vrStereoConfig'rightScreenCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'rightScreenCenter f config =
    (\rightScreenCenter' -> config { RL.vrStereoConfig'rightScreenCenter = rightScreenCenter' }) <$>
    f (RL.vrStereoConfig'rightScreenCenter config)
_vrStereoConfig'scale :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'scale f config =
    (\scale' -> config { RL.vrStereoConfig'scale = scale' }) <$>
    f (RL.vrStereoConfig'scale config)
_vrStereoConfig'scaleIn :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'scaleIn f config =
    (\scaleIn' -> config { RL.vrStereoConfig'scaleIn = scaleIn' }) <$>
    f (RL.vrStereoConfig'scaleIn config)


_filePathList'capacity :: Lens' RL.FilePathList Integer
_filePathList'capacity f (RL.FilePathList capacity paths) =
    (\capacity' -> RL.FilePathList capacity' paths) <$> f capacity
_filePathList'paths :: Lens' RL.FilePathList [String]
_filePathList'paths f (RL.FilePathList capacity paths) =
    (\paths' -> RL.FilePathList capacity paths') <$> f paths


_rlVertexBuffer'elementCount :: Lens' RL.RLVertexBuffer Int
_rlVertexBuffer'elementCount f buffer =
    (\elementCount' -> buffer { RL.rlVertexBuffer'elementCount = elementCount' }) <$>
    f (RL.rlVertexBuffer'elementCount buffer)
_rlVertexBuffer'vertices :: Lens' RL.RLVertexBuffer [RL.Vector3]
_rlVertexBuffer'vertices f buffer =
    (\vertices' -> buffer { RL.rlVertexBuffer'vertices = vertices' }) <$>
    f (RL.rlVertexBuffer'vertices buffer)
_rlVertexBuffer'texcoords :: Lens' RL.RLVertexBuffer [RL.Vector2]
_rlVertexBuffer'texcoords f buffer =
    (\texcoords' -> buffer { RL.rlVertexBuffer'texcoords = texcoords' }) <$>
    f (RL.rlVertexBuffer'texcoords buffer)
_rlVertexBuffer'colors :: Lens' RL.RLVertexBuffer [RL.Color]
_rlVertexBuffer'colors f buffer =
    (\colors' -> buffer { RL.rlVertexBuffer'colors = colors' }) <$>
    f (RL.rlVertexBuffer'colors buffer)
_rlVertexBuffer'indices :: Lens' RL.RLVertexBuffer [Integer]
_rlVertexBuffer'indices f buffer =
    (\indices' -> buffer { RL.rlVertexBuffer'indices = indices' }) <$>
    f (RL.rlVertexBuffer'indices buffer)
_rlVertexBuffer'vaoId :: Lens' RL.RLVertexBuffer Integer
_rlVertexBuffer'vaoId f buffer =
    (\vaoId' -> buffer { RL.rlVertexBuffer'vaoId = vaoId' }) <$>
    f (RL.rlVertexBuffer'vaoId buffer)
_rlVertexBuffer'vboId :: Lens' RL.RLVertexBuffer [Integer]
_rlVertexBuffer'vboId f buffer =
    (\vboId' -> buffer { RL.rlVertexBuffer'vboId = vboId' }) <$>
    f (RL.rlVertexBuffer'vboId buffer)


_rlDrawCall'mode :: Lens' RL.RLDrawCall RL.RLDrawMode
_rlDrawCall'mode f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\mode' -> RL.RLDrawCall mode' vertexCount vertexAlignment textureId) <$>
    f mode
_rlDrawCall'vertexCount :: Lens' RL.RLDrawCall Int
_rlDrawCall'vertexCount f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\vertexCount' -> RL.RLDrawCall mode vertexCount' vertexAlignment textureId) <$>
    f vertexCount
_rlDrawCall'vertexAlignment :: Lens' RL.RLDrawCall Int
_rlDrawCall'vertexAlignment f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\vertexAlignment' -> RL.RLDrawCall mode vertexCount vertexAlignment' textureId) <$>
    f vertexAlignment
_rlDrawCall'textureId :: Lens' RL.RLDrawCall Integer
_rlDrawCall'textureId f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\textureId' -> RL.RLDrawCall mode vertexCount vertexAlignment textureId') <$>
    f textureId


_rlRenderBatch'bufferCount :: Lens' RL.RLRenderBatch Int
_rlRenderBatch'bufferCount f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\bufferCount' -> RL.RLRenderBatch bufferCount' currentBuffer vertexBuffers draws drawCounter currentDepth) <$>
    f bufferCount
_rlRenderBatch'currentBuffer :: Lens' RL.RLRenderBatch Int
_rlRenderBatch'currentBuffer f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\currentBuffer' -> RL.RLRenderBatch bufferCount currentBuffer' vertexBuffers draws drawCounter currentDepth) <$>
    f currentBuffer
_rlRenderBatch'vertexBuffers :: Lens' RL.RLRenderBatch [RL.RLVertexBuffer]
_rlRenderBatch'vertexBuffers f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\vertexBuffers' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers' draws drawCounter currentDepth) <$>
    f vertexBuffers
_rlRenderBatch'draws :: Lens' RL.RLRenderBatch [RL.RLDrawCall]
_rlRenderBatch'draws f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\draws' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws' drawCounter currentDepth) <$>
    f draws
_rlRenderBatch'drawCounter :: Lens' RL.RLRenderBatch Int
_rlRenderBatch'drawCounter f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\drawCounter' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter' currentDepth) <$>
    f drawCounter
_rlRenderBatch'currentDepth :: Lens' RL.RLRenderBatch Float
_rlRenderBatch'currentDepth f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\currentDepth' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth') <$>
    f currentDepth
