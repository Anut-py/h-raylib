{-# OPTIONS -Wall #-}

module Raylib.Util.Lenses where

import Control.Lens (Lens')
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr)
import qualified Raylib.Types as RL



_vector2'x :: Lens' RL.Vector2 Float
_vector2'x f (RL.Vector2 x y) = (\x' -> RL.Vector2 x' y) <$> f x
{-# INLINE _vector2'x #-}
_vector2'y :: Lens' RL.Vector2 Float
_vector2'y f (RL.Vector2 x y) = (\y' -> RL.Vector2 x y') <$> f y
{-# INLINE _vector2'y #-}


_vector3'x :: Lens' RL.Vector3 Float
_vector3'x f (RL.Vector3 x y z) = (\x' -> RL.Vector3 x' y z) <$> f x
{-# INLINE _vector3'x #-}
_vector3'y :: Lens' RL.Vector3 Float
_vector3'y f (RL.Vector3 x y z) = (\y' -> RL.Vector3 x y' z) <$> f y
{-# INLINE _vector3'y #-}
_vector3'z :: Lens' RL.Vector3 Float
_vector3'z f (RL.Vector3 x y z) = (\z' -> RL.Vector3 x y z') <$> f z
{-# INLINE _vector3'z #-}

_vector4'x :: Lens' RL.Vector4 Float
_vector4'x f (RL.Vector4 x y z w) = (\x' -> RL.Vector4 x' y z w) <$> f x
{-# INLINE _vector4'x #-}
_vector4'y :: Lens' RL.Vector4 Float
_vector4'y f (RL.Vector4 x y z w) = (\y' -> RL.Vector4 x y' z w) <$> f y
{-# INLINE _vector4'y #-}
_vector4'z :: Lens' RL.Vector4 Float
_vector4'z f (RL.Vector4 x y z w) = (\z' -> RL.Vector4 x y z' w) <$> f z
{-# INLINE _vector4'z #-}
_vector4'w :: Lens' RL.Vector4 Float
_vector4'w f (RL.Vector4 x y z w) = (\w' -> RL.Vector4 x y z w') <$> f w
{-# INLINE _vector4'w #-}


_matrix'm0 :: Lens' RL.Matrix Float
_matrix'm0 f matrix = (\m' -> matrix { RL.matrix'm0 = m' }) <$> f (RL.matrix'm0 matrix)
{-# INLINE _matrix'm0 #-}
_matrix'm1 :: Lens' RL.Matrix Float
_matrix'm1 f matrix = (\m' -> matrix { RL.matrix'm1 = m' }) <$> f (RL.matrix'm1 matrix)
{-# INLINE _matrix'm1 #-}
_matrix'm2 :: Lens' RL.Matrix Float
_matrix'm2 f matrix = (\m' -> matrix { RL.matrix'm2 = m' }) <$> f (RL.matrix'm2 matrix)
{-# INLINE _matrix'm2 #-}
_matrix'm3 :: Lens' RL.Matrix Float
_matrix'm3 f matrix = (\m' -> matrix { RL.matrix'm3 = m' }) <$> f (RL.matrix'm3 matrix)
{-# INLINE _matrix'm3 #-}
_matrix'm4 :: Lens' RL.Matrix Float
_matrix'm4 f matrix = (\m' -> matrix { RL.matrix'm4 = m' }) <$> f (RL.matrix'm4 matrix)
{-# INLINE _matrix'm4 #-}
_matrix'm5 :: Lens' RL.Matrix Float
_matrix'm5 f matrix = (\m' -> matrix { RL.matrix'm5 = m' }) <$> f (RL.matrix'm5 matrix)
{-# INLINE _matrix'm5 #-}
_matrix'm6 :: Lens' RL.Matrix Float
_matrix'm6 f matrix = (\m' -> matrix { RL.matrix'm6 = m' }) <$> f (RL.matrix'm6 matrix)
{-# INLINE _matrix'm6 #-}
_matrix'm7 :: Lens' RL.Matrix Float
_matrix'm7 f matrix = (\m' -> matrix { RL.matrix'm7 = m' }) <$> f (RL.matrix'm7 matrix)
{-# INLINE _matrix'm7 #-}
_matrix'm8 :: Lens' RL.Matrix Float
_matrix'm8 f matrix = (\m' -> matrix { RL.matrix'm8 = m' }) <$> f (RL.matrix'm8 matrix)
{-# INLINE _matrix'm8 #-}
_matrix'm9 :: Lens' RL.Matrix Float
_matrix'm9 f matrix = (\m' -> matrix { RL.matrix'm9 = m' }) <$> f (RL.matrix'm9 matrix)
{-# INLINE _matrix'm9 #-}
_matrix'm10 :: Lens' RL.Matrix Float
_matrix'm10 f matrix = (\m' -> matrix { RL.matrix'm10 = m' }) <$> f (RL.matrix'm10 matrix)
{-# INLINE _matrix'm10 #-}
_matrix'm11 :: Lens' RL.Matrix Float
_matrix'm11 f matrix = (\m' -> matrix { RL.matrix'm11 = m' }) <$> f (RL.matrix'm11 matrix)
{-# INLINE _matrix'm11 #-}
_matrix'm12 :: Lens' RL.Matrix Float
_matrix'm12 f matrix = (\m' -> matrix { RL.matrix'm12 = m' }) <$> f (RL.matrix'm12 matrix)
{-# INLINE _matrix'm12 #-}
_matrix'm13 :: Lens' RL.Matrix Float
_matrix'm13 f matrix = (\m' -> matrix { RL.matrix'm13 = m' }) <$> f (RL.matrix'm13 matrix)
{-# INLINE _matrix'm13 #-}
_matrix'm14 :: Lens' RL.Matrix Float
_matrix'm14 f matrix = (\m' -> matrix { RL.matrix'm14 = m' }) <$> f (RL.matrix'm14 matrix)
{-# INLINE _matrix'm14 #-}
_matrix'm15 :: Lens' RL.Matrix Float
_matrix'm15 f matrix = (\m' -> matrix { RL.matrix'm15 = m' }) <$> f (RL.matrix'm15 matrix)
{-# INLINE _matrix'm15 #-}


_color'r :: Lens' RL.Color Word8
_color'r f (RL.Color r g b a) = (\r' -> RL.Color r' g b a) <$> f r
{-# INLINE _color'r #-}
_color'g :: Lens' RL.Color Word8
_color'g f (RL.Color r g b a) = (\g' -> RL.Color r g' b a) <$> f g
{-# INLINE _color'g #-}
_color'b :: Lens' RL.Color Word8
_color'b f (RL.Color r g b a) = (\b' -> RL.Color r g b' a) <$> f b
{-# INLINE _color'b #-}
_color'a :: Lens' RL.Color Word8
_color'a f (RL.Color r g b a) = (\a' -> RL.Color r g b a') <$> f a
{-# INLINE _color'a #-}


_rectangle'x :: Lens' RL.Rectangle Float
_rectangle'x f (RL.Rectangle x y width height) = (\x' -> RL.Rectangle x' y width height) <$> f x
{-# INLINE _rectangle'x #-}
_rectangle'y :: Lens' RL.Rectangle Float
_rectangle'y f (RL.Rectangle x y width height) = (\y' -> RL.Rectangle x y' width height) <$> f y
{-# INLINE _rectangle'y #-}
_rectangle'width :: Lens' RL.Rectangle Float
_rectangle'width f (RL.Rectangle x y width height) = (\width' -> RL.Rectangle x y width' height) <$> f width
{-# INLINE _rectangle'width #-}
_rectangle'height :: Lens' RL.Rectangle Float
_rectangle'height f (RL.Rectangle x y width height) = (\height' -> RL.Rectangle x y width height') <$> f height
{-# INLINE _rectangle'height #-}


_image'data :: Lens' RL.Image [Word8]
_image'data f (RL.Image imgData width height mipmaps format) =
    (\imgData' -> RL.Image imgData' width height mipmaps format) <$> f imgData
{-# INLINE _image'data #-}
_image'width :: Lens' RL.Image Int
_image'width f (RL.Image imgData width height mipmaps format) =
    (\width' -> RL.Image imgData width' height mipmaps format) <$> f width
{-# INLINE _image'width #-}
_image'height :: Lens' RL.Image Int
_image'height f (RL.Image imgData width height mipmaps format) =
    (\height' -> RL.Image imgData width height' mipmaps format) <$> f height
{-# INLINE _image'height #-}
_image'mipmaps :: Lens' RL.Image Int
_image'mipmaps f (RL.Image imgData width height mipmaps format) =
    (\mipmaps' -> RL.Image imgData width height mipmaps' format) <$> f mipmaps
{-# INLINE _image'mipmaps #-}
_image'format :: Lens' RL.Image RL.PixelFormat
_image'format f (RL.Image imgData width height mipmaps format) =
    (\format' -> RL.Image imgData width height mipmaps format') <$> f format
{-# INLINE _image'format #-}


_texture'id :: Lens' RL.Texture Integer
_texture'id f (RL.Texture ident width height mipmaps format) =
    (\ident' -> RL.Texture ident' width height mipmaps format) <$> f ident
{-# INLINE _texture'id #-}
_texture'width :: Lens' RL.Texture Int
_texture'width f (RL.Texture ident width height mipmaps format) =
    (\width' -> RL.Texture ident width' height mipmaps format) <$> f width
{-# INLINE _texture'width #-}
_texture'height :: Lens' RL.Texture Int
_texture'height f (RL.Texture ident width height mipmaps format) =
    (\height' -> RL.Texture ident width height' mipmaps format) <$> f height
{-# INLINE _texture'height #-}
_texture'mipmaps :: Lens' RL.Texture Int
_texture'mipmaps f (RL.Texture ident width height mipmaps format) =
    (\mipmaps' -> RL.Texture ident width height mipmaps' format) <$> f mipmaps
{-# INLINE _texture'mipmaps #-}
_texture'format :: Lens' RL.Texture RL.PixelFormat
_texture'format f (RL.Texture ident width height mipmaps format) =
    (\format' -> RL.Texture ident width height mipmaps format') <$> f format
{-# INLINE _texture'format #-}


_renderTexture'id :: Lens' RL.RenderTexture Integer
_renderTexture'id f (RL.RenderTexture ident texture depth) =
    (\ident' -> RL.RenderTexture ident' texture depth) <$> f ident
{-# INLINE _renderTexture'id #-}
_renderTexture'texture :: Lens' RL.RenderTexture RL.Texture
_renderTexture'texture f (RL.RenderTexture ident texture depth) =
    (\texture' -> RL.RenderTexture ident texture' depth) <$> f texture
{-# INLINE _renderTexture'texture #-}
_renderTexture'depth :: Lens' RL.RenderTexture RL.Texture
_renderTexture'depth f (RL.RenderTexture ident texture depth) =
    (\depth' -> RL.RenderTexture ident texture depth') <$> f depth
{-# INLINE _renderTexture'depth #-}


_nPatchInfo'source :: Lens' RL.NPatchInfo RL.Rectangle
_nPatchInfo'source f (RL.NPatchInfo source left top right bottom layout) =
    (\source' -> RL.NPatchInfo source' left top right bottom layout) <$> f source
{-# INLINE _nPatchInfo'source #-}
_nPatchInfo'left :: Lens' RL.NPatchInfo Int
_nPatchInfo'left f (RL.NPatchInfo source left top right bottom layout) =
    (\left' -> RL.NPatchInfo source left' top right bottom layout) <$> f left
{-# INLINE _nPatchInfo'left #-}
_nPatchInfo'top :: Lens' RL.NPatchInfo Int
_nPatchInfo'top f (RL.NPatchInfo source left top right bottom layout) =
    (\top' -> RL.NPatchInfo source left top' right bottom layout) <$> f top
{-# INLINE _nPatchInfo'top #-}
_nPatchInfo'right :: Lens' RL.NPatchInfo Int
_nPatchInfo'right f (RL.NPatchInfo source left top right bottom layout) =
    (\right' -> RL.NPatchInfo source left top right' bottom layout) <$> f right
{-# INLINE _nPatchInfo'right #-}
_nPatchInfo'bottom :: Lens' RL.NPatchInfo Int
_nPatchInfo'bottom f (RL.NPatchInfo source left top right bottom layout) =
    (\bottom' -> RL.NPatchInfo source left top right bottom' layout) <$> f bottom
{-# INLINE _nPatchInfo'bottom #-}
_nPatchInfo'layout :: Lens' RL.NPatchInfo RL.NPatchLayout
_nPatchInfo'layout f (RL.NPatchInfo source left top right bottom layout) =
    (\layout' -> RL.NPatchInfo source left top right bottom layout') <$> f layout
{-# INLINE _nPatchInfo'layout #-}


_glyphInfo'value :: Lens' RL.GlyphInfo Int
_glyphInfo'value f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\value' -> RL.GlyphInfo value' offsetX offsetY advanceX image) <$> f value
{-# INLINE _glyphInfo'value #-}
_glyphInfo'offsetX :: Lens' RL.GlyphInfo Int
_glyphInfo'offsetX f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\offsetX' -> RL.GlyphInfo value offsetX' offsetY advanceX image) <$> f offsetX
{-# INLINE _glyphInfo'offsetX #-}
_glyphInfo'offsetY :: Lens' RL.GlyphInfo Int
_glyphInfo'offsetY f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\offsetY' -> RL.GlyphInfo value offsetX offsetY' advanceX image) <$> f offsetY
{-# INLINE _glyphInfo'offsetY #-}
_glyphInfo'advanceX :: Lens' RL.GlyphInfo Int
_glyphInfo'advanceX f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\advanceX' -> RL.GlyphInfo value offsetX offsetY advanceX' image) <$> f advanceX
{-# INLINE _glyphInfo'advanceX #-}
_glyphInfo'image :: Lens' RL.GlyphInfo RL.Image
_glyphInfo'image f (RL.GlyphInfo value offsetX offsetY advanceX image) =
    (\image' -> RL.GlyphInfo value offsetX offsetY advanceX image') <$> f image
{-# INLINE _glyphInfo'image #-}


_font'baseSize :: Lens' RL.Font Int
_font'baseSize f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\baseSize' -> RL.Font baseSize' glyphCount glyphPadding texture recs glyphs) <$> f baseSize
{-# INLINE _font'baseSize #-}
_font'glyphCount :: Lens' RL.Font Int
_font'glyphCount f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\glyphCount' -> RL.Font baseSize glyphCount' glyphPadding texture recs glyphs) <$> f glyphCount
{-# INLINE _font'glyphCount #-}
_font'glyphPadding :: Lens' RL.Font Int
_font'glyphPadding f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\glyphPadding' -> RL.Font baseSize glyphCount glyphPadding' texture recs glyphs) <$> f glyphPadding
{-# INLINE _font'glyphPadding #-}
_font'texture :: Lens' RL.Font RL.Texture
_font'texture f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\texture' -> RL.Font baseSize glyphCount glyphPadding texture' recs glyphs) <$> f texture
{-# INLINE _font'texture #-}
_font'recs :: Lens' RL.Font [RL.Rectangle]
_font'recs f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\recs' -> RL.Font baseSize glyphCount glyphPadding texture recs' glyphs) <$> f recs
{-# INLINE _font'recs #-}
_font'glyphs :: Lens' RL.Font [RL.GlyphInfo]
_font'glyphs f (RL.Font baseSize glyphCount glyphPadding texture recs glyphs) =
    (\glyphs' -> RL.Font baseSize glyphCount glyphPadding texture recs glyphs') <$> f glyphs
{-# INLINE _font'glyphs #-}


_camera3D'position :: Lens' RL.Camera3D RL.Vector3
_camera3D'position f (RL.Camera3D position target up fovy projection) =
    (\position' -> RL.Camera3D position' target up fovy projection) <$> f position
{-# INLINE _camera3D'position #-}
_camera3D'target :: Lens' RL.Camera3D RL.Vector3
_camera3D'target f (RL.Camera3D position target up fovy projection) =
    (\target' -> RL.Camera3D position target' up fovy projection) <$> f target
{-# INLINE _camera3D'target #-}
_camera3D'up :: Lens' RL.Camera3D RL.Vector3
_camera3D'up f (RL.Camera3D position target up fovy projection) =
    (\up' -> RL.Camera3D position target up' fovy projection) <$> f up
{-# INLINE _camera3D'up #-}
_camera3D'fovy :: Lens' RL.Camera3D Float
_camera3D'fovy f (RL.Camera3D position target up fovy projection) =
    (\fovy' -> RL.Camera3D position target up fovy' projection) <$> f fovy
{-# INLINE _camera3D'fovy #-}
_camera3D'projection :: Lens' RL.Camera3D RL.CameraProjection
_camera3D'projection f (RL.Camera3D position target up fovy projection) =
    (\projection' -> RL.Camera3D position target up fovy projection') <$> f projection
{-# INLINE _camera3D'projection #-}


_camera2D'offset :: Lens' RL.Camera2D RL.Vector2
_camera2D'offset f (RL.Camera2D offset target rotation zoom) =
    (\offset' -> RL.Camera2D offset' target rotation zoom) <$> f offset
{-# INLINE _camera2D'offset #-}
_camera2D'target :: Lens' RL.Camera2D RL.Vector2
_camera2D'target f (RL.Camera2D offset target rotation zoom) =
    (\target' -> RL.Camera2D offset target' rotation zoom) <$> f target
{-# INLINE _camera2D'target #-}
_camera2D'rotation :: Lens' RL.Camera2D Float
_camera2D'rotation f (RL.Camera2D offset target rotation zoom) =
    (\rotation' -> RL.Camera2D offset target rotation' zoom) <$> f rotation
{-# INLINE _camera2D'rotation #-}
_camera2D'zoom :: Lens' RL.Camera2D Float
_camera2D'zoom f (RL.Camera2D offset target rotation zoom) =
    (\zoom' -> RL.Camera2D offset target rotation zoom') <$> f zoom
{-# INLINE _camera2D'zoom #-}


_mesh'vertexCount :: Lens' RL.Mesh Int
_mesh'vertexCount f mesh =
    (\vertexCount' -> mesh { RL.mesh'vertexCount = vertexCount' }) <$>
    f (RL.mesh'vertexCount mesh)
{-# INLINE _mesh'vertexCount #-}
_mesh'triangleCount :: Lens' RL.Mesh Int
_mesh'triangleCount f mesh =
    (\triangleCount' -> mesh { RL.mesh'triangleCount = triangleCount' }) <$>
    f (RL.mesh'triangleCount mesh)
{-# INLINE _mesh'triangleCount #-}
_mesh'vertices :: Lens' RL.Mesh [RL.Vector3]
_mesh'vertices f mesh =
    (\vertices' -> mesh { RL.mesh'vertices = vertices' }) <$>
    f (RL.mesh'vertices mesh)
{-# INLINE _mesh'vertices #-}
_mesh'texcoords :: Lens' RL.Mesh [RL.Vector2]
_mesh'texcoords f mesh =
    (\texcoords' -> mesh { RL.mesh'texcoords = texcoords' }) <$>
    f (RL.mesh'texcoords mesh)
{-# INLINE _mesh'texcoords #-}
_mesh'texcoords2 :: Lens' RL.Mesh (Maybe [RL.Vector2])
_mesh'texcoords2 f mesh =
    (\texcoords2' -> mesh { RL.mesh'texcoords2 = texcoords2' }) <$>
    f (RL.mesh'texcoords2 mesh)
{-# INLINE _mesh'texcoords2 #-}
_mesh'normals :: Lens' RL.Mesh [RL.Vector3]
_mesh'normals f mesh =
    (\normals' -> mesh { RL.mesh'normals = normals' }) <$>
    f (RL.mesh'normals mesh)
{-# INLINE _mesh'normals #-}
_mesh'tangents :: Lens' RL.Mesh (Maybe [RL.Vector4])
_mesh'tangents f mesh =
    (\tangents' -> mesh { RL.mesh'tangents = tangents' }) <$>
    f (RL.mesh'tangents mesh)
{-# INLINE _mesh'tangents #-}
_mesh'colors :: Lens' RL.Mesh (Maybe [RL.Color])
_mesh'colors f mesh =
    (\colors' -> mesh { RL.mesh'colors = colors' }) <$>
    f (RL.mesh'colors mesh)
{-# INLINE _mesh'colors #-}
_mesh'indices :: Lens' RL.Mesh (Maybe [Word16])
_mesh'indices f mesh =
    (\indices' -> mesh { RL.mesh'indices = indices' }) <$>
    f (RL.mesh'indices mesh)
{-# INLINE _mesh'indices #-}
_mesh'animVertices :: Lens' RL.Mesh (Maybe [RL.Vector3])
_mesh'animVertices f mesh =
    (\animVertices' -> mesh { RL.mesh'animVertices = animVertices' }) <$>
    f (RL.mesh'animVertices mesh)
{-# INLINE _mesh'animVertices #-}
_mesh'animNormals :: Lens' RL.Mesh (Maybe [RL.Vector3])
_mesh'animNormals f mesh =
    (\animNormals' -> mesh { RL.mesh'animNormals = animNormals' }) <$>
    f (RL.mesh'animNormals mesh)
{-# INLINE _mesh'animNormals #-}
_mesh'boneIds :: Lens' RL.Mesh (Maybe [Word8])
_mesh'boneIds f mesh =
    (\boneIds' -> mesh { RL.mesh'boneIds = boneIds' }) <$>
    f (RL.mesh'boneIds mesh)
{-# INLINE _mesh'boneIds #-}
_mesh'boneWeights :: Lens' RL.Mesh (Maybe [Float])
_mesh'boneWeights f mesh =
    (\boneWeights' -> mesh { RL.mesh'boneWeights = boneWeights' }) <$>
    f (RL.mesh'boneWeights mesh)
{-# INLINE _mesh'boneWeights #-}
_mesh'vaoId :: Lens' RL.Mesh Integer
_mesh'vaoId f mesh =
    (\vaoId' -> mesh { RL.mesh'vaoId = vaoId' }) <$>
    f (RL.mesh'vaoId mesh)
{-# INLINE _mesh'vaoId #-}
_mesh'vboId :: Lens' RL.Mesh (Maybe [Integer])
_mesh'vboId f mesh =
    (\vboId' -> mesh { RL.mesh'vboId = vboId' }) <$>
    f (RL.mesh'vboId mesh)
{-# INLINE _mesh'vboId #-}


_shader'id :: Lens' RL.Shader Integer
_shader'id f (RL.Shader ident locs) = (\ident' -> RL.Shader ident' locs) <$> f ident
{-# INLINE _shader'id #-}
_shader'locs :: Lens' RL.Shader [Int]
_shader'locs f (RL.Shader ident locs) = (\locs' -> RL.Shader ident locs') <$> f locs
{-# INLINE _shader'locs #-}


_materialMap'texture :: Lens' RL.MaterialMap RL.Texture
_materialMap'texture f (RL.MaterialMap texture color value) =
    (\texture' -> RL.MaterialMap texture' color value) <$> f texture
{-# INLINE _materialMap'texture #-}
_materialMap'color :: Lens' RL.MaterialMap RL.Color
_materialMap'color f (RL.MaterialMap texture color value) =
    (\color' -> RL.MaterialMap texture color' value) <$> f color
{-# INLINE _materialMap'color #-}
_materialMap'value :: Lens' RL.MaterialMap Float
_materialMap'value f (RL.MaterialMap texture color value) =
    (\value' -> RL.MaterialMap texture color value') <$> f value
{-# INLINE _materialMap'value #-}


_material'shader :: Lens' RL.Material RL.Shader
_material'shader f (RL.Material shader maps params) =
    (\shader' -> RL.Material shader' maps params) <$> f shader
{-# INLINE _material'shader #-}
_material'maps :: Lens' RL.Material (Maybe [RL.MaterialMap])
_material'maps f (RL.Material shader maps params) =
    (\maps' -> RL.Material shader maps' params) <$> f maps
{-# INLINE _material'maps #-}
_material'params :: Lens' RL.Material [Float]
_material'params f (RL.Material shader maps params) =
    (\params' -> RL.Material shader maps params') <$> f params
{-# INLINE _material'params #-}


_transform'translation :: Lens' RL.Transform RL.Vector3
_transform'translation f (RL.Transform translation rotation scale) =
    (\translation' -> RL.Transform translation' rotation scale) <$> f translation
{-# INLINE _transform'translation #-}
_transform'rotation :: Lens' RL.Transform RL.Quaternion
_transform'rotation f (RL.Transform translation rotation scale) =
    (\rotation' -> RL.Transform translation rotation' scale) <$> f rotation
{-# INLINE _transform'rotation #-}
_transform'scale :: Lens' RL.Transform RL.Vector3
_transform'scale f (RL.Transform translation rotation scale) =
    (\scale' -> RL.Transform translation rotation scale') <$> f scale
{-# INLINE _transform'scale #-}


_boneInfo'name :: Lens' RL.BoneInfo String
_boneInfo'name f (RL.BoneInfo name parent) = (\name' -> RL.BoneInfo name' parent) <$> f name
{-# INLINE _boneInfo'name #-}
_boneInfo'parent :: Lens' RL.BoneInfo Int
_boneInfo'parent f (RL.BoneInfo name parent) = (\parent' -> RL.BoneInfo name parent') <$> f parent
{-# INLINE _boneInfo'parent #-}


_model'transform :: Lens' RL.Model RL.Matrix
_model'transform f model =
    (\transform' -> model { RL.model'transform = transform' }) <$> f (RL.model'transform model)
{-# INLINE _model'transform #-}
_model'meshes :: Lens' RL.Model [RL.Mesh]
_model'meshes f model =
    (\meshes' -> model { RL.model'meshes = meshes' }) <$> f (RL.model'meshes model)
{-# INLINE _model'meshes #-}
_model'materials :: Lens' RL.Model [RL.Material]
_model'materials f model =
    (\materials' -> model { RL.model'materials = materials' }) <$> f (RL.model'materials model)
{-# INLINE _model'materials #-}
_model'meshMaterial :: Lens' RL.Model [Int]
_model'meshMaterial f model =
    (\meshMaterial' -> model { RL.model'meshMaterial = meshMaterial' }) <$> f (RL.model'meshMaterial model)
{-# INLINE _model'meshMaterial #-}
_model'boneCount :: Lens' RL.Model Int
_model'boneCount f model =
    (\boneCount' -> model { RL.model'boneCount = boneCount' }) <$> f (RL.model'boneCount model)
{-# INLINE _model'boneCount #-}
_model'bones :: Lens' RL.Model (Maybe [RL.BoneInfo])
_model'bones f model =
    (\bones' -> model { RL.model'bones = bones' }) <$> f (RL.model'bones model)
{-# INLINE _model'bones #-}
_model'bindPose :: Lens' RL.Model (Maybe [RL.Transform])
_model'bindPose f model =
    (\bindPose' -> model { RL.model'bindPose = bindPose' }) <$> f (RL.model'bindPose model)
{-# INLINE _model'bindPose #-}


_modelAnimation'boneCount :: Lens' RL.ModelAnimation Int
_modelAnimation'boneCount f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\boneCount' -> RL.ModelAnimation boneCount' frameCount bones framePoses) <$> f boneCount
{-# INLINE _modelAnimation'boneCount #-}
_modelAnimation'frameCount :: Lens' RL.ModelAnimation Int
_modelAnimation'frameCount f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\frameCount' -> RL.ModelAnimation boneCount frameCount' bones framePoses) <$> f frameCount
{-# INLINE _modelAnimation'frameCount #-}
_modelAnimation'bones :: Lens' RL.ModelAnimation [RL.BoneInfo]
_modelAnimation'bones f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\bones' -> RL.ModelAnimation boneCount frameCount bones' framePoses) <$> f bones
{-# INLINE _modelAnimation'bones #-}
_modelAnimation'framePoses :: Lens' RL.ModelAnimation [[RL.Transform]]
_modelAnimation'framePoses f (RL.ModelAnimation boneCount frameCount bones framePoses) =
    (\framePoses' -> RL.ModelAnimation boneCount frameCount bones framePoses') <$> f framePoses
{-# INLINE _modelAnimation'framePoses #-}


_ray'position :: Lens' RL.Ray RL.Vector3
_ray'position f (RL.Ray position direction) = (\position' -> RL.Ray position' direction) <$> f position
{-# INLINE _ray'position #-}
_ray'direction :: Lens' RL.Ray RL.Vector3
_ray'direction f (RL.Ray position direction) = (\direction' -> RL.Ray position direction') <$> f direction
{-# INLINE _ray'direction #-}


_rayCollision'hit :: Lens' RL.RayCollision Bool
_rayCollision'hit f (RL.RayCollision hit distance point normal) =
    (\hit' -> RL.RayCollision hit' distance point normal) <$> f hit
{-# INLINE _rayCollision'hit #-}
_rayCollision'distance :: Lens' RL.RayCollision Float
_rayCollision'distance f (RL.RayCollision hit distance point normal) =
    (\distance' -> RL.RayCollision hit distance' point normal) <$> f distance
{-# INLINE _rayCollision'distance #-}
_rayCollision'point :: Lens' RL.RayCollision RL.Vector3
_rayCollision'point f (RL.RayCollision hit distance point normal) =
    (\point' -> RL.RayCollision hit distance point' normal) <$> f point
{-# INLINE _rayCollision'point #-}
_rayCollision'normal :: Lens' RL.RayCollision RL.Vector3
_rayCollision'normal f (RL.RayCollision hit distance point normal) =
    (\normal' -> RL.RayCollision hit distance point normal') <$> f normal
{-# INLINE _rayCollision'normal #-}


_boundingBox'min :: Lens' RL.BoundingBox RL.Vector3
_boundingBox'min f (RL.BoundingBox bbMin bbMax) = (\bbMin' -> RL.BoundingBox bbMin' bbMax) <$> f bbMin
{-# INLINE _boundingBox'min #-}
_boundingBox'max :: Lens' RL.BoundingBox RL.Vector3
_boundingBox'max f (RL.BoundingBox bbMin bbMax) = (\bbMax' -> RL.BoundingBox bbMin bbMax') <$> f bbMax
{-# INLINE _boundingBox'max #-}


_wave'frameCount :: Lens' RL.Wave Integer
_wave'frameCount f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\frameCount' -> RL.Wave frameCount' sampleRate sampleSize channels waveData) <$> f frameCount
{-# INLINE _wave'frameCount #-}
_wave'sampleRate :: Lens' RL.Wave Integer
_wave'sampleRate f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\sampleRate' -> RL.Wave frameCount sampleRate' sampleSize channels waveData) <$> f sampleRate
{-# INLINE _wave'sampleRate #-}
_wave'sampleSize :: Lens' RL.Wave Integer
_wave'sampleSize f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\sampleSize' -> RL.Wave frameCount sampleRate sampleSize' channels waveData) <$> f sampleSize
{-# INLINE _wave'sampleSize #-}
_wave'channels :: Lens' RL.Wave Integer
_wave'channels f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\channels' -> RL.Wave frameCount sampleRate sampleSize channels' waveData) <$> f channels
{-# INLINE _wave'channels #-}
_wave'data :: Lens' RL.Wave [Int]
_wave'data f (RL.Wave frameCount sampleRate sampleSize channels waveData) =
    (\waveData' -> RL.Wave frameCount sampleRate sampleSize channels waveData') <$> f waveData
{-# INLINE _wave'data #-}


_rAudioBuffer'converter :: Lens' RL.RAudioBuffer [Int]
_rAudioBuffer'converter f buffer =
    (\converter' -> buffer { RL.rAudioBuffer'converter = converter' }) <$>
    f (RL.rAudioBuffer'converter buffer)
{-# INLINE _rAudioBuffer'converter #-}
_rAudioBuffer'callback :: Lens' RL.RAudioBuffer RL.AudioCallback
_rAudioBuffer'callback f buffer =
    (\callback' -> buffer { RL.rAudioBuffer'callback = callback' }) <$>
    f (RL.rAudioBuffer'callback buffer)
{-# INLINE _rAudioBuffer'callback #-}
_rAudioBuffer'processor :: Lens' RL.RAudioBuffer (Maybe RL.RAudioProcessor)
_rAudioBuffer'processor f buffer =
    (\processor' -> buffer { RL.rAudioBuffer'processor = processor' }) <$>
    f (RL.rAudioBuffer'processor buffer)
{-# INLINE _rAudioBuffer'processor #-}
_rAudioBuffer'volume :: Lens' RL.RAudioBuffer Float
_rAudioBuffer'volume f buffer =
    (\volume' -> buffer { RL.rAudioBuffer'volume = volume' }) <$>
    f (RL.rAudioBuffer'volume buffer)
{-# INLINE _rAudioBuffer'volume #-}
_rAudioBuffer'pitch :: Lens' RL.RAudioBuffer Float
_rAudioBuffer'pitch f buffer =
    (\pitch' -> buffer { RL.rAudioBuffer'pitch = pitch' }) <$>
    f (RL.rAudioBuffer'pitch buffer)
{-# INLINE _rAudioBuffer'pitch #-}
_rAudioBuffer'pan :: Lens' RL.RAudioBuffer Float
_rAudioBuffer'pan f buffer =
    (\pan' -> buffer { RL.rAudioBuffer'pan = pan' }) <$>
    f (RL.rAudioBuffer'pan buffer)
{-# INLINE _rAudioBuffer'pan #-}
_rAudioBuffer'playing :: Lens' RL.RAudioBuffer Bool
_rAudioBuffer'playing f buffer =
    (\playing' -> buffer { RL.rAudioBuffer'playing = playing' }) <$>
    f (RL.rAudioBuffer'playing buffer)
{-# INLINE _rAudioBuffer'playing #-}
_rAudioBuffer'paused :: Lens' RL.RAudioBuffer Bool
_rAudioBuffer'paused f buffer =
    (\paused' -> buffer { RL.rAudioBuffer'paused = paused' }) <$>
    f (RL.rAudioBuffer'paused buffer)
{-# INLINE _rAudioBuffer'paused #-}
_rAudioBuffer'looping :: Lens' RL.RAudioBuffer Bool
_rAudioBuffer'looping f buffer =
    (\looping' -> buffer { RL.rAudioBuffer'looping = looping' }) <$>
    f (RL.rAudioBuffer'looping buffer)
{-# INLINE _rAudioBuffer'looping #-}
_rAudioBuffer'usage :: Lens' RL.RAudioBuffer Int
_rAudioBuffer'usage f buffer =
    (\usage' -> buffer { RL.rAudioBuffer'usage = usage' }) <$>
    f (RL.rAudioBuffer'usage buffer)
{-# INLINE _rAudioBuffer'usage #-}
_rAudioBuffer'isSubBufferProcessed :: Lens' RL.RAudioBuffer [Bool]
_rAudioBuffer'isSubBufferProcessed f buffer =
    (\isSubBufferProcessed' -> buffer { RL.rAudioBuffer'isSubBufferProcessed = isSubBufferProcessed' }) <$>
    f (RL.rAudioBuffer'isSubBufferProcessed buffer)
{-# INLINE _rAudioBuffer'isSubBufferProcessed #-}
_rAudioBuffer'sizeInFrames :: Lens' RL.RAudioBuffer Integer
_rAudioBuffer'sizeInFrames f buffer =
    (\sizeInFrames' -> buffer { RL.rAudioBuffer'sizeInFrames = sizeInFrames' }) <$>
    f (RL.rAudioBuffer'sizeInFrames buffer)
{-# INLINE _rAudioBuffer'sizeInFrames #-}
_rAudioBuffer'frameCursorPos :: Lens' RL.RAudioBuffer Integer
_rAudioBuffer'frameCursorPos f buffer =
    (\frameCursorPos' -> buffer { RL.rAudioBuffer'frameCursorPos = frameCursorPos' }) <$>
    f (RL.rAudioBuffer'frameCursorPos buffer)
{-# INLINE _rAudioBuffer'frameCursorPos #-}
_rAudioBuffer'framesProcessed :: Lens' RL.RAudioBuffer Integer
_rAudioBuffer'framesProcessed f buffer =
    (\framesProcessed' -> buffer { RL.rAudioBuffer'framesProcessed = framesProcessed' }) <$>
    f (RL.rAudioBuffer'framesProcessed buffer)
{-# INLINE _rAudioBuffer'framesProcessed #-}
_rAudioBuffer'data :: Lens' RL.RAudioBuffer [Word8]
_rAudioBuffer'data f buffer =
    (\data' -> buffer { RL.rAudioBuffer'data = data' }) <$>
    f (RL.rAudioBuffer'data buffer)
{-# INLINE _rAudioBuffer'data #-}
_rAudioBuffer'next :: Lens' RL.RAudioBuffer (Maybe RL.RAudioBuffer)
_rAudioBuffer'next f buffer =
    (\next' -> buffer { RL.rAudioBuffer'next = next' }) <$>
    f (RL.rAudioBuffer'next buffer)
{-# INLINE _rAudioBuffer'next #-}
_rAudioBuffer'prev :: Lens' RL.RAudioBuffer (Maybe RL.RAudioBuffer)
_rAudioBuffer'prev f buffer =
    (\prev' -> buffer { RL.rAudioBuffer'prev = prev' }) <$>
    f (RL.rAudioBuffer'prev buffer)
{-# INLINE _rAudioBuffer'prev #-}


_rAudioProcessor'process :: Lens' RL.RAudioProcessor (Maybe RL.AudioCallback)
_rAudioProcessor'process f (RL.RAudioProcessor process next prev) =
    (\process' -> RL.RAudioProcessor process' next prev) <$> f process
{-# INLINE _rAudioProcessor'process #-}
_rAudioProcessor'next :: Lens' RL.RAudioProcessor (Maybe RL.RAudioProcessor)
_rAudioProcessor'next f (RL.RAudioProcessor process next prev) =
    (\next' -> RL.RAudioProcessor process next' prev) <$> f next
{-# INLINE _rAudioProcessor'next #-}
_rAudioProcessor'prev :: Lens' RL.RAudioProcessor (Maybe RL.RAudioProcessor)
_rAudioProcessor'prev f (RL.RAudioProcessor process next prev) =
    (\prev' -> RL.RAudioProcessor process next prev') <$> f prev
{-# INLINE _rAudioProcessor'prev #-}


_AudioStream'buffer :: Lens' RL.AudioStream (Ptr RL.RAudioBuffer)
_AudioStream'buffer f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\buffer' -> RL.AudioStream buffer' processor sampleRate sampleSize channels) <$>
    f buffer
{-# INLINE _AudioStream'buffer#-}
_AudioStream'processor :: Lens' RL.AudioStream (Ptr RL.RAudioProcessor)
_AudioStream'processor f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\processor' -> RL.AudioStream buffer processor' sampleRate sampleSize channels) <$>
    f processor
{-# INLINE _AudioStream'processor #-}
_AudioStream'sampleRate :: Lens' RL.AudioStream Integer
_AudioStream'sampleRate f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\sampleRate' -> RL.AudioStream buffer processor sampleRate' sampleSize channels) <$>
    f sampleRate
{-# INLINE _AudioStream'sampleRate #-}
_AudioStream'sampleSize :: Lens' RL.AudioStream Integer
_AudioStream'sampleSize f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\sampleSize' -> RL.AudioStream buffer processor sampleRate sampleSize' channels) <$>
    f sampleSize
{-# INLINE _AudioStream'sampleSize #-}
_AudioStream'channels :: Lens' RL.AudioStream Integer
_AudioStream'channels f (RL.AudioStream buffer processor sampleRate sampleSize channels) =
    (\channels' -> RL.AudioStream buffer processor sampleRate sampleSize channels') <$>
    f channels
{-# INLINE _AudioStream'channels #-}


_sound'stream :: Lens' RL.Sound RL.AudioStream
_sound'stream f (RL.Sound stream frameCount) = (\stream' -> RL.Sound stream' frameCount) <$> f stream
{-# INLINE _sound'stream #-}
_sound'frameCount :: Lens' RL.Sound Integer
_sound'frameCount f (RL.Sound stream frameCount) = (\frameCount' -> RL.Sound stream frameCount') <$> f frameCount
{-# INLINE _sound'frameCount #-}


_music'stream :: Lens' RL.Music RL.AudioStream
_music'stream f (RL.Music stream frameCount looping ctxType ctxData) =
    (\stream' -> RL.Music stream' frameCount looping ctxType ctxData) <$>
    f stream
{-# INLINE _music'stream #-}
_music'frameCount :: Lens' RL.Music Integer
_music'frameCount f (RL.Music stream frameCount looping ctxType ctxData) =
    (\frameCount' -> RL.Music stream frameCount' looping ctxType ctxData) <$>
    f frameCount
{-# INLINE _music'frameCount #-}
_music'looping :: Lens' RL.Music Bool
_music'looping f (RL.Music stream frameCount looping ctxType ctxData) =
    (\looping' -> RL.Music stream frameCount looping' ctxType ctxData) <$>
    f looping
{-# INLINE _music'looping #-}
_music'ctxType :: Lens' RL.Music RL.MusicContextType
_music'ctxType f (RL.Music stream frameCount looping ctxType ctxData) =
    (\ctxType' -> RL.Music stream frameCount looping ctxType' ctxData) <$>
    f ctxType
{-# INLINE _music'ctxType #-}
_music'ctxData :: Lens' RL.Music (Ptr ())
_music'ctxData f (RL.Music stream frameCount looping ctxType ctxData) =
    (\ctxData' -> RL.Music stream frameCount looping ctxType ctxData') <$>
    f ctxData
{-# INLINE _music'ctxData #-}


_vrDeviceInfo'hResolution :: Lens' RL.VrDeviceInfo Int
_vrDeviceInfo'hResolution f device =
    (\hResolution' -> device { RL.vrDeviceInfo'hResolution = hResolution' }) <$>
    f (RL.vrDeviceInfo'hResolution device)
{-# INLINE _vrDeviceInfo'hResolution #-}
_vrDeviceInfo'vResolution :: Lens' RL.VrDeviceInfo Int
_vrDeviceInfo'vResolution f device =
    (\vResolution' -> device { RL.vrDeviceInfo'vResolution = vResolution' }) <$>
    f (RL.vrDeviceInfo'vResolution device)
{-# INLINE _vrDeviceInfo'vResolution #-}
_vrDeviceInfo'hScreenSize :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'hScreenSize f device =
    (\hScreenSize' -> device { RL.vrDeviceInfo'hScreenSize = hScreenSize' }) <$>
    f (RL.vrDeviceInfo'hScreenSize device)
{-# INLINE _vrDeviceInfo'hScreenSize #-}
_vrDeviceInfo'vScreenSize :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'vScreenSize f device =
    (\vScreenSize' -> device { RL.vrDeviceInfo'vScreenSize = vScreenSize' }) <$>
    f (RL.vrDeviceInfo'vScreenSize device)
{-# INLINE _vrDeviceInfo'vScreenSize #-}
_vrDeviceInfo'vScreenCenter :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'vScreenCenter f device =
    (\vScreenCenter' -> device { RL.vrDeviceInfo'vScreenCenter = vScreenCenter' }) <$>
    f (RL.vrDeviceInfo'vScreenCenter device)
{-# INLINE _vrDeviceInfo'vScreenCenter #-}
_vrDeviceInfo'eyeToScreenDistance :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'eyeToScreenDistance f device =
    (\eyeToScreenDistance' -> device { RL.vrDeviceInfo'eyeToScreenDistance = eyeToScreenDistance' }) <$>
    f (RL.vrDeviceInfo'eyeToScreenDistance device)
{-# INLINE _vrDeviceInfo'eyeToScreenDistance #-}
_vrDeviceInfo'lensSeparationDistance :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'lensSeparationDistance f device =
    (\lensSeparationDistance' -> device { RL.vrDeviceInfo'lensSeparationDistance = lensSeparationDistance' }) <$>
    f (RL.vrDeviceInfo'lensSeparationDistance device)
{-# INLINE _vrDeviceInfo'lensSeparationDistance #-}
_vrDeviceInfo'interpupillaryDistance :: Lens' RL.VrDeviceInfo Float
_vrDeviceInfo'interpupillaryDistance f device =
    (\interpupillaryDistance' -> device { RL.vrDeviceInfo'interpupillaryDistance = interpupillaryDistance' }) <$>
    f (RL.vrDeviceInfo'interpupillaryDistance device)
{-# INLINE _vrDeviceInfo'interpupillaryDistance #-}
_vrDeviceInfo'lensDistortionValues :: Lens' RL.VrDeviceInfo [Float]
_vrDeviceInfo'lensDistortionValues f device =
    (\lensDistortionValues' -> device { RL.vrDeviceInfo'lensDistortionValues = lensDistortionValues' }) <$>
    f (RL.vrDeviceInfo'lensDistortionValues device)
{-# INLINE _vrDeviceInfo'lensDistortionValues #-}
_vrDeviceInfo'chromaAbCorrection :: Lens' RL.VrDeviceInfo [Float]
_vrDeviceInfo'chromaAbCorrection f device =
    (\chromaAbCorrection' -> device { RL.vrDeviceInfo'chromaAbCorrection = chromaAbCorrection' }) <$>
    f (RL.vrDeviceInfo'chromaAbCorrection device)
{-# INLINE _vrDeviceInfo'chromaAbCorrection #-}


_vrStereoConfig'projection :: Lens' RL.VrStereoConfig [RL.Matrix]
_vrStereoConfig'projection f config =
    (\projection' -> config { RL.vrStereoConfig'projection = projection' }) <$>
    f (RL.vrStereoConfig'projection config)
{-# INLINE _vrStereoConfig'projection #-}
_vrStereoConfig'viewOffset :: Lens' RL.VrStereoConfig [RL.Matrix]
_vrStereoConfig'viewOffset f config =
    (\viewOffset' -> config { RL.vrStereoConfig'viewOffset = viewOffset' }) <$>
    f (RL.vrStereoConfig'viewOffset config)
{-# INLINE _vrStereoConfig'viewOffset #-}
_vrStereoConfig'leftLensCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'leftLensCenter f config =
    (\leftLensCenter' -> config { RL.vrStereoConfig'leftLensCenter = leftLensCenter' }) <$>
    f (RL.vrStereoConfig'leftLensCenter config)
{-# INLINE _vrStereoConfig'leftLensCenter #-}
_vrStereoConfig'rightLensCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'rightLensCenter f config =
    (\rightLensCenter' -> config { RL.vrStereoConfig'rightLensCenter = rightLensCenter' }) <$>
    f (RL.vrStereoConfig'rightLensCenter config)
{-# INLINE _vrStereoConfig'rightLensCenter #-}
_vrStereoConfig'leftScreenCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'leftScreenCenter f config =
    (\leftScreenCenter' -> config { RL.vrStereoConfig'leftScreenCenter = leftScreenCenter' }) <$>
    f (RL.vrStereoConfig'leftScreenCenter config)
{-# INLINE _vrStereoConfig'leftScreenCenter #-}
_vrStereoConfig'rightScreenCenter :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'rightScreenCenter f config =
    (\rightScreenCenter' -> config { RL.vrStereoConfig'rightScreenCenter = rightScreenCenter' }) <$>
    f (RL.vrStereoConfig'rightScreenCenter config)
{-# INLINE _vrStereoConfig'rightScreenCenter #-}
_vrStereoConfig'scale :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'scale f config =
    (\scale' -> config { RL.vrStereoConfig'scale = scale' }) <$>
    f (RL.vrStereoConfig'scale config)
{-# INLINE _vrStereoConfig'scale #-}
_vrStereoConfig'scaleIn :: Lens' RL.VrStereoConfig [Float]
_vrStereoConfig'scaleIn f config =
    (\scaleIn' -> config { RL.vrStereoConfig'scaleIn = scaleIn' }) <$>
    f (RL.vrStereoConfig'scaleIn config)
{-# INLINE _vrStereoConfig'scaleIn #-}


_filePathList'capacity :: Lens' RL.FilePathList Integer
_filePathList'capacity f (RL.FilePathList capacity paths) =
    (\capacity' -> RL.FilePathList capacity' paths) <$> f capacity
{-# INLINE _filePathList'capacity #-}
_filePathList'paths :: Lens' RL.FilePathList [String]
_filePathList'paths f (RL.FilePathList capacity paths) =
    (\paths' -> RL.FilePathList capacity paths') <$> f paths
{-# INLINE _filePathList'paths #-}


_rlVertexBuffer'elementCount :: Lens' RL.RLVertexBuffer Int
_rlVertexBuffer'elementCount f buffer =
    (\elementCount' -> buffer { RL.rlVertexBuffer'elementCount = elementCount' }) <$>
    f (RL.rlVertexBuffer'elementCount buffer)
{-# INLINE _rlVertexBuffer'elementCount #-}
_rlVertexBuffer'vertices :: Lens' RL.RLVertexBuffer [RL.Vector3]
_rlVertexBuffer'vertices f buffer =
    (\vertices' -> buffer { RL.rlVertexBuffer'vertices = vertices' }) <$>
    f (RL.rlVertexBuffer'vertices buffer)
{-# INLINE _rlVertexBuffer'vertices #-}
_rlVertexBuffer'texcoords :: Lens' RL.RLVertexBuffer [RL.Vector2]
_rlVertexBuffer'texcoords f buffer =
    (\texcoords' -> buffer { RL.rlVertexBuffer'texcoords = texcoords' }) <$>
    f (RL.rlVertexBuffer'texcoords buffer)
{-# INLINE _rlVertexBuffer'texcoords #-}
_rlVertexBuffer'colors :: Lens' RL.RLVertexBuffer [RL.Color]
_rlVertexBuffer'colors f buffer =
    (\colors' -> buffer { RL.rlVertexBuffer'colors = colors' }) <$>
    f (RL.rlVertexBuffer'colors buffer)
{-# INLINE _rlVertexBuffer'colors #-}
_rlVertexBuffer'indices :: Lens' RL.RLVertexBuffer [Integer]
_rlVertexBuffer'indices f buffer =
    (\indices' -> buffer { RL.rlVertexBuffer'indices = indices' }) <$>
    f (RL.rlVertexBuffer'indices buffer)
{-# INLINE _rlVertexBuffer'indices #-}
_rlVertexBuffer'vaoId :: Lens' RL.RLVertexBuffer Integer
_rlVertexBuffer'vaoId f buffer =
    (\vaoId' -> buffer { RL.rlVertexBuffer'vaoId = vaoId' }) <$>
    f (RL.rlVertexBuffer'vaoId buffer)
{-# INLINE _rlVertexBuffer'vaoId #-}
_rlVertexBuffer'vboId :: Lens' RL.RLVertexBuffer [Integer]
_rlVertexBuffer'vboId f buffer =
    (\vboId' -> buffer { RL.rlVertexBuffer'vboId = vboId' }) <$>
    f (RL.rlVertexBuffer'vboId buffer)
{-# INLINE _rlVertexBuffer'vboId #-}


_rlDrawCall'mode :: Lens' RL.RLDrawCall RL.RLDrawMode
_rlDrawCall'mode f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\mode' -> RL.RLDrawCall mode' vertexCount vertexAlignment textureId) <$>
    f mode
{-# INLINE _rlDrawCall'mode #-}
_rlDrawCall'vertexCount :: Lens' RL.RLDrawCall Int
_rlDrawCall'vertexCount f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\vertexCount' -> RL.RLDrawCall mode vertexCount' vertexAlignment textureId) <$>
    f vertexCount
{-# INLINE _rlDrawCall'vertexCount #-}
_rlDrawCall'vertexAlignment :: Lens' RL.RLDrawCall Int
_rlDrawCall'vertexAlignment f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\vertexAlignment' -> RL.RLDrawCall mode vertexCount vertexAlignment' textureId) <$>
    f vertexAlignment
{-# INLINE _rlDrawCall'vertexAlignment #-}
_rlDrawCall'textureId :: Lens' RL.RLDrawCall Integer
_rlDrawCall'textureId f (RL.RLDrawCall mode vertexCount vertexAlignment textureId) =
    (\textureId' -> RL.RLDrawCall mode vertexCount vertexAlignment textureId') <$>
    f textureId
{-# INLINE _rlDrawCall'textureId #-}


_rlRenderBatch'bufferCount :: Lens' RL.RLRenderBatch Int
_rlRenderBatch'bufferCount f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\bufferCount' -> RL.RLRenderBatch bufferCount' currentBuffer vertexBuffers draws drawCounter currentDepth) <$>
    f bufferCount
{-# INLINE _rlRenderBatch'bufferCount #-}
_rlRenderBatch'currentBuffer :: Lens' RL.RLRenderBatch Int
_rlRenderBatch'currentBuffer f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\currentBuffer' -> RL.RLRenderBatch bufferCount currentBuffer' vertexBuffers draws drawCounter currentDepth) <$>
    f currentBuffer
{-# INLINE _rlRenderBatch'currentBuffer #-}
_rlRenderBatch'vertexBuffers :: Lens' RL.RLRenderBatch [RL.RLVertexBuffer]
_rlRenderBatch'vertexBuffers f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\vertexBuffers' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers' draws drawCounter currentDepth) <$>
    f vertexBuffers
{-# INLINE _rlRenderBatch'vertexBuffers #-}
_rlRenderBatch'draws :: Lens' RL.RLRenderBatch [RL.RLDrawCall]
_rlRenderBatch'draws f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\draws' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws' drawCounter currentDepth) <$>
    f draws
{-# INLINE _rlRenderBatch'draws #-}
_rlRenderBatch'drawCounter :: Lens' RL.RLRenderBatch Int
_rlRenderBatch'drawCounter f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\drawCounter' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter' currentDepth) <$>
    f drawCounter
{-# INLINE _rlRenderBatch'drawCounter #-}
_rlRenderBatch'currentDepth :: Lens' RL.RLRenderBatch Float
_rlRenderBatch'currentDepth f (RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) =
    (\currentDepth' -> RL.RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth') <$>
    f currentDepth
{-# INLINE _rlRenderBatch'currentDepth #-}
