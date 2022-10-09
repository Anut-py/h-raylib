{-# OPTIONS -Wall #-}
module Raylib.Types where

-- This file includes Haskell counterparts to the structs defined in raylib

import Foreign.C
    ( CString, CChar, CUShort, CUInt, CInt, CUChar, CFloat )
import Foreign
    ( Ptr,
      plusPtr,
      pokeArray,
      peekArray,
      Storable(pokeByteOff, poke, peek, alignment, sizeOf, peekByteOff) )

{- typedef struct Vector2 {
            float x; float y;
        } Vector2; -}
data Vector2 = Vector2
  { vector2'x :: CFloat,
    vector2'y :: CFloat
  }
  deriving (Eq, Show)

p'Vector2'x p = plusPtr p 0

p'Vector2'x :: Ptr Vector2 -> Ptr CFloat

p'Vector2'y p = plusPtr p 4

p'Vector2'y :: Ptr Vector2 -> Ptr CFloat

instance Storable Vector2 where
  sizeOf _ = 8
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    return $ Vector2 v0 v1
  poke _p (Vector2 v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    return ()

{- typedef struct Vector3 {
            float x; float y; float z;
        } Vector3; -}
data Vector3 = Vector3
  { vector3'x :: CFloat,
    vector3'y :: CFloat,
    vector3'z :: CFloat
  }
  deriving (Eq, Show)

p'Vector3'x p = plusPtr p 0

p'Vector3'x :: Ptr Vector3 -> Ptr CFloat

p'Vector3'y p = plusPtr p 4

p'Vector3'y :: Ptr Vector3 -> Ptr CFloat

p'Vector3'z p = plusPtr p 8

p'Vector3'z :: Ptr Vector3 -> Ptr CFloat

instance Storable Vector3 where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    return $ Vector3 v0 v1 v2
  poke _p (Vector3 v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    return ()

{- typedef struct Vector4 {
            float x; float y; float z; float w;
        } Vector4; -}
data Vector4 = Vector4
  { vector4'x :: CFloat,
    vector4'y :: CFloat,
    vector4'z :: CFloat,
    vector4'w :: CFloat
  }
  deriving (Eq, Show)

p'Vector4'x p = plusPtr p 0

p'Vector4'x :: Ptr Vector4 -> Ptr CFloat

p'Vector4'y p = plusPtr p 4

p'Vector4'y :: Ptr Vector4 -> Ptr CFloat

p'Vector4'z p = plusPtr p 8

p'Vector4'z :: Ptr Vector4 -> Ptr CFloat

p'Vector4'w p = plusPtr p 12

p'Vector4'w :: Ptr Vector4 -> Ptr CFloat

instance Storable Vector4 where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    return $ Vector4 v0 v1 v2 v3
  poke _p (Vector4 v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    return ()

{- typedef Vector4 Quaternion; -}
type Quaternion = Vector4

{- typedef struct Matrix {
            float m0, m4, m8, m12;
            float m1, m5, m9, m13;
            float m2, m6, m10, m14;
            float m3, m7, m11, m15;
        } Matrix; -}
data Matrix = Matrix
  { matrix'm0 :: CFloat,
    matrix'm4 :: CFloat,
    matrix'm8 :: CFloat,
    matrix'm12 :: CFloat,
    matrix'm1 :: CFloat,
    matrix'm5 :: CFloat,
    matrix'm9 :: CFloat,
    matrix'm13 :: CFloat,
    matrix'm2 :: CFloat,
    matrix'm6 :: CFloat,
    matrix'm10 :: CFloat,
    matrix'm14 :: CFloat,
    matrix'm3 :: CFloat,
    matrix'm7 :: CFloat,
    matrix'm11 :: CFloat,
    matrix'm15 :: CFloat
  }
  deriving (Eq, Show)

p'Matrix'm0 p = plusPtr p 0

p'Matrix'm0 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm4 p = plusPtr p 4

p'Matrix'm4 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm8 p = plusPtr p 8

p'Matrix'm8 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm12 p = plusPtr p 12

p'Matrix'm12 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm1 p = plusPtr p 16

p'Matrix'm1 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm5 p = plusPtr p 20

p'Matrix'm5 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm9 p = plusPtr p 24

p'Matrix'm9 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm13 p = plusPtr p 28

p'Matrix'm13 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm2 p = plusPtr p 32

p'Matrix'm2 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm6 p = plusPtr p 36

p'Matrix'm6 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm10 p = plusPtr p 40

p'Matrix'm10 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm14 p = plusPtr p 44

p'Matrix'm14 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm3 p = plusPtr p 48

p'Matrix'm3 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm7 p = plusPtr p 52

p'Matrix'm7 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm11 p = plusPtr p 56

p'Matrix'm11 :: Ptr Matrix -> Ptr CFloat

p'Matrix'm15 p = plusPtr p 60

p'Matrix'm15 :: Ptr Matrix -> Ptr CFloat

instance Storable Matrix where
  sizeOf _ = 64
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    v5 <- peekByteOff _p 20
    v6 <- peekByteOff _p 24
    v7 <- peekByteOff _p 28
    v8 <- peekByteOff _p 32
    v9 <- peekByteOff _p 36
    v10 <- peekByteOff _p 40
    v11 <- peekByteOff _p 44
    v12 <- peekByteOff _p 48
    v13 <- peekByteOff _p 52
    v14 <- peekByteOff _p 56
    v15 <- peekByteOff _p 60
    return $ Matrix v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  poke _p (Matrix v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    pokeByteOff _p 20 v5
    pokeByteOff _p 24 v6
    pokeByteOff _p 28 v7
    pokeByteOff _p 32 v8
    pokeByteOff _p 36 v9
    pokeByteOff _p 40 v10
    pokeByteOff _p 44 v11
    pokeByteOff _p 48 v12
    pokeByteOff _p 52 v13
    pokeByteOff _p 56 v14
    pokeByteOff _p 60 v15
    return ()

{- typedef struct Color {
            unsigned char r; unsigned char g; unsigned char b; unsigned char a;
        } Color; -}
data Color = Color
  { color'r :: CUChar,
    color'g :: CUChar,
    color'b :: CUChar,
    color'a :: CUChar
  }
  deriving (Eq, Show)

p'Color'r p = plusPtr p 0

p'Color'r :: Ptr Color -> Ptr CUChar

p'Color'g p = plusPtr p 1

p'Color'g :: Ptr Color -> Ptr CUChar

p'Color'b p = plusPtr p 2

p'Color'b :: Ptr Color -> Ptr CUChar

p'Color'a p = plusPtr p 3

p'Color'a :: Ptr Color -> Ptr CUChar

instance Storable Color where
  sizeOf _ = 4
  alignment _ = 1
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 1
    v2 <- peekByteOff _p 2
    v3 <- peekByteOff _p 3
    return $ Color v0 v1 v2 v3
  poke _p (Color v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 1 v1
    pokeByteOff _p 2 v2
    pokeByteOff _p 3 v3
    return ()

{- typedef struct Rectangle {
            float x; float y; float width; float height;
        } Rectangle; -}
data Rectangle = Rectangle
  { rectangle'x :: CFloat,
    rectangle'y :: CFloat,
    rectangle'width :: CFloat,
    rectangle'height :: CFloat
  }
  deriving (Eq, Show)

p'Rectangle'x p = plusPtr p 0

p'Rectangle'x :: Ptr Rectangle -> Ptr CFloat

p'Rectangle'y p = plusPtr p 4

p'Rectangle'y :: Ptr Rectangle -> Ptr CFloat

p'Rectangle'width p = plusPtr p 8

p'Rectangle'width :: Ptr Rectangle -> Ptr CFloat

p'Rectangle'height p = plusPtr p 12

p'Rectangle'height :: Ptr Rectangle -> Ptr CFloat

instance Storable Rectangle where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    return $ Rectangle v0 v1 v2 v3
  poke _p (Rectangle v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    return ()

{- typedef struct Image {
            void * data; int width; int height; int mipmaps; int format;
        } Image; -}
data Image = Image
  { image'data :: Ptr (),
    image'width :: CInt,
    image'height :: CInt,
    image'mipmaps :: CInt,
    image'format :: CInt
  }
  deriving (Eq, Show)

p'Image'data p = plusPtr p 0

p'Image'data :: Ptr Image -> Ptr (Ptr ())

p'Image'width p = plusPtr p 4

p'Image'width :: Ptr Image -> Ptr CInt

p'Image'height p = plusPtr p 8

p'Image'height :: Ptr Image -> Ptr CInt

p'Image'mipmaps p = plusPtr p 12

p'Image'mipmaps :: Ptr Image -> Ptr CInt

p'Image'format p = plusPtr p 16

p'Image'format :: Ptr Image -> Ptr CInt

instance Storable Image where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ Image v0 v1 v2 v3 v4
  poke _p (Image v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct Texture {
            unsigned int id; int width; int height; int mipmaps; int format;
        } Texture; -}
data Texture = Texture
  { texture'id :: CUInt,
    texture'width :: CInt,
    texture'height :: CInt,
    texture'mipmaps :: CInt,
    texture'format :: CInt
  }
  deriving (Eq, Show)

p'Texture'id p = plusPtr p 0

p'Texture'id :: Ptr Texture -> Ptr CUInt

p'Texture'width p = plusPtr p 4

p'Texture'width :: Ptr Texture -> Ptr CInt

p'Texture'height p = plusPtr p 8

p'Texture'height :: Ptr Texture -> Ptr CInt

p'Texture'mipmaps p = plusPtr p 12

p'Texture'mipmaps :: Ptr Texture -> Ptr CInt

p'Texture'format p = plusPtr p 16

p'Texture'format :: Ptr Texture -> Ptr CInt

instance Storable Texture where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ Texture v0 v1 v2 v3 v4
  poke _p (Texture v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef Texture Texture2D; -}
type Texture2D = Texture

{- typedef Texture TextureCubemap; -}
type TextureCubemap = Texture

{- typedef struct RenderTexture {
            unsigned int id; Texture texture; Texture depth;
        } RenderTexture; -}
data RenderTexture = RenderTexture
  { rendertexture'id :: CUInt,
    rendertexture'texture :: Texture,
    rendertexture'depth :: Texture
  }
  deriving (Eq, Show)

p'RenderTexture'id p = plusPtr p 0

p'RenderTexture'id :: Ptr RenderTexture -> Ptr CUInt

p'RenderTexture'texture p = plusPtr p 4

p'RenderTexture'texture :: Ptr RenderTexture -> Ptr Texture

p'RenderTexture'depth p = plusPtr p 24

p'RenderTexture'depth :: Ptr RenderTexture -> Ptr Texture

instance Storable RenderTexture where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 24
    return $ RenderTexture v0 v1 v2
  poke _p (RenderTexture v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 24 v2
    return ()

{- typedef RenderTexture RenderTexture2D; -}
type RenderTexture2D = RenderTexture

{- typedef struct NPatchInfo {
            Rectangle source;
            int left;
            int top;
            int right;
            int bottom;
            int layout;
        } NPatchInfo; -}
data NPatchInfo = NPatchInfo
  { nPatchinfo'source :: Rectangle,
    nPatchinfo'left :: CInt,
    nPatchinfo'top :: CInt,
    nPatchinfo'right :: CInt,
    nPatchinfo'bottom :: CInt,
    nPatchinfo'layout :: CInt
  }
  deriving (Eq, Show)

p'NPatchInfo'source p = plusPtr p 0

p'NPatchInfo'source :: Ptr NPatchInfo -> Ptr Rectangle

p'NPatchInfo'left p = plusPtr p 16

p'NPatchInfo'left :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'top p = plusPtr p 20

p'NPatchInfo'top :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'right p = plusPtr p 24

p'NPatchInfo'right :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'bottom p = plusPtr p 28

p'NPatchInfo'bottom :: Ptr NPatchInfo -> Ptr CInt

p'NPatchInfo'layout p = plusPtr p 32

p'NPatchInfo'layout :: Ptr NPatchInfo -> Ptr CInt

instance Storable NPatchInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 20
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 28
    v5 <- peekByteOff _p 32
    return $ NPatchInfo v0 v1 v2 v3 v4 v5
  poke _p (NPatchInfo v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 20 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 28 v4
    pokeByteOff _p 32 v5
    return ()

{- typedef struct GlyphInfo {
            int value; int offsetX; int offsetY; int advanceX; Image image;
        } GlyphInfo; -}
data GlyphInfo = GlyphInfo
  { glyphinfo'value :: CInt,
    glyphInfo'offsetX :: CInt,
    glyphInfo'offsetY :: CInt,
    glyphInfo'advanceX :: CInt,
    glyphinfo'image :: Image
  }
  deriving (Eq, Show)

p'GlyphInfo'value p = plusPtr p 0

p'GlyphInfo'value :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'offsetX p = plusPtr p 4

p'GlyphInfo'offsetX :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'offsetY p = plusPtr p 8

p'GlyphInfo'offsetY :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'advanceX p = plusPtr p 12

p'GlyphInfo'advanceX :: Ptr GlyphInfo -> Ptr CInt

p'GlyphInfo'image p = plusPtr p 16

p'GlyphInfo'image :: Ptr GlyphInfo -> Ptr Image

instance Storable GlyphInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ GlyphInfo v0 v1 v2 v3 v4
  poke _p (GlyphInfo v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct Font {
            int baseSize;
            int glyphCount;
            int glyphPadding;
            Texture2D texture;
            Rectangle * recs;
            GlyphInfo * glyphs;
        } Font; -}
data Font = Font
  { font'baseSize :: CInt,
    font'glyphCount :: CInt,
    font'glyphPadding :: CInt,
    font'texture :: Texture,
    font'recs :: Ptr Rectangle,
    font'glyphs :: Ptr GlyphInfo
  }
  deriving (Eq, Show)

p'Font'baseSize p = plusPtr p 0

p'Font'baseSize :: Ptr Font -> Ptr CInt

p'Font'glyphCount p = plusPtr p 4

p'Font'glyphCount :: Ptr Font -> Ptr CInt

p'Font'glyphPadding p = plusPtr p 8

p'Font'glyphPadding :: Ptr Font -> Ptr CInt

p'Font'texture p = plusPtr p 12

p'Font'texture :: Ptr Font -> Ptr Texture

p'Font'recs p = plusPtr p 32

p'Font'recs :: Ptr Font -> Ptr (Ptr Rectangle)

p'Font'glyphs p = plusPtr p 36

p'Font'glyphs :: Ptr Font -> Ptr (Ptr GlyphInfo)

instance Storable Font where
  sizeOf _ = 40
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 32
    v5 <- peekByteOff _p 36
    return $ Font v0 v1 v2 v3 v4 v5
  poke _p (Font v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 32 v4
    pokeByteOff _p 36 v5
    return ()

{- typedef struct Camera3D {
            Vector3 position;
            Vector3 target;
            Vector3 up;
            float fovy;
            int projection;
        } Camera3D; -}
data Camera3D = Camera3D
  { camera3D'position :: Vector3,
    camera3D'target :: Vector3,
    camera3D'up :: Vector3,
    camera3d'fovy :: CFloat,
    camera3d'projection :: CInt
  }
  deriving (Eq, Show)

p'Camera3D'position p = plusPtr p 0

p'Camera3D'position :: Ptr Camera3D -> Ptr Vector3

p'Camera3D'target p = plusPtr p 12

p'Camera3D'target :: Ptr Camera3D -> Ptr Vector3

p'Camera3D'up p = plusPtr p 24

p'Camera3D'up :: Ptr Camera3D -> Ptr Vector3

p'Camera3D'fovy p = plusPtr p 36

p'Camera3D'fovy :: Ptr Camera3D -> Ptr CFloat

p'Camera3D'projection p = plusPtr p 40

p'Camera3D'projection :: Ptr Camera3D -> Ptr CInt

instance Storable Camera3D where
  sizeOf _ = 44
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 36
    v4 <- peekByteOff _p 40
    return $ Camera3D v0 v1 v2 v3 v4
  poke _p (Camera3D v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 36 v3
    pokeByteOff _p 40 v4
    return ()

{- typedef Camera3D Camera; -}
type Camera = Camera3D

{- typedef struct Camera2D {
            Vector2 offset; Vector2 target; float rotation; float zoom;
        } Camera2D; -}
data Camera2D = Camera2D
  { camera2D'offset :: Vector2,
    camera2D'target :: Vector2,
    camera2d'rotation :: CFloat,
    camera2d'zoom :: CFloat
  }
  deriving (Eq, Show)

p'Camera2D'offset p = plusPtr p 0

p'Camera2D'offset :: Ptr Camera2D -> Ptr Vector2

p'Camera2D'target p = plusPtr p 8

p'Camera2D'target :: Ptr Camera2D -> Ptr Vector2

p'Camera2D'rotation p = plusPtr p 16

p'Camera2D'rotation :: Ptr Camera2D -> Ptr CFloat

p'Camera2D'zoom p = plusPtr p 20

p'Camera2D'zoom :: Ptr Camera2D -> Ptr CFloat

instance Storable Camera2D where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 20
    return $ Camera2D v0 v1 v2 v3
  poke _p (Camera2D v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 20 v3
    return ()

{- typedef struct Mesh {
            int vertexCount;
            int triangleCount;
            float * vertices;
            float * texcoords;
            float * texcoords2;
            float * normals;
            float * tangents;
            unsigned char * colors;
            unsigned short * indices;
            float * animVertices;
            float * animNormals;
            unsigned char * boneIds;
            float * boneWeights;
            unsigned int vaoId;
            unsigned int * vboId;
        } Mesh; -}
data Mesh = Mesh
  { mesh'vertexCount :: CInt,
    mesh'triangleCount :: CInt,
    mesh'vertices :: Ptr CFloat,
    mesh'texcoords :: Ptr CFloat,
    mesh'texcoords2 :: Ptr CFloat,
    mesh'normals :: Ptr CFloat,
    mesh'tangents :: Ptr CFloat,
    mesh'colors :: Ptr CUChar,
    mesh'indices :: Ptr CUShort,
    mesh'animVertices :: Ptr CFloat,
    mesh'animNormals :: Ptr CFloat,
    mesh'boneIds :: Ptr CUChar,
    mesh'boneWeights :: Ptr CFloat,
    mesh'vaoId :: CUInt,
    mesh'vboId :: Ptr CUInt
  }
  deriving (Eq, Show)

p'Mesh'vertexCount p = plusPtr p 0

p'Mesh'vertexCount :: Ptr Mesh -> Ptr CInt

p'Mesh'triangleCount p = plusPtr p 4

p'Mesh'triangleCount :: Ptr Mesh -> Ptr CInt

p'Mesh'vertices p = plusPtr p 8

p'Mesh'vertices :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'texcoords p = plusPtr p 12

p'Mesh'texcoords :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'texcoords2 p = plusPtr p 16

p'Mesh'texcoords2 :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'normals p = plusPtr p 20

p'Mesh'normals :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'tangents p = plusPtr p 24

p'Mesh'tangents :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'colors p = plusPtr p 28

p'Mesh'colors :: Ptr Mesh -> Ptr (Ptr CUChar)

p'Mesh'indices p = plusPtr p 32

p'Mesh'indices :: Ptr Mesh -> Ptr (Ptr CUShort)

p'Mesh'animVertices p = plusPtr p 36

p'Mesh'animVertices :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'animNormals p = plusPtr p 40

p'Mesh'animNormals :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'boneIds p = plusPtr p 44

p'Mesh'boneIds :: Ptr Mesh -> Ptr (Ptr CUChar)

p'Mesh'boneWeights p = plusPtr p 48

p'Mesh'boneWeights :: Ptr Mesh -> Ptr (Ptr CFloat)

p'Mesh'vaoId p = plusPtr p 52

p'Mesh'vaoId :: Ptr Mesh -> Ptr CUInt

p'Mesh'vboId p = plusPtr p 56

p'Mesh'vboId :: Ptr Mesh -> Ptr (Ptr CUInt)

instance Storable Mesh where
  sizeOf _ = 60
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    v5 <- peekByteOff _p 20
    v6 <- peekByteOff _p 24
    v7 <- peekByteOff _p 28
    v8 <- peekByteOff _p 32
    v9 <- peekByteOff _p 36
    v10 <- peekByteOff _p 40
    v11 <- peekByteOff _p 44
    v12 <- peekByteOff _p 48
    v13 <- peekByteOff _p 52
    v14 <- peekByteOff _p 56
    return $ Mesh v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  poke _p (Mesh v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    pokeByteOff _p 20 v5
    pokeByteOff _p 24 v6
    pokeByteOff _p 28 v7
    pokeByteOff _p 32 v8
    pokeByteOff _p 36 v9
    pokeByteOff _p 40 v10
    pokeByteOff _p 44 v11
    pokeByteOff _p 48 v12
    pokeByteOff _p 52 v13
    pokeByteOff _p 56 v14
    return ()

{- typedef struct Shader {
            unsigned int id; int * locs;
        } Shader; -}
data Shader = Shader
  { shader'id :: CUInt,
    shader'locs :: Ptr CInt
  }
  deriving (Eq, Show)

p'Shader'id p = plusPtr p 0

p'Shader'id :: Ptr Shader -> Ptr CUInt

p'Shader'locs p = plusPtr p 4

p'Shader'locs :: Ptr Shader -> Ptr (Ptr CInt)

instance Storable Shader where
  sizeOf _ = 8
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    return $ Shader v0 v1
  poke _p (Shader v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    return ()

{- typedef struct MaterialMap {
            Texture2D texture; Color color; float value;
        } MaterialMap; -}
data MaterialMap = MaterialMap
  { materialmap'texture :: Texture,
    materialmap'color :: Color,
    materialmap'value :: CFloat
  }
  deriving (Eq, Show)

p'MaterialMap'texture p = plusPtr p 0

p'MaterialMap'texture :: Ptr MaterialMap -> Ptr Texture

p'MaterialMap'color p = plusPtr p 20

p'MaterialMap'color :: Ptr MaterialMap -> Ptr Color

p'MaterialMap'value p = plusPtr p 24

p'MaterialMap'value :: Ptr MaterialMap -> Ptr CFloat

instance Storable MaterialMap where
  sizeOf _ = 28
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 20
    v2 <- peekByteOff _p 24
    return $ MaterialMap v0 v1 v2
  poke _p (MaterialMap v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 20 v1
    pokeByteOff _p 24 v2
    return ()

{- typedef struct Material {
            Shader shader; MaterialMap * maps; float params[4];
        } Material; -}
data Material = Material
  { material'shader :: Shader,
    material'maps :: Ptr MaterialMap,
    material'params :: [CFloat]
  }
  deriving (Eq, Show)

p'Material'shader p = plusPtr p 0

p'Material'shader :: Ptr Material -> Ptr Shader

p'Material'maps p = plusPtr p 8

p'Material'maps :: Ptr Material -> Ptr (Ptr MaterialMap)

p'Material'params p = plusPtr p 12

p'Material'params :: Ptr Material -> Ptr CFloat

instance Storable Material where
  sizeOf _ = 28
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- let s2 = div 16 $ sizeOf (undefined :: CFloat) in peekArray s2 (plusPtr _p 12)
    return $ Material v0 v1 v2
  poke _p (Material v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    let s2 = div 16 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 12) (take s2 v2)
    return ()

{- typedef struct Transform {
            Vector3 translation; Quaternion rotation; Vector3 scale;
        } Transform; -}
data Transform = Transform
  { transform'translation :: Vector3,
    transform'rotation :: Vector4,
    transform'scale :: Vector3
  }
  deriving (Eq, Show)

p'Transform'translation p = plusPtr p 0

p'Transform'translation :: Ptr Transform -> Ptr Vector3

p'Transform'rotation p = plusPtr p 12

p'Transform'rotation :: Ptr Transform -> Ptr Vector4

p'Transform'scale p = plusPtr p 28

p'Transform'scale :: Ptr Transform -> Ptr Vector3

instance Storable Transform where
  sizeOf _ = 40
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    v2 <- peekByteOff _p 28
    return $ Transform v0 v1 v2
  poke _p (Transform v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    pokeByteOff _p 28 v2
    return ()

{- typedef struct BoneInfo {
            char name[32]; int parent;
        } BoneInfo; -}
data BoneInfo = BoneInfo
  { boneInfo'name :: [CChar],
    boneinfo'parent :: CInt
  }
  deriving (Eq, Show)

p'BoneInfo'name p = plusPtr p 0

p'BoneInfo'name :: Ptr BoneInfo -> Ptr CChar

p'BoneInfo'parent p = plusPtr p 32

p'BoneInfo'parent :: Ptr BoneInfo -> Ptr CInt

instance Storable BoneInfo where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- let s0 = div 32 $ sizeOf (undefined :: CChar) in peekArray s0 (plusPtr _p 0)
    v1 <- peekByteOff _p 32
    return $ BoneInfo v0 v1
  poke _p (BoneInfo v0 v1) = do
    let s0 = div 32 $ sizeOf (undefined :: CChar)
    pokeArray (plusPtr _p 0) (take s0 v0)
    pokeByteOff _p 32 v1
    return ()

{- typedef struct Model {
            Matrix transform;
            int meshCount;
            int materialCount;
            Mesh * meshes;
            Material * materials;
            int * meshMaterial;
            int boneCount;
            BoneInfo * bones;
            Transform * bindPose;
        } Model; -}
data Model = Model
  { model'transform :: Matrix,
    model'meshCount :: CInt,
    model'materialCount :: CInt,
    model'meshes :: Ptr Mesh,
    model'materials :: Ptr Material,
    model'meshMaterial :: Ptr CInt,
    model'boneCount :: CInt,
    model'bones :: Ptr BoneInfo,
    model'bindPose :: Ptr Transform
  }
  deriving (Eq, Show)

p'Model'transform p = plusPtr p 0

p'Model'transform :: Ptr Model -> Ptr Matrix

p'Model'meshCount p = plusPtr p 64

p'Model'meshCount :: Ptr Model -> Ptr CInt

p'Model'materialCount p = plusPtr p 68

p'Model'materialCount :: Ptr Model -> Ptr CInt

p'Model'meshes p = plusPtr p 72

p'Model'meshes :: Ptr Model -> Ptr (Ptr Mesh)

p'Model'materials p = plusPtr p 76

p'Model'materials :: Ptr Model -> Ptr (Ptr Material)

p'Model'meshMaterial p = plusPtr p 80

p'Model'meshMaterial :: Ptr Model -> Ptr (Ptr CInt)

p'Model'boneCount p = plusPtr p 84

p'Model'boneCount :: Ptr Model -> Ptr CInt

p'Model'bones p = plusPtr p 88

p'Model'bones :: Ptr Model -> Ptr (Ptr BoneInfo)

p'Model'bindPose p = plusPtr p 92

p'Model'bindPose :: Ptr Model -> Ptr (Ptr Transform)

instance Storable Model where
  sizeOf _ = 96
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 64
    v2 <- peekByteOff _p 68
    v3 <- peekByteOff _p 72
    v4 <- peekByteOff _p 76
    v5 <- peekByteOff _p 80
    v6 <- peekByteOff _p 84
    v7 <- peekByteOff _p 88
    v8 <- peekByteOff _p 92
    return $ Model v0 v1 v2 v3 v4 v5 v6 v7 v8
  poke _p (Model v0 v1 v2 v3 v4 v5 v6 v7 v8) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 64 v1
    pokeByteOff _p 68 v2
    pokeByteOff _p 72 v3
    pokeByteOff _p 76 v4
    pokeByteOff _p 80 v5
    pokeByteOff _p 84 v6
    pokeByteOff _p 88 v7
    pokeByteOff _p 92 v8
    return ()

{- typedef struct ModelAnimation {
            int boneCount;
            int frameCount;
            BoneInfo * bones;
            Transform * * framePoses;
        } ModelAnimation; -}
data ModelAnimation = ModelAnimation
  { modelAnimation'boneCount :: CInt,
    modelAnimation'frameCount :: CInt,
    modelAnimation'bones :: Ptr BoneInfo,
    modelAnimation'framePoses :: Ptr (Ptr Transform)
  }
  deriving (Eq, Show)

p'ModelAnimation'boneCount p = plusPtr p 0

p'ModelAnimation'boneCount :: Ptr ModelAnimation -> Ptr CInt

p'ModelAnimation'frameCount p = plusPtr p 4

p'ModelAnimation'frameCount :: Ptr ModelAnimation -> Ptr CInt

p'ModelAnimation'bones p = plusPtr p 8

p'ModelAnimation'bones :: Ptr ModelAnimation -> Ptr (Ptr BoneInfo)

p'ModelAnimation'framePoses p = plusPtr p 12

p'ModelAnimation'framePoses :: Ptr ModelAnimation -> Ptr (Ptr (Ptr Transform))

instance Storable ModelAnimation where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    return $ ModelAnimation v0 v1 v2 v3
  poke _p (ModelAnimation v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    return ()

{- typedef struct Ray {
            Vector3 position; Vector3 direction;
        } Ray; -}
data Ray = Ray
  { ray'position :: Vector3,
    ray'direction :: Vector3
  }
  deriving (Eq, Show)

p'Ray'position p = plusPtr p 0

p'Ray'position :: Ptr Ray -> Ptr Vector3

p'Ray'direction p = plusPtr p 12

p'Ray'direction :: Ptr Ray -> Ptr Vector3

instance Storable Ray where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    return $ Ray v0 v1
  poke _p (Ray v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    return ()

{- typedef struct RayCollision {
            _Bool hit; float distance; Vector3 point; Vector3 normal;
        } RayCollision; -}
data RayCollision = RayCollision
  { raycollision'hit :: CInt,
    raycollision'distance :: CFloat,
    rayCollision'point :: Vector3,
    rayCollision'normal :: Vector3
  }
  deriving (Eq, Show)

p'RayCollision'hit p = plusPtr p 0

p'RayCollision'hit :: Ptr RayCollision -> Ptr CInt

p'RayCollision'distance p = plusPtr p 4

p'RayCollision'distance :: Ptr RayCollision -> Ptr CFloat

p'RayCollision'point p = plusPtr p 8

p'RayCollision'point :: Ptr RayCollision -> Ptr Vector3

p'RayCollision'normal p = plusPtr p 20

p'RayCollision'normal :: Ptr RayCollision -> Ptr Vector3

instance Storable RayCollision where
  sizeOf _ = 32
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 20
    return $ RayCollision v0 v1 v2 v3
  poke _p (RayCollision v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 20 v3
    return ()

{- typedef struct BoundingBox {
            Vector3 min; Vector3 max;
        } BoundingBox; -}
data BoundingBox = BoundingBox
  { boundingBox'min :: Vector3,
    boundingBox'max :: Vector3
  }
  deriving (Eq, Show)

p'BoundingBox'min p = plusPtr p 0

p'BoundingBox'min :: Ptr BoundingBox -> Ptr Vector3

p'BoundingBox'max p = plusPtr p 12

p'BoundingBox'max :: Ptr BoundingBox -> Ptr Vector3

instance Storable BoundingBox where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 12
    return $ BoundingBox v0 v1
  poke _p (BoundingBox v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 12 v1
    return ()

{- typedef struct Wave {
            unsigned int frameCount;
            unsigned int sampleRate;
            unsigned int sampleSize;
            unsigned int channels;
            void * data;
        } Wave; -}
data Wave = Wave
  { wave'frameCount :: CUInt,
    wave'sampleRate :: CUInt,
    wave'sampleSize :: CUInt,
    wave'channels :: CUInt,
    wave'data :: Ptr ()
  }
  deriving (Eq, Show)

p'Wave'frameCount p = plusPtr p 0

p'Wave'frameCount :: Ptr Wave -> Ptr CUInt

p'Wave'sampleRate p = plusPtr p 4

p'Wave'sampleRate :: Ptr Wave -> Ptr CUInt

p'Wave'sampleSize p = plusPtr p 8

p'Wave'sampleSize :: Ptr Wave -> Ptr CUInt

p'Wave'channels p = plusPtr p 12

p'Wave'channels :: Ptr Wave -> Ptr CUInt

p'Wave'data p = plusPtr p 16

p'Wave'data :: Ptr Wave -> Ptr (Ptr ())

instance Storable Wave where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ Wave v0 v1 v2 v3 v4
  poke _p (Wave v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct rAudioBuffer rAudioBuffer; -}
data RAudioBuffer = RAudioBuffer

{- typedef struct rAudioProcessor rAudioProcessor; -}
data RAudioProcessor = RAudioProcessor

{- typedef struct AudioStream {
            rAudioBuffer * buffer;
            rAudioProcessor * processor;
            unsigned int sampleRate;
            unsigned int sampleSize;
            unsigned int channels;
        } AudioStream; -}
data AudioStream = AudioStream
  { audioStream'buffer :: Ptr RAudioBuffer,
    audioStream'processor :: Ptr RAudioProcessor,
    audioStream'sampleRate :: CUInt,
    audioStream'sampleSize :: CUInt,
    audiostream'channels :: CUInt
  }
  deriving (Eq, Show)

p'AudioStream'buffer p = plusPtr p 0

p'AudioStream'buffer :: Ptr AudioStream -> Ptr (Ptr RAudioBuffer)

p'AudioStream'processor p = plusPtr p 4

p'AudioStream'processor :: Ptr AudioStream -> Ptr (Ptr rAudioProcessor)

p'AudioStream'sampleRate p = plusPtr p 8

p'AudioStream'sampleRate :: Ptr AudioStream -> Ptr CUInt

p'AudioStream'sampleSize p = plusPtr p 12

p'AudioStream'sampleSize :: Ptr AudioStream -> Ptr CUInt

p'AudioStream'channels p = plusPtr p 16

p'AudioStream'channels :: Ptr AudioStream -> Ptr CUInt

instance Storable AudioStream where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    return $ AudioStream v0 v1 v2 v3 v4
  poke _p (AudioStream v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    return ()

{- typedef struct Sound {
            AudioStream stream; unsigned int frameCount;
        } Sound; -}
data Sound = Sound
  { sound'stream :: AudioStream,
    sound'frameCount :: CUInt
  }
  deriving (Eq, Show)

p'Sound'stream p = plusPtr p 0

p'Sound'stream :: Ptr Sound -> Ptr AudioStream

p'Sound'frameCount p = plusPtr p 20

p'Sound'frameCount :: Ptr Sound -> Ptr CUInt

instance Storable Sound where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 20
    return $ Sound v0 v1
  poke _p (Sound v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 20 v1
    return ()

{- typedef struct Music {
            AudioStream stream;
            unsigned int frameCount;
            _Bool looping;
            int ctxType;
            void * ctxData;
        } Music; -}
data Music = Music
  { musistream :: AudioStream,
    musiframeCount :: CUInt,
    musilooping :: CInt,
    musictxType :: CInt,
    musictxData :: Ptr ()
  }
  deriving (Eq, Show)

p'Musistream p = plusPtr p 0

p'Musistream :: Ptr Music -> Ptr AudioStream

p'MusiframeCount p = plusPtr p 20

p'MusiframeCount :: Ptr Music -> Ptr CUInt

p'Musilooping p = plusPtr p 24

p'Musilooping :: Ptr Music -> Ptr CInt

p'MusictxType p = plusPtr p 28

p'MusictxType :: Ptr Music -> Ptr CInt

p'MusictxData p = plusPtr p 32

p'MusictxData :: Ptr Music -> Ptr (Ptr ())

instance Storable Music where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 20
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 28
    v4 <- peekByteOff _p 32
    return $ Music v0 v1 v2 v3 v4
  poke _p (Music v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 20 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 28 v3
    pokeByteOff _p 32 v4
    return ()

{- typedef struct VrDeviceInfo {
            int hResolution;
            int vResolution;
            float hScreenSize;
            float vScreenSize;
            float vScreenCenter;
            float eyeToScreenDistance;
            float lensSeparationDistance;
            float interpupillaryDistance;
            float lensDistortionValues[4];
            float chromaAbCorrection[4];
        } VrDeviceInfo; -}
data VrDeviceInfo = VrDeviceInfo
  { vrDeviceInfo'hResolution :: CInt,
    vrDeviceInfo'vResolution :: CInt,
    vrDeviceInfo'hScreenSize :: CFloat,
    vrDeviceInfo'vScreenSize :: CFloat,
    vrDeviceInfo'vScreenCenter :: CFloat,
    vrDeviceInfo'eyeToScreenDistance :: CFloat,
    vrDeviceInfo'lensSeparationDistance :: CFloat,
    vrDeviceInfo'interpupillaryDistance :: CFloat,
    vrDeviceInfo'lensDistortionValues :: [CFloat],
    vrDeviceInfo'chromaAbCorrection :: [CFloat]
  }
  deriving (Eq, Show)

p'VrDeviceInfo'hResolution p = plusPtr p 0

p'VrDeviceInfo'hResolution :: Ptr VrDeviceInfo -> Ptr CInt

p'VrDeviceInfo'vResolution p = plusPtr p 4

p'VrDeviceInfo'vResolution :: Ptr VrDeviceInfo -> Ptr CInt

p'VrDeviceInfo'hScreenSize p = plusPtr p 8

p'VrDeviceInfo'hScreenSize :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'vScreenSize p = plusPtr p 12

p'VrDeviceInfo'vScreenSize :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'vScreenCenter p = plusPtr p 16

p'VrDeviceInfo'vScreenCenter :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'eyeToScreenDistance p = plusPtr p 20

p'VrDeviceInfo'eyeToScreenDistance :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'lensSeparationDistance p = plusPtr p 24

p'VrDeviceInfo'lensSeparationDistance :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'interpupillaryDistance p = plusPtr p 28

p'VrDeviceInfo'interpupillaryDistance :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'lensDistortionValues p = plusPtr p 32

p'VrDeviceInfo'lensDistortionValues :: Ptr VrDeviceInfo -> Ptr CFloat

p'VrDeviceInfo'chromaAbCorrection p = plusPtr p 48

p'VrDeviceInfo'chromaAbCorrection :: Ptr VrDeviceInfo -> Ptr CFloat

instance Storable VrDeviceInfo where
  sizeOf _ = 64
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 12
    v4 <- peekByteOff _p 16
    v5 <- peekByteOff _p 20
    v6 <- peekByteOff _p 24
    v7 <- peekByteOff _p 28
    v8 <- let s8 = div 16 $ sizeOf (undefined :: CFloat) in peekArray s8 (plusPtr _p 32)
    v9 <- let s9 = div 16 $ sizeOf (undefined :: CFloat) in peekArray s9 (plusPtr _p 48)
    return $ VrDeviceInfo v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  poke _p (VrDeviceInfo v0 v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 12 v3
    pokeByteOff _p 16 v4
    pokeByteOff _p 20 v5
    pokeByteOff _p 24 v6
    pokeByteOff _p 28 v7
    let s8 = div 16 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 32) (take s8 v8)
    let s9 = div 16 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 48) (take s9 v9)
    return ()

{- typedef struct VrStereoConfig {
            Matrix projection[2];
            Matrix viewOffset[2];
            float leftLensCenter[2];
            float rightLensCenter[2];
            float leftScreenCenter[2];
            float rightScreenCenter[2];
            float scale[2];
            float scaleIn[2];
        } VrStereoConfig; -}
data VrStereoConfig = VrStereoConfig
  { vrStereoConfig'projection :: [Matrix],
    vrStereoConfig'viewOffset :: [Matrix],
    vrStereoConfig'leftLensCenter :: [CFloat],
    vrStereoConfig'rightLensCenter :: [CFloat],
    vrStereoConfig'leftScreenCenter :: [CFloat],
    vrStereoConfig'rightScreenCenter :: [CFloat],
    vrStereoConfig'scale :: [CFloat],
    vrStereoConfig'scaleIn :: [CFloat]
  }
  deriving (Eq, Show)

p'VrStereoConfig'projection p = plusPtr p 0

p'VrStereoConfig'projection :: Ptr VrStereoConfig -> Ptr Matrix

p'VrStereoConfig'viewOffset p = plusPtr p 128

p'VrStereoConfig'viewOffset :: Ptr VrStereoConfig -> Ptr Matrix

p'VrStereoConfig'leftLensCenter p = plusPtr p 256

p'VrStereoConfig'leftLensCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'rightLensCenter p = plusPtr p 264

p'VrStereoConfig'rightLensCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'leftScreenCenter p = plusPtr p 272

p'VrStereoConfig'leftScreenCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'rightScreenCenter p = plusPtr p 280

p'VrStereoConfig'rightScreenCenter :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'scale p = plusPtr p 288

p'VrStereoConfig'scale :: Ptr VrStereoConfig -> Ptr CFloat

p'VrStereoConfig'scaleIn p = plusPtr p 296

p'VrStereoConfig'scaleIn :: Ptr VrStereoConfig -> Ptr CFloat

instance Storable VrStereoConfig where
  sizeOf _ = 304
  alignment _ = 4
  peek _p = do
    v0 <- let s0 = div 128 $ sizeOf (undefined :: Matrix) in peekArray s0 (plusPtr _p 0)
    v1 <- let s1 = div 128 $ sizeOf (undefined :: Matrix) in peekArray s1 (plusPtr _p 128)
    v2 <- let s2 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s2 (plusPtr _p 256)
    v3 <- let s3 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s3 (plusPtr _p 264)
    v4 <- let s4 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s4 (plusPtr _p 272)
    v5 <- let s5 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s5 (plusPtr _p 280)
    v6 <- let s6 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s6 (plusPtr _p 288)
    v7 <- let s7 = div 8 $ sizeOf (undefined :: CFloat) in peekArray s7 (plusPtr _p 296)
    return $ VrStereoConfig v0 v1 v2 v3 v4 v5 v6 v7
  poke _p (VrStereoConfig v0 v1 v2 v3 v4 v5 v6 v7) = do
    let s0 = div 128 $ sizeOf (undefined :: Matrix)
    pokeArray (plusPtr _p 0) (take s0 v0)
    let s1 = div 128 $ sizeOf (undefined :: Matrix)
    pokeArray (plusPtr _p 128) (take s1 v1)
    let s2 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 256) (take s2 v2)
    let s3 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 264) (take s3 v3)
    let s4 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 272) (take s4 v4)
    let s5 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 280) (take s5 v5)
    let s6 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 288) (take s6 v6)
    let s7 = div 8 $ sizeOf (undefined :: CFloat)
    pokeArray (plusPtr _p 296) (take s7 v7)
    return ()

{- typedef struct FilePathList {
            unsigned int capacity; unsigned int count; char * * paths;
        } FilePathList; -}
data FilePathList = FilePathList
  { filePathlist'capacity :: CUInt,
    filePathlist'count :: CUInt,
    filePathList'paths :: Ptr CString
  }
  deriving (Eq, Show)

p'FilePathList'capacity p = plusPtr p 0

p'FilePathList'capacity :: Ptr FilePathList -> Ptr CUInt

p'FilePathList'count p = plusPtr p 4

p'FilePathList'count :: Ptr FilePathList -> Ptr CUInt

p'FilePathList'paths p = plusPtr p 8

p'FilePathList'paths :: Ptr FilePathList -> Ptr (Ptr CString)

instance Storable FilePathList where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    return $ FilePathList v0 v1 v2
  poke _p (FilePathList v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    return ()