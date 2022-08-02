{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib where

import Foreign.C.String (CString, CStringLen, CWString, CWStringLen)
import Foreign.C.Types
    ( CChar(..),
      CDouble(..),
      CFloat(..),
      CInt(..),
      CLong(..),
      CUChar,
      CUInt(..),
      CUShort )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr ( Ptr, FunPtr, plusPtr )
import Foreign.Storable
    ( Storable(pokeByteOff, poke, peek, alignment, sizeOf,
               peekByteOff) )

#include "raylib.h"
#include "bindings.h"

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

type ConfigFlags = CUInt

flag'vsyncHint = 64

flag'vsyncHint :: (Num a) => a

flag'fullscreenMode = 2

flag'fullscreenMode :: (Num a) => a

flag'windowResizable = 4

flag'windowResizable :: (Num a) => a

flag'windowUndecorated = 8

flag'windowUndecorated :: (Num a) => a

flag'windowHidden = 128

flag'windowHidden :: (Num a) => a

flag'windowMinimized = 512

flag'windowMinimized :: (Num a) => a

flag'windowMaximized = 1024

flag'windowMaximized :: (Num a) => a

flag'windowUnfocused = 2048

flag'windowUnfocused :: (Num a) => a

flag'windowTopmost = 4096

flag'windowTopmost :: (Num a) => a

flag'windowAlwaysRun = 256

flag'windowAlwaysRun :: (Num a) => a

flag'windowTransparent = 16

flag'windowTransparent :: (Num a) => a

flag'windowHighdpi = 8192

flag'windowHighdpi :: (Num a) => a

flag'windowMousePassthrough = 16384

flag'windowMousePassthrough :: (Num a) => a

flag'msaa4xHint = 32

flag'msaa4xHint :: (Num a) => a

flag'interlacedHint = 65536

flag'interlacedHint :: (Num a) => a

type TraceLogLevel = CUInt

log'all = 0

log'all :: (Num a) => a

log'trace = 1

log'trace :: (Num a) => a

log'debug = 2

log'debug :: (Num a) => a

log'info = 3

log'info :: (Num a) => a

log'warning = 4

log'warning :: (Num a) => a

log'error = 5

log'error :: (Num a) => a

log'fatal = 6

log'fatal :: (Num a) => a

log'none = 7

log'none :: (Num a) => a

{- typedef enum {
            key'null = 0,
            key'apostrophe = 39,
            key'comma = 44,
            key'minus = 45,
            key'period = 46,
            key'slash = 47,
            key'zero = 48,
            key'one = 49,
            key'two = 50,
            key'three = 51,
            key'four = 52,
            key'five = 53,
            key'six = 54,
            key'seven = 55,
            key'eight = 56,
            key'nine = 57,
            key'semicolon = 59,
            key'equal = 61,
            key'a = 65,
            key'b = 66,
            key'c = 67,
            key'd = 68,
            key'e = 69,
            key'f = 70,
            key'g = 71,
            key'h = 72,
            key'i = 73,
            key'j = 74,
            key'k = 75,
            key'l = 76,
            key'm = 77,
            key'n = 78,
            key'o = 79,
            key'p = 80,
            key'q = 81,
            key'r = 82,
            key's = 83,
            key't = 84,
            key'u = 85,
            key'v = 86,
            key'w = 87,
            key'x = 88,
            key'y = 89,
            key'z = 90,
            key'left_BRACKET = 91,
            key'backslash = 92,
            key'right_BRACKET = 93,
            key'grave = 96,
            key'space = 32,
            key'escape = 256,
            key'enter = 257,
            key'tab = 258,
            key'backspace = 259,
            key'insert = 260,
            key'delete = 261,
            key'right = 262,
            key'left = 263,
            key'down = 264,
            key'up = 265,
            key'page_UP = 266,
            key'page_DOWN = 267,
            key'home = 268,
            key'end = 269,
            key'caps_LOCK = 280,
            key'scroll_LOCK = 281,
            key'num_LOCK = 282,
            key'print_SCREEN = 283,
            key'pause = 284,
            key'f1 = 290,
            key'f2 = 291,
            key'f3 = 292,
            key'f4 = 293,
            key'f5 = 294,
            key'f6 = 295,
            key'f7 = 296,
            key'f8 = 297,
            key'f9 = 298,
            key'f10 = 299,
            key'f11 = 300,
            key'f12 = 301,
            key'left_SHIFT = 340,
            key'left_CONTROL = 341,
            key'left_ALT = 342,
            key'left_SUPER = 343,
            key'right_SHIFT = 344,
            key'right_CONTROL = 345,
            key'right_ALT = 346,
            key'right_SUPER = 347,
            key'kb_MENU = 348,
            key'kp_0 = 320,
            key'kp_1 = 321,
            key'kp_2 = 322,
            key'kp_3 = 323,
            key'kp_4 = 324,
            key'kp_5 = 325,
            key'kp_6 = 326,
            key'kp_7 = 327,
            key'kp_8 = 328,
            key'kp_9 = 329,
            key'kp_DECIMAL = 330,
            key'kp_DIVIDE = 331,
            key'kp_MULTIPLY = 332,
            key'kp_SUBTRACT = 333,
            key'kp_ADD = 334,
            key'kp_ENTER = 335,
            key'kp_EQUAL = 336,
            key'back = 4,
            key'menu = 82,
            key'volume_UP = 24,
            key'volume_DOWN = 25
        } KeyboardKey; -}
type KeyboardKey = CUInt

key'null = 0

key'null :: (Num a) => a

key'apostrophe = 39

key'apostrophe :: (Num a) => a

key'comma = 44

key'comma :: (Num a) => a

key'minus = 45

key'minus :: (Num a) => a

key'period = 46

key'period :: (Num a) => a

key'slash = 47

key'slash :: (Num a) => a

key'zero = 48

key'zero :: (Num a) => a

key'one = 49

key'one :: (Num a) => a

key'two = 50

key'two :: (Num a) => a

key'three = 51

key'three :: (Num a) => a

key'four = 52

key'four :: (Num a) => a

key'five = 53

key'five :: (Num a) => a

key'six = 54

key'six :: (Num a) => a

key'seven = 55

key'seven :: (Num a) => a

key'eight = 56

key'eight :: (Num a) => a

key'nine = 57

key'nine :: (Num a) => a

key'semicolon = 59

key'semicolon :: (Num a) => a

key'equal = 61

key'equal :: (Num a) => a

key'a = 65

key'a :: (Num a) => a

key'b = 66

key'b :: (Num a) => a

key'c = 67

key'c :: (Num a) => a

key'd = 68

key'd :: (Num a) => a

key'e = 69

key'e :: (Num a) => a

key'f = 70

key'f :: (Num a) => a

key'g = 71

key'g :: (Num a) => a

key'h = 72

key'h :: (Num a) => a

key'i = 73

key'i :: (Num a) => a

key'j = 74

key'j :: (Num a) => a

key'k = 75

key'k :: (Num a) => a

key'l = 76

key'l :: (Num a) => a

key'm = 77

key'm :: (Num a) => a

key'n = 78

key'n :: (Num a) => a

key'o = 79

key'o :: (Num a) => a

key'p = 80

key'p :: (Num a) => a

key'q = 81

key'q :: (Num a) => a

key'r = 82

key'r :: (Num a) => a

key's = 83

key's :: (Num a) => a

key't = 84

key't :: (Num a) => a

key'u = 85

key'u :: (Num a) => a

key'v = 86

key'v :: (Num a) => a

key'w = 87

key'w :: (Num a) => a

key'x = 88

key'x :: (Num a) => a

key'y = 89

key'y :: (Num a) => a

key'z = 90

key'z :: (Num a) => a

key'leftBracket = 91

key'leftBracket :: (Num a) => a

key'backslash = 92

key'backslash :: (Num a) => a

key'rightBracket = 93

key'rightBracket :: (Num a) => a

key'grave = 96

key'grave :: (Num a) => a

key'space = 32

key'space :: (Num a) => a

key'escape = 256

key'escape :: (Num a) => a

key'enter = 257

key'enter :: (Num a) => a

key'tab = 258

key'tab :: (Num a) => a

key'backspace = 259

key'backspace :: (Num a) => a

key'insert = 260

key'insert :: (Num a) => a

key'delete = 261

key'delete :: (Num a) => a

key'right = 262

key'right :: (Num a) => a

key'left = 263

key'left :: (Num a) => a

key'down = 264

key'down :: (Num a) => a

key'up = 265

key'up :: (Num a) => a

key'pageUp = 266

key'pageUp :: (Num a) => a

key'pageDown = 267

key'pageDown :: (Num a) => a

key'home = 268

key'home :: (Num a) => a

key'end = 269

key'end :: (Num a) => a

key'capsLock = 280

key'capsLock :: (Num a) => a

key'scrollLock = 281

key'scrollLock :: (Num a) => a

key'numLock = 282

key'numLock :: (Num a) => a

key'printScreen = 283

key'printScreen :: (Num a) => a

key'pause = 284

key'pause :: (Num a) => a

key'f1 = 290

key'f1 :: (Num a) => a

key'f2 = 291

key'f2 :: (Num a) => a

key'f3 = 292

key'f3 :: (Num a) => a

key'f4 = 293

key'f4 :: (Num a) => a

key'f5 = 294

key'f5 :: (Num a) => a

key'f6 = 295

key'f6 :: (Num a) => a

key'f7 = 296

key'f7 :: (Num a) => a

key'f8 = 297

key'f8 :: (Num a) => a

key'f9 = 298

key'f9 :: (Num a) => a

key'f10 = 299

key'f10 :: (Num a) => a

key'f11 = 300

key'f11 :: (Num a) => a

key'f12 = 301

key'f12 :: (Num a) => a

key'leftShift = 340

key'leftShift :: (Num a) => a

key'leftControl = 341

key'leftControl :: (Num a) => a

key'leftAlt = 342

key'leftAlt :: (Num a) => a

key'leftSuper = 343

key'leftSuper :: (Num a) => a

key'rightShift = 344

key'rightShift :: (Num a) => a

key'rightControl = 345

key'rightControl :: (Num a) => a

key'rightAlt = 346

key'rightAlt :: (Num a) => a

key'rightSuper = 347

key'rightSuper :: (Num a) => a

key'kbMenu = 348

key'kbMenu :: (Num a) => a

key'kp0 = 320

key'kp0 :: (Num a) => a

key'kp1 = 321

key'kp1 :: (Num a) => a

key'kp2 = 322

key'kp2 :: (Num a) => a

key'kp3 = 323

key'kp3 :: (Num a) => a

key'kp4 = 324

key'kp4 :: (Num a) => a

key'kp5 = 325

key'kp5 :: (Num a) => a

key'kp6 = 326

key'kp6 :: (Num a) => a

key'kp7 = 327

key'kp7 :: (Num a) => a

key'kp8 = 328

key'kp8 :: (Num a) => a

key'kp9 = 329

key'kp9 :: (Num a) => a

key'kpDecimal = 330

key'kpDecimal :: (Num a) => a

key'kpDivide = 331

key'kpDivide :: (Num a) => a

key'kpMultiply = 332

key'kpMultiply :: (Num a) => a

key'kpSubtract = 333

key'kpSubtract :: (Num a) => a

key'kpAdd = 334

key'kpAdd :: (Num a) => a

key'kpEnter = 335

key'kpEnter :: (Num a) => a

key'kpEqual = 336

key'kpEqual :: (Num a) => a

key'back = 4

key'back :: (Num a) => a

key'menu = 82

key'menu :: (Num a) => a

key'volumeUp = 24

key'volumeUp :: (Num a) => a

key'volumeDown = 25

key'volumeDown :: (Num a) => a

{- typedef enum {
            mouseButton'left = 0,
            mouseButton'right = 1,
            mouseButton'middle = 2,
            mouseButton'side = 3,
            mouseButton'extra = 4,
            mouseButton'forward = 5,
            mouseButton'back = 6
        } MouseButton; -}
type MouseButton = CUInt

mouseButton'left = 0

mouseButton'left :: (Num a) => a

mouseButton'right = 1

mouseButton'right :: (Num a) => a

mouseButton'middle = 2

mouseButton'middle :: (Num a) => a

mouseButton'side = 3

mouseButton'side :: (Num a) => a

mouseButton'extra = 4

mouseButton'extra :: (Num a) => a

mouseButton'forward = 5

mouseButton'forward :: (Num a) => a

mouseButton'back = 6

mouseButton'back :: (Num a) => a

{- typedef enum {
            mouseCursor'default = 0,
            mouseCursor'arrow = 1,
            mouseCursor'ibeam = 2,
            mouseCursor'crosshair = 3,
            mouseCursor'pointing_HAND = 4,
            mouseCursor'resize_EW = 5,
            mouseCursor'resize_NS = 6,
            mouseCursor'resize_NWSE = 7,
            mouseCursor'resize_NESW = 8,
            mouseCursor'resize_ALL = 9,
            mouseCursor'not_ALLOWED = 10
        } MouseCursor; -}
type MouseCursor = CUInt

mouseCursor'default = 0

mouseCursor'default :: (Num a) => a

mouseCursor'arrow = 1

mouseCursor'arrow :: (Num a) => a

mouseCursor'ibeam = 2

mouseCursor'ibeam :: (Num a) => a

mouseCursor'crosshair = 3

mouseCursor'crosshair :: (Num a) => a

mouseCursor'pointingHand = 4

mouseCursor'pointingHand :: (Num a) => a

mouseCursor'resizeEW = 5

mouseCursor'resizeEW :: (Num a) => a

mouseCursor'resizeNS = 6

mouseCursor'resizeNS :: (Num a) => a

mouseCursor'resizeNWSE = 7

mouseCursor'resizeNWSE :: (Num a) => a

mouseCursor'resizeNESW = 8

mouseCursor'resizeNESW :: (Num a) => a

mouseCursor'resizeAll = 9

mouseCursor'resizeAll :: (Num a) => a

mouseCursor'notAllowed = 10

mouseCursor'notAllowed :: (Num a) => a

{- typedef enum {
            gamepadButton'unknown = 0,
            gamepadButton'leftFace_UP,
            gamepadButton'leftFace_RIGHT,
            gamepadButton'leftFace_DOWN,
            gamepadButton'leftFace_LEFT,
            gamepadButton'rightFace_UP,
            gamepadButton'rightFace_RIGHT,
            gamepadButton'rightFace_DOWN,
            gamepadButton'rightFace_LEFT,
            gamepadButton'leftTrigger_1,
            gamepadButton'leftTrigger_2,
            gamepadButton'rightTrigger_1,
            gamepadButton'rightTrigger_2,
            gamepadButton'middleLeft,
            gamepadButton'middle,
            gamepadButton'middleRight,
            gamepadButton'leftThumb,
            gamepadButton'rightThumb
        } GamepadButton; -}
type GamepadButton = CUInt

gamepadButton'unknown = 0

gamepadButton'unknown :: (Num a) => a

gamepadButton'leftFaceUp = 1

gamepadButton'leftFaceUp :: (Num a) => a

gamepadButton'leftFaceRight = 2

gamepadButton'leftFaceRight :: (Num a) => a

gamepadButton'leftFaceDown = 3

gamepadButton'leftFaceDown :: (Num a) => a

gamepadButton'leftFaceLeft = 4

gamepadButton'leftFaceLeft :: (Num a) => a

gamepadButton'rightFaceUp = 5

gamepadButton'rightFaceUp :: (Num a) => a

gamepadButton'rightFaceRight = 6

gamepadButton'rightFaceRight :: (Num a) => a

gamepadButton'rightFaceDown = 7

gamepadButton'rightFaceDown :: (Num a) => a

gamepadButton'rightFaceLeft = 8

gamepadButton'rightFaceLeft :: (Num a) => a

gamepadButton'leftTrigger1 = 9

gamepadButton'leftTrigger1 :: (Num a) => a

gamepadButton'leftTrigger2 = 10

gamepadButton'leftTrigger2 :: (Num a) => a

gamepadButton'rightTrigger1 = 11

gamepadButton'rightTrigger1 :: (Num a) => a

gamepadButton'rightTrigger2 = 12

gamepadButton'rightTrigger2 :: (Num a) => a

gamepadButton'middleLeft = 13

gamepadButton'middleLeft :: (Num a) => a

gamepadButton'middle = 14

gamepadButton'middle :: (Num a) => a

gamepadButton'middleRight = 15

gamepadButton'middleRight :: (Num a) => a

gamepadButton'leftThumb = 16

gamepadButton'leftThumb :: (Num a) => a

gamepadButton'rightThumb = 17

gamepadButton'rightThumb :: (Num a) => a

{- typedef enum {
            GAMEPAD_AXIS_LEFT_X = 0,
            GAMEPAD_AXIS_LEFT_Y = 1,
            GAMEPAD_AXIS_RIGHT_X = 2,
            GAMEPAD_AXIS_RIGHT_Y = 3,
            GAMEPAD_AXIS_LEFT_TRIGGER = 4,
            GAMEPAD_AXIS_RIGHT_TRIGGER = 5
        } GamepadAxis; -}
type GamepadAxis = CUInt

gamepadAxisLeftX = 0

gamepadAxisLeftX :: (Num a) => a

gamepadAxisLeftY = 1

gamepadAxisLeftY :: (Num a) => a

gamepadAxisRightX = 2

gamepadAxisRightX :: (Num a) => a

gamepadAxisRightY = 3

gamepadAxisRightY :: (Num a) => a

gamepadAxisLeftTrigger = 4

gamepadAxisLeftTrigger :: (Num a) => a

gamepadAxisRightTrigger = 5

gamepadAxisRightTrigger :: (Num a) => a

{- typedef enum {
            materialMap'albedo = 0,
            materialMap'metalness,
            materialMap'normal,
            materialMap'roughness,
            materialMap'occlusion,
            materialMap'emission,
            materialMap'height,
            materialMap'cubemap,
            materialMap'irradiance,
            materialMap'prefilter,
            materialMap'brdf
        } MaterialMapIndex; -}
type MaterialMapIndex = CUInt

materialMap'albedo = 0

materialMap'albedo :: (Num a) => a

materialMap'metalness = 1

materialMap'metalness :: (Num a) => a

materialMap'normal = 2

materialMap'normal :: (Num a) => a

materialMap'roughness = 3

materialMap'roughness :: (Num a) => a

materialMap'occlusion = 4

materialMap'occlusion :: (Num a) => a

materialMap'emission = 5

materialMap'emission :: (Num a) => a

materialMap'height = 6

materialMap'height :: (Num a) => a

materialMap'cubemap = 7

materialMap'cubemap :: (Num a) => a

materialMap'irradiance = 8

materialMap'irradiance :: (Num a) => a

materialMap'prefilter = 9

materialMap'prefilter :: (Num a) => a

materialMap'brdf = 10

materialMap'brdf :: (Num a) => a

{- typedef enum {
            shaderLoc'vertexPosition = 0,
            shaderLoc'vertexTexcoord01,
            shaderLoc'vertexTexcoord02,
            shaderLoc'vertexNormal,
            shaderLoc'vertexTangent,
            shaderLoc'vertexColor,
            shaderLoc'matrixMvp,
            shaderLoc'matrixView,
            shaderLoc'matrixProjection,
            shaderLoc'matrixModel,
            shaderLoc'matrixNormal,
            shaderLoc'vectorView,
            shaderLoc'colorDiffuse,
            shaderLoc'colorSpecular,
            shaderLoc'colorAmbient,
            shaderLoc'mapAlbedo,
            shaderLoc'mapMetalness,
            shaderLoc'mapNormal,
            shaderLoc'mapRoughness,
            shaderLoc'mapOcclusion,
            shaderLoc'mapEmission,
            shaderLoc'mapHeight,
            shaderLoc'mapCubemap,
            shaderLoc'mapIrradiance,
            shaderLoc'mapPrefilter,
            shaderLoc'mapBrdf
        } ShaderLocationIndex; -}
type ShaderLocationIndex = CUInt

shaderLoc'vertexPosition = 0

shaderLoc'vertexPosition :: (Num a) => a

shaderLoc'vertexTexcoord01 = 1

shaderLoc'vertexTexcoord01 :: (Num a) => a

shaderLoc'vertexTexcoord02 = 2

shaderLoc'vertexTexcoord02 :: (Num a) => a

shaderLoc'vertexNormal = 3

shaderLoc'vertexNormal :: (Num a) => a

shaderLoc'vertexTangent = 4

shaderLoc'vertexTangent :: (Num a) => a

shaderLoc'vertexColor = 5

shaderLoc'vertexColor :: (Num a) => a

shaderLoc'matrixMvp = 6

shaderLoc'matrixMvp :: (Num a) => a

shaderLoc'matrixView = 7

shaderLoc'matrixView :: (Num a) => a

shaderLoc'matrixProjection = 8

shaderLoc'matrixProjection :: (Num a) => a

shaderLoc'matrixModel = 9

shaderLoc'matrixModel :: (Num a) => a

shaderLoc'matrixNormal = 10

shaderLoc'matrixNormal :: (Num a) => a

shaderLoc'vectorView = 11

shaderLoc'vectorView :: (Num a) => a

shaderLoc'colorDiffuse = 12

shaderLoc'colorDiffuse :: (Num a) => a

shaderLoc'colorSpecular = 13

shaderLoc'colorSpecular :: (Num a) => a

shaderLoc'colorAmbient = 14

shaderLoc'colorAmbient :: (Num a) => a

shaderLoc'mapAlbedo = 15

shaderLoc'mapAlbedo :: (Num a) => a

shaderLoc'mapMetalness = 16

shaderLoc'mapMetalness :: (Num a) => a

shaderLoc'mapNormal = 17

shaderLoc'mapNormal :: (Num a) => a

shaderLoc'mapRoughness = 18

shaderLoc'mapRoughness :: (Num a) => a

shaderLoc'mapOcclusion = 19

shaderLoc'mapOcclusion :: (Num a) => a

shaderLoc'mapEmission = 20

shaderLoc'mapEmission :: (Num a) => a

shaderLoc'mapHeight = 21

shaderLoc'mapHeight :: (Num a) => a

shaderLoc'mapCubemap = 22

shaderLoc'mapCubemap :: (Num a) => a

shaderLoc'mapIrradiance = 23

shaderLoc'mapIrradiance :: (Num a) => a

shaderLoc'mapPrefilter = 24

shaderLoc'mapPrefilter :: (Num a) => a

shaderLoc'mapBrdf = 25

shaderLoc'mapBrdf :: (Num a) => a

{- typedef enum {
            shaderUniform'float = 0,
            shaderUniform'vec2,
            shaderUniform'vec3,
            shaderUniform'vec4,
            shaderUniform'int,
            shaderUniform'ivec2,
            shaderUniform'ivec3,
            shaderUniform'ivec4,
            shaderUniform'sampler2d
        } ShaderUniformDataType; -}
type ShaderUniformDataType = CUInt

shaderUniform'float = 0

shaderUniform'float :: (Num a) => a

shaderUniform'vec2 = 1

shaderUniform'vec2 :: (Num a) => a

shaderUniform'vec3 = 2

shaderUniform'vec3 :: (Num a) => a

shaderUniform'vec4 = 3

shaderUniform'vec4 :: (Num a) => a

shaderUniform'int = 4

shaderUniform'int :: (Num a) => a

shaderUniform'ivec2 = 5

shaderUniform'ivec2 :: (Num a) => a

shaderUniform'ivec3 = 6

shaderUniform'ivec3 :: (Num a) => a

shaderUniform'ivec4 = 7

shaderUniform'ivec4 :: (Num a) => a

shaderUniform'sampler2d = 8

shaderUniform'sampler2d :: (Num a) => a

{- typedef enum {
            shaderAttrib'float = 0,
            shaderAttrib'vec2,
            shaderAttrib'vec3,
            shaderAttrib'vec4
        } ShaderAttributeDataType; -}
type ShaderAttributeDataType = CUInt

shaderAttrib'float = 0

shaderAttrib'float :: (Num a) => a

shaderAttrib'vec2 = 1

shaderAttrib'vec2 :: (Num a) => a

shaderAttrib'vec3 = 2

shaderAttrib'vec3 :: (Num a) => a

shaderAttrib'vec4 = 3

shaderAttrib'vec4 :: (Num a) => a

{- typedef enum {
            pixelFormat'uncompressedGrayscale = 1,
            pixelFormat'uncompressedGray_ALPHA,
            pixelFormat'uncompressedR5g6b5,
            pixelFormat'uncompressedR8g8b8,
            pixelFormat'uncompressedR5g5b5a1,
            pixelFormat'uncompressedR4g4b4a4,
            pixelFormat'uncompressedR8g8b8a8,
            pixelFormat'uncompressedR32,
            pixelFormat'uncompressedR32g32b32,
            pixelFormat'uncompressedR32g32b32a32,
            pixelFormat'compressedDxt1_RGB,
            pixelFormat'compressedDxt1_RGBA,
            pixelFormat'compressedDxt3_RGBA,
            pixelFormat'compressedDxt5_RGBA,
            pixelFormat'compressedEtc1_RGB,
            pixelFormat'compressedEtc2_RGB,
            pixelFormat'compressedEtc2_EAC_RGBA,
            pixelFormat'compressedPvrt_RGB,
            pixelFormat'compressedPvrt_RGBA,
            pixelFormat'compressedAstc_4x4_RGBA,
            pixelFormat'compressedAstc_8x8_RGBA
        } PixelFormat; -}
type PixelFormat = CUInt

pixelFormat'uncompressedGrayscale = 1

pixelFormat'uncompressedGrayscale :: (Num a) => a

pixelFormat'uncompressedGrayAlpha = 2

pixelFormat'uncompressedGrayAlpha :: (Num a) => a

pixelFormat'uncompressedR5G6B5 = 3

pixelFormat'uncompressedR5G6B5 :: (Num a) => a

pixelFormat'uncompressedR8G8B8 = 4

pixelFormat'uncompressedR8G8B8 :: (Num a) => a

pixelFormat'uncompressedR5G5B5A1 = 5

pixelFormat'uncompressedR5G5B5A1 :: (Num a) => a

pixelFormat'uncompressedR4G4B4A4 = 6

pixelFormat'uncompressedR4G4B4A4 :: (Num a) => a

pixelFormat'uncompressedR8G8B8A8 = 7

pixelFormat'uncompressedR8G8B8A8 :: (Num a) => a

pixelFormat'uncompressedR32 = 8

pixelFormat'uncompressedR32 :: (Num a) => a

pixelFormat'uncompressedR32G32B32 = 9

pixelFormat'uncompressedR32G32B32 :: (Num a) => a

pixelFormat'uncompressedR32G32B32A32 = 10

pixelFormat'uncompressedR32G32B32A32 :: (Num a) => a

pixelFormat'compressedDxt1Rgb = 11

pixelFormat'compressedDxt1Rgb :: (Num a) => a

pixelFormat'compressedDxt1Rgba = 12

pixelFormat'compressedDxt1Rgba :: (Num a) => a

pixelFormat'compressedDxt3Rgba = 13

pixelFormat'compressedDxt3Rgba :: (Num a) => a

pixelFormat'compressedDxt5Rgba = 14

pixelFormat'compressedDxt5Rgba :: (Num a) => a

pixelFormat'compressedEtc1Rgb = 15

pixelFormat'compressedEtc1Rgb :: (Num a) => a

pixelFormat'compressedEtc2Rgb = 16

pixelFormat'compressedEtc2Rgb :: (Num a) => a

pixelFormat'compressedEtc2EacRgba = 17

pixelFormat'compressedEtc2EacRgba :: (Num a) => a

pixelFormat'compressedPvrtRgb = 18

pixelFormat'compressedPvrtRgb :: (Num a) => a

pixelFormat'compressedPvrtRgba = 19

pixelFormat'compressedPvrtRgba :: (Num a) => a

pixelFormat'compressedAstc4x4Rgba = 20

pixelFormat'compressedAstc4x4Rgba :: (Num a) => a

pixelFormat'compressedAstc8x8Rgba = 21

pixelFormat'compressedAstc8x8Rgba :: (Num a) => a

{- typedef enum {
            textureFilter'point = 0,
            textureFilter'bilinear,
            textureFilter'trilinear,
            textureFilter'anisotropic_4X,
            textureFilter'anisotropic_8X,
            textureFilter'anisotropic_16X
        } TextureFilter; -}
type TextureFilter = CUInt

textureFilter'point = 0

textureFilter'point :: (Num a) => a

textureFilter'bilinear = 1

textureFilter'bilinear :: (Num a) => a

textureFilter'trilinear = 2

textureFilter'trilinear :: (Num a) => a

textureFilter'anisotropic4x = 3

textureFilter'anisotropic4x :: (Num a) => a

textureFilter'anisotropic8x = 4

textureFilter'anisotropic8x :: (Num a) => a

textureFilter'anisotropic16x = 5

textureFilter'anisotropic16x :: (Num a) => a

{- typedef enum {
            textureWrap'repeat = 0,
            textureWrap'clamp,
            textureWrap'mirror_REPEAT,
            textureWrap'mirror_CLAMP
        } TextureWrap; -}
type TextureWrap = CUInt

textureWrap'repeat = 0

textureWrap'repeat :: (Num a) => a

textureWrap'clamp = 1

textureWrap'clamp :: (Num a) => a

textureWrap'mirrorRepeat = 2

textureWrap'mirrorRepeat :: (Num a) => a

textureWrap'mirrorClamp = 3

textureWrap'mirrorClamp :: (Num a) => a

{- typedef enum {
            cubemapLayout'autoDetect = 0,
            cubemapLayout'lineVertical,
            cubemapLayout'lineHorizontal,
            cubemapLayout'crossThree_BY_FOUR,
            cubemapLayout'crossFour_BY_THREE,
            cubemapLayout'panorama
        } CubemapLayout; -}
type CubemapLayout = CUInt

cubemapLayout'autoDetect = 0

cubemapLayout'autoDetect :: (Num a) => a

cubemapLayout'lineVertical = 1

cubemapLayout'lineVertical :: (Num a) => a

cubemapLayout'lineHorizontal = 2

cubemapLayout'lineHorizontal :: (Num a) => a

cubemapLayout'crossThreeByFour = 3

cubemapLayout'crossThreeByFour :: (Num a) => a

cubemapLayout'crossThreeByThree = 4

cubemapLayout'crossThreeByThree :: (Num a) => a

cubemapLayout'panorama = 5

cubemapLayout'panorama :: (Num a) => a

{- typedef enum {
            FONT_DEFAULT = 0, FONT_BITMAP, FONT_SDF
        } FontType; -}
type FontType = CUInt

font'default = 0

font'default :: (Num a) => a

font'bitmap = 1

font'bitmap :: (Num a) => a

font'sdf = 2

font'sdf :: (Num a) => a

{- typedef enum {
            blend'alpha = 0,
            blend'additive,
            blend'multiplied,
            blend'addColors,
            blend'subtractColors,
            blend'alphaPremultiply,
            blend'custom
        } BlendMode; -}
type BlendMode = CUInt

blend'alpha = 0

blend'alpha :: (Num a) => a

blend'additive = 1

blend'additive :: (Num a) => a

blend'multiplied = 2

blend'multiplied :: (Num a) => a

blend'addColors = 3

blend'addColors :: (Num a) => a

blend'subtractColors = 4

blend'subtractColors :: (Num a) => a

blend'alphaPremultiply = 5

blend'alphaPremultiply :: (Num a) => a

blend'custom = 6

blend'custom :: (Num a) => a

{- typedef enum {
            gesture'none = 0,
            gesture'tap = 1,
            gesture'doubletap = 2,
            gesture'hold = 4,
            gesture'drag = 8,
            gesture'swipeRight = 16,
            gesture'swipeLeft = 32,
            gesture'swipeUp = 64,
            gesture'swipeDown = 128,
            gesture'pinchIn = 256,
            gesture'pinchOut = 512
        } Gesture; -}
type Gesture = CUInt

gesture'none = 0

gesture'none :: (Num a) => a

gesture'tap = 1

gesture'tap :: (Num a) => a

gesture'doubletap = 2

gesture'doubletap :: (Num a) => a

gesture'hold = 4

gesture'hold :: (Num a) => a

gesture'drag = 8

gesture'drag :: (Num a) => a

gesture'swipeRight = 16

gesture'swipeRight :: (Num a) => a

gesture'swipeLeft = 32

gesture'swipeLeft :: (Num a) => a

gesture'swipeUp = 64

gesture'swipeUp :: (Num a) => a

gesture'swipeDown = 128

gesture'swipeDown :: (Num a) => a

gesture'pinchIn = 256

gesture'pinchIn :: (Num a) => a

gesture'pinchOut = 512

gesture'pinchOut :: (Num a) => a

{- typedef enum {
            camera'custom = 0,
            camera'free,
            camera'orbital,
            camera'first_PERSON,
            camera'third_PERSON
        } CameraMode; -}
type CameraMode = CUInt

cameraMode'custom = 0

cameraMode'custom :: (Num a) => a

cameraMode'free = 1

cameraMode'free :: (Num a) => a

cameraMode'orbital = 2

cameraMode'orbital :: (Num a) => a

cameraMode'firstPerson = 3

cameraMode'firstPerson :: (Num a) => a

cameraMode'thirdPerson = 4

cameraMode'thirdPerson :: (Num a) => a

{- typedef enum {
            camera'perspective = 0, camera'orthographic
        } CameraProjection; -}
type CameraProjection = CUInt

cameraProjection'perspective = 0

cameraProjection'perspective :: (Num a) => a

cameraProjection'orthographic = 1

cameraProjection'orthographic :: (Num a) => a

{- typedef enum {
            npatch'ninePatch = 0,
            npatch'threePatch_VERTICAL,
            npatch'threePatch_HORIZONTAL
        } NPatchLayout; -}
type NPatchLayout = CUInt

npatch'ninePatch = 0

npatch'ninePatch :: (Num a) => a

npatch'threePatchVertical = 1

npatch'threePatchVertical :: (Num a) => a

npatch'threePatchHorizontal = 2

npatch'threePatchHorizontal :: (Num a) => a

-- type TraceLogCallback = FunPtr (CInt -> CString -> __builtin_va_list -> IO ())

-- foreign import ccall "wrapper"
--   mk'TraceLogCallback ::
--     (CInt -> CString -> __builtin_va_list -> IO ()) -> IO TraceLogCallback

-- foreign import ccall "dynamic"
--   mK'TraceLogCallback ::
--     TraceLogCallback -> (CInt -> CString -> __builtin_va_list -> IO ())

type LoadFileDataCallback = FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall "wrapper"
  mk'loadFileDataCallback ::
    (CString -> Ptr CUInt -> IO (Ptr CUChar)) -> IO LoadFileDataCallback

foreign import ccall "dynamic"
  mK'loadFileDataCallback ::
    LoadFileDataCallback -> (CString -> Ptr CUInt -> IO (Ptr CUChar))

type SaveFileDataCallback = FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall "wrapper"
  mk'saveFileDataCallback ::
    (CString -> Ptr () -> CUInt -> IO CInt) -> IO SaveFileDataCallback

foreign import ccall "dynamic"
  mK'saveFileDataCallback ::
    SaveFileDataCallback -> (CString -> Ptr () -> CUInt -> IO CInt)

type LoadFileTextCallback = FunPtr (CString -> IO CString)

foreign import ccall "wrapper"
  mk'loadFileTextCallback ::
    (CString -> IO CString) -> IO LoadFileTextCallback

foreign import ccall "dynamic"
  mK'loadFileTextCallback ::
    LoadFileTextCallback -> (CString -> IO CString)

type SaveFileTextCallback = FunPtr (CString -> CString -> IO CInt)

foreign import ccall "wrapper"
  mk'saveFileTextCallback ::
    (CString -> CString -> IO CInt) -> IO SaveFileTextCallback

foreign import ccall "dynamic"
  mK'saveFileTextCallback ::
    SaveFileTextCallback -> (CString -> CString -> IO CInt)

foreign import ccall "raylib.h InitWindow"
  initWindow ::
    CInt -> CInt -> CString -> IO ()

foreign import ccall "raylib.h &InitWindow"
  p'initWindow ::
    FunPtr (CInt -> CInt -> CString -> IO ())

foreign import ccall "raylib.h WindowShouldClose"
  windowShouldClose ::
    IO CInt

foreign import ccall "raylib.h &WindowShouldClose"
  p'windowShouldClose ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h CloseWindow"
  closeWindow ::
    IO ()

foreign import ccall "raylib.h &CloseWindow"
  p'closeWindow ::
    FunPtr (IO ())

foreign import ccall "raylib.h IsWindowReady"
  isWindowReady ::
    IO CInt

foreign import ccall "raylib.h &IsWindowReady"
  p'isWindowReady ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowFullscreen"
  isWindowFullscreen ::
    IO CInt

foreign import ccall "raylib.h &IsWindowFullscreen"
  p'isWindowFullscreen ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowHidden"
  isWindowHidden ::
    IO CInt

foreign import ccall "raylib.h &IsWindowHidden"
  p'isWindowHidden ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowMinimized"
  isWindowMinimized ::
    IO CInt

foreign import ccall "raylib.h &IsWindowMinimized"
  p'isWindowMinimized ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowMaximized"
  isWindowMaximized ::
    IO CInt

foreign import ccall "raylib.h &IsWindowMaximized"
  p'isWindowMaximized ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowFocused"
  isWindowFocused ::
    IO CInt

foreign import ccall "raylib.h &IsWindowFocused"
  p'isWindowFocused ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowResized"
  isWindowResized ::
    IO CInt

foreign import ccall "raylib.h &IsWindowResized"
  p'isWindowResized ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsWindowState"
  isWindowState ::
    CUInt -> IO CInt

foreign import ccall "raylib.h &IsWindowState"
  p'isWindowState ::
    FunPtr (CUInt -> IO CInt)

foreign import ccall "raylib.h SetWindowState"
  setWindowState ::
    CUInt -> IO ()

foreign import ccall "raylib.h &SetWindowState"
  p'setWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall "raylib.h ClearWindowState"
  clearWindowState ::
    CUInt -> IO ()

foreign import ccall "raylib.h &ClearWindowState"
  p'clearWindowState ::
    FunPtr (CUInt -> IO ())

foreign import ccall "raylib.h ToggleFullscreen"
  toggleFullscreen ::
    IO ()

foreign import ccall "raylib.h &ToggleFullscreen"
  p'toggleFullscreen ::
    FunPtr (IO ())

foreign import ccall "raylib.h MaximizeWindow"
  maximizeWindow ::
    IO ()

foreign import ccall "raylib.h &MaximizeWindow"
  p'maximizeWindow ::
    FunPtr (IO ())

foreign import ccall "raylib.h MinimizeWindow"
  minimizeWindow ::
    IO ()

foreign import ccall "raylib.h &MinimizeWindow"
  p'minimizeWindow ::
    FunPtr (IO ())

foreign import ccall "raylib.h RestoreWindow"
  restoreWindow ::
    IO ()

foreign import ccall "raylib.h &RestoreWindow"
  p'restoreWindow ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h SetWindowIcon" setWindowIcon :: Ptr Image -> IO ()



foreign import ccall "raylib.h &SetWindowIcon"
  p'setWindowIcon ::
    FunPtr (Image -> IO ())

foreign import ccall "raylib.h SetWindowTitle"
  setWindowTitle ::
    CString -> IO ()

foreign import ccall "raylib.h &SetWindowTitle"
  p'setWindowTitle ::
    FunPtr (CString -> IO ())

foreign import ccall "raylib.h SetWindowPosition"
  setWindowPosition ::
    CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetWindowPosition"
  p'setWindowPosition ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall "raylib.h SetWindowMonitor"
  setWindowMonitor ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetWindowMonitor"
  p'setWindowMonitor ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h SetWindowMinSize"
  setWindowMinSize ::
    CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetWindowMinSize"
  p'setWindowMinSize ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall "raylib.h SetWindowSize"
  setWindowSize ::
    CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetWindowSize"
  p'setWindowSize ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall "raylib.h SetWindowOpacity"
  setWindowOpacity ::
    CFloat -> IO ()

foreign import ccall "raylib.h &SetWindowOpacity"
  p'setWindowOpacity ::
    FunPtr (CFloat -> IO ())

foreign import ccall "raylib.h GetWindowHandle"
  getWindowHandle ::
    IO (Ptr ())

foreign import ccall "raylib.h &GetWindowHandle"
  p'getWindowHandle ::
    FunPtr (IO (Ptr ()))

foreign import ccall "raylib.h GetScreenWidth"
  getScreenWidth ::
    IO CInt

foreign import ccall "raylib.h &GetScreenWidth"
  p'getScreenWidth ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetScreenHeight"
  getScreenHeight ::
    IO CInt

foreign import ccall "raylib.h &GetScreenHeight"
  p'getScreenHeight ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetRenderWidth"
  getRenderWidth ::
    IO CInt

foreign import ccall "raylib.h &GetRenderWidth"
  p'getRenderWidth ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetRenderHeight"
  getRenderHeight ::
    IO CInt

foreign import ccall "raylib.h &GetRenderHeight"
  p'getRenderHeight ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetMonitorCount"
  getMonitorCount ::
    IO CInt

foreign import ccall "raylib.h &GetMonitorCount"
  p'getMonitorCount ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetCurrentMonitor"
  getCurrentMonitor ::
    IO CInt

foreign import ccall "raylib.h &GetCurrentMonitor"
  p'getCurrentMonitor ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h GetMonitorPosition" getMonitorPosition :: CInt -> IO (Ptr Vector2)



foreign import ccall "raylib.h &GetMonitorPosition"
  p'getMonitorPosition ::
    FunPtr (CInt -> IO Vector2)

foreign import ccall "raylib.h GetMonitorWidth"
  getMonitorWidth ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetMonitorWidth"
  p'getMonitorWidth ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetMonitorHeight"
  getMonitorHeight ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetMonitorHeight"
  p'getMonitorHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetMonitorPhysicalWidth"
  getMonitorPhysicalWidth ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetMonitorPhysicalWidth"
  p'getMonitorPhysicalWidth ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetMonitorPhysicalHeight"
  getMonitorPhysicalHeight ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetMonitorPhysicalHeight"
  p'getMonitorPhysicalHeight ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetMonitorRefreshRate"
  getMonitorRefreshRate ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetMonitorRefreshRate"
  p'getMonitorRefreshRate ::
    FunPtr (CInt -> IO CInt)

foreign import ccall safe "bindings.h GetWindowPosition" getWindowPosition :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetWindowPosition"
  p'getWindowPosition ::
    FunPtr (IO Vector2)

foreign import ccall safe "bindings.h GetWindowScaleDPI" getWindowScaleDPI :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetWindowScaleDPI"
  p'getWindowScaleDPI ::
    FunPtr (IO Vector2)

foreign import ccall "raylib.h GetMonitorName"
  getMonitorName ::
    CInt -> IO CString

foreign import ccall "raylib.h &GetMonitorName"
  p'getMonitorName ::
    FunPtr (CInt -> IO CString)

foreign import ccall "raylib.h SetClipboardText"
  setClipboardText ::
    CString -> IO ()

foreign import ccall "raylib.h &SetClipboardText"
  p'setClipboardText ::
    FunPtr (CString -> IO ())

foreign import ccall "raylib.h GetClipboardText"
  getClipboardText ::
    IO CString

foreign import ccall "raylib.h &GetClipboardText"
  p'getClipboardText ::
    FunPtr (IO CString)

foreign import ccall "raylib.h EnableEventWaiting"
  enableEventWaiting ::
    IO ()

foreign import ccall "raylib.h &EnableEventWaiting"
  p'enableEventWaiting ::
    FunPtr (IO ())

foreign import ccall "raylib.h DisableEventWaiting"
  disableEventWaiting ::
    IO ()

foreign import ccall "raylib.h &DisableEventWaiting"
  p'disableEventWaiting ::
    FunPtr (IO ())

foreign import ccall "raylib.h SwapScreenBuffer"
  swapScreenBuffer ::
    IO ()

foreign import ccall "raylib.h &SwapScreenBuffer"
  p'swapScreenBuffer ::
    FunPtr (IO ())

foreign import ccall "raylib.h PollInputEvents"
  pollInputEvents ::
    IO ()

foreign import ccall "raylib.h &PollInputEvents"
  p'pollInputEvents ::
    FunPtr (IO ())

foreign import ccall "raylib.h WaitTime"
  waitTime ::
    CDouble -> IO ()

foreign import ccall "raylib.h &WaitTime"
  p'waitTime ::
    FunPtr (CDouble -> IO ())

foreign import ccall "raylib.h ShowCursor"
  showCursor ::
    IO ()

foreign import ccall "raylib.h &ShowCursor"
  p'showCursor ::
    FunPtr (IO ())

foreign import ccall "raylib.h HideCursor"
  hideCursor ::
    IO ()

foreign import ccall "raylib.h &HideCursor"
  p'hideCursor ::
    FunPtr (IO ())

foreign import ccall "raylib.h IsCursorHidden"
  isCursorHidden ::
    IO CInt

foreign import ccall "raylib.h &IsCursorHidden"
  p'isCursorHidden ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h EnableCursor"
  enableCursor ::
    IO ()

foreign import ccall "raylib.h &EnableCursor"
  p'enableCursor ::
    FunPtr (IO ())

foreign import ccall "raylib.h DisableCursor"
  disableCursor ::
    IO ()

foreign import ccall "raylib.h &DisableCursor"
  p'disableCursor ::
    FunPtr (IO ())

foreign import ccall "raylib.h IsCursorOnScreen"
  isCursorOnScreen ::
    IO CInt

foreign import ccall "raylib.h &IsCursorOnScreen"
  p'isCursorOnScreen ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h ClearBackground" clearBackground :: Ptr Color -> IO ()



foreign import ccall "raylib.h &ClearBackground"
  p'clearBackground ::
    FunPtr (Color -> IO ())

foreign import ccall "raylib.h BeginDrawing"
  beginDrawing ::
    IO ()

foreign import ccall "raylib.h &BeginDrawing"
  p'beginDrawing ::
    FunPtr (IO ())

foreign import ccall "raylib.h EndDrawing"
  endDrawing ::
    IO ()

foreign import ccall "raylib.h &EndDrawing"
  p'endDrawing ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginMode2D" beginMode2D :: Ptr Camera2D -> IO ()



foreign import ccall "raylib.h &BeginMode2D"
  p'beginMode2D ::
    FunPtr (Camera2D -> IO ())

foreign import ccall "raylib.h EndMode2D"
  endMode2D ::
    IO ()

foreign import ccall "raylib.h &EndMode2D"
  p'endMode2D ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginMode3D" beginMode3D :: Ptr Camera3D -> IO ()



foreign import ccall "raylib.h &BeginMode3D"
  p'beginMode3D ::
    FunPtr (Camera3D -> IO ())

foreign import ccall "raylib.h EndMode3D"
  endMode3D ::
    IO ()

foreign import ccall "raylib.h &EndMode3D"
  p'endMode3D ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginTextureMode" beginTextureMode :: Ptr RenderTexture -> IO ()



foreign import ccall "raylib.h &BeginTextureMode"
  p'beginTextureMode ::
    FunPtr (RenderTexture -> IO ())

foreign import ccall "raylib.h EndTextureMode"
  endTextureMode ::
    IO ()

foreign import ccall "raylib.h &EndTextureMode"
  p'endTextureMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginShaderMode" beginShaderMode :: Ptr Shader -> IO ()



foreign import ccall "raylib.h &BeginShaderMode"
  p'beginShaderMode ::
    FunPtr (Shader -> IO ())

foreign import ccall "raylib.h EndShaderMode"
  endShaderMode ::
    IO ()

foreign import ccall "raylib.h &EndShaderMode"
  p'endShaderMode ::
    FunPtr (IO ())

foreign import ccall "raylib.h BeginBlendMode"
  beginBlendMode ::
    CInt -> IO ()

foreign import ccall "raylib.h &BeginBlendMode"
  p'beginBlendMode ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h EndBlendMode"
  endBlendMode ::
    IO ()

foreign import ccall "raylib.h &EndBlendMode"
  p'endBlendMode ::
    FunPtr (IO ())

foreign import ccall "raylib.h BeginScissorMode"
  beginScissorMode ::
    CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &BeginScissorMode"
  p'beginScissorMode ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall "raylib.h EndScissorMode"
  endScissorMode ::
    IO ()

foreign import ccall "raylib.h &EndScissorMode"
  p'endScissorMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h BeginVrStereoMode" beginVrStereoMode :: Ptr VrStereoConfig -> IO ()



foreign import ccall "raylib.h &BeginVrStereoMode"
  p'beginVrStereoMode ::
    FunPtr (VrStereoConfig -> IO ())

foreign import ccall "raylib.h EndVrStereoMode"
  endVrStereoMode ::
    IO ()

foreign import ccall "raylib.h &EndVrStereoMode"
  p'endVrStereoMode ::
    FunPtr (IO ())

foreign import ccall safe "bindings.h LoadVrStereoConfig" loadVrStereoConfig :: Ptr VrDeviceInfo -> IO (Ptr VrStereoConfig)



foreign import ccall "raylib.h &LoadVrStereoConfig"
  p'loadVrStereoConfig ::
    FunPtr (VrDeviceInfo -> IO VrStereoConfig)

foreign import ccall safe "bindings.h UnloadVrStereoConfig" unloadVrStereoConfig :: Ptr VrStereoConfig -> IO ()



foreign import ccall "raylib.h &UnloadVrStereoConfig"
  p'unloadVrStereoConfig ::
    FunPtr (VrStereoConfig -> IO ())

foreign import ccall safe "bindings.h LoadShader" loadShader :: CString -> CString -> IO (Ptr Shader)



foreign import ccall "raylib.h &LoadShader"
  p'loadShader ::
    FunPtr (CString -> CString -> IO Shader)

foreign import ccall safe "bindings.h LoadShaderFromMemory" loadShaderFromMemory :: CString -> CString -> IO (Ptr Shader)



foreign import ccall "raylib.h &LoadShaderFromMemory"
  p'loadShaderFromMemory ::
    FunPtr (CString -> CString -> IO Shader)

foreign import ccall safe "bindings.h GetShaderLocation" getShaderLocation :: Ptr Shader -> CString -> IO CInt



foreign import ccall "raylib.h &GetShaderLocation"
  p'getShaderLocation ::
    FunPtr (Shader -> CString -> IO CInt)

foreign import ccall safe "bindings.h GetShaderLocationAttrib" getShaderLocationAttrib :: Ptr Shader -> CString -> IO CInt



foreign import ccall "raylib.h &GetShaderLocationAttrib"
  p'getShaderLocationAttrib ::
    FunPtr (Shader -> CString -> IO CInt)

foreign import ccall safe "bindings.h SetShaderValue" setShaderValue :: Ptr Shader -> CInt -> Ptr () -> CInt -> IO ()



foreign import ccall "raylib.h &SetShaderValue"
  p'setShaderValue ::
    FunPtr (Shader -> CInt -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShaderValueV" setShaderValueV :: Ptr Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ()



foreign import ccall "raylib.h &SetShaderValueV"
  p'setShaderValueV ::
    FunPtr (Shader -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShaderValueMatrix" setShaderValueMatrix :: Ptr Shader -> CInt -> Ptr Matrix -> IO ()



foreign import ccall "raylib.h &SetShaderValueMatrix"
  p'setShaderValueMatrix ::
    FunPtr (Shader -> CInt -> Matrix -> IO ())

foreign import ccall safe "bindings.h SetShaderValueTexture" setShaderValueTexture :: Ptr Shader -> CInt -> Ptr Texture -> IO ()



foreign import ccall "raylib.h &SetShaderValueTexture"
  p'setShaderValueTexture ::
    FunPtr (Shader -> CInt -> Texture -> IO ())

foreign import ccall safe "bindings.h UnloadShader" unloadShader :: Ptr Shader -> IO ()



foreign import ccall "raylib.h &UnloadShader"
  p'unloadShader ::
    FunPtr (Shader -> IO ())

foreign import ccall safe "bindings.h GetMouseRay" getMouseRay :: Ptr Vector2 -> Ptr Camera3D -> IO (Ptr Ray)



foreign import ccall "raylib.h &GetMouseRay"
  p'getMouseRay ::
    FunPtr (Vector2 -> Camera3D -> IO Ray)

foreign import ccall safe "bindings.h GetCameraMatrix" getCameraMatrix :: Ptr Camera3D -> IO (Ptr Matrix)



foreign import ccall "raylib.h &GetCameraMatrix"
  p'getCameraMatrix ::
    FunPtr (Camera3D -> IO Matrix)

foreign import ccall safe "bindings.h GetCameraMatrix2D" getCameraMatrix2D :: Ptr Camera2D -> IO (Ptr Matrix)



foreign import ccall "raylib.h &GetCameraMatrix2D"
  p'getCameraMatrix2D ::
    FunPtr (Camera2D -> IO Matrix)

foreign import ccall safe "bindings.h GetWorldToScreen" getWorldToScreen :: Ptr Vector3 -> Ptr Camera3D -> IO (Ptr Vector2)



foreign import ccall "raylib.h &GetWorldToScreen"
  p'getWorldToScreen ::
    FunPtr (Vector3 -> Camera3D -> IO Vector2)

foreign import ccall safe "bindings.h GetScreenToWorld2D" getScreenToWorld2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)



foreign import ccall "raylib.h &GetScreenToWorld2D"
  p'getScreenToWorld2D ::
    FunPtr (Vector2 -> Camera2D -> IO Vector2)

foreign import ccall safe "bindings.h GetWorldToScreenEx" getWorldToScreenEx :: Ptr Vector3 -> Ptr Camera3D -> CInt -> CInt -> IO (Ptr Vector2)



foreign import ccall "raylib.h &GetWorldToScreenEx"
  p'getWorldToScreenEx ::
    FunPtr (Vector3 -> Camera3D -> CInt -> CInt -> IO Vector2)

foreign import ccall safe "bindings.h GetWorldToScreen2D" getWorldToScreen2D :: Ptr Vector2 -> Ptr Camera2D -> IO (Ptr Vector2)



foreign import ccall "raylib.h &GetWorldToScreen2D"
  p'getWorldToScreen2D ::
    FunPtr (Vector2 -> Camera2D -> IO Vector2)

foreign import ccall "raylib.h SetTargetFPS"
  setTargetFPS ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetTargetFPS"
  p'setTargetFPS ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h GetFPS"
  getFPS ::
    IO CInt

foreign import ccall "raylib.h &GetFPS"
  p'getFPS ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetFrameTime"
  getFrameTime ::
    IO CFloat

foreign import ccall "raylib.h &GetFrameTime"
  p'getFrameTime ::
    FunPtr (IO CFloat)

foreign import ccall "raylib.h GetTime"
  getTime ::
    IO CDouble

foreign import ccall "raylib.h &GetTime"
  p'getTime ::
    FunPtr (IO CDouble)

foreign import ccall "raylib.h GetRandomValue"
  getRandomValue ::
    CInt -> CInt -> IO CInt

foreign import ccall "raylib.h &GetRandomValue"
  p'getRandomValue ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall "raylib.h SetRandomSeed"
  setRandomSeed ::
    CUInt -> IO ()

foreign import ccall "raylib.h &SetRandomSeed"
  p'setRandomSeed ::
    FunPtr (CUInt -> IO ())

foreign import ccall "raylib.h TakeScreenshot"
  takeScreenshot ::
    CString -> IO ()

foreign import ccall "raylib.h &TakeScreenshot"
  p'takeScreenshot ::
    FunPtr (CString -> IO ())

foreign import ccall "raylib.h SetConfigFlags"
  setConfigFlags ::
    CUInt -> IO ()

foreign import ccall "raylib.h &SetConfigFlags"
  p'setConfigFlags ::
    FunPtr (CUInt -> IO ())

foreign import ccall "raylib.h TraceLog"
  traceLog ::
    CInt -> CString -> IO ()

foreign import ccall "raylib.h &TraceLog"
  p'traceLog ::
    FunPtr (CInt -> CString -> IO ())

foreign import ccall "raylib.h SetTraceLogLevel"
  setTraceLogLevel ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetTraceLogLevel"
  p'setTraceLogLevel ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h MemAlloc"
  memAlloc ::
    CInt -> IO (Ptr ())

foreign import ccall "raylib.h &MemAlloc"
  p'memAlloc ::
    FunPtr (CInt -> IO (Ptr ()))

foreign import ccall "raylib.h MemRealloc"
  memRealloc ::
    Ptr () -> CInt -> IO (Ptr ())

foreign import ccall "raylib.h &MemRealloc"
  p'memRealloc ::
    FunPtr (Ptr () -> CInt -> IO (Ptr ()))

foreign import ccall "raylib.h MemFree"
  memFree ::
    Ptr () -> IO ()

foreign import ccall "raylib.h &MemFree"
  p'memFree ::
    FunPtr (Ptr () -> IO ())

foreign import ccall "raylib.h OpenURL"
  openURL ::
    CString -> IO ()

foreign import ccall "raylib.h &OpenURL"
  p'openURL ::
    FunPtr (CString -> IO ())

-- foreign import ccall "raylib.h SetTraceLogCallback"
--   SetTraceLogCallback ::
--     TraceLogCallback -> IO ()

-- foreign import ccall "raylib.h &SetTraceLogCallback"
--   p'SetTraceLogCallback ::
--     FunPtr (TraceLogCallback -> IO ())

foreign import ccall "raylib.h SetLoadFileDataCallback"
  setLoadFileDataCallback ::
    LoadFileDataCallback -> IO ()

foreign import ccall "raylib.h &SetLoadFileDataCallback"
  p'setLoadFileDataCallback ::
    FunPtr (LoadFileDataCallback -> IO ())

foreign import ccall "raylib.h SetSaveFileDataCallback"
  setSaveFileDataCallback ::
    SaveFileDataCallback -> IO ()

foreign import ccall "raylib.h &SetSaveFileDataCallback"
  p'setSaveFileDataCallback ::
    FunPtr (SaveFileDataCallback -> IO ())

foreign import ccall "raylib.h SetLoadFileTextCallback"
  setLoadFileTextCallback ::
    LoadFileTextCallback -> IO ()

foreign import ccall "raylib.h &SetLoadFileTextCallback"
  p'setLoadFileTextCallback ::
    FunPtr (LoadFileTextCallback -> IO ())

foreign import ccall "raylib.h SetSaveFileTextCallback"
  setSaveFileTextCallback ::
    SaveFileTextCallback -> IO ()

foreign import ccall "raylib.h &SetSaveFileTextCallback"
  p'setSaveFileTextCallback ::
    FunPtr (SaveFileTextCallback -> IO ())

foreign import ccall "raylib.h LoadFileData"
  loadFileData ::
    CString -> Ptr CUInt -> IO (Ptr CUChar)

foreign import ccall "raylib.h &LoadFileData"
  p'loadFileData ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr CUChar))

foreign import ccall "raylib.h UnloadFileData"
  unloadFileData ::
    Ptr CUChar -> IO ()

foreign import ccall "raylib.h &UnloadFileData"
  p'unloadFileData ::
    FunPtr (Ptr CUChar -> IO ())

foreign import ccall "raylib.h SaveFileData"
  saveFileData ::
    CString -> Ptr () -> CUInt -> IO CInt

foreign import ccall "raylib.h &SaveFileData"
  p'saveFileData ::
    FunPtr (CString -> Ptr () -> CUInt -> IO CInt)

foreign import ccall "raylib.h ExportDataAsCode"
  exportDataAsCode ::
    CString -> CUInt -> CString -> IO CInt

foreign import ccall "raylib.h &ExportDataAsCode"
  p'exportDataAsCode ::
    FunPtr (CString -> CUInt -> CString -> IO CInt)

foreign import ccall "raylib.h LoadFileText"
  loadFileText ::
    CString -> IO CString

foreign import ccall "raylib.h &LoadFileText"
  p'loadFileText ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h UnloadFileText"
  unloadFileText ::
    CString -> IO ()

foreign import ccall "raylib.h &UnloadFileText"
  p'unloadFileText ::
    FunPtr (CString -> IO ())

foreign import ccall "raylib.h SaveFileText"
  saveFileText ::
    CString -> CString -> IO CInt

foreign import ccall "raylib.h &SaveFileText"
  p'saveFileText ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall "raylib.h FileExists"
  fileExists ::
    CString -> IO CInt

foreign import ccall "raylib.h &FileExists"
  p'fileExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall "raylib.h DirectoryExists"
  directoryExists ::
    CString -> IO CInt

foreign import ccall "raylib.h &DirectoryExists"
  p'directoryExists ::
    FunPtr (CString -> IO CInt)

foreign import ccall "raylib.h IsFileExtension"
  isFileExtension ::
    CString -> CString -> IO CInt

foreign import ccall "raylib.h &IsFileExtension"
  p'isFileExtension ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall "raylib.h GetFileLength"
  getFileLength ::
    CString -> IO CInt

foreign import ccall "raylib.h &GetFileLength"
  p'getFileLength ::
    FunPtr (CString -> IO CInt)

foreign import ccall "raylib.h GetFileExtension"
  getFileExtension ::
    CString -> IO CString

foreign import ccall "raylib.h &GetFileExtension"
  p'getFileExtension ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h GetFileName"
  getFileName ::
    CString -> IO CString

foreign import ccall "raylib.h &GetFileName"
  p'getFileName ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h GetFileNameWithoutExt"
  getFileNameWithoutExt ::
    CString -> IO CString

foreign import ccall "raylib.h &GetFileNameWithoutExt"
  p'getFileNameWithoutExt ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h GetDirectoryPath"
  getDirectoryPath ::
    CString -> IO CString

foreign import ccall "raylib.h &GetDirectoryPath"
  p'getDirectoryPath ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h GetPrevDirectoryPath"
  getPrevDirectoryPath ::
    CString -> IO CString

foreign import ccall "raylib.h &GetPrevDirectoryPath"
  p'getPrevDirectoryPath ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h GetWorkingDirectory"
  getWorkingDirectory ::
    IO CString

foreign import ccall "raylib.h &GetWorkingDirectory"
  p'getWorkingDirectory ::
    FunPtr (IO CString)

foreign import ccall "raylib.h GetApplicationDirectory"
  getApplicationDirectory ::
    IO CString

foreign import ccall "raylib.h &GetApplicationDirectory"
  p'getApplicationDirectory ::
    FunPtr (IO CString)

foreign import ccall "raylib.h ChangeDirectory"
  changeDirectory ::
    CString -> IO CInt

foreign import ccall "raylib.h &ChangeDirectory"
  p'changeDirectory ::
    FunPtr (CString -> IO CInt)

foreign import ccall "raylib.h IsPathFile"
  isPathFile ::
    CString -> IO CInt

foreign import ccall "raylib.h &IsPathFile"
  p'isPathFile ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "bindings.h LoadDirectoryFiles" loadDirectoryFiles :: CString -> IO (Ptr FilePathList)



foreign import ccall "raylib.h &LoadDirectoryFiles"
  p'loadDirectoryFiles ::
    FunPtr (CString -> IO FilePathList)

foreign import ccall safe "bindings.h LoadDirectoryFilesEx" loadDirectoryFilesEx :: CString -> CString -> CInt -> IO (Ptr FilePathList)



foreign import ccall "raylib.h &LoadDirectoryFilesEx"
  p'loadDirectoryFilesEx ::
    FunPtr (CString -> CString -> CInt -> IO FilePathList)

foreign import ccall safe "bindings.h UnloadDirectoryFiles" unloadDirectoryFiles :: Ptr FilePathList -> IO ()



foreign import ccall "raylib.h &UnloadDirectoryFiles"
  p'unloadDirectoryFiles ::
    FunPtr (FilePathList -> IO ())

foreign import ccall "raylib.h IsFileDropped"
  isFileDropped ::
    IO CInt

foreign import ccall "raylib.h &IsFileDropped"
  p'isFileDropped ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h LoadDroppedFiles" loadDroppedFiles :: IO (Ptr FilePathList)



foreign import ccall "raylib.h &LoadDroppedFiles"
  p'loadDroppedFiles ::
    FunPtr (IO FilePathList)

foreign import ccall safe "bindings.h UnloadDroppedFiles" unloadDroppedFiles :: Ptr FilePathList -> IO ()



foreign import ccall "raylib.h &UnloadDroppedFiles"
  p'unloadDroppedFiles ::
    FunPtr (FilePathList -> IO ())

foreign import ccall "raylib.h GetFileModTime"
  getFileModTime ::
    CString -> IO CLong

foreign import ccall "raylib.h &GetFileModTime"
  p'getFileModTime ::
    FunPtr (CString -> IO CLong)

foreign import ccall "raylib.h CompressData"
  compressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall "raylib.h &CompressData"
  p'compressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall "raylib.h DecompressData"
  decompressData ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall "raylib.h &DecompressData"
  p'decompressData ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall "raylib.h EncodeDataBase64"
  encodeDataBase64 ::
    Ptr CUChar -> CInt -> Ptr CInt -> IO CString

foreign import ccall "raylib.h &EncodeDataBase64"
  p'encodeDataBase64 ::
    FunPtr (Ptr CUChar -> CInt -> Ptr CInt -> IO CString)

foreign import ccall "raylib.h DecodeDataBase64"
  decodeDataBase64 ::
    Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall "raylib.h &DecodeDataBase64"
  p'decodeDataBase64 ::
    FunPtr (Ptr CUChar -> Ptr CInt -> IO (Ptr CUChar))

foreign import ccall "raylib.h IsKeyPressed"
  isKeyPressed ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsKeyPressed"
  p'isKeyPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h IsKeyDown"
  isKeyDown ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsKeyDown"
  p'isKeyDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h IsKeyReleased"
  isKeyReleased ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsKeyReleased"
  p'isKeyReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h IsKeyUp"
  isKeyUp ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsKeyUp"
  p'isKeyUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h SetExitKey"
  setExitKey ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetExitKey"
  p'setExitKey ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h GetKeyPressed"
  getKeyPressed ::
    IO CInt

foreign import ccall "raylib.h &GetKeyPressed"
  p'getKeyPressed ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetCharPressed"
  getCharPressed ::
    IO CInt

foreign import ccall "raylib.h &GetCharPressed"
  p'getCharPressed ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h IsGamepadAvailable"
  isGamepadAvailable ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsGamepadAvailable"
  p'isGamepadAvailable ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetGamepadName"
  getGamepadName ::
    CInt -> IO CString

foreign import ccall "raylib.h &GetGamepadName"
  p'getGamepadName ::
    FunPtr (CInt -> IO CString)

foreign import ccall "raylib.h IsGamepadButtonPressed"
  isGamepadButtonPressed ::
    CInt -> CInt -> IO CInt

foreign import ccall "raylib.h &IsGamepadButtonPressed"
  p'isGamepadButtonPressed ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall "raylib.h IsGamepadButtonDown"
  isGamepadButtonDown ::
    CInt -> CInt -> IO CInt

foreign import ccall "raylib.h &IsGamepadButtonDown"
  p'isGamepadButtonDown ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall "raylib.h IsGamepadButtonReleased"
  isGamepadButtonReleased ::
    CInt -> CInt -> IO CInt

foreign import ccall "raylib.h &IsGamepadButtonReleased"
  p'isGamepadButtonReleased ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall "raylib.h IsGamepadButtonUp"
  isGamepadButtonUp ::
    CInt -> CInt -> IO CInt

foreign import ccall "raylib.h &IsGamepadButtonUp"
  p'isGamepadButtonUp ::
    FunPtr (CInt -> CInt -> IO CInt)

foreign import ccall "raylib.h GetGamepadButtonPressed"
  getGamepadButtonPressed ::
    IO CInt

foreign import ccall "raylib.h &GetGamepadButtonPressed"
  p'getGamepadButtonPressed ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetGamepadAxisCount"
  getGamepadAxisCount ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetGamepadAxisCount"
  p'getGamepadAxisCount ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetGamepadAxisMovement"
  getGamepadAxisMovement ::
    CInt -> CInt -> IO CFloat

foreign import ccall "raylib.h &GetGamepadAxisMovement"
  p'getGamepadAxisMovement ::
    FunPtr (CInt -> CInt -> IO CFloat)

foreign import ccall "raylib.h SetGamepadMappings"
  setGamepadMappings ::
    CString -> IO CInt

foreign import ccall "raylib.h &SetGamepadMappings"
  p'setGamepadMappings ::
    FunPtr (CString -> IO CInt)

foreign import ccall "raylib.h IsMouseButtonPressed"
  isMouseButtonPressed ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsMouseButtonPressed"
  p'isMouseButtonPressed ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h IsMouseButtonDown"
  isMouseButtonDown ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsMouseButtonDown"
  p'isMouseButtonDown ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h IsMouseButtonReleased"
  isMouseButtonReleased ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsMouseButtonReleased"
  p'isMouseButtonReleased ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h IsMouseButtonUp"
  isMouseButtonUp ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsMouseButtonUp"
  p'isMouseButtonUp ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetMouseX"
  getMouseX ::
    IO CInt

foreign import ccall "raylib.h &GetMouseX"
  p'getMouseX ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetMouseY"
  getMouseY ::
    IO CInt

foreign import ccall "raylib.h &GetMouseY"
  p'getMouseY ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h GetMousePosition" getMousePosition :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetMousePosition"
  p'getMousePosition ::
    FunPtr (IO Vector2)

foreign import ccall safe "bindings.h GetMouseDelta" getMouseDelta :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetMouseDelta"
  p'getMouseDelta ::
    FunPtr (IO Vector2)

foreign import ccall "raylib.h SetMousePosition"
  setMousePosition ::
    CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetMousePosition"
  p'setMousePosition ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall "raylib.h SetMouseOffset"
  setMouseOffset ::
    CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetMouseOffset"
  p'setMouseOffset ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall "raylib.h SetMouseScale"
  setMouseScale ::
    CFloat -> CFloat -> IO ()

foreign import ccall "raylib.h &SetMouseScale"
  p'setMouseScale ::
    FunPtr (CFloat -> CFloat -> IO ())

foreign import ccall "raylib.h GetMouseWheelMove"
  getMouseWheelMove ::
    IO CFloat

foreign import ccall "raylib.h &GetMouseWheelMove"
  p'getMouseWheelMove ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetMouseWheelMoveV" getMouseWheelMoveV :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetMouseWheelMoveV"
  p'getMouseWheelMoveV ::
    FunPtr (IO Vector2)

foreign import ccall "raylib.h SetMouseCursor"
  setMouseCursor ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetMouseCursor"
  p'setMouseCursor ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h GetTouchX"
  getTouchX ::
    IO CInt

foreign import ccall "raylib.h &GetTouchX"
  p'getTouchX ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetTouchY"
  getTouchY ::
    IO CInt

foreign import ccall "raylib.h &GetTouchY"
  p'getTouchY ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h GetTouchPosition" getTouchPosition :: CInt -> IO (Ptr Vector2)



foreign import ccall "raylib.h &GetTouchPosition"
  p'getTouchPosition ::
    FunPtr (CInt -> IO Vector2)

foreign import ccall "raylib.h GetTouchPointId"
  getTouchPointId ::
    CInt -> IO CInt

foreign import ccall "raylib.h &GetTouchPointId"
  p'getTouchPointId ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetTouchPointCount"
  getTouchPointCount ::
    IO CInt

foreign import ccall "raylib.h &GetTouchPointCount"
  p'getTouchPointCount ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h SetGesturesEnabled"
  setGesturesEnabled ::
    CUInt -> IO ()

foreign import ccall "raylib.h &SetGesturesEnabled"
  p'setGesturesEnabled ::
    FunPtr (CUInt -> IO ())

foreign import ccall "raylib.h IsGestureDetected"
  isGestureDetected ::
    CInt -> IO CInt

foreign import ccall "raylib.h &IsGestureDetected"
  p'isGestureDetected ::
    FunPtr (CInt -> IO CInt)

foreign import ccall "raylib.h GetGestureDetected"
  getGestureDetected ::
    IO CInt

foreign import ccall "raylib.h &GetGestureDetected"
  p'getGestureDetected ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h GetGestureHoldDuration"
  getGestureHoldDuration ::
    IO CFloat

foreign import ccall "raylib.h &GetGestureHoldDuration"
  p'getGestureHoldDuration ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetGestureDragVector" getGestureDragVector :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetGestureDragVector"
  p'getGestureDragVector ::
    FunPtr (IO Vector2)

foreign import ccall "raylib.h GetGestureDragAngle"
  getGestureDragAngle ::
    IO CFloat

foreign import ccall "raylib.h &GetGestureDragAngle"
  p'getGestureDragAngle ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h GetGesturePinchVector" getGesturePinchVector :: IO (Ptr Vector2)



foreign import ccall "raylib.h &GetGesturePinchVector"
  p'getGesturePinchVector ::
    FunPtr (IO Vector2)

foreign import ccall "raylib.h GetGesturePinchAngle"
  getGesturePinchAngle ::
    IO CFloat

foreign import ccall "raylib.h &GetGesturePinchAngle"
  p'getGesturePinchAngle ::
    FunPtr (IO CFloat)

foreign import ccall safe "bindings.h SetCameraMode" setCameraMode :: Ptr Camera3D -> CInt -> IO ()



foreign import ccall "raylib.h &SetCameraMode"
  p'setCameraMode ::
    FunPtr (Camera3D -> CInt -> IO ())

foreign import ccall "raylib.h UpdateCamera"
  updateCamera ::
    Ptr Camera3D -> IO ()

foreign import ccall "raylib.h &UpdateCamera"
  p'updateCamera ::
    FunPtr (Ptr Camera3D -> IO ())

foreign import ccall "raylib.h SetCameraPanControl"
  setCameraPanControl ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetCameraPanControl"
  p'setCameraPanControl ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h SetCameraAltControl"
  setCameraAltControl ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetCameraAltControl"
  p'setCameraAltControl ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h SetCameraSmoothZoomControl"
  setCameraSmoothZoomControl ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetCameraSmoothZoomControl"
  p'setCameraSmoothZoomControl ::
    FunPtr (CInt -> IO ())

foreign import ccall "raylib.h SetCameraMoveControls"
  setCameraMoveControls ::
    CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetCameraMoveControls"
  p'setCameraMoveControls ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h SetShapesTexture" setShapesTexture :: Ptr Texture -> Ptr Rectangle -> IO ()



foreign import ccall "raylib.h &SetShapesTexture"
  p'setShapesTexture ::
    FunPtr (Texture -> Rectangle -> IO ())

foreign import ccall safe "bindings.h DrawPixel" drawPixel :: CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPixel"
  p'drawPixel ::
    FunPtr (CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPixelV" drawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPixelV"
  p'drawPixelV ::
    FunPtr (Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLine" drawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLine"
  p'drawLine ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineV" drawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLineV"
  p'drawLineV ::
    FunPtr (Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineEx" drawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLineEx"
  p'drawLineEx ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezier" drawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLineBezier"
  p'drawLineBezier ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezierQuad" drawLineBezierQuad :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLineBezierQuad"
  p'drawLineBezierQuad ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineBezierCubic" drawLineBezierCubic :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLineBezierCubic"
  p'drawLineBezierCubic ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawLineStrip" drawLineStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLineStrip"
  p'drawLineStrip ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircle" drawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircle"
  p'drawCircle ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleSector" drawCircleSector :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircleSector"
  p'drawCircleSector ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleSectorLines" drawCircleSectorLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircleSectorLines"
  p'drawCircleSectorLines ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleGradient" drawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircleGradient"
  p'drawCircleGradient ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleV" drawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircleV"
  p'drawCircleV ::
    FunPtr (Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircleLines" drawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircleLines"
  p'drawCircleLines ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawEllipse" drawEllipse :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawEllipse"
  p'drawEllipse ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawEllipseLines" drawEllipseLines :: CInt -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawEllipseLines"
  p'drawEllipseLines ::
    FunPtr (CInt -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRing" drawRing :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRing"
  p'drawRing ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRingLines" drawRingLines :: Ptr Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRingLines"
  p'drawRingLines ::
    FunPtr (Vector2 -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangle" drawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangle"
  p'drawRectangle ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleV" drawRectangleV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleV"
  p'drawRectangleV ::
    FunPtr (Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRec" drawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleRec"
  p'drawRectangleRec ::
    FunPtr (Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectanglePro" drawRectanglePro :: Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectanglePro"
  p'drawRectanglePro ::
    FunPtr (Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientV" drawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleGradientV"
  p'drawRectangleGradientV ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientH" drawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleGradientH"
  p'drawRectangleGradientH ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleGradientEx" drawRectangleGradientEx :: Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleGradientEx"
  p'drawRectangleGradientEx ::
    FunPtr (Rectangle -> Color -> Color -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleLines" drawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleLines"
  p'drawRectangleLines ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleLinesEx" drawRectangleLinesEx :: Ptr Rectangle -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleLinesEx"
  p'drawRectangleLinesEx ::
    FunPtr (Rectangle -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRounded" drawRectangleRounded :: Ptr Rectangle -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleRounded"
  p'drawRectangleRounded ::
    FunPtr (Rectangle -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRectangleRoundedLines" drawRectangleRoundedLines :: Ptr Rectangle -> CFloat -> CInt -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRectangleRoundedLines"
  p'drawRectangleRoundedLines ::
    FunPtr (Rectangle -> CFloat -> CInt -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangle" drawTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTriangle"
  p'drawTriangle ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleLines" drawTriangleLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTriangleLines"
  p'drawTriangleLines ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleFan" drawTriangleFan :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTriangleFan"
  p'drawTriangleFan ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleStrip" drawTriangleStrip :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTriangleStrip"
  p'drawTriangleStrip ::
    FunPtr (Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPoly" drawPoly :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPoly"
  p'drawPoly ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPolyLines" drawPolyLines :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPolyLines"
  p'drawPolyLines ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPolyLinesEx" drawPolyLinesEx :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPolyLinesEx"
  p'drawPolyLinesEx ::
    FunPtr (Vector2 -> CInt -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h CheckCollisionRecs" checkCollisionRecs :: Ptr Rectangle -> Ptr Rectangle -> IO CInt



foreign import ccall "raylib.h &CheckCollisionRecs"
  p'checkCollisionRecs ::
    FunPtr (Rectangle -> Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionCircles" checkCollisionCircles :: Ptr Vector2 -> CFloat -> Ptr Vector2 -> CFloat -> IO CInt



foreign import ccall "raylib.h &CheckCollisionCircles"
  p'checkCollisionCircles ::
    FunPtr (Vector2 -> CFloat -> Vector2 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionCircleRec" checkCollisionCircleRec :: Ptr Vector2 -> CFloat -> Ptr Rectangle -> IO CInt



foreign import ccall "raylib.h &CheckCollisionCircleRec"
  p'checkCollisionCircleRec ::
    FunPtr (Vector2 -> CFloat -> Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointRec" checkCollisionPointRec :: Ptr Vector2 -> Ptr Rectangle -> IO CInt



foreign import ccall "raylib.h &CheckCollisionPointRec"
  p'checkCollisionPointRec ::
    FunPtr (Vector2 -> Rectangle -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointCircle" checkCollisionPointCircle :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> IO CInt



foreign import ccall "raylib.h &CheckCollisionPointCircle"
  p'checkCollisionPointCircle ::
    FunPtr (Vector2 -> Vector2 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointTriangle" checkCollisionPointTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CInt



foreign import ccall "raylib.h &CheckCollisionPointTriangle"
  p'checkCollisionPointTriangle ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionLines" checkCollisionLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> IO CInt



foreign import ccall "raylib.h &CheckCollisionLines"
  p'checkCollisionLines ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> Vector2 -> Ptr Vector2 -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionPointLine" checkCollisionPointLine :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> IO CInt



foreign import ccall "raylib.h &CheckCollisionPointLine"
  p'checkCollisionPointLine ::
    FunPtr (Vector2 -> Vector2 -> Vector2 -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetCollisionRec" getCollisionRec :: Ptr Rectangle -> Ptr Rectangle -> IO (Ptr Rectangle)



foreign import ccall "raylib.h &GetCollisionRec"
  p'getCollisionRec ::
    FunPtr (Rectangle -> Rectangle -> IO Rectangle)

foreign import ccall safe "bindings.h LoadImage" loadImage :: CString -> IO (Ptr Image)



foreign import ccall "raylib.h &LoadImage"
  p'loadImage ::
    FunPtr (CString -> IO Image)

foreign import ccall safe "bindings.h LoadImageRaw" loadImageRaw :: CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)



foreign import ccall "raylib.h &LoadImageRaw"
  p'loadImageRaw ::
    FunPtr (CString -> CInt -> CInt -> CInt -> CInt -> IO Image)

foreign import ccall safe "bindings.h LoadImageAnim" loadImageAnim :: CString -> Ptr CInt -> IO (Ptr Image)



foreign import ccall "raylib.h &LoadImageAnim"
  p'loadImageAnim ::
    FunPtr (CString -> Ptr CInt -> IO Image)

foreign import ccall safe "bindings.h LoadImageFromMemory" loadImageFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Image)



foreign import ccall "raylib.h &LoadImageFromMemory"
  p'loadImageFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Image)

foreign import ccall safe "bindings.h LoadImageFromTexture" loadImageFromTexture :: Ptr Texture -> IO (Ptr Image)



foreign import ccall "raylib.h &LoadImageFromTexture"
  p'loadImageFromTexture ::
    FunPtr (Texture -> IO Image)

foreign import ccall safe "bindings.h LoadImageFromScreen" loadImageFromScreen :: IO (Ptr Image)



foreign import ccall "raylib.h &LoadImageFromScreen"
  p'loadImageFromScreen ::
    FunPtr (IO Image)

foreign import ccall safe "bindings.h UnloadImage" unloadImage :: Ptr Image -> IO ()



foreign import ccall "raylib.h &UnloadImage"
  p'unloadImage ::
    FunPtr (Image -> IO ())

foreign import ccall safe "bindings.h ExportImage" exportImage :: Ptr Image -> CString -> IO CInt



foreign import ccall "raylib.h &ExportImage"
  p'exportImage ::
    FunPtr (Image -> CString -> IO CInt)

foreign import ccall safe "bindings.h ExportImageAsCode" exportImageAsCode :: Ptr Image -> CString -> IO CInt



foreign import ccall "raylib.h &ExportImageAsCode"
  p'exportImageAsCode ::
    FunPtr (Image -> CString -> IO CInt)

foreign import ccall safe "bindings.h GenImageColor" genImageColor :: CInt -> CInt -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageColor"
  p'genImageColor ::
    FunPtr (CInt -> CInt -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageGradientV" genImageGradientV :: CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageGradientV"
  p'genImageGradientV ::
    FunPtr (CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageGradientH" genImageGradientH :: CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageGradientH"
  p'genImageGradientH ::
    FunPtr (CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageGradientRadial" genImageGradientRadial :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageGradientRadial"
  p'genImageGradientRadial ::
    FunPtr (CInt -> CInt -> CFloat -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageChecked" genImageChecked :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageChecked"
  p'genImageChecked ::
    FunPtr (CInt -> CInt -> CInt -> CInt -> Color -> Color -> IO Image)

foreign import ccall safe "bindings.h GenImageWhiteNoise" genImageWhiteNoise :: CInt -> CInt -> CFloat -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageWhiteNoise"
  p'genImageWhiteNoise ::
    FunPtr (CInt -> CInt -> CFloat -> IO Image)

foreign import ccall safe "bindings.h GenImageCellular" genImageCellular :: CInt -> CInt -> CInt -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageCellular"
  p'genImageCellular ::
    FunPtr (CInt -> CInt -> CInt -> IO Image)

foreign import ccall safe "bindings.h ImageCopy" imageCopy :: Ptr Image -> IO (Ptr Image)



foreign import ccall "raylib.h &ImageCopy"
  p'imageCopy ::
    FunPtr (Image -> IO Image)

foreign import ccall safe "bindings.h ImageFromImage" imageFromImage :: Ptr Image -> Ptr Rectangle -> IO (Ptr Image)



foreign import ccall "raylib.h &ImageFromImage"
  p'imageFromImage ::
    FunPtr (Image -> Rectangle -> IO Image)

foreign import ccall safe "bindings.h ImageText" imageText :: CString -> CInt -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &ImageText"
  p'imageText ::
    FunPtr (CString -> CInt -> Color -> IO Image)

foreign import ccall safe "bindings.h ImageTextEx" imageTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> Ptr Color -> IO (Ptr Image)



foreign import ccall "raylib.h &ImageTextEx"
  p'imageTextEx ::
    FunPtr (Font -> CString -> CFloat -> CFloat -> Color -> IO Image)

foreign import ccall "raylib.h ImageFormat"
  imageFormat ::
    Ptr Image -> CInt -> IO ()

foreign import ccall "raylib.h &ImageFormat"
  p'imageFormat ::
    FunPtr (Ptr Image -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageToPOT" imageToPOT :: Ptr Image -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageToPOT"
  p'imageToPOT ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall safe "bindings.h ImageCrop" imageCrop :: Ptr Image -> Ptr Rectangle -> IO ()



foreign import ccall "raylib.h &ImageCrop"
  p'imageCrop ::
    FunPtr (Ptr Image -> Rectangle -> IO ())

foreign import ccall "raylib.h ImageAlphaCrop"
  imageAlphaCrop ::
    Ptr Image -> CFloat -> IO ()

foreign import ccall "raylib.h &ImageAlphaCrop"
  p'imageAlphaCrop ::
    FunPtr (Ptr Image -> CFloat -> IO ())

foreign import ccall safe "bindings.h ImageAlphaClear" imageAlphaClear :: Ptr Image -> Ptr Color -> CFloat -> IO ()



foreign import ccall "raylib.h &ImageAlphaClear"
  p'imageAlphaClear ::
    FunPtr (Ptr Image -> Color -> CFloat -> IO ())

foreign import ccall safe "bindings.h ImageAlphaMask" imageAlphaMask :: Ptr Image -> Ptr Image -> IO ()



foreign import ccall "raylib.h &ImageAlphaMask"
  p'imageAlphaMask ::
    FunPtr (Ptr Image -> Image -> IO ())

foreign import ccall "raylib.h ImageAlphaPremultiply"
  imageAlphaPremultiply ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageAlphaPremultiply"
  p'imageAlphaPremultiply ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageResize"
  imageResize ::
    Ptr Image -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &ImageResize"
  p'imageResize ::
    FunPtr (Ptr Image -> CInt -> CInt -> IO ())

foreign import ccall "raylib.h ImageResizeNN"
  imageResizeNN ::
    Ptr Image -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &ImageResizeNN"
  p'imageResizeNN ::
    FunPtr (Ptr Image -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageResizeCanvas" imageResizeCanvas :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageResizeCanvas"
  p'imageResizeCanvas ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall "raylib.h ImageMipmaps"
  imageMipmaps ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageMipmaps"
  p'imageMipmaps ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageDither"
  imageDither ::
    Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &ImageDither"
  p'imageDither ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> IO ())

foreign import ccall "raylib.h ImageFlipVertical"
  imageFlipVertical ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageFlipVertical"
  p'imageFlipVertical ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageFlipHorizontal"
  imageFlipHorizontal ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageFlipHorizontal"
  p'imageFlipHorizontal ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageRotateCW"
  imageRotateCW ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageRotateCW"
  p'imageRotateCW ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageRotateCCW"
  imageRotateCCW ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageRotateCCW"
  p'imageRotateCCW ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall safe "bindings.h ImageColorTint" imageColorTint :: Ptr Image -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageColorTint"
  p'imageColorTint ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall "raylib.h ImageColorInvert"
  imageColorInvert ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageColorInvert"
  p'imageColorInvert ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageColorGrayscale"
  imageColorGrayscale ::
    Ptr Image -> IO ()

foreign import ccall "raylib.h &ImageColorGrayscale"
  p'imageColorGrayscale ::
    FunPtr (Ptr Image -> IO ())

foreign import ccall "raylib.h ImageColorContrast"
  imageColorContrast ::
    Ptr Image -> CFloat -> IO ()

foreign import ccall "raylib.h &ImageColorContrast"
  p'imageColorContrast ::
    FunPtr (Ptr Image -> CFloat -> IO ())

foreign import ccall "raylib.h ImageColorBrightness"
  imageColorBrightness ::
    Ptr Image -> CInt -> IO ()

foreign import ccall "raylib.h &ImageColorBrightness"
  p'imageColorBrightness ::
    FunPtr (Ptr Image -> CInt -> IO ())

foreign import ccall safe "bindings.h ImageColorReplace" imageColorReplace :: Ptr Image -> Ptr Color -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageColorReplace"
  p'imageColorReplace ::
    FunPtr (Ptr Image -> Color -> Color -> IO ())

foreign import ccall safe "bindings.h LoadImageColors" loadImageColors :: Ptr Image -> IO (Ptr Color)



foreign import ccall "raylib.h &LoadImageColors"
  p'loadImageColors ::
    FunPtr (Image -> IO (Ptr Color))

foreign import ccall safe "bindings.h LoadImagePalette" loadImagePalette :: Ptr Image -> CInt -> Ptr CInt -> IO (Ptr Color)



foreign import ccall "raylib.h &LoadImagePalette"
  p'loadImagePalette ::
    FunPtr (Image -> CInt -> Ptr CInt -> IO (Ptr Color))

foreign import ccall "raylib.h UnloadImageColors"
  unloadImageColors ::
    Ptr Color -> IO ()

foreign import ccall "raylib.h &UnloadImageColors"
  p'unloadImageColors ::
    FunPtr (Ptr Color -> IO ())

foreign import ccall "raylib.h UnloadImagePalette"
  unloadImagePalette ::
    Ptr Color -> IO ()

foreign import ccall "raylib.h &UnloadImagePalette"
  p'unloadImagePalette ::
    FunPtr (Ptr Color -> IO ())

foreign import ccall safe "bindings.h GetImageAlphaBorder" getImageAlphaBorder :: Ptr Image -> CFloat -> IO (Ptr Rectangle)



foreign import ccall "raylib.h &GetImageAlphaBorder"
  p'getImageAlphaBorder ::
    FunPtr (Image -> CFloat -> IO Rectangle)

foreign import ccall safe "bindings.h GetImageColor" getImageColor :: Ptr Image -> CInt -> CInt -> IO (Ptr Color)



foreign import ccall "raylib.h &GetImageColor"
  p'getImageColor ::
    FunPtr (Image -> CInt -> CInt -> IO Color)

foreign import ccall safe "bindings.h ImageClearBackground" imageClearBackground :: Ptr Image -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageClearBackground"
  p'imageClearBackground ::
    FunPtr (Ptr Image -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawPixel" imageDrawPixel :: Ptr Image -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawPixel"
  p'imageDrawPixel ::
    FunPtr (Ptr Image -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawPixelV" imageDrawPixelV :: Ptr Image -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawPixelV"
  p'imageDrawPixelV ::
    FunPtr (Ptr Image -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawLine" imageDrawLine :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawLine"
  p'imageDrawLine ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawLineV" imageDrawLineV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawLineV"
  p'imageDrawLineV ::
    FunPtr (Ptr Image -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircle" imageDrawCircle :: Ptr Image -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawCircle"
  p'imageDrawCircle ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawCircleV" imageDrawCircleV :: Ptr Image -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawCircleV"
  p'imageDrawCircleV ::
    FunPtr (Ptr Image -> Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangle" imageDrawRectangle :: Ptr Image -> CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawRectangle"
  p'imageDrawRectangle ::
    FunPtr (Ptr Image -> CInt -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleV" imageDrawRectangleV :: Ptr Image -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawRectangleV"
  p'imageDrawRectangleV ::
    FunPtr (Ptr Image -> Vector2 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleRec" imageDrawRectangleRec :: Ptr Image -> Ptr Rectangle -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawRectangleRec"
  p'imageDrawRectangleRec ::
    FunPtr (Ptr Image -> Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawRectangleLines" imageDrawRectangleLines :: Ptr Image -> Ptr Rectangle -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawRectangleLines"
  p'imageDrawRectangleLines ::
    FunPtr (Ptr Image -> Rectangle -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDraw" imageDraw :: Ptr Image -> Ptr Image -> Ptr Rectangle -> Ptr Rectangle -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDraw"
  p'imageDraw ::
    FunPtr (Ptr Image -> Image -> Rectangle -> Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawText" imageDrawText :: Ptr Image -> CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawText"
  p'imageDrawText ::
    FunPtr (Ptr Image -> CString -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h ImageDrawTextEx" imageDrawTextEx :: Ptr Image -> Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &ImageDrawTextEx"
  p'imageDrawTextEx ::
    FunPtr (Ptr Image -> Font -> CString -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h LoadTexture" loadTexture :: CString -> IO (Ptr Texture)



foreign import ccall "raylib.h &LoadTexture"
  p'loadTexture ::
    FunPtr (CString -> IO Texture)

foreign import ccall safe "bindings.h LoadTextureFromImage" loadTextureFromImage :: Ptr Image -> IO (Ptr Texture)



foreign import ccall "raylib.h &LoadTextureFromImage"
  p'loadTextureFromImage ::
    FunPtr (Image -> IO Texture)

foreign import ccall safe "bindings.h LoadTextureCubemap" loadTextureCubemap :: Ptr Image -> CInt -> IO (Ptr Texture)



foreign import ccall "raylib.h &LoadTextureCubemap"
  p'loadTextureCubemap ::
    FunPtr (Image -> CInt -> IO Texture)

foreign import ccall safe "bindings.h LoadRenderTexture" loadRenderTexture :: CInt -> CInt -> IO (Ptr RenderTexture)



foreign import ccall "raylib.h &LoadRenderTexture"
  p'loadRenderTexture ::
    FunPtr (CInt -> CInt -> IO RenderTexture)

foreign import ccall safe "bindings.h UnloadTexture" unloadTexture :: Ptr Texture -> IO ()



foreign import ccall "raylib.h &UnloadTexture"
  p'unloadTexture ::
    FunPtr (Texture -> IO ())

foreign import ccall safe "bindings.h UnloadRenderTexture" unloadRenderTexture :: Ptr RenderTexture -> IO ()



foreign import ccall "raylib.h &UnloadRenderTexture"
  p'unloadRenderTexture ::
    FunPtr (RenderTexture -> IO ())

foreign import ccall safe "bindings.h UpdateTexture" updateTexture :: Ptr Texture -> Ptr () -> IO ()



foreign import ccall "raylib.h &UpdateTexture"
  p'updateTexture ::
    FunPtr (Texture -> Ptr () -> IO ())

foreign import ccall safe "bindings.h UpdateTextureRec" updateTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr () -> IO ()



foreign import ccall "raylib.h &UpdateTextureRec"
  p'updateTextureRec ::
    FunPtr (Texture -> Rectangle -> Ptr () -> IO ())

foreign import ccall "raylib.h GenTextureMipmaps"
  genTextureMipmaps ::
    Ptr Texture -> IO ()

foreign import ccall "raylib.h &GenTextureMipmaps"
  p'genTextureMipmaps ::
    FunPtr (Ptr Texture -> IO ())

foreign import ccall safe "bindings.h SetTextureFilter" setTextureFilter :: Ptr Texture -> CInt -> IO ()



foreign import ccall "raylib.h &SetTextureFilter"
  p'setTextureFilter ::
    FunPtr (Texture -> CInt -> IO ())

foreign import ccall safe "bindings.h SetTextureWrap" setTextureWrap :: Ptr Texture -> CInt -> IO ()



foreign import ccall "raylib.h &SetTextureWrap"
  p'setTextureWrap ::
    FunPtr (Texture -> CInt -> IO ())

foreign import ccall safe "bindings.h DrawTexture" drawTexture :: Ptr Texture -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTexture"
  p'drawTexture ::
    FunPtr (Texture -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureV" drawTextureV :: Ptr Texture -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextureV"
  p'drawTextureV ::
    FunPtr (Texture -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureEx" drawTextureEx :: Ptr Texture -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextureEx"
  p'drawTextureEx ::
    FunPtr (Texture -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureRec" drawTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextureRec"
  p'drawTextureRec ::
    FunPtr (Texture -> Rectangle -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureQuad" drawTextureQuad :: Ptr Texture -> Ptr Vector2 -> Ptr Vector2 -> Ptr Rectangle -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextureQuad"
  p'drawTextureQuad ::
    FunPtr (Texture -> Vector2 -> Vector2 -> Rectangle -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureTiled" drawTextureTiled :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextureTiled"
  p'drawTextureTiled ::
    FunPtr (Texture -> Rectangle -> Rectangle -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTexturePro" drawTexturePro :: Ptr Texture -> Ptr Rectangle -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTexturePro"
  p'drawTexturePro ::
    FunPtr (Texture -> Rectangle -> Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextureNPatch" drawTextureNPatch :: Ptr Texture -> Ptr NPatchInfo -> Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextureNPatch"
  p'drawTextureNPatch ::
    FunPtr (Texture -> NPatchInfo -> Rectangle -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTexturePoly" drawTexturePoly :: Ptr Texture -> Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTexturePoly"
  p'drawTexturePoly ::
    FunPtr (Texture -> Vector2 -> Ptr Vector2 -> Ptr Vector2 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h Fade" fade :: Ptr Color -> CFloat -> IO (Ptr Color)



foreign import ccall "raylib.h &Fade"
  p'fade ::
    FunPtr (Color -> CFloat -> IO Color)

foreign import ccall safe "bindings.h ColorToInt" colorToInt :: Ptr Color -> IO CInt



foreign import ccall "raylib.h &ColorToInt"
  p'colorToInt ::
    FunPtr (Color -> IO CInt)

foreign import ccall safe "bindings.h ColorNormalize" colorNormalize :: Ptr Color -> IO (Ptr Vector4)



foreign import ccall "raylib.h &ColorNormalize"
  p'colorNormalize ::
    FunPtr (Color -> IO Vector4)

foreign import ccall safe "bindings.h ColorFromNormalized" colorFromNormalized :: Ptr Vector4 -> IO (Ptr Color)



foreign import ccall "raylib.h &ColorFromNormalized"
  p'colorFromNormalized ::
    FunPtr (Vector4 -> IO Color)

foreign import ccall safe "bindings.h ColorToHSV" colorToHSV :: Ptr Color -> IO (Ptr Vector3)



foreign import ccall "raylib.h &ColorToHSV"
  p'colorToHSV ::
    FunPtr (Color -> IO Vector3)

foreign import ccall safe "bindings.h ColorFromHSV" colorFromHSV :: CFloat -> CFloat -> CFloat -> IO (Ptr Color)



foreign import ccall "raylib.h &ColorFromHSV"
  p'colorFromHSV ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Color)

foreign import ccall safe "bindings.h ColorAlpha" colorAlpha :: Ptr Color -> CFloat -> IO (Ptr Color)



foreign import ccall "raylib.h &ColorAlpha"
  p'colorAlpha ::
    FunPtr (Color -> CFloat -> IO Color)

foreign import ccall safe "bindings.h ColorAlphaBlend" colorAlphaBlend :: Ptr Color -> Ptr Color -> Ptr Color -> IO (Ptr Color)



foreign import ccall "raylib.h &ColorAlphaBlend"
  p'colorAlphaBlend ::
    FunPtr (Color -> Color -> Color -> IO Color)

foreign import ccall safe "bindings.h GetColor" getColor :: CUInt -> IO (Ptr Color)



foreign import ccall "raylib.h &GetColor"
  p'getColor ::
    FunPtr (CUInt -> IO Color)

foreign import ccall safe "bindings.h GetPixelColor" getPixelColor :: Ptr () -> CInt -> IO (Ptr Color)



foreign import ccall "raylib.h &GetPixelColor"
  p'getPixelColor ::
    FunPtr (Ptr () -> CInt -> IO Color)

foreign import ccall safe "bindings.h SetPixelColor" setPixelColor :: Ptr () -> Ptr Color -> CInt -> IO ()



foreign import ccall "raylib.h &SetPixelColor"
  p'setPixelColor ::
    FunPtr (Ptr () -> Color -> CInt -> IO ())

foreign import ccall "raylib.h GetPixelDataSize"
  getPixelDataSize ::
    CInt -> CInt -> CInt -> IO CInt

foreign import ccall "raylib.h &GetPixelDataSize"
  p'getPixelDataSize ::
    FunPtr (CInt -> CInt -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetFontDefault" getFontDefault :: IO (Ptr Font)



foreign import ccall "raylib.h &GetFontDefault"
  p'getFontDefault ::
    FunPtr (IO Font)

foreign import ccall safe "bindings.h LoadFont" loadFont :: CString -> IO (Ptr Font)



foreign import ccall "raylib.h &LoadFont"
  p'loadFont ::
    FunPtr (CString -> IO Font)

foreign import ccall safe "bindings.h LoadFontEx" loadFontEx :: CString -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)



foreign import ccall "raylib.h &LoadFontEx"
  p'loadFontEx ::
    FunPtr (CString -> CInt -> Ptr CInt -> CInt -> IO Font)

foreign import ccall safe "bindings.h LoadFontFromImage" loadFontFromImage :: Ptr Image -> Ptr Color -> CInt -> IO (Ptr Font)



foreign import ccall "raylib.h &LoadFontFromImage"
  p'loadFontFromImage ::
    FunPtr (Image -> Color -> CInt -> IO Font)

foreign import ccall safe "bindings.h LoadFontFromMemory" loadFontFromMemory :: CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO (Ptr Font)



foreign import ccall "raylib.h &LoadFontFromMemory"
  p'loadFontFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> IO Font)

foreign import ccall "raylib.h LoadFontData"
  loadFontData ::
    Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo)

foreign import ccall "raylib.h &LoadFontData"
  p'loadFontData ::
    FunPtr (Ptr CUChar -> CInt -> CInt -> Ptr CInt -> CInt -> CInt -> IO (Ptr GlyphInfo))

foreign import ccall safe "bindings.h GenImageFontAtlas" genImageFontAtlas :: Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Image)



foreign import ccall "raylib.h &GenImageFontAtlas"
  p'genImageFontAtlas ::
    FunPtr (Ptr GlyphInfo -> Ptr (Ptr Rectangle) -> CInt -> CInt -> CInt -> CInt -> IO Image)

foreign import ccall "raylib.h UnloadFontData"
  unloadFontData ::
    Ptr GlyphInfo -> CInt -> IO ()

foreign import ccall "raylib.h &UnloadFontData"
  p'unloadFontData ::
    FunPtr (Ptr GlyphInfo -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadFont" unloadFont :: Ptr Font -> IO ()



foreign import ccall "raylib.h &UnloadFont"
  p'unloadFont ::
    FunPtr (Font -> IO ())

foreign import ccall safe "bindings.h ExportFontAsCode" exportFontAsCode :: Ptr Font -> CString -> IO CInt



foreign import ccall "raylib.h &ExportFontAsCode"
  p'exportFontAsCode ::
    FunPtr (Font -> CString -> IO CInt)

foreign import ccall "raylib.h DrawFPS"
  drawFPS ::
    CInt -> CInt -> IO ()

foreign import ccall "raylib.h &DrawFPS"
  p'drawFPS ::
    FunPtr (CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h DrawText" drawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawText"
  p'drawText ::
    FunPtr (CString -> CInt -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextEx" drawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextEx"
  p'drawTextEx ::
    FunPtr (Font -> CString -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextPro" drawTextPro :: Ptr Font -> CString -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextPro"
  p'drawTextPro ::
    FunPtr (Font -> CString -> Vector2 -> Vector2 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextCodepoint" drawTextCodepoint :: Ptr Font -> CInt -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextCodepoint"
  p'drawTextCodepoint ::
    FunPtr (Font -> CInt -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTextCodepoints" drawTextCodepoints :: Ptr Font -> Ptr CInt -> CInt -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTextCodepoints"
  p'drawTextCodepoints ::
    FunPtr (Font -> Ptr CInt -> CInt -> Vector2 -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall "raylib.h MeasureText"
  measureText ::
    CString -> CInt -> IO CInt

foreign import ccall "raylib.h &MeasureText"
  p'measureText ::
    FunPtr (CString -> CInt -> IO CInt)

foreign import ccall safe "bindings.h MeasureTextEx" measureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> IO (Ptr Vector2)



foreign import ccall "raylib.h &MeasureTextEx"
  p'measureTextEx ::
    FunPtr (Font -> CString -> CFloat -> CFloat -> IO Vector2)

foreign import ccall safe "bindings.h GetGlyphIndex" getGlyphIndex :: Ptr Font -> CInt -> IO CInt



foreign import ccall "raylib.h &GetGlyphIndex"
  p'getGlyphIndex ::
    FunPtr (Font -> CInt -> IO CInt)

foreign import ccall safe "bindings.h GetGlyphInfo" getGlyphInfo :: Ptr Font -> CInt -> IO (Ptr GlyphInfo)



foreign import ccall "raylib.h &GetGlyphInfo"
  p'getGlyphInfo ::
    FunPtr (Font -> CInt -> IO GlyphInfo)

foreign import ccall safe "bindings.h GetGlyphAtlasRec" getGlyphAtlasRec :: Ptr Font -> CInt -> IO (Ptr Rectangle)



foreign import ccall "raylib.h &GetGlyphAtlasRec"
  p'getGlyphAtlasRec ::
    FunPtr (Font -> CInt -> IO Rectangle)

foreign import ccall "raylib.h LoadCodepoints"
  loadCodepoints ::
    CString -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall "raylib.h &LoadCodepoints"
  p'loadCodepoints ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr CInt))

foreign import ccall "raylib.h UnloadCodepoints"
  unloadCodepoints ::
    Ptr CInt -> IO ()

foreign import ccall "raylib.h &UnloadCodepoints"
  p'unloadCodepoints ::
    FunPtr (Ptr CInt -> IO ())

foreign import ccall "raylib.h GetCodepointCount"
  getCodepointCount ::
    CString -> IO CInt

foreign import ccall "raylib.h &GetCodepointCount"
  p'getCodepointCount ::
    FunPtr (CString -> IO CInt)

foreign import ccall "raylib.h GetCodepoint"
  getCodepoint ::
    CString -> Ptr CInt -> IO CInt

foreign import ccall "raylib.h &GetCodepoint"
  p'getCodepoint ::
    FunPtr (CString -> Ptr CInt -> IO CInt)

foreign import ccall "raylib.h CodepointToUTF8"
  codepointToUTF8 ::
    CInt -> Ptr CInt -> IO CString

foreign import ccall "raylib.h &CodepointToUTF8"
  p'codepointToUTF8 ::
    FunPtr (CInt -> Ptr CInt -> IO CString)

foreign import ccall "raylib.h TextCodepointsToUTF8"
  textCodepointsToUTF8 ::
    Ptr CInt -> CInt -> IO CString

foreign import ccall "raylib.h &TextCodepointsToUTF8"
  p'textCodepointsToUTF8 ::
    FunPtr (Ptr CInt -> CInt -> IO CString)

foreign import ccall "raylib.h TextCopy"
  textCopy ::
    CString -> CString -> IO CInt

foreign import ccall "raylib.h &TextCopy"
  p'textCopy ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall "raylib.h TextIsEqual"
  textIsEqual ::
    CString -> CString -> IO CInt

foreign import ccall "raylib.h &TextIsEqual"
  p'textIsEqual ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall "raylib.h TextLength"
  textLength ::
    CString -> IO CUInt

foreign import ccall "raylib.h &TextLength"
  p'textLength ::
    FunPtr (CString -> IO CUInt)

foreign import ccall "raylib.h TextFormat"
  textFormat ::
    CString -> IO CString

foreign import ccall "raylib.h &TextFormat"
  p'textFormat ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h TextSubtext"
  textSubtext ::
    CString -> CInt -> CInt -> IO CString

foreign import ccall "raylib.h &TextSubtext"
  p'textSubtext ::
    FunPtr (CString -> CInt -> CInt -> IO CString)

foreign import ccall "raylib.h TextReplace"
  textReplace ::
    CString -> CString -> CString -> IO CString

foreign import ccall "raylib.h &TextReplace"
  p'textReplace ::
    FunPtr (CString -> CString -> CString -> IO CString)

foreign import ccall "raylib.h TextInsert"
  textInsert ::
    CString -> CString -> CInt -> IO CString

foreign import ccall "raylib.h &TextInsert"
  p'textInsert ::
    FunPtr (CString -> CString -> CInt -> IO CString)

foreign import ccall "raylib.h TextJoin"
  textJoin ::
    Ptr CString -> CInt -> CString -> IO CString

foreign import ccall "raylib.h &TextJoin"
  p'textJoin ::
    FunPtr (Ptr CString -> CInt -> CString -> IO CString)

foreign import ccall "raylib.h TextSplit"
  textSplit ::
    CString -> CChar -> Ptr CInt -> IO (Ptr CString)

foreign import ccall "raylib.h &TextSplit"
  p'textSplit ::
    FunPtr (CString -> CChar -> Ptr CInt -> IO (Ptr CString))

foreign import ccall "raylib.h TextAppend"
  textAppend ::
    CString -> CString -> Ptr CInt -> IO ()

foreign import ccall "raylib.h &TextAppend"
  p'textAppend ::
    FunPtr (CString -> CString -> Ptr CInt -> IO ())

foreign import ccall "raylib.h TextFindIndex"
  textFindIndex ::
    CString -> CString -> IO CInt

foreign import ccall "raylib.h &TextFindIndex"
  p'textFindIndex ::
    FunPtr (CString -> CString -> IO CInt)

foreign import ccall "raylib.h TextToUpper"
  textToUpper ::
    CString -> IO CString

foreign import ccall "raylib.h &TextToUpper"
  p'textToUpper ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h TextToLower"
  textToLower ::
    CString -> IO CString

foreign import ccall "raylib.h &TextToLower"
  p'textToLower ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h TextToPascal"
  textToPascal ::
    CString -> IO CString

foreign import ccall "raylib.h &TextToPascal"
  p'textToPascal ::
    FunPtr (CString -> IO CString)

foreign import ccall "raylib.h TextToInteger"
  textToInteger ::
    CString -> IO CInt

foreign import ccall "raylib.h &TextToInteger"
  p'textToInteger ::
    FunPtr (CString -> IO CInt)

foreign import ccall safe "bindings.h DrawLine3D" drawLine3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawLine3D"
  p'drawLine3D ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPoint3D" drawPoint3D :: Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPoint3D"
  p'drawPoint3D ::
    FunPtr (Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCircle3D" drawCircle3D :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCircle3D"
  p'drawCircle3D ::
    FunPtr (Vector3 -> CFloat -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangle3D" drawTriangle3D :: Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTriangle3D"
  p'drawTriangle3D ::
    FunPtr (Vector3 -> Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawTriangleStrip3D" drawTriangleStrip3D :: Ptr Vector3 -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawTriangleStrip3D"
  p'drawTriangleStrip3D ::
    FunPtr (Ptr Vector3 -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCube" drawCube :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCube"
  p'drawCube ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeV" drawCubeV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCubeV"
  p'drawCubeV ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeWires" drawCubeWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCubeWires"
  p'drawCubeWires ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeWiresV" drawCubeWiresV :: Ptr Vector3 -> Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCubeWiresV"
  p'drawCubeWiresV ::
    FunPtr (Vector3 -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeTexture" drawCubeTexture :: Ptr Texture -> Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCubeTexture"
  p'drawCubeTexture ::
    FunPtr (Texture -> Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCubeTextureRec" drawCubeTextureRec :: Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCubeTextureRec"
  p'drawCubeTextureRec ::
    FunPtr (Texture -> Rectangle -> Vector3 -> CFloat -> CFloat -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawSphere" drawSphere :: Ptr Vector3 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawSphere"
  p'drawSphere ::
    FunPtr (Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawSphereEx" drawSphereEx :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawSphereEx"
  p'drawSphereEx ::
    FunPtr (Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawSphereWires" drawSphereWires :: Ptr Vector3 -> CFloat -> CInt -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawSphereWires"
  p'drawSphereWires ::
    FunPtr (Vector3 -> CFloat -> CInt -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinder" drawCylinder :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCylinder"
  p'drawCylinder ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderEx" drawCylinderEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCylinderEx"
  p'drawCylinderEx ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderWires" drawCylinderWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCylinderWires"
  p'drawCylinderWires ::
    FunPtr (Vector3 -> CFloat -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawCylinderWiresEx" drawCylinderWiresEx :: Ptr Vector3 -> Ptr Vector3 -> CFloat -> CFloat -> CInt -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawCylinderWiresEx"
  p'drawCylinderWiresEx ::
    FunPtr (Vector3 -> Vector3 -> CFloat -> CFloat -> CInt -> Color -> IO ())

foreign import ccall safe "bindings.h DrawPlane" drawPlane :: Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawPlane"
  p'drawPlane ::
    FunPtr (Vector3 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawRay" drawRay :: Ptr Ray -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawRay"
  p'drawRay ::
    FunPtr (Ray -> Color -> IO ())

foreign import ccall "raylib.h DrawGrid"
  drawGrid ::
    CInt -> CFloat -> IO ()

foreign import ccall "raylib.h &DrawGrid"
  p'drawGrid ::
    FunPtr (CInt -> CFloat -> IO ())

foreign import ccall safe "bindings.h LoadModel" loadModel :: CString -> IO (Ptr Model)



foreign import ccall "raylib.h &LoadModel"
  p'loadModel ::
    FunPtr (CString -> IO Model)

foreign import ccall safe "bindings.h LoadModelFromMesh" loadModelFromMesh :: Ptr Mesh -> IO (Ptr Model)



foreign import ccall "raylib.h &LoadModelFromMesh"
  p'loadModelFromMesh ::
    FunPtr (Mesh -> IO Model)

foreign import ccall safe "bindings.h UnloadModel" unloadModel :: Ptr Model -> IO ()



foreign import ccall "raylib.h &UnloadModel"
  p'unloadModel ::
    FunPtr (Model -> IO ())

foreign import ccall safe "bindings.h UnloadModelKeepMeshes" unloadModelKeepMeshes :: Ptr Model -> IO ()



foreign import ccall "raylib.h &UnloadModelKeepMeshes"
  p'unloadModelKeepMeshes ::
    FunPtr (Model -> IO ())

foreign import ccall safe "bindings.h GetModelBoundingBox" getModelBoundingBox :: Ptr Model -> IO (Ptr BoundingBox)



foreign import ccall "raylib.h &GetModelBoundingBox"
  p'getModelBoundingBox ::
    FunPtr (Model -> IO BoundingBox)

foreign import ccall safe "bindings.h DrawModel" drawModel :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawModel"
  p'drawModel ::
    FunPtr (Model -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawModelEx" drawModelEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawModelEx"
  p'drawModelEx ::
    FunPtr (Model -> Vector3 -> Vector3 -> CFloat -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawModelWires" drawModelWires :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawModelWires"
  p'drawModelWires ::
    FunPtr (Model -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawModelWiresEx" drawModelWiresEx :: Ptr Model -> Ptr Vector3 -> Ptr Vector3 -> CFloat -> Ptr Vector3 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawModelWiresEx"
  p'drawModelWiresEx ::
    FunPtr (Model -> Vector3 -> Vector3 -> CFloat -> Vector3 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBoundingBox" drawBoundingBox :: Ptr BoundingBox -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawBoundingBox"
  p'drawBoundingBox ::
    FunPtr (BoundingBox -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboard" drawBillboard :: Ptr Camera3D -> Ptr Texture -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawBillboard"
  p'drawBillboard ::
    FunPtr (Camera3D -> Texture -> Vector3 -> CFloat -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboardRec" drawBillboardRec :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector2 -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawBillboardRec"
  p'drawBillboardRec ::
    FunPtr (Camera3D -> Texture -> Rectangle -> Vector3 -> Vector2 -> Color -> IO ())

foreign import ccall safe "bindings.h DrawBillboardPro" drawBillboardPro :: Ptr Camera3D -> Ptr Texture -> Ptr Rectangle -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()



foreign import ccall "raylib.h &DrawBillboardPro"
  p'drawBillboardPro ::
    FunPtr (Camera3D -> Texture -> Rectangle -> Vector3 -> Vector3 -> Vector2 -> Vector2 -> CFloat -> Color -> IO ())

foreign import ccall "raylib.h UploadMesh"
  uploadMesh ::
    Ptr Mesh -> CInt -> IO ()

foreign import ccall "raylib.h &UploadMesh"
  p'uploadMesh ::
    FunPtr (Ptr Mesh -> CInt -> IO ())

foreign import ccall safe "bindings.h UpdateMeshBuffer" updateMeshBuffer :: Ptr Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ()



foreign import ccall "raylib.h &UpdateMeshBuffer"
  p'updateMeshBuffer ::
    FunPtr (Mesh -> CInt -> Ptr () -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadMesh" unloadMesh :: Ptr Mesh -> IO ()



foreign import ccall "raylib.h &UnloadMesh"
  p'unloadMesh ::
    FunPtr (Mesh -> IO ())

foreign import ccall safe "bindings.h DrawMesh" drawMesh :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> IO ()



foreign import ccall "raylib.h &DrawMesh"
  p'drawMesh ::
    FunPtr (Mesh -> Material -> Matrix -> IO ())

foreign import ccall safe "bindings.h DrawMeshInstanced" drawMeshInstanced :: Ptr Mesh -> Ptr Material -> Ptr Matrix -> CInt -> IO ()



foreign import ccall "raylib.h &DrawMeshInstanced"
  p'drawMeshInstanced ::
    FunPtr (Mesh -> Material -> Ptr Matrix -> CInt -> IO ())

foreign import ccall safe "bindings.h ExportMesh" exportMesh :: Ptr Mesh -> CString -> IO CInt



foreign import ccall "raylib.h &ExportMesh"
  p'exportMesh ::
    FunPtr (Mesh -> CString -> IO CInt)

foreign import ccall safe "bindings.h GetMeshBoundingBox" getMeshBoundingBox :: Ptr Mesh -> IO (Ptr BoundingBox)



foreign import ccall "raylib.h &GetMeshBoundingBox"
  p'getMeshBoundingBox ::
    FunPtr (Mesh -> IO BoundingBox)

foreign import ccall "raylib.h GenMeshTangents"
  genMeshTangents ::
    Ptr Mesh -> IO ()

foreign import ccall "raylib.h &GenMeshTangents"
  p'genMeshTangents ::
    FunPtr (Ptr Mesh -> IO ())

foreign import ccall safe "bindings.h GenMeshPoly" genMeshPoly :: CInt -> CFloat -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshPoly"
  p'genMeshPoly ::
    FunPtr (CInt -> CFloat -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshPlane" genMeshPlane :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshPlane"
  p'genMeshPlane ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCube" genMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshCube"
  p'genMeshCube ::
    FunPtr (CFloat -> CFloat -> CFloat -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshSphere" genMeshSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshSphere"
  p'genMeshSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshHemiSphere" genMeshHemiSphere :: CFloat -> CInt -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshHemiSphere"
  p'genMeshHemiSphere ::
    FunPtr (CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCylinder" genMeshCylinder :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshCylinder"
  p'genMeshCylinder ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCone" genMeshCone :: CFloat -> CFloat -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshCone"
  p'genMeshCone ::
    FunPtr (CFloat -> CFloat -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshTorus" genMeshTorus :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshTorus"
  p'genMeshTorus ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshKnot" genMeshKnot :: CFloat -> CFloat -> CInt -> CInt -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshKnot"
  p'genMeshKnot ::
    FunPtr (CFloat -> CFloat -> CInt -> CInt -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshHeightmap" genMeshHeightmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshHeightmap"
  p'genMeshHeightmap ::
    FunPtr (Image -> Vector3 -> IO Mesh)

foreign import ccall safe "bindings.h GenMeshCubicmap" genMeshCubicmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)



foreign import ccall "raylib.h &GenMeshCubicmap"
  p'genMeshCubicmap ::
    FunPtr (Image -> Vector3 -> IO Mesh)

foreign import ccall "raylib.h LoadMaterials"
  loadMaterials ::
    CString -> Ptr CInt -> IO (Ptr Material)

foreign import ccall "raylib.h &LoadMaterials"
  p'loadMaterials ::
    FunPtr (CString -> Ptr CInt -> IO (Ptr Material))

foreign import ccall safe "bindings.h LoadMaterialDefault" loadMaterialDefault :: IO (Ptr Material)



foreign import ccall "raylib.h &LoadMaterialDefault"
  p'loadMaterialDefault ::
    FunPtr (IO Material)

foreign import ccall safe "bindings.h UnloadMaterial" unloadMaterial :: Ptr Material -> IO ()



foreign import ccall "raylib.h &UnloadMaterial"
  p'unloadMaterial ::
    FunPtr (Material -> IO ())

foreign import ccall safe "bindings.h SetMaterialTexture" setMaterialTexture :: Ptr Material -> CInt -> Ptr Texture -> IO ()



foreign import ccall "raylib.h &SetMaterialTexture"
  p'setMaterialTexture ::
    FunPtr (Ptr Material -> CInt -> Texture -> IO ())

foreign import ccall "raylib.h SetModelMeshMaterial"
  setModelMeshMaterial ::
    Ptr Model -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &SetModelMeshMaterial"
  p'setModelMeshMaterial ::
    FunPtr (Ptr Model -> CInt -> CInt -> IO ())

foreign import ccall "raylib.h LoadModelAnimations"
  loadModelAnimations ::
    CString -> Ptr CUInt -> IO (Ptr ModelAnimation)

foreign import ccall "raylib.h &LoadModelAnimations"
  p'loadModelAnimations ::
    FunPtr (CString -> Ptr CUInt -> IO (Ptr ModelAnimation))

foreign import ccall safe "bindings.h UpdateModelAnimation" updateModelAnimation :: Ptr Model -> Ptr ModelAnimation -> CInt -> IO ()



foreign import ccall "raylib.h &UpdateModelAnimation"
  p'updateModelAnimation ::
    FunPtr (Model -> ModelAnimation -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadModelAnimation" unloadModelAnimation :: Ptr ModelAnimation -> IO ()



foreign import ccall "raylib.h &UnloadModelAnimation"
  p'unloadModelAnimation ::
    FunPtr (ModelAnimation -> IO ())

foreign import ccall "raylib.h UnloadModelAnimations"
  unloadModelAnimations ::
    Ptr ModelAnimation -> CUInt -> IO ()

foreign import ccall "raylib.h &UnloadModelAnimations"
  p'unloadModelAnimations ::
    FunPtr (Ptr ModelAnimation -> CUInt -> IO ())

foreign import ccall safe "bindings.h IsModelAnimationValid" isModelAnimationValid :: Ptr Model -> Ptr ModelAnimation -> IO CInt



foreign import ccall "raylib.h &IsModelAnimationValid"
  p'isModelAnimationValid ::
    FunPtr (Model -> ModelAnimation -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionSpheres" checkCollisionSpheres :: Ptr Vector3 -> CFloat -> Ptr Vector3 -> CFloat -> IO CInt



foreign import ccall "raylib.h &CheckCollisionSpheres"
  p'checkCollisionSpheres ::
    FunPtr (Vector3 -> CFloat -> Vector3 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionBoxes" checkCollisionBoxes :: Ptr BoundingBox -> Ptr BoundingBox -> IO CInt



foreign import ccall "raylib.h &CheckCollisionBoxes"
  p'checkCollisionBoxes ::
    FunPtr (BoundingBox -> BoundingBox -> IO CInt)

foreign import ccall safe "bindings.h CheckCollisionBoxSphere" checkCollisionBoxSphere :: Ptr BoundingBox -> Ptr Vector3 -> CFloat -> IO CInt



foreign import ccall "raylib.h &CheckCollisionBoxSphere"
  p'checkCollisionBoxSphere ::
    FunPtr (BoundingBox -> Vector3 -> CFloat -> IO CInt)

foreign import ccall safe "bindings.h GetRayCollisionSphere" getRayCollisionSphere :: Ptr Ray -> Ptr Vector3 -> CFloat -> IO (Ptr RayCollision)



foreign import ccall "raylib.h &GetRayCollisionSphere"
  p'getRayCollisionSphere ::
    FunPtr (Ray -> Vector3 -> CFloat -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionBox" getRayCollisionBox :: Ptr Ray -> Ptr BoundingBox -> IO (Ptr RayCollision)



foreign import ccall "raylib.h &GetRayCollisionBox"
  p'getRayCollisionBox ::
    FunPtr (Ray -> BoundingBox -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionMesh" getRayCollisionMesh :: Ptr Ray -> Ptr Mesh -> Ptr Matrix -> IO (Ptr RayCollision)



foreign import ccall "raylib.h &GetRayCollisionMesh"
  p'getRayCollisionMesh ::
    FunPtr (Ray -> Mesh -> Matrix -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionTriangle" getRayCollisionTriangle :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)



foreign import ccall "raylib.h &GetRayCollisionTriangle"
  p'getRayCollisionTriangle ::
    FunPtr (Ray -> Vector3 -> Vector3 -> Vector3 -> IO RayCollision)

foreign import ccall safe "bindings.h GetRayCollisionQuad" getRayCollisionQuad :: Ptr Ray -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> Ptr Vector3 -> IO (Ptr RayCollision)



foreign import ccall "raylib.h &GetRayCollisionQuad"
  p'getRayCollisionQuad ::
    FunPtr (Ray -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> IO RayCollision)

type AudioCallback = FunPtr (Ptr () -> CUInt -> IO ())

foreign import ccall "wrapper"
  mk'audioCallback ::
    (Ptr () -> CUInt -> IO ()) -> IO AudioCallback

foreign import ccall "dynamic"
  mK'audioCallback ::
    AudioCallback -> (Ptr () -> CUInt -> IO ())

foreign import ccall "raylib.h InitAudioDevice"
  initAudioDevice ::
    IO ()

foreign import ccall "raylib.h &InitAudioDevice"
  p'initAudioDevice ::
    FunPtr (IO ())

foreign import ccall "raylib.h CloseAudioDevice"
  closeAudioDevice ::
    IO ()

foreign import ccall "raylib.h &CloseAudioDevice"
  p'closeAudioDevice ::
    FunPtr (IO ())

foreign import ccall "raylib.h IsAudioDeviceReady"
  isAudioDeviceReady ::
    IO CInt

foreign import ccall "raylib.h &IsAudioDeviceReady"
  p'isAudioDeviceReady ::
    FunPtr (IO CInt)

foreign import ccall "raylib.h SetMasterVolume"
  setMasterVolume ::
    CFloat -> IO ()

foreign import ccall "raylib.h &SetMasterVolume"
  p'setMasterVolume ::
    FunPtr (CFloat -> IO ())

foreign import ccall safe "bindings.h LoadWave" loadWave :: CString -> IO (Ptr Wave)



foreign import ccall "raylib.h &LoadWave"
  p'loadWave ::
    FunPtr (CString -> IO Wave)

foreign import ccall safe "bindings.h LoadWaveFromMemory" loadWaveFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Wave)



foreign import ccall "raylib.h &LoadWaveFromMemory"
  p'loadWaveFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Wave)

foreign import ccall safe "bindings.h LoadSound" loadSound :: CString -> IO (Ptr Sound)



foreign import ccall "raylib.h &LoadSound"
  p'loadSound ::
    FunPtr (CString -> IO Sound)

foreign import ccall safe "bindings.h LoadSoundFromWave" loadSoundFromWave :: Ptr Wave -> IO (Ptr Sound)



foreign import ccall "raylib.h &LoadSoundFromWave"
  p'loadSoundFromWave ::
    FunPtr (Wave -> IO Sound)

foreign import ccall safe "bindings.h UpdateSound" updateSound :: Ptr Sound -> Ptr () -> CInt -> IO ()



foreign import ccall "raylib.h &UpdateSound"
  p'updateSound ::
    FunPtr (Sound -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h UnloadWave" unloadWave :: Ptr Wave -> IO ()



foreign import ccall "raylib.h &UnloadWave"
  p'unloadWave ::
    FunPtr (Wave -> IO ())

foreign import ccall safe "bindings.h UnloadSound" unloadSound :: Ptr Sound -> IO ()



foreign import ccall "raylib.h &UnloadSound"
  p'unloadSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h ExportWave" exportWave :: Ptr Wave -> CString -> IO CInt



foreign import ccall "raylib.h &ExportWave"
  p'exportWave ::
    FunPtr (Wave -> CString -> IO CInt)

foreign import ccall safe "bindings.h ExportWaveAsCode" exportWaveAsCode :: Ptr Wave -> CString -> IO CInt



foreign import ccall "raylib.h &ExportWaveAsCode"
  p'exportWaveAsCode ::
    FunPtr (Wave -> CString -> IO CInt)

foreign import ccall safe "bindings.h PlaySound" playSound :: Ptr Sound -> IO ()



foreign import ccall "raylib.h &PlaySound"
  p'playSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h StopSound" stopSound :: Ptr Sound -> IO ()



foreign import ccall "raylib.h &StopSound"
  p'stopSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h PauseSound" pauseSound :: Ptr Sound -> IO ()



foreign import ccall "raylib.h &PauseSound"
  p'pauseSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h ResumeSound" resumeSound :: Ptr Sound -> IO ()



foreign import ccall "raylib.h &ResumeSound"
  p'resumeSound ::
    FunPtr (Sound -> IO ())

foreign import ccall safe "bindings.h PlaySoundMulti" playSoundMulti :: Ptr Sound -> IO ()



foreign import ccall "raylib.h &PlaySoundMulti"
  p'playSoundMulti ::
    FunPtr (Sound -> IO ())

foreign import ccall "raylib.h StopSoundMulti"
  stopSoundMulti ::
    IO ()

foreign import ccall "raylib.h &StopSoundMulti"
  p'stopSoundMulti ::
    FunPtr (IO ())

foreign import ccall "raylib.h GetSoundsPlaying"
  getSoundsPlaying ::
    IO CInt

foreign import ccall "raylib.h &GetSoundsPlaying"
  p'getSoundsPlaying ::
    FunPtr (IO CInt)

foreign import ccall safe "bindings.h IsSoundPlaying" isSoundPlaying :: Ptr Sound -> IO CInt



foreign import ccall "raylib.h &IsSoundPlaying"
  p'isSoundPlaying ::
    FunPtr (Sound -> IO CInt)

foreign import ccall safe "bindings.h SetSoundVolume" setSoundVolume :: Ptr Sound -> CFloat -> IO ()



foreign import ccall "raylib.h &SetSoundVolume"
  p'setSoundVolume ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetSoundPitch" setSoundPitch :: Ptr Sound -> CFloat -> IO ()



foreign import ccall "raylib.h &SetSoundPitch"
  p'setSoundPitch ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetSoundPan" setSoundPan :: Ptr Sound -> CFloat -> IO ()



foreign import ccall "raylib.h &SetSoundPan"
  p'setSoundPan ::
    FunPtr (Sound -> CFloat -> IO ())

foreign import ccall safe "bindings.h WaveCopy" waveCopy :: Ptr Wave -> IO (Ptr Wave)



foreign import ccall "raylib.h &WaveCopy"
  p'waveCopy ::
    FunPtr (Wave -> IO Wave)

foreign import ccall "raylib.h WaveCrop"
  waveCrop ::
    Ptr Wave -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &WaveCrop"
  p'waveCrop ::
    FunPtr (Ptr Wave -> CInt -> CInt -> IO ())

foreign import ccall "raylib.h WaveFormat"
  waveFormat ::
    Ptr Wave -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "raylib.h &WaveFormat"
  p'waveFormat ::
    FunPtr (Ptr Wave -> CInt -> CInt -> CInt -> IO ())

foreign import ccall safe "bindings.h LoadWaveSamples" loadWaveSamples :: Ptr Wave -> IO (Ptr CFloat)



foreign import ccall "raylib.h &LoadWaveSamples"
  p'loadWaveSamples ::
    FunPtr (Wave -> IO (Ptr CFloat))

foreign import ccall "raylib.h UnloadWaveSamples"
  unloadWaveSamples ::
    Ptr CFloat -> IO ()

foreign import ccall "raylib.h &UnloadWaveSamples"
  p'unloadWaveSamples ::
    FunPtr (Ptr CFloat -> IO ())

foreign import ccall safe "bindings.h LoadMusicStream" loadMusicStream :: CString -> IO (Ptr Music)



foreign import ccall "raylib.h &LoadMusicStream"
  p'loadMusicStream ::
    FunPtr (CString -> IO Music)

foreign import ccall safe "bindings.h LoadMusicStreamFromMemory" loadMusicStreamFromMemory :: CString -> Ptr CUChar -> CInt -> IO (Ptr Music)



foreign import ccall "raylib.h &LoadMusicStreamFromMemory"
  p'loadMusicStreamFromMemory ::
    FunPtr (CString -> Ptr CUChar -> CInt -> IO Music)

foreign import ccall safe "bindings.h UnloadMusicStream" unloadMusicStream :: Ptr Music -> IO ()



foreign import ccall "raylib.h &UnloadMusicStream"
  p'unloadMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h PlayMusicStream" playMusicStream :: Ptr Music -> IO ()



foreign import ccall "raylib.h &PlayMusicStream"
  p'playMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h IsMusicStreamPlaying" isMusicStreamPlaying :: Ptr Music -> IO CInt



foreign import ccall "raylib.h &IsMusicStreamPlaying"
  p'isMusicStreamPlaying ::
    FunPtr (Music -> IO CInt)

foreign import ccall safe "bindings.h UpdateMusicStream" updateMusicStream :: Ptr Music -> IO ()



foreign import ccall "raylib.h &UpdateMusicStream"
  p'updateMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h StopMusicStream" stopMusicStream :: Ptr Music -> IO ()



foreign import ccall "raylib.h &StopMusicStream"
  p'stopMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h PauseMusicStream" pauseMusicStream :: Ptr Music -> IO ()



foreign import ccall "raylib.h &PauseMusicStream"
  p'pauseMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h ResumeMusicStream" resumeMusicStream :: Ptr Music -> IO ()



foreign import ccall "raylib.h &ResumeMusicStream"
  p'resumeMusicStream ::
    FunPtr (Music -> IO ())

foreign import ccall safe "bindings.h SeekMusicStream" seekMusicStream :: Ptr Music -> CFloat -> IO ()



foreign import ccall "raylib.h &SeekMusicStream"
  p'seekMusicStream ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicVolume" setMusicVolume :: Ptr Music -> CFloat -> IO ()



foreign import ccall "raylib.h &SetMusicVolume"
  p'setMusicVolume ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicPitch" setMusicPitch :: Ptr Music -> CFloat -> IO ()



foreign import ccall "raylib.h &SetMusicPitch"
  p'setMusicPitch ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetMusicPan" setMusicPan :: Ptr Music -> CFloat -> IO ()



foreign import ccall "raylib.h &SetMusicPan"
  p'setMusicPan ::
    FunPtr (Music -> CFloat -> IO ())

foreign import ccall safe "bindings.h GetMusicTimeLength" getMusicTimeLength :: Ptr Music -> IO CFloat



foreign import ccall "raylib.h &GetMusicTimeLength"
  p'getMusicTimeLength ::
    FunPtr (Music -> IO CFloat)

foreign import ccall safe "bindings.h GetMusicTimePlayed" getMusicTimePlayed :: Ptr Music -> IO CFloat



foreign import ccall "raylib.h &GetMusicTimePlayed"
  p'getMusicTimePlayed ::
    FunPtr (Music -> IO CFloat)

foreign import ccall safe "bindings.h LoadAudioStream" loadAudioStream :: CUInt -> CUInt -> CUInt -> IO (Ptr AudioStream)



foreign import ccall "raylib.h &LoadAudioStream"
  p'loadAudioStream ::
    FunPtr (CUInt -> CUInt -> CUInt -> IO AudioStream)

foreign import ccall safe "bindings.h UnloadAudioStream" unloadAudioStream :: Ptr AudioStream -> IO ()



foreign import ccall "raylib.h &UnloadAudioStream"
  p'unloadAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h UpdateAudioStream" updateAudioStream :: Ptr AudioStream -> Ptr () -> CInt -> IO ()



foreign import ccall "raylib.h &UpdateAudioStream"
  p'updateAudioStream ::
    FunPtr (AudioStream -> Ptr () -> CInt -> IO ())

foreign import ccall safe "bindings.h IsAudioStreamProcessed" isAudioStreamProcessed :: Ptr AudioStream -> IO CInt



foreign import ccall "raylib.h &IsAudioStreamProcessed"
  p'isAudioStreamProcessed ::
    FunPtr (AudioStream -> IO CInt)

foreign import ccall safe "bindings.h PlayAudioStream" playAudioStream :: Ptr AudioStream -> IO ()



foreign import ccall "raylib.h &PlayAudioStream"
  p'playAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h PauseAudioStream" pauseAudioStream :: Ptr AudioStream -> IO ()



foreign import ccall "raylib.h &PauseAudioStream"
  p'pauseAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h ResumeAudioStream" resumeAudioStream :: Ptr AudioStream -> IO ()



foreign import ccall "raylib.h &ResumeAudioStream"
  p'resumeAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h IsAudioStreamPlaying" isAudioStreamPlaying :: Ptr AudioStream -> IO CInt



foreign import ccall "raylib.h &IsAudioStreamPlaying"
  p'isAudioStreamPlaying ::
    FunPtr (AudioStream -> IO CInt)

foreign import ccall safe "bindings.h StopAudioStream" stopAudioStream :: Ptr AudioStream -> IO ()



foreign import ccall "raylib.h &StopAudioStream"
  p'stopAudioStream ::
    FunPtr (AudioStream -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamVolume" setAudioStreamVolume :: Ptr AudioStream -> CFloat -> IO ()



foreign import ccall "raylib.h &SetAudioStreamVolume"
  p'setAudioStreamVolume ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamPitch" setAudioStreamPitch :: Ptr AudioStream -> CFloat -> IO ()



foreign import ccall "raylib.h &SetAudioStreamPitch"
  p'setAudioStreamPitch ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamPan" setAudioStreamPan :: Ptr AudioStream -> CFloat -> IO ()



foreign import ccall "raylib.h &SetAudioStreamPan"
  p'setAudioStreamPan ::
    FunPtr (AudioStream -> CFloat -> IO ())

foreign import ccall "raylib.h SetAudioStreamBufferSizeDefault"
  setAudioStreamBufferSizeDefault ::
    CInt -> IO ()

foreign import ccall "raylib.h &SetAudioStreamBufferSizeDefault"
  p'setAudioStreamBufferSizeDefault ::
    FunPtr (CInt -> IO ())

foreign import ccall safe "bindings.h SetAudioStreamCallback" setAudioStreamCallback :: Ptr AudioStream -> Ptr AudioCallback -> IO ()



foreign import ccall "raylib.h &SetAudioStreamCallback"
  p'setAudioStreamCallback ::
    FunPtr (AudioStream -> AudioCallback -> IO ())

foreign import ccall safe "bindings.h AttachAudioStreamProcessor" attachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()



foreign import ccall "raylib.h &AttachAudioStreamProcessor"
  p'attachAudioStreamProcessor ::
    FunPtr (AudioStream -> AudioCallback -> IO ())

foreign import ccall safe "bindings.h DetachAudioStreamProcessor" detachAudioStreamProcessor :: Ptr AudioStream -> Ptr AudioCallback -> IO ()



foreign import ccall "raylib.h &DetachAudioStreamProcessor"
  p'detachAudioStreamProcessor ::
    FunPtr (AudioStream -> AudioCallback -> IO ())
