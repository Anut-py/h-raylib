{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS -Wall #-}

-- | Bindings for types used in @rlgl@
module Raylib.Types.Util.RLGL
  ( -- * Enumerations
    RLGLVersion (..),
    RLTraceLogLevel (..),
    RLPixelFormat (..),
    RLTextureFilter (..),
    RLBlendMode (..),
    RLShaderLocationIndex (..),
    RLShaderUniformDataType (..),
    RLShaderAttributeDataType (..),
    RLFramebufferAttachType (..),
    RLFramebufferAttachTextureType (..),
    RLCullMode (..),
    RLMatrixMode (..),
    RLDrawMode (..),
    RLTextureParam (..),
    RLShaderType (..),
    RLBufferHint (..),
    RLBitField (..),

    -- * Structures
    RLVertexBuffer (..),
    RLDrawCall (..),
    RLRenderBatch (..),

    -- * Pointer utilities
    p'rlVertexBuffer'elementCount,
    p'rlVertexBuffer'vertices,
    p'rlVertexBuffer'texcoords,
    p'rlVertexBuffer'colors,
    p'rlVertexBuffer'indices,
    p'rlVertexBuffer'vaoId,
    p'rlVertexBuffer'vboId,
    p'rlDrawCall'mode,
    p'rlDrawCall'vertexCount,
    p'rlDrawCall'vertexAlignment,
    p'rlDrawCall'textureId,
    p'rlRenderBatch'bufferCount,
    p'rlRenderBatch'currentBuffer,
    p'rlRenderBatch'vertexBuffers,
    p'rlRenderBatch'draws,
    p'rlRenderBatch'drawCounter,
    p'rlRenderBatch'currentDepth,
  )
where

import Foreign
  ( Ptr,
    Storable (alignment, peek, poke, sizeOf),
    castPtr,
    newArray,
    peekArray,
    plusPtr,
  )
import Foreign.C
  ( CFloat,
    CInt (..),
    CUInt,
  )
import Raylib.Internal.Foreign (Freeable (rlFreeDependents), c'free, peekStaticArray, pokeStaticArray, rlFreeArray)
import Raylib.Types.Core (Color, Vector2, Vector3)

---------------------------------------
-- rlgl enums -------------------------
---------------------------------------

-- | OpenGL version
data RLGLVersion
  = -- | OpenGL 1.1
    RLOpenGL11
  | -- | OpenGL 2.1 (GLSL 120)
    RLOpenGL21
  | -- | OpenGL 3.3 (GLSL 330)
    RLOpenGL33
  | -- | OpenGL 4.3 (using GLSL 330)
    RLOpenGL43
  | -- | OpenGL ES 2.0 (GLSL 100)
    RLOpenGLES20
  deriving (Eq, Show)

instance Enum RLGLVersion where
  fromEnum n = case n of
    RLOpenGL11 -> 0
    RLOpenGL21 -> 1
    RLOpenGL33 -> 2
    RLOpenGL43 -> 3
    RLOpenGLES20 -> 4
  toEnum n = case n of
    0 -> RLOpenGL11
    1 -> RLOpenGL21
    2 -> RLOpenGL33
    3 -> RLOpenGL43
    4 -> RLOpenGLES20
    _ -> error $ "(RLGLVersion.toEnum) Invalid value: " ++ show n

instance Storable RLGLVersion where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Trace log level.
-- NOTE: Organized by priority level
data RLTraceLogLevel
  = -- | Display all logs
    RLLogAll
  | -- | Trace logging, intended for internal use only
    RLLogTrace
  | -- | Debug logging, used for internal debugging, it should be disabled on release builds
    RLLogDebug
  | -- | Info logging, used for program execution info
    RLLogInfo
  | -- | Warning logging, used on recoverable failures
    RLLogWarning
  | -- | Error logging, used on unrecoverable failures
    RLLogError
  | -- | Fatal logging, used to abort program: exit(EXIT_FAILURE)
    RLLogFatal
  | -- | Disable logging
    RLLogNone
  deriving (Eq, Show, Enum)

instance Storable RLTraceLogLevel where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Texture pixel formats.
-- NOTE: Support depends on OpenGL version
data RLPixelFormat
  = -- | 8 bit per pixel (no alpha)
    RLPixelFormatUncompressedGrayscale
  | -- | 8*2 bpp (2 channels)
    RLPixelFormatUncompressedGrayAlpha
  | -- | 16 bpp
    RLPixelFormatUncompressedR5G6B5
  | -- | 24 bpp
    RLPixelFormatUncompressedR8G8B8
  | -- | 16 bpp (1 bit alpha)
    RLPixelFormatUncompressedR5G5B5A1
  | -- | 16 bpp (4 bit alpha)
    RLPixelFormatUncompressedR4G4B4A4
  | -- | 32 bpp
    RLPixelFormatUncompressedR8G8B8A8
  | -- | 32 bpp (1 channel - float)
    RLPixelFormatUncompressedR32
  | -- | 32*3 bpp (3 channels - float)
    RLPixelFormatUncompressedR32G32B32
  | -- | 32*4 bpp (4 channels - float)
    RLPixelFormatUncompressedR32G32B32A32
  | -- | 16 bpp (1 channel - half float)
    RLPixelFormatUncompressedR16
  | -- | 16*3 bpp (3 channels - half float)
    RLPixelFormatUncompressedR16G16B16
  | -- | 16*4 bpp (4 channels - half float)
    RLPixelFormatUncompressedR16G16B16A16
  | -- | 4 bpp (no alpha)
    RLPixelFormatCompressedDxt1Rgb
  | -- | 4 bpp (1 bit alpha)
    RLPixelFormatCompressedDxt1Rgba
  | -- | 8 bpp
    RLPixelFormatCompressedDxt3Rgba
  | -- | 8 bpp
    RLPixelFormatCompressedDxt5Rgba
  | -- | 4 bpp
    RLPixelFormatCompressedEtc1Rgb
  | -- | 4 bpp
    RLPixelFormatCompressedEtc2Rgb
  | -- | 8 bpp
    RLPixelFormatCompressedEtc2EacRgba
  | -- | 4 bpp
    RLPixelFormatCompressedPvrtRgb
  | -- | 4 bpp
    RLPixelFormatCompressedPvrtRgba
  | -- | 8 bpp
    RLPixelFormatCompressedAstc4x4Rgba
  | -- | 2 bpp
    RLPixelFormatCompressedAstc8x8Rgba
  deriving (Eq, Show)

instance Enum RLPixelFormat where
  fromEnum n = case n of
    RLPixelFormatUncompressedGrayscale -> 1
    RLPixelFormatUncompressedGrayAlpha -> 2
    RLPixelFormatUncompressedR5G6B5 -> 3
    RLPixelFormatUncompressedR8G8B8 -> 4
    RLPixelFormatUncompressedR5G5B5A1 -> 5
    RLPixelFormatUncompressedR4G4B4A4 -> 6
    RLPixelFormatUncompressedR8G8B8A8 -> 7
    RLPixelFormatUncompressedR32 -> 8
    RLPixelFormatUncompressedR32G32B32 -> 9
    RLPixelFormatUncompressedR32G32B32A32 -> 10
    RLPixelFormatUncompressedR16 -> 11
    RLPixelFormatUncompressedR16G16B16 -> 12
    RLPixelFormatUncompressedR16G16B16A16 -> 13
    RLPixelFormatCompressedDxt1Rgb -> 14
    RLPixelFormatCompressedDxt1Rgba -> 15
    RLPixelFormatCompressedDxt3Rgba -> 16
    RLPixelFormatCompressedDxt5Rgba -> 17
    RLPixelFormatCompressedEtc1Rgb -> 18
    RLPixelFormatCompressedEtc2Rgb -> 19
    RLPixelFormatCompressedEtc2EacRgba -> 20
    RLPixelFormatCompressedPvrtRgb -> 21
    RLPixelFormatCompressedPvrtRgba -> 22
    RLPixelFormatCompressedAstc4x4Rgba -> 23
    RLPixelFormatCompressedAstc8x8Rgba -> 24

  toEnum n = case n of
    1 -> RLPixelFormatUncompressedGrayscale
    2 -> RLPixelFormatUncompressedGrayAlpha
    3 -> RLPixelFormatUncompressedR5G6B5
    4 -> RLPixelFormatUncompressedR8G8B8
    5 -> RLPixelFormatUncompressedR5G5B5A1
    6 -> RLPixelFormatUncompressedR4G4B4A4
    7 -> RLPixelFormatUncompressedR8G8B8A8
    8 -> RLPixelFormatUncompressedR32
    9 -> RLPixelFormatUncompressedR32G32B32
    10 -> RLPixelFormatUncompressedR32G32B32A32
    11 -> RLPixelFormatUncompressedR16
    12 -> RLPixelFormatUncompressedR16G16B16
    13 -> RLPixelFormatUncompressedR16G16B16A16
    14 -> RLPixelFormatCompressedDxt1Rgb
    15 -> RLPixelFormatCompressedDxt1Rgba
    16 -> RLPixelFormatCompressedDxt3Rgba
    17 -> RLPixelFormatCompressedDxt5Rgba
    18 -> RLPixelFormatCompressedEtc1Rgb
    19 -> RLPixelFormatCompressedEtc2Rgb
    20 -> RLPixelFormatCompressedEtc2EacRgba
    21 -> RLPixelFormatCompressedPvrtRgb
    22 -> RLPixelFormatCompressedPvrtRgba
    23 -> RLPixelFormatCompressedAstc4x4Rgba
    24 -> RLPixelFormatCompressedAstc8x8Rgba
    _ -> error $ "(RLPixelFormat.toEnum) Invalid value: " ++ show n

instance Storable RLPixelFormat where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Texture parameters: filter mode.
-- NOTE 1: Filtering considers mipmaps if available in the texture.
-- NOTE 2: Filter is accordingly set for minification and magnification.
data RLTextureFilter
  = -- | No filter, just pixel approximation
    RLTextureFilterPoint
  | -- | Linear filtering
    RLTextureFilterBilinear
  | -- | Trilinear filtering (linear with mipmaps)
    RLTextureFilterTrilinear
  | -- | Anisotropic filtering 4x
    RLTextureFilterAnisotropic4x
  | -- | Anisotropic filtering 8x
    RLTextureFilterAnisotropic8x
  | -- | Anisotropic filtering 16x
    RLTextureFilterAnisotropic16x
  deriving (Eq, Show, Enum)

instance Storable RLTextureFilter where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Color blending modes (pre-defined)
data RLBlendMode
  = -- | Blend textures considering alpha (default)
    RlBlendAlpha
  | -- | Blend textures adding colors
    RlBlendAdditive
  | -- | Blend textures multiplying colors
    RlBlendMultiplied
  | -- | Blend textures adding colors (alternative)
    RlBlendAddColors
  | -- | Blend textures subtracting colors (alternative)
    RlBlendSubtractColors
  | -- | Blend premultiplied textures considering alpha
    RlBlendAlphaPremultiply
  | -- | Blend textures using custom src/dst factors (use rlSetBlendFactors())
    RlBlendCustom
  | -- | Blend textures using custom src/dst factors (use rlSetBlendFactorsSeparate())
    RlBlendCustomSeparate
  deriving (Eq, Show, Enum)

instance Storable RLBlendMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Shader location point type
data RLShaderLocationIndex
  = -- | Shader location: vertex attribute: position
    RLShaderLocVertexPosition
  | -- | Shader location: vertex attribute: texcoord01
    RLShaderLocVertexTexcoord01
  | -- | Shader location: vertex attribute: texcoord02
    RLShaderLocVertexTexcoord02
  | -- | Shader location: vertex attribute: normal
    RLShaderLocVertexNormal
  | -- | Shader location: vertex attribute: tangent
    RLShaderLocVertexTangent
  | -- | Shader location: vertex attribute: color
    RLShaderLocVertexColor
  | -- | Shader location: matrix uniform: model-view-projection
    RLShaderLocMatrixMVP
  | -- | Shader location: matrix uniform: view (camera transform)
    RLShaderLocMatrixView
  | -- | Shader location: matrix uniform: projection
    RLShaderLocMatrixProjection
  | -- | Shader location: matrix uniform: model (transform)
    RLShaderLocMatrixModel
  | -- | Shader location: matrix uniform: normal
    RLShaderLocMatrixNormal
  | -- | Shader location: vector uniform: view
    RLShaderLocVectorView
  | -- | Shader location: vector uniform: diffuse color
    RLShaderLocColorDiffuse
  | -- | Shader location: vector uniform: specular color
    RLShaderLocColorSpecular
  | -- | Shader location: vector uniform: ambient color
    RLShaderLocColorAmbient
  | -- | Shader location: sampler2d texture: albedo (same as: RL_SHADER_LOC_MAP_DIFFUSE)
    RLShaderLocMapAlbedo
  | -- | Shader location: sampler2d texture: metalness (same as: RL_SHADER_LOC_MAP_SPECULAR)
    RLShaderLocMapMetalness
  | -- | Shader location: sampler2d texture: normal
    RLShaderLocMapNormal
  | -- | Shader location: sampler2d texture: roughness
    RLShaderLocMapRoughness
  | -- | Shader location: sampler2d texture: occlusion
    RLShaderLocMapOcclusion
  | -- | Shader location: sampler2d texture: emission
    RLShaderLocMapEmission
  | -- | Shader location: sampler2d texture: height
    RLShaderLocMapHeight
  | -- | Shader location: samplerCube texture: cubemap
    RLShaderLocMapCubemap
  | -- | Shader location: samplerCube texture: irradiance
    RLShaderLocMapIrradiance
  | -- | Shader location: samplerCube texture: prefilter
    RLShaderLocMapPrefilter
  | -- | Shader location: sampler2d texture: brdf
    RLShaderLocMapBRDF
  deriving (Eq, Show, Enum)

instance Storable RLShaderLocationIndex where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Shader uniform data type
data RLShaderUniformDataType
  = -- | Shader uniform type: float
    RLShaderUniformFloat
  | -- | Shader uniform type: vec2 (2 float)
    RLShaderUniformVec2
  | -- | Shader uniform type: vec3 (3 float)
    RLShaderUniformVec3
  | -- | Shader uniform type: vec4 (4 float)
    RLShaderUniformVec4
  | -- | Shader uniform type: int
    RLShaderUniformInt
  | -- | Shader uniform type: ivec2 (2 int)
    RLShaderUniformIVec2
  | -- | Shader uniform type: ivec3 (3 int)
    RLShaderUniformIVec3
  | -- | Shader uniform type: ivec4 (4 int)
    RLShaderUniformIVec4
  | -- | Shader uniform type: sampler2d
    RLShaderUniformSampler2D
  deriving (Eq, Show, Enum)

instance Storable RLShaderUniformDataType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Shader attribute data types
data RLShaderAttributeDataType
  = -- | Shader attribute type: float
    RLShaderAttribFloat
  | -- | Shader attribute type: vec2 (2 float)
    RLShaderAttribVec2
  | -- | Shader attribute type: vec3 (3 float)
    RLShaderAttribVec3
  | -- | Shader attribute type: vec4 (4 float)
    RLShaderAttribVec4
  deriving (Eq, Show, Enum)

instance Storable RLShaderAttributeDataType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Framebuffer attachment type.
-- NOTE: By default up to 8 color channels are defined, but it can be more
data RLFramebufferAttachType
  = -- | Framebuffer attachment type: color 0
    RLAttachmentColorChannel0
  | -- | Framebuffer attachment type: color 1
    RLAttachmentColorChannel1
  | -- | Framebuffer attachment type: color 2
    RLAttachmentColorChannel2
  | -- | Framebuffer attachment type: color 3
    RLAttachmentColorChannel3
  | -- | Framebuffer attachment type: color 4
    RLAttachmentColorChannel4
  | -- | Framebuffer attachment type: color 5
    RLAttachmentColorChannel5
  | -- | Framebuffer attachment type: color 6
    RLAttachmentColorChannel6
  | -- | Framebuffer attachment type: color 7
    RLAttachmentColorChannel7
  | -- | Framebuffer attachment type: depth
    RLAttachmentDepth
  | -- | Framebuffer attachment type: stencil
    RLAttachmentStencil
  deriving (Eq, Show)

instance Enum RLFramebufferAttachType where
  fromEnum n = case n of
    RLAttachmentColorChannel0 -> 0
    RLAttachmentColorChannel1 -> 1
    RLAttachmentColorChannel2 -> 2
    RLAttachmentColorChannel3 -> 3
    RLAttachmentColorChannel4 -> 4
    RLAttachmentColorChannel5 -> 5
    RLAttachmentColorChannel6 -> 6
    RLAttachmentColorChannel7 -> 7
    RLAttachmentDepth -> 100
    RLAttachmentStencil -> 200

  toEnum n = case n of
    0 -> RLAttachmentColorChannel0
    1 -> RLAttachmentColorChannel1
    2 -> RLAttachmentColorChannel2
    3 -> RLAttachmentColorChannel3
    4 -> RLAttachmentColorChannel4
    5 -> RLAttachmentColorChannel5
    6 -> RLAttachmentColorChannel6
    7 -> RLAttachmentColorChannel7
    100 -> RLAttachmentDepth
    200 -> RLAttachmentStencil
    _ -> error $ "(RLFramebufferAttachType.toEnum) Invalid value: " ++ show n

instance Storable RLFramebufferAttachType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Framebuffer texture attachment type
data RLFramebufferAttachTextureType
  = -- | Framebuffer texture attachment type: cubemap, +X side
    RLAttachmentCubemapPositiveX
  | -- | Framebuffer texture attachment type: cubemap, -X side
    RLAttachmentCubemapNegativeX
  | -- | Framebuffer texture attachment type: cubemap, +Y side
    RLAttachmentCubemapPositiveY
  | -- | Framebuffer texture attachment type: cubemap, -Y side
    RLAttachmentCubemapNegativeY
  | -- | Framebuffer texture attachment type: cubemap, +Z side
    RLAttachmentCubemapPositiveZ
  | -- | Framebuffer texture attachment type: cubemap, -Z side
    RLAttachmentCubemapNegativeZ
  | -- | Framebuffer texture attachment type: texture2d
    RLAttachmentTexture2D
  | -- | Framebuffer texture attachment type: renderbuffer
    RLAttachmentRenderBuffer
  deriving (Eq, Show)

instance Enum RLFramebufferAttachTextureType where
  fromEnum n = case n of
    RLAttachmentCubemapPositiveX -> 0
    RLAttachmentCubemapNegativeX -> 1
    RLAttachmentCubemapPositiveY -> 2
    RLAttachmentCubemapNegativeY -> 3
    RLAttachmentCubemapPositiveZ -> 4
    RLAttachmentCubemapNegativeZ -> 5
    RLAttachmentTexture2D -> 100
    RLAttachmentRenderBuffer -> 200

  toEnum n = case n of
    0 -> RLAttachmentCubemapPositiveX
    1 -> RLAttachmentCubemapNegativeX
    2 -> RLAttachmentCubemapPositiveY
    3 -> RLAttachmentCubemapNegativeY
    4 -> RLAttachmentCubemapPositiveZ
    5 -> RLAttachmentCubemapNegativeZ
    100 -> RLAttachmentTexture2D
    200 -> RLAttachmentRenderBuffer
    _ -> error $ "(RLFramebufferAttachTextureType.toEnum) Invalid value: " ++ show n

instance Storable RLFramebufferAttachTextureType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Face culling mode
data RLCullMode
  = RLCullFaceFront
  | RLCullFaceBack
  deriving (Eq, Show, Enum)

instance Storable RLCullMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Matrix modes (equivalent to OpenGL)
data RLMatrixMode
  = -- | GL_MODELVIEW
    RLModelView
  | -- | GL_PROJECTION
    RLProjection
  | -- | GL_TEXTURE
    RLTexture
  deriving (Eq, Show)

instance Enum RLMatrixMode where
  fromEnum n = case n of
    RLModelView -> 0x1700
    RLProjection -> 0x1701
    RLTexture -> 0x1702

  toEnum n = case n of
    0x1700 -> RLModelView
    0x1701 -> RLProjection
    0x1702 -> RLTexture
    _ -> error $ "(RLMatrixMode.toEnum) Invalid value: " ++ show n

instance Storable RLMatrixMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Primitive assembly draw modes
data RLDrawMode
  = -- | GL_LINES
    RLLines
  | -- | GL_TRIANGLES
    RLTriangles
  | -- | GL_QUADS
    RLQuads
  deriving (Eq, Show)

instance Enum RLDrawMode where
  fromEnum n = case n of
    RLLines -> 0x0001
    RLTriangles -> 0x0004
    RLQuads -> 0x0007

  toEnum n = case n of
    0x0001 -> RLLines
    0x0004 -> RLTriangles
    0x0007 -> RLQuads
    _ -> error $ "(RLDrawMode.toEnum) Invalid value: " ++ show n

instance Storable RLDrawMode where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | Texture parameters (equivalent to OpenGL defines)
data RLTextureParam
  = -- | GL_TEXTURE_WRAP_S
    RLTextureParamWrapS
  | -- | GL_TEXTURE_WRAP_T
    RLTextureParamWrapT
  | -- | GL_TEXTURE_MAG_FILTER
    RLTextureParamMagFilter
  | -- | GL_TEXTURE_MIN_FILTER
    RLTextureParamMinFilter
  | -- | GL_NEAREST
    RLTextureParamFilterNearest
  | -- | GL_LINEAR
    RLTextureParamFilterLinear
  | -- | GL_NEAREST_MIPMAP_NEAREST
    RLTextureParamFilterMipNearest
  | -- | GL_NEAREST_MIPMAP_LINEAR
    RLTextureParamFilterNearestMipLinear
  | -- | GL_LINEAR_MIPMAP_NEAREST
    RLTextureParamFilterLinearMipNearest
  | -- | GL_LINEAR_MIPMAP_LINEAR
    RLTextureParamFilterMipLinear
  | -- | Anisotropic filter (custom identifier)
    RLTextureParamFilterAnisotropic
  | -- | Texture mipmap bias, percentage ratio (custom identifier)
    RLTextureParamMipmapBiasRatio
  deriving (Eq, Show)

instance Enum RLTextureParam where
  fromEnum n = case n of
    RLTextureParamWrapS -> 0x2802
    RLTextureParamWrapT -> 0x2803
    RLTextureParamMagFilter -> 0x2800
    RLTextureParamMinFilter -> 0x2801
    RLTextureParamFilterNearest -> 0x2600
    RLTextureParamFilterLinear -> 0x2601
    RLTextureParamFilterMipNearest -> 0x2700
    RLTextureParamFilterNearestMipLinear -> 0x2702
    RLTextureParamFilterLinearMipNearest -> 0x2701
    RLTextureParamFilterMipLinear -> 0x2703
    RLTextureParamFilterAnisotropic -> 0x3000
    RLTextureParamMipmapBiasRatio -> 0x4000

  toEnum n = case n of
    0x2802 -> RLTextureParamWrapS
    0x2803 -> RLTextureParamWrapT
    0x2800 -> RLTextureParamMagFilter
    0x2801 -> RLTextureParamMinFilter
    0x2600 -> RLTextureParamFilterNearest
    0x2601 -> RLTextureParamFilterLinear
    0x2700 -> RLTextureParamFilterMipNearest
    0x2702 -> RLTextureParamFilterNearestMipLinear
    0x2701 -> RLTextureParamFilterLinearMipNearest
    0x2703 -> RLTextureParamFilterMipLinear
    0x3000 -> RLTextureParamFilterAnisotropic
    0x4000 -> RLTextureParamMipmapBiasRatio
    _ -> error $ "(RLTextureParam.toEnum) Invalid value: " ++ show n

instance Storable RLTextureParam where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | OpenGL shader type
data RLShaderType
  = -- | GL_FRAGMENT_SHADER
    RLFragmentShader
  | -- | GL_VERTEX_SHADER
    RLVertexShader
  | -- | GL_COMPUTE_SHADER
    RLComputeShader
  deriving (Eq, Show)

instance Enum RLShaderType where
  fromEnum n = case n of
    RLFragmentShader -> 0x8B30
    RLVertexShader -> 0x8B31
    RLComputeShader -> 0x91B9

  toEnum n = case n of
    0x8B30 -> RLFragmentShader
    0x8B31 -> RLVertexShader
    0x91B9 -> RLComputeShader
    _ -> error $ "(RLShaderType.toEnum) Invalid value: " ++ show n

instance Storable RLShaderType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | GL buffer usage hint
data RLBufferHint
  = -- | GL_STREAM_DRAW
    RLBufferHintStreamDraw
  | -- | GL_STREAM_READ
    RLBufferHintStreamRead
  | -- | GL_STREAM_COPY
    RLBufferHintStreamCopy
  | -- | GL_STATIC_DRAW
    RLBufferHintStaticDraw
  | -- | GL_STATIC_READ
    RLBufferHintStaticRead
  | -- | GL_STATIC_COPY
    RLBufferHintStaticCopy
  | -- | GL_DYNAMIC_DRAW
    RLBufferHintDynamicDraw
  | -- | GL_DYNAMIC_READ
    RLBufferHintDynamicRead
  | -- | GL_DYNAMIC_COPY
    RLBufferHintDynamicCopy
  deriving (Eq, Show)

instance Enum RLBufferHint where
  fromEnum n = case n of
    RLBufferHintStreamDraw -> 0x88E0
    RLBufferHintStreamRead -> 0x88E1
    RLBufferHintStreamCopy -> 0x88E2
    RLBufferHintStaticDraw -> 0x88E4
    RLBufferHintStaticRead -> 0x88E5
    RLBufferHintStaticCopy -> 0x88E6
    RLBufferHintDynamicDraw -> 0x88E8
    RLBufferHintDynamicRead -> 0x88E9
    RLBufferHintDynamicCopy -> 0x88EA

  toEnum n = case n of
    0x88E0 -> RLBufferHintStreamDraw
    0x88E1 -> RLBufferHintStreamRead
    0x88E2 -> RLBufferHintStreamCopy
    0x88E4 -> RLBufferHintStaticDraw
    0x88E5 -> RLBufferHintStaticRead
    0x88E6 -> RLBufferHintStaticCopy
    0x88E8 -> RLBufferHintDynamicDraw
    0x88E9 -> RLBufferHintDynamicRead
    0x88EA -> RLBufferHintDynamicCopy
    _ -> error $ "(RLBufferHint.toEnum) Invalid value: " ++ show n

instance Storable RLBufferHint where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

-- | GL buffer mask
data RLBitField
  = -- | GL_COLOR_BUFFER_BIT
    RLGLColorBuffer
  | -- | GL_DEPTH_BUFFER_BIT
    RLGLDepthBuffer
  | -- | GL_STENCIL_BUFFER_BIT
    RLGLStencilBuffer
  deriving (Eq, Show)

instance Enum RLBitField where
  fromEnum n = case n of
    RLGLColorBuffer -> 0x00004000
    RLGLDepthBuffer -> 0x00000100
    RLGLStencilBuffer -> 0x00000400

  toEnum n = case n of
    0x00004000 -> RLGLColorBuffer
    0x00000100 -> RLGLDepthBuffer
    0x00000400 -> RLGLStencilBuffer
    _ -> error $ "(RLGLBitField.toEnum) Invalid value: " ++ show n

instance Storable RLBitField where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr)
    return $ toEnum $ fromEnum (val :: CInt)
  poke ptr v = poke (castPtr ptr) (fromIntegral (fromEnum v) :: CInt)

---------------------------------------
-- rlgl structures --------------------
---------------------------------------

-- | Dynamic vertex buffers (position + texcoords + colors + indices arrays)
data RLVertexBuffer = RLVertexBuffer
  { -- | Number of elements in the buffer (QUADS)
    rlVertexBuffer'elementCount :: Int,
    -- | Vertex position (shader-location = 0)
    rlVertexBuffer'vertices :: [Vector3],
    -- | Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
    rlVertexBuffer'texcoords :: [Vector2],
    -- | Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
    rlVertexBuffer'colors :: [Color],
    -- | Vertex indices (in case vertex data comes indexed) (6 indices per quad)
    rlVertexBuffer'indices :: [Integer],
    -- | OpenGL Vertex Array Object id
    rlVertexBuffer'vaoId :: Integer,
    -- | OpenGL Vertex Buffer Objects id (4 types of vertex data)
    rlVertexBuffer'vboId :: [Integer]
  }
  deriving (Eq, Show)

instance Storable RLVertexBuffer where
  sizeOf _ = 64
  alignment _ = 8
  peek _p = do
    elementCount <- fromIntegral <$> peek (p'rlVertexBuffer'elementCount _p)
    vertices <- peekArray elementCount =<< peek (p'rlVertexBuffer'vertices _p)
    texcoords <- peekArray elementCount =<< peek (p'rlVertexBuffer'texcoords _p)
    colors <- peekArray elementCount =<< peek (p'rlVertexBuffer'colors _p)
    indices <- map fromIntegral <$> (peekArray elementCount =<< peek (p'rlVertexBuffer'indices _p))
    vaoId <- fromIntegral <$> peek (p'rlVertexBuffer'vaoId _p)
    vboId <- map fromIntegral <$> peekStaticArray 4 (p'rlVertexBuffer'vboId _p)
    return $ RLVertexBuffer elementCount vertices texcoords colors indices vaoId vboId
  poke _p (RLVertexBuffer elementCount vertices texcoords colors indices vaoId vboId) = do
    poke (p'rlVertexBuffer'elementCount _p) (fromIntegral elementCount)
    poke (p'rlVertexBuffer'vertices _p) =<< newArray vertices
    poke (p'rlVertexBuffer'texcoords _p) =<< newArray texcoords
    poke (p'rlVertexBuffer'colors _p) =<< newArray colors
    poke (p'rlVertexBuffer'indices _p) =<< newArray (map fromIntegral indices)
    poke (p'rlVertexBuffer'vaoId _p) (fromIntegral vaoId)
    pokeStaticArray (p'rlVertexBuffer'vboId _p) (map fromIntegral vboId)
    return ()

p'rlVertexBuffer'elementCount :: Ptr RLVertexBuffer -> Ptr CInt
p'rlVertexBuffer'elementCount = (`plusPtr` 0)

-- array (rlVertexBuffer'elementCount)
p'rlVertexBuffer'vertices :: Ptr RLVertexBuffer -> Ptr (Ptr Vector3)
p'rlVertexBuffer'vertices = (`plusPtr` 8)

-- array (rlVertexBuffer'elementCount)
p'rlVertexBuffer'texcoords :: Ptr RLVertexBuffer -> Ptr (Ptr Vector2)
p'rlVertexBuffer'texcoords = (`plusPtr` 16)

-- array (rlVertexBuffer'elementCount)
p'rlVertexBuffer'colors :: Ptr RLVertexBuffer -> Ptr (Ptr Color)
p'rlVertexBuffer'colors = (`plusPtr` 24)

-- array (rlVertexBuffer'elementCount)
p'rlVertexBuffer'indices :: Ptr RLVertexBuffer -> Ptr (Ptr CUInt)
p'rlVertexBuffer'indices = (`plusPtr` 32)

p'rlVertexBuffer'vaoId :: Ptr RLVertexBuffer -> Ptr CUInt
p'rlVertexBuffer'vaoId = (`plusPtr` 40)

-- static array (4)
p'rlVertexBuffer'vboId :: Ptr RLVertexBuffer -> Ptr CUInt
p'rlVertexBuffer'vboId = (`plusPtr` 44)

instance Freeable RLVertexBuffer where
  rlFreeDependents _ ptr = do
    c'free . castPtr =<< peek (p'rlVertexBuffer'vertices ptr)
    c'free . castPtr =<< peek (p'rlVertexBuffer'texcoords ptr)
    c'free . castPtr =<< peek (p'rlVertexBuffer'colors ptr)
    c'free . castPtr =<< peek (p'rlVertexBuffer'indices ptr)

-- | Draw call type.
--
-- NOTE: Only texture changes register a new draw, other state-change-related elements are not
-- used at this moment (vaoId, shaderId, matrices), raylib just forces a batch draw call if any
-- of those state changes happen (this is done in the core module).
data RLDrawCall = RLDrawCall
  { -- | Drawing mode: LINES, TRIANGLES, QUADS
    rlDrawCall'mode :: RLDrawMode,
    -- | Number of vertices of the draw
    rlDrawCall'vertexCount :: Int,
    -- | Number of vertices required for index alignment (LINES, TRIANGLES)
    rlDrawCall'vertexAlignment :: Int,
    -- | Texture id to be used on the draw -> Used to create new draw call if changed
    rlDrawCall'textureId :: Integer
  }
  deriving (Eq, Show, Freeable)

instance Storable RLDrawCall where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    mode <- peek (p'rlDrawCall'mode _p)
    vertexCount <- fromIntegral <$> peek (p'rlDrawCall'vertexCount _p)
    vertexAlignment <- fromIntegral <$> peek (p'rlDrawCall'vertexAlignment _p)
    textureId <- fromIntegral <$> peek (p'rlDrawCall'textureId _p)
    return $ RLDrawCall mode vertexCount vertexAlignment textureId
  poke _p (RLDrawCall mode vertexCount vertexAlignment textureId) = do
    poke (p'rlDrawCall'mode _p) mode
    poke (p'rlDrawCall'vertexCount _p) (fromIntegral vertexCount)
    poke (p'rlDrawCall'vertexAlignment _p) (fromIntegral vertexAlignment)
    poke (p'rlDrawCall'textureId _p) (fromIntegral textureId)
    return ()

p'rlDrawCall'mode :: Ptr RLDrawCall -> Ptr RLDrawMode
p'rlDrawCall'mode = (`plusPtr` 0)

p'rlDrawCall'vertexCount :: Ptr RLDrawCall -> Ptr CInt
p'rlDrawCall'vertexCount = (`plusPtr` 4)

p'rlDrawCall'vertexAlignment :: Ptr RLDrawCall -> Ptr CInt
p'rlDrawCall'vertexAlignment = (`plusPtr` 8)

p'rlDrawCall'textureId :: Ptr RLDrawCall -> Ptr CUInt
p'rlDrawCall'textureId = (`plusPtr` 12)

-- rlRenderBatch type
data RLRenderBatch = RLRenderBatch
  { -- | Number of vertex buffers (multi-buffering support)
    rlRenderBatch'bufferCount :: Int,
    -- | Current buffer tracking in case of multi-buffering
    rlRenderBatch'currentBuffer :: Int,
    -- | Dynamic buffer(s) for vertex data
    rlRenderBatch'vertexBuffers :: [RLVertexBuffer],
    -- | Draw calls array, depends on textureId
    rlRenderBatch'draws :: [RLDrawCall],
    -- | Draw calls counter
    rlRenderBatch'drawCounter :: Int,
    -- | Current depth value for next draw
    rlRenderBatch'currentDepth :: Float
  }
  deriving (Eq, Show)

instance Storable RLRenderBatch where
  sizeOf _ = 32
  alignment _ = 8
  peek _p = do
    bufferCount <- fromIntegral <$> peek (p'rlRenderBatch'bufferCount _p)
    currentBuffer <- fromIntegral <$> peek (p'rlRenderBatch'currentBuffer _p)
    vertexBuffers <- peekArray bufferCount =<< peek (p'rlRenderBatch'vertexBuffers _p)
    draws <- peekArray 256 =<< peek (p'rlRenderBatch'draws _p)
    drawCounter <- fromIntegral <$> peek (p'rlRenderBatch'drawCounter _p)
    currentDepth <- realToFrac <$> peek (p'rlRenderBatch'currentDepth _p)
    return $ RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth
  poke _p (RLRenderBatch bufferCount currentBuffer vertexBuffers draws drawCounter currentDepth) = do
    poke (p'rlRenderBatch'bufferCount _p) (fromIntegral bufferCount)
    poke (p'rlRenderBatch'currentBuffer _p) (fromIntegral currentBuffer)
    poke (p'rlRenderBatch'vertexBuffers _p) =<< newArray vertexBuffers
    poke (p'rlRenderBatch'draws _p) =<< newArray draws
    poke (p'rlRenderBatch'drawCounter _p) (fromIntegral drawCounter)
    poke (p'rlRenderBatch'currentDepth _p) (realToFrac currentDepth)
    return ()

p'rlRenderBatch'bufferCount :: Ptr RLRenderBatch -> Ptr CInt
p'rlRenderBatch'bufferCount = (`plusPtr` 0)

p'rlRenderBatch'currentBuffer :: Ptr RLRenderBatch -> Ptr CInt
p'rlRenderBatch'currentBuffer = (`plusPtr` 4)

-- array (rlRenderBatch'bufferCount)
p'rlRenderBatch'vertexBuffers :: Ptr RLRenderBatch -> Ptr (Ptr RLVertexBuffer)
p'rlRenderBatch'vertexBuffers = (`plusPtr` 8)

-- array (256)
p'rlRenderBatch'draws :: Ptr RLRenderBatch -> Ptr (Ptr RLDrawCall)
p'rlRenderBatch'draws = (`plusPtr` 16)

p'rlRenderBatch'drawCounter :: Ptr RLRenderBatch -> Ptr CInt
p'rlRenderBatch'drawCounter = (`plusPtr` 24)

p'rlRenderBatch'currentDepth :: Ptr RLRenderBatch -> Ptr CFloat
p'rlRenderBatch'currentDepth = (`plusPtr` 28)

instance Freeable RLRenderBatch where
  rlFreeDependents val ptr = do
    rlFreeArray (rlRenderBatch'vertexBuffers val) =<< peek (p'rlRenderBatch'vertexBuffers ptr)
    c'free . castPtr =<< peek (p'rlRenderBatch'draws ptr)
