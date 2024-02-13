{-# OPTIONS -Wall #-}

-- | Bindings to @rlgl@
module Raylib.Util.RLGL
  ( -- * Matrix operations
    rlMatrixMode,
    rlPushMatrix,
    rlPopMatrix,
    rlLoadIdentity,
    rlTranslatef,
    rlRotatef,
    rlScalef,
    rlMultMatrixf,
    rlFrustum,
    rlOrtho,
    rlViewport,

    -- * Vertex level operations
    rlBegin,
    rlEnd,
    rlVertex2i,
    rlVertex2f,
    rlVertex3f,
    rlTexCoord2f,
    rlNormal3f,
    rlColor4ub,
    rlColor3f,
    rlColor4f,

    -- * OpenGL style functions (common to 1.1, 3.3+, ES2)

    -- | NOTE: These functions are used to completely abstract raylib code from OpenGL layer,
    --   some of them are direct wrappers over OpenGL calls, some others are custom

    -- ** Vertex buffers state
    rlEnableVertexArray,
    rlDisableVertexArray,
    rlEnableVertexBuffer,
    rlDisableVertexBuffer,
    rlEnableVertexBufferElement,
    rlDisableVertexBufferElement,
    rlEnableVertexAttribute,
    rlDisableVertexAttribute,

    -- ** Textures state
    rlActiveTextureSlot,
    rlEnableTexture,
    rlDisableTexture,
    rlEnableTextureCubemap,
    rlDisableTextureCubemap,
    rlTextureParameters,
    rlCubemapParameters,

    -- ** Shader state
    rlEnableShader,
    rlDisableShader,

    -- ** Framebuffer state
    rlEnableFramebuffer,
    rlDisableFramebuffer,
    rlGetActiveFramebuffer,
    rlActiveDrawBuffers,
    rlBlitFramebuffer,
    rlBindFramebuffer,

    -- ** General render state
    rlEnableColorBlend,
    rlDisableColorBlend,
    rlEnableDepthTest,
    rlDisableDepthTest,
    rlEnableDepthMask,
    rlDisableDepthMask,
    rlEnableBackfaceCulling,
    rlDisableBackfaceCulling,
    rlColorMask,
    rlSetCullFace,
    rlEnableScissorTest,
    rlDisableScissorTest,
    rlScissor,
    rlEnableWireMode,
    rlEnablePointMode,
    rlDisableWireMode,
    rlSetLineWidth,
    rlGetLineWidth,
    rlEnableSmoothLines,
    rlDisableSmoothLines,
    rlEnableStereoRender,
    rlDisableStereoRender,
    rlIsStereoRenderEnabled,
    rlClearColor,
    rlClearScreenBuffers,
    rlCheckErrors,
    rlSetBlendMode,
    rlSetBlendFactors,
    rlSetBlendFactorsSeparate,

    -- * rlgl functionality

    -- ** rlgl initialization functions
    rlglInit,
    rlglClose,
    rlLoadExtensions,
    rlGetVersion,
    rlSetFramebufferWidth,
    rlGetFramebufferWidth,
    rlSetFramebufferHeight,
    rlGetFramebufferHeight,
    rlGetTextureIdDefault,
    rlGetShaderIdDefault,
    rlGetShaderLocsDefault,

    -- ** Render batch management

    -- | NOTE: rlgl provides a default render batch to behave like OpenGL 1.1 immediate mode
    --   but this render batch API is exposed in case custom batches are required
    rlLoadRenderBatch,
    rlUnloadRenderBatch,
    rlDrawRenderBatch,
    rlSetRenderBatchActive,
    rlDrawRenderBatchActive,
    rlCheckRenderBatchLimit,
    rlSetTexture,

    -- ** Vertex buffers management
    rlLoadVertexArray,
    rlLoadVertexBuffer,
    rlLoadVertexBufferElement,
    rlUpdateVertexBuffer,
    rlUpdateVertexBufferElements,
    rlUnloadVertexArray,
    rlUnloadVertexBuffer,
    rlSetVertexAttribute,
    rlSetVertexAttributeDivisor,
    rlSetVertexAttributeDefault,
    rlDrawVertexArray,
    rlDrawVertexArrayElements,
    rlDrawVertexArrayInstanced,
    rlDrawVertexArrayElementsInstanced,

    -- ** Textures management
    rlLoadTexture,
    rlLoadTextureDepth,
    rlLoadTextureCubemap,
    rlUpdateTexture,
    rlGetGlTextureFormats,
    rlGetPixelFormatName,
    rlUnloadTexture,
    rlGenTextureMipmaps,
    rlReadTexturePixels,
    rlReadScreenPixels,

    -- ** Framebuffer management (fbo)
    rlLoadFramebuffer,
    rlFramebufferAttach,
    rlFramebufferComplete,
    rlUnloadFramebuffer,

    -- ** Shaders management
    rlLoadShaderCode,
    rlCompileShader,
    rlLoadShaderProgram,
    rlUnloadShaderProgram,
    rlGetLocationUniform,
    rlGetLocationAttrib,
    rlSetUniform,
    rlSetUniformMatrix,
    rlSetUniformSampler,
    rlSetShader,

    -- ** Compute shader management
    rlLoadComputeShaderProgram,
    rlComputeShaderDispatch,

    -- ** Shader buffer storage object management (ssbo)
    rlLoadShaderBuffer,
    rlUnloadShaderBuffer,
    rlUpdateShaderBuffer,
    rlBindShaderBuffer,
    rlCopyShaderBuffer,
    rlGetShaderBufferSize,

    -- ** Buffer management
    rlBindImageTexture,

    -- ** Matrix state management
    rlGetMatrixModelview,
    rlGetMatrixProjection,
    rlGetMatrixTransform,
    rlGetMatrixProjectionStereo,
    rlGetMatrixViewOffsetStereo,
    rlSetMatrixProjection,
    rlSetMatrixModelview,
    rlSetMatrixProjectionStereo,
    rlSetMatrixViewOffsetStereo,

    -- ** Quick and dirty cube/quad buffers load->draw->unload
    rlLoadDrawCube,
    rlLoadDrawQuad,
  )
where

import Foreign
  ( Ptr,
    Storable (peek, poke, sizeOf),
    Word8,
    castPtr,
    fromBool,
    malloc,
    nullPtr,
    toBool,
  )
import Foreign.C (CInt, CUChar, CUInt, CUShort, withCString)
import Raylib.Internal.Foreign
  ( Freeable,
    configsToBitflag,
    pop,
    popCArray,
    withFreeable,
    withFreeableArray,
    withFreeableArrayLen,
  )
import Raylib.Internal.Native
  ( c'rlActiveDrawBuffers,
    c'rlActiveTextureSlot,
    c'rlBegin,
    c'rlBindFramebuffer,
    c'rlBindImageTexture,
    c'rlBindShaderBuffer,
    c'rlBlitFramebuffer,
    c'rlCheckErrors,
    c'rlCheckRenderBatchLimit,
    c'rlClearColor,
    c'rlClearScreenBuffers,
    c'rlColor3f,
    c'rlColor4f,
    c'rlColor4ub,
    c'rlColorMask,
    c'rlCompileShader,
    c'rlComputeShaderDispatch,
    c'rlCopyShaderBuffer,
    c'rlCubemapParameters,
    c'rlDisableBackfaceCulling,
    c'rlDisableColorBlend,
    c'rlDisableDepthMask,
    c'rlDisableDepthTest,
    c'rlDisableFramebuffer,
    c'rlDisableScissorTest,
    c'rlDisableShader,
    c'rlDisableSmoothLines,
    c'rlDisableStereoRender,
    c'rlDisableTexture,
    c'rlDisableTextureCubemap,
    c'rlDisableVertexArray,
    c'rlDisableVertexAttribute,
    c'rlDisableVertexBuffer,
    c'rlDisableVertexBufferElement,
    c'rlDisableWireMode,
    c'rlDrawRenderBatch,
    c'rlDrawRenderBatchActive,
    c'rlDrawVertexArray,
    c'rlDrawVertexArrayElements,
    c'rlDrawVertexArrayElementsInstanced,
    c'rlDrawVertexArrayInstanced,
    c'rlEnableBackfaceCulling,
    c'rlEnableColorBlend,
    c'rlEnableDepthMask,
    c'rlEnableDepthTest,
    c'rlEnableFramebuffer,
    c'rlEnablePointMode,
    c'rlEnableScissorTest,
    c'rlEnableShader,
    c'rlEnableSmoothLines,
    c'rlEnableStereoRender,
    c'rlEnableTexture,
    c'rlEnableTextureCubemap,
    c'rlEnableVertexArray,
    c'rlEnableVertexAttribute,
    c'rlEnableVertexBuffer,
    c'rlEnableVertexBufferElement,
    c'rlEnableWireMode,
    c'rlEnd,
    c'rlFramebufferAttach,
    c'rlFramebufferComplete,
    c'rlFrustum,
    c'rlGenTextureMipmaps,
    c'rlGetActiveFramebuffer,
    c'rlGetFramebufferHeight,
    c'rlGetFramebufferWidth,
    c'rlGetGlTextureFormats,
    c'rlGetLineWidth,
    c'rlGetLocationAttrib,
    c'rlGetLocationUniform,
    c'rlGetMatrixModelview,
    c'rlGetMatrixProjection,
    c'rlGetMatrixProjectionStereo,
    c'rlGetMatrixTransform,
    c'rlGetMatrixViewOffsetStereo,
    c'rlGetPixelDataSize,
    c'rlGetShaderBufferSize,
    c'rlGetShaderIdDefault,
    c'rlGetShaderLocsDefault,
    c'rlGetTextureIdDefault,
    c'rlGetVersion,
    c'rlIsStereoRenderEnabled,
    c'rlLoadComputeShaderProgram,
    c'rlLoadDrawCube,
    c'rlLoadDrawQuad,
    c'rlLoadExtensions,
    c'rlLoadFramebuffer,
    c'rlLoadIdentity,
    c'rlLoadRenderBatch,
    c'rlLoadShaderBuffer,
    c'rlLoadShaderCode,
    c'rlLoadShaderProgram,
    c'rlLoadTexture,
    c'rlLoadTextureCubemap,
    c'rlLoadTextureDepth,
    c'rlLoadVertexArray,
    c'rlLoadVertexBuffer,
    c'rlLoadVertexBufferElement,
    c'rlMatrixMode,
    c'rlMultMatrixf,
    c'rlNormal3f,
    c'rlOrtho,
    c'rlPopMatrix,
    c'rlPushMatrix,
    c'rlReadScreenPixels,
    c'rlReadTexturePixels,
    c'rlRotatef,
    c'rlScalef,
    c'rlScissor,
    c'rlSetBlendFactors,
    c'rlSetBlendFactorsSeparate,
    c'rlSetBlendMode,
    c'rlSetCullFace,
    c'rlSetFramebufferHeight,
    c'rlSetFramebufferWidth,
    c'rlSetLineWidth,
    c'rlSetMatrixModelview,
    c'rlSetMatrixProjection,
    c'rlSetMatrixProjectionStereo,
    c'rlSetMatrixViewOffsetStereo,
    c'rlSetRenderBatchActive,
    c'rlSetShader,
    c'rlSetTexture,
    c'rlSetUniform,
    c'rlSetUniformMatrix,
    c'rlSetUniformSampler,
    c'rlSetVertexAttribute,
    c'rlSetVertexAttributeDefault,
    c'rlSetVertexAttributeDivisor,
    c'rlTexCoord2f,
    c'rlTextureParameters,
    c'rlTranslatef,
    c'rlUnloadFramebuffer,
    c'rlUnloadRenderBatch,
    c'rlUnloadShaderBuffer,
    c'rlUnloadShaderProgram,
    c'rlUnloadTexture,
    c'rlUnloadVertexArray,
    c'rlUnloadVertexBuffer,
    c'rlUpdateShaderBuffer,
    c'rlUpdateTexture,
    c'rlUpdateVertexBuffer,
    c'rlUpdateVertexBufferElements,
    c'rlVertex2f,
    c'rlVertex2i,
    c'rlVertex3f,
    c'rlViewport,
    c'rlglClose,
    c'rlglInit,
  )
import Raylib.Types
  ( Matrix,
    RLBitField,
    RLBlendMode,
    RLBufferHint,
    RLCullMode,
    RLDrawMode,
    RLFramebufferAttachTextureType,
    RLFramebufferAttachType,
    RLMatrixMode,
    RLPixelFormat (..),
    RLRenderBatch,
    RLShaderType,
    RLTextureParam,
    ShaderUniformDataV,
    unpackShaderUniformDataV,
  )

-- | Choose the current matrix to be transformed
rlMatrixMode :: RLMatrixMode -> IO ()
rlMatrixMode mode = c'rlMatrixMode (fromIntegral $ fromEnum mode)

-- | Push the current matrix to stack
rlPushMatrix :: IO ()
rlPushMatrix = c'rlPushMatrix

-- | Pop latest inserted matrix from stack
rlPopMatrix :: IO ()
rlPopMatrix = c'rlPopMatrix

-- | Reset current matrix to identity matrix
rlLoadIdentity :: IO ()
rlLoadIdentity = c'rlLoadIdentity

-- | Multiply the current matrix by a translation matrix
rlTranslatef :: Float -> Float -> Float -> IO ()
rlTranslatef x y z = c'rlTranslatef (realToFrac x) (realToFrac y) (realToFrac z)

-- | Multiply the current matrix by a rotation matrix
rlRotatef :: Float -> Float -> Float -> Float -> IO ()
rlRotatef angle x y z = c'rlRotatef (realToFrac angle) (realToFrac x) (realToFrac y) (realToFrac z)

-- | Multiply the current matrix by a scaling matrix
rlScalef :: Float -> Float -> Float -> IO ()
rlScalef x y z = c'rlScalef (realToFrac x) (realToFrac y) (realToFrac z)

-- | Multiply the current matrix by another matrix
rlMultMatrixf :: [Float] -> IO ()
rlMultMatrixf matf = withFreeableArray (map realToFrac matf) c'rlMultMatrixf

-- | Multiply the current matrix by a perspective matrix generated by parameters
rlFrustum :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
rlFrustum left right bottom top znear zfar = c'rlFrustum (realToFrac left) (realToFrac right) (realToFrac bottom) (realToFrac top) (realToFrac znear) (realToFrac zfar)

-- | Multiply the current matrix by an orthographic matrix generated by parameters
rlOrtho :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
rlOrtho left right bottom top znear zfar = c'rlOrtho (realToFrac left) (realToFrac right) (realToFrac bottom) (realToFrac top) (realToFrac znear) (realToFrac zfar)

-- | Set the viewport area
rlViewport :: Int -> Int -> Int -> Int -> IO ()
rlViewport x y width height = c'rlViewport (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

-- | Initialize drawing mode (how to organize vertex)
rlBegin :: RLDrawMode -> IO ()
rlBegin mode = c'rlBegin (fromIntegral $ fromEnum mode)

-- | Finish vertex providing
rlEnd :: IO ()
rlEnd = c'rlEnd

-- | Define one vertex (position) - 2 int
rlVertex2i :: Int -> Int -> IO ()
rlVertex2i x y = c'rlVertex2i (fromIntegral x) (fromIntegral y)

-- | Define one vertex (position) - 2 float
rlVertex2f :: Float -> Float -> IO ()
rlVertex2f x y = c'rlVertex2f (realToFrac x) (realToFrac y)

-- | Define one vertex (position) - 3 float
rlVertex3f :: Float -> Float -> Float -> IO ()
rlVertex3f x y z = c'rlVertex3f (realToFrac x) (realToFrac y) (realToFrac z)

-- | Define one vertex (texture coordinate) - 2 float
rlTexCoord2f :: Float -> Float -> IO ()
rlTexCoord2f x y = c'rlTexCoord2f (realToFrac x) (realToFrac y)

-- | Define one vertex (normal) - 3 float
rlNormal3f :: Float -> Float -> Float -> IO ()
rlNormal3f x y z = c'rlNormal3f (realToFrac x) (realToFrac y) (realToFrac z)

-- | Define one vertex (color) - 4 byte
rlColor4ub :: Word8 -> Word8 -> Word8 -> Word8 -> IO ()
rlColor4ub r g b a = c'rlColor4ub (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

-- | Define one vertex (color) - 3 float
rlColor3f :: Float -> Float -> Float -> IO ()
rlColor3f r g b = c'rlColor3f (realToFrac r) (realToFrac g) (realToFrac b)

-- | Define one vertex (color) - 4 float
rlColor4f :: Float -> Float -> Float -> Float -> IO ()
rlColor4f r g b a = c'rlColor4f (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | Enable vertex array (VAO, if supported)
rlEnableVertexArray :: Integer -> IO Bool
rlEnableVertexArray vaoId = toBool <$> c'rlEnableVertexArray (fromIntegral vaoId)

-- | Disable vertex array (VAO, if supported)
rlDisableVertexArray :: IO ()
rlDisableVertexArray = c'rlDisableVertexArray

-- | Enable vertex buffer (VBO)
rlEnableVertexBuffer :: Integer -> IO ()
rlEnableVertexBuffer vboId = c'rlEnableVertexBuffer (fromIntegral vboId)

-- | Disable vertex buffer (VBO)
rlDisableVertexBuffer :: IO ()
rlDisableVertexBuffer = c'rlDisableVertexBuffer

-- | Enable vertex buffer element (VBO element)
rlEnableVertexBufferElement :: Integer -> IO ()
rlEnableVertexBufferElement vboeId = c'rlEnableVertexBufferElement (fromIntegral vboeId)

-- | Disable vertex buffer element (VBO element)
rlDisableVertexBufferElement :: IO ()
rlDisableVertexBufferElement = c'rlDisableVertexBufferElement

-- | Enable vertex attribute index
rlEnableVertexAttribute :: Integer -> IO ()
rlEnableVertexAttribute index = c'rlEnableVertexAttribute (fromIntegral index)

-- | Disable vertex attribute index
rlDisableVertexAttribute :: Integer -> IO ()
rlDisableVertexAttribute index = c'rlDisableVertexAttribute (fromIntegral index)

-- OpenGL 1.1 only, not implemented
-- -- | Enable attribute state pointer
-- rlEnableStatePointer :: Int -> Ptr () -> IO ()

-- -- | Disable attribute state pointer
-- rlDisableStatePointer :: Int -> IO ()

-- | Select and active a texture slot
rlActiveTextureSlot :: Int -> IO ()
rlActiveTextureSlot slot = c'rlActiveTextureSlot (fromIntegral slot)

-- | Enable texture
rlEnableTexture :: Integer -> IO ()
rlEnableTexture tId = c'rlEnableTexture (fromIntegral tId)

-- | Disable texture
rlDisableTexture :: IO ()
rlDisableTexture = c'rlDisableTexture

-- | Enable texture cubemap
rlEnableTextureCubemap :: Integer -> IO ()
rlEnableTextureCubemap tId = c'rlEnableTextureCubemap (fromIntegral tId)

-- | Disable texture cubemap
rlDisableTextureCubemap :: IO ()
rlDisableTextureCubemap = c'rlDisableTextureCubemap

-- | Set texture parameters (filter, wrap)
rlTextureParameters :: Integer -> RLTextureParam -> Int -> IO ()
rlTextureParameters tId param value = c'rlTextureParameters (fromIntegral tId) (fromIntegral $ fromEnum param) (fromIntegral value)

-- | Set cubemap parameters (filter, wrap)
rlCubemapParameters :: Integer -> RLTextureParam -> Int -> IO ()
rlCubemapParameters tId param value = c'rlCubemapParameters (fromIntegral tId) (fromIntegral $ fromEnum param) (fromIntegral value)

-- | Enable shader program
rlEnableShader :: Integer -> IO ()
rlEnableShader sId = c'rlEnableShader (fromIntegral sId)

-- | Disable shader program
rlDisableShader :: IO ()
rlDisableShader = c'rlDisableShader

-- | Enable render texture (fbo)
rlEnableFramebuffer :: Integer -> IO ()
rlEnableFramebuffer fboId = c'rlEnableFramebuffer (fromIntegral fboId)

-- | Disable render texture (fbo), return to default framebuffer
rlDisableFramebuffer :: IO ()
rlDisableFramebuffer = c'rlDisableFramebuffer

-- | Get the currently active render texture (fbo), 0 for default framebuffer
rlGetActiveFramebuffer :: IO Integer
rlGetActiveFramebuffer = fromIntegral <$> c'rlGetActiveFramebuffer

-- | Activate multiple draw color buffers
rlActiveDrawBuffers :: Int -> IO ()
rlActiveDrawBuffers count = c'rlActiveDrawBuffers (fromIntegral count)

-- | Blit active framebuffer to main framebuffer
rlBlitFramebuffer :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [RLBitField] -> IO ()
rlBlitFramebuffer srcX srcY srcWidth srcHeight dstX dstY dstWidth dstHeight bufferMask =
  c'rlBlitFramebuffer (fromIntegral srcX) (fromIntegral srcY) (fromIntegral srcWidth) (fromIntegral srcHeight) (fromIntegral dstX) (fromIntegral dstY) (fromIntegral dstWidth) (fromIntegral dstHeight) (fromIntegral (configsToBitflag bufferMask))

-- | Bind framebuffer (FBO)
rlBindFramebuffer :: Integer -> Integer -> IO ()
rlBindFramebuffer target framebuffer = c'rlBindFramebuffer (fromIntegral target) (fromIntegral framebuffer)

-- | Enable color blending
rlEnableColorBlend :: IO ()
rlEnableColorBlend = c'rlEnableColorBlend

-- | Disable color blending
rlDisableColorBlend :: IO ()
rlDisableColorBlend = c'rlDisableColorBlend

-- | Enable depth test
rlEnableDepthTest :: IO ()
rlEnableDepthTest = c'rlEnableDepthTest

-- | Disable depth test
rlDisableDepthTest :: IO ()
rlDisableDepthTest = c'rlDisableDepthTest

-- | Enable depth write
rlEnableDepthMask :: IO ()
rlEnableDepthMask = c'rlEnableDepthMask

-- | Disable depth write
rlDisableDepthMask :: IO ()
rlDisableDepthMask = c'rlDisableDepthMask

-- | Enable backface culling
rlEnableBackfaceCulling :: IO ()
rlEnableBackfaceCulling = c'rlEnableBackfaceCulling

-- | Disable backface culling
rlDisableBackfaceCulling :: IO ()
rlDisableBackfaceCulling = c'rlDisableBackfaceCulling

-- | Color mask control
rlColorMask :: Bool -> Bool -> Bool -> Bool -> IO ()
rlColorMask r g b a = c'rlColorMask (fromBool r) (fromBool g) (fromBool b) (fromBool a)

-- | Set face culling mode
rlSetCullFace :: RLCullMode -> IO ()
rlSetCullFace mode = c'rlSetCullFace (fromIntegral $ fromEnum mode)

-- | Enable scissor test
rlEnableScissorTest :: IO ()
rlEnableScissorTest = c'rlEnableScissorTest

-- | Disable scissor test
rlDisableScissorTest :: IO ()
rlDisableScissorTest = c'rlDisableScissorTest

-- | Scissor test
rlScissor :: Int -> Int -> Int -> Int -> IO ()
rlScissor x y width height = c'rlScissor (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

-- | Enable wire mode
rlEnableWireMode :: IO ()
rlEnableWireMode = c'rlEnableWireMode

-- | Enable point mode
rlEnablePointMode :: IO ()
rlEnablePointMode = c'rlEnablePointMode

-- | Disable wire and point mode
rlDisableWireMode :: IO ()
rlDisableWireMode = c'rlDisableWireMode

-- | Set the line drawing width
rlSetLineWidth :: Float -> IO ()
rlSetLineWidth width = c'rlSetLineWidth (realToFrac width)

-- | Get the line drawing width
rlGetLineWidth :: IO Float
rlGetLineWidth = realToFrac <$> c'rlGetLineWidth

-- | Enable line aliasing
rlEnableSmoothLines :: IO ()
rlEnableSmoothLines = c'rlEnableSmoothLines

-- | Disable line aliasing
rlDisableSmoothLines :: IO ()
rlDisableSmoothLines = c'rlDisableSmoothLines

-- | Enable stereo rendering
rlEnableStereoRender :: IO ()
rlEnableStereoRender = c'rlEnableStereoRender

-- | Disable stereo rendering
rlDisableStereoRender :: IO ()
rlDisableStereoRender = c'rlDisableStereoRender

-- | Check if stereo render is enabled
rlIsStereoRenderEnabled :: IO Bool
rlIsStereoRenderEnabled = toBool <$> c'rlIsStereoRenderEnabled

-- | Clear color buffer with color
rlClearColor :: Word8 -> Word8 -> Word8 -> Word8 -> IO ()
rlClearColor r g b a = c'rlClearColor (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

-- | Clear used screen buffers (color and depth)
rlClearScreenBuffers :: IO ()
rlClearScreenBuffers = c'rlClearScreenBuffers

-- | Check and log OpenGL error codes
rlCheckErrors :: IO ()
rlCheckErrors = c'rlCheckErrors

-- | Set blending mode
rlSetBlendMode :: RLBlendMode -> IO ()
rlSetBlendMode mode = c'rlSetBlendMode (fromIntegral $ fromEnum mode)

-- | Set blending mode factor and equation (using OpenGL factors)
rlSetBlendFactors :: Int -> Int -> Int -> IO ()
rlSetBlendFactors glSrcFactor glDstFactor glEquation = c'rlSetBlendFactors (fromIntegral glSrcFactor) (fromIntegral glDstFactor) (fromIntegral glEquation)

-- | Set blending mode factors and equations separately (using OpenGL factors)
rlSetBlendFactorsSeparate :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
rlSetBlendFactorsSeparate glSrcRGB glDstRGB glSrcAlpha glDstAlpha glEqRGB glEqAlpha =
  c'rlSetBlendFactorsSeparate (fromIntegral glSrcRGB) (fromIntegral glDstRGB) (fromIntegral glSrcAlpha) (fromIntegral glDstAlpha) (fromIntegral glEqRGB) (fromIntegral glEqAlpha)

-- | Initialize rlgl (buffers, shaders, textures, states)
rlglInit :: Int -> Int -> IO ()
rlglInit width height = c'rlglInit (fromIntegral width) (fromIntegral height)

-- | De-initialize rlgl (buffers, shaders, textures)
rlglClose :: IO ()
rlglClose = c'rlglClose

-- | Load OpenGL extensions (loader function required)
rlLoadExtensions :: Ptr () -> IO ()
rlLoadExtensions = c'rlLoadExtensions

-- | Get current OpenGL version
rlGetVersion :: IO Int
rlGetVersion = fromIntegral <$> c'rlGetVersion

-- | Set current framebuffer width
rlSetFramebufferWidth :: Int -> IO ()
rlSetFramebufferWidth width = c'rlSetFramebufferWidth (fromIntegral width)

-- | Get default framebuffer width
rlGetFramebufferWidth :: IO Int
rlGetFramebufferWidth = fromIntegral <$> c'rlGetFramebufferWidth

-- | Set current framebuffer height
rlSetFramebufferHeight :: Int -> IO ()
rlSetFramebufferHeight height = c'rlSetFramebufferHeight (fromIntegral height)

-- | Get default framebuffer height
rlGetFramebufferHeight :: IO Int
rlGetFramebufferHeight = fromIntegral <$> c'rlGetFramebufferHeight

-- | Get default texture id
rlGetTextureIdDefault :: IO Integer
rlGetTextureIdDefault = fromIntegral <$> c'rlGetTextureIdDefault

-- | Get default shader id
rlGetShaderIdDefault :: IO Integer
rlGetShaderIdDefault = fromIntegral <$> c'rlGetShaderIdDefault

-- | Get default shader locations
rlGetShaderLocsDefault :: IO [Int]
rlGetShaderLocsDefault = map fromIntegral <$> (popCArray 32 =<< c'rlGetShaderLocsDefault)

-- | Load a render batch system
rlLoadRenderBatch :: Int -> Int -> IO RLRenderBatch
rlLoadRenderBatch numBuffers bufferElements = c'rlLoadRenderBatch (fromIntegral numBuffers) (fromIntegral bufferElements) >>= pop

-- | Unload render batch system
rlUnloadRenderBatch :: RLRenderBatch -> IO ()
rlUnloadRenderBatch batch = withFreeable batch c'rlUnloadRenderBatch

-- | Draw render batch data (Update->Draw->Reset)
rlDrawRenderBatch :: RLRenderBatch -> IO RLRenderBatch
rlDrawRenderBatch batch = withFreeable batch (\p -> c'rlDrawRenderBatch p >> peek p)

-- | Set the active render batch for rlgl (NULL for default internal)
rlSetRenderBatchActive :: Maybe RLRenderBatch -> IO ()
rlSetRenderBatchActive Nothing = c'rlSetRenderBatchActive nullPtr
rlSetRenderBatchActive (Just val) = do
  ptr <- malloc
  poke ptr val
  c'rlSetRenderBatchActive ptr

-- | Update and draw internal render batch
rlDrawRenderBatchActive :: IO ()
rlDrawRenderBatchActive = c'rlDrawRenderBatchActive

-- | Check internal buffer overflow for a given number of vertex
rlCheckRenderBatchLimit :: Int -> IO Bool
rlCheckRenderBatchLimit vCount = toBool <$> c'rlCheckRenderBatchLimit (fromIntegral vCount)

-- | Set current texture for render batch and check buffers limits
rlSetTexture :: Integer -> IO ()
rlSetTexture tId = c'rlSetTexture (fromIntegral tId)

-- | Load vertex array (vao) if supported
rlLoadVertexArray :: IO Integer
rlLoadVertexArray = fromIntegral <$> c'rlLoadVertexArray

-- | Load a vertex buffer attribute
rlLoadVertexBuffer :: (Freeable a, Storable a) => [a] -> Int -> Bool -> IO Integer
rlLoadVertexBuffer buffer size dynamic =
  fromIntegral <$> withFreeableArray buffer (\p -> c'rlLoadVertexBuffer (castPtr p) (fromIntegral size) (fromBool dynamic))

-- | Load a new attributes element buffer (typically the buffer data will be a list of `Int`s)
rlLoadVertexBufferElement :: (Freeable a, Storable a) => [a] -> Int -> Bool -> IO Integer
rlLoadVertexBufferElement buffer size dynamic =
  fromIntegral <$> withFreeableArray buffer (\p -> c'rlLoadVertexBufferElement (castPtr p) (fromIntegral size) (fromBool dynamic))

-- | Update GPU buffer with new data.
-- WARNING: Fails on empty list
rlUpdateVertexBuffer :: (Freeable a, Storable a) => Integer -> [a] -> Int -> Int -> IO ()
rlUpdateVertexBuffer bufferId bufferData size offset =
  withFreeableArray bufferData (\p -> c'rlUpdateVertexBuffer (fromIntegral bufferId) (castPtr p) (fromIntegral size) (fromIntegral offset))

-- | Update vertex buffer elements with new data (typically the buffer data will be a list of `Int`s).
-- WARNING: Fails on empty list
rlUpdateVertexBufferElements :: (Freeable a, Storable a) => Integer -> [a] -> Int -> Int -> IO ()
rlUpdateVertexBufferElements bufferId bufferData size offset =
  withFreeableArray bufferData (\p -> c'rlUpdateVertexBufferElements (fromIntegral bufferId) (castPtr p) (fromIntegral size) (fromIntegral offset))

-- | Unload vertex array object (VAO)
rlUnloadVertexArray :: Integer -> IO ()
rlUnloadVertexArray vaoId = c'rlUnloadVertexArray (fromIntegral vaoId)

-- | Unload vertex buffer (VBO)
rlUnloadVertexBuffer :: Integer -> IO ()
rlUnloadVertexBuffer vboId = c'rlUnloadVertexBuffer (fromIntegral vboId)

-- TODO: improve types for the functions below

-- | Set vertex attribute (the type must be a valid GLenum value)
rlSetVertexAttribute :: Integer -> Int -> Int -> Bool -> Int -> Ptr () -> IO ()
rlSetVertexAttribute index compSize aType normalized stride =
  c'rlSetVertexAttribute (fromIntegral index) (fromIntegral compSize) (fromIntegral aType) (fromBool normalized) (fromIntegral stride)

-- | Set vertex attribute divisor
rlSetVertexAttributeDivisor :: Integer -> Int -> IO ()
rlSetVertexAttributeDivisor index divisor = c'rlSetVertexAttributeDivisor (fromIntegral index) (fromIntegral divisor)

-- | Set vertex attribute default value
rlSetVertexAttributeDefault :: Int -> Ptr () -> Int -> Int -> IO ()
rlSetVertexAttributeDefault locIndex value attribType count =
  c'rlSetVertexAttributeDefault (fromIntegral locIndex) value (fromIntegral attribType) (fromIntegral count)

-- | Draw vertex array
rlDrawVertexArray :: Int -> Int -> IO ()
rlDrawVertexArray offset count = c'rlDrawVertexArray (fromIntegral offset) (fromIntegral count)

-- | Draw vertex array elements
rlDrawVertexArrayElements :: Int -> [Int] -> IO ()
rlDrawVertexArrayElements offset buffer =
  withFreeableArray
    (map fromIntegral buffer :: [CUShort])
    (c'rlDrawVertexArrayElements (fromIntegral offset) (fromIntegral $ length buffer) . castPtr)

-- | Draw vertex array instanced
rlDrawVertexArrayInstanced :: Int -> Int -> Int -> IO ()
rlDrawVertexArrayInstanced offset count instances = c'rlDrawVertexArrayInstanced (fromIntegral offset) (fromIntegral count) (fromIntegral instances)

-- | Draw vertex array elements instanced
rlDrawVertexArrayElementsInstanced :: Int -> [Int] -> Int -> IO ()
rlDrawVertexArrayElementsInstanced offset buffer instances =
  withFreeableArray
    (map fromIntegral buffer :: [CUShort])
    ( \p ->
        c'rlDrawVertexArrayElementsInstanced (fromIntegral offset) (fromIntegral $ length buffer) (castPtr p) (fromIntegral instances)
    )

-- | Load texture in GPU
rlLoadTexture :: [Int] -> Int -> Int -> RLPixelFormat -> Int -> IO Integer
rlLoadTexture tData width height format mipmapCount =
  fromIntegral
    <$> withFreeableArray
      (map fromIntegral tData :: [CUShort])
      (\p -> c'rlLoadTexture (castPtr p) (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) (fromIntegral mipmapCount))

-- | Load depth texture/renderbuffer (to be attached to fbo)
rlLoadTextureDepth :: Int -> Int -> Bool -> IO Integer
rlLoadTextureDepth width height useRenderBuffer = fromIntegral <$> c'rlLoadTextureDepth (fromIntegral width) (fromIntegral height) (fromBool useRenderBuffer)

-- | Load texture cubemap
rlLoadTextureCubemap :: [Int] -> RLPixelFormat -> IO Integer
rlLoadTextureCubemap tData format =
  fromIntegral
    <$> withFreeableArrayLen (map fromIntegral tData :: [CUShort]) (\l p -> c'rlLoadTextureCubemap (castPtr p) (fromIntegral $ l * sizeOf (0 :: CUShort)) (fromIntegral $ fromEnum format))

-- | Update GPU texture with new data
rlUpdateTexture :: (Freeable a, Storable a) => Integer -> Int -> Int -> Int -> Int -> RLPixelFormat -> [a] -> IO ()
rlUpdateTexture tId offsetX offsetY width height format tData =
  withFreeableArray tData (c'rlUpdateTexture (fromIntegral tId) (fromIntegral offsetX) (fromIntegral offsetY) (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) . castPtr)

-- | Get OpenGL internal formats
rlGetGlTextureFormats ::
  RLPixelFormat ->
  -- | Return type as tuple: (glInternalFormat, glFormat, glType)
  IO (Integer, Integer, Integer)
rlGetGlTextureFormats format =
  withFreeable
    (0 :: CUInt)
    ( \gif ->
        withFreeable
          (0 :: CUInt)
          ( \gf ->
              withFreeable
                (0 :: CUInt)
                ( \gt -> do
                    c'rlGetGlTextureFormats (fromIntegral $ fromEnum format) gif gf gt
                    glInternalFormat <- fromIntegral <$> peek gif
                    glFormat <- fromIntegral <$> peek gf
                    glType <- fromIntegral <$> peek gt
                    return (glInternalFormat, glFormat, glType)
                )
          )
    )

-- | Get name string for pixel format
rlGetPixelFormatName :: RLPixelFormat -> String
rlGetPixelFormatName format =
  case format of
    RLPixelFormatUncompressedGrayscale -> "GRAYSCALE"
    RLPixelFormatUncompressedGrayAlpha -> "GRAY_ALPHA"
    RLPixelFormatUncompressedR5G6B5 -> "R5G6B5"
    RLPixelFormatUncompressedR8G8B8 -> "R8G8B8"
    RLPixelFormatUncompressedR5G5B5A1 -> "R5G5B5A1"
    RLPixelFormatUncompressedR4G4B4A4 -> "R4G4B4A4"
    RLPixelFormatUncompressedR8G8B8A8 -> "R8G8B8A8"
    RLPixelFormatUncompressedR32 -> "R32"
    RLPixelFormatUncompressedR32G32B32 -> "R32G32B32"
    RLPixelFormatUncompressedR32G32B32A32 -> "R32G32B32A32"
    RLPixelFormatUncompressedR16 -> "R16"
    RLPixelFormatUncompressedR16G16B16 -> "R16G16B16"
    RLPixelFormatUncompressedR16G16B16A16 -> "R16G16B16A16"
    RLPixelFormatCompressedDxt1Rgb -> "DXT1_RGB"
    RLPixelFormatCompressedDxt1Rgba -> "DXT1_RGBA"
    RLPixelFormatCompressedDxt3Rgba -> "DXT3_RGBA"
    RLPixelFormatCompressedDxt5Rgba -> "DXT5_RGBA"
    RLPixelFormatCompressedEtc1Rgb -> "ETC1_RGB"
    RLPixelFormatCompressedEtc2Rgb -> "ETC2_RGB"
    RLPixelFormatCompressedEtc2EacRgba -> "ETC2_RGBA"
    RLPixelFormatCompressedPvrtRgb -> "PVRT_RGB"
    RLPixelFormatCompressedPvrtRgba -> "PVRT_RGBA"
    RLPixelFormatCompressedAstc4x4Rgba -> "ASTC_4x4_RGBA"
    RLPixelFormatCompressedAstc8x8Rgba -> "ASTC_8x8_RGBA"

-- | Unload texture from GPU memory
rlUnloadTexture :: Integer -> IO ()
rlUnloadTexture tId = c'rlUnloadTexture (fromIntegral tId)

-- | Generate mipmap data for selected texture
rlGenTextureMipmaps ::
  Integer ->
  Int ->
  Int ->
  RLPixelFormat ->
  -- | The number of mipmaps generated
  IO Int
rlGenTextureMipmaps tId width height format =
  fromIntegral <$> withFreeable (0 :: CInt) (\p -> c'rlGenTextureMipmaps (fromIntegral tId) (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format) p >> peek p)

-- | Read texture pixel data
rlReadTexturePixels :: Integer -> Int -> Int -> RLPixelFormat -> IO [Word8]
rlReadTexturePixels tId width height format = do
  ptr <- c'rlReadTexturePixels (fromIntegral tId) (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format)
  size <- fromIntegral <$> c'rlGetPixelDataSize (fromIntegral width) (fromIntegral height) (fromIntegral $ fromEnum format)
  map fromIntegral <$> popCArray size (castPtr ptr :: Ptr CUChar)

-- | Read screen pixel data (color buffer)
rlReadScreenPixels :: Int -> Int -> IO [Word8]
rlReadScreenPixels width height =
  map fromIntegral <$> (c'rlReadScreenPixels (fromIntegral width) (fromIntegral height) >>= popCArray (width * height * 4))

-- | Load an empty framebuffer
rlLoadFramebuffer :: IO Integer
rlLoadFramebuffer = fromIntegral <$> c'rlLoadFramebuffer

-- | Attach texture/renderbuffer to a framebuffer
rlFramebufferAttach :: Integer -> Integer -> RLFramebufferAttachType -> RLFramebufferAttachTextureType -> Int -> IO ()
rlFramebufferAttach fboId texId attachType texType mipLevel =
  c'rlFramebufferAttach (fromIntegral fboId) (fromIntegral texId) (fromIntegral $ fromEnum attachType) (fromIntegral $ fromEnum texType) (fromIntegral mipLevel)

-- | Verify framebuffer is complete
rlFramebufferComplete :: Integer -> IO Bool
rlFramebufferComplete fboId = toBool <$> c'rlFramebufferComplete (fromIntegral fboId)

-- | Delete framebuffer from GPU
rlUnloadFramebuffer :: Integer -> IO ()
rlUnloadFramebuffer fboId = c'rlUnloadFramebuffer (fromIntegral fboId)

-- | Load shader from code strings
rlLoadShaderCode :: String -> String -> IO Integer
rlLoadShaderCode vsCode fsCode =
  fromIntegral <$> withCString vsCode (withCString fsCode . c'rlLoadShaderCode)

-- | Compile custom shader and return shader id
rlCompileShader :: String -> RLShaderType -> IO Integer
rlCompileShader shaderCode shaderType =
  fromIntegral <$> withCString shaderCode (\s -> c'rlCompileShader s (fromIntegral $ fromEnum shaderType))

-- | Load custom shader program
rlLoadShaderProgram :: Integer -> Integer -> IO Integer
rlLoadShaderProgram vsShaderId fsShaderId =
  fromIntegral <$> c'rlLoadShaderProgram (fromIntegral vsShaderId) (fromIntegral fsShaderId)

-- | Unload shader program
rlUnloadShaderProgram :: Integer -> IO ()
rlUnloadShaderProgram shaderId = c'rlUnloadShaderProgram (fromIntegral shaderId)

-- | Get shader location uniform
rlGetLocationUniform :: Integer -> String -> IO Int
rlGetLocationUniform shaderId uniformName =
  fromIntegral <$> withCString uniformName (c'rlGetLocationUniform (fromIntegral shaderId))

-- | Get shader location attribute
rlGetLocationAttrib :: Integer -> String -> IO Int
rlGetLocationAttrib shaderId attribName =
  fromIntegral <$> withCString attribName (c'rlGetLocationAttrib (fromIntegral shaderId))

-- | Set shader value uniform
rlSetUniform :: Int -> ShaderUniformDataV -> IO ()
rlSetUniform locIndex value = do
  (dataType, ptr, count) <- unpackShaderUniformDataV value
  c'rlSetUniform (fromIntegral locIndex) ptr (fromIntegral $ fromEnum dataType) (fromIntegral count)

-- | Set shader value matrix
rlSetUniformMatrix :: Int -> Matrix -> IO ()
rlSetUniformMatrix locIndex mat = withFreeable mat (c'rlSetUniformMatrix (fromIntegral locIndex))

-- | Set shader value sampler
rlSetUniformSampler :: Int -> Integer -> IO ()
rlSetUniformSampler locIndex textureId = c'rlSetUniformSampler (fromIntegral locIndex) (fromIntegral textureId)

-- | Set shader currently active (id and locations)
rlSetShader :: Integer -> [Int] -> IO ()
rlSetShader shaderId locs = withFreeableArray (map fromIntegral locs :: [CInt]) (c'rlSetShader (fromIntegral shaderId))

-- | Load compute shader program
rlLoadComputeShaderProgram :: Integer -> IO Integer
rlLoadComputeShaderProgram shaderId = fromIntegral <$> c'rlLoadComputeShaderProgram (fromIntegral shaderId)

-- | Dispatch compute shader (equivalent to *draw* for graphics pipeline)
rlComputeShaderDispatch :: Integer -> Integer -> Integer -> IO ()
rlComputeShaderDispatch groupX groupY groupZ =
  c'rlComputeShaderDispatch (fromIntegral groupX) (fromIntegral groupY) (fromIntegral groupZ)

-- | Load shader storage buffer object (SSBO).
-- WARNING: Fails if list is empty
rlLoadShaderBuffer :: (Freeable a, Storable a) => Integer -> [a] -> RLBufferHint -> IO Integer
rlLoadShaderBuffer size bufferData hint =
  fromIntegral <$> withFreeableArray bufferData (\p -> c'rlLoadShaderBuffer (fromIntegral size) (castPtr p) (fromIntegral $ fromEnum hint))

-- | Unload shader storage buffer object (SSBO)
rlUnloadShaderBuffer :: Integer -> IO ()
rlUnloadShaderBuffer ssboId = c'rlUnloadShaderBuffer (fromIntegral ssboId)

-- | Update SSBO buffer data
rlUpdateShaderBuffer :: (Freeable a, Storable a) => Integer -> a -> Integer -> IO ()
rlUpdateShaderBuffer ssboId sbData offset =
  withFreeable sbData (\p -> c'rlUpdateShaderBuffer (fromIntegral ssboId) (castPtr p) (fromIntegral $ sizeOf sbData) (fromIntegral offset))

-- | Bind SSBO buffer
rlBindShaderBuffer :: Integer -> Integer -> IO ()
rlBindShaderBuffer ssboId index = c'rlBindShaderBuffer (fromIntegral ssboId) (fromIntegral index)

-- Read SSBO buffer data (GPU->CPU)
-- TODO: bind; skipped because I'm not sure how to bind this correctly
-- rlReadShaderBuffer :: Integer -> Integer -> Integer -> IO (Ptr ())
-- rlReadShaderBuffer ssboId count offset = undefined

-- | Copy SSBO data between buffers
rlCopyShaderBuffer :: Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
rlCopyShaderBuffer destId srcId destOffset srcOffset count = c'rlCopyShaderBuffer (fromIntegral destId) (fromIntegral srcId) (fromIntegral destOffset) (fromIntegral srcOffset) (fromIntegral count)

-- | Get SSBO buffer size
rlGetShaderBufferSize :: Integer -> IO Integer
rlGetShaderBufferSize ssboId = fromIntegral <$> c'rlGetShaderBufferSize (fromIntegral ssboId)

-- | Bind image texture
rlBindImageTexture :: Integer -> Integer -> RLPixelFormat -> Bool -> IO ()
rlBindImageTexture tId index format readonly = c'rlBindImageTexture (fromIntegral tId) (fromIntegral index) (fromIntegral $ fromEnum format) (fromBool readonly)

-- | Get internal modelview matrix
rlGetMatrixModelview :: IO Matrix
rlGetMatrixModelview = c'rlGetMatrixModelview >>= pop

-- | Get internal projection matrix
rlGetMatrixProjection :: IO Matrix
rlGetMatrixProjection = c'rlGetMatrixProjection >>= pop

-- | Get internal accumulated transform matrix
rlGetMatrixTransform :: IO Matrix
rlGetMatrixTransform = c'rlGetMatrixTransform >>= pop

-- | Get internal projection matrix for stereo render (selected eye)
rlGetMatrixProjectionStereo :: Int -> IO Matrix
rlGetMatrixProjectionStereo eye = c'rlGetMatrixProjectionStereo (fromIntegral eye) >>= pop

-- | Get internal view offset matrix for stereo render (selected eye)
rlGetMatrixViewOffsetStereo :: Int -> IO Matrix
rlGetMatrixViewOffsetStereo eye = c'rlGetMatrixViewOffsetStereo (fromIntegral eye) >>= pop

-- | Set a custom projection matrix (replaces internal projection matrix)
rlSetMatrixProjection :: Matrix -> IO ()
rlSetMatrixProjection matrix = withFreeable matrix c'rlSetMatrixProjection

-- | Set a custom modelview matrix (replaces internal modelview matrix)
rlSetMatrixModelview :: Matrix -> IO ()
rlSetMatrixModelview matrix = withFreeable matrix c'rlSetMatrixModelview

-- | Set eyes projection matrices for stereo rendering
rlSetMatrixProjectionStereo :: Matrix -> Matrix -> IO ()
rlSetMatrixProjectionStereo right left = withFreeable right (withFreeable left . c'rlSetMatrixProjectionStereo)

-- | Set eyes view offsets matrices for stereo rendering
rlSetMatrixViewOffsetStereo :: Matrix -> Matrix -> IO ()
rlSetMatrixViewOffsetStereo right left = withFreeable right (withFreeable left . c'rlSetMatrixViewOffsetStereo)

-- | Load and draw a cube
rlLoadDrawCube :: IO ()
rlLoadDrawCube = c'rlLoadDrawCube

-- | Load and draw a quad
rlLoadDrawQuad :: IO ()
rlLoadDrawQuad = c'rlLoadDrawQuad
