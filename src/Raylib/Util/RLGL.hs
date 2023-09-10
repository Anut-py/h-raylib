{-# OPTIONS -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Raylib.Util.RLGL
  ( rlMatrixMode,
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
    rlEnableVertexArray,
    rlDisableVertexArray,
    rlEnableVertexBuffer,
    rlDisableVertexBuffer,
    rlEnableVertexBufferElement,
    rlDisableVertexBufferElement,
    rlEnableVertexAttribute,
    rlDisableVertexAttribute,
    rlActiveTextureSlot,
    rlEnableTexture,
    rlDisableTexture,
    rlEnableTextureCubemap,
    rlDisableTextureCubemap,
    rlTextureParameters,
    rlCubemapParameters,
    rlEnableShader,
    rlDisableShader,
    rlEnableFramebuffer,
    rlDisableFramebuffer,
    rlActiveDrawBuffers,
    rlEnableColorBlend,
    rlDisableColorBlend,
    rlEnableDepthTest,
    rlDisableDepthTest,
    rlEnableDepthMask,
    rlDisableDepthMask,
    rlEnableBackfaceCulling,
    rlDisableBackfaceCulling,
    rlSetCullFace,
    rlEnableScissorTest,
    rlDisableScissorTest,
    rlScissor,
    rlEnableWireMode,
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
    rlLoadRenderBatch,
    rlUnloadRenderBatch,
    rlDrawRenderBatch,
    rlSetRenderBatchActive,
    rlDrawRenderBatchActive,
    rlCheckRenderBatchLimit,
    rlSetTexture,
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
    rlLoadFramebuffer,
    rlFramebufferAttach,
    rlFramebufferComplete,
    rlUnloadFramebuffer,
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
    rlLoadComputeShaderProgram,
    rlComputeShaderDispatch,
    rlLoadShaderBuffer,
    rlUnloadShaderBuffer,
    rlUpdateShaderBuffer,
    rlBindShaderBuffer,
    rlCopyShaderBuffer,
    rlGetShaderBufferSize,
    rlBindImageTexture,
    rlGetMatrixModelview,
    rlGetMatrixProjection,
    rlGetMatrixTransform,
    rlGetMatrixProjectionStereo,
    rlGetMatrixViewOffsetStereo,
    rlSetMatrixProjection,
    rlSetMatrixModelview,
    rlSetMatrixProjectionStereo,
    rlSetMatrixViewOffsetStereo,
    rlLoadDrawCube,
    rlLoadDrawQuad,
  )
where

import Foreign
    ( Ptr,
      Storable(sizeOf, poke, peek),
      malloc,
      fromBool,
      toBool,
      castPtr,
      nullPtr, Word8 )
import Foreign.C ( CUInt, CInt, CUChar, withCString, CUShort )
import Raylib.ForeignUtil
    ( pop,
      popCArray,
      withFreeable,
      Freeable,
      withFreeableArray,
      withFreeableArrayLen )
import Raylib.Native
    ( c'rlSetMatrixViewOffsetStereo,
      c'rlSetMatrixProjectionStereo,
      c'rlSetMatrixModelview,
      c'rlSetMatrixProjection,
      c'rlGetMatrixViewOffsetStereo,
      c'rlGetMatrixProjectionStereo,
      c'rlGetMatrixTransform,
      c'rlGetMatrixProjection,
      c'rlGetMatrixModelview,
      c'rlBindImageTexture,
      c'rlGetShaderBufferSize,
      c'rlCopyShaderBuffer,
      c'rlBindShaderBuffer,
      c'rlUpdateShaderBuffer,
      c'rlUnloadShaderBuffer,
      c'rlLoadShaderBuffer,
      c'rlComputeShaderDispatch,
      c'rlLoadComputeShaderProgram,
      c'rlSetShader,
      c'rlSetUniformSampler,
      c'rlSetUniformMatrix,
      c'rlSetUniform,
      c'rlGetLocationAttrib,
      c'rlGetLocationUniform,
      c'rlUnloadShaderProgram,
      c'rlLoadShaderProgram,
      c'rlCompileShader,
      c'rlLoadShaderCode,
      c'rlUnloadFramebuffer,
      c'rlFramebufferComplete,
      c'rlFramebufferAttach,
      c'rlLoadFramebuffer,
      c'rlReadScreenPixels,
      c'rlReadTexturePixels,
      c'rlGenTextureMipmaps,
      c'rlUnloadTexture,
      c'rlGetGlTextureFormats,
      c'rlUpdateTexture,
      c'rlLoadTextureCubemap,
      c'rlLoadTextureDepth,
      c'rlLoadTexture,
      c'rlDrawVertexArrayElementsInstanced,
      c'rlDrawVertexArrayInstanced,
      c'rlDrawVertexArrayElements,
      c'rlDrawVertexArray,
      c'rlSetVertexAttributeDefault,
      c'rlSetVertexAttributeDivisor,
      c'rlSetVertexAttribute,
      c'rlUnloadVertexBuffer,
      c'rlUnloadVertexArray,
      c'rlUpdateVertexBufferElements,
      c'rlUpdateVertexBuffer,
      c'rlLoadVertexBufferElement,
      c'rlLoadVertexBuffer,
      c'rlLoadVertexArray,
      c'rlSetTexture,
      c'rlCheckRenderBatchLimit,
      c'rlSetRenderBatchActive,
      c'rlDrawRenderBatch,
      c'rlUnloadRenderBatch,
      c'rlLoadRenderBatch,
      c'rlGetShaderLocsDefault,
      c'rlGetShaderIdDefault,
      c'rlGetTextureIdDefault,
      c'rlGetFramebufferHeight,
      c'rlSetFramebufferHeight,
      c'rlGetFramebufferWidth,
      c'rlSetFramebufferWidth,
      c'rlGetVersion,
      c'rlLoadExtensions,
      c'rlglInit,
      c'rlSetBlendFactorsSeparate,
      c'rlSetBlendFactors,
      c'rlSetBlendMode,
      c'rlClearColor,
      c'rlIsStereoRenderEnabled,
      c'rlGetLineWidth,
      c'rlSetLineWidth,
      c'rlScissor,
      c'rlSetCullFace,
      c'rlActiveDrawBuffers,
      c'rlEnableFramebuffer,
      c'rlEnableShader,
      c'rlCubemapParameters,
      c'rlTextureParameters,
      c'rlEnableTextureCubemap,
      c'rlEnableTexture,
      c'rlActiveTextureSlot,
      c'rlDisableVertexAttribute,
      c'rlEnableVertexAttribute,
      c'rlEnableVertexBufferElement,
      c'rlEnableVertexBuffer,
      c'rlEnableVertexArray,
      c'rlColor4f,
      c'rlColor3f,
      c'rlColor4ub,
      c'rlNormal3f,
      c'rlTexCoord2f,
      c'rlVertex3f,
      c'rlVertex2f,
      c'rlVertex2i,
      c'rlBegin,
      c'rlViewport,
      c'rlOrtho,
      c'rlFrustum,
      c'rlMultMatrixf,
      c'rlScalef,
      c'rlRotatef,
      c'rlTranslatef,
      c'rlMatrixMode,
      c'rlGetPixelDataSize )
import Raylib.Types
    ( unpackShaderUniformDataV,
      Matrix,
      RLBlendMode,
      RLCullMode,
      RLFramebufferAttachTextureType,
      RLFramebufferAttachType,
      RLPixelFormat(..),
      RLRenderBatch,
      ShaderUniformDataV,
      RLMatrixMode,
      RLDrawMode,
      RLTextureParam,
      RLShaderType,
      RLBufferHint )

-- | Choose the current matrix to be transformed
rlMatrixMode :: RLMatrixMode -> IO ()
rlMatrixMode mode = c'rlMatrixMode (fromIntegral $ fromEnum mode)

-- | Push the current matrix to stack
foreign import ccall safe "rlgl.h rlPushMatrix" rlPushMatrix :: IO ()

-- | Pop latest inserted matrix from stack
foreign import ccall safe "rlgl.h rlPopMatrix" rlPopMatrix :: IO ()

-- | Reset current matrix to identity matrix
foreign import ccall safe "rlgl.h rlLoadIdentity" rlLoadIdentity :: IO ()

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
foreign import ccall safe "rlgl.h rlEnd" rlEnd :: IO ()

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
foreign import ccall safe "rlgl.h rlDisableVertexArray" rlDisableVertexArray :: IO ()

-- | Enable vertex buffer (VBO)
rlEnableVertexBuffer :: Integer -> IO ()
rlEnableVertexBuffer vboId = c'rlEnableVertexBuffer (fromIntegral vboId)

-- | Disable vertex buffer (VBO)
foreign import ccall safe "rlgl.h rlDisableVertexBuffer" rlDisableVertexBuffer :: IO ()

-- | Enable vertex buffer element (VBO element)
rlEnableVertexBufferElement :: Integer -> IO ()
rlEnableVertexBufferElement vboeId = c'rlEnableVertexBufferElement (fromIntegral vboeId)

-- | Disable vertex buffer element (VBO element)
foreign import ccall safe "rlgl.h rlDisableVertexBufferElement" rlDisableVertexBufferElement :: IO ()

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
foreign import ccall safe "rlgl.h rlDisableTexture" rlDisableTexture :: IO ()

-- | Enable texture cubemap
rlEnableTextureCubemap :: Integer -> IO ()
rlEnableTextureCubemap tId = c'rlEnableTextureCubemap (fromIntegral tId)

-- | Disable texture cubemap
foreign import ccall safe "rlgl.h rlDisableTextureCubemap" rlDisableTextureCubemap :: IO ()

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
foreign import ccall safe "rlgl.h rlDisableShader" rlDisableShader :: IO ()

-- | Enable render texture (fbo)
rlEnableFramebuffer :: Integer -> IO ()
rlEnableFramebuffer fboId = c'rlEnableFramebuffer (fromIntegral fboId)

-- | Disable render texture (fbo), return to default framebuffer
foreign import ccall safe "rlgl.h rlDisableFramebuffer" rlDisableFramebuffer :: IO ()

-- | Activate multiple draw color buffers
rlActiveDrawBuffers :: Int -> IO ()
rlActiveDrawBuffers count = c'rlActiveDrawBuffers (fromIntegral count)

-- | Enable color blending
foreign import ccall safe "rlgl.h rlEnableColorBlend" rlEnableColorBlend :: IO ()

-- | Disable color blending
foreign import ccall safe "rlgl.h rlDisableColorBlend" rlDisableColorBlend :: IO ()

-- | Enable depth test
foreign import ccall safe "rlgl.h rlEnableDepthTest" rlEnableDepthTest :: IO ()

-- | Disable depth test
foreign import ccall safe "rlgl.h rlDisableDepthTest" rlDisableDepthTest :: IO ()

-- | Enable depth write
foreign import ccall safe "rlgl.h rlEnableDepthMask" rlEnableDepthMask :: IO ()

-- | Disable depth write
foreign import ccall safe "rlgl.h rlDisableDepthMask" rlDisableDepthMask :: IO ()

-- | Enable backface culling
foreign import ccall safe "rlgl.h rlEnableBackfaceCulling" rlEnableBackfaceCulling :: IO ()

-- | Disable backface culling
foreign import ccall safe "rlgl.h rlDisableBackfaceCulling" rlDisableBackfaceCulling :: IO ()

-- | Set face culling mode
rlSetCullFace :: RLCullMode -> IO ()
rlSetCullFace mode = c'rlSetCullFace (fromIntegral $ fromEnum mode)

-- | Enable scissor test
foreign import ccall safe "rlgl.h rlEnableScissorTest" rlEnableScissorTest :: IO ()

-- | Disable scissor test
foreign import ccall safe "rlgl.h rlDisableScissorTest" rlDisableScissorTest :: IO ()

-- | Scissor test
rlScissor :: Int -> Int -> Int -> Int -> IO ()
rlScissor x y width height = c'rlScissor (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

-- | Enable wire mode
foreign import ccall safe "rlgl.h rlEnableWireMode" rlEnableWireMode :: IO ()

-- | Disable wire mode
foreign import ccall safe "rlgl.h rlDisableWireMode" rlDisableWireMode :: IO ()

-- | Set the line drawing width
rlSetLineWidth :: Float -> IO ()
rlSetLineWidth width = c'rlSetLineWidth (realToFrac width)

-- | Get the line drawing width
rlGetLineWidth :: IO Float
rlGetLineWidth = realToFrac <$> c'rlGetLineWidth

-- | Enable line aliasing
foreign import ccall safe "rlgl.h rlEnableSmoothLines" rlEnableSmoothLines :: IO ()

-- | Disable line aliasing
foreign import ccall safe "rlgl.h rlDisableSmoothLines" rlDisableSmoothLines :: IO ()

-- | Enable stereo rendering
foreign import ccall safe "rlgl.h rlEnableStereoRender" rlEnableStereoRender :: IO ()

-- | Disable stereo rendering
foreign import ccall safe "rlgl.h rlDisableStereoRender" rlDisableStereoRender :: IO ()

-- | Check if stereo render is enabled
rlIsStereoRenderEnabled :: IO Bool
rlIsStereoRenderEnabled = toBool <$> c'rlIsStereoRenderEnabled

-- | Clear color buffer with color
rlClearColor :: Word8 -> Word8 -> Word8 -> Word8 -> IO ()
rlClearColor r g b a = c'rlClearColor (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

-- | Clear used screen buffers (color and depth)
foreign import ccall safe "rlgl.h rlClearScreenBuffers" rlClearScreenBuffers :: IO ()

-- | Check and log OpenGL error codes
foreign import ccall safe "rlgl.h rlCheckErrors" rlCheckErrors :: IO ()

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
foreign import ccall safe "rlgl.h rlglClose" rlglClose :: IO ()

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
foreign import ccall safe "rlgl.h rlDrawRenderBatchActive" rlDrawRenderBatchActive :: IO ()

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
rlLoadFramebuffer :: Int -> Int -> IO Integer
rlLoadFramebuffer width height = fromIntegral <$> c'rlLoadFramebuffer (fromIntegral width) (fromIntegral height)

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
-- Skipped because I'm not sure how to bind this correctly
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
foreign import ccall safe "rlgl.h rlLoadDrawCube" rlLoadDrawCube :: IO ()

-- | Load and draw a quad
foreign import ccall safe "rlgl.h rlLoadDrawQuad" rlLoadDrawQuad :: IO ()
