/**
 * See rlgl_bindings.h
 */

#include "rlgl_bindings.h"

RLBIND rlRenderBatch *rlLoadRenderBatch_(int numBuffers, int bufferElements)
{
  rlRenderBatch *ptr = (rlRenderBatch *)malloc(sizeof(rlRenderBatch));
  *ptr = rlLoadRenderBatch(numBuffers, bufferElements);
  return ptr;
}

RLBIND void rlUnloadRenderBatch_(rlRenderBatch *batch)
{
  rlUnloadRenderBatch(*batch);
}

RLBIND void rlSetUniformMatrix_(int locIndex, Matrix *mat)
{
  rlSetUniformMatrix(locIndex, *mat);
}

RLBIND Matrix *rlGetMatrixProjectionStereo_(int eye)
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixProjectionStereo(eye);
  return ptr;
}

RLBIND Matrix *rlGetMatrixViewOffsetStereo_(int eye)
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixViewOffsetStereo(eye);
  return ptr;
}

RLBIND void rlSetMatrixProjection_(Matrix *proj)
{
  rlSetMatrixProjection(*proj);
}

RLBIND void rlSetMatrixModelview_(Matrix *view)
{
  rlSetMatrixModelview(*view);
}

RLBIND void rlSetMatrixProjectionStereo_(Matrix *right, Matrix *left)
{
  rlSetMatrixProjectionStereo(*right, *left);
}

RLBIND void rlSetMatrixViewOffsetStereo_(Matrix *right, Matrix *left)
{
  rlSetMatrixViewOffsetStereo(*right, *left);
}

RLBIND Matrix *rlGetMatrixModelview_()
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixModelview();
  return ptr;
}

RLBIND Matrix *rlGetMatrixProjection_()
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixProjection();
  return ptr;
}

RLBIND Matrix *rlGetMatrixTransform_()
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixTransform();
  return ptr;
}

RLBIND void rlMatrixMode_(int a)
{
  rlMatrixMode(a);
}

RLBIND void rlTranslatef_(float a, float b, float c)
{
  rlTranslatef(a, b, c);
}

RLBIND void rlRotatef_(float a, float b, float c, float d)
{
  rlRotatef(a, b, c, d);
}

RLBIND void rlScalef_(float a, float b, float c)
{
  rlScalef(a, b, c);
}

RLBIND void rlMultMatrixf_(const float *a)
{
  rlMultMatrixf(a);
}

RLBIND void rlFrustum_(double a, double b, double c, double d, double e, double f)
{
  rlFrustum(a, b, c, d, e, f);
}

RLBIND void rlOrtho_(double a, double b, double c, double d, double e, double f)
{
  rlOrtho(a, b, c, d, e, f);
}

RLBIND void rlViewport_(int a, int b, int c, int d)
{
  rlViewport(a, b, c, d);
}

RLBIND void rlSetClipPlanes_(double a, double b)
{
  rlSetClipPlanes(a, b);
}

RLBIND double rlGetCullDistanceNear_() {
  return rlGetCullDistanceNear();
}

RLBIND double rlGetCullDistanceFar_() {
  return rlGetCullDistanceFar();
}

RLBIND void rlBegin_(int a)
{
  rlBegin(a);
}

RLBIND void rlVertex2i_(int a, int b)
{
  rlVertex2i(a, b);
}

RLBIND void rlVertex2f_(float a, float b)
{
  rlVertex2f(a, b);
}

RLBIND void rlVertex3f_(float a, float b, float c)
{
  rlVertex3f(a, b, c);
}

RLBIND void rlTexCoord2f_(float a, float b)
{
  rlTexCoord2f(a, b);
}

RLBIND void rlNormal3f_(float a, float b, float c)
{
  rlNormal3f(a, b, c);
}

RLBIND void rlColor4ub_(unsigned char a, unsigned char b, unsigned char c, unsigned char d)
{
  rlColor4ub(a, b, c, d);
}

RLBIND void rlColor3f_(float a, float b, float c)
{
  rlColor3f(a, b, c);
}

RLBIND void rlColor4f_(float a, float b, float c, float d)
{
  rlColor4f(a, b, c, d);
}

RLBIND bool rlEnableVertexArray_(unsigned int a)
{
  return rlEnableVertexArray(a);
}

RLBIND void rlEnableVertexBuffer_(unsigned int a)
{
  rlEnableVertexBuffer(a);
}

RLBIND void rlEnableVertexBufferElement_(unsigned int a)
{
  rlEnableVertexBufferElement(a);
}

RLBIND void rlEnableVertexAttribute_(unsigned int a)
{
  rlEnableVertexAttribute(a);
}

RLBIND void rlDisableVertexAttribute_(unsigned int a)
{
  rlDisableVertexAttribute(a);
}

RLBIND void rlActiveTextureSlot_(int a)
{
  rlActiveTextureSlot(a);
}

RLBIND void rlEnableTexture_(unsigned int a)
{
  rlEnableTexture(a);
}

RLBIND void rlEnableTextureCubemap_(unsigned int a)
{
  rlEnableTextureCubemap(a);
}

RLBIND void rlTextureParameters_(unsigned int a, int b, int c)
{
  rlTextureParameters(a, b, c);
}

RLBIND void rlCubemapParameters_(unsigned int a, int b, int c)
{
  rlCubemapParameters(a, b, c);
}

RLBIND void rlEnableShader_(unsigned int a)
{
  rlEnableShader(a);
}

RLBIND void rlEnableFramebuffer_(unsigned int a)
{
  rlEnableFramebuffer(a);
}

RLBIND void rlActiveDrawBuffers_(int a)
{
  rlActiveDrawBuffers(a);
}

RLBIND void rlBlitFramebuffer_(int a, int b, int c, int d, int e, int f, int g, int h, int i)
{
  rlBlitFramebuffer(a, b, c, d, e, f, g, h, i);
}

RLBIND void rlBindFramebuffer_(unsigned int a, unsigned int b)
{
  rlBindFramebuffer(a, b);
}

RLBIND void rlColorMask_(bool a, bool b, bool c, bool d)
{
  rlColorMask(a, b, c, d);
}

RLBIND void rlSetCullFace_(int a)
{
  rlSetCullFace(a);
}

RLBIND void rlScissor_(int a, int b, int c, int d)
{
  rlScissor(a, b, c, d);
}

RLBIND void rlSetLineWidth_(float a)
{
  rlSetLineWidth(a);
}

RLBIND float rlGetLineWidth_()
{
  return rlGetLineWidth();
}

RLBIND bool rlIsStereoRenderEnabled_()
{
  return rlIsStereoRenderEnabled();
}

RLBIND void rlClearColor_(unsigned char a, unsigned char b, unsigned char c, unsigned char d)
{
  rlClearColor(a, b, c, d);
}

RLBIND void rlSetBlendMode_(int a)
{
  rlSetBlendMode(a);
}

RLBIND void rlSetBlendFactors_(int a, int b, int c)
{
  rlSetBlendFactors(a, b, c);
}

RLBIND void rlSetBlendFactorsSeparate_(int a, int b, int c, int d, int e, int f)
{
  rlSetBlendFactorsSeparate(a, b, c, d, e, f);
}

RLBIND void rlglInit_(int a, int b)
{
  rlglInit(a, b);
}

RLBIND void rlLoadExtensions_(void *a)
{
  rlLoadExtensions(a);
}

RLBIND int rlGetVersion_()
{
  return rlGetVersion();
}

RLBIND void rlSetFramebufferWidth_(int a)
{
  rlSetFramebufferWidth(a);
}

RLBIND int rlGetFramebufferWidth_()
{
  return rlGetFramebufferWidth();
}

RLBIND void rlSetFramebufferHeight_(int a)
{
  rlSetFramebufferHeight(a);
}

RLBIND int rlGetFramebufferHeight_()
{
  return rlGetFramebufferHeight();
}

RLBIND unsigned int rlGetTextureIdDefault_()
{
  return rlGetTextureIdDefault();
}

RLBIND unsigned int rlGetShaderIdDefault_()
{
  return rlGetShaderIdDefault();
}

RLBIND int *rlGetShaderLocsDefault_()
{
  return rlGetShaderLocsDefault();
}

RLBIND void rlDrawRenderBatch_(rlRenderBatch *a)
{
  rlDrawRenderBatch(a);
}

RLBIND void rlSetRenderBatchActive_(rlRenderBatch *a)
{
  rlSetRenderBatchActive(a);
}

RLBIND bool rlCheckRenderBatchLimit_(int a)
{
  return rlCheckRenderBatchLimit(a);
}

RLBIND void rlSetTexture_(unsigned int a)
{
  rlSetTexture(a);
}

RLBIND unsigned int rlLoadVertexArray_()
{
  return rlLoadVertexArray();
}

RLBIND unsigned int rlLoadVertexBuffer_(const void *a, int b, bool c)
{
  return rlLoadVertexBuffer(a, b, c);
}

RLBIND unsigned int rlLoadVertexBufferElement_(const void *a, int b, bool c)
{
  return rlLoadVertexBufferElement(a, b, c);
}

RLBIND void rlUpdateVertexBuffer_(unsigned int a, const void *b, int c, int d)
{
  rlUpdateVertexBuffer(a, b, c, d);
}

RLBIND void rlUpdateVertexBufferElements_(unsigned int a, const void *b, int c, int d)
{
  rlUpdateVertexBufferElements(a, b, c, d);
}

RLBIND void rlUnloadVertexArray_(unsigned int a)
{
  rlUnloadVertexArray(a);
}

RLBIND void rlUnloadVertexBuffer_(unsigned int a)
{
  rlUnloadVertexBuffer(a);
}

RLBIND void rlSetVertexAttribute_(unsigned int a, int b, int c, bool d, int e, int f)
{
  rlSetVertexAttribute(a, b, c, d, e, f);
}

RLBIND void rlSetVertexAttributeDivisor_(unsigned int a, int b)
{
  rlSetVertexAttributeDivisor(a, b);
}

RLBIND void rlSetVertexAttributeDefault_(int a, const void *b, int c, int d)
{
  rlSetVertexAttributeDefault(a, b, c, d);
}

RLBIND void rlDrawVertexArray_(int a, int b)
{
  rlDrawVertexArray(a, b);
}

RLBIND void rlDrawVertexArrayElements_(int a, int b, const void *c)
{
  rlDrawVertexArrayElements(a, b, c);
}

RLBIND void rlDrawVertexArrayInstanced_(int a, int b, int c)
{
  rlDrawVertexArrayInstanced(a, b, c);
}

RLBIND void rlDrawVertexArrayElementsInstanced_(int a, int b, const void *c, int d)
{
  rlDrawVertexArrayElementsInstanced(a, b, c, d);
}

RLBIND unsigned int rlLoadTexture_(const void *a, int b, int c, int d, int e)
{
  return rlLoadTexture(a, b, c, d, e);
}

RLBIND unsigned int rlLoadTextureDepth_(int a, int b, bool c)
{
  return rlLoadTextureDepth(a, b, c);
}

RLBIND unsigned int rlLoadTextureCubemap_(const void *a, int b, int c)
{
  return rlLoadTextureCubemap(a, b, c);
}

RLBIND void rlUpdateTexture_(unsigned int a, int b, int c, int d, int e, int f, const void *g)
{
  rlUpdateTexture(a, b, c, d, e, f, g);
}

RLBIND void rlGetGlTextureFormats_(int a, unsigned int *b, unsigned int *c, unsigned int *d)
{
  rlGetGlTextureFormats(a, b, c, d);
}

RLBIND const char *rlGetPixelFormatName_(unsigned int a)
{
  return rlGetPixelFormatName(a);
}

RLBIND void rlUnloadTexture_(unsigned int a)
{
  rlUnloadTexture(a);
}

RLBIND void rlGenTextureMipmaps_(unsigned int a, int b, int c, int d, int *e)
{
  rlGenTextureMipmaps(a, b, c, d, e);
}

RLBIND void *rlReadTexturePixels_(unsigned int a, int b, int c, int d)
{
  return rlReadTexturePixels(a, b, c, d);
}

RLBIND unsigned char *rlReadScreenPixels_(int a, int b)
{
  return rlReadScreenPixels(a, b);
}

RLBIND unsigned int rlLoadFramebuffer_()
{
  return rlLoadFramebuffer();
}

RLBIND void rlFramebufferAttach_(unsigned int a, unsigned int b, int c, int d, int e)
{
  rlFramebufferAttach(a, b, c, d, e);
}

RLBIND bool rlFramebufferComplete_(unsigned int a)
{
  return rlFramebufferComplete(a);
}

RLBIND void rlUnloadFramebuffer_(unsigned int a)
{
  rlUnloadFramebuffer(a);
}

RLBIND unsigned int rlLoadShaderCode_(const char *a, const char *b)
{
  return rlLoadShaderCode(a, b);
}

RLBIND unsigned int rlCompileShader_(const char *a, int b)
{
  return rlCompileShader(a, b);
}

RLBIND unsigned int rlLoadShaderProgram_(unsigned int a, unsigned int b)
{
  return rlLoadShaderProgram(a, b);
}

RLBIND void rlUnloadShaderProgram_(unsigned int a)
{
  rlUnloadShaderProgram(a);
}

RLBIND int rlGetLocationUniform_(unsigned int a, const char *b)
{
  return rlGetLocationUniform(a, b);
}

RLBIND int rlGetLocationAttrib_(unsigned int a, const char *b)
{
  return rlGetLocationAttrib(a, b);
}

RLBIND void rlSetUniform_(int a, const void *b, int c, int d)
{
  rlSetUniform(a, b, c, d);
}

RLBIND void rlSetUniformSampler_(int a, unsigned int b)
{
  rlSetUniformSampler(a, b);
}

RLBIND void rlSetShader_(unsigned int a, int *b)
{
  rlSetShader(a, b);
}

RLBIND unsigned int rlLoadComputeShaderProgram_(unsigned int a)
{
  return rlLoadComputeShaderProgram(a);
}

RLBIND void rlComputeShaderDispatch_(unsigned int a, unsigned int b, unsigned int c)
{
  rlComputeShaderDispatch(a, b, c);
}

RLBIND unsigned int rlLoadShaderBuffer_(unsigned int a, const void *b, int c)
{
  return rlLoadShaderBuffer(a, b, c);
}

RLBIND void rlUnloadShaderBuffer_(unsigned int a)
{
  rlUnloadShaderBuffer(a);
}

RLBIND void rlUpdateShaderBuffer_(unsigned int a, const void *b, unsigned int c, unsigned int d)
{
  rlUpdateShaderBuffer(a, b, c, d);
}

RLBIND void rlBindShaderBuffer_(unsigned int a, unsigned int b)
{
  rlBindShaderBuffer(a, b);
}

RLBIND void rlReadShaderBuffer_(unsigned int a, void *b, unsigned int c, unsigned int d)
{
  rlReadShaderBuffer(a, b, c, d);
}

RLBIND void rlCopyShaderBuffer_(unsigned int a, unsigned int b, unsigned int c, unsigned int d, unsigned int e)
{
  rlCopyShaderBuffer(a, b, c, d, e);
}

RLBIND unsigned int rlGetShaderBufferSize_(unsigned int a)
{
  return rlGetShaderBufferSize(a);
}

RLBIND void rlBindImageTexture_(unsigned int a, unsigned int b, int c, bool d)
{
  rlBindImageTexture(a, b, c, d);
}

RLBIND void rlPushMatrix_()
{
  rlPushMatrix();
}

RLBIND void rlPopMatrix_()
{
  rlPopMatrix();
}

RLBIND void rlLoadIdentity_()
{
  rlLoadIdentity();
}

RLBIND void rlEnd_()
{
  rlEnd();
}

RLBIND void rlDisableVertexArray_()
{
  rlDisableVertexArray();
}

RLBIND void rlDisableVertexBuffer_()
{
  rlDisableVertexBuffer();
}

RLBIND void rlDisableVertexBufferElement_()
{
  rlDisableVertexBufferElement();
}

RLBIND void rlDisableTexture_()
{
  rlDisableTexture();
}

RLBIND void rlDisableTextureCubemap_()
{
  rlDisableTextureCubemap();
}

RLBIND void rlDisableShader_()
{
  rlDisableShader();
}

RLBIND void rlDisableFramebuffer_()
{
  rlDisableFramebuffer();
}

RLBIND unsigned int rlGetActiveFramebuffer_()
{
  return rlGetActiveFramebuffer();
}

RLBIND void rlEnableColorBlend_()
{
  rlEnableColorBlend();
}

RLBIND void rlDisableColorBlend_()
{
  rlDisableColorBlend();
}

RLBIND void rlEnableDepthTest_()
{
  rlEnableDepthTest();
}

RLBIND void rlDisableDepthTest_()
{
  rlDisableDepthTest();
}

RLBIND void rlEnableDepthMask_()
{
  rlEnableDepthMask();
}

RLBIND void rlDisableDepthMask_()
{
  rlDisableDepthMask();
}

RLBIND void rlEnableBackfaceCulling_()
{
  rlEnableBackfaceCulling();
}

RLBIND void rlDisableBackfaceCulling_()
{
  rlDisableBackfaceCulling();
}

RLBIND void rlEnableScissorTest_()
{
  rlEnableScissorTest();
}

RLBIND void rlDisableScissorTest_()
{
  rlDisableScissorTest();
}

RLBIND void rlEnableWireMode_()
{
  rlEnableWireMode();
}

RLBIND void rlEnablePointMode_()
{
  rlEnablePointMode();
}

RLBIND void rlDisableWireMode_()
{
  rlDisableWireMode();
}

RLBIND void rlEnableSmoothLines_()
{
  rlEnableSmoothLines();
}

RLBIND void rlDisableSmoothLines_()
{
  rlDisableSmoothLines();
}

RLBIND void rlEnableStereoRender_()
{
  rlEnableStereoRender();
}

RLBIND void rlDisableStereoRender_()
{
  rlDisableStereoRender();
}

RLBIND void rlClearScreenBuffers_()
{
  rlClearScreenBuffers();
}

RLBIND void rlCheckErrors_()
{
  rlCheckErrors();
}

RLBIND void rlglClose_()
{
  rlglClose();
}

RLBIND void rlDrawRenderBatchActive_()
{
  rlDrawRenderBatchActive();
}

RLBIND void rlLoadDrawCube_()
{
  rlLoadDrawCube();
}

RLBIND void rlLoadDrawQuad_()
{
  rlLoadDrawQuad();
}
