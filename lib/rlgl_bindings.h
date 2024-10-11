/**
 * @file rlgl_bindings.h
 * @author Anut-py
 * @brief Required methods for binding Haskell to rlgl
 */

#include "rl_common.h"

rlRenderBatch *rlLoadRenderBatch_(int numBuffers, int bufferElements);

void rlUnloadRenderBatch_(rlRenderBatch *batch);

void rlSetUniformMatrix_(int locIndex, Matrix *mat);

void rlSetUniformMatrices_(int locIndex, const Matrix *mat, int count);

Matrix *rlGetMatrixProjectionStereo_(int eye);

Matrix *rlGetMatrixViewOffsetStereo_(int eye);

void rlSetMatrixProjection_(Matrix *proj);

void rlSetMatrixModelview_(Matrix *view);

void rlSetMatrixProjectionStereo_(Matrix *right, Matrix *left);

void rlSetMatrixViewOffsetStereo_(Matrix *right, Matrix *left);

Matrix *rlGetMatrixModelview_();

Matrix *rlGetMatrixProjection_();

Matrix *rlGetMatrixTransform_();

void rlMatrixMode_(int a);

void rlTranslatef_(float a, float b, float c);

void rlRotatef_(float a, float b, float c, float d);

void rlScalef_(float a, float b, float c);

void rlMultMatrixf_(const float *a);

void rlFrustum_(double a, double b, double c, double d, double e, double f);

void rlOrtho_(double a, double b, double c, double d, double e, double f);

void rlViewport_(int a, int b, int c, int d);

void rlSetClipPlanes_(double a, double b);

double rlGetCullDistanceNear_();

double rlGetCullDistanceFar_();

void rlBegin_(int a);

void rlVertex2i_(int a, int b);

void rlVertex2f_(float a, float b);

void rlVertex3f_(float a, float b, float c);

void rlTexCoord2f_(float a, float b);

void rlNormal3f_(float a, float b, float c);

void rlColor4ub_(unsigned char a, unsigned char b, unsigned char c, unsigned char d);

void rlColor3f_(float a, float b, float c);

void rlColor4f_(float a, float b, float c, float d);

bool rlEnableVertexArray_(unsigned int a);

void rlEnableVertexBuffer_(unsigned int a);

void rlEnableVertexBufferElement_(unsigned int a);

void rlEnableVertexAttribute_(unsigned int a);

void rlDisableVertexAttribute_(unsigned int a);

void rlActiveTextureSlot_(int a);

void rlEnableTexture_(unsigned int a);

void rlEnableTextureCubemap_(unsigned int a);

void rlTextureParameters_(unsigned int a, int b, int c);

void rlCubemapParameters_(unsigned int a, int b, int c);

void rlEnableShader_(unsigned int a);

void rlEnableFramebuffer_(unsigned int a);

void rlActiveDrawBuffers_(int a);

void rlBlitFramebuffer_(int a, int b, int c, int d, int e, int f, int g, int h, int undefined);

void rlBindFramebuffer_(unsigned int a, unsigned int b);

void rlColorMask_(bool a, bool b, bool c, bool d);

void rlSetCullFace_(int a);

void rlScissor_(int a, int b, int c, int d);

void rlSetLineWidth_(float a);

float rlGetLineWidth_();

bool rlIsStereoRenderEnabled_();

void rlClearColor_(unsigned char a, unsigned char b, unsigned char c, unsigned char d);

void rlSetBlendMode_(int a);

void rlSetBlendFactors_(int a, int b, int c);

void rlSetBlendFactorsSeparate_(int a, int b, int c, int d, int e, int f);

void rlglInit_(int a, int b);

void rlLoadExtensions_(void *a);

int rlGetVersion_();

void rlSetFramebufferWidth_(int a);

int rlGetFramebufferWidth_();

void rlSetFramebufferHeight_(int a);

int rlGetFramebufferHeight_();

unsigned int rlGetTextureIdDefault_();

unsigned int rlGetShaderIdDefault_();

int *rlGetShaderLocsDefault_();

void rlDrawRenderBatch_(rlRenderBatch *a);

void rlSetRenderBatchActive_(rlRenderBatch *a);

bool rlCheckRenderBatchLimit_(int a);

void rlSetTexture_(unsigned int a);

unsigned int rlLoadVertexArray_();

unsigned int rlLoadVertexBuffer_(const void *a, int b, bool c);

unsigned int rlLoadVertexBufferElement_(const void *a, int b, bool c);

void rlUpdateVertexBuffer_(unsigned int a, const void *b, int c, int d);

void rlUpdateVertexBufferElements_(unsigned int a, const void *b, int c, int d);

void rlUnloadVertexArray_(unsigned int a);

void rlUnloadVertexBuffer_(unsigned int a);

void rlSetVertexAttribute_(unsigned int a, int b, int c, bool d, int e, int f);

void rlSetVertexAttributeDivisor_(unsigned int a, int b);

void rlSetVertexAttributeDefault_(int a, const void *b, int c, int d);

void rlDrawVertexArray_(int a, int b);

void rlDrawVertexArrayElements_(int a, int b, const void *c);

void rlDrawVertexArrayInstanced_(int a, int b, int c);

void rlDrawVertexArrayElementsInstanced_(int a, int b, const void *c, int d);

unsigned int rlLoadTexture_(const void *a, int b, int c, int d, int e);

unsigned int rlLoadTextureDepth_(int a, int b, bool c);

unsigned int rlLoadTextureCubemap_(const void *a, int b, int c);

void rlUpdateTexture_(unsigned int a, int b, int c, int d, int e, int f, const void *g);

void rlGetGlTextureFormats_(int a, unsigned int *b, unsigned int *c, unsigned int *d);

const char *rlGetPixelFormatName_(unsigned int a);

void rlUnloadTexture_(unsigned int a);

void rlGenTextureMipmaps_(unsigned int a, int b, int c, int d, int *e);

void *rlReadTexturePixels_(unsigned int a, int b, int c, int d);

unsigned char *rlReadScreenPixels_(int a, int b);

unsigned int rlLoadFramebuffer_();

void rlFramebufferAttach_(unsigned int a, unsigned int b, int c, int d, int e);

bool rlFramebufferComplete_(unsigned int a);

void rlUnloadFramebuffer_(unsigned int a);

unsigned int rlLoadShaderCode_(const char *a, const char *b);

unsigned int rlCompileShader_(const char *a, int b);

unsigned int rlLoadShaderProgram_(unsigned int a, unsigned int b);

void rlUnloadShaderProgram_(unsigned int a);

int rlGetLocationUniform_(unsigned int a, const char *b);

int rlGetLocationAttrib_(unsigned int a, const char *b);

void rlSetUniform_(int a, const void *b, int c, int d);

void rlSetUniformSampler_(int a, unsigned int b);

void rlSetShader_(unsigned int a, int *b);

unsigned int rlLoadComputeShaderProgram_(unsigned int a);

void rlComputeShaderDispatch_(unsigned int a, unsigned int b, unsigned int c);

unsigned int rlLoadShaderBuffer_(unsigned int a, const void *b, int c);

void rlUnloadShaderBuffer_(unsigned int a);

void rlUpdateShaderBuffer_(unsigned int a, const void *b, unsigned int c, unsigned int d);

void rlBindShaderBuffer_(unsigned int a, unsigned int b);

void rlReadShaderBuffer_(unsigned int a, void *b, unsigned int c, unsigned int d);

void rlCopyShaderBuffer_(unsigned int a, unsigned int b, unsigned int c, unsigned int d, unsigned int e);

unsigned int rlGetShaderBufferSize_(unsigned int a);

void rlBindImageTexture_(unsigned int a, unsigned int b, int c, bool d);

void rlPushMatrix_();

void rlPopMatrix_();

void rlLoadIdentity_();

void rlEnd_();

void rlDisableVertexArray_();

void rlDisableVertexBuffer_();

void rlDisableVertexBufferElement_();

void rlDisableTexture_();

void rlDisableTextureCubemap_();

void rlDisableShader_();

void rlDisableFramebuffer_();

unsigned int rlGetActiveFramebuffer_();

void rlEnableColorBlend_();

void rlDisableColorBlend_();

void rlEnableDepthTest_();

void rlDisableDepthTest_();

void rlEnableDepthMask_();

void rlDisableDepthMask_();

void rlEnableBackfaceCulling_();

void rlDisableBackfaceCulling_();

void rlEnableScissorTest_();

void rlDisableScissorTest_();

void rlEnableWireMode_();

void rlEnablePointMode_();

void rlDisableWireMode_();

void rlEnableSmoothLines_();

void rlDisableSmoothLines_();

void rlEnableStereoRender_();

void rlDisableStereoRender_();

void rlClearScreenBuffers_();

void rlCheckErrors_();

void rlglClose_();

void rlDrawRenderBatchActive_();

void rlLoadDrawCube_();

void rlLoadDrawQuad_();
