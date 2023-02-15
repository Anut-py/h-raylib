/**
 * @file rl_bindings.h
 * @author Anut-py
 * @brief Required methods for binding Haskell to Raylib
 *
 * Haskell does not support interfacing with C directly through structs (e.g. Vector2).
 * In order to achieve this, wrapper functions that use pointers need to be written. This
 * file contains wrapper functions for all Raylib functions that do not take pointers.
 */

#include <raylib.h>

void SetWindowIcon_(Image *a);

Vector2 *GetMonitorPosition_(int a);

Vector2 *GetWindowPosition_();

Vector2 *GetWindowScaleDPI_();

void ClearBackground_(Color *a);

void BeginMode2D_(Camera2D *a);

void BeginMode3D_(Camera3D *a);

void BeginTextureMode_(RenderTexture *a);

void BeginShaderMode_(Shader *a);

void BeginVrStereoMode_(VrStereoConfig *a);

VrStereoConfig *LoadVrStereoConfig_(VrDeviceInfo *a);

void UnloadVrStereoConfig_(VrStereoConfig *a);

Shader *LoadShader_(char *a, char *b);

Shader *LoadShaderFromMemory_(char *a, char *b);

bool IsShaderReady_(Shader *a);

int GetShaderLocation_(Shader *a, char *b);

int GetShaderLocationAttrib_(Shader *a, char *b);

void SetShaderValue_(Shader *a, int b, const void *c, int d);

void SetShaderValueV_(Shader *a, int b, const void *c, int d, int e);

void SetShaderValueMatrix_(Shader *a, int b, Matrix *c);

void SetShaderValueTexture_(Shader *a, int b, Texture *c);

void UnloadShader_(Shader *a);

Ray *GetMouseRay_(Vector2 *a, Camera3D *b);

Matrix *GetCameraMatrix_(Camera3D *a);

Matrix *GetCameraMatrix2D_(Camera2D *a);

Vector2 *GetWorldToScreen_(Vector3 *a, Camera3D *b);

Vector2 *GetScreenToWorld2D_(Vector2 *a, Camera2D *b);

Vector2 *GetWorldToScreenEx_(Vector3 *a, Camera3D *b, int c, int d);

Vector2 *GetWorldToScreen2D_(Vector2 *a, Camera2D *b);

FilePathList *LoadDirectoryFiles_(char *a);

FilePathList *LoadDirectoryFilesEx_(char *a, char *b, int c);

void UnloadDirectoryFiles_(FilePathList *a);

FilePathList *LoadDroppedFiles_();

void UnloadDroppedFiles_(FilePathList *a);

Vector2 *GetMousePosition_();

Vector2 *GetMouseDelta_();

Vector2 *GetMouseWheelMoveV_();

Vector2 *GetTouchPosition_(int a);

Vector2 *GetGestureDragVector_();

Vector2 *GetGesturePinchVector_();

void SetCameraMode_(Camera3D *a, int b);

void SetShapesTexture_(Texture *a, Rectangle *b);

void DrawPixel_(int a, int b, Color *c);

void DrawPixelV_(Vector2 *a, Color *b);

void DrawLine_(int a, int b, int c, int d, Color *e);

void DrawLineV_(Vector2 *a, Vector2 *b, Color *c);

void DrawLineEx_(Vector2 *a, Vector2 *b, float c, Color *d);

void DrawLineBezier_(Vector2 *a, Vector2 *b, float c, Color *d);

void DrawLineBezierQuad_(Vector2 *a, Vector2 *b, Vector2 *c, float d, Color *e);

void DrawLineBezierCubic_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f);

void DrawLineStrip_(Vector2 *a, int b, Color *c);

void DrawCircle_(int a, int b, float c, Color *d);

void DrawCircleSector_(Vector2 *a, float b, float c, float d, int e, Color *f);

void DrawCircleSectorLines_(Vector2 *a, float b, float c, float d, int e, Color *f);

void DrawCircleGradient_(int a, int b, float c, Color *d, Color *e);

void DrawCircleV_(Vector2 *a, float b, Color *c);

void DrawCircleLines_(int a, int b, float c, Color *d);

void DrawEllipse_(int a, int b, float c, float d, Color *e);

void DrawEllipseLines_(int a, int b, float c, float d, Color *e);

void DrawRing_(Vector2 *a, float b, float c, float d, float e, int f, Color *g);

void DrawRingLines_(Vector2 *a, float b, float c, float d, float e, int f, Color *g);

void DrawRectangle_(int a, int b, int c, int d, Color *e);

void DrawRectangleV_(Vector2 *a, Vector2 *b, Color *c);

void DrawRectangleRec_(Rectangle *a, Color *b);

void DrawRectanglePro_(Rectangle *a, Vector2 *b, float c, Color *d);

void DrawRectangleGradientV_(int a, int b, int c, int d, Color *e, Color *f);

void DrawRectangleGradientH_(int a, int b, int c, int d, Color *e, Color *f);

void DrawRectangleGradientEx_(Rectangle *a, Color *b, Color *c, Color *d, Color *e);

void DrawRectangleLines_(int a, int b, int c, int d, Color *e);

void DrawRectangleLinesEx_(Rectangle *a, float b, Color *c);

void DrawRectangleRounded_(Rectangle *a, float b, int c, Color *d);

void DrawRectangleRoundedLines_(Rectangle *a, float b, int c, float d, Color *e);

void DrawTriangle_(Vector2 *a, Vector2 *b, Vector2 *c, Color *d);

void DrawTriangleLines_(Vector2 *a, Vector2 *b, Vector2 *c, Color *d);

void DrawTriangleFan_(Vector2 *a, int b, Color *c);

void DrawTriangleStrip_(Vector2 *a, int b, Color *c);

void DrawPoly_(Vector2 *a, int b, float c, float d, Color *e);

void DrawPolyLines_(Vector2 *a, int b, float c, float d, Color *e);

void DrawPolyLinesEx_(Vector2 *a, int b, float c, float d, float e, Color *f);

int CheckCollisionRecs_(Rectangle *a, Rectangle *b);

int CheckCollisionCircles_(Vector2 *a, float b, Vector2 *c, float d);

int CheckCollisionCircleRec_(Vector2 *a, float b, Rectangle *c);

int CheckCollisionPointRec_(Vector2 *a, Rectangle *b);

int CheckCollisionPointCircle_(Vector2 *a, Vector2 *b, float c);

int CheckCollisionPointTriangle_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d);

int CheckCollisionLines_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, Vector2 *e);

int CheckCollisionPointLine_(Vector2 *a, Vector2 *b, Vector2 *c, int d);

Rectangle *GetCollisionRec_(Rectangle *a, Rectangle *b);

Image *LoadImage_(char *a);

Image *LoadImageRaw_(char *a, int b, int c, int d, int e);

Image *LoadImageAnim_(char *a, int *b);

Image *LoadImageFromMemory_(char *a, unsigned char *b, int c);

Image *LoadImageFromTexture_(Texture *a);

Image *LoadImageFromScreen_();

bool IsImageReady_(Image *a);

void UnloadImage_(Image *a);

int ExportImage_(Image *a, char *b);

int ExportImageAsCode_(Image *a, char *b);

Image *GenImageColor_(int a, int b, Color *c);

Image *GenImageGradientV_(int a, int b, Color *c, Color *d);

Image *GenImageGradientH_(int a, int b, Color *c, Color *d);

Image *GenImageGradientRadial_(int a, int b, float c, Color *d, Color *e);

Image *GenImageChecked_(int a, int b, int c, int d, Color *e, Color *f);

Image *GenImageWhiteNoise_(int a, int b, float c);

Image *GenImagePerlinNoise_(int a, int b, int c, int d, float e);

Image *GenImageCellular_(int a, int b, int c);

Image *GenImageText_(int a, int b, char *c);

Image *ImageCopy_(Image *a);

Image *ImageFromImage_(Image *a, Rectangle *b);

Image *ImageText_(char *a, int b, Color *c);

Image *ImageTextEx_(Font *a, char *b, float c, float d, Color *e);

void ImageToPOT_(Image *a, Color *b);

void ImageCrop_(Image *a, Rectangle *b);

void ImageAlphaClear_(Image *a, Color *b, float c);

void ImageAlphaMask_(Image *a, Image *b);

void ImageResizeCanvas_(Image *a, int b, int c, int d, int e, Color *f);

void ImageColorTint_(Image *a, Color *b);

void ImageColorReplace_(Image *a, Color *b, Color *c);

Color *LoadImageColors_(Image *a);

Color *LoadImagePalette_(Image *a, int b, int *c);

Rectangle *GetImageAlphaBorder_(Image *a, float b);

Color *GetImageColor_(Image *a, int b, int c);

void ImageClearBackground_(Image *a, Color *b);

void ImageDrawPixel_(Image *a, int b, int c, Color *d);

void ImageDrawPixelV_(Image *a, Vector2 *b, Color *c);

void ImageDrawLine_(Image *a, int b, int c, int d, int e, Color *f);

void ImageDrawLineV_(Image *a, Vector2 *b, Vector2 *c, Color *d);

void ImageDrawCircle_(Image *a, int b, int c, int d, Color *e);

void ImageDrawCircleV_(Image *a, Vector2 *b, int c, Color *d);

void ImageDrawCircleLines_(Image *a, int b, int c, int d, Color *e);

void ImageDrawCircleLinesV_(Image *a, Vector2 *b, int c, Color *d);

void ImageDrawRectangle_(Image *a, int b, int c, int d, int e, Color *f);

void ImageDrawRectangleV_(Image *a, Vector2 *b, Vector2 *c, Color *d);

void ImageDrawRectangleRec_(Image *a, Rectangle *b, Color *c);

void ImageDrawRectangleLines_(Image *a, Rectangle *b, int c, Color *d);

void ImageDraw_(Image *a, Image *b, Rectangle *c, Rectangle *d, Color *e);

void ImageDrawText_(Image *a, char *b, int c, int d, int e, Color *f);

void ImageDrawTextEx_(Image *a, Font *b, char *c, Vector2 *d, float e, float f, Color *g);

Texture *LoadTexture_(char *a);

Texture *LoadTextureFromImage_(Image *a);

Texture *LoadTextureCubemap_(Image *a, int b);

RenderTexture *LoadRenderTexture_(int a, int b);

bool IsTextureReady_(Texture* a);

void UnloadTexture_(Texture *a);

bool IsRenderTextureReady_(RenderTexture* a);

void UnloadRenderTexture_(RenderTexture *a);

void UpdateTexture_(Texture *a, const void *b);

void UpdateTextureRec_(Texture *a, Rectangle *b, const void *c);

void SetTextureFilter_(Texture *a, int b);

void SetTextureWrap_(Texture *a, int b);

void DrawTexture_(Texture *a, int b, int c, Color *d);

void DrawTextureV_(Texture *a, Vector2 *b, Color *c);

void DrawTextureEx_(Texture *a, Vector2 *b, float c, float d, Color *e);

void DrawTextureRec_(Texture *a, Rectangle *b, Vector2 *c, Color *d);

void DrawTexturePro_(Texture *a, Rectangle *b, Rectangle *c, Vector2 *d, float e, Color *f);

void DrawTextureNPatch_(Texture *a, NPatchInfo *b, Rectangle *c, Vector2 *d, float e, Color *f);

Color *Fade_(Color *a, float b);

int ColorToInt_(Color *a);

Vector4 *ColorNormalize_(Color *a);

Color *ColorFromNormalized_(Vector4 *a);

Vector3 *ColorToHSV_(Color *a);

Color *ColorFromHSV_(float a, float b, float c);

Color *ColorAlpha_(Color *a, float b);

Color *ColorAlphaBlend_(Color *a, Color *b, Color *c);

Color *GetColor_(unsigned int a);

Color *GetPixelColor_(void *a, int b);

void SetPixelColor_(void *a, Color *b, int c);

Font *GetFontDefault_();

Font *LoadFont_(char *a);

Font *LoadFontEx_(char *a, int b, int *c, int d);

Font *LoadFontFromImage_(Image *a, Color *b, int c);

Font *LoadFontFromMemory_(char *a, unsigned char *b, int c, int d, int *e, int f);

Image *GenImageFontAtlas_(GlyphInfo *a, Rectangle **b, int c, int d, int e, int f);

bool IsFontReady_(Font* a);

void UnloadFont_(Font *a);

int ExportFontAsCode_(Font *a, char *b);

void DrawText_(char *a, int b, int c, int d, Color *e);

void DrawTextEx_(Font *a, char *b, Vector2 *c, float d, float e, Color *f);

void DrawTextPro_(Font *a, char *b, Vector2 *c, Vector2 *d, float e, float f, float g, Color *h);

void DrawTextCodepoint_(Font *a, int b, Vector2 *c, float d, Color *e);

void DrawTextCodepoints_(Font *a, int *b, int c, Vector2 *d, float e, float f, Color *g);

Vector2 *MeasureTextEx_(Font *a, char *b, float c, float d);

int GetGlyphIndex_(Font *a, int b);

GlyphInfo *GetGlyphInfo_(Font *a, int b);

Rectangle *GetGlyphAtlasRec_(Font *a, int b);

void DrawLine3D_(Vector3 *a, Vector3 *b, Color *c);

void DrawPoint3D_(Vector3 *a, Color *b);

void DrawCircle3D_(Vector3 *a, float b, Vector3 *c, float d, Color *e);

void DrawTriangle3D_(Vector3 *a, Vector3 *b, Vector3 *c, Color *d);

void DrawTriangleStrip3D_(Vector3 *a, int b, Color *c);

void DrawCube_(Vector3 *a, float b, float c, float d, Color *e);

void DrawCubeV_(Vector3 *a, Vector3 *b, Color *c);

void DrawCubeWires_(Vector3 *a, float b, float c, float d, Color *e);

void DrawCubeWiresV_(Vector3 *a, Vector3 *b, Color *c);

void DrawCubeTexture_(Texture *a, Vector3 *b, float c, float d, float e, Color *f);

void DrawCubeTextureRec_(Texture *a, Rectangle *b, Vector3 *c, float d, float e, float f, Color *g);

void DrawSphere_(Vector3 *a, float b, Color *c);

void DrawSphereEx_(Vector3 *a, float b, int c, int d, Color *e);

void DrawSphereWires_(Vector3 *a, float b, int c, int d, Color *e);

void DrawCylinder_(Vector3 *a, float b, float c, float d, int e, Color *f);

void DrawCylinderEx_(Vector3 *a, Vector3 *b, float c, float d, int e, Color *f);

void DrawCylinderWires_(Vector3 *a, float b, float c, float d, int e, Color *f);

void DrawCylinderWiresEx_(Vector3 *a, Vector3 *b, float c, float d, int e, Color *f);

void DrawCapsule_(Vector3 *a, Vector3 *b, float c, int d, int e, Color *f);

void DrawCapsuleEx_(Vector3 *a, Vector3 *b, float c, int d, int e, Color *f);

void DrawPlane_(Vector3 *a, Vector2 *b, Color *c);

void DrawRay_(Ray *a, Color *b);

Model *LoadModel_(char *a);

Model *LoadModelFromMesh_(Mesh *a);

bool IsModelReady_(Model* a);

void UnloadModel_(Model *a);

void UnloadModelKeepMeshes_(Model *a);

BoundingBox *GetModelBoundingBox_(Model *a);

void DrawModel_(Model *a, Vector3 *b, float c, Color *d);

void DrawModelEx_(Model *a, Vector3 *b, Vector3 *c, float d, Vector3 *e, Color *f);

void DrawModelWires_(Model *a, Vector3 *b, float c, Color *d);

void DrawModelWiresEx_(Model *a, Vector3 *b, Vector3 *c, float d, Vector3 *e, Color *f);

void DrawBoundingBox_(BoundingBox *a, Color *b);

void DrawBillboard_(Camera3D *a, Texture *b, Vector3 *c, float d, Color *e);

void DrawBillboardRec_(Camera3D *a, Texture *b, Rectangle *c, Vector3 *d, Vector2 *e, Color *f);

void DrawBillboardPro_(Camera3D *a, Texture *b, Rectangle *c, Vector3 *d, Vector3 *e, Vector2 *f, Vector2 *g, float h, Color *i);

void UpdateMeshBuffer_(Mesh *a, int b, const void *c, int d, int e);

void UnloadMesh_(Mesh *a);

void DrawMesh_(Mesh *a, Material *b, Matrix *c);

void DrawMeshInstanced_(Mesh *a, Material *b, Matrix *c, int d);

int ExportMesh_(Mesh *a, char *b);

BoundingBox *GetMeshBoundingBox_(Mesh *a);

Mesh *GenMeshPoly_(int a, float b);

Mesh *GenMeshPlane_(float a, float b, int c, int d);

Mesh *GenMeshCube_(float a, float b, float c);

Mesh *GenMeshSphere_(float a, int b, int c);

Mesh *GenMeshHemiSphere_(float a, int b, int c);

Mesh *GenMeshCylinder_(float a, float b, int c);

Mesh *GenMeshCone_(float a, float b, int c);

Mesh *GenMeshTorus_(float a, float b, int c, int d);

Mesh *GenMeshKnot_(float a, float b, int c, int d);

Mesh *GenMeshHeightmap_(Image *a, Vector3 *b);

Mesh *GenMeshCubicmap_(Image *a, Vector3 *b);

Material *LoadMaterialDefault_();

bool IsMaterialReady_(Material *a);

void UnloadMaterial_(Material *a);

void SetMaterialTexture_(Material *a, int b, Texture *c);

void UpdateModelAnimation_(Model *a, ModelAnimation *b, int c);

void UnloadModelAnimation_(ModelAnimation *a);

int IsModelAnimationValid_(Model *a, ModelAnimation *b);

int CheckCollisionSpheres_(Vector3 *a, float b, Vector3 *c, float d);

int CheckCollisionBoxes_(BoundingBox *a, BoundingBox *b);

int CheckCollisionBoxSphere_(BoundingBox *a, Vector3 *b, float c);

RayCollision *GetRayCollisionSphere_(Ray *a, Vector3 *b, float c);

RayCollision *GetRayCollisionBox_(Ray *a, BoundingBox *b);

RayCollision *GetRayCollisionMesh_(Ray *a, Mesh *b, Matrix *c);

RayCollision *GetRayCollisionTriangle_(Ray *a, Vector3 *b, Vector3 *c, Vector3 *d);

RayCollision *GetRayCollisionQuad_(Ray *a, Vector3 *b, Vector3 *c, Vector3 *d, Vector3 *e);

Wave *LoadWave_(char *a);

Wave *LoadWaveFromMemory_(char *a, unsigned char *b, int c);

Sound *LoadSound_(char *a);

Sound *LoadSoundFromWave_(Wave *a);

void UpdateSound_(Sound *a, const void *b, int c);

bool IsWaveReady_(Wave *a);

void UnloadWave_(Wave *a);

bool IsSoundReady_(Sound *a);

void UnloadSound_(Sound *a);

int ExportWave_(Wave *a, char *b);

int ExportWaveAsCode_(Wave *a, char *b);

void PlaySound_(Sound *a);

void StopSound_(Sound *a);

void PauseSound_(Sound *a);

void ResumeSound_(Sound *a);

void PlaySoundMulti_(Sound *a);

int IsSoundPlaying_(Sound *a);

void SetSoundVolume_(Sound *a, float b);

void SetSoundPitch_(Sound *a, float b);

void SetSoundPan_(Sound *a, float b);

Wave *WaveCopy_(Wave *a);

float *LoadWaveSamples_(Wave *a);

Music *LoadMusicStream_(char *a);

Music *LoadMusicStreamFromMemory_(char *a, unsigned char *b, int c);

bool IsMusicReady_(Music *a);

void UnloadMusicStream_(Music *a);

void PlayMusicStream_(Music *a);

int IsMusicStreamPlaying_(Music *a);

void UpdateMusicStream_(Music *a);

void StopMusicStream_(Music *a);

void PauseMusicStream_(Music *a);

void ResumeMusicStream_(Music *a);

void SeekMusicStream_(Music *a, float b);

void SetMusicVolume_(Music *a, float b);

void SetMusicPitch_(Music *a, float b);

void SetMusicPan_(Music *a, float b);

float GetMusicTimeLength_(Music *a);

float GetMusicTimePlayed_(Music *a);

AudioStream *LoadAudioStream_(unsigned int a, unsigned int b, unsigned int c);

bool IsAudioStreamReady_(AudioStream *a);

void UnloadAudioStream_(AudioStream *a);

void UpdateAudioStream_(AudioStream *a, const void *b, int c);

int IsAudioStreamProcessed_(AudioStream *a);

void PlayAudioStream_(AudioStream *a);

void PauseAudioStream_(AudioStream *a);

void ResumeAudioStream_(AudioStream *a);

int IsAudioStreamPlaying_(AudioStream *a);

void StopAudioStream_(AudioStream *a);

void SetAudioStreamVolume_(AudioStream *a, float b);

void SetAudioStreamPitch_(AudioStream *a, float b);

void SetAudioStreamPan_(AudioStream *a, float b);

void SetAudioStreamCallback_(AudioStream *a, AudioCallback *b);

void AttachAudioStreamProcessor_(AudioStream *a, AudioCallback *b);

void DetachAudioStreamProcessor_(AudioStream *a, AudioCallback *b);
