/**
 * @file rl_bindings.h
 * @author Anut-py
 * @brief Required methods for binding Haskell to Raylib
 *
 * Haskell does not support interfacing with C directly through structs (e.g. Vector2).
 * In order to achieve this, wrapper functions that use pointers need to be written. This
 * file contains wrapper functions for all Raylib functions that do not take pointers.
 */

#include "rl_common.h"

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

AutomationEventList *LoadAutomationEventList_(char *a);

void UnloadAutomationEventList_(AutomationEventList *a);

bool ExportAutomationEventList_(AutomationEventList *a, char *b);

void PlayAutomationEvent_(AutomationEvent *a);

Vector2 *GetMousePosition_();

Vector2 *GetMouseDelta_();

Vector2 *GetMouseWheelMoveV_();

Vector2 *GetTouchPosition_(int a);

Vector2 *GetGestureDragVector_();

Vector2 *GetGesturePinchVector_();

void UpdateCameraPro_(Camera3D *a, Vector3 *b, Vector3 *c, float d);

void SetShapesTexture_(Texture *a, Rectangle *b);

Texture2D *GetShapesTexture_();

Rectangle *GetShapesTextureRectangle_();

void DrawPixel_(int a, int b, Color *c);

void DrawPixelV_(Vector2 *a, Color *b);

void DrawLine_(int a, int b, int c, int d, Color *e);

void DrawLineV_(Vector2 *a, Vector2 *b, Color *c);

void DrawLineEx_(Vector2 *a, Vector2 *b, float c, Color *d);

void DrawLineStrip_(Vector2 *a, int b, Color *c);

void DrawLineBezier_(Vector2 *a, Vector2 *b, float c, Color *d);

void DrawCircle_(int a, int b, float c, Color *d);

void DrawCircleSector_(Vector2 *a, float b, float c, float d, int e, Color *f);

void DrawCircleSectorLines_(Vector2 *a, float b, float c, float d, int e, Color *f);

void DrawCircleGradient_(int a, int b, float c, Color *d, Color *e);

void DrawCircleV_(Vector2 *a, float b, Color *c);

void DrawCircleLines_(int a, int b, float c, Color *d);

void DrawCircleLinesV_(Vector2 *a, float b, Color *c);

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

void DrawSplineLinear_(Vector2 *a, int b, float c, Color *d);

void DrawSplineBasis_(Vector2 *a, int b, float c, Color *d);

void DrawSplineCatmullRom_(Vector2 *a, int b, float c, Color *d);

void DrawSplineBezierQuadratic_(Vector2 *a, int b, float c, Color *d);

void DrawSplineBezierCubic_(Vector2 *a, int b, float c, Color *d);

void DrawSplineSegmentLinear_(Vector2 *a, Vector2 *b, float c, Color *d);

void DrawSplineSegmentBasis_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f);

void DrawSplineSegmentCatmullRom_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f);

void DrawSplineSegmentBezierQuadratic_(Vector2 *a, Vector2 *b, Vector2 *c, float d, Color *e);

void DrawSplineSegmentBezierCubic_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f);

Vector2 *GetSplinePointLinear_(Vector2 *a, Vector2 *b, float c);

Vector2 *GetSplinePointBasis_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e);

Vector2 *GetSplinePointCatmullRom_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e);

Vector2 *GetSplinePointBezierQuad_(Vector2 *a, Vector2 *b, Vector2 *c, float d);

Vector2 *GetSplinePointBezierCubic_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e);

bool CheckCollisionRecs_(Rectangle *a, Rectangle *b);

bool CheckCollisionCircles_(Vector2 *a, float b, Vector2 *c, float d);

bool CheckCollisionCircleRec_(Vector2 *a, float b, Rectangle *c);

bool CheckCollisionPointRec_(Vector2 *a, Rectangle *b);

bool CheckCollisionPointCircle_(Vector2 *a, Vector2 *b, float c);

bool CheckCollisionPointTriangle_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d);

bool CheckCollisionPointPoly_(Vector2 *a, Vector2 *b, int c);

bool CheckCollisionLines_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, Vector2 *e);

bool CheckCollisionPointLine_(Vector2 *a, Vector2 *b, Vector2 *c, int d);

Rectangle *GetCollisionRec_(Rectangle *a, Rectangle *b);

Image *LoadImage_(char *a);

Image *LoadImageRaw_(char *a, int b, int c, int d, int e);

Image *LoadImageSvg_(char *a, int b, int c);

Image *LoadImageAnim_(char *a, int *b);

Image *LoadImageAnimFromMemory_(char *a, unsigned char *b, int c, int *d);

Image *LoadImageFromMemory_(char *a, unsigned char *b, int c);

Image *LoadImageFromTexture_(Texture *a);

Image *LoadImageFromScreen_();

bool IsImageReady_(Image *a);

void UnloadImage_(Image *a);

int ExportImage_(Image *a, char *b);

unsigned char *ExportImageToMemory_(Image *a, char *fileType, int *fileSize);

int ExportImageAsCode_(Image *a, char *b);

Image *GenImageColor_(int a, int b, Color *c);

Image *GenImageGradientLinear_(int a, int b, int c, Color *d, Color *e);

Image *GenImageGradientRadial_(int a, int b, float c, Color *d, Color *e);

Image *GenImageGradientSquare_(int a, int b, float c, Color *d, Color *e);

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

bool IsTextureReady_(Texture *a);

void UnloadTexture_(Texture *a);

bool IsRenderTextureReady_(RenderTexture *a);

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

int GetPixelDataSize_(int a, int b, int c);

Font *GetFontDefault_();

Font *LoadFont_(char *a);

Font *LoadFontEx_(char *a, int b, int *c, int d);

Font *LoadFontFromImage_(Image *a, Color *b, int c);

Font *LoadFontFromMemory_(char *a, unsigned char *b, int c, int d, int *e, int f);

Image *GenImageFontAtlas_(GlyphInfo *a, Rectangle **b, int c, int d, int e, int f);

bool IsFontReady_(Font *a);

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

bool IsModelReady_(Model *a);

void UnloadModel_(Model *a);

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

bool ExportMesh_(Mesh *a, char *b);

bool ExportMeshAsCode_(Mesh *a, char *b);

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

bool IsModelAnimationValid_(Model *a, ModelAnimation *b);

bool CheckCollisionSpheres_(Vector3 *a, float b, Vector3 *c, float d);

bool CheckCollisionBoxes_(BoundingBox *a, BoundingBox *b);

bool CheckCollisionBoxSphere_(BoundingBox *a, Vector3 *b, float c);

RayCollision *GetRayCollisionSphere_(Ray *a, Vector3 *b, float c);

RayCollision *GetRayCollisionBox_(Ray *a, BoundingBox *b);

RayCollision *GetRayCollisionMesh_(Ray *a, Mesh *b, Matrix *c);

RayCollision *GetRayCollisionTriangle_(Ray *a, Vector3 *b, Vector3 *c, Vector3 *d);

RayCollision *GetRayCollisionQuad_(Ray *a, Vector3 *b, Vector3 *c, Vector3 *d, Vector3 *e);

Wave *LoadWave_(char *a);

Wave *LoadWaveFromMemory_(char *a, unsigned char *b, int c);

Sound *LoadSound_(char *a);

Sound *LoadSoundFromWave_(Wave *a);

Sound *LoadSoundAlias_(Sound *a);

void UpdateSound_(Sound *a, const void *b, int c);

bool IsWaveReady_(Wave *a);

void UnloadWave_(Wave *a);

bool IsSoundReady_(Sound *a);

void UnloadSound_(Sound *a);

void UnloadSoundAlias_(Sound *a);

int ExportWave_(Wave *a, char *b);

int ExportWaveAsCode_(Wave *a, char *b);

void PlaySound_(Sound *a);

void StopSound_(Sound *a);

void PauseSound_(Sound *a);

void ResumeSound_(Sound *a);

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

void AttachAudioMixedProcessor_(AudioCallback *a);

void DetachAudioMixedProcessor_(AudioCallback *a);

void InitWindow_(int a, int b, const char *c);

bool WindowShouldClose_();

void CloseWindow_();

bool IsWindowReady_();

bool IsWindowFullscreen_();

bool IsWindowHidden_();

bool IsWindowMinimized_();

bool IsWindowMaximized_();

bool IsWindowFocused_();

bool IsWindowResized_();

bool IsWindowState_(unsigned int a);

void SetWindowState_(unsigned int a);

void ClearWindowState_(unsigned int a);

void ToggleFullscreen_();

void ToggleBorderlessWindowed_();

void MaximizeWindow_();

void MinimizeWindow_();

void RestoreWindow_();

void SetWindowIcons_(Image *a, int b);

void SetWindowTitle_(const char *a);

void SetWindowPosition_(int a, int b);

void SetWindowMonitor_(int a);

void SetWindowMinSize_(int a, int b);

void SetWindowMaxSize_(int a, int b);

void SetWindowSize_(int a, int b);

void SetWindowOpacity_(float a);

void SetWindowFocused_();

void *GetWindowHandle_();

int GetScreenWidth_();

int GetScreenHeight_();

int GetRenderWidth_();

int GetRenderHeight_();

int GetMonitorCount_();

int GetCurrentMonitor_();

int GetMonitorWidth_(int a);

int GetMonitorHeight_(int a);

int GetMonitorPhysicalWidth_(int a);

int GetMonitorPhysicalHeight_(int a);

int GetMonitorRefreshRate_(int a);

const char *GetMonitorName_(int a);

void SetClipboardText_(const char *a);

const char *GetClipboardText_();

void EnableEventWaiting_();

void DisableEventWaiting_();

void SwapScreenBuffer_();

void PollInputEvents_();

void WaitTime_(double a);

void ShowCursor_();

void HideCursor_();

bool IsCursorHidden_();

void EnableCursor_();

void DisableCursor_();

bool IsCursorOnScreen_();

void BeginDrawing_();

void EndDrawing_();

void EndMode2D_();

void EndMode3D_();

void EndTextureMode_();

void EndShaderMode_();

void BeginBlendMode_(int a);

void EndBlendMode_();

void BeginScissorMode_(int a, int b, int c, int d);

void EndScissorMode_();

void EndVrStereoMode_();

void SetTargetFPS_(int a);

int GetFPS_();

float GetFrameTime_();

double GetTime_();

void SetRandomSeed_(unsigned int a);

int GetRandomValue_(int a, int b);

int *LoadRandomSequence_(unsigned int a, int b, int c);

void TakeScreenshot_(const char *a);

void SetConfigFlags_(unsigned int a);

void TraceLog_(int a, const char *b);

void SetTraceLogLevel_(int a);

void *MemAlloc_(unsigned int a);

void *MemRealloc_(void *a, unsigned int b);

void MemFree_(void *a);

void OpenURL_(const char *a);

void SetLoadFileDataCallback_(LoadFileDataCallback a);

void SetSaveFileDataCallback_(SaveFileDataCallback a);

void SetLoadFileTextCallback_(LoadFileTextCallback a);

void SetSaveFileTextCallback_(SaveFileTextCallback a);

unsigned char *LoadFileData_(const char *a, int *b);

void UnloadFileData_(unsigned char *a);

bool SaveFileData_(const char *a, void *b, int c);

bool ExportDataAsCode_(const unsigned char *a, int b, const char *c);

char *LoadFileText_(const char *a);

void UnloadFileText_(char *a);

bool SaveFileText_(const char *a, char *b);

bool FileExists_(const char *a);

bool DirectoryExists_(const char *a);

bool IsFileExtension_(const char *a, const char *b);

int GetFileLength_(const char *a);

const char *GetFileExtension_(const char *a);

const char *GetFileName_(const char *a);

const char *GetFileNameWithoutExt_(const char *a);

const char *GetDirectoryPath_(const char *a);

const char *GetPrevDirectoryPath_(const char *a);

const char *GetWorkingDirectory_();

const char *GetApplicationDirectory_();

bool ChangeDirectory_(const char *a);

bool IsPathFile_(const char *a);

bool IsFileDropped_();

long GetFileModTime_(const char *a);

unsigned char *CompressData_(const unsigned char *a, int b, int *c);

unsigned char *DecompressData_(const unsigned char *a, int b, int *c);

char *EncodeDataBase64_(const unsigned char *a, int b, int *c);

unsigned char *DecodeDataBase64_(const unsigned char *a, int *b);

void SetAutomationEventList_(AutomationEventList *a);

void SetAutomationEventBaseFrame_(int a);

void StartAutomationEventRecording_();

void StopAutomationEventRecording_();

bool IsKeyPressed_(int a);

bool IsKeyPressedRepeat_(int a);

bool IsKeyDown_(int a);

bool IsKeyReleased_(int a);

bool IsKeyUp_(int a);

void SetExitKey_(int a);

int GetKeyPressed_();

int GetCharPressed_();

bool IsGamepadAvailable_(int a);

const char *GetGamepadName_(int a);

bool IsGamepadButtonPressed_(int a, int b);

bool IsGamepadButtonDown_(int a, int b);

bool IsGamepadButtonReleased_(int a, int b);

bool IsGamepadButtonUp_(int a, int b);

int GetGamepadButtonPressed_();

int GetGamepadAxisCount_(int a);

float GetGamepadAxisMovement_(int a, int b);

int SetGamepadMappings_(const char *a);

bool IsMouseButtonPressed_(int a);

bool IsMouseButtonDown_(int a);

bool IsMouseButtonReleased_(int a);

bool IsMouseButtonUp_(int a);

int GetMouseX_();

int GetMouseY_();

void SetMousePosition_(int a, int b);

void SetMouseOffset_(int a, int b);

void SetMouseScale_(float a, float b);

float GetMouseWheelMove_();

void SetMouseCursor_(int a);

int GetTouchX_();

int GetTouchY_();

int GetTouchPointId_(int a);

int GetTouchPointCount_();

void SetGesturesEnabled_(unsigned int a);

bool IsGestureDetected_(unsigned int a);

int GetGestureDetected_();

float GetGestureHoldDuration_();

float GetGestureDragAngle_();

float GetGesturePinchAngle_();

void UpdateCamera_(Camera *a, int b);

void ImageFormat_(Image *a, int b);

void ImageAlphaCrop_(Image *a, float b);

void ImageAlphaPremultiply_(Image *a);

void ImageBlurGaussian_(Image *a, int b);

void ImageKernelConvolution_(Image *a, float *b, int c);

void ImageResize_(Image *a, int b, int c);

void ImageResizeNN_(Image *a, int newWidth, int b);

void ImageMipmaps_(Image *a);

void ImageDither_(Image *a, int b, int c, int d, int e);

void ImageFlipVertical_(Image *a);

void ImageFlipHorizontal_(Image *a);

void ImageRotate_(Image *a, int b);

void ImageRotateCW_(Image *a);

void ImageRotateCCW_(Image *a);

void ImageColorInvert_(Image *a);

void ImageColorGrayscale_(Image *a);

void ImageColorContrast_(Image *a, float b);

void ImageColorBrightness_(Image *a, int b);

void GenTextureMipmaps_(Texture2D *a);

GlyphInfo *LoadFontData_(const unsigned char *a, int b, int c, int *d, int e, int f);

void UnloadFontData_(GlyphInfo *a, int b);

void DrawFPS_(int a, int b);

void SetTextLineSpacing_(int a);

int MeasureText_(const char *a, int b);

char *LoadUTF8_(const int *a, int b);

int *LoadCodepoints_(const char *a, int *b);

int GetCodepointCount_(const char *a);

int GetCodepointNext_(const char *a, int *b);

int GetCodepointPrevious_(const char *a, int *b);

const char *CodepointToUTF8_(int a, int *b);

void DrawGrid_(int a, float b);

void UploadMesh_(Mesh *a, bool b);

void GenMeshTangents_(Mesh *a);

Material *LoadMaterials_(const char *a, int *b);

void SetModelMeshMaterial_(Model *a, int b, int c);

ModelAnimation *LoadModelAnimations_(const char *a, int *b);

void UnloadModelAnimations_(ModelAnimation *a, int b);

void InitAudioDevice_();

void CloseAudioDevice_();

bool IsAudioDeviceReady_();

void SetMasterVolume_(float a);

float GetMasterVolume_();

void WaveCrop_(Wave *a, int b, int c);

void WaveFormat_(Wave *a, int b, int c, int d);

void UnloadWaveSamples_(float *a);

void SetAudioStreamBufferSizeDefault_(int a);
