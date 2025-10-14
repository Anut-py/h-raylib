/**
 * See rl_bindings.h
 */

#include "rl_bindings.h"
#include <stdio.h>

RLBIND void SetWindowIcon_(Image *a)
{
    SetWindowIcon(*a);
}

RLBIND Vector2 *GetMonitorPosition_(int a)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetMonitorPosition(a);
    return ptr;
}

RLBIND Vector2 *GetWindowPosition_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetWindowPosition();
    return ptr;
}

RLBIND Vector2 *GetWindowScaleDPI_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetWindowScaleDPI();
    return ptr;
}

RLBIND void ClearBackground_(Color *a)
{
    ClearBackground(*a);
}

RLBIND void BeginMode2D_(Camera2D *a)
{
    BeginMode2D(*a);
}

RLBIND void BeginMode3D_(Camera3D *a)
{
    BeginMode3D(*a);
}

RLBIND void BeginTextureMode_(RenderTexture *a)
{
    BeginTextureMode(*a);
}

RLBIND void BeginShaderMode_(Shader *a)
{
    BeginShaderMode(*a);
}

RLBIND void BeginVrStereoMode_(VrStereoConfig *a)
{
    BeginVrStereoMode(*a);
}

RLBIND VrStereoConfig *LoadVrStereoConfig_(VrDeviceInfo *a)
{
    VrStereoConfig *ptr = (VrStereoConfig *)malloc(sizeof(VrStereoConfig));
    *ptr = LoadVrStereoConfig(*a);
    return ptr;
}

RLBIND void UnloadVrStereoConfig_(VrStereoConfig *a)
{
    UnloadVrStereoConfig(*a);
}

RLBIND Shader *LoadShader_(char *a, char *b)
{
    Shader *ptr = (Shader *)malloc(sizeof(Shader));
    *ptr = LoadShader(a, b);
    return ptr;
}

RLBIND Shader *LoadShaderFromMemory_(char *a, char *b)
{
    Shader *ptr = (Shader *)malloc(sizeof(Shader));
    *ptr = LoadShaderFromMemory(a, b);
    return ptr;
}

RLBIND bool IsShaderValid_(Shader *a)
{
    return IsShaderValid(*a);
}

RLBIND int GetShaderLocation_(Shader *a, char *b)
{
    return GetShaderLocation(*a, b);
}

RLBIND int GetShaderLocationAttrib_(Shader *a, char *b)
{
    return GetShaderLocationAttrib(*a, b);
}

RLBIND void SetShaderValue_(Shader *a, int b, const void *c, int d)
{
    SetShaderValue(*a, b, c, d);
}

RLBIND void SetShaderValueV_(Shader *a, int b, const void *c, int d, int e)
{
    SetShaderValueV(*a, b, c, d, e);
}

RLBIND void SetShaderValueMatrix_(Shader *a, int b, Matrix *c)
{
    SetShaderValueMatrix(*a, b, *c);
}

RLBIND void SetShaderValueTexture_(Shader *a, int b, Texture *c)
{
    SetShaderValueTexture(*a, b, *c);
}

RLBIND void UnloadShader_(Shader *a)
{
    UnloadShader(*a);
}

RLBIND Ray *GetScreenToWorldRay_(Vector2 *a, Camera3D *b)
{
    Ray *ptr = (Ray *)malloc(sizeof(Ray));
    *ptr = GetScreenToWorldRay(*a, *b);
    return ptr;
}

RLBIND Ray *GetScreenToWorldRayEx_(Vector2 *a, Camera3D *b, float c, float d)
{
    Ray *ptr = (Ray *)malloc(sizeof(Ray));
    *ptr = GetScreenToWorldRayEx(*a, *b, c, d);
    return ptr;
}

RLBIND Matrix *GetCameraMatrix_(Camera3D *a)
{
    Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
    *ptr = GetCameraMatrix(*a);
    return ptr;
}

RLBIND Matrix *GetCameraMatrix2D_(Camera2D *a)
{
    Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
    *ptr = GetCameraMatrix2D(*a);
    return ptr;
}

RLBIND Vector2 *GetWorldToScreen_(Vector3 *a, Camera3D *b)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetWorldToScreen(*a, *b);
    return ptr;
}

RLBIND Vector2 *GetScreenToWorld2D_(Vector2 *a, Camera2D *b)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetScreenToWorld2D(*a, *b);
    return ptr;
}

RLBIND Vector2 *GetWorldToScreenEx_(Vector3 *a, Camera3D *b, int c, int d)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetWorldToScreenEx(*a, *b, c, d);
    return ptr;
}

RLBIND Vector2 *GetWorldToScreen2D_(Vector2 *a, Camera2D *b)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetWorldToScreen2D(*a, *b);
    return ptr;
}

RLBIND FilePathList *LoadDirectoryFiles_(char *a)
{
    FilePathList *ptr = (FilePathList *)malloc(sizeof(FilePathList));
    *ptr = LoadDirectoryFiles(a);
    return ptr;
}

RLBIND FilePathList *LoadDirectoryFilesEx_(char *a, char *b, int c)
{
    FilePathList *ptr = (FilePathList *)malloc(sizeof(FilePathList));
    *ptr = LoadDirectoryFilesEx(a, b, c);
    return ptr;
}

RLBIND void UnloadDirectoryFiles_(FilePathList *a)
{
    UnloadDirectoryFiles(*a);
}

RLBIND FilePathList *LoadDroppedFiles_()
{
    FilePathList *ptr = (FilePathList *)malloc(sizeof(FilePathList));
    *ptr = LoadDroppedFiles();
    return ptr;
}

RLBIND void UnloadDroppedFiles_(FilePathList *a)
{
    UnloadDroppedFiles(*a);
}

RLBIND AutomationEventList *LoadAutomationEventList_(char *a)
{
    AutomationEventList *ptr = (AutomationEventList *)malloc(sizeof(AutomationEventList));
    *ptr = LoadAutomationEventList(a);
    return ptr;
}

RLBIND void UnloadAutomationEventList_(AutomationEventList *a)
{
    UnloadAutomationEventList(*a);
}

RLBIND bool ExportAutomationEventList_(AutomationEventList *a, char *b)
{
    return ExportAutomationEventList(*a, b);
}

RLBIND void PlayAutomationEvent_(AutomationEvent *a)
{
    PlayAutomationEvent(*a);
}

RLBIND Vector2 *GetMousePosition_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetMousePosition();
    return ptr;
}

RLBIND Vector2 *GetMouseDelta_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetMouseDelta();
    return ptr;
}

RLBIND Vector2 *GetMouseWheelMoveV_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetMouseWheelMoveV();
    return ptr;
}

RLBIND Vector2 *GetTouchPosition_(int a)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetTouchPosition(a);
    return ptr;
}

RLBIND Vector2 *GetGestureDragVector_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetGestureDragVector();
    return ptr;
}

RLBIND Vector2 *GetGesturePinchVector_()
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetGesturePinchVector();
    return ptr;
}

RLBIND void UpdateCameraPro_(Camera3D *a, Vector3 *b, Vector3 *c, float d)
{
    UpdateCameraPro(a, *b, *c, d);
}

RLBIND void SetShapesTexture_(Texture *a, Rectangle *b)
{
    SetShapesTexture(*a, *b);
}

RLBIND Texture2D *GetShapesTexture_()
{
    Texture2D *ptr = (Texture2D *)malloc(sizeof(Texture2D));
    *ptr = GetShapesTexture();
    return ptr;
}

RLBIND Rectangle *GetShapesTextureRectangle_()
{
    Rectangle *ptr = (Rectangle *)malloc(sizeof(Rectangle));
    *ptr = GetShapesTextureRectangle();
    return ptr;
}

RLBIND void DrawPixel_(int a, int b, Color *c)
{
    DrawPixel(a, b, *c);
}

RLBIND void DrawPixelV_(Vector2 *a, Color *b)
{
    DrawPixelV(*a, *b);
}

RLBIND void DrawLine_(int a, int b, int c, int d, Color *e)
{
    DrawLine(a, b, c, d, *e);
}

RLBIND void DrawLineV_(Vector2 *a, Vector2 *b, Color *c)
{
    DrawLineV(*a, *b, *c);
}

RLBIND void DrawLineEx_(Vector2 *a, Vector2 *b, float c, Color *d)
{
    DrawLineEx(*a, *b, c, *d);
}

RLBIND void DrawLineStrip_(const Vector2 *a, int b, Color *c)
{
    DrawLineStrip(a, b, *c);
}

RLBIND void DrawLineBezier_(Vector2 *a, Vector2 *b, float c, Color *d)
{
    DrawLineBezier(*a, *b, c, *d);
}

RLBIND void DrawCircle_(int a, int b, float c, Color *d)
{
    DrawCircle(a, b, c, *d);
}

RLBIND void DrawCircleSector_(Vector2 *a, float b, float c, float d, int e, Color *f)
{
    DrawCircleSector(*a, b, c, d, e, *f);
}

RLBIND void DrawCircleSectorLines_(Vector2 *a, float b, float c, float d, int e, Color *f)
{
    DrawCircleSectorLines(*a, b, c, d, e, *f);
}

RLBIND void DrawCircleGradient_(int a, int b, float c, Color *d, Color *e)
{
    DrawCircleGradient(a, b, c, *d, *e);
}

RLBIND void DrawCircleV_(Vector2 *a, float b, Color *c)
{
    DrawCircleV(*a, b, *c);
}

RLBIND void DrawCircleLines_(int a, int b, float c, Color *d)
{
    DrawCircleLines(a, b, c, *d);
}

RLBIND void DrawCircleLinesV_(Vector2 *a, float b, Color *c)
{
    DrawCircleLinesV(*a, b, *c);
}

RLBIND void DrawEllipse_(int a, int b, float c, float d, Color *e)
{
    DrawEllipse(a, b, c, d, *e);
}

RLBIND void DrawEllipseLines_(int a, int b, float c, float d, Color *e)
{
    DrawEllipseLines(a, b, c, d, *e);
}

RLBIND void DrawRing_(Vector2 *a, float b, float c, float d, float e, int f, Color *g)
{
    DrawRing(*a, b, c, d, e, f, *g);
}

RLBIND void DrawRingLines_(Vector2 *a, float b, float c, float d, float e, int f, Color *g)
{
    DrawRingLines(*a, b, c, d, e, f, *g);
}

RLBIND void DrawRectangle_(int a, int b, int c, int d, Color *e)
{
    DrawRectangle(a, b, c, d, *e);
}

RLBIND void DrawRectangleV_(Vector2 *a, Vector2 *b, Color *c)
{
    DrawRectangleV(*a, *b, *c);
}

RLBIND void DrawRectangleRec_(Rectangle *a, Color *b)
{
    DrawRectangleRec(*a, *b);
}

RLBIND void DrawRectanglePro_(Rectangle *a, Vector2 *b, float c, Color *d)
{
    DrawRectanglePro(*a, *b, c, *d);
}

RLBIND void DrawRectangleGradientV_(int a, int b, int c, int d, Color *e, Color *f)
{
    DrawRectangleGradientV(a, b, c, d, *e, *f);
}

RLBIND void DrawRectangleGradientH_(int a, int b, int c, int d, Color *e, Color *f)
{
    DrawRectangleGradientH(a, b, c, d, *e, *f);
}

RLBIND void DrawRectangleGradientEx_(Rectangle *a, Color *b, Color *c, Color *d, Color *e)
{
    DrawRectangleGradientEx(*a, *b, *c, *d, *e);
}

RLBIND void DrawRectangleLines_(int a, int b, int c, int d, Color *e)
{
    DrawRectangleLines(a, b, c, d, *e);
}

RLBIND void DrawRectangleLinesEx_(Rectangle *a, float b, Color *c)
{
    DrawRectangleLinesEx(*a, b, *c);
}

RLBIND void DrawRectangleRounded_(Rectangle *a, float b, int c, Color *d)
{
    DrawRectangleRounded(*a, b, c, *d);
}

RLBIND void DrawRectangleRoundedLines_(Rectangle *a, float b, int c, Color *d)
{
    DrawRectangleRoundedLines(*a, b, c, *d);
}

RLBIND void DrawRectangleRoundedLinesEx_(Rectangle *a, float b, int c, float d, Color *e)
{
    DrawRectangleRoundedLinesEx(*a, b, c, d, *e);
}

RLBIND void DrawTriangle_(Vector2 *a, Vector2 *b, Vector2 *c, Color *d)
{
    DrawTriangle(*a, *b, *c, *d);
}

RLBIND void DrawTriangleLines_(Vector2 *a, Vector2 *b, Vector2 *c, Color *d)
{
    DrawTriangleLines(*a, *b, *c, *d);
}

RLBIND void DrawTriangleFan_(const Vector2 *a, int b, Color *c)
{
    DrawTriangleFan(a, b, *c);
}

RLBIND void DrawTriangleStrip_(const Vector2 *a, int b, Color *c)
{
    DrawTriangleStrip(a, b, *c);
}

RLBIND void DrawPoly_(Vector2 *a, int b, float c, float d, Color *e)
{
    DrawPoly(*a, b, c, d, *e);
}

RLBIND void DrawPolyLines_(Vector2 *a, int b, float c, float d, Color *e)
{
    DrawPolyLines(*a, b, c, d, *e);
}

RLBIND void DrawPolyLinesEx_(Vector2 *a, int b, float c, float d, float e, Color *f)
{
    DrawPolyLinesEx(*a, b, c, d, e, *f);
}

RLBIND void DrawSplineLinear_(const Vector2 *a, int b, float c, Color *d)
{
    DrawSplineLinear(a, b, c, *d);
}

RLBIND void DrawSplineBasis_(const Vector2 *a, int b, float c, Color *d)
{
    DrawSplineBasis(a, b, c, *d);
}

RLBIND void DrawSplineCatmullRom_(const Vector2 *a, int b, float c, Color *d)
{
    DrawSplineCatmullRom(a, b, c, *d);
}

RLBIND void DrawSplineBezierQuadratic_(const Vector2 *a, int b, float c, Color *d)
{
    DrawSplineBezierQuadratic(a, b, c, *d);
}

RLBIND void DrawSplineBezierCubic_(const Vector2 *a, int b, float c, Color *d)
{
    DrawSplineBezierCubic(a, b, c, *d);
}

RLBIND void DrawSplineSegmentLinear_(Vector2 *a, Vector2 *b, float c, Color *d)
{
    DrawSplineSegmentLinear(*a, *b, c, *d);
}

RLBIND void DrawSplineSegmentBasis_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f)
{
    DrawSplineSegmentBasis(*a, *b, *c, *d, e, *f);
}

RLBIND void DrawSplineSegmentCatmullRom_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f)
{
    DrawSplineSegmentCatmullRom(*a, *b, *c, *d, e, *f);
}

RLBIND void DrawSplineSegmentBezierQuadratic_(Vector2 *a, Vector2 *b, Vector2 *c, float d, Color *e)
{
    DrawSplineSegmentBezierQuadratic(*a, *b, *c, d, *e);
}

RLBIND void DrawSplineSegmentBezierCubic_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e, Color *f)
{
    DrawSplineSegmentBezierCubic(*a, *b, *c, *d, e, *f);
}

RLBIND Vector2 *GetSplinePointLinear_(Vector2 *a, Vector2 *b, float c)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetSplinePointLinear(*a, *b, c);
    return ptr;
}

RLBIND Vector2 *GetSplinePointBasis_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetSplinePointBasis(*a, *b, *c, *d, e);
    return ptr;
}

RLBIND Vector2 *GetSplinePointCatmullRom_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetSplinePointCatmullRom(*a, *b, *c, *d, e);
    return ptr;
}

RLBIND Vector2 *GetSplinePointBezierQuad_(Vector2 *a, Vector2 *b, Vector2 *c, float d)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetSplinePointBezierQuad(*a, *b, *c, d);
    return ptr;
}

RLBIND Vector2 *GetSplinePointBezierCubic_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, float e)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = GetSplinePointBezierCubic(*a, *b, *c, *d, e);
    return ptr;
}

RLBIND bool CheckCollisionRecs_(Rectangle *a, Rectangle *b)
{
    return CheckCollisionRecs(*a, *b);
}

RLBIND bool CheckCollisionCircles_(Vector2 *a, float b, Vector2 *c, float d)
{
    return CheckCollisionCircles(*a, b, *c, d);
}

RLBIND bool CheckCollisionCircleRec_(Vector2 *a, float b, Rectangle *c)
{
    return CheckCollisionCircleRec(*a, b, *c);
}

RLBIND bool CheckCollisionPointRec_(Vector2 *a, Rectangle *b)
{
    return CheckCollisionPointRec(*a, *b);
}

RLBIND bool CheckCollisionPointCircle_(Vector2 *a, Vector2 *b, float c)
{
    return CheckCollisionPointCircle(*a, *b, c);
}

RLBIND bool CheckCollisionPointTriangle_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d)
{
    return CheckCollisionPointTriangle(*a, *b, *c, *d);
}

RLBIND bool CheckCollisionPointPoly_(Vector2 *a, const Vector2 *b, int c)
{
    return CheckCollisionPointPoly(*a, b, c);
}

RLBIND bool CheckCollisionLines_(Vector2 *a, Vector2 *b, Vector2 *c, Vector2 *d, Vector2 *e)
{
    return CheckCollisionLines(*a, *b, *c, *d, e);
}

RLBIND bool CheckCollisionPointLine_(Vector2 *a, Vector2 *b, Vector2 *c, int d)
{
    return CheckCollisionPointLine(*a, *b, *c, d);
}

RLBIND bool CheckCollisionCircleLine_(Vector2 *a, float b, Vector2 *c, Vector2 *d)
{
    return CheckCollisionCircleLine(*a, b, *c, *d);
}

RLBIND Rectangle *GetCollisionRec_(Rectangle *a, Rectangle *b)
{
    Rectangle *ptr = (Rectangle *)malloc(sizeof(Rectangle));
    *ptr = GetCollisionRec(*a, *b);
    return ptr;
}

RLBIND Image *LoadImage_(char *a)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImage(a);
    return ptr;
}

RLBIND Image *LoadImageRaw_(char *a, int b, int c, int d, int e)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImageRaw(a, b, c, d, e);
    return ptr;
}

RLBIND Image *LoadImageAnim_(char *a, int *b)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImageAnim(a, b);
    return ptr;
}

RLBIND Image *LoadImageAnimFromMemory_(char *a, unsigned char *b, int c, int *d)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImageAnimFromMemory(a, b, c, d);
    return ptr;
}

RLBIND Image *LoadImageFromMemory_(char *a, unsigned char *b, int c)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImageFromMemory(a, b, c);
    return ptr;
}

RLBIND Image *LoadImageFromTexture_(Texture *a)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImageFromTexture(*a);
    return ptr;
}

RLBIND Image *LoadImageFromScreen_()
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = LoadImageFromScreen();
    return ptr;
}

RLBIND bool IsImageValid_(Image *a)
{
    return IsImageValid(*a);
}

RLBIND void UnloadImage_(Image *a)
{
    UnloadImage(*a);
}

RLBIND int ExportImage_(Image *a, char *b)
{
    return ExportImage(*a, b);
}

RLBIND unsigned char *ExportImageToMemory_(Image *a, char *fileType, int *fileSize)
{
    return ExportImageToMemory(*a, fileType, fileSize);
}

RLBIND int ExportImageAsCode_(Image *a, char *b)
{
    return ExportImageAsCode(*a, b);
}

RLBIND Image *GenImageColor_(int a, int b, Color *c)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageColor(a, b, *c);
    return ptr;
}

RLBIND Image *GenImageGradientLinear_(int a, int b, int c, Color *d, Color *e)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageGradientLinear(a, b, c, *d, *e);
    return ptr;
}

RLBIND Image *GenImageGradientRadial_(int a, int b, float c, Color *d, Color *e)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageGradientRadial(a, b, c, *d, *e);
    return ptr;
}

RLBIND Image *GenImageGradientSquare_(int a, int b, float c, Color *d, Color *e)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageGradientSquare(a, b, c, *d, *e);
    return ptr;
}

RLBIND Image *GenImageChecked_(int a, int b, int c, int d, Color *e, Color *f)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageChecked(a, b, c, d, *e, *f);
    return ptr;
}

RLBIND Image *GenImageWhiteNoise_(int a, int b, float c)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageWhiteNoise(a, b, c);
    return ptr;
}

RLBIND Image *GenImagePerlinNoise_(int a, int b, int c, int d, float e)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImagePerlinNoise(a, b, c, d, e);
    return ptr;
}

RLBIND Image *GenImageCellular_(int a, int b, int c)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageCellular(a, b, c);
    return ptr;
}

RLBIND Image *GenImageText_(int a, int b, char *c)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageText(a, b, c);
    return ptr;
}

RLBIND Image *ImageCopy_(Image *a)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = ImageCopy(*a);
    return ptr;
}

RLBIND Image *ImageFromImage_(Image *a, Rectangle *b)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = ImageFromImage(*a, *b);
    return ptr;
}

RLBIND Image *ImageFromChannel_(Image *a, int b)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = ImageFromChannel(*a, b);
    return ptr;
}

RLBIND Image *ImageText_(char *a, int b, Color *c)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = ImageText(a, b, *c);
    return ptr;
}

RLBIND Image *ImageTextEx_(Font *a, char *b, float c, float d, Color *e)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = ImageTextEx(*a, b, c, d, *e);
    return ptr;
}

RLBIND void ImageToPOT_(Image *a, Color *b)
{
    ImageToPOT(a, *b);
}

RLBIND void ImageCrop_(Image *a, Rectangle *b)
{
    ImageCrop(a, *b);
}

RLBIND void ImageAlphaClear_(Image *a, Color *b, float c)
{
    ImageAlphaClear(a, *b, c);
}

RLBIND void ImageAlphaMask_(Image *a, Image *b)
{
    ImageAlphaMask(a, *b);
}

RLBIND void ImageResizeCanvas_(Image *a, int b, int c, int d, int e, Color *f)
{
    ImageResizeCanvas(a, b, c, d, e, *f);
}

RLBIND void ImageColorTint_(Image *a, Color *b)
{
    ImageColorTint(a, *b);
}

RLBIND void ImageColorReplace_(Image *a, Color *b, Color *c)
{
    ImageColorReplace(a, *b, *c);
}

RLBIND Color *LoadImageColors_(Image *a)
{
    return LoadImageColors(*a);
}

RLBIND Color *LoadImagePalette_(Image *a, int b, int *c)
{
    return LoadImagePalette(*a, b, c);
}

RLBIND Rectangle *GetImageAlphaBorder_(Image *a, float b)
{
    Rectangle *ptr = (Rectangle *)malloc(sizeof(Rectangle));
    *ptr = GetImageAlphaBorder(*a, b);
    return ptr;
}

RLBIND Color *GetImageColor_(Image *a, int b, int c)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = GetImageColor(*a, b, c);
    return ptr;
}

RLBIND void ImageClearBackground_(Image *a, Color *b)
{
    ImageClearBackground(a, *b);
}

RLBIND void ImageDrawPixel_(Image *a, int b, int c, Color *d)
{
    ImageDrawPixel(a, b, c, *d);
}

RLBIND void ImageDrawPixelV_(Image *a, Vector2 *b, Color *c)
{
    ImageDrawPixelV(a, *b, *c);
}

RLBIND void ImageDrawLine_(Image *a, int b, int c, int d, int e, Color *f)
{
    ImageDrawLine(a, b, c, d, e, *f);
}

RLBIND void ImageDrawLineV_(Image *a, Vector2 *b, Vector2 *c, Color *d)
{
    ImageDrawLineV(a, *b, *c, *d);
}

RLBIND void ImageDrawCircle_(Image *a, int b, int c, int d, Color *e)
{
    ImageDrawCircle(a, b, c, d, *e);
}

RLBIND void ImageDrawCircleV_(Image *a, Vector2 *b, int c, Color *d)
{
    ImageDrawCircleV(a, *b, c, *d);
}

RLBIND void ImageDrawCircleLines_(Image *a, int b, int c, int d, Color *e)
{
    ImageDrawCircleLines(a, b, c, d, *e);
}

RLBIND void ImageDrawCircleLinesV_(Image *a, Vector2 *b, int c, Color *d)
{
    ImageDrawCircleLinesV(a, *b, c, *d);
}

RLBIND void ImageDrawRectangle_(Image *a, int b, int c, int d, int e, Color *f)
{
    ImageDrawRectangle(a, b, c, d, e, *f);
}

RLBIND void ImageDrawRectangleV_(Image *a, Vector2 *b, Vector2 *c, Color *d)
{
    ImageDrawRectangleV(a, *b, *c, *d);
}

RLBIND void ImageDrawRectangleRec_(Image *a, Rectangle *b, Color *c)
{
    ImageDrawRectangleRec(a, *b, *c);
}

RLBIND void ImageDrawRectangleLines_(Image *a, Rectangle *b, int c, Color *d)
{
    ImageDrawRectangleLines(a, *b, c, *d);
}

RLBIND void ImageDrawTriangle_(Image *a, Vector2 *b, Vector2 *c, Vector2 *d, Color *e)
{
    ImageDrawTriangle(a, *b, *c, *d, *e);
}

RLBIND void ImageDrawTriangleEx_(Image *a, Vector2 *b, Vector2 *c, Vector2 *d, Color *e, Color *f, Color *g)
{
    ImageDrawTriangleEx(a, *b, *c, *d, *e, *f, *g);
}

RLBIND void ImageDrawTriangleLines_(Image *a, Vector2 *b, Vector2 *c, Vector2 *d, Color *e)
{
    ImageDrawTriangleLines(a, *b, *c, *d, *e);
}

RLBIND void ImageDrawTriangleFan_(Image *a, Vector2 *b, int c, Color *d)
{
    ImageDrawTriangleFan(a, b, c, *d);
}

RLBIND void ImageDrawTriangleStrip_(Image *a, Vector2 *b, int c, Color *d)
{
    ImageDrawTriangleStrip(a, b, c, *d);
}

RLBIND void ImageDraw_(Image *a, Image *b, Rectangle *c, Rectangle *d, Color *e)
{
    ImageDraw(a, *b, *c, *d, *e);
}

RLBIND void ImageDrawText_(Image *a, char *b, int c, int d, int e, Color *f)
{
    ImageDrawText(a, b, c, d, e, *f);
}

RLBIND void ImageDrawTextEx_(Image *a, Font *b, char *c, Vector2 *d, float e, float f, Color *g)
{
    ImageDrawTextEx(a, *b, c, *d, e, f, *g);
}

RLBIND Texture *LoadTexture_(char *a)
{
    Texture *ptr = (Texture *)malloc(sizeof(Texture));
    *ptr = LoadTexture(a);
    return ptr;
}

RLBIND Texture *LoadTextureFromImage_(Image *a)
{
    Texture *ptr = (Texture *)malloc(sizeof(Texture));
    *ptr = LoadTextureFromImage(*a);
    return ptr;
}

RLBIND Texture *LoadTextureCubemap_(Image *a, int b)
{
    Texture *ptr = (Texture *)malloc(sizeof(Texture));
    *ptr = LoadTextureCubemap(*a, b);
    return ptr;
}

RLBIND RenderTexture *LoadRenderTexture_(int a, int b)
{
    RenderTexture *ptr = (RenderTexture *)malloc(sizeof(RenderTexture));
    *ptr = LoadRenderTexture(a, b);
    return ptr;
}

RLBIND bool IsTextureValid_(Texture *a)
{
    return IsTextureValid(*a);
}

RLBIND void UnloadTexture_(Texture *a)
{
    UnloadTexture(*a);
}

RLBIND bool IsRenderTextureValid_(RenderTexture *a)
{
    return IsRenderTextureValid(*a);
}

RLBIND void UnloadRenderTexture_(RenderTexture *a)
{
    UnloadRenderTexture(*a);
}

RLBIND void UpdateTexture_(Texture *a, const void *b)
{
    UpdateTexture(*a, b);
}

RLBIND void UpdateTextureRec_(Texture *a, Rectangle *b, const void *c)
{
    UpdateTextureRec(*a, *b, c);
}

RLBIND void SetTextureFilter_(Texture *a, int b)
{
    SetTextureFilter(*a, b);
}

RLBIND void SetTextureWrap_(Texture *a, int b)
{
    SetTextureWrap(*a, b);
}

RLBIND void DrawTexture_(Texture *a, int b, int c, Color *d)
{
    DrawTexture(*a, b, c, *d);
}

RLBIND void DrawTextureV_(Texture *a, Vector2 *b, Color *c)
{
    DrawTextureV(*a, *b, *c);
}

RLBIND void DrawTextureEx_(Texture *a, Vector2 *b, float c, float d, Color *e)
{
    DrawTextureEx(*a, *b, c, d, *e);
}

RLBIND void DrawTextureRec_(Texture *a, Rectangle *b, Vector2 *c, Color *d)
{
    DrawTextureRec(*a, *b, *c, *d);
}

RLBIND void DrawTexturePro_(Texture *a, Rectangle *b, Rectangle *c, Vector2 *d, float e, Color *f)
{
    DrawTexturePro(*a, *b, *c, *d, e, *f);
}

RLBIND void DrawTextureNPatch_(Texture *a, NPatchInfo *b, Rectangle *c, Vector2 *d, float e, Color *f)
{
    DrawTextureNPatch(*a, *b, *c, *d, e, *f);
}

RLBIND Color *Fade_(Color *a, float b)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = Fade(*a, b);
    return ptr;
}

RLBIND int ColorToInt_(Color *a)
{
    return ColorToInt(*a);
}

RLBIND Vector4 *ColorNormalize_(Color *a)
{
    Vector4 *ptr = (Vector4 *)malloc(sizeof(Vector4));
    *ptr = ColorNormalize(*a);
    return ptr;
}

RLBIND Color *ColorFromNormalized_(Vector4 *a)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorFromNormalized(*a);
    return ptr;
}

RLBIND Vector3 *ColorToHSV_(Color *a)
{
    Vector3 *ptr = (Vector3 *)malloc(sizeof(Vector3));
    *ptr = ColorToHSV(*a);
    return ptr;
}

RLBIND Color *ColorFromHSV_(float a, float b, float c)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorFromHSV(a, b, c);
    return ptr;
}

RLBIND Color *ColorTint_(Color *a, Color *b)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorTint(*a, *b);
    return ptr;
}

RLBIND Color *ColorBrightness_(Color *a, float b)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorBrightness(*a, b);
    return ptr;
}

RLBIND Color *ColorContrast_(Color *a, float b)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorContrast(*a, b);
    return ptr;
}

RLBIND Color *ColorAlpha_(Color *a, float b)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorAlpha(*a, b);
    return ptr;
}

RLBIND Color *ColorAlphaBlend_(Color *a, Color *b, Color *c)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorAlphaBlend(*a, *b, *c);
    return ptr;
}

RLBIND Color *ColorLerp_(Color *a, Color *b, float c)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = ColorLerp(*a, *b, c);
    return ptr;
}

RLBIND Color *GetColor_(unsigned int a)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = GetColor(a);
    return ptr;
}

RLBIND Color *GetPixelColor_(void *a, int b)
{
    Color *ptr = (Color *)malloc(sizeof(Color));
    *ptr = GetPixelColor(a, b);
    return ptr;
}

RLBIND void SetPixelColor_(void *a, Color *b, int c)
{
    SetPixelColor(a, *b, c);
}

RLBIND int GetPixelDataSize_(int a, int b, int c)
{
    return GetPixelDataSize(a, b, c);
}

RLBIND Font *GetFontDefault_()
{
    Font defaultFont = GetFontDefault();
    Font *defaultFontCopy = (Font *)malloc(sizeof(Font));

    defaultFontCopy->baseSize = defaultFont.baseSize;
    defaultFontCopy->glyphCount = defaultFont.glyphCount;
    defaultFontCopy->glyphPadding = defaultFont.glyphPadding;
    defaultFontCopy->texture = defaultFont.texture;

    defaultFontCopy->glyphs = malloc(sizeof(GlyphInfo) * defaultFont.glyphCount);
    defaultFontCopy->recs = malloc(sizeof(Rectangle) * defaultFont.glyphCount);

    for (int i = 0; i < defaultFont.glyphCount; i++)
    {
        defaultFontCopy->glyphs[i] = defaultFont.glyphs[i];
        defaultFontCopy->glyphs[i].image = ImageCopy(defaultFont.glyphs[i].image);
        defaultFontCopy->recs[i] = defaultFont.recs[i];
    }

    return defaultFontCopy;
}

RLBIND Font *LoadFont_(char *a)
{
    Font *ptr = (Font *)malloc(sizeof(Font));
    *ptr = LoadFont(a);
    return ptr;
}

RLBIND Font *LoadFontEx_(char *a, int b, int *c, int d)
{
    Font *ptr = (Font *)malloc(sizeof(Font));
    *ptr = LoadFontEx(a, b, c, d);
    return ptr;
}

RLBIND Font *LoadFontFromImage_(Image *a, Color *b, int c)
{
    Font *ptr = (Font *)malloc(sizeof(Font));
    *ptr = LoadFontFromImage(*a, *b, c);
    return ptr;
}

RLBIND Font *LoadFontFromMemory_(char *a, unsigned char *b, int c, int d, int *e, int f)
{
    Font *ptr = (Font *)malloc(sizeof(Font));
    *ptr = LoadFontFromMemory(a, b, c, d, e, f);
    return ptr;
}

RLBIND Image *GenImageFontAtlas_(GlyphInfo *a, Rectangle **b, int c, int d, int e, int f)
{
    Image *ptr = (Image *)malloc(sizeof(Image));
    *ptr = GenImageFontAtlas(a, b, c, d, e, f);
    return ptr;
}

RLBIND bool IsFontValid_(Font *a)
{
    return IsFontValid(*a);
}

RLBIND void UnloadFont_(Font *a)
{
    UnloadFont(*a);
}

RLBIND int ExportFontAsCode_(Font *a, char *b)
{
    return ExportFontAsCode(*a, b);
}

RLBIND void DrawText_(char *a, int b, int c, int d, Color *e)
{
    DrawText(a, b, c, d, *e);
}

RLBIND void DrawTextEx_(Font *a, char *b, Vector2 *c, float d, float e, Color *f)
{
    DrawTextEx(*a, b, *c, d, e, *f);
}

RLBIND void DrawTextPro_(Font *a, char *b, Vector2 *c, Vector2 *d, float e, float f, float g, Color *h)
{
    DrawTextPro(*a, b, *c, *d, e, f, g, *h);
}

RLBIND void DrawTextCodepoint_(Font *a, int b, Vector2 *c, float d, Color *e)
{
    DrawTextCodepoint(*a, b, *c, d, *e);
}

RLBIND void DrawTextCodepoints_(Font *a, int *b, int c, Vector2 *d, float e, float f, Color *g)
{
    DrawTextCodepoints(*a, b, c, *d, e, f, *g);
}

RLBIND Vector2 *MeasureTextEx_(Font *a, char *b, float c, float d)
{
    Vector2 *ptr = (Vector2 *)malloc(sizeof(Vector2));
    *ptr = MeasureTextEx(*a, b, c, d);
    return ptr;
}

RLBIND int GetGlyphIndex_(Font *a, int b)
{
    return GetGlyphIndex(*a, b);
}

RLBIND GlyphInfo *GetGlyphInfo_(Font *a, int b)
{
    GlyphInfo *ptr = (GlyphInfo *)malloc(sizeof(GlyphInfo));
    *ptr = GetGlyphInfo(*a, b);
    return ptr;
}

RLBIND Rectangle *GetGlyphAtlasRec_(Font *a, int b)
{
    Rectangle *ptr = (Rectangle *)malloc(sizeof(Rectangle));
    *ptr = GetGlyphAtlasRec(*a, b);
    return ptr;
}

RLBIND void DrawLine3D_(Vector3 *a, Vector3 *b, Color *c)
{
    DrawLine3D(*a, *b, *c);
}

RLBIND void DrawPoint3D_(Vector3 *a, Color *b)
{
    DrawPoint3D(*a, *b);
}

RLBIND void DrawCircle3D_(Vector3 *a, float b, Vector3 *c, float d, Color *e)
{
    DrawCircle3D(*a, b, *c, d, *e);
}

RLBIND void DrawTriangle3D_(Vector3 *a, Vector3 *b, Vector3 *c, Color *d)
{
    DrawTriangle3D(*a, *b, *c, *d);
}

RLBIND void DrawTriangleStrip3D_(const Vector3 *a, int b, Color *c)
{
    DrawTriangleStrip3D(a, b, *c);
}

RLBIND void DrawCube_(Vector3 *a, float b, float c, float d, Color *e)
{
    DrawCube(*a, b, c, d, *e);
}

RLBIND void DrawCubeV_(Vector3 *a, Vector3 *b, Color *c)
{
    DrawCubeV(*a, *b, *c);
}

RLBIND void DrawCubeWires_(Vector3 *a, float b, float c, float d, Color *e)
{
    DrawCubeWires(*a, b, c, d, *e);
}

RLBIND void DrawCubeWiresV_(Vector3 *a, Vector3 *b, Color *c)
{
    DrawCubeWiresV(*a, *b, *c);
}

RLBIND void DrawSphere_(Vector3 *a, float b, Color *c)
{
    DrawSphere(*a, b, *c);
}

RLBIND void DrawSphereEx_(Vector3 *a, float b, int c, int d, Color *e)
{
    DrawSphereEx(*a, b, c, d, *e);
}

RLBIND void DrawSphereWires_(Vector3 *a, float b, int c, int d, Color *e)
{
    DrawSphereWires(*a, b, c, d, *e);
}

RLBIND void DrawCylinder_(Vector3 *a, float b, float c, float d, int e, Color *f)
{
    DrawCylinder(*a, b, c, d, e, *f);
}

RLBIND void DrawCylinderEx_(Vector3 *a, Vector3 *b, float c, float d, int e, Color *f)
{
    DrawCylinderEx(*a, *b, c, d, e, *f);
}

RLBIND void DrawCylinderWires_(Vector3 *a, float b, float c, float d, int e, Color *f)
{
    DrawCylinderWires(*a, b, c, d, e, *f);
}

RLBIND void DrawCylinderWiresEx_(Vector3 *a, Vector3 *b, float c, float d, int e, Color *f)
{
    DrawCylinderWiresEx(*a, *b, c, d, e, *f);
}

RLBIND void DrawCapsule_(Vector3 *a, Vector3 *b, float c, int d, int e, Color *f)
{
    DrawCapsule(*a, *b, c, d, e, *f);
}

RLBIND void DrawCapsuleWires_(Vector3 *a, Vector3 *b, float c, int d, int e, Color *f)
{
    DrawCapsuleWires(*a, *b, c, d, e, *f);
}

RLBIND void DrawPlane_(Vector3 *a, Vector2 *b, Color *c)
{
    DrawPlane(*a, *b, *c);
}

RLBIND void DrawRay_(Ray *a, Color *b)
{
    DrawRay(*a, *b);
}

RLBIND Model *LoadModel_(char *a)
{
    Model *ptr = (Model *)malloc(sizeof(Model));
    *ptr = LoadModel(a);
    return ptr;
}

RLBIND Model *LoadModelFromMesh_(Mesh *a)
{
    Model *ptr = (Model *)malloc(sizeof(Model));
    *ptr = LoadModelFromMesh(*a);
    return ptr;
}

RLBIND bool IsModelValid_(Model *a)
{
    return IsModelValid(*a);
}

RLBIND void UnloadModel_(Model *a)
{
    UnloadModel(*a);
}

RLBIND BoundingBox *GetModelBoundingBox_(Model *a)
{
    BoundingBox *ptr = (BoundingBox *)malloc(sizeof(BoundingBox));
    *ptr = GetModelBoundingBox(*a);
    return ptr;
}

RLBIND void DrawModel_(Model *a, Vector3 *b, float c, Color *d)
{
    DrawModel(*a, *b, c, *d);
}

RLBIND void DrawModelEx_(Model *a, Vector3 *b, Vector3 *c, float d, Vector3 *e, Color *f)
{
    DrawModelEx(*a, *b, *c, d, *e, *f);
}

RLBIND void DrawModelWires_(Model *a, Vector3 *b, float c, Color *d)
{
    DrawModelWires(*a, *b, c, *d);
}

RLBIND void DrawModelWiresEx_(Model *a, Vector3 *b, Vector3 *c, float d, Vector3 *e, Color *f)
{
    DrawModelWiresEx(*a, *b, *c, d, *e, *f);
}

RLBIND void DrawModelPoints_(Model *a, Vector3 *b, float c, Color *d)
{
    DrawModelPoints(*a, *b, c, *d);
}

RLBIND void DrawModelPointsEx_(Model *a, Vector3 *b, Vector3 *c, float d, Vector3 *e, Color *f)
{
    DrawModelPointsEx(*a, *b, *c, d, *e, *f);
}

RLBIND void DrawBoundingBox_(BoundingBox *a, Color *b)
{
    DrawBoundingBox(*a, *b);
}

RLBIND void DrawBillboard_(Camera3D *a, Texture *b, Vector3 *c, float d, Color *e)
{
    DrawBillboard(*a, *b, *c, d, *e);
}

RLBIND void DrawBillboardRec_(Camera3D *a, Texture *b, Rectangle *c, Vector3 *d, Vector2 *e, Color *f)
{
    DrawBillboardRec(*a, *b, *c, *d, *e, *f);
}

RLBIND void DrawBillboardPro_(Camera3D *a, Texture *b, Rectangle *c, Vector3 *d, Vector3 *e, Vector2 *f, Vector2 *g, float h, Color *i)
{
    DrawBillboardPro(*a, *b, *c, *d, *e, *f, *g, h, *i);
}

RLBIND void UpdateMeshBuffer_(Mesh *a, int b, const void *c, int d, int e)
{
    UpdateMeshBuffer(*a, b, c, d, e);
}

RLBIND void UnloadMesh_(Mesh *a)
{
    UnloadMesh(*a);
}

RLBIND void DrawMesh_(Mesh *a, Material *b, Matrix *c)
{
    DrawMesh(*a, *b, *c);
}

RLBIND void DrawMeshInstanced_(Mesh *a, Material *b, Matrix *c, int d)
{
    DrawMeshInstanced(*a, *b, c, d);
}

RLBIND bool ExportMesh_(Mesh *a, char *b)
{
    return ExportMesh(*a, b);
}

RLBIND bool ExportMeshAsCode_(Mesh *a, char *b)
{
    return ExportMeshAsCode(*a, b);
}

RLBIND BoundingBox *GetMeshBoundingBox_(Mesh *a)
{
    BoundingBox *ptr = (BoundingBox *)malloc(sizeof(BoundingBox));
    *ptr = GetMeshBoundingBox(*a);
    return ptr;
}

RLBIND Mesh *GenMeshPoly_(int a, float b)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshPoly(a, b);
    return ptr;
}

RLBIND Mesh *GenMeshPlane_(float a, float b, int c, int d)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshPlane(a, b, c, d);
    return ptr;
}

RLBIND Mesh *GenMeshCube_(float a, float b, float c)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshCube(a, b, c);
    return ptr;
}

RLBIND Mesh *GenMeshSphere_(float a, int b, int c)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshSphere(a, b, c);
    return ptr;
}

RLBIND Mesh *GenMeshHemiSphere_(float a, int b, int c)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshHemiSphere(a, b, c);
    return ptr;
}

RLBIND Mesh *GenMeshCylinder_(float a, float b, int c)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshCylinder(a, b, c);
    return ptr;
}

RLBIND Mesh *GenMeshCone_(float a, float b, int c)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshCone(a, b, c);
    return ptr;
}

RLBIND Mesh *GenMeshTorus_(float a, float b, int c, int d)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshTorus(a, b, c, d);
    return ptr;
}

RLBIND Mesh *GenMeshKnot_(float a, float b, int c, int d)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshKnot(a, b, c, d);
    return ptr;
}

RLBIND Mesh *GenMeshHeightmap_(Image *a, Vector3 *b)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshHeightmap(*a, *b);
    return ptr;
}

RLBIND Mesh *GenMeshCubicmap_(Image *a, Vector3 *b)
{
    Mesh *ptr = (Mesh *)malloc(sizeof(Mesh));
    *ptr = GenMeshCubicmap(*a, *b);
    return ptr;
}

RLBIND Material *LoadMaterialDefault_()
{
    Material *ptr = (Material *)malloc(sizeof(Material));
    *ptr = LoadMaterialDefault();
    return ptr;
}

RLBIND bool IsMaterialValid_(Material *a)
{
    return IsMaterialValid(*a);
}

RLBIND void UnloadMaterial_(Material *a)
{
    UnloadMaterial(*a);
}

RLBIND void SetMaterialTexture_(Material *a, int b, Texture *c)
{
    SetMaterialTexture(a, b, *c);
}

RLBIND void UpdateModelAnimation_(Model *a, ModelAnimation *b, int c)
{
    UpdateModelAnimation(*a, *b, c);
}

RLBIND void UnloadModelAnimation_(ModelAnimation *a)
{
    UnloadModelAnimation(*a);
}

RLBIND bool IsModelAnimationValid_(Model *a, ModelAnimation *b)
{
    return IsModelAnimationValid(*a, *b);
}

RLBIND void UpdateModelAnimationBoneMatrices_(Model *a, ModelAnimation *b, int c)
{
    UpdateModelAnimationBoneMatrices(*a, *b, c);
}

RLBIND bool CheckCollisionSpheres_(Vector3 *a, float b, Vector3 *c, float d)
{
    return CheckCollisionSpheres(*a, b, *c, d);
}

RLBIND bool CheckCollisionBoxes_(BoundingBox *a, BoundingBox *b)
{
    return CheckCollisionBoxes(*a, *b);
}

RLBIND bool CheckCollisionBoxSphere_(BoundingBox *a, Vector3 *b, float c)
{
    return CheckCollisionBoxSphere(*a, *b, c);
}

RLBIND RayCollision *GetRayCollisionSphere_(Ray *a, Vector3 *b, float c)
{
    RayCollision *ptr = (RayCollision *)malloc(sizeof(RayCollision));
    *ptr = GetRayCollisionSphere(*a, *b, c);
    return ptr;
}

RLBIND RayCollision *GetRayCollisionBox_(Ray *a, BoundingBox *b)
{
    RayCollision *ptr = (RayCollision *)malloc(sizeof(RayCollision));
    *ptr = GetRayCollisionBox(*a, *b);
    return ptr;
}

RLBIND RayCollision *GetRayCollisionMesh_(Ray *a, Mesh *b, Matrix *c)
{
    RayCollision *ptr = (RayCollision *)malloc(sizeof(RayCollision));
    *ptr = GetRayCollisionMesh(*a, *b, *c);
    return ptr;
}

RLBIND RayCollision *GetRayCollisionTriangle_(Ray *a, Vector3 *b, Vector3 *c, Vector3 *d)
{
    RayCollision *ptr = (RayCollision *)malloc(sizeof(RayCollision));
    *ptr = GetRayCollisionTriangle(*a, *b, *c, *d);
    return ptr;
}

RLBIND RayCollision *GetRayCollisionQuad_(Ray *a, Vector3 *b, Vector3 *c, Vector3 *d, Vector3 *e)
{
    RayCollision *ptr = (RayCollision *)malloc(sizeof(RayCollision));
    *ptr = GetRayCollisionQuad(*a, *b, *c, *d, *e);
    return ptr;
}

RLBIND Wave *LoadWave_(char *a)
{
    Wave *ptr = (Wave *)malloc(sizeof(Wave));
    *ptr = LoadWave(a);
    return ptr;
}

RLBIND Wave *LoadWaveFromMemory_(char *a, unsigned char *b, int c)
{
    Wave *ptr = (Wave *)malloc(sizeof(Wave));
    *ptr = LoadWaveFromMemory(a, b, c);
    return ptr;
}

RLBIND Sound *LoadSound_(char *a)
{
    Sound *ptr = (Sound *)malloc(sizeof(Sound));
    *ptr = LoadSound(a);
    return ptr;
}

RLBIND Sound *LoadSoundFromWave_(Wave *a)
{
    Sound *ptr = (Sound *)malloc(sizeof(Sound));
    *ptr = LoadSoundFromWave(*a);
    return ptr;
}

RLBIND Sound *LoadSoundAlias_(Sound *a)
{
    Sound *ptr = (Sound *)malloc(sizeof(Sound));
    *ptr = LoadSoundAlias(*a);
    return ptr;
}

RLBIND void UpdateSound_(Sound *a, const void *b, int c)
{
    UpdateSound(*a, b, c);
}

RLBIND bool IsWaveValid_(Wave *a)
{
    return IsWaveValid(*a);
}

RLBIND void UnloadWave_(Wave *a)
{
    UnloadWave(*a);
}

RLBIND bool IsSoundValid_(Sound *a)
{
    return IsSoundValid(*a);
}

RLBIND void UnloadSound_(Sound *a)
{
    UnloadSound(*a);
}

RLBIND void UnloadSoundAlias_(Sound *a)
{
    UnloadSoundAlias(*a);
}

RLBIND int ExportWave_(Wave *a, char *b)
{
    return ExportWave(*a, b);
}

RLBIND int ExportWaveAsCode_(Wave *a, char *b)
{
    return ExportWaveAsCode(*a, b);
}

RLBIND void PlaySound_(Sound *a)
{
    PlaySound(*a);
}

RLBIND void StopSound_(Sound *a)
{
    StopSound(*a);
}

RLBIND void PauseSound_(Sound *a)
{
    PauseSound(*a);
}

RLBIND void ResumeSound_(Sound *a)
{
    ResumeSound(*a);
}

RLBIND int IsSoundPlaying_(Sound *a)
{
    return IsSoundPlaying(*a);
}

RLBIND void SetSoundVolume_(Sound *a, float b)
{
    SetSoundVolume(*a, b);
}

RLBIND void SetSoundPitch_(Sound *a, float b)
{
    SetSoundPitch(*a, b);
}

RLBIND void SetSoundPan_(Sound *a, float b)
{
    SetSoundPan(*a, b);
}

RLBIND Wave *WaveCopy_(Wave *a)
{
    Wave *ptr = (Wave *)malloc(sizeof(Wave));
    *ptr = WaveCopy(*a);
    return ptr;
}

RLBIND float *LoadWaveSamples_(Wave *a)
{
    return LoadWaveSamples(*a);
}

RLBIND Music *LoadMusicStream_(char *a)
{
    Music *ptr = (Music *)malloc(sizeof(Music));
    *ptr = LoadMusicStream(a);
    return ptr;
}

RLBIND Music *LoadMusicStreamFromMemory_(char *a, unsigned char *b, int c)
{
    Music *ptr = (Music *)malloc(sizeof(Music));
    *ptr = LoadMusicStreamFromMemory(a, b, c);
    return ptr;
}

RLBIND bool IsMusicValid_(Music *a)
{
    return IsMusicValid(*a);
}

RLBIND void UnloadMusicStream_(Music *a)
{
    UnloadMusicStream(*a);
}

RLBIND void PlayMusicStream_(Music *a)
{
    PlayMusicStream(*a);
}

RLBIND int IsMusicStreamPlaying_(Music *a)
{
    return IsMusicStreamPlaying(*a);
}

RLBIND void UpdateMusicStream_(Music *a)
{
    UpdateMusicStream(*a);
}

RLBIND void StopMusicStream_(Music *a)
{
    StopMusicStream(*a);
}

RLBIND void PauseMusicStream_(Music *a)
{
    PauseMusicStream(*a);
}

RLBIND void ResumeMusicStream_(Music *a)
{
    ResumeMusicStream(*a);
}

RLBIND void SeekMusicStream_(Music *a, float b)
{
    SeekMusicStream(*a, b);
}

RLBIND void SetMusicVolume_(Music *a, float b)
{
    SetMusicVolume(*a, b);
}

RLBIND void SetMusicPitch_(Music *a, float b)
{
    SetMusicPitch(*a, b);
}

RLBIND void SetMusicPan_(Music *a, float b)
{
    SetMusicPan(*a, b);
}

RLBIND float GetMusicTimeLength_(Music *a)
{
    return GetMusicTimeLength(*a);
}

RLBIND float GetMusicTimePlayed_(Music *a)
{
    return GetMusicTimePlayed(*a);
}

RLBIND AudioStream *LoadAudioStream_(unsigned int a, unsigned int b, unsigned int c)
{
    AudioStream *ptr = (AudioStream *)malloc(sizeof(AudioStream));
    *ptr = LoadAudioStream(a, b, c);
    return ptr;
}

RLBIND bool IsAudioStreamValid_(AudioStream *a)
{
    return IsAudioStreamValid(*a);
}

RLBIND void UnloadAudioStream_(AudioStream *a)
{
    UnloadAudioStream(*a);
}

RLBIND void UpdateAudioStream_(AudioStream *a, const void *b, int c)
{
    UpdateAudioStream(*a, b, c);
}

RLBIND int IsAudioStreamProcessed_(AudioStream *a)
{
    return IsAudioStreamProcessed(*a);
}

RLBIND void PlayAudioStream_(AudioStream *a)
{
    PlayAudioStream(*a);
}

RLBIND void PauseAudioStream_(AudioStream *a)
{
    PauseAudioStream(*a);
}

RLBIND void ResumeAudioStream_(AudioStream *a)
{
    ResumeAudioStream(*a);
}

RLBIND int IsAudioStreamPlaying_(AudioStream *a)
{
    return IsAudioStreamPlaying(*a);
}

RLBIND void StopAudioStream_(AudioStream *a)
{
    StopAudioStream(*a);
}

RLBIND void SetAudioStreamVolume_(AudioStream *a, float b)
{
    SetAudioStreamVolume(*a, b);
}

RLBIND void SetAudioStreamPitch_(AudioStream *a, float b)
{
    SetAudioStreamPitch(*a, b);
}

RLBIND void SetAudioStreamPan_(AudioStream *a, float b)
{
    SetAudioStreamPan(*a, b);
}

RLBIND void SetAudioStreamCallback_(AudioStream *a, AudioCallback b)
{
    SetAudioStreamCallback(*a, b);
}

RLBIND void AttachAudioStreamProcessor_(AudioStream *a, AudioCallback b)
{
    AttachAudioStreamProcessor(*a, b);
}

RLBIND void DetachAudioStreamProcessor_(AudioStream *a, AudioCallback b)
{
    DetachAudioStreamProcessor(*a, b);
}

RLBIND void AttachAudioMixedProcessor_(AudioCallback a)
{
    AttachAudioMixedProcessor(a);
}

RLBIND void DetachAudioMixedProcessor_(AudioCallback a)
{
    DetachAudioMixedProcessor(a);
}

RLBIND void InitWindow_(int a, int b, const char *c)
{
    InitWindow(a, b, c);
}

RLBIND bool WindowShouldClose_()
{
    return WindowShouldClose();
}

RLBIND void CloseWindow_()
{
    CloseWindow();
}

RLBIND bool IsWindowReady_()
{
    return IsWindowReady();
}

RLBIND bool IsWindowFullscreen_()
{
    return IsWindowFullscreen();
}

RLBIND bool IsWindowHidden_()
{
    return IsWindowHidden();
}

RLBIND bool IsWindowMinimized_()
{
    return IsWindowMinimized();
}

RLBIND bool IsWindowMaximized_()
{
    return IsWindowMaximized();
}

RLBIND bool IsWindowFocused_()
{
    return IsWindowFocused();
}

RLBIND bool IsWindowResized_()
{
    return IsWindowResized();
}

RLBIND bool IsWindowState_(unsigned int a)
{
    return IsWindowState(a);
}

RLBIND void SetWindowState_(unsigned int a)
{
    SetWindowState(a);
}

RLBIND void ClearWindowState_(unsigned int a)
{
    ClearWindowState(a);
}

RLBIND void ToggleFullscreen_()
{
    ToggleFullscreen();
}

RLBIND void ToggleBorderlessWindowed_()
{
    ToggleBorderlessWindowed();
}

RLBIND void MaximizeWindow_()
{
    MaximizeWindow();
}

RLBIND void MinimizeWindow_()
{
    MinimizeWindow();
}

RLBIND void RestoreWindow_()
{
    RestoreWindow();
}

RLBIND void SetWindowIcons_(Image *a, int b)
{
    SetWindowIcons(a, b);
}

RLBIND void SetWindowTitle_(const char *a)
{
    SetWindowTitle(a);
}

RLBIND void SetWindowPosition_(int a, int b)
{
    SetWindowPosition(a, b);
}

RLBIND void SetWindowMonitor_(int a)
{
    SetWindowMonitor(a);
}

RLBIND void SetWindowMinSize_(int a, int b)
{
    SetWindowMinSize(a, b);
}

RLBIND void SetWindowMaxSize_(int a, int b)
{
    SetWindowMaxSize(a, b);
}

RLBIND void SetWindowSize_(int a, int b)
{
    SetWindowSize(a, b);
}

RLBIND void SetWindowOpacity_(float a)
{
    SetWindowOpacity(a);
}

RLBIND void SetWindowFocused_()
{
    SetWindowFocused();
}

RLBIND void *GetWindowHandle_()
{
    return GetWindowHandle();
}

RLBIND int GetScreenWidth_()
{
    return GetScreenWidth();
}

RLBIND int GetScreenHeight_()
{
    return GetScreenHeight();
}

RLBIND int GetRenderWidth_()
{
    return GetRenderWidth();
}

RLBIND int GetRenderHeight_()
{
    return GetRenderHeight();
}

RLBIND int GetMonitorCount_()
{
    return GetMonitorCount();
}

RLBIND int GetCurrentMonitor_()
{
    return GetCurrentMonitor();
}

RLBIND int GetMonitorWidth_(int a)
{
    return GetMonitorWidth(a);
}

RLBIND int GetMonitorHeight_(int a)
{
    return GetMonitorHeight(a);
}

RLBIND int GetMonitorPhysicalWidth_(int a)
{
    return GetMonitorPhysicalWidth(a);
}

RLBIND int GetMonitorPhysicalHeight_(int a)
{
    return GetMonitorPhysicalHeight(a);
}

RLBIND int GetMonitorRefreshRate_(int a)
{
    return GetMonitorRefreshRate(a);
}

RLBIND const char *GetMonitorName_(int a)
{
    return GetMonitorName(a);
}

RLBIND void SetClipboardText_(const char *a)
{
    SetClipboardText(a);
}

RLBIND const char *GetClipboardText_()
{
    return GetClipboardText();
}

RLBIND void EnableEventWaiting_()
{
    EnableEventWaiting();
}

RLBIND void DisableEventWaiting_()
{
    DisableEventWaiting();
}

RLBIND void SwapScreenBuffer_()
{
    SwapScreenBuffer();
}

RLBIND void PollInputEvents_()
{
    PollInputEvents();
}

RLBIND void WaitTime_(double a)
{
    WaitTime(a);
}

RLBIND void ShowCursor_()
{
    ShowCursor();
}

RLBIND void HideCursor_()
{
    HideCursor();
}

RLBIND bool IsCursorHidden_()
{
    return IsCursorHidden();
}

RLBIND void EnableCursor_()
{
    EnableCursor();
}

RLBIND void DisableCursor_()
{
    DisableCursor();
}

RLBIND bool IsCursorOnScreen_()
{
    return IsCursorOnScreen();
}

RLBIND void BeginDrawing_()
{
    BeginDrawing();
}

RLBIND void EndDrawing_()
{
    EndDrawing();
}

RLBIND void EndMode2D_()
{
    EndMode2D();
}

RLBIND void EndMode3D_()
{
    EndMode3D();
}

RLBIND void EndTextureMode_()
{
    EndTextureMode();
}

RLBIND void EndShaderMode_()
{
    EndShaderMode();
}

RLBIND void BeginBlendMode_(int a)
{
    BeginBlendMode(a);
}

RLBIND void EndBlendMode_()
{
    EndBlendMode();
}

RLBIND void BeginScissorMode_(int a, int b, int c, int d)
{
    BeginScissorMode(a, b, c, d);
}

RLBIND void EndScissorMode_()
{
    EndScissorMode();
}

RLBIND void EndVrStereoMode_()
{
    EndVrStereoMode();
}

RLBIND void SetTargetFPS_(int a)
{
    SetTargetFPS(a);
}

RLBIND int GetFPS_()
{
    return GetFPS();
}

RLBIND float GetFrameTime_()
{
    return GetFrameTime();
}

RLBIND double GetTime_()
{
    return GetTime();
}

RLBIND void SetRandomSeed_(unsigned int a)
{
    SetRandomSeed(a);
}

RLBIND int GetRandomValue_(int a, int b)
{
    return GetRandomValue(a, b);
}

RLBIND int *LoadRandomSequence_(unsigned int a, int b, int c)
{
    return LoadRandomSequence(a, b, c);
}

RLBIND void TakeScreenshot_(const char *a)
{
    TakeScreenshot(a);
}

RLBIND void SetConfigFlags_(unsigned int a)
{
    SetConfigFlags(a);
}

RLBIND void TraceLog_(int a, const char *b)
{
    TraceLog(a, b);
}

RLBIND void SetTraceLogLevel_(int a)
{
    SetTraceLogLevel(a);
}

RLBIND void *MemAlloc_(unsigned int a)
{
    return MemAlloc(a);
}

RLBIND void *MemRealloc_(void *a, unsigned int b)
{
    return MemRealloc(a, b);
}

RLBIND void MemFree_(void *a)
{
    MemFree(a);
}

RLBIND void OpenURL_(const char *a)
{
    OpenURL(a);
}

TraceLogCallback_ customCallback;

void CustomCallback(int logLevel, const char *text, va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);

    int len = vsnprintf(NULL, 0, text, args_copy);
    va_end(args_copy);

    if (len < 0) return;

    char *formatted = malloc(len + 1);

    vsnprintf(formatted, len + 1, text, args);
    customCallback(logLevel, formatted);

    free(formatted);
}

RLBIND void SetTraceLogCallback_(TraceLogCallback_ a)
{
    customCallback = a;
    SetTraceLogCallback(&CustomCallback);
}

RLBIND void SetLoadFileDataCallback_(LoadFileDataCallback a)
{
    SetLoadFileDataCallback(a);
}

RLBIND void SetSaveFileDataCallback_(SaveFileDataCallback a)
{
    SetSaveFileDataCallback(a);
}

RLBIND void SetLoadFileTextCallback_(LoadFileTextCallback a)
{
    SetLoadFileTextCallback(a);
}

RLBIND void SetSaveFileTextCallback_(SaveFileTextCallback a)
{
    SetSaveFileTextCallback(a);
}

RLBIND unsigned char *LoadFileData_(const char *a, int *b)
{
    return LoadFileData(a, b);
}

RLBIND void UnloadFileData_(unsigned char *a)
{
    UnloadFileData(a);
}

RLBIND bool SaveFileData_(const char *a, void *b, int c)
{
    return SaveFileData(a, b, c);
}

RLBIND bool ExportDataAsCode_(const unsigned char *a, int b, const char *c)
{
    return ExportDataAsCode(a, b, c);
}

RLBIND char *LoadFileText_(const char *a)
{
    return LoadFileText(a);
}

RLBIND void UnloadFileText_(char *a)
{
    UnloadFileText(a);
}

RLBIND bool SaveFileText_(const char *a, char *b)
{
    return SaveFileText(a, b);
}

RLBIND bool FileExists_(const char *a)
{
    return FileExists(a);
}

RLBIND bool DirectoryExists_(const char *a)
{
    return DirectoryExists(a);
}

RLBIND bool IsFileExtension_(const char *a, const char *b)
{
    return IsFileExtension(a, b);
}

RLBIND int GetFileLength_(const char *a)
{
    return GetFileLength(a);
}

RLBIND const char *GetFileExtension_(const char *a)
{
    return GetFileExtension(a);
}

RLBIND const char *GetFileName_(const char *a)
{
    return GetFileName(a);
}

RLBIND const char *GetFileNameWithoutExt_(const char *a)
{
    return GetFileNameWithoutExt(a);
}

RLBIND const char *GetDirectoryPath_(const char *a)
{
    return GetDirectoryPath(a);
}

RLBIND const char *GetPrevDirectoryPath_(const char *a)
{
    return GetPrevDirectoryPath(a);
}

RLBIND const char *GetWorkingDirectory_()
{
    return GetWorkingDirectory();
}

RLBIND const char *GetApplicationDirectory_()
{
    return GetApplicationDirectory();
}

RLBIND int MakeDirectory_(const char *a)
{
    return MakeDirectory(a);
}

RLBIND bool ChangeDirectory_(const char *a)
{
    return ChangeDirectory(a);
}

RLBIND bool IsPathFile_(const char *a)
{
    return IsPathFile(a);
}

RLBIND bool IsFileNameValid_(const char *a)
{
    return IsFileNameValid(a);
}

RLBIND bool IsFileDropped_()
{
    return IsFileDropped();
}

RLBIND long GetFileModTime_(const char *a)
{
    return GetFileModTime(a);
}

RLBIND unsigned char *CompressData_(const unsigned char *a, int b, int *c)
{
    return CompressData(a, b, c);
}

RLBIND unsigned char *DecompressData_(const unsigned char *a, int b, int *c)
{
    return DecompressData(a, b, c);
}

RLBIND char *EncodeDataBase64_(const unsigned char *a, int b, int *c)
{
    return EncodeDataBase64(a, b, c);
}

RLBIND unsigned char *DecodeDataBase64_(const unsigned char *a, int *b)
{
    return DecodeDataBase64(a, b);
}

RLBIND unsigned int ComputeCRC32_(unsigned char *a, int b)
{
    return ComputeCRC32(a, b);
}

RLBIND unsigned int *ComputeMD5_(unsigned char *a, int b)
{
    return ComputeMD5(a, b);
}

RLBIND unsigned int *ComputeSHA1_(unsigned char *a, int b)
{
    return ComputeSHA1(a, b);
}

RLBIND void SetAutomationEventList_(AutomationEventList *a)
{
    SetAutomationEventList(a);
}

RLBIND void SetAutomationEventBaseFrame_(int a)
{
    SetAutomationEventBaseFrame(a);
}

RLBIND void StartAutomationEventRecording_()
{
    StartAutomationEventRecording();
}

RLBIND void StopAutomationEventRecording_()
{
    StopAutomationEventRecording();
}

RLBIND bool IsKeyPressed_(int a)
{
    return IsKeyPressed(a);
}

RLBIND bool IsKeyPressedRepeat_(int a)
{
    return IsKeyPressedRepeat(a);
}

RLBIND bool IsKeyDown_(int a)
{
    return IsKeyDown(a);
}

RLBIND bool IsKeyReleased_(int a)
{
    return IsKeyReleased(a);
}

RLBIND bool IsKeyUp_(int a)
{
    return IsKeyUp(a);
}

RLBIND void SetExitKey_(int a)
{
    SetExitKey(a);
}

RLBIND int GetKeyPressed_()
{
    return GetKeyPressed();
}

RLBIND int GetCharPressed_()
{
    return GetCharPressed();
}

RLBIND bool IsGamepadAvailable_(int a)
{
    return IsGamepadAvailable(a);
}

RLBIND const char *GetGamepadName_(int a)
{
    return GetGamepadName(a);
}

RLBIND bool IsGamepadButtonPressed_(int a, int b)
{
    return IsGamepadButtonPressed(a, b);
}

RLBIND bool IsGamepadButtonDown_(int a, int b)
{
    return IsGamepadButtonDown(a, b);
}

RLBIND bool IsGamepadButtonReleased_(int a, int b)
{
    return IsGamepadButtonReleased(a, b);
}

RLBIND bool IsGamepadButtonUp_(int a, int b)
{
    return IsGamepadButtonUp(a, b);
}

RLBIND int GetGamepadButtonPressed_()
{
    return GetGamepadButtonPressed();
}

RLBIND int GetGamepadAxisCount_(int a)
{
    return GetGamepadAxisCount(a);
}

RLBIND float GetGamepadAxisMovement_(int a, int b)
{
    return GetGamepadAxisMovement(a, b);
}

RLBIND int SetGamepadMappings_(const char *a)
{
    return SetGamepadMappings(a);
}

RLBIND void SetGamepadVibration_(int a, float b, float c, float d)
{
    SetGamepadVibration(a, b, c, d);
}

RLBIND bool IsMouseButtonPressed_(int a)
{
    return IsMouseButtonPressed(a);
}

RLBIND bool IsMouseButtonDown_(int a)
{
    return IsMouseButtonDown(a);
}

RLBIND bool IsMouseButtonReleased_(int a)
{
    return IsMouseButtonReleased(a);
}

RLBIND bool IsMouseButtonUp_(int a)
{
    return IsMouseButtonUp(a);
}

RLBIND int GetMouseX_()
{
    return GetMouseX();
}

RLBIND int GetMouseY_()
{
    return GetMouseY();
}

RLBIND void SetMousePosition_(int a, int b)
{
    SetMousePosition(a, b);
}

RLBIND void SetMouseOffset_(int a, int b)
{
    SetMouseOffset(a, b);
}

RLBIND void SetMouseScale_(float a, float b)
{
    SetMouseScale(a, b);
}

RLBIND float GetMouseWheelMove_()
{
    return GetMouseWheelMove();
}

RLBIND void SetMouseCursor_(int a)
{
    SetMouseCursor(a);
}

RLBIND int GetTouchX_()
{
    return GetTouchX();
}

RLBIND int GetTouchY_()
{
    return GetTouchY();
}

RLBIND int GetTouchPointId_(int a)
{
    return GetTouchPointId(a);
}

RLBIND int GetTouchPointCount_()
{
    return GetTouchPointCount();
}

RLBIND void SetGesturesEnabled_(unsigned int a)
{
    SetGesturesEnabled(a);
}

RLBIND bool IsGestureDetected_(unsigned int a)
{
    return IsGestureDetected(a);
}

RLBIND int GetGestureDetected_()
{
    return GetGestureDetected();
}

RLBIND float GetGestureHoldDuration_()
{
    return GetGestureHoldDuration();
}

RLBIND float GetGestureDragAngle_()
{
    return GetGestureDragAngle();
}

RLBIND float GetGesturePinchAngle_()
{
    return GetGesturePinchAngle();
}

RLBIND void UpdateCamera_(Camera *a, int b)
{
    UpdateCamera(a, b);
}

RLBIND void ImageFormat_(Image *a, int b)
{
    ImageFormat(a, b);
}

RLBIND void ImageAlphaCrop_(Image *a, float b)
{
    ImageAlphaCrop(a, b);
}

RLBIND void ImageAlphaPremultiply_(Image *a)
{
    ImageAlphaPremultiply(a);
}

RLBIND void ImageBlurGaussian_(Image *a, int b)
{
    ImageBlurGaussian(a, b);
}

RLBIND void ImageKernelConvolution_(Image *a, const float *b, int c)
{
    ImageKernelConvolution(a, b, c);
}

RLBIND void ImageResize_(Image *a, int b, int c)
{
    ImageResize(a, b, c);
}

RLBIND void ImageResizeNN_(Image *a, int b, int c)
{
    ImageResizeNN(a, b, c);
}

RLBIND void ImageMipmaps_(Image *a)
{
    ImageMipmaps(a);
}

RLBIND void ImageDither_(Image *a, int b, int c, int d, int e)
{
    ImageDither(a, b, c, d, e);
}

RLBIND void ImageFlipVertical_(Image *a)
{
    ImageFlipVertical(a);
}

RLBIND void ImageFlipHorizontal_(Image *a)
{
    ImageFlipHorizontal(a);
}

RLBIND void ImageRotate_(Image *a, int b)
{
    ImageRotate(a, b);
}

RLBIND void ImageRotateCW_(Image *a)
{
    ImageRotateCW(a);
}

RLBIND void ImageRotateCCW_(Image *a)
{
    ImageRotateCCW(a);
}

RLBIND void ImageColorInvert_(Image *a)
{
    ImageColorInvert(a);
}

RLBIND void ImageColorGrayscale_(Image *a)
{
    ImageColorGrayscale(a);
}

RLBIND void ImageColorContrast_(Image *a, float b)
{
    ImageColorContrast(a, b);
}

RLBIND void ImageColorBrightness_(Image *a, int b)
{
    ImageColorBrightness(a, b);
}

RLBIND void GenTextureMipmaps_(Texture2D *a)
{
    GenTextureMipmaps(a);
}

RLBIND GlyphInfo *LoadFontData_(const unsigned char *a, int b, int c, int *d, int e, int f)
{
    return LoadFontData(a, b, c, d, e, f);
}

RLBIND void UnloadFontData_(GlyphInfo *a, int b)
{
    UnloadFontData(a, b);
}

RLBIND void DrawFPS_(int a, int b)
{
    DrawFPS(a, b);
}

RLBIND void SetTextLineSpacing_(int a)
{
    SetTextLineSpacing(a);
}

RLBIND int MeasureText_(const char *a, int b)
{
    return MeasureText(a, b);
}

RLBIND char *LoadUTF8_(const int *a, int b)
{
    return LoadUTF8(a, b);
}

RLBIND int *LoadCodepoints_(const char *a, int *b)
{
    return LoadCodepoints(a, b);
}

RLBIND int GetCodepointCount_(const char *a)
{
    return GetCodepointCount(a);
}

RLBIND int GetCodepointNext_(const char *a, int *b)
{
    return GetCodepointNext(a, b);
}

RLBIND int GetCodepointPrevious_(const char *a, int *b)
{
    return GetCodepointPrevious(a, b);
}

RLBIND const char *CodepointToUTF8_(int a, int *b)
{
    return CodepointToUTF8(a, b);
}

RLBIND void DrawGrid_(int a, float b)
{
    DrawGrid(a, b);
}

RLBIND void UploadMesh_(Mesh *a, bool b)
{
    UploadMesh(a, b);
}

RLBIND void GenMeshTangents_(Mesh *a)
{
    GenMeshTangents(a);
}

RLBIND Material *LoadMaterials_(const char *a, int *b)
{
    return LoadMaterials(a, b);
}

RLBIND void SetModelMeshMaterial_(Model *a, int b, int c)
{
    SetModelMeshMaterial(a, b, c);
}

RLBIND ModelAnimation *LoadModelAnimations_(const char *a, int *b)
{
    return LoadModelAnimations(a, b);
}

RLBIND void UnloadModelAnimations_(ModelAnimation *a, int b)
{
    UnloadModelAnimations(a, b);
}

RLBIND void InitAudioDevice_()
{
    InitAudioDevice();
}

RLBIND void CloseAudioDevice_()
{
    CloseAudioDevice();
}

RLBIND bool IsAudioDeviceReady_()
{
    return IsAudioDeviceReady();
}

RLBIND void SetMasterVolume_(float a)
{
    SetMasterVolume(a);
}

RLBIND float GetMasterVolume_()
{
    return GetMasterVolume();
}

RLBIND void WaveCrop_(Wave *a, int b, int c)
{
    WaveCrop(a, b, c);
}

RLBIND void WaveFormat_(Wave *a, int b, int c, int d)
{
    WaveFormat(a, b, c, d);
}

RLBIND void UnloadWaveSamples_(float *a)
{
    UnloadWaveSamples(a);
}

RLBIND void SetAudioStreamBufferSizeDefault_(int a)
{
    SetAudioStreamBufferSizeDefault(a);
}
