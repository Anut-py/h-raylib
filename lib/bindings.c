/**
 * See bindings.h
 */

#include "bindings.h"
#include <stdlib.h>

void SetWindowIcon_(Image* a) {
    SetWindowIcon(*a);
}

Vector2* GetMonitorPosition_(int a) {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetMonitorPosition(a);
    return ptr;
}

Vector2* GetWindowPosition_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetWindowPosition();
    return ptr;
}

Vector2* GetWindowScaleDPI_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetWindowScaleDPI();
    return ptr;
}

void ClearBackground_(Color* a) {
    ClearBackground(*a);
}

void BeginMode2D_(Camera2D* a) {
    BeginMode2D(*a);
}

void BeginMode3D_(Camera3D* a) {
    BeginMode3D(*a);
}

void BeginTextureMode_(RenderTexture* a) {
    BeginTextureMode(*a);
}

void BeginShaderMode_(Shader* a) {
    BeginShaderMode(*a);
}

void BeginVrStereoMode_(VrStereoConfig* a) {
    BeginVrStereoMode(*a);
}

VrStereoConfig* LoadVrStereoConfig_(VrDeviceInfo* a) {
    VrStereoConfig* ptr = (VrStereoConfig*) malloc(sizeof (VrStereoConfig));
    *ptr = LoadVrStereoConfig(*a);
    return ptr;
}

void UnloadVrStereoConfig_(VrStereoConfig* a) {
    UnloadVrStereoConfig(*a);
}

Shader* LoadShader_(string a, string b) {
    Shader* ptr = (Shader*) malloc(sizeof (Shader));
    *ptr = LoadShader(a, b);
    return ptr;
}

Shader* LoadShaderFromMemory_(string a, string b) {
    Shader* ptr = (Shader*) malloc(sizeof (Shader));
    *ptr = LoadShaderFromMemory(a, b);
    return ptr;
}

int GetShaderLocation_(Shader* a, string b) {
    return GetShaderLocation(*a, b);
}

int GetShaderLocationAttrib_(Shader* a, string b) {
    return GetShaderLocationAttrib(*a, b);
}

void SetShaderValue_(Shader* a, int b, const void* c, int d) {
    SetShaderValue(*a, b, c, d);
}

void SetShaderValueV_(Shader* a, int b, const void* c, int d, int e) {
    SetShaderValueV(*a, b, c, d, e);
}

void SetShaderValueMatrix_(Shader* a, int b, Matrix* c) {
    SetShaderValueMatrix(*a, b, *c);
}

void SetShaderValueTexture_(Shader* a, int b, Texture* c) {
    SetShaderValueTexture(*a, b, *c);
}

void UnloadShader_(Shader* a) {
    UnloadShader(*a);
}

Ray* GetMouseRay_(Vector2* a, Camera3D* b) {
    Ray* ptr = (Ray*) malloc(sizeof (Ray));
    *ptr = GetMouseRay(*a, *b);
    return ptr;
}

Matrix* GetCameraMatrix_(Camera3D* a) {
    Matrix* ptr = (Matrix*) malloc(sizeof (Matrix));
    *ptr = GetCameraMatrix(*a);
    return ptr;
}

Matrix* GetCameraMatrix2D_(Camera2D* a) {
    GetCameraMatrix2D(*a);
}

Vector2* GetWorldToScreen_(Vector3* a, Camera3D* b) {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetWorldToScreen(*a, *b);
    return ptr;
}

Vector2* GetScreenToWorld2D_(Vector2* a, Camera2D* b) {
    GetScreenToWorld2D(*a, *b);
}

Vector2* GetWorldToScreenEx_(Vector3* a, Camera3D* b, int c, int d) {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetWorldToScreenEx(*a, *b, c, d);
    return ptr;
}

Vector2* GetWorldToScreen2D_(Vector2* a, Camera2D* b) {
    GetWorldToScreen2D(*a, *b);
}

FilePathList* LoadDirectoryFiles_(string a) {
    FilePathList* ptr = (FilePathList*) malloc(sizeof (FilePathList));
    *ptr = LoadDirectoryFiles(a);
    return ptr;
}

FilePathList* LoadDirectoryFilesEx_(string a, string b, int c) {
    FilePathList* ptr = (FilePathList*) malloc(sizeof (FilePathList));
    *ptr = LoadDirectoryFilesEx(a, b, c);
    return ptr;
}

void UnloadDirectoryFiles_(FilePathList* a) {
    UnloadDirectoryFiles(*a);
}

FilePathList* LoadDroppedFiles_() {
    FilePathList* ptr = (FilePathList*) malloc(sizeof (FilePathList));
    *ptr = LoadDroppedFiles();
    return ptr;
}

void UnloadDroppedFiles_(FilePathList* a) {
    UnloadDroppedFiles(*a);
}

Vector2* GetMousePosition_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetMousePosition();
    return ptr;
}

Vector2* GetMouseDelta_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetMouseDelta();
    return ptr;
}

Vector2* GetMouseWheelMoveV_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetMouseWheelMoveV();
    return ptr;
}

Vector2* GetTouchPosition_(int a) {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetTouchPosition(a);
    return ptr;
}

Vector2* GetGestureDragVector_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetGestureDragVector();
    return ptr;
}

Vector2* GetGesturePinchVector_() {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = GetGesturePinchVector();
    return ptr;
}

void SetCameraMode_(Camera3D* a, int b) {
    SetCameraMode(*a, b);
}

void SetShapesTexture_(Texture* a, Rectangle* b) {
    SetShapesTexture(*a, *b);
}

void DrawPixel_(int a, int b, Color* c) {
    DrawPixel(a, b, *c);
}

void DrawPixelV_(Vector2* a, Color* b) {
    DrawPixelV(*a, *b);
}

void DrawLine_(int a, int b, int c, int d, Color* e) {
    DrawLine(a, b, c, d, *e);
}

void DrawLineV_(Vector2* a, Vector2* b, Color* c) {
    DrawLineV(*a, *b, *c);
}

void DrawLineEx_(Vector2* a, Vector2* b, float c, Color* d) {
    DrawLineEx(*a, *b, c, *d);
}

void DrawLineBezier_(Vector2* a, Vector2* b, float c, Color* d) {
    DrawLineBezier(*a, *b, c, *d);
}

void DrawLineBezierQuad_(Vector2* a, Vector2* b, Vector2* c, float d, Color* e) {
    DrawLineBezierQuad(*a, *b, *c, d, *e);
}

void DrawLineBezierCubic_(Vector2* a, Vector2* b, Vector2* c, Vector2* d, float e, Color* f) {
    DrawLineBezierCubic(*a, *b, *c, *d, e, *f);
}

void DrawLineStrip_(Vector2* a, int b, Color* c) {
    DrawLineStrip(a, b, *c);
}

void DrawCircle_(int a, int b, float c, Color* d) {
    DrawCircle(a, b, c, *d);
}

void DrawCircleSector_(Vector2* a, float b, float c, float d, int e, Color* f) {
    DrawCircleSector(*a, b, c, d, e, *f);
}

void DrawCircleSectorLines_(Vector2* a, float b, float c, float d, int e, Color* f) {
    DrawCircleSectorLines(*a, b, c, d, e, *f);
}

void DrawCircleGradient_(int a, int b, float c, Color* d, Color* e) {
    DrawCircleGradient(a, b, c, *d, *e);
}

void DrawCircleV_(Vector2* a, float b, Color* c) {
    DrawCircleV(*a, b, *c);
}

void DrawCircleLines_(int a, int b, float c, Color* d) {
    DrawCircleLines(a, b, c, *d);
}

void DrawEllipse_(int a, int b, float c, float d, Color* e) {
    DrawEllipse(a, b, c, d, *e);
}

void DrawEllipseLines_(int a, int b, float c, float d, Color* e) {
    DrawEllipseLines(a, b, c, d, *e);
}

void DrawRing_(Vector2* a, float b, float c, float d, float e, int f, Color* g) {
    DrawRing(*a, b, c, d, e, f, *g);
}

void DrawRingLines_(Vector2* a, float b, float c, float d, float e, int f, Color* g) {
    DrawRingLines(*a, b, c, d, e, f, *g);
}

void DrawRectangle_(int a, int b, int c, int d, Color* e) {
    DrawRectangle(a, b, c, d, *e);
}

void DrawRectangleV_(Vector2* a, Vector2* b, Color* c) {
    DrawRectangleV(*a, *b, *c);
}

void DrawRectangleRec_(Rectangle* a, Color* b) {
    DrawRectangleRec(*a, *b);
}

void DrawRectanglePro_(Rectangle* a, Vector2* b, float c, Color* d) {
    DrawRectanglePro(*a, *b, c, *d);
}

void DrawRectangleGradientV_(int a, int b, int c, int d, Color* e, Color* f) {
    DrawRectangleGradientV(a, b, c, d, *e, *f);
}

void DrawRectangleGradientH_(int a, int b, int c, int d, Color* e, Color* f) {
    DrawRectangleGradientH(a, b, c, d, *e, *f);
}

void DrawRectangleGradientEx_(Rectangle* a, Color* b, Color* c, Color* d, Color* e) {
    DrawRectangleGradientEx(*a, *b, *c, *d, *e);
}

void DrawRectangleLines_(int a, int b, int c, int d, Color* e) {
    DrawRectangleLines(a, b, c, d, *e);
}

void DrawRectangleLinesEx_(Rectangle* a, float b, Color* c) {
    DrawRectangleLinesEx(*a, b, *c);
}

void DrawRectangleRounded_(Rectangle* a, float b, int c, Color* d) {
    DrawRectangleRounded(*a, b, c, *d);
}

void DrawRectangleRoundedLines_(Rectangle* a, float b, int c, float d, Color* e) {
    DrawRectangleRoundedLines(*a, b, c, d, *e);
}

void DrawTriangle_(Vector2* a, Vector2* b, Vector2* c, Color* d) {
    DrawTriangle(*a, *b, *c, *d);
}

void DrawTriangleLines_(Vector2* a, Vector2* b, Vector2* c, Color* d) {
    DrawTriangleLines(*a, *b, *c, *d);
}

void DrawTriangleFan_(Vector2* a, int b, Color* c) {
    DrawTriangleFan(a, b, *c);
}

void DrawTriangleStrip_(Vector2* a, int b, Color* c) {
    DrawTriangleStrip(a, b, *c);
}

void DrawPoly_(Vector2* a, int b, float c, float d, Color* e) {
    DrawPoly(*a, b, c, d, *e);
}

void DrawPolyLines_(Vector2* a, int b, float c, float d, Color* e) {
    DrawPolyLines(*a, b, c, d, *e);
}

void DrawPolyLinesEx_(Vector2* a, int b, float c, float d, float e, Color* f) {
    DrawPolyLinesEx(*a, b, c, d, e, *f);
}

int CheckCollisionRecs_(Rectangle* a, Rectangle* b) {
    return CheckCollisionRecs(*a, *b);
}

int CheckCollisionCircles_(Vector2* a, float b, Vector2* c, float d) {
    return CheckCollisionCircles(*a, b, *c, d);
}

int CheckCollisionCircleRec_(Vector2* a, float b, Rectangle* c) {
    return CheckCollisionCircleRec(*a, b, *c);
}

int CheckCollisionPointRec_(Vector2* a, Rectangle* b) {
    return CheckCollisionPointRec(*a, *b);
}

int CheckCollisionPointCircle_(Vector2* a, Vector2* b, float c) {
    return CheckCollisionPointCircle(*a, *b, c);
}

int CheckCollisionPointTriangle_(Vector2* a, Vector2* b, Vector2* c, Vector2* d) {
    return CheckCollisionPointTriangle(*a, *b, *c, *d);
}

int CheckCollisionLines_(Vector2* a, Vector2* b, Vector2* c, Vector2* d, Vector2* e) {
    return CheckCollisionLines(*a, *b, *c, *d, e);
}

int CheckCollisionPointLine_(Vector2* a, Vector2* b, Vector2* c, int d) {
    return CheckCollisionPointLine(*a, *b, *c, d);
}

Rectangle* GetCollisionRec_(Rectangle* a, Rectangle* b) {
    Rectangle* ptr = (Rectangle*) malloc(sizeof (Rectangle));
    *ptr = GetCollisionRec(*a, *b);
    return ptr;
}

Image* LoadImage_(string a) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = LoadImage(a);
    return ptr;
}

Image* LoadImageRaw_(string a, int b, int c, int d, int e) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = LoadImageRaw(a, b, c, d, e);
    return ptr;
}

Image* LoadImageAnim_(string a, int* b) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = LoadImageAnim(a, b);
    return ptr;
}

Image* LoadImageFromMemory_(string a, unsigned char* b, int c) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = LoadImageFromMemory(a, b, c);
    return ptr;
}

Image* LoadImageFromTexture_(Texture* a) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = LoadImageFromTexture(*a);
    return ptr;
}

Image* LoadImageFromScreen_() {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = LoadImageFromScreen();
    return ptr;
}

void UnloadImage_(Image* a) {
    UnloadImage(*a);
}

int ExportImage_(Image* a, string b) {
    return ExportImage(*a, b);
}

int ExportImageAsCode_(Image* a, string b) {
    return ExportImageAsCode(*a, b);
}

Image* GenImageColor_(int a, int b, Color* c) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageColor(a, b, *c);
    return ptr;
}

Image* GenImageGradientV_(int a, int b, Color* c, Color* d) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageGradientV(a, b, *c, *d);
    return ptr;
}

Image* GenImageGradientH_(int a, int b, Color* c, Color* d) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageGradientH(a, b, *c, *d);
    return ptr;
}

Image* GenImageGradientRadial_(int a, int b, float c, Color* d, Color* e) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageGradientRadial(a, b, c, *d, *e);
    return ptr;
}

Image* GenImageChecked_(int a, int b, int c, int d, Color* e, Color* f) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageChecked(a, b, c, d, *e, *f);
    return ptr;
}

Image* GenImageWhiteNoise_(int a, int b, float c) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageWhiteNoise(a, b, c);
    return ptr;
}

Image* GenImageCellular_(int a, int b, int c) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageCellular(a, b, c);
    return ptr;
}

Image* ImageCopy_(Image* a) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = ImageCopy(*a);
    return ptr;
}

Image* ImageFromImage_(Image* a, Rectangle* b) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = ImageFromImage(*a, *b);
    return ptr;
}

Image* ImageText_(string a, int b, Color* c) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = ImageText(a, b, *c);
    return ptr;
}

Image* ImageTextEx_(Font* a, string b, float c, float d, Color* e) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = ImageTextEx(*a, b, c, d, *e);
    return ptr;
}

void ImageToPOT_(Image* a, Color* b) {
    ImageToPOT(a, *b);
}

void ImageCrop_(Image* a, Rectangle* b) {
    ImageCrop(a, *b);
}

void ImageAlphaClear_(Image* a, Color* b, float c) {
    ImageAlphaClear(a, *b, c);
}

void ImageAlphaMask_(Image* a, Image* b) {
    ImageAlphaMask(a, *b);
}

void ImageResizeCanvas_(Image* a, int b, int c, int d, int e, Color* f) {
    ImageResizeCanvas(a, b, c, d, e, *f);
}

void ImageColorTint_(Image* a, Color* b) {
    ImageColorTint(a, *b);
}

void ImageColorReplace_(Image* a, Color* b, Color* c) {
    ImageColorReplace(a, *b, *c);
}

Color* LoadImageColors_(Image* a) {
    LoadImageColors(*a);
}

Color* LoadImagePalette_(Image* a, int b, int* c) {
    LoadImagePalette(*a, b, c);
}

Rectangle* GetImageAlphaBorder_(Image* a, float b) {
    Rectangle* ptr = (Rectangle*) malloc(sizeof (Rectangle));
    *ptr = GetImageAlphaBorder(*a, b);
    return ptr;
}

Color* GetImageColor_(Image* a, int b, int c) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = GetImageColor(*a, b, c);
    return ptr;
}

void ImageClearBackground_(Image* a, Color* b) {
    ImageClearBackground(a, *b);
}

void ImageDrawPixel_(Image* a, int b, int c, Color* d) {
    ImageDrawPixel(a, b, c, *d);
}

void ImageDrawPixelV_(Image* a, Vector2* b, Color* c) {
    ImageDrawPixelV(a, *b, *c);
}

void ImageDrawLine_(Image* a, int b, int c, int d, int e, Color* f) {
    ImageDrawLine(a, b, c, d, e, *f);
}

void ImageDrawLineV_(Image* a, Vector2* b, Vector2* c, Color* d) {
    ImageDrawLineV(a, *b, *c, *d);
}

void ImageDrawCircle_(Image* a, int b, int c, int d, Color* e) {
    ImageDrawCircle(a, b, c, d, *e);
}

void ImageDrawCircleV_(Image* a, Vector2* b, int c, Color* d) {
    ImageDrawCircleV(a, *b, c, *d);
}

void ImageDrawRectangle_(Image* a, int b, int c, int d, int e, Color* f) {
    ImageDrawRectangle(a, b, c, d, e, *f);
}

void ImageDrawRectangleV_(Image* a, Vector2* b, Vector2* c, Color* d) {
    ImageDrawRectangleV(a, *b, *c, *d);
}

void ImageDrawRectangleRec_(Image* a, Rectangle* b, Color* c) {
    ImageDrawRectangleRec(a, *b, *c);
}

void ImageDrawRectangleLines_(Image* a, Rectangle* b, int c, Color* d) {
    ImageDrawRectangleLines(a, *b, c, *d);
}

void ImageDraw_(Image* a, Image* b, Rectangle* c, Rectangle* d, Color* e) {
    ImageDraw(a, *b, *c, *d, *e);
}

void ImageDrawText_(Image* a, string b, int c, int d, int e, Color* f) {
    ImageDrawText(a, b, c, d, e, *f);
}

void ImageDrawTextEx_(Image* a, Font* b, string c, Vector2* d, float e, float f, Color* g) {
    ImageDrawTextEx(a, *b, c, *d, e, f, *g);
}

Texture* LoadTexture_(string a) {
    Texture* ptr = (Texture*) malloc(sizeof (Texture));
    *ptr = LoadTexture(a);
    return ptr;
}

Texture* LoadTextureFromImage_(Image* a) {
    Texture* ptr = (Texture*) malloc(sizeof (Texture));
    *ptr = LoadTextureFromImage(*a);
    return ptr;
}

Texture* LoadTextureCubemap_(Image* a, int b) {
    Texture* ptr = (Texture*) malloc(sizeof (Texture));
    *ptr = LoadTextureCubemap(*a, b);
    return ptr;
}

RenderTexture* LoadRenderTexture_(int a, int b) {
    RenderTexture* ptr = (RenderTexture*) malloc(sizeof (RenderTexture));
    *ptr = LoadRenderTexture(a, b);
    return ptr;
}

void UnloadTexture_(Texture* a) {
    UnloadTexture(*a);
}

void UnloadRenderTexture_(RenderTexture* a) {
    UnloadRenderTexture(*a);
}

void UpdateTexture_(Texture* a, const void* b) {
    UpdateTexture(*a, b);
}

void UpdateTextureRec_(Texture* a, Rectangle* b, const void* c) {
    UpdateTextureRec(*a, *b, c);
}

void SetTextureFilter_(Texture* a, int b) {
    SetTextureFilter(*a, b);
}

void SetTextureWrap_(Texture* a, int b) {
    SetTextureWrap(*a, b);
}

void DrawTexture_(Texture* a, int b, int c, Color* d) {
    DrawTexture(*a, b, c, *d);
}

void DrawTextureV_(Texture* a, Vector2* b, Color* c) {
    DrawTextureV(*a, *b, *c);
}

void DrawTextureEx_(Texture* a, Vector2* b, float c, float d, Color* e) {
    DrawTextureEx(*a, *b, c, d, *e);
}

void DrawTextureRec_(Texture* a, Rectangle* b, Vector2* c, Color* d) {
    DrawTextureRec(*a, *b, *c, *d);
}

void DrawTextureQuad_(Texture* a, Vector2* b, Vector2* c, Rectangle* d, Color* e) {
    DrawTextureQuad(*a, *b, *c, *d, *e);
}

void DrawTextureTiled_(Texture* a, Rectangle* b, Rectangle* c, Vector2* d, float e, float f, Color* g) {
    DrawTextureTiled(*a, *b, *c, *d, e, f, *g);
}

void DrawTexturePro_(Texture* a, Rectangle* b, Rectangle* c, Vector2* d, float e, Color* f) {
    DrawTexturePro(*a, *b, *c, *d, e, *f);
}

void DrawTextureNPatch_(Texture* a, NPatchInfo* b, Rectangle* c, Vector2* d, float e, Color* f) {
    DrawTextureNPatch(*a, *b, *c, *d, e, *f);
}

void DrawTexturePoly_(Texture* a, Vector2* b, Vector2* c, Vector2* d, int e, Color* f) {
    DrawTexturePoly(*a, *b, c, d, e, *f);
}

Color* Fade_(Color* a, float b) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = Fade(*a, b);
    return ptr;
}

int ColorToInt_(Color* a) {
    return ColorToInt(*a);
}

Vector4* ColorNormalize_(Color* a) {
    Vector4* ptr = (Vector4*) malloc(sizeof (Vector4));
    *ptr = ColorNormalize(*a);
    return ptr;
}

Color* ColorFromNormalized_(Vector4* a) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = ColorFromNormalized(*a);
    return ptr;
}

Vector3* ColorToHSV_(Color* a) {
    Vector3* ptr = (Vector3*) malloc(sizeof (Vector3));
    *ptr = ColorToHSV(*a);
    return ptr;
}

Color* ColorFromHSV_(float a, float b, float c) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = ColorFromHSV(a, b, c);
    return ptr;
}

Color* ColorAlpha_(Color* a, float b) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = ColorAlpha(*a, b);
    return ptr;
}

Color* ColorAlphaBlend_(Color* a, Color* b, Color* c) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = ColorAlphaBlend(*a, *b, *c);
    return ptr;
}

Color* GetColor_(unsigned int a) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = GetColor(a);
    return ptr;
}

Color* GetPixelColor_(void* a, int b) {
    Color* ptr = (Color*) malloc(sizeof (Color));
    *ptr = GetPixelColor(a, b);
    return ptr;
}

void SetPixelColor_(void* a, Color* b, int c) {
    SetPixelColor(a, *b, c);
}

Font* GetFontDefault_() {
    Font* ptr = (Font*) malloc(sizeof (Font));
    *ptr = GetFontDefault();
    return ptr;
}

Font* LoadFont_(string a) {
    Font* ptr = (Font*) malloc(sizeof (Font));
    *ptr = LoadFont(a);
    return ptr;
}

Font* LoadFontEx_(string a, int b, int* c, int d) {
    Font* ptr = (Font*) malloc(sizeof (Font));
    *ptr = LoadFontEx(a, b, c, d);
    return ptr;
}

Font* LoadFontFromImage_(Image* a, Color* b, int c) {
    Font* ptr = (Font*) malloc(sizeof (Font));
    *ptr = LoadFontFromImage(*a, *b, c);
    return ptr;
}

Font* LoadFontFromMemory_(string a, unsigned char* b, int c, int d, int* e, int f) {
    Font* ptr = (Font*) malloc(sizeof (Font));
    *ptr = LoadFontFromMemory(a, b, c, d, e, f);
    return ptr;
}

Image* GenImageFontAtlas_(GlyphInfo* a, Rectangle** b, int c, int d, int e, int f) {
    Image* ptr = (Image*) malloc(sizeof (Image));
    *ptr = GenImageFontAtlas(a, b, c, d, e, f);
    return ptr;
}

void UnloadFont_(Font* a) {
    UnloadFont(*a);
}

int ExportFontAsCode_(Font* a, string b) {
    return ExportFontAsCode(*a, b);
}

void DrawText_(string a, int b, int c, int d, Color* e) {
    DrawText(a, b, c, d, *e);
}

void DrawTextEx_(Font* a, string b, Vector2* c, float d, float e, Color* f) {
    DrawTextEx(*a, b, *c, d, e, *f);
}

void DrawTextPro_(Font* a, string b, Vector2* c, Vector2* d, float e, float f, float g, Color* h) {
    DrawTextPro(*a, b, *c, *d, e, f, g, *h);
}

void DrawTextCodepoint_(Font* a, int b, Vector2* c, float d, Color* e) {
    DrawTextCodepoint(*a, b, *c, d, *e);
}

void DrawTextCodepoints_(Font* a, int* b, int c, Vector2* d, float e, float f, Color* g) {
    DrawTextCodepoints(*a, b, c, *d, e, f, *g);
}

Vector2* MeasureTextEx_(Font* a, string b, float c, float d) {
    Vector2* ptr = (Vector2*) malloc(sizeof (Vector2));
    *ptr = MeasureTextEx(*a, b, c, d);
    return ptr;
}

int GetGlyphIndex_(Font* a, int b) {
    return GetGlyphIndex(*a, b);
}

GlyphInfo* GetGlyphInfo_(Font* a, int b) {
    GlyphInfo* ptr = (GlyphInfo*) malloc(sizeof (GlyphInfo));
    *ptr = GetGlyphInfo(*a, b);
    return ptr;
}

Rectangle* GetGlyphAtlasRec_(Font* a, int b) {
    Rectangle* ptr = (Rectangle*) malloc(sizeof (Rectangle));
    *ptr = GetGlyphAtlasRec(*a, b);
    return ptr;
}

void DrawLine3D_(Vector3* a, Vector3* b, Color* c) {
    DrawLine3D(*a, *b, *c);
}

void DrawPoint3D_(Vector3* a, Color* b) {
    DrawPoint3D(*a, *b);
}

void DrawCircle3D_(Vector3* a, float b, Vector3* c, float d, Color* e) {
    DrawCircle3D(*a, b, *c, d, *e);
}

void DrawTriangle3D_(Vector3* a, Vector3* b, Vector3* c, Color* d) {
    DrawTriangle3D(*a, *b, *c, *d);
}

void DrawTriangleStrip3D_(Vector3* a, int b, Color* c) {
    DrawTriangleStrip3D(a, b, *c);
}

void DrawCube_(Vector3* a, float b, float c, float d, Color* e) {
    DrawCube(*a, b, c, d, *e);
}

void DrawCubeV_(Vector3* a, Vector3* b, Color* c) {
    DrawCubeV(*a, *b, *c);
}

void DrawCubeWires_(Vector3* a, float b, float c, float d, Color* e) {
    DrawCubeWires(*a, b, c, d, *e);
}

void DrawCubeWiresV_(Vector3* a, Vector3* b, Color* c) {
    DrawCubeWiresV(*a, *b, *c);
}

void DrawCubeTexture_(Texture* a, Vector3* b, float c, float d, float e, Color* f) {
    DrawCubeTexture(*a, *b, c, d, e, *f);
}

void DrawCubeTextureRec_(Texture* a, Rectangle* b, Vector3* c, float d, float e, float f, Color* g) {
    DrawCubeTextureRec(*a, *b, *c, d, e, f, *g);
}

void DrawSphere_(Vector3* a, float b, Color* c) {
    DrawSphere(*a, b, *c);
}

void DrawSphereEx_(Vector3* a, float b, int c, int d, Color* e) {
    DrawSphereEx(*a, b, c, d, *e);
}

void DrawSphereWires_(Vector3* a, float b, int c, int d, Color* e) {
    DrawSphereWires(*a, b, c, d, *e);
}

void DrawCylinder_(Vector3* a, float b, float c, float d, int e, Color* f) {
    DrawCylinder(*a, b, c, d, e, *f);
}

void DrawCylinderEx_(Vector3* a, Vector3* b, float c, float d, int e, Color* f) {
    DrawCylinderEx(*a, *b, c, d, e, *f);
}

void DrawCylinderWires_(Vector3* a, float b, float c, float d, int e, Color* f) {
    DrawCylinderWires(*a, b, c, d, e, *f);
}

void DrawCylinderWiresEx_(Vector3* a, Vector3* b, float c, float d, int e, Color* f) {
    DrawCylinderWiresEx(*a, *b, c, d, e, *f);
}

void DrawPlane_(Vector3* a, Vector2* b, Color* c) {
    DrawPlane(*a, *b, *c);
}

void DrawRay_(Ray* a, Color* b) {
    DrawRay(*a, *b);
}

Model* LoadModel_(string a) {
    Model* ptr = (Model*) malloc(sizeof (Model));
    *ptr = LoadModel(a);
    return ptr;
}

Model* LoadModelFromMesh_(Mesh* a) {
    Model* ptr = (Model*) malloc(sizeof (Model));
    *ptr = LoadModelFromMesh(*a);
    return ptr;
}

void UnloadModel_(Model* a) {
    UnloadModel(*a);
}

void UnloadModelKeepMeshes_(Model* a) {
    UnloadModelKeepMeshes(*a);
}

BoundingBox* GetModelBoundingBox_(Model* a) {
    BoundingBox* ptr = (BoundingBox*) malloc(sizeof (BoundingBox));
    *ptr = GetModelBoundingBox(*a);
    return ptr;
}

void DrawModel_(Model* a, Vector3* b, float c, Color* d) {
    DrawModel(*a, *b, c, *d);
}

void DrawModelEx_(Model* a, Vector3* b, Vector3* c, float d, Vector3* e, Color* f) {
    DrawModelEx(*a, *b, *c, d, *e, *f);
}

void DrawModelWires_(Model* a, Vector3* b, float c, Color* d) {
    DrawModelWires(*a, *b, c, *d);
}

void DrawModelWiresEx_(Model* a, Vector3* b, Vector3* c, float d, Vector3* e, Color* f) {
    DrawModelWiresEx(*a, *b, *c, d, *e, *f);
}

void DrawBoundingBox_(BoundingBox* a, Color* b) {
    DrawBoundingBox(*a, *b);
}

void DrawBillboard_(Camera3D* a, Texture* b, Vector3* c, float d, Color* e) {
    DrawBillboard(*a, *b, *c, d, *e);
}

void DrawBillboardRec_(Camera3D* a, Texture* b, Rectangle* c, Vector3* d, Vector2* e, Color* f) {
    DrawBillboardRec(*a, *b, *c, *d, *e, *f);
}

void DrawBillboardPro_(Camera3D* a, Texture* b, Rectangle* c, Vector3* d, Vector3* e, Vector2* f, Vector2* g, float h, Color* i) {
    DrawBillboardPro(*a, *b, *c, *d, *e, *f, *g, h, *i);
}

void UpdateMeshBuffer_(Mesh* a, int b, const void* c, int d, int e) {
    UpdateMeshBuffer(*a, b, c, d, e);
}

void UnloadMesh_(Mesh* a) {
    UnloadMesh(*a);
}

void DrawMesh_(Mesh* a, Material* b, Matrix* c) {
    DrawMesh(*a, *b, *c);
}

void DrawMeshInstanced_(Mesh* a, Material* b, Matrix* c, int d) {
    DrawMeshInstanced(*a, *b, c, d);
}

int ExportMesh_(Mesh* a, string b) {
    return ExportMesh(*a, b);
}

BoundingBox* GetMeshBoundingBox_(Mesh* a) {
    BoundingBox* ptr = (BoundingBox*) malloc(sizeof (BoundingBox));
    *ptr = GetMeshBoundingBox(*a);
    return ptr;
}

Mesh* GenMeshPoly_(int a, float b) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshPoly(a, b);
    return ptr;
}

Mesh* GenMeshPlane_(float a, float b, int c, int d) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshPlane(a, b, c, d);
    return ptr;
}

Mesh* GenMeshCube_(float a, float b, float c) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshCube(a, b, c);
    return ptr;
}

Mesh* GenMeshSphere_(float a, int b, int c) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshSphere(a, b, c);
    return ptr;
}

Mesh* GenMeshHemiSphere_(float a, int b, int c) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshHemiSphere(a, b, c);
    return ptr;
}

Mesh* GenMeshCylinder_(float a, float b, int c) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshCylinder(a, b, c);
    return ptr;
}

Mesh* GenMeshCone_(float a, float b, int c) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshCone(a, b, c);
    return ptr;
}

Mesh* GenMeshTorus_(float a, float b, int c, int d) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshTorus(a, b, c, d);
    return ptr;
}

Mesh* GenMeshKnot_(float a, float b, int c, int d) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshKnot(a, b, c, d);
    return ptr;
}

Mesh* GenMeshHeightmap_(Image* a, Vector3* b) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshHeightmap(*a, *b);
    return ptr;
}

Mesh* GenMeshCubicmap_(Image* a, Vector3* b) {
    Mesh* ptr = (Mesh*) malloc(sizeof (Mesh));
    *ptr = GenMeshCubicmap(*a, *b);
    return ptr;
}

Material* LoadMaterialDefault_() {
    Material* ptr = (Material*) malloc(sizeof (Material));
    *ptr = LoadMaterialDefault();
    return ptr;
}

void UnloadMaterial_(Material* a) {
    UnloadMaterial(*a);
}

void SetMaterialTexture_(Material* a, int b, Texture* c) {
    SetMaterialTexture(a, b, *c);
}

void UpdateModelAnimation_(Model* a, ModelAnimation* b, int c) {
    UpdateModelAnimation(*a, *b, c);
}

void UnloadModelAnimation_(ModelAnimation* a) {
    UnloadModelAnimation(*a);
}

int IsModelAnimationValid_(Model* a, ModelAnimation* b) {
    return IsModelAnimationValid(*a, *b);
}

int CheckCollisionSpheres_(Vector3* a, float b, Vector3* c, float d) {
    return CheckCollisionSpheres(*a, b, *c, d);
}

int CheckCollisionBoxes_(BoundingBox* a, BoundingBox* b) {
    return CheckCollisionBoxes(*a, *b);
}

int CheckCollisionBoxSphere_(BoundingBox* a, Vector3* b, float c) {
    return CheckCollisionBoxSphere(*a, *b, c);
}

RayCollision* GetRayCollisionSphere_(Ray* a, Vector3* b, float c) {
    RayCollision* ptr = (RayCollision*) malloc(sizeof (RayCollision));
    *ptr = GetRayCollisionSphere(*a, *b, c);
    return ptr;
}

RayCollision* GetRayCollisionBox_(Ray* a, BoundingBox* b) {
    RayCollision* ptr = (RayCollision*) malloc(sizeof (RayCollision));
    *ptr = GetRayCollisionBox(*a, *b);
    return ptr;
}

RayCollision* GetRayCollisionMesh_(Ray* a, Mesh* b, Matrix* c) {
    RayCollision* ptr = (RayCollision*) malloc(sizeof (RayCollision));
    *ptr = GetRayCollisionMesh(*a, *b, *c);
    return ptr;
}

RayCollision* GetRayCollisionTriangle_(Ray* a, Vector3* b, Vector3* c, Vector3* d) {
    RayCollision* ptr = (RayCollision*) malloc(sizeof (RayCollision));
    *ptr = GetRayCollisionTriangle(*a, *b, *c, *d);
    return ptr;
}

RayCollision* GetRayCollisionQuad_(Ray* a, Vector3* b, Vector3* c, Vector3* d, Vector3* e) {
    RayCollision* ptr = (RayCollision*) malloc(sizeof (RayCollision));
    *ptr = GetRayCollisionQuad(*a, *b, *c, *d, *e);
    return ptr;
}

Wave* LoadWave_(string a) {
    Wave* ptr = (Wave*) malloc(sizeof (Wave));
    *ptr = LoadWave(a);
    return ptr;
}

Wave* LoadWaveFromMemory_(string a, unsigned char* b, int c) {
    Wave* ptr = (Wave*) malloc(sizeof (Wave));
    *ptr = LoadWaveFromMemory(a, b, c);
    return ptr;
}

Sound* LoadSound_(string a) {
    Sound* ptr = (Sound*) malloc(sizeof (Sound));
    *ptr = LoadSound(a);
    return ptr;
}

Sound* LoadSoundFromWave_(Wave* a) {
    Sound* ptr = (Sound*) malloc(sizeof (Sound));
    *ptr = LoadSoundFromWave(*a);
    return ptr;
}

void UpdateSound_(Sound* a, const void* b, int c) {
    UpdateSound(*a, b, c);
}

void UnloadWave_(Wave* a) {
    UnloadWave(*a);
}

void UnloadSound_(Sound* a) {
    UnloadSound(*a);
}

int ExportWave_(Wave* a, string b) {
    return ExportWave(*a, b);
}

int ExportWaveAsCode_(Wave* a, string b) {
    return ExportWaveAsCode(*a, b);
}

void PlaySound_(Sound* a) {
    PlaySound(*a);
}

void StopSound_(Sound* a) {
    StopSound(*a);
}

void PauseSound_(Sound* a) {
    PauseSound(*a);
}

void ResumeSound_(Sound* a) {
    ResumeSound(*a);
}

void PlaySoundMulti_(Sound* a) {
    PlaySoundMulti(*a);
}

int IsSoundPlaying_(Sound* a) {
    return IsSoundPlaying(*a);
}

void SetSoundVolume_(Sound* a, float b) {
    SetSoundVolume(*a, b);
}

void SetSoundPitch_(Sound* a, float b) {
    SetSoundPitch(*a, b);
}

void SetSoundPan_(Sound* a, float b) {
    SetSoundPan(*a, b);
}

Wave* WaveCopy_(Wave* a) {
    Wave* ptr = (Wave*) malloc(sizeof (Wave));
    *ptr = WaveCopy(*a);
    return ptr;
}

float* LoadWaveSamples_(Wave* a) {
    LoadWaveSamples(*a);
}

Music* LoadMusicStream_(string a) {
    Music* ptr = (Music*) malloc(sizeof (Music));
    *ptr = LoadMusicStream(a);
    return ptr;
}

Music* LoadMusicStreamFromMemory_(string a, unsigned char* b, int c) {
    Music* ptr = (Music*) malloc(sizeof (Music));
    *ptr = LoadMusicStreamFromMemory(a, b, c);
    return ptr;
}

void UnloadMusicStream_(Music* a) {
    UnloadMusicStream(*a);
}

void PlayMusicStream_(Music* a) {
    PlayMusicStream(*a);
}

int IsMusicStreamPlaying_(Music* a) {
    return IsMusicStreamPlaying(*a);
}

void UpdateMusicStream_(Music* a) {
    UpdateMusicStream(*a);
}

void StopMusicStream_(Music* a) {
    StopMusicStream(*a);
}

void PauseMusicStream_(Music* a) {
    PauseMusicStream(*a);
}

void ResumeMusicStream_(Music* a) {
    ResumeMusicStream(*a);
}

void SeekMusicStream_(Music* a, float b) {
    SeekMusicStream(*a, b);
}

void SetMusicVolume_(Music* a, float b) {
    SetMusicVolume(*a, b);
}

void SetMusicPitch_(Music* a, float b) {
    SetMusicPitch(*a, b);
}

void SetMusicPan_(Music* a, float b) {
    SetMusicPan(*a, b);
}

float GetMusicTimeLength_(Music* a) {
    return GetMusicTimeLength(*a);
}

float GetMusicTimePlayed_(Music* a) {
    return GetMusicTimePlayed(*a);
}

AudioStream* LoadAudioStream_(unsigned int a, unsigned int b, unsigned int c) {
    AudioStream* ptr = (AudioStream*) malloc(sizeof (AudioStream));
    *ptr = LoadAudioStream(a, b, c);
    return ptr;
}

void UnloadAudioStream_(AudioStream* a) {
    UnloadAudioStream(*a);
}

void UpdateAudioStream_(AudioStream* a, const void* b, int c) {
    UpdateAudioStream(*a, b, c);
}

int IsAudioStreamProcessed_(AudioStream* a) {
    return IsAudioStreamProcessed(*a);
}

void PlayAudioStream_(AudioStream* a) {
    PlayAudioStream(*a);
}

void PauseAudioStream_(AudioStream* a) {
    PauseAudioStream(*a);
}

void ResumeAudioStream_(AudioStream* a) {
    ResumeAudioStream(*a);
}

int IsAudioStreamPlaying_(AudioStream* a) {
    return IsAudioStreamPlaying(*a);
}

void StopAudioStream_(AudioStream* a) {
    StopAudioStream(*a);
}

void SetAudioStreamVolume_(AudioStream* a, float b) {
    SetAudioStreamVolume(*a, b);
}

void SetAudioStreamPitch_(AudioStream* a, float b) {
    SetAudioStreamPitch(*a, b);
}

void SetAudioStreamPan_(AudioStream* a, float b) {
    SetAudioStreamPan(*a, b);
}

void SetAudioStreamCallback_(AudioStream* a, AudioCallback* b) {
    SetAudioStreamCallback(*a, *b);
}

void AttachAudioStreamProcessor_(AudioStream* a, AudioCallback* b) {
    AttachAudioStreamProcessor(*a, *b);
}

void DetachAudioStreamProcessor_(AudioStream* a, AudioCallback* b) {
    DetachAudioStreamProcessor(*a, *b);
}

