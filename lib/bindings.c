/**
 * See bindings.h
 */

#include "bindings.h"

void SetWindowIcon_(Image* a) {
    SetWindowIcon(*a);
}

Vector2* GetMonitorPosition_(int a) {
    Vector2 val = GetMonitorPosition(a);
    return &val;
}

Vector2* GetWindowPosition_() {
    Vector2 val = GetWindowPosition();
    return &val;
}

Vector2* GetWindowScaleDPI_() {
    Vector2 val = GetWindowScaleDPI();
    return &val;
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
    VrStereoConfig val = LoadVrStereoConfig(*a);
    return &val;
}

void UnloadVrStereoConfig_(VrStereoConfig* a) {
    UnloadVrStereoConfig(*a);
}

Shader* LoadShader_(string a, string b) {
    Shader val = LoadShader(a, b);
    return &val;
}

Shader* LoadShaderFromMemory_(string a, string b) {
    Shader val = LoadShaderFromMemory(a, b);
    return &val;
}

int GetShaderLocation_(Shader* a, string b) {
    GetShaderLocation(*a, b);
}

int GetShaderLocationAttrib_(Shader* a, string b) {
    GetShaderLocationAttrib(*a, b);
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
    Ray val = GetMouseRay(*a, *b);
    return &val;
}

Matrix* GetCameraMatrix_(Camera3D* a) {
    Matrix val = GetCameraMatrix(*a);
    return &val;
}

Matrix* GetCameraMatrix2D_(Camera2D* a) {
    GetCameraMatrix2D(*a);
}

Vector2* GetWorldToScreen_(Vector3* a, Camera3D* b) {
    Vector2 val = GetWorldToScreen(*a, *b);
    return &val;
}

Vector2* GetScreenToWorld2D_(Vector2* a, Camera2D* b) {
    GetScreenToWorld2D(*a, *b);
}

Vector2* GetWorldToScreenEx_(Vector3* a, Camera3D* b, int c, int d) {
    Vector2 val = GetWorldToScreenEx(*a, *b, c, d);
    return &val;
}

Vector2* GetWorldToScreen2D_(Vector2* a, Camera2D* b) {
    GetWorldToScreen2D(*a, *b);
}

FilePathList* LoadDirectoryFiles_(string a) {
    FilePathList val = LoadDirectoryFiles(a);
    return &val;
}

FilePathList* LoadDirectoryFilesEx_(string a, string b, int c) {
    FilePathList val = LoadDirectoryFilesEx(a, b, c);
    return &val;
}

void UnloadDirectoryFiles_(FilePathList* a) {
    UnloadDirectoryFiles(*a);
}

FilePathList* LoadDroppedFiles_() {
    FilePathList val = LoadDroppedFiles();
    return &val;
}

void UnloadDroppedFiles_(FilePathList* a) {
    UnloadDroppedFiles(*a);
}

Vector2* GetMousePosition_() {
    Vector2 val = GetMousePosition();
    return &val;
}

Vector2* GetMouseDelta_() {
    Vector2 val = GetMouseDelta();
    return &val;
}

Vector2* GetMouseWheelMoveV_() {
    Vector2 val = GetMouseWheelMoveV();
    return &val;
}

Vector2* GetTouchPosition_(int a) {
    Vector2 val = GetTouchPosition(a);
    return &val;
}

Vector2* GetGestureDragVector_() {
    Vector2 val = GetGestureDragVector();
    return &val;
}

Vector2* GetGesturePinchVector_() {
    Vector2 val = GetGesturePinchVector();
    return &val;
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
    CheckCollisionRecs(*a, *b);
}

int CheckCollisionCircles_(Vector2* a, float b, Vector2* c, float d) {
    CheckCollisionCircles(*a, b, *c, d);
}

int CheckCollisionCircleRec_(Vector2* a, float b, Rectangle* c) {
    CheckCollisionCircleRec(*a, b, *c);
}

int CheckCollisionPointRec_(Vector2* a, Rectangle* b) {
    CheckCollisionPointRec(*a, *b);
}

int CheckCollisionPointCircle_(Vector2* a, Vector2* b, float c) {
    CheckCollisionPointCircle(*a, *b, c);
}

int CheckCollisionPointTriangle_(Vector2* a, Vector2* b, Vector2* c, Vector2* d) {
    CheckCollisionPointTriangle(*a, *b, *c, *d);
}

int CheckCollisionLines_(Vector2* a, Vector2* b, Vector2* c, Vector2* d, Vector2* e) {
    CheckCollisionLines(*a, *b, *c, *d, e);
}

int CheckCollisionPointLine_(Vector2* a, Vector2* b, Vector2* c, int d) {
    CheckCollisionPointLine(*a, *b, *c, d);
}

Rectangle* GetCollisionRec_(Rectangle* a, Rectangle* b) {
    Rectangle val = GetCollisionRec(*a, *b);
    return &val;
}

Image* LoadImage_(string a) {
    Image val = LoadImage(a);
    return &val;
}

Image* LoadImageRaw_(string a, int b, int c, int d, int e) {
    Image val = LoadImageRaw(a, b, c, d, e);
    return &val;
}

Image* LoadImageAnim_(string a, int* b) {
    Image val = LoadImageAnim(a, b);
    return &val;
}

Image* LoadImageFromMemory_(string a, unsigned char* b, int c) {
    Image val = LoadImageFromMemory(a, b, c);
    return &val;
}

Image* LoadImageFromTexture_(Texture* a) {
    Image val = LoadImageFromTexture(*a);
    return &val;
}

Image* LoadImageFromScreen_() {
    Image val = LoadImageFromScreen();
    return &val;
}

void UnloadImage_(Image* a) {
    UnloadImage(*a);
}

int ExportImage_(Image* a, string b) {
    ExportImage(*a, b);
}

int ExportImageAsCode_(Image* a, string b) {
    ExportImageAsCode(*a, b);
}

Image* GenImageColor_(int a, int b, Color* c) {
    Image val = GenImageColor(a, b, *c);
    return &val;
}

Image* GenImageGradientV_(int a, int b, Color* c, Color* d) {
    Image val = GenImageGradientV(a, b, *c, *d);
    return &val;
}

Image* GenImageGradientH_(int a, int b, Color* c, Color* d) {
    Image val = GenImageGradientH(a, b, *c, *d);
    return &val;
}

Image* GenImageGradientRadial_(int a, int b, float c, Color* d, Color* e) {
    Image val = GenImageGradientRadial(a, b, c, *d, *e);
    return &val;
}

Image* GenImageChecked_(int a, int b, int c, int d, Color* e, Color* f) {
    Image val = GenImageChecked(a, b, c, d, *e, *f);
    return &val;
}

Image* GenImageWhiteNoise_(int a, int b, float c) {
    Image val = GenImageWhiteNoise(a, b, c);
    return &val;
}

Image* GenImageCellular_(int a, int b, int c) {
    Image val = GenImageCellular(a, b, c);
    return &val;
}

Image* ImageCopy_(Image* a) {
    Image val = ImageCopy(*a);
    return &val;
}

Image* ImageFromImage_(Image* a, Rectangle* b) {
    Image val = ImageFromImage(*a, *b);
    return &val;
}

Image* ImageText_(string a, int b, Color* c) {
    Image val = ImageText(a, b, *c);
    return &val;
}

Image* ImageTextEx_(Font* a, string b, float c, float d, Color* e) {
    Image val = ImageTextEx(*a, b, c, d, *e);
    return &val;
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
    Rectangle val = GetImageAlphaBorder(*a, b);
    return &val;
}

Color* GetImageColor_(Image* a, int b, int c) {
    Color val = GetImageColor(*a, b, c);
    return &val;
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
    Texture val = LoadTexture(a);
    return &val;
}

Texture* LoadTextureFromImage_(Image* a) {
    Texture val = LoadTextureFromImage(*a);
    return &val;
}

Texture* LoadTextureCubemap_(Image* a, int b) {
    Texture val = LoadTextureCubemap(*a, b);
    return &val;
}

RenderTexture* LoadRenderTexture_(int a, int b) {
    RenderTexture val = LoadRenderTexture(a, b);
    return &val;
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
    Color val = Fade(*a, b);
    return &val;
}

int ColorToInt_(Color* a) {
    ColorToInt(*a);
}

Vector4* ColorNormalize_(Color* a) {
    Vector4 val = ColorNormalize(*a);
    return &val;
}

Color* ColorFromNormalized_(Vector4* a) {
    Color val = ColorFromNormalized(*a);
    return &val;
}

Vector3* ColorToHSV_(Color* a) {
    Vector3 val = ColorToHSV(*a);
    return &val;
}

Color* ColorFromHSV_(float a, float b, float c) {
    Color val = ColorFromHSV(a, b, c);
    return &val;
}

Color* ColorAlpha_(Color* a, float b) {
    Color val = ColorAlpha(*a, b);
    return &val;
}

Color* ColorAlphaBlend_(Color* a, Color* b, Color* c) {
    Color val = ColorAlphaBlend(*a, *b, *c);
    return &val;
}

Color* GetColor_(unsigned int a) {
    Color val = GetColor(a);
    return &val;
}

Color* GetPixelColor_(const void* a, int b) {
    Color val = GetPixelColor(a, b);
    return &val;
}

void SetPixelColor_(const void* a, Color* b, int c) {
    SetPixelColor(a, *b, c);
}

Font* GetFontDefault_() {
    Font val = GetFontDefault();
    return &val;
}

Font* LoadFont_(string a) {
    Font val = LoadFont(a);
    return &val;
}

Font* LoadFontEx_(string a, int b, int* c, int d) {
    Font val = LoadFontEx(a, b, c, d);
    return &val;
}

Font* LoadFontFromImage_(Image* a, Color* b, int c) {
    Font val = LoadFontFromImage(*a, *b, c);
    return &val;
}

Font* LoadFontFromMemory_(string a, unsigned char* b, int c, int d, int* e, int f) {
    Font val = LoadFontFromMemory(a, b, c, d, e, f);
    return &val;
}

Image* GenImageFontAtlas_(GlyphInfo* a, Rectangle** b, int c, int d, int e, int f) {
    Image val = GenImageFontAtlas(a, b, c, d, e, f);
    return &val;
}

void UnloadFont_(Font* a) {
    UnloadFont(*a);
}

int ExportFontAsCode_(Font* a, string b) {
    ExportFontAsCode(*a, b);
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
    Vector2 val = MeasureTextEx(*a, b, c, d);
    return &val;
}

int GetGlyphIndex_(Font* a, int b) {
    GetGlyphIndex(*a, b);
}

GlyphInfo* GetGlyphInfo_(Font* a, int b) {
    GlyphInfo val = GetGlyphInfo(*a, b);
    return &val;
}

Rectangle* GetGlyphAtlasRec_(Font* a, int b) {
    Rectangle val = GetGlyphAtlasRec(*a, b);
    return &val;
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
    Model val = LoadModel(a);
    return &val;
}

Model* LoadModelFromMesh_(Mesh* a) {
    Model val = LoadModelFromMesh(*a);
    return &val;
}

void UnloadModel_(Model* a) {
    UnloadModel(*a);
}

void UnloadModelKeepMeshes_(Model* a) {
    UnloadModelKeepMeshes(*a);
}

BoundingBox* GetModelBoundingBox_(Model* a) {
    BoundingBox val = GetModelBoundingBox(*a);
    return &val;
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
    ExportMesh(*a, b);
}

BoundingBox* GetMeshBoundingBox_(Mesh* a) {
    BoundingBox val = GetMeshBoundingBox(*a);
    return &val;
}

Mesh* GenMeshPoly_(int a, float b) {
    Mesh val = GenMeshPoly(a, b);
    return &val;
}

Mesh* GenMeshPlane_(float a, float b, int c, int d) {
    Mesh val = GenMeshPlane(a, b, c, d);
    return &val;
}

Mesh* GenMeshCube_(float a, float b, float c) {
    Mesh val = GenMeshCube(a, b, c);
    return &val;
}

Mesh* GenMeshSphere_(float a, int b, int c) {
    Mesh val = GenMeshSphere(a, b, c);
    return &val;
}

Mesh* GenMeshHemiSphere_(float a, int b, int c) {
    Mesh val = GenMeshHemiSphere(a, b, c);
    return &val;
}

Mesh* GenMeshCylinder_(float a, float b, int c) {
    Mesh val = GenMeshCylinder(a, b, c);
    return &val;
}

Mesh* GenMeshCone_(float a, float b, int c) {
    Mesh val = GenMeshCone(a, b, c);
    return &val;
}

Mesh* GenMeshTorus_(float a, float b, int c, int d) {
    Mesh val = GenMeshTorus(a, b, c, d);
    return &val;
}

Mesh* GenMeshKnot_(float a, float b, int c, int d) {
    Mesh val = GenMeshKnot(a, b, c, d);
    return &val;
}

Mesh* GenMeshHeightmap_(Image* a, Vector3* b) {
    Mesh val = GenMeshHeightmap(*a, *b);
    return &val;
}

Mesh* GenMeshCubicmap_(Image* a, Vector3* b) {
    Mesh val = GenMeshCubicmap(*a, *b);
    return &val;
}

Material* LoadMaterialDefault_() {
    Material val = LoadMaterialDefault();
    return &val;
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
    IsModelAnimationValid(*a, *b);
}

int CheckCollisionSpheres_(Vector3* a, float b, Vector3* c, float d) {
    CheckCollisionSpheres(*a, b, *c, d);
}

int CheckCollisionBoxes_(BoundingBox* a, BoundingBox* b) {
    CheckCollisionBoxes(*a, *b);
}

int CheckCollisionBoxSphere_(BoundingBox* a, Vector3* b, float c) {
    CheckCollisionBoxSphere(*a, *b, c);
}

RayCollision* GetRayCollisionSphere_(Ray* a, Vector3* b, float c) {
    RayCollision val = GetRayCollisionSphere(*a, *b, c);
    return &val;
}

RayCollision* GetRayCollisionBox_(Ray* a, BoundingBox* b) {
    RayCollision val = GetRayCollisionBox(*a, *b);
    return &val;
}

RayCollision* GetRayCollisionMesh_(Ray* a, Mesh* b, Matrix* c) {
    RayCollision val = GetRayCollisionMesh(*a, *b, *c);
    return &val;
}

RayCollision* GetRayCollisionTriangle_(Ray* a, Vector3* b, Vector3* c, Vector3* d) {
    RayCollision val = GetRayCollisionTriangle(*a, *b, *c, *d);
    return &val;
}

RayCollision* GetRayCollisionQuad_(Ray* a, Vector3* b, Vector3* c, Vector3* d, Vector3* e) {
    RayCollision val = GetRayCollisionQuad(*a, *b, *c, *d, *e);
    return &val;
}

Wave* LoadWave_(string a) {
    Wave val = LoadWave(a);
    return &val;
}

Wave* LoadWaveFromMemory_(string a, unsigned char* b, int c) {
    Wave val = LoadWaveFromMemory(a, b, c);
    return &val;
}

Sound* LoadSound_(string a) {
    Sound val = LoadSound(a);
    return &val;
}

Sound* LoadSoundFromWave_(Wave* a) {
    Sound val = LoadSoundFromWave(*a);
    return &val;
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
    ExportWave(*a, b);
}

int ExportWaveAsCode_(Wave* a, string b) {
    ExportWaveAsCode(*a, b);
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
    IsSoundPlaying(*a);
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
    Wave val = WaveCopy(*a);
    return &val;
}

float* LoadWaveSamples_(Wave* a) {
    LoadWaveSamples(*a);
}

Music* LoadMusicStream_(string a) {
    Music val = LoadMusicStream(a);
    return &val;
}

Music* LoadMusicStreamFromMemory_(string a, unsigned char* b, int c) {
    Music val = LoadMusicStreamFromMemory(a, b, c);
    return &val;
}

void UnloadMusicStream_(Music* a) {
    UnloadMusicStream(*a);
}

void PlayMusicStream_(Music* a) {
    PlayMusicStream(*a);
}

int IsMusicStreamPlaying_(Music* a) {
    IsMusicStreamPlaying(*a);
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
    GetMusicTimeLength(*a);
}

float GetMusicTimePlayed_(Music* a) {
    GetMusicTimePlayed(*a);
}

AudioStream* LoadAudioStream_(unsigned int a, unsigned int b, unsigned int c) {
    AudioStream val = LoadAudioStream(a, b, c);
    return &val;
}

void UnloadAudioStream_(AudioStream* a) {
    UnloadAudioStream(*a);
}

void UpdateAudioStream_(AudioStream* a, const void* b, int c) {
    UpdateAudioStream(*a, b, c);
}

int IsAudioStreamProcessed_(AudioStream* a) {
    IsAudioStreamProcessed(*a);
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
    IsAudioStreamPlaying(*a);
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

