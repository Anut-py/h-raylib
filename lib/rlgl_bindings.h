/**
 * @file rlgl_bindings.h
 * @author Anut-py
 * @brief Required methods for binding Haskell to rlgl
 */

#include <rlgl.h>

rlRenderBatch *rlLoadRenderBatch_(int numBuffers, int bufferElements);

void rlUnloadRenderBatch_(rlRenderBatch *batch);

void rlSetUniformMatrix_(int locIndex, Matrix *mat);

Matrix *rlGetMatrixProjectionStereo_(int eye);

Matrix *rlGetMatrixViewOffsetStereo_(int eye);

void rlSetMatrixProjection_(Matrix *proj);

void rlSetMatrixModelview_(Matrix *view);

void rlSetMatrixProjectionStereo_(Matrix *right, Matrix *left);

void rlSetMatrixViewOffsetStereo_(Matrix *right, Matrix *left);
