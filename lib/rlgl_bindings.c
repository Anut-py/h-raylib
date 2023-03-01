/**
 * See rlgl_bindings.h
 */

#include "rlgl_bindings.h"
#include <stdlib.h>

rlRenderBatch *rlLoadRenderBatch_(int numBuffers, int bufferElements)
{
  rlRenderBatch *ptr = (rlRenderBatch *)malloc(sizeof(rlRenderBatch));
  *ptr = rlLoadRenderBatch(numBuffers, bufferElements);
  return ptr;
}

void rlUnloadRenderBatch_(rlRenderBatch *batch)
{
  rlUnloadRenderBatch(*batch);
}

void rlSetUniformMatrix_(int locIndex, Matrix *mat)
{
  rlSetUniformMatrix(locIndex, *mat);
}

Matrix *rlGetMatrixProjectionStereo_(int eye)
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixProjectionStereo(eye);
  return ptr;
}

Matrix *rlGetMatrixViewOffsetStereo_(int eye)
{
  Matrix *ptr = (Matrix *)malloc(sizeof(Matrix));
  *ptr = rlGetMatrixViewOffsetStereo(eye);
  return ptr;
}

void rlSetMatrixProjection_(Matrix *proj)
{
  rlSetMatrixProjection(*proj);
}

void rlSetMatrixModelview_(Matrix *view)
{
  rlSetMatrixModelview(*view);
}

void rlSetMatrixProjectionStereo_(Matrix *right, Matrix *left)
{
  rlSetMatrixProjectionStereo(*right, *left);
}

void rlSetMatrixViewOffsetStereo_(Matrix *right, Matrix *left)
{
  rlSetMatrixViewOffsetStereo(*right, *left);
}
