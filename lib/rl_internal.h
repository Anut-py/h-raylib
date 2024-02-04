/**
 * @file rl_internal.h
 * @author Anut-py
 * @brief Additional functions required for C/Haskell FFI
 */
#include "rl_common.h"

void UnloadAudioBuffer_(rAudioBuffer *buffer);

void UnloadAudioBufferAlias(rAudioBuffer *buffer);

void UnloadMusicStreamData(int ctxType, void *ctxData);

int rlGetPixelDataSize(int width, int height, int format);
