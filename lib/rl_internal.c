/**
 * See rl_internal.h
 */

#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
#include "rl_internal.h"
#include <config.h>
#include <utils.h>

void UnloadAudioBuffer_(rAudioBuffer *buffer) {
    UnloadAudioBuffer(buffer);
}

void UnloadMusicStreamData(int ctxType, void *ctxData) {
    Music music = {0};
    music.ctxData = ctxData;
    music.ctxType = ctxType;

    UnloadMusicStream(music);
}