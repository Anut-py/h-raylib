/**
 * See rl_internal.h
 */

#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
#include "rl_internal.h"
#include <config.h>

void UnloadAudioBuffer_(rAudioBuffer *buffer)
{
    UnloadAudioBuffer(buffer);
}

void UnloadMusicStreamData(int ctxType, void *ctxData)
{
    Music music = {0};
    music.ctxData = ctxData;
    music.ctxType = ctxType;

    UnloadMusicStream(music);
}

int rlGetPixelDataSize(int width, int height, int format)
{
    int dataSize = 0; // Size in bytes
    int bpp = 0;      // Bits per pixel

    switch (format)
    {
    case RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE:
        bpp = 8;
        break;
    case RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA:
    case RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5:
    case RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1:
    case RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4:
        bpp = 16;
        break;
    case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8:
        bpp = 32;
        break;
    case RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8:
        bpp = 24;
        break;
    case RL_PIXELFORMAT_UNCOMPRESSED_R32:
        bpp = 32;
        break;
    case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32:
        bpp = 32 * 3;
        break;
    case RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32:
        bpp = 32 * 4;
        break;
    case RL_PIXELFORMAT_COMPRESSED_DXT1_RGB:
    case RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA:
    case RL_PIXELFORMAT_COMPRESSED_ETC1_RGB:
    case RL_PIXELFORMAT_COMPRESSED_ETC2_RGB:
    case RL_PIXELFORMAT_COMPRESSED_PVRT_RGB:
    case RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA:
        bpp = 4;
        break;
    case RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA:
    case RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA:
    case RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA:
    case RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA:
        bpp = 8;
        break;
    case RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA:
        bpp = 2;
        break;
    default:
        break;
    }

    dataSize = width * height * bpp / 8; // Total data size in bytes

    // Most compressed formats works on 4x4 blocks,
    // if texture is smaller, minimum dataSize is 8 or 16
    if ((width < 4) && (height < 4))
    {
        if ((format >= RL_PIXELFORMAT_COMPRESSED_DXT1_RGB) && (format < RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA))
            dataSize = 8;
        else if ((format >= RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA) && (format < RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA))
            dataSize = 16;
    }

    return dataSize;
}