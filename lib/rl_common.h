#include <raylib.h>
#include <rlgl.h>
#include <raygui.h>
#include <config.h>
#include <stdlib.h>

#ifndef RLBIND

#ifdef __EMSCRIPTEN__

#include <emscripten.h>
#define RLBIND EMSCRIPTEN_KEEPALIVE __attribute__((always_inline))

#else

#define RLBIND __attribute__((always_inline))

#endif

#endif
