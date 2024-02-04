#include <raylib.h>
#include <rlgl.h>
#include <raygui.h>
#include <config.h>
#include <stdlib.h>

#ifndef RLBIND

#ifdef __EMSCRIPTEN__

#include <emscripten.h>
#define RLBIND EMSCRIPTEN_KEEPALIVE

#else

#define RLBIND

#endif

#endif
