#ifndef RLBIND

#ifdef __EMSCRIPTEN__

#include <emscripten.h>
#define RLBIND EMSCRIPTEN_KEEPALIVE

#else

#define RLBIND

#endif

#endif
