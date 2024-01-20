#include <web.h>

// See https://gitlab.haskell.org/ghc/ghc-wasm-meta#custom-imports
void _jslog(void *buf, uint32_t len) __attribute__((__import_module__("env"), __import_name__("log")));
void jslog(void *buf, uint32_t len) { _jslog(buf, len); }

void _jsfree(void *ptr) __attribute__((__import_module__("env"), __import_name__("free")));
void jsfree(void *ptr) { _jsfree(ptr); }

void *_callRaylibFunction(char *name, uint32_t nameLen, void **params, uint32_t *sizeParams, uint8_t *typeParams, uint32_t numParams, uint32_t returnSizeBytes, uint8_t returnType) __attribute__((__import_module__("env"), __import_name__("callRaylibFunction")));
void *callRaylibFunction(
  char *name, uint32_t nameLen, void **params, uint32_t *sizeParams, uint8_t *typeParams, uint32_t numParams, uint32_t returnSizeBytes, uint8_t returnType)
{
  return _callRaylibFunction(name, nameLen, params, sizeParams, typeParams, numParams, returnSizeBytes, returnType);
}
