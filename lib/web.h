#ifndef WEB_H
#define WEB_H

#include <stdint.h>

void jslog(void *buf, uint32_t len);
void jsfree(void *ptr);
void *callRaylibFunction(char *name, uint32_t nameLen, void **params, uint32_t *sizeParams, uint8_t *typeParams, uint32_t numParams, uint32_t returnSizeBytes, uint8_t returnType);

#endif