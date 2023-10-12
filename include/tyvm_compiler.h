#include <stdlib.h>

typedef struct {
  const char *ptr;
  size_t len;
} Source;

typedef struct {
  char *ptr;
  size_t len;
  size_t cap;
} Bytecode;

Bytecode tyvm_compile(Source source);
void tyvm_bytecode_free(Bytecode bytecode);