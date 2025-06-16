#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include "printer.h"

int64_t bird_main(int64_t* heap_cursor, int64_t* end_of_heap) asm("bird_main");

int main(int argc, char** argv) {

  // Creating a tiny 32-word heap so GC is easy to inspect
  int64_t* heap_cursor = malloc(sizeof(int64_t)* 128);
  int64_t* heap_end = heap_cursor + 128;


  int64_t result = bird_main(heap_cursor, heap_end);
  // printValue("%"PRId64"\n", result);
  printValue(result);
  return 0;
}
