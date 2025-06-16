#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Expected an int.\n");
      break;
    case 2:
      printf("Expected boolean. \n");
      break;
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
