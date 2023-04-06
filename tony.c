#include <stdlib.h>

int tuxedo(void **test) { return 1; }

int main() {
  void *test = malloc(sizeof(int));
  *(int *)test = 5;

  tuxedo(&test);

  return 0;
}
