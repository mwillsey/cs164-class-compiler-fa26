#include <inttypes.h>
#include <stdio.h>

// int64_t entry() { return 4000000000000; }
extern int64_t entry();

int main(int argc, char **argv) {
  printf("%" PRIi64, entry());
  return 0;
}