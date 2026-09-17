#include <inttypes.h>
#include <stdio.h>

#define num_shift 2
#define num_mask 0b11
#define num_tag 0b00

#define bool_shift 7
#define bool_mask 0b1111111
#define bool_tag 0b0011111

void print_value(uint64_t value) {
  if ((value & num_mask) == num_tag) {
    int64_t int_value = (int64_t)value;
    printf("%" PRIi64, int_value >> num_shift);
  } else if ((value & bool_mask) == bool_tag) {
    if (value >> bool_shift) {
      printf("true");
    } else {
      printf("false");
    }
  } else {
    printf("BAD VALUE %" PRIu64, value);
  }
}

// int64_t entry() { return 4000000000000; }
extern uint64_t entry();

int main(int argc, char **argv) {
  print_value(entry());
  return 0;
}