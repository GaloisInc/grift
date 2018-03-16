#include "fib.h"

int fib_slow(int n) {
  if (n <= 0) return 0;
  if (n == 1) return 1;

  return fib(n-1) + fib(n-2);
}
