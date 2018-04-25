const int INPUT = 20;

int fib(int x) {
  if (x <= 0) return 0;
  if (x == 1) return 1;
  return fib(x-1) + fib(x-2);
}

int main() {
  return fib(INPUT);
}
