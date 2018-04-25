const long int INPUT = 20;

long int fact(long int x) {
  long int res = 1;
  for (; x > 0; --x) res *= x;
  return res;
}


long int main() {
  return fact(INPUT);
}
