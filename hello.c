int main() {
  int a = 0;
  const int b = 1 - 1 * 2 + 1;
  int c = 1, d = 2;
  if (a || b) {
    c = 3;
  } else {
    d = 3;
  }
  return a + b + c + d;
}
