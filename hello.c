int f() {
  return 0;
}

int main() {
  int sum = 0;
  sum =(f() || f());
  return sum;
}
