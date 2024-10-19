int f() {
  return 0;
}

int main() {
  int sum = 0;
  sum =(f() || 5);
  return sum;
}
