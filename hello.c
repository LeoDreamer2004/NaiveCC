int main() {
  int i = 0;
  while (i < 10) {
    {continue;}
    continue;
    i = 20;
    continue;
    i = i + 1;
  }
  return i;
}
