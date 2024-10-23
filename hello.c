int main() {
    int x1 = 1;
    int x2 = 2;
    int x3 = 3;
    int x4 = 4;
    int x5 = 5;
    int x6 = 6;
    int x7 = 7;
    int x8 = 8;
    int x9 = 9;
    int x10 = 10;
    int x11 = 11;
    int x12 = 12;
    int x13 = 13;
    int x14 = 14;
    int x15 = 15;
    if (x2>x1) {
        x1 = x1+1;
    }
    if (x3>x2) {
        x2 = x2+1;
    }
    if (x4>x3) {
        x3 = x3+1;
        if (x5>x4) {
            x4 = x4+1;
        }
    }

    if (x6>x5 && x7 > x6) {
        x5 = x5+1;
    }
    if (x8>x7 && x9 > x8) {
        x6 = x6+1;
    }
    if (x10>x9 || x11 > x10) {
        x7 = x7+1;
    }
    return x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15;
}