#include <iostream>
#include "GCD.cpp"

int GCDApp() {
  GCD gcd;
  gcd.io_a = 6;
  gcd.io_b = 3;
  gcd.io_e = true;
  gcd.eval(true);
  gcd.io_e = false;
  for (int i = 0; i < 5; i++) {
    std::cout << "i is " << i << "\n";
    std::cout << "x is " << gcd.x << " y is " << gcd.y << "\n";
    std::cout << std::boolalpha;   
    std::cout << "io_v is " << gcd.io_v << "\n";
    gcd.eval(true);
  }
} 