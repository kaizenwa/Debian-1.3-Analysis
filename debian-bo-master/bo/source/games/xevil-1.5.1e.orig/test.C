#include <iostream.h>
#include <iomanip.h>

float val = 3.1903287419247023983427;

void foo() {
  cout << setprecision(3) << val << endl;
  cout << val << endl;
}


void bar() {
  cout << val << endl;
}

void main() {
  foo();
  bar();
}


