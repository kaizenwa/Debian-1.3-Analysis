#include <iostream.h>

class A {
public:
  friend A f(A &a);
};

A &f(A &a) {
  cout << "Blah\n";
}

