#include <iostream.h>

class A {
   A() {}    // private constructor
};

main() {
  A* a = new A();
  if (a) {
     cout << "a != NULL\n";
  } else {
     cout << "a == NULL\n";
  }
}
