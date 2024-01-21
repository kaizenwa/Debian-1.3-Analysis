#include <iostream.h>

class A {
 public:
  virtual ~A() {cout << "executed ~A()\n";};
};

class B : public A {
 public:
  virtual ~B() {cout << "executed ~B()\n";};
};

main() {
  cout << "starting\n";
  B b;
  b.~A();
  cout << "done\n";
};

