// arg-matching file
// Subject: argument matching depending on the def order
// From: kondo@akane.mech.ibaraki.ac.jp
// Date: Fri, 04 Sep 92 17:41:05 JST

#include <iostream.h>
//  check the order of declarations
class A {
public:
    void f(double* p) { cout << "A(double*)\n"; }
    void f(int* p) { cout << "A(int*)\n"; }
};

class B {
public:
    void f(int* p) { cout << "B(int*)\n"; }
    void f(double* p) { cout << "B(double*)\n"; }
};

main()
{
    A a;
    B b;

    a.f(0);
    b.f(0);
}
