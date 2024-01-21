struct A {
  struct B { void f (); };
  struct C : B { void f () { B::f (); } };
};
