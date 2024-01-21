struct A { A (int); };
struct B : A {};
void f () { B (0); }
