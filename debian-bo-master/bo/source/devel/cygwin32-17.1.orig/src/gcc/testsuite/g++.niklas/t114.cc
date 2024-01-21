struct A { A(); };
struct B { B(A&); };
B b(A());
