struct A {};
struct B : virtual A { B(); };
struct C : B {};
struct D { D(C&); };
D d(C());
