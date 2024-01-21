void f ();
void g1 (void (*) (...)); void h1 () { g1 (f); }
struct S { void g2 (void (*) (...)); void h2 () { g2 (f); } };
