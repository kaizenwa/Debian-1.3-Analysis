class X // Indentation has been done so to see the similarities.
{
public:
  X() {}
         X(X& x) {x.i=7;} // ERROR - Both functions modify the XFAIL *-*-*
  void bar(X& x) {x.i=7;} // ERROR - reference parameter x.
  int i;
};

X foo() { X x; return x; }

main() 
{
  X   x(foo()); // ERROR - Compiler doesn't warn about temporary reference. XFAIL *-*-*
  x.bar(foo()); // ERROR - The same mistake is warned about in this case.
}
