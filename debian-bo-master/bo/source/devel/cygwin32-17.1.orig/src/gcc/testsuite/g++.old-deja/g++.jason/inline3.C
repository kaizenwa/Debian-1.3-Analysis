// PRMS Id: 5732
// From the January '95 working paper:
// Build don't link:

class X
{
   public:
      int f();			// ERROR - 
      inline int g();
      int h();
};

void k(X* p)
{
   int i = p->f();
   int j = p->g();	// ERROR - inline called before definition
   // ...
}

inline int X::f()
{			// ERROR - called before defined as inline
   // ...
   return 0;
}

inline int X::g()
{
   // ...
   return 0;
}

inline int X::h()	// now X::h() has internal linkage
{
   // ...
   return 0;
}
