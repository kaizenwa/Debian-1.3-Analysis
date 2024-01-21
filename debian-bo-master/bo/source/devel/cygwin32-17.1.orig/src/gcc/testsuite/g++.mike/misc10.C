extern "C" int printf(const char*, ...);

class B;

class AX
{
 protected:
   int x;

 public:
   operator B();
};


class B
{
 private:
   int x;
 public:
   B(const AX&);
};


int foo(B b);


main()
{
   AX a;
   foo(a);		// ERROR - ambiguous
}
