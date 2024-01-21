// It seems reasonable to allow B::operator= below to count as a copy
// assignment operator, and the Booch Components depend on it.

int status = 1;

struct A {
  virtual A& operator= (const A&) = 0;
};

struct B: public A {
  virtual A& operator= (const A&) { status = 0; return *this; }
};

struct C: public B { };

main() 
{
  C c;
  c = c;
  return status;
}
