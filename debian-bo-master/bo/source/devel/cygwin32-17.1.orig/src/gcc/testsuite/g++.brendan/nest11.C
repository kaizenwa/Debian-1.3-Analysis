class A {
  int x;

  struct B {
    int x;
  };
  struct C {
    int bug (A::B &y);
  };
};

int
A::C::bug (A::B &y)
{
  return y.x;
}

