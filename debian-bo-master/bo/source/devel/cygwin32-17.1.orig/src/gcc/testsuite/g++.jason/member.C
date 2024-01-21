struct Y
{
  struct X
    {
      int A;
      int Y::X::* foo () { undef1(1); return &Y::X::A; }
      int bar () { return A; }
    };
};

int Y::X::* foo ()
{
  undef2(1);
  return &Y::X::A;
}

int Y::X::* (* foo2 ())()
{
  undef3(1);
  return foo;
}

int (Y::X::* bar2 ()) ()
{
  undef4(1);
  return Y::X::bar;
}

int Y::X::* (Y::X::* foo3 ())()
{
  undef5(1);
  return Y::X::foo;
}
