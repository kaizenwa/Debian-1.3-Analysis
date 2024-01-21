struct A
{
  static void f ();
  struct B
  {
    void h () { f (); }
  };
};
