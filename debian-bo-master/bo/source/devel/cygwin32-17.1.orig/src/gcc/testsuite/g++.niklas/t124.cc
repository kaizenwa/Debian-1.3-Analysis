struct A
{
  static void f ();
  struct B
  {
    static void g () { f (); }
  };
};
