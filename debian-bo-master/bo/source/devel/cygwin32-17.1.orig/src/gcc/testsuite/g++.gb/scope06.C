struct C {
  struct D {
    int x;
    void foo ();
  };
  const int Ok = 0;
};

void C::D::foo ()
{
  x = Ok;
}
