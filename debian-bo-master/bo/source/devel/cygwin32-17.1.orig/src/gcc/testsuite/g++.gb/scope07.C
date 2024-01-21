struct D {
  friend class A;
  friend class B;
  friend class C;

  void foo ();
};

void D::foo () { }
