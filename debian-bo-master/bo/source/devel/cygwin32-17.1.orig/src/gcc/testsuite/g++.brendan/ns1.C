struct B 
{
  void f(char);
  void g(char);
};
  
class C 
{
  int g();
};

class D2 : public B 
{
  using B::f;  // ok: B is a base of D
  using C::g;  // error: C isn't a base of D2
};
