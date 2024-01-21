class A
{
private:
  A () {}

friend struct B;
};

class B
{
public:
  A a;
};

B b;

main () {}
