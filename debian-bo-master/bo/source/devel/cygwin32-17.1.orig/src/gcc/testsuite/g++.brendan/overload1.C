class Foo
{
public:
  int f (void);
};

class Bar : public Foo
{
public:
  int f (int);
};

main ()
{
  Bar b;

  b.f ();
  b.f (10);
}
