
class a {

private:
  a (int i);

public:
  a ();
};

void test ()
{
  a *ap = new a;
  a *ap2 = new a (3);
}
