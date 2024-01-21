// ARM $5.3.2

void f() { }

struct foo { int bit : 1; };

int
main()
{
  // sizeof may not be applied to a bit-field
  foo f;
  int i = sizeof (f.bit);

  return 0;
}
