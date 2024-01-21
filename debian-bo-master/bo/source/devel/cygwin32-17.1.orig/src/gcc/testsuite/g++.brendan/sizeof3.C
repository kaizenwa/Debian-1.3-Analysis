// ARM $5.3.2

class bar;

int
main()
{
  // sizeof may not be applied to an undefined class
  int k = sizeof (bar);

  return 0;
}
