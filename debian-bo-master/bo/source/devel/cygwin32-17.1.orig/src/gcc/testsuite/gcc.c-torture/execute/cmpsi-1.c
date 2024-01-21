f (unsigned int x, unsigned int y)
{
  if (x == 0)
    dummy ();
  x -= y;
  if (x < (unsigned int) 0x80000000)
    abort ();
  return x;
}

dummy () {}

main ()
{
  f (0x7ffffff3, 0x80000001);
  exit (0);
}
