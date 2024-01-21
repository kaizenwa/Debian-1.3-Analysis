int a = 100000;
int b = 21475;

f ()
{
  return ((long long) a * (long long) b) >> 16;
}

main ()
{
  if (f () < 0)
    abort ();
  exit (0);
}
