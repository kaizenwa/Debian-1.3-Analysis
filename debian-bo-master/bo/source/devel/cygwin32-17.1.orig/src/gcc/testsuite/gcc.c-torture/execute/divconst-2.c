f (x)
{
  return x / (-0x7fffffff - 1);
}

main ()
{
  if (f (-1) != 0 || f (0x7fffffff) != 0 || f (-0x7fffffff - 1) != 1)
    abort ();
  exit (0);
}
