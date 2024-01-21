f (x, y)
{
  return (x > 1) ? y : (y & 1);
}

main ()
{
  if (f (2, 0xdecade) != 0xdecade)
    abort ();
  exit (0);
}
