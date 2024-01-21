char a[10] = "deadbeef";

char
acc_a (int i)
{
  return a[i-2000000000];
}

main ()
{
  if (acc_a (2000000000) != 'd')
    abort ();
  exit (0);
}
