unsigned*
f(p)unsigned*p;
{
  unsigned a = (*p++) >> 24;
  return p + a;
}

main ()
{
  unsigned x = 0x80000000;
  if (f(&x) != &x + 0x81)
    abort();
  exit(0);
}
