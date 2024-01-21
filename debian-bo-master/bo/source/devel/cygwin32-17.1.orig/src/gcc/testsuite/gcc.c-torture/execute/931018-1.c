unsigned int a[0x1000];
extern unsigned v;

main ()
{
  f (v);
  f (v);
  exit (0);
}

f (a)
     unsigned int a;
{
  if (a != 0xdeadbeef)
    abort();
}

const unsigned v = 0xdeadbeef;
