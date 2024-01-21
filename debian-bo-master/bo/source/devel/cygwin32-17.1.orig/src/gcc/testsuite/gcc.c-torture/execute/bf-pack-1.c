struct foo
{
  unsigned half:16;
  unsigned whole:32 __attribute__ ((packed));
};

f (struct foo *q)
{
  if (q->half != 0x1234)
    abort ();
  if (q->whole != 0x56789abc)
    abort ();
}

main ()
{
  struct foo bar;

  bar.half = 0x1234;
  bar.whole = 0x56789abc;
  f (&bar);
  exit (0);
}
