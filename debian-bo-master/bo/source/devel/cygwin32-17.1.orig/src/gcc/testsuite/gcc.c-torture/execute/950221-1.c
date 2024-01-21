struct parsefile
{
  int fd;
  char *buf;
};
struct parsefile basepf;
struct parsefile *parsefile = &basepf;
int filler[0x3000];
int el;

char *
g1 (a, b)
     int a;
     int *b;
{
}

g2 (a)
     int a;
{
  if (a != 0xdeadbeef)
    abort ();
  exit (0);
}

f ()
{
  register char *p, *q;
  register int i;
  register int something;

  if (parsefile->fd == 0 && el)
    {
      const char *rl_cp;
      int len;
      rl_cp = g1 (el, &len);
      strcpy (p, rl_cp);
    }
  else
    {
    alabel:
      i = g2 (parsefile->fd);
    }
}

main ()
{
  el = 0;
  parsefile->fd = 0xdeadbeef;
  f ();
}
