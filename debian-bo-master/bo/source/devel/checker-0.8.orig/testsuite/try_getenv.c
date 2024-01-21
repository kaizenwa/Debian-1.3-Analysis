#include <stdlib.h>
#include <stdio.h>

main()
{
  char *s;
  time (0);
  s= getenv ("HOME");
  printf ("HOME=%s\n", s);
}
