#include <stdio.h>

main()
{
  char *st;
#ifndef __PIC__
  run();
#endif
  st = strdup("Hello");
  printf("%s world\n", st);
}
