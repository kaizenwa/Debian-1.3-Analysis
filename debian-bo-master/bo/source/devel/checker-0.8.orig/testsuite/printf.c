#include <stdlib.h>
void
my_bye(void)
{
  printf("exiting...");
}

main()
{
 char *str = "Hello!";
 atexit(my_bye);
 printf("%%s  : %s\n", str);
 printf("%%12s: %12s\n", str);
 printf("%%09s: %09s\n", str);
 exit(1);
}

#if 0
 %s  : Hello!
%12s:       Hello!
%09s: 000Hello!
#endif
