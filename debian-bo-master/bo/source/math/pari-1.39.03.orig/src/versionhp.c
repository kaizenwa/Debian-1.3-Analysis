#include "genpari.h"

#ifdef __cplusplus
char *pariversion = "                   (C++ hppa version)\n";
#else
char *pariversion = "                     (hppa version)\n";
#endif

long
mulmodll(ulong a, ulong b, ulong c)
{
  divll(mulll(a,b),c);return hiremainder;
}

#include <time.h>

long
timer(void)
{
  static clock_t oldclocks;
  clock_t totalclocks = clock();
  ulong delay = (totalclocks - oldclocks) * 1000. / CLOCKS_PER_SEC;
  oldclocks = totalclocks;
  return delay;
}

long
timer2(void)
{
  static clock_t oldclocks;
  clock_t totalclocks = clock();
  ulong delay = (totalclocks - oldclocks) * 1000. / CLOCKS_PER_SEC;
  oldclocks = totalclocks;
  return delay;
}
