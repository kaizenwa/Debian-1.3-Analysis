#include "mathl.h"

#ifndef SOFT_387

long double
modfl(long double x, long double *iptr)
{
  long double tmp;
  volatile short cw, cwtmp;

  __asm__ volatile ("fnstcw %0" : "=m" (cw) : );
  cwtmp = cw | 0xc00;
  __asm__ volatile ("fldcw %0" : : "m" (cwtmp));
  __asm__ volatile ("frndint" : "=t" (tmp) : "0" (x));
  __asm__ volatile ("fldcw %0" : : "m" (cw));
  *iptr = tmp;

  return (x - tmp);
}

#else

long double fabsl(), floorl();

/*  Returns fractional part of d, stores integer part in *integ
 */
long double modf(long double d, long double *integ)
{
  long double i, f;

  f = fabsl(d);
  i = floorl(f);
  f -= i;
  if( d < 0.0 )
    {
      i = -i;
      f = -f;
    }
  *integ = i;
  return f;
}
#endif
