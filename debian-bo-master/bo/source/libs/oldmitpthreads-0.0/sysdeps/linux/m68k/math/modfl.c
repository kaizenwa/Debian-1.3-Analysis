#include <ansidecl.h>
#define __NO_MATH_INLINES
#include <math.h>

long double
DEFUN (modfl, (x, iptr),
       long double x AND long double *iptr)
{
  return __modfl (x, iptr);
}
