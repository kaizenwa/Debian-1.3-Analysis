#include "math-ieee.h"

#define _IEEE754_DOUBLE_BIAS            0x3ff   /* added to exp of ieee754_double */

#define shiftleft(dp,n)	{	/* n = 0 to 32 */ \
	dp->ieee.mantissa0 = ((dp->ieee.mantissa0 << (n)) + (dp->ieee.mantissa1 >> (32-(n)))) \
		& 0x0FFFFF; dp->ieee.mantissa1 <<= (n); dp->ieee.exponent -= (n); }


/*  Returns fractional part of d, stores integer part in *integ
 */
double
modf (double d, double *integ)
{
  union ieee754_double *dp = (union ieee754_double *)&d;
  union ieee754_double *ip = (union ieee754_double *)integ;
  int e = dp->ieee.exponent - _IEEE754_DOUBLE_BIAS;

  if (e < 0)
    {		/* no integer part */
      *integ = 0;
      return d;
    }

  /* compute integer: clear fractional part where necessary 
   */
  *integ = d;
  if (e <= 20)
    {
      ip->ieee.mantissa0 &= (-1L << (20-e));		/* split in mant1... */
      ip->ieee.mantissa1 = 0;
    }
  else 
   if (e <= 52) 
     ip->ieee.mantissa1 &= (-1L << (52-e));		/* split in mant2 */
   else return 0;				/* no fractional part */

  /* compute fractional part: shift left over integer part
   */
  if (e)
    if (e <= 32)
      shiftleft (dp,e)
    else
      {
	dp->ieee.mantissa0 = (dp->ieee.mantissa1 << (e-32)) & 0x0FFFFF;
	dp->ieee.mantissa1 = 0;
	dp->ieee.exponent -= e;
      }

  /* adjust fractional part shifting left... 
   */
  if (dp->ieee.mantissa0 == 0 && dp->ieee.mantissa1 == 0)	/* fraction is zero */
    return 0;

  while (!(dp->ieee.mantissa0 & 0x080000)) 	/* stack to the left */
    shiftleft (dp,1);

  shiftleft (dp,1);			/* lose 'invisible bit' */
  return d;
}
