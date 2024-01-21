/************************************************************************
 * cmds.c -- part of rpncalc.c						*
 *									*
 * Refer to rpncalc.c for copyrights and license permissions.           *
 ************************************************************************/

/* $Id: cmds.c,v 1.5 1997/01/19 20:01:18 david Rel $
 * $Log: cmds.c,v $
 * Revision 1.5  1997/01/19 20:01:18  david
 * FreeBSD `port': Added #define EOVERFLOW ERANGE.
 *
 * Revision 1.4  1997/01/19 18:49:47  david
 * Removed some casts to make gcc -Wall -W... happy.
 *
 * Revision 1.4  1997/01/19 18:19:23  david
 * Removed some casts to make gcc -Wall -W... happy.
 *
 * Revision 1.2  1996/09/13 20:21:29  david
 * lclint additions
 *
 * Revision 1.1  1996/07/13 20:49:35  david
 * Cleanup and renaming due to linting of the source.
 *
 * Revision 1.0  1995/12/31 18:21:07  david
 * Initial revision
 * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <limits.h>

#include "cmds.h"

extern int errno;
extern int pushtostack;
extern int digits;
extern enum BASE base;

static double res;

/* -------------------------------------------------------- constants */

double pi(void) { return M_PI; }

double e(void)  { return M_E; }

/* -------------------------------------------------------- unary operators */
#ifndef linux
double pow10(double y) { return pow(10,y); }

double pow2(double y)  { return pow(2,y); }
#endif

#ifdef freebsd
#define EOVERFLOW ERANGE
#endif

double chs(double f)   { return -f; }

double sqr(double f)   { return f * f; }

double inv(double f)
{
  if (f != 0)   res = 1 / f;
  else          { res = HUGE_VAL; errno = ERANGE; }	    
  return res;
}

double log2(double f)  { return log(f) / M_LN2; }

double fact(double d)
{
  long int i, n;

  n=(long int)d;
  if (n < 0)
  {
    fprintf(stderr, "defined only for arguments >= 0.\n");
    pushtostack=0;
  }
  else 
  {
    if ((n == 0) || (n == 1)) res = 1;
    else
    {
      res = 1;
      for (i = n; i > 1; i--) res *= i;
    }
  }
  return res;
}

double prec(double p)
{
  digits=ceil(p); pushtostack=0;
  return 0.0; /* dummy value */
}

double not(double l) { return (double)(~(long int)l); }

/* ------------------------------------------------------- binary operators */

double plus(double s1, double s2) { return s1 + s2; }

double minus(double s, double m)  { return  s-m; }

double multiply(double f1, double f2) { return f1 * f2; }

double divide(double n, double d)
{
  if (d != 0) res = n / d;
  else      { res = HUGE_VAL; errno = ERANGE; }	    
  return res;
}

#ifndef sunos4
static ldiv_t quotrem;

double idiv(double dn, double dd)
{
  long int n, d;

  n=(long int)dn; d=(long int)dd;
  if ((n <= LONG_MAX) && (d <= LONG_MAX))
  {
    if (d != 0)
    {
      quotrem = ldiv(n,d); 
      res = (double)quotrem.quot;
    }
    else
    { 
      res = HUGE_VAL; errno = ERANGE; 
    }	 
  }
  else
  { 
    errno = EOVERFLOW; pushtostack=0;
  }	    
  return res;
}
#else
double idiv(double dn, double dd)
{
  long int n, d;

  res=(long)(n/d);
  return res;
}
#endif

#ifndef sunos4
double mod(double dn, double dd)
{
 long int n, d;

 n=(long int)dn; d=(long int)dd;
 if ((n <= LONG_MAX) && (d <= LONG_MAX))
 {
   if (d != 0)
   {
     quotrem = ldiv(n,d);
     res = (double)quotrem.rem;  
   }
   else
   { 
     res = HUGE_VAL; errno = ERANGE; 
   }	    
 }
 else
 {	
   errno = EOVERFLOW; pushtostack=0;
 }	    
 return res;
}
#else
double mod(double dn, double dd)
{
 res=fmod(dn, dd);
 return res;
}
#endif

/* Stein's Greatest Common Divisor (GCD) algorithm */
long int stein(double d1, double d2)
{
  long int n1, n2;
  long int d;				 /* difference */
  long int c;				 /* c: shift-count (see below) */
  long int t;				 /* temporary */

  c = 0; n1=(long int)d1; n2=(long int)d2;
  /* both integers are even; shift them until one gets odd. */
  while (((n1 & 1) == 0) && ((n2 & 1) == 0))
  {
    n1 >>= 1; n2 >>= 1;  c++;
  }

  do
  {
    if ((n2 & 1) == 0) { t = n1; n1 = n2; n2 = t; }
    
    while ((n1 & 1) == 0)
      n1 >>= 1;

    /*
     * shift n1 until it gets odd.
     */
    d = n1 - n2;
    if (d < 0) { n2 = n1; d = -d; }
    n1 = d >> 1;
  }
  while (n1 > 0);

  return (n2 * (1 << c));
}

double gcd(double dn, double dd)
{
  long int n, d;

  n=(long int)dn; d=(long int)dd;
  if ((n <= LONG_MAX) && (d <= LONG_MAX))
  {
    if (d != 0) res = stein(dn,dd);
    else      { res = HUGE_VAL; errno = ERANGE; }
  }
  else
  {
#ifndef sunos4	      
    errno = EOVERFLOW;	
#endif
    pushtostack = 0;
  }
  return res;
}

double and(double l1, double l2)
{
  return (double)((long int)l1 & (long int)l2);
}

double or(double l1, double l2)
{
  return (double)((long int)l1 | (long int)l2);
}

double xor(double l1, double l2)
{
  return (double)((long int)l1 ^ (long int)l2);
}

/* ------------------------------------------------------- n-ary operators */
double sum(void)
{
  long int i,n;

  n = depth(); res = 0;
  for(i=1;i<=n;i++) res += pick((double)i);
  return res;
}

double prod(void)
{
  long int i,n;

  n = depth(); res = 1;
  for(i=1;i<=n;i++) res *= pick((double)i);
  return res;
}
