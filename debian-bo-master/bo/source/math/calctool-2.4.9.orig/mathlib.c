
/*  @(#)mathlib.c 1.2 90/02/02
 *
 *  These are the mathematical routines used by calctool.
 *
 *  This is being done because libm.a doesn't appear to be as portable
 *  as originally assumed.
 *
 *  It is hoped that your system supplies all the mathematical functions
 *  required by calctool. If not then, it is possible to use the needed
 *  ones from this library. Look in mathlib.h for a set of definitions,
 *  and uncomment and set appropriately.
 *
 *  These routines are taken from two sources:
 *
 *  1/ Fred Fishs' portable maths library which was posted to the
 *     comp.sources.unix newsgroup on April 1987.
 *
 *     acos, acosh, asin, asinh, atan, atanh, cos, cosh, dabs,
 *     exp, log, log10, mod, poly, sin, sinh, sqrt, tan, tanh.
 *
 *  2/ The BSD4.3 maths library found on uunet in
 *     bsd_sources/src/usr.lib/libm.
 *
 *     pow, pow_p, scalb, logb, copysign, finite, drem, exp__E,
 *     log__L.
 *
 *  Customised and condensed by Rich Burridge,
 *                              Sun Microsystems, Australia.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

/************************************************************************
 *                                                                      *
 *                              N O T I C E                             *
 *                                                                      *
 *                      Copyright Abandoned, 1987, Fred Fish            *
 *                                                                      *
 *      This previously copyrighted work has been placed into the       *
 *      public domain by the author (Fred Fish) and may be freely used  *
 *      for any purpose, private or commercial.  I would appreciate     *
 *      it, as a courtesy, if this notice is left in all copies and     *
 *      derivative works.  Thank you, and enjoy...                      *
 *                                                                      *
 *      The author makes no warranty of any kind with respect to this   *
 *      product and explicitly disclaims any implied warranties of      *
 *      merchantability or fitness for any particular purpose.          *
 *                                                                      *
 ************************************************************************
 */

#include <stdio.h>
#include <errno.h>
#include "mathlib.h"
#include "calctool.h"

double atan(), cos(), cosh(), dabs(), exp(), frexp() ;
double ldexp(), log(), mod(), modf(), poly(), sin() ;
double sinh(), sqrt() ;

extern double frexp(), ldexp(), modf() ;


/*  FUNCTION
 *
 *      acos   double precision arc cosine
 *
 *  DESCRIPTION
 *
 *      Returns double precision arc cosine of double precision
 *      floating point argument.
 *
 *  USAGE
 *
 *      double acos(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran IV-plus user's guide, Digital Equipment Corp. pp B-1.
 *
 *  RESTRICTIONS
 *
 *      The maximum relative error for the approximating polynomial
 *      in atan is 10**(-16.82).  However, this assumes exact arithmetic
 *      in the polynomial evaluation.  Additional rounding and
 *      truncation errors may occur as the argument is reduced
 *      to the range over which the polynomial approximation
 *      is valid, and as the polynomial is evaluated using
 *      finite-precision arithmetic.
 *
 *  INTERNALS
 *
 *      Computes arccosine(x) from:
 *
 *              (1)     If x < -1.0  or x > +1.0 then call
 *                      doerr and return 0.0 by default.
 *
 *              (2)     If x = 0.0 then acos(x) = PI/2.
 *
 *              (3)     If x = 1.0 then acos(x) = 0.0
 *
 *              (4)     If x = -1.0 then acos(x) = PI.
 *
 *              (5)     If 0.0 < x < 1.0 then
 *                      acos(x) = atan(Y)
 *                      Y = sqrt[1-(x**2)] / x
 *
 *              (6)     If -1.0 < x < 0.0 then
 *                      acos(x) = atan(Y) + PI
 *                      Y = sqrt[1-(x**2)] / x
 */

#ifdef   NEED_ACOS
double
acos(x)
double x ;
{
  double y ;
  auto double retval ;

  if (x > 1.0 || x < -1.0)
    {
      doerr("acos", "DOMAIN", EDOM) ;
      retval = 0.0 ;
    }
  else if (x == 0.0)  retval = HALFPI ;
  else if (x == 1.0)  retval = 0.0 ;
  else if (x == -1.0) retval = PI ;
  else
    {
      y = atan(sqrt(1.0 - (x * x)) / x) ;
      if (x > 0.0) retval = y ;
      else retval = y + PI ;
    }    
  return(retval) ;
}
#endif /*NEED_ACOS*/


/*  FUNCTION
 *
 *      acosh   double precision hyperbolic arc cosine
 *
 *  DESCRIPTION
 *
 *      Returns double precision hyperbolic arc cosine of double precision
 *      floating point number.
 *
 *  USAGE
 *
 *      double acosh(x)
 *      double x ;
 *
 *  RESTRICTIONS
 *
 *      The range of the ACOSH function is all real numbers greater
 *      than or equal to 1.0 however large arguments may cause
 *      overflow in the x squared portion of the function evaluation.
 *
 *      For precision information refer to documentation of the
 *      floating point library primatives called.
 *
 *  INTERNALS
 *
 *      Computes acosh(x) from:
 *
 *              1.      If x < 1.0 then report illegal
 *                      argument and return zero.
 *
 *              2.      If x > sqrt(MAXDOUBLE) then
 *                      set x = sqrt(MAXDOUBLE and
 *                      continue after reporting overflow.
 *
 *              3.      acosh(x) = log [x+sqrt(x**2 - 1)]
 */

#ifdef   NEED_ACOSH
double
acosh(x)
double x ;
{
  auto double retval ;

  if (x < 1.0)
    {
      doerr("acosh", "DOMAIN", ERANGE) ;
      retval = 0.0 ;
    }
  else if (x > SQRT_MAXDOUBLE)
    {
      doerr("acosh", "OVERFLOW", ERANGE) ;
      x = SQRT_MAXDOUBLE ;
      retval = log(2* SQRT_MAXDOUBLE) ;
    }
  else retval = log(x + sqrt(x*x - 1.0)) ;
  return(retval) ;
}
#endif /*NEED_ACOSH*/


/*
 *  FUNCTION
 *
 *      asin   double precision arc sine
 *
 *  DESCRIPTION
 *
 *      Returns double precision arc sine of double precision
 *      floating point argument.
 *
 *      If argument is less than -1.0 or greater than +1.0, calls
 *      doerr with a DOMAIN error.  If doerr does not handle
 *      the error then prints error message and returns 0.
 *
 *  USAGE
 *
 *      double asin(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran IV-plus user's guide, Digital Equipment Corp. pp B-2.
 *
 *  RESTRICTIONS
 *
 *      For precision information refer to documentation of the floating
 *      point library primatives called.
 *
 *  INTERNALS
 *
 *      Computes arcsine(x) from:
 *
 *              (1)     If x < -1.0 or x > +1.0 then
 *                      call doerr and return 0.0 by default.
 *
 *              (2)     If x = 0.0 then asin(x) = 0.0
 *
 *              (3)     If x = 1.0 then asin(x) = PI/2.
 *
 *              (4)     If x = -1.0 then asin(x) = -PI/2
 *
 *              (5)     If -1.0 < x < 1.0 then
 *                      asin(x) = atan(y)
 *                      y = x / sqrt[1-(x**2)]
 */

#ifdef NEED_ASIN
double
asin(x)
double x ;
{
  auto double retval ;

  if ( x > 1.0 || x < -1.0)
    {
      doerr("asin", "DOMAIN", EDOM) ;
      retval = 0.0 ;
    }
  else if (x == 0.0) retval = 0.0 ;
  else if (x == 1.0) retval = HALFPI ;
  else if (x == -1.0) retval = -HALFPI ;
  else retval = atan(x / sqrt (1.0 - (x * x))) ;
  return(retval) ;
}
#endif /*NEED_ASIN*/


/*  FUNCTION
 *
 *      asinh   double precision hyperbolic arc sine
 *
 *  DESCRIPTION
 *
 *      Returns double precision hyperbolic arc sine of double precision
 *      floating point number.
 *
 *  USAGE
 *
 *      double asinh(x)
 *      double x ;
 *
 *  RESTRICTIONS
 *
 *      The domain of the ASINH function is the entire real axis
 *      however the evaluation of x squared may cause overflow
 *      for large magnitudes.
 *
 *      For precision information refer to documentation of the
 *      floating point library routines called.
 *
 *  INTERNALS
 *
 *      Computes asinh(x) from:
 *
 *              1.      Let xmax = sqrt(MAXDOUBLE - 1)
 *
 *              2.      If x < -xmax or xmax < x then
 *                      let x = xmax and flag overflow.
 *
 *              3.      asinh(x) = log [x+sqrt(x**2 + 1)]
 */

#ifdef NEED_ASINH
double
asinh(x)
double x ;
{
  auto double retval ;

  if (x < -SQRT_MAXDOUBLE || x > SQRT_MAXDOUBLE)
    {
      doerr("asinh", "OVERFLOW", ERANGE) ;
      retval = log(2 * SQRT_MAXDOUBLE) ;
    }
  else retval = log(x + sqrt(x*x + 1.0)) ;
  return(retval) ;
}
#endif /*NEED_ASINH*/


/*  FUNCTION
 *
 *      atan   double precision arc tangent
 *
 *  DESCRIPTION
 *
 *      Returns double precision arc tangent of double precision
 *      floating point argument.
 *
 *  USAGE
 *
 *      double atan(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran 77 user's guide, Digital Equipment Corp. pp B-3
 *
 *      Computer Approximations, J.F. Hart et al, John Wiley & Sons,
 *      1968, pp. 120-130.
 *
 *  RESTRICTIONS
 *
 *      The maximum relative error for the approximating polynomial
 *      is 10**(-16.82).  However, this assumes exact arithmetic
 *      in the polynomial evaluation.  Additional rounding and
 *      truncation errors may occur as the argument is reduced
 *      to the range over which the polynomial approximation
 *      is valid, and as the polynomial is evaluated using
 *      finite-precision arithmetic.
 *
 *  INTERNALS
 *
 *      Computes arctangent(x) from:
 *
 *              (1)     If x < 0 then negate x, perform steps
 *                      2, 3, and 4, and negate the returned
 *                      result.  This makes use of the identity
 *                      atan(-x) = -atan(x).
 *
 *              (2)     If argument x > 1 at this point then
 *                      test to be sure that x can be inverted
 *                      without underflowing.  If not, reduce
 *                      x to largest possible number that can
 *                      be inverted, issue warning, and continue.
 *                      Perform steps 3 and 4 with arg = 1/x
 *                      and subtract returned result from
 *                      pi/2.  This makes use of the identity
 *                      atan(x) = pi/2 - atan(1/x) for x>0.
 *
 *              (3)     At this point 0 <= x <= 1.  If
 *                      x > tan(pi/12) then perform step 4
 *                      with arg = (x*sqrt(3)-1)/(sqrt(3)+x)
 *                      and add pi/6 to returned result.  This
 *                      last transformation maps arguments
 *                      greater than tan(pi/12) to arguments
 *                      in range 0...tan(pi/12).
 *
 *              (4)     At this point the argument has been
 *                      transformed so that it lies in the
 *                      range -tan(pi/12)...tan(pi/12).
 *                      Since very small arguments may cause
 *                      underflow in the polynomial evaluation,
 *                      a final check is performed.  If the
 *                      argument is less than the underflow
 *                      bound, the function returns x, since
 *                      atan(x) approaches asin(x) which
 *                      approaches x, as x goes to zero.
 *
 *              (5)     atan(x) is now computed by one of the
 *                      approximations given in the cited
 *                      reference (Hart).  That is:
 *
 *                      atan(x) = x * SUM [ C[i] * x**(2*i) ]
 *                      over i = {0,1,...8}.
 *
 *                      Where:
 *
 *                      C[0] =  .9999999999999999849899
 *                      C[1] =  -.333333333333299308717
 *                      C[2] =  .1999999999872944792
 *                      C[3] =  -.142857141028255452
 *                      C[4] =  .11111097898051048
 *                      C[5] =  -.0909037114191074
 *                      C[6] =  .0767936869066
 *                      C[7] =  -.06483193510303
 *                      C[8] =  .0443895157187
 *                      (coefficients from HART table #4945 pg 267)
 */

#ifdef   NEED_ATAN

#define  LAST_BOUND  0.2679491924311227074725     /*  tan (PI/12) */

static double atan_coeffs[] = {
    .9999999999999999849899,                    /* P0 must be first     */
    -.333333333333299308717,
    .1999999999872944792,
    -.142857141028255452,
    .11111097898051048,
    -.0909037114191074,
    .0767936869066,
    -.06483193510303,
    .0443895157187                              /* Pn must be last      */
} ;

double
atan(x)
double x ;
{
  register int order ;
  double t1, t2, xt2 ;
  auto double retval ;

  if (x < 0.0) retval = -(atan(-x)) ;
  else if (x > 1.0)
    {
      if (x < MAXDOUBLE && x > -MAXDOUBLE)
        retval = HALFPI - atan(1.0 / x) ;
      else
        {
          doerr("atan", "UNDERFLOW", EDOM) ;
          retval = 0.0 ;
        }    
    }
  else if (x > LAST_BOUND)
    {
      t1 = x * SQRT3 - 1.0 ;
      t2 = SQRT3 + x ;
      retval = SIXTHPI + atan(t1 / t2) ;
    }
  else if (x < X16_UNDERFLOWS)
    {
      doerr("atan", "PLOSS", EDOM) ;
      retval = x ;
    }
  else
    {
      xt2 = x * x ;
      order = sizeof(atan_coeffs) / sizeof(double) ;
      order -= 1 ;
      retval = x * poly(order, atan_coeffs, xt2) ;
    }
  return(retval) ;
}
#endif /*NEED_ATAN*/


/*  FUNCTION
 *
 *      atanh   double precision hyperbolic arc tangent
 *
 *  DESCRIPTION
 *
 *      Returns double precision hyperbolic arc tangent of double precision
 *      floating point number.
 *
 *  USAGE
 *
 *      double atanh(x)
 *      double x ;
 *
 *  RESTRICTIONS
 *
 *      The range of the atanh function is -1.0 to +1.0 exclusive.
 *      Certain pathological cases near 1.0 may cause overflow
 *      in evaluation of 1+x / 1-x, depending upon machine exponent
 *      range and mantissa precision.
 *
 *      For precision information refer to documentation of the
 *      other floating point library routines called.
 *
 *  INTERNALS
 *
 *      Computes atanh(x) from:
 *
 *              1.      If x <= -1.0 or x >= 1.0
 *                      then report argument out of range and return 0.0
 *
 *              2.      atanh(x) = 0.5 * log((1+x)/(1-x))
 */

#ifdef   NEED_ATANH
double
atanh(x)
double x ;
{
  auto double retval ;

  if (x <= -1.0 || x >= 1.0)
    {
      doerr("atan", "DOMAIN", ERANGE) ;
      retval = 0.0 ;
    }
  else retval = 0.5 * log((1+x) / (1-x)) ;
  return(retval) ;
}
#endif /*NEED_ATANH*/


/*  FUNCTION
 *
 *      cos  -  double precision cosine
 *
 *  DESCRIPTION
 *
 *      Returns double precision cosine of double precision
 *      floating point argument.
 *
 *  USAGE
 *
 *      double cos(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Computer Approximations, J.F. Hart et al, John Wiley & Sons,
 *      1968, pp. 112-120.
 *
 *  RESTRICTIONS
 *
 *      The sin and cos routines are interactive in the sense that
 *      in the process of reducing the argument to the range -PI/4
 *      to PI/4, each may call the other.  Ultimately one or the
 *      other uses a polynomial approximation on the reduced
 *      argument.  The sin approximation has a maximum relative error
 *      of 10**(-17.59) and the cos approximation has a maximum
 *      relative error of 10**(-16.18).
 *
 *      These error bounds assume exact arithmetic
 *      in the polynomial evaluation.  Additional rounding and
 *      truncation errors may occur as the argument is reduced
 *      to the range over which the polynomial approximation
 *      is valid, and as the polynomial is evaluated using
 *      finite-precision arithmetic.
 *
 *  INTERNALS
 *
 *      Computes cos(x) from:
 *
 *              (1)     Reduce argument x to range -PI to PI.
 *
 *              (2)     If x > PI/2 then call cos recursively
 *                      using relation cos(x) = -cos(x - PI).
 *
 *              (3)     If x < -PI/2 then call cos recursively
 *                      using relation cos(x) = -cos(x + PI).
 *
 *              (4)     If x > PI/4 then call sin using
 *                      relation cos(x) = sin(PI/2 - x).
 *
 *              (5)     If x < -PI/4 then call cos using
 *                      relation cos(x) = sin(PI/2 + x).
 *
 *              (6)     If x would cause underflow in approx
 *                      evaluation arithmetic then return
 *                      sqrt(1.0 - x**2).
 *
 *              (7)     By now x has been reduced to range
 *                      -PI/4 to PI/4 and the approximation
 *                      from HART pg. 119 can be used:
 *
 *                      cos(x) = ( p(y) / q(y) )
 *                      Where:
 *
 *                      y    =  x * (4/PI)
 *
 *                      p(y) =  SUM [ Pj * (y**(2*j)) ]
 *                      over j = {0,1,2,3}
 *
 *                      q(y) =  SUM [ Qj * (y**(2*j)) ]
 *                      over j = {0,1,2,3}
 *
 *                      P0   =  0.12905394659037374438571854e+7
 *                      P1   =  -0.3745670391572320471032359e+6
 *                      P2   =  0.134323009865390842853673e+5
 *                      P3   =  -0.112314508233409330923e+3
 *                      Q0   =  0.12905394659037373590295914e+7
 *                      Q1   =  0.234677731072458350524124e+5
 *                      Q2   =  0.2096951819672630628621e+3
 *                      Q3   =  1.0000...
 *                      (coefficients from HART table #3843 pg 244)
 *
 *
 *      **** NOTE ****    The range reduction relations used in
 *      this routine depend on the final approximation being valid
 *      over the negative argument range in addition to the positive
 *      argument range.  The particular approximation chosen from
 *      HART satisfies this requirement, although not explicitly
 *      stated in the text.  This may not be true of other
 *      approximations given in the reference.
 */

#ifdef   NEED_COS

static double cos_pcoeffs[] = {
    0.12905394659037374438e7,
   -0.37456703915723204710e6,
    0.13432300986539084285e5,
   -0.11231450823340933092e3
} ;

static double cos_qcoeffs[] = {
    0.12905394659037373590e7,
    0.23467773107245835052e5,
    0.20969518196726306286e3,
    1.0
} ;

double
cos(x)
double x ;
{
  auto double y, yt2 ;
  auto double retval ;

  if (x < -PI || x > PI)
    {
      x = mod(x, TWOPI) ;
           if (x > PI)  x = x - TWOPI ;
      else if (x < -PI) x = x + TWOPI ;
    }    
       if (x > HALFPI)    retval = -(cos(x - PI)) ;
  else if (x < -HALFPI)   retval = -(cos(x + PI)) ;
  else if (x > FOURTHPI)  retval = sin(HALFPI - x) ;
  else if (x < -FOURTHPI) retval = sin(HALFPI + x) ;
  else if (x < X6_UNDERFLOWS && x > -X6_UNDERFLOWS)
    retval = sqrt(1.0 - (x * x)) ;
  else
    {
      y = x / FOURTHPI ;
      yt2 = y * y ;
      retval = poly(3, cos_pcoeffs, yt2) / poly(3, cos_qcoeffs, yt2) ;
    }
  return(retval) ;
}
#endif /*NEED_COS*/


/*  FUNCTION
 *
 *      cosh   double precision hyperbolic cosine
 *
 *  DESCRIPTION
 *
 *      Returns double precision hyperbolic cosine of double precision
 *      floating point number.
 *
 *  USAGE
 *
 *      double cosh(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran IV plus user's guide, Digital Equipment Corp. pp B-4
 *
 *  RESTRICTIONS
 *
 *      Inputs greater than log(MAXDOUBLE) result in overflow.
 *      Inputs less than log(MINDOUBLE) result in underflow.
 *
 *      For precision information refer to documentation of the
 *      floating point library routines called.
 *
 *  INTERNALS
 *
 *      Computes hyperbolic cosine from:
 *
 *              cosh(X) = 0.5 * (exp(X) + exp(-X))
 */


#ifdef   NEED_COSH
double
cosh(x)
double x ;
{
  auto double retval ;

  if (x > LOGE_MAXDOUBLE)
    {
      doerr("cosh", "OVERFLOW", ERANGE) ;
      retval = MAXDOUBLE ;
    }
  else if (x < LOGE_MINDOUBLE)
    {
      doerr("cosh", "UNDERFLOW", ERANGE) ;
      retval = MINDOUBLE ;
    }
  else
    {
      x = exp(x) ;
      retval = 0.5 * (x + 1.0 / x) ;
    }
  return(retval) ;
}
#endif /*NEED_COSH*/


/*  FUNCTION
 *
 *      dabs   double precision absolute value
 *
 *  DESCRIPTION
 *
 *      Returns absolute value of double precision number.
 *
 *  USAGE
 *
 *      double dabs(x)
 *      double x ;
 */

#ifdef   NEED_EXP
double
dabs(x)
double x ;
{
  if (x < 0.0) x = -x ;
  return(x) ;
}
#endif /*NEED_EXP*/


/*  FUNCTION
 *
 *      exp   double precision exponential
 *
 *  DESCRIPTION
 *
 *      Returns double precision exponential of double precision
 *      floating point number.
 *
 *  USAGE
 *
 *      double exp(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran IV plus user's guide, Digital Equipment Corp. pp B-3
 *
 *      Computer Approximations, J.F. Hart et al, John Wiley & Sons,
 *      1968, pp. 96-104.
 *
 *  RESTRICTIONS
 *
 *      Inputs greater than log(MAXDOUBLE) result in overflow.
 *      Inputs less than log(MINDOUBLE) result in underflow.
 *
 *      The maximum relative error for the approximating polynomial
 *      is 10**(-16.4).  However, this assumes exact arithmetic
 *      in the polynomial evaluation.  Additional rounding and
 *      truncation errors may occur as the argument is reduced
 *      to the range over which the polynomial approximation
 *      is valid, and as the polynomial is evaluated using
 *      finite precision arithmetic.
 *
 *
 *  INTERNALS
 *
 *      Computes exponential from:
 *
 *              exp(x)  =       2**y  *  2**z  *  2**w
 *
 *      Where:
 *
 *              y       =       int ( x * log2(e) )
 *
 *              v       =       16 * frac ( x * log2(e))
 *
 *              z       =       (1/16) * int (v)
 *
 *              w       =       (1/16) * frac (v)
 *
 *      Note that:
 *
 *              0 =< v < 16
 *
 *              z = {0, 1/16, 2/16, ...15/16}
 *
 *              0 =< w < 1/16
 *
 *      Then:
 *
 *              2**z is looked up in a table of 2**0, 2**1/16, ...
 *
 *              2**w is computed from an approximation:
 *
 *                      2**w    =  (A + B) / (A - B)
 *
 *                      A       =  C + (D * w * w)
 *
 *                      B       =  w * (E + (F * w * w))
 *
 *                      C       =  20.8137711965230361973
 *
 *                      D       =  1.0
 *
 *                      E       =  7.2135034108448192083
 *
 *                      F       =  0.057761135831801928
 *
 *              Coefficients are from HART, table #1121, pg 206.
 *
 *              Effective multiplication by 2**y is done by a
 *              floating point scale with y as scale argument.
 */

#ifdef   NEED_EXP

#define  C           20.8137711965230361973   /* Polynomial approx coeff. */
#define  D           1.0                      /* Polynomial approx coeff. */
#define  E           7.2135034108448192083    /* Polynomial approx coeff. */
#define  F           0.057761135831801928     /* Polynomial approx coeff. */

/************************************************************************
 *                                                                      *
 *      This table is fixed in size and reasonably hardware             *
 *      independent.  The given constants were generated on a           *
 *      DECSYSTEM 20 using double precision FORTRAN.                    *
 *                                                                      *
 ************************************************************************
 */

static double fpof2tbl[] = {
    1.00000000000000000000,             /* 2 ** 0/16  */
    1.04427378242741384020,             /* 2 ** 1/16  */
    1.09050773266525765930,             /* 2 ** 2/16  */
    1.13878863475669165390,             /* 2 ** 3/16  */
    1.18920711500272106640,             /* 2 ** 4/16  */
    1.24185781207348404890,             /* 2 ** 5/16  */
    1.29683955465100966610,             /* 2 ** 6/16  */
    1.35425554693689272850,             /* 2 ** 7/16  */
    1.41421356237309504880,             /* 2 ** 8/16  */
    1.47682614593949931110,             /* 2 ** 9/16  */
    1.54221082540794082350,             /* 2 ** 10/16 */
    1.61049033194925430820,             /* 2 ** 11/16 */
    1.68179283050742908600,             /* 2 ** 12/16 */
    1.75625216037329948340,             /* 2 ** 13/16 */
    1.83400808640934246360,             /* 2 ** 14/16 */
    1.91520656139714729380              /* 2 ** 15/16 */
} ;

double
exp(x)
double x ;
{
  register int ival, y ;
  auto double a, b, rtnval, t, temp, v, w, wpof2, zpof2 ;
  auto double retval ;

  if (x > LOGE_MAXDOUBLE)
    {
      doerr("exp", "OVERFLOW", ERANGE) ;
      retval = MAXDOUBLE ;
    }
  else if (x <= LOGE_MINDOUBLE)
    {
      doerr("exp", "UNDERFLOW", ERANGE) ;
      retval = 0.0 ;
    }
   else
    {
      t = x * LOG2E ;
      (void) modf(t, &temp) ;
      y = temp ;
      v = 16.0 * modf(t, &temp) ;
      (void) modf(dabs(v), &temp) ;
      ival = temp ;
      if (x < 0.0) zpof2 = 1.0 / fpof2tbl[ival] ;
      else         zpof2 = fpof2tbl[ival] ;
      w = modf(v, &temp) / 16.0 ;
      a = C + (D * w * w) ;
      b = w * (E + (F * w * w)) ;
      wpof2 = (a + b) / (a - b) ;
      retval = ldexp((wpof2 * zpof2), y) ;
    }
  return(retval) ;
}
#endif /*NEED_EXP*/


/*  FUNCTION
 *
 *      log   double precision natural log
 *
 *  DESCRIPTION
 *
 *      Returns double precision natural log of double precision
 *      floating point argument.
 *
 *  USAGE
 *
 *      double log(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Computer Approximations, J.F. Hart et al, John Wiley & Sons,
 *      1968, pp. 105-111.
 *
 *  RESTRICTIONS
 *
 *      The absolute error in the approximating polynomial is
 *      10**(-19.38).  Note that contrary to DEC's assertion
 *      in the F4P user's guide, the error is absolute and not
 *      relative.
 *
 *      This error bound assumes exact arithmetic
 *      in the polynomial evaluation.  Additional rounding and
 *      truncation errors may occur as the argument is reduced
 *      to the range over which the polynomial approximation
 *      is valid, and as the polynomial is evaluated using
 *      finite-precision arithmetic.
 *
 *  INTERNALS
 *
 *      Computes log(X) from:
 *
 *        (1)   If argument is zero then flag an error
 *              and return minus infinity (or rather its
 *              machine representation).
 *
 *        (2)   If argument is negative then flag an
 *              error and return minus infinity.
 *
 *        (3)   Given that x = m * 2**k then extract
 *              mantissa m and exponent k.
 *
 *        (4)   Transform m which is in range [0.5,1.0]
 *              to range [1/sqrt(2), sqrt(2)] by:
 *
 *                      s = m * sqrt(2)
 *
 *        (5)   Compute z = (s - 1) / (s + 1)
 *
 *        (6)   Now use the approximation from HART
 *              page 111 to find log(s):
 *
 *              log(s) = z * ( P(z**2) / Q(z**2) )
 *
 *              Where:
 *
 *              P(z**2) =  SUM [ Pj * (z**(2*j)) ]
 *              over j = {0,1,2,3}
 *
 *              Q(z**2) =  SUM [ Qj * (z**(2*j)) ]
 *              over j = {0,1,2,3}
 *
 *              P0 =  -0.240139179559210509868484e2
 *              P1 =  0.30957292821537650062264e2
 *              P2 =  -0.96376909336868659324e1
 *              P3 =  0.4210873712179797145
 *              Q0 =  -0.120069589779605254717525e2
 *              Q1 =  0.19480966070088973051623e2
 *              Q2 =  -0.89111090279378312337e1
 *              Q3 =  1.0000
 *
 *              (coefficients from HART table #2705 pg 229)
 *
 *        (7)   Finally, compute log(x) from:
 *
 *              log(x) = (k * log(2)) - log(sqrt(2)) + log(s)
 */

#ifdef   NEED_LOG

static double log_pcoeffs[] = {
   -0.24013917955921050986e2,
    0.30957292821537650062e2,
   -0.96376909336868659324e1,
    0.4210873712179797145
} ;

static double log_qcoeffs[] = {
   -0.12006958977960525471e2,
    0.19480966070088973051e2,
   -0.89111090279378312337e1,
    1.0000
} ;

double
log(x)
double x ;
{
  auto int k ;
  auto double pqofz, s, z, zt2 ;
  auto double retval ;
 
  if (x == 0.0)
    {
      doerr("log", "SINGULARITY", EDOM) ;
      retval = -MAXDOUBLE ;
    }
  else if (x < 0.0)
    {
      doerr("log", "DOMAIN", EDOM) ;
      retval = -MAXDOUBLE ;
    }
  else
    {
      s = SQRT2 * frexp(x, &k) ;
      z = (s - 1.0) / (s + 1.0) ;
      zt2 = z * z ;
      pqofz = z * (poly(3, log_pcoeffs, zt2) / poly(3, log_qcoeffs, zt2)) ;
      x = k * LN2 ;
      x -= LNSQRT2 ;
      x += pqofz ;
      retval = x ;
    }
  return(retval) ;
}
#endif /*NEED_LOG*/


/*  FUNCTION
 *
 *      log10   double precision common log
 *
 *  DESCRIPTION
 *
 *      Returns double precision common log of double precision
 *      floating point argument.
 *
 *  USAGE
 *
 *      double log10(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      PDP-11 Fortran IV-plus users guide, Digital Equip. Corp.,
 *      1975, pp. B-3.
 *
 *  RESTRICTIONS
 *
 *      For precision information refer to documentation of the
 *      floating point library routines called.
 *
 *  INTERNALS
 *
 *      Computes log10(x) from:
 *
 *              log10(x) = log10(e) * log(x)
 */

#ifdef   NEED_LOG10
double
log10(x)
double x ;
{
  x = LOG10E * log(x) ;
  return(x) ;
}
#endif /*NEED_LOG10*/


/*  FUNCTION
 *
 *      mod   double precision modulo
 *
 *  DESCRIPTION
 *
 *      Returns double precision modulo of two double
 *      precision arguments.
 *
 *  USAGE
 *
 *      double mod(value, base)
 *      double value ;
 *      double base ;
 */

#ifdef NEED_COS || NEED_SIN

double mod(value, base)
double value ;
double base ;
{
  auto double intpart ;
 
  value /= base ;
  value = modf(value, &intpart) ;
  value *= base ;
  return(value) ;
}

#endif /* NEED_COS || NEED_SIN */


/*  FUNCTION
 *
 *      poly   double precision polynomial evaluation
 *
 *  DESCRIPTION
 *
 *      Evaluates a polynomial and returns double precision
 *      result.  Is passed a the order of the polynomial,
 *      a pointer to an array of double precision polynomial
 *      coefficients (in ascending order), and the independent
 *      variable.
 *
 *  USAGE
 *
 *      double poly(order, coeffs, x)
 *      int order ;
 *      double *coeffs ;
 *      double x ;
 *
 *  INTERNALS
 *
 *      Evalates the polynomial using recursion and the form:
 *
 *              P(x) = P0 + x(P1 + x(P2 +...x(Pn)))
 */

#ifdef NEED_ATAN || NEED_COS || NEED_LOG || NEED_SIN

double
poly(order, coeffs, x)
register int order ;
double *coeffs ;
double x ;
{
  auto double curr_coeff, rtn_value ;

  if (order <= 0) rtn_value = *coeffs ;
  else
    {
      curr_coeff = *coeffs ;
      coeffs++ ;
      rtn_value = curr_coeff + x * poly(--order, coeffs, x) ;
    }
  return(rtn_value) ;
}

#endif /* NEED_ATAN || NEED_COS || NEED_LOG || NEED_SIN */


/*  FUNCTION
 *
 *  sin  double precision sine
 *
 *  DESCRIPTION
 *
 *  Returns double precision sine of double precision
 *  floating point argument.
 *
 *  USAGE
 *
 *  double sin(x)
 *  double x ;
 *
 *  REFERENCES
 *
 *  Computer Approximations, J.F. Hart et al, John Wiley & Sons,
 *  1968, pp. 112-120.
 *
 *  RESTRICTIONS
 *
 *  The sin and cos routines are interactive in the sense that
 *  in the process of reducing the argument to the range -PI/4
 *  to PI/4, each may call the other.  Ultimately one or the
 *  other uses a polynomial approximation on the reduced
 *  argument.  The sin approximation has a maximum relative error
 *  of 10**(-17.59) and the cos approximation has a maximum
 *  relative error of 10**(-16.18).
 *
 *  These error bounds assume exact arithmetic
 *  in the polynomial evaluation.  Additional rounding and
 *  truncation errors may occur as the argument is reduced
 *  to the range over which the polynomial approximation
 *  is valid, and as the polynomial is evaluated using
 *  finite-precision arithmetic.
 *  
 *  INTERNALS
 *
 *  Computes sin(x) from:
 *
 *    (1)  Reduce argument x to range -PI to PI.
 *
 *    (2)  If x > PI/2 then call sin recursively
 *    using relation sin(x) = -sin(x - PI).
 *
 *    (3)  If x < -PI/2 then call sin recursively
 *    using relation sin(x) = -sin(x + PI).
 *
 *    (4)  If x > PI/4 then call cos using
 *    relation sin(x) = cos(PI/2 - x).
 *
 *    (5)  If x < -PI/4 then call cos using
 *    relation sin(x) = -cos(PI/2 + x).
 *
 *    (6)  If x is small enough that polynomial
 *    evaluation would cause underflow
 *    then return x, since sin(x)
 *    approaches x as x approaches zero.
 *
 *    (7)  By now x has been reduced to range
 *    -PI/4 to PI/4 and the approximation
 *    from HART pg. 118 can be used:
 *
 *    sin(x) = y * ( p(y) / q(y) )
 *    Where:
 *
 *    y    =  x * (4/PI)
 *
 *    p(y) =  SUM [ Pj * (y**(2*j)) ]
 *    over j = {0,1,2,3}
 *
 *    q(y) =  SUM [ Qj * (y**(2*j)) ]
 *    over j = {0,1,2,3}
 *
 *    P0   =  0.206643433369958582409167054e+7
 *    P1   =  -0.18160398797407332550219213e+6
 *    P2   =  0.359993069496361883172836e+4
 *    P3   =  -0.2010748329458861571949e+2
 *    Q0   =  0.263106591026476989637710307e+7
 *    Q1   =  0.3927024277464900030883986e+5
 *    Q2   =  0.27811919481083844087953e+3
 *    Q3   =  1.0000...
 *    (coefficients from HART table #3063 pg 234)
 *
 *
 *  **** NOTE ****    The range reduction relations used in
 *  this routine depend on the final approximation being valid
 *  over the negative argument range in addition to the positive
 *  argument range.  The particular approximation chosen from
 *  HART satisfies this requirement, although not explicitly
 *  stated in the text.  This may not be true of other
 *  approximations given in the reference.
 */

#ifdef   NEED_SIN

static double sin_pcoeffs[] = {
    0.20664343336995858240e7,
   -0.18160398797407332550e6,
    0.35999306949636188317e4,
   -0.20107483294588615719e2
} ;

static double sin_qcoeffs[] = {
    0.26310659102647698963e7,
    0.39270242774649000308e5,
    0.27811919481083844087e3,
    1.0
} ;

double
sin(x)
double x ;
{
  double y, yt2 ;
  auto double retval ;

  if (x < -PI || x > PI)
    {
      x = mod(x, TWOPI) ;
           if (x > PI)  x = x - TWOPI ;
      else if (x < -PI) x = x + TWOPI ;
    }
       if (x > HALFPI)    retval = -(sin(x - PI)) ;
  else if (x < -HALFPI)   retval = -(sin(x + PI)) ;
  else if (x > FOURTHPI)  retval = cos(HALFPI - x) ;
  else if (x < -FOURTHPI) retval = -(cos(HALFPI + x)) ;
  else if (x < X6_UNDERFLOWS && x > -X6_UNDERFLOWS) retval = x ;
  else
    {
      y = x / FOURTHPI ;
      yt2 = y * y ;
      retval = y * (poly(3, sin_pcoeffs, yt2) / poly(3, sin_qcoeffs, yt2)) ;
    }
  return(retval) ;
}
#endif /*NEED_SIN*/


/*  FUNCTION
 *
 *      sinh   double precision hyperbolic sine
 *
 *  DESCRIPTION
 *
 *      Returns double precision hyperbolic sine of double precision
 *      floating point number.
 *
 *  USAGE
 *
 *      double sinh(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran IV plus user's guide, Digital Equipment Corp. pp B-5
 *
 *  RESTRICTIONS
 *
 *      Inputs greater than ln(MAXDOUBLE) result in overflow.
 *      Inputs less than ln(MINDOUBLE) result in underflow.
 *
 *      For precision information refer to documentation of the
 *      floating point library routines called.
 *
 *  INTERNALS
 *
 *      Computes hyperbolic sine from:
 *
 *              sinh(x) = 0.5 * (exp(x) - 1.0/exp(x))
 */

#ifdef   NEED_SINH
double
sinh(x)
double x ;
{
  auto double retval ;

  if (x > LOGE_MAXDOUBLE)
    {
      doerr("sinh", "OVERFLOW", ERANGE) ;
      retval = MAXDOUBLE ;
    }
  else if (x < LOGE_MINDOUBLE)
    {
      doerr("sinh", "UNDERFLOW", ERANGE) ;
      retval = MINDOUBLE ;
    }
  else
    {
      x = exp(x) ;
      retval = 0.5 * (x - (1.0 / x)) ;
    }
  return(retval) ;
}
#endif /*NEED_SINH*/


/*  FUNCTION
 *
 *      sqrt   double precision square root
 *
 *  DESCRIPTION
 *
 *      Returns double precision square root of double precision
 *      floating point argument.
 *
 *  USAGE
 *
 *      double sqrt(x)
 *      double x ;
 *
 *  REFERENCES
 *
 *      Fortran IV-PLUS user's guide, Digital Equipment Corp. pp B-7.
 *
 *      Computer Approximations, J.F. Hart et al, John Wiley & Sons,
 *      1968, pp. 89-96.
 *
 *  RESTRICTIONS
 *
 *      The relative error is 10**(-30.1) after three applications
 *      of Heron's iteration for the square root.
 *
 *      However, this assumes exact arithmetic in the iterations
 *      and initial approximation.  Additional errors may occur
 *      due to truncation, rounding, or machine precision limits.
 *
 *  INTERNALS
 *
 *      Computes square root by:
 *
 *        (1)   Range reduction of argument to [0.5,1.0]
 *              by application of identity:
 *
 *              sqrt(x)  =  2**(k/2) * sqrt(x * 2**(-k))
 *
 *              k is the exponent when x is written as
 *              a mantissa times a power of 2 (m * 2**k).
 *              It is assumed that the mantissa is
 *              already normalized (0.5 =< m < 1.0).
 *
 *        (2)   An approximation to sqrt(m) is obtained
 *              from:
 *
 *              u = sqrt(m) = (P0 + P1*m) / (Q0 + Q1*m)
 *
 *              P0 = 0.594604482
 *              P1 = 2.54164041
 *              Q0 = 2.13725758
 *              Q1 = 1.0
 *
 *              (coefficients from HART table #350 pg 193)
 *
 *        (3)   Three applications of Heron's iteration are
 *              performed using:
 *
 *              y[n+1] = 0.5 * (y[n] + (m/y[n]))
 *
 *              where y[0] = u = approx sqrt(m)
 *
 *        (4)   If the value of k was odd then y is either
 *              multiplied by the square root of two or
 *              divided by the square root of two for k positive
 *              or negative respectively.  This rescales y
 *              by multiplying by 2**frac(k/2).
 *
 *        (5)   Finally, y is rescaled by int(k/2) which
 *              is equivalent to multiplication by 2**int(k/2).
 *
 *              The result of steps 4 and 5 is that the value
 *              of y between 0.5 and 1.0 has been rescaled by
 *              2**(k/2) which removes the original rescaling
 *              done prior to finding the mantissa square root.
 *
 *  NOTES
 *
 *      The Convergent Technologies compiler optimizes division
 *      by powers of two to "arithmetic shift right" instructions.
 *      This is ok for positive integers but gives different
 *      results than other C compilers for negative integers.
 *      For example, (-1)/2 is -1 on the CT box, 0 on every other
 *      machine I tried.
 */

#ifdef   NEED_SQRT

#define  ITERATIONS  3                        /* Number of iterations */
#define  P0          0.594604482              /* Approximation coeff  */
#define  P1          2.54164041               /* Approximation coeff  */
#define  Q0          2.13725758               /* Approximation coeff  */
#define  Q1          1.0                      /* Approximation coeff  */

double
sqrt(x)
double x ;
{
  register int bugfix, count, kmod2 ;
  auto int exponent, k ;
  auto double m, u, y ;
  auto double retval ;
  
  if (x == 0.0) retval = 0.0 ;
  else if (x < 0.0)
    {
      doerr("sqrt", "DOMAIN", EDOM) ;
      retval = 0.0 ;
    }
  else
    {
      m = frexp(x, &k) ;
      u = (P0 + (P1 * m)) / (Q0 + (Q1 * m)) ;
      for (count = 0, y = u; count < ITERATIONS; count++)
        y = 0.5 * (y + (m / y)) ;
           if ((kmod2 = (k % 2)) < 0) y /= SQRT2 ;
      else if (kmod2 > 0)             y *= SQRT2 ;
      bugfix = 2 ;
      retval = ldexp(y, k / bugfix) ;
    }
  return(retval) ;
}
#endif /*NEED_SQRT*/


/*  FUNCTION
 *
 *      tan   Double precision tangent
 *
 *  DESCRIPTION
 *
 *      Returns tangent of double precision floating point number.
 *
 *  USAGE
 *
 *      double tan(x)
 *      double x ;
 *
 *  INTERNALS
 *
 *      Computes the tangent from tan(x) = sin(x) / cos(x).
 *
 *      If cos(x) = 0 and sin(x) >= 0, then returns largest
 *      floating point number on host machine.
 *
 *      If cos(x) = 0 and sin(x) < 0, then returns smallest
 *      floating point number on host machine.
 *
 *  REFERENCES
 *
 *      Fortran IV plus user's guide, Digital Equipment Corp. pp. B-8
 */

#ifdef   NEED_TAN
double
tan(x)
double x ;
{
  double cosx, sinx ;
  auto double retval ;

  sinx = sin(x) ;
  cosx = cos(x) ;
  if (cosx == 0.0)
    {
      doerr("tan", "OVERFLOW", ERANGE) ;
      if (sinx >= 0.0) retval = MAXDOUBLE ;
      else             retval = -MAXDOUBLE ;
    }
  else retval = sinx / cosx ;
  return(retval) ;
}
#endif /*NEED_TAN*/


/*  FUNCTION
 *
 *  tanh   double precision hyperbolic tangent
 *
 *  DESCRIPTION
 *
 *  Returns double precision hyperbolic tangent of double precision
 *  floating point number.
 *
 *  USAGE
 *
 *  double tanh(x)
 *  double x ;
 *
 *  REFERENCES
 *
 *  Fortran IV plus user's guide, Digital Equipment Corp. pp B-5
 *
 *  RESTRICTIONS
 *
 *  For precision information refer to documentation of the
 *  floating point library routines called.
 *  
 *  INTERNALS
 *
 *  Computes hyperbolic tangent from:
 *
 *    tanh(x) = 1.0 for x > TANH_MAXARG
 *      = -1.0 for x < -TANH_MAXARG
 *      =  sinh(x) / cosh(x) otherwise
 */

#ifdef   NEED_TANH
double
tanh(x)
double x ;
{
  auto double retval ;
  register int positive ;

  if (x > TANH_MAXARG || x < -TANH_MAXARG)
    {
      if (x > 0.0) positive = 1 ;
      else positive = 0 ;
      doerr("tanh", "PLOSS", ERANGE) ;
      if (positive) retval = 1.0 ;
      else          retval = -1.0 ;
    }
  else retval = sinh(x) / cosh(x) ;
  return(retval) ;
}
#endif   /*NEED_TANH*/


/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * All recipients should regard themselves as participants in an ongoing
 * research project and hence should feel obligated to report their
 * experiences (good or bad) with these elementary function codes, using
 * the sendbug(8) program, to the authors.
 */

#ifdef   NEED_POW
/* POW(X,Y)
 * RETURN X**Y
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85;
 * REVISED BY K.C. NG on 7/10/85.
 *
 * Required system supported functions:
 *      scalb(x,n)
 *      logb(x)
 *      copysign(x,y)
 *      finite(x)
 *      drem(x,y)
 *
 * Required kernel functions:
 *      exp__E(a, c)    ...return  exp(a+c) - 1 - a*a/2
 *      log__L(x)       ...return  (log(1+x) - 2s)/s, s=x/(2+x)
 *      pow_p(x, y)     ...return  +(anything)**(finite non zero)
 *
 * Method
 *      1. Compute and return log(x) in three pieces:
 *              log(x) = n*ln2 + hi + lo,
 *         where n is an integer.
 *      2. Perform y * log(x) by simulating muti-precision arithmetic and
 *         return the answer in three pieces:
 *              y * log(x) = m * ln2 + hi + lo,
 *         where m is an integer.
 *      3. Return x ** y = exp(y * log(x))
 *              = 2^m * (exp(hi + lo)).
 *
 * Special cases:
 *      (anything) ** 0  is 1 ;
 *      (anything) ** 1  is itself;
 *      (anything) ** NaN is NaN;
 *      NaN ** (anything except 0) is NaN;
 *      +-(anything > 1) ** +INF is +INF;
 *      +-(anything > 1) ** -INF is +0;
 *      +-(anything < 1) ** +INF is +0;
 *      +-(anything < 1) ** -INF is +INF;
 *      +-1 ** +-INF is NaN and signal INVALID;
 *      +0 ** +(anything except 0, NaN)  is +0;
 *      -0 ** +(anything except 0, NaN, odd integer)  is +0;
 *      +0 ** -(anything except 0, NaN)  is +INF and signal DIV-BY-ZERO;
 *      -0 ** -(anything except 0, NaN, odd integer)  is +INF with signal;
 *      -0 ** (odd integer) = -( +0 ** (odd integer) );
 *      +INF ** +(anything except 0,NaN) is +INF;
 *      +INF ** -(anything except 0,NaN) is +0;
 *      -INF ** (odd integer) = -( +INF ** (odd integer) );
 *      -INF ** (even integer) = ( +INF ** (even integer) );
 *      -INF ** -(anything except integer,NaN) is NaN with signal;
 *      -(x=anything) ** (k=integer) is (-1)**k * (x ** k);
 *      -(anything except 0) ** (non-integer) is NaN with signal;
 *
 * Accuracy:
 *      pow(x, y) returns x**y nearly rounded. In particular, on a SUN, a VAX,
 *      and a Zilog Z8000,
 *                      pow(integer, integer)
 *      always returns the correct integer provided it is representable.
 *      In a test run with 100,000 random arguments with 0 < x, y < 20.0
 *      on a VAX, the maximum observed error was 1.79 ulps (units in the
 *      last place).
 *
 * Constants :
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if defined(vax) || defined(tahoe)        /* VAX D format */
#include <errno.h>
extern double infnan() ;
#ifdef vax
#define _0x(A,B)        0x/**/A/**/B
#else   /* vax */
#define _0x(A,B)        0x/**/B/**/A
#endif  /* vax */
/* static double */
/* ln2hi  =  6.9314718055829871446E-1    , Hex  2^  0   *  .B17217F7D00000 */
/* ln2lo  =  1.6465949582897081279E-12   , Hex  2^-39   *  .E7BCD5E4F1D9CC */
/* invln2 =  1.4426950408889634148E0     , Hex  2^  1   *  .B8AA3B295C17F1 */
/* sqrt2  =  1.4142135623730950622E0     ; Hex  2^  1   *  .B504F333F9DE65 */
static long     ln2hix[] = { _0x(7217,4031), _0x(0000,f7d0)} ;
static long     ln2lox[] = { _0x(bcd5,2ce7), _0x(d9cc,e4f1)} ;
static long    invln2x[] = { _0x(aa3b,40b8), _0x(17f1,295c)} ;
static long     sqrt2x[] = { _0x(04f3,40b5), _0x(de65,33f9)} ;
#define  ln2hi    (*(double*) ln2hix)
#define  ln2lo    (*(double*) ln2lox)
#define  invln2   (*(double*) invln2x)
#define  sqrt2    (*(double*) sqrt2x)
#else   /* defined(vax)||defined(tahoe) */
static double
ln2hi  =  6.9314718036912381649E-1    , /* Hex  2^ -1   *  1.62E42FEE00000 */
ln2lo  =  1.9082149292705877000E-10   , /* Hex  2^-33   *  1.A39EF35793C76 */
invln2 =  1.4426950408889633870E0     , /* Hex  2^  0   *  1.71547652B82FE */
sqrt2  =  1.4142135623730951455E0     ; /* Hex  2^  0   *  1.6A09E667F3BCD */
#endif  /* defined(vax) || defined(tahoe) */

static double zero = 0.0 ;

double
pow(x, y)
double x, y ;
{
  double drem(), pow_p(), copysign(), t ;
  int finite() ;

       if (y == 0.0) return(1.0) ;
#if !defined(vax) && !defined(tahoe)
  else if (y == 1.0 || x != x) return(x) ;   /* if x is NaN or y = 1 */
#else
  else if (y == 1.0) return(x) ;             /* if y = 1 */
#endif  /* !defined(vax) && !defined(tahoe) */

#if !defined(vax) && !defined(tahoe)
  else if (y != y) return(y) ;               /* if y is NaN */
#endif  /* !defined(vax) && !defined(tahoe) */
  else if (!finite(y))                       /* if y is INF */
         if ((t = copysign(x, 1.0)) == 1.0)
           return(0.0 / zero) ;
    else if (t > 1.0) return((y > 0.0) ? y : 0.0) ;
    else return((y < 0.0) ? -y : 0.0) ;
  else if (y == 2.0) return(x * x) ;
  else if (y == -1.0) return(1.0 / x) ;

/* sign(x) = 1 */

  else if (copysign(1.0, x) == 1.0) return(pow_p(x, y)) ;

/* sign(x)= -1 */
/* if y is an even integer */

  else if ((t = drem(y, 2.0)) == 0.0) return(pow_p(-x, y)) ;

/* if y is an odd integer */

  else if (copysign(t, 1.0) == 1.0) return(-pow_p(-x, y)) ;

/* Henceforth y is not an integer */

  else if (x == 0.0) return((y > 0.0) ? -x : 1.0 / (-x)) ;   /* x is -0 */
  else                                  /* return NaN */
    {
#if defined(vax) || defined(tahoe)
      return(infnan(EDOM)) ;            /* NaN */
#else   /* defined(vax) || defined(tahoe) */
      return(0.0 / zero) ;
#endif  /* defined(vax) || defined(tahoe) */
     }
}


/* pow_p(x,y) return x**y for x with sign = 1 and finite y */

static double
pow_p(x, y)
double x, y ;
{
  double logb(), scalb(), copysign(), log__L(), exp__E() ;
  double c, s, t, z, tx, ty ;

#ifdef tahoe
  double tahoe_tmp ;
#endif  /* tahoe */
  float sx, sy ;
  long k = 0 ;
  int n, m ;

  if (x == 0.0 || !finite(x))             /* if x is +INF or +0 */
    {

#if defined(vax) || defined(tahoe)
      return((y > 0.0) ? x : infnan(ERANGE)) ;  /* if y < 0.0, return +INF */
#else   /* defined(vax) || defined(tahoe) */
      return((y > 0.0) ? x : 1.0 / x) ;
#endif  /* defined(vax) || defined(tahoe) */
     }
  if (x == 1.0) return(x) ;     /* if x = 1.0, return 1 since y is finite */

/* reduce x to z in [sqrt(1/2)-1, sqrt(2)-1] */

    z = scalb(x, -(n = logb(x))) ;

#if !defined(vax) && !defined(tahoe)      /* IEEE double; subnormal number */
  if (n <= -1022)
    {
      n += (m = logb(z)) ;
      z = scalb(z, -m) ;
    }
#endif  /* !defined(vax) && !defined(tahoe) */

  if (z >= sqrt2)
    {
      n += 1 ;
      z *= 0.5 ;
    }
  z -= 1.0 ;

/* log(x) = nlog2 + log(1 + z) ~ nlog2 + t + tx */

  s = z / (2.0 + z) ;
  c = z * z * 0.5 ;
  tx = s * (c + log__L(s * s)) ;
  t = z - (c - tx) ;
  tx += (z - t) - c ;

/* if y * log(x) is neither too big nor too small */

  if ((s = logb(y) + logb(n + t)) < 12.0)
    if (s > -60.0)
      {

/* compute y * log(x) ~ mlog2 + t + c */

        s = y * (n + invln2 * t) ;
        m = s + copysign(0.5, s) ;      /* m := nint(y * log(x)) */
        k = y ;
        if ((double) k == y)            /* if y is an integer */
          {
            k = m - k * n ;
            sx = t ;
            tx += (t - sx) ;
          }
        else                            /* if y is not an integer */
          {
            k = m ;
            tx += n * ln2lo ;
            sx = (c = n * ln2hi) + t ;
            tx += (c - sx) + t ;
          }

/* end of checking whether k == y */

        sy = y ;
        ty = y - sy ;                   /* y ~ sy + ty */

#ifdef tahoe
        s = (tahoe_tmp = sx) * sy - k * ln2hi ;
#else   /* tahoe */
        s = (double) sx * sy - k * ln2hi ;  /* (sy + ty) * (sx + tx) - kln2 */
#endif  /* tahoe */

        z = (tx * ty - k * ln2lo) ;
        tx = tx * sy ; ty = sx * ty ;
        t = ty + z ; t += tx ; t += s ;
        c = -((((t-s)-tx)-ty)-z) ;

/* return exp(y * log(x)) */

        t += exp__E(t,c) ;
        return(scalb(1.0 + t, m)) ;
      }

/* end of if log(y * log(x)) > -60.0 */

  else                  /* exp(+- tiny) = 1 with inexact flag */
    {
      ln2hi + ln2lo ;
      return(1.0) ;
    }
  else if (copysign(1.0, y) * (n + invln2 * t) < 0.0)
    return(scalb(1.0, -5000)) ;     /* exp(-(big#)) underflows to zero */
  else
    return(scalb(1.0, 5000)) ;      /* exp(+(big#)) overflows to INF */
}


/* Some IEEE standard 754 recommended functions and remainder and sqrt for
 * supporting the C elementary functions.
 ******************************************************************************
 * WARNING:
 *      These codes are developed (in double) to support the C elementary
 * functions temporarily. They are not universal, and some of them are very
 * slow (in particular, drem and sqrt is extremely inefficient). Each
 * computer system should have its implementation of these functions using
 * its own assembler.
 ******************************************************************************
 *
 * IEEE 754 required operations:
 *     drem(x, p)
 *              returns  x REM y  =  x - [x/y]*y , where [x/y] is the integer
 *              nearest x/y; in half way case, choose the even one.
 *     sqrt(x)
 *              returns the square root of x correctly rounded according to
 *              the rounding mod.
 *
 * IEEE 754 recommended functions:
 * (a) copysign(x, y)
 *              returns x with the sign of y.
 * (b) scalb(x, N)
 *              returns  x * (2 ** N), for integer values N.
 * (c) logb(x)
 *              returns the unbiased exponent of x, a signed integer in
 *              double precision, except that logb(0) is -INF, logb(INF)
 *              is +INF, and logb(NAN) is that NAN.
 * (d) finite(x)
 *              returns the value TRUE if -INF < x < +INF and returns
 *              FALSE otherwise.
 *
 * CODED IN C BY K.C. NG, 11/25/84;
 * REVISED BY K.C. NG on 1/22/85, 2/13/85, 3/24/85.
 */

#if      defined(vax) || defined(tahoe)      /* VAX D format */
  static unsigned short msign = 0x7fff, mexp = 0x7f80 ;
  static short  prep1 = 57, gap = 7, bias = 129 ;
  static double novf = 1.7E38, nunf = 3.0E-39 ;
#else /* defined(vax) || defined(tahoe) */
  static unsigned short msign = 0x7fff, mexp = 0x7ff0 ;
  static short prep1 = 54, gap = 4, bias = 1023 ;
  static double novf = 1.7E308, nunf = 3.0E-308 ;
#endif  /* defined(vax) || defined(tahoe) */

double
scalb(x, N)
double x ;
int N ;
{
  int k ;
  double scalb() ;

#ifdef national
  unsigned short *px = (unsigned short *) &x + 3 ;
#else   /* national */
  unsigned short *px = (unsigned short *) &x ;
#endif  /* national */

  if (x == 0.0) return(x) ;

#if defined(vax) || defined(tahoe)

  if ((k = *px & mexp) != ~msign)
    {
      if (N < -260) return(nunf * nunf) ;
      else if (N > 260)
        {
          extern double infnan(), copysign() ;
          return(copysign(infnan(ERANGE), x)) ;
        }
#else  /* defined(vax) || defined(tahoe) */

  if ((k = *px & mexp) != mexp)
    {
           if (N < -2100) return(nunf * nunf) ;
      else if (N > 2100) return(novf + novf) ;
      if (k == 0)
        {
          x *= scalb(1.0, (int) prep1) ;
          N -= prep1 ;
          return(scalb(x, N)) ;
        }
#endif  /* defined(vax) || defined(tahoe) */

      if ((k = (k >> gap) + N) > 0)
        if (k < (mexp >> gap)) *px = (*px & ~mexp) | (k << gap) ;
        else x = novf + novf ;               /* overflow */
      else
        if (k > -prep1)
          {                                  /* gradual underflow */
            *px = (*px & ~mexp) | (short) (1 << gap) ;
            x *= scalb(1.0, k-1) ;
          }
        else return(nunf * nunf) ;
    }
  return(x) ;
}


double
copysign(x, y)
double x, y ;
{
#ifdef   national
  unsigned short *px = (unsigned short *) &x + 3,
                 *py = (unsigned short *) &y + 3 ;
#else /* national */
  unsigned short *px = (unsigned short *) &x,
                 *py = (unsigned short *) &y ;
#endif  /* national */

#if defined(vax) || defined(tahoe)
  if ((*px & mexp) == 0) return(x) ;
#endif  /* defined(vax) || defined(tahoe) */

  *px = (*px & msign) | (*py & ~msign) ;
  return(x) ;
}


double
logb(x)
double x ;
{
#ifdef national
  short *px = (short *) &x + 3, k ;
#else   /* national */
  short *px = (short *) &x, k ;
#endif  /* national */

#if defined(vax) || defined(tahoe)
  return (int) (((*px & mexp) >> gap) - bias) ;
#else   /* defined(vax) || defined(tahoe) */

  if ((k = *px & mexp) != mexp)
         if (k != 0)   return((k >> gap) - bias) ;
    else if (x != 0.0) return(-1022.0) ;
    else               return(-(1.0 / zero)) ;
  else if (x != x) return(x) ;
  else
    {
      *px &= msign ;
      return(x) ;
    }
#endif  /* defined(vax) || defined(tahoe) */
}


finite(x)
double x ;
{
#if defined(vax) || defined(tahoe)
  return(1) ;
#else   /* defined(vax) || defined(tahoe) */
#ifdef national
  return((*((short *) &x + 3) & mexp) != mexp) ;
#else   /* national */
  return((*((short *) &x) & mexp) != mexp) ;
#endif  /* national */
#endif  /* defined(vax) || defined(tahoe) */
}


double
drem(x, p)
double x, p ;
{
  short sign ;
  double hp, dp, tmp, drem(), scalb() ;
  unsigned short k ;

#ifdef national
  unsigned short *px = (unsigned short *) &x   + 3,
                 *pp = (unsigned short *) &p   + 3,
                 *pd = (unsigned short *) &dp  + 3,
                 *pt = (unsigned short *) &tmp + 3 ;
#else   /* national */
  unsigned short *px = (unsigned short *) &x,
                 *pp = (unsigned short *) &p  ,
                 *pd = (unsigned short *) &dp ,
                 *pt = (unsigned short *) &tmp ;
#endif  /* national */

  *pp &= msign ;

#if defined(vax) || defined(tahoe)
  if ((*px & mexp) == ~msign)  /* is x a reserved operand? */
#else   /* defined(vax) || defined(tahoe) */
  if ((*px & mexp) == mexp)
#endif  /* defined(vax) || defined(tahoe) */
    return (x-p) - (x-p) ;     /* create nan if x is inf */

  if (p == 0.0)
    {
      doerr("drem", "SINGULARITY", EDOM) ;
#if defined(vax) || defined(tahoe)
      extern double infnan() ;
      return(infnan(EDOM)) ;
#else   /* defined(vax) || defined(tahoe) */
      return(0.0 / zero) ;
#endif  /* defined(vax) || defined(tahoe) */
    }

#if defined(vax) || defined(tahoe)
  if ((*pp & mexp) == ~msign)  /* is p a reserved operand? */
#else   /* defined(vax) || defined(tahoe) */
  if ((*pp & mexp) == mexp)
#endif  /* defined(vax)||defined(tahoe) */
    {
      if (p != p) return p ;
      else return x ;
    }
  else if (((*pp & mexp) >> gap) <= 1)

/* subnormal p, or almost subnormal p */

    {
      double b ;
      b = scalb(1.0, (int) prep1) ;
      p *= b ;
      x = drem(x, p) ;
      x *= b ;
      return(drem(x, p) / b) ;
    }
  else if (p >= novf / 2)
    {
      p /= 2 ;
      x /= 2 ;
      return(drem(x, p) * 2) ;
    }
  else
    {
      dp = p + p ;
      hp = p / 2 ;
      sign = *px & ~msign ;
      *px &= msign ;
      while (x > dp)
        {
          k = (*px & mexp) - (*pd & mexp) ;
          tmp = dp ;
          *pt += k ;

#if defined(vax) || defined(tahoe)
          if (x < tmp) *pt -= 128 ;
#else /* defined(vax) || defined(tahoe) */
          if (x < tmp) *pt -= 16 ;
#endif  /* defined(vax) || defined(tahoe) */

            x -= tmp ;
        }
      if (x > hp)
        {
          x -= p ;
          if (x >= hp) x -= p ;
        }

#if defined(vax) || defined(tahoe)
      if (x)
#endif  /* defined(vax) || defined(tahoe) */
        *px ^= sign ;
      return(x) ;
    }
}


/* exp__E(x,c)
 * ASSUMPTION: c << x  SO THAT  fl(x+c)=x.
 * (c is the correction term for x)
 * exp__E RETURNS
 *
 *                       /  exp(x+c) - 1 - x ,  1E-19 < |x| < .3465736
 *       exp__E(x,c) =  |
 *                       \  0 ,  |x| < 1E-19.
 *
 * DOUBLE PRECISION (IEEE 53 bits, VAX D FORMAT 56 BITS)
 * KERNEL FUNCTION OF EXP, EXPM1, POW FUNCTIONS
 * CODED IN C BY K.C. NG, 1/31/85;
 * REVISED BY K.C. NG on 3/16/85, 4/16/85.
 *
 * Required system supported function:
 *      copysign(x,y)
 *
 * Method:
 *      1. Rational approximation. Let r=x+c.
 *         Based on
 *                                   2 * sinh(r/2)
 *                exp(r) - 1 =   ----------------------   ,
 *                               cosh(r/2) - sinh(r/2)
 *         exp__E(r) is computed using
 *                   x*x            (x/2)*W - ( Q - ( 2*P  + x*P ) )
 *                   --- + (c + x*[---------------------------------- + c ])
 *                    2                          1 - W
 *         where  P := p1*x^2 + p2*x^4,
 *                Q := q1*x^2 + q2*x^4 (for 56 bits precision, add q3*x^6)
 *                W := x/2-(Q-x*P),
 *
 *         (See the listing below for the values of p1,p2,q1,q2,q3. The poly-
 *          nomials P and Q may be regarded as the approximations to sinh
 *          and cosh :
 *              sinh(r/2) =  r/2 + r * P  ,  cosh(r/2) =  1 + Q . )
 *
 *         The coefficients were obtained by a special Remez algorithm.
 *
 * Approximation error:
 *
 *   |  exp(x) - 1                         |        2**(-57),  (IEEE double)
 *   | ------------  -  (exp__E(x,0)+x)/x  |  <=
 *   |       x                             |        2**(-69).  (VAX D)
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if defined(vax) || defined(tahoe)        /* VAX D format */
#ifdef vax
#define _0x(A,B)        0x/**/A/**/B
#else   /* vax */
#define _0x(A,B)        0x/**/B/**/A
#endif  /* vax */

/* static double */
/* p1     =  1.5150724356786683059E-2    , Hex  2^ -6   *  .F83ABE67E1066A */
/* p2     =  6.3112487873718332688E-5    , Hex  2^-13   *  .845B4248CD0173 */
/* q1     =  1.1363478204690669916E-1    , Hex  2^ -3   *  .E8B95A44A2EC45 */
/* q2     =  1.2624568129896839182E-3    , Hex  2^ -9   *  .A5790572E4F5E7 */
/* q3     =  1.5021856115869022674E-6    ; Hex  2^-19   *  .C99EB4604AC395 */

static long        p1x[] = { _0x(3abe,3d78), _0x(066a,67e1) } ;
static long        p2x[] = { _0x(5b42,3984), _0x(0173,48cd) } ;
static long        q1x[] = { _0x(b95a,3ee8), _0x(ec45,44a2) } ;
static long        q2x[] = { _0x(7905,3ba5), _0x(f5e7,72e4) } ;
static long        q3x[] = { _0x(9eb4,36c9), _0x(c395,604a) } ;
#define  p1  (*(double*) p1x)
#define  p2  (*(double*) p2x)
#define  q1  (*(double*) q1x)
#define  q2  (*(double*) q2x)
#define  q3  (*(double*) q3x)
#else   /* defined(vax) || defined(tahoe) */

static double
p1 = 1.3887401997267371720E-2,     /* Hex  2^ -7   *  1.C70FF8B3CC2CF */
p2 = 3.3044019718331897649E-5,     /* Hex  2^-15   *  1.15317DF4526C4 */
q1 = 1.1110813732786649355E-1,     /* Hex  2^ -4   *  1.C719538248597 */
q2 = 9.9176615021572857300E-4 ;    /* Hex  2^-10   *  1.03FC4CB8C98E8 */
#endif  /* defined(vax) || defined(tahoe) */

double
exp__E(x, c)
double x, c ;
{
  static double small = 1.0E-19 ;
  double copysign(), z, p, q, xp, xh, w ;

  if (copysign(x, 1.0) > small)
     {
       z = x * x ;
       p = z * (p1 + z * p2) ;

#if defined(vax) || defined(tahoe)
       q = z * (q1 + z * (q2 + z * q3)) ;
#else   /* defined(vax) || defined(tahoe) */
       q = z * (q1 + z * q2) ;
#endif  /* defined(vax) || defined(tahoe) */
       xp = x * p ;
       xh = x * 0.5 ;
       w = xh - (q - xp) ;
       p = p + p ;
       c += x * ((xh * w - (q - (p + xp))) / (1.0 - w) + c) ;
       return(z * 0.5 + c) ;
     }

/* end of |x| > small */

   else
     {
       if (x != 0.0) 1.0 + small ;      /* raise the inexact flag */
       return(copysign(0.0, x)) ;
     }
}


/* log__L(Z)
 *              LOG(1+X) - 2S                          X
 * RETURN      ---------------  WHERE Z = S*S,  S = ------- , 0 <= Z <= .0294... *                    S                              2 + X
 *
 * DOUBLE PRECISION (VAX D FORMAT 56 bits or IEEE DOUBLE 53 BITS)
 * KERNEL FUNCTION FOR LOG; TO BE USED IN LOG1P, LOG, AND POW FUNCTIONS
 * CODED IN C BY K.C. NG, 1/19/85;
 * REVISED BY K.C. Ng, 2/3/85, 4/16/85.
 *
 * Method :
 *      1. Polynomial approximation: let s = x/(2+x).
 *         Based on log(1+x) = log(1+s) - log(1-s)
 *               = 2s + 2/3 s**3 + 2/5 s**5 + .....,
 *
 *         (log(1+x) - 2s)/s is computed by
 *
 *             z*(L1 + z*(L2 + z*(... (L7 + z*L8)...)))
 *
 *         where z=s*s. (See the listing below for Lk's values.) The
 *         coefficients are obtained by a special Remez algorithm.
 *
 * Accuracy:
 *      Assuming no rounding error, the maximum magnitude of the approximation
 *      error (absolute) is 2**(-58.49) for IEEE double, and 2**(-63.63)
 *      for VAX D format.
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if defined(vax) || defined(tahoe)        /* VAX D format (56 bits) */
#ifdef vax
#define _0x(A,B)        0x/**/A/**/B
#else   /* vax */
#define _0x(A,B)        0x/**/B/**/A
#endif  /* vax */

/* static double */
/* L1     =  6.6666666666666703212E-1    , Hex  2^  0   *  .AAAAAAAAAAAAC5 */
/* L2     =  3.9999999999970461961E-1    , Hex  2^ -1   *  .CCCCCCCCCC2684 */
/* L3     =  2.8571428579395698188E-1    , Hex  2^ -1   *  .92492492F85782 */
/* L4     =  2.2222221233634724402E-1    , Hex  2^ -2   *  .E38E3839B7AF2C */
/* L5     =  1.8181879517064680057E-1    , Hex  2^ -2   *  .BA2EB4CC39655E */
/* L6     =  1.5382888777946145467E-1    , Hex  2^ -2   *  .9D8551E8C5781D */
/* L7     =  1.3338356561139403517E-1    , Hex  2^ -2   *  .8895B3907FCD92 */
/* L8     =  1.2500000000000000000E-1    , Hex  2^ -2   *  .80000000000000 */

static long L1x[] = { _0x(aaaa,402a), _0x(aac5,aaaa) } ;
static long L2x[] = { _0x(cccc,3fcc), _0x(2684,cccc) } ;
static long L3x[] = { _0x(4924,3f92), _0x(5782,92f8) } ;
static long L4x[] = { _0x(8e38,3f63), _0x(af2c,39b7) } ;
static long L5x[] = { _0x(2eb4,3f3a), _0x(655e,cc39) } ;
static long L6x[] = { _0x(8551,3f1d), _0x(781d,e8c5) } ;
static long L7x[] = { _0x(95b3,3f08), _0x(cd92,907f) } ;
static long L8x[] = { _0x(0000,3f00), _0x(0000,0000) } ;

#define  L1  (*(double*) L1x)
#define  L2  (*(double*) L2x)
#define  L3  (*(double*) L3x)
#define  L4  (*(double*) L4x)
#define  L5  (*(double*) L5x)
#define  L6  (*(double*) L6x)
#define  L7  (*(double*) L7x)
#define  L8  (*(double*) L8x)
#else   /* defined(vax) || defined(tahoe) */

static double
L1 =  6.6666666666667340202E-1,       /* Hex  2^ -1   *  1.5555555555592 */
L2 =  3.9999999999416702146E-1,       /* Hex  2^ -2   *  1.999999997FF24 */
L3 =  2.8571428742008753154E-1,       /* Hex  2^ -2   *  1.24924941E07B4 */
L4 =  2.2222198607186277597E-1,       /* Hex  2^ -3   *  1.C71C52150BEA6 */
L5 =  1.8183562745289935658E-1,       /* Hex  2^ -3   *  1.74663CC94342F */
L6 =  1.5314087275331442206E-1,       /* Hex  2^ -3   *  1.39A1EC014045B */
L7 =  1.4795612545334174692E-1 ;      /* Hex  2^ -3   *  1.2F039F0085122 */
#endif  /* defined(vax) || defined(tahoe) */

double
log__L(z)
double z ;
{
#if defined(vax) || defined(tahoe)
  return(z * (L1 + z * (L2 + z * (L3 + z * (L4 + z *
             (L5 + z * (L6 + z * (L7 + z * L8)))))))) ;
#else   /* defined(vax) || defined(tahoe) */
  return(z * (L1 + z * (L2 + z * (L3 + z * (L4 + z *
             (L5 + z * (L6 + z * L7))))))) ;
#endif  /* defined(vax) || defined(tahoe) */
}
#endif /*NEED_POW*/


/*  Error detecting addition, subtraction, multiplication and division routines.
 *
 *  Routines supplied by Sisira Jayasinghe, Structural Dynamics Research Corp.
 *  2000 Eastman Dr. Milford, OH 45150 USA <spsisira@sdrc.UUCP>
 */

double
addition(x, y)
double x, y ;
{
  if (y > (HUGE - x)) doerr("add", "OVERFLOW", ERANGE) ;
  else
    {
      x += y ;
      return(x) ;
    }
  return(0.0) ;
}


double
subtraction(x, y)
double x, y ;
{
  if (y > (HUGE - x)) doerr("sub", "OVERFLOW", ERANGE) ;
  else
    {
      x -= y ;
      return(x) ;
    }
  return(0.0) ;
}


double
multiply(x, y)
double x, y ;
{
  double a, b ;

  if (y == 0.0) return(0.0) ;
  else
    {
      a = log(x) ;
      b = log(y) ;
      if ((a + b) >= log(HUGE)) doerr("mult", "OVERFLOW", ERANGE) ;
      else
        {
          x *= y ;
          return(x) ;
        }
    }
  return(0.0) ;
}


double
division(x, y)
double x, y ;
{
  double a, b ;

  if (y == 0.0) doerr("div", "OVERFLOW", ERANGE) ;
  else
    {
      a = log(x) ;
      b = log(y) ;
      if ((a - b) >= log(HUGE)) doerr("div", "OVERFLOW", ERANGE) ;
      else
        {
          x /= y ;
          return(x) ;
        }
    }   
  return(0.0) ;
}
