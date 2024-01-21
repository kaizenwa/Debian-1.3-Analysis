
/*  @(#)mathlib.h 1.2 90/02/02
 *
 *  Definitions used with the portable math library.
 *
 *  This is being done because libm.a doesn't appear to be as portable
 *  as originally assumed.
 *
 *  These routines are taken from two sources:
 *
 *  1/ Fred Fishs' portable maths library which was posted to the
 *     comp.sources.unix newsgroup on April 1987.
 *
 *     acos, acosh, asin, asinh, atan, atanh, cos, cosh, dabss,
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

/*      This file gets included with all of the floating point math
 *      library routines when they are compiled.  Note that
 *      this is the proper place to put machine dependencies
 *      whenever possible.
 *
 *      It should be pointed out that for simplicity's sake, the
 *      environment parameters are defined as floating point constants,
 *      rather than octal or hexadecimal initializations of allocated
 *      storage areas.  This means that the range of allowed numbers
 *      may not exactly match the hardware's capabilities.  For example,
 *      if the maximum positive double precision floating point number
 *      is EXACTLY 1.11...E100 and the constant "MAXDOUBLE is
 *      defined to be 1.11E100 then the numbers between 1.11E100 and
 *      1.11...E100 are considered to be undefined.  For most
 *      applications, this will cause no problems.
 *
 *      An alternate method is to allocate a global static "double" variable,
 *      say "maxdouble", and use a union declaration and initialization
 *      to initialize it with the proper bits for the EXACT maximum value.
 *      This was not done because the only compilers available to the
 *      author did not fully support union initialization features.
 */

extern double acos(), acosh(), asin(), asinh(), atan(), atanh() ;
extern double cos(), cosh(), exp(), fabs(), log(), log10(), pow() ;
extern double sin(), sinh(), sqrt(), tan(), tanh() ;

/*START============start of definitions from <values.h>============START
 *
 *  If your system has a /usr/include/values.h, or has another include
 *  file which defines:
 *      MAXDOUBLE       =>      Maximum double precision number
 *      MINDOUBLE       =>      Minimum double precision number
 *      DMAXEXP         =>      Maximum exponent of a double
 *      DMINEXP         =>      Minimum exponent of a double
 *
 *  you can comment out these definitions down to the END line below.
 */

#ifndef  BITSPERBYTE
/* These values work with any binary representation of integers
 * where the high-order bit contains the sign. */

/* a number used normally for size of a shift */
#if gcos
#define BITSPERBYTE     9
#else
#define BITSPERBYTE     8
#endif
#define BITS(type)      (BITSPERBYTE * (int)sizeof(type))

/*  Various values that describe the binary floating-point representation
 *  MAXDOUBLE    - the largest double
 *                       ((_EXPBASE ** DMAXEXP) * (1 - (_EXPBASE ** -DSIGNIF)))
 *  MINDOUBLE    - the smallest double (_EXPBASE ** (DMINEXP - 1))
 *  DMAXEXP      - the maximum exponent of a double (as returned by frexp())
 *  DMINEXP      - the minimum exponent of a double (as returned by frexp())
 *  DSIGNIF      - the number of significant bits in a double
 *  _IEEE        - 1 if IEEE standard representation is used
 *  _DEXPLEN     - the number of bits for the exponent of a double
 *  _HIDDENBIT   - 1 if high-significance bit of mantissa is implicit
 */

#if u3b || u3b5 || sun
#define MAXDOUBLE       1.79769313486231470e+308
#define MINDOUBLE       4.94065645841246544e-324
#define _IEEE           1
#define _DEXPLEN        11
#define _HIDDENBIT      1
#define DMINEXP (-(DMAXEXP + DSIGNIF - _HIDDENBIT - 3))
#endif

#if pdp11 || vax
#define MAXDOUBLE       1.701411834604692293e+38
#define MINDOUBLE       (0.01 * 2.938735877055718770e-37)
#define _IEEE           0
#define _DEXPLEN        8
#define _HIDDENBIT      1
#define DMINEXP (-DMAXEXP)
#endif

#if gcos
#define MAXDOUBLE       1.7014118346046923171e+38
#define MINDOUBLE       2.9387358770557187699e-39
#define _IEEE           0
#define _DEXPLEN        8
#define _HIDDENBIT      0
#define DMINEXP (-(DMAXEXP + 1))
#endif

#define DSIGNIF (BITS(double) - _DEXPLEN + _HIDDENBIT - 1)
#define DMAXEXP ((1 << _DEXPLEN - 1) - 1 + _IEEE)

#endif /*BITSPERBYTE*/

/*END==============end of definitions from <values.h>==================END*/


#define  LOG2_MAXDOUBLE  (DMAXEXP + 1)
#define  LOG2_MINDOUBLE  (DMINEXP - 1)
#define  LOGE_MAXDOUBLE  (LOG2_MAXDOUBLE / LOG2E)
#define  LOGE_MINDOUBLE  (LOG2_MINDOUBLE / LOG2E)

/*
 *  The following are hacks which should be fixed when I understand all
 *  the issues a little better.   |tanh(TANH_MAXARG)| = 1.0
 */

#define  TANH_MAXARG     16
#define  SQRT_MAXDOUBLE  1.304380e19

#define  TWOPI           (2.0 * PI)
#define  HALFPI          (PI / 2.0)
#define  FOURTHPI        (PI / 4.0)
#define  SIXTHPI         (PI / 6.0)
#define  LOG2E           1.4426950408889634074   /* Log to base 2 of e */
#define  LOG10E          0.4342944819032518276
#define  SQRT2           1.4142135623730950488
#define  SQRT3           1.7320508075688772935
#define  LN2             0.6931471805599453094
#define  LNSQRT2         0.3465735902799726547

/*      MC68000 HARDWARE DEPENDENCIES
 *
 *      cc -DIEEE       =>      uses IEEE floating point format
 *
 *      Apologies for the double negative. I want as few definitions
 *      needed in the default case as possible.
 */

#ifndef  NOIEEE
#define  X6_UNDERFLOWS   (4.209340e-52)   /* X**6 almost underflows */
#define  X16_UNDERFLOWS  (5.421010e-20)   /* X**16 almost underflows */
#endif /*NOIEEE*/

/*  It is hoped that your system supplies all the mathematical functions
 *  required by calctool. If not then, it is possible to use the needed
 *  ones from the portable math library routines that comes with these
 *  sources.
 *
 *  There is one definition for each routine used by calctool. These are
 *  commented out by default to signify that this system has that routine.
 *  If you are missing any, then uncomment the appropriate definitions.
 */

/*#define  NEED_ACOS  */
#define  NEED_ACOSH
/*#define  NEED_ASIN  */
#define  NEED_ASINH
/*#define  NEED_ATAN  */
#define  NEED_ATANH
/*#define  NEED_COS   */
/*#define  NEED_COSH  */
/*#define  NEED_EXP   */
/*#define  NEED_LOG   */
/*#define  NEED_LOG10 */
/*#define  NEED_POW   */
/*#define  NEED_SIN   */
/*#define  NEED_SINH  */
/*#define  NEED_SQRT  */
/*#define  NEED_TAN   */
/*#define  NEED_TANH  */
