/* sin, cos, etc, for S-Lang */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */



#include "config.h"

#include <math.h>
#ifndef FLOAT_TYPE 
#define FLOAT_TYPE 5
#endif

#include "slang.h"
#include "_slang.h"

#ifndef HAVE_STDLIB_H
  extern double atof ();
#endif

#ifndef pc_system
#include <signal.h>
#include <errno.h>

#define SIGNAL  SLsignal

static void math_floating_point_exception (int sig)
{
   sig = errno;
   SLang_Error = INTRINSIC_ERROR;
   (void) SIGNAL (SIGFPE, math_floating_point_exception);
   errno = sig;
}
#endif



#if defined(_MSC_VER) && defined(_MT)
#define DMATH1(_x) dmath1((double (_pascal *)(double))(_x))
static float64 dmath1(double (_pascal *f) (double))
#else
#define DMATH1(_x) dmath1((double (*)(double))(_x))
static float64 dmath1(double (*f) (double))
#endif
{
   float64 x; 
   int dum1, dum2;
   if (SLang_pop_float(&x, &dum1, &dum2)) return(0.0);
   
   return (float64) (*f)((double) x);
}

#if defined(_MSC_VER) && defined(_MT)
#define DMATH2(_x) dmath2((double (_pascal *)(double, double))(_x))
static float64 dmath2(double (_pascal *f) (double, double))
#else
#define DMATH2(_x) dmath2((double (*)(double, double))(_x))
static float64 dmath2(double (*f) (double, double))
#endif
{
   float64 x, y; 
   int dum1, dum2;
   if (SLang_pop_float(&y, &dum1, &dum2)
       || SLang_pop_float(&x, &dum1, &dum2)) return (0.0);

   return (float64) (*f)((double) x, (double) y);
}


static float64 math_cos		(void)	{ return DMATH1(cos); }
static float64 math_sin		(void)	{ return DMATH1(sin); }
static float64 math_tan		(void)	{ return DMATH1(tan); }
static float64 math_atan	(void)	{ return DMATH1(atan); }
static float64 math_acos	(void)	{ return DMATH1(acos); }
static float64 math_asin	(void)	{ return DMATH1(asin); }
static float64 math_exp		(void)	{ return DMATH1(exp); }
static float64 math_log		(void)	{ return DMATH1(log); }
static float64 math_sqrt	(void)	{ return DMATH1(sqrt); }
static float64 math_log10	(void)	{ return DMATH1(log10); }
static float64 math_pow		(void)	{ return DMATH2(pow); }

/* usage here is a1 a2 ... an n x ==> a1x^n + a2 x ^(n - 1) + ... + an */
static float64 math_poly (void)
{
   int n;
   int dum1, dum2;
   double xn = 1.0, sum = 0.0;
   float64 an, x;
   
   if ((SLang_pop_float(&x, &dum1, &dum2))
       || (SLang_pop_integer(&n))) return(0.0);
   
   while (n-- > 0)
     {
	if (SLang_pop_float(&an, &dum1, &dum2)) break;
	(void) dum1; (void) dum2;
	sum += an * xn;
	xn = xn * x;
     }
   return (float64) sum;
}

static float64 Const_E =  2.718281828459045;
static float64 Const_Pi = 3.141592653589793;

static float64 slmath_do_float (void)
{
   float64 f = 0.0;
   unsigned char stype;
   SLang_Object_Type obj;
   

   if (SLang_pop(&obj)) return(f);

   stype = obj.sub_type;
   if (stype == INT_TYPE)
     {
	f = (float64) obj.v.i_val;
     }
   else if (stype == FLOAT_TYPE)
     {
	f = obj.v.f_val;
     }
   else if (stype == STRING_TYPE)
     {
	/* Should check for parse error here but later. */
	f = atof(obj.v.s_val);
	if (obj.main_type == SLANG_DATA) SLFREE(obj.v.s_val);
     }
   else SLang_Error = TYPE_MISMATCH;
   return f;
}

static SLang_Name_Type slmath_table[] =
{
   MAKE_INTRINSIC(".polynom", math_poly, FLOAT_TYPE, 0),
   /* Prototype: Float polynom (a, b, ..., c, Integer n, Float x);
    * This function returns the value of the polynomial expression:
    * @ ax^n + bx^(n - 1) + ... c
    * Related Functions: @pow@
    */
   MAKE_INTRINSIC(".sin", math_sin, FLOAT_TYPE, 0),
   /* Prototype: Float sin (Float x);
    * This function returns the sine of @x@.
    * Related Functions: @cos@, @asin@
    */
   MAKE_INTRINSIC(".cos", math_cos, FLOAT_TYPE, 0),
   /* Prototype: Float cos (Float x);
    * This function returns the cosine of @x@.
    * Related Functions: @sin@, @acos@
    */
   MAKE_INTRINSIC(".tan", math_tan, FLOAT_TYPE, 0),
   /* Prototype: Float tan (Float x);
    * This function returns the tangent of @x@.
    * Related Functions: @cos@, @sin@, @sqrt@, @atan@
    */
   MAKE_INTRINSIC(".atan", math_atan, FLOAT_TYPE, 0),
   /* Prototype: Float atan (Float x);
    * This function returns the arc tangent of @x@.
    * Related Functions: @sqrt@, @tan@, 
    */
   MAKE_INTRINSIC(".acos", math_acos, FLOAT_TYPE, 0),
   /* Prototype: Float acos (Float x);
    * This function returns the arc cosine of @x@.
    * Related Functions: @cos@, @sin@
    */
   MAKE_INTRINSIC(".asin", math_asin, FLOAT_TYPE, 0),
   /* Prototype: Float asin (Float x);
    * This function returns the arc sine of @x@.
    * Related Functions: @cos@, @sin@
    */
   MAKE_INTRINSIC(".exp", math_exp, FLOAT_TYPE, 0),
   /* Prototype: Float exp (Float x);
    * This function returns the exponental of @x@.  That is, @2.7182818...@
    * to the @x@ power.
    * Related Functions: @sqrt@, @log@
    */
   MAKE_INTRINSIC(".log", math_log, FLOAT_TYPE, 0),
   /* Prototype: Float log (Float x);
    * This function returns the natural logarithm of @x@.
    * Note: @x@ must be greater than zero.
    * Related Functions: @exp@, @log10@
    */
   MAKE_INTRINSIC(".sqrt", math_sqrt, FLOAT_TYPE, 0),
   /* Prototype: Float sqrt (Float x);
    * This function returns the square root of @x@.
    * Note: @x@ must be greater than or equal to zero.
    * Related Functions: @pow@
    */
   MAKE_INTRINSIC(".log10", math_log10, FLOAT_TYPE, 0),
   /* Prototype: Float log10 (Float x);
    * This function returns the base ten logarithm of @x@.
    * Note: @x@ must be greater than zero.
    * Related Functions: @log@, @pow@
    */
   MAKE_INTRINSIC(".pow", math_pow, FLOAT_TYPE, 0),
   /* Prototype: Float pow (Float x, Float y);
    * This function returns the value of @x@ raised to the @y@ power.
    * Note: @x@ must be greater than or equal to zero.
    * Related Functions: @exp@, @log@, @sqrt@
    */
   MAKE_VARIABLE(".E", &Const_E, FLOAT_TYPE, 1),
   /* Prototype: Float E = 2.718281828459045;
    * The variable E is a read-only variable that represents the value of
    * the base of the natural logarithms.
    */
   MAKE_VARIABLE(".PI", &Const_Pi, FLOAT_TYPE, 1),
   /* Prototype: Float PI = 3.141592653589793;
    * The variable PI is a read-only floating point number.
    */
   MAKE_INTRINSIC(".float",  slmath_do_float, FLOAT_TYPE, 0),
   /* Prototype: Float float (Object x);
    * The @float@ function takes an object such as a string or an integer
    * and converts it to its floating point equivalent.
    * For example, @float ("12.34")@ returns the floating point number @12.34@.
    * Related Functions: @integer@, @string@, @char@, @Sprintf@
    */
   SLANG_END_TABLE
};

int init_SLmath(void)
{
#ifndef pc_system
   (void) SIGNAL (SIGFPE, math_floating_point_exception);
#endif
   if (!SLdefine_for_ifdef ("SLMATH")) return 0;
   return SLang_add_table(slmath_table, "_Math");
}

