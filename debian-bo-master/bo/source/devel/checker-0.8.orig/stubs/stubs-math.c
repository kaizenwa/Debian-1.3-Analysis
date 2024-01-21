/* Checker stubs for functions defined in math.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_MATH_H
#include <math.h>
#include "checker_api.h"

#if 0
#define HAVE_acos
#define HAVE_asin
#define HAVE_atan
#define HAVE_atan2
#define HAVE_cos
#define HAVE_sin
#define HAVE_tan
#define HAVE_cosh
#define HAVE_sinh
#define HAVE_tanh
#define HAVE_acosh
#define HAVE_asinh
#define HAVE_atanh
#define HAVE_exp
#define HAVE_frexp
#define HAVE_ldexp
#define HAVE_log
#define HAVE_log10
#define HAVE_expm1
#define HAVE_log1p
#define HAVE_modf
#define HAVE_pow
#define HAVE_sqrt
#define HAVE_cbrt
#define HAVE_ceil
#define HAVE_fabs
#define HAVE_floor
#define HAVE_fmod
#define HAVE___isinf
#define HAVE___isnan
#define HAVE___finite
#define HAVE___infnan
#define HAVE___copysign
#define HAVE___rint
#define HAVE_rint
#define HAVE_hypot
#define HAVE_isinf
#define HAVE_isnan
#define HAVE_finite
#define HAVE_infnan
#define HAVE_copysign
#define HAVE_drem
#define HAVE_pow2
#define HAVE_pow10
#define HAVE_erf
#define HAVE_erfc
#define HAVE_j0
#define HAVE_j1
#define HAVE_jn
#define HAVE_lgamma
#define HAVE_y0
#define HAVE_y1
#define HAVE_yn
#define HAVE_acosl
#define HAVE_asinl
#define HAVE_atanl
#define HAVE_cosl
#define HAVE_sinl
#define HAVE_tanl
#define HAVE_coshl
#define HAVE_sinhl
#define HAVE_tanhl
#define HAVE_acoshl
#define HAVE_asinhl
#define HAVE_atanhl
#define HAVE_expl
#define HAVE_frexpl
#define HAVE_ldexpl
#define HAVE_logl
#define HAVE_log10l
#define HAVE_expm1l
#define HAVE_log1pl
#define HAVE_modfl
#define HAVE_powl
#define HAVE_atan2l
#define HAVE_sqrtl
#define HAVE_cbrtl
#define HAVE_log2l
#define HAVE_ceill
#define HAVE_fabsl
#define HAVE_floorl
#define HAVE_fmodl
#define HAVE___isinfl
#define HAVE___isnanl
#define HAVE_hypotl
#define HAVE_pow2l
#define HAVE_pow10l
#define HAVE_erfl
#define HAVE_erfcl
#define HAVE_j0l
#define HAVE_j1l
#define HAVE_jnl
#define HAVE_lgammal
#define HAVE_y0l
#define HAVE_y1l
#define HAVE_ynl
#endif

/* compiled from: . */
#ifdef HAVE_acos
/* From `/usr/include/math.h:34'.  */
double
chkr$acos (double x)
{
#if USE_BI_JUMP
  __builtin_jump (acos);
#else
  return acos (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_acos */

#ifdef HAVE_asin
/* From `/usr/include/math.h:36'.  */
double
chkr$asin (double x)
{
#if USE_BI_JUMP
  __builtin_jump (asin);
#else
  return asin (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_asin */

#ifdef HAVE_atan
/* From `/usr/include/math.h:38'.  */
double
chkr$atan (double x)
{
#if USE_BI_JUMP
  __builtin_jump (atan);
#else
  return atan (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atan */

#ifdef HAVE_atan2
/* From `/usr/include/math.h:41'.  */
double
chkr$atan2 (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (atan2);
#else
  return atan2 (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atan2 */

#ifdef HAVE_cos
/* From `/usr/include/math.h:45'.  */
double
chkr$cos (double x)
{
#if USE_BI_JUMP
  __builtin_jump (cos);
#else
  return cos (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cos */

#ifdef HAVE_sin
/* From `/usr/include/math.h:47'.  */
double
chkr$sin (double x)
{
#if USE_BI_JUMP
  __builtin_jump (sin);
#else
  return sin (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sin */

#ifdef HAVE_tan
/* From `/usr/include/math.h:49'.  */
double
chkr$tan (double x)
{
#if USE_BI_JUMP
  __builtin_jump (tan);
#else
  return tan (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tan */

#ifdef HAVE_cosh
/* From `/usr/include/math.h:55'.  */
double
chkr$cosh (double x)
{
#if USE_BI_JUMP
  __builtin_jump (cosh);
#else
  return cosh (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cosh */

#ifdef HAVE_sinh
/* From `/usr/include/math.h:57'.  */
double
chkr$sinh (double x)
{
#if USE_BI_JUMP
  __builtin_jump (sinh);
#else
  return sinh (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sinh */

#ifdef HAVE_tanh
/* From `/usr/include/math.h:59'.  */
double
chkr$tanh (double x)
{
#if USE_BI_JUMP
  __builtin_jump (tanh);
#else
  return tanh (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tanh */

#ifdef HAVE_acosh
/* From `/usr/include/math.h:63'.  */
double
chkr$acosh (double x)
{
#if USE_BI_JUMP
  __builtin_jump (acosh);
#else
  return acosh (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_acosh */

#ifdef HAVE_asinh
/* From `/usr/include/math.h:65'.  */
double
chkr$asinh (double x)
{
#if USE_BI_JUMP
  __builtin_jump (asinh);
#else
  return asinh (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_asinh */

#ifdef HAVE_atanh
/* From `/usr/include/math.h:68'.  */
double
chkr$atanh (double x)
{
#if USE_BI_JUMP
  __builtin_jump (atanh);
#else
  return atanh (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atanh */

#ifdef HAVE_exp
/* From `/usr/include/math.h:75'.  */
double
chkr$exp (double x)
{
#if USE_BI_JUMP
  __builtin_jump (exp);
#else
  return exp (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_exp */

#ifdef HAVE_frexp
/* From `/usr/include/math.h:78'.  */
double
chkr$frexp (double x, int *exp)
{
  stubs_chkr_check_addr (exp, sizeof (int), CHKR_WO, "exp");
#if USE_BI_JUMP
  __builtin_jump (frexp);
#else
  return frexp (x, exp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_frexp */

#ifdef HAVE_ldexp
/* From `/usr/include/math.h:81'.  */
double
chkr$ldexp (double x, int exp)
{
#if USE_BI_JUMP
  __builtin_jump (ldexp);
#else
  return ldexp (x, exp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ldexp */

#ifdef HAVE_log
/* From `/usr/include/math.h:84'.  */
double
chkr$log (double x)
{
#if USE_BI_JUMP
  __builtin_jump (log);
#else
  return log (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_log */

#ifdef HAVE_log10
/* From `/usr/include/math.h:87'.  */
double
chkr$log10 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (log10);
#else
  return log10 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_log10 */

#ifdef HAVE_expm1
/* From `/usr/include/math.h:90'.  */
double
chkr$expm1 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (expm1);
#else
  return expm1 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_expm1 */

#ifdef HAVE_log1p
/* From `/usr/include/math.h:93'.  */
double
chkr$log1p (double x)
{
#if USE_BI_JUMP
  __builtin_jump (log1p);
#else
  return log1p (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_log1p */

#ifdef HAVE_modf
/* From `/usr/include/math.h:96'.  */
double
chkr$modf (double x, double *ent)
{
  stubs_chkr_check_addr (ent, sizeof (double), CHKR_WO, "ent");
#if USE_BI_JUMP
  __builtin_jump (modf);
#else
  return modf (x, ent);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_modf */

#ifdef HAVE_pow
/* From `/usr/include/math.h:104'.  */
double
chkr$pow (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (pow);
#else
  return pow (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pow */

#ifdef HAVE_sqrt
/* From `/usr/include/math.h:107'.  */
double
chkr$sqrt (double x)
{
#if USE_BI_JUMP
  __builtin_jump (sqrt);
#else
  return sqrt (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sqrt */

#ifdef HAVE_cbrt
/* From `/usr/include/math.h:111'.  */
double
chkr$cbrt (double x)
{
#if USE_BI_JUMP
  __builtin_jump (cbrt);
#else
  return cbrt (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cbrt */

#ifdef HAVE_ceil
/* From `/usr/include/math.h:118'.  */
double
chkr$ceil (double x)
{
#if USE_BI_JUMP
  __builtin_jump (ceil);
#else
  return ceil (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ceil */

#ifdef HAVE_fabs
/* From `/usr/include/math.h:121'.  */
double
chkr$fabs (double x)
{
#if USE_BI_JUMP
  __builtin_jump (fabs);
#else
  return fabs (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fabs */

#ifdef HAVE_floor
/* From `/usr/include/math.h:124'.  */
double
chkr$floor (double x)
{
#if USE_BI_JUMP
  __builtin_jump (floor);
#else
  return floor (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_floor */

#ifdef HAVE_fmod
/* From `/usr/include/math.h:127'.  */
double
chkr$fmod (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (fmod);
#else
  return fmod (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fmod */

#ifdef HAVE___isinf
/* From `/usr/include/math.h:132'.  */
int
chkr$__isinf (double x)
{
#if USE_BI_JUMP
  __builtin_jump (__isinf);
#else
  return __isinf (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___isinf */

#ifdef HAVE___isnan
/* From `/usr/include/math.h:135'.  */
int
chkr$__isnan (double x)
{
#if USE_BI_JUMP
  __builtin_jump (__isnan);
#else
  return __isnan (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___isnan */

#ifdef HAVE___finite
/* From `/usr/include/math.h:139'.  */
int
chkr$__finite (double x)
{
#if USE_BI_JUMP
  __builtin_jump (__finite);
#else
  return __finite (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___finite */

#ifdef HAVE___infnan
/* From `/usr/include/math.h:150'.  */
double
chkr$__infnan (int x)
{
#if USE_BI_JUMP
  __builtin_jump (__infnan);
#else
  return __infnan (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___infnan */

#ifdef HAVE___copysign
/* From `/usr/include/math.h:154'.  */
double
chkr$__copysign (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (__copysign);
#else
  return __copysign (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___copysign */

#ifdef HAVE___rint
/* From `/usr/include/math.h:159'.  */
double
chkr$__rint (double x)
{
#if USE_BI_JUMP
  __builtin_jump (__rint);
#else
  return __rint (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___rint */

#ifdef HAVE_rint
/* From `/usr/include/math.h:160'.  */
double
chkr$rint (double x)
{
#if USE_BI_JUMP
  __builtin_jump (rint);
#else
  return rint (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rint */

#ifdef HAVE_hypot
/* From `/usr/include/math.h:164'.  */
double
chkr$hypot (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (hypot);
#else
  return hypot (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_hypot */

#ifdef HAVE_isinf
/* From `/usr/include/math.h:168'.  */
int
chkr$isinf (double x)
{
#if USE_BI_JUMP
  __builtin_jump (isinf);
#else
  return isinf (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isinf */

#ifdef HAVE_isnan
/* From `/usr/include/math.h:169'.  */
int
chkr$isnan (double x)
{
#if USE_BI_JUMP
  __builtin_jump (isnan);
#else
  return isnan (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isnan */

#ifdef HAVE_finite
/* From `/usr/include/math.h:170'.  */
int
chkr$finite (double x)
{
#if USE_BI_JUMP
  __builtin_jump (finite);
#else
  return finite (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_finite */

#ifdef HAVE_infnan
/* From `/usr/include/math.h:171'.  */
double
chkr$infnan (int x)
{
#if USE_BI_JUMP
  __builtin_jump (infnan);
#else
  return infnan (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_infnan */

#ifdef HAVE_copysign
/* From `/usr/include/math.h:173'.  */
double
chkr$copysign (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (copysign);
#else
  return copysign (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_copysign */

#ifdef HAVE_drem
/* From `/usr/include/math.h:174'.  */
double
chkr$drem (double x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (drem);
#else
  return drem (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_drem */

#ifdef HAVE_pow2
/* From `/usr/include/math.h:190'.  */
double
chkr$pow2 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (pow2);
#else
  return pow2 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pow2 */

#ifdef HAVE_pow10
/* From `/usr/include/math.h:193'.  */
double
chkr$pow10 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (pow10);
#else
  return pow10 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pow10 */

#ifdef HAVE_erf
/* From `/usr/include/math.h:196'.  */
double
chkr$erf (double x)
{
#if USE_BI_JUMP
  __builtin_jump (erf);
#else
  return erf (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_erf */

#ifdef HAVE_erfc
/* From `/usr/include/math.h:199'.  */
double
chkr$erfc (double x)
{
#if USE_BI_JUMP
  __builtin_jump (erfc);
#else
  return erfc (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_erfc */

#ifdef HAVE_j0
/* From `/usr/include/math.h:202'.  */
double
chkr$j0 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (j0);
#else
  return j0 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_j0 */

#ifdef HAVE_j1
/* From `/usr/include/math.h:205'.  */
double
chkr$j1 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (j1);
#else
  return j1 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_j1 */

#ifdef HAVE_jn
/* From `/usr/include/math.h:208'.  */
double
chkr$jn (int x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (jn);
#else
  return jn (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_jn */

#ifdef HAVE_lgamma
/* From `/usr/include/math.h:211'.  */
double
chkr$lgamma (double x)
{
#if USE_BI_JUMP
  __builtin_jump (lgamma);
#else
  return lgamma (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_lgamma */

#ifdef HAVE_y0
/* From `/usr/include/math.h:214'.  */
double
chkr$y0 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (y0);
#else
  return y0 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_y0 */

#ifdef HAVE_y1
/* From `/usr/include/math.h:217'.  */
double
chkr$y1 (double x)
{
#if USE_BI_JUMP
  __builtin_jump (y1);
#else
  return y1 (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_y1 */

#ifdef HAVE_yn
/* From `/usr/include/math.h:220'.  */
double
chkr$yn (int x, double y)
{
#if USE_BI_JUMP
  __builtin_jump (yn);
#else
  return yn (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_yn */

#ifdef HAVE_acosl
/* From `/usr/include/math.h:223'.  */
__long_double_t
chkr$acosl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (acosl);
#else
  return acosl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_acosl */

#ifdef HAVE_asinl
/* From `/usr/include/math.h:224'.  */
__long_double_t
chkr$asinl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (asinl);
#else
  return asinl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_asinl */

#ifdef HAVE_atanl
/* From `/usr/include/math.h:225'.  */
__long_double_t
chkr$atanl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (atanl);
#else
  return atanl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atanl */

#ifdef HAVE_cosl
/* From `/usr/include/math.h:226'.  */
__long_double_t
chkr$cosl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (cosl);
#else
  return cosl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cosl */

#ifdef HAVE_sinl
/* From `/usr/include/math.h:227'.  */
__long_double_t
chkr$sinl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (sinl);
#else
  return sinl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sinl */

#ifdef HAVE_tanl
/* From `/usr/include/math.h:228'.  */
__long_double_t
chkr$tanl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (tanl);
#else
  return tanl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tanl */

#ifdef HAVE_acoshl
/* From `/usr/include/math.h:230'.  */
__long_double_t
chkr$acoshl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (acoshl);
#else
  return acoshl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_acoshl */

#ifdef HAVE_asinhl
/* From `/usr/include/math.h:231'.  */
__long_double_t
chkr$asinhl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (asinhl);
#else
  return asinhl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_asinhl */

#ifdef HAVE_atanhl
/* From `/usr/include/math.h:233'.  */
__long_double_t
chkr$atanhl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (atanhl);
#else
  return atanhl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atanhl */

#ifdef HAVE_coshl
/* From `/usr/include/math.h:234'.  */
__long_double_t
chkr$coshl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (coshl);
#else
  return coshl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_coshl */

#ifdef HAVE_sinhl
/* From `/usr/include/math.h:235'.  */
__long_double_t
chkr$sinhl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (sinhl);
#else
  return sinhl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sinhl */

#ifdef HAVE_tanhl
/* From `/usr/include/math.h:236'.  */
__long_double_t
chkr$tanhl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (tanhl);
#else
  return tanhl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tanhl */

#ifdef HAVE_expl
/* From `/usr/include/math.h:237'.  */
__long_double_t
chkr$expl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (expl);
#else
  return expl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_expl */

#ifdef HAVE_expm1l
/* From `/usr/include/math.h:238'.  */
__long_double_t
chkr$expm1l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (expm1l);
#else
  return expm1l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_expm1l */

#ifdef HAVE_frexpl
/* From `/usr/include/math.h:239'.  */
__long_double_t
chkr$frexpl (__long_double_t x, int *exp)
{
  stubs_chkr_check_addr (exp, sizeof (int), CHKR_WO, "exp");
#if USE_BI_JUMP
  __builtin_jump (frexpl);
#else
  return frexpl (x, exp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_frexpl */

#ifdef HAVE_ldexpl
/* From `/usr/include/math.h:240'.  */
__long_double_t
chkr$ldexpl (__long_double_t x, int y)
{
#if USE_BI_JUMP
  __builtin_jump (ldexpl);
#else
  return ldexpl (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ldexpl */

#ifdef HAVE_logl
/* From `/usr/include/math.h:241'.  */
__long_double_t
chkr$logl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (logl);
#else
  return logl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_logl */

#ifdef HAVE_log10l
/* From `/usr/include/math.h:242'.  */
__long_double_t
chkr$log10l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (log10l);
#else
  return log10l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_log10l */

#ifdef HAVE_modfl
/* From `/usr/include/math.h:243'.  */
__long_double_t
chkr$modfl (__long_double_t x, __long_double_t *y)
{
  stubs_chkr_check_addr (y, sizeof (__long_double_t), CHKR_WO, "y");
#if USE_BI_JUMP
  __builtin_jump (modfl);
#else
  return modfl (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_modfl */

#ifdef HAVE_powl
/* From `/usr/include/math.h:244'.  */
__long_double_t
chkr$powl (__long_double_t x, __long_double_t y)
{
#if USE_BI_JUMP
  __builtin_jump (powl);
#else
  return powl (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_powl */

#ifdef HAVE_sqrtl
/* From `/usr/include/math.h:245'.  */
__long_double_t
chkr$sqrtl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (sqrtl);
#else
  return sqrtl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sqrtl */

#ifdef HAVE_atan2l
/* From `/usr/include/math.h:247'.  */
__long_double_t
chkr$atan2l (__long_double_t x, __long_double_t y)
{
#if USE_BI_JUMP
  __builtin_jump (atan2l);
#else
  return atan2l (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atan2l */

#ifdef HAVE_hypotl
/* From `/usr/include/math.h:248'.  */
__long_double_t
chkr$hypotl (__long_double_t x, __long_double_t y)
{
#if USE_BI_JUMP
  __builtin_jump (hypotl);
#else
  return hypotl (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_hypotl */

#ifdef HAVE_erfl
/* From `/usr/include/math.h:250'.  */
__long_double_t
chkr$erfl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (erfl);
#else
  return erfl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_erfl */

#ifdef HAVE_erfcl
/* From `/usr/include/math.h:251'.  */
__long_double_t
chkr$erfcl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (erfcl);
#else
  return erfcl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_erfcl */

#ifdef HAVE_lgammal
/* From `/usr/include/math.h:252'.  */
__long_double_t
chkr$lgammal (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (lgammal);
#else
  return lgammal (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_lgammal */

#ifdef HAVE_j0l
/* From `/usr/include/math.h:253'.  */
__long_double_t
chkr$j0l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (j0l);
#else
  return j0l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_j0l */

#ifdef HAVE_y0l
/* From `/usr/include/math.h:254'.  */
__long_double_t
chkr$y0l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (y0l);
#else
  return y0l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_y0l */

#ifdef HAVE_j1l
/* From `/usr/include/math.h:255'.  */
__long_double_t
chkr$j1l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (j1l);
#else
  return j1l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_j1l */

#ifdef HAVE_y1l
/* From `/usr/include/math.h:256'.  */
__long_double_t
chkr$y1l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (y1l);
#else
  return y1l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_y1l */

#ifdef HAVE_jnl
/* From `/usr/include/math.h:257'.  */
__long_double_t
chkr$jnl (int x, __long_double_t arg1)
{
#if USE_BI_JUMP
  __builtin_jump (jnl);
#else
  return jnl (x, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_jnl */

#ifdef HAVE_ynl
/* From `/usr/include/math.h:258'.  */
__long_double_t
chkr$ynl (int x, __long_double_t arg1)
{
#if USE_BI_JUMP
  __builtin_jump (ynl);
#else
  return ynl (x, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ynl */

#ifdef HAVE_cbrtl
/* From `/usr/include/math.h:259'.  */
__long_double_t
chkr$cbrtl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (cbrtl);
#else
  return cbrtl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cbrtl */

#ifdef HAVE_log1pl
/* From `/usr/include/math.h:261'.  */
__long_double_t
chkr$log1pl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (log1pl);
#else
  return log1pl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_log1pl */

#ifdef HAVE_log2l
/* From `/usr/include/math.h:262'.  */
__long_double_t
chkr$log2l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (log2l);
#else
  return log2l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_log2l */

#ifdef HAVE_pow2l
/* From `/usr/include/math.h:263'.  */
__long_double_t
chkr$pow2l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (pow2l);
#else
  return pow2l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pow2l */

#ifdef HAVE_pow10l
/* From `/usr/include/math.h:264'.  */
__long_double_t
chkr$pow10l (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (pow10l);
#else
  return pow10l (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pow10l */

#ifdef HAVE_ceill
/* From `/usr/include/math.h:267'.  */
__long_double_t
chkr$ceill (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (ceill);
#else
  return ceill (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ceill */

#ifdef HAVE_fabsl
/* From `/usr/include/math.h:270'.  */
__long_double_t
chkr$fabsl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (fabsl);
#else
  return fabsl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fabsl */

#ifdef HAVE_floorl
/* From `/usr/include/math.h:273'.  */
__long_double_t
chkr$floorl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (floorl);
#else
  return floorl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_floorl */

#ifdef HAVE_fmodl
/* From `/usr/include/math.h:277'.  */
__long_double_t
chkr$fmodl (__long_double_t x, __long_double_t y)
{
#if USE_BI_JUMP
  __builtin_jump (fmodl);
#else
  return fmodl (x, y);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fmodl */

#ifdef HAVE___isinfl
/* From `/usr/include/math.h:282'.  */
int
chkr$__isinfl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (__isinfl);
#else
  return __isinfl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___isinfl */

#ifdef HAVE___isnanl
/* From `/usr/include/math.h:285'.  */
int
chkr$__isnanl (__long_double_t x)
{
#if USE_BI_JUMP
  __builtin_jump (__isnanl);
#else
  return __isnanl (x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___isnanl */

#endif /* HAVE_MATH_H */
