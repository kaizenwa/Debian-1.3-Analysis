/* Copyright (C) 1993  Olaf Flebbe

EXPERIMENTAL IMPLEMENTATION OF proposals of the NCEG

*/

#ifndef _FP_H
#define _FP_H

#define float_t long double
#define double_t long double

#define HUGE_VALL INFINITYL

#define FP_NAN       1
#define FP_INFINITE  2
#define FP_NORMAL    3
#define FP_SUBNORMAL 4
#define FP_ZERO      5

#define fpclassify(x) ((sizeof(x) == sizeof(float)) ? fpclassifyf(x) \
	: (sizeof(x) == sizeof(double)) ? fpclassifyd(x) \
	: fpclassifyl(x))

#define signbit(x) ( x < 0 ? 1 : 0)

#define isfinite( x) ( int ___x = fpclassify( x); \
                       !( ___x==FP_NAN || ___x==FP_INFINITE))
#define isnormal(x)  (fpclassify( x) == FP_NORMAL)
#define isnanl(x)     (fpclassifyl( x) == FP_NAN)
#define isinfl(x)     (fpclassifyl( x) == FP_INFINITE)

#define DECIMAL_DIGIT 19


long double acoshl( long double _x);
long double acosl ( long double _x);
long double asinhl ( long double _x);
long double asinl ( long double _x);
long double atanl ( long double _x);
long double atan2l( long double _x, long double _y);
long double atanhl( long double _x);
long double fabsl ( long double _x);
long double floorl( long double _x);
long double fmodl ( long double _x, long double _y);
long double hypotl( long double _x, long double _y);
long double sqrtl ( long double _x);
long double sinl  ( long double _x);
long double logl  ( long double _x);
long double expl  ( long double _x); 

int fpclassifyf( float _x);
int fpclassifyd( double _x);
int fpclassifyl( long double _x);

#ifndef M_E
#define M_E         2.7182818284590452354l
#endif
#ifndef M_LOG2E
#define M_LOG2E     1.4426950408889634074l
#endif
#ifndef M_LOG10E
#define M_LOG10E    0.43429448190325182765l
#endif
#ifndef M_LN2
#define M_LN2       0.69314718055994530942l
#endif
#ifndef M_LN10
#define M_LN10      2.30258509299404568402l
#endif
#ifndef M_PI
#define M_PI        3.14159265358979323846l
#endif
#ifndef M_PI_2
#define M_PI_2      1.57079632679489661923l
#endif
#ifndef M_1_PI
#define M_1_PI      0.31830988618379067154l
#endif
#ifndef M_PI_4
#define M_PI_4      0.78539816339744830962l
#endif
#ifndef M_2_PI
#define M_2_PI      0.63661977236758134308l
#endif
#ifndef M_2_SQRTPI
#define M_2_SQRTPI  1.12837916709551257390l
#endif
#ifndef M_SQRT2
#define M_SQRT2     1.41421356237309504880l
#endif
#ifndef M_SQRT1_2
#define M_SQRT1_2   0.70710678118654752440l
#endif

/* private definition */
long double __infnanl( int error);


#endif

