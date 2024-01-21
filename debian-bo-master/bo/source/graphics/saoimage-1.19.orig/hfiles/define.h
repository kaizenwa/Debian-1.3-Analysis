#ifndef lint
static char defineId[] = "%W%  %G%";
#endif

/*
 * Module:	Define.h
 * Purpose:	Define commonly used constants and macros
 */

#define YES	1
#define UNKNOWN 0
#define NO	-1
#define DONT_CARE -2
#define U_DONT_CARE 0xA0000000

#define SZ_FNAME 128
#define SZ_LINE 256

#define SMALL_NUMBER 1.0E-30
#define LARGE_NUMBER 1.0E30

/* we don't know if every math.h has a PI defined */
#ifndef PI
#define PI 3.14159265358979323846
#endif
#define TWO_PI 6.28318530717958647692

/* STRUCTURES AND DEFINITIONS USED BY MANY FILES */
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define SQR(a) ((a) * (a))
#define ABS(a) ((a) < 0 ? (-(a)) : (a))

/*
 * Macro:	RND
 * Purpose:	Round to nearest integer value, regardless of sign
 */
#define RND(a) (((a) < 0.0) ? ((int)((a) - 0.5)) : ((int)((a) + 0.5)))

/*
 * Macro:	INTERP
 * Purpose:	Given X along a straight line, interpolate to solve for Y
 * Used by:	Color map routines
 * Inputs:	a,b are X at each end,  e,f are Y at each end, x is given X
 * Exception:	a must be <= b
 * Exception:	x must be between a and b (inclusive)
 * Exception:	All values should be floating point
 * Method:	If x is same as a, return e, else do interpolation.
 *		If (b-a)==0, then (x-a)==0, so zero-divide is avoided.
 */
#define INTERP(a,x,b,e,f) ((x-(a))<SMALL_NUMBER) ? (e):(((f-(e))*(x-(a))/(b-(a)))+e)

/*
 * Macro:	INCSZ
 * Purpose:	Determine the increment in Y per unit step in X
 * Used by:	Color map routines
 * Inputs:	a,b are X at each end, e,f are Y at each end, z = scale factor
 * Note:	z is the scale factor for a,b (e.g. 0-1 -> 0-255, z=255)
 * Exception:	a must be <= b
 */
#define INCSZ(a,b,e,f,z) ((b-(a))<SMALL_NUMBER) ? (0.0):((f-(e))/((b-(a))*z))
