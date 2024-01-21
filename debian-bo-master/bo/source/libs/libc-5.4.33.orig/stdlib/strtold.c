/* The actual implementation for all floating point sizes is in strtod.c.
   These macros tell it to produce the `long double' version, `strtold'.  */

#define	FLOAT		long double
#define	FLT		LDBL
#define	STRTOF		strtold
#define	MPN2FLOAT	__mpn_construct_long_double
#define	FLOAT_HUGE_VAL	HUGE_VALl

#define	__STRTOF	__strtold
#include "strtod.c"

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__strtold, strtold);
#endif
