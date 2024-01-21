/* The actual implementation for all floating point sizes is in strtod.c.
   These macros tell it to produce the `float' version, `strtof'.  */

#define	FLOAT		float
#define	FLT		FLT
#define	STRTOF		strtof
#define	MPN2FLOAT	__mpn_construct_float
#define	FLOAT_HUGE_VAL	HUGE_VALf

#define	__STRTOF		__strtof

#include "strtod.c"

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__strtof, strtof);
#endif
