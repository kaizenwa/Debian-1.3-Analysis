/************** Machine (and compiler) dependent definitions. **************
 *
 *	These defs for GCC 1.34 on a UNIX-PC.  Written by Steve
 *	Simmons (scs@lokkur.dexter.mi.us).  Shoot me, not Kim.
 */

/*      MACHINE TYPE	DEFINED TYPE		VALUE RANGE	*/

typedef unsigned char	int8;		/*        0 ..     255 */
typedef short		int16;		/*  -10,000 ..  10,000 */
typedef long		int32;		/* -100,000 .. 100,000 */
typedef unsigned long	uint32;		/* 	  0 ..  2^31-1 */


/*
 * 	Define NO_VARARGS if the varargs feature is not available
 *
 *	Also define NO_VARARGS if the vprintf/vsprintf routines are not
 *	available (however, this will only by safe on some machines, like
 *	the VAX).
 *
 */

/* #define NO_VARARGS */

#ifdef NETWORK_DATABASE
YOU LOSE ON NETWORK_DATABASE
#endif
