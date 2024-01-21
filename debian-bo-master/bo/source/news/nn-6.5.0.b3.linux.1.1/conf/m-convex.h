/************** Machine (and compiler) dependent definitions. **************
 *
 *	Define appropriate types for the following ranges of integer
 *	variables.  These are processor & compiler dependent, but the
 *	distributed definitions will probably work on most systems.
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

#define NO_VARARGS

/*
 *	Define STRCSPN if the strcspn() function is not available.
 */

#define STRCSPN 	/* */

/*
 *	Define NO_SIGINTERRUPT on BSD based systems which don't have
 *	a siginterrupt() function, but provides an SV_INTERRUPT flag
 *	in <signal.h>.
 */

#define NO_SIGINTERRUPT	/* */


/*
 *	Define NETWORK_BYTE_ORDER if the machine's int32's are
 *	already in network byte order, i.e. m68k based.
 */

#define NETWORK_BYTE_ORDER	/* */
