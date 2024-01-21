#ifndef lint
static char SccsScaleId[] = "%W%  %G%";
#endif

/* Module:	Scale.h
 * Purpose:	Define the range of accepted image values and other scale
 *		histogram parameters
 * Note:	Assumes full range of signed short
 * Modified:	{0} Michael VanHilst	initial version	    28 September 1988
 *		{n} <who> -- <does what> -- <when>
 */

#define SCALEBUFSZ 65536 /* size of buffer (full range of short 0x10000) */
#define SCALEWIDTH 65534 /* range of image values allowed in histogram */
#define SCALEMIN -32767	 /* minimum image value allowed */
#define SCALEMAX 32767	 /* maximum image value allowed */
#define SCALEOFF 32768	 /* offset from allocated array to zero (0x8000) */
