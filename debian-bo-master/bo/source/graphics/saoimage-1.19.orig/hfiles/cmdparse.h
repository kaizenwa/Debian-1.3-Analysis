#ifndef lint
static char SccsParseId[] = "%W%  %G%";
#endif

/* Module:	CmdParse.h
 * Purpose:	Define the use of the bits in the parsing status word
 * Modified:	{0} Michael VanHilst	initial version	       9 January 1989
 *		{n} <who> -- <does what> -- <when>
 */

#define CMD_FNAME	  4
#define CMD_FTYPE	  8
#define CMD_FREAD	 16
#define CMD_ROTATE	 32
#define CMD_SCALE	 64
#define CMD_SCALIM	128
#define CMD_COLOR	256
#define CMD_GEOMETRY	512
#define CMD_CONNECT    1024
