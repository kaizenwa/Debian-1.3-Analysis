#ifndef lint
static char SccsBtnCId[] = "%W%  %G%";
#endif

/*
 * Module:	btnchars.h
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Define structures character bitmaps, and include character sets
 * Copyright:	1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		20 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

struct bitfont {
  int width;
  int height;
  int byte_width;
  char *bits;
};

#include "outchars.h"
#include "inchars.h"
