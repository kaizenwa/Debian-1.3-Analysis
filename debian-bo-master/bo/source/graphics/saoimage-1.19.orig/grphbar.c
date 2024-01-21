#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphbar.c (Color Graph Bar)
 * Purpose:	Fill a buffer with a color chart of the allocated colors
 * Subroutine:	fill_colorbar()			returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include "hfiles/define.h"	/* YES, NO, MIN, MAX and more */

/*
 * Subroutine:	fill_colorbar
 * Purpose:	Fill byte buffer with bars of increasing value covering
 *		colormap range
 * PostState:	Buffer will contain data to represent a color sweep in one
 *		diirection while repeating itself in the other.  For a
 *		horizontal color bar, there will be vertical stripes of
 *		increasing (or decreasing) colors from left to right.
 * Note:	Positive polarity is from left (horizontal) or top (vertical).
 */
void fill_colorbar ( data, width, height, low, high,
		     vertical, descend, pixels )
     unsigned char data[];	/* i/o: space to recieve sweep data */
     int width, height;		/* i: buffer dimensions */
     int low, high;		/* i: limits of the sweep range */
     int vertical;		/* i: 0=horizontal sweep, else vertical */
     int descend;		/* i: descend-val-with-increasing-coord */
     unsigned long pixels[];	/* i: continuous array of pixel values */
{
  double valinc;
  int latitude, longitude;
  int range;
  int row;
  int next_row;
  unsigned char val;
  register unsigned char *buf;
  register int i, bufinc;

  /* axis of 0 has horizontal bar, else vertical */
  if( vertical ) {
    /* vertical */
    latitude = height;
    longitude = width;
    bufinc = 1;
    next_row = 0;
  } else {
    /* horizontal */
    latitude = width;
    longitude = height;
    bufinc = width;
    next_row = 1 - (width * height);
  }
  range = high - low;
  if( range < 0 )
    range -= 1;
  else
    range += 1;
  valinc = (double)range / (double)latitude;
  buf = data;
  for( row=0; row<latitude; row++ ) {
    /* non-negative polarity starts at low value and increases */
    if( descend )
      val = (unsigned char)
	pixels[MAX(low, (high - (int)((double)row * valinc)))];
    else
      val = (unsigned char)
	pixels[MIN(high, (low + (int)((double)row * valinc)))];
    for( i=0; i<longitude; i++ ) {
      *buf = val;
      buf += bufinc;
    }
    buf += next_row;
  }
}
