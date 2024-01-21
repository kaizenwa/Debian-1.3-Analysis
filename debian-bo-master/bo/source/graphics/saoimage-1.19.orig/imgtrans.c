#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgtrans.c (Image Transformation)
 * Purpose:	Routines to orthogonally translate buffers.
 * Subroutine:	rotate_buf()			returns: void
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	      8 December 1988
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr */
#include "hfiles/define.h"	/* define MIN, MAX, etc. */

/*
 * Subroutine:	rotate_buf
 * Purpose:	Rotate a buffer as indicated by the code
 * Parameter:	rotcode
 * 	0-3 = rotate clockwise by code * 90 degrees
 * 	4-7 = flip y axis then rotate clockwise by (code - 4) * 90 degrees
 * Called by:	load_image() in ImageRead.c
 */
void rotate_buf ( buf, flip, rotcode, width, height, buf_squared, buf_doubled )
     short *buf;
     int flip;
     int rotcode;
     int width, height;
     int buf_squared, buf_doubled;
{
  int maxdim;
  void xflip_buf(), yflip_buf(), zflip_buf(), cwturn_buf(), ccwturn_buf();
  void transfer_buf();
  static void square_buf(), unsquare_buf();

  if( flip )
    rotcode += 4;
  if( rotcode & 1 ) {
    if( buf_doubled ) {
      short *dst;
      dst = buf + (width * height);
      transfer_buf(buf, dst, width, height, rotcode);
      transfer_buf(dst, buf, width, height, 0);
      return;
    } else if( buf_squared || (width == height) ) {
      maxdim = MAX(width, height);
    } else {
      (void)fprintf(stderr, "ERROR: no buffer to rotate image\n");
      return;
    }
  }
  /* if not a square or like a square with filler on bottom */
  switch( rotcode ) {
  case 0:
    return;
  case 2:
    /* 180 degree rotation */
    zflip_buf(buf, width, height);
    break;
  case 4:
    /* flip along y axis */
    xflip_buf(buf, width, height);
    break;
  case 6:
    /* flip then rotate 180 degrees clockwise */
    yflip_buf(buf, width, height);
    break;
  case 5:
    /* flip along y axis, then rotate */
    xflip_buf(buf, width, height);
  case 1:
    /* 90 degree clockwise rotation */
    if( width < height )
      square_buf(buf, width, height, 1);
    cwturn_buf(buf, maxdim);
    if( height < width )
      unsquare_buf(buf, width, height, 0);
    break;
  case 7:
    /* flip along y axis, then rotate */
    xflip_buf(buf, width, height);
  case 3:
    /* 270 degree clockwise rotation */
    if( width < height )
      square_buf(buf, width, height, 0);
    ccwturn_buf(buf, maxdim);
    if( height < width )
      unsquare_buf(buf, width, height, 1);
    break;
  }
}

/*
 * Subroutine:	square_buf
 * Purpose:	Rearrange lines in a buffer for square symmetry
 * Exception:	Assumes that height of rectangle is greater than width
 * Parameter:	left
 *	left = 1 will put reactangle on left with lines padded out
 *	left = 0 will put rectangle on right with padded in front of lines
 */
static void square_buf ( buf, width, height, left )
     short *buf;
     int width, height;
     int left;
{
  short *rectline, *sqline;
  int bytes;

  /* make buf square with lines filled to end */
  rectline = buf + ((height - 1) * width);
  if( left == 0 )
    sqline = buf + (height * height) - width;
  else
    sqline = buf + (height * height) - height;
  bytes = width * sizeof(short);
  while( sqline > rectline ) {
    bcopy((char *)rectline, (char *)sqline, bytes);
    rectline -= width;
    sqline -= height;
  }
}

/*
 * Subroutine:	unsquare_buf
 * Purpose:	Make rectangular data in square buffer occupy rectangle at top
 * Exception:	Assumes output height is greater than width
 *	left = 1 moves data which is on left side of square
 *	left = 0 moves data which is on right side of square
 */
static void unsquare_buf ( buf, width, height, left )
     short *buf;
     int width, height;
     int left;
{
  short *rectline, *sqline;
  int bytes;
  int i;

  /* make buf square with lines filled to end */
  if( left == 0 ) {
    rectline = buf;
    sqline = buf + (height - width);
  } else {
    rectline = buf + width;
    sqline = buf + height;
  }
  bytes = width * sizeof(short);
  for( i=0; i<height; i++ ) {
    bcopy((char *)sqline, (char *)rectline, bytes);
    rectline += width;
    sqline += height;
  }
}
