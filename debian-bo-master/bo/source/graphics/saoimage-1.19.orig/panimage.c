#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	panimage.c (Pan Image)
 * Purpose:	Make and put the pan window display
 * Subroutine:	disp_panbox()			returns: void
 * Subroutine:	map_panbox()			returns: void
 * Subroutine:	show_dispcoords()		returns: void
 * Xlib calls:	XSync()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 29 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MIN, MAX, DONT_CARE, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/scale.h"	/* define scaling constants */

/*
 * Subroutine:	disp_panbox
 * Purpose:	Redraw the panbox window display
 * Xlib calls:	XSync()
 */
void disp_panbox ( )
{
  void disp_window(), draw_pancursor();

  /* put up the image and make sure it is up before drawing the cursor */
  disp_window(&panbox);
  XSync(panbox.display, 0);
  draw_pancursor();
}

/*
 * Subroutine:	map_panbox
 * Purpose:	Fill the panbox display buffer, mapping from its short buffer
 */
void map_panbox ( )
{
  static void map_panbuf();
#ifdef SUMBLOCK
  static void map_adj_panbuf();
#endif
  void panimage_halftone();

  if( color.ncolors <= 1 ) {
    panbox.image.format = XYBitmap;
    panbox.image.depth = 1;
    panbox.image.bits_per_pixel = 1;
    panbox.image.bytes_per_line = (panbox.image.width + 7) / 8;
    panimage_halftone();
  } else {
    panbox.image.format = ZPixmap;
    panbox.image.depth = color.screen_depth;
    panbox.image.bits_per_pixel = 8;
    panbox.image.bytes_per_line = panbox.image.width;
#ifdef SUMBLOCK
    /* CHANGE SCALE WHEN SUMMED ZOOMING GAVE A DIFFERENT VALUE RANGE */
    if( buffer.panbuf_summing != buffer.scalemap_summing ) {
      int ratio_num, ratio_denom;

      /* select largest denominator which won't overflow */
      /* WARNING: make sure panbuf_max > 0!!! */
      ratio_denom = 32767 / buffer.scale_max;
      ratio_num = (ratio_denom * buffer.scale_max) / buffer.panbuf_max;
      map_adj_panbuf(ratio_num, ratio_denom);
    } else
#endif
      map_panbuf();
  }
}

/*
 * Subroutine:	show_dispcoords
 * Purpose:	Print the file coordinates of the display's center and zoom
 */
void show_dispcoords ( )
{
  float cenX, cenY;
  float zoom;
  void d_transform();

  /* calculate file coords of center of display */
  d_transform(&coord.imgtofile,
	      (double)coord.id.cenX, (double)coord.id.cenY, &cenX, &cenY);
  if( coord.filetoimg.inx_outx != 0.0 )
    zoom = coord.filetoimg.inx_outx * coord.imgtodisp.inx_outx;
  else
    zoom = coord.filetoimg.iny_outx * coord.imgtodisp.inx_outx;
  /* take abs */
  if( zoom < 0.0 ) zoom = -zoom;
  (void)printf("Display from image file:\n");
  if( zoom < 1.0 )
    (void)printf("Center: X=%.2f, Y=%.2f, Blocking: %.4f (%d)\n",
		 (double)cenX, (double)cenY, zoom, (int)(-1.0/zoom));
  else
    (void)printf("Center: X=%.2f, Y=%.2f, Blocking: %.1f\n",
		 (double)cenX, (double)cenY, zoom);
}

/*
 * Subroutine:	map_panbuf
 * Purpose:	Map short image data to display buffer
 * Exception:	Buffers both buffers have same size
 */
static void map_panbuf ( )
{
  register unsigned char *display;	/* l: pointer to display buffer */
  register unsigned char *dispend;	/* l: end of display buffer ptr */
  register short *image;		/* l: pointer to short integer data */
  register unsigned char *lookup;	/* l: pointer to lookup table */

  image = buffer.panbuf;
  lookup = buffer.scalemap + SCALEOFF;
  display = (unsigned char *)panbox.image.data;
  dispend = display + (panbox.xwidth * panbox.yheight);
  /* loop through both buffers */
  while( display < dispend ) {
    *display++ = lookup[*image++];
  }
}

#ifdef SUMBLOCK
/*
 * Subroutine:	map_adj_panbuf
 * Purpose:	Map short integer data to byte display buffer, adjusting
 *		for value changes due to summed blocking applied to short buf.
 * Exception:	Buffers have same size
 * Note:	Blocking factor may differ from that used for scale map.
 */
static void map_adj_panbuf ( numerator, denominator )
     register int numerator;		/* i: scale adjustment numberator */
     register int denominator;		/* i: scale adjustment denominator */
{
  register unsigned char *display;	/* l: pointer to display buffer */
  register unsigned char *dispend;	/* l: end of display buffer ptr */
  register short *image;		/* l: pointer to short integer data */
  register unsigned char *lookup;	/* l: pointer to lookup table */
  register int imval, round;

  image = buffer.panbuf;
  lookup = buffer.scalemap + SCALEOFF;
  display = panbox.byteimage;
  dispend = display + (panbox.xwidth * panbox.yheight);
  round = (denominator + 1) / 2;
  while( display < dispend ) {
    if( (imval = *image++) != 0 ) {
      if (imval > 0)
	imval = ((imval * numerator) + round) / denominator;
      else
	imval = ((imval * numerator) - round) / denominator;
    }
    *display++ = lookup[imval];
  }
}
#endif
