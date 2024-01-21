#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	display.c (Display)
 * Purpose:	Draw images in their windows, including the main display
 * Subroutine:	disp_window()			returns: void
 * Subroutine:	disp_dispbox()			returns: void
 * Subroutine:	map_dispbox()			returns: void
 * Subroutine:	clear_margins()			returns: void
 * Xlib calls:	XPutImage(), XSync();
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  26 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MIN, MAX, DONT_CARE, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/scale.h"	/* define scaling constants */

/*
 * Subroutine:	disp_window
 * Purpose:	Redraw the window's display image with no frills
 * Xlib call:	XPutImage()
 */
void disp_window ( window )
     struct windowRec *window;
{
  GC gc, set_gc(), set_gc_with_background();

  if( window->image.depth == 1 )
    gc = set_gc_with_background(&(color.gcset.disp),
				color.gcset.disp.background);
  else
    gc = set_gc(&(color.gcset.disp));

  XPutImage(window->display, window->ID, gc, &window->image, 0, 0,
	    window->xzero, window->yzero,
	    (unsigned int)window->xwidth, (unsigned int)window->yheight);
}

/*
 * Subroutine:	disp_dispbox
 * Purpose:	Redraw the dispbox window display including cursor graphics
 */
void disp_dispbox ( )
{
  void disp_window(), disp_regions(), disp_cursor();

  /* put up the image */
  disp_window(&dispbox);
  /* make sure image is up before proceeding */
  XSync(dispbox.display, 0);
  /* redraw the region outlines (if visible is indicated) */
  disp_regions(&cursor);
  /* redraw the cursor (if visible is indicated) */
  disp_cursor(&cursor);
}

/*
 * Subroutine:	map_dispbox
 * Purpose:	Redraw the main display buffer from the short image buffer
 */
void map_dispbox ( )
{
  void unset_blink(), clear_margins(), map_buf_repzoom(), map_buf_subzoom();
  void map_buf_repzoom_adj(), make_halftone_display(), map_buf_subzoom_adj();

  /* restore original buffer if we were blinking */
  unset_blink();
  if( color.ncolors <= 1 ) {
    /* contstruct XYBitmap image */
    dispbox.image.format = XYBitmap;
    dispbox.image.depth = 1;
    dispbox.image.bits_per_pixel = 1;
    dispbox.image.bytes_per_line = (dispbox.image.width + 7) / 8;
    make_halftone_display();
  } else {
    /* construct ZPixmap image */
    dispbox.image.format = ZPixmap;
    dispbox.image.depth = color.screen_depth;
    dispbox.image.bits_per_pixel = 8;
    dispbox.image.bytes_per_line = dispbox.image.width;
    if( coord.bd.clip ) {
      /* if image does not fill display buffer, do something about it */
      clear_margins((unsigned char *)dispbox.image.data, &coord.bd,
		    &coord.disp, 1, color.hard.std_white);
    }
    /* draw the image with the appropriate zoom */
#ifdef SUMBLOCK
    if( buffer.scalemap_summing != buffer.shortbuf_summing ) {
      /* compensate for loading shortbuf with differently summed values */
      if( coord.bd.block < -1 )
	map_buf_repzoom_adj(&coord, (unsigned char *)dispbox.image.data,
			    buffer.shortbuf, buffer.scalemap + SCALEOFF,
			    buffer.scalemap_summing, buffer.shortbuf_summing);
      else
	map_buf_subzoom_adj(&coord, (unsigned char *)dispbox.image.data,
			    buffer.shortbuf, buffer.scalemap + SCALEOFF,
			    MAX(coord.bd.block, 1),
			    buffer.scalemap_summing, buffer.shortbuf_summing);
    } else {
#endif
      /* no special compensation */
      if( coord.bd.block < -1 )
	map_buf_repzoom(&coord, (unsigned char *)dispbox.image.data,
			buffer.shortbuf, buffer.scalemap + SCALEOFF);
      else
	map_buf_subzoom(&coord, (unsigned char *)dispbox.image.data,
			buffer.shortbuf, buffer.scalemap + SCALEOFF,
			MAX(coord.bd.block, 1));
#ifdef SUMBLOCK
    }
#endif
  }
}

/*
 * Subroutine:	clear_margins
 * Purpose:	Take action to indicate unfilled display margins
 */
void clear_margins ( destbuf, ab, bsys, clear, border_color )
     unsigned char *destbuf;		/* destination buffer */
     Edges *ab;				/* mapping parameters */
     Coordsys *bsys;			/* destination parameters */
     int clear;				/* clear unused part of buf */
     register int border_color;		/* color for outline */
{
  register int dest_width;
  int x1, x2, y1, y2;
  short dest_Xwdth;

  dest_width = bsys->width;
  dest_Xwdth = ab->dstXwdth;
  x1 = ab->dstX1;
  x2 = bsys->X2i - ab->dstX2;
  y1 = ab->dstY1;
  y2 = bsys->Y2i - ab->dstY2;

  /* if unused buffer is to be cleared */
  if( clear ) {
    /* if image leaves gap at top - clear top of buffer */
    if( y1 > 0 ) {
      bzero((char *)destbuf, y1 * dest_width);
    }
    /* if image leaves a gap at the bottom - clear bottom of buffer */
    if( y2 > 0 ) {
      bzero((char *)destbuf + ((y1 + ab->dstYhght) * dest_width),
	    y2 * dest_width);
    }
    /* if image leaves gap on either side */
    if( (x1 > 0) || (x2 > 0) ) {
      register unsigned char *dest;
      register int count;
      register int xtot;

      xtot = x1 + x2;
      dest = destbuf + (y1 * dest_width);
      /* clear first line on left */
      if( x1 > 0 ) {
	bzero((char *)dest, x1);
      }
      dest += x1 + dest_Xwdth;
      count = ab->dstYhght;
      /* repeatedly clear right and left together */
      while( --count > 0 ) {
	/* clear combination of line on right continued into line on left */
	bzero((char *)dest, xtot);
	dest += dest_width;
      }
      /* clear last line on right */
      if( x2 > 0 ) {
	bzero((char *)dest, x2);
      }
    }
  }
  /* if a border is to be drawn on exposed edges */
  if( border_color != DONT_CARE ) {
    if( x1 > 0 ) {
      register unsigned char *dest;
      register int count;

      dest = destbuf + ((y1 * dest_width) + x1 - 1);
      count = ab->dstYhght;
      while( count-- > 0 ) {
	*dest = border_color;
	dest += dest_width;
      }
      ++dest_Xwdth;
      --x1;
    }
    if( x2 > 0 ) {
      register unsigned char *dest;
      register int count;

      dest = destbuf + ((y1 * dest_width) + ab->dstX2 + 1);
      count = ab->dstYhght;
      while (count-- > 0) {
	*dest = border_color;
	dest += dest_width;
      }
      ++dest_Xwdth;
    }
    if( y1 > 0 ) {
      register unsigned char *dest;
      register unsigned char *displimit;

      dest = destbuf + (((y1 - 1) * dest_width) + x1);
      displimit = dest + dest_Xwdth;
      while (dest < displimit) {
	*(dest++) = border_color;
      }
    }
    if( y2 > 0 ) {
      register unsigned char *dest;
      register unsigned char *displimit;

      dest = destbuf + (((ab->dstY2 + 1) * dest_width) + x1);
      displimit = dest + dest_Xwdth;
      while( dest < displimit ) {
	*(dest++) = border_color;
      }
    }
  }
}
