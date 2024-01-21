#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	sclctrl.c (Scale Control)
 * Purpose:	Construct the image-to-display map
 * Subroutine:	select_scalemap()		returns: void
 * Subroutine:	new_scalemap()			returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *		{1} MVH enable setting one limit w/o other   24 November 1989
 *		{2} MVH use full area for small image histogram  19 June 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes  */
#include "hfiles/define.h"	/* define MIN, MAX, DONT_CARE, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/scale.h"	/* define scaling constants */

#ifdef ANSIC
static void new_histogram();
static void get_histlims ( int *histogram, int *pmin, int *pmax );
static int get_histogram_total ( int *histogram, int pmin, int pmax );
static void make_histogram ( short *shortbuf, int *histogram, int ncols,
			     int x, y, width, height, int *area );
#else
static void new_histogram();
static void get_histlims();
static int get_histogram_total();
static void make_histogram();
#endif

/*
 * Subroutine:	select_scalemap
 * Purpose:	Respond to input from the scale menu (mostly rescaling)
 */
void select_scalemap ()
{
  void new_scalemap(), save_blink();
  void map_panbox(), disp_panbox(), map_dispbox(), disp_dispbox();

  if( (control.response[0] == SOP) && (control.response[1] != 0) ) {
    if( control.response[1] == SOP_Blink ) {
      save_blink((int)control.event.xbutton.button);
    } else {
      color.scale.mode = control.response[1];
      /* make the new lookup table */
      new_scalemap();
      /* refill display buffers with rescaled values and display */
      map_panbox();
      disp_panbox();
      /* panbox first because it is faster */
      map_dispbox();
      disp_dispbox();
    }
  }
}

/*
 * Subroutine:	new_scalemap
 * Purpose:	Scalemap calculation for a new image
 */
void new_scalemap ( )
{
  static double cmdMin = 0.0;
  static double cmdMax = 0.0;
  static int min_given = 0;	/* stored values to detect change */
  static int max_given = 0;
  static int imtool_200 = 0;
  static int init = 1;		/* identify first time called */
  static void new_histogram();
  void make_scalemap(), color_logo();
#ifdef IMTOOL
  void set_imtool_colors();
#endif

  /* if the image has been changed, recompute min, max, and histogram */
  if( (buffer.mm.img_leftX != coord.id.srcX1) ||
      (buffer.mm.img_topY != coord.id.srcY1) ||
      (buffer.mm.img_rightX != coord.id.srcX2) ||
      (buffer.mm.img_lowY != coord.id.srcY2) ||
      (img.imtool_200 != imtool_200) ||
      (min_given != buffer.min_given) || (cmdMin != buffer.cmdMin) ||
      (max_given != buffer.max_given) || (cmdMax != buffer.cmdMax) ) {
    new_histogram();
    min_given = buffer.min_given;
    max_given = buffer.max_given;
    cmdMin = buffer.cmdMin;
    cmdMax = buffer.cmdMax;
    imtool_200 = img.imtool_200;
  }
  /* make the scale map */
  if( color.ncolors == 1 ) {
    color.ncolors = 256;
    make_scalemap(buffer.scale_min, buffer.scale_max);
    color.ncolors = 1;
  } else {
    make_scalemap(buffer.scale_min, buffer.scale_max);
    /* if this is imtool, map its graphics image levels */
    if( init && (img.file_type == SOP_Logo) )
      color_logo(buffer.scalemap + SCALEOFF,
		 color.hard.blue, color.hard.yellow);
#ifdef IMTOOL
    else if( img.imtool_200 )
      set_imtool_colors();
#endif
    init = 0;
  }
}

/*
 * Subroutine:	new_histogram
 * Purpose:	Do all coordinate stuff surrounding histogram calculation
 *		and call make_histogram
 */
static void new_histogram ( )
{
  float X1, Y1, X2, Y2;
  void i_transform();

  /* calculate min, max, and histogram but not completely to the edges */
  if( (coord.buf.Xwdth > 32) && (coord.buf.Yhght > 32) ) {
    if( control.verbose )
      (void)printf("Assessment of window display (in 1 from edges):\n");
    i_transform(&coord.imgtobuf,
		coord.id.srcX1 + 1, coord.id.srcY1 + 1, &X1, &Y1);
    i_transform(&coord.imgtobuf,
		coord.id.srcX2 - 1, coord.id.srcY2 - 1, &X2, &Y2);
  } else {
    if( control.verbose )
      (void)printf("Assessment of window display:\n");
    i_transform(&coord.imgtobuf, coord.id.srcX1, coord.id.srcY1, &X1, &Y1);
    i_transform(&coord.imgtobuf, coord.id.srcX2, coord.id.srcY2, &X2, &Y2);
  }
  make_histogram(buffer.shortbuf, buffer.histogram, coord.buf.width,
		 (int)X1, (int)Y1, 1+(int)X2-(int)X1, 1+(int)Y2-(int)Y1,
		 &buffer.hist_area);
  get_histlims(buffer.histogram, &buffer.scale_min, &buffer.scale_max);
  if( control.verbose ) {
    if( img.fiscaled == 0 ) {
      (void)printf("Pixel minimum = %d, pixel maximum = %d\n",
		   buffer.scale_min, buffer.scale_max);
    } else {
      float fmin, fmax;
      fmin = (buffer.scale_min * img.fiscale) + img.fibias;
      fmax = (buffer.scale_max * img.fiscale) + img.fibias;
      (void)printf("Pixel minimum = %g, pixel maximum = %g\n", fmin, fmax);
#ifdef DEBUG
      (void)printf("(unscaled - min: %d, max: %d)\n",
		   buffer.scale_min, buffer.scale_max);
#endif
    }
  }
  /* store info about this calculation for future reference */
  buffer.mm.img_leftX = coord.id.srcX1;
  buffer.mm.img_topY = coord.id.srcY1;
  buffer.mm.img_rightX = coord.id.srcX2;
  buffer.mm.img_lowY = coord.id.srcY2;
  buffer.scalemap_summing = buffer.shortbuf_summing;
  /* allow command line limit override */
  if( buffer.min_given || buffer.max_given ) {
    int histmin, histmax;
    if( buffer.min_given ) {
      if( img.fiscaled )
	buffer.cmdmin = (buffer.cmdMin - img.fibias) / img.fiscale;
      else
	buffer.cmdmin = buffer.cmdMin;
      histmin = MAX(buffer.cmdmin, buffer.scale_min);
      buffer.scale_min = buffer.cmdmin;
      if( control.verbose )
	(void)printf("  min clipped at %g", buffer.cmdMin);
    } else
      histmin = buffer.scale_min;
    if( buffer.max_given ) {
      if( img.fiscaled )
	buffer.cmdmax = (buffer.cmdMax - img.fibias) / img.fiscale;
      else
	buffer.cmdmax = buffer.cmdMax;
      histmax = MIN(buffer.cmdmax, buffer.scale_max);
      buffer.scale_max = buffer.cmdmax;
      if( control.verbose )
	(void)printf("  max clipped at %g for scaling\n", buffer.cmdMax);
    } else {
      histmax = buffer.scale_max;
      (void)printf(" for scaling\n");
    }
    buffer.hist_area = get_histogram_total(buffer.histogram, histmin, histmax);
  }
}

/*
 * Subroutine:	make_histogram
 * Purpose:	Fill in a data histogram for specified buffer subsection
 */
#ifdef ANSIC
static void make_histogram ( short *shortbuf, int *histogram, int ncols,
			     int x, y, width, height, int *area )
{
  *area = width * height;
  memset((void *)histogram, 0, SCALEBUFSZ * sizeof(int));
#else
static void
make_histogram ( shortbuf, histogram, ncols, x, y, width, height, area )
     short *shortbuf;			/* data buffer */
     int *histogram;			/* histogram buffer */
     int ncols;				/* width of data buffer */
     int x, y, width, height;		/* subsection specification */
     int *area;			/* returned values */
{
  *area = width * height;
  bzero((char *)histogram, SCALEBUFSZ * sizeof(int));
#endif
  {
    register short *buf;
    register short *bufend;
    register int *hist;

    /* initialize min and max */
    buf = shortbuf + (y * ncols) + x;
    /* offset to zero (+32768 or 0x8000) */
    hist = histogram + SCALEOFF;
    /* if we are reading an entire array, use a simple approach */
    if( ncols == width ) {
      bufend = buf + (*area);
      while( buf < bufend ) {
	++hist[*buf++];
      }
    } else {
      register short *lineend, *bufend;
      register int jump;

      /* if we are reading part of an array, allow for partial line */
      lineend = buf + width;
      jump = ncols - width;
      bufend = buf + (height * ncols);
      while( buf < bufend ) {
	while( buf < lineend ) {
	  ++hist[*buf++];
	}
	buf += jump;
	lineend += ncols;
      }
    }
  }
}

/*
 * Subroutine:	get_histlims
 * Purpose:	Find the min and max non-zero entries in the histogram
 */
#ifdef ANSIC
static void get_histlims ( int *histogram, int *pmin, int *pmax )
#else
static void get_histlims ( histogram, pmin, pmax )
     int *histogram;
     int *pmin;
     int *pmax;
#endif
{
  register int *hist;
  register int i, limit;

  /* offset hist to zero (+32768 or 0x8000)  */
  hist = histogram + SCALEOFF;
  /* count up to first value for min */
  limit = SCALEMAX;
  for( i = SCALEMIN; ((i <= limit) && (hist[i] == 0)); i++ );
  *pmin = i;
  /* count down to last value for max */
  limit = SCALEMIN;
  for( i = SCALEMAX; ((i >= limit) && (hist[i] == 0)); i-- );
  *pmax = i;
}

/*
 * Subroutine:	get_histogram_total
 * Purpose:	Find the min and max non-zero entries in the histogram
 */
#ifdef ANSIC
static int get_histogram_total ( int *histogram, int pmin, int pmax )
#else
static int get_histogram_total ( histogram, pmin, pmax )
     int *histogram;
     int pmin;
     int pmax;
#endif
{
  register int *hist;
  register int *end;
  register int count;

  /* offset hist to zero (+32768 or 0x8000)  */
  hist = histogram + SCALEOFF;
  end = hist + pmax;
  hist -= pmin;
  count = 0;
  do {
    count += (int)(*hist);
  } while( ++hist <= end );
  return( count );
}
