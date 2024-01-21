#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mgfydraw.c (Magnify Draw)
 * Purpose:	Quickly draw the zoomed piece of image in the zoom box
 * Subroutine:	draw_magnifier()			returns: void
 * Copyright:	1988, 1991, 1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	      3 December 1988
 *		{1} MVH improved limit checking, added comments	   1 Jan 1991
 *		{2} MVH same fix as 1, for bitmap display	  21 Jun 1991
 *		{3} Doug Mink  cast valbuf to short for comp	   4 May 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* define GCspec */
#include "hfiles/magnify.h"	/* magnifier quick access structure */

struct magRec magset;


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		draw_magnifier(	double bufx, double bufy );

#else

void draw_magnifier();
GC set_gc(), set_gc_with_background();
void blank_scope(), mark_Zmagnifier(), mark_XYmagnifier();

#endif


/* Subroutine:	draw_magnifier
 * Purpose:	Draw image in zoombox, quickly (use magset parameters)
 */
#ifdef ANSIC
void draw_magnifier( double bufx, double bufy )
#else
void draw_magnifier ( bufx, bufy )
     double bufx, bufy;
#endif
{
  int magXwdth, magYhght;	/* Dimensions of destination area drawn */
  int magX1, magY1;		/* Upper left coords of destination drawn */
  int bufX1, bufY1;		/* Upper left coords of source area */
  int Xrep1, Yrep1;		/* Repetitions of first pixel (on UL edge) */
  int margin;
  int imgjump;			/* Memory offset from end to start of next */
  /* Variables used in display loop */
  int rep;
  unsigned char *lookup;
  unsigned char *magline;
  int yline;
  int yrepend;

  /* Check for special handling */
  margin = 0;
  /* Check for being completely beyoind buffer and set initial dimensions */
  if( bufx < magset.buf.X1lim ) {
    blank_scope();
    return;
  }
  if( bufx <= magset.buf.X2bdr )
    magXwdth = magset.win.width;
  else {
    if( bufx > magset.buf.X2lim ) {
      blank_scope();
      return;
    }
    /* Note rounding for a negative number */
    magXwdth = magset.win.width +
      (int)(((magset.buf.X2bdr - bufx) * (float)magset.zoom_rep) - 0.5);
    margin = 1;
  }
  if( bufy < magset.buf.Y1lim ) {
    blank_scope();
    return;
  }
  if( bufy <= magset.buf.Y2bdr )
    magYhght = magset.win.height;
  else {
    if( bufy > magset.buf.Y2lim ) {
      blank_scope();
      return;
    }
    /* Note rounding for a negative number */
    magYhght = magset.win.height +
      (int)(((magset.buf.Y2bdr - bufy) * (float)magset.zoom_rep) - 0.5);
    margin = 1;
  }
  /* Convert to buffer coordinates, and calculate partial offsets */
  bufx -= magset.buf.Xcen;
  if( bufx >= 0.0 ) {
    int xrepoff;

    /* Left edge of magnifier within buffer */
    magX1 = 0;
    bufX1 = (int)bufx;
    /* Number of replications of first pixel is less fractional overlap */
    xrepoff = (int)((bufx - (float)bufX1) * magset.zoom_rep);
    if( xrepoff < magset.zoom_rep ) {
      Xrep1 = magset.zoom_rep - xrepoff;
      imgjump = magset.buf.width -
	(1 + ((magXwdth + xrepoff - 1) / magset.zoom_rep));
    } else {
      /* If rounds to next edge, skip first pixel */
      bufX1 += 1;
      Xrep1 = magset.zoom_rep;
      imgjump = magset.buf.width - (1 + ((magXwdth - 1) / magset.zoom_rep));
    }
  } else {
    /* Left edge of magnifier beyond buffer */
    magX1 = -(int)(0.5 + (bufx * magset.zoom_rep));
    magXwdth -= magX1;
    bufX1 = 0;
    Xrep1 = magset.zoom_rep;
    imgjump = magset.buf.width - (1 + ((magXwdth - 1) / magset.zoom_rep));
    margin = 1;
  }
  bufy -= magset.buf.Ycen;
  if( bufy >= 0.0 ) {
    /* Top edge of magnifier within buffer */
    magY1 = 0;
    bufY1 = (int)bufy;
    /* Number of replications of first pixel is less fractional overlap */
    Yrep1 = magset.zoom_rep -
      (int)((bufy - (float)bufY1) * (float)magset.zoom_rep);
    /* If rounds to next edge, skip first pixel */
    if( Yrep1 <= 0 ) {
      bufY1 += 1;
      Yrep1 = magset.zoom_rep;
    }
  } else {
    /* Top edge of magnifier beyond buffer */
    magY1 = -(int)(0.5 + (bufy * (float)magset.zoom_rep));
    magYhght -= magY1;
    bufY1 = 0;
    Yrep1 = magset.zoom_rep;
    margin = 1;
  }
  if( !magset.halftone ) {
    /* Variables used only in color loop */
    int magjump;
    register unsigned char *magbuf;
    if( margin == 1 ) {
      /* Image doesn't cover entire field, blank field, compute offset start */
      bzero(magset.data, magset.data_size);
      magbuf = (unsigned char *)magset.data + magX1 +
	(magset.win.width * magY1);
    } else {
      /* Simple start at top left corner of magnibox buffer */
      magbuf = (unsigned char *)magset.data;
    }
    {
      register unsigned char *xrowend;
      register unsigned char *xrepend;
      register short *imgbuf;

      /* Compute start in data buffer, get scaling lookup table */
      imgbuf = magset.buf.shortbuf + bufX1 + (bufY1 * magset.buf.width);
      lookup = magset.lookup;
      magjump = magset.win.width - magXwdth;
      rep = magset.zoom_rep;
      yline = 0;
      yrepend = Yrep1;
      while( yline < magYhght ) {
	magline = magbuf;
	/* Create row of magnibuf from image buffer data */
	xrowend = magbuf + magXwdth;
	xrepend = magbuf + Xrep1;
	while( magbuf < xrowend ) {
	  unsigned char val;

	  val = lookup[*imgbuf++];
	  /* Make sure repetition does not run beyond end */
	  if( xrepend > xrowend )
	    xrepend = xrowend;
	  while( magbuf < xrepend )
	    *magbuf++ = val;
	  xrepend += rep;
	}
	magbuf += magjump;
	imgbuf += imgjump;
	/* Make sure repetition does not run beyond end */
	if( yrepend > magYhght )
	  yrepend = magYhght;
	/* Copy original row for rep'd lines (magnification in Y direction) */
	while( ++yline < yrepend ) {
	  register unsigned char *copy;

	  copy = magline;
	  while( copy < xrowend )
	    *magbuf++ = *copy++;
	  magbuf += magjump;
	}
	yrepend += rep;
      }
    }
    {
      GC gc;
      /* Install the sighting mark */
      mark_Zmagnifier();
      gc = set_gc(magset.gcset_disp);
      XPutImage(magset.win.display, magset.win.ID, gc, magset.image,
		0, 0, magset.win.x, magset.win.y,
		magset.win.width, magset.win.height);
    }
  } else {
    /* Variables used only in bitmap loop */
    unsigned char *bitmap_row;
    short *matrix_row;
    int first_bit;
    unsigned char *valbuf;
    unsigned char linebuf[1024];

    /* Blank field */
    bzero((char *)magset.data, magset.bitmap_size);
    if( margin == 1 ) {
      /* Image doesn't cover entire field - compute offset start  */
      bitmap_row = (unsigned char *)
	magset.data + (magX1 / 8) + (magset.bytes_per_line * magY1);
      first_bit = magX1 & 7;
    } else {
      /* Simple start at top left corner of magnibox buffer */
      bitmap_row = (unsigned char *)magset.data;
      first_bit = 0;
    }
    {
      register unsigned char *bitmap_byte_or_xrepend;
      register unsigned char *xrowend;
      register short *imgbuf;

      /* Compute start in data buffer, get scaling lookup table */
      imgbuf = magset.buf.shortbuf + bufX1 + (bufY1 * magset.buf.width);
      lookup = magset.lookup;
      rep = magset.zoom_rep;
      matrix_row = magset.matrix;
      yline = 0;
      xrowend = linebuf + magXwdth;
      yrepend = Yrep1;
      while( yline < magYhght ) {
	/* Load scaled image data into temporary value buffer */
	valbuf = linebuf;
	bitmap_byte_or_xrepend = valbuf + Xrep1;
	while( valbuf < xrowend ) {
	  unsigned char val;

	  val = lookup[*imgbuf++];
	  if( bitmap_byte_or_xrepend > xrowend )
	    bitmap_byte_or_xrepend = xrowend;
	  while( valbuf < bitmap_byte_or_xrepend )
	    *valbuf++ = val;
	  bitmap_byte_or_xrepend += rep;
	}
	imgbuf += imgjump;
	/* Copy data line row for rep'd lines, using dither mask */
	bitmap_byte_or_xrepend = bitmap_row;
	if( magset.inverse ) {
	  if( yrepend > magYhght )
	    yrepend = magYhght;
	  while( yline < yrepend ) {
	    register short *matrix;
	    register int bitmap_bit;

	    /* Count lines when actually written */
	    yline++;
	    /* Set markers for start of a line */
	    bitmap_bit = first_bit;
	    valbuf = linebuf;
	    /* Point mask at start of dither mtrx row (each row is 16 wide) */
	    matrix = matrix_row;
	    while( valbuf < xrowend ) {
	      /* Set bit by comparing val to matrix entry */
	      if( (short)*valbuf++ <= matrix[bitmap_bit] ) 
		*bitmap_byte_or_xrepend |= 1 << bitmap_bit;
	      /* Check for next bitmap word */
	      if( ++bitmap_bit >= 8 ) {
		bitmap_bit = 0;
		++bitmap_byte_or_xrepend;
		if( matrix == matrix_row )
		  matrix += 8;
		else
		  matrix = matrix_row;
	      }
	    }
	    if( (matrix_row += 16) >= magset.matrix_end )
	      matrix_row = magset.matrix;
	    bitmap_byte_or_xrepend = (bitmap_row += magset.bytes_per_line);
	  }
	  yrepend += rep;
	} else {
	  if( yrepend > magYhght )
	    yrepend = magYhght;
	  while( yline < yrepend ) {
	    register short *matrix;
	    register int bitmap_bit;

	    /* Count lines when actually written */
	    yline++;
	    /* Set markers for start of a line */
	    bitmap_bit = first_bit;
	    valbuf = linebuf;
	    /* Point mask at start of dither mtrx row (each row is 16 wide) */
	    matrix = matrix_row;
	    while( valbuf < xrowend ) {
	      /* Set bit by comparing val to matrix entry */
	      if( (short)*valbuf++ > matrix[bitmap_bit] ) 
		*bitmap_byte_or_xrepend |= 1 << bitmap_bit;
	      /* Check for next bitmap word */
	      if( ++bitmap_bit >= 8 ) {
		bitmap_bit = 0;
		++bitmap_byte_or_xrepend;
		if( matrix == matrix_row )
		  matrix += 8;
		else
		  matrix = matrix_row;
	      }
	    }
	    if( (matrix_row += 16) >= magset.matrix_end )
	      matrix_row = magset.matrix;
	    bitmap_byte_or_xrepend = (bitmap_row += magset.bytes_per_line);
	  }
	  yrepend += rep;
	}
      }
    }
    {
      GC gc;
      /* Install the sighting mark */
      mark_XYmagnifier();
      gc = set_gc_with_background(magset.gcset_disp,
				  magset.gcset_disp->background);
      XPutImage(magset.win.display, magset.win.ID, gc, magset.image,
		0, 0, magset.win.x, magset.win.y,
		magset.win.width, magset.win.height);
    }
  }
}
