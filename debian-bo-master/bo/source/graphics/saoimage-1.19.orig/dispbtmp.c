#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	dispbtmp.c (Display Bitmap)
 * Purpose:	Routines to map 16 bit data to a single plane bitmap
 * Subroutine:	init_halftone()			returns: void
 * Subroutine:	select_halftone()		returns: void
 * Subroutine:	make_halftone_panimage()	returns: void
 * Subroutine:	make_halftone_display()		returns: void
 * Xlib calls:	none
 * Copyright:	1987, 1988, 1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     31 December 1987
 *		{1} MVH signed scalemap, some new names	      3 December 1988
 *		{2} MVH new color structure for use with X11	  10 May 1989
 *		{3} MVH update on partial display buffer     24 November 1989
 *		{4} MVH changed calls for replcate zoom		 21 June 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/constant.h"		/* define codes */
#include "hfiles/define.h"		/* MIN, MAX and more */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */
#include "hfiles/scale.h"		/* scaling parameters & struct */
#include "defs/dither.def"		/* halftoning tool buffers */

/*
 * Subroutine:	init_halftone
 * Purpose:	Initialize color struct pointers for halftoning
 */
void init_halftone ()
{
  color.halftone.errbuf = &errbuf[0];
  switch( color.halftone.matrixID ) {
  case BOP_Matrix2:
    color.halftone.matrix = &dotmatrix2[0][0];
    break;
  case BOP_Matrix1:
  default:
    color.halftone.matrix = &dotmatrix1[0][0];
    break;
  }
}

/*
 * Subroutine:	select_halftone
 * Purpose:	Respond to halftone selection command
 */
void select_halftone ()
{
  static void new_halftone();

  switch( control.response[1] ) {
  case 0:
    /*0 is main menu selection, change only main mode */
    control.mode = BOP;
    break;
  case BOP_Invert:
    /* invert is a toggle, switch cursor colors, redraw disp and pan boxes */
    color.halftone.inverse = !color.halftone.inverse;
    new_halftone(1);
    break;
  case BOP_Diffuse:
    if( color.halftone.mode != BOP_Diffuse ) {
      color.halftone.mode = BOP_Diffuse;
      new_halftone(0);
    }
    break;
  case BOP_Matrix1:
    if( (color.halftone.mode != BOP_Dither) ||
        (color.halftone.matrix != &dotmatrix1[0][0]) ) {
      color.halftone.mode = BOP_Dither;
      color.halftone.matrixID = BOP_Matrix1;
      color.halftone.matrix = &dotmatrix1[0][0];
      new_halftone(1);
    }
    break;
  case BOP_Matrix2:
    if( (color.halftone.mode != BOP_Dither) ||
        (color.halftone.matrix != &dotmatrix2[0][0]) ) {
      color.halftone.mode = BOP_Dither;
      color.halftone.matrixID = BOP_Matrix2;
      color.halftone.matrix = &dotmatrix2[0][0];
      new_halftone(1);
    }
    break;
  }
}

/*
 * Subroutine:	new_halftone
 * Purpose:	Redo halftone images of both dispbox and panbox
 */
static void new_halftone ( magnifier )
     int magnifier;	/* i: include-magnifier */
{
  void make_halftone_display(), disp_dispbox(), disp_panbox(), draw_colorbar();
  void set_magnifier_matrix(), panimage_halftone(), map_halftone_colorbar();

  map_halftone_colorbar( 1, 0 );
  if( magnifier )
    set_magnifier_matrix(color.halftone.matrix, color.halftone.inverse);
  make_halftone_display();
  disp_dispbox();
  panimage_halftone();
  disp_panbox();
  draw_colorbar(0);
}

/*
 * Subroutine:	make_halftone_panimage
 * Purpose:	Make halftone bitmap for pan window (panbox)
 */
void panimage_halftone ( )
{
  void dither_sample(), diffuse_sample();

  bzero(panbox.image.data, panbox.image.bytes_per_line * panbox.yheight);
  if( color.halftone.mode == BOP_Dither ) {
    dither_sample(buffer.panbuf, panbox.xwidth, 1,
		  (unsigned char *)panbox.image.data,
		  panbox.image.bytes_per_line,
		  0, 0, panbox.xwidth, panbox.yheight,
		  buffer.scalemap + SCALEOFF, color.halftone.matrix,
		  color.halftone.inverse);
  } else if( color.halftone.mode == BOP_Diffuse ) {
    diffuse_sample(buffer.panbuf, panbox.xwidth, 1,
		   (unsigned char *)panbox.image.data,
		   panbox.image.bytes_per_line,
		   0, 0, panbox.xwidth, panbox.yheight,
		   buffer.scalemap + SCALEOFF, color.halftone.errbuf,
		   color.halftone.inverse);
  } 
}

/*
 * Subroutine:	make_halftone_display
 * Purpose:	Make halftone bitmap for display window (dispbox)
 */
void make_halftone_display ( )
{
  short *shortimage;
  void diffuse_sample(), diffuse_replicate();
  void dither_sample(), dither_replicate();
  
  /* point to where the image begins in the image BUFFER */
  shortimage = buffer.shortbuf +
    (coord.bd.srcY1 * coord.buf.width) + coord.bd.srcX1;
  /* zero out display buffer for lines to draw (will | with existing bits) */
  if( coord.bd.clip )
    /* if leaving space due to panning/zooming, zero everything */
    bzero(dispbox.image.data, dispbox.image.bytes_per_line * dispbox.yheight);
  else
    bzero(dispbox.image.data + (coord.bd.dstY1 * dispbox.image.bytes_per_line),
	  coord.bd.dstYhght * dispbox.image.bytes_per_line);
  /* halftone black and white images */
  if( coord.bd.block < 0 ) {
    int xrep1, yrep1;
    if( coord.bd.dst_x < 0 )
      xrep1 = coord.bd.dst_x + ((coord.bd.src_x + 1) * (-coord.bd.block));
    else
      xrep1 = -coord.bd.block;
    if( coord.bd.dst_y < 0 )
      yrep1 = coord.bd.dst_y + ((coord.bd.src_y + 1) * (-coord.bd.block));
    else
      yrep1 = -coord.bd.block;
    if( color.halftone.mode == BOP_Diffuse ) {
      diffuse_replicate(shortimage, coord.buf.width, -coord.bd.block,
			(unsigned char *)dispbox.image.data,
			dispbox.image.bytes_per_line, xrep1, yrep1,
			coord.bd.dstX1, coord.bd.dstY1,
			coord.bd.dstXwdth-1, coord.bd.dstYhght-1,
			buffer.scalemap + SCALEOFF, color.halftone.errbuf,
			color.halftone.inverse);
    } else {
      dither_replicate(shortimage, coord.buf.width, -coord.bd.block,
		       (unsigned char *)dispbox.image.data,
		       dispbox.image.bytes_per_line, xrep1, yrep1,
		       coord.bd.dstX1, coord.bd.dstY1,
		       coord.bd.dstXwdth-1, coord.bd.dstYhght-1,
		       buffer.scalemap + SCALEOFF, color.halftone.matrix,
		       color.halftone.inverse);
    }
  } else {
    if( color.halftone.mode == BOP_Diffuse ) {
      diffuse_sample(shortimage, coord.buf.width, coord.bd.block,
		     (unsigned char *)dispbox.image.data,
		     dispbox.image.bytes_per_line, 
		     coord.bd.dstX1, coord.bd.dstY1,
		     coord.bd.dstXwdth, coord.bd.dstYhght,
		     buffer.scalemap + SCALEOFF, color.halftone.errbuf,
		     color.halftone.inverse);
    } else {
      dither_sample(shortimage, coord.buf.width, coord.bd.block,
		    (unsigned char *)dispbox.image.data,
		    dispbox.image.bytes_per_line,
		    coord.bd.dstX1, coord.bd.dstY1,
		    coord.bd.dstXwdth, coord.bd.dstYhght,
		    buffer.scalemap + SCALEOFF, color.halftone.matrix,
		    color.halftone.inverse);
    }
  }
}
