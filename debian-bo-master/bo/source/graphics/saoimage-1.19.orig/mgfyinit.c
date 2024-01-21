#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mgfyinit.c (Magnify Initialize)
 * Purpose:	Initialize parameters for drawing the zoomed piece of image
 *		in the magnifier box
 * Subroutine:	init_magnifier()			returns: void
 * Subroutine:	set_magnifier()				returns: void
 * Subroutine:	set_magnifier_dither()
 * Xlib calls:	XTextWidth()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 6 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MAX, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */
#include "hfiles/scale.h"	/* get SCALEOFF */
#include "hfiles/magnify.h"	/* magnifier quick access structure */

extern struct magRec magset;

/*
 * Subroutine:	init_magnifier
 * Purpose:	Init that which gets set only once, and then the rest as well
 * Xlib calls:	XTextWidth()
 */
void init_magnifier ( scope_track, text_track )
     int scope_track, text_track;
{
  XFontStruct *get_fontstruct();
  void set_magnifier();

  /* set initial tracking status */
  magset.view = scope_track;
  magset.label = text_track;
  /* default magnification factor */
  magset.magnify = 4;
  /* set window known info */
  magset.win.display = magnibox.display;
  magset.win.ID = magnibox.ID;
  magset.win.x = magnibox.xzero;
  magset.win.y = magnibox.yzero;
  magset.image = &magnibox.image;
  magset.gcset_disp = &color.gcset.disp;
  magset.gcset_aim = &color.gcset.menu;
  /* set pointer to scalemap for signed indexing */
  magset.lookup = buffer.scalemap + SCALEOFF;
  /* set initial coords to default to display center */
  magset.buf.X = -1.0;
  /* note that buffer is not yet declared */
  magset.data_size = 0;
  /* set up initial test label params */
  magset.text.fontstruct = get_fontstruct(1);
  magset.text.font = magset.text.fontstruct->fid;
  magset.text.space = XTextWidth(magset.text.fontstruct, " ", 1);
  /* check for proportional fonts */
  if( XTextWidth(magset.text.fontstruct, "0", 1) != magset.text.space ) {
    char num[2];
    int i, max = 0;
    for( i=0; i<10; i++ ) {
      num[0] = '0' + i;
      if( (magset.text.numsz[i] =
	   XTextWidth(magset.text.fontstruct, num, 1)) > max )
	max = magset.text.numsz[i];
    }
    magset.text.dash = XTextWidth(magset.text.fontstruct, "-", 1);
    magset.text.dot = XTextWidth(magset.text.fontstruct, ".", 1);
    magset.text.e = XTextWidth(magset.text.fontstruct, "e", 1);
    magset.text.width = 6 * max;
    /* this value may need to be tweeked */
    magset.text.x_x = 3;
    magset.text.y_x = magset.text.x_x + (7 * max);
    magset.text.val_x = magset.text.y_x + (7 * max);
    magset.text.proportional = 1;
  } else {
    /* uniform width font */
    magset.text.width = 23;
    magset.text.x_xoff = 5;
    magset.text.x_x = 5;
    magset.text.proportional = 0;
  }
  magset.text.yoff =
    (2 * magset.text.fontstruct->descent) + magset.text.fontstruct->ascent;
  magset.text.foreground = color.hard.std_white;
  magset.text.background = color.hard.std_black;
  magset.matrix = color.halftone.matrix;
  magset.matrix_end = magset.matrix + 256;
  magset.inverse = color.halftone.inverse;
  set_magnifier();
}

/*
 * Subroutine:	set_magnifier()
 * Purpose:	Set up initial parameters for magnifier box
 * Use:		Call whenever the magnifier box is resized or the image buffer
 *		is altered (reloaded or altered)
 */
void set_magnifier ( )
{
  int buf_sz;
  float hot_x, hot_y;
  char *calloc_errchk();
  void init_magnifier_XYmark(), init_magnifier_Zmark();

  /* set source buffer */
  magset.buf.shortbuf = buffer.shortbuf;
  magset.buf.width = coord.buf.width;
  /* set current window drawing area parameters */
  magset.win.width = magnibox.xwidth;
  magset.win.height = magnibox.yheight;
  if( color.ncolors <= 1 ) {
    magset.halftone = 1;
    magset.bytes_per_line = (magset.win.width + 7) / 8;
    magset.image->format = XYBitmap;
    magset.image->depth = 1;
    magset.image->bits_per_pixel = 1;
  } else {
    magset.halftone = 0;
    magset.bytes_per_line = magset.win.width;
    magset.image->format = ZPixmap;
    magset.image->depth = color.screen_depth;
    magset.image->bits_per_pixel = 8;
  }
  buf_sz = magset.bytes_per_line * (magset.win.height + 1);
  if( buf_sz > magset.data_size ) {
    if( magset.data != NULL )
      free((char *)magset.data);
    /* recoverable alloc (we could settle for less) */
    if( (magset.data = calloc_errchk(buf_sz, sizeof(char), (char *)NULL))
       == 0 ) {
      magset.win.width = 128;
      magset.win.height = 128;
      if( magset.halftone )
	magset.bytes_per_line = (magset.win.width + 7) / 8;
      else
	magset.bytes_per_line = magset.win.width;
      buf_sz = magset.bytes_per_line * (magset.win.height + 1);
      /* this is our minimum request */
      magset.data = calloc_errchk(buf_sz, sizeof(char), "magnifier buffer");
    }
    magset.data_size = buf_sz;
  }
  magset.bitmap_size = magset.bytes_per_line * magset.win.height;
  magset.image->data = magset.data;
  magset.image->bytes_per_line = magset.bytes_per_line;
  magset.image->width = magset.win.width;
  magset.image->height = magset.win.height;
  /* determine where to place aim sighting mark */
  magset.data_x_hot = (magset.win.width - 1) / 2;
  magset.data_y_hot = (magset.win.height - 1) / 2;
  /* disp to magnibox zoom must not be less than 1 (needless extra work) */
  if( magset.magnify < 1 )
    magset.magnify = 1;
  /* calculate buffer to magnifier zoom */
  if( coord.disptobuf.inx_outx != 0.0 ) {
    magset.zoom_rep =
      MAX(1, (int)(0.5 + ((float)magset.magnify / coord.disptobuf.inx_outx)));
  } else if( coord.disptobuf.iny_outx != 0.0 ) {
    magset.zoom_rep =
      MAX(1, (int)(0.5 + ((float)magset.magnify / coord.disptobuf.iny_outx)));
  } else
    magset.zoom_rep = magset.magnify;
  /* define the margin regions (that clip against an edge) */
  hot_x = 0.5 + (float)((magset.win.width - 1) / 2);
  hot_y = 0.5 + (float)((magset.win.height - 1) / 2);
  magset.buf.Xcen = hot_x / (float)magset.zoom_rep;
  magset.buf.X2bdr = (float)coord.buf.width - magset.buf.Xcen;
  magset.buf.X2lim = (float)coord.buf.width + magset.buf.Xcen;
  magset.buf.X1lim =
    (hot_x - (float)magset.win.width) / (float)magset.zoom_rep;
  magset.buf.Ycen = hot_y / (float)magset.zoom_rep;
  magset.buf.Y2bdr = (float)coord.buf.height - magset.buf.Ycen;
  magset.buf.Y2lim = (float)coord.buf.height + magset.buf.Ycen;
  magset.buf.Y1lim =
    (hot_y - (float)magset.win.height) / (float)magset.zoom_rep;
  init_magnifier_XYmark();
  init_magnifier_Zmark();
  /* set parameters for text label */
  magset.text.y = btnbox.y - (magset.text.yoff + 2);
#ifdef FOO /* this would center the label, else left justify */
  if( magset.text.proportional == 0 )
    magset.text.x_x = (panbox.x - (magset.text.width * magset.text.space)) / 2;
#endif
}

/*
 * Subroutine:	set_magnifier_matrix
 * Purpose:	Change the dither matrix used by scope
 */
void set_magnifier_matrix ( matrix, inverse )
     short *matrix;
     int inverse;
{
  void redraw_magnifier();
  if( (magset.matrix != matrix) || (magset.inverse != inverse) ) {
    magset.matrix = matrix;
    magset.matrix_end = matrix + 256;
    magset.inverse = inverse;
    redraw_magnifier();
  }
}
                                                  

                                                               
                                                               
                                                               
                                                               
                     
