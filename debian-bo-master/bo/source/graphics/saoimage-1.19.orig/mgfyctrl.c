#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mgfyctrl.c (Magnify Control)
 * Purpose:	Initialize params and organize drawing the magnifier window
 * Subroutine:	magnify_disp()			returns: void
 * Subroutine:	magnify_pan()			returns: void
 * Subroutine:	clear_coord_area()		returns: void
 * Subroutine:	redraw_magnifier()		returns: void
 * Xlib calls:	XCheckWindowEvent(), XSync()
 * Copyright:	1989,1994,1995,1996 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 6 June 1989
 *		{1} MVH added support for second set of coords	  7 Nov 1989
 *		{2} Doug Mink added support for WCS		 18 Oct 1994
 *		{3} DJM changed WCS arguments           	  6 Jul 1995
 *		{4} DJM overwrite excess chars in WCS area     	 17 Aug 1995
 *		{5} DJM overwrite excess chars in pixel area   	  2 Feb 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */
#include "hfiles/constant.h"	/* codes */
#include "hfiles/magnify.h"	/* magnifier quick access structure */

extern struct windowRec desktop;
extern struct magRec magset;

#define VAL_SZ 10
/*
 * Subroutine:	magnify_disp
 * Purpose:	Magnify location of a dispbox event
 * Xlib calls:	XCheckWindowEvent(), XSync()
 */
void magnify_disp ( event, view, text )
     XEvent *event;		/* i: XEvent for location of mouse */
     int view, text;
{
  void draw_magnifier();
  static void label_file_coords(), label_file_coords_proportional();

  /* get only the most recent mouse moved event */
  XSync(dispbox.display, 0);
  while( XCheckWindowEvent(dispbox.display, dispbox.ID,
			   PointerMotionMask, event) );
  /* draw image fragment in scope magnibox if desired */
  if( view ) {
    /* get buffer coordinates */
    magset.buf.X = ((float)event->xmotion.x * coord.disptobuf.inx_outx) +
      coord.disptobuf.iadd_outx;
    magset.buf.Y = ((float)event->xmotion.y * coord.disptobuf.iny_outy) +
      coord.disptobuf.iadd_outy;
    draw_magnifier((double)magset.buf.X, (double)magset.buf.Y);
    if( text ) {
      if( magset.text.proportional )
	label_file_coords_proportional((double)magset.buf.X,
				       (double)magset.buf.Y);
      else
	label_file_coords((double)magset.buf.X, (double)magset.buf.Y);
    }
  } else if( text ) {
    float bufx, bufy;

    /* get buffer coordinates */
    bufx = ((float)event->xmotion.x * coord.disptobuf.inx_outx) +
      coord.disptobuf.iadd_outx;
    bufy = ((float)event->xmotion.y * coord.disptobuf.iny_outy) +
      coord.disptobuf.iadd_outy;
    if( magset.text.proportional )
	label_file_coords_proportional((double)bufx, (double)bufy);
    else
      label_file_coords((double)bufx, (double)bufy);
  }
}

/*
 * Subroutine:	magnify_pan
 * Purpose:	Magnify location of a panbox event
 * Xlib calls:	XCheckWindowEvent(), XSync()
 */
void magnify_pan ( event )
     XEvent *event;		/* i: XEvent for location of mouse */
{
  void draw_magnifier();

  /* get only the most recent mouse moved event */
  XSync(panbox.display, 0);
  while( XCheckWindowEvent(panbox.display, panbox.ID,
			   PointerMotionMask, event) );
  /* draw image fragment in scope magnibox */
  /* get buffer coordinates */
  magset.buf.X = (coord.imgtobuf.inx_outx *
		 (((float)event->xmotion.x * coord.pantoimg.inx_outx) +
		  coord.pantoimg.iadd_outx)) + coord.imgtobuf.add_outx;
  magset.buf.Y = (coord.imgtobuf.iny_outy *
		 (((float)event->xmotion.y * coord.pantoimg.iny_outy) +
		  coord.pantoimg.iadd_outy)) + coord.imgtobuf.add_outy;
  draw_magnifier((double)magset.buf.X, (double)magset.buf.Y);
}

/*
 * Subroutine:	redraw_magnifier
 * Purpose:	Draw image piece in zoombox window, using last coords or
 *		dispbox center
 */
void redraw_magnifier ( )
{
  void draw_magnifier(), d_transform();

  /* if called by a window redraw event, redraw as it last was */
  if( magset.buf.X < 0 )
    d_transform(&coord.disptobuf, (double)coord.disp.cenX,
		 (double)coord.disp.cenY, &magset.buf.X, &magset.buf.Y);
  draw_magnifier((double)magset.buf.X, (double)magset.buf.Y);
}

/*
 * Subroutine:	label_file_coords
 * Purpose:	Show pointer coordinates and image value in display window
 * Xlib calls:	XDrawImageString()
 */
static void label_file_coords ( bufX, bufY )
     double bufX, bufY;
{
  int val;
  static char string[48];
  int lstr = 48;
  int lwcs, lpix, strlen();
  float fileX, fileY;
  GC gc, set_edit_gc();
  void d_transform();
  static void draw_proportional_coord();
  static int lwcs0 = 0;
  static int lpix0 = 0;
  int i;
  int iswcs();

  gc = set_edit_gc(magset.text.font,
		   magset.text.foreground, magset.text.background);
  d_transform(&coord.buftofile, bufX, bufY, &fileX, &fileY);

  /* Cursor is not on the image */
  if( (bufX < coord.buf.X1) || (bufX > coord.buf.X2) ||
      (bufY < coord.buf.Y1) || (bufY > coord.buf.Y2) ) {
    sprintf(string, " %6.1f %6.1f       x ", fileX, fileY);
    lpix = strlen (string);
    if (lpix < lpix0) {
      for (i=lpix; i<lpix0; i++)
	string[i] = ' ';
      lpix = lpix0;
      }
    else
      lpix0 = lpix;
    XDrawImageString(desktop.display, desktop.ID, gc, magset.text.x_x,
		     magset.text.y, string, lpix);

  /* Image values are scaled */
  } else if( img.fiscaled ) {
    double rval;
    if( (buffer.filebuf == NULL) ||
        (buffer.filebuf == (char *)buffer.shortbuf) ) {
      /* values scaled, originals not available */
      val = buffer.shortbuf[(int)bufX + ((int)bufY * coord.buf.width)];
      rval = ((double)val * img.fiscale) + img.fibias;
      /* print strings with spaces padding out the end */
      if( val <= buffer.clipmin )
	sprintf(string, " %6.1f %6.1f  <%.4g ", fileX, fileY, rval);
      else if( val >= buffer.clipmax )
	sprintf(string, " %6.1f %6.1f  >%.4g ", fileX, fileY, rval);
      else
	sprintf(string, " %6.1f %6.1f   %.4g ", fileX, fileY, rval);
    } else {
      /* values scaled, originals in filebuf */
      float fbX, fbY;
      d_transform(&coord.buftofbuf, bufX, bufY, &fbX, &fbY);
      if( img.storage_type == ARR_I4 ) {
	rval = (double)
	  *((int *)(buffer.filebuf +
		    (((int)fbX + ((int)fbY * coord.fbuf.width)) *
		     sizeof(int))));
      } else if( img.storage_type == ARR_R4 ) {
	rval = (double)
	  *((float *)(buffer.filebuf +
		      (((int)fbX + ((int)fbY * coord.fbuf.width)) *
		       sizeof(float))));
      } else if( img.storage_type == ARR_R8 ) {
	rval = *((double *)(buffer.filebuf +
			    (((int)fbX + ((int)fbY * coord.fbuf.width)) *
			     sizeof(double))));
      } else
	rval = 0.0;
      if( img.fscaled )
	rval = img.fbias + (rval * img.fscale);
      sprintf(string, " %6.1f %6.1f   %.4g ", fileX, fileY, rval);
    }
    lpix = strlen (string);
    if (lpix < lpix0) {
      for (i=lpix; i<lpix0; i++)
	string[i] = ' ';
      lpix = lpix0;
      }
    else
      lpix0 = lpix;
    XDrawImageString(desktop.display, desktop.ID, gc, magset.text.x_x,
		     magset.text.y, string, lpix);
    if (iswcs(wcs)) {
      pix2wcst (wcs,(double)fileX,(double)fileY,string,lstr);
      lwcs = strlen (string);
      if (lwcs < lwcs0) {
	for (i=lwcs; i<lwcs0; i++)
	  string[i] = ' ';
	lwcs = lwcs0;
	}
      else
	lwcs0 = lwcs;
      XDrawImageString(desktop.display, desktop.ID, gc, magset.text.x_x,
		       magset.text.y-20, string, lwcs);
      }

  /* Image values are straight from the file */
  } else {
    val = buffer.shortbuf[(int)bufX + ((int)bufY * coord.buf.width)];
    sprintf(string, " %6.1f %6.1f  %6d ", fileX, fileY, val);
    lpix = strlen (string);
    if (lpix < lpix0) {
      for (i=lpix; i<lpix0; i++)
	string[i] = ' ';
      lpix = lpix0;
      }
    else
      lpix0 = lpix;
    XDrawImageString(desktop.display, desktop.ID, gc, magset.text.x_x,
		     magset.text.y, string, 23);
    if (iswcs(wcs)) {
      pix2wcst (wcs,(double)fileX,(double)fileY,string,lstr);
      lwcs = strlen (string);
      if (lwcs < lwcs0) {
	for (i=lwcs; i<lwcs0; i++)
	  string[i] = ' ';
	lwcs = lwcs0;
	}
      else
        lwcs0 = lwcs;
      XDrawImageString(desktop.display, desktop.ID, gc, magset.text.x_x,
		       magset.text.y-20, string, lwcs);
      }
  }
}

/*
 * Subroutine:	clear_coord_area
 * Purpose:	Erase area of coords, esp. when imtool_aux is no longer used
 */
void clear_coord_area ()
{
  int height;
  height = magset.text.yoff * 2 + 1;
  XClearArea(desktop.display, desktop.ID, btnbox.y - (height + 1), 1,
	     panbox.x - 2, height, False);
}

/*
 * Subroutine:	label_file_coords_proportional
 * Purpose:	Show pointer coordinates and image value in display window
 *		Special handling for proporitonal fonts is good for the coords,
 *		but has not been refined for the val section.
 * Xlib call:	XDrawImageString()
 */
static void label_file_coords_proportional ( bufX, bufY )
     double bufX, bufY;
{
  int val;
  static char string[48];
  float fileX, fileY;
  GC gc, set_edit_gc();
  void d_transform();
  static void draw_proportional_number();

  gc = set_edit_gc(magset.text.font,
		   magset.text.foreground, magset.text.background);
  d_transform(&coord.buftofile, bufX, bufY, &fileX, &fileY);
  sprintf(string, "        %6.1f    ", fileX);
  draw_proportional_number(string, 8, magset.text.x_x, magset.text.width, gc);
  sprintf(string, "        %6.1f    ", fileY);
  draw_proportional_number(string, 8, magset.text.y_x, magset.text.width, gc);
  if( (bufX < coord.buf.X1) || (bufX > coord.buf.X2) ||
      (bufY < coord.buf.Y1) || (bufY > coord.buf.Y2) ) {
    sprintf(string, "                        x        ");
    draw_proportional_number(string, 24, magset.text.val_x,
			     2 * magset.text.width, gc);
  } else {
    double dval;
    int ival, clip, i;
    if( get_pixel_val((int)bufX, (int)bufY, &ival, &dval, &clip) ) {
      /* value is an integer */
      integer_string(ival, clip, &(string[16]), VAL_SZ);
    } else {
      real_string(dval, &string[17], VAL_SZ - 1);
      if( clip ) {
	if( clip > 0 )
	  string[16] = '>';
	else
	  string[16] = '<';
      } else
	string[16] = ' ';
    }
    for( i = 0; i < 16; i++ )
      string[i] = ' ';
    for( i = 16 + VAL_SZ; i < (VAL_SZ + 20); i++ )
      string[i] = ' ';
    draw_proportional_number(string, 16, magset.text.val_x,
			     2 * magset.text.width, gc);
  }
}

/*
 * Subroutine:	draw_proportional_coord
 * Purpose:	Draw proportional text to cover area and place decimal point
 */
static void draw_proportional_number ( string, first_num, x, width, gc )
     char *string;
     int first_num;	/* i: where number begins (after leading spaces) */
     int x;
     int width;		/* i: pixel width to fill with label */
     GC gc;
{
  int size, count, offset;
  int letter, leading, not_done;

  leading = 1;
  not_done = 1;
  /* do a quick XTextWidth using a restricted table */
  for( count = 0, size = 0; not_done; count++ ) {
    letter = string[count + first_num];
    if( letter == ' ' ) {
      if( leading )
	size += magset.text.space;
      else
	/* label ends at first space after text */
	not_done = 0;
    } else {
      leading = 0;
      if( (letter >= '0') && (letter <= '9') )
	/* sizes of numbers */
	size += magset.text.numsz[letter - '0'];
      else if( letter == '.' )
	size += magset.text.dot;
      else if( (letter == '-') || (letter == '+') )
	size += magset.text.dash;
      else if( letter == '\0' ) {
	char *space;
	/* oops, no trailing spaces - put them in */
	space = &(string[count + first_num]);
	*space = ' ';
	*(space + 1) = ' ';
	*(space + 2) = '\0';
	not_done = 0;
      } else
	/* this covers whatever else ('e', 'E', '<'. '>') */
	size += magset.text.e;
    }
  }
  /* determine number of extra spaces to cover area and their x offset */
  offset = (width - size) / magset.text.space;
  x += (width - (size + (offset * magset.text.space)));
  if( (first_num -= offset) < 0 ) {
    /* move starting posisiont forward, shorten string */
    x -= (first_num * magset.text.space);
    count += first_num;
    first_num = 0;
  }
  count += (offset + 2);
  XDrawImageString(desktop.display, desktop.ID, gc, x,
		   magset.text.y, &(string[first_num]), count);
}

/*
 * Subroutine:	blank_scope
 * Purpose:	Fill scope with blank data
 */
void blank_scope()
{
  GC gc, set_gc();
  void mark_Zmagnifier();

  /* blank field */
  bzero(magset.data, magset.data_size);
  if( !magset.halftone ) {
    /* install the sighting mark */
    mark_Zmagnifier();
  } else {
#ifdef NOTYET /* %% */
    mark_XYmagnifier();
#endif
  }
  gc = set_gc(magset.gcset_disp);
  XPutImage(magset.win.display, magset.win.ID, gc, magset.image,
	    0, 0, magset.win.x, magset.win.y,
	    magset.win.width, magset.win.height);
}
