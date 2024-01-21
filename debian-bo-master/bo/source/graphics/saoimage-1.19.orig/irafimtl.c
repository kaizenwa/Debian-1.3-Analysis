#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	irafimtl.c (IRAF Imtool)
 * Purpose:	Respond to input from IRAF meant for Imtool
 * Subroutine:	imtool_response()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} based on code from Doug Tody's IRAF Imtool (NOAO)
 *		{1} Michael VanHilst	adapted			9 July 1989
 *		{2} MVH Hacked for Ultrix fifo problems		6 Oct 1989
 *		{3} Jay Travisano (STScI)  VMS,DEBUG_IMTOOL     10 Nov 1989
 *		{n} <who> -- <does what> -- <when>
 * "For the moment we take an IIS model 70 command/data stream as input; this
 * is used to load images into the image display.  This is a kludge interface
 * for the prototype, convenient since the high level software is written for
 * the IIS." - explanation by Doug Tody
 */

#ifdef IMTOOL

#include <stdio.h>		/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>		/* get X types and constants */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main ximage parameter structures */
#include "hfiles/imtool.h"


static int frame_number = 1;
static int packet_y1, packet_y2;
static int imtool_cols=512;
static int imtool_rows=512;
static int packet_cols=512;
static int wcs_piped=0;		/* l: set if wcs info sent through pipe */
static char wcsbuf[SZ_WCSBUF];

/*
 * Subroutine:	imtool_response
 * Purpose:	Read imtool messages from iraf (return 1 if pipe trouble)
 */
void imtool_response ( port, imhead, ndatabytes )
     struct connectRec *port;	/* i: struct of port for IRAF input */
     struct imtoolRec *imhead;	/* i: packet header, already read in */
     int ndatabytes;		/* i: number of bytes in expected message */
{
  char ibuf[SZ_FIFOBUF];
  int bytes, n;
  int temp;

  int imtool_input(), write_connection(), update_wcs(), read_connection();
  void imtool_newimage(), map_dispbox(), disp_dispbox();
  void set_curpos_to_iraf_trigger(), set_cursor_from_iraf(), imtool_output();
  void set_imtool_scale(), set_cursor_to_iraf(), disp_subpiece();
  void map_panbox(), disp_panbox(), send_curpos_to_iraf(), flush_connection();
  static int get_frame_no();

  switch( imhead->subunit & 077 ) {
  case FEEDBACK:
    /* The IIS feedback unit is used to clear the screen */
    bzero(buffer.filebuf, buffer.filebuf_sz);
    if( buffer.filebuf != (char *)buffer.shortbuf )
      bzero((char *)buffer.shortbuf, buffer.shortbuf_sz * sizeof(short));
    map_dispbox();
    disp_dispbox();
    /* clear the panbox */
    bzero((char *)buffer.panbuf, buffer.panbuf_sz * sizeof(short));
    map_panbox();
    disp_panbox();
    /* this must be a new start, so reset everything just in case we haven't */
    imtool_newimage(0, imhead);
    break;
  case WCS:
    /* Read or write the WCS for a frame.  The frame number to
     * which the WCS applies is passed in Z and the frame buffer
     * configuration in T.  The client changes the frame buffer
     * configuration in a WCS set.  The WCS text follows the header
     * as byte packed ASCII data.
     */
    if( imhead->tid & IIS_READ ) {
      if( control.IRAF_out.open ) {
#ifdef FRAMERR
/* include this code if requested frame must match our wcs  */
	/* Return the WCS for the referenced frame. */
	int frame;
	frame = get_frame_no(imhead->z);
	if( frame != fb_frameno ) {
	  char emsg[SZ_WCSBUF];
	  (void)strcpy(emsg, "[NOSUCHFRAME]\n");
	  (void)write_connection(&control.IRAF_out, emsg, SZ_WCSBUF);
	} else
#endif
	  (void)write_connection(&control.IRAF_out, wcsbuf, SZ_WCSBUF);
      } else
	(void)fprintf(stderr, "Warning: output pipe not open\n");
    } else {
      int new_wcs;
      char buf[SZ_WCSBUF];

      /* Read in and set up the WCS. */
      if( (bytes = read_connection(port, buf, ndatabytes))
	 == ndatabytes ) {
	if( strncmp(wcsbuf, buf, SZ_WCSBUF) != 0 )
	  new_wcs = 1;
	else
	  new_wcs = 0;
	strncpy(wcsbuf, buf, SZ_WCSBUF);
	wcs_piped = update_wcs(&img, &coord, frame_number, wcsbuf);
	if( new_wcs )
	  imtool_newimage(1, imhead);
      }
      ndatabytes -= bytes;
    }
    break;
  case IMCURSOR:
    /* Read or write the logical image cursor.  This is an extension
     * added to provide a high level cursor read facility; this is
     * not the same as a low level access to the IIS cursor subunit.
     * Cursor reads may be either nonblocking (immediate) or blocking,
     * using the keyboard or mouse to terminate the read, and
     * coordinates may be returned in either image (world) or frame
     * buffer pixel coordinates.
     */
    frame_number = get_frame_no(imhead->z);
    if( imhead->tid & IIS_READ ) {
      /* Read the logical image cursor.  In the case of a blocking
       * read all we do is initiate a cursor read; completion occurs
       * when the user hits a key or button.
       */
      if( imhead->tid & IMC_SAMPLE ) {
	/* Sample the cursor position. */
	/* Return the cursor value on the output datastream encoded
	 * in a fixed size ascii buffer.
	 */
	send_curpos_to_iraf(cursor.file.X, cursor.file.Y, frame_number,
			    imhead->z, 0, "");
      } else {
	/* Initiate a user triggered cursor read. */
	set_curpos_to_iraf_trigger(frame_number, imhead->z);
      }
    } else
      set_cursor_from_iraf((double)(imhead->x & 077777),
			   (double)(imhead->y & 077777));
    break;
  case MEMORY:
    frame_number = get_frame_no(imhead->z);
    if( imhead->tid & IIS_READ ) {
      imtool_output(imhead, &control.IRAF_out, buffer.shortbuf,
		    img.filecols, img.filerows);
    } else {
      temp = imtool_input(imhead, &control.IRAF_in, ibuf,
			  packet_cols, imtool_cols, &packet_y1, &packet_y2,
			  (short *)buffer.filebuf, buffer.shortbuf_sz * 2);
      if( img.imtool_200 == 0 ) {
	/* if we were caught unawares! */
	img.imtool_200 = 1;
	set_imtool_scale();
      }
      if( temp >= 0 )
	ndatabytes = 0;
      /* if not doing full reset, map and display just this piece */
      disp_subpiece(0, packet_y1, imtool_cols - 1, packet_y2 - 1);
    }
    if( ndatabytes == 0 )
      return;
  default:
    /* Ignore unsupported command input. */
    break;
  }
  /* Discard any extra data */
  if( !(imhead->tid & IIS_READ) ) {
    for( bytes = ndatabytes; bytes > 0; bytes -= n ) {
      n = (bytes < SZ_FIFOBUF) ? bytes : SZ_FIFOBUF;
      if( (n = read_connection(&control.IRAF_in, ibuf, n)) <= 0 )
	break;
    }
  }
}

/*
 * Procedure:	imtool_newimage
 * Purpose:	Reset buffer sizes, coordinate systems and parameters as
 *		needed.
 */
void imtool_newimage ( is_wcs, imhead )
     int is_wcs;
     struct imtoolRec *imhead;
{
  int fbconfig;
  int cols, rows;
  int get_fbconfig(), update_wcs();
  void set_disptran(), combine_transform(), set_edges(), set_dispoff();
  void adjust_cursor_coords(), set_magnifier(), disp_subpiece(), set_tdisp();
  void set_imtool_scale(), imtool_reinit(), new_pancursor();
  static int get_frame_no();

  /* set size and frame */
  if( is_wcs )
    fbconfig = (imhead->t & 077) + 1;
  else
    fbconfig = (imhead->tid & IMT_FBCONFIG) + 1;
  frame_number = get_frame_no(imhead->z);
  /* config values assume a zero based array, table starts at 1 */
  if( img.fbconfig != fbconfig ) {
    img.fbconfig = fbconfig;
    /* reset defaults if failed on config set and no known size */
    if( (get_fbconfig(fbconfig, &cols, &rows) == 0) && (cols <=0) ) {
      (void)get_fbconfig(0, &cols, &rows);
    }
    packet_cols = cols;
    imtool_cols = cols;
    imtool_rows = rows;
  }
  /* fix up img coordsys parameters and make sure enough space is alloc'd */
  imtool_reinit(imtool_cols, imtool_rows);
  /* reinit sets imgtofile to a default, restore true wcs if we know it */
  if( wcs_piped )
    (void)update_wcs(&img, &coord, frame_number, wcsbuf);
  /* update the coords right away (correct or default) */
  /* set display params to center the new image with zoom 1 */
  coord.tid.cenX = coord.img.cenX;
  coord.tid.cenY = coord.img.cenY;
  coord.tid.zoom = 1.0;
  set_tdisp(&coord);
  /* set disp to img and disp to file transforms */
  set_disptran(&coord);
  /* set transform to get from disp to buf coords */
  combine_transform(&coord.disptobuf, &coord.disptoimg, &coord.imgtobuf);
  set_edges(&coord.disptobuf, &coord.buf, &coord.disp, &coord.bd);
  /* redefine the edges in the display window if at edge of buffer */
  if( (coord.bd.block < 0) || (coord.bd.clip != 0) )
    set_dispoff(&coord.disptobuf, &coord.disp, &coord.bd);
  /* update the panbox cursor (again) since we reset the display center */
  new_pancursor(0);
  /* update cursor and magnifier info to correspond to current display */
  adjust_cursor_coords(&cursor, &coord);
  set_magnifier();
  /* set scale and colors for the standard imtool range and options */
  img.imtool_200 = 1;
  set_imtool_scale();
}

/*
 * Subroutine:	get_frame_no
 * Purpose:	Figure out the integer frame number for its mask
 */
static int get_frame_no ( mask )
     int mask;
{
  int bit, bitmask;

  mask = mask & 07777;
  bit = 0;
  bitmask = 1;
  while( bitmask <= mask ) {
    bitmask = bitmask << 1;
    ++bit;
  }
  return( bit );
}
#endif
