#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	iraffdbk.c (IRAF Feedback)
 * Purpose:	Set the mouse pointer icon for each window and mode
 * Subroutine:	set_cursor_from_iraf()		returns: void
 * Subroutine:	send_curpos_to_iraf()		returns: void
 * Subroutine:	trigger_curpos_to_iraf()	returns: int
 * Subroutine:	set_curpos_to_iraf_trigger()	returns: void
 * Origin:	Some sections modeled after code by Doug Tody, NOAO
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 15 Aug 1989
 *		{1} MVH use generalized remote connection IO	28 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#ifdef IMTOOL

#include <stdio.h>			/* stderr, NULL, etc. */
#include <ctype.h>			/* isspace, isprint, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* window manager stuff */
#include "hfiles/constant.h"		/* constants and codes */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */

#define	SZ_IMCURVAL	160
static char *NO_OUT_MSG = "Warning: output connection not open\n";
/*
 * Subroutine:	set_cursor_from_iraf
 * Purpose:	Respond to an imtool IMCURSOR/IIS_WRITE packet from the pipe
 * Note:	imtool equiv.: Write (set) the logical image cursor position.
 */
void set_cursor_from_iraf ( fileX, fileY )
     double fileX, fileY;	/* i: cursor coords in file coord system */
{
  float winX, winY;
  float bufX, bufY;
  PolyPoint *poly;
  unsigned long background;	/* l: temp for switch of colors */
  void d_transform(), add_polygon_vertex(), draw_magnifier(), disp_region();
  void set_cursor_from_file_coords(), set_annuli_from_file_coords();
  void label_region_cycle_magnifier(), save_cursor_as_region(), erase_cursor();
  void disp_cursor(), disp_dispbox();

  /* erase current cursor if it can be erased */
  if( cursor.overwrites_image_data == 0 )
    erase_cursor(&cursor);

  cursor.file.X = (float)fileX;
  cursor.file.Y = (float)fileY;
  /* get window coordinates */
  d_transform(&coord.filetodisp, fileX, fileY, &winX, &winY);
  if( cursor.type == COP_Polygon ) {
    /* add polygon vertex at end of vertex list */
    cursor.win.X = winX;
    cursor.win.Y = winY;
    cursor.win.x = (int)winX;
    cursor.win.y = (int)winY;
    poly = &cursor.poly[cursor.poly_cnt];
    add_polygon_vertex(&cursor, cursor.poly_cnt, (int)winX, (int)winY);
    poly->winX = winX;
    poly->winY = winY;
    poly->fileX = (float)fileX;
    poly->fileY = (float)fileY;
    poly->unset = 0;
  } else {
    if( cursor.annuli ) {
      /* move center for all annuli */
      set_annuli_from_file_coords(&cursor, &coord.filetodisp);
    } else
      /* move cursor center */
      set_cursor_from_file_coords(&cursor, &coord.filetodisp);
  }
  if( cursor.type == COP_Point ) {
    /* save point as region, and draw it */
    save_cursor_as_region(&cursor, 0);
    cursor.index = cursor.next_region->index;
    disp_region(cursor.next_region);
  } else {
    if( cursor.overwrites_image_data )
      /* redraw image and any saved cursors */
      disp_dispbox();
    else
      disp_cursor(&cursor);
  }
  /* show cursor position in magnifier, including coordinate label */
  d_transform(&coord.disptobuf, cursor.win.X, cursor.win.Y, &bufX, &bufY);
  draw_magnifier ((double)bufX, (double)bufY);
  background = cursor.draw->background;
  cursor.draw->background = color.gcset.excl.background;
  label_region_cycle_magnifier (&cursor, 0);
  cursor.draw->background = background;
}

/*
 * Subroutine:	send_curpos_to_iraf
 * Purpose:	Return the cursor value on the output datastream to the
 *		client which requested the cursor read.
 * Note:	imtool equivalent subroutine: gio_retcursorval()
 */
void send_curpos_to_iraf ( fileX, fileY, frameno, z, key, strval )
     double fileX, fileY;	/* i: cursor coordinates */
     int frameno;		/* i: frame number given by last WCS packet */
     int z;			/* i: value of request packet iis.z */
     int key;			/* i: keystroke used as trigger */
     char *strval;		/* i: optional string value */
{
  char	curval[SZ_IMCURVAL];
  char	keystr[20];
  int write_connection();

  if( control.IRAF_out.open == 0 ) {
    (void)fprintf(stderr, NO_OUT_MSG);
    return;
  }
  if( key == EOF ) {
    /* user indicated abort of readback process */
    sprintf(curval, "EOF\n");
  } else {
    if( isprint(key) && (!isspace(key)) ) {
      keystr[0] = key;
      keystr[1] = '\0';
    } else
      sprintf(keystr, "\\%03o", key);
    /* Encode the cursor value and key. */
    sprintf(curval, "%10.3f %10.3f %d %s %s\n",
		  fileX, fileY, (frameno * 100) + z, keystr, strval);
  }
  /* Send it to the client program. */
  /* Send it to the client program. */
  (void)write_connection(&control.IRAF_out, curval, sizeof(curval));
}

static int trigger_request_count = 0;
static int frame, iis_z;
/*
 * Subroutine:	set_curpos_to_iraf_trigger
 * Purpose:	Set up mode to send cursor position when a key is struck
 * Note:	Equivalent imtool subroutine: gio_readcursor()
 * GIO_READCURSOR -- Initiate an image cursor read.  Save the current
 * mouse coordinates if outside the imtool window, restore the mouse to the
 * imtool window, and change the cursor shape to indicate that a cursor read
 * is in progress.  May be called while a cursor read is already in progress
 * to reset the cursor-read cursor pixrect.
 */
void set_curpos_to_iraf_trigger ( frameno, z )
     int frameno;
     int z;		/* i: z value from iis packet which requested mode */
{
  void set_trigger_key_mouse(), set_iraf_key_trigger();

  if( control.IRAF_out.open == 0 ) {
    (void)fprintf(stderr, NO_OUT_MSG);
    return;
  }
  if( trigger_request_count == 0 ) {
    /* change display window cursor */
    set_trigger_key_mouse(1);
    /* put trigger in key response subroutine */
    set_iraf_key_trigger(1);
  }
  frame = frameno;
  iis_z = z;
  trigger_request_count++;
}

/*
 * Subroutine:	trigger_curpos_to_iraf
 * Purpose:	Send cursor position to IRAF in response to trigger event
 */
int trigger_curpos_to_iraf ( event, key )
     XKeyEvent *event;
     int key;
{
  float fileX, fileY;
  PolyPoint *poly;
  void update_annuli_centers(), send_curpos_to_iraf(), set_trigger_key_mouse();
  void i_transform(), erase_cursor(), disp_dispbox(), set_cursor_file_coords();
  void save_cursor_as_region(), set_iraf_key_trigger(), add_polygon_vertex();
  void note_trigger_key_position(), move_annuli(), disp_region();
  void make_cursor();

  if( control.IRAF_out.open == 0 ) {
    (void)fprintf(stderr, NO_OUT_MSG);
    return(0);
  }
  /* Map ctrl/d and ctrl/z into EOF. */
  i_transform(&coord.disptofile, event->x, event->y, &fileX, &fileY);
  if( (key == '\004') || (key == '\032') ) {
    key = EOF;
  } else {
#ifdef CURTOO
    if( control.mode == COP ) {
      if( cursor.annuli ) {
	move_annuli(&cursor, event->x, event->y);
	update_annuli_centers(&cursor);
	save_cursor_as_region(&cursor, 0);
      } else {
	if( cursor.overwrites_image_data == 0 )
	  erase_cursor(&cursor);
	cursor.win.X = (double)event->x + 0.5;
	cursor.win.Y = (double)event->y + 0.5;
	cursor.win.x = event->x;
	cursor.win.y = event->y;
	if( cursor.type == COP_Polygon ) {
	  /* add polygon vertex at end of vertex list */
	  poly = &cursor.poly[cursor.poly_cnt];
	  add_polygon_vertex(&cursor, cursor.poly_cnt, event->x, event->y);
	  poly->winX = (double)event->x;
	  poly->winY = (double)event->y;
	  poly->fileX = fileX;
	  poly->fileY = fileY;
	  poly->unset = 0;
	} else {
	  /* force file coordinates into agreement with window coordinates */
	  set_cursor_file_coords(&cursor, &coord.disptofile, 0);
	  set_cursor_file_coords(&cursor, &coord.disptofile, 1);
	  /* make new drawing vertices */
	  make_cursor(&cursor);
	  save_cursor_as_region(&cursor, 0);
	}
      }
      if( cursor.overwrites_image_data )
	/* redraw image and any saved cursors */
	disp_dispbox();
      if( cursor.type != COP_Polygon )
	disp_region(cursor.next_region);
    }
#endif
  }
  /* send the coords and key through the fifo */
  send_curpos_to_iraf((double)fileX, (double)fileY, frame, iis_z, key, "");
  /* note the current position for returning the mouse pointer */
  note_trigger_key_position((int)event->x, (int)event->y);
  if( --trigger_request_count <= 0 ) {
    /* restore display window cursor */
    set_trigger_key_mouse(0);
    /* clear trigger in key response subroutine */
    set_iraf_key_trigger(0);
  }
  return( 1 );
}

#endif
              
                                                               
                                                               
                                                               
                                                               
                                                               
                                                     
