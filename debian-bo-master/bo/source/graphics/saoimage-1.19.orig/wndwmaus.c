#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wndwmaus.c (Window Mouse)
 * Purpose:	Set the mouse pointer icon for each window and mode
 * Subroutine:  init_mousepointers()		returns: void
 * Subroutine:	new_dispboxmouse()		returns: void
 * Subroutine:	set_trigger_key_mouse()		returns: void
 * Xlib calls:	DefaultScreen, DefaultRootWindow, DefaultColormap
 * Xlib calls:	BlackPixel, WhitePixel, XQueryColors()
 * Xlib calls:	XCreateBitmapFromData(), XCreatePixmapCursor(), XFreePixmap()
 * Xlib calls:	XDefineCursor(), XSync(), XQueryPointer(), XWarpPointer()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		30 April 1989
 *		{1} MVH icon for key to trigger IRAF write        16 Aug 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* window manager stuff */
#include "hfiles/constant.h"		/* constants and codes */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */

#include "defs/mouse.def"		/* bitmaps for mouse cursors */
extern Window root;

static Cursor pancursor;
static Cursor cursorcursor;
static Cursor colorcursor;
static Cursor shuttlecursor;
static Cursor missilecursor;
#ifdef NOTUSED
static Cursor plaincursor;
#endif
#ifdef IMTOOL
static Cursor keycursor;
#endif

/*
 * Subroutine:	init_mousepointers
 * Purpose:	Set up the various mouse pointer icons for all windows
 */
void init_mousepointers ( dispdisplay, auxdisplay )
     Display *dispdisplay;	/* i: display for the dispbox */
     Display *auxdisplay;	/* i: display for the auxiliary windows */
{
  static Cursor make_mouse_cursor();

  /* create special btnbox cursor for button menu window */
  btnbox.attrs.cursor =
    make_mouse_cursor(auxdisplay, button_bits, button_mask_bits,
		      button_width, button_height,
		      button_x_hot, button_y_hot);
  btnbox.valuemask |= CWCursor;
  /* create special missile cursor for magnifier window */
  magnibox.attrs.cursor = 
    make_mouse_cursor(auxdisplay, missile_bits, missile_mask_bits,
		      missile_width, missile_height,
		      missile_x_hot, missile_y_hot);
  magnibox.valuemask |= CWCursor;
  /* create special color graph manipulation cursor */
  colorbox.attrs.cursor =
    make_mouse_cursor(auxdisplay, cgraph_bits, cgraph_bits,
		      cgraph_width, cgraph_height,
		      cgraph_x_hot, cgraph_y_hot);
  colorbox.valuemask |= CWCursor;
  graphbox.attrs.cursor = colorbox.attrs.cursor;
  graphbox.valuemask |= CWCursor;
  /* create special pan and zoom cursor */
  panbox.attrs.cursor =
    make_mouse_cursor(auxdisplay, pan_bits, pan_mask_bits,
		      pan_width, pan_height, pan_x_hot, pan_y_hot);
  panbox.valuemask |= CWCursor;
  /* create selection of cursors for display window */
  /* create special pan and zoom cursor */
  if( dispdisplay == auxdisplay ) {
    pancursor = panbox.attrs.cursor;
  } else {
    pancursor =
      make_mouse_cursor(dispdisplay, pan_bits, pan_mask_bits,
			pan_width, pan_height, pan_x_hot, pan_y_hot);
  }
  /* create special color manipulation cursor */
  colorcursor =
    make_mouse_cursor(dispdisplay, cgraph_bits, color_mask_bits,
		      cgraph_width, cgraph_height,
		      cgraph_x_hot, cgraph_y_hot);
  /* create special cursor manipulation cursor */
  cursorcursor =
    make_mouse_cursor(dispdisplay, cursor_bits, cursor_mask_bits,
		      cursor_width, cursor_height,
		      cursor_x_hot, cursor_y_hot);
#ifdef IMTOOL
  /* create special key triggered cursor readback cursor for IRAF */
  keycursor = 
    make_mouse_cursor(dispdisplay, key_bits, key_mask_bits,
		      key_width, key_height, key_x_hot, key_y_hot);
#endif
  /* create special shuttle cursor */
  shuttlecursor =
    make_mouse_cursor(dispdisplay, shuttle_bits, shuttle_mask_bits,
		      shuttle_width, shuttle_height,
		      shuttle_x_hot, shuttle_y_hot);
  /* install the appropriate cursor in dispbox */
  switch( control.mode ) {
  case VOP:  /* COLOR */
    dispbox.attrs.cursor = colorcursor;
    break;
  case ZOP:  /* PAN */
    dispbox.attrs.cursor = pancursor;
    break;
  case COP:  /* CURSOR */
    dispbox.attrs.cursor = cursorcursor;
    break;
  case EOP:  /* ENV */
  case SOP:  /* SCALE */
  default:
    dispbox.attrs.cursor = shuttlecursor;
    break;
  }
}

/*
 * Subroutine:	make_mouse_cursor
 * Purpose:	Make Xlib calls to actually produce a mouse cursor
 * Returns:	Cursor handle ready for use
 * Xlib calls:	DefaultScreen, DefaultRootWindow, DefaultColormap
 * Xlib calls:	BlackPixel, WhitePixel, XQueryColors()
 * Xlib calls:	XCreateBitmapFromData(), XCreatePixmapCursor(), XFreePixmap()
 * Note:	Uses black and white of display's root window's color map
 */
static Cursor make_mouse_cursor ( display, bits, mask_bits,
				  width, height, x_hot, y_hot )
     Display *display;
     char *bits, *mask_bits;
     unsigned int width, height;
     unsigned int x_hot, y_hot;
{
  static XColor foreback[2];
  static Display *display_base = NULL;
  Cursor cursor;
  Pixmap source, mask;

  /* renew basic params only if display is different */
  if( display_base != display ) {
    foreback[0].pixel = (unsigned long)color.hard.std_black;
    foreback[1].pixel = (unsigned long)color.hard.std_white;
    XQueryColors(display, color.colormap, foreback, 2);
    display_base = display;
  }
  source = XCreateBitmapFromData(display, root, bits, width, height);
  mask = XCreateBitmapFromData(display, root, mask_bits, width, height);
  cursor = XCreatePixmapCursor
    (display, source, mask, &foreback[0], &foreback[1], x_hot, y_hot);
  XFreePixmap(display, source);
  XFreePixmap(display, mask);
  return( cursor );
}

static short oldmode = -1;

#ifdef IMTOOL
/* mouse positions saved for cursor interaction with IRAF */
static int freeze_dispboxmouse = 0;
static int trigger_x = -1;
static int trigger_y;
/*
 * Subroutine:	note_trigger_key_position
 * Purpose:	set the reference position for placing the cursor in next
 *		trigger key from iraf
 */
void note_trigger_key_position ( x, y )
     int x, y;
{
  trigger_x = x;
  trigger_y = y;
}
    
/*
 * Subroutine:	set_trigger_key_mouse
 * Purpose:	set special displaybox cursor to indicate cursor readback
 *		trigger on any key is in effect
 * Xlib calls:	XDefineCursor(), XSync(), XQueryPointer(), XWarpPointer()
 */
void set_trigger_key_mouse ( state )
     int state;
{
  static int root_x, root_y = 0;
  Window root_ret, child_ret;
  int win_x, win_y;
  int temp;
  unsigned int mask;
  void new_dispboxmouse(), raise_windows();

  if( (freeze_dispboxmouse = state) ) {
    /* save current mouse position unless it is not on this screen */
    if( XQueryPointer(dispbox.display, root, &root_ret, &child_ret,
		      &root_x, &root_y, &win_x, &win_y, &mask) == False )
      root_y = 0;
    /* change pointer appearance for this function */
    XDefineCursor(dispbox.display, dispbox.ID, keycursor);
    /* raise the main set of windows, before warp (but not the graphbox) */
    temp = graphbox.active;
    graphbox.active = 0;
    raise_windows();
    XSync(dispbox.display, 0);
    graphbox.active = temp;
    /* if pointer is not in display window, move it there */
    if( child_ret != dispbox.ID ) {
      /* if trigger not set, or cursor was moved more recently, use curpos */
      if( trigger_x < 0 ) {
	if( (cursor.win.x > 0) && (cursor.win.x < dispbox.width) &&
	    (cursor.win.y > 0) && (cursor.win.y < dispbox.height) )
	  XWarpPointer(dispbox.display, None, dispbox.ID, 0, 0, 0, 0,
		       cursor.win.x, cursor.win.y);
	else
	  /* if cursor position wouldn't work, just center it */
	  XWarpPointer(dispbox.display, None, dispbox.ID, 0, 0, 0, 0,
		       dispbox.width / 2, dispbox.height / 2);
      } else
	/* return to last trigger position set by IRAF routines */
	XWarpPointer(dispbox.display, None, dispbox.ID, 0, 0, 0, 0,
		     trigger_x, trigger_y);
    } else
      root_y = 0;
    XSync(dispbox.display, 0);
    oldmode = -1;
  } else {
    /* restore mouse position (NOW) if it was saved */
    if( root_y > 0 ) {
      XWarpPointer(dispbox.display, dispbox.ID, root, 0, 0, 0, 0,
		   root_x, root_y);
      XSync(dispbox.display, 0);
      root_y = 0;
    }
    new_dispboxmouse();
  }
}
#endif   

/*
 * Subroutine:	new_dispboxmouse
 * Purpose:	Install dispbox mouse cursor appropriate for the  mode
 * Xlib calls:	XDefineCursor()
 * Note:	Does installation only if mode is actually different
 */
void new_dispboxmouse ( )
{

#ifdef IMTOOL
  if( freeze_dispboxmouse )
    return;
#endif
  if( control.mode == oldmode )
    return;
  oldmode = control.mode;
  switch( control.mode ) {
  /* COLOR */
  case VOP:
    XDefineCursor(dispbox.display, dispbox.ID, colorcursor);
    break;
  /* PAN */
  case ZOP:
    XDefineCursor(dispbox.display, dispbox.ID, pancursor);
    break;
  /* CURSOR */
  case COP:
    XDefineCursor(dispbox.display, dispbox.ID, cursorcursor);
    break;
  /* ENV */
  case EOP:
    XDefineCursor(dispbox.display, dispbox.ID, shuttlecursor);
    break;
  /* SCALE */
  case SOP:
    XDefineCursor(dispbox.display, dispbox.ID, shuttlecursor);
    break;
  }
}
