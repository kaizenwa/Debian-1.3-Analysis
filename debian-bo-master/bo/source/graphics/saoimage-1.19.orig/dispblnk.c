#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	dispblnk.c (Display Blink)
 * Purpose:	Switch between stored displays
 * Subroutine:	save_blink()		returns: void
 * Subroutine:	free_blink()		returns: void
 * Subroutine:	clear_blink()		returns: void
 * Subroutine:	unset_blink()		returns: void
 * Subroutine:	control_blink()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  26 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr, NULL */
#include <X11/Xlib.h>		/* get X types and constants */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main ximage parameter structures */
#include "hfiles/constant.h"	/* define SOP */

#define MetaMask Mod1Mask

static struct {
  char *buf;
  Pixmap pixmap;
  int depth;
  int bufsz;
  int byte_width;
} blink[4] = { { 0 }, { 0 }, { 0 }, { 0 } };
static int current_index = 0;

/*
 * Subroutine:	save_blink
 * Purpose:	Save the current display in a blink buffer
 */
void save_blink ( button )
     int button;
{
  int bufsz, index;
  GC gc, set_gc();
  char *calloc_errchk();

  if( blink[0].buf == NULL ) {
    blink[0].buf = dispbox.image.data;
    blink[0].depth = dispbox.image.depth;
    blink[0].byte_width = dispbox.image.bytes_per_line;
  }
  if( blink[0].depth == 1 )
    bufsz = ((dispbox.xwidth + 7) / 8) * dispbox.yheight * sizeof(short);
  else
    bufsz = dispbox.xwidth * dispbox.yheight;
  if( button == Button1 )
    index = 1;
  else if( button == Button2 )
    index = 2;
  else
    index = 3;
  if( blink[index].bufsz != bufsz ) {
    if( blink[index].buf != NULL ) {
      free((char *)blink[index].buf);
      blink[index].buf = NULL;
    }
    if( blink[index].pixmap != NULL ) {
      XFreePixmap (dispbox.display, blink[index].pixmap);
      blink[index].pixmap = NULL;
    }
    blink[index].depth = blink[0].depth;
    if( (blink[index].pixmap =
	 XCreatePixmap(dispbox.display, dispbox.ID, dispbox.xwidth,
		       dispbox.yheight, blink[index].depth)) == NULL )
      blink[index].buf = calloc_errchk(bufsz, 1, "blink");
    blink[index].bufsz = bufsz;
  }
  if( blink[index].pixmap != NULL ) {
    gc = set_gc(&(color.gcset.disp));
    if( blink[index].depth == 1 )
      XCopyPlane(dispbox.display, dispbox.ID, blink[index].pixmap, gc,
		 dispbox.xzero, dispbox.yzero,
		 dispbox.xwidth, dispbox.yheight, 0, 0, 1);
    else
      XCopyArea(dispbox.display, dispbox.ID, blink[index].pixmap, gc,
		dispbox.xzero, dispbox.yzero,
		dispbox.xwidth, dispbox.yheight, 0, 0);
  } else {
    bcopy((char *)dispbox.image.data, blink[index].buf, bufsz);
    blink[index].depth = dispbox.image.depth;
    blink[index].byte_width = dispbox.image.bytes_per_line;
  }
}

/*
 * Subroutine:	free_blink
 * Purpose:	When dispbox changes size, dump all current buffers
 */
void free_blink ( )
{
  int i;
  void clear_blink();

  clear_blink();
  for( i=1; i<4; i++ ) {
    if( blink[i].buf != NULL ) {
      free((char *)blink[i].buf);
      blink[i].buf = NULL;
    }
    if( blink[i].pixmap != NULL ) {
      XFreePixmap (dispbox.display, blink[i].pixmap);
      blink[i].pixmap = NULL;
    }
    blink[i].bufsz = 0;
  }
}

/*
 * Subroutine:	clear_blink
 * Purpose:	Reset basic blink parameters when color arrangements change
 */
void clear_blink ( )
{
  if( (blink[0].buf != NULL) && (current_index != 0) ) {
    dispbox.image.depth = blink[0].depth;
    dispbox.image.bytes_per_line = blink[0].byte_width;
    dispbox.image.data = blink[0].buf;
  }
  current_index = 0;
  blink[0].buf = NULL;
}

/*
 * Subroutine:	unset_blink
 * Purpose:	Indicate that display no longer holds a blinking image
 *		Use when display is redrawn by regular display update routine
 */
void unset_blink ( )
{
  current_index = 0;
}

/*
 * Subroutine:	display_blink
 */
static int display_blink ( index )
     int index;
{
  GC gc, set_gc();
  void disp_dispbox();

  if( blink[index].pixmap != NULL ) {
    gc = set_gc(&(color.gcset.disp));
    if( blink[index].depth == 1 )
      XCopyPlane(dispbox.display, blink[index].pixmap, dispbox.ID, gc, 0, 0,
		 dispbox.xwidth, dispbox.yheight,
		 dispbox.xzero, dispbox.yzero, 1);
    else
      XCopyArea(dispbox.display, blink[index].pixmap, dispbox.ID, gc, 0, 0,
		dispbox.xwidth, dispbox.yheight, dispbox.xzero, dispbox.yzero);
  } else if( blink[index].buf != NULL ) {
    dispbox.image.data = blink[index].buf;
    dispbox.image.depth = blink[index].depth;
    dispbox.image.bytes_per_line = blink[index].byte_width;
    disp_dispbox();
  } else
    return( 0 );
  current_index = index;
  XSync (dispbox.display, 0);
  return( 1 );
}

/*
 * Subroutine:	control_blink
 * Purpose:	Control blinking of stored displays
 * For each new button press, put it on top of stack and display its buffer
 * For each button release, if it is on top of stack, release it and display
 *  one below, else just release it
 */
void control_blink ()
{
  static int oldmode;
  static int buttons = 0;
  static int btnstack[4];
  int i;
  static int display_blink();

  if( control.event.type == ButtonPress ) {
    /* don't respond if it's with a meta key (window resize) */
    if( control.event.xbutton.state & (ControlMask | MetaMask) )
      return;
    if( ++buttons == 1 ) {
      /* store mode (may be called from other mode) */
      oldmode = control.mode;
      control.mode = SOP;
      /* events to send through here in any case */
      control.priority = ButtonPressMask | ButtonReleaseMask;
    }
    btnstack[buttons] = control.event.xbutton.button;
  } else if( control.event.type == ButtonRelease ) {
    /* don't respond if we weren't doing anything */
    if( buttons <= 0 )
      return;
    if( buttons == 1 ) {
      /* unset tracking priority */
      control.mode =  oldmode;
      control.priority = 0;
    }
    if( btnstack[buttons] != control.event.xbutton.button ) {
      /* button other than top one released, lower the stack */
      for( i = 1;
	   (i <= buttons) && (btnstack[i] != control.event.xbutton.button);
	   i++ );
      while( i<buttons ) {
	btnstack[i] = btnstack[i+1];
	++i;
      }
      --buttons;
      return;
    } else {
      --buttons;
    }
  } else {
    /* not our kind of event */
    return;
  }
  /* which buffer is indicated? */
  switch( btnstack[buttons] ) {
  case Button1:
    i = 1;
    break;
  case Button2:
    i = 2;
    break;
  case Button3:
    i = 3;
    break;
  default:
    i = 0;
    break;
  }
  /* display the selected buffer */
  if( display_blink(i) == 0 )
    (void)fprintf(stderr, "WARNING: Frame[%d] is empty\n", buttons);
}
