#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrctrl.c (Cursor Control)
 * Purpose:	Manipulate the software cursor in response to mouse events
 * Subroutine:	control_cursor()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 4 June 1989
 *		{1} MVH added arrow and text cursors		  1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  stderr, NULL, etc.  */
#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include "hfiles/constant.h"	/*  Define codes  */
#include "hfiles/struct.h"	/*  Declare structure types  */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */

static unsigned int three_button_mask = Button1Mask|Button2Mask|Button3Mask;


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void control_cursor()

#else

  int request_delete_polygon_vertex(), start_size();
  Bool statemotion_in_disp();		/* Function for XCheckIfEvent */
  void grab_polygon_vertex(), draw_annuli(), point_region(), disp_dispbox();
  void delete_annulus(), set_cursor_file_coords(), set_polygon_file_coords();
  void make_new_annulus(), size_cursor(), report_cursor_info(), angle_cursor();
  void move_annuli(), move_cursor(), size_annuli(), update_annuli_centers();
  void save_textcursor();
#ifdef IMTOOL
  void note_trigger_key_position();
#endif

#endif


/*  Subroutine:	control_cursor
 *  Purpose:	Respond to mouse commands
 *  Note:	When not tracking, call on ButtonPress only.
 *		When tracking priority set, enter on Press, Release, & Motion
 */
void control_cursor()
{
  static int oldmode = COP;		/* restore control mode when done */
  static short originating_button;	/* the first determines the action */
  static short cursor_has_moved=0;	/* position params may need updating */
  int not_erased_yet = 1;		/* move-or-size-must-erase-old */

  if( control.event.type == ButtonPress ) {
    /*  If mouse button pressed (and no others), start tracking  */
    if( control.priority == 0 ) {
      /*  Don't respond if control button is down  */
      if( control.event.xbutton.state & ControlMask )
	return;
      /*  If not already tracking, set tracking mode  */
      originating_button = control.event.xbutton.button;
      if( cursor.type == COP_Point ) {
	/*  For pointing cursor, this is a region command  */
	point_region(&control.event.xbutton);
	/*  Announce the coordinates  */
	if( control.verbose )
	  report_cursor_info(&cursor);
#ifdef IMTOOL
	/*  Give this position over last imcur position for warping mouse  */
	note_trigger_key_position(-1, -1);
#endif
	return;
      } else {
	if( (originating_button != Button1) && (cursor.type == COP_Text) ) {
	  save_textcursor(&control.event.xbutton);
	} else {
	  if( originating_button == Button3 ) {
	    if( cursor.annuli )
	      delete_annulus(&cursor, &control.event);
	  } else {
	    if( cursor.type == COP_Polygon ) {
	      /*  Set up parameters for polygon tracking  */
	      grab_polygon_vertex(&cursor, &control.event,
				  (originating_button == Button2) );
	      not_erased_yet = 0;
	    } else if( originating_button == Button2 ) {
	      if( (cursor.type == COP_Box) || (cursor.type == COP_Ellipse) )
		cursor.ctrl.active_side = start_size(&cursor, &control.event);
	    }
	  }
	}
      }
      /*  Store mode (in case called from other mode)  */
      oldmode = control.mode;
      control.mode = COP;
      /*  Set mask for mouse tracking events  */
      control.priority = ButtonPress | ButtonRelease | MotionNotify;
    } else {
      /*  Ignore additional buttons  */
      control.completed = 1;
      return;
    }
  } else if( control.event.type == ButtonRelease ) {
    /*  If mouse button released (and no others are down), end tracking  */
    /*  Which buttons were being held before?  */
    int mask_of_released_button;	/*  State mask of event button  */
    switch( control.event.xbutton.button ) {
    case Button1:
      mask_of_released_button = Button1Mask;
      break;
    case Button2:
      mask_of_released_button = Button2Mask;
      break;
    case Button3:
      mask_of_released_button = Button3Mask;
      break;
    default:
      /*  Default case must fail test  */
      mask_of_released_button = 7;
    }
    /*  If that was the last button, we are done  */
    if( (control.event.xbutton.state & three_button_mask) ==
        mask_of_released_button ) {
      /*  Update cursor file coordinates  */
      if( cursor.type == COP_Polygon ) {
	/*  Update all polygon coordinates which have changed  */
	set_polygon_file_coords(&cursor, &coord.disptofile);
      } else {
	if( cursor_has_moved ) {
	  cursor.win.X = (double)cursor.win.x + 0.5;
	  cursor.win.Y = (double)cursor.win.y + 0.5;
	  set_cursor_file_coords(&cursor, &coord.disptofile, 1);
	  if( cursor.annuli )
	    update_annuli_centers(&cursor);
	  cursor_has_moved = 0;
#ifdef IMTOOL
	  /*  Give this position over last imcur position for warping mouse  */
	  note_trigger_key_position(-1, -1);
#endif
	} else {
	  /*  Else cursor was sized  */
	  set_cursor_file_coords (&cursor, &coord.disptofile, 0);
	  /*  If finished sizing an annulus, put it in the annulus list  */
	  if( cursor.annuli && (originating_button == Button2) ) {
	    make_new_annulus (&cursor);
	    if( !cursor.overwrites_image_data )
	      /*  Redraw all annuli in case we tracked across any  */
	      draw_annuli(&cursor, cursor.draw);
	  }
	}
      }
      control.completed = 1;
      /*  For point cursor, we never entered tracking mode.  end here  */
      if( cursor.type != COP_Point ) {
	if( control.verbose )
	  /*  Announce the coordinates  */
	  report_cursor_info (&cursor);
	/*  Unset priority tracking mode  */
	control.priority = 0;
	control.mode = oldmode;
	/*  If image altered by tracking, redraw the image (exc. poly del)  */
	if( cursor.overwrites_image_data &&
	    ((originating_button != Button3) || (cursor.type != COP_Polygon)) )
	  disp_dispbox();
      }
    }
    /*  Return on any release event  */
    return;
  } else if( control.event.type != MotionNotify )
    return;
  /*  Make sure this is a dispbox event event  */
  if( control.event.xbutton.window != dispbox.ID )
    return;
  /*  Get only the most recent move  */
  XSync(cursor.win.display, 0);
  while( XCheckIfEvent(cursor.win.display, &control.event, statemotion_in_disp,
		       (char *)(&three_button_mask)) );
  /*  Must be mouse moved - call relevant cursor activity  */
  switch( originating_button ) {
  case Button1:
    /*  Move the cursor  */
    if( cursor.annuli )
      move_annuli(&cursor, control.event.xmotion.x, control.event.xmotion.y);
    else
      move_cursor(&cursor, &control.event, not_erased_yet);
    cursor_has_moved = 1;
    break;
  case Button2:
    /*  Resize the cursor  */
    if( cursor.annuli )
      size_annuli(&cursor, &control.event);
    else
      size_cursor(&cursor, &control.event, not_erased_yet);
    break;
  case Button3:
    /*  Rotate the cursor or delete point  */
    if( !cursor.annuli ) {
      if( (cursor.type == COP_Box) ||
	  (cursor.type == COP_Ellipse) ||
	  (cursor.type == COP_Arrow) ) {
	angle_cursor(&cursor, &control.event);
      } else if( cursor.type == COP_Polygon ) {
	/*  This is a delete point operation, redisplay if needed  */
	if( request_delete_polygon_vertex(&cursor, &control.event) )
	  disp_dispbox();
      }
      /*  Else do nothing for now  */
    }
    break;
  }
  /*  Suppress the zoombox and everything else if not tracking  */
  if( ((control.event.xbutton.state & (ShiftMask | LockMask)) ==0) ^
      (control.tracking !=0) )
    control.completed = 1;
}
