#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mainslct.c (Main Select)
 * Purpose:	Respond to selections from user interfaces
 * Subroutine:	dispatch_select()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} MVH support for text cursor			   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  stderr, NULL, etc.  */
#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include "hfiles/constant.h"	/*  Define codes  */
#include "hfiles/struct.h"	/*  Declare structure types  */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */

#ifdef ANSIC

void	    dispatch_select(	int *response_data);

static void select_environment(	int response[]);

#else

  static void select_environment();
  void select_cursor(), select_region(), select_halftone(), select_scalemap();
  void new_dispboxmouse(), select_color(), select_pan();

#endif


/*  Subroutine:	dispatch_select
 *  Purpose:	Dispatcher for selections from the buttonbox or other
 *		selection interface
 */
#ifdef ANSIC
void dispatch_select ( int *response_data )
#else
void dispatch_select ( response_data )
     int *response_data;
#endif
{
  /*  Check for no selection data  */
  if( response_data == NULL )
    return;
  /*  Check for selection with control key down  */
  if( control.event.xbutton.state & ControlMask )
    return;
  /*  Check for switching out of cursor in text mode  */
  if( control.mode == COP ) {
    if(   (cursor.type == COP_Text) && (response_data[1] == 0)
       && (response_data[0] != COP) && (response_data[0] != ROP) )
      grab_keys_for_textcursor(0);
  }
  control.response = response_data;
  /*  Check pane (mode) part of response  */
  switch( response_data[0] ) {
    /*  Environment panel selection  */
  case EOP:
    select_environment(response_data);
    control.mode = EOP;
    break;
  case SOP:
    /*  From the image scale pane  */
    select_scalemap();
    control.mode = SOP;
    break;
  case ZOP:
    /*  From the pan pane  */
    select_pan();
    break;
  case COP:
    /*  From the cursor menu pane  */
    select_cursor();
    break;
  case MOP:
  case VOP:
    /*  From the color menu pane  */
    select_color();
    break;
  case BOP:
    select_halftone();
    break;
  case ROP:
    select_region();
    break;
  }
  /*  Install the correct mouse icon for the new mode  */
  new_dispboxmouse();
}


/*  Subroutine:	select_environment
 *  Purpose:	Response to selections from the environment button sub-menu
 */
#ifdef ANSIC
static void select_environment ( int response[] )
#else
static void select_environment ( response )
     int response[];
#endif
{
  void say_goodbye(), raise_windows(), get_new_cmd();
  void screen_dump(), disp_dispbox();

  /*  Check the response code  */
  switch( response[1] ) {
  case 0:
    return;
  case EOP_Verbose:
    /*  Toggle verbose output of text  */
    control.verbose = !control.verbose;
    break;
  case EOP_Exit:
    /*  Selected exit  */
    say_goodbye(0);
    break;
  case EOP_Raise:
    raise_windows();
    break;
  case EOP_NewFile:
    /*  Read new command line with a popup window (see CmdNew.c)  */
    get_new_cmd();
    break;
  case EOP_Output:
    /*  Output hardcopy to a printer  */
    screen_dump(!control.print_buttons);
    break;
  case EOP_Track:
    /*  Flip the tracking mode flags  */
    control.tracking ^= 1;
    control.magni_track ^= 1;
    break;
  case EOP_TextTrack:
    /*  Flip the tracking mode flag  */
    control.coord_track ^= 1;
    break;
  default:
    break;
  }
}
