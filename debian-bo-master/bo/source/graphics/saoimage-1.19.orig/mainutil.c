#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mainutil.c (Main Utilities)
 * Purpose:	Basic utility calls
 * Subroutine:	calloc_errchk()			returns: char *
 * Subroutine:	exit_errmsg()			returns: void
 * Subroutine:	not_different()			returns: int
 * Subroutine:	statemotion_in_disp()		returns: Bool
 * Subroutine:	specificmotion_in_disp()	returns: Bool
 * Subroutine:	statedmotion_in_disp()		returns: Bool
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  11 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define SMALL_NUMBER */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */

static char *errnote = " allocation failure\n";
/*
 * Subroutine:	calloc_errchk
 * Purpose:	Calloc with printf'less error message and exit for failure
 * Note:	If message is given, print it and exit on failure
 * Note:	If no message is given, return 0 on failure
 */
char *calloc_errchk ( count, size, errmess )
     int count;
     unsigned int size;
     char *errmess;
{
  char *space;
  char *calloc();

  if( (space = (char *)calloc((unsigned)count, size)) == NULL ) {
    if( errmess == NULL )
      return(0);
    else {
      fputs (errmess, stderr);
      fputs (errnote, stderr);
      exit( 100 );
    }
  }
  return( space );
}

static char *exitnote = "SAO_IMAGE EXIT ON ERROR: ";
static char *exitcr = "\n";
/*
 * Subroutine:	exit_errmsg
 * Purpose:	Exit with a message
 */
void exit_errmsg ( errmess )
     char *errmess;
{
  void say_goodbye();

  fputs (exitnote, stderr);
  if( errmess != NULL )
    fputs (errmess, stderr);
  fputs (exitcr, stderr);
  say_goodbye( 10 );
}

/*
 * Subroutine:	not_different
 * Purpose:	compare two values for a significant difference
 * Returns:	1 if there is virtually no difference, else 0
 */
int not_different ( val1, val2 )
     double val1;
     double val2;
{
  if( val1 > val2 ) {
    if( (val1 - val2) > SMALL_NUMBER )
      return( 0 );
  } else {
    if( (val2 - val1) > SMALL_NUMBER )
      return( 0 );
  }
  return( 1 );
}

/*
 * Subroutine:	statemotion_in_disp
 * Purpose:	Subroutine to use with XCheckIfEvent to get the latest
 *		ButtonMotion event in the dispbox.  Track until all masked
 *		buttons or states return to 0.
 */
Bool statemotion_in_disp ( display, event, arg )
     Display *display;
     XEvent *event;
     char *arg;
{
  unsigned int mask = *((unsigned int *)arg);
  if( (event->type == MotionNotify) &&
      (event->xmotion.window == dispbox.ID) &&
      ((event->xmotion.state & mask) != 0) )
    return( True );
  else
    return( False );
}

/*
 * Subroutine:	specificmotion_in_disp
 * Purpose:	Subroutine to use with XCheckIfEvent to get the latest
 *		ButtonMotion event in the dispbox.  Track until any masked
 *		button or state returns to 0.
 */
Bool specificmotion_in_disp ( display, event, arg )
     Display *display;
     XEvent *event;
     char *arg;
{
  unsigned int mask = *((unsigned int *)arg);
  if( (event->type == MotionNotify) &&
      (event->xmotion.window == dispbox.ID) &&
      ((event->xmotion.state & mask) == mask) )
    return( True );
  else
    return( False );
}

/*
 * Subroutine:	statedmotion_in_disp
 * Purpose:	Subroutine to use with XCheckIfEvent to get the latest
 *		ButtonMotion event in the dispbox.  Track until any state
 *		differs from initial mask.
 */
Bool statedmotion_in_disp ( display, event, arg )
     Display *display;
     XEvent *event;
     char *arg;
{
  unsigned int mask = *((unsigned int *)arg);
  if( (event->type == MotionNotify) &&
      (event->xmotion.window == dispbox.ID) &&
      (event->xmotion.state == mask) )
    return( True );
  else
    return( False );
}
