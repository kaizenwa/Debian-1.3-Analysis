#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mainkey.c (Main Key)
 * Purpose:	Dispatch responses mapped to various keyboard keys
 * Subroutine:	key_response()			returns: void
 * Subroutine:	set_iraf_key_trigger()		returns: void
 * Xlib calls:	XWarpPointer(), XSync()
 * Copyright:	1989,1991,1994,1995,1996 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		29 June 1989
 *		{1} MVH display window key to trigger IRAF write 16 Aug 1989
 *		{2} MVH support for text cursor			  1 Jun 1991 
 *		{3} Doug Mink support for centroid		 27 Oct 1994 
 *		{4} Doug Mink add help                           21 Jun 1995
 *		{5} Doug Mink reword wcs calls                    6 Jul 1995
 *		{6} Doug Mink add coordinate conversion          17 Aug 1995
 *		{7} Doug Mink only change system for J,G,B       24 Jan 1996
 *		{8} Doug Mink Do not quit with q if imtool        9 Feb 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "hfiles/constant.h"	/*  Define codes  */
#include "hfiles/struct.h"	/*  Declare structure types  */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */

#define MAX_MAPPED_STRING_LENGTH 16
#define MetaMask Mod1Mask


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		grab_keys_for_textcursor(	int state);
void		set_iraf_key_trigger(	int state);
void		key_response();

#endif


static int textcursor = 0;
/*  Subroutine:	grab_keys_for_textcursor
 *  Purpose:	Direct all key input to the text cursor editor
 */
#ifdef ANSIC
void grab_keys_for_textcursor ( int state )
#else
void grab_keys_for_textcursor ( state )
     int state;
#endif
{
  textcursor = state;
}


#ifdef IMTOOL
static int iraf_trigger = 0;
/*  Subroutine:	set_iraf_key_trigger
 *  Purpose:	Set and clear trigger for cursor readback through IRAF pipe
 */
#ifdef ANSIC
void set_iraf_key_trigger ( int state )
#else
void set_iraf_key_trigger ( state )
     int state;
#endif
{
  iraf_trigger = state;
}
#endif


/*  Subroutine:	key_response
 *  Purpose:	Respond to a key stroke
 */
void key_response()
{
  KeySym keysym;
  XComposeStatus compose;
  int char_cnt;
  int x, y;
  char vstring[16];
  char string[16];
  int ix,iy,lstr;
  double xcent,ycent;
  static int bufsize = MAX_MAPPED_STRING_LENGTH;
  char buffer[MAX_MAPPED_STRING_LENGTH];
#ifdef IMTOOL
  int trigger_curpos_to_iraf();
#endif
  void magnify_pan(), magnify_disp(), say_goodbye(), disp_panbox();
  void disp_dispbox(), raise_windows(), get_new_cmd(), print_table();
  void ascii_region(), textcursor_keyentry();
  void print_center(), print_cursor(), wcscom();
  void get_cursor_pos();
  int iswcs();
  double xbuf, ybuf, xfile, yfile;
  char sysout0[16];

  /*  Decode the key event to ascii  */
  char_cnt = XLookupString(&control.event.xkey, buffer, bufsize,
			   &keysym, &compose);
  /*  Key input from display window may be redirected for special purposes  */
  if( control.event.xkey.window == dispbox.ID ) {
#ifdef IMTOOL
    if( iraf_trigger && (char_cnt == 1) &&
	trigger_curpos_to_iraf(&control.event.xkey, (int)buffer[0]) )
      return;
#endif
    /*  Redirect keyboard input for textcursor editor  */
    if( textcursor ) {
      textcursor_keyentry(&control.event.xkey, keysym);
      return;
    }
  }
  /*  Ignore modifier key pressing  */
  if( ((control.event.xkey.state & (ControlMask | MetaMask)) == 0) ||
      (keysym >= XK_BackSpace) ) {
    switch( keysym ) {
    case XK_Shift_L:
    case XK_Shift_R:
#ifdef IMTOOL
      /*  Don't update tracking now if in IRAF interactive mode  */
      if( iraf_trigger )
	return;
#endif
      if( control.event.xkey.window == panbox.ID ) {
	/*  Update magnifier if in panbox  */
	magnify_pan (&control.event);
      } else if( (control.event.xkey.window == dispbox.ID) &&
		 (!control.magni_track) ) {
	/*  If in disp window and not now tracking, update magnifier  */
	magnify_disp (&control.event, 1, !control.coord_track);
      }
      break;
#ifdef SUN
      /*  Shifted arrow keys on Sun keypad  */
    case XK_R8:
    case XK_R10:
    case XK_R12:
    case XK_R14:
      if( keysym == XK_R8 )
	keysym = XK_Up;
      else if( keysym == XK_R10 )
	keysym = XK_Left;
      else if( keysym == XK_R12 )
	keysym = XK_Right;
      else if( keysym == XK_R14 )
	keysym = XK_Down;
#endif
    case XK_Up:
    case XK_Down:
    case XK_Left:
    case XK_Right:
      /*  Use the mouse coordinates at time of key strike  */
      if( keysym == XK_Up )
	y = control.event.xkey.y - 1;
      else if( keysym == XK_Down )
	y = control.event.xkey.y + 1;
      else
	y = control.event.xkey.y;
      if( keysym == XK_Right )
	x = control.event.xkey.x + 1;
      else if( keysym == XK_Left )
	x = control.event.xkey.x - 1;
      else
	x = control.event.xkey.x;
      /*  Move the mouse (supercede any recent mouse movement)  */
      XSync (control.event.xkey.display, 0);
      XWarpPointer(control.event.xkey.display, None,
		   control.event.xkey.window, 0, 0, 0, 0, x, y);
      break;
    case XK_A:
    case XK_a:
      /*  a or r (?) TO RAISE ALL WINDOWS OUT OF KAOS  */
      disp_panbox();
      disp_dispbox();
      raise_windows();
      break;
    case XK_L:
    case XK_l:
      /*  Debugging print_table without scaling (fool it about scaling)  */
      x = img.fiscaled;
      img.fiscaled = 0;
      print_table();
      img.fiscaled = x;
      break;
    case XK_N:
    case XK_n:
      /*  Open a new image file (see CmdNew.c)  */
      get_new_cmd();
      break;
/* #ifdef QUITKEY */
    case XK_Q:
    case XK_q:
      /*  q FOR QUIT  */
      if (!img.imtool_200)
	say_goodbye(0);
      break;
/* #endif */
    case XK_R:
    case XK_r:
      raise_windows();
      break;
    case XK_T:
    case XK_t:
      /*  Print table of image values if in display or pan windows  */
      print_table();
      break;
    case XK_Delete:
    case XK_D:
    case XK_d:
    case XK_E:
    case XK_e:
    case XK_S:
    case XK_s:
      /*  Character control in region (cursor) mode  */
      if( control.mode == COP ) {
	if( (control.event.xkey.window == dispbox.ID) ||
	    (cursor.type == COP_Text) )
	ascii_region(&control.event.xkey, keysym);
      }
      break;
    /* Read next image in a 3-D image */ 
    case XK_plus:
      img.nimage = img.nimage + 1;
      (void)new_image();
      break;
    /* Read previous image in a 3-D image */ 
    case XK_minus:
      if (img.nimage > 1) img.nimage = img.nimage - 1;
      (void)new_image();
      break;
    /* save current cursor position within image in pixel list file */
    case XK_P:
    case XK_p:
    /* save current cursor position within image in region list file */
    case XK_1:
      /* save current cursor position within image in region list file */
    case XK_2:
	break;

    /* Change coordinate system in which cursor tracking is occurring */
    case XK_B:
    case XK_b:
	wcsoutinit (wcs,"FK4");
	magnify_disp (&control.event, 0, 1);
	break;
    case XK_G:
    case XK_g:
	wcsoutinit (wcs,"GALACTIC");
	magnify_disp (&control.event, 0, 1);
	break;
    case XK_J:
    case XK_j:
	wcsoutinit (wcs,"FK5");
	magnify_disp (&control.event, 0, 1);
	break;

    case XK_C:
    case XK_c:
	print_cursor (&control.event);
	break;

    case XK_W:
    case XK_w:
	if (iswcs (wcs))
	    /* Get file coordinates of cursor */
	    (void)get_cursor_pos (&control.event, &xbuf, &ybuf, &xfile, &yfile);
	    wcscom (wcs,xfile,yfile);
	break;

    case XK_X:
    case XK_x:
	print_center ();
	break;

    case XK_question:
	printf ("SAOimage keyboard commands:\n");
	printf ("A rAise all SAOimage windows to the top and refresh\n");
	printf ("B track coordinates of cursor in B1950 (FK4) coordinates\n");
	printf ("C print Coordinates of cursor (x,y, current WCS if present)\n");
	printf ("D Delete the smallest region enclosing the mouse\n");
	printf ("E save current cursor as an excluded region\n");
	printf ("G track coordinates of cursor in Galactic coordinates\n");
	printf ("J track coordinates of cursor in J2000 (FK5) coordinates\n");
	printf ("N enter New command-line arguments\n");
	printf ("Q Quit SAOimage\n");
	printf ("R Raise all SAOimage windows to the top\n");
	printf ("S Save current cursor as an included region\n");
	printf ("T print table of the pixel values around current mouse position\n");
	printf ("W execute WCS command\n");
	printf ("X print centroid coordinates of cursor (X,y, WCS if present)\n");
	printf ("+ get next image from 3-D image file\n");
	printf ("- get previous image from 3-D image file\n");
	printf ("arrow keys move the cursor\n");
	break;

    default:
      break;
    }
  }
}

/*
 * Subroutine:  new_image
 * Purpose:     Having the image record set, do all that is needed to load
 *              a new image from the same file
 */
static int new_image ( )
{
  int init_image(), init_imagebuf();
  void set_tdisp(), new_display(), new_panbox(), disp_panbox();

  /* get the image dimensions (need file name, type) */
  if( init_image() == 0 )
    return(0);
  /* resize the image buffer */
  (void)init_imagebuf();
  /* set display params and force reassessing and rescaling the image */
  set_tdisp(&coord);
  buffer.mm.img_leftX = coord.id.srcX1 + 1;
  /* read new image data */
  new_display(1, 1, 1, 1);
  /* replace the panbox with a new one */
  new_panbox(1);
  disp_panbox();
  return( 1 );
}
