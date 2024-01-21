#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	CrsrText.c (CursorText)
 * Purpose:	Initialize and change states of the software cursor
 * Subroutine:	init_textcursor()		returns: void
 * Subroutine:	new_textcursor()		returns: void
 * Subroutine:	draw_textcursor()		returns: void
 * Subroutine:	reset_textcursor_coords()	returns: void
 * Subroutine:	move_textcursor()		returns: void
 * Subroutine:	textcursor_keyentry()		returns: void
 * Subroutine:	reload_textcursor()		returns: void
 * Subroutine:	save_textcursor()		returns: void
 * Copyright:	1990 Michael VanHilst
 *		You may do anything you like with this file except remove
 *		this copyright.  Michael VanHilst makes no representations
 *		about the suitability of this software for any purpose.
 *		It is provided "as is" without express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	       1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/*  strlen, strcat, strcpy, strrchr  */
#else
#include <strings.h>		/*  strlen, strcat, strcpy, rindex  */
#endif
#else
#include <string.h>		/*  strlen, strcat, strcpy, strrchr  */
#endif

#include <stdio.h>		/*  stderr, NULL, etc.  */
#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "hfiles/constant.h"	/*  Define codes  */
#include "hfiles/struct.h"	/*  Declare structure types  */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */
#include "hfiles/define.h"	/*  Define DONT_CARE, U_DONT_CARE  */
#include "hfiles/edit.h"
#include "hfiles/region.h"	/*  regdrawRec region drawing parameters  */

struct regdrawRec rgdraw;

/*  Ammount of space for string is actually (CURSOR_MAX - 1)*sizeof(XPoint)  */
struct curText {
  int char_cnt;
  char string[CURSOR_MAX*2];
};

static EditStruct *cursor_edit;
static XFontStruct *fontstruct;
static int init = 1;


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void	    new_textcursor();
void	    clear_textcursor();
void	    save_textcursor(		XButtonEvent *xbutton);
void	    reload_textcursor();
void	    textcursor_keyentry(	XKeyEvent *xkey, KeySym keysym);
void	    draw_textcursor(		struct cursorRec *crsr, GCspec *draw);
void	    reset_textcursor_coords(	struct cursorRec *crsr);
void	    move_textcursor(		int xinc, int yinc);
static void init_textcursor();
static void draw_text_cursor_cursor(	EditStruct *edit, GC gc);
static void clear_text_area(		EditStruct *edit,
					int x, int y, int width, int height);
static void blacken_textcursor_cursor(	EditStruct *edit, int cur_x);
void	    write_text_region(		struct cursorRec *region, char* line);

#else

  XFontStruct *get_fontstruct();
  EditStruct *get_edit_struct();
  void init_edit_struct(), grab_keys_for_textcursor();
  void grab_keys_for_textcursor(), clear_edit_buf();
  void set_cursor_file_coords(), save_cursor_as_region(), disp_region();
  void store_edit_struct();
  void load_edit_string(), reset_textcursor_coords();
  void set_edit_draw_func(), unset_edit_draw_func();
  int emacs_response();
  GC set_text_gc();
  void draw_textcursor();
  static void init_textcursor(), draw_textcursor_cursor();
  static void clear_text_area(), blacken_textcursor_cursor();

#endif


/*  Subroutine:	init_textcursor
 *  Purpose:	initialize editor parameters for text cursor editing
 */
static void init_textcursor()
{
  /*  Get same font used for region labels  */
  fontstruct = get_fontstruct(1);
  /*  Allocate text editor struct space  */
  cursor_edit = get_edit_struct((CURSOR_MAX-1)*4);
  /*  Initialize text editor parameters  */
  init_edit_struct(dispbox.display, dispbox.ID, cursor_edit, fontstruct,
		   cursor.draw->foreground, cursor.draw->background);
  cursor_edit->y = cursor.win.y;
  cursor_edit->x = cursor.win.x;
  cursor_edit->max_pixlen = 512;
  cursor_edit->area_y =
    cursor_edit->y - cursor_edit->fontstruct->max_bounds.ascent;
  /*  Set this value initially so position cursor will appear  */
  cursor_edit->pixlen[1] = XTextWidth(cursor_edit->fontstruct, "e", 1);
}


/*  Subroutine:	new_textcursor()
 *  Purpose:	set up cursor struct for text cursor interactions
 */
void new_textcursor()
{
  struct curText *curtext;

  if( init ) {
    init_textcursor();
    init = 0;
  }
  curtext = (struct curText *)cursor.points;
  curtext->char_cnt = 0;
  curtext->string[0] = '\0';
  cursor.point_cnt = 2;
  grab_keys_for_textcursor(1);
}


/*  Subroutine:	clear_textcursor
 *  Purpose:	empty text cursor buffer and ungrab keyboard input
 */
void clear_textcursor()
{
  grab_keys_for_textcursor(0);
  clear_edit_buf(cursor_edit);
}


/*  Subroutine:	save_textcursor
 *  Purpose:	save string in editor on region save event
 */
#ifdef ANSIC
void save_textcursor ( XButtonEvent *xbutton )
#else
void save_textcursor ( xbutton )
     XButtonEvent *xbutton;
#endif
{
  struct curText *curtext;

  /*  Force file coordinates into agreement with window coordinates  */
  set_cursor_file_coords(&cursor, &coord.disptofile, 1);
  /*  Save cursor as a region  */
  if( xbutton->button == Button2 )
    save_cursor_as_region(&cursor, 1);
  else
    save_cursor_as_region(&cursor, 0);
  cursor.index = cursor.next_region->index;
  disp_region(cursor.next_region);
  store_edit_struct(cursor_edit);
  curtext = (struct curText *)cursor.next_region->points;
  --(curtext->char_cnt);
  curtext->string[curtext->char_cnt] = '\0';
}


/*  Subroutine:	reload_textcursor
 *  Purpose:	Put cursor string into editor buffer
 */
void reload_textcursor()
{
  struct curText *curtext;

  curtext = (struct curText *)cursor.points;
  load_edit_string(cursor_edit, curtext->string, curtext->char_cnt);
  reset_textcursor_coords(&cursor);
}


/*  Subroutine:	textcursor_keyentry
 *  Purpose:	Respond to key entry while in textcursor mode
 */
#ifdef ANSIC
void textcursor_keyentry ( XKeyEvent *xkey, KeySym keysym )
#else
void textcursor_keyentry ( xkey, keysym )
     XKeyEvent *xkey;
     KeySym keysym;
#endif
{
  struct curText *curtext;
  int old_end_x, old_cur_x;

  /*  Always use latest func, mask, and background  */
  if( cursor.draw->mask != AllPlanes ) {
    old_end_x = 0;
    old_cur_x = -1;
    draw_textcursor(&cursor, &color.gcset.undraw);
  } else {
    old_end_x = cursor_edit->pixlen[cursor_edit->char_cnt];
    old_cur_x = cursor_edit->pixlen[cursor_edit->active_position];
  }
  /*  Intercept Linefeed so editor won't act on it  */
  if( (keysym == XK_Linefeed) || (keysym == XK_Return) ) {
    /*  Do a save and move cursor for next line on a carriage return  */
    if( keysym == XK_Return )
      ascii_region(&control.event.xkey, XK_s);
    else
      ascii_region(&control.event.xkey, XK_e);
    /*  Move line done hight of text and set float coordinate as well  */
    if( (cursor.win.y += cursor_edit->area_height) < 0 )
      cursor.win.Y = (double)cursor.win.y - 0.5;
    else
      cursor.win.Y = (double)cursor.win.y + 0.5;
    curtext = (struct curText *)cursor.points;
    curtext->string[0] = '\0';
    curtext->string[1] = '\0';
    curtext->char_cnt = 0;
    cursor.point_cnt = 2;
    load_edit_string(cursor_edit, curtext->string, 0);
    cursor_edit->active_position = 0;
    reset_textcursor_coords(&cursor);
    /*  Redraw the display window to clean things up and show new positions  */
    disp_dispbox();
    return;
  } else {
    /*  Suppress drawing by the editor (we do it here)  */
    set_edit_draw_func(cursor.draw->func, 0);
    (void)emacs_response(xkey, cursor_edit);
    /*  Update cursor string to account for changes (incl. space on end)  */
    curtext = (struct curText *)cursor.points;
#ifdef ANSIC
    (void)strncpy(curtext->string, cursor_edit->string,
		  (size_t)(cursor_edit->char_cnt + 1));
#else
    strncpy(curtext->string, cursor_edit->string, cursor_edit->char_cnt + 1);
#endif
    curtext->string[cursor_edit->char_cnt + 1] = '\0';
    curtext->char_cnt = cursor_edit->char_cnt;
    /*  Restore the default editor string drawing functions  */
    unset_edit_draw_func();
  }
  /*  Use point count to reserve space for one byte more than string  */
  /*  Point count is used by save cursor to copy bytes to region record  */
  cursor.point_cnt = ((curtext->char_cnt + 1) / sizeof(XPoint)) + 2;
  /*  If string got shorter and not erasing, blank stuff no longer covered  */
  if( old_end_x > cursor_edit->pixlen[cursor_edit->char_cnt] ) {
    int new_end_x;
    GC gc;

    new_end_x = cursor_edit->pixlen[cursor_edit->char_cnt];
    clear_text_area(cursor_edit, cursor_edit->x + new_end_x,
		    cursor_edit->y
		     - cursor_edit->fontstruct->max_bounds.ascent,
		    old_end_x - new_end_x,
		    cursor_edit->area_height);
  }
  /*  If cursor was or will not be erased, set it to background color  */
  if( old_cur_x >= old_end_x )
    blacken_textcursor_cursor(cursor_edit, old_cur_x);
  /*  Draw new string  */
  draw_textcursor(&cursor, cursor.draw);
}


/*  Subroutine:	draw_textcursor
 *  Purpose:	draw a cursor, using only cursorRec info
 */
#ifdef ANSIC
void draw_textcursor ( struct cursorRec *crsr, GCspec *draw )
#else
void draw_textcursor ( crsr, draw )
     struct cursorRec *crsr;
     GCspec *draw;
#endif
{
  struct curText *curtext;
  GC gc;


  curtext = (struct curText *)crsr->points;
  if( draw == NULL )
    draw = crsr->draw;
  /*  Make sure info is available  */
  if( init ) {
    init_textcursor();
    init = 0;
  }
  /*  Stencil chars if tracking the string or not having a background  */
  if( (draw->func == GXxor) ||
      (draw->mask != AllPlanes) ) {
    gc = set_text_gc(cursor_edit->font, draw->foreground, U_DONT_CARE,
		     draw->func, draw->mask);
    XDrawString(crsr->win.display, crsr->win.ID, gc, crsr->win.x,
		crsr->win.y, curtext->string, curtext->char_cnt);
    if( crsr == &cursor ) {
      /*  Update coords (cursor may have been moved in non-text mode)  */
      cursor_edit->x = cursor.win.x;
      cursor_edit->y = cursor.win.y;
      draw_textcursor_cursor(cursor_edit, gc);
    }
  } else {
    unsigned long foreground, background; /*  Text and background colors */

    /*  These backgrounds don't work with overlays, should use image dark  */
    foreground = draw->foreground;
    if( draw == &color.gcset.incl ) {
      /*  Include is ALWAYS yellow or white on black  */
      background = color.hard.true_black;
      /*  This must be keyed to what disppsct.c does for hardcopy  */
      if( foreground == color.hard.std_black )
	foreground == color.hard.true_white;
    } else if( draw == &color.gcset.excl ) {
      /*  Exclude is ALWAYS red or black on white  */
      background = color.hard.true_white;
      /*  This must be keyed to what disppsct.c does for hardcopy  */
      if( foreground == color.hard.std_white )
	foreground = color.hard.true_black;
    } else if( draw->foreground == color.hard.true_black )
      background = color.hard.true_white;
    else
      background = color.hard.true_black;
    gc = set_text_gc(cursor_edit->font, foreground, background,
		     draw->func, draw->mask);
    XDrawImageString(cursor_edit->display, cursor_edit->ID, gc, crsr->win.x,
		     crsr->win.y, curtext->string, curtext->char_cnt);
    if( crsr == &cursor ) {
      cursor_edit->background = background;
      /*  Update coords (cursor may have been moved in non-text mode)  */
      cursor_edit->x = cursor.win.x;
      cursor_edit->y = cursor.win.y;
      draw_textcursor_cursor(cursor_edit, gc);
    }
  }
}


/*  Subroutine:	draw_textcursor_cursor
 *  Xlib calls:	XDrawImageString()
 */
#ifdef ANSIC
static void draw_textcursor_cursor ( EditStruct *edit, GC gc )
#else
static void draw_textcursor_cursor ( edit, gc )
     EditStruct *edit;
     GC gc;
#endif
{
  int x;

  x = edit->x + edit->pixlen[edit->active_position] -
    edit->pixlen[edit->first_char_shown];
  XDrawLine(edit->display, edit->ID, gc, x, edit->y + 1,
	    x + edit->pixlen[1] - 1, edit->y + 1);
}


/*  Subroutine:	reset_textcursor_coords
 */
#ifdef ANSIC
void reset_textcursor_coords( struct cursorRec *crsr )
#else
void reset_textcursor_coords( crsr )
     struct cursorRec *crsr;
#endif
{
  if( crsr == &cursor ) {
    cursor_edit->x = cursor.win.x;
    cursor_edit->y = cursor.win.y;
    cursor_edit->area_y =
      cursor_edit->y - cursor_edit->fontstruct->max_bounds.ascent;
    cursor_edit->active_position = cursor_edit->char_cnt;
  }
}


/*  Subroutine:	move_textcursor
 *  Purpose:	Move the text drawing coords when text cursor is moved
 */
#ifdef ANSIC
void move_textcursor ( int xinc, int yinc )
#else
void move_textcursor ( xinc, yinc )
     int xinc, yinc;
#endif
{
  cursor_edit->x += xinc;
  cursor_edit->y += yinc;
  cursor_edit->area_y += yinc;
}


/*  Subroutine:	clear_text_area
 *  Purpose:	Clear area around edit string, mindful of mask in use
 *  Note:	Assumes only called with GXcopy, AllPlanes, ImageString cursor
 */
#ifdef ANSIC
static void clear_text_area ( EditStruct *edit,
			      int x, int y, int width, int height )
#else
static void clear_text_area ( edit, x, y, width, height )
     EditStruct *edit;
     int x, y, width, height;
#endif
{
  GC gc;

  gc = set_text_gc(edit->font, edit->background, U_DONT_CARE,
		   GXcopy, AllPlanes);
  XFillRectangle(edit->display, edit->ID, gc, x, y,
		 (unsigned int)width, (unsigned int)height);
}


/*  Subroutine:	blacken_textcursor_cursor
 *  Xlib calls:	XDrawImageString()
 */
#ifdef ANSIC
static void blacken_textcursor_cursor ( EditStruct *edit, int cur_x )
#else
static void blacken_textcursor_cursor ( edit, cur_x )
     EditStruct *edit;
     int cur_x;
#endif
{
  int x;
  GC gc;

  gc = set_text_gc(edit->font, edit->background, U_DONT_CARE,
		   GXcopy, AllPlanes);
  x = edit->x + cur_x - edit->pixlen[edit->first_char_shown];
  XDrawLine(edit->display, edit->ID, gc, x, edit->y + 1,
	    x + edit->pixlen[1] - 1, edit->y + 1);
}


/*  Subroutine:	write_text_region
 *  Purpose:	Write detail part of region file line for a text cursor
 */
#ifdef ANSIC
void write_text_region ( struct cursorRec *region, char* line )
#else
void write_text_region ( region, line )
     struct cursorRec *region;
     char* line;
#endif
{
  struct curText *curtext;
  int len;

  curtext = (struct curText *)region->points;
  curtext->string[curtext->char_cnt] = '\0';
  len = strlen(line);
  (void)sprintf(&line[len], ",%1d,\"%s\")",
		curtext->char_cnt, curtext->string);
}
