#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	editdraw.c (Editor Draw)
 * Purpose:	Draw the edited line in the popup window
 * Subroutine:	set_edit_draw_func()		returns: void
 * Subroutine:	unset_edit_draw_func()		returns: void
 * Subroutine:	draw_new_string()		returns: void
 * Subroutine:	redraw_edit_string()		returns: void
 * Subroutine:	draw_edit_string()		returns: void
 * Subroutine:	draw_edit_cursor()		returns: void
 * Xlib calls:	XClearArea(), XDrawImageString(), XPutImage()
 * Copyright:	1989, 1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 July 1989
 *		{1} MVH ability to use specified func and mask	   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include "hfiles/define.h"	/*  Define DONT_CARE, U_DONT_CARE  */
#include "hfiles/edit.h"


/*  Flag to switch between default drawing and client mask and function  */
int basic_func = 1;
/*  Values used to draw with GC func other than GXcopy to AllPlanes  */
int editor_func = GXcopy;
unsigned long editor_mask = AllPlanes;

static char left_bits[7] = { 0x20, 0x30, 0x38, 0x3c, 0x38, 0x30, 0x20 };
static char right_bits[7] = { 0x04, 0x0c, 0x1c, 0x3c, 0x1c, 0x0c, 0x04 };

static XImage ximage = {
  8, 7, 0, XYBitmap, NULL, LSBFirst, 8, LSBFirst, 8, 1, 1, 0 };

/* define offsets for drawing overflow marks */
#define MARK_LEFT 10
#define MARK_RIGHT 2
#define MARK_Y 8


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		set_edit_draw_func(	int func, unsigned long mask);
void		unset_edit_draw_func();
void		draw_new_string(	EditStruct *edit, int clear);
void		redraw_edit_string(	EditStruct *edit);
void		draw_edit_string(	EditStruct *edit, int first_char,
					int first_x, int nchars);
void		draw_edit_cursor(	EditStruct *edit, int erase);
static void	mark_left(		EditStruct *edit, GC gc);
static void	clear_left(		EditStruct *edit);
static void	mark_right(		EditStruct *edit, GC gc);
static void	clear_right(		EditStruct *edit);
static void	clear_area(		EditStruct *edit, int x, int y,
					int width, int height);

#else

  void draw_edit_string(), draw_edit_cursor();
  GC set_text_gc();
  static void clear_area();
  static void mark_left(), clear_left(), mark_right(), clear_right();

#endif


/*  Subroutine:	set_edit_draw_func
 *  Purpose:	Set draw func to func and mask
 */
#ifdef ANSIC
void set_edit_draw_func ( int func, unsigned long mask )
#else
void set_edit_draw_func ( func, mask )
     int func;
     unsigned long mask;
#endif
{
  editor_func = func;
  editor_mask = mask;
  basic_func = 0;
}


/*  Subroutine:	unset_edit_draw_func
 *  Purpose:	Unset draw func to func and mask
 */
void unset_edit_draw_func()
{
  editor_func = GXcopy;
  editor_mask = AllPlanes;
  basic_func = 1;
}


/*  Subroutine:	draw_new_string
 *  Purpose:	Draw a string for the first time (new window or new string)
 */
#ifdef ANSIC
void draw_new_string ( EditStruct *edit, int clear )
#else
void draw_new_string ( edit, clear )
     EditStruct *edit;
     int clear;		/* i: clear-window-before-drawing */
#endif
{
  int first_char, first_x, nchars, i, len;

  first_char = 0;
  first_x = 0;
  if( edit->pixlen[edit->char_cnt + 2] > edit->max_pixlen ) {
    edit->oversize = 1;
    len = edit->pixlen[edit->char_cnt + 2];
    for( i = 0;
	(i <= edit->char_cnt) && (len - edit->pixlen[i]) > edit->max_pixlen;
	i++ );
    first_char = i;
    nchars = edit->char_cnt + 1 - i;
  } else {
    edit->oversize = 0;
    nchars = edit->char_cnt + 1;
  }
  edit->left_mark_drawn = 0;
  edit->right_mark_drawn = 0;
  edit->last_x_shown = 0;
  edit->first_char_shown = first_char;
  edit->active_position = edit->char_cnt;
  /*  Draw if the mask is not 0  */
  if( editor_mask ) {
    if( clear )
      clear_area(edit, edit->x - MARK_LEFT, edit->area_y,
		 edit->max_pixlen + MARK_LEFT + MARK_RIGHT + 8,
		 edit->area_height);
    draw_edit_string(edit, first_char, first_x, nchars);
  }
}


/*  Subroutine:	redraw_edit_string
 *  Purpose:	Redraw the string as it was after a popup window expose event
 */
#ifdef ANSIC
void redraw_edit_string ( EditStruct *edit )
#else
void redraw_edit_string ( edit )
     EditStruct *edit;
#endif
{
  int first_char, first_x, nchars;

  edit->left_mark_drawn = 0;
  edit->right_mark_drawn = 0;
  first_x = 0;
  first_char = edit->first_char_shown;
  nchars = edit->last_char_shown + 1 - first_char;
  draw_edit_string(edit, first_char, first_x, nchars);
}


/*  Subroutine:	draw_edit_string
 *  Purpose:	Display a section of string and the text cursor
 *  Xlib calls:	XDrawImageString()
 */
#ifdef ANSIC
void draw_edit_string ( EditStruct *edit, int first_char,
			int first_x, int nchars )
#else
void draw_edit_string ( edit, first_char, first_x, nchars )
     EditStruct *edit;
     int first_char;
     int first_x;
     int nchars;
#endif
{
  int x;
  GC gc;

  /*  Don't draw if the mask is 0  */
  if( editor_mask == 0 ) {
    edit->last_x_shown = edit->pixlen[first_char + nchars]
      - edit->pixlen[edit->first_char_shown];
    edit->last_char_shown = first_char + nchars - 1;
    return;
  }
  gc = set_text_gc(edit->font, edit->foreground, edit->background,
		   editor_func, editor_mask);
  /*  Stencil chars if tracking the string or not having a background  */
  if( (editor_func == GXxor) ||
      (edit->background == U_DONT_CARE) ||
      (edit->background == edit->foreground) )
    XDrawString(edit->display, edit->ID, gc, edit->x + first_x, edit->y,
		&(edit->string[first_char]), nchars);
  else
    XDrawImageString(edit->display, edit->ID, gc, edit->x + first_x, edit->y,
		     &(edit->string[first_char]), nchars);
  /*  Calculate x offset at end of string  */
  x = edit->pixlen[first_char + nchars] - edit->pixlen[edit->first_char_shown];
  /*  Clean up after end of string  */
  if( editor_mask && (x < edit->last_x_shown) )
    clear_area(edit, edit->x + x, edit->area_y,
	       edit->last_x_shown - x, edit->area_height);
  edit->last_x_shown = x;
  edit->last_char_shown = first_char + nchars - 1;
  /*  Don't draw anything is mask is 0  */
  if( editor_mask == 0 )
    return;
  /*  Mark line truncation at either end  */
  if( edit->oversize || edit->left_mark_drawn || edit->right_mark_drawn ) {
    if( edit->first_char_shown > 0 ) {
      if( !edit->left_mark_drawn ) {
	mark_left(edit, gc);
	edit->left_mark_drawn = 1;
      }
    } else if( edit->left_mark_drawn ) {
      clear_left(edit);
      edit->left_mark_drawn = 0;
    }
    if( edit->char_cnt > edit->last_char_shown ) {
      if( !edit->right_mark_drawn ) {
	mark_right(edit, gc);
	edit->right_mark_drawn = 1;
      }
    } else if( edit->right_mark_drawn ) {
      clear_right(edit);
      edit->right_mark_drawn = 0;
    }
  }
  draw_edit_cursor(edit, 0);
}


/*  Subroutine:	draw_edit_cursor
 *  Xlib calls:	XDrawImageString()
 */
#ifdef ANSIC
void draw_edit_cursor ( EditStruct *edit, int erase )
#else
void draw_edit_cursor ( edit, erase )
     EditStruct *edit;
     int erase;
#endif
{
  int x;
  GC gc;

  /*  Don't draw cursor if no mask bits  */
  if( editor_mask == 0 )
    return;
  if( erase )
    gc = set_text_gc(edit->font, edit->foreground, edit->background,
		     editor_func, editor_mask);
  else
    gc = set_text_gc(edit->font, edit->background, edit->foreground,
		     editor_func, editor_mask);
  x = edit->pixlen[edit->active_position] -
    edit->pixlen[edit->first_char_shown];
  XDrawImageString(edit->display, edit->ID, gc, edit->x + x, edit->y,
		   &(edit->string[edit->active_position]), 1);
}


/*  Subroutine:	mark_left
 *  Xlib calls:	XPutImage()
 */
#ifdef ANSIC
static void mark_left ( EditStruct *edit, GC gc )
#else
static void mark_left ( edit, gc )
     EditStruct *edit;
     GC gc;
#endif
{
  ximage.data = left_bits;
  XPutImage(edit->display, edit->ID, gc, &ximage, 0, 0,
	    edit->x - MARK_LEFT, edit->y - MARK_Y, 8, 7);
}


/*  Subroutine:	clear_left
 */
#ifdef ANSIC
static void clear_left ( EditStruct *edit )
#else
static void clear_left ( edit )
     EditStruct *edit;
#endif
{
  clear_area(edit, edit->x - MARK_LEFT, edit->y - MARK_Y, 8, 7);
}


/*  Subroutine:	mark_right
 *  Xlib calls:	XPutImage()
 */
#ifdef ANSIC
static void mark_right ( EditStruct *edit, GC gc )
#else
static void mark_right ( edit, gc )
     EditStruct *edit;
     GC gc;
#endif
{
  ximage.data = right_bits;
  XPutImage(edit->display, edit->ID, gc, &ximage, 0, 0,
	    edit->x + edit->max_pixlen + MARK_RIGHT, edit->y - MARK_Y, 8, 7);
}


/*  Subroutine:	clear_right
 */
#ifdef ANSIC
static void clear_right ( EditStruct *edit )
#else
static void clear_right ( edit )
     EditStruct *edit;
#endif
{
  clear_area(edit, edit->x + edit->max_pixlen + MARK_RIGHT,
	     edit->y - MARK_Y, 8, 7);
}


/*  Subroutine:	clear_area
 *  Purpose:	Clear area around edit string, mindful of mask in use
 */
#ifdef ANSIC
static void clear_area ( EditStruct *edit,
			 int x, int y, int width, int height )
#else
static void clear_area ( edit, x, y, width, height )
     EditStruct *edit;
     int x, y, width, height;
#endif
{
  GC gc;

  gc = set_text_gc(edit->font, edit->background, U_DONT_CARE,
		   editor_func, editor_mask);
  XFillRectangle(edit->display, edit->ID, gc, x, y,
		 (unsigned int)width, (unsigned int)height);
}
