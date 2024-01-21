#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	editrdrw.c (Editor Redraw)
 * Purpose:	Move cursor and redraw the line after editing
 * Subroutine:	move_edit_cursor()		returns: void
 * Subroutine:	place_edit_cursor()		returns: void
 * Subroutine:	redraw_after_delete()		returns: void
 * Subroutine:	redraw_after_insert()		returns: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 July 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include "hfiles/edit.h"

/*
 * Subroutine:	move_edit_cursor
 */
void move_edit_cursor ( edit, movement )
     EditStruct *edit;
     int movement;
{
  void place_edit_cursor();
  place_edit_cursor(edit, edit->active_position + movement);
}

/*
 * Subroutine:	place_edit_cursor
 */
void place_edit_cursor ( edit, new_position )
     EditStruct *edit;
     int new_position;
{
  int first_char, nchars;
  int size, i;
  void draw_edit_string(), draw_edit_cursor();

  if( new_position < 0 )
    new_position = 0;
  if( new_position > edit->char_cnt )
    new_position = edit->char_cnt;
  if( edit->oversize ) {
    if( new_position < edit->first_char_shown ) {
      first_char = new_position;
      edit->first_char_shown = new_position;
      size = edit->pixlen[first_char] + edit->max_pixlen;
      for( i = edit->char_cnt; edit->pixlen[i] > size; i-- );
      nchars = i - first_char;
      edit->active_position = new_position;
      draw_edit_string(edit, first_char, 0, nchars);
    } else if( new_position > edit->last_char_shown ) {
      size = edit->pixlen[new_position + 1] - edit->max_pixlen;
      for( i = 0; edit->pixlen[i] < size; i++ );
      first_char = i;
      edit->first_char_shown = i;
      nchars = 1 + new_position - i;
      edit->active_position = new_position;
      draw_edit_string(edit, first_char, 0, nchars);
    } else {
      draw_edit_cursor(edit, 1);
      edit->active_position = new_position;
      draw_edit_cursor(edit, 0);
    }
  }
  draw_edit_cursor (edit, 1);
  edit->active_position = new_position;
  draw_edit_cursor (edit, 0);
}

/*
 * Subroutine:	redraw_after_delete
 * Purpose:	Update the displayed string after character deletion
 * Note:	Characters not moved are not redrawn
 * Note:	Characters after any change are redrawn.  If proportional
 *		spacing font is used, they might need it.
 */
void redraw_after_delete ( edit )
     EditStruct *edit;
{
  int i;
  int first_char, first_x, nchars;
  int size, excess;
  void draw_edit_string();

  /* this section covers deciding what to print */
  first_char = edit->active_position;
  first_x = edit->pixlen[first_char];
  nchars = 1 + edit->char_cnt - edit->active_position;
  if( edit->oversize ) {
    /* line was oversized before */
    if( (edit->pixlen[edit->active_position+1] <= edit->max_pixlen) ) {
      /* line is no longer oversized */
      edit->oversize = 0;
      first_char = 0;
      first_x = 0;
      nchars = edit->char_cnt + 1;
      edit->first_char_shown = 0;
    } else {
      /* line is still oversized */
      if( edit->active_position < edit->first_char_shown )
	/* active position moves back before section shown */
	edit->first_char_shown = edit->active_position;
      size = edit->pixlen[edit->first_char_shown] + edit->max_pixlen;
      if( (excess = size - edit->pixlen[edit->char_cnt+1]) >
	  edit->charsz[edit->first_char_shown - 1]) {
	/* offset section ends short of end of window (space for more chars) */
	for( i = edit->first_char_shown - 1;
	     (i >= 0) && (excess > edit->charsz[i]); i-- )
	  excess -= edit->charsz[i];
	/* move first_char_shown to use up entire space */
	edit->first_char_shown = i + 1;
	first_char = edit->first_char_shown;
	nchars = 1 + edit->char_cnt - first_char;
      } else {
	/* line will run beyond space, determine how many chars will fit */
	for( i = edit->char_cnt; edit->pixlen[i+1] > size; i-- );
	nchars = 1 + i - first_char;
      }
      first_x =
	edit->pixlen[edit->first_char_shown] - edit->pixlen[first_char];
    }
  }
  draw_edit_string(edit, first_char, first_x, nchars);
}

/*
 * Subroutine:	redraw_after_insert
 * Purpose:	Update the displayed string after character insertion
 * Note:	Insertion is assumed to be directly before active position
 * Note:	Characters not moved are not redrawn
 * Note:	Characters after any change are redrawn.  If proportional
 *		spacing font is used, they might need it.
 */
void redraw_after_insert ( edit, insert_cnt )
     EditStruct *edit;
     int insert_cnt;	/* i: number of characters just inserted */
{
  int i;
  int first_char, first_x, nchars;
  int excess, size;
  void draw_edit_string();

  if( edit->pixlen[edit->char_cnt + 1] > edit->max_pixlen ) {
    edit->oversize = 1;
    if( (edit->pixlen[edit->active_position + 1] -
	 edit->pixlen[edit->first_char_shown]) > edit->max_pixlen ) {
      /* first_char_shown must be moved up to put active position in display */
      excess = edit->pixlen[edit->active_position + 1] - edit->max_pixlen;
      for( i=0; (i < edit->char_cnt) && (edit->pixlen[i] < excess); i++ );
      edit->first_char_shown = i;
      first_char = i;
      nchars = 1 + edit->active_position - i;
    } else {
      /* first_char_shown can stay in place, which chars must be drawn? */
      first_char = edit->active_position - insert_cnt;
      size = edit->max_pixlen + edit->pixlen[edit->first_char_shown];
      for( i = edit->char_cnt; edit->pixlen[i] > size; i-- );
      nchars = 1 + i - first_char;
    }
    first_x = edit->pixlen[first_char] - edit->pixlen[edit->first_char_shown];
  } else {
    first_char = edit->active_position - insert_cnt;
    nchars = 1 + edit->char_cnt - first_char;
    first_x = edit->pixlen[first_char];
  }
  draw_edit_string(edit, first_char, first_x, nchars);
}
