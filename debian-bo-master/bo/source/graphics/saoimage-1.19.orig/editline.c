#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	editline.c (Editor Line)
 * Purpose:	Manipulate the popup editor character string
 * Subroutine:	insert_chars()			returns: void
 * Subroutine:	delete_chars_backward()		returns: void
 * Subroutine:	delete_chars_forward()		returns: void
 * Subroutine:	next_word_end()			returns: int
 * Subroutine:	last_word_end()			returns: int
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
 * Subroutine:	insert_chars
 * Purpose:	Insert new characters into the edit line
 * Returns:	Number of characters inserted
 */
void insert_chars ( edit, str, len )
     EditStruct *edit;
     char *str;
     int len;
{
  int i, j;
  void redraw_after_insert();

  /* check for end of buffer */
  if( (edit->char_cnt + len) > edit->max_char_cnt ) {
    (void)fprintf(stderr, "Line buffer full!\n");
    if( edit->char_cnt < edit->max_char_cnt ) {
      len = edit->max_char_cnt - edit->char_cnt;
    } else
      return;
  }
  /* move later characters forward */
  for( i = edit->char_cnt, j = i + len;
       i >= edit->active_position; i--, j-- ) {
    edit->charsz[j] = edit->charsz[i];
    edit->string[j] = edit->string[i];
  }
  /* insert new characters */
  for( j = 0, ++i; j < len; j++, i++ ) {
    edit->string[i] = str[j];
    edit->charsz[i] = XTextWidth(edit->fontstruct, &str[j], 1);
    edit->pixlen[i+1] = edit->pixlen[i] + edit->charsz[i];
  }
  edit->char_cnt += len;
  edit->active_position += len;
  /* update running lengths */
  for( i--; i <= edit->char_cnt; i++ )
    edit->pixlen[i+1] = edit->pixlen[i] + edit->charsz[i];
  redraw_after_insert(edit, len);
}

/*
 * Subroutine:	delete_chars_backward
 * Purpose:	Delete characters before the active position
 */
void delete_chars_backward ( edit, len )
     EditStruct *edit;
     int len;
{
  int i, j;
  void redraw_after_delete();

  /* delete before beginning of line? */
  if( len > edit->active_position ) {
    (void)fprintf(stderr, "Beginning of line!\n");
    if( edit->active_position > 0 )
      len = edit->active_position;
    else
      return;
  }
  /* move later characters backward */
  for( i = edit->active_position, j = i - len;
       i <= edit->char_cnt; i++, j++ ) {
    edit->charsz[j] = edit->charsz[i];
    edit->string[j] = edit->string[i];
  }
  edit->char_cnt -= len;
  edit->active_position -= len;
  /* update running lengths */
  for( i=edit->active_position; i<=edit->char_cnt; i++ )
    edit->pixlen[i+1] = edit->pixlen[i] + edit->charsz[i];
  redraw_after_delete(edit);
}

/*
 * Subroutine:	delete_chars_forward
 * Purpose:	Delete characters from the active position forward
 */
void delete_chars_forward ( edit, len )
     EditStruct *edit;
     int len;
{
  int i, j;
  void redraw_after_delete();

  /* delete beyond end of line? */
  if( (edit->active_position + len) > edit->char_cnt ) {
    if( edit->active_position < edit->char_cnt )
      len = edit->char_cnt - edit->active_position;
    else
      return;
  }
  /* move later characters backward */
  for( j = edit->active_position, i = j + len;
       i <= edit->char_cnt; i++, j++ ) {
    edit->charsz[j] = edit->charsz[i];
    edit->string[j] = edit->string[i];
  }
  edit->char_cnt -= len;
  /* update running lengths */
  for( i=edit->active_position; i<=edit->char_cnt; i++ )
    edit->pixlen[i+1] = edit->pixlen[i] + edit->charsz[i];    
  redraw_after_delete(edit);
}

/*
 * Subroutine:	next_word_end
 * Returns:	The distance from the active position to the next space
 *		following a character (or the end of the line)
 */
int next_word_end ( edit )
     EditStruct *edit;
{
  int i;
  for( i=edit->active_position;
       (i < edit->char_cnt) && ((edit->string[i] == ' ') ||
				  (edit->string[i] == '/') ||
				  (edit->string[i] == ',') ||
				  (edit->string[i] == '.') ||
				  (edit->string[i] == ';') ||
				  (edit->string[i] == ':')); i++ );
  /* string[char_cnt]==' ', so loop will stop there, if nowhere else */
  if( i < edit->char_cnt ) {
    while( (edit->string[i] != ' ') && (edit->string[i] != '/') &&
	   (edit->string[i] != ',') && (edit->string[i] != '.') &&
	   (edit->string[i] != ';') && (edit->string[i] != ':') )
      i++;
  }
  return( i - edit->active_position );
}

/*
 * Subroutine:	last_word_start
 * Returns:	The distance from the active position to the first character
 *		following a space found in a backward search (or the start of
 *		the line)
 */
int last_word_start ( edit )
     EditStruct *edit;
{
  int i;
  if( edit->active_position == 0 )
    return( 0 );
  for( i = edit->active_position - 1;
       (i >= 0) && ((edit->string[i] == ' ') || (edit->string[i] == '/') ||
		    (edit->string[i] == ',') || (edit->string[i] == '.') ||
		    (edit->string[i] == ';') || (edit->string[i] == ':'));
      i-- );
  if( i >= 0 ) {
    while( (i >= 0) &&
	   ((edit->string[i] != ' ') && (edit->string[i] != '/') &&
	    (edit->string[i] != ',') && (edit->string[i] != '.') &&
	    (edit->string[i] != ';') && (edit->string[i] != ':')) )
      i--;
  }
  return( (i+1) - edit->active_position );
}
