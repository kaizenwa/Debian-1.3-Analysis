#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	editinit.c (Editor Initialization)
 * Subroutine:	get_edit_struct()		returns: EditStruct *
 * Subroutine:	init_edit_struct()		returns: void
 * Subroutine:	load_edit_string()		returns: void
 * Subroutine:	load_edit_struct()		returns: void
 * Subroutine:	store_edit_struct()		returns: void
 * Subroutine:	recall_edit_struct()		returns: void
 * Subroutine:	clear_edit_buf()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 July 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{2} MVH load_edit_string() without save		18 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>
#include "hfiles/edit.h"

/*
 * Subroutine:	get_edit_struct
 * Returns:	Newly allocated popup editor structure
 */
EditStruct *get_edit_struct ( max_char_cnt )
     int max_char_cnt;
{
  EditStruct *edit;
  char *editbuf;
  int line_sz, i;
  char *calloc_errchk();

  /* set line_sz to first int boundary after (maxchar + 1) */
  line_sz = ((max_char_cnt + sizeof(int)) / sizeof(int)) * sizeof(int);
  editbuf = calloc_errchk(1, (unsigned int)sizeof(EditStruct) +
			  (line_sz * 
			   (((STACK_SZ + 2) * sizeof(int)) + STACK_SZ + 1)),
			   (char *)NULL);
  if( editbuf == NULL )
    return( NULL );
  edit = (EditStruct *)editbuf;
  editbuf += sizeof(EditStruct);
  edit->string = editbuf;
  for( i=0; i<STACK_SZ; i++ ) {
    editbuf += line_sz;
    edit->buf[i].string = editbuf;
  }
  editbuf += line_sz;
  line_sz = line_sz * sizeof(int);
  edit->pixlen = (int *)editbuf;
  editbuf += line_sz;
  edit->charsz = (int *)editbuf;
  for( i=0; i<STACK_SZ; i++ ) {
    editbuf += line_sz;
    edit->buf[i].charsz = (int *)editbuf;
  }
  edit->max_char_cnt = max_char_cnt;
  edit->pixlen[0] = 0;
  return( edit );
}

/*
 * Subroutine:	init_edit_struct
 * Purpose:	Initialize font and drawing info for popup editor structure
 */ 
void init_edit_struct ( display, w, edit, fontstruct, foreground, background )
     Display *display;		/* i: display which has popup window */
     Window w;			/* i: popup window */
     EditStruct *edit;		/* i: struct for edit strings */
     XFontStruct *fontstruct;	/* i: info about the font */
     unsigned long foreground;	/* i: needed for text GC */
     unsigned long background;
{
  edit->display = display;
  edit->ID = w;
  edit->fontstruct = fontstruct;
  edit->font = fontstruct->fid;
  edit->foreground = foreground;
  edit->background = background;
  edit->area_height =
    fontstruct->max_bounds.ascent + fontstruct->max_bounds.descent;
  if( edit->charsz[0] == 0 ) {
    edit->string[0] = ' ';
    edit->charsz[0] = XTextWidth(fontstruct, edit->string, 1);
    edit->pixlen[1] = edit->charsz[0];
  }
  edit->stack_cnt = 0;
  edit->stack_index = -1;
}

/*
 * Subroutine:	load_edit_string
 * Purpose:	Store a passed string in the current string buffer
 */
void load_edit_string ( edit, string, char_cnt )
     EditStruct *edit;
     char *string;
     int char_cnt;
{
  int i;

  if( char_cnt >= edit->max_char_cnt )
    char_cnt = edit->max_char_cnt - 1;
  (void)strncpy(edit->string, string, char_cnt);
  edit->char_cnt = char_cnt;
  edit->string[char_cnt] = ' ';
  edit->string[char_cnt+1] = '\0';
  for( i=0; i<=char_cnt; i++ ) {
    edit->charsz[i] = XTextWidth(edit->fontstruct, &(edit->string[i]), 1);
    edit->pixlen[i+1] = edit->pixlen[i] + edit->charsz[i];
  }
  if( edit->pixlen[char_cnt + 1] > edit->max_pixlen )
    edit->oversize = 1;
  else
    edit->oversize = 0;
}

/*
 * Subroutine:	load_edit_struct
 * Purpose:	Store a passed string in the editor storage stack
 * Note:	Also makes it the default entry
 * Exception:	Assume no other stored strings (erases any)
 */
void load_edit_struct ( edit, string, char_cnt )
     EditStruct *edit;
     char *string;
     int char_cnt;
{
  void store_edit_struct(), load_edit_string();

  load_edit_string(edit, string, char_cnt);
  store_edit_struct(edit);
}

/*
 * Subroutine:	recall_edit_struct
 * Purpose:	Retrieve a line from the storage stack
 */
void recall_edit_struct ( edit, next, clear )
     EditStruct *edit;
     int next;
     int clear;		/* next <0 clears current string */
{
  int i, char_cnt;
  void clear_edit_buf();

  if( next >= edit->stack_cnt ) {
    edit->stack_index = edit->stack_cnt - 1;
    return;
  } else if( next < 0 ) {
    if( clear )
      clear_edit_buf (edit);
    edit->stack_index = -1;
    return;
  }
  char_cnt = edit->buf[next].char_cnt;
  edit->char_cnt = char_cnt;
  bcopy(edit->buf[next].string, edit->string, char_cnt + 1);
  bcopy((char *)edit->buf[next].charsz, (char *)edit->charsz,
	(char_cnt + 1) * sizeof(int));
  for( i=0; i<=char_cnt; i++ ) {
    edit->pixlen[i+1] = edit->pixlen[i] + edit->charsz[i];
  }
  if( edit->pixlen[edit->char_cnt + 1] > edit->max_pixlen )
    edit->oversize = 1;
  else
    edit->oversize = 0;
  edit->active_position = 0;
  edit->stack_index = next;
}

/*
 * Subroutine:	store_edit_struct
 * Purpose:	Put current line in the store stack
 */
void store_edit_struct ( edit )
     EditStruct *edit;
{
  int *charsz, i;
  char *string;

  if( edit->stack_cnt < STACK_SZ )
    (edit->stack_cnt)++;
  charsz = edit->buf[edit->stack_cnt-1].charsz;
  string = edit->buf[edit->stack_cnt-1].string;
  for( i=edit->stack_cnt-1; i>0; i-- ) {
    edit->buf[i].char_cnt = edit->buf[i-1].char_cnt;
    edit->buf[i].charsz = edit->buf[i-1].charsz;
    edit->buf[i].string = edit->buf[i-1].string;
  }
  edit->buf[0].char_cnt = edit->char_cnt;
  edit->buf[0].charsz = charsz;
  edit->buf[0].string = string;
  bcopy(edit->string, string, edit->char_cnt + 1);
  bcopy((char *)edit->charsz, (char *)charsz,
	(edit->char_cnt + 1) * sizeof(int));
  edit->active_position = edit->char_cnt;
  edit->stack_index = -1;
}

/*
 * Subroutine:	clear_edit_buf
 * Purpose:	Set edit buffer string to no characters
 */
void clear_edit_buf ( edit )
     EditStruct *edit;
{
  edit->string[0] = edit->string[edit->char_cnt];
  edit->charsz[0] = edit->charsz[edit->char_cnt];
  edit->pixlen[1] = edit->charsz[0];
  edit->char_cnt = 0;
  edit->active_position = 0;
}
