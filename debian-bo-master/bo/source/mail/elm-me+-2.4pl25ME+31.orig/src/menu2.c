/* $Id: menu2.c,v 1.1 1995/08/28 02:08:24 elkins Exp $
 *
 * This code originally written by Michael Elkins <elkins@aero.org>.
 *
 * $Log: menu2.c,v $
 * Revision 1.1  1995/08/28 02:08:24  elkins
 * Initial revision
 *
 */

#include "headers.h"
#include "menu2.h"
#include "me.h"

#define BOL 1
#define STR 2
#define INT 3

static int do_item P_((struct menu_item *));
static int do_item (it)
     struct menu_item *it;
{
  short len;
  char buf[STRING];
  int ch;

  ClearLine (elm_LINES-2);
  Write_to_screen (it->option, 0);
  len = strlen (it->option) + 1;
  if (it->type == STR)
    return 
      optionally_enter (it->d.c, elm_LINES-2, len, 
			OE_APPEND_CURRENT|OE_REDRAW_MARK,
			it->size);
  else if (it->type == BOL) {
    for (;;) {
      MoveCursor (elm_LINES-2, len);
      CleartoEOLN ();
      if (*it->d.i == TRUE)
	Write_to_screen ("TRUE", 0);
      else if (*it->d.i == FALSE)
	Write_to_screen ("FALSE", 0);
      ch = ReadCh (OE_REDRAW_MARK);
      if (ch == EOF) 
	return -1;
      if (ch == REDRAW_MARK)
	return REDRAW_MARK;
      if (ch == '\n')
	return 0;
      else if (ch == ' ')
	*it->d.i = *it->d.i ? FALSE : TRUE;
    }
  }
  else if (it->type == INT) {
    int code;
    sprintf (buf, "%d", *it->d.i);
    code = optionally_enter (buf, elm_LINES-2, len, 
			     OE_APPEND_CURRENT|OE_REDRAW_MARK,
			     sizeof buf);
    if (code != 0)
      return code;
    *it->d.i = atoi (buf);
  }
  return 0;
}

void
generic_menu (items, max, title, prompt)
     struct menu_item items[];
     short max;
     char *prompt, *title;
{
  short i = 0;
  char buf[STRING], buf2[STRING];
  int ch;
  short update = TRUE;
  int precmd = 0;

  ClearScreen ();
  for (;;) {
    if (update) {
      Centerline (1, title);
      for (i = 0 ; i < max ; i++) {
	sprintf (buf, "%-30.30s", items[i].option);
	if (items[i].type == BOL) {
	  if (*items[i].d.i == TRUE)
	    strfcat (buf, "TRUE", sizeof buf);
	  else
	    strfcat (buf, "FALSE", sizeof buf);
	}
	else if (items[i].type == STR)
	  strfcat (buf, items[i].d.c, sizeof buf);
	else if (items[i].type == INT) {
	  sprintf (buf2, "%d", *items[i].d.i);
	  strfcat (buf, buf2, sizeof buf);
	}
	ClearLine(items[i].offset);
	PutLine0 (items[i].offset, 0, buf);
      }
      update = FALSE;
    }
    ClearLine (elm_LINES-2);
    PutLine0 (elm_LINES-2, 0, prompt);
    if (precmd) {
      ch = precmd;
      precmd = 0;
    } else
      ch = ReadCh (REDRAW_MARK);
    if (EOF == ch)
      return;
    switch (ch) {
    case '\n':
      return;
    case REDRAW_MARK:
      update = TRUE;
      break;
    default:
      for (i = 0; i < max ; i++) {
	if (ch == items[i].key) {
	  int code = do_item (&items[i]);
	  if (REDRAW_MARK == code)
	    precmd = ch;
	  if (-1 == code)
	    return;
	  update = TRUE;
	  break;
	}
      }
      if (i == max && isascii(ch)) {
	for (i = 0; i < max ; i++) {
	  if (isascii (items[i].key) && 
	      tolower(ch) == tolower(items[i].key)) {
	    int code = do_item (&items[i]);
	    if (REDRAW_MARK == code)
	      precmd = ch;
	    if (-1 == code)
	      return;
	    update = TRUE;
	    break;
	  }
	}
      }
    }
  }
  /* Not reached. */
}
