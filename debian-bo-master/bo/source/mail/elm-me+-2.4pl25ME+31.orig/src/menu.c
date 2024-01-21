/* $Id: menu.c,v 1.2 1995/06/23 21:45:10 elkins Exp $
 *
 * API for creating menus in Elm.
 * Initially written by: Michael Elkins <elkins@aero.org>, 17 May 1995.
 *
 * PLEASE SEND ME ANY CHANGES YOU MAKE TO THIS API!
 *
 * The basic sequence of using this api is as follows:
 *
 *  menu_t menu;
 *
 *  MenuInit(&menu, "Test Menu", "Please Select an option: ", "q)uit");
 *  for (...)
 *    MenuAdd(&menu, "Menu item n");
 *  for (;;) {
 *    switch(MenuLoop(&menu)) {
 *    case '\n':
 *      selected_item = MenuCurrent(&menu);
 *      MenuDestroy(&menu);
 *      return(selected_item);
 *    }
 *  }
 *
 * $Log: menu.c,v $
 * Revision 1.2  1995/06/23  21:45:10  elkins
 * MenuLoop() should always display the prompt.
 *
 * Revision 1.1  1995/06/01  23:06:50  elkins
 * Initial revision
 *
 *
 */

#include "menu.h"
#include "headers.h"
#include "me.h"

/* creates a new menu instance.  'm' is a pointer to a 'menu_t', 't' is the
   menu title, 'p' is the prompt string and 'h' is a help string. 'h' may
   be specified as NULL if no help line is desired.  */
void
MenuInit (m, t, p, h)
menu_t *m;
char *t, *p, *h;
{
  int Len;
  m->data = 0;
  m->max = m->len = m->current = 0;
  m->update = 1; /* so we can see something on the first MenuLoop() call! */

  Len = strlen(t) + 1;
  m->title = (char *) safe_malloc(Len);
  strfcpy(m->title, t, Len);

  Len = strlen(p) + 1;
  m->prompt = (char *) safe_malloc(Len);
  strfcpy(m->prompt, p, Len);

  m->prompt_length = strlen(m->prompt);
  if (h) {
    Len = strlen(h) + 1;
    m->help = (char *) safe_malloc(Len);
    strfcpy(m->help, h, Len);
  } else
    m->help = 0;
  m->pagetop = 0;
}

void
MenuDestroy (m)
menu_t *m;
{
  if (m->title)
    free(m->title);
  if (m->prompt)
    free(m->prompt);
  DestroyDynamicArray(m->data);
}

int
MenuAdd (m, s)
menu_t *m;
char *s;
{
  int Len;
  if (m->len == m->max)
    m->data = (char **) DynamicArray(m->data, sizeof(char*),
				     &m->max, elm_LINES);
  if (!m->data)
    return(-1);

  Len = strlen(s) + 1;
  m->data[m->len] = (char *) safe_malloc(Len);
  strfcpy(m->data[m->len], s, Len);

  ++m->len;
  return(0);
}

int
MenuPrintLine (s, voffset, isCurrent) 
char *s;
int voffset, isCurrent;
{
  char *buf = (char*)safe_malloc(sizeof(char)*(elm_COLUMNS+1));

  if (!buf)
    return(-1);

  if (isCurrent && !arrow_cursor)
    StartInverse();
  sprintf(buf, "%s %-*.*s", isCurrent && arrow_cursor ? "->" : "  ",
	  elm_COLUMNS-4, elm_COLUMNS-4, s);
  PutLine0 (voffset, 0, buf);
  if (isCurrent && !arrow_cursor)
    EndInverse();
  free(buf);
  return(0);
}

/* the main loop for a menu.  it takes care of moving the selection and
   returns the command that the user selected. */
int
MenuLoop (m)
menu_t *m;
{
  int j, key_offset;
  int cmd;

  for (;;) {
    if (m->update) {
      ClearScreen();
      Centerline(1, m->title);
      for (j = 0; j < elm_LINES-6 && m->pagetop + j < m->len; j++)
	MenuPrintLine(m->data[m->pagetop+j], 3+j, m->pagetop+j == m->current);
      if (m->help)
	Centerline(elm_LINES-1, m->help);
      m->update = 0;
    }
    PutLine0(elm_LINES-2, 0, m->prompt);

    switch(cmd = ReadCh(REDRAW_MARK|READCH_CURSOR)) {
    case RIGHT_MARK:
    case PAGEDOWN_MARK:
      if (m->pagetop + elm_LINES-6 < m->len) {
	m->pagetop += elm_LINES-6;
	m->current = m->pagetop;
	m->update = 1;
      } else
	Centerline(elm_LINES, "You are already on the last page!");
      break;
    case LEFT_MARK:
    case PAGEUP_MARK:
      if (m->pagetop != 0) {
	m->pagetop -= elm_LINES-6;
	if (m->pagetop < 0)
	  m->pagetop = 0;
	m->current = m->pagetop;
	m->update = 1;
      } else
	Centerline(elm_LINES, "You are on the first page!");
      break;
    case REDRAW_MARK:
    case ctrl('L'):
      m->update = 1;
      break;
    case HOME_MARK:
    case '=': /* first item */
      m->current = 0;
      m->pagetop = 0;
      m->update = 1;
      break;
    case '*': /* last item */
      m->current = m->len - 1;
      m->pagetop = m->len - elm_LINES + 6;
      if (m->pagetop < 0)
	m->pagetop = 0;
      m->update = 1;
      break;
    case UP_MARK:
    case 'k':
    case 'K':
    case ctrl('P'):
previous_entry:
      if (m->current > 0) {
	if (m->current != m->pagetop) {
	  MenuPrintLine(m->data[m->current], 3 + m->current - m->pagetop, 0);
	  --m->current;
	  MenuPrintLine(m->data[m->current], 3 + m->current - m->pagetop, 1);
	} else { /* move to the previous page */
	  --m->current;
	  m->pagetop -= elm_LINES-6;
	  if (m->pagetop < 0)
	    m->pagetop = 0;
	  m->update = 1;
	}
      }
      break;
    case DOWN_MARK:
    case 'j':
    case 'J':
    case ctrl('N'):
    case 'n':
next_entry:
      if (m->current < m->len-1) {
	if (m->current < m->pagetop + elm_LINES-7) {
	  MenuPrintLine(m->data[m->current], 3 + m->current -  m->pagetop, 0);
	  ++m->current;
	  MenuPrintLine(m->data[m->current], 3 + m->current - m->pagetop, 1);
	} else { /* move to the next page */
	  ++m->current;
	  m->pagetop = m->current;
	  m->update = 1;
	}
      } else
	Centerline(elm_LINES, "You are on the last item!");
      break;
    default:
      return(cmd);
    }
  }
  /* not reached */
}
