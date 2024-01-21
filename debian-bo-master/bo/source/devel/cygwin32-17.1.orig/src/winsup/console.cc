/* console for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <windows.h>
#include "winsup.h"

/* FIXME: It's not clear what the right thing to do is if there is no console
   (e.g. if the user has telnet'd in, or similar).  Should Screen{Rows,Cols}
   return -1?  */

#define DEFAULT_ROWS 25
#define DEFAULT_COLS 80

int ScreenRows ()
{
  CONSOLE_SCREEN_BUFFER_INFO p;
  if (GetConsoleScreenBufferInfo (u->self->hmap[1].h->get_handle (), &p))
    return p.dwSize.Y;
  else
    return DEFAULT_ROWS;
}

int ScreenCols ()
{
  CONSOLE_SCREEN_BUFFER_INFO p;
  if (GetConsoleScreenBufferInfo (u->self->hmap[1].h->get_handle (), &p))
    return p.dwSize.X;
  else
    return DEFAULT_COLS;
}

void  ScreenGetCursor (int *row, int *col)
{
  CONSOLE_SCREEN_BUFFER_INFO p;
  if (GetConsoleScreenBufferInfo (u->self->hmap[1].h->get_handle (), &p))
    {
      *row = p.dwCursorPosition.Y;
      *col = p.dwCursorPosition.X;
    }
  else
    {
      *row = *col = 0;
    }
}

void ScreenSetCursor (int row, int col)
{
  COORD p;
  p.X = col;
  p.Y = row;
  SetConsoleCursorPosition (u->self->hmap[1].h->get_handle (), p);
}
