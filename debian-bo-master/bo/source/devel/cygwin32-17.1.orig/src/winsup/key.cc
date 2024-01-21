/*  key.c

  Steve Chamberlain of Cygnus Support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <windows.h>
#include "winsup.h"

int kbhit ()
{
  INPUT_RECORD i;
  DWORD n = 0;
  printf ("peek %d\n", PeekConsoleInput (u->self->hmap[0].h->get_handle (),
					&i,
					1,
					&n));

  return n;
}


int getkey ()
{

  INPUT_RECORD i;
  DWORD n;
  in ("getkey");


  do
    {
      ReadConsoleInput (u->self->hmap[0].h->get_handle (),
			&i,
			1,
			&n);


    } while (i.EventType != KEY_EVENT
	     || !i.Event.KeyEvent.bKeyDown
	     ||  i.Event.KeyEvent.ASCIICHAR == 0);
  out ("getkey");
  return i.Event.KeyEvent.ASCIICHAR;
}
