/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     facility.c
 * Abstract:        facility word set
 */

#include "yforth.h"
#include "udio.h"
#include "facility.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _at_x_y() {
	register Cell y = *sp++;
	d_gotoxy(*sp++, y);
}

void _key_question() {
	*--sp = FFLAG(d_kbhit());
}

void _page() {
	d_clrscr();
}

