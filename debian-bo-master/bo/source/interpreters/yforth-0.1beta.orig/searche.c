/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:
 * Abstract:
 */

#include <stdio.h>
#include "yforth.h"
#include "searche.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _also() {
	if (top < WORD_LISTS) {
		top++;
		list[top] = list[top - 1];
	}
}

void _forth() {
	list[top] = forth_wid;
}

void _only() {
	top = 0;
	list[0] = forth_wid;
}

void _order() {
	register int i;
	printf("[%p] ", voc);
	for (i = 0; i <= top; i++) printf("%d: %p ", i, list[i]);
}

void _previous() {
	if (top >= 0) top--;
}


