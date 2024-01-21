/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: tools.c
 * Abstract:	Programming Tools word set
 */

#include <stdio.h>
#include "yforth.h"
#include "tools.h"
#include "core.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _dot_s() {
	register Cell *p = sp;
	while (p < sp_top) {
		*--sp = *p;
		_dot();
		p++;
	}
}

void _question() {
	_fetch();
	_dot();
}

void _dump() {
	register UCell u = *sp++;
	register Char *addr = (Char *) *sp++;
	while (u) {
		register int i;
		printf("%08p: ", addr);
		for (i = 0; i < 16; i++)
			if ((int) (u - i) > 0) printf("%02x ", *(addr + i) & 0xff);
			else printf("   ");
		for (i = 0; i < 16 && (u - i) > 0; i++)
			printf("%c", *(addr + i) < 32 ? '.' : *(addr + i));
		putchar('\n');
		addr += i;
		u -= i;
	}
}

void _see() {
	_error = E_NOPRIM;
}

void _words() {
	register int i = 0;
	register struct word_def *p;
	register Cell col = 1;
	while (i < VOC_HASH) {
		p = voc->voc[i++];
		while (p) {
			*--sp = (Cell) p->name;
			_count();
			if (col + sp[0] > 79) {
				col = 1;
				_c_r();
			}
			col += sp[0] + 1;
			_type();
			_b_l();
			_emit();
			p = p->link;
		}
	}
}

