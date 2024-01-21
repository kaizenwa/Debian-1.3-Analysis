/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:
 * Abstract:
 */

#include <string.h>
#include <ctype.h>
#include "yforth.h"
#include "string.h"
#include "core.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _dash_trailing() {
	register Char *s = (Char *) sp[1];
	register int i = sp[0];
	while (i-- > 0) if (!isspace(s[i])) break;
	sp[0] = i + 1;
}

void _slash_string() {
	register Cell n = *sp++;
	sp[1] = (Cell) ((Char *) sp[1] + n);
	sp[0] -= n;
}

void _blank() {
	register UCell u = (UCell) *sp++;
	register Char *s = (Char *) *sp++;
	if (u) memset(s, ' ', u);
}

void _c_move() {
	register UCell u = (UCell) *sp++;
	register Char *dest = (Char *) *sp++;
	register Char *source = (Char *) *sp++;
	while (u--) *dest++ = *source++;
}

void _c_move_up() {
	register UCell u = (UCell) *sp++;
	register Char *dest = (Char *) *sp++ + u;
	register Char *source = (Char *) *sp++ + u;
	while (u--) *--dest = *--source;
}

void _compare() {
	register UCell u2 = (UCell) *sp++;
	register Char *s2 = (Char *) *sp++;
	register UCell u1 = (UCell) *sp++;
	register Char *s1 = (Char *) *sp;
	register UCell m = u2 <= u1 ? u2 : u1;
	while (m) {
		if (*s1 != *s2) break;
		s1++;
		s2++;
		m--;
	}
	if (u1 == u2 && !m) *sp = 0;
	else if (!m) *sp = u1 < u2 ? -1 : 1;
	else *sp = *s1 < *s2 ? -1 : 1;
}

void _search() {
	register UCell u2 = (UCell) *sp++;
	register Char *s2 = (Char *) sp[0];
	register UCell u1 = (UCell) sp[1];
	register Char *s1 = (Char *) sp[2];
	if (u2 > u1) *sp = FFLAG(0);
	else {
		while (u1 >= u2) {
			*--sp = (Cell) s1;
			*--sp = (Cell) u1;
			*--sp = (Cell) s2;
			*--sp = (Cell) u2;
			_compare();
			if (!(*sp++)) {
				sp[2] = (Cell) s1;
				sp[1] = (Cell) u1;
				sp[0] = FFLAG(1);
				break;
			} else {
				s1++;
				u1--;
			}
		}
	}
}

void _s_literal() {
	register UCell u = (UCell) *sp++;
	register Char *s = (Char *) *sp++;
	compile_cell((Cell) _do_literal);
	compile_cell((Cell) s);
	compile_cell((Cell) _do_literal);
	compile_cell((Cell) u);
}

