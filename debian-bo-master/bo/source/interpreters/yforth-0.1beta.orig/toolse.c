/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: toolse.c
 * Abstract:	Programming Tools extension word set
 */

#include <stdio.h>
#include <stdlib.h>
#include "yforth.h"
#include "toolse.h"
#include "core.h"
#include "coree.h"
#include "block.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _bye() {
#if BLOCK_DEF
	close_block_file();
#endif
    exit(0);
}

void _ahead() {
	compile_cell((Cell) _branch);
	*--sp = (Cell) _dp;
	compile_cell(0);
}

void _bracket_if() {
	register Cell flag = *sp++;
	register Cell nest = 1;
	register Cell ok = FFLAG(1);
	if (!flag) {
		do {
			_b_l();
			_word();
			sp++;
			if (!*_dp) {
				_refill();
				ok = *sp++;
			} else {
				if (!strmatch("[IF]", _dp, 4)) nest++;
				else if (!strmatch("[THEN]", _dp, 6) ||
						 (!strmatch("[ELSE]", _dp, 6) && nest == 1)) nest--;
			}
		} while (nest && ok);
	}
}

void _bracket_else() {
	register Cell nest = 1;
	register Cell ok = FFLAG(1);
	do {
		_b_l();
		_word();
		sp++;
		if (!*_dp) {
			_refill();
			ok = *sp++;
		} else {
			if (!strmatch("[IF]", _dp, 4)) nest++;
			else if (!strmatch("[THEN]", _dp, 6)) nest--;
		}
	} while (nest && ok);
}

void _bracket_then() {
}

