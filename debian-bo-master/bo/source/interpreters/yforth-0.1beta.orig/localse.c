/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     localse.c
 * Abstract:        locals-extension word set
 */

#include "yforth.h"
#include "core.h"
#include "locals.h"
#include "localse.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _locals_bar() {
	while (1) {
		_b_l();
		_word();
		_count();
		if (sp[0] != 1 || *((Char *) sp[1]) != '|') {
			_paren_local_paren();
			compile_cell((Cell) _to_r);
		} else break;
	}
}

