/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: blocke.c
 * Abstract:	Block extension word set
 */

#include <stdio.h>
#include "yforth.h"
#include "core.h"
#include "coree.h"
#include "block.h"
#include "blocke.h"

/**************************************************************************/
/* VARIABLES ************** ***********************************************/
/**************************************************************************/

UCell _s_c_r;

/**************************************************************************/
/* WORDS ****************** ***********************************************/
/**************************************************************************/

void _empty_buffers() {
	register int i;
	for (i = 0; i < NUM_BLOCKS; i++) block_data[i].block_no = 0;
}

void _list() {
	register Char *buffer;
	register int i;
	_block();
	buffer = (Char *) *sp++;
	for (i = 0; i < BLOCK_SIZE; i += 64) {
		*--sp = i / 64;
		*--sp = 2;
		_dot_r();
		*--sp = ':';
		_emit();
		_b_l();
		_emit();
		*--sp = (Cell) buffer + i;
		*--sp = 64;
		_type();
		_c_r();
	}
}

void _thru() {
	register UCell u2 = (UCell) *sp++;
	register UCell u1 = (UCell) *sp++;
	for (; u1 <= u2; u1++) {
		*--sp = u1;
		_load();
	}
}


