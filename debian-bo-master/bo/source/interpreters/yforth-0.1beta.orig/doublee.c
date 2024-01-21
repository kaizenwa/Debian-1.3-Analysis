/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     doublee.c
 * Abstract:        double-extension word set
 */

#include "yforth.h"
#include "doublee.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _two_rote() {
	register DCell d1 = GET_DCELL(sp);
	register DCell d2 = GET_DCELL(sp + 2);
	register DCell d3 = GET_DCELL(sp + 4);
	PUT_DCELL(sp, d3);
	PUT_DCELL(sp + 2, d1);
	PUT_DCELL(sp + 4, d2);
}

void _d_u_less() {
	register UDCell ud1 = GET_DCELL(sp + 2);
	register UDCell ud2 = GET_DCELL(sp);
	sp += 3;
	sp[0] = FFLAG(ud1 < ud2);
}

