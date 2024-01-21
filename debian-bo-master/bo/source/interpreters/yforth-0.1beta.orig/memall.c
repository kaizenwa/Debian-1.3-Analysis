/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: memall.c
 * Abstract:    Memory allocation word set
 */

#include <string.h>
#include <malloc.h>
#include "yforth.h"
#include "core.h"
#include "coree.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _allocate() {
    register void *addr = malloc(*sp);
    if (addr == NULL) *sp = 0;
    else *sp = (Cell) addr;
    *--sp = FFLAG(addr == NULL);
}

void _free() {
    free((void *) *sp);
    *sp = FFLAG(0);
}

void _resize() {
    register void *addr = realloc((void *) sp[1], sp[0]);
    if (addr == NULL) sp[1] = 0;
    else sp[1] = (Cell) addr;
    *sp = FFLAG(addr == NULL);
}


