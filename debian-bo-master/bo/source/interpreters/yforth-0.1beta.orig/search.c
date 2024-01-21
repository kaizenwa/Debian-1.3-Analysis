/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: 
 * Abstract:
 */

#include <stdlib.h>
#include "core.h"
#include "search.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

struct vocabulary *list[WORD_LISTS];
Cell top;		/* indice primo vocabolario sulla pila */
struct vocabulary *voc;	/* ptr al vocabolario usato per le definzioni */
struct vocabulary *forth_wid;

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _definitions() {
	voc = list[top];
}

void _forth_wordlist() {
	*--sp = (Cell) forth_wid;
}

void _get_current() {
	*--sp = (Cell) voc;
}

void _get_order() {
	register Cell i;
	for (i = 0; i <= top; i++) *--sp = (Cell) list[i];
	*--sp = top;
}

void _search_wordlist() {
	register struct vocabulary *wid = (struct vocabulary *) *sp++;
	register Cell len = *sp++;
	register Char *addr = (Char *) *sp;
	register struct word_def *xt = search_wordlist(addr, len, wid);
	set_find_stack(addr, xt);
	if (!*sp) *++sp = 0;
}

void _set_current() {
	voc = (struct vocabulary *) *sp++;
}

void _set_order() {
	register Cell n = *sp++;
	register int i;
	for (i = 0; i < n; i++)
		if (i < WORD_LISTS) list[i] = (struct vocabulary *) *sp++;
		else sp++;
	top = n - 1;
}

void _wordlist() {
	register struct vocabulary *v;
	register int i;
	_align();
	v = (struct vocabulary *) _dp;
	_dp += sizeof(struct vocabulary);
	for (i = 0; i < VOC_HASH; i++) v->voc[i] = NULL;
	*--sp = (Cell) v;
}


/**************************************************************************/
/* AUXILIARY FUNCTIONS ****************************************************/
/**************************************************************************/

void save_vocabulary(struct voc_marker *vm) {
	register int i;
	for (i = 0; i < WORD_LISTS; i++) {
		vm->list[i] = list[i];
		if (list[i]) vm->v_list[i] = *list[i];
	}
	vm->top = top;
	vm->voc = voc;
	vm->_dp = _dp;
	vm->last = _last;
}

void load_vocabulary(struct voc_marker *vm) {
	register int i;
	for (i = 0; i < WORD_LISTS; i++) {
		list[i] = vm->list[i];
		if (list[i]) *list[i] = vm->v_list[i];
	}
	top = vm->top;
	voc = vm->voc;
	_dp = vm->_dp;
	_last = vm->last;
}
