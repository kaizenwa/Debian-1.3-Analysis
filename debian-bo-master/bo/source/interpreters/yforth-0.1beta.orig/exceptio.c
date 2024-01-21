/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     exceptio.c
 * Abstract:        exception word set
 */

#include <malloc.h>
#include <setjmp.h>
#include "yforth.h"
#include "core.h"
#include "exceptio.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

struct exception_frame *top_frame;  /* ptr to the top of exception stack */

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _catch() {
	register struct exception_frame *frame =
		(struct exception_frame *) malloc(sizeof(struct exception_frame));
	if (frame) {
		register int ret_val;
		if ((ret_val = setjmp(frame->catch_buf)) == 0) {
            /* Executed when "catch" is invoked */
			save_input_specification();
			frame->sp = sp + 1;
			frame->rp = rp;
			frame->bp = bp;
			frame->fp = fp;
			frame->last = top_frame;
			top_frame = frame;
			exec_word((struct word_def *) *sp++);
			*--sp = 0;
		} else *--sp = ret_val;
		frame = top_frame;
		sp = frame->sp;
		rp = frame->rp;
		bp = frame->bp;
		top_frame = frame->last;
		free(frame);
		restore_input_specification();
	}
}

void _throw() {
	register Cell n = *sp++;
	if (n) {
		if (top_frame) longjmp(top_frame->catch_buf, n);
		else if (n == -1) ;
		else if (n == -2) _type();
		sp = sp_top;
		longjmp(warm_start_jump, 1);
	}
}

