/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     float.c
 * Abstract:        floating word set
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "yforth.h"
#include "core.h"
#include "float.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _to_float() {
	register Cell len = *sp++;
	register Char *s = (Char *) *sp;
	extern Char *s_tmp_buffer[];
	Char *endptr;
	memcpy(s_tmp_buffer, s, len);
	if (toupper(s[len - 1]) == 'E' || toupper(s[len - 1]) == 'D') s[len++] = '0'; 
	s[len] = '\0';
	*--fp = (Real) strtod(s, &endptr);
	if (!*endptr) *sp = FFLAG(1);
	else {
		*sp = FFLAG(0);
		fp++;
	}
}

void _d_to_f() {
	register DCell d = GET_DCELL(sp);
	*--fp = (Real) d;
	sp += 2;
}

void _f_store() {
	register Real *addr = (Real *) *sp++;
	*addr = *fp++;
}

void _f_star() {
	fp[1] *= fp[0];
	fp++;
}

void _f_plus() {
	fp[1] += fp[0];
	fp++;
}

void _f_minus() {
	fp[1] -= fp[0];
	fp++;
}

void _f_slash() {
	fp[1] /= fp[0];
	fp++;
}

void _f_zero_less() {
	sp--;
	*sp = FFLAG(*fp < 0.0);
	fp++;
}

void _f_zero_equals() {
	sp--;
	*sp = FFLAG(*fp == 0.0);
	fp++;
}

void _f_less_than() {
	sp--;
	*sp = FFLAG(fp[1] < fp[0]);
	fp += 2;
}

void _f_to_d() {
	register DCell d = (DCell) *fp++;
	sp -= 2;
	PUT_DCELL(sp, d);
}

void _f_fetch() {
	*--fp = *((Real *) *sp++);
}

void _f_constant() {
	register Real r = *fp++;
	create_definition(A_FCONSTANT);
	compile_real(r);
	mark_word(_last);
}

void _f_depth() {
	*--sp = fp_top - fp;
}

void _f_drop() {
	fp++;
}

void _f_dupe() {
	fp--;
	fp[0] = fp[1];
}

void _f_literal() {
	compile_cell((Cell) _do_fliteral);
	compile_real(fp[0]);
	fp++;
}

void _float_plus() {
	sp[0] += sizeof(Real);
}

void _floats() {
	sp[0] *= sizeof(Real);
}

void _floor() {
	fp[0] = floor(fp[0]);
}

void _f_max() {
	if (fp[0] > fp[1]) fp[1] = fp[0];
	fp++;
}

void _f_min() {
	if (fp[0] < fp[1]) fp[1] = fp[0];
	fp++;
}

void _f_negate() {
	fp[0] = -fp[0];
}

void _f_over() {
	fp--;
	fp[0] = fp[2];
}

void _f_rote() {
	register Real temp = fp[0];
	fp[0] = fp[2];
	fp[2] = fp[1];
	fp[1] = temp;
}

void _f_round() {
	fp[0] = floor(fp[0] + 0.5);
}

void _f_swap() {
	register Real temp = fp[0];
	fp[0] = fp[1];
	fp[1] = temp;
}

void _f_variable() {
	create_definition(A_FVARIABLE);
	compile_real(0.0);
	mark_word(_last);
}

void _represent() {
    register Real x = *fp++;
    register int m;
    register int sign = 0;
    static char buf[128];
    if (x < 0.0) {
        sign = 1;
        x = -x;
    }
    if (x != 0.0) {
        m = (int) floor(log10(x)) + 1;
        x /= pow(10, m);
        if (x >= 1.0) {
            x /= 10;
            m++;
        }
    } else m = 0;
    sprintf(buf, "%0.*f", sp[0], x);
    strncpy((Char *) sp[1], buf + 2, sp[0]);
    sp--;
    sp[2] = m;
    sp[1] = FFLAG(sign);
	sp[0] = FFLAG(1);
}

