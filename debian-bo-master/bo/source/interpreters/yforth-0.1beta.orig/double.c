/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     double.c
 * Abstract:        double-number word set
 */

#include <stdio.h>
#include "yforth.h"
#include "core.h"
#include "double.h"

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _two_constant() {
	register DCell d = GET_DCELL(sp);
	sp += 2;
	create_definition(A_2CONSTANT);
	compile_cell((Cell) d);
	compile_cell((Cell) (d >> CellBits));
	mark_word(_last);
}

void _two_literal() {
	compile_cell((Cell) _do_literal);
    compile_cell((Cell) sp[1]);
	compile_cell((Cell) _do_literal);
    compile_cell((Cell) sp[0]);
	sp += 2;
}

void _two_variable() {
	create_definition(A_2VARIABLE);
	compile_cell(0);
	compile_cell(0);
	mark_word(_last);
}

void _d_plus() {
	register DCell d1 = GET_DCELL(sp + 2);
	register DCell d2 = GET_DCELL(sp);
	d1 += d2;
	sp += 2;
	PUT_DCELL(sp, d1);
}

void _d_minus() {
	register DCell d1 = GET_DCELL(sp + 2);
	register DCell d2 = GET_DCELL(sp);
	d1 -= d2;
	sp += 2;
	PUT_DCELL(sp, d1);
}

void _d_dot() {
	register DCell u = GET_DCELL(sp);
	register int usign = u < 0;
	if (usign) u = -u;
	PUT_DCELL(sp, u);
	_less_number_sign();
	_number_sign_s();
	if (usign) {
		*--sp = '-';
		_hold();
	}
	_number_sign_greater();
	_type();
	putchar(' ');
}

void _d_dot_r() {
	register Cell r = *sp++;
	register DCell u = GET_DCELL(sp);
	register int usign = u < 0;
	if (usign && _base == 10) u = -u;
	PUT_DCELL(sp, u);
	_less_number_sign();
	_number_sign_s();
	if (usign) {
		*--sp = '-';
		_hold();
	}
	_number_sign_greater();
	if (sp[0] < r) {
		sp--;
		sp[0] = r - sp[1];
		_spaces();
	}
	_type();
	putchar(' ');
}

void _d_zero_less() {
	register DCell d = GET_DCELL(sp);
	sp++;
	sp[0] = FFLAG(d < 0);
}

void _d_zero_equals() {
	register DCell d = GET_DCELL(sp);
	sp++;
	sp[0] = FFLAG(d == 0);
}

void _d_two_star() {
	register DCell d = GET_DCELL(sp);
	d <<= 1;
	PUT_DCELL(sp, d);
}

void _d_two_slash() {
	register DCell d = GET_DCELL(sp);
	d >>= 1;
	PUT_DCELL(sp, d);
}

void _d_less_than() {
	register DCell d1 = GET_DCELL(sp + 2);
	register DCell d2 = GET_DCELL(sp);
	sp += 3;
	sp[0] = FFLAG(d1 < d2);
}

void _d_equals() {
	register DCell d1 = GET_DCELL(sp + 2);
	register DCell d2 = GET_DCELL(sp);
	sp += 3;
	sp[0] = FFLAG(d1 == d2);
}

void _dabs() {
	register DCell d = GET_DCELL(sp);
	d = d > 0 ? d : -d;
	PUT_DCELL(sp, d);
}

void _dmax() {
	register DCell d1 = GET_DCELL(sp + 2);
	register DCell d2 = GET_DCELL(sp);
	sp += 2;
	if (d2 > d1) PUT_DCELL(sp, d2);
}

void _dmin() {
	register DCell d1 = GET_DCELL(sp + 2);
	register DCell d2 = GET_DCELL(sp);
	sp += 2;
	if (d2 < d1) PUT_DCELL(sp, d2);
}

void _dnegate() {
	register DCell d = -GET_DCELL(sp);
	PUT_DCELL(sp, d);
}

void _m_star_slash() {
	register Cell n2 = *sp++;
	register Cell n1 = *sp++;
	register DCell d = GET_DCELL(sp);
	d = (d * n1) / n2;
	PUT_DCELL(sp, d);
}

void _m_plus() {
	_s_to_d();
	_d_plus();
}


