/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     coree.c
 * Abstract:        Core extension word set
 */

#include "yforth.h"

#include <string.h>
#include <stdio.h>
#include "core.h"
#include "coree.h"
#include "double.h"
#include "locals.h"
#include "block.h"
#include "search.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

Char * _pad;

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _dot_paren() {
    *--sp = ')';
    _word();
    _count();
    _type();
}

void _dot_r() {
	register Cell u = *sp++;
	_s_to_d();
	*--sp = u;
	_d_dot_r();
}

void _zero_not_equals() {
	sp[0] = FFLAG(sp[0] != 0);
}

void _zero_greater() {
    sp[0] = FFLAG(sp[0] > 0);
}

void _two_to_r() {
    rp -= 2;
    rp[0] = *sp++;
	rp[1] = *sp++;
}

void _two_r_from() {
    sp -= 2;
    sp[0] = *rp++;
    sp[1] = *rp++;
}

void _two_r_fetch() {
    sp -= 2;
    sp[0] = rp[0];
    sp[1] = rp[1];
}

void _colon_no_name() {
    register struct word_def *def;
    _align();
    def = (struct word_def *) _dp;
	def->name = 0;
	def->link = 0;
	def->class = A_COLON;
	_dp += sizeof(struct word_def) - sizeof(Cell);
	_state = COMPILE;
	*--sp = (Cell) def;
	init_locals();
}

void _not_equals() {
	sp[1] = FFLAG(sp[0] != sp[1]);
    sp++;
}

void _question_do() {
    compile_cell((Cell) _paren_question_do_paren);
    *--sp = (Cell) _dp;
    compile_cell(0);
    *--sp = (Cell) _dp;
    *--sp = 1;  /* e' un ?do */
}

void _paren_question_do_paren() {
    if (sp[0] == sp[1]) ip += 1 + (Cell) *ip;
    else {
		*--rp = *sp++;
		*--rp = *sp++;
		ip++;
	}
}

void _again() {
	register Cell *dest = (Cell *) *sp++;
	compile_cell((Cell) _branch);
	compile_cell(dest - ((Cell *) _dp) - 1);
}

void _c_quote() {
	register Char *cur;
	register Cell *patch;
	compile_cell((Cell) _branch);
	patch = (Cell *) _dp;
	compile_cell(0);
	cur = _dp;
	*--sp = '"';
	_word();
	sp++;
	_dp = (Char *) WORD_PTR(_dp);
    *patch = ((Cell *) _dp) - patch - 1;
    compile_cell((Cell) _do_literal);
    compile_cell((Cell) cur);
}

void _compile_comma() {
    compile_word((struct word_def *) *sp++);
}

void _erase() {
    register UCell u = (UCell) *sp++;
    register Char *addr = (Char *) *sp++;
    if (u) memset(addr, 0, u);
}

void _false() {
    *--sp = FFLAG(0);
}

void _hex() {
	_base = 16;
}

void _marker() {
	struct voc_marker vm;
	save_vocabulary(&vm);
	create_definition(A_MARKER);
	memcpy(_dp, &vm, sizeof(struct voc_marker));
	_dp += ALIGN_PTR(sizeof(struct voc_marker));
	mark_word(_last);
}

void _nip() {
    sp[1] = sp[0];
    sp++;
}

void _parse() {
	register Char delim = (Char) *sp;
    register Char *orig = &_input_buffer[_to_in];
    register int i = 0;
    while (_to_in < _in_input_buffer && _input_buffer[_to_in] != delim) {
        _to_in++;
        i++;
    }
	*sp = (Cell) orig;
    *--sp = i;
    if (_to_in < _in_input_buffer) _to_in++;
}

void _pick() {
	sp[0] = sp[sp[0] + 1];
}

void _refill() {
	if (_b_l_k != 0) {
		current_block = _b_l_k++;
		_to_in = 0;
		*--sp = _b_l_k;
		_block();
		_input_buffer = (Char *) *sp++;
		_in_input_buffer = BLOCK_SIZE;
		*sp = FFLAG(_b_l_k && _input_buffer != NULL);
	} else if (_source_id == 0) {
        *--sp = (Cell) _tib;
		*--sp = tib_size;
        _accept();
		_input_buffer = _tib;
        _in_input_buffer = *sp;
        _to_in = 0;
        *sp = FFLAG(1);
    } else if (_source_id == -1) {
		*--sp = FFLAG(0);
	} else if (_env_file) {
		if (fgets(_input_buffer, FILE_BUFFER_SIZE, (FILE *) _source_id)) {
			_in_input_buffer = strlen(_input_buffer);
			if (_in_input_buffer && _input_buffer[_in_input_buffer - 1] == '\n')
				_in_input_buffer--;
			_to_in = 0;
			*--sp = FFLAG(1);
		} else *--sp = FFLAG(0);
	} else *--sp = FFLAG(0);
}

void _restore_input() {
	sp++;
	_b_l_k = *sp++;
	_to_in = *sp++;
	_in_input_buffer = *sp++;
	_input_buffer = (Char *) *sp++;
	_source_id = *sp++;
	if (_source_id == 0) {
	} else if (_source_id == -1) {
	} else {
	}
	*--sp = FFLAG(1);
}

void _roll() {
	register Cell u = *sp++;
	register Cell xu = sp[u];
	register int i;
	for (i = u; i > 0; i--) sp[i] = sp[i - 1];
	sp[0] = xu;
}

void _save_input() {
	if (_source_id == 0) {
	} else if (_source_id == -1) {
	} else {
	}
	*--sp = _source_id;
	*--sp = (Cell) _input_buffer;
	*--sp = _in_input_buffer;
	*--sp = _to_in;
	*--sp = _b_l_k;
	*--sp = 5;
}

void _true() {
	*--sp = FFLAG(1);
}

void _tuck() {
	sp--;
	sp[0] = sp[1];
	sp[1] = sp[2];
	sp[2] = sp[0];
}

void _u_dot_r() {
    register Cell r = *sp++;
	*--sp = 0;
	_less_number_sign();
	_number_sign_s();
	_number_sign_greater();
	if (sp[0] < r) {
		sp--;
		sp[0] = r - sp[1];
		_spaces();
	}
    _type();
    putchar(' ');
}

void _u_greater_than() {
    sp[1] = FFLAG((UCell) sp[1] > (UCell) sp[0]);
	sp++;
}

void _unused() {
	*--sp = (dspace_size - (_dp - dp0)) * sizeof(Cell);
}

void _within() {
	register Cell n3 = *sp++;
    register Cell n2 = *sp++;
    register Cell n1 = *sp;
    sp[0] = FFLAG((n2 < n3 && (n2 <= n1 && n1 < n3)) ||
                  (n2 > n3 && (n2 <= n1 || n1 < n3)));
}

void _backslash() {
    _to_in = _in_input_buffer;
}

void _bracket_compile() {
	*--sp = ' ';
    _word();
    sp++;
    compile_word(search_word(_dp + 1, *_dp));
}

void _value() {
	create_definition(A_VALUE);
	compile_cell((Cell) sp[0]);
	sp++;
	mark_word(_last);
}

void _paren_write_value_paren() {
	register Cell *p = (Cell *) (*ip++);
	*p = *sp++;
}

void _to() {
	_b_l();
	_word();
	_find();
	if (*sp++) {
		register struct word_def *xt = (struct word_def *) *sp++;
		if ((xt->class & A_WORD) == A_VALUE) {
			if (_state == INTERPRET) xt->func[0] = (pfp) *sp++;
			else {
				compile_cell((Cell) _paren_write_value_paren);
				compile_cell((Cell) &xt->func[0]);
			}
		} else if (xt->class & A_WORD == A_LOCAL && _state == COMPILE) {
			compile_cell((Cell) _paren_write_local_paren);
			compile_cell((Cell) xt->func[0]);
		} else {
			/* ... */
		}
	} else sp++;
}

void _paren_marker_paren() {
	exec_marker((struct voc_marker *) ip++);
}

/**************************************************************************/
/* AUXILIARY FUNCTIONS ****************************************************/
/**************************************************************************/

void exec_marker(struct voc_marker *vm) {
	load_vocabulary(vm);
}

