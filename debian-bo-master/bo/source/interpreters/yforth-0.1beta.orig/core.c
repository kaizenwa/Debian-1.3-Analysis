/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     core.c
 * Abstract:        Core word set
 */

#include <string.h>
#include <setjmp.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "yforth.h"
#include "udio.h"
#include "core.h"
#include "coree.h"
#include "float.h"
#include "double.h"
#include "toolse.h"
#include "locals.h"
#include "block.h"
#include "exceptio.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

Char s_tmp_buffer[TMP_BUFFER_SIZE];     /* used by s" */

Cell _to_in;                            /* ptr to parse area */
Cell _source_id;                        /* input source device */
Char * _tib;                            /* ptr to terminal input buffer */
Char * _input_buffer;                   /* current input buffer */
Cell _in_input_buffer;                  /* # of chars in input buffer */
Cell _base;                             /* base is base */
Char * _dp;                             /* dictionary pointer */
Cell _error;                            /* error code */
struct word_def * _last;                /* ptr to last defined word */
Cell _state;                            /* state of the interpreter */
Cell _check_system = 1;                 /* 1 => check stacks overflow & underflow */
                                        /* Some variables used by environment? follows... */
Cell _env_slash_counted_string;         
Cell _env_slash_hold;
Cell _env_slash_pad;
Cell _env_address_unit_bits;
Cell _env_core;
Cell _env_core_ext;
Cell _env_floored;
Cell _env_max_char;
Cell _env_max_d;
Cell _env_max_n;
Cell _env_max_u;
Cell _env_max_ud;
Cell _env_return_stack_cells;
Cell _env_stack_cells;
Cell _env_double;
Cell _env_double_ext;
Cell _env_floating;
Cell _env_floating_stack;
Cell _env_max_float;
Cell _env_floating_ext;
Cell _env_memory_alloc;
Cell _env_memory_alloc_ext;
Cell _env_search_order;
Cell _env_search_order_ext;
Cell _env_wordlists;
Cell _env_tools;
Cell _env_tools_ext;
Cell _env_number_locals;
Cell _env_locals;
Cell _env_locals_ext;
Cell _env_facility;
Cell _env_facility_ext;
Cell _env_block;
Cell _env_block_ext;
Cell _env_exception;
Cell _env_exception_ext;
Cell _env_file;
Cell _env_file_ext;
Cell _env_string;
Cell _env_string_ext;

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _dot_quote() {
	compile_cell((Cell) _paren_dot_quote_paren);
	*--sp = '"';
	_word();
	_dp = (Char *) WORD_PTR(_dp);
    sp++;
}

void _paren_dot_quote_paren() {
    register Char *addr = (Char *) ip;
    *--sp = (Cell) (addr + 1);
    *--sp = (Cell) *addr;
	_type();
	ip = (pfp *) WORD_PTR((Char *) ip);
}

void _type() {
    register Cell u = *sp++;
    register Char *addr = (Char *) *sp++;
    while (u--) putchar(*addr++);
}

void _u_dot() {
	*--sp = 0;
    _less_number_sign();
    _number_sign_s();
    _number_sign_greater();
	_type();
    putchar(' ');
}

void _c_r() {
	putchar('\n');
}

void _emit() {
	putchar(*sp++);
}

#ifdef DOUBLE_DEF
void _dot() {
	_s_to_d();
	_d_dot();
}
#else
void _dot() {
	register DCell u = *sp;
	register int usign = u < 0;
	if (usign) u = -u;
	sp--;
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
#endif

void _space() {
    putchar(' ');
}

void _spaces() {
    register UCell u = *sp++;
    while (u--) putchar(' ');
}

void _less_number_sign() {
	in_pnos = 0;
	p_pnos = pnos + pnos_size;
}

void _number_sign() {
	register UDCell ud1 = GET_DCELL(sp);
	register int rem = ud1 % _base;
	ud1 /= _base;
	PUT_DCELL(sp, ud1);
	if (rem < 10) *--p_pnos = rem + '0';
	else *--p_pnos = rem - 10 + 'a';
	in_pnos++;
}

void _hold() {
	register Char ch = (Char) *sp++;
	*--p_pnos = ch;
	in_pnos++;
}

void _number_sign_s() {
	do _number_sign();
	while (sp[0] || sp[1]);
}

void _number_sign_greater() {
	sp[1] = (Cell) p_pnos;
	sp[0] = in_pnos;
}

void _store() {
	register Cell *addr = (Cell *) *sp++;
	*addr = *sp++;
}

void _star() {
    sp[1] *= *sp;
    sp++;
}

void _star_slash() {
    register DCell d = (DCell) sp[1] * (DCell) sp[2];
    sp[2] = d / (DCell) sp[0];
    sp += 2;
}

void _star_slash_mod() {
	register DCell d = (DCell) sp[1] * (DCell) sp[2];
    sp[2] = d % (DCell) sp[0];
	sp[1] = d / (DCell) sp[0];
	sp++;
}

void _plus() {
	sp[1] += sp[0];
	sp++;
}

void _plus_store() {
    register Cell *addr = (Cell *) *sp++;
    *addr += *sp++;
}

void _minus() {
    sp[1] -= sp[0];
    sp++;
}

void _slash() {
	sp[1] /= sp[0];
	sp++;
}

void _slash_mod() {
	register Cell n1 = sp[1];
	register Cell n2 = sp[0];
    sp[1] = n1 % n2;
    sp[0] = n1 / n2;
}

void _zero_less() {
    sp[0] = FFLAG(sp[0] < 0);
}

void _zero_equals() {
    sp[0] = FFLAG(sp[0] == 0);
}

void _one_plus() {
	sp[0]++;
}

void _one_minus() {
    sp[0]--;
}

void _two_store() {
    register Cell *addr = (Cell *) *sp++;
    *addr++ = *sp++;
    *addr = *sp++;
}

void _two_star() {
    sp[0] <<= 1;
}

void _two_slash() {
	sp[0] >>= 1;
}

void _two_fetch() {
    register Cell *addr = (Cell *) *sp;
    *sp-- = *(addr + 1);
    *sp = *addr;
}

void _two_drop() {
    sp += 2;
}

void _two_dupe() {
    sp -= 2;
    sp[0] = sp[2];
    sp[1] = sp[3];
}

void _two_over() {
	sp -= 2;
    sp[0] = sp[4];
    sp[1] = sp[5];
}

void _two_swap() {
    register Cell x4 = sp[0];
	register Cell x3 = sp[1];
	sp[0] = sp[2];
    sp[1] = sp[3];
    sp[2] = x4;
    sp[3] = x3;
}

void _less_than() {
    sp[1] = FFLAG(sp[1] < sp[0]);
    sp++;
}

void _equals() {
	sp[1] = FFLAG(sp[1] == sp[0]);
    sp++;
}

void _greater_than() {
    sp[1] = FFLAG(sp[1] > sp[0]);
    sp++;
}

void _to_r() {
    *--rp = *sp++;
}

void _question_dupe() {
    if (sp[0]) sp--, sp[0] = sp[1];
}

void _fetch() {
    sp[0] = *((Cell *) sp[0]);
}

void _abs() {
    register Cell n = sp[0];
    sp[0] = n >= 0 ? n : -n;
}

void _align() {
	_dp = (Char *) ALIGN_PTR(_dp);
}

void _aligned() {
    sp[0] = ALIGN_PTR((Cell *) sp[0]);
}

void _and() {
	sp[1] &= sp[0];
    sp++;
}

void _b_l() {
	*--sp = ' ';
}

void _c_store() {
    register Char *addr = (Char *) *sp++;
    *addr = (Char) *sp++;
}

void _c_fetch() {
    register Char *addr = (Char *) *sp;
    *sp = (Cell) *addr;
}

void _cell_plus() {
	sp[0] += sizeof(Cell);
}

void _cells() {
    sp[0] *= sizeof(Cell);
}

void _char_plus() {
    sp[0] += sizeof(Char);
}

void _chars() {
    sp[0] *= sizeof(Char);
}

void _depth() {
	register Cell dep = sp_top - sp;
	*--sp = dep;
}

void _drop() {
	sp++;
}

void _dupe() {
    sp--;
    sp[0] = sp[1];
}

void _f_m_slash_mod() {
    register Cell n1 = *sp++;
	register DCell d1 = GET_DCELL(sp);
    sp[0] = d1 / n1;
    sp[1] = d1 % n1;
#if !FLOORED_DIVISION
	if (*sp < 0) {
		sp[0]--;
		if (sp[1] > 0) sp[1]++;
		else sp[1]--;
		sp[1] = -sp[1];
	}	
#endif
}

void _invert() {
    sp[0] = ~sp[0];
}

void _l_shift() {
	register UCell u = (UCell) *sp++;
    sp[0] <<= u;
}

void _m_star() {
    register DCell d = (DCell) sp[1] * (DCell) sp[0];
	PUT_DCELL(sp, d);
}

void _max() {
    register Cell n2 = *sp++;
    sp[0] = sp[0] > n2 ? sp[0] : n2;
}

void _min() {
    register Cell n2 = *sp++;
    sp[0] = sp[0] < n2 ? sp[0] : n2;
}

void _mod() {
    sp[1] %= sp[0];
    sp++;
}

void _negate() {
    sp[0] = -sp[0];
}

void _or() {
	sp[1] |= sp[0];
    sp++;
}

void _over() {
    sp--;
    sp[0] = sp[2];
}

void _r_from() {
	*--sp = *rp++;
}

void _r_fetch() {
    *--sp = *rp;
}

void _rote() {
    register Cell x3 = sp[0];
    register Cell x2 = sp[1];
    register Cell x1 = sp[2];
    sp[0] = x1;
    sp[1] = x3;
    sp[2] = x2;
}

void _r_shift() {
    register UCell u = (UCell) *sp++;
	((UCell *) sp)[0] >>= u;
}

void _s_to_d() {
    register DCell d = (DCell) (*sp--);
	PUT_DCELL(sp, d);
}

void _s_m_slash_rem() {
    register Cell n1 = *sp++;
	register DCell d1 = GET_DCELL(sp);
    sp[0] = d1 / n1;
    sp[1] = d1 % n1;
#if FLOORED_DIVISION
	if (*sp < 0) {
		sp[0]++;
		if (sp[1] > 0) sp[1]--;
		else sp[1]++;
		sp[1] = -sp[1];
	}	
#endif
}

void _swap() {
    register Cell temp = sp[0];
    sp[0] = sp[1];
    sp[1] = temp;
}

void _u_less_than() {
    sp[1] = FFLAG((UCell) sp[1] < (UCell) sp[0]);
	sp++;
}

void _u_m_star() {
	register UDCell ud = (UDCell) sp[1] * (UDCell) sp[0];
	PUT_DCELL(sp, ud);
}

void _u_m_slash_mod() {
	register UCell u1 = *sp++;
	register UDCell ud = GET_DCELL(sp);
	sp[1] = ud % u1;
	sp[0] = ud / u1;
}

void _xor() {
	sp[1] ^= sp[0];
	sp++;
}

void _do_literal() {
    *--sp = (Cell) *ip++;
}

void _do_fliteral() {
	*--fp = (Real) *((Real *) ip);
	ip += sizeof(Real) / sizeof(Cell);
}

void _word() {
	register Char *addr;
	register Char delim = (Char) *sp;
	register int i, j;
	while (_to_in < _in_input_buffer && _input_buffer[_to_in] == delim) _to_in++;
	_parse();
	i = *_dp = *sp++;
	addr = (Char *) *sp;
	for (j = 0; j < i; j++) *(_dp + j + 1) = *addr++;
	*(_dp + i + 1) = ' ';
	*sp = (Cell) _dp;
}

void _to_number() {
	register UCell u1 = (UCell) *sp;
    register Char *addr = (Char *) *(sp + 1);
	register UDCell ud1 = GET_DCELL(sp + 2);
	while (is_base_digit(*addr) && u1) {
		ud1 *= _base;
		if (*addr <= '9') ud1 += *addr - '0';
		else ud1 += toupper(*addr) - 'A' + 10;
		addr++;
		u1--;
	}
	PUT_DCELL(sp + 2, ud1);
	*(sp + 1) = (Cell) addr;
	*sp = u1;
}

void _read_const() {
	register Cell n;
	register Cell usign = 1;
	register UDCell num;
	register const_type = 1;
	register Char *orig = (Char *) sp[1];
	register Cell orig_len = sp[0];
	if (sp[0] && *((Char *) sp[1]) == '-') {
		usign = -1;
		sp[1] += sizeof(Char);
		sp[0]--;
	}
	while (sp[0]) {
		_to_number();
		if (sp[0] && *((Char *) sp[1]) == '.') {
			const_type = 2;
			sp[0]--;
			sp[1] += sizeof(Char);
		} else break;
	}
	n = *sp++;
	num = GET_DCELL(sp + 1);
	if (usign < 0) {
		num = -num;
		PUT_DCELL(sp + 1, num);
	}
	if (!n) *sp = const_type;
#ifdef FLOAT_DEF
	else {
		if (_base == 10) {
			sp++;
			sp[1] = (Cell) orig;
			sp[0] = orig_len;
			_to_float();
			if (*sp) sp[0] = 3;
		} else *sp = 0;
	}
#else
	else *sp = 0;
#endif
}

void _interpret() {
	register struct word_def *xt;
	while (!_error && _to_in < _in_input_buffer) {
		*--sp = ' ';
		_word();
		sp++;
		if (!(*_dp)) continue; 				/* Please forget this! */
		xt = search_word(_dp + 1, *_dp);
		if (xt) {
			if (_state == INTERPRET) {
				if (xt->class & COMP_ONLY) _error = E_NOCOMP;
				else exec_word(xt);
			} else /* _state == COMPILE */ {
				if (xt->class & IMMEDIATE) exec_word(xt);
				else compile_word(xt);
			}
		} else /* xt == 0 */ {
			register UDCell num;
			*--sp = 0;
			*--sp = 0;
			*--sp = (Cell) (_dp + sizeof(Char));
			*--sp = (Cell) *_dp;
			_read_const();
			if (!(*sp)) {
				sp++;
				_error = E_NOWORD;
			} else {
				switch (*sp++) {
					case 1:
						num = GET_DCELL(sp);
						if (_state == INTERPRET) sp++;
						else {
							sp += 2;
							compile_cell((Cell) _do_literal);
							compile_cell((Cell) num);
						}
						break;
					case 2:
						num = GET_DCELL(sp);
						if (_state == COMPILE) {
							sp += 2;
							compile_cell((Cell) _do_literal);
							compile_cell((Cell) num);
							compile_cell((Cell) _do_literal);
							compile_cell((Cell) (num >> CellBits));
						}
						break;
					case 3:
						if (_state == COMPILE) {
							compile_cell((Cell) _do_fliteral);
							compile_real(*fp);
							fp++;
						}
						break;
				}
			}
		}
	}
}

void _accept() {
    register Cell n1 = *sp++;
    register Char *addr = (Char *) *sp;
	register int i = 0;
    register char ch;
    do {
		ch = getchar();
        i = process_char(addr, n1, i, ch);
    } while (ch != '\n');
    *sp = i;
}

void _source() {
    *--sp = (Cell) _input_buffer;
    *--sp = _in_input_buffer;
}

void _paren() {
	register Cell eof = 1;
	do {
		while (_to_in < _in_input_buffer && _input_buffer[_to_in] != ')') _to_in++;
		if (_source_id != 0 && _source_id != -1 && _to_in == _in_input_buffer) {
			_refill();
			eof = !(*sp++);
		}
	} while (_to_in == _in_input_buffer && !eof);
	if (_to_in < _in_input_buffer) _to_in++;
}

void _evaluate() {
    register Cell u = *sp++;
	register Char *addr = (Char *) *sp++;
	save_input_specification();
	_source_id = -1;
	_in_input_buffer = u;
	_input_buffer = addr;
	_to_in = 0;
	_b_l_k = 0;
	_interpret();
	restore_input_specification();
}

void _view_error_msg() {
	static struct an_error {
		char *msg;
		char please_abort;
		char print_word;
	} err_msg[] = {
		{ "everything allright",				0, 0 },
		{ "no input avaliable",					0, 0 },
		{ "unknown word",						0, 1 },
		{ "word must be compiled",				0, 1 },
		{ "corrupted dictionary",				1, 0 },
		{ "not enough memory",					0, 0 },
		{ "data-stack underflow",				1, 0 },
		{ "data-stack overflow",				1, 0 },
		{ "return-stack underflow",				1, 0 },
		{ "return-stack overflow",				1, 0 },
		{ "floating-stack underflow",			1, 0 },
		{ "floating-stack overflow",			1, 0 },
		{ "data-space corrupted",				1, 0 },
		{ "data-space exhausted",				1, 0 },
		{ "unable to access image file",		0, 0 },
		{ "primitive not implemented",          0, 1 },
		{ "floating-point/math exception",		0, 0 },
		{ "segmentation fault",					0, 0 },
		{ "file not found",						0, 0 },
	};
	if (err_msg[-_error].print_word) {
		putchar('[');
		*--sp = (Cell) _dp;
		_count();
		_type();
		printf("] ");
	}
	printf("error(%d): %s.\n", -_error, err_msg[-_error].msg);
	if (err_msg[-_error].please_abort) {
		printf("Aborting...\n");
		_abort();
	}
}

void _quit() {
	while (1) {
		rp = rp_top;
		_source_id = 0;
		_input_buffer = _tib;
		_state = INTERPRET;
		_error = E_OK;
		while (_error == E_OK) {
			_refill();
			if (*sp++) {
				_to_in = 0;
				_interpret();
				if (_state == INTERPRET && !_error) printf("ok\n");
				else if (_state == COMPILE) printf("ko ");
			} else _error = E_NOINPUT;
			if (_error == E_OK && _check_system) check_system();
		}
		_view_error_msg();
	}
}

void _comma() {
	*((Cell *) _dp) = *sp++;
	_dp += sizeof(Cell);
}

void _allot() {
	_dp += *sp++;
}

void _c_comma() {
	*_dp++ = (Char) *sp++;
}

void _here() {
    *--sp = (Cell) _dp;
}

void _do_exit() {
	ip = 0;
}

void _exit_imm() {
	clear_locals();
	compile_cell((Cell) _do_exit);
}

void _paren_do_colon_paren() {
	*--rp = (Cell) (ip + 1);
	ip = (pfp *) *ip;
	while (ip) (*ip++)();
	ip = (pfp *) *rp++;
}

void _colon() {
	create_definition(A_COLON);
	_state = COMPILE;
	init_locals();
}

void _variable() {
	create_definition(A_VARIABLE);
	compile_cell(0);
	mark_word(_last);
}

void _constant() {
	register Cell x = *sp++;
	create_definition(A_CONSTANT);
	compile_cell(x);
	mark_word(_last);
}

void _create() {
	create_definition(A_CREATE);
	compile_cell(0);
	mark_word(_last);
}

void _does() {
	compile_cell((Cell) _paren_does_paren);
	_exit_imm();
	mark_word(_last);
	init_locals();
}

void _paren_does_paren() {
	_last->func[0] = (pfp) (ip + 1);
}

void _semi_colon() {
	_exit_imm();
	_state = INTERPRET;
	mark_word(_last);
}

void _zero_branch() {
	if (*sp++) ip++;
	else ip += 1 + (Cell) *ip;
}

void _branch() {
	ip += 1 + (Cell) *ip;
}

void _if() {
	compile_cell((Cell) _zero_branch);
	*--sp = (Cell) _dp;
	compile_cell(0);
}

void _then() {
    register Cell *patch = (Cell *) *sp++;
    *patch = ((Cell *) _dp) - patch - 1;
}

void _else() {
	_ahead();
	*--sp = 1;
	_roll();
	_then();
}

void _begin() {
    *--sp = (Cell) _dp;
}

void _do() {
    compile_cell((Cell) _paren_do_paren);
    *--sp = (Cell) _dp;
    *--sp = 0;  /* Non e' un ?do */
}

void _paren_do_paren() {
    *--rp = *sp++;
    *--rp = *sp++;
    /* R: index limit --- */
}

void _loop() {
    register Cell q_do = *sp++;
    register Cell *dest = (Cell *) *sp++;
    compile_cell((Cell) _paren_loop_paren);
    compile_cell(dest - ((Cell *) _dp) - 1);
    if (q_do) {
        register Cell *patch = (Cell *) *sp++;
        *patch = ((Cell *) _dp) - patch - 1;
    }
}

void _paren_loop_paren() {
    if (rp[0] == ++rp[1]) {
        ip++;
        rp += 2;
    } else ip += 1 + (Cell) *ip;
}

void _i() {
    *--sp = rp[1];
}

void _j() {
    *--sp = rp[3];
}

void _plus_loop() {
    register Cell q_do = *sp++;
    register Cell *dest = (Cell *) *sp++;
    compile_cell((Cell) _paren_plus_loop_paren);
    compile_cell(dest - ((Cell *) _dp) - 1);
    if (q_do) {
        register Cell *patch = (Cell *) *sp++;
        *patch = ((Cell *) _dp) - patch - 1;
    }
}

void _paren_plus_loop_paren() {
    register Cell old_index = *rp;
    rp[1] += *sp++;
    if (old_index < rp[1] && rp[0] >= rp[1]) {
        ip++;
        rp += 2;
    } else ip += 1 + (Cell) *ip;
}

void _find() {
	register Char *addr = (Char *) *sp;
	register Cell len = (Cell) *addr++;
	register struct word_def *xt = search_word(addr, len);
	set_find_stack(addr, xt);
}

void _recurse() {
    compile_cell((Cell) _paren_do_colon_paren);
    compile_cell((Cell) &_last->func[0]);
}

void _tick() {
    register Char *addr;
    *--sp = ' ';
	_word();
	addr = (Char *) *sp;
	if (!(*sp = (Cell) search_word(addr + 1, *addr))) _error = E_NOWORD;
}

void _to_body() {
	*sp = (Cell) &((struct word_def *) *sp)->func[0];
}

void _abort() {
	*--sp = -1;
	_throw();
}

void _abort_quote() {
	_if();
	_s_quote();
	compile_cell((Cell) _do_literal);
	compile_cell(-2);
	compile_cell((Cell) _throw);
	_then();
}

void _count() {
	register Char *addr = (Char *) *sp;
    sp--;
    sp[0] = (Cell) *addr;
    sp[1]++;
}

void _decimal() {
    _base = 10;
}

void _environment_query() {
	register Cell len = *sp++;
	register Char *addr = (Char *) *sp++;
	static struct {
		Char *name;
		Cell *var;
	} kw[] = {
		{ "/COUNTED-STRING",		&_env_slash_counted_string },
		{ "/HOLD",					&_env_slash_hold },
		{ "/PAD",					&_env_slash_pad },
		{ "ADDRESS-UNIT-BITS",		&_env_address_unit_bits },
		{ "CORE",					&_env_core },
		{ "CORE-EXT",				&_env_core_ext },
		{ "FLOORED",				&_env_floored },
		{ "MAX-CHAR",				&_env_max_char },
		{ "MAX-D",					&_env_max_d },
		{ "MAX-N",					&_env_max_n },
		{ "MAX-U",					&_env_max_u },
		{ "MAX-UD",					&_env_max_ud },
		{ "RETURN-STACK-CELLS",		&_env_return_stack_cells },
		{ "STACK-CELLS",			&_env_stack_cells },
		{ "DOUBLE",					&_env_double },
		{ "DOUBLE-EXT",				&_env_double_ext },
		{ "FLOATING",				&_env_floating },
		{ "FLOATING-STACK",			&_env_floating_stack },
		{ "MAX-FLOAT",				&_env_max_float },
		{ "FLOATING-EXT",			&_env_floating_ext },
		{ "MEMORY-ALLOC",			&_env_memory_alloc },
		{ "MEMORY-ALLOC-EXT",		&_env_memory_alloc_ext },
		{ "SEARCH-ORDER",			&_env_search_order },
		{ "WORDLISTS",				&_env_wordlists },
		{ "SEARCH-ORDER-EXT",		&_env_search_order_ext },
		{ "TOOLS",					&_env_tools },
		{ "TOOLS-EXT",				&_env_tools_ext },
		{ "#LOCALS",				&_env_number_locals },
		{ "LOCALS",					&_env_locals },
		{ "LOCALS-EXT",				&_env_locals_ext },
		{ "FACILITY",				&_env_facility },
		{ "FACILITY-EXT",			&_env_facility_ext },
		{ "BLOCK",					&_env_block },
		{ "BLOCK-EXT",				&_env_block_ext },
		{ "EXCEPTION",				&_env_exception },
		{ "EXCEPTION-EXT",			&_env_exception_ext },
		{ "FILE",					&_env_file },
		{ "FILE-EXT",				&_env_file_ext },
		{ "STRING",					&_env_string },
		{ "STRING-EXT",				&_env_string_ext },
		{ NULL, 					NULL },
	};
	register int i = 0;
	for (i = 0; i < len; i++) addr[i] = toupper(addr[i]);
	i = 0;
	while (kw[i].name && memcmp(addr, kw[i].name, len)) i++;
	if (kw[i].name) {
		if (!strcmp(kw[i].name + 1, "MAX-UD")) {
			sp -= 2;
			PUT_DCELL(sp, MAX_UD);
		} else if (!strcmp(kw[i].name + 1, "MAX-FLOAT"))
			*--fp = MAX_F;
		else *--sp = *kw[i].var;
		*--sp = FFLAG(1);
	} else *--sp = FFLAG(0);
}

void _execute() {
	exec_word((struct word_def *) *sp++);
}

void _fill() {
	register int c = (int) *sp++;
	register UCell u = (UCell) *sp++;
	register Char *addr = (Char *) *sp++;
	if (u) memset(addr, c, u);
}

void _immediate() {
    _last->class |= IMMEDIATE;
}

void _key() {
	*--sp = d_getch();
}

void _leave() {
    rp += 2;
	while (*ip != _paren_loop_paren && *ip != _paren_plus_loop_paren) ip++;
    ip += 2;
}

void _literal() {
    compile_cell((Cell) _do_literal);
    compile_cell(sp[0]);
    sp++;
}

void _move() {
    register UCell u = (UCell) *sp++;
    register Char *dest = (Char *) *sp++;
    register Char *source = (Char *) *sp++;
    if (u) memmove(dest, source, u);
}

void _postpone() {
    *--sp = ' ';
    _word();
    _find();
    if (*sp++ > 0)  /* IMMEDIATE word */
        compile_word((struct word_def *) *sp++);
    else {
        compile_cell((Cell) _paren_compile_paren);
        compile_cell(sp[0]);
        sp++;
    }
}

void _paren_compile_paren() {
    compile_word((struct word_def *) *sp++);
}

void _s_quote() {
	if (_state == INTERPRET) {
		*--sp = '"';
		_word();
		memcpy(s_tmp_buffer, _dp, *_dp + 1);
		sp[0] = (Cell) s_tmp_buffer;
		_count();
	} else {
		_c_quote();
		compile_cell((Cell) _count);
	}
}

void _sign() {
    if (*sp++ < 0) {
		*p_pnos-- = '-';
		in_pnos++;
    }
}

void _unloop() {
    rp += 2;
}

void _left_bracket() {
    _state = INTERPRET;
}

void _bracket_tick() {
    _tick();
    _literal();
}

void _char() {
	*--sp = ' ';
	_word();
	sp[0] = _dp[1];
}

void _bracket_char() {
	_char();
	_literal();
}

void _right_bracket() {
    _state = COMPILE;
}

void _while() {
	_if();
	*--sp = 1;
	_roll();
}

void _repeat() {
	_again();
	_then();
}

void _do_value() {
	*--sp = (Cell) *((Cell *) *ip++);
}

/**************************************************************************/
/* AUXILIARY FUNCTIONS ****************************************************/
/**************************************************************************/

/* strmatch: compare two strings, the first is expressed as (s1, len), while
 * the second is a counted string pointed by "s2". If the two strings are
 * identical return 0, 1 otherwise. The comparison is case INsensitive
 */
int strmatch(const Char *s1, const Char *s2, int len1) {
	if (len1 != *s2++) return (1);
	else {
		while (len1--) if (toupper(*s1++) != toupper(*s2++)) return (1);
		return (0);
	}
}

/* search_wordlist: search a word (name, len) within the selected vocabulary.
 * Called by "search_word"
 */
struct word_def *search_wordlist(Char *name, Cell len, struct vocabulary *wid) {
	register struct word_def *p = wid->voc[hash_func(name, len)];
	while (p && strmatch(name, p->name, len)) p = p->link;
	return (p);
}

/* search_word: search the word (name, len) into the vocabularies, starting
 * with the vocabulary on the top of the vocabularies stack. If found,
 * return the word's execution token, which is a pointer to the structure
 * "word_def" of the word. If not found, return NULL.
 */
struct word_def *search_word(Char *name, Cell len) {
	register struct word_def *p;
	register Cell ttop = top;
	if (locals_defined()) {
		p = get_first_local();
		while (p && strmatch(name, p->name, len)) p = p->link;
		if (p) return (p);
	}
	while (ttop >= 0) {
		p = search_wordlist(name, len, ttop >= 0 ? list[ttop] : forth_wid);
		if (p) return (p);
		ttop--;
	}
	return (0);
}

/* ins_word: add the word with execution token "p" in the current
 * compilation vocabulary
 */
void ins_word(struct word_def *p) {
	register int hash = hash_func(p->name + 1, *p->name);
	p->link = voc->voc[hash];
}

/* mark_word: make the word with execution token "p" visible, by updating
 * the compilation vocabulary head pointer
 */
void mark_word(struct word_def *p) {
	register int hash = hash_func(p->name + 1, *p->name);
	voc->voc[hash] = p;
}

/* set_find_stack: setup the data stack after a search in the vocabularies
 * as reuired by the word "find"
 */
void set_find_stack(Char *addr, struct word_def *xt) {
	if (xt) {
		*sp = (Cell) xt;
		if (xt->class & IMMEDIATE) *--sp = 1;
		else *--sp = (Cell) -1;
	} else {
		*sp = (Cell) addr;
		*--sp = 0;
	}
}

/* is_base_digit: return true if the digit "ch" is valid in the current base
 * stored in the variable "base".
 */
int is_base_digit(Char ch) {
	ch = toupper(ch);
	if (ch >= '0' && ch <= '9') {
		if (ch - '0' < _base) return (1);
		else return (0);
	}
	if (ch >= 'A' && ch <= 'Z') {
		if (ch - 'A' + 10 < _base) return (1);
		else return (0);
	}
	return (0);
}

/* process_char: do the work when a key is stroken on the keyboard.
 * "addr" is a base pointer to the buffer where the characters are to be
 * stored, "max_len" is the size of the buffer, "cur_pos" the current
 * position within the buffer, and "ch" the character to be processed.
 */
int process_char(Char *addr, int max_len, int cur_pos, char ch) {
	switch (ch) {
		case '\b':
			if (cur_pos) cur_pos--;
			else putchar('\a');
			break;
		case 0:
        case EOF:
        default:
            if (ch >= 32) {
				if (cur_pos < max_len) addr[cur_pos++] = ch;
				else putchar('\a');
            }
			break;
	}
	return cur_pos;
}

/* create_definition: create a new word in the dictionary allocating the
 * space for the name, which is stored yet by the call to "word", then
 * allocating a structure "word_def" and setting the "class" field to the
 * value passed to the function.
 */
void create_definition(Cell class) {
	register struct word_def *def;
	register Char *name;
	*--sp = (Cell) ' ';
	name = _dp;
	_word();
	sp++;
	_dp = (Char *) WORD_PTR(_dp);
	_align();
	def = (struct word_def *) _dp;
	_last = def;
	def->name = name;
	def->class = class;
	ins_word(def);
	_dp += sizeof(struct word_def) - sizeof(Cell);
}

/* exec_colon: execute a colon definition, with the first instruction pointed
 * by "ip0"
 */
void exec_colon(pfp *ip0) {
	register pfp *old_ip = ip;
	ip = ip0;
	while (ip) (*ip++)();
	ip = old_ip;
}

/* exec_word: execute the word with execution token "xt" when interpreting
 */
void exec_word(struct word_def *xt) {
	switch (xt->class & A_WORD) {
		case A_PRIMITIVE: xt->func[0](); break;
		case A_FVARIABLE:
		case A_2VARIABLE:
		case A_VARIABLE: *--sp = (Cell) &xt->func[0]; break;
		case A_COLON: exec_colon(&xt->func[0]); break;
		case A_VALUE:
		case A_USER:
		case A_CONSTANT: *--sp = (Cell) xt->func[0]; break;
		case A_2CONSTANT:
			*--sp = (Cell) xt->func[0];
			*--sp = (Cell) xt->func[1];
			break;
		case A_FCONSTANT: *--fp = *((Real *) &xt->func[0]); break;
		case A_CREATE:
			*--sp = (Cell) &xt->func[1];
			if (xt->func[0]) exec_colon((pfp *) xt->func[0]);
			break;
		case A_MARKER:
			exec_marker((struct voc_marker *) &xt->func[0]);
			break;
		case A_LOCAL:
		default: _error = E_NOVOC; break;
	}
}

/* compile_word: compile word with execution token "xt" within the dictionary
 */
void compile_word(struct word_def *xt) {
	switch (xt->class & A_WORD) {
		case A_PRIMITIVE:
			compile_cell((Cell) xt->func[0]);
			break;
		case A_VARIABLE:
		case A_2VARIABLE:
		case A_FVARIABLE:
			compile_cell((Cell) _do_literal);
			compile_cell((Cell) &xt->func[0]);
			break;
		case A_VALUE:
			compile_cell((Cell) _do_value);
			compile_cell((Cell) &xt->func[0]);
			break;
		case A_USER:
		case A_CONSTANT:
			compile_cell((Cell) _do_literal);
			compile_cell((Cell) xt->func[0]);
			break;
		case A_2CONSTANT:
			compile_cell((Cell) _do_literal);
			compile_cell((Cell) xt->func[0]);
			compile_cell((Cell) _do_literal);
			compile_cell((Cell) xt->func[1]);
			break;
		case A_FCONSTANT:
			compile_cell((Cell) _do_fliteral);
			compile_real(*((Real *) &xt->func[0]));
			break;
		case A_COLON:
			compile_cell((Cell) _paren_do_colon_paren);
			compile_cell((Cell) &xt->func[0]);
			break;
		case A_CREATE:
			compile_cell((Cell) _do_literal);
			compile_cell((Cell) &xt->func[1]);
			if (xt->func[0]) {
				compile_cell((Cell) _paren_do_colon_paren);
				compile_cell((Cell) xt->func[0]);
			}
			break;
		case A_LOCAL:
			compile_cell((Cell) _paren_read_local_paren);
			compile_cell((Cell) xt->func[0]);
			break;
		case A_MARKER:
			compile_cell((Cell) _paren_marker_paren);
			compile_cell((Cell) &xt->func[0]);
			break;
		default: _error = E_NOVOC; break;
	}
}

/* save_input_specification: save all the information needed to restore the
 * state of current input later. First the word "save-input" is called, and
 * then each Cell on the stack is copied in the return stack
 */
void save_input_specification() {
	register int dim, dim1;
	_save_input();
	dim1 = dim = *sp++;
	while (dim--) _to_r();
	*--sp = (Cell) dim1;
	_to_r();
}

/* restore_input_specification: restore the input source by calling
 * "restore-input" after that the Cells on the return stack has been moved
 * on the data stack
 */
void restore_input_specification() {
	register int dim = *rp++, dim1 = dim;
	while (dim--) _r_from();
	*--sp = (Cell) dim1;
	_restore_input();
	sp++;
}

/* check_system: perform some tests to verify that's everything ok */
void check_system() {
	if (sp > sp_top) _error = E_DSTK_UNDER;
	else if (sp < sp_base) _error = E_DSTK_OVER;
	else if (rp > rp_top) _error = E_RSTK_UNDER;
	else if (rp < rp_base) _error = E_RSTK_OVER;
	else if (fstack_size && fp > fp_top) _error = E_FSTK_UNDER;
	else if (fstack_size && fp < fp_base) _error = E_FSTK_OVER;
	else if (_dp < dp0) _error = E_DSPACE_UNDER;
	else if (_dp > dp0 + dspace_size) _error = E_DSPACE_OVER;
}
