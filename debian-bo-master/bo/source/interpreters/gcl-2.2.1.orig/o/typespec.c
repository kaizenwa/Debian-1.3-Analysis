/*
 Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

This file is part of GNU Common Lisp, herein referred to as GCL

GCL is free software; you can redistribute it and/or modify it under
the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
License for more details.

You should have received a copy of the GNU Library General Public License 
along with GCL; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

/*
	typespec.c

	type specifier routines
*/

#define NEED_MP_H
#include "include.h"




object sLkeyword;


check_type_integer(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_fixnum && t != t_bignum)
		*p = wrong_type_argument(sLinteger, *p);
}

check_type_non_negative_integer(p)
object *p;
{
	enum type t;

	for (;;) {
		t = type_of(*p);
		if (t == t_fixnum) {
			if (fix((*p)) >= 0)
				break;
		} else if (t == t_bignum) {
			if (big_sign((*p)) >= 0)
				break;
		}
		*p = wrong_type_argument(TSnon_negative_integer, *p);
	}
}

check_type_rational(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_fixnum &&
	       t != t_bignum && t != t_ratio)
		*p = wrong_type_argument(sLrational, *p);
}

check_type_float(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_shortfloat && t != t_longfloat)
		*p = wrong_type_argument(sLfloat, *p);
}

check_type_or_integer_float(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_fixnum && t != t_bignum &&
	       t != t_shortfloat && t != t_longfloat)
		*p = wrong_type_argument(TSor_integer_float, *p);
}

check_type_or_rational_float(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_fixnum && t != t_bignum &&
	       t != t_ratio && t != t_shortfloat && t != t_longfloat)
		*p = wrong_type_argument(TSor_rational_float, *p);
}

check_type_number(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_fixnum && t != t_bignum &&
	       t != t_ratio && t != t_shortfloat && t != t_longfloat &&
	       t != t_complex)
		*p = wrong_type_argument(sLnumber, *p);
}

check_type_bit(p)
object *p;
{
	while (type_of(*p) != t_fixnum ||
	       fix((*p)) != 0 && fix((*p)) != 1)
		*p = wrong_type_argument(sLbit, *p);
}

check_type_character(p)
object *p;
{
	while (type_of(*p) != t_character)
		*p = wrong_type_argument(sLcharacter, *p);
}

check_type_string_char(p)
object *p;
{
	while (type_of(*p) != t_character ||
	       char_font((*p)) != 0 ||
	       char_bits((*p)) != 0)
		*p = wrong_type_argument(sLcharacter, *p);
}

check_type_symbol(p)
object *p;
{
	while (type_of(*p) != t_symbol)
		*p = wrong_type_argument(sLsymbol, *p);
}

check_type_or_symbol_string(p)
object *p;
{
	while (type_of(*p) != t_symbol && type_of(*p) != t_string)
		*p = wrong_type_argument(TSor_symbol_string, *p);
}

check_type_or_string_symbol(p)
object *p;
{
	while (type_of(*p) != t_symbol && type_of(*p) != t_string)
		*p = wrong_type_argument(TSor_string_symbol, *p);
}

check_type_or_symbol_string_package(p)
object *p;
{
	while (type_of(*p) != t_symbol &&
	       type_of(*p) != t_string &&
	       type_of(*p) != t_package)
		*p = wrong_type_argument(TSor_symbol_string_package,
 					   *p);
}

check_type_package(p)
object *p;
{
	while (type_of(*p) != t_package)
		*p = wrong_type_argument(sLpackage, *p);
}

check_type_string(p)
object *p;
{
	while (type_of(*p) != t_string)
		*p = wrong_type_argument(sLstring, *p);
}

check_type_bit_vector(p)
object *p;
{
	while (type_of(*p) != t_bitvector)
		*p = wrong_type_argument(sLbit_vector, *p);
}

check_type_cons(p)
object *p;
{
	while (type_of(*p) != t_cons)
		*p = wrong_type_argument(sLcons, *p);
}

check_type_stream(p)
object *p;
{
	while (type_of(*p) != t_stream)
		*p = wrong_type_argument(sLstream, *p);
}

check_type_readtable(p)
object *p;
{
	while (type_of(*p) != t_readtable)
		*p = wrong_type_argument(sLreadtable, *p);
}

#ifdef UNIX
check_type_or_Pathname_string_symbol(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_pathname &&
	       t != t_string && t != t_symbol)
		*p = wrong_type_argument(
			TSor_pathname_string_symbol, *p);
}
#endif

check_type_or_pathname_string_symbol_stream(p)
object *p;
{
	enum type t;

	while ((t = type_of(*p)) != t_pathname &&
	       t != t_string && t != t_symbol && t != t_stream)
		*p = wrong_type_argument(
			TSor_pathname_string_symbol_stream, *p);
}

check_type_random_state(p)
object *p;
{
	while (type_of(*p) != t_random)
		*p = wrong_type_argument(sLrandom_state, *p);
}

check_type_hash_table(p)
object *p;
{
	while (type_of(*p) != t_hashtable)
		*p = wrong_type_argument(sLhash_table, *p);
}

check_type_array(p)
object *p;
{
BEGIN:
	switch (type_of(*p)) {
	case t_array:
	case t_vector:
	case t_string:
	case t_bitvector:
		return;

	default:
		*p = wrong_type_argument(sLarray, *p);
		goto BEGIN;
	}
}

check_type_vector(p)
object *p;
{
BEGIN:
	switch (type_of(*p)) {
	case t_vector:
	case t_string:
	case t_bitvector:
		return;

	default:
		*p = wrong_type_argument(sLvector, *p);
		goto BEGIN;
	}
}

void
check_type(x,t)
     object x;
     int t;
{if (type_of(x) !=t)
   FEerror("~s is not a ~a",2,
	   x,make_simple_string(tm_table[t].tm_name +1));
}
   


Ltype_of()
{
	int i;

	check_arg(1);

	switch (type_of(vs_base[0])) {
	case t_fixnum:
		vs_base[0] = sLfixnum;
		break;

	case t_bignum:
		vs_base[0] = sLbignum;
		break;

	case t_ratio:
		vs_base[0] = sLratio;
		break;

	case t_shortfloat:
		vs_base[0] = sLshort_float;
		break;

	case t_longfloat:
		vs_base[0] = sLlong_float;
		break;

	case t_complex:
		vs_base[0] = sLcomplex;
		break;

	case t_character:
		if (char_font(vs_base[0]) != 0
		 || char_bits(vs_base[0]) != 0)
			vs_base[0] = sLcharacter;
		else {
			i = char_code(vs_base[0]);
			if (' ' <= i && i < '\177' || i == '\n')
				vs_base[0] = sLstandard_char;
			else
				vs_base[0] = sLstring_char;
		}
		break;

	case t_symbol:
		if (vs_base[0]->s.s_hpack == keyword_package)
			vs_base[0] = sLkeyword;
		else
			vs_base[0] = sLsymbol;
		break;

	case t_package:
		vs_base[0] = sLpackage;
		break;

	case t_cons:
		vs_base[0] = sLcons;
		break;

	case t_hashtable:
		vs_base[0] = sLhash_table;
		break;

	case t_array:
		if (vs_base[0]->a.a_adjustable ||
		    vs_base[0]->a.a_displaced->c.c_car == Cnil)
			vs_base[0] = sLarray;
		else
			vs_base[0] = sLsimple_array;
		break;

	case t_vector:
		if (vs_base[0]->v.v_adjustable ||
		    vs_base[0]->v.v_hasfillp ||
		    vs_base[0]->v.v_displaced->c.c_car == Cnil ||
		    (enum aelttype)vs_base[0]->v.v_elttype != aet_object)
			vs_base[0] = sLvector;
		else
			vs_base[0] = sLsimple_vector;
		break;

	case t_string:
		if (vs_base[0]->st.st_adjustable ||
		    vs_base[0]->st.st_hasfillp ||
		    vs_base[0]->st.st_displaced->c.c_car == Cnil)
			vs_base[0] = sLstring;
		else
			vs_base[0] = sLsimple_string;
		break;

	case t_bitvector:
		if (vs_base[0]->bv.bv_adjustable ||
		    vs_base[0]->bv.bv_hasfillp ||
		    vs_base[0]->bv.bv_displaced->c.c_car == Cnil)
			vs_base[0] = sLbit_vector;
		else
			vs_base[0] = sLsimple_bit_vector;
		break;

	case t_structure:
		
		vs_base[0] = S_DATA(vs_base[0]->str.str_def)->name;
		break;

	case t_stream:
#ifdef USER_DEFINED_STREAMS
		if (vs_base[0]->sm.sm_mode == (int)smm_user_defined)
		   vs_base[0]= vs_base[0]->sm.sm_object1->str.str_self[8];
		else
#endif
		vs_base[0] = sLstream;
		break;

	case t_readtable:
		vs_base[0] = sLreadtable;
		break;

	case t_pathname:
		vs_base[0] = sLpathname;
		break;

	case t_random:
		vs_base[0] = sLrandom_state;
		break;

	case t_sfun:
	case t_gfun:	
	case t_cfun:
        case t_vfun:
	case t_afun:
	case t_cclosure:
        case t_closure:
		vs_base[0] = sLcompiled_function;
		break;

	default:
		error("not a lisp data object");
	}
}

DEF_ORDINARY("COMMON",sLcommon,LISP,"");
DEF_ORDINARY("NULL",sLnull,LISP,"");
DEF_ORDINARY("CONS",sLcons,LISP,"");
DEF_ORDINARY("LIST",sLlist,LISP,"");
DEF_ORDINARY("SYMBOL",sLsymbol,LISP,"");
DEF_ORDINARY("ARRAY",sLarray,LISP,"");
DEF_ORDINARY("VECTOR",sLvector,LISP,"");
DEF_ORDINARY("BIT-VECTOR",sLbit_vector,LISP,"");
DEF_ORDINARY("STRING",sLstring,LISP,"");
DEF_ORDINARY("SEQUENCE",sLsequence,LISP,"");
DEF_ORDINARY("SIMPLE-ARRAY",sLsimple_array,LISP,"");
DEF_ORDINARY("SIMPLE-VECTOR",sLsimple_vector,LISP,"");
DEF_ORDINARY("SIMPLE-BIT-VECTOR",sLsimple_bit_vector,LISP,"");
DEF_ORDINARY("SIMPLE-STRING",sLsimple_string,LISP,"");
DEF_ORDINARY("FUNCTION",sLfunction,LISP,"");
DEF_ORDINARY("COMPILED-FUNCTION",sLcompiled_function,LISP,"");
DEF_ORDINARY("PATHNAME",sLpathname,LISP,"");
DEF_ORDINARY("CHARACTER",sLcharacter,LISP,"");
DEF_ORDINARY("NUMBER",sLnumber,LISP,"");
DEF_ORDINARY("RATIONAL",sLrational,LISP,"");
DEF_ORDINARY("FLOAT",sLfloat,LISP,"");
DEF_ORDINARY("STRING-CHAR",sLstring_char,LISP,"");
DEF_ORDINARY("INTEGER",sLinteger,LISP,"");
DEF_ORDINARY("RATIO",sLratio,LISP,"");
DEF_ORDINARY("SHORT-FLOAT",sLshort_float,LISP,"");
DEF_ORDINARY("STANDARD-CHAR",sLstandard_char,LISP,"");
DEF_ORDINARY("FIXNUM",sLfixnum,LISP,"");
DEF_ORDINARY("COMPLEX",sLcomplex,LISP,"");
DEF_ORDINARY("SINGLE-FLOAT",sLsingle_float,LISP,"");
DEF_ORDINARY("PACKAGE",sLpackage,LISP,"");
DEF_ORDINARY("BIGNUM",sLbignum,LISP,"");
DEF_ORDINARY("RANDOM-STATE",sLrandom_state,LISP,"");
DEF_ORDINARY("DOUBLE-FLOAT",sLdouble_float,LISP,"");
DEF_ORDINARY("STREAM",sLstream,LISP,"");
DEF_ORDINARY("BIT",sLbit,LISP,"");
DEF_ORDINARY("READTABLE",sLreadtable,LISP,"");
DEF_ORDINARY("LONG-FLOAT",sLlong_float,LISP,"");
DEF_ORDINARY("HASH-TABLE",sLhash_table,LISP,"");
DEF_ORDINARY("KEYWORD",sLkeyword,LISP,"");
DEF_ORDINARY("STRUCTURE",sLstructure,LISP,"");
DEF_ORDINARY("SATISFIES",sLsatisfies,LISP,"");
DEF_ORDINARY("MEMBER",sLmember,LISP,"");
DEF_ORDINARY("NOT",sLnot,LISP,"");
DEF_ORDINARY("OR",sLor,LISP,"");
DEF_ORDINARY("AND",sLand,LISP,"");
DEF_ORDINARY("VALUES",sLvalues,LISP,"");
DEF_ORDINARY("MOD",sLmod,LISP,"");
DEF_ORDINARY("SIGNED-BYTE",sLsigned_byte,LISP,"");
DEF_ORDINARY("UNSIGNED-BYTE",sLunsigned_byte,LISP,"");
DEF_ORDINARY("SIGNED-CHAR",sLsigned_char,LISP,"");
DEF_ORDINARY("UNSIGNED-CHAR",sLunsigned_char,LISP,"");
DEF_ORDINARY("SIGNED-SHORT",sLsigned_short,LISP,"");
DEF_ORDINARY("UNSIGNED-SHORT",sLunsigned_short,LISP,"");
DEF_ORDINARY("*",sLA,LISP,"");
DEF_ORDINARY("PLUSP",sLplusp,LISP,"");
DEFCONST("CHAR-SIZE",sSchar_size,SI,small_fixnum(CHAR_SIZE),"Size in bits of a character");
DEFCONST("SHORT-SIZE",sSshort_size,SI,small_fixnum(CHAR_SIZE*sizeof(short)),"Size in bits of a short integer");

     
init_typespec()
{
}

init_typespec_function()
{
	TSor_symbol_string
	= make_cons(sLor, make_cons(sLsymbol, make_cons(sLstring, Cnil)));
	enter_mark_origin(&TSor_symbol_string);
	TSor_string_symbol
	= make_cons(sLor, make_cons(sLstring, make_cons(sLsymbol, Cnil)));
	enter_mark_origin(&TSor_string_symbol);
	TSor_symbol_string_package
	= make_cons(sLor,
		    make_cons(sLsymbol,
			      make_cons(sLstring,
					make_cons(sLpackage, Cnil))));
	enter_mark_origin(&TSor_symbol_string_package);

	TSnon_negative_integer
	= make_cons(sLinteger,
		    make_cons(make_fixnum(0), make_cons(sLA, Cnil)));
	enter_mark_origin(&TSnon_negative_integer);
	TSpositive_number = make_cons(sLsatisfies, make_cons(sLplusp, Cnil));
	enter_mark_origin(&TSpositive_number);
	TSor_integer_float
	= make_cons(sLor, make_cons(sLinteger, make_cons(sLfloat, Cnil)));
	enter_mark_origin(&TSor_integer_float);
	TSor_rational_float
	= make_cons(sLor, make_cons(sLrational, make_cons(sLfloat, Cnil)));
	enter_mark_origin(&TSor_rational_float);
#ifdef UNIX
	TSor_pathname_string_symbol
	= make_cons(sLor,
		    make_cons(sLpathname,
			      make_cons(sLstring,
					make_cons(sLsymbol,
						  Cnil))));
	enter_mark_origin(&TSor_pathname_string_symbol);
#endif
	TSor_pathname_string_symbol_stream
	= make_cons(sLor,
		    make_cons(sLpathname,
			      make_cons(sLstring,
					make_cons(sLsymbol,
						  make_cons(sLstream,
							    Cnil)))));
	enter_mark_origin(&TSor_pathname_string_symbol_stream);

	make_function("TYPE-OF", Ltype_of);
}				
