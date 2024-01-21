/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     float.h
 * Abstract:        include file for "floating" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __FLOAT_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __FLOAT_H__
#define __FLOAT_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(to_float,						">float",				0)
code(d_to_f,						"d>f",					0)
code(f_store,						"f!",					0)
code(f_star,						"f*",					0)
code(f_plus,						"f+",					0)
code(f_minus,						"f-",					0)
code(f_slash,						"f/",					0)
code(f_zero_less,					"f0<",					0)
code(f_zero_equals,					"f0=",					0)
code(f_less_than,					"f<",					0)
code(f_to_d,						"f>d",					0)
code(f_fetch,						"f@",					0)
code(align,							"falign",				0)
code(aligned,						"faligned",				0)
code(f_constant,					"fconstant",			0)
code(f_depth,						"fdepth",				0)
code(f_drop,						"fdrop",				0)
code(f_dupe,						"fdup",					0)
code(f_literal,						"fliteral",				COMP_ONLY | IMMEDIATE)
code(float_plus,					"float+",				0)
code(floats,						"floats",				0)
code(floor,							"floor",				0)
code(f_max,							"fmax",					0)
code(f_min,							"fmin",					0)
code(f_negate,						"fnegate",				0)
code(f_over,						"fover",				0)
code(f_rote,                        "frot",					0)
code(f_round,						"fround",				0)
code(f_swap,						"fswap",				0)
code(f_variable,					"fvariable",			0)
code(represent,                     "represent",            0)

#ifdef PROTOTYPES

#endif

#endif

