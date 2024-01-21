/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     double.h
 * Abstract:        include file for "double-numbers" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __DOUBLE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __DOUBLE_H__
#define __DOUBLE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(two_constant,						"2constant",			0)
code(two_literal,						"2literal",				COMP_ONLY | IMMEDIATE)
code(two_variable,						"2variable",			0)
code(d_plus,							"d+",					0)
code(d_minus,							"d-",					0)
code(d_dot,								"d.",					0)
code(d_dot_r,							"d.r",					0)
code(d_zero_less,						"d0<",					0)
code(d_zero_equals,						"d0=",					0)
code(d_two_star,						"d2*",					0)
code(d_two_slash,						"d2/",					0)
code(d_less_than,						"d<",					0)
code(d_equals,							"d=",					0)
code(drop,								"d>s",					0)
code(dabs,								"dabs",					0)
code(dmax,								"dmax",					0)
code(dmin,								"dmin",					0)
code(dnegate,							"dnegate",				0)
code(m_star_slash,						"m*/",					0)
code(m_plus,							"m+",					0)

#ifdef PROTOTYPES

#endif

#endif

