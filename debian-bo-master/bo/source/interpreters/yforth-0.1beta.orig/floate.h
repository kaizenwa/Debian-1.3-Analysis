/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     floate.h
 * Abstract:        include file for "floating-extension" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __FLOATE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __FLOATE_H__
#define __FLOATE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(d_f_store,                     "df!",                  0)
code(d_f_fetch,                     "df@",                  0)
code(align,                         "dfalign",              0)
code(aligned,                       "dfaligned",            0)
code(d_float_plus,                  "dfloat+",              0)
code(d_floats,                      "dfloats",              0)
code(f_star_star,                   "f**",                  0)
code(f_dot,                         "f.",                   0)
code(f_abs,                         "fabs",                 0)
code(f_a_cos,                       "facos",                0)
code(f_a_cosh,                      "facosh",               0)
code(f_a_log,                       "falog",                0)
code(f_a_sin,                       "fasin",                0)
code(f_a_sinh,                      "fasinh",               0)
code(f_a_tan,                       "fatan",                0)
code(f_a_tan2,                      "fatan2",               0)
code(f_a_tanh,                      "fatanh",               0)
code(f_cos,                         "fcos",                 0)
code(f_cosh,                        "fcosh",                0)
code(f_e_dot,                       "fe.",                  0)
code(f_exp,                         "fexp",                 0)
code(f_exp_m_one,                   "fexpm1",               0)
code(f_ln,                          "fln",                  0)
code(f_ln_p_one,                    "flnp1",                0)
code(f_log,                         "flog",                 0)
code(f_s_dot,                       "fs.",                  0)
code(f_sin,                         "fsin",                 0)
code(f_sin_cos,                     "fsincos",              0)
code(f_sinh,                        "fsinh",                0)
code(f_sqrt,                        "fsqrt",                0)
code(f_tan,                         "ftan",                 0)
code(f_tanh,                        "ftanh",                0)
code(f_proximate,                   "f~",                   0)
code(precision,                     "precision",            0)
code(set_precision,                 "set-precision",        0)
code(s_f_store,                     "sf!",                  0)
code(s_f_fetch,                     "sf@",                  0)
code(align,                         "sfalign",              0)
code(aligned,                       "sfaligned",            0)
code(s_float_plus,                	"sfloat+",              0)
code(s_floats,                      "sfloats",              0)

#ifdef PROTOTYPES

#endif

#endif

