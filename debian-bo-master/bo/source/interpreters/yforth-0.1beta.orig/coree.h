/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     coree.h
 * Abstract:        Include file for "core-extension" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __COREE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __COREE_H__
#define __COREE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

variable(Char *, pad,                   "pad")
variable(Cell, source_id,               "source-id")

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(dot_paren,                     ".(",                   IMMEDIATE)
code(dot_r,							".r",					0)
code(zero_not_equals,               "0<>",                  0)
code(zero_greater,                  "0>",                   0)
code(two_to_r,                      "2>r",                  COMP_ONLY)
code(two_r_from,                    "2r>",                  COMP_ONLY)
code(two_r_fetch,                   "2r@",                  COMP_ONLY)
code(colon_no_name,                 ":noname",              0)
code(not_equals,                    "<>",                   0)
code(question_do,                   "?do",                  COMP_ONLY | IMMEDIATE)
code(again,                         "again",                COMP_ONLY | IMMEDIATE)
code(c_quote,                       "c\"",                  COMP_ONLY | IMMEDIATE)
code(compile_comma,                 "compile,",             COMP_ONLY)
code(erase,                         "erase",                0)
code(false,                         "false",                0)
code(hex,                           "hex",                  0)
code(marker,                        "marker",               0)
code(nip,                           "nip",                  0)
code(parse,                         "parse",                0)
code(pick,                          "pick",                 0)
code(refill,                        "refill",               0)
code(restore_input,                 "restore-input",        0)
code(roll,                          "roll",                 0)
code(save_input,                    "save-input",           0)
code(true,                          "true",                 0)
code(tuck,                          "tuck",                 0)
code(u_dot_r,						"u.r",					0)
code(u_greater_than,                "u>",                   0)
code(unused,                        "unused",               0)
code(within,                        "within",               0)
code(backslash,                     "\\",                   IMMEDIATE)
code(bracket_compile,               "[compile]",            COMP_ONLY)
code(value,							"value",				0)
code(to,							"to",					IMMEDIATE)

code(paren_question_do_paren,       "(?do)",                0)
code(paren_write_value_paren,		"(wValue)",				0)
code(paren_marker_paren,			"(marker)",				0)

#ifdef PROTOTYPES

/**************************************************************************/
/* AUXILIARY FUNCTIONS PROTOTYPES *****************************************/
/**************************************************************************/

void exec_marker(struct voc_marker *vm);

#endif

#endif

