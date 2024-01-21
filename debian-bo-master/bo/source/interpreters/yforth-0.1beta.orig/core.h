/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     core.h
 * Abstract:        include file for "core" word set
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __CORE_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __CORE_H__
#define __CORE_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

variable(Cell, to_in,                   ">in")
variable(Cell, source_id,				"source-id")
variable(Char *, tib,                   "tib")
variable(Char *, input_buffer,          "input-buffer")
variable(Cell, in_input_buffer,         "in-input-buffer")
variable(Cell, base,                    "base")
variable(Char *, dp,                    "dp")
variable(Cell, error,                   "error")
variable(struct word_def *, last,       "last")
variable(Cell, state,                   "state")
variable(Cell, env_slash_counted_string, "&counted-string")
variable(Cell, env_slash_hold,           "&hold")
variable(Cell, env_slash_pad,            "&pad")
variable(Cell, env_address_unit_bits,    "&address-unit-bits")
variable(Cell, env_core,                 "&core")
variable(Cell, env_core_ext,             "&core-ext")
variable(Cell, env_floored,              "&floored")
variable(Cell, env_max_char,             "&max-char")
variable(Cell, env_max_d,                "&max-d")
variable(Cell, env_max_n,                "&max-n")
variable(Cell, env_max_u,                "&max-u")
variable(Cell, env_max_ud,               "&max-ud")
variable(Cell, env_return_stack_cells,   "&return-stack-cells")
variable(Cell, env_stack_cells,          "&stack-cells")
variable(Cell, env_double,               "&double")
variable(Cell, env_double_ext,           "&double-ext")
variable(Cell, env_floating,             "&floating")
variable(Cell, env_floating_stack,       "&floating-stack")
variable(Cell, env_max_float,            "&max-float")
variable(Cell, env_floating_ext,         "&floating-ext")
variable(Cell, env_memory_alloc,         "&memory-alloc")
variable(Cell, env_memory_alloc_ext,	 "&memory-alloc-ext")
variable(Cell, env_search_order,         "&search-order")
variable(Cell, env_wordlists,            "&wordlists")
variable(Cell, env_search_order_ext,     "&search-order-ext")
variable(Cell, env_tools,				"&tools")
variable(Cell, env_tools_ext,			"&tools-ext")
variable(Cell, env_number_locals,		"&#locals")
variable(Cell, env_locals,				"&locals")
variable(Cell, env_locals_ext,			"&locals-ext")
variable(Cell, env_facility,			"&facility")
variable(Cell, env_facility_ext,		"&facility-ext")
variable(Cell, env_block,				"&block")
variable(Cell, env_block_ext,			"&block-ext")
variable(Cell, env_exception,			"&exception")
variable(Cell, env_exception_ext,		"&exception-ext")
variable(Cell, env_file,				"&file")
variable(Cell, env_file_ext,			"&file-ext")
variable(Cell, env_string,				"&string")
variable(Cell, env_string_ext,			"&string-ext")
variable(Cell, check_system,			"(check-system)")

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(store,                         "!",                    0)
code(star,                          "*",                    0)
code(star_slash,                    "*/",                   0)
code(star_slash_mod,                "*/mod",                0)
code(plus,                          "+",                    0)
code(plus_store,                    "+!",                   0)
code(minus,                         "-",                    0)
code(slash,                         "/",                    0)
code(slash_mod,                     "/mod",                 0)
code(zero_less,                     "0<",                   0)
code(zero_equals,                   "0=",                   0)
code(one_plus,                      "1+",                   0)
code(one_minus,                     "1-",                   0)
code(two_store,                     "2!",                   0)
code(two_star,                      "2*",                   0)
code(two_slash,                     "2/",                   0)
code(two_fetch,                     "2@",                   0)
code(two_drop,                      "2drop",                0)
code(two_dupe,                      "2dup",                 0)
code(two_over,                      "2over",                0)
code(two_swap,                      "2swap",                0)
code(less_than,                     "<",                    0)
code(equals,                        "=",                    0)
code(greater_than,                  ">",                    0)
code(to_r,                          ">r",                   COMP_ONLY)
code(question_dupe,                 "?dup",                 0)
code(fetch,                         "@",                    0)
code(abs,                           "abs",                  0)
code(align,                         "align",                0)
code(aligned,                       "aligned",              0)
code(and,                           "and",                  0)
code(b_l,                           "bl",                   0)
code(c_store,                       "c!",                   0)
code(c_fetch,                       "c@",                   0)
code(cell_plus,                     "cell+",                0)
code(cells,                         "cells",                0)
code(char_plus,                     "char+",                0)
code(chars,                         "chars",                0)
code(depth,                         "depth",                0)
code(drop,                          "drop",                 0)
code(dupe,                          "dup",                  0)
code(f_m_slash_mod,                 "fm/mod",               0)
code(invert,                        "invert",               0)
code(l_shift,                       "lshift",               0)
code(m_star,                        "m*",                   0)
code(max,                           "max",                  0)
code(min,                           "min",                  0)
code(mod,                           "mod",                  0)
code(negate,                        "negate",               0)
code(or,                            "or",                   0)
code(over,                          "over",                 0)
code(r_from,                        "r>",                   COMP_ONLY)
code(r_fetch,                       "r@",                   COMP_ONLY)
code(rote,                          "rot",                  0)
code(r_shift,                       "rshift",               0)
code(s_to_d,                        "s>d",                  0)
code(s_m_slash_rem,                 "sm/rem",               0)
code(swap,                          "swap",                 0)
code(u_less_than,                   "u<",                   0)
code(u_m_star,                      "um*",                  0)
code(u_m_slash_mod,                 "um/mod",               0)
code(xor,                           "xor",                  0)
code(word,                          "word",                 0)
code(to_number,                     ">number",              0)
code(interpret,                     "interpret",            0)
code(accept,                        "accept",               0)
code(source,                        "source",               0)
code(paren,                         "(",                    0)
code(evaluate,                      "evaluate",             0)
code(quit,                          "quit",                 0)
code(comma,                         ",",                    0)
code(allot,                         "allot",                0)
code(c_comma,                       "c,",                   0)
code(here,                          "here",                 0)
code(exit_imm,                      "exit",                 COMP_ONLY | IMMEDIATE)
code(colon,                         ":",                    0)
code(variable,                      "variable",             0)
code(constant,                      "constant",             0)
code(create,                        "create",               0)
code(does,							"does>",				COMP_ONLY | IMMEDIATE)
code(semi_colon,                    ";",                    COMP_ONLY | IMMEDIATE)
code(if,                            "if",                 	COMP_ONLY | IMMEDIATE)
code(then,                          "then",                 COMP_ONLY | IMMEDIATE)
code(else,                          "else",                 COMP_ONLY | IMMEDIATE)
code(begin,                         "begin",                COMP_ONLY | IMMEDIATE)
code(do,                            "do",                   COMP_ONLY | IMMEDIATE)
code(loop,                          "loop",                 COMP_ONLY | IMMEDIATE)
code(i,                             "i",                    COMP_ONLY)
code(j,                             "j",                    COMP_ONLY)
code(plus_loop,                     "+loop",                COMP_ONLY | IMMEDIATE)
code(recurse,                       "recurse",              COMP_ONLY | IMMEDIATE)
code(find,                          "find",                 0)
code(less_number_sign,				"<#",					0)
code(number_sign,					"#",					0)
code(hold,							"hold",					0)
code(number_sign_s,					"#s",					0)
code(number_sign_greater,			"#>",					0)
code(dot,                           ".",                    0)
code(c_r,							"cr",					0)
code(emit,							"emit",					0)
code(space,							"space",				0)
code(spaces,						"spaces",				0)
code(type,							"type",					0)
code(u_dot,							"u.",					0)
code(dot_quote,                     ".\"",                  COMP_ONLY | IMMEDIATE)
code(tick,                          "'",                    0)
code(to_body,                       ">body",                0)
code(abort,                         "abort",                0)
code(abort_quote,                   "abort\"",              COMP_ONLY | IMMEDIATE)
code(count,                         "count",                0)
code(decimal,                       "decimal",              0)
code(environment_query,             "environment?",         0)
code(execute,                       "execute",              0)
code(fill,                          "fill",                 0)
code(immediate,                     "immediate",            0)
code(key,                           "key",                  0)
code(leave,                         "leave",                COMP_ONLY)
code(literal,                       "literal",              COMP_ONLY | IMMEDIATE)
code(move,                          "move",                 0)
code(postpone,                      "postpone",             COMP_ONLY | IMMEDIATE)
code(s_quote,                       "s\"",                  IMMEDIATE)
code(sign,                          "sign",                 0)
code(unloop,                        "unloop",               COMP_ONLY)
code(left_bracket,                  "[",                    COMP_ONLY | IMMEDIATE)
code(bracket_tick,                  "[']",                  COMP_ONLY | IMMEDIATE)
code(char,							"char",					0)
code(bracket_char,                  "[char]",               COMP_ONLY | IMMEDIATE)
code(right_bracket,                 "]",                    0)
code(while,							"while",				COMP_ONLY | IMMEDIATE)
code(repeat,						"repeat",				COMP_ONLY | IMMEDIATE)
code(paren_does_paren,				"(does)",				0)
code(paren_compile_paren,           "(compile)",            0)
code(paren_do_paren,                "(do)",                 0)
code(paren_loop_paren,              "(loop)",               0)
code(paren_plus_loop_paren,         "(+loop)",              0)
code(paren_dot_quote_paren,         "(.\")",                0)
code(paren_do_colon_paren,          "(doCol)",              0)
code(zero_branch,                   "(0branch)",            0)
code(branch,                        "(branch)",             0)
code(do_literal,                    "(doLit)",              0)
code(do_fliteral,					"(doFLit)",				0)
code(do_exit,						"(doExit)",				0)
code(do_value,						"(doValue)",			0)
code(view_error_msg,                "view-error-message",   0)
code(read_const,					"read-const",			0)

#ifdef PROTOTYPES

/**************************************************************************/
/* AUXILIARY FUNCTIONS PROTOTYPES *****************************************/
/**************************************************************************/

struct word_def *search_wordlist(Char *name, Cell len, struct vocabulary *wid);
struct word_def *search_word(Char *name, Cell len);
void ins_word(struct word_def *p);
void mark_word(struct word_def *p);
void set_find_stack(Char *addr, struct word_def *xt);
int strmatch(const Char *s1, const Char *s2, int len1);
int is_base_digit(Char ch);
int process_char(Char *addr, int max_len, int cur_pos, char ch);
void create_definition(Cell class);
void exec_colon(pfp *ip0);
void exec_word(struct word_def *xt);
void compile_word(struct word_def *xt);
void save_input_specification(void);
void restore_input_specification(void);
void check_system(void);

#endif

#endif
