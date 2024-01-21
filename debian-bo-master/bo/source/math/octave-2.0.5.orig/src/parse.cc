
/*  A Bison parser, made from parse.y
 by  GNU Bison version 1.25
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	EXPR_AND_AND	258
#define	EXPR_OR_OR	259
#define	EXPR_AND	260
#define	EXPR_OR	261
#define	EXPR_NOT	262
#define	EXPR_LT	263
#define	EXPR_LE	264
#define	EXPR_EQ	265
#define	EXPR_NE	266
#define	EXPR_GE	267
#define	EXPR_GT	268
#define	LEFTDIV	269
#define	EMUL	270
#define	EDIV	271
#define	ELEFTDIV	272
#define	EPLUS	273
#define	EMINUS	274
#define	QUOTE	275
#define	TRANSPOSE	276
#define	PLUS_PLUS	277
#define	MINUS_MINUS	278
#define	POW	279
#define	EPOW	280
#define	NUM	281
#define	IMAG_NUM	282
#define	NAME	283
#define	SCREW	284
#define	END	285
#define	PLOT	286
#define	TEXT	287
#define	STYLE	288
#define	FOR	289
#define	WHILE	290
#define	IF	291
#define	ELSEIF	292
#define	ELSE	293
#define	SWITCH	294
#define	CASE	295
#define	OTHERWISE	296
#define	BREAK	297
#define	CONTINUE	298
#define	FUNC_RET	299
#define	UNWIND	300
#define	CLEANUP	301
#define	TRY	302
#define	CATCH	303
#define	GLOBAL	304
#define	TEXT_ID	305
#define	LEXICAL_ERROR	306
#define	FCN	307
#define	SCREW_TWO	308
#define	ELLIPSIS	309
#define	ALL_VA_ARGS	310
#define	END_OF_INPUT	311
#define	USING	312
#define	TITLE	313
#define	WITH	314
#define	COLON	315
#define	OPEN_BRACE	316
#define	CLOSE_BRACE	317
#define	CLEAR	318
#define	UNARY	319

#line 27 "parse.y"

#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef YYBYACC
#include <cstdlib>
#endif

#include <strstream.h>

#include "Matrix.h"

#include "defun.h"
#include "error.h"
#include "input.h"
#include "lex.h"
#include "oct-hist.h"
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pt-cmd.h"
#include "pt-const.h"
#include "pt-fcn.h"
#include "pt-fvc.h"
#include "pt-mat.h"
#include "pt-mvr.h"
#include "pt-exp.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "pt-pr-code.h"
#include "symtab.h"
#include "token.h"
#include "utils.h"
#include "variables.h"

// If TRUE, generate a warning for the assignment in things like
//
//   octave> if (a = 2 < n)
//
// but not
//
//   octave> if ((a = 2) < n)
//
static bool Vwarn_assign_as_truth_value;

// If TRUE, generate a warning for variable swich labels.
static bool Vwarn_variable_switch_label;

// If TRUE, generate a warning for the comma in things like
//
//   octave> global a, b = 2
//
static bool Vwarn_comma_in_global_decl;

// If TRUE, generate warning if declared function name disagrees with
// the name of the file in which it is defined.
static bool Vwarn_function_name_clash;

// If TRUE, generate warning if a statement in a function is not
// terminated with a semicolon.  Useful for checking functions that
// should only produce output using explicit printing statements.
static bool Vwarn_missing_semicolon;

// Temporary symbol table pointer used to cope with bogus function syntax.
symbol_table *tmp_local_sym_tab = 0;

// The current input line number.
int input_line_number = 0;

// The column of the current token.
int current_input_column = 1;

// Buffer for help text snagged from function files.
string help_buf;

// Forward declarations for some functions defined at the bottom of
// the file.

// Generic error messages.
static void yyerror (char *s);

// Error mesages for mismatched end tokens.
static void end_error (char *type, token::end_tok_type ettype, int l, int c);

// Check to see that end tokens are properly matched.
static int check_end (token *tok, token::end_tok_type expected);

// Try to figure out early if an expression should become an
// assignment to the built-in variable ans.
static tree_expression *maybe_convert_to_ans_assign (tree_expression *expr);

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.
static void maybe_warn_assign_as_truth_value (tree_expression *expr);

// Maybe print a warning about switch labels that aren't constants.
static void maybe_warn_variable_switch_label (tree_expression *expr);

// Create a plot command.
static tree_plot_command *make_plot_command
	 (token *tok, plot_limits *range, subplot_list *list);

// Finish building a range.
static tree_expression *finish_colon_expression (tree_colon_expression *e);

// Build a constant.
static tree_constant *make_constant (int op, token *tok_val);

// Build a binary expression.
static tree_expression *make_binary_op
	 (int op, tree_expression *op1,	token *tok_val, tree_expression *op2);

// Build a boolean expression.
static tree_expression *make_boolean_op
	 (int op, tree_expression *op1,	token *tok_val, tree_expression *op2);

// Build a prefix expression.
static tree_expression *make_prefix_op
	 (int op, tree_identifier *op1, token *tok_val);

// Build a postfix expression.
static tree_expression *make_postfix_op
	 (int op, tree_identifier *op1, token *tok_val);

// Build a binary expression.
static tree_expression *make_unary_op
	 (int op, tree_expression *op1, token *tok_val);

// Build an unwind-protect command.
static tree_command *make_unwind_command
	 (token *unwind_tok, tree_statement_list *body,
	  tree_statement_list *cleanup, token *end_tok);

// Build a try-catch command.
static tree_command *make_try_command
	 (token *try_tok, tree_statement_list *body,
	  tree_statement_list *cleanup, token *end_tok);

// Build a while command.
static tree_command *make_while_command
	 (token *while_tok, tree_expression *expr,
	  tree_statement_list *body, token *end_tok);

// Build a for command.
static tree_command *make_for_command
	 (token *for_tok, tree_index_expression *var,
	  tree_expression *expr, tree_statement_list *body,
	  token *end_tok);

// Build a for command a different way.
static tree_command *make_for_command
	 (token *for_tok, tree_matrix_row *mr, tree_expression *expr,
	  tree_statement_list *body, token *end_tok);

// Build a break command.
static tree_command *make_break_command (token *break_tok);

// Build a continue command.
static tree_command *make_continue_command (token *continue_tok);

// Build a return command.
static tree_command *make_return_command (token *return_tok);

// Start an if command.
static tree_if_command_list *start_if_command
	 (tree_expression *expr, tree_statement_list *list);

// Finish an if command.
static tree_if_command *finish_if_command
	 (token *if_tok, tree_if_command_list *list, token *end_tok);

// Build an elseif clause.
static tree_if_clause *make_elseif_clause
	 (tree_expression *expr, tree_statement_list *list);

// Finish a switch command.
static tree_switch_command *finish_switch_command
	 (token *switch_tok, tree_expression *expr,
	  tree_switch_case_list *list, token *end_tok);

// Build a switch case.
static tree_switch_case *make_switch_case
	 (tree_expression *expr, tree_statement_list *list);

// Build an assignment to a variable.
static tree_expression *make_simple_assignment
	 (tree_index_expression *var, token *eq_tok, tree_expression *expr);

// Make an expression that handles assignment of multiple values.
static tree_expression *make_multi_val_ret
	 (tree_matrix_row *mr, tree_expression *rhs, token *eq_tok);

// Begin defining a function.
static tree_function *start_function_def
	 (tree_parameter_list *param_list, tree_statement_list *body);

// Do most of the work for defining a function.
static tree_function *frob_function_def
	 (tree_identifier *id, tree_function *fcn);

// Finish defining a function.
static tree_function *finish_function_def (token *var, tree_function *fcn);

// Finish defining a function a different way.
static tree_function *finish_function_def
	 (tree_parameter_list *ret_list, tree_function *fcn);

// Make an index expression.
static tree_index_expression *make_index_expression
	 (tree_indirect_ref *indir, tree_argument_list *args);

// Finish building a matrix list.
static tree_expression *finish_matrix (tree_matrix *m);

// Maybe print a warning.  Duh.
static void maybe_warn_missing_semi (tree_statement_list *);

// Set the print flag for a statement based on the separator type.
static void set_stmt_print_flag (tree_statement_list *, char, bool);

#define ABORT_PARSE \
  do \
    { \
      global_command = 0; \
      yyerrok; \
      if (interactive) \
	YYACCEPT; \
      else \
	YYABORT; \
    } \
  while (0)


#line 266 "parse.y"
typedef union
{
// The type of the basic tokens returned by the lexer.
  token *tok_val;

// Types for the nonterminals we generate.
  char sep_type;
  tree *tree_type;
  tree_matrix *tree_matrix_type;
  tree_matrix_row *tree_matrix_row_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
  tree_identifier *tree_identifier_type;
  tree_indirect_ref *tree_indirect_ref_type;
  tree_function *tree_function_type;
  tree_index_expression *tree_index_expression_type;
  tree_colon_expression *tree_colon_expression_type;
  tree_argument_list *tree_argument_list_type;
  tree_parameter_list *tree_parameter_list_type;
  tree_command *tree_command_type;
  tree_if_command *tree_if_command_type;
  tree_if_clause *tree_if_clause_type;
  tree_if_command_list *tree_if_command_list_type;
  tree_switch_command *tree_switch_command_type;
  tree_switch_case *tree_switch_case_type;
  tree_switch_case_list *tree_switch_case_list_type;
  tree_global *tree_global_type;
  tree_global_init_list *tree_global_init_list_type;
  tree_global_command *tree_global_command_type;
  tree_statement *tree_statement_type;
  tree_statement_list *tree_statement_list_type;
  tree_plot_command *tree_plot_command_type;
  subplot *subplot_type;
  subplot_list *subplot_list_type;
  plot_limits *plot_limits_type;
  plot_range *plot_range_type;
  subplot_using *subplot_using_type;
  subplot_style *subplot_style_type;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		352
#define	YYFLAG		-32768
#define	YYNTBASE	79

#define YYTRANSLATE(x) ((unsigned)(x) <= 319 ? yytranslate[x] : 152)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    72,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    75,
    76,     7,     6,    71,     5,    78,     8,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     4,    70,     2,
     3,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    74,     2,    77,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
    62,    63,    64,    65,    66,    67,    68,    69,    73
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,     9,    11,    13,    16,    19,    21,
    23,    26,    28,    32,    33,    35,    38,    40,    44,    46,
    48,    51,    54,    58,    60,    63,    67,    73,    78,    83,
    87,    90,    91,    93,    97,    99,   102,   104,   106,   108,
   111,   114,   117,   120,   123,   126,   130,   134,   138,   142,
   146,   150,   152,   155,   158,   162,   165,   168,   172,   177,
   179,   182,   184,   188,   190,   194,   195,   197,   199,   201,
   203,   205,   207,   215,   223,   229,   237,   248,   250,   252,
   254,   258,   260,   263,   267,   270,   276,   280,   286,   288,
   291,   293,   296,   302,   306,   307,   309,   313,   315,   317,
   319,   321,   325,   327,   329,   331,   333,   336,   340,   343,
   346,   349,   352,   355,   359,   366,   369,   372,   375,   378,
   382,   386,   390,   394,   398,   402,   406,   410,   414,   418,
   422,   426,   430,   434,   438,   442,   446,   450,   454,   458,
   462,   466,   470,   474,   477,   479,   482,   483,   484,   485,
   486,   487,   492,   497,   503,   508,   513,   516,   520,   523,
   528,   531,   534,   538,   543,   548,   552,   554,   556,   558,
   560,   561,   566,   568,   572,   577,   580,   581,   584,   588,
   591,   596,   600,   604,   607,   611,   613,   615,   617,   619,
   623,   627,   631,   636,   638,   641,   643,   647,   649,   652,
   654,   658,   660,   662,   665,   668,   669,   671,   673,   675,
   677,   680,   683,   686,   687
};

static const short yyrhs[] = {    80,
     0,    62,     0,    82,    81,     0,    81,     0,    72,     0,
    82,     0,    82,    72,     0,    82,    62,     0,    57,     0,
     1,     0,    83,   149,     0,    87,     0,    83,   148,    87,
     0,     0,    85,     0,    86,   151,     0,    87,     0,    86,
   150,    87,     0,   103,     0,    98,     0,    37,    69,     0,
    37,    91,     0,    37,    89,    91,     0,    90,     0,    90,
    90,     0,    90,    90,    90,     0,    67,   115,    66,   115,
    68,     0,    67,    66,   115,    68,     0,    67,   115,    66,
    68,     0,    67,    66,    68,     0,    67,    68,     0,     0,
    92,     0,    91,    71,    92,     0,   115,     0,   115,    93,
     0,    94,     0,    96,     0,    97,     0,    94,    96,     0,
    96,    94,     0,    94,    97,     0,    97,    94,     0,    96,
    97,     0,    97,    96,     0,    94,    96,    97,     0,    94,
    97,    96,     0,    96,    94,    97,     0,    96,    97,    94,
     0,    97,    94,    96,     0,    97,    96,    94,     0,    95,
     0,    95,   115,     0,    63,   115,     0,    95,    66,   115,
     0,    64,   115,     0,    65,    39,     0,    65,    39,   115,
     0,    65,    39,   115,   115,     0,   115,     0,    55,   100,
     0,   101,     0,   100,   102,   101,     0,   141,     0,   141,
     3,   115,     0,     0,    71,     0,    88,     0,   126,     0,
    99,     0,   109,     0,   104,     0,    51,   151,    84,    52,
   151,    84,    36,     0,    53,   151,    84,    54,   151,    84,
    36,     0,    41,   115,   151,    84,    36,     0,    40,   137,
     3,   115,   151,    84,    36,     0,    40,    74,   114,   146,
    59,     3,   115,   151,    84,    36,     0,    48,     0,    49,
     0,    50,     0,    42,   105,    36,     0,   106,     0,   106,
   108,     0,   115,   151,    84,     0,   106,   107,     0,    43,
   151,   115,   151,    84,     0,    44,   151,    84,     0,    45,
   115,   151,   110,    36,     0,   111,     0,   111,   113,     0,
   112,     0,   111,   112,     0,    46,   151,   115,   151,    85,
     0,    47,   151,    84,     0,     0,   116,     0,    32,     3,
   115,     0,   117,     0,    32,     0,    33,     0,    38,     0,
    75,   116,    76,     0,   119,     0,   137,     0,   118,     0,
   143,     0,    74,    77,     0,    74,    70,    77,     0,    28,
   141,     0,    29,   141,     0,    13,   116,     0,     6,   116,
     0,     5,   116,     0,   137,     3,   116,     0,    74,   114,
   146,    59,     3,   116,     0,   141,    28,     0,   141,    29,
     0,   116,    26,     0,   116,    27,     0,   116,    30,   116,
     0,   116,    31,   116,     0,   116,     6,   116,     0,   116,
     5,   116,     0,   116,     7,   116,     0,   116,     8,   116,
     0,   116,    24,   116,     0,   116,    25,   116,     0,   116,
    21,   116,     0,   116,    22,   116,     0,   116,    20,   116,
     0,   116,    23,   116,     0,   116,    14,   116,     0,   116,
    15,   116,     0,   116,    16,   116,     0,   116,    18,   116,
     0,   116,    19,   116,     0,   116,    17,   116,     0,   116,
    11,   116,     0,   116,    12,   116,     0,   116,     9,   116,
     0,   116,    10,   116,     0,   116,     4,   116,     0,   118,
     4,   116,     0,   141,   120,     0,    38,     0,   120,    38,
     0,     0,     0,     0,     0,     0,    58,   121,   125,   127,
     0,    58,   121,   125,   131,     0,    35,   124,   121,     3,
   131,     0,   129,   121,     3,   131,     0,    74,   124,   123,
   122,     0,   128,    77,     0,   128,    60,    77,     0,   130,
    77,     0,   130,    71,    60,    77,     0,   128,   141,     0,
   128,     1,     0,   130,    71,   141,     0,   141,   124,   123,
   132,     0,   139,   151,    84,   133,     0,   151,    84,   133,
     0,    36,     0,    62,     0,   135,     0,   141,     0,     0,
   135,    78,   136,    56,     0,   134,     0,   134,    75,    76,
     0,   134,    75,   142,    76,     0,   134,    74,     0,     0,
    75,    76,     0,    75,    60,    76,     0,   140,    76,     0,
   140,    71,    60,    76,     0,    75,   138,   141,     0,   140,
    71,   141,     0,    75,     1,     0,   140,    71,     1,     0,
    34,     0,     4,     0,   115,     0,    61,     0,   142,    71,
     4,     0,   142,    71,   115,     0,   142,    71,    61,     0,
    74,   114,   144,    77,     0,   145,     0,   145,    70,     0,
   146,     0,   145,    70,   146,     0,   147,     0,   147,    71,
     0,   115,     0,   147,    71,   115,     0,    71,     0,    70,
     0,   148,    71,     0,   148,    70,     0,     0,   148,     0,
    71,     0,    70,     0,    72,     0,   150,    71,     0,   150,
    70,     0,   150,    72,     0,     0,   150,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   396,   402,   408,   410,   414,   416,   418,   420,   424,   426,
   429,   436,   438,   446,   448,   452,   459,   464,   472,   474,
   476,   484,   489,   496,   498,   500,   504,   506,   508,   510,
   512,   516,   518,   520,   527,   529,   533,   535,   537,   539,
   541,   543,   545,   547,   549,   551,   553,   555,   557,   559,
   561,   565,   570,   577,   582,   586,   590,   592,   594,   598,
   602,   609,   611,   617,   619,   628,   629,   637,   639,   641,
   643,   645,   647,   652,   657,   662,   667,   673,   678,   683,
   690,   697,   699,   706,   708,   715,   719,   723,   730,   732,
   739,   741,   748,   752,   756,   760,   762,   773,   780,   782,
   784,   786,   791,   793,   795,   797,   799,   801,   803,   805,
   807,   809,   811,   813,   815,   820,   822,   824,   826,   828,
   830,   832,   834,   836,   838,   840,   842,   844,   846,   848,
   850,   852,   854,   856,   858,   860,   862,   864,   866,   868,
   870,   874,   879,   886,   893,   898,   908,   912,   916,   920,
   924,   928,   934,   942,   944,   948,   951,   956,   963,   968,
   976,   978,   984,   991,   998,  1000,  1004,  1012,  1019,  1025,
  1030,  1032,  1035,  1037,  1039,  1041,  1050,  1054,  1059,  1066,
  1073,  1083,  1085,  1090,  1096,  1104,  1111,  1117,  1119,  1125,
  1132,  1137,  1146,  1150,  1152,  1156,  1158,  1165,  1167,  1171,
  1173,  1180,  1182,  1184,  1186,  1190,  1192,  1196,  1198,  1200,
  1202,  1204,  1206,  1210,  1212
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","'='","':'",
"'-'","'+'","'*'","'/'","EXPR_AND_AND","EXPR_OR_OR","EXPR_AND","EXPR_OR","EXPR_NOT",
"EXPR_LT","EXPR_LE","EXPR_EQ","EXPR_NE","EXPR_GE","EXPR_GT","LEFTDIV","EMUL",
"EDIV","ELEFTDIV","EPLUS","EMINUS","QUOTE","TRANSPOSE","PLUS_PLUS","MINUS_MINUS",
"POW","EPOW","NUM","IMAG_NUM","NAME","SCREW","END","PLOT","TEXT","STYLE","FOR",
"WHILE","IF","ELSEIF","ELSE","SWITCH","CASE","OTHERWISE","BREAK","CONTINUE",
"FUNC_RET","UNWIND","CLEANUP","TRY","CATCH","GLOBAL","TEXT_ID","LEXICAL_ERROR",
"FCN","SCREW_TWO","ELLIPSIS","ALL_VA_ARGS","END_OF_INPUT","USING","TITLE","WITH",
"COLON","OPEN_BRACE","CLOSE_BRACE","CLEAR","';'","','","'\\n'","UNARY","'['",
"'('","')'","']'","'.'","input","input1","parse_error","simple_list","simple_list1",
"opt_list","list","list1","statement","plot_command","ranges","ranges1","plot_command1",
"plot_command2","plot_options","using","using1","title","style","ans_expression",
"global_decl","global_decl1","global_decl2","optcomma","command","if_command",
"if_cmd_list","if_cmd_list1","elseif_clause","else_clause","switch_command",
"case_list","case_list1","switch_case","default_case","screwed_again","expression",
"simple_expr","simple_expr1","colon_expr","word_list_cmd","word_list","g_symtab",
"in_return_list","local_symtab","safe","are_we_screwed","func_def","func_def1",
"return_list_x","return_list","return_list1","func_def2","func_def3","fcn_end_or_eof",
"indirect_ref","indirect_ref1","@1","variable","in_param_list","param_list",
"param_list1","identifier","arg_list","matrix","rows","rows1","matrix_row","matrix_row1",
"sep_no_nl","opt_sep_no_nl","sep","opt_sep", NULL
};
#endif

static const short yyr1[] = {     0,
    79,    79,    79,    79,    80,    80,    80,    80,    81,    81,
    82,    83,    83,    84,    84,    85,    86,    86,    87,    87,
    87,    88,    88,    89,    89,    89,    90,    90,    90,    90,
    90,    91,    91,    91,    92,    92,    93,    93,    93,    93,
    93,    93,    93,    93,    93,    93,    93,    93,    93,    93,
    93,    94,    94,    95,    95,    96,    97,    97,    97,    98,
    99,   100,   100,   101,   101,   102,   102,   103,   103,   103,
   103,   103,   103,   103,   103,   103,   103,   103,   103,   103,
   104,   105,   105,   106,   106,   107,   108,   109,   110,   110,
   111,   111,   112,   113,   114,   115,   115,   116,   117,   117,
   117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   117,   118,   118,   119,   120,   120,   121,   122,   123,   124,
   125,   126,   126,   127,   127,   128,   129,   129,   129,   129,
   130,   130,   130,   131,   132,   132,   133,   133,   134,   135,
   136,   135,   137,   137,   137,   137,   138,   139,   139,   139,
   139,   140,   140,   140,   140,   141,   142,   142,   142,   142,
   142,   142,   143,   144,   144,   145,   145,   146,   146,   147,
   147,   148,   148,   148,   148,   149,   149,   150,   150,   150,
   150,   150,   150,   151,   151
};

static const short yyr2[] = {     0,
     1,     1,     2,     1,     1,     1,     2,     2,     1,     1,
     2,     1,     3,     0,     1,     2,     1,     3,     1,     1,
     2,     2,     3,     1,     2,     3,     5,     4,     4,     3,
     2,     0,     1,     3,     1,     2,     1,     1,     1,     2,
     2,     2,     2,     2,     2,     3,     3,     3,     3,     3,
     3,     1,     2,     2,     3,     2,     2,     3,     4,     1,
     2,     1,     3,     1,     3,     0,     1,     1,     1,     1,
     1,     1,     7,     7,     5,     7,    10,     1,     1,     1,
     3,     1,     2,     3,     2,     5,     3,     5,     1,     2,
     1,     2,     5,     3,     0,     1,     3,     1,     1,     1,
     1,     3,     1,     1,     1,     1,     2,     3,     2,     2,
     2,     2,     2,     3,     6,     2,     2,     2,     2,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     2,     1,     2,     0,     0,     0,     0,
     0,     4,     4,     5,     4,     4,     2,     3,     2,     4,
     2,     2,     3,     4,     4,     3,     1,     1,     1,     1,
     0,     4,     1,     3,     4,     2,     0,     2,     3,     2,
     4,     3,     3,     2,     3,     1,     1,     1,     1,     3,
     3,     3,     4,     1,     2,     1,     3,     1,     2,     1,
     3,     1,     1,     2,     2,     0,     1,     1,     1,     1,
     2,     2,     2,     0,     1
};

static const short yydefact[] = {     0,
    10,     0,     0,     0,     0,     0,    99,   100,   186,    32,
   101,     0,     0,     0,     0,    78,    79,    80,   214,   214,
     0,     9,   147,     2,     5,    95,     0,     1,     4,     0,
   206,    12,    68,    20,    70,    19,    72,    71,    60,    96,
    98,   105,   103,    69,   173,   169,   104,   170,   106,    99,
   113,   112,   111,   109,   110,     0,     0,    21,    32,    24,
    22,    33,    35,    95,     0,   170,   214,     0,    82,   214,
   214,   209,   208,   210,   215,    14,    14,    61,    62,    64,
   151,     0,   107,     0,     0,     8,     7,     3,   203,   202,
   207,    11,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   118,   119,     0,     0,     0,   176,     0,
   171,     0,   116,   117,   145,   144,    97,     0,    31,     0,
    23,    25,     0,     0,     0,     0,    36,    37,    52,    38,
    39,     0,     0,    14,    81,   214,   214,    85,    83,    14,
     0,   212,   211,   213,     0,    15,   214,    17,     0,    67,
     0,     0,     0,   108,   200,     0,   194,   196,   198,   102,
   205,   204,    13,   142,   123,   122,   124,   125,   140,   141,
   138,   139,   132,   133,   134,   137,   135,   136,   130,   128,
   129,   131,   126,   127,   120,   121,   143,   187,   189,   174,
   188,     0,     0,   114,   146,    30,     0,     0,    26,    34,
    54,    56,    57,    40,    42,     0,    53,    41,    44,    43,
    45,     0,   214,     0,     0,    14,    84,   214,     0,    89,
    91,   214,   215,    16,   214,    63,    65,   150,   150,   152,
     0,   147,     0,   153,   150,   193,   195,     0,   199,     0,
   175,   172,    28,    29,     0,    58,    46,    47,    55,    48,
    49,    50,    51,     0,    14,    75,   214,    87,     0,    88,
   214,    92,    90,    14,    18,    14,   147,   149,   162,     0,
   157,   161,     0,     0,   159,   149,   197,     0,   201,   190,
   192,   191,    27,    59,     0,     0,    14,   214,    14,     0,
     0,     0,   148,   158,     0,     0,   163,   214,   115,   214,
    76,    86,     0,    94,    73,    74,     0,   156,   155,   160,
     0,   164,   214,     0,    14,    14,    93,   154,   184,     0,
   178,     0,    14,     0,   180,     0,     0,   179,   182,     0,
   185,     0,   183,   167,   168,   166,    77,   165,   181,     0,
     0,     0
};

static const short yydefgoto[] = {   350,
    28,    29,    30,    31,   155,   156,   157,   158,    33,    59,
    60,    61,    62,   137,   138,   139,   140,   141,    34,    35,
    78,    79,   161,    36,    37,    68,    69,   148,   149,    38,
   229,   230,   231,   273,    84,    39,    40,    41,    42,    43,
   126,    81,   318,   303,   277,   163,    44,   240,   241,   242,
   243,   244,   322,   346,    45,    46,   203,    47,   332,   323,
   324,    48,   202,    49,   166,   167,   168,   169,    91,    92,
    75,    76
};

static const short yypact[] = {   302,
-32768,   606,   606,   606,    16,    16,    58,-32768,-32768,   576,
-32768,   -16,   636,   636,   636,-32768,-32768,-32768,    54,    54,
    16,-32768,-32768,-32768,-32768,   -37,   606,-32768,-32768,    75,
   -14,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   678,
-32768,    70,-32768,-32768,     3,    10,   118,     6,-32768,-32768,
   124,   124,   124,-32768,-32768,   636,   196,-32768,   636,    63,
    65,-32768,    80,-32768,   131,-32768,    54,   102,   116,    54,
    54,-32768,-32768,-32768,    81,   525,   525,   -11,-32768,   145,
-32768,    90,-32768,   636,   166,-32768,-32768,-32768,-32768,-32768,
   474,-32768,   606,   606,   606,   606,   606,   606,   606,   606,
   606,   606,   606,   606,   606,   606,   606,   606,   606,   606,
   606,   606,   606,-32768,-32768,   606,   606,   606,-32768,   357,
-32768,   606,-32768,-32768,-32768,   141,-32768,   409,-32768,    91,
    65,    63,   636,   636,   636,   155,-32768,    97,   392,    68,
   100,   636,   636,   525,-32768,    54,    54,-32768,-32768,   525,
   149,-32768,-32768,-32768,   146,-32768,    54,-32768,   150,-32768,
    16,   636,    -4,-32768,-32768,   122,   130,   144,   135,-32768,
-32768,-32768,-32768,   791,   812,   812,    53,    53,   708,   708,
   736,   736,   764,   764,   764,   764,   764,   764,    53,    53,
    53,    53,   812,   812,   124,   124,   791,-32768,-32768,-32768,
-32768,   -44,   152,   678,-32768,-32768,   143,   588,-32768,-32768,
-32768,-32768,   636,   147,   157,   636,-32768,   147,   156,   157,
   156,   154,    54,   186,   636,   525,-32768,    54,   190,   119,
-32768,    54,   423,-32768,    54,-32768,-32768,-32768,-32768,-32768,
     5,-32768,   -34,-32768,-32768,-32768,   636,   224,   636,   374,
-32768,-32768,-32768,-32768,   163,   636,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,   230,   525,-32768,    54,-32768,   636,-32768,
    54,-32768,-32768,   525,-32768,   525,-32768,-32768,-32768,   158,
-32768,-32768,   234,     2,-32768,-32768,-32768,   606,-32768,-32768,
-32768,-32768,-32768,-32768,   636,   202,   525,    54,   525,   204,
   207,   241,-32768,-32768,    16,   168,-32768,    -3,   678,    54,
-32768,-32768,   525,-32768,-32768,-32768,    16,-32768,-32768,-32768,
    11,-32768,    54,   -25,   525,   525,-32768,-32768,-32768,   170,
-32768,    16,   525,    13,-32768,    -7,   211,-32768,-32768,    -7,
-32768,   172,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   250,
   251,-32768
};

static const short yypgoto[] = {-32768,
-32768,   222,-32768,-32768,   -58,   -54,-32768,    25,-32768,-32768,
   -51,   201,   128,-32768,   -99,-32768,  -130,  -125,-32768,-32768,
-32768,    94,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    35,-32768,   205,     7,     1,-32768,-32768,-32768,
-32768,  -218,-32768,   -20,  -191,-32768,-32768,-32768,-32768,-32768,
-32768,  -279,-32768,   -68,-32768,-32768,-32768,   261,-32768,-32768,
-32768,    -5,-32768,-32768,-32768,-32768,  -132,-32768,-32768,-32768,
   117,   -18
};


#define	YYLAST		843


static const short yytable[] = {    54,
    55,    77,    51,    52,    53,   279,    66,   214,   132,   222,
   221,   329,   215,   341,   219,    80,    63,     9,   159,    67,
    70,    71,   -66,   283,    32,   319,   250,    85,   344,     9,
   238,   251,    82,   123,   124,     9,   284,   328,     9,    83,
   218,   220,   285,   125,  -177,   334,     9,   278,   144,     9,
   335,   150,   151,   286,   345,    89,    90,    64,   302,   160,
    56,   306,   127,   130,   280,    63,    72,    73,    74,   239,
   330,   321,   342,   118,    -6,     1,   119,   120,   114,   115,
   209,   281,   116,   117,   258,   224,   331,   121,   257,   262,
   165,   227,   260,   174,   175,   176,   177,   178,   179,   180,
   181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
   191,   192,   193,   194,   287,   173,   195,   196,   197,   261,
   122,   263,   204,    72,    73,    74,   201,   225,   226,    57,
   134,    22,   136,   143,   207,   133,    86,   145,   234,    63,
   211,   212,   134,   135,   136,   217,    87,   162,   165,   223,
   152,   153,   154,   116,   117,    80,   208,   245,   146,   147,
   135,   136,   134,   135,   228,   271,   164,   268,   237,    93,
    94,    95,    96,    97,    98,    99,   100,   101,   205,   102,
   103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
   113,   114,   115,   213,   228,   116,   117,   232,   246,   247,
     2,     3,   248,   235,   265,   249,   296,   252,     4,   269,
   253,   136,   264,   274,   255,   300,   276,   301,   134,   256,
   135,   266,   259,     5,     6,   270,   288,     7,     8,     9,
   293,   267,   295,    11,   304,   282,   305,   311,   312,   315,
   314,   170,   316,   317,   320,   338,   347,   349,   297,   351,
   352,    88,   299,   165,   236,   289,   292,   275,   327,   131,
   210,   128,   294,   129,   272,   308,   336,   337,   142,    26,
    27,   348,    65,   233,   340,   298,     0,     0,   307,   313,
     0,     0,     0,     0,     0,     0,     0,     0,   309,   325,
     0,   326,     0,     0,     0,     0,     0,     0,     0,   245,
     0,   310,     1,     0,   333,     0,     2,     3,     0,     0,
     0,   245,     0,     0,     4,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   339,     0,   343,     5,
     6,     0,     0,     7,     8,     9,     0,     0,    10,    11,
     0,    12,    13,    14,     0,     0,    15,     0,     0,    16,
    17,    18,    19,     0,    20,     0,    21,     0,    22,    23,
   198,     2,     3,    24,     0,     0,     0,     0,     0,     4,
     0,     0,     0,    25,     0,    26,    27,   290,     2,     3,
     0,     0,     0,     0,     5,     6,     4,     0,     7,     8,
     9,     0,     0,     0,    11,     0,     2,     3,     0,     0,
     0,     5,     6,     0,     4,     7,     8,     9,     0,     0,
     0,    11,     0,     2,     3,     0,     0,   199,     0,     5,
     6,     4,     0,     7,     8,     9,     0,     2,     3,    11,
    26,    27,   200,     0,   291,     4,     5,     6,     0,     0,
     7,     8,     9,     0,     0,     0,    11,    26,    27,     0,
     5,     6,     0,     0,     7,     8,     9,   216,     0,    10,
    11,     0,    12,    13,    14,    26,    27,    15,     0,     0,
    16,    17,    18,    19,     0,    20,   206,    21,     2,     3,
    23,     0,    26,    27,     0,     0,     4,     0,     0,     0,
     0,     0,   152,   153,   154,     0,    26,    27,     0,     0,
     0,     5,     6,     0,     0,     7,     8,     9,     0,     0,
    10,    11,     0,    12,    13,    14,     0,     0,    15,     0,
     0,    16,    17,    18,    19,     0,    20,     0,    21,     2,
     3,    23,     0,     0,     0,     0,     0,     4,     0,     0,
     0,     0,     0,   171,   172,     0,     0,    26,    27,     0,
     0,     0,     5,     6,     0,     0,     7,     8,     9,     0,
     0,    10,    11,     0,    12,    13,    14,     0,     0,    15,
     0,     0,    16,    17,    18,    19,     0,    20,     0,    21,
     2,     3,    23,     0,     0,     0,     0,     0,     4,     0,
     0,     0,     2,     3,     0,     0,     0,     0,    26,    27,
     4,     0,     0,     5,     6,     0,     0,     7,     8,     9,
     2,     3,     0,    11,     0,     5,     6,     0,     4,     7,
     8,     9,     0,     0,     0,    11,     0,     0,     0,     0,
     0,     0,     0,     5,     6,     0,     0,    50,     8,     9,
     2,     3,    57,    11,    58,     0,     0,     0,     4,    26,
    27,     0,     0,     0,     0,   254,     0,     0,     0,     0,
     0,    26,    27,     5,     6,     0,     0,     7,     8,     9,
     0,     0,     0,    11,     0,     0,     0,     0,     0,    26,
    27,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     0,   102,   103,   104,   105,   106,   107,   108,   109,   110,
   111,   112,   113,   114,   115,     0,     0,   116,   117,    26,
    27,    93,    94,    95,    96,    97,     0,     0,   100,   101,
     0,   102,   103,   104,   105,   106,   107,   108,   109,   110,
   111,   112,   113,   114,   115,     0,     0,   116,   117,    93,
    94,    95,    96,    97,     0,     0,     0,     0,     0,   102,
   103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
   113,   114,   115,     0,     0,   116,   117,    93,    94,    95,
    96,    97,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   108,   109,   110,   111,   112,   113,   114,
   115,     0,     0,   116,   117,    94,    95,    96,    97,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   108,   109,   110,   111,   112,   113,   114,   115,    96,    97,
   116,   117,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   108,   109,   110,   111,     0,     0,   114,   115,     0,
     0,   116,   117
};

static const short yycheck[] = {     5,
     6,    20,     2,     3,     4,     1,    12,   138,    60,   142,
   141,     1,   138,     1,   140,    21,    10,    34,    77,    13,
    14,    15,    34,   242,     0,   305,    71,    27,    36,    34,
    35,    76,    70,    28,    29,    34,    71,   317,    34,    77,
   140,   141,    77,    38,    34,    71,    34,   239,    67,    34,
    76,    70,    71,   245,    62,    70,    71,    74,   277,    71,
     3,    60,    56,    57,    60,    59,    70,    71,    72,    74,
    60,    75,    60,     4,     0,     1,    74,    75,    26,    27,
   132,    77,    30,    31,   215,   144,    76,    78,   214,   220,
    84,   150,   218,    93,    94,    95,    96,    97,    98,    99,
   100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
   110,   111,   112,   113,   247,    91,   116,   117,   118,   219,
     3,   221,   122,    70,    71,    72,   120,   146,   147,    67,
    63,    57,    65,     3,   128,    71,    62,    36,   157,   133,
   134,   135,    63,    64,    65,   139,    72,     3,   142,   143,
    70,    71,    72,    30,    31,   161,    66,   163,    43,    44,
    64,    65,    63,    64,    46,    47,    77,   226,   162,     4,
     5,     6,     7,     8,     9,    10,    11,    12,    38,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    39,    46,    30,    31,    52,    77,    70,
     5,     6,    59,    54,   223,    71,   265,    56,    13,   228,
    68,    65,    59,   232,   208,   274,   235,   276,    63,   213,
    64,    36,   216,    28,    29,    36,     3,    32,    33,    34,
    68,   225,     3,    38,    77,   241,     3,    36,   297,    36,
   299,    76,    36,     3,    77,    76,    36,    76,   267,     0,
     0,    30,   271,   247,   161,   249,   250,   233,   313,    59,
   133,    66,   256,    68,   230,   286,   325,   326,    64,    74,
    75,   340,    12,   157,   333,   269,    -1,    -1,   284,   298,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   288,   308,
    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   305,
    -1,   295,     1,    -1,   323,    -1,     5,     6,    -1,    -1,
    -1,   317,    -1,    -1,    13,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   332,    -1,   334,    28,
    29,    -1,    -1,    32,    33,    34,    -1,    -1,    37,    38,
    -1,    40,    41,    42,    -1,    -1,    45,    -1,    -1,    48,
    49,    50,    51,    -1,    53,    -1,    55,    -1,    57,    58,
     4,     5,     6,    62,    -1,    -1,    -1,    -1,    -1,    13,
    -1,    -1,    -1,    72,    -1,    74,    75,     4,     5,     6,
    -1,    -1,    -1,    -1,    28,    29,    13,    -1,    32,    33,
    34,    -1,    -1,    -1,    38,    -1,     5,     6,    -1,    -1,
    -1,    28,    29,    -1,    13,    32,    33,    34,    -1,    -1,
    -1,    38,    -1,     5,     6,    -1,    -1,    61,    -1,    28,
    29,    13,    -1,    32,    33,    34,    -1,     5,     6,    38,
    74,    75,    76,    -1,    61,    13,    28,    29,    -1,    -1,
    32,    33,    34,    -1,    -1,    -1,    38,    74,    75,    -1,
    28,    29,    -1,    -1,    32,    33,    34,    66,    -1,    37,
    38,    -1,    40,    41,    42,    74,    75,    45,    -1,    -1,
    48,    49,    50,    51,    -1,    53,    68,    55,     5,     6,
    58,    -1,    74,    75,    -1,    -1,    13,    -1,    -1,    -1,
    -1,    -1,    70,    71,    72,    -1,    74,    75,    -1,    -1,
    -1,    28,    29,    -1,    -1,    32,    33,    34,    -1,    -1,
    37,    38,    -1,    40,    41,    42,    -1,    -1,    45,    -1,
    -1,    48,    49,    50,    51,    -1,    53,    -1,    55,     5,
     6,    58,    -1,    -1,    -1,    -1,    -1,    13,    -1,    -1,
    -1,    -1,    -1,    70,    71,    -1,    -1,    74,    75,    -1,
    -1,    -1,    28,    29,    -1,    -1,    32,    33,    34,    -1,
    -1,    37,    38,    -1,    40,    41,    42,    -1,    -1,    45,
    -1,    -1,    48,    49,    50,    51,    -1,    53,    -1,    55,
     5,     6,    58,    -1,    -1,    -1,    -1,    -1,    13,    -1,
    -1,    -1,     5,     6,    -1,    -1,    -1,    -1,    74,    75,
    13,    -1,    -1,    28,    29,    -1,    -1,    32,    33,    34,
     5,     6,    -1,    38,    -1,    28,    29,    -1,    13,    32,
    33,    34,    -1,    -1,    -1,    38,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    28,    29,    -1,    -1,    32,    33,    34,
     5,     6,    67,    38,    69,    -1,    -1,    -1,    13,    74,
    75,    -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
    -1,    74,    75,    28,    29,    -1,    -1,    32,    33,    34,
    -1,    -1,    -1,    38,    -1,    -1,    -1,    -1,    -1,    74,
    75,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    -1,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    -1,    -1,    30,    31,    74,
    75,     4,     5,     6,     7,     8,    -1,    -1,    11,    12,
    -1,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    -1,    -1,    30,    31,     4,
     5,     6,     7,     8,    -1,    -1,    -1,    -1,    -1,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    -1,    -1,    30,    31,     4,     5,     6,
     7,     8,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    20,    21,    22,    23,    24,    25,    26,
    27,    -1,    -1,    30,    31,     5,     6,     7,     8,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    20,    21,    22,    23,    24,    25,    26,    27,     7,     8,
    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    20,    21,    22,    23,    -1,    -1,    26,    27,    -1,
    -1,    30,    31
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/local/share/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 196 "/usr/local/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 397 "parse.y"
{
		    global_command = yyvsp[0].tree_statement_list_type;
		    promptflag = 1;
		    YYACCEPT;
		  ;
    break;}
case 2:
#line 403 "parse.y"
{
		    global_command = 0;
		    promptflag = 1;
		    YYABORT;
		  ;
    break;}
case 3:
#line 409 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 4:
#line 411 "parse.y"
{ ABORT_PARSE; ;
    break;}
case 5:
#line 415 "parse.y"
{ yyval.tree_statement_list_type = 0; ;
    break;}
case 6:
#line 417 "parse.y"
{ yyval.tree_statement_list_type = yyvsp[0].tree_statement_list_type; ;
    break;}
case 7:
#line 419 "parse.y"
{ yyval.tree_statement_list_type = yyvsp[-1].tree_statement_list_type; ;
    break;}
case 8:
#line 421 "parse.y"
{ yyval.tree_statement_list_type = yyvsp[-1].tree_statement_list_type; ;
    break;}
case 9:
#line 425 "parse.y"
{ yyerror ("parse error"); ;
    break;}
case 11:
#line 430 "parse.y"
{
		    set_stmt_print_flag (yyvsp[-1].tree_statement_list_type, yyvsp[0].sep_type, false);
		    yyval.tree_statement_list_type = yyvsp[-1].tree_statement_list_type;
		  ;
    break;}
case 12:
#line 437 "parse.y"
{ yyval.tree_statement_list_type = new tree_statement_list (yyvsp[0].tree_statement_type); ;
    break;}
case 13:
#line 439 "parse.y"
{
		    set_stmt_print_flag (yyvsp[-2].tree_statement_list_type, yyvsp[-1].sep_type, false);
		    yyvsp[-2].tree_statement_list_type->append (yyvsp[0].tree_statement_type);
		    yyval.tree_statement_list_type = yyvsp[-2].tree_statement_list_type;
		  ;
    break;}
case 14:
#line 447 "parse.y"
{ yyval.tree_statement_list_type = new tree_statement_list (); ;
    break;}
case 15:
#line 449 "parse.y"
{ yyval.tree_statement_list_type = yyvsp[0].tree_statement_list_type; ;
    break;}
case 16:
#line 453 "parse.y"
{
		    set_stmt_print_flag (yyvsp[-1].tree_statement_list_type, yyvsp[0].sep_type, true);
		    yyval.tree_statement_list_type = yyvsp[-1].tree_statement_list_type;
		  ;
    break;}
case 17:
#line 460 "parse.y"
{
		    lexer_flags.beginning_of_function = 0;
		    yyval.tree_statement_list_type = new tree_statement_list (yyvsp[0].tree_statement_type);
		  ;
    break;}
case 18:
#line 465 "parse.y"
{
		    set_stmt_print_flag (yyvsp[-2].tree_statement_list_type, yyvsp[-1].sep_type, true);
		    yyvsp[-2].tree_statement_list_type->append (yyvsp[0].tree_statement_type);
		    yyval.tree_statement_list_type = yyvsp[-2].tree_statement_list_type;
		  ;
    break;}
case 19:
#line 473 "parse.y"
{ yyval.tree_statement_type = new tree_statement (yyvsp[0].tree_command_type); ;
    break;}
case 20:
#line 475 "parse.y"
{ yyval.tree_statement_type = new tree_statement (yyvsp[0].tree_expression_type); ;
    break;}
case 21:
#line 477 "parse.y"
{
		    symbol_record *sr = lookup_by_name ("clearplot", 0);
		    tree_identifier *id = new tree_identifier (sr);
		    yyval.tree_statement_type = new tree_statement (id);
		  ;
    break;}
case 22:
#line 485 "parse.y"
{
		    if (! (yyval.tree_plot_command_type = make_plot_command (yyvsp[-1].tok_val, 0, yyvsp[0].subplot_list_type)))
		      ABORT_PARSE;
		  ;
    break;}
case 23:
#line 490 "parse.y"
{
		    if (! (yyval.tree_plot_command_type = make_plot_command (yyvsp[-2].tok_val, yyvsp[-1].plot_limits_type, yyvsp[0].subplot_list_type)))
		      ABORT_PARSE;
		  ;
    break;}
case 24:
#line 497 "parse.y"
{ yyval.plot_limits_type = new plot_limits (yyvsp[0].plot_range_type); ;
    break;}
case 25:
#line 499 "parse.y"
{ yyval.plot_limits_type = new plot_limits (yyvsp[-1].plot_range_type, yyvsp[0].plot_range_type); ;
    break;}
case 26:
#line 501 "parse.y"
{ yyval.plot_limits_type = new plot_limits (yyvsp[-2].plot_range_type, yyvsp[-1].plot_range_type, yyvsp[0].plot_range_type); ;
    break;}
case 27:
#line 505 "parse.y"
{ yyval.plot_range_type = new plot_range (yyvsp[-3].tree_expression_type, yyvsp[-1].tree_expression_type); ;
    break;}
case 28:
#line 507 "parse.y"
{ yyval.plot_range_type = new plot_range (0, yyvsp[-1].tree_expression_type); ;
    break;}
case 29:
#line 509 "parse.y"
{ yyval.plot_range_type = new plot_range (yyvsp[-2].tree_expression_type, 0); ;
    break;}
case 30:
#line 511 "parse.y"
{ yyval.plot_range_type = new plot_range (); ;
    break;}
case 31:
#line 513 "parse.y"
{ yyval.plot_range_type = new plot_range (); ;
    break;}
case 32:
#line 517 "parse.y"
{ yyval.subplot_list_type = 0; ;
    break;}
case 33:
#line 519 "parse.y"
{ yyval.subplot_list_type = new subplot_list (yyvsp[0].subplot_type); ;
    break;}
case 34:
#line 521 "parse.y"
{
		    yyvsp[-2].subplot_list_type->append (yyvsp[0].subplot_type);
		    yyval.subplot_list_type = yyvsp[-2].subplot_list_type;
		  ;
    break;}
case 35:
#line 528 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[0].tree_expression_type); ;
    break;}
case 36:
#line 530 "parse.y"
{ yyval.subplot_type = yyvsp[0].subplot_type->set_data (yyvsp[-1].tree_expression_type); ;
    break;}
case 37:
#line 534 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[0].subplot_using_type, 0, 0); ;
    break;}
case 38:
#line 536 "parse.y"
{ yyval.subplot_type = new subplot (0, yyvsp[0].tree_expression_type, 0); ;
    break;}
case 39:
#line 538 "parse.y"
{ yyval.subplot_type = new subplot (0, 0, yyvsp[0].subplot_style_type); ;
    break;}
case 40:
#line 540 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[-1].subplot_using_type, yyvsp[0].tree_expression_type, 0); ;
    break;}
case 41:
#line 542 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[0].subplot_using_type, yyvsp[-1].tree_expression_type, 0); ;
    break;}
case 42:
#line 544 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[-1].subplot_using_type, 0, yyvsp[0].subplot_style_type); ;
    break;}
case 43:
#line 546 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[0].subplot_using_type, 0, yyvsp[-1].subplot_style_type); ;
    break;}
case 44:
#line 548 "parse.y"
{ yyval.subplot_type = new subplot (0, yyvsp[-1].tree_expression_type, yyvsp[0].subplot_style_type); ;
    break;}
case 45:
#line 550 "parse.y"
{ yyval.subplot_type = new subplot (0, yyvsp[0].tree_expression_type, yyvsp[-1].subplot_style_type); ;
    break;}
case 46:
#line 552 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[-2].subplot_using_type, yyvsp[-1].tree_expression_type, yyvsp[0].subplot_style_type); ;
    break;}
case 47:
#line 554 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[-2].subplot_using_type, yyvsp[0].tree_expression_type, yyvsp[-1].subplot_style_type); ;
    break;}
case 48:
#line 556 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[-1].subplot_using_type, yyvsp[-2].tree_expression_type, yyvsp[0].subplot_style_type); ;
    break;}
case 49:
#line 558 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[0].subplot_using_type, yyvsp[-2].tree_expression_type, yyvsp[-1].subplot_style_type); ;
    break;}
case 50:
#line 560 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[-1].subplot_using_type, yyvsp[0].tree_expression_type, yyvsp[-2].subplot_style_type); ;
    break;}
case 51:
#line 562 "parse.y"
{ yyval.subplot_type = new subplot (yyvsp[0].subplot_using_type, yyvsp[-1].tree_expression_type, yyvsp[-2].subplot_style_type); ;
    break;}
case 52:
#line 566 "parse.y"
{
		    lexer_flags.in_plot_using = 0;
		    yyval.subplot_using_type = yyvsp[0].subplot_using_type;
		  ;
    break;}
case 53:
#line 571 "parse.y"
{
		    lexer_flags.in_plot_using = 0;
		    yyval.subplot_using_type = yyvsp[-1].subplot_using_type->set_format (yyvsp[0].tree_expression_type);
		  ;
    break;}
case 54:
#line 578 "parse.y"
{
		    subplot_using *tmp = new subplot_using ();
		    yyval.subplot_using_type = tmp->add_qualifier (yyvsp[0].tree_expression_type);
		  ;
    break;}
case 55:
#line 583 "parse.y"
{ yyval.subplot_using_type = yyvsp[-2].subplot_using_type->add_qualifier (yyvsp[0].tree_expression_type); ;
    break;}
case 56:
#line 587 "parse.y"
{ yyval.tree_expression_type = yyvsp[0].tree_expression_type; ;
    break;}
case 57:
#line 591 "parse.y"
{ yyval.subplot_style_type = new subplot_style (yyvsp[0].tok_val->text ()); ;
    break;}
case 58:
#line 593 "parse.y"
{ yyval.subplot_style_type = new subplot_style (yyvsp[-1].tok_val->text (), yyvsp[0].tree_expression_type); ;
    break;}
case 59:
#line 595 "parse.y"
{ yyval.subplot_style_type = new subplot_style (yyvsp[-2].tok_val->text (), yyvsp[-1].tree_expression_type, yyvsp[0].tree_expression_type); ;
    break;}
case 60:
#line 599 "parse.y"
{ yyval.tree_expression_type = maybe_convert_to_ans_assign (yyvsp[0].tree_expression_type); ;
    break;}
case 61:
#line 603 "parse.y"
{
		    yyval.tree_global_command_type = new tree_global_command (yyvsp[0].tree_global_init_list_type, yyvsp[-1].tok_val->line (),
						  yyvsp[-1].tok_val->column ());
		  ;
    break;}
case 62:
#line 610 "parse.y"
{ yyval.tree_global_init_list_type = new tree_global_init_list (yyvsp[0].tree_global_type); ;
    break;}
case 63:
#line 612 "parse.y"
{
		    yyvsp[-2].tree_global_init_list_type->append (yyvsp[0].tree_global_type);
		    yyval.tree_global_init_list_type = yyvsp[-2].tree_global_init_list_type;
		  ;
    break;}
case 64:
#line 618 "parse.y"
{ yyval.tree_global_type = new tree_global (yyvsp[0].tree_identifier_type); ;
    break;}
case 65:
#line 620 "parse.y"
{
		    tree_simple_assignment_expression *tmp_ass;
		    tmp_ass = new tree_simple_assignment_expression
		      (yyvsp[-2].tree_identifier_type, yyvsp[0].tree_expression_type, 0, 0, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ());
		    yyval.tree_global_type = new tree_global (tmp_ass);
		  ;
    break;}
case 67:
#line 630 "parse.y"
{
		    if (Vwarn_comma_in_global_decl)
		      warning ("comma in global declaration not\
 interpreted as a command separator");
		  ;
    break;}
case 68:
#line 638 "parse.y"
{ yyval.tree_command_type = yyvsp[0].tree_plot_command_type; ;
    break;}
case 69:
#line 640 "parse.y"
{ yyval.tree_command_type = yyvsp[0].tree_command_type; ;
    break;}
case 70:
#line 642 "parse.y"
{ yyval.tree_command_type = yyvsp[0].tree_global_command_type; ;
    break;}
case 71:
#line 644 "parse.y"
{ yyval.tree_command_type = yyvsp[0].tree_switch_command_type; ;
    break;}
case 72:
#line 646 "parse.y"
{ yyval.tree_command_type = yyvsp[0].tree_if_command_type; ;
    break;}
case 73:
#line 648 "parse.y"
{
		    if (! (yyval.tree_command_type = make_unwind_command (yyvsp[-6].tok_val, yyvsp[-4].tree_statement_list_type, yyvsp[-1].tree_statement_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 74:
#line 653 "parse.y"
{
		    if (! (yyval.tree_command_type = make_try_command (yyvsp[-6].tok_val, yyvsp[-4].tree_statement_list_type, yyvsp[-1].tree_statement_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 75:
#line 658 "parse.y"
{
		    if (! (yyval.tree_command_type = make_while_command (yyvsp[-4].tok_val, yyvsp[-3].tree_expression_type, yyvsp[-1].tree_statement_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 76:
#line 663 "parse.y"
{
		    if (! (yyval.tree_command_type = make_for_command (yyvsp[-6].tok_val, yyvsp[-5].tree_index_expression_type, yyvsp[-3].tree_expression_type, yyvsp[-1].tree_statement_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 77:
#line 669 "parse.y"
{
		    if (! (yyval.tree_command_type = make_for_command (yyvsp[-9].tok_val, yyvsp[-6].tree_matrix_row_type, yyvsp[-3].tree_expression_type, yyvsp[-1].tree_statement_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 78:
#line 674 "parse.y"
{
		    if (! (yyval.tree_command_type = make_break_command (yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 79:
#line 679 "parse.y"
{
		    if (! (yyval.tree_command_type = make_continue_command (yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 80:
#line 684 "parse.y"
{
		    if (! (yyval.tree_command_type = make_return_command (yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 81:
#line 691 "parse.y"
{
		    if (! (yyval.tree_if_command_type = finish_if_command (yyvsp[-2].tok_val, yyvsp[-1].tree_if_command_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 82:
#line 698 "parse.y"
{ yyval.tree_if_command_list_type = yyvsp[0].tree_if_command_list_type; ;
    break;}
case 83:
#line 700 "parse.y"
{
		    yyvsp[-1].tree_if_command_list_type->append (yyvsp[0].tree_if_clause_type);
		    yyval.tree_if_command_list_type = yyvsp[-1].tree_if_command_list_type;
		  ;
    break;}
case 84:
#line 707 "parse.y"
{ yyval.tree_if_command_list_type = start_if_command (yyvsp[-2].tree_expression_type, yyvsp[0].tree_statement_list_type); ;
    break;}
case 85:
#line 709 "parse.y"
{
		    yyvsp[-1].tree_if_command_list_type->append (yyvsp[0].tree_if_clause_type);
		    yyval.tree_if_command_list_type = yyvsp[-1].tree_if_command_list_type;
		  ;
    break;}
case 86:
#line 716 "parse.y"
{ yyval.tree_if_clause_type = make_elseif_clause (yyvsp[-2].tree_expression_type, yyvsp[0].tree_statement_list_type); ;
    break;}
case 87:
#line 720 "parse.y"
{ yyval.tree_if_clause_type = new tree_if_clause (yyvsp[0].tree_statement_list_type); ;
    break;}
case 88:
#line 724 "parse.y"
{
		    if (! (yyval.tree_switch_command_type = finish_switch_command (yyvsp[-4].tok_val, yyvsp[-3].tree_expression_type, yyvsp[-1].tree_switch_case_list_type, yyvsp[0].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 89:
#line 731 "parse.y"
{ yyval.tree_switch_case_list_type = yyvsp[0].tree_switch_case_list_type; ;
    break;}
case 90:
#line 733 "parse.y"
{
		    yyvsp[-1].tree_switch_case_list_type->append (yyvsp[0].tree_switch_case_type);
		    yyval.tree_switch_case_list_type = yyvsp[-1].tree_switch_case_list_type;
		  ;
    break;}
case 91:
#line 740 "parse.y"
{ yyval.tree_switch_case_list_type = new tree_switch_case_list (yyvsp[0].tree_switch_case_type); ;
    break;}
case 92:
#line 742 "parse.y"
{
		    yyvsp[-1].tree_switch_case_list_type->append (yyvsp[0].tree_switch_case_type);
		    yyval.tree_switch_case_list_type = yyvsp[-1].tree_switch_case_list_type;
		  ;
    break;}
case 93:
#line 749 "parse.y"
{ yyval.tree_switch_case_type = make_switch_case (yyvsp[-2].tree_expression_type, yyvsp[0].tree_statement_list_type); ;
    break;}
case 94:
#line 753 "parse.y"
{ yyval.tree_switch_case_type = new tree_switch_case (yyvsp[0].tree_statement_list_type); ;
    break;}
case 95:
#line 757 "parse.y"
{ lexer_flags.maybe_screwed_again++; ;
    break;}
case 96:
#line 761 "parse.y"
{ yyval.tree_expression_type = yyvsp[0].tree_expression_type; ;
    break;}
case 97:
#line 763 "parse.y"
{
		    yyerror ("invalid assignment to a number");
		    yyval.tree_expression_type = 0;
		    ABORT_PARSE;
		  ;
    break;}
case 98:
#line 774 "parse.y"
{
		    if (! (yyval.tree_expression_type = yyvsp[0].tree_expression_type))
		      ABORT_PARSE;
		  ;
    break;}
case 99:
#line 781 "parse.y"
{ yyval.tree_expression_type = make_constant (NUM, yyvsp[0].tok_val); ;
    break;}
case 100:
#line 783 "parse.y"
{ yyval.tree_expression_type = make_constant (IMAG_NUM, yyvsp[0].tok_val); ;
    break;}
case 101:
#line 785 "parse.y"
{ yyval.tree_expression_type = make_constant (TEXT, yyvsp[0].tok_val); ;
    break;}
case 102:
#line 787 "parse.y"
{
		    yyvsp[-1].tree_expression_type->mark_in_parens ();
		    yyval.tree_expression_type = yyvsp[-1].tree_expression_type;
		  ;
    break;}
case 103:
#line 792 "parse.y"
{ yyval.tree_expression_type = yyvsp[0].tree_index_expression_type; ;
    break;}
case 104:
#line 794 "parse.y"
{ yyval.tree_expression_type = yyvsp[0].tree_index_expression_type; ;
    break;}
case 105:
#line 796 "parse.y"
{ yyval.tree_expression_type = finish_colon_expression (yyvsp[0].tree_colon_expression_type); ;
    break;}
case 106:
#line 798 "parse.y"
{ yyval.tree_expression_type = yyvsp[0].tree_expression_type; ;
    break;}
case 107:
#line 800 "parse.y"
{ yyval.tree_expression_type = new tree_constant (Matrix ()); ;
    break;}
case 108:
#line 802 "parse.y"
{ yyval.tree_expression_type = new tree_constant (Matrix ()); ;
    break;}
case 109:
#line 804 "parse.y"
{ yyval.tree_expression_type = make_prefix_op (PLUS_PLUS, yyvsp[0].tree_identifier_type, yyvsp[-1].tok_val); ;
    break;}
case 110:
#line 806 "parse.y"
{ yyval.tree_expression_type = make_prefix_op (MINUS_MINUS, yyvsp[0].tree_identifier_type, yyvsp[-1].tok_val); ;
    break;}
case 111:
#line 808 "parse.y"
{ yyval.tree_expression_type = make_unary_op (EXPR_NOT, yyvsp[0].tree_expression_type, yyvsp[-1].tok_val); ;
    break;}
case 112:
#line 810 "parse.y"
{ yyval.tree_expression_type = yyvsp[0].tree_expression_type; ;
    break;}
case 113:
#line 812 "parse.y"
{ yyval.tree_expression_type = make_unary_op ('-', yyvsp[0].tree_expression_type, yyvsp[-1].tok_val); ;
    break;}
case 114:
#line 814 "parse.y"
{ yyval.tree_expression_type = make_simple_assignment (yyvsp[-2].tree_index_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 115:
#line 816 "parse.y"
{
		    if (! (yyval.tree_expression_type = make_multi_val_ret (yyvsp[-3].tree_matrix_row_type, yyvsp[0].tree_expression_type, yyvsp[-1].tok_val)))
		      ABORT_PARSE;
		  ;
    break;}
case 116:
#line 821 "parse.y"
{ yyval.tree_expression_type = make_postfix_op (PLUS_PLUS, yyvsp[-1].tree_identifier_type, yyvsp[0].tok_val); ;
    break;}
case 117:
#line 823 "parse.y"
{ yyval.tree_expression_type = make_postfix_op (MINUS_MINUS, yyvsp[-1].tree_identifier_type, yyvsp[0].tok_val); ;
    break;}
case 118:
#line 825 "parse.y"
{ yyval.tree_expression_type = make_unary_op (QUOTE, yyvsp[-1].tree_expression_type, yyvsp[0].tok_val); ;
    break;}
case 119:
#line 827 "parse.y"
{ yyval.tree_expression_type = make_unary_op (TRANSPOSE, yyvsp[-1].tree_expression_type, yyvsp[0].tok_val); ;
    break;}
case 120:
#line 829 "parse.y"
{ yyval.tree_expression_type = make_binary_op (POW, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 121:
#line 831 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EPOW, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 122:
#line 833 "parse.y"
{ yyval.tree_expression_type = make_binary_op ('+', yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 123:
#line 835 "parse.y"
{ yyval.tree_expression_type = make_binary_op ('-', yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 124:
#line 837 "parse.y"
{ yyval.tree_expression_type = make_binary_op ('*', yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 125:
#line 839 "parse.y"
{ yyval.tree_expression_type = make_binary_op ('/', yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 126:
#line 841 "parse.y"
{ yyval.tree_expression_type = make_binary_op ('+', yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 127:
#line 843 "parse.y"
{ yyval.tree_expression_type = make_binary_op ('-', yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 128:
#line 845 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EMUL, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 129:
#line 847 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EDIV, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 130:
#line 849 "parse.y"
{ yyval.tree_expression_type = make_binary_op (LEFTDIV, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 131:
#line 851 "parse.y"
{ yyval.tree_expression_type = make_binary_op (ELEFTDIV, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 132:
#line 853 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_LT, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 133:
#line 855 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_LE, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 134:
#line 857 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_EQ, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 135:
#line 859 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_GE, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 136:
#line 861 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_GT, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 137:
#line 863 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_NE, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 138:
#line 865 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_AND, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 139:
#line 867 "parse.y"
{ yyval.tree_expression_type = make_binary_op (EXPR_OR, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 140:
#line 869 "parse.y"
{ yyval.tree_expression_type = make_boolean_op (EXPR_AND_AND, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 141:
#line 871 "parse.y"
{ yyval.tree_expression_type = make_boolean_op (EXPR_OR_OR, yyvsp[-2].tree_expression_type, yyvsp[-1].tok_val, yyvsp[0].tree_expression_type); ;
    break;}
case 142:
#line 875 "parse.y"
{
		    yyval.tree_colon_expression_type = new tree_colon_expression
		      (yyvsp[-2].tree_expression_type, yyvsp[0].tree_expression_type, yyvsp[-1].tok_val->line (), yyvsp[-1].tok_val->column ());
		  ;
    break;}
case 143:
#line 880 "parse.y"
{
		    if (! (yyval.tree_colon_expression_type = yyvsp[-2].tree_colon_expression_type->chain (yyvsp[0].tree_expression_type)))
		      ABORT_PARSE;
		  ;
    break;}
case 144:
#line 887 "parse.y"
{
		    yyval.tree_index_expression_type = new tree_index_expression
		      (yyvsp[-1].tree_identifier_type, yyvsp[0].tree_argument_list_type, yyvsp[-1].tree_identifier_type->line (), yyvsp[-1].tree_identifier_type->column ());
		  ;
    break;}
case 145:
#line 894 "parse.y"
{
		    tree_constant *tmp = make_constant (TEXT, yyvsp[0].tok_val);
		    yyval.tree_argument_list_type = new tree_argument_list (tmp);
		  ;
    break;}
case 146:
#line 899 "parse.y"
{
		    tree_constant *tmp = make_constant (TEXT, yyvsp[0].tok_val);
		    yyvsp[-1].tree_argument_list_type->append (tmp);
		    yyval.tree_argument_list_type = yyvsp[-1].tree_argument_list_type;
		  ;
    break;}
case 147:
#line 909 "parse.y"
{ curr_sym_tab = global_sym_tab; ;
    break;}
case 148:
#line 913 "parse.y"
{ lexer_flags.looking_at_return_list = 1; ;
    break;}
case 149:
#line 917 "parse.y"
{ curr_sym_tab = tmp_local_sym_tab; ;
    break;}
case 150:
#line 921 "parse.y"
{ lexer_flags.maybe_screwed = 0; ;
    break;}
case 151:
#line 925 "parse.y"
{ lexer_flags.maybe_screwed = 1; ;
    break;}
case 152:
#line 929 "parse.y"
{
		    curr_sym_tab = top_level_sym_tab;
		    lexer_flags.defining_func = 0;
		    yyval.tree_command_type = 0;
		  ;
    break;}
case 153:
#line 935 "parse.y"
{
		    curr_sym_tab = top_level_sym_tab;
		    lexer_flags.defining_func = 0;
		    yyval.tree_command_type = 0;
		  ;
    break;}
case 154:
#line 943 "parse.y"
{ yyval.tree_function_type = finish_function_def (yyvsp[-4].tok_val, yyvsp[0].tree_function_type); ;
    break;}
case 155:
#line 945 "parse.y"
{ yyval.tree_function_type = finish_function_def (yyvsp[-3].tree_parameter_list_type, yyvsp[0].tree_function_type); ;
    break;}
case 157:
#line 952 "parse.y"
{
		    lexer_flags.looking_at_return_list = 0;
		    yyval.tree_parameter_list_type = new tree_parameter_list ();
		  ;
    break;}
case 158:
#line 957 "parse.y"
{
		    lexer_flags.looking_at_return_list = 0;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    yyval.tree_parameter_list_type = tmp;
		  ;
    break;}
case 159:
#line 964 "parse.y"
{
		    lexer_flags.looking_at_return_list = 0;
		    yyval.tree_parameter_list_type = yyvsp[-1].tree_parameter_list_type;
		  ;
    break;}
case 160:
#line 969 "parse.y"
{
		    lexer_flags.looking_at_return_list = 0;
		    yyvsp[-3].tree_parameter_list_type->mark_varargs ();
		    yyval.tree_parameter_list_type = yyvsp[-3].tree_parameter_list_type;
		  ;
    break;}
case 161:
#line 977 "parse.y"
{ yyval.tree_parameter_list_type = new tree_parameter_list (yyvsp[0].tree_identifier_type); ;
    break;}
case 162:
#line 979 "parse.y"
{
		    yyerror ("invalid function return list");
		    yyval.tree_parameter_list_type = 0;
		    ABORT_PARSE;
		  ;
    break;}
case 163:
#line 985 "parse.y"
{
		    yyvsp[-2].tree_parameter_list_type->append (yyvsp[0].tree_identifier_type);
		    yyval.tree_parameter_list_type = yyvsp[-2].tree_parameter_list_type;
		  ;
    break;}
case 164:
#line 992 "parse.y"
{
		    if (! (yyval.tree_function_type = frob_function_def (yyvsp[-3].tree_identifier_type, yyvsp[0].tree_function_type)))
		      ABORT_PARSE;
		  ;
    break;}
case 165:
#line 999 "parse.y"
{ yyval.tree_function_type = start_function_def (yyvsp[-3].tree_parameter_list_type, yyvsp[-1].tree_statement_list_type); ;
    break;}
case 166:
#line 1001 "parse.y"
{ yyval.tree_function_type = start_function_def (0, yyvsp[-1].tree_statement_list_type); ;
    break;}
case 167:
#line 1005 "parse.y"
{
		    if (check_end (yyvsp[0].tok_val, token::function_end))
		      ABORT_PARSE;

		    if (reading_fcn_file)
		      check_for_garbage_after_fcn_def ();
		  ;
    break;}
case 168:
#line 1013 "parse.y"
{
		    if (! (reading_fcn_file || reading_script_file))
		      YYABORT;
		  ;
    break;}
case 169:
#line 1020 "parse.y"
{
		    lexer_flags.looking_at_indirect_ref = 0;
		    yyval.tree_indirect_ref_type = yyvsp[0].tree_indirect_ref_type;
		  ;
    break;}
case 170:
#line 1026 "parse.y"
{
		    yyval.tree_indirect_ref_type = new tree_indirect_ref (yyvsp[0].tree_identifier_type, yyvsp[0].tree_identifier_type->line (),
						yyvsp[0].tree_identifier_type->column ());
		  ;
    break;}
case 171:
#line 1031 "parse.y"
{ lexer_flags.looking_at_indirect_ref = 1; ;
    break;}
case 172:
#line 1032 "parse.y"
{ yyval.tree_indirect_ref_type = new tree_indirect_ref (yyvsp[-3].tree_indirect_ref_type, yyvsp[0].tok_val->text ()); ;
    break;}
case 173:
#line 1036 "parse.y"
{ yyval.tree_index_expression_type = make_index_expression (yyvsp[0].tree_indirect_ref_type, 0); ;
    break;}
case 174:
#line 1038 "parse.y"
{ yyval.tree_index_expression_type = make_index_expression (yyvsp[-2].tree_indirect_ref_type, 0); ;
    break;}
case 175:
#line 1040 "parse.y"
{ yyval.tree_index_expression_type = make_index_expression (yyvsp[-3].tree_indirect_ref_type, yyvsp[-1].tree_argument_list_type); ;
    break;}
case 176:
#line 1042 "parse.y"
{
		    yyerror ("use `(\' and `)\' as index operators, not\
 `[\' and `]\'"); 
		    yyval.tree_index_expression_type = 0;
		    ABORT_PARSE;
		  ;
    break;}
case 177:
#line 1051 "parse.y"
{ lexer_flags.looking_at_parameter_list = 1; ;
    break;}
case 178:
#line 1055 "parse.y"
{
		    lexer_flags.quote_is_transpose = 0;
		    yyval.tree_parameter_list_type = 0;
		  ;
    break;}
case 179:
#line 1060 "parse.y"
{
		    lexer_flags.quote_is_transpose = 0;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    yyval.tree_parameter_list_type = tmp;
		  ;
    break;}
case 180:
#line 1067 "parse.y"
{
		    lexer_flags.looking_at_parameter_list = 0;
		    lexer_flags.quote_is_transpose = 0;
		    yyvsp[-1].tree_parameter_list_type->mark_as_formal_parameters ();
		    yyval.tree_parameter_list_type = yyvsp[-1].tree_parameter_list_type;
		  ;
    break;}
case 181:
#line 1074 "parse.y"
{
		    lexer_flags.looking_at_parameter_list = 0;
		    lexer_flags.quote_is_transpose = 0;
		    yyvsp[-3].tree_parameter_list_type->mark_as_formal_parameters ();
		    yyvsp[-3].tree_parameter_list_type->mark_varargs ();
		    yyval.tree_parameter_list_type = yyvsp[-3].tree_parameter_list_type;
		  ;
    break;}
case 182:
#line 1084 "parse.y"
{ yyval.tree_parameter_list_type = new tree_parameter_list (yyvsp[0].tree_identifier_type); ;
    break;}
case 183:
#line 1086 "parse.y"
{
		    yyvsp[-2].tree_parameter_list_type->append (yyvsp[0].tree_identifier_type);
		    yyval.tree_parameter_list_type = yyvsp[-2].tree_parameter_list_type;
		  ;
    break;}
case 184:
#line 1091 "parse.y"
{
		    yyerror ("invalid parameter list");
		    yyval.tree_parameter_list_type = 0;
		    ABORT_PARSE;
		  ;
    break;}
case 185:
#line 1097 "parse.y"
{
		    yyerror ("invalid parameter list");
		    yyval.tree_parameter_list_type = 0;
		    ABORT_PARSE;
		  ;
    break;}
case 186:
#line 1105 "parse.y"
{
		    yyval.tree_identifier_type = new tree_identifier
		      (yyvsp[0].tok_val->sym_rec (), yyvsp[0].tok_val->line (), yyvsp[0].tok_val->column ());
		  ;
    break;}
case 187:
#line 1112 "parse.y"
{
		    tree_constant *colon =
		      new tree_constant (tree_constant::magic_colon_t);
		    yyval.tree_argument_list_type = new tree_argument_list (colon);
		  ;
    break;}
case 188:
#line 1118 "parse.y"
{ yyval.tree_argument_list_type = new tree_argument_list (yyvsp[0].tree_expression_type); ;
    break;}
case 189:
#line 1120 "parse.y"
{
		    tree_constant *all_va_args =
		      new tree_constant (tree_constant::all_va_args_t);
		    yyval.tree_argument_list_type = new tree_argument_list (all_va_args);
		  ;
    break;}
case 190:
#line 1126 "parse.y"
{
		    tree_constant *colon =
		      new tree_constant (tree_constant::magic_colon_t);
		    yyvsp[-2].tree_argument_list_type->append (colon);
		    yyval.tree_argument_list_type = yyvsp[-2].tree_argument_list_type;
		  ;
    break;}
case 191:
#line 1133 "parse.y"
{
		    yyvsp[-2].tree_argument_list_type->append (yyvsp[0].tree_expression_type);
		    yyval.tree_argument_list_type = yyvsp[-2].tree_argument_list_type;
		  ;
    break;}
case 192:
#line 1138 "parse.y"
{
		    tree_constant *all_va_args =
		      new tree_constant (tree_constant::all_va_args_t);
		    yyvsp[-2].tree_argument_list_type->append (all_va_args);
		    yyval.tree_argument_list_type = yyvsp[-2].tree_argument_list_type;
		  ;
    break;}
case 193:
#line 1147 "parse.y"
{ yyval.tree_expression_type = finish_matrix (yyvsp[-1].tree_matrix_type); ;
    break;}
case 194:
#line 1151 "parse.y"
{ yyval.tree_matrix_type = yyvsp[0].tree_matrix_type; ;
    break;}
case 195:
#line 1153 "parse.y"
{ yyval.tree_matrix_type = yyvsp[-1].tree_matrix_type; ;
    break;}
case 196:
#line 1157 "parse.y"
{ yyval.tree_matrix_type = new tree_matrix (yyvsp[0].tree_matrix_row_type); ;
    break;}
case 197:
#line 1159 "parse.y"
{
		    yyvsp[-2].tree_matrix_type->append (yyvsp[0].tree_matrix_row_type);
		    yyval.tree_matrix_type = yyvsp[-2].tree_matrix_type;
		  ;
    break;}
case 198:
#line 1166 "parse.y"
{ yyval.tree_matrix_row_type = yyvsp[0].tree_matrix_row_type; ;
    break;}
case 199:
#line 1168 "parse.y"
{ yyval.tree_matrix_row_type = yyvsp[-1].tree_matrix_row_type; ;
    break;}
case 200:
#line 1172 "parse.y"
{ yyval.tree_matrix_row_type = new tree_matrix_row (yyvsp[0].tree_expression_type); ;
    break;}
case 201:
#line 1174 "parse.y"
{
		    yyvsp[-2].tree_matrix_row_type->append (yyvsp[0].tree_expression_type);
		    yyval.tree_matrix_row_type = yyvsp[-2].tree_matrix_row_type;
		  ;
    break;}
case 202:
#line 1181 "parse.y"
{ yyval.sep_type = ','; ;
    break;}
case 203:
#line 1183 "parse.y"
{ yyval.sep_type = ';'; ;
    break;}
case 204:
#line 1185 "parse.y"
{ yyval.sep_type = yyvsp[-1].sep_type; ;
    break;}
case 205:
#line 1187 "parse.y"
{ yyval.sep_type = yyvsp[-1].sep_type; ;
    break;}
case 206:
#line 1191 "parse.y"
{ yyval.sep_type = 0; ;
    break;}
case 207:
#line 1193 "parse.y"
{ yyval.sep_type = yyvsp[0].sep_type; ;
    break;}
case 208:
#line 1197 "parse.y"
{ yyval.sep_type = ','; ;
    break;}
case 209:
#line 1199 "parse.y"
{ yyval.sep_type = ';'; ;
    break;}
case 210:
#line 1201 "parse.y"
{ yyval.sep_type = '\n'; ;
    break;}
case 211:
#line 1203 "parse.y"
{ yyval.sep_type = yyvsp[-1].sep_type; ;
    break;}
case 212:
#line 1205 "parse.y"
{ yyval.sep_type = yyvsp[-1].sep_type; ;
    break;}
case 213:
#line 1207 "parse.y"
{ yyval.sep_type = yyvsp[-1].sep_type; ;
    break;}
case 214:
#line 1211 "parse.y"
{ yyval.sep_type = 0; ;
    break;}
case 215:
#line 1213 "parse.y"
{ yyval.sep_type = yyvsp[0].sep_type; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 498 "/usr/local/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 1216 "parse.y"


// Generic error messages.

static void
yyerror (char *s)
{
  int err_col = current_input_column - 1;

  ostrstream output_buf;

  if (reading_fcn_file || reading_script_file)
    output_buf << "parse error near line " << input_line_number
	       << " of file " << curr_fcn_file_full_name;
  else
    output_buf << "parse error:";

  if (s && strcmp (s, "parse error") != 0)
    output_buf << "\n\n  " << s;

  output_buf << "\n\n";

  if (! current_input_line.empty ())
    {
      size_t len = current_input_line.length ();

      if (current_input_line[len-1] == '\n')
        current_input_line.resize (len-1);

// Print the line, maybe with a pointer near the error token.

      output_buf << ">>> " << current_input_line << "\n";

      if (err_col == 0)
	err_col = len;

      for (int i = 0; i < err_col + 3; i++)
	output_buf << " ";

      output_buf << "^";
    }

  output_buf << "\n" << ends;

  char *msg = output_buf.str ();

  parse_error ("%s", msg);

  delete [] msg;
}

// Error mesages for mismatched end tokens.

static void
end_error (char *type, token::end_tok_type ettype, int l, int c)
{
  static char *fmt = "`%s' command matched by `%s' near line %d column %d";

  switch (ettype)
    {
    case token::simple_end:
      error (fmt, type, "end", l, c);
      break;

    case token::for_end:
      error (fmt, type, "endfor", l, c);
      break;

    case token::function_end:
      error (fmt, type, "endfunction", l, c);
      break;

    case token::if_end:
      error (fmt, type, "endif", l, c);
      break;

    case token::while_end:
      error (fmt, type, "endwhile", l, c); 
      break;

    case token::unwind_protect_end:
      error (fmt, type, "end_unwind_protect", l, c); 
      break;

    default:
      panic_impossible ();
      break;
    }
}

// Check to see that end tokens are properly matched.

static int
check_end (token *tok, token::end_tok_type expected)
{
  token::end_tok_type ettype = tok->ettype ();
  if (ettype != expected && ettype != token::simple_end)
    {
      yyerror ("parse error");

      int l = tok->line ();
      int c = tok->column ();

      switch (expected)
	{
	case token::for_end:
	  end_error ("for", ettype, l, c);
	  break;

	case token::function_end:
	  end_error ("function", ettype, l, c);
	  break;

	case token::if_end:
	  end_error ("if", ettype, l, c);
	  break;

	case token::try_catch_end:
	  end_error ("try", ettype, l, c);
	  break;

	case token::switch_end:
	  end_error ("switch", ettype, l, c);
	  break;

	case token::unwind_protect_end:
	  end_error ("unwind_protect", ettype, l, c);
	  break;

	case token::while_end:
	  end_error ("while", ettype, l, c);
	  break;

	default:
	  panic_impossible ();
	  break;
	}
      return 1;
    }
  else
    return 0;
}

// Try to figure out early if an expression should become an
// assignment to the built-in variable ans.
//
// Need to make sure that the expression is not already an identifier
// that has a name, or an assignment expression.
//
// Note that an expression can not be just an identifier now -- it
// must at least be an index expression (see the definition of the
// non-terminal variable above).

static tree_expression *
maybe_convert_to_ans_assign (tree_expression *expr)
{
  if (expr->is_index_expression ())
    {
      expr->mark_for_possible_ans_assign ();
      return expr;
    }
  else if (expr->is_assignment_expression ()
	   || expr->is_prefix_expression ())
    {
      return expr;
    }
  else
    {
      // XXX FIXME XXX -- making ans_id static, passing its address to
      // tree_simple_assignment_expression along with a flag to not
      // delete it seems to create a memory leak.  Hmm.

      static symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);
      tree_identifier *ans_id = new tree_identifier (sr);

      int l = expr->line ();
      int c = expr->column ();

      return new tree_simple_assignment_expression (ans_id, expr, 0, 1, l, c);
    }
}

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.

static void
maybe_warn_assign_as_truth_value (tree_expression *expr)
{
  if (Vwarn_assign_as_truth_value
      && expr->is_assignment_expression ()
      && expr->is_in_parens () < 2)
    {
      warning ("suggest parenthesis around assignment used as truth value");
    }
}

// Maybe print a warning about switch labels that aren't constants.

static void
maybe_warn_variable_switch_label (tree_expression *expr)
{
  if (Vwarn_variable_switch_label && ! expr->is_constant ())
    {
      warning ("variable switch label");
    }
}

// Create a plot command.

static tree_plot_command *
make_plot_command (token *tok, plot_limits *range, subplot_list *list)
{
  if (range)
    {
      if (tok->pttype () == token::replot)
	{
	  yyerror ("cannot specify new ranges with replot");
	  return 0;
	}
    }
  else if (! list && tok->pttype () != token::replot)
    {
      yyerror ("must have something to plot");
      return 0;
    }

  lexer_flags.plotting = 0;
  lexer_flags.past_plot_range = 0;
  lexer_flags.in_plot_range = 0;
  lexer_flags.in_plot_using = 0;
  lexer_flags.in_plot_style = 0;
  
  return new tree_plot_command (list, range, tok->pttype ());
}

static tree_expression *
fold (tree_binary_expression *e)
{
  tree_expression *retval = 0;

  tree_expression *op1 = e->lhs ();
  tree_expression *op2 = e->rhs ();

  if (op1->is_constant () && op2->is_constant ())
    {
      octave_value tmp = e->eval (0);

      if (! error_state)
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  ostrstream buf;

	  tree_print_code tpc (buf);

	  e->accept (tpc);

	  buf << ends;

	  char *s = buf.str ();

	  tc_retval->stash_original_text (s);

	  delete [] s;

	  delete e;

	  retval = tc_retval;
	}
      else
	delete e;
    }
  else
    retval = e;

  return retval;
}

static tree_expression *
fold (tree_unary_expression *e)
{
  tree_expression *retval = 0;

  tree_expression *op1 = e->operand ();

  if (op1->is_constant ())
    {
      octave_value tmp = e->eval (0);

      if (! error_state)
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  ostrstream buf;

	  tree_print_code tpc (buf);

	  e->accept (tpc);

	  buf << ends;

	  char *s = buf.str ();

	  tc_retval->stash_original_text (s);

	  delete [] s;

	  delete e;

	  retval = tc_retval;
	}
      else
	delete e;
    }
  else
    retval = e;

  return retval;
}

// Finish building a range.

static tree_expression *
finish_colon_expression (tree_colon_expression *e)
{
  tree_expression *retval = 0;

  tree_expression *base = e->base ();
  tree_expression *limit = e->limit ();
  tree_expression *incr = e->increment ();

  if (base->is_constant () && limit->is_constant ()
      && (! incr || (incr && incr->is_constant ())))
    {
      octave_value tmp = e->eval (0);

      if (! error_state)
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  ostrstream buf;

	  tree_print_code tpc (buf);

	  e->accept (tpc);

	  buf << ends;

	  char *s = buf.str ();

	  tc_retval->stash_original_text (s);

	  delete [] s;

	  delete e;

	  retval = tc_retval;
	}
      else
	delete e;
    }
  else
    retval = e;

  return retval;
}

// Make a constant.

static tree_constant *
make_constant (int op, token *tok_val)
{
  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_constant *retval;

  switch (op)
    {
    case NUM:
      {
	retval = new tree_constant (tok_val->number (), l, c);
	retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case IMAG_NUM:
      {
	Complex C (0.0, tok_val->number ());
	retval = new tree_constant (C, l, c);
	retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case TEXT:
      retval = new tree_constant (tok_val->text (), l, c);
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

// Build a binary expression.

static tree_expression *
make_binary_op (int op, tree_expression *op1, token *tok_val,
		tree_expression *op2)
{
  tree_binary_expression::type t;

  switch (op)
    {
    case POW:
      t = tree_binary_expression::power;
      break;

    case EPOW:
      t = tree_binary_expression::elem_pow;
      break;

    case '+':
      t = tree_binary_expression::add;
      break;

    case '-':
      t = tree_binary_expression::subtract;
      break;

    case '*':
      t = tree_binary_expression::multiply;
      break;

    case '/':
      t = tree_binary_expression::divide;
      break;

    case EMUL:
      t = tree_binary_expression::el_mul;
      break;

    case EDIV:
      t = tree_binary_expression::el_div;
      break;

    case LEFTDIV:
      t = tree_binary_expression::leftdiv;
      break;

    case ELEFTDIV:
      t = tree_binary_expression::el_leftdiv;
      break;

    case EXPR_LT:
      t = tree_binary_expression::cmp_lt;
      break;

    case EXPR_LE:
      t = tree_binary_expression::cmp_le;
      break;

    case EXPR_EQ:
      t = tree_binary_expression::cmp_eq;
      break;

    case EXPR_GE:
      t = tree_binary_expression::cmp_ge;
      break;

    case EXPR_GT:
      t = tree_binary_expression::cmp_gt;
      break;

    case EXPR_NE:
      t = tree_binary_expression::cmp_ne;
      break;

    case EXPR_AND:
      t = tree_binary_expression::and;
      break;

    case EXPR_OR:
      t = tree_binary_expression::or;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_binary_expression *e
    = new tree_binary_expression (op1, op2, l, c, t);

  return fold (e);
}

// Build a boolean expression.

static tree_expression *
make_boolean_op (int op, tree_expression *op1, token *tok_val,
		 tree_expression *op2)
{
  tree_boolean_expression::type t;

  switch (op)
    {
    case EXPR_AND_AND:
      t = tree_boolean_expression::and;
      break;

    case EXPR_OR_OR:
      t = tree_boolean_expression::or;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_boolean_expression *e
    = new tree_boolean_expression (op1, op2, l, c, t);

  return fold (e);
}

// Build a prefix expression.

static tree_expression *
make_prefix_op (int op, tree_identifier *op1, token *tok_val)
{
  tree_prefix_expression::type t;

  switch (op)
    {
    case PLUS_PLUS:
      t = tree_prefix_expression::increment;
      break;

    case MINUS_MINUS:
      t = tree_prefix_expression::decrement;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_prefix_expression (op1, l, c, t);
}

// Build a postfix expression.

static tree_expression *
make_postfix_op (int op, tree_identifier *op1, token *tok_val)
{
  tree_postfix_expression::type t;

  switch (op)
    {
    case PLUS_PLUS:
      t = tree_postfix_expression::increment;
      break;

    case MINUS_MINUS:
      t = tree_postfix_expression::decrement;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_postfix_expression (op1, l, c, t);
}

// Build a unary expression.

static tree_expression *
make_unary_op (int op, tree_expression *op1, token *tok_val)
{
  tree_unary_expression::type t;

  switch (op)
    {
    case QUOTE:
      t = tree_unary_expression::hermitian;
      break;

    case TRANSPOSE:
      t = tree_unary_expression::transpose;
      break;

    case EXPR_NOT:
      t = tree_unary_expression::not;
      break;

    case '-':
      t = tree_unary_expression::uminus;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_unary_expression *e
    = new tree_unary_expression (op1, l, c, t);

  return fold (e);
}

// Build an unwind-protect command.

static tree_command *
make_unwind_command (token *unwind_tok, tree_statement_list *body,
		     tree_statement_list *cleanup, token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::unwind_protect_end))
    {
      int l = unwind_tok->line ();
      int c = unwind_tok->column ();

      retval = new tree_unwind_protect_command (body, cleanup, l, c);
    }

  return retval;
}

// Build a try-catch command.

static tree_command *
make_try_command (token *try_tok, tree_statement_list *body,
		  tree_statement_list *cleanup, token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::try_catch_end))
    {
      int l = try_tok->line ();
      int c = try_tok->column ();

      retval = new tree_try_catch_command (body, cleanup, l, c);
    }

  return retval;
}

// Build a while command.

static tree_command *
make_while_command (token *while_tok, tree_expression *expr,
		    tree_statement_list *body, token *end_tok)
{
  tree_command *retval = 0;

  maybe_warn_assign_as_truth_value (expr);

  if (! check_end (end_tok, token::while_end))
    {
      lexer_flags.looping--;

      int l = while_tok->line ();
      int c = while_tok->column ();

      retval = new tree_while_command (expr, body, l, c);
    }

  return retval;
}

// Build a for command.

static tree_command *
make_for_command (token *for_tok, tree_index_expression *var,
		  tree_expression *expr, tree_statement_list *body,
		  token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::for_end))
    {
      lexer_flags.looping--;

      int l = for_tok->line ();
      int c = for_tok->column ();

      retval = new tree_for_command (var, expr, body, l, c);
    }

  return retval;
}

// Build a for command a different way.

static tree_command *
make_for_command (token *for_tok, tree_matrix_row *mr,
		  tree_expression *expr, tree_statement_list *body,
		  token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::for_end))
    {
      lexer_flags.looping--;

      tree_return_list *id_list = mr->to_return_list ();

      int l = for_tok->line ();
      int c = for_tok->column ();

      retval = new tree_for_command (id_list, expr, body, l, c);
    }

  return retval;
}

// Build a break command.

static tree_command *
make_break_command (token *break_tok)
{
  tree_command *retval = 0;

  int l = break_tok->line ();
  int c = break_tok->column ();

  if (lexer_flags.looping || lexer_flags.defining_func || reading_script_file)
    retval = new tree_break_command (l, c);
  else
    retval = new tree_no_op_command ("break", l, c);

  return retval;
}

// Build a continue command.

static tree_command *
make_continue_command (token *continue_tok)
{
  tree_command *retval = 0;

  int l = continue_tok->line ();
  int c = continue_tok->column ();

  if (lexer_flags.looping)
    retval = new tree_continue_command (l, c);
  else
    retval = new tree_no_op_command ("continue", l, c);

  return retval;
}

// Build a return command.

static tree_command *
make_return_command (token *return_tok)
{
  tree_command *retval = 0;

  int l = return_tok->line ();
  int c = return_tok->column ();

  if (lexer_flags.defining_func || reading_script_file)
    retval = new tree_return_command (l, c);
  else
    retval = new tree_no_op_command ("return", l, c);

  return retval;
}

// Start an if command.

static tree_if_command_list *
start_if_command (tree_expression *expr, tree_statement_list *list)
{
  maybe_warn_assign_as_truth_value (expr);

  tree_if_clause *t = new tree_if_clause (expr, list);

  return new tree_if_command_list (t);
}

// Finish an if command.

static tree_if_command *
finish_if_command (token *if_tok, tree_if_command_list *list,
		   token *end_tok)
{
  tree_if_command *retval = 0;

  if (! check_end (end_tok, token::if_end))
    {
      int l = if_tok->line ();
      int c = if_tok->column ();

      retval = new tree_if_command (list, l, c);
    }

  return retval;
}

// Build an elseif clause.

static tree_if_clause *
make_elseif_clause (tree_expression *expr, tree_statement_list *list)
{
  maybe_warn_assign_as_truth_value (expr);

  return new tree_if_clause (expr, list);
}

// Finish a switch command.

static tree_switch_command *
finish_switch_command (token *switch_tok, tree_expression *expr,
		       tree_switch_case_list *list, token *end_tok)
{
  tree_switch_command *retval = 0;

  if (! check_end (end_tok, token::switch_end))
    {
      int l = switch_tok->line ();
      int c = switch_tok->column ();

      retval = new tree_switch_command (expr, list, l, c);
    }

  return retval;
}

// Build a switch case.

static tree_switch_case *
make_switch_case (tree_expression *expr, tree_statement_list *list)
{
  maybe_warn_variable_switch_label (expr);

  return new tree_switch_case (expr, list);
}

// Build an assignment to a variable.

static tree_expression *
make_simple_assignment (tree_index_expression *var, token *eq_tok,
			tree_expression *expr)
{
  int l = eq_tok->line ();
  int c = eq_tok->column ();

  return new tree_simple_assignment_expression (var, expr, 0, 0, l, c);
}

// Make an expression that handles assignment of multiple values.

static tree_expression *
make_multi_val_ret (tree_matrix_row *mr, tree_expression *rhs, token *eq_tok)
{
// Convert the matrix list to a list of identifiers.  If that fails,
// we can abort here, without losing anything -- no other possible
// syntax is valid if we've seen the equals sign as the next token
// after the `]'. 

  tree_expression *retval = 0;

  lexer_flags.maybe_screwed_again--;

  tree_return_list *id_list = mr->to_return_list ();

  if (id_list)
    {
      int list_len = id_list->length ();

      if (list_len == 1)
	{
	  tree_index_expression *lhs = id_list->remove_front ();

	  int l = eq_tok->line ();
	  int c = eq_tok->column ();

	  retval = new tree_simple_assignment_expression (lhs, rhs,
							  0, 0, l, c);
	}
      else if (list_len > 1)
	{
	  if (rhs->is_multi_val_ret_expression ())
	    {
	      tree_multi_val_ret *t = (tree_multi_val_ret *) rhs;

	      int l = eq_tok->line ();
	      int c = eq_tok->column ();

	      retval = new tree_multi_assignment_expression (id_list, t,
							     0, l, c);
	    }
	  else
	    yyerror ("RHS must be an expression that returns multiple values");
	}
      else
	panic_impossible ();
    }
  else
    yyerror ("invalid identifier list for assignment");

  return retval;
}

// Begin defining a function.

static tree_function *
start_function_def (tree_parameter_list *param_list,
		    tree_statement_list *body)
{
  body->mark_as_function_body ();

  tree_function *fcn = new tree_function (body, curr_sym_tab);

  fcn->define_param_list (param_list);

  return fcn;
}

// Do most of the work for defining a function.

static tree_function *
frob_function_def (tree_identifier *id, tree_function *fcn)
{
  string id_name = id->name ();

  // If input is coming from a file, issue a warning if the name of
  // the file does not match the name of the function stated in the
  // file.  Matlab doesn't provide a diagnostic (it ignores the stated
  // name).

  fcn->stash_function_name (id_name);

  if (reading_fcn_file)
    {
      if (curr_fcn_file_name != id_name)
	{
	  if (Vwarn_function_name_clash)
	    warning ("function name `%s' does not agree with function\
 file name `%s'", id_name.c_str (), curr_fcn_file_full_name.c_str ());

	  global_sym_tab->rename (id_name, curr_fcn_file_name);

	  if (error_state)
	    return 0;

	  id_name = id->name ();
	}

      fcn->stash_function_name (id_name);
      fcn->stash_fcn_file_name ();
      fcn->stash_fcn_file_time (time (0));
      fcn->mark_as_system_fcn_file ();
    }
  else if (! (input_from_tmp_history_file || input_from_startup_file)
	   && reading_script_file
	   && curr_fcn_file_name == id_name)
    {
      warning ("function `%s' defined within script file `%s'",
	       id_name.c_str (), curr_fcn_file_full_name.c_str ());
    }

  top_level_sym_tab->clear (id_name);

  id->define (fcn);

  id->document (help_buf);

  return fcn;
}

// Finish defining a function.

static tree_function *
finish_function_def (token *var, tree_function *fcn)
{
  symbol_record *sr = var->sym_rec ();

  int l = var->line ();
  int c = var->column ();

  tree_identifier *tmp = new tree_identifier (sr, l, c);

  tree_parameter_list *tpl = new tree_parameter_list (tmp);

  tpl->mark_as_formal_parameters ();

  return fcn->define_ret_list (tpl);
}

// Finish defining a function a different way.

static tree_function *
finish_function_def (tree_parameter_list *ret_list, tree_function *fcn)
{
  ret_list->mark_as_formal_parameters ();

  return fcn->define_ret_list (ret_list);
}

static tree_index_expression *
make_index_expression (tree_indirect_ref *indir, tree_argument_list *args)
{
  tree_index_expression *retval = 0;

  int l = indir->line ();
  int c = indir->column ();

  if (indir->is_identifier_only ())
    {
      indir->preserve_identifier ();
      retval = new tree_index_expression (indir->ident (), args, l, c);
      delete indir;
    }
  else
    retval =  new tree_index_expression (indir, args, l, c);

  return retval;
}

// Finish building a matrix list.

static tree_expression *
finish_matrix (tree_matrix *m)
{
  tree_expression *retval = 0;

  lexer_flags.maybe_screwed_again--;

  if (m->all_elements_are_constant ())
    {
      octave_value tmp = m->eval (0);

      if (! error_state)
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  ostrstream buf;

	  tree_print_code tpc (buf);

	  m->accept (tpc);

	  buf << ends;

	  char *s = buf.str ();

	  tc_retval->stash_original_text (s);

	  delete [] s;

	  delete m;

	  retval = tc_retval;
	}
      else
	delete m;
    }
  else
    retval = m;

  return retval;
}

static void
maybe_warn_missing_semi (tree_statement_list *t)
{
  if (lexer_flags.defining_func && Vwarn_missing_semicolon)
    {
      tree_statement *tmp = t->rear();

      if (tmp->is_expression ())
	warning ("missing semicolon near line %d, column %d in file `%s'",
		 tmp->line (), tmp->column (),
		 curr_fcn_file_full_name.c_str ());
    }
}

static void
set_stmt_print_flag (tree_statement_list *list, char sep,
		     bool warn_missing_semi)
{
  switch (sep)
    {
    case ';':
      {
	tree_statement *tmp = list->rear ();
	tmp->set_print_flag (0);
      }
      break;

    case 0:
    case ',':
    case '\n':
      if (warn_missing_semi)
	maybe_warn_missing_semi (list);
      break;

    default:
      warning ("unrecognized separator type!");
      break;
    }
}

static int
warn_assign_as_truth_value (void)
{
  Vwarn_assign_as_truth_value
    = check_preference ("warn_assign_as_truth_value");

  return 0;
}

static int
warn_comma_in_global_decl (void)
{
  Vwarn_comma_in_global_decl = check_preference ("warn_comma_in_global_decl");

  return 0;
}

static int
warn_function_name_clash (void)
{
  Vwarn_function_name_clash = check_preference ("warn_function_name_clash");

  return 0;
}

static int
warn_missing_semicolon (void)
{
  Vwarn_missing_semicolon = check_preference ("warn_missing_semicolon");

  return 0;
}

static int
warn_variable_switch_label (void)
{
  Vwarn_variable_switch_label
    = check_preference ("warn_variable_switch_label");

  return 0;
}

void
symbols_of_parse (void)
{
  DEFVAR (warn_assign_as_truth_value, 1.0, 0, warn_assign_as_truth_value,
    "produce warning for assignments used as truth values");

  DEFVAR (warn_comma_in_global_decl, 1.0, 0, warn_comma_in_global_decl,
    "produce warning for commas in global declarations");

  DEFVAR (warn_function_name_clash, 1.0, 0, warn_function_name_clash,
    "produce warning if function name conflicts with file name");

  DEFVAR (warn_missing_semicolon, 0.0, 0, warn_missing_semicolon,
    "produce a warning if a statement in a function file is not\n\
terminated with a semicolon");

  DEFVAR (warn_variable_switch_label, 0.0, 0, warn_variable_switch_label,
    "produce warning for variables used as switch labels");
}

/*
;;; Local Variables: ***
;;; mode: text ***
;;; End: ***
*/
