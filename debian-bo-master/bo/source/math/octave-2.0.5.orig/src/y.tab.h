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


extern YYSTYPE yylval;
