extern char *malloc(), *realloc();

# line 2 "gen.y"

#include "global_parse.h"
#include "result_types.h"
#include "file_dict.h"
#include "proc_dict.h"
#include "class_dict.h"
#include "browser_pars.h"
#include "browser_util.h"
#include <stdio.h>


/*------------------------------------------------------------------------------
*/
char yy_class_name[256];

static char yy_class_stack[50][256];
static int yy_class_stack_top = 0;
static int yy_in_class = 0;

static ScopeType yy_scope;

#define S_PRECEDENCE       10
#define P_PRECEDENCE       10
#define A_PRECEDENCE       20
#define MAX_PRECEDENCE     30

#define parentheses(ref_prec, inter, result) \
          if (inter.precedence < ref_prec)   \
            strcat(result.text, "(");        \
          strcat(result.text, inter.text);   \
          if (inter.precedence < ref_prec)   \
            strcat(result.text, ")");        \
          result.precedence = ref_prec;

# define ARRAY_TOK 257
# define UNSIGNED_TOK 258
# define SIGNED_TOK 259
# define CHAR_TOK 260
# define SHORT_TOK 261
# define INT_TOK 262
# define LONG_TOK 263
# define STRUCT_TOK 264
# define UNION_TOK 265
# define CLASS_TOK 266
# define ENUM_TOK 267
# define CONST_TOK 268
# define ELLIPSIS_TOK 269
# define IDENT_TOK 270
# define CPLUS_TOK 271
# define C_TOK 272
# define DECL_TOK 273
# define SYNC_TOK 274
# define PUBLIC_TOK 275
# define PROTECTED_TOK 276
# define PRIVATE_TOK 277
# define STATIC_TOK 278
# define INLINE_TOK 279
# define LEX_ERROR_TOK 280
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 376 "gen.y"


int yyerror(error_msg)
    char* error_msg;
{
	if ( OpVerbose () == True ) {
	  fprintf(stderr, "warning >>>>>>  Internal Parser Error at line %d\n", line_count);
	  fprintf(stderr, "warning    >>  %s\n", error_msg);
	}
}


int browser_yyparse(file_name)
    char* file_name;
{
  FILE* file;
  int   result;

  result = 1;
  *yy_class_name = '\0';
  file = fopen(file_name, "r");
  if (file != Null) {
    flex_init(file);
    result = yyparse();
    fclose(file);
  }
  else
    fprintf(stderr, "\n>>>>>>  Unable to open file %s\n", file_name);
  return(result);
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 11,
	};
# define YYNPROD 80
# define YYLAST 300
int yyact[]={

   118,    13,    14,   132,     8,     9,    67,     6,    86,    26,
    96,     4,     5,    10,    23,    24,    25,    72,    85,    73,
    90,    71,    72,    72,    73,    73,    71,    71,    11,    12,
    47,    34,    35,    33,    36,    37,    38,    39,    40,    41,
    42,    32,    20,    43,    34,    35,    33,    36,    37,    38,
    39,    40,    41,    42,    84,    72,    43,    73,    83,    71,
   126,   125,    89,    67,    67,    67,    88,    65,    62,    67,
    67,    61,    60,    16,   103,    79,    80,    81,    82,    75,
    76,    77,    78,   124,   105,    59,    46,   121,    28,    29,
   128,    94,    48,    22,    93,    19,    18,   102,    17,    31,
    92,    49,    64,     2,    21,   120,    15,     7,    44,    45,
     3,     1,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    30,     0,     0,    68,     0,
     0,     0,    74,     0,     0,     0,     0,     0,     0,    91,
     0,     0,     0,     0,     0,    95,    54,    55,    53,    56,
    50,    51,    52,    97,     0,    57,    54,    55,    53,    56,
    50,    51,    52,     0,    58,    57,    54,    55,    53,    56,
    50,    51,    52,     0,     0,    57,    54,    55,    53,    56,
    50,    51,    52,   116,   117,    57,   113,   114,    91,    26,
   101,   115,     0,   104,    23,    24,    25,   119,   106,    27,
   107,   109,   111,   112,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   131,   127,
     0,     0,     0,   129,   130,     0,    63,     0,     0,     0,
     0,    66,     0,     0,     0,     0,     0,   122,     0,   123,
     0,     0,     0,    87,     0,     0,     0,   110,     0,    70,
     0,     0,   108,    69,    70,    70,     0,     0,     0,     0,
    98,    99,   100,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    70,    66,    66,
    66,     0,     0,     0,    66,    66,     0,     0,     0,   105 };
int yypact[]={

 -1000,  -267, -1000, -1000,  -243,  -270, -1000,  -197,    58, -1000,
 -1000,    56, -1000,    55, -1000,   -81,    28,  -227,  -227,  -227,
   -95,    41, -1000,  -198,  -199,  -202,    28, -1000, -1000,    26,
 -1000,   -15,  -214, -1000,  -181,  -185, -1000, -1000, -1000,  -212,
  -216,  -252,  -262,    28,    25,    21,  -105, -1000, -1000, -1000,
 -1000, -1000, -1000,    54,    51, -1000, -1000, -1000,  -115,  -261,
    28,    28,    28, -1000,  -227,  -194, -1000,  -227,  -173,    17,
 -1000,   -16,   -21,    17,    17, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -194,  -194,
 -1000, -1000, -1000,  -227,  -227,  -125, -1000, -1000, -1000, -1000,
 -1000,    43, -1000, -1000, -1000, -1000,  -173,  -173,    17,  -173,
    17,    42,  -173, -1000, -1000, -1000,    20,    19, -1000, -1000,
 -1000, -1000,  -173,  -173,    50,  -194,  -194, -1000,  -227, -1000,
 -1000,   -38, -1000 };
int yypgo[]={

     0,   111,   101,   110,   107,   106,    86,   100,   104,   199,
    93,    92,    89,    97,   125,    99,   128,   102 };
int yyr1[]={

     0,     1,     1,     1,     1,     1,     1,     2,     2,     2,
     2,     4,     7,     5,     8,     8,    10,    10,    10,    10,
     6,     6,    11,    11,    11,    11,    11,    11,    11,    11,
    11,     3,     3,     3,     3,     3,     3,     3,    13,    13,
    12,    12,    12,    12,    12,    14,    14,    14,    15,    15,
    15,    15,    15,    15,    15,    15,    15,    15,    15,    15,
    15,    15,    15,    15,    15,    15,    15,     9,     9,    17,
    17,    16,    16,    16,    16,    16,    16,    16,    16,    16 };
int yyr2[]={

     0,     0,     4,     4,     4,     4,     4,    12,    10,    14,
    12,     1,     1,     5,     2,     6,     7,     7,     7,     5,
     2,     4,     2,     3,     3,     3,    11,    11,     2,     2,
     3,    13,    13,     5,     5,    11,     3,     3,     1,     3,
     1,     3,     5,     7,     5,     5,     7,     7,     3,     5,
     5,     3,     5,     5,     3,     5,     5,     3,     3,     3,
     5,     5,     5,     5,     5,     5,     5,     1,     9,     1,
     7,     1,     3,     5,     7,     5,     7,     5,    13,     7 };
int yychk[]={

 -1000,    -1,    -2,    -3,   278,   279,   274,    -4,   271,   272,
   280,   271,   272,   271,   272,    -5,   270,    40,    40,    40,
   123,    -8,   -10,   275,   276,   277,   270,    -9,    60,   -12,
   -14,   -15,   268,   260,   258,   259,   261,   262,   263,   264,
   265,   266,   267,   270,   -12,   -12,    -6,   125,   -11,    -2,
   275,   276,   277,   273,   271,   272,   274,   280,   123,    44,
   270,   270,   270,    -9,   -17,    41,   269,    44,   -16,   268,
   270,    42,    38,    40,   -15,   260,   261,   262,   263,   260,
   261,   262,   263,   270,   270,   270,   270,    -9,    41,    41,
   125,   -11,    -7,    40,    40,    -6,   125,   -10,    -9,    -9,
    -9,   -14,   -13,   268,   -14,   257,   -16,   -16,   268,   -16,
   268,   -16,   -16,   -13,   -13,    -7,   -12,   -12,   125,    -7,
    62,    44,   -16,   -16,    41,    41,    41,    -7,    40,   -13,
   -13,   -12,    41 };
int yydef[]={

     1,    -2,     2,     3,     4,     5,     6,     0,     0,    36,
    37,     0,    33,     0,    34,     0,    67,    40,    40,    40,
    11,     0,    14,     0,     0,     0,    67,    13,    69,     0,
    41,    71,     0,    48,    57,    58,    51,    54,    59,     0,
     0,     0,     0,    67,     0,     0,    11,    12,    20,    22,
    23,    24,    25,     0,     0,    28,    29,    30,    11,     0,
    67,    67,    67,    19,     0,    38,    42,    44,    45,    71,
    72,    71,    71,    71,    71,    49,    52,    55,    60,    50,
    53,    56,    61,    62,    63,    64,    65,    66,    38,    38,
    12,    21,     8,    40,    40,    11,    12,    15,    16,    17,
    18,     0,    35,    39,    43,    77,    47,    73,    71,    75,
    71,     0,    46,    31,    32,     7,     0,     0,    12,    10,
    68,    70,    74,    76,    79,    38,    38,     9,    40,    26,
    27,     0,    78 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"*",	42,
	"&",	38,
	"ARRAY_TOK",	257,
	"UNSIGNED_TOK",	258,
	"SIGNED_TOK",	259,
	"CHAR_TOK",	260,
	"SHORT_TOK",	261,
	"INT_TOK",	262,
	"LONG_TOK",	263,
	"STRUCT_TOK",	264,
	"UNION_TOK",	265,
	"CLASS_TOK",	266,
	"ENUM_TOK",	267,
	"CONST_TOK",	268,
	"ELLIPSIS_TOK",	269,
	"IDENT_TOK",	270,
	"CPLUS_TOK",	271,
	"C_TOK",	272,
	"DECL_TOK",	273,
	"SYNC_TOK",	274,
	"PUBLIC_TOK",	275,
	"PROTECTED_TOK",	276,
	"PRIVATE_TOK",	277,
	"STATIC_TOK",	278,
	"INLINE_TOK",	279,
	"LEX_ERROR_TOK",	280,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"file : /* empty */",
	"file : file class_decl",
	"file : file procedure_impl",
	"file : file STATIC_TOK",
	"file : file INLINE_TOK",
	"file : file SYNC_TOK",
	"class_decl : class_start class_name '{' member_list '}' class_end",
	"class_decl : class_start class_name '{' '}' class_end",
	"class_decl : class_start class_name parent_list '{' member_list '}' class_end",
	"class_decl : class_start class_name parent_list '{' '}' class_end",
	"class_start : /* empty */",
	"class_end : /* empty */",
	"class_name : IDENT_TOK opt_template_args",
	"parent_list : parent",
	"parent_list : parent_list ',' parent",
	"parent : PUBLIC_TOK IDENT_TOK opt_template_args",
	"parent : PROTECTED_TOK IDENT_TOK opt_template_args",
	"parent : PRIVATE_TOK IDENT_TOK opt_template_args",
	"parent : IDENT_TOK opt_template_args",
	"member_list : member_decl",
	"member_list : member_list member_decl",
	"member_decl : class_decl",
	"member_decl : PUBLIC_TOK",
	"member_decl : PROTECTED_TOK",
	"member_decl : PRIVATE_TOK",
	"member_decl : DECL_TOK '(' arguments_list ')' trail_decl",
	"member_decl : CPLUS_TOK '(' arguments_list ')' trail_decl",
	"member_decl : C_TOK",
	"member_decl : SYNC_TOK",
	"member_decl : LEX_ERROR_TOK",
	"procedure_impl : STATIC_TOK CPLUS_TOK '(' arguments_list ')' trail_decl",
	"procedure_impl : INLINE_TOK CPLUS_TOK '(' arguments_list ')' trail_decl",
	"procedure_impl : STATIC_TOK C_TOK",
	"procedure_impl : INLINE_TOK C_TOK",
	"procedure_impl : CPLUS_TOK '(' arguments_list ')' trail_decl",
	"procedure_impl : C_TOK",
	"procedure_impl : LEX_ERROR_TOK",
	"trail_decl : /* empty */",
	"trail_decl : CONST_TOK",
	"arguments_list : /* empty */",
	"arguments_list : argument",
	"arguments_list : arguments_list ELLIPSIS_TOK",
	"arguments_list : arguments_list ',' argument",
	"arguments_list : arguments_list ','",
	"argument : type declarator",
	"argument : CONST_TOK type declarator",
	"argument : type CONST_TOK declarator",
	"type : CHAR_TOK",
	"type : UNSIGNED_TOK CHAR_TOK",
	"type : SIGNED_TOK CHAR_TOK",
	"type : SHORT_TOK",
	"type : UNSIGNED_TOK SHORT_TOK",
	"type : SIGNED_TOK SHORT_TOK",
	"type : INT_TOK",
	"type : UNSIGNED_TOK INT_TOK",
	"type : SIGNED_TOK INT_TOK",
	"type : UNSIGNED_TOK",
	"type : SIGNED_TOK",
	"type : LONG_TOK",
	"type : UNSIGNED_TOK LONG_TOK",
	"type : SIGNED_TOK LONG_TOK",
	"type : STRUCT_TOK IDENT_TOK",
	"type : UNION_TOK IDENT_TOK",
	"type : CLASS_TOK IDENT_TOK",
	"type : ENUM_TOK IDENT_TOK",
	"type : IDENT_TOK opt_template_args",
	"opt_template_args : /* empty */",
	"opt_template_args : '<' template_args argument '>'",
	"template_args : /* empty */",
	"template_args : template_args argument ','",
	"declarator : /* empty */",
	"declarator : IDENT_TOK",
	"declarator : '*' declarator",
	"declarator : '*' CONST_TOK declarator",
	"declarator : '&' declarator",
	"declarator : '&' CONST_TOK declarator",
	"declarator : declarator ARRAY_TOK",
	"declarator : '(' declarator ')' '(' arguments_list ')'",
	"declarator : '(' declarator ')'",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 11:
# line 76 "gen.y"
{ if (yy_in_class)
      {
	strcpy(yy_class_stack[yy_class_stack_top], yy_class_name);
	yy_class_stack_top++;
      }
      yy_in_class++;
    } break;
case 12:
# line 85 "gen.y"
{ yy_in_class--;

      if (yy_in_class)
      {
	yy_class_stack_top--;
	strcpy(yy_class_name, yy_class_stack[yy_class_stack_top]);
      }
    } break;
case 13:
# line 97 "gen.y"
{
      strcpy(yy_class_name, yypvt[-1].text);
      if (add_class_decl(yypvt[-1].text, yypvt[-1].position) == BERROR)
        YYABORT;
      yy_scope = PUBLIC_SCOPE;
    } break;
case 16:
# line 116 "gen.y"
{
			char	buf[256];
			sprintf (buf,"%s%s", yypvt[-1].text, yypvt[-0].text);
      if (add_parent(yy_class_name, yypvt[-1].text, PUBLIC_SCOPE) == BERROR)
        YYABORT;
    } break;
case 17:
# line 122 "gen.y"
{
			char	buf[256];
			sprintf (buf,"%s%s", yypvt[-1].text, yypvt[-0].text);
      if (add_parent(yy_class_name, buf, PROTECTED_SCOPE) == BERROR)
        YYABORT;
    } break;
case 18:
# line 128 "gen.y"
{
			char	buf[256];
			sprintf (buf,"%s%s", yypvt[-1].text, yypvt[-0].text);
      if (add_parent(yy_class_name, buf, PRIVATE_SCOPE) == BERROR)
        YYABORT;
    } break;
case 19:
# line 134 "gen.y"
{
			char	buf[256];
			sprintf (buf,"%s%s", yypvt[-1].text, yypvt[-0].text);
      if (add_parent(yy_class_name, buf, PRIVATE_SCOPE) == BERROR)
        YYABORT;
    } break;
case 23:
# line 155 "gen.y"
{
      yy_scope = PUBLIC_SCOPE;
    } break;
case 24:
# line 158 "gen.y"
{
      yy_scope = PROTECTED_SCOPE;
    } break;
case 25:
# line 161 "gen.y"
{
      yy_scope = PRIVATE_SCOPE;
    } break;
case 26:
# line 164 "gen.y"
{
      strcpy(yyval.text, yypvt[-4].text);
      strcat(yyval.text, "(");
      strcat(yyval.text, yypvt[-2].text);
      strcat(yyval.text, ")");
      strcat(yyval.text, yypvt[-0].text);
      if (add_method_decl(yy_class_name, yyval.text,
                          yy_scope, yypvt[-4].is_virtual, yypvt[-4].position) == BERROR)
        YYABORT;
    } break;
case 27:
# line 174 "gen.y"
{
      strcpy(yyval.text, yypvt[-4].text);
      strcat(yyval.text, "(");
      strcat(yyval.text, yypvt[-2].text);
      strcat(yyval.text, ")");
      strcat(yyval.text, yypvt[-0].text);
      if (add_method_decl(yy_class_name, yyval.text,
                          yy_scope, yypvt[-4].is_virtual, yypvt[-4].position) == BERROR)
        YYABORT;
      (void) add_method_impl(yy_class_name, yyval.text, yypvt[-4].position);
    } break;
case 30:
# line 187 "gen.y"
{
      fprintf(stderr, ">>>>>>  Arguments buffer overflow at line %d\n", yypvt[-0].position);
      YYABORT;
    } break;
case 31:
# line 197 "gen.y"
{
      } break;
case 32:
# line 199 "gen.y"
{
      } break;
case 33:
# line 201 "gen.y"
{
      } break;
case 34:
# line 203 "gen.y"
{
      } break;
case 35:
# line 205 "gen.y"
{
      strcpy(yyval.text, yypvt[-4].text);
      strcat(yyval.text, "(");
      strcat(yyval.text, yypvt[-2].text);
      strcat(yyval.text, ")");
      strcat(yyval.text, yypvt[-0].text);
      if (yy_class_name[0] == '\0') {
        (void) add_proc_impl(yyval.text, CPLUS_PROC, yypvt[-4].position);
      }
      else {
        if (add_method_impl(yy_class_name, yyval.text, yypvt[-4].position) == BERROR)
          YYABORT;
      }
      *yy_class_name = '\0';
    } break;
case 36:
# line 220 "gen.y"
{
      strcpy(yyval.text, yypvt[-0].text);
      strcat(yyval.text, "()");
      (void) add_proc_impl(yyval.text, C_PROC, yypvt[-0].position);
      *yy_class_name = '\0';
    } break;
case 37:
# line 226 "gen.y"
{
      fprintf(stderr, ">>>>>>  Arguments buffer overflow at line %d\n", yypvt[-0].position);
      YYABORT;
    } break;
case 38:
# line 236 "gen.y"
{ yyval.text[0] = '\0';         } break;
case 39:
# line 237 "gen.y"
{ strcpy(yyval.text, " const"); } break;
case 40:
# line 244 "gen.y"
{ yyval.text[0] = '\0';        } break;
case 41:
# line 245 "gen.y"
{ strcpy(yyval.text, yypvt[-0].text); } break;
case 42:
# line 246 "gen.y"
{
                                   strcpy(yyval.text, yypvt[-1].text);
                                   strcat(yyval.text, " ...");
                                 } break;
case 43:
# line 250 "gen.y"
{
                                   strcpy(yyval.text, yypvt[-2].text);
                                   strcat(yyval.text, ", ");
                                   strcat(yyval.text, yypvt[-0].text);
                                 } break;
case 44:
# line 255 "gen.y"
{
                                   strcpy(yyval.text, yypvt[-1].text);
                                   strcat(yyval.text, ", ");
                                 } break;
case 45:
# line 265 "gen.y"
{ 
      strcpy(yyval.text, yypvt[-1].text);
      strcat(yyval.text, yypvt[-0].text);
    } break;
case 46:
# line 269 "gen.y"
{ 
      strcpy(yyval.text, "const ");
      strcat(yyval.text, yypvt[-1].text);
      strcat(yyval.text, yypvt[-0].text);
    } break;
case 47:
# line 274 "gen.y"
{ 
      strcpy(yyval.text, "const ");
      strcat(yyval.text, yypvt[-2].text);
      strcat(yyval.text, yypvt[-0].text);
    } break;
case 48:
# line 285 "gen.y"
{ strcpy(yyval.text, "char"); } break;
case 49:
# line 286 "gen.y"
{ strcpy(yyval.text, "unsigned char"); } break;
case 50:
# line 287 "gen.y"
{ strcpy(yyval.text, "char"); } break;
case 51:
# line 288 "gen.y"
{ strcpy(yyval.text, "short"); } break;
case 52:
# line 289 "gen.y"
{ strcpy(yyval.text, "unsigned short"); } break;
case 53:
# line 290 "gen.y"
{ strcpy(yyval.text, "short"); } break;
case 54:
# line 291 "gen.y"
{ strcpy(yyval.text, "int"); } break;
case 55:
# line 292 "gen.y"
{ strcpy(yyval.text, "unsigned int"); } break;
case 56:
# line 293 "gen.y"
{ strcpy(yyval.text, "int"); } break;
case 57:
# line 294 "gen.y"
{ strcpy(yyval.text, "unsigned int"); } break;
case 58:
# line 295 "gen.y"
{ strcpy(yyval.text, "int"); } break;
case 59:
# line 296 "gen.y"
{ strcpy(yyval.text, "long"); } break;
case 60:
# line 297 "gen.y"
{ strcpy(yyval.text, "unsigned long"); } break;
case 61:
# line 298 "gen.y"
{ strcpy(yyval.text, "long"); } break;
case 62:
# line 299 "gen.y"
{ strcpy(yyval.text, yypvt[-0].text); } break;
case 63:
# line 300 "gen.y"
{ strcpy(yyval.text, yypvt[-0].text); } break;
case 64:
# line 301 "gen.y"
{ strcpy(yyval.text, yypvt[-0].text); } break;
case 65:
# line 302 "gen.y"
{ strcpy(yyval.text, yypvt[-0].text); } break;
case 66:
# line 303 "gen.y"
{
				   strcpy(yyval.text, yypvt[-1].text);
				   strcat(yyval.text, yypvt[-0].text);
				 } break;
case 67:
# line 311 "gen.y"
{ yyval.text[0] = '\0'; } break;
case 68:
# line 312 "gen.y"
{
					   sprintf(yyval.text, "<%s%s>",
					   yypvt[-3].text, yypvt[-2].text);
				         } break;
case 69:
# line 318 "gen.y"
{ yyval.text[0] = '\0'; } break;
case 70:
# line 319 "gen.y"
{
					   sprintf(yyval.text, "%s%s, ",
					   yypvt[-2].text, yypvt[-1].text);
					 } break;
case 71:
# line 328 "gen.y"
{ 
      yyval.text[0]    = '\0';
      yyval.precedence = MAX_PRECEDENCE;
    } break;
case 72:
# line 332 "gen.y"
{ 
      yyval.text[0]    = '\0';
      yyval.precedence = MAX_PRECEDENCE;
    } break;
case 73:
# line 336 "gen.y"
{
      strcpy(yyval.text, "*");
      parentheses(S_PRECEDENCE, yypvt[-0], yyval)
    } break;
case 74:
# line 340 "gen.y"
{
      strcpy(yyval.text, "*const ");
      parentheses(S_PRECEDENCE, yypvt[-1], yyval)
    } break;
case 75:
# line 344 "gen.y"
{
      strcpy(yyval.text, "&");
      parentheses(P_PRECEDENCE, yypvt[-0], yyval)
    } break;
case 76:
# line 348 "gen.y"
{
      strcpy(yyval.text, "&const ");
      parentheses(P_PRECEDENCE, yypvt[-1], yyval)
    } break;
case 77:
# line 352 "gen.y"
{
      yyval.text[0] = '\0';
      parentheses(A_PRECEDENCE, yypvt[-1], yyval)
      strcat(yyval.text, "[]");
    } break;
case 78:
# line 357 "gen.y"
{
      strcpy(yyval.text, "(");
      strcat(yyval.text, yypvt[-4].text);
      strcat(yyval.text, ")");
      strcat(yyval.text, " (");
      strcat(yyval.text, yypvt[-1].text);
      strcat(yyval.text, ")");
      yyval.precedence = MAX_PRECEDENCE;
    } break;
case 79:
# line 366 "gen.y"
{
      strcpy(yyval.text, yypvt[-1].text);
      yyval.precedence = yypvt[-1].precedence;
    } break;
	}
	goto yystack;		/* reset registers in driver code */
}
