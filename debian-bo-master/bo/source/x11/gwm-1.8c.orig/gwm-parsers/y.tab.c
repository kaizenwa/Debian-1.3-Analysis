
# line 2 "wool.yac"
/* Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
 * Copyright 1989 Massachusetts Institute of Technology
 */
/***************************\
* 			    *
*  Yacc grammar for Wool   *
* 			    *
\***************************/

#include <stdio.h>
#include <ctype.h>
#include "EXTERN.h"
#include "wool.h"
#include "wl_atom.h"
#include "wl_coll.h"
#include "wl_list.h"
#include "INTERN.h"
#include "yacc.h"
#if defined SVR4
#define SYSV
#endif
#ifdef SYSV
#include <string.h>
#else /* SYSV */
#include <strings.h>
#endif /* SYSV */


# line 31 "wool.yac"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
{
    WOOL_OBJECT wool_object;
    WOOL_Collection wool_collection;
} YYSTYPE;
# define END_OF_FILE 257
# define STRING 258
# define NON_CLOSED_STRING 259
# define QUOTECHAR 260
# define NUMBER 261
# define HEX_NUMBER 262
# define LEFTPAR 263
# define RIGHTPAR 264
# define LEFTBRA 265
# define RIGHTBRA 266
# define NAME 267

#include <malloc.h>
#include <memory.h>
#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 93 "wool.yac"


#include "lex.yy.c"	/* lexical created by lex */

yyerror(s)
char *s;
{
    wool_error(SYNTAX_ERROR, s);
}

/*
 * Some routines to deal with strings:
 */

/*
 * strip_string strips strings from \, ", etc... 
 * copies string raw_string to string stripped_string
 */

static char *stripped_string;
static int   stripped_string_limit;

char *strip_string(raw_string)
char *raw_string;
{
    register char  *p, *q, c;
    int num;

    if ((int) strlen(raw_string) > stripped_string_limit) {
	stripped_string_limit = strlen(raw_string);
	stripped_string = (char *)
	    Realloc(stripped_string, stripped_string_limit);
    }
    for (p = raw_string + 1, q = stripped_string; *p; p++, q++) {
	switch (*p) {
	case '"':
	    *q = '\0';
	    break;
	case '\\':
	    switch (*(++p)) {
	    case '\n':
		q--;
		break;
	    case 'n':
		*q = '\n';
		break;
	    case 'r':
		*q = '\r';
		break;
	    case 't':
		*q = '\t';
		break;
	    case 'e':
		*q = '\033';
		break;
	    case 'x':
		sscanf(++p,"%2x",&num);
		*q = num;
		p++;
		break;
	    default:
		if ((*p <= '9') && (*p >= '0')) {
		    *q = (char)
			(*p - '0') * 64 + (*(++p) - '0') * 8 + *(++p) - '0';
		} else {
		    *q = *p;
		}
	    }
	    break;
	default:
	    *q = *p;
	}
    }
    return stripped_string;
}
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 16
# define YYLAST 53
yytabelem yyact[]={

    13,    19,    10,    12,    11,     8,     9,     5,    18,     6,
     0,     7,    13,     4,    10,    12,    11,     8,     9,     5,
     3,     6,    13,     7,    10,    12,    11,     8,     9,     5,
     2,     6,    21,     7,    13,     0,    10,    12,    11,     8,
     9,     5,    17,     6,    15,     7,    20,    20,     1,     0,
     0,    16,    14 };
yytabelem yypact[]={

  -244,-10000000,-10000000,  -244,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,  -222,-10000000,-10000000,-10000000,  -256,  -234,-10000000,-10000000,-10000000,
-10000000,-10000000 };
yytabelem yypgo[]={

     0,    30,    48,    44 };
yytabelem yyr1[]={

     0,     2,     2,     2,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     3,     3 };
yytabelem yyr2[]={

     0,     3,     5,     3,     7,     7,     3,     3,     3,     3,
     5,     3,     7,     3,     5,     1 };
yytabelem yychk[]={

-10000000,    -2,    -1,   264,   257,   263,   265,   267,   261,   262,
   258,   260,   259,   256,    -2,    -3,    -3,    -1,   264,   257,
    -1,   266 };
yytabelem yydef[]={

     0,    -2,     1,     0,     3,    15,    15,     6,     7,     8,
     9,     0,    11,    13,     2,     0,     0,    10,     4,    12,
    14,     5 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"END_OF_FILE",	257,
	"STRING",	258,
	"NON_CLOSED_STRING",	259,
	"QUOTECHAR",	260,
	"NUMBER",	261,
	"HEX_NUMBER",	262,
	"LEFTPAR",	263,
	"RIGHTPAR",	264,
	"LEFTBRA",	265,
	"RIGHTBRA",	266,
	"NAME",	267,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"main_s_exp : s_expression",
	"main_s_exp : RIGHTPAR main_s_exp",
	"main_s_exp : END_OF_FILE",
	"s_expression : LEFTPAR list_of_s_expressions RIGHTPAR",
	"s_expression : LEFTBRA list_of_s_expressions RIGHTBRA",
	"s_expression : NAME",
	"s_expression : NUMBER",
	"s_expression : HEX_NUMBER",
	"s_expression : STRING",
	"s_expression : QUOTECHAR s_expression",
	"s_expression : NON_CLOSED_STRING",
	"s_expression : LEFTPAR list_of_s_expressions END_OF_FILE",
	"s_expression : error",
	"list_of_s_expressions : list_of_s_expressions s_expression",
	"list_of_s_expressions : /* empty */",
};
#endif /* YYDEBUG */
/*
 * Copyright (c) 1993 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)yaccpar	6.12	93/06/07 SMI"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
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
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
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
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

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

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
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
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

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
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
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
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
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
					printf( "%s\n", yytoks[yy_i].t_name );
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
			skip_init:
				yynerrs++;
				/* FALLTHRU */
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
						printf( _POP_, *yy_ps,
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

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
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
						printf( "token %s\n",
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
			printf( "Reduce by (%d) \"%s\"\n",
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
		
case 1:
# line 48 "wool.yac"
{yyval.wool_object = wool_read_expr = yypvt[-0].wool_object;
			 YYACCEPT;} break;
case 2:
# line 51 "wool.yac"
{yyval.wool_object = wool_read_expr = yypvt[-0].wool_object;
			 YYACCEPT;} break;
case 3:
# line 54 "wool.yac"
{yyval.wool_object = wool_read_expr = NULL;
			 YYACCEPT;} break;
case 4:
# line 59 "wool.yac"
{yyval.wool_object = (WOOL_OBJECT) WLList_make(yypvt[-1].wool_collection);} break;
case 5:
# line 61 "wool.yac"
{yyval.wool_object = WLCollection_progn(yypvt[-1].wool_collection);} break;
case 6:
# line 63 "wool.yac"
{yyval.wool_object = (WOOL_OBJECT) wool_atom(yytext);} break;
case 7:
# line 65 "wool.yac"
{yyval.wool_object = (WOOL_OBJECT) 
			   WLNumber_make((Num) atoi(yytext));} break;
case 8:
# line 68 "wool.yac"
{int num;
			 sscanf(yytext+2,"%x",&num);
			 yyval.wool_object = (WOOL_OBJECT) WLNumber_make((Num) num);} break;
case 9:
# line 72 "wool.yac"
{yyval.wool_object = (WOOL_OBJECT)
			    WLString_make(strip_string(yytext));} break;
case 10:
# line 75 "wool.yac"
{yyval.wool_object = (WOOL_OBJECT) WLQuotedExpr_make(yypvt[-0].wool_object);} break;
case 11:
# line 77 "wool.yac"
{yyval.wool_object = NIL;
		    yyerror("Non closed string");} break;
case 12:
# line 80 "wool.yac"
{yyval.wool_object = NIL;
			yyerror("Lacking \")\" at the end of file!");} break;
case 13:
# line 83 "wool.yac"
{yyval.wool_object = NIL;} break;
case 14:
# line 87 "wool.yac"
{yyval.wool_collection = WLCollection_add(yypvt[-1].wool_collection, yypvt[-0].wool_object);} break;
case 15:
# line 89 "wool.yac"
{yyval.wool_collection = WLCollection_make();} break;
	}
	goto yystack;		/* reset registers in driver code */
}

