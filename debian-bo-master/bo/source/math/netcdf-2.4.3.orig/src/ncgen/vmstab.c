/* yacc -D vmstab.h -o vmstab.c ncgen.y */
#ifdef YYTRACE
#define YYDEBUG 1
#else
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#endif
/*
 * Portable way of defining ANSI C prototypes
 */
#ifndef YY_ARGS
#if defined(__STDC__) || defined(__cplusplus)  
#define YY_ARGS(x)	x
#define YY_FNARG1(type, name) (type name)       
#define	YY_FNARG4(t1, n1, t2, n2, t3, n3, t4, n4) \
		(t1 n1, t2 n2, t3 n3, t4 n4)
#else
#define YY_ARGS(x)	()
#define YY_FNARG1(type, name) (name) type name; 
#define YY_FNARG4(t1, n1, t2, n2, t3, n3, t4, n4) \
		(n1, n2, n3, n4) t1 n1; t2 n2; t3 n3; t4 n4;
#endif
#endif

#if YYDEBUG
typedef struct yyNamedType_tag {	/* Tokens */
	char	* name;		/* printable name */
	short	token;		/* token # */
	short	type;		/* token type */
} yyNamedType;
typedef struct yyTypedRules_tag {	/* Typed rule table */
	char	* name;		/* compressed rule string */
	short	type;		/* rule result type */
} yyTypedRules;

#endif

#line 9 "ncgen.y"

#ifndef lint
static char SccsId[] = "$Id: vmstab.c,v 1.21 1996/05/02 15:43:03 steve Exp $";
#endif
#include        <string.h>
#include	<stdlib.h>
#include	<netcdf.h>
#include 	"generic.h"
#include        "ncgen.h"
#include	"genlib.h"	/* for grow_darray() et al */

typedef struct Symbol {		/* symbol table entry */
	char    	*name;
	struct Symbol   *next;
	unsigned	is_dim : 1;	/* appears as netCDF dimension */
	unsigned	is_var : 1;	/* appears as netCDF variable */
	unsigned	is_att : 1;	/* appears as netCDF attribute */
	int             dnum;	        /* handle as a dimension */
	int             vnum;	        /* handle as a variable */
	} *YYSTYPE1;

/* True if string a equals string b*/
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

#define YYSTYPE YYSTYPE1
YYSTYPE install(), lookup();
YYSTYPE symlist;		/* symbol table: linked list */

int  put_variable();
void init_netcdf();		/* initializes netcdf counts (e.g. nvars) */
void define_netcdf();		/* generates all define mode stuff */
void load_netcdf();		/* generates variable puts */
void close_netcdf();		/* generates close */

void *emalloc(), *erealloc();	/* malloc that checks for memory exhausted */
void clearout();		/* initializes symbol table */
void nc_getfill();		/* to get fill value for various types */
void nc_putfill();		/* to get fill value for various types */
char *nctype();		/* returns type name from number */
void nc_fill();		/* fills a generic array with a value */

extern int derror_count;	/* counts errors in netcdf definition */
extern int lineno;		/* line number for error messages */

static int not_a_string;	/* whether last constant read was a string */
static char termstring[MAXTRST]; /* last terminal string read */
static double double_val;	/* last double value read */
static float float_val;		/* last float value read */
static long long_val;		/* last long value read */
static short short_val;		/* last short value read */
static char char_val;		/* last char value read */
static char byte_val;		/* last byte value read */

static nc_type type_code;	/* holds declared type for variables */
static nc_type atype_code;	/* holds derived type for attributes */
static char *netcdfname;	/* to construct netcdf file name */
static void *att_space;		/* pointer to block for attribute values */
static nc_type valtype;		/* type code for list of attribute values  */

static char *char_valp;		/* pointers used to accumulate data values */
static char *byte_valp;
static short *short_valp;
static nclong *long_valp;
static float *float_valp;
static double *double_valp;
static void *rec_cur;		/* pointer to where next data value goes */
static void *rec_start;		/* start of space for a record of data */
#define NC_UNLIMITED_K	257
#define BYTE_K	258
#define CHAR_K	259
#define SHORT_K	260
#define LONG_K	261
#define FLOAT_K	262
#define DOUBLE_K	263
#define IDENT	264
#define TERMSTRING	265
#define BYTE_CONST	266
#define CHAR_CONST	267
#define SHORT_CONST	268
#define LONG_CONST	269
#define FLOAT_CONST	270
#define DOUBLE_CONST	271
#define DIMENSIONS	272
#define VARIABLES	273
#define NETCDF	274
#define DATA	275
#define FILLVALUE	276
extern int yychar, yyerrflag;
#ifndef YYSTYPE
#define YYSTYPE int
#endif
extern YYSTYPE yylval;
#if YYDEBUG
yyTypedRules yyRules[] = {
	{ "&00: %01 &00",  0},
	{ "%02:",  0},
	{ "%05:",  0},
	{ "%01: &19 &22 %02 %03 %04 %05 %06 &23",  0},
	{ "%03:",  0},
	{ "%03: &17 %07",  0},
	{ "%07: %08 &24",  0},
	{ "%07: %07 %08 &24",  0},
	{ "%08: %09",  0},
	{ "%08: %08 &25 %09",  0},
	{ "%09: %10 &26 &14",  0},
	{ "%09: %10 &26 &02",  0},
	{ "%10: %11",  0},
	{ "%11: &09",  0},
	{ "%04:",  0},
	{ "%04: &18 %12",  0},
	{ "%12: %13 &24",  0},
	{ "%12: %12 %13 &24",  0},
	{ "%13: %14",  0},
	{ "%13: %15",  0},
	{ "%14: %16 %17",  0},
	{ "%16: &03",  0},
	{ "%16: &04",  0},
	{ "%16: &05",  0},
	{ "%16: &06",  0},
	{ "%16: &07",  0},
	{ "%16: &08",  0},
	{ "%17: %18",  0},
	{ "%17: %17 &25 %18",  0},
	{ "%20:",  0},
	{ "%18: %19 %20 %21",  0},
	{ "%19: &09",  0},
	{ "%21:",  0},
	{ "%21: &27 %22 &28",  0},
	{ "%22: %23",  0},
	{ "%22: %22 &25 %23",  0},
	{ "%23: %11",  0},
	{ "%25:",  0},
	{ "%15: %24 %25 &26 %26",  0},
	{ "%24: %27 &29 %28",  0},
	{ "%24: &29 %28",  0},
	{ "%27: %19",  0},
	{ "%28: &09",  0},
	{ "%26: %29",  0},
	{ "%26: %26 &25 %29",  0},
	{ "%29: %30",  0},
	{ "%30: &12",  0},
	{ "%30: &10",  0},
	{ "%30: &11",  0},
	{ "%30: &13",  0},
	{ "%30: &14",  0},
	{ "%30: &15",  0},
	{ "%30: &16",  0},
	{ "%06:",  0},
	{ "%06: &20 %31",  0},
	{ "%31: %32 &24",  0},
	{ "%31: %31 %32 &24",  0},
	{ "%33:",  0},
	{ "%32: %27 %33 &26 %34",  0},
	{ "%34: %35",  0},
	{ "%34: %34 &25 %35",  0},
	{ "%36:",  0},
	{ "%35: %36 %37",  0},
	{ "%37: &12",  0},
	{ "%37: &10",  0},
	{ "%37: &11",  0},
	{ "%37: &13",  0},
	{ "%37: &14",  0},
	{ "%37: &15",  0},
	{ "%37: &16",  0},
	{ "%37: &21",  0},
{ "$accept",  0},{ "error",  0}
};
yyNamedType yyTokenTypes[] = {
	{ "$end",  0,  0},
	{ "error",  256,  0},
	{ "NC_UNLIMITED_K",  257,  0},
	{ "BYTE_K",  258,  0},
	{ "CHAR_K",  259,  0},
	{ "SHORT_K",  260,  0},
	{ "LONG_K",  261,  0},
	{ "FLOAT_K",  262,  0},
	{ "DOUBLE_K",  263,  0},
	{ "IDENT",  264,  0},
	{ "TERMSTRING",  265,  0},
	{ "BYTE_CONST",  266,  0},
	{ "CHAR_CONST",  267,  0},
	{ "SHORT_CONST",  268,  0},
	{ "LONG_CONST",  269,  0},
	{ "FLOAT_CONST",  270,  0},
	{ "DOUBLE_CONST",  271,  0},
	{ "DIMENSIONS",  272,  0},
	{ "VARIABLES",  273,  0},
	{ "NETCDF",  274,  0},
	{ "DATA",  275,  0},
	{ "FILLVALUE",  276,  0},
	{ "'{'",  123,  0},
	{ "'}'",  125,  0},
	{ "';'",  59,  0},
	{ "','",  44,  0},
	{ "'='",  61,  0},
	{ "'('",  40,  0},
	{ "')'",  41,  0},
	{ "':'",  58,  0}

};
#endif
static short yydef[] = {

	  -1,   -5,  -13,   71,   69,  -19,   68,  -23,   65,    3, 
	   4,   14
};
static short yyex[] = {

	   0,    0,   -1,    1,  273,   72,  275,   72,  125,   72, 
	  -1,    1,  275,   70,  125,   70,   -1,    1,  125,   66, 
	  -1,    1,   59,   67,   44,   67,   -1,    1
};
static short yyact[] = {

	 -39,  274,  -78,  123,  -38,  272,  -83,  264,  -35,  273, 
	 -34,   61,  -33,  -79,   59,   44,  -31,  -72,  -71,  -70, 
	 -69,  -68,  -67,  -90,  264,  263,  262,  261,  260,  259, 
	 258,   58,  -74,  -75,  269,  257,  -33,  -80,   59,   44, 
	 -60,  264,  -27,   58,  -90,  264,  -84,   59,  -24,  275, 
	 -22,   61,  -21,   44,  -85,   59,  -76,  125,  -57,  -56, 
	 -58,  -55,  -54,  -53,  -52,  271,  270,  269,  268,  267, 
	 266,  265,  -19,   40,  -97,   59,  -16,   44,  -50,   61, 
	 -98,   59,  -91,  -14,   44,   41,  -47,  -46,  -48,  -45, 
	 -44,  -43,  -42,  -41,  276,  271,  270,  269,  268,  267, 
	 266,  265,  -12,   44,   -1
};
static short yypact[] = {

	   4,    5,    9,    7,   24,   49,   53,   73,   45,   77, 
	 103,  104,   94,    7,   84,   65,   81,   79,    7,   75, 
	  45,   65,   57,   45,   55,   51,   41,   47,   45,   43, 
	  41,   38,    7,   34,   24,   14,   11,    7,    3,    1
};
static short yygo[] = {

	  -1,   -2,   -3,  -77,   -6,  -23,   -4,  -32,  -36,    3, 
	 -82,  -81,   32,  -37,  -64,  -64,  -73,   18,   13,   -5, 
	 -25,  -28,    4,  -86,  -87,  -29,   -7,  -89,  -88,   20, 
	 -66,  -66,  -61,   28,   20,   -8,  -65,  -15,  -93,  -92, 
	  13,  -63,  -26,  -10,  -51,  -51,  -30,   23,    8,  -94, 
	 -62,   26,  -96,  -95,   15,  -59,   -9,  -17,  -20,    8, 
	 -18,  -11, -100,  -99,   11,  -13,  -49,   -1
};
static short yypgo[] = {

	   0,    0,    0,   24,   58,   66,   66,   66,   66,   66, 
	  66,   66,   66,   63,   65,   60,   55,   55,   55,   55, 
	  55,   55,   55,   53,   50,   46,   41,   42,   39,   28, 
	  35,   25,   25,   25,   25,   25,   25,   13,   11,   11, 
	   0,    4,    1,    6,    6,    8,    8,   16,   19,   19, 
	  21,   21,   26,   26,   32,   36,   37,   37,   41,   43, 
	  43,   56,   56,   61,   61,    5,    5,   36,   23,    3, 
	   3,    2,    2,    0
};
static short yyrlen[] = {

	   0,    0,    0,    4,    4,    1,    1,    1,    1,    1, 
	   1,    1,    1,    2,    0,    0,    1,    1,    1,    1, 
	   1,    1,    1,    1,    1,    1,    2,    0,    1,    3, 
	   0,    1,    1,    1,    1,    1,    1,    1,    3,    3, 
	   8,    0,    0,    2,    3,    1,    3,    1,    2,    3, 
	   1,    1,    1,    3,    1,    3,    1,    3,    3,    1, 
	   3,    2,    3,    1,    3,    2,    0,    0,    2,    2, 
	   0,    2,    0,    2
};
#define YYS0	39
#define YYDELTA	35
#define YYNPACT	40
#define YYNDEF	12

#define YYr71	0
#define YYr72	1
#define YYr73	2
#define YYr38	3
#define YYr58	4
#define YYr70	5
#define YYr69	6
#define YYr68	7
#define YYr67	8
#define YYr66	9
#define YYr65	10
#define YYr64	11
#define YYr63	12
#define YYr62	13
#define YYr61	14
#define YYr57	15
#define YYr52	16
#define YYr51	17
#define YYr50	18
#define YYr49	19
#define YYr48	20
#define YYr47	21
#define YYr46	22
#define YYr45	23
#define YYr42	24
#define YYr41	25
#define YYr40	26
#define YYr37	27
#define YYr36	28
#define YYr30	29
#define YYr29	30
#define YYr26	31
#define YYr25	32
#define YYr24	33
#define YYr23	34
#define YYr22	35
#define YYr21	36
#define YYr12	37
#define YYr11	38
#define YYr10	39
#define YYr3	40
#define YYr2	41
#define YYr1	42
#define YYrACCEPT	YYr71
#define YYrERROR	YYr72
#define YYrLR2	YYr73
#if YYDEBUG
char * yystoken[] = {

	"error",
	"NC_UNLIMITED_K",
	"BYTE_K",
	"CHAR_K",
	"SHORT_K",
	"LONG_K",
	"FLOAT_K",
	"DOUBLE_K",
	"IDENT",
	"TERMSTRING",
	"BYTE_CONST",
	"CHAR_CONST",
	"SHORT_CONST",
	"LONG_CONST",
	"FLOAT_CONST",
	"DOUBLE_CONST",
	"DIMENSIONS",
	"VARIABLES",
	"NETCDF",
	"DATA",
	"FILLVALUE",
	0
};
char * yysvar[] = {
	"$accept",
	"ncdesc",
	"$1",
	"dimsection",
	"vasection",
	"$2",
	"datasection",
	"dimdecls",
	"dimdecline",
	"dimdecl",
	"dimd",
	"dim",
	"vadecls",
	"vadecl",
	"vardecl",
	"attdecl",
	"type",
	"varlist",
	"varspec",
	"var",
	"$29",
	"dimspec",
	"dimlist",
	"vdim",
	"att",
	"$37",
	"attvallist",
	"avar",
	"attr",
	"aconst",
	"attconst",
	"datadecls",
	"datadecl",
	"$57",
	"constlist",
	"dconst",
	"$61",
	"const",
	0
};
short yyrmap[] = {

	  71,   72,   73,   38,   58,   70,   69,   68,   67,   66, 
	  65,   64,   63,   62,   61,   57,   52,   51,   50,   49, 
	  48,   47,   46,   45,   42,   41,   40,   37,   36,   30, 
	  29,   26,   25,   24,   23,   22,   21,   12,   11,   10, 
	   3,    2,    1,    6,    7,    8,    9,   13,   16,   17, 
	  18,   19,   27,   28,   31,   33,   34,   35,   39,   43, 
	  44,   55,   56,   59,   60,   54,   53,   32,   20,   15, 
	  14,    5,    4,    0
};
short yysmap[] = {

	   2,    4,    6,   12,   34,   35,   46,   53,   58,   69, 
	  87,   98,   85,   83,   79,   76,   75,   73,   70,   57, 
	  54,   52,   50,   49,   48,   43,   42,   33,   30,   21, 
	  20,   18,   16,   15,   13,   11,    9,    5,    1,    0, 
	  89,   90,   91,   92,   93,   94,   95,   96,   97,   80, 
	  56,   60,   61,   62,   63,   64,   65,   66,   67,   40, 
	  19,   41,   22,   77,   71,   44,   24,   25,   26,   27, 
	  28,   29,    8,   36,   37,   59,   14,    3,   17,   39, 
	  10,   38,    7,   47,   55,   32,   31,   45,   72,   23, 
	  84,   78,   88,   51,   68,   82,   74,   81,   86,   99
};
int yyntoken = 30;
int yynvar = 38;
int yynstate = 100;
int yynrule = 74;
#endif

#if YYDEBUG
/*
 * Package up YACC context for tracing
 */
typedef struct yyTraceItems_tag {
	int	state, lookahead, errflag, done;
	int	rule, npop;
	short	* states;
	int	nstates;
	YYSTYPE * values;
	int	nvalues;
	short	* types;
} yyTraceItems;
#endif

#line 2 "/etc/yyparse.c"

/*
 * Copyright 1985, 1990 by Mortice Kern Systems Inc.  All rights reserved.
 * 
 * Automaton to interpret LALR(1) tables.
 *
 * Macros:
 *	yyclearin - clear the lookahead token.
 *	yyerrok - forgive a pending error
 *	YYERROR - simulate an error
 *	YYACCEPT - halt and return 0
 *	YYABORT - halt and return 1
 *	YYRETURN(value) - halt and return value.  You should use this
 *		instead of return(value).
 *	YYREAD - ensure yychar contains a lookahead token by reading
 *		one if it does not.  See also YYSYNC.
 *	YYRECOVERING - 1 if syntax error detected and not recovered
 *		yet; otherwise, 0.
 *
 * Preprocessor flags:
 *	YYDEBUG - includes debug code if 1.  The parser will print
 *		 a travelogue of the parse if this is defined as 1
 *		 and yydebug is non-zero.
 *		yacc -t sets YYDEBUG to 1, but not yydebug.
 *	YYTRACE - turn on YYDEBUG, and undefine default trace functions
 *		so that the interactive functions in 'ytrack.c' will
 *		be used.
 *	YYSSIZE - size of state and value stacks (default 150).
 *	YYSTATIC - By default, the state stack is an automatic array.
 *		If this is defined, the stack will be static.
 *		In either case, the value stack is static.
 *	YYALLOC - Dynamically allocate both the state and value stacks
 *		by calling malloc() and free().
 *	YYSYNC - if defined, yacc guarantees to fetch a lookahead token
 *		before any action, even if it doesnt need it for a decision.
 *		If YYSYNC is defined, YYREAD will never be necessary unless
 *		the user explicitly sets yychar = -1
 *
 * Copyright (c) 1983, by the University of Waterloo
 */
/*
 * Prototypes
 */

extern int yylex YY_ARGS((void));

#if YYDEBUG

#include <stdlib.h>		/* common prototypes */
#include <string.h>
#if defined(__cplusplus)
extern "C"  {
#endif
extern char *	yyValue YY_ARGS((YYSTYPE, int));	/* print yylval */
extern void yyShowState YY_ARGS((yyTraceItems *));
extern void yyShowReduce YY_ARGS((yyTraceItems *));
extern void yyShowGoto YY_ARGS((yyTraceItems *));
extern void yyShowShift YY_ARGS((yyTraceItems *));
extern void yyShowErrRecovery YY_ARGS((yyTraceItems *));
extern void yyShowErrDiscard YY_ARGS((yyTraceItems *));

extern void yyShowRead YY_ARGS((int));
#if defined(__cplusplus)
}
#endif
#endif

/*
 * If YYDEBUG defined and yydebug set,
 * tracing functions will be called at appropriate times in yyparse()
 * Pass state of YACC parse, as filled into yyTraceItems yyx
 * If yyx.done is set by the tracing function, yyparse() will terminate
 * with a return value of -1
 */
#define YY_TRACE(fn) { \
	yyx.state = yystate; yyx.lookahead = yychar; yyx.errflag =yyerrflag; \
	yyx.states = yys+1; yyx.nstates = yyps-yys; \
	yyx.values = yyv+1; yyx.nvalues = yypv-yyv; \
	yyx.types = yytypev+1; yyx.done = 0; \
	yyx.rule = yyi; yyx.npop = yyj; \
	fn(&yyx); \
	if (yyx.done) YYRETURN(-1); }

#ifndef I18N
#define m_textmsg(id, str, cls)	(str)
#else /*I18N*/
extern	char* m_textmsg YY_ARGS((int id, const char* str, char* cls));
#endif/*I18N*/

#ifndef YYSSIZE
# define YYSSIZE	150
#endif

#define YYERROR		goto yyerrlabel
#define yyerrok		yyerrflag = 0
#if YYDEBUG
#define yyclearin	{ if (yydebug) yyShowRead(-1); yychar = -1; }
#else
#define yyclearin	yychar = -1
#endif
#define YYACCEPT	YYRETURN(0)
#define YYABORT		YYRETURN(1)
#define YYRECOVERING()	(yyerrflag != 0)
#ifdef YYALLOC
# define YYRETURN(val)	{ retval = (val); goto yyReturn; }
#else
# define YYRETURN(val)	return(val)
#endif
#if YYDEBUG
/* The if..else makes this macro behave exactly like a statement */
# define YYREAD	if (yychar < 0) {					\
			if ((yychar = yylex()) < 0)			\
				yychar = 0;				\
			if (yydebug)					\
				yyShowRead(yychar);			\
		} else
#else
# define YYREAD	if (yychar < 0) {					\
			if ((yychar = yylex()) < 0)			\
				yychar = 0;				\
		} else
#endif

#define YYERRCODE	256		/* value of `error' */
#define	YYQYYP	yyq[yyq-yyp]

YYSTYPE	yyval;				/* $ */
YYSTYPE	*yypvt;				/* $n */
YYSTYPE	yylval;				/* yylex() sets this */

int	yychar,				/* current token */
	yyerrflag,			/* error flag */
	yynerrs;			/* error count */

#if YYDEBUG
int yydebug = 0;		/* debug if this flag is set */
extern char	*yysvar[];	/* table of non-terminals (aka 'variables') */
extern yyNamedType yyTokenTypes[];	/* table of terminals & their types */
extern short	yyrmap[], yysmap[];	/* map internal rule/states */
extern int	yynstate, yynvar, yyntoken, yynrule;

extern int	yyGetType YY_ARGS((int));	/* token type */
extern char	*yyptok YY_ARGS((int));	/* printable token string */
extern int	yyExpandName YY_ARGS((int, int, char *, int));
				  /* expand yyRules[] or yyStates[] */
static char *	yygetState YY_ARGS((int));

#define yyassert(condition, msg, arg) \
	if (!(condition)) { \
		printf(m_textmsg(2824, "\nyacc bug: ", "1")); \
		printf(msg, arg); \
		YYABORT; }
#else /* !YYDEBUG */
#define yyassert(condition, msg, arg)
#endif

#line 764 "ncgen.y"
/* PROGRAMS */

/* get lexical input routine generated by lex  */
#include "ncgenyy.c"

#ifdef vms
void
#else
int
#endif
yyerror(s)	/* called for yacc syntax error */
     char *s;
{
	derror(s);
#ifndef vms
	return -1;
#endif
}

/* undefine yywrap macro, in case we are using bison instead of yacc */
#ifdef yywrap
#undef yywrap
#endif

int
yywrap()			/* returns 1 on EOF if no more input */
{
    return  1;
}


/* Symbol table operations for ncgen tool */

YYSTYPE lookup(sname)       /* find sname in symbol table (linear search) */
char *sname;
{
    YYSTYPE sp;
    for (sp = symlist; sp != (YYSTYPE) 0; sp = sp -> next)
	if (STREQ(sp -> name, sname)) {
	    return sp;
	}
    return 0;			/* 0 ==> not found */
}

YYSTYPE install(sname)  /* install sname in symbol table */
char *sname;
{
    YYSTYPE sp;

    sp = (YYSTYPE) emalloc (sizeof (struct Symbol));
    sp -> name = (char *) emalloc (strlen (sname) + 1);/* +1 for '\0' */
    (void) strcpy (sp -> name, sname);
    sp -> next = symlist;	/* put at front of list */
    sp -> is_dim = 0;
    sp -> is_var = 0;
    sp -> is_att = 0;
    symlist = sp;
    return sp;
}

void
clearout()	/* reset symbol table to empty */
{
    YYSTYPE sp, tp;
    for (sp = symlist; sp != (YYSTYPE) 0;) {
	tp = sp -> next;
	free (sp -> name);
	free ((char *) sp);
	sp = tp;
    }
    symlist = 0;
}

yyparse()
{
	register short		yyi, *yyp;	/* for table lookup */
	register short		*yyps;		/* top of state stack */
	register short		yystate;	/* current state */
	register YYSTYPE	*yypv;		/* top of value stack */
	register short		*yyq;
	register int		yyj;
#if YYDEBUG
	yyTraceItems	yyx;			/* trace block */
	short	* yytp;
	int	yyruletype = 0;
#endif
#ifdef YYSTATIC
	static short	yys[YYSSIZE + 1];
	static YYSTYPE	yyv[YYSSIZE + 1];
#if YYDEBUG
	static short	yytypev[YYSSIZE+1];	/* type assignments */
#endif
#else /* ! YYSTATIC */
#ifdef YYALLOC
	YYSTYPE *yyv;
	short	*yys;
#if YYDEBUG
	short	*yytypev;
#endif
	YYSTYPE save_yylval;
	YYSTYPE save_yyval;
	YYSTYPE *save_yypvt;
	int save_yychar, save_yyerrflag, save_yynerrs;
	int retval;
#else
	short		yys[YYSSIZE + 1];
	static YYSTYPE	yyv[YYSSIZE + 1];	/* historically static */
#if YYDEBUG
	short	yytypev[YYSSIZE+1];		/* mirror type table */
#endif
#endif /* ! YYALLOC */
#endif /* ! YYSTATIC */


#ifdef YYALLOC
	yys = (short *) malloc((YYSSIZE + 1) * sizeof(short));
	yyv = (YYSTYPE *) malloc((YYSSIZE + 1) * sizeof(YYSTYPE));
#if YYDEBUG
	yytypev = (short *) malloc((YYSSIZE+1) * sizeof(short));
#endif
	if (yys == (short *)0 || yyv == (YYSTYPE *)0
#if YYDEBUG
		|| yytypev == (short *) 0
#endif
	) {
		yyerror("Not enough space for parser stacks");
		return 1;
	}
	save_yylval = yylval;
	save_yyval = yyval;
	save_yypvt = yypvt;
	save_yychar = yychar;
	save_yyerrflag = yyerrflag;
	save_yynerrs = yynerrs;
#endif

	yynerrs = 0;
	yyerrflag = 0;
	yyclearin;
	yyps = yys;
	yypv = yyv;
	*yyps = yystate = YYS0;		/* start state */
#if YYDEBUG
	yytp = yytypev;
	yyi = yyj = 0;			/* silence compiler warnings */
#endif

yyStack:
	yyassert((unsigned)yystate < yynstate, m_textmsg(587, "state %d\n", "1"), yystate);
	if (++yyps > &yys[YYSSIZE]) {
		yyerror("Parser stack overflow");
		YYABORT;
	}
	*yyps = yystate;	/* stack current state */
	*++yypv = yyval;	/* ... and value */
#if YYDEBUG
	*++yytp = yyruletype;	/* ... and type */

	if (yydebug)
		YY_TRACE(yyShowState)
#endif

	/*
	 *	Look up next action in action table.
	 */
yyEncore:
#ifdef YYSYNC
	YYREAD;
#endif
	if (yystate >= sizeof yypact/sizeof yypact[0]) 	/* simple state */
		yyi = yystate - YYDELTA;	/* reduce in any case */
	else {
		if(*(yyp = &yyact[yypact[yystate]]) >= 0) {
			/* Look for a shift on yychar */
#ifndef YYSYNC
			YYREAD;
#endif
			yyq = yyp;
			yyi = yychar;
			while (yyi < *yyp++)
				;
			if (yyi == yyp[-1]) {
				yystate = ~YYQYYP;
#if YYDEBUG
				if (yydebug) {
					yyruletype = yyGetType(yychar);
					YY_TRACE(yyShowShift)
				}
#endif
				yyval = yylval;	/* stack what yylex() set */
				yyclearin;		/* clear token */
				if (yyerrflag)
					yyerrflag--;	/* successful shift */
				goto yyStack;
			}
		}

		/*
	 	 *	Fell through - take default action
	 	 */

		if (yystate >= sizeof yydef /sizeof yydef[0])
			goto yyError;
		if ((yyi = yydef[yystate]) < 0)	 { /* default == reduce? */
			/* Search exception table */
			yyassert((unsigned)~yyi < sizeof yyex/sizeof yyex[0],
				m_textmsg(2825, "exception %d\n", "1"), yystate);
			yyp = &yyex[~yyi];
#ifndef YYSYNC
			YYREAD;
#endif
			while((yyi = *yyp) >= 0 && yyi != yychar)
				yyp += 2;
			yyi = yyp[1];
			yyassert(yyi >= 0,
				 m_textmsg(2826, "Ex table not reduce %d\n", "1"), yyi);
		}
	}

	yyassert((unsigned)yyi < yynrule, m_textmsg(2827, "reduce %d\n", "1"), yyi);
	yyj = yyrlen[yyi];
#if YYDEBUG
	if (yydebug)
		YY_TRACE(yyShowReduce)
	yytp -= yyj;
#endif
	yyps -= yyj;		/* pop stacks */
	yypvt = yypv;		/* save top */
	yypv -= yyj;
	yyval = yypv[1];	/* default action $ = $1 */
#if YYDEBUG
	yyruletype = yyRules[yyrmap[yyi]].type;
#endif

	switch (yyi) {		/* perform semantic action */
		
case YYr1: {	/* ncdesc :  NETCDF '{' */
#line 110 "ncgen.y"
 init_netcdf(); 
} break;

case YYr2: {	/* ncdesc :  NETCDF '{' $1 dimsection vasection */
#line 113 "ncgen.y"

		       if (derror_count == 0)
			 define_netcdf(netcdfname);
		   
} break;

case YYr3: {	/* ncdesc :  NETCDF '{' $1 dimsection vasection $2 datasection '}' */
#line 119 "ncgen.y"

		       if (derror_count == 0)
			 close_netcdf();
		   
} break;

case YYr10: {	/* dimdecl :  dimd '=' LONG_CONST */
#line 134 "ncgen.y"
 if (long_val <= 0)
			 derror("negative dimension size");
		     dims[ndims].size = long_val;
		     ndims++;
		   
} break;

case YYr11: {	/* dimdecl :  dimd '=' NC_UNLIMITED_K */
#line 140 "ncgen.y"
  if (rec_dim != -1)
			 derror("only one NC_UNLIMITED dimension allowed");
		     rec_dim = ndims; 
		     dims[ndims].size = NC_UNLIMITED;
		     ndims++;
		   
} break;

case YYr12: {	/* dimd :  dim */
#line 148 "ncgen.y"
 if (yypvt[0]->is_dim == 1) {
		        derror( "duplicate dimension declaration for %s",
		                yypvt[0]->name);
		     }
	             yypvt[0]->is_dim = 1;
		     yypvt[0]->dnum = ndims;
		     
		     grow_darray(ndims,  
				 &dims); 
		     dims[ndims].name = (char *) emalloc(strlen(yypvt[0]->name)+1);
		     (void) strcpy(dims[ndims].name, yypvt[0]->name);
		   
} break;

case YYr21: {	/* type :  BYTE_K */
#line 173 "ncgen.y"
 type_code = NC_BYTE; 
} break;

case YYr22: {	/* type :  CHAR_K */
#line 174 "ncgen.y"
 type_code = NC_CHAR; 
} break;

case YYr23: {	/* type :  SHORT_K */
#line 175 "ncgen.y"
 type_code = NC_SHORT; 
} break;

case YYr24: {	/* type :  LONG_K */
#line 176 "ncgen.y"
 type_code = NC_LONG; 
} break;

case YYr25: {	/* type :  FLOAT_K */
#line 177 "ncgen.y"
 type_code = NC_FLOAT; 
} break;

case YYr26: {	/* type :  DOUBLE_K */
#line 178 "ncgen.y"
 type_code = NC_DOUBLE; 
} break;

case YYr29: {	/* varspec :  var */
#line 184 "ncgen.y"

		    static struct vars dummyvar;

		    dummyvar.name = "dummy";
		    dummyvar.type = NC_DOUBLE;
		    dummyvar.ndims = 0;
		    dummyvar.dims = 0;
		    dummyvar.fill_value.doublev = FILL_DOUBLE;
		    dummyvar.has_data = 0;

		    nvdims = 0;
		    
		    if (yypvt[0]->is_var == 1) {
		       derror( "duplicate variable declaration for %s",
		               yypvt[0]->name);
		    }
	            yypvt[0]->is_var = 1;
		    yypvt[0]->vnum = nvars;
		    
		    grow_varray(nvars,  
				&vars); 
		    vars[nvars] = dummyvar; 
		    vars[nvars].name = (char *) emalloc(strlen(yypvt[0]->name)+1);
		    (void) strcpy(vars[nvars].name, yypvt[0]->name);
		    vars[nvars].type = type_code;
		    
		    nc_getfill(type_code, &vars[nvars].fill_value);
		    vars[nvars].has_data = 0; 
		   
} break;

case YYr30: {	/* varspec :  var $29 dimspec */
#line 215 "ncgen.y"

		    vars[nvars].ndims = nvdims;
		    nvars++;
		   
} break;

case YYr36: {	/* vdim :  dim */
#line 229 "ncgen.y"

		    if (nvdims >= MAX_VAR_DIMS) {
		       derror("%s has too many dimensions",vars[nvars].name);
		    }
		    if (yypvt[0]->is_dim == 1)
		       dimnum = yypvt[0]->dnum;
		    else {
		       derror( "%s is not declared as a dimension",
			       yypvt[0]->name);
	               dimnum = ndims;
		    }
		    if (rec_dim != -1 && dimnum == rec_dim && nvdims != 0) {
		       derror("unlimited dimension must be first");
		    }
		    grow_iarray(nvdims, 
				&vars[nvars].dims); 
		    vars[nvars].dims[nvdims] = dimnum;
                    nvdims++;
		   
} break;

case YYr37: {	/* attdecl :  att */
#line 250 "ncgen.y"

		       valnum = 0;
		       valtype = NC_UNSPECIFIED;
		       
		       att_space = emalloc(MAX_NC_ATTSIZE);
		       
		       char_valp = (char *) att_space;
		       byte_valp = (char *) att_space;
		       short_valp = (short *) att_space;
		       long_valp = (nclong *) att_space;
		       float_valp = (float *) att_space;
		       double_valp = (double *) att_space;
		   
} break;

case YYr38: {	/* attdecl :  att $37 '=' attvallist */
#line 264 "ncgen.y"

		       {	
			   int i;
			   for(i=0; i<natts; i++) { 
			       if(atts[i].var == varnum &&
				  STREQ(atts[i].name,atts[natts].name)) {
				   derror("duplicate attribute %s:%s",
					  vars[varnum].name,atts[natts].name);
			       }
			   }
		       }
		       atts[natts].var = varnum ;
		       atts[natts].type = valtype;
		       atts[natts].len = valnum;
		       
		       att_space = erealloc(att_space, valnum*nctypelen(valtype));
		       atts[natts].val = att_space;
		       if (STREQ(atts[natts].name, _FillValue)) {
			   nc_putfill(atts[natts].type,
				       atts[natts].val,
				       &vars[atts[natts].var].fill_value);
			   if(atts[natts].type != vars[atts[natts].var].type) {
			       derror("variable %s: %s type mismatch",
				      vars[atts[natts].var].name, _FillValue);
			   }
		       }
		       natts++;
		   
} break;

case YYr40: {	/* att :  ':' attr */
#line 295 "ncgen.y"

		    varnum = -1;  
		   
} break;

case YYr41: {	/* avar :  var */
#line 301 "ncgen.y"
 if (yypvt[0]->is_var == 1)
		       varnum = yypvt[0]->vnum;
		    else {
		      derror("%s not declared as a variable, fatal error",
			     yypvt[0]->name);
		      YYABORT;
		      }
		   
} break;

case YYr42: {	/* attr :  IDENT */
#line 311 "ncgen.y"

		       
		       grow_aarray(natts,  
				   &atts); 
		       atts[natts].name = (char *) emalloc(strlen(yypvt[0]->name)+1);
		       (void) strcpy(atts[natts].name,yypvt[0]->name);
		   
} break;

case YYr45: {	/* aconst :  attconst */
#line 323 "ncgen.y"

		    if (valtype == NC_UNSPECIFIED)
		      valtype = atype_code;
		    if (valtype != atype_code)
		      derror("values for attribute must be all of same type");
		   
} break;

case YYr46: {	/* attconst :  CHAR_CONST */
#line 332 "ncgen.y"

		       atype_code = NC_CHAR;
		       *char_valp++ = char_val;
		       valnum++;
		   
} break;

case YYr47: {	/* attconst :  TERMSTRING */
#line 338 "ncgen.y"

		       atype_code = NC_CHAR;
		       {
			   
			   int len = strlen(termstring);
			   if (len == 0) 
			       len = 1;
			   (void)strncpy(char_valp,termstring,len);
			   valnum += len;
			   char_valp += len;
		       }
		   
} break;

case YYr48: {	/* attconst :  BYTE_CONST */
#line 351 "ncgen.y"

		       atype_code = NC_BYTE;
		       *byte_valp++ = byte_val;
		       valnum++;
		   
} break;

case YYr49: {	/* attconst :  SHORT_CONST */
#line 357 "ncgen.y"

		       atype_code = NC_SHORT;
		       *short_valp++ = short_val;
		       valnum++;
		   
} break;

case YYr50: {	/* attconst :  LONG_CONST */
#line 363 "ncgen.y"

		       atype_code = NC_LONG;
		       *long_valp++ = long_val;
		       valnum++;
		   
} break;

case YYr51: {	/* attconst :  FLOAT_CONST */
#line 369 "ncgen.y"

		       atype_code = NC_FLOAT;
		       *float_valp++ = float_val;
		       valnum++;
		   
} break;

case YYr52: {	/* attconst :  DOUBLE_CONST */
#line 375 "ncgen.y"

		       atype_code = NC_DOUBLE;
		       *double_valp++ = double_val;
		       valnum++;
		   
} break;

case YYr57: {	/* datadecl :  avar */
#line 390 "ncgen.y"

		       valtype = vars[varnum].type; 
		       valnum = 0;	
		       vars[varnum].has_data = 1;
		       
		       var_size = nctypelen(valtype);
		       if (vars[varnum].ndims == 0)
			   var_len = 1;
		       else if (vars[varnum].dims[0] == rec_dim) {
			   var_len = 1; 
			   netcdf_record_number = 0;
		       }
		       else
			   var_len = dims[vars[varnum].dims[0]].size;
		       for(dimnum = 1; dimnum < vars[varnum].ndims; dimnum++)
			 var_len = var_len*dims[vars[varnum].dims[dimnum]].size;
		       
		       if (var_len*var_size != (unsigned)(var_len*var_size)) {
			   derror("too much data for this machine");
			   exit(9);
		       }
		       rec_start = malloc ((unsigned)(var_len*var_size));
		       if (rec_start == 0) {
			   derror ("out of memory\n");
			   exit(3);
		       }
		       rec_cur = rec_start;
		       switch (valtype) {
			 case NC_CHAR:
			   char_valp = (char *) rec_start;
			   break;
			 case NC_BYTE:
			   byte_valp = (char *) rec_start;
			   break;
			 case NC_SHORT:
			   short_valp = (short *) rec_start;
			   break;
			 case NC_LONG:
			   long_valp = (nclong *) rec_start;
			   break;
			 case NC_FLOAT:
			   float_valp = (float *) rec_start;
			   break;
			 case NC_DOUBLE:
			   double_valp = (double *) rec_start;
			   break;
		       }
		 
} break;

case YYr58: {	/* datadecl :  avar $57 '=' constlist */
#line 439 "ncgen.y"

		       if (valnum > 0 && valnum < var_len) { 
			   nc_fill(valtype,
				    var_len - valnum,
				    rec_cur,
				    vars[varnum].fill_value);
			   
			   if (derror_count == 0)
			     put_variable(rec_start);
		       }
		       free ((char *) rec_start);
		 
} break;

case YYr61: {	/* dconst :  */
#line 456 "ncgen.y"

		       if(valnum >= var_len) {
			   derror("too many values for this variable, %d >= %d",
				  valnum, var_len);
			   exit (4);
		       }
		       not_a_string = 1;
                   
} break;

case YYr62: {	/* dconst :  $61 const */
#line 465 "ncgen.y"

		       if (not_a_string) {
			   switch (valtype) {
			     case NC_CHAR:
			       rec_cur = (void *) char_valp;
			       break;
			     case NC_BYTE:
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			       rec_cur = (void *) short_valp;
			       break;
			     case NC_LONG:
			       rec_cur = (void *) long_valp;
			       break;
			     case NC_FLOAT:
			       rec_cur = (void *) float_valp;
			       break;
			     case NC_DOUBLE:
			       rec_cur = (void *) double_valp;
			       break;
			   }
		       }
		       if (valnum >= var_len) {
			   
			   if (derror_count == 0)
			     put_variable(rec_start);
			   
			   
			   if (vars[varnum].ndims > 0 &&
			       vars[varnum].dims[0] == rec_dim) {
			       valnum = 0;
			       netcdf_record_number++;
			       rec_cur = rec_start;
			       switch (valtype) {
				 case NC_CHAR:
				   char_valp = (char *) rec_start;
				   break;
				 case NC_BYTE:
				   byte_valp = (char *) rec_start;
				   break;
				 case NC_SHORT:
				   short_valp = (short *) rec_start;
				   break;
				 case NC_LONG:
				   long_valp = (nclong *) rec_start;
				   break;
				 case NC_FLOAT:
				   float_valp = (float *) rec_start;
				   break;
				 case NC_DOUBLE:
				   double_valp = (double *) rec_start;
				   break;
			       }
			   }
		       }
		 
} break;

case YYr63: {	/* const :  CHAR_CONST */
#line 525 "ncgen.y"

		       atype_code = NC_CHAR;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = char_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = char_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = char_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = char_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = char_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = char_val;
			   break;
		       }
		       valnum++;
		   
} break;

case YYr64: {	/* const :  TERMSTRING */
#line 550 "ncgen.y"

		       not_a_string = 0;
		       atype_code = NC_CHAR;
		       {
			   int len = strlen(termstring);

			   if(valnum+len > var_len) {
			       derror("string won't fit in this variable, %d>%d", 
				      valnum, var_len);
			       exit (5);
			   }
			   switch (valtype) {
			     case NC_CHAR:
			       {
				   int i, sl, ld;
				   (void)strncpy(char_valp,termstring,len);
				   
				   ld = vars[varnum].ndims-1;
				   if (ld > 0) {
				       sl = dims[vars[varnum].dims[ld]].size;
				       for (i =len;i<sl;i++)
					   char_valp[i] = '\0';
				       if (sl < len)
					   sl = len;
				       valnum += sl;
				       char_valp += sl;
				       rec_cur = (void *) char_valp;
				   } else { 
				       valnum += len;
				       char_valp += len;
				       rec_cur = (void *) char_valp;
				   }
			       }
			       break;
			     case NC_BYTE:
			       (void)strncpy(byte_valp,termstring,len);
			       byte_valp += len;
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			     case NC_LONG:
			     case NC_FLOAT:
			     case NC_DOUBLE:
			       derror("string value invalid for %s variable",
				      nctype(valtype));
			       break;
			   }
		       }
		   
} break;

case YYr65: {	/* const :  BYTE_CONST */
#line 600 "ncgen.y"

		       atype_code = NC_BYTE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = byte_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = byte_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = byte_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = byte_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = byte_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = byte_val;
			   break;
		       }
		       valnum++;
		   
} break;

case YYr66: {	/* const :  SHORT_CONST */
#line 625 "ncgen.y"

		       atype_code = NC_SHORT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = short_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = short_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = short_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = short_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = short_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = short_val;
			   break;
		       }
		       valnum++;
		   
} break;

case YYr67: {	/* const :  LONG_CONST */
#line 650 "ncgen.y"

		       atype_code = NC_LONG;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = long_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = long_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = long_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = long_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = long_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = long_val;
			   break;
		       }
		       valnum++;
		   
} break;

case YYr68: {	/* const :  FLOAT_CONST */
#line 675 "ncgen.y"

		       atype_code = NC_FLOAT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = float_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = float_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = float_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = float_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = float_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = float_val;
			   break;
		       }
		       valnum++;
		   
} break;

case YYr69: {	/* const :  DOUBLE_CONST */
#line 700 "ncgen.y"

		       atype_code = NC_DOUBLE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = double_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = double_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = double_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = double_val;
			   break;
			 case NC_FLOAT:
			   if (double_val == FILL_DOUBLE)
			     *float_valp++ = FILL_FLOAT;
			   else
			     *float_valp++ = double_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = double_val;
			   break;
		       }
		       valnum++;
		   
} break;

case YYr70: {	/* const :  FILLVALUE */
#line 728 "ncgen.y"

		       
		       switch (valtype) {
		       case NC_CHAR:
			   nc_fill(valtype, 1, (void *)char_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_BYTE:
			   nc_fill(valtype, 1, (void *)byte_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_SHORT:
			   nc_fill(valtype, 1, (void *)short_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_LONG:
			   nc_fill(valtype, 1, (void *)long_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_FLOAT:
			   nc_fill(valtype, 1, (void *)float_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_DOUBLE:
			   nc_fill(valtype, 1, (void *)double_valp++,
				   vars[varnum].fill_value);
			   break;
		       }
		       valnum++;
		   
} break;
#line 314 "/etc/yyparse.c"
	case YYrACCEPT:
		YYACCEPT;
	case YYrERROR:
		goto yyError;
	}

	/*
	 *	Look up next state in goto table.
	 */

	yyp = &yygo[yypgo[yyi]];
	yyq = yyp++;
	yyi = *yyps;
	while (yyi < *yyp++)
		;

	yystate = ~(yyi == *--yyp? YYQYYP: *yyq);
#if YYDEBUG
	if (yydebug)
		YY_TRACE(yyShowGoto)
#endif
	goto yyStack;

yyerrlabel:	;		/* come here from YYERROR	*/
/*
#pragma used yyerrlabel
 */
	yyerrflag = 1;
	if (yyi == YYrERROR) {
		yyps--;
		yypv--;
#if YYDEBUG
		yytp--;
#endif
	}

yyError:
	switch (yyerrflag) {

	case 0:		/* new error */
		yynerrs++;
		yyi = yychar;
		yyerror("Syntax error");
		if (yyi != yychar) {
			/* user has changed the current token */
			/* try again */
			yyerrflag++;	/* avoid loops */
			goto yyEncore;
		}

	case 1:		/* partially recovered */
	case 2:
		yyerrflag = 3;	/* need 3 valid shifts to recover */
			
		/*
		 *	Pop states, looking for a
		 *	shift on `error'.
		 */

		for ( ; yyps > yys; yyps--, yypv--
#if YYDEBUG
					, yytp--
#endif
		) {
			if (*yyps >= sizeof yypact/sizeof yypact[0])
				continue;
			yyp = &yyact[yypact[*yyps]];
			yyq = yyp;
			do
				;
			while (YYERRCODE < *yyp++);

			if (YYERRCODE == yyp[-1]) {
				yystate = ~YYQYYP;
				goto yyStack;
			}
				
			/* no shift in this state */
#if YYDEBUG
			if (yydebug && yyps > yys+1)
				YY_TRACE(yyShowErrRecovery)
#endif
			/* pop stacks; try again */
		}
		/* no shift on error - abort */
		break;

	case 3:
		/*
		 *	Erroneous token after
		 *	an error - discard it.
		 */

		if (yychar == 0)  /* but not EOF */
			break;
#if YYDEBUG
		if (yydebug)
			YY_TRACE(yyShowErrDiscard)
#endif
		yyclearin;
		goto yyEncore;	/* try again in same state */
	}
	YYABORT;

#ifdef YYALLOC
yyReturn:
	yylval = save_yylval;
	yyval = save_yyval;
	yypvt = save_yypvt;
	yychar = save_yychar;
	yyerrflag = save_yyerrflag;
	yynerrs = save_yynerrs;
	free((char *)yys);
	free((char *)yyv);
	return(retval);
#endif
}

		
#if YYDEBUG
/*
 * Return type of token
 */
int
yyGetType YY_FNARG1(int, tok)
{
	yyNamedType * tp;
	for (tp = &yyTokenTypes[yyntoken-1]; tp > yyTokenTypes; tp--)
		if (tp->token == tok)
			return tp->type;
	return 0;
}
/*
 * Print a token legibly.
 */
char *
yyptok YY_FNARG1(int, tok)
{
	yyNamedType * tp;
	for (tp = &yyTokenTypes[yyntoken-1]; tp > yyTokenTypes; tp--)
		if (tp->token == tok)
			return tp->name;
	return "";
}

/*
 * Read state 'num' from YYStatesFile
 */
#ifdef YYTRACE
static FILE *yyStatesFile = (FILE *) 0;
static char yyReadBuf[YYMAX_READ+1];

static char *
yygetState YY_FNARG1(int, num)
{
	int	size;

	if (yyStatesFile == (FILE *) 0
	 && (yyStatesFile = fopen(YYStatesFile, "r")) == (FILE *) 0)
		return "yyExpandName: cannot open states file";

	if (num < yynstate - 1)
		size = (int)(yyStates[num+1] - yyStates[num]);
	else {
		/* length of last item is length of file - ptr(last-1) */
		if (fseek(yyStatesFile, 0L, 2) < 0)
			goto cannot_seek;
		size = (int) (ftell(yyStatesFile) - yyStates[num]);
	}
	if (size < 0 || size > YYMAX_READ)
		return "yyExpandName: bad read size";
	if (fseek(yyStatesFile, yyStates[num], 0) < 0) {
	cannot_seek:
		return "yyExpandName: cannot seek in states file";
	}

	(void) fread(yyReadBuf, 1, size, yyStatesFile);
	yyReadBuf[size] = '\0';
	return yyReadBuf;
}
#endif /* YYTRACE */
/*
 * Expand encoded string into printable representation
 * Used to decode yyStates and yyRules strings.
 * If the expansion of 's' fits in 'buf', return 1; otherwise, 0.
 */
int
yyExpandName YY_FNARG4( int, num, int, isrule, char*, buf, int, len)
{
	int	i, n, cnt, type;
	char	* endp, * cp;
	char	*s;

	if (isrule)
		s = yyRules[num].name;
	else
#ifdef YYTRACE
		s = yygetState(num);
#else
		s = "*no states*";
#endif

	for (endp = buf + len - 8; *s; s++) {
		if (buf >= endp) {		/* too large: return 0 */
		full:	(void) strcpy(buf, " ...\n");
			return 0;
		} else if (*s == '%') {		/* nonterminal */
			type = 0;
			cnt = yynvar;
			goto getN;
		} else if (*s == '&') {		/* terminal */
			type = 1;
			cnt = yyntoken;
		getN:
			if (cnt < 100)
				i = 2;
			else if (cnt < 1000)
				i = 3;
			else
				i = 4;
			for (n = 0; i-- > 0; )
				n = (n * 10) + *++s - '0';
			if (type == 0) {
				if (n >= yynvar)
					goto too_big;
				cp = yysvar[n];
			} else if (n >= yyntoken) {
			    too_big:
				cp = "<range err>";
			} else
				cp = yyTokenTypes[n].name;

			if ((i = strlen(cp)) + buf > endp)
				goto full;
			(void) strcpy(buf, cp);
			buf += i;
		} else
			*buf++ = *s;
	}
	*buf = '\0';
	return 1;
}
#ifndef YYTRACE
/*
 * Show current state of yyparse
 */
void
yyShowState YY_FNARG1(yyTraceItems*, tp)
{
	short * p;
	YYSTYPE * q;

	printf(
	    m_textmsg(2828, "state %d (%d), char %s (%d)\n", "1"),
	      yysmap[tp->state], tp->state,
	      yyptok(tp->lookahead), tp->lookahead);
}
/*
 * show results of reduction
 */
void
yyShowReduce YY_FNARG1(yyTraceItems*, tp)
{
	printf("reduce %d (%d), pops %d (%d)\n",
		yyrmap[tp->rule], tp->rule,
		tp->states[tp->nstates - tp->npop],
		yysmap[tp->states[tp->nstates - tp->npop]]);
}
void
yyShowRead YY_FNARG1(int, val)
{
	printf(m_textmsg(2829, "read %s (%d)\n", "1"), yyptok(val), val);
}
void
yyShowGoto YY_FNARG1(yyTraceItems*, tp)
{
	printf(m_textmsg(2830, "goto %d (%d)\n", "1"), yysmap[tp->state], tp->state);
}
void
yyShowShift YY_FNARG1(yyTraceItems*, tp)
{
	printf(m_textmsg(2831, "shift %d (%d)\n", "1"), yysmap[tp->state], tp->state);
}
void
yyShowErrRecovery YY_FNARG1(yyTraceItems*, tp)
{
	short	* top = tp->states + tp->nstates - 1;

	printf(
	m_textmsg(2832, "Error recovery pops state %d (%d), uncovers %d (%d)\n", "1"),
		yysmap[*top], *top, yysmap[*(top-1)], *(top-1));
}
void
yyShowErrDiscard YY_FNARG1(yyTraceItems*, tp)
{
	printf(m_textmsg(2833, "Error recovery discards %s (%d), ", "1"),
		yyptok(tp->lookahead), tp->lookahead);
}
#endif	/* ! YYTRACE */
#endif	/* YYDEBUG */
