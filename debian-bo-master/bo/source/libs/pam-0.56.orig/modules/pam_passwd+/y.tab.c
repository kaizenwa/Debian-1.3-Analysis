
/*  A Bison parser, made from test.y with Bison version GNU Bison version 1.24
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	AND	258
#define	LAND	259
#define	LOR	260
#define	DIV	261
#define	EOL	262
#define	EQ	263
#define	FILENAME	264
#define	GE	265
#define	GT	266
#define	LE	267
#define	LPAR	268
#define	LT	269
#define	MINUS	270
#define	MOD	271
#define	NE	272
#define	NOT	273
#define	NUMBER	274
#define	OR	275
#define	PATEQ	276
#define	PATNE	277
#define	STRING	278
#define	PLUS	279
#define	PROGRAM	280
#define	RPAR	281
#define	TIMES	282
#define	UNK	283

#line 1 "test.y"

/*
 * this file implements the grammar for the password tests
 * the escapes are handled in the caller
 */

#include "passwd.h"

/* Add the additional parameter for the "struct _options" pointer */
#define YYPARSE_PARAM		yyparse_opt
#define YYLEX_PARAM		yyparse_opt

/* Ensure that the option is passed to the yyeerror code as well */
#define yyerror(msg)		test_yyerror(msg,yyparse_opt)

#line 20 "test.y"
typedef union {
	char *cval;		/* something parsed as a string */
	int ival;		/* something parsed as a number */
} YYSTYPE;
#line 78 "test.y"

/* Variables were moved to the "struct _options" data */

static int yylex(YYSTYPE *yylval, void *yyparse_opt);
void test_yyerror (const char *msg, void *yyparse_opt);

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		90
#define	YYFLAG		-32768
#define	YYNTBASE	29

#define YYTRANSLATE(x) ((unsigned)(x) <= 283 ? yytranslate[x] : 34)

static const char yytranslate[] = {     0,
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
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     3,     6,     8,    12,    16,    20,    23,    25,    27,
    31,    35,    39,    43,    47,    51,    55,    59,    63,    67,
    71,    75,    79,    83,    87,    91,    95,    99,   103,   107,
   111,   115,   119,   123,   127,   131,   135,   139,   143,   147,
   151,   155,   158,   161,   165,   169
};

static const short yyrhs[] = {    30,
     7,     0,     1,     7,     0,     7,     0,    13,    30,    26,
     0,    30,     4,    30,     0,    30,     5,    30,     0,    18,
    30,     0,    31,     0,    32,     0,    23,     8,    23,     0,
    23,    17,    23,     0,    23,    21,    23,     0,    23,    22,
    23,     0,    23,     8,     9,     0,    23,    17,     9,     0,
    23,    21,     9,     0,    23,    22,     9,     0,    23,     8,
    25,     0,    23,    17,    25,     0,    23,    21,    25,     0,
    23,    22,    25,     0,     9,     8,    23,     0,     9,    17,
    23,     0,     9,    21,    23,     0,     9,    22,    23,     0,
    25,     8,    23,     0,    25,    17,    23,     0,    25,    21,
    23,     0,    25,    22,    23,     0,    33,    14,    33,     0,
    33,    11,    33,     0,    33,    17,    33,     0,    33,     8,
    33,     0,    33,    10,    33,     0,    33,    12,    33,     0,
    13,    33,    26,     0,    33,     3,    33,     0,    33,    24,
    33,     0,    33,    15,    33,     0,    33,    16,    33,     0,
    33,    20,    33,     0,    24,    33,     0,    15,    33,     0,
    33,    27,    33,     0,    33,     6,    33,     0,    19,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    92,    94,    96,   100,   102,   104,   106,   108,   110,   114,
   118,   122,   129,   136,   140,   144,   148,   152,   156,   160,
   164,   168,   172,   176,   180,   184,   188,   192,   196,   202,
   204,   206,   208,   210,   212,   216,   218,   220,   222,   224,
   226,   228,   230,   232,   234,   236
};

static const char * const yytname[] = {   "$","error","$undefined.","AND","LAND",
"LOR","DIV","EOL","EQ","FILENAME","GE","GT","LE","LPAR","LT","MINUS","MOD","NE",
"NOT","NUMBER","OR","PATEQ","PATNE","STRING","PLUS","PROGRAM","RPAR","TIMES",
"UNK","stat","test","string","prim","number",""
};
#endif

static const short yyr1[] = {     0,
    29,    29,    29,    30,    30,    30,    30,    30,    30,    31,
    31,    31,    31,    31,    31,    31,    31,    31,    31,    31,
    31,    31,    31,    31,    31,    31,    31,    31,    31,    32,
    32,    32,    32,    32,    32,    33,    33,    33,    33,    33,
    33,    33,    33,    33,    33,    33
};

static const short yyr2[] = {     0,
     2,     2,     1,     3,     3,     3,     2,     1,     1,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     2,     2,     3,     3,     1
};

static const short yydefact[] = {     0,
     0,     3,     0,     0,     0,     0,    46,     0,     0,     0,
     0,     8,     9,     0,     2,     0,     0,     0,     0,     0,
     0,     0,    43,     7,     0,     0,     0,     0,    42,     0,
     0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    22,
    23,    24,    25,     4,    36,     0,    14,    10,    18,    15,
    11,    19,    16,    12,    20,    17,    13,    21,    26,    27,
    28,    29,     5,     6,    37,    45,    33,    34,    31,    35,
    30,    39,    40,    32,    41,    38,    44,     0,     0,     0
};

static const short yydefgoto[] = {    88,
    11,    12,    13,    14
};

static const short yypact[] = {     7,
     0,-32768,    91,   105,    -9,   105,-32768,   117,    -9,   119,
    83,-32768,-32768,    65,-32768,   -14,   -12,     5,    29,    -2,
    43,    -9,    -3,    57,     4,    41,   108,   112,    -3,    33,
    42,    49,    51,   105,   105,-32768,    -9,    -9,    -9,    -9,
    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,    -9,-32768,
-32768,-32768,-32768,-32768,-32768,    80,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    57,    57,    78,    -3,    95,    95,    95,    95,
    95,-32768,    -3,    95,    78,-32768,    -3,    23,    97,-32768
};

static const short yypgoto[] = {-32768,
    13,-32768,-32768,    -4
};


#define	YYLAST		141


static const short yytable[] = {    21,
    23,    34,    35,    22,    29,     5,    15,     1,    50,     7,
    51,    44,    57,     2,     9,     3,    20,    56,    24,     4,
    48,     5,    89,    54,     6,     7,    58,    52,    59,     8,
     9,    10,    75,    76,    77,    78,    79,    80,    81,    82,
    83,    84,    85,    86,    87,    37,    73,    74,    38,    60,
    39,    53,    40,    41,    42,    69,    43,    44,    45,    46,
    34,    35,    47,    61,    70,    62,    48,    37,    55,    49,
    38,    71,    39,    72,    40,    41,    42,     0,    43,    44,
    45,    46,    37,    38,    47,    38,    34,    35,    48,    36,
     0,    49,    44,    45,    44,    45,    90,    37,    16,    47,
    38,    48,     0,    48,    49,    55,    49,    17,     0,    44,
    45,    18,    19,     3,    47,     0,    63,     4,    48,     5,
    66,    49,     6,     7,    25,     0,    30,     8,     9,    10,
    64,     0,    65,    26,    67,    31,    68,    27,    28,    32,
    33
};

static const short yycheck[] = {     4,
     5,     4,     5,    13,     9,    15,     7,     1,    23,    19,
    23,    15,     9,     7,    24,     9,     4,    22,     6,    13,
    24,    15,     0,    26,    18,    19,    23,    23,    25,    23,
    24,    25,    37,    38,    39,    40,    41,    42,    43,    44,
    45,    46,    47,    48,    49,     3,    34,    35,     6,     9,
     8,    23,    10,    11,    12,    23,    14,    15,    16,    17,
     4,     5,    20,    23,    23,    25,    24,     3,    26,    27,
     6,    23,     8,    23,    10,    11,    12,    -1,    14,    15,
    16,    17,     3,     6,    20,     6,     4,     5,    24,     7,
    -1,    27,    15,    16,    15,    16,     0,     3,     8,    20,
     6,    24,    -1,    24,    27,    26,    27,    17,    -1,    15,
    16,    21,    22,     9,    20,    -1,     9,    13,    24,    15,
     9,    27,    18,    19,     8,    -1,     8,    23,    24,    25,
    23,    -1,    25,    17,    23,    17,    25,    21,    22,    21,
    22
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

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
#define __yy_memcpy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (from, to, count)
     char *from;
     char *to;
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
__yy_memcpy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 192 "/usr/lib/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#else
#define YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#endif

int
yyparse(YYPARSE_PARAM)
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
      __yy_memcpy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
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
#line 93 "test.y"
{ ((struct _options *)yyparse_opt)->retval = yyvsp[-1].ival ; ;
    break;}
case 2:
#line 95 "test.y"
{ ((struct _options *)yyparse_opt)->retval = 1; ;
    break;}
case 3:
#line 97 "test.y"
{ ((struct _options *)yyparse_opt)->retval = 0; ;
    break;}
case 4:
#line 101 "test.y"
{ yyval.ival = yyvsp[-1].ival ; ;
    break;}
case 5:
#line 103 "test.y"
{ yyval.ival = yyvsp[-2].ival && yyvsp[0].ival ; ;
    break;}
case 6:
#line 105 "test.y"
{ yyval.ival = yyvsp[-2].ival || yyvsp[0].ival ; ;
    break;}
case 7:
#line 107 "test.y"
{ yyval.ival = ! yyvsp[0].ival ; ;
    break;}
case 8:
#line 109 "test.y"
{ yyval.ival = yyvsp[0].ival ; ;
    break;}
case 9:
#line 111 "test.y"
{ yyval.ival = yyvsp[0].ival ; ;
    break;}
case 10:
#line 115 "test.y"
{ yyval.ival = (strcmp( yyvsp[-2].cval , yyvsp[0].cval ) == 0);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 11:
#line 119 "test.y"
{ yyval.ival = (strcmp( yyvsp[-2].cval , yyvsp[0].cval ) != 0);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 12:
#line 123 "test.y"
{ if (smatch(((struct _options *)yyparse_opt), yyvsp[0].cval ))
			    YYERROR;
			  yyval.ival = match((struct _options *)yyparse_opt, yyvsp[-2].cval );
			  free_pattern ((struct _options *)yyparse_opt);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 13:
#line 130 "test.y"
{ if (smatch(((struct _options *)yyparse_opt), yyvsp[0].cval ))
			    YYERROR;
			  yyval.ival = !match(((struct _options *)yyparse_opt), yyvsp[-2].cval );
			  free_pattern ((struct _options *)yyparse_opt);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 14:
#line 137 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 1, yyvsp[-2].cval , yyvsp[0].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 15:
#line 141 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 0, yyvsp[-2].cval , yyvsp[0].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 16:
#line 145 "test.y"
{ yyval.ival = patinfp(((struct _options *)yyparse_opt), 1, yyvsp[-2].cval , yyvsp[0].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 17:
#line 149 "test.y"
{ yyval.ival = patinfp(((struct _options *)yyparse_opt), 0, yyvsp[-2].cval , yyvsp[0].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 18:
#line 153 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 1, yyvsp[-2].cval , yyvsp[0].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 19:
#line 157 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 0, yyvsp[-2].cval , yyvsp[0].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 20:
#line 161 "test.y"
{ yyval.ival = patinfp(((struct _options *)yyparse_opt), 1, yyvsp[-2].cval , yyvsp[0].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 21:
#line 165 "test.y"
{ yyval.ival = patinfp(((struct _options *)yyparse_opt), 0, yyvsp[-2].cval , yyvsp[0].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 22:
#line 169 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 1, yyvsp[0].cval , yyvsp[-2].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 23:
#line 173 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 0, yyvsp[0].cval , yyvsp[-2].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 24:
#line 177 "test.y"
{ yyval.ival = patfp(((struct _options *)yyparse_opt), 1, yyvsp[0].cval , yyvsp[-2].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 25:
#line 181 "test.y"
{ yyval.ival = patfp(((struct _options *)yyparse_opt), 0, yyvsp[0].cval , yyvsp[-2].cval , fopen, fclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 26:
#line 185 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 1, yyvsp[0].cval , yyvsp[-2].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 27:
#line 189 "test.y"
{ yyval.ival = strfp(((struct _options *)yyparse_opt), 0, yyvsp[0].cval , yyvsp[-2].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 28:
#line 193 "test.y"
{ yyval.ival = patfp(((struct _options *)yyparse_opt), 1, yyvsp[0].cval , yyvsp[-2].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 29:
#line 197 "test.y"
{ yyval.ival = patfp(((struct _options *)yyparse_opt), 0, yyvsp[0].cval , yyvsp[-2].cval , popen, pclose);
			  (void) free( yyvsp[-2].cval ); (void) free( yyvsp[0].cval );
			;
    break;}
case 30:
#line 203 "test.y"
{ yyval.ival = yyvsp[-2].ival < yyvsp[0].ival ; ;
    break;}
case 31:
#line 205 "test.y"
{ yyval.ival = yyvsp[-2].ival > yyvsp[0].ival ; ;
    break;}
case 32:
#line 207 "test.y"
{ yyval.ival = yyvsp[-2].ival != yyvsp[0].ival ; ;
    break;}
case 33:
#line 209 "test.y"
{ yyval.ival = yyvsp[-2].ival == yyvsp[0].ival ; ;
    break;}
case 34:
#line 211 "test.y"
{ yyval.ival = yyvsp[-2].ival >= yyvsp[0].ival ; ;
    break;}
case 35:
#line 213 "test.y"
{ yyval.ival = yyvsp[-2].ival <= yyvsp[0].ival ; ;
    break;}
case 36:
#line 217 "test.y"
{ yyval.ival = yyvsp[-1].ival ; ;
    break;}
case 37:
#line 219 "test.y"
{ yyval.ival = yyvsp[-2].ival & yyvsp[0].ival ; ;
    break;}
case 38:
#line 221 "test.y"
{ yyval.ival = yyvsp[-2].ival + yyvsp[0].ival ; ;
    break;}
case 39:
#line 223 "test.y"
{ yyval.ival = yyvsp[-2].ival - yyvsp[0].ival ; ;
    break;}
case 40:
#line 225 "test.y"
{ yyval.ival = yyvsp[-2].ival % yyvsp[0].ival ; ;
    break;}
case 41:
#line 227 "test.y"
{ yyval.ival = yyvsp[-2].ival | yyvsp[0].ival ; ;
    break;}
case 42:
#line 229 "test.y"
{ yyval.ival = yyvsp[-1].ival ; ;
    break;}
case 43:
#line 231 "test.y"
{ yyval.ival = - yyvsp[-1].ival ; ;
    break;}
case 44:
#line 233 "test.y"
{ yyval.ival = yyvsp[-2].ival * yyvsp[0].ival ; ;
    break;}
case 45:
#line 235 "test.y"
{ yyval.ival = yyvsp[-2].ival / yyvsp[0].ival ; ;
    break;}
case 46:
#line 237 "test.y"
{ yyval.ival = yyvsp[0].ival ; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 487 "/usr/lib/bison.simple"

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
#line 240 "test.y"


static int yylex_number (YYSTYPE *yylval, struct _options *opt, int next_char)
  {
    register int rval;		/* used for return values */
/*
 * Process numerical data
 */
    if (next_char != '0')
      {
	rval      = next_char - '0';
	next_char = (int) (*opt->lptr++) & 0xFF;
	while (isdigit (next_char))
	  {
	    rval = (rval * 10) + (next_char - '0');
	    next_char = (int) (*opt->lptr++) & 0xFF;
	  }
	--opt->lptr;
	yylval->ival = rval;
	return(NUMBER);
      }
/*
 * Process hexadecimal input
 */
    next_char = (*opt->lptr++) & 0xFF;
    rval      = 0;
    if (next_char == 'x' || next_char == 'X')
      {
	next_char = (*opt->lptr++) & 0xFF;
	while (isxdigit (next_char))
	  {
	    if (next_char >= 'a')
	      {
		next_char ^= ('a' ^ 'A');
	      }
	    next_char -= '0';
	    if (next_char >= 10)
	      {
		next_char -= 7;
	      }
	    rval = (rval << 4) + next_char;
	    next_char = (*opt->lptr++) & 0xFF;
	  }
	--opt->lptr;
	yylval->ival = rval;
	return(NUMBER);
      }
/*
 * Process an octal digit
 */ 

#define isoctal(c) (((c) >= '0') && ((c) <= '7'))

    if (isoctal (next_char))
      {
	rval      = next_char - '0';
	next_char = (*opt->lptr++) & 0xFF;
	if (isoctal (next_char))
	  {
	    rval      = (rval << 3) + (next_char - '0');
	    next_char = (*opt->lptr++) & 0xFF;
	    if (isoctal (next_char))
	      {
		rval      = (rval << 3) + (next_char - '0');
		next_char = (*opt->lptr++) & 0xFF;
	      }
	  }
      }
    --opt->lptr;
    yylval->ival = rval;
    return(NUMBER);
#undef isoctal
  }

static int yylex_escape (YYSTYPE *yylval, struct _options *opt, int next_char)
  {
    next_char = (*opt->lptr++) & 0xFF;
    switch (next_char)
      {
    case 'a':
        yylval->ival = 7;  /* '\a' */
	return(NUMBER);

    case 'b':
        yylval->ival = '\b';
	return(NUMBER);

    case 'f':
        yylval->ival = '\f';
	return(NUMBER);

    case 'n':
        yylval->ival = '\n';
	return(NUMBER);

    case 'r':
        yylval->ival = '\r';
	return(NUMBER);

    case 't':
        yylval->ival = '\t';
	return(NUMBER);

    case 0:
	--opt->lptr;
	break;

    default:
        break;
      }

    yylval->ival = next_char;
    return(NUMBER);
  }

static int yylex_string (YYSTYPE *yylval, struct _options *opt,
			 int quo, int type)
  {
    char parbuf[BUFSIZ];	/* used to save strings */
/*
 * collect the file or program or pattern
 */
    opt->lptr = getcstring (opt->lptr, parbuf, quo);
/*
 * if a closing quote, skip it
 */
    if (*opt->lptr)
        opt->lptr++;
/*
 * return the file or program as the value
 */
    yylval->cval = strdup(parbuf);
    return (type);
  }

/*
 * this is the lexer -- it's pretty dumb
 */

static int yylex(YYSTYPE *yylval, void *yyparse_opt)
  {
    struct _options *opt = (struct _options *) yyparse_opt;
    int next_char;

#define dual(code,yes,no) {		\
    next_char = (*opt->lptr++) & 0xFF;	\
    if (next_char == code) {		\
        return yes;			\
    }					\
    --opt->lptr;			\
    return no;				\
}

#define tri(code1,yes1,code2,yes2,no) {	\
    next_char = (*opt->lptr++) & 0xFF;	\
    if (next_char == code1) {		\
        return yes1;			\
    }					\
    if (next_char == code2) {		\
        return yes2;			\
    }					\
    --opt->lptr;			\
    return no;				\
}

/*
 * this is hit at the end of string
 * we need to do it this way because the '\0' (EOL)
 * token must be returned, so we have to return
 * another "end of input" token -- in other words,
 * the end of input character is NOT the same as 
 * the end of string (EOL) character
 */
    if (opt->ateol)
      {
	opt->ateol = 0;
	return (-1);	/* YACC's end of file character */
      }
/*
 * eat leading white spaces; may have a backslash in front
 * (since a tab separates the test field and the message
 * field, if there is a tab in the test field it must be
 * escaped)
 */
    for (;;)
      {
	next_char = (int) (*opt->lptr) & 0xFF;
	if (next_char == 0)
	  {
	    opt->ateol = 1;
	    return(EOL);
	  }
	
	++opt->lptr;
	if (isspace (next_char))
	  {
	    continue;
	  }
/*
 * Process numerical data
 */
	if (isdigit (next_char))
	  {
	    return (yylex_number (yylval, opt, next_char));
	  }
/*
 * Process the escape character
 */
	if (next_char == '\\')
	  {
	    return (yylex_escape (yylval, opt, next_char));
	  }
/*
 * The character sequence is not special. Try to analyize it.
 */
	switch (next_char)
	  {
	case '@':		/* rest of line is comment */
	case '\t':		/* rest of line is error message */
	case '\n':		/* end of line */
	case '\0':		/* end of line */
	    opt->ateol = 1;
	    return(EOL);
	case '(':		/* begin grouping */
	    return(LPAR);
	case ')':		/* end grouping */
	    return(RPAR);
	case '~':		/* negation */
	    return(NOT);
	case '+':		/* add */
	    return(PLUS);
	case '-':		/* subtract */
	    return(MINUS);
	case '*':		/* multiply */
	    return(TIMES);
	case '/':		/* divide */
	    return(DIV);
	case '%':		/* remainder */
	    return(MOD);
	case '&':		/* disjunction */
	    dual ('&', LAND, AND);
	case '|':		/* conjunction */
	    dual ('|', LOR, OR);
	case '>':		/* relation */
	    dual ('=', GE, GT);
	case '<':		/* relation */
	    dual ('=', LE, LT);
	case '!':		/* relation, logical negation */
	    tri ('=', NE, '~', PATNE, NOT);
	case '=':
	    tri ('=', EQ, '~', PATEQ, EQ);

	case '\'':			/* pattern */
	case '\"':
	    return (yylex_string (yylval, opt, next_char, STRING));

	case '{':			/* program */
	    return (yylex_string (yylval, opt, '}', PROGRAM));

	case '[':			/* file */
	    return (yylex_string (yylval, opt, ']', FILENAME));
	  }
	break;
      }
/*
 * unknown
 */
    return(UNK);

#undef dual
#undef tri
}

/*
 * report grammar errors (yacc)
 */

void test_yyerror (const char *msg, void *yyparse_opt)
{
    logfunc ((struct _options *)yyparse_opt, LG_SYNTAX,
	     "%s at line %d (at \"%8s\")",
	     msg,
	     ((struct _options *)yyparse_opt)->linect,
	     ((struct _options *)yyparse_opt)->lptr - 1);

    ((struct _options *)yyparse_opt)->ateol = 1;
}

/*==================== DRIVER ====================*/
/*
 * this runs the test in "buf" on the password
 * and returns 1 if it is a bad password, 0 if not.
 */

int passtest (struct _options *opt, char *buf)
{
    /* clear the end of line flag */
    opt->ateol = 0;

    /* clobber any trailing newline */
    opt->lptr = strchr (buf, '\n');
    if (opt->lptr)
        *opt->lptr = '\0';
/*
 * set up the pointer to the input
 * for yylex, the lexical analyzer
 */
    opt->lptr   = buf;
/*
 * parse the statement and if the password is bad, let retval be 1.
 * yyparse returns 0 if the grammer is valid and reaches a final
 * state. It return non-zero if there is a 'stop' condition. Neither
 * of these indicate the state of the grammer finding a good rule.
 */
    opt->retval = 1;
    if (yyparse((void *) opt) != 0)
        opt->retval = 1;
/*
 * test is syntactically and semantically correct
 * and return the result
 */
    return (opt->retval == 0);
}
