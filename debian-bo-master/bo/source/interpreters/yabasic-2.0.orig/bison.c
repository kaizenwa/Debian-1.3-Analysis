
/*  A Bison parser, made from yabasic.bison with Bison version GNU Bison version 1.24
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	NUMBER	258
#define	SYMBOL	259
#define	STRSYM	260
#define	STRING	261
#define	SEP	262
#define	FOR	263
#define	TO	264
#define	STEP	265
#define	NEXT	266
#define	GOTO	267
#define	GOSUB	268
#define	LABEL	269
#define	ON	270
#define	IF	271
#define	THEN	272
#define	ELSE	273
#define	ENDIF	274
#define	DO	275
#define	PRINT	276
#define	INPUT	277
#define	RETURN	278
#define	DIM	279
#define	END	280
#define	AND	281
#define	OR	282
#define	NOT	283
#define	NE	284
#define	LE	285
#define	GE	286
#define	LT	287
#define	GT	288
#define	EQ	289
#define	READ	290
#define	DATA	291
#define	RESTORE	292
#define	OPEN	293
#define	CLOSE	294
#define	WINDOW	295
#define	DOT	296
#define	LINE	297
#define	CIRCLE	298
#define	TEXT	299
#define	CLEAR	300
#define	PRINTER	301
#define	WAIT	302
#define	BELL	303
#define	UMINUS	304


/*
     YABASIC --- a tiny integrated Basic Compiler/Interpreter

     BISON - part
     
     this Program is subject to the GNU General Public License;
     see the file yabasic.c for details.
*/

#include "yabasic.h"     /* definitions of yabasic */
#include <malloc.h>

extern int dimcount;
extern int errorlevel;
extern int interactive; /* true, if commands come from stdin */	

void __yy_bcopy(char *,char *,int); /* prototype missing */

int yylineno=1;
int yylex(void);



typedef union {
  double number;        /* double number */
  int token;            /* token of command */
  char *string;         /* quoted string */
  char *symbol;         /* general symbol */
  char *strsym;         /* string symbol */
  int sep;              /* number of newlines as seperator */
} YYSTYPE;

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

#ifndef YYDEBUG
#define YYDEBUG 1
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		292
#define	YYFLAG		-32768
#define	YYNTBASE	60

#define YYTRANSLATE(x) ((unsigned)(x) <= 304 ? yytranslate[x] : 111)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,    58,     2,     2,     2,     2,    56,
    57,    51,    50,    55,    49,     2,    52,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,    59,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    53,     2,     2,     2,     2,     2,     2,
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
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    54
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     3,     8,     9,    11,    13,    16,    19,    21,
    23,    26,    29,    30,    31,    38,    39,    40,    47,    50,
    57,    62,    65,    70,    75,    78,    81,    83,    86,    88,
    91,    94,   100,   108,   113,   122,   129,   136,   143,   150,
   153,   156,   159,   163,   166,   169,   171,   173,   175,   179,
   180,   188,   190,   192,   196,   197,   203,   207,   211,   212,
   220,   222,   224,   225,   231,   235,   239,   243,   247,   251,
   255,   258,   260,   263,   266,   267,   273,   274,   282,   283,
   289,   290,   298,   300,   304,   305,   306,   307,   321,   323,
   326,   327,   329,   330,   331,   332,   342,   346,   350,   354,
   357,   361,   365,   369,   373,   377,   381,   385,   389,   390,
   393,   395,   399,   401,   402,   408,   410,   411,   417,   419,
   423,   425,   426,   432,   434,   435,   441,   443,   445,   449,
   453,   454,   456,   457,   459,   463,   465,   469,   470,   473,
   476,   478,   479,   481,   482,   484,   488,   490,   494,   496,
   500,   502
};

static const short yyrhs[] = {    62,
     0,     0,    60,     7,    61,    62,     0,     0,    67,     0,
    71,     0,    20,    69,     0,    20,    73,     0,    82,     0,
    88,     0,    12,     4,     0,    13,     4,     0,     0,     0,
    15,     4,    63,    12,    64,   109,     0,     0,     0,    15,
     4,    65,    13,    66,   110,     0,    14,     4,     0,    38,
   106,    55,    69,    55,     6,     0,    38,   106,    55,    69,
     0,    39,   106,     0,    21,   105,   104,   107,     0,    22,
   105,   103,    94,     0,    35,    98,     0,    36,   102,     0,
    37,     0,    37,     4,     0,    23,     0,    24,    76,     0,
    38,    40,     0,    38,    40,    73,    55,    73,     0,    38,
    40,    73,    55,    73,    55,    73,     0,    41,    73,    55,
    73,     0,    42,    73,    55,    73,     9,    73,    55,    73,
     0,    43,    73,    55,    73,    55,    73,     0,    44,    69,
    55,    73,    55,    73,     0,    44,    73,    55,    73,    55,
    69,     0,    44,    73,    55,    73,    55,    73,     0,    39,
    40,     0,    45,    40,     0,    38,    46,     0,    38,    46,
    69,     0,    39,    46,     0,    47,    73,     0,    48,     0,
     4,     0,    25,     0,     5,    34,    69,     0,     0,     5,
    56,    68,   108,    57,    34,    69,     0,     5,     0,     6,
     0,    69,    50,    69,     0,     0,     5,    56,    70,   108,
    57,     0,    56,    69,    57,     0,     4,    34,    73,     0,
     0,     4,    56,    72,   108,    57,    34,    73,     0,     3,
     0,     4,     0,     0,     4,    56,    74,   108,    57,     0,
    56,    73,    57,     0,    73,    50,    73,     0,    73,    49,
    73,     0,    73,    51,    73,     0,    73,    52,    73,     0,
    73,    53,    73,     0,    49,    73,     0,     3,     0,    50,
     3,     0,    49,     3,     0,     0,     4,    56,    77,    81,
    57,     0,     0,    76,    55,     4,    56,    78,    81,    57,
     0,     0,     5,    56,    79,    81,    57,     0,     0,    76,
    55,     5,    56,    80,    81,    57,     0,    73,     0,    81,
    55,    73,     0,     0,     0,     0,     8,     4,    34,    73,
    83,     9,    73,    86,    84,    60,    85,    11,    87,     0,
     7,     0,    10,    75,     0,     0,     4,     0,     0,     0,
     0,    16,    92,    89,    17,    60,    90,    93,    91,    19,
     0,    56,    92,    57,     0,    92,    27,    92,     0,    92,
    26,    92,     0,    28,    92,     0,    69,    34,    69,     0,
    69,    29,    69,     0,    73,    34,    73,     0,    73,    29,
    73,     0,    73,    32,    73,     0,    73,    30,    73,     0,
    73,    33,    73,     0,    73,    31,    73,     0,     0,    18,
    60,     0,    95,     0,    94,    55,    95,     0,     4,     0,
     0,     4,    56,    96,   108,    57,     0,     5,     0,     0,
     5,    56,    97,   108,    57,     0,    99,     0,    98,    55,
    99,     0,     4,     0,     0,     4,    56,   100,   108,    57,
     0,     5,     0,     0,     5,    56,   101,   108,    57,     0,
     6,     0,    75,     0,   102,    55,     6,     0,   102,    55,
    75,     0,     0,     6,     0,     0,    73,     0,   104,    55,
    73,     0,    69,     0,   104,    55,    69,     0,     0,    58,
     3,     0,    58,     3,     0,     3,     0,     0,    59,     0,
     0,    73,     0,   108,    55,    73,     0,    69,     0,   108,
    55,    69,     0,     4,     0,   109,    55,     4,     0,     4,
     0,   110,    55,     4,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    66,    67,    69,    72,    73,    74,    75,    76,    77,    78,
    79,    80,    81,    81,    82,    83,    83,    84,    85,    86,
    88,    90,    91,    92,    93,    94,    95,    96,    97,    98,
    99,   100,   101,   102,   103,   105,   107,   108,   109,   111,
   112,   113,   114,   115,   116,   117,   118,   120,   122,   123,
   123,   127,   128,   129,   130,   130,   131,   134,   135,   135,
   139,   140,   141,   141,   142,   143,   144,   145,   146,   147,
   148,   151,   152,   153,   155,   155,   156,   156,   157,   157,
   158,   158,   161,   162,   165,   167,   172,   178,   180,   181,
   184,   185,   188,   189,   190,   191,   193,   194,   195,   196,
   197,   198,   199,   200,   201,   202,   203,   204,   206,   207,
   210,   211,   214,   215,   215,   217,   218,   218,   222,   223,
   226,   227,   227,   229,   230,   230,   234,   235,   236,   237,
   240,   241,   244,   245,   246,   247,   248,   251,   252,   255,
   256,   259,   260,   263,   264,   265,   266,   267,   270,   271,
   274,   275
};

static const char * const yytname[] = {   "$","error","$undefined.","NUMBER",
"SYMBOL","STRSYM","STRING","SEP","FOR","TO","STEP","NEXT","GOTO","GOSUB","LABEL",
"ON","IF","THEN","ELSE","ENDIF","DO","PRINT","INPUT","RETURN","DIM","END","AND",
"OR","NOT","NE","LE","GE","LT","GT","EQ","READ","DATA","RESTORE","OPEN","CLOSE",
"WINDOW","DOT","LINE","CIRCLE","TEXT","CLEAR","PRINTER","WAIT","BELL","'-'",
"'+'","'*'","'/'","'^'","UMINUS","','","'('","')'","'#'","';'","statement_list",
"@1","statement","@2","@3","@4","@5","string_assignment","@6","string_expression",
"@7","assignment","@8","expression","@9","const","dimlist","@10","@11","@12",
"@13","boundlist","for_loop","@14","@15","@16","step_part","next_symbol","if_clause",
"@17","@18","@19","comparison","else_part","inputlist","input","@20","@21","readlist",
"readitem","@22","@23","datalist","prompt","printlist","stream","hashed_number",
"semicolon","functionlist","goto_list","gosub_list",""
};
#endif

static const short yyr1[] = {     0,
    60,    61,    60,    62,    62,    62,    62,    62,    62,    62,
    62,    62,    63,    64,    62,    65,    66,    62,    62,    62,
    62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
    62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
    62,    62,    62,    62,    62,    62,    62,    62,    67,    68,
    67,    69,    69,    69,    70,    69,    69,    71,    72,    71,
    73,    73,    74,    73,    73,    73,    73,    73,    73,    73,
    73,    75,    75,    75,    77,    76,    78,    76,    79,    76,
    80,    76,    81,    81,    83,    84,    85,    82,    86,    86,
    87,    87,    89,    90,    91,    88,    92,    92,    92,    92,
    92,    92,    92,    92,    92,    92,    92,    92,    93,    93,
    94,    94,    95,    96,    95,    95,    97,    95,    98,    98,
    99,   100,    99,    99,   101,    99,   102,   102,   102,   102,
   103,   103,   104,   104,   104,   104,   104,   105,   105,   106,
   106,   107,   107,   108,   108,   108,   108,   108,   109,   109,
   110,   110
};

static const short yyr2[] = {     0,
     1,     0,     4,     0,     1,     1,     2,     2,     1,     1,
     2,     2,     0,     0,     6,     0,     0,     6,     2,     6,
     4,     2,     4,     4,     2,     2,     1,     2,     1,     2,
     2,     5,     7,     4,     8,     6,     6,     6,     6,     2,
     2,     2,     3,     2,     2,     1,     1,     1,     3,     0,
     7,     1,     1,     3,     0,     5,     3,     3,     0,     7,
     1,     1,     0,     5,     3,     3,     3,     3,     3,     3,
     2,     1,     2,     2,     0,     5,     0,     7,     0,     5,
     0,     7,     1,     3,     0,     0,     0,    13,     1,     2,
     0,     1,     0,     0,     0,     9,     3,     3,     3,     2,
     3,     3,     3,     3,     3,     3,     3,     3,     0,     2,
     1,     3,     1,     0,     5,     1,     0,     5,     1,     3,
     1,     0,     5,     1,     0,     5,     1,     1,     3,     3,
     0,     1,     0,     1,     3,     1,     3,     0,     2,     2,
     1,     0,     1,     0,     1,     3,     1,     3,     1,     3,
     1,     3
};

static const short yydefact[] = {     4,
    47,     0,     0,     0,     0,     0,     0,     0,     0,   138,
   138,    29,     0,    48,     0,     0,    27,     0,     0,     0,
     0,     0,     0,     0,     0,    46,     0,     1,     5,     6,
     9,    10,     0,    59,     0,    50,     0,    11,    12,    19,
    13,    61,    62,    52,    53,     0,     0,     0,     0,     0,
    93,     0,     7,     8,     0,   133,   131,     0,     0,    30,
   121,   124,    25,   119,    72,   127,     0,     0,   128,    26,
    28,   141,    31,    42,     0,     0,    40,    44,    22,     0,
     0,     0,     0,     0,     0,    41,    45,     2,    58,   144,
     0,    49,   144,     0,     0,     0,    63,    55,   100,    71,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   139,   136,   134,   142,   132,     0,    75,    79,
     0,   122,   125,     0,    74,    73,     0,     0,    43,   140,
     0,     0,     0,     0,     0,     0,     4,   147,   145,     0,
     0,    85,    14,    17,   144,   144,    57,    65,    97,   102,
   101,    54,   104,   106,   108,   105,   107,   103,    67,    66,
    68,    69,    70,    99,    98,     4,     0,   143,    23,   113,
   116,    24,   111,     0,     0,     0,     0,   144,   144,   120,
   129,   130,     0,    21,    34,     0,     0,     0,     0,     3,
     0,     0,     0,     0,     0,     0,     0,     0,    94,   137,
   135,   114,   117,     0,    83,     0,     0,    77,    81,     0,
     0,    32,     0,     0,     0,     0,     0,   148,   146,     0,
     0,     0,   149,    15,   151,    18,    64,    56,   109,   144,
   144,   112,     0,    76,    80,     0,     0,   123,   126,     0,
    20,     0,    36,    37,    38,    39,    60,    51,     0,     0,
     0,     4,    95,     0,     0,    84,     0,     0,    33,     0,
    89,     0,    86,   150,   152,   110,     0,   115,   118,    78,
    82,    35,    90,     4,    96,    87,     0,    91,    92,    88,
     0,     0
};

static const short yydefgoto[] = {    27,
   147,    28,    95,   205,    96,   206,    29,    93,   148,   156,
    30,    90,   149,   155,    69,    60,   184,   246,   185,   247,
   216,    31,   204,   284,   287,   273,   290,    32,   120,   239,
   277,    51,   263,   182,   183,   240,   241,    63,    64,   188,
   189,    70,   128,   126,    56,    76,   179,   150,   234,   236
};

static const short yypact[] = {   243,
   -27,   -10,    54,    56,    81,    91,   147,   115,    17,   100,
   100,-32768,    37,-32768,    83,    30,   161,     3,    13,     6,
     6,     6,    17,   135,     6,-32768,    11,-32768,-32768,-32768,
-32768,-32768,     6,-32768,    21,-32768,   150,-32768,-32768,-32768,
   179,-32768,   159,   174,-32768,   115,     6,   115,    95,   263,
   106,    17,   154,   130,   210,    17,   225,   178,   180,   177,
   181,   184,   186,-32768,-32768,-32768,   240,   241,-32768,   190,
-32768,-32768,     6,    21,   247,   191,-32768,-32768,-32768,     6,
   220,   249,   256,    67,   268,-32768,   130,-32768,   130,    17,
    21,   154,    17,     6,   242,   239,-32768,-32768,-32768,-32768,
   144,   157,   104,    21,    21,    21,     6,     6,     6,     6,
     6,     6,     6,     6,     6,     6,     6,   115,   115,   244,
   -22,   176,-32768,   154,   130,    -3,-32768,   207,-32768,-32768,
   216,-32768,-32768,    83,-32768,-32768,    48,   275,   154,-32768,
    21,     6,     6,     6,     6,     6,   243,   154,   130,   -23,
    12,   130,-32768,-32768,    17,    17,-32768,-32768,-32768,   154,
   154,-32768,   130,   130,   130,   130,   130,   130,    97,    97,
   221,   221,-32768,-32768,   234,   243,    17,-32768,-32768,   227,
   233,   222,-32768,     6,     6,   254,   266,    17,    17,-32768,
-32768,-32768,     6,    92,   130,   103,   282,   289,   296,-32768,
    17,   269,   295,   267,   332,   339,    68,    89,   343,   154,
   130,-32768,-32768,   207,   130,   102,   105,-32768,-32768,   113,
   117,   303,   351,     6,     6,     6,    17,   154,   130,     6,
    21,     6,-32768,   309,-32768,   311,-32768,-32768,   349,    17,
    17,-32768,     6,-32768,-32768,     6,     6,-32768,-32768,     6,
-32768,   310,   130,   130,   154,   130,   130,   154,    40,   364,
   365,   243,-32768,   140,   141,   130,   145,   148,   130,     6,
-32768,    34,-32768,-32768,-32768,   343,   352,-32768,-32768,-32768,
-32768,   130,-32768,   243,-32768,   343,   359,   368,-32768,-32768,
   373,-32768
};

static const short yypgoto[] = {  -168,
-32768,   228,-32768,-32768,-32768,-32768,-32768,-32768,    22,-32768,
-32768,-32768,    -8,-32768,  -133,-32768,-32768,-32768,-32768,-32768,
  -183,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   -43,-32768,-32768,   160,-32768,-32768,-32768,   245,-32768,
-32768,-32768,-32768,-32768,   366,   357,-32768,   -74,-32768,-32768
};


#define	YYLAST		379


static const short yytable[] = {    50,
    54,   217,    99,   192,   103,    72,    33,   209,    42,    43,
   291,    81,    82,    83,    85,    72,    87,    88,   151,    42,
    43,    44,    45,    35,    89,    44,    45,   106,    34,    49,
    53,   201,    65,   202,   157,    66,    65,    50,   100,   102,
    58,    59,    73,   122,    84,    36,   271,   125,    74,   272,
    65,   177,    77,   191,    47,   178,    92,    37,    78,    38,
    75,    80,   267,   268,   138,    47,   201,    49,   203,   101,
    75,   122,    52,   121,   174,   175,    91,   124,    67,    68,
   207,   208,    67,    68,    39,   152,    61,    62,   113,   114,
   115,   116,   117,   276,    40,   139,    67,    68,   163,   164,
   165,   166,   167,   168,   169,   170,   171,   172,   173,    50,
    50,   224,   121,   220,   221,   286,   106,    42,    43,    44,
    45,   145,   201,   104,   237,   160,   161,   162,   105,   118,
   119,   118,   119,   195,   196,   197,   198,   199,   283,    49,
    49,   106,    46,   201,   106,   238,   223,   115,   116,   117,
    41,   113,   114,   115,   116,   117,   243,    55,   244,   243,
   159,   245,   194,    47,    71,   264,   265,   201,   211,   248,
    48,   201,   104,   249,    86,   215,   215,   105,   113,   114,
   115,   116,   117,    94,   222,   107,   108,   109,   110,   111,
   112,   -16,   229,   106,   201,   201,   278,   279,   210,   243,
   157,   280,   243,   106,   281,   113,   114,   115,   116,   117,
   180,   181,   123,   158,    97,   252,   253,   254,   256,   186,
   187,   257,   228,   259,   113,   114,   115,   116,   117,    98,
   127,   131,   158,   129,   266,   130,   132,   215,   215,   133,
   134,   269,   135,   136,   137,   141,     1,     2,   255,   140,
     3,   154,   258,   153,     4,     5,     6,     7,     8,   118,
   176,   282,     9,    10,    11,    12,    13,    14,   113,   114,
   115,   116,   117,   117,   142,   232,   214,    15,    16,    17,
    18,    19,   212,    20,    21,    22,    23,    24,   213,    25,
    26,   107,   108,   109,   110,   111,   112,   113,   114,   115,
   116,   117,   230,   143,   113,   114,   115,   116,   117,   218,
   144,   113,   114,   115,   116,   117,   113,   114,   115,   116,
   117,   219,   146,   113,   114,   115,   116,   117,   231,   193,
   113,   114,   115,   116,   117,   233,   225,   113,   114,   115,
   116,   117,   235,   226,   113,   114,   115,   116,   117,    88,
   227,   113,   114,   115,   116,   117,   251,   250,   113,   114,
   115,   116,   117,   260,   270,   261,   262,   274,   275,   288,
   285,   289,   292,   242,   200,    79,    57,     0,   190
};

static const short yycheck[] = {     8,
     9,   185,    46,   137,    48,     3,    34,   176,     3,     4,
     0,    20,    21,    22,    23,     3,    25,     7,    93,     3,
     4,     5,     6,    34,    33,     5,     6,    50,    56,     8,
     9,    55,     3,    57,    57,     6,     3,    46,    47,    48,
     4,     5,    40,    52,    23,    56,     7,    56,    46,    10,
     3,    55,    40,     6,    49,    59,    35,     4,    46,     4,
    58,    56,   246,   247,    73,    49,    55,    46,    57,    48,
    58,    80,    56,    52,   118,   119,    56,    56,    49,    50,
   155,   156,    49,    50,     4,    94,     4,     5,    49,    50,
    51,    52,    53,   262,     4,    74,    49,    50,   107,   108,
   109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
   119,     9,    91,   188,   189,   284,    50,     3,     4,     5,
     6,    55,    55,    29,    57,   104,   105,   106,    34,    26,
    27,    26,    27,   142,   143,   144,   145,   146,   272,   118,
   119,    50,    28,    55,    50,    57,    55,    51,    52,    53,
     4,    49,    50,    51,    52,    53,    55,    58,    57,    55,
    57,    57,   141,    49,     4,   240,   241,    55,   177,    57,
    56,    55,    29,    57,    40,   184,   185,    34,    49,    50,
    51,    52,    53,    34,   193,    29,    30,    31,    32,    33,
    34,    13,   201,    50,    55,    55,    57,    57,   177,    55,
    57,    57,    55,    50,    57,    49,    50,    51,    52,    53,
     4,     5,     3,    57,    56,   224,   225,   226,   227,     4,
     5,   230,   201,   232,    49,    50,    51,    52,    53,    56,
     6,    55,    57,    56,   243,    56,    56,   246,   247,    56,
    55,   250,     3,     3,    55,    55,     4,     5,   227,     3,
     8,    13,   231,    12,    12,    13,    14,    15,    16,    26,
    17,   270,    20,    21,    22,    23,    24,    25,    49,    50,
    51,    52,    53,    53,    55,     9,    55,    35,    36,    37,
    38,    39,    56,    41,    42,    43,    44,    45,    56,    47,
    48,    29,    30,    31,    32,    33,    34,    49,    50,    51,
    52,    53,    34,    55,    49,    50,    51,    52,    53,    56,
    55,    49,    50,    51,    52,    53,    49,    50,    51,    52,
    53,    56,    55,    49,    50,    51,    52,    53,    34,    55,
    49,    50,    51,    52,    53,     4,    55,    49,    50,    51,
    52,    53,     4,    55,    49,    50,    51,    52,    53,     7,
    55,    49,    50,    51,    52,    53,     6,    55,    49,    50,
    51,    52,    53,    55,    55,    55,    18,     4,     4,    11,
    19,     4,     0,   214,   147,    19,    11,    -1,   134
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */


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

case 2:
{yylineno+=yyvsp[0].sep;
	     		if (yyvsp[0].sep==2 && interactive) {YYACCEPT;}
                        if (errorlevel<=ERROR) {YYABORT;};
    break;}
case 7:
{pop();;
    break;}
case 8:
{pop();;
    break;}
case 11:
{create_goto(yyvsp[0].symbol);;
    break;}
case 12:
{create_gosub(yyvsp[0].symbol);;
    break;}
case 13:
{create_pushdblsym(yyvsp[0].symbol);;
    break;}
case 14:
{create_skipper();;
    break;}
case 15:
{create_nop();;
    break;}
case 16:
{create_pushdblsym(yyvsp[0].symbol);;
    break;}
case 17:
{create_skipper();;
    break;}
case 18:
{create_nop();;
    break;}
case 19:
{create_label(yyvsp[0].symbol);;
    break;}
case 20:
{create_myopen(yyvsp[-4].number,yyvsp[0].string);;
    break;}
case 21:
{create_myopen(yyvsp[-2].number,"a");;
    break;}
case 22:
{create_myclose(yyvsp[0].number);;
    break;}
case 23:
{;
    break;}
case 27:
{create_restore("");;
    break;}
case 28:
{create_restore(yyvsp[0].symbol);;
    break;}
case 29:
{create_return();;
    break;}
case 31:
{create_openwin(0);;
    break;}
case 32:
{create_openwin(2);;
    break;}
case 33:
{create_openwin(3);;
    break;}
case 34:
{create_dot();;
    break;}
case 35:
{create_line();;
    break;}
case 36:
{create_circle();
    break;}
case 37:
{create_text(1==1);;
    break;}
case 38:
{create_text(1!=1);;
    break;}
case 39:
{
	error(ERROR,"Only string allowed as TEXT");;
    break;}
case 40:
{create_closewin();;
    break;}
case 41:
{create_clearwin();;
    break;}
case 42:
{create_openprinter(0);;
    break;}
case 43:
{create_openprinter(1);;
    break;}
case 44:
{create_closeprinter();;
    break;}
case 45:
{create_wait();;
    break;}
case 46:
{create_bell();;
    break;}
case 47:
{sprintf(string,"'%s' is not a yabasic-command",yyvsp[0].symbol);
            error(ERROR,string);YYABORT;;
    break;}
case 48:
{create_myend();;
    break;}
case 49:
{create_popstrsym(yyvsp[-2].strsym);;
    break;}
case 50:
{pushletter("l");;
    break;}
case 51:
{create_call(yyvsp[-6].strsym);;
    break;}
case 52:
{create_pushstrsym(yyvsp[0].strsym);;
    break;}
case 53:
{create_pushstr(yyvsp[0].string);;
    break;}
case 54:
{create_concat();;
    break;}
case 55:
{pushletter("r");;
    break;}
case 56:
{create_call(yyvsp[-4].strsym);;
    break;}
case 58:
{create_popdblsym(yyvsp[-2].symbol);;
    break;}
case 59:
{pushletter("l");;
    break;}
case 60:
{create_call(yyvsp[-6].symbol);;
    break;}
case 61:
{create_pushdbl(yyvsp[0].number);;
    break;}
case 62:
{create_pushdblsym(yyvsp[0].symbol);;
    break;}
case 63:
{pushletter("r");;
    break;}
case 64:
{create_call(yyvsp[-4].symbol);;
    break;}
case 66:
{create_dblbin('+');;
    break;}
case 67:
{create_dblbin('-');;
    break;}
case 68:
{create_dblbin('*');;
    break;}
case 69:
{create_dblbin('/');;
    break;}
case 70:
{create_dblbin('^');;
    break;}
case 71:
{create_negate();;
    break;}
case 72:
{yyval.number=yyvsp[0].number;
    break;}
case 73:
{yyval.number=yyvsp[0].number;
    break;}
case 74:
{yyval.number=-yyvsp[0].number;;
    break;}
case 75:
{dimcount=0;;
    break;}
case 76:
{create_dim(yyvsp[-4].symbol,'d');;
    break;}
case 77:
{dimcount=0;;
    break;}
case 78:
{create_dim(yyvsp[-4].symbol,'d');;
    break;}
case 79:
{dimcount=0;;
    break;}
case 80:
{create_dim(yyvsp[-4].strsym,'s');;
    break;}
case 81:
{dimcount=0;;
    break;}
case 82:
{create_dim(yyvsp[-4].strsym,'s');;
    break;}
case 83:
{dimcount++;;
    break;}
case 84:
{dimcount++;;
    break;}
case 85:
{create_popdblsym(yyvsp[-2].symbol);pushgoto();create_pushdblsym(yyvsp[-2].symbol);;
    break;}
case 86:
{
	     create_dblrelop((yyvsp[0].number>0)?'{':'}');
             create_decide();
             pushlabel();;
    break;}
case 87:
{
             create_pushdbl(yyvsp[-2].number);
	     create_pushdblsym(yyvsp[-8].symbol);	
             create_dblbin('+');
	     create_popdblsym(yyvsp[-8].symbol);
             swap();popgoto();poplabel();;
    break;}
case 89:
{yyval.number=1.0;;
    break;}
case 90:
{yyval.number=yyvsp[0].number;;
    break;}
case 92:
{;;
    break;}
case 93:
{create_decide();pushlabel();;
    break;}
case 94:
{pushlabel();swap();poplabel();;
    break;}
case 95:
{poplabel();;
    break;}
case 98:
{create_boole('|');;
    break;}
case 99:
{create_boole('&');;
    break;}
case 100:
{create_boole('!');;
    break;}
case 101:
{create_strrelop('=');;
    break;}
case 102:
{create_strrelop('!');;
    break;}
case 103:
{create_dblrelop('=');;
    break;}
case 104:
{create_dblrelop('!');;
    break;}
case 105:
{create_dblrelop('<');;
    break;}
case 106:
{create_dblrelop('{');;
    break;}
case 107:
{create_dblrelop('>');;
    break;}
case 108:
{create_dblrelop('}');;
    break;}
case 113:
{create_myread('d');create_popdblsym(yyvsp[0].symbol);;
    break;}
case 114:
{pushletter("l");;
    break;}
case 115:
{create_myread('d');create_call(yyvsp[-4].symbol);;
    break;}
case 116:
{create_myread('s');create_popstrsym(yyvsp[0].strsym);;
    break;}
case 117:
{pushletter("l");;
    break;}
case 118:
{create_myread('s');create_call(yyvsp[-4].strsym);;
    break;}
case 121:
{create_readdata('d');create_popdblsym(yyvsp[0].symbol);;
    break;}
case 122:
{pushletter("l");;
    break;}
case 123:
{create_readdata('d');create_call(yyvsp[-4].symbol);;
    break;}
case 124:
{create_readdata('s');create_popstrsym(yyvsp[0].strsym);;
    break;}
case 125:
{pushletter("l");;
    break;}
case 126:
{create_readdata('s');create_call(yyvsp[-4].strsym);;
    break;}
case 127:
{create_strdata(yyvsp[0].string);;
    break;}
case 128:
{create_dbldata(yyvsp[0].number);;
    break;}
case 129:
{create_strdata(yyvsp[0].string);;
    break;}
case 130:
{create_dbldata(yyvsp[0].number);;
    break;}
case 131:
{create_prompt("?");;
    break;}
case 132:
{create_prompt(yyvsp[0].string);;
    break;}
case 134:
{create_print('d');;
    break;}
case 135:
{create_print('d');;
    break;}
case 136:
{create_print('s');;
    break;}
case 137:
{create_print('s');;
    break;}
case 138:
{create_myswitch(0.0);;
    break;}
case 139:
{create_myswitch(yyvsp[0].number);;
    break;}
case 140:
{yyval.number=yyvsp[0].number;;
    break;}
case 141:
{yyval.number=yyvsp[0].number;;
    break;}
case 142:
{create_print('n');;
    break;}
case 145:
{pushletter("d");concat();;
    break;}
case 146:
{pushletter("d");concat();;
    break;}
case 147:
{pushletter("s");concat();;
    break;}
case 148:
{pushletter("s");concat();;
    break;}
case 149:
{create_goto(yyvsp[0].symbol);;
    break;}
case 150:
{create_goto(yyvsp[0].symbol);;
    break;}
case 151:
{create_gosub(yyvsp[0].symbol);;
    break;}
case 152:
{create_gosub(yyvsp[0].symbol);;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */


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
