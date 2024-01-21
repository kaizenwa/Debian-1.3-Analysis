// This file was modified for -*- C++ -*-
// using $RCSfile: yacctoC.bison,v $ $Revision: 1.6 $
extern void yyerror(char *s);
extern int yylex();


/*  A Bison parser, made from vsl-gramma.Y
 by  GNU Bison version 1.25
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	IDENTIFIER	258
#define	STRING	259
#define	INTEGER	260
#define	ARROW	261
#define	IF	262
#define	THEN	263
#define	ELSE	264
#define	ELSIF	265
#define	FI	266
#define	OR	267
#define	AND	268
#define	NOT	269
#define	LET	270
#define	IN	271
#define	WHERE	272
#define	OVERRIDE	273
#define	REPLACE	274
#define	EQ	275
#define	NE	276
#define	GT	277
#define	GE	278
#define	LT	279
#define	LE	280
#define	HALIGN	281
#define	VALIGN	282
#define	UALIGN	283
#define	TALIGN	284
#define	APPEND	285
#define	CONS	286
#define	THREEDOTS	287

#line 4 "vsl-gramma.Y"


// Copyright (C) 1995 Technische Universitaet Braunschweig, Germany.
// Written by Andreas Zeller <zeller@ips.cs.tu-bs.de>.
// 
// This file is part of the DDD Library.
// 
// The DDD Library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
// 
// The DDD Library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU Library General Public License for more details.
// 
// You should have received a copy of the GNU Library General Public
// License along with the DDD Library -- see the file COPYING.LIB.
// If not, write to the Free Software Foundation, Inc.,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
// 
// DDD is the data display debugger.
// For details, see the DDD World-Wide-Web page, 
// `http://www.cs.tu-bs.de/softech/ddd/',
// or send a mail to the DDD developers at `ddd@ips.cs.tu-bs.de'.


// Some declarations and utilities

char VSL_yacc_rcsid[] = 
    "$Id: vsl-gramma.Y,v 1.9 1997/04/25 06:41:07 zeller Exp $";


// Funktionsaufrufe

// Geeigneten Knoten fuer Funktionsnamen zurueckgeben
VSLNode *VSLLib::_call(const string& func_name, VSLNode *arg)
{
    // Definitionsliste finden
    VSLDefList* def = deflist(func_name);
    if (def == 0)
    {
	VSLLib::parse_error("'" + func_name + "(...)' undefined");
	delete arg;
	arg = 0;
    }

    // Aufruf zurueckgeben
    if (def && arg)
	return new DefCallNode(def, arg);

    return 0;
}

VSLNode *VSLLib::call(const string& name)
{
    return _call(name, new EmptyListNode);
}

VSLNode *VSLLib::call(const string& name, VSLNode *arg)
{
    if (arg)
	return _call(name, new FixListNode(arg));

    return 0;
}

VSLNode *VSLLib::call(const string& name, VSLNode *arg1, VSLNode *arg2)
{
    if (arg1 && arg2)
	return _call(name, new FixListNode(arg1, arg2));

    return 0;
}

VSLNode *VSLLib::call(const string& name, 
		      VSLNode *arg1, VSLNode *arg2, VSLNode *arg3)
{
    if (arg1 && arg2 && arg3)
	return _call(name, new FixListNode(arg1, arg2, arg3));

    return 0;
}

// Some settings
#define YYERROR_VERBOSE

#ifdef YYERROR_VERBOSE
#define YYDEBUG 1
#endif

#line 143 "vsl-gramma.Y"
typedef struct _YYSTYPE  {
    // Our special yacctoC program makes this a struct -- 
    // thus we use an anonymous union (does not harm in other cases)
    union {
	VSLNode *node;
	string *str;
	int num;
	double fnum;
	struct {
	    string *id;
	    VSLNode *pattern;
	    string *file;
	    int line;
	} header;
	struct {
	    VSLNode *pattern;
	    VSLNode *args;
	} vardef;
    };
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		189
#define	YYFLAG		-32768
#define	YYNTBASE	44

#define YYTRANSLATE(x) ((unsigned)(x) <= 287 ? yytranslate[x] : 82)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,    37,     2,     2,    39,
    40,    35,    33,    41,    34,     2,    36,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,    38,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    42,     2,    43,     2,     2,     2,     2,     2,     2,     2,
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
    26,    27,    28,    29,    30,    31,    32
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     3,     6,     9,    12,    14,    16,    18,    21,
    23,    26,    28,    30,    34,    38,    42,    46,    50,    54,
    58,    62,    66,    70,    74,    78,    82,    86,    90,    94,
    98,   100,   102,   104,   107,   110,   113,   116,   118,   120,
   124,   127,   131,   133,   135,   139,   143,   147,   151,   153,
   155,   157,   159,   161,   163,   165,   168,   172,   175,   179,
   181,   183,   187,   191,   194,   196,   198,   200,   202,   205,
   207,   210,   213,   216,   219,   223,   227,   231,   235,   239,
   243,   247,   251,   255,   259,   263,   267,   271,   275,   279,
   283,   287,   291,   298,   304,   307,   309,   313,   315,   318,
   320,   324,   326,   329,   331,   335
};

static const short yyrhs[] = {    45,
     0,     0,    45,    46,     0,    47,    38,     0,    51,    38,
     0,    76,     0,    79,     0,    38,     0,     1,    38,     0,
    48,     0,    49,    74,     0,    49,     0,    50,     0,    39,
    20,    40,     0,    39,    21,    40,     0,    39,    22,    40,
     0,    39,    23,    40,     0,    39,    24,    40,     0,    39,
    25,    40,     0,    39,    26,    40,     0,    39,    27,    40,
     0,    39,    28,    40,     0,    39,    29,    40,     0,    39,
    33,    40,     0,    39,    34,    40,     0,    39,    35,    40,
     0,    39,    36,    40,     0,    39,    37,    40,     0,    39,
    31,    40,     0,    39,    14,    40,     0,     3,     0,    52,
     0,    54,     0,    53,    56,     0,    48,    20,     0,    55,
    56,     0,    48,     6,     0,    57,     0,    59,     0,    15,
    61,    58,     0,    16,    57,     0,    41,    61,    58,     0,
    62,     0,    60,     0,    59,    17,    61,     0,    60,    41,
    61,     0,    62,    20,    62,     0,    39,    57,    40,     0,
    63,     0,    66,     0,    71,     0,    70,     0,    72,     0,
    69,     0,    75,     0,    42,    43,     0,    42,    64,    43,
     0,    39,    40,     0,    39,    65,    40,     0,    57,     0,
    65,     0,    62,    30,    62,     0,    62,    41,    64,     0,
    62,    32,     0,    32,     0,    67,     0,    68,     0,     4,
     0,    67,     4,     0,     5,     0,    49,    74,     0,    14,
    62,     0,    33,    62,     0,    34,    62,     0,    62,    20,
    62,     0,    62,    21,    62,     0,    62,    22,    62,     0,
    62,    23,    62,     0,    62,    24,    62,     0,    62,    25,
    62,     0,    62,    26,    62,     0,    62,    27,    62,     0,
    62,    28,    62,     0,    62,    29,    62,     0,    62,    33,
    62,     0,    62,    34,    62,     0,    62,    35,    62,     0,
    62,    36,    62,     0,    62,    37,    62,     0,    62,    31,
    62,     0,    62,    12,    62,     0,    62,    13,    62,     0,
     7,    62,     8,    57,    73,    11,     0,    10,    62,     8,
    57,    73,     0,     9,    57,     0,    63,     0,    39,    57,
    40,     0,    50,     0,    18,    77,     0,    78,     0,    77,
    41,    78,     0,    49,     0,    19,    80,     0,    81,     0,
    80,    41,    81,     0,    49,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   181,   183,   184,   186,   187,   188,   189,   190,   191,   195,
   207,   214,   224,   226,   228,   230,   232,   234,   236,   238,
   240,   242,   244,   246,   248,   250,   252,   254,   256,   258,
   261,   264,   265,   267,   280,   293,   306,   319,   328,   330,
   336,   338,   344,   346,   349,   355,   362,   371,   373,   375,
   377,   379,   381,   383,   385,   388,   390,   392,   394,   397,
   401,   406,   410,   414,   418,   423,   431,   434,   436,   442,
   445,   451,   455,   457,   466,   470,   474,   478,   482,   486,
   490,   494,   498,   502,   506,   510,   514,   518,   522,   526,
   530,   536,   543,   552,   559,   562,   564,   567,   588,   590,
   591,   593,   602,   604,   605,   607
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","IDENTIFIER",
"STRING","INTEGER","ARROW","IF","THEN","ELSE","ELSIF","FI","OR","AND","NOT",
"LET","IN","WHERE","OVERRIDE","REPLACE","EQ","NE","GT","GE","LT","LE","HALIGN",
"VALIGN","UALIGN","TALIGN","APPEND","CONS","THREEDOTS","'+'","'-'","'*'","'/'",
"'%'","';'","'('","')'","','","'['","']'","file","item_list","item","function_declaration",
"function_header","function_identifier","identifier","function_definition","local_definition",
"local_header","global_definition","global_header","function_body","box_expression_with_defs",
"in_box_expression","box_expression_with_wheres","box_expression_with_where",
"var_definition","box_expression","list_expression","box_expression_list","multiple_box_expression_list",
"const_expression","string_constant","numeric_constant","function_call","unary_expression",
"binary_expression","cond_expression","else_expression","function_argument",
"argument_or_function","override_declaration","override_list","override_identifier",
"replace_declaration","replace_list","replace_identifier", NULL
};
#endif

static const short yyr1[] = {     0,
    44,    45,    45,    46,    46,    46,    46,    46,    46,    47,
    48,    48,    49,    49,    49,    49,    49,    49,    49,    49,
    49,    49,    49,    49,    49,    49,    49,    49,    49,    49,
    50,    51,    51,    52,    53,    54,    55,    56,    57,    57,
    58,    58,    59,    59,    60,    60,    61,    62,    62,    62,
    62,    62,    62,    62,    62,    63,    63,    63,    63,    64,
    64,    65,    65,    65,    65,    66,    66,    67,    67,    68,
    69,    70,    70,    70,    71,    71,    71,    71,    71,    71,
    71,    71,    71,    71,    71,    71,    71,    71,    71,    71,
    71,    71,    72,    73,    73,    74,    74,    75,    76,    77,
    77,    78,    79,    80,    80,    81
};

static const short yyr2[] = {     0,
     1,     0,     2,     2,     2,     1,     1,     1,     2,     1,
     2,     1,     1,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     1,     1,     1,     2,     2,     2,     2,     1,     1,     3,
     2,     3,     1,     1,     3,     3,     3,     3,     1,     1,
     1,     1,     1,     1,     1,     2,     3,     2,     3,     1,
     1,     3,     3,     2,     1,     1,     1,     1,     2,     1,
     2,     2,     2,     2,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     6,     5,     2,     1,     3,     1,     2,     1,
     3,     1,     2,     1,     3,     1
};

static const short yydefact[] = {     2,
     0,     0,    31,     0,     0,     8,     0,     3,     0,    10,
    12,    13,     0,    32,     0,    33,     0,     6,     7,     9,
   102,    99,   100,   106,   103,   104,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     4,    37,    35,     0,     0,    96,    11,
     5,    68,    70,     0,     0,     0,     0,     0,     0,     0,
    98,    34,    38,    39,    44,    43,    49,    50,    66,    67,
    54,    52,    51,    53,    55,    36,     0,     0,    30,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    29,
    24,    25,    26,    27,    28,    65,    58,     0,    43,     0,
    56,    60,     0,    61,     0,    72,     0,     0,    73,    74,
     0,     0,     0,     0,    71,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    69,   101,   105,    97,     0,
    64,     0,    59,    57,     0,     0,     0,    40,     0,    48,
    45,    46,    91,    92,    75,    76,    77,    78,    79,    80,
    81,    82,    83,    84,    90,    85,    86,    87,    88,    89,
    62,    63,     0,    41,     0,    47,     0,     0,     0,    42,
    95,     0,    93,     0,     0,    94,     0,     0,     0
};

static const short yydefgoto[] = {   187,
     1,     8,     9,    10,    60,    61,    13,    14,    15,    16,
    17,    62,    63,   148,    64,    65,   107,    66,    67,   103,
   100,    68,    69,    70,    71,    72,    73,    74,   179,    50,
    75,    18,    22,    23,    19,    25,    26
};

static const short yypact[] = {-32768,
    16,    -5,-32768,    22,    22,-32768,   498,-32768,     1,     8,
   -24,-32768,    14,-32768,   223,-32768,   223,-32768,-32768,-32768,
-32768,   -14,-32768,-32768,    -1,-32768,    18,    30,    32,    38,
    47,    49,    63,    64,    88,    92,    93,    94,    95,   103,
   105,   114,   115,-32768,-32768,-32768,   177,    17,-32768,-32768,
-32768,-32768,-32768,   300,   300,   300,   300,   300,   137,   -24,
   -16,-32768,-32768,    25,    56,   409,-32768,-32768,    53,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    22,    22,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   116,   323,   135,
-32768,-32768,   110,-32768,   353,-32768,    -3,   435,    90,    90,
   236,   276,   288,   138,-32768,   300,   300,   300,   300,   300,
   300,   300,   300,   300,   300,   300,   300,   300,   300,   300,
   300,   300,   300,   300,   300,-32768,-32768,-32768,-32768,   300,
-32768,   190,-32768,-32768,   223,   223,   300,-32768,   300,-32768,
-32768,-32768,   480,   516,    40,    40,    57,    57,    57,    57,
   152,   218,   528,   113,    57,    90,    90,-32768,-32768,-32768,
   409,-32768,    20,-32768,    -3,   461,   223,   300,   172,-32768,
-32768,   383,-32768,   223,    20,-32768,   196,   198,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,     2,     4,-32768,-32768,-32768,-32768,
-32768,   173,   -47,    24,-32768,-32768,  -106,   -11,    -7,    58,
   -46,-32768,-32768,-32768,-32768,-32768,-32768,-32768,    21,   141,
-32768,-32768,-32768,   125,-32768,-32768,   129
};


#define	YYLAST		565


static const short yytable[] = {    98,
   102,   104,    11,    49,    12,    21,    24,    12,    12,   151,
   152,   114,   146,    45,    47,    -1,     2,    48,     3,     3,
    52,    53,   -13,    54,     3,   -13,    77,    46,   177,   178,
    55,    56,    20,     4,     5,    99,    99,   147,    44,    78,
   175,   116,   105,   106,   108,   109,   110,    99,    96,    57,
    58,    51,    49,     6,     7,    59,   136,    79,    48,   101,
     7,   122,   123,   124,   125,   126,   127,   128,   129,    80,
   130,    81,   131,   132,   133,   134,   135,    82,    21,    24,
    12,    12,   126,   127,   128,   129,    83,   130,    84,   131,
   132,   133,   134,   135,   102,   104,   117,   173,   174,   106,
   109,   110,    85,    86,   108,   108,   153,   154,   155,   156,
   157,   158,   159,   160,   161,   162,   163,   164,   165,   166,
   167,   168,   169,   170,   133,   134,   135,    87,   171,   181,
    99,    88,    89,    90,    91,   108,   185,   176,   126,     3,
    52,    53,    92,    54,    93,   131,   132,   133,   134,   135,
   111,    56,   144,    94,    95,   139,    28,    29,    30,    31,
    32,    33,    34,    35,    36,    37,   182,    38,    96,   112,
   113,    41,    42,    43,   143,    59,    97,   150,    48,     3,
    52,    53,   183,    54,   131,   132,   133,   134,   135,    76,
    55,    56,     3,    52,    53,   188,    54,   189,   180,   172,
   115,   137,     0,    55,    56,   186,   138,     0,    96,    57,
    58,     0,     0,     0,     0,    59,    97,     0,    48,     0,
     0,    96,    57,    58,     0,     3,    52,    53,    59,    54,
     0,    48,     0,     0,     0,     0,    55,    56,     3,    52,
    53,     0,    54,   126,     0,   128,   129,     0,     0,    55,
   131,   132,   133,   134,   135,    57,    58,     0,     0,     0,
     0,    59,     0,     0,    48,     0,     0,     0,    57,    58,
     0,     0,     0,     0,    59,    79,     0,    48,     3,    52,
    53,     0,    54,     0,     0,     0,     0,     0,     0,    55,
     3,    52,    53,     0,    54,     0,     0,     0,     0,     0,
     0,    55,     3,    52,    53,     0,    54,     0,    57,    58,
     0,     0,     0,    55,    59,    91,     0,    48,     0,     0,
    57,    58,     0,     0,     0,     0,    59,    92,     0,    48,
     0,     0,    57,    58,   118,   119,     0,     0,    59,     0,
     0,    48,   120,   121,   122,   123,   124,   125,   126,   127,
   128,   129,   140,   130,   141,   131,   132,   133,   134,   135,
   145,     0,     0,   142,   118,   119,     0,     0,     0,     0,
     0,     0,   120,   121,   122,   123,   124,   125,   126,   127,
   128,   129,     0,   130,     0,   131,   132,   133,   134,   135,
   184,     0,     0,     0,   118,   119,     0,     0,     0,     0,
     0,     0,   120,   121,   122,   123,   124,   125,   126,   127,
   128,   129,     0,   130,     0,   131,   132,   133,   134,   135,
   118,   119,     0,     0,     0,     0,     0,     0,   120,   121,
   122,   123,   124,   125,   126,   127,   128,   129,     0,   130,
     0,   131,   132,   133,   134,   135,   118,   119,     0,     0,
     0,     0,     0,     0,   149,   121,   122,   123,   124,   125,
   126,   127,   128,   129,     0,   130,     0,   131,   132,   133,
   134,   135,   -75,   -75,     0,     0,     0,     0,     0,     0,
   -75,   -75,   122,   123,   124,   125,   126,   127,   128,   129,
     0,   130,   119,   131,   132,   133,   134,   135,     0,   120,
   121,   122,   123,   124,   125,   126,   127,   128,   129,     0,
   130,    27,   131,   132,   133,   134,   135,    28,    29,    30,
    31,    32,    33,    34,    35,    36,    37,     0,    38,     0,
    39,    40,    41,    42,    43,   120,   121,   122,   123,   124,
   125,   126,   127,   128,   129,     0,   130,     0,   131,   132,
   133,   134,   135,   126,     0,     0,   129,     0,     0,     0,
   131,   132,   133,   134,   135
};

static const short yycheck[] = {    47,
    48,    48,     1,    11,     1,     4,     5,     4,     5,   116,
   117,    59,    16,     6,    39,     0,     1,    42,     3,     3,
     4,     5,    39,     7,     3,    42,    41,    20,     9,    10,
    14,    15,    38,    18,    19,    47,    48,    41,    38,    41,
   147,    17,    54,    55,    56,    57,    58,    59,    32,    33,
    34,    38,    60,    38,    39,    39,     4,    40,    42,    43,
    39,    22,    23,    24,    25,    26,    27,    28,    29,    40,
    31,    40,    33,    34,    35,    36,    37,    40,    77,    78,
    77,    78,    26,    27,    28,    29,    40,    31,    40,    33,
    34,    35,    36,    37,   142,   142,    41,   145,   146,   111,
   112,   113,    40,    40,   116,   117,   118,   119,   120,   121,
   122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
   132,   133,   134,   135,    35,    36,    37,    40,   140,   177,
   142,    40,    40,    40,    40,   147,   184,   149,    26,     3,
     4,     5,    40,     7,    40,    33,    34,    35,    36,    37,
    14,    15,    43,    40,    40,    40,    20,    21,    22,    23,
    24,    25,    26,    27,    28,    29,   178,    31,    32,    33,
    34,    35,    36,    37,    40,    39,    40,    40,    42,     3,
     4,     5,    11,     7,    33,    34,    35,    36,    37,    17,
    14,    15,     3,     4,     5,     0,     7,     0,   175,   142,
    60,    77,    -1,    14,    15,   185,    78,    -1,    32,    33,
    34,    -1,    -1,    -1,    -1,    39,    40,    -1,    42,    -1,
    -1,    32,    33,    34,    -1,     3,     4,     5,    39,     7,
    -1,    42,    -1,    -1,    -1,    -1,    14,    15,     3,     4,
     5,    -1,     7,    26,    -1,    28,    29,    -1,    -1,    14,
    33,    34,    35,    36,    37,    33,    34,    -1,    -1,    -1,
    -1,    39,    -1,    -1,    42,    -1,    -1,    -1,    33,    34,
    -1,    -1,    -1,    -1,    39,    40,    -1,    42,     3,     4,
     5,    -1,     7,    -1,    -1,    -1,    -1,    -1,    -1,    14,
     3,     4,     5,    -1,     7,    -1,    -1,    -1,    -1,    -1,
    -1,    14,     3,     4,     5,    -1,     7,    -1,    33,    34,
    -1,    -1,    -1,    14,    39,    40,    -1,    42,    -1,    -1,
    33,    34,    -1,    -1,    -1,    -1,    39,    40,    -1,    42,
    -1,    -1,    33,    34,    12,    13,    -1,    -1,    39,    -1,
    -1,    42,    20,    21,    22,    23,    24,    25,    26,    27,
    28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
     8,    -1,    -1,    41,    12,    13,    -1,    -1,    -1,    -1,
    -1,    -1,    20,    21,    22,    23,    24,    25,    26,    27,
    28,    29,    -1,    31,    -1,    33,    34,    35,    36,    37,
     8,    -1,    -1,    -1,    12,    13,    -1,    -1,    -1,    -1,
    -1,    -1,    20,    21,    22,    23,    24,    25,    26,    27,
    28,    29,    -1,    31,    -1,    33,    34,    35,    36,    37,
    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    21,
    22,    23,    24,    25,    26,    27,    28,    29,    -1,    31,
    -1,    33,    34,    35,    36,    37,    12,    13,    -1,    -1,
    -1,    -1,    -1,    -1,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    -1,    31,    -1,    33,    34,    35,
    36,    37,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
    -1,    31,    13,    33,    34,    35,    36,    37,    -1,    20,
    21,    22,    23,    24,    25,    26,    27,    28,    29,    -1,
    31,    14,    33,    34,    35,    36,    37,    20,    21,    22,
    23,    24,    25,    26,    27,    28,    29,    -1,    31,    -1,
    33,    34,    35,    36,    37,    20,    21,    22,    23,    24,
    25,    26,    27,    28,    29,    -1,    31,    -1,    33,    34,
    35,    36,    37,    26,    -1,    -1,    29,    -1,    -1,    -1,
    33,    34,    35,    36,    37
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

// This deletes the "kill" array upon destruction.
// Will be used as automatic variable in yyparse().
// Added by $RCSfile: yacctoC.bison,v $.
struct YYMEMHANDLER {
    YYSTYPE *kill;

    YYMEMHANDLER():
	kill(0)
    {}
    ~YYMEMHANDLER()
    {
	if (kill != 0)
    	delete[] kill;
        kill = 0;
    }
};

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <stdlib.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <stdlib.h>
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
  YYMEMHANDLER yymem; // Added by $RCSfile: yacctoC.bison,v $.

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

      // Added by $RCSfile: yacctoC.bison,v $. 
      YYSTYPE *new_yyvs = new YYSTYPE [yystacksize];
      if (new_yyvs == 0)
      {
          yyerror("parser stack overflow");
          return 2;
      }
      for (int yycopy = 0; yycopy < yystacksize; yycopy++)
      {
          new_yyvs[yycopy] = yyvs[yycopy];
      }
      delete[] yyvs; yyvs = new_yyvs;
      yymem.kill = yyvs; // make sure yyvs is deleted upon return 

#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1, size * sizeof (*yylsp));
#endif
      yyvs1 = yyvs1; /* Avoid warnings about unused `yyvs1' - AZ */
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

case 10:
#line 196 "vsl-gramma.Y"
{
				  if (yyvsp[0].header.pattern)
				  {
				    vsllib->add(*yyvsp[0].header.id,
				      yyvsp[0].header.pattern, 0, False,
				      *yyvsp[0].header.file, yyvsp[0].header.line);
				  }
				  delete yyvsp[0].header.id;
				  delete yyvsp[0].header.file;
				;
    break;}
case 11:
#line 208 "vsl-gramma.Y"
{
				  yyval.header.id      = yyvsp[-1].str;
				  yyval.header.pattern = yyvsp[0].node;
				  yyval.header.file    = new string(yyfilename);
				  yyval.header.line    = yylinenumber;
				;
    break;}
case 12:
#line 215 "vsl-gramma.Y"
{
				  yyval.header.id      = new string("#" + *yyvsp[0].str);
				  yyval.header.pattern = new EmptyListNode;
				  yyval.header.file    = new string(yyfilename);
				  yyval.header.line    = yylinenumber;

				  delete yyvsp[0].str;
				;
    break;}
case 13:
#line 225 "vsl-gramma.Y"
{ yyval.str = yyvsp[0].str; ;
    break;}
case 14:
#line 227 "vsl-gramma.Y"
{ yyval.str = new string("(=)"); ;
    break;}
case 15:
#line 229 "vsl-gramma.Y"
{ yyval.str = new string("(<>)"); ;
    break;}
case 16:
#line 231 "vsl-gramma.Y"
{ yyval.str = new string("(>)"); ;
    break;}
case 17:
#line 233 "vsl-gramma.Y"
{ yyval.str = new string("(>=)"); ;
    break;}
case 18:
#line 235 "vsl-gramma.Y"
{ yyval.str = new string("(<)"); ;
    break;}
case 19:
#line 237 "vsl-gramma.Y"
{ yyval.str = new string("(<=)"); ;
    break;}
case 20:
#line 239 "vsl-gramma.Y"
{ yyval.str = new string("(&)"); ;
    break;}
case 21:
#line 241 "vsl-gramma.Y"
{ yyval.str = new string("(|)"); ;
    break;}
case 22:
#line 243 "vsl-gramma.Y"
{ yyval.str = new string("(^)"); ;
    break;}
case 23:
#line 245 "vsl-gramma.Y"
{ yyval.str = new string("(~)"); ;
    break;}
case 24:
#line 247 "vsl-gramma.Y"
{ yyval.str = new string("(+)"); ;
    break;}
case 25:
#line 249 "vsl-gramma.Y"
{ yyval.str = new string("(-)"); ;
    break;}
case 26:
#line 251 "vsl-gramma.Y"
{ yyval.str = new string("(*)"); ;
    break;}
case 27:
#line 253 "vsl-gramma.Y"
{ yyval.str = new string("(/)"); ;
    break;}
case 28:
#line 255 "vsl-gramma.Y"
{ yyval.str = new string("(%)"); ;
    break;}
case 29:
#line 257 "vsl-gramma.Y"
{ yyval.str = new string("(::)"); ;
    break;}
case 30:
#line 259 "vsl-gramma.Y"
{ yyval.str = new string("(not)"); ;
    break;}
case 31:
#line 262 "vsl-gramma.Y"
{ yyval.str = new string((char *)yytext); ;
    break;}
case 34:
#line 268 "vsl-gramma.Y"
{ 
				  if (yyvsp[-1].header.pattern)
				  {
				    // Funktion definieren
				    vsllib->add(*yyvsp[-1].header.id,
				      yyvsp[-1].header.pattern, yyvsp[0].node, False,
				      *yyvsp[-1].header.file, yyvsp[-1].header.line);
				  }
				  delete yyvsp[-1].header.id;
				  delete yyvsp[-1].header.file;
				;
    break;}
case 35:
#line 281 "vsl-gramma.Y"
{
				  if (yyvsp[-1].header.pattern)
				  {
				    // Funktion bereits deklarieren
				    // (fuer rekursive Aufrufe)
				    vsllib->add(*yyvsp[-1].header.id,
				      yyvsp[-1].header.pattern->dup(), 0, False,
				      yyfilename, yylinenumber);
				  }
				  yyval.header = yyvsp[-1].header;
				;
    break;}
case 36:
#line 294 "vsl-gramma.Y"
{ 
				  if (yyvsp[-1].header.pattern)
				  {
				    // Funktion definieren
				    vsllib->add(*yyvsp[-1].header.id, 
				      yyvsp[-1].header.pattern, yyvsp[0].node, True,
				      *yyvsp[-1].header.file, yyvsp[-1].header.line);
				  }
				  delete yyvsp[-1].header.id;
				  delete yyvsp[-1].header.file;
				;
    break;}
case 37:
#line 307 "vsl-gramma.Y"
{
				  if (yyvsp[-1].header.pattern)
				  {
				    // Funktion bereits deklarieren
				    // (fuer rekursive Aufrufe)
				    vsllib->add(*yyvsp[-1].header.id,
					yyvsp[-1].header.pattern->dup(), 0, True,
					yyfilename, yylinenumber);
				  }
				  yyval.header = yyvsp[-1].header;
				;
    break;}
case 38:
#line 320 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 39:
#line 329 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 40:
#line 331 "vsl-gramma.Y"
{ 
				  yyval.node = (yyvsp[-1].vardef.pattern && yyvsp[-1].vardef.args && yyvsp[0].node) ?
				    new LetNode(yyvsp[-1].vardef.pattern, yyvsp[-1].vardef.args, yyvsp[0].node) : 0;
				;
    break;}
case 41:
#line 337 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 42:
#line 339 "vsl-gramma.Y"
{ 
				  yyval.node = (yyvsp[-1].vardef.pattern && yyvsp[-1].vardef.args && yyvsp[0].node) ?
				    new LetNode(yyvsp[-1].vardef.pattern, yyvsp[-1].vardef.args, yyvsp[0].node) : 0;
				;
    break;}
case 43:
#line 345 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 44:
#line 347 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 45:
#line 351 "vsl-gramma.Y"
{
				  yyval.node = (yyvsp[0].vardef.pattern && yyvsp[0].vardef.args && yyvsp[-2].node) ?
				    new WhereNode(yyvsp[0].vardef.pattern, yyvsp[0].vardef.args, yyvsp[-2].node) : 0;
				;
    break;}
case 46:
#line 357 "vsl-gramma.Y"
{
				  yyval.node = (yyvsp[0].vardef.pattern && yyvsp[0].vardef.args && yyvsp[-2].node) ?
				    new WhereNode(yyvsp[0].vardef.pattern, yyvsp[0].vardef.args, yyvsp[-2].node) : 0;
				;
    break;}
case 47:
#line 363 "vsl-gramma.Y"
{
				  yyval.vardef.pattern = yyvsp[-2].node;
				  yyval.vardef.args    = yyvsp[0].node;
				;
    break;}
case 48:
#line 372 "vsl-gramma.Y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 49:
#line 374 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 50:
#line 376 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 51:
#line 378 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 52:
#line 380 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 53:
#line 382 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 54:
#line 384 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 55:
#line 386 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 56:
#line 389 "vsl-gramma.Y"
{ yyval.node = new EmptyListNode; ;
    break;}
case 57:
#line 391 "vsl-gramma.Y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 58:
#line 393 "vsl-gramma.Y"
{ yyval.node = new EmptyListNode; ;
    break;}
case 59:
#line 395 "vsl-gramma.Y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 60:
#line 398 "vsl-gramma.Y"
{ 
				  yyval.node = (yyvsp[0].node) ? new FixListNode(yyvsp[0].node) : 0;
				;
    break;}
case 61:
#line 402 "vsl-gramma.Y"
{ 
				  yyval.node = yyvsp[0].node; 
				;
    break;}
case 62:
#line 407 "vsl-gramma.Y"
{ 
				  yyval.node = (yyvsp[-2].node && yyvsp[0].node) ? new ListNode(yyvsp[-2].node, yyvsp[0].node) : 0;
				;
    break;}
case 63:
#line 411 "vsl-gramma.Y"
{
				  yyval.node = (yyvsp[-2].node && yyvsp[0].node) ? new ListNode(yyvsp[-2].node, yyvsp[0].node) : 0;
				;
    break;}
case 64:
#line 415 "vsl-gramma.Y"
{ 
				  yyval.node = yyvsp[-1].node; 
				;
    break;}
case 65:
#line 419 "vsl-gramma.Y"
{
				  yyval.node = new NameNode("...");
				;
    break;}
case 66:
#line 424 "vsl-gramma.Y"
{ 
				  // bug workaround
				  char *buf = (char *)*yyvsp[0].str;
				  string name = buf;
				  yyval.node = new StringNode(name);
				  delete yyvsp[0].str;
				;
    break;}
case 67:
#line 432 "vsl-gramma.Y"
{ yyval.node = new NumNode(yyvsp[0].num); ;
    break;}
case 68:
#line 435 "vsl-gramma.Y"
{ yyval.str = new string(unquote((char *)yytext)); ;
    break;}
case 69:
#line 437 "vsl-gramma.Y"
{ 
				  yyval.str = yyvsp[-1].str;
				  *yyval.str += unquote((char *)yytext);
				;
    break;}
case 70:
#line 443 "vsl-gramma.Y"
{ yyval.num = atoi((char *)yytext); ;
    break;}
case 71:
#line 446 "vsl-gramma.Y"
{
				  yyval.node = (yyvsp[0].node) ? 
				    vsllib->_call(*yyvsp[-1].str, yyvsp[0].node) : 0;
				;
    break;}
case 72:
#line 452 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(not)", yyvsp[0].node); 
				;
    break;}
case 73:
#line 456 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 74:
#line 458 "vsl-gramma.Y"
{
				  // -x durch 0-x simulieren
				  yyval.node = (yyvsp[0].node) ? vsllib->call("(-)", 
					new NullNode, yyvsp[0].node) : 0;
				;
    break;}
case 75:
#line 467 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(=)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 76:
#line 471 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(<>)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 77:
#line 475 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(>)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 78:
#line 479 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(>=)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 79:
#line 483 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(<)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 80:
#line 487 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(<=)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 81:
#line 491 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(&)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 82:
#line 495 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(|)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 83:
#line 499 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(^)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 84:
#line 503 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(~)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 85:
#line 507 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(+)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 86:
#line 511 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(-)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 87:
#line 515 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(*)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 88:
#line 519 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(/)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 89:
#line 523 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(%)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 90:
#line 527 "vsl-gramma.Y"
{ 
				  yyval.node = vsllib->call("(::)", yyvsp[-2].node, yyvsp[0].node); 
				;
    break;}
case 91:
#line 531 "vsl-gramma.Y"
{ 
				  // if expr-1 then true else expr-2
				  yyval.node = (yyvsp[-2].node && yyvsp[0].node) ? 
			 	    new TestNode(yyvsp[-2].node, new TrueNode, yyvsp[0].node) : 0;
				;
    break;}
case 92:
#line 537 "vsl-gramma.Y"
{ 
				  // if expr-1 then expr-2 else false
				  yyval.node = (yyvsp[-2].node && yyvsp[0].node) ? 
				    new TestNode(yyvsp[-2].node, yyvsp[0].node, new FalseNode) : 0;
				;
    break;}
case 93:
#line 547 "vsl-gramma.Y"
{ 
				  yyval.node = (yyvsp[-4].node && yyvsp[-2].node && yyvsp[-1].node) ?
					new TestNode(yyvsp[-4].node, yyvsp[-2].node, yyvsp[-1].node) : 0;
				;
    break;}
case 94:
#line 555 "vsl-gramma.Y"
{ 
				  yyval.node = (yyvsp[-3].node && yyvsp[-1].node && yyvsp[0].node) ?
					new TestNode(yyvsp[-3].node, yyvsp[-1].node, yyvsp[0].node) : 0;
				;
    break;}
case 95:
#line 560 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 96:
#line 563 "vsl-gramma.Y"
{ yyval.node = yyvsp[0].node; ;
    break;}
case 97:
#line 565 "vsl-gramma.Y"
{ yyval.node = (yyvsp[-1].node) ? new FixListNode(yyvsp[-1].node) : 0; ;
    break;}
case 98:
#line 568 "vsl-gramma.Y"
{
				  if (*yyvsp[0].str == "_")
				    yyval.node = new DummyNode;
				  else
				  {
				    // Wenn Funktion definiert, diese nehmen;
				    // sonst Platzhalter Variable bilden.

				    if (vsllib->deflist("#" + *yyvsp[0].str))
				      yyval.node = vsllib->call("#" + *yyvsp[0].str);
				    else
				      yyval.node = new NameNode(*yyvsp[0].str);
				  }

				  delete yyvsp[0].str;
				;
    break;}
case 102:
#line 594 "vsl-gramma.Y"
{
				  string func_name = *yyvsp[0].str;
				  if (vsllib->override(func_name)
				    && vsllib->override("#" + func_name))
				    VSLLib::parse_error("'" + func_name + 
				      "(...)' undefined");
				;
    break;}
case 106:
#line 608 "vsl-gramma.Y"
{
				  string func_name = *yyvsp[0].str;
				  if (vsllib->replace(func_name)
				    && vsllib->replace("#" + func_name))
				    VSLLib::parse_error("'" + func_name + 
				      "(...)' undefined");
				;
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
	       x < (int)(sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = new char [size + 15];
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (int)(sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      delete [] msg;
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
#line 616 "vsl-gramma.Y"
 /* DO NOT REMOVE THIS COMMENT -- MUNCH-YACC DEPENDS ON IT */
