#if ( hpux || _AIX || ultrix || __osf__)
/* nothing */
#else
extern char *malloc(), *realloc();
#endif

    /* This file is a transformation of James A. Roskind's C++ grammar */


    /* Copyright (C) 1989,1990 James A. Roskind, All rights reserved.
    This grammar was developed  and  written  by  James  A.  Roskind.
    Copying  of  this  grammar  description, as a whole, is permitted
    providing this notice is intact and applicable  in  all  complete
    copies.   Translations as a whole to other parser generator input
    languages  (or  grammar  description  languages)   is   permitted
    provided  that  this  notice is intact and applicable in all such
    copies,  along  with  a  disclaimer  that  the  contents  are   a
    translation.   The reproduction of derived text, such as modified
    versions of this grammar, or the output of parser generators,  is
    permitted,  provided  the  resulting  work includes the copyright
    notice "Portions Copyright (c)  1989,  1990  James  A.  Roskind".
    Derived products, such as compilers, translators, browsers, etc.,
    that  use  this  grammar,  must also provide the notice "Portions
    Copyright  (c)  1989,  1990  James  A.  Roskind"  in   a   manner
    appropriate  to  the  utility,  and in keeping with copyright law
    (e.g.: EITHER displayed when first invoked/executed; OR displayed
    continuously on display terminal; OR via placement in the  object
    code  in  form  readable in a printout, with or near the title of
    the work, or at the end of the file).  No royalties, licenses  or
    commissions  of  any  kind are required to copy this grammar, its
    translations, or derivative products, when the copies are made in
    compliance with this notice. Persons or corporations that do make
    copies in compliance with this notice may charge  whatever  price
    is  agreeable  to  a  buyer, for such copies or derivative works.
    THIS GRAMMAR IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESS  OR
    IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED
    WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR
    PURPOSE.

    James A. Roskind
    Independent Consultant
    516 Latania Palm Drive
    Indialantic FL, 32903
    (407)729-4348
    jar@ileaf.com


    ---end of copyright notice---


This file is a companion file to a C++ grammar description file.

*/


/* FILENAME: C.Y */

/*  This  is a grammar file for the dpANSI C language.  This file was
last modified by J. Roskind on 3/7/90. Version 1.00 */




/* ACKNOWLEDGMENT:

Without the effort expended by the ANSI C standardizing committee,  I
would  have been lost.  Although the ANSI C standard does not include
a fully disambiguated syntax description, the committee has at  least
provided most of the disambiguating rules in narratives.

Several  reviewers  have also recently critiqued this grammar, and/or
assisted in discussions during it's preparation.  These reviewers are
certainly not responsible for the errors I have committed  here,  but
they  are responsible for allowing me to provide fewer errors.  These
colleagues include: Bruce Blodgett, and Mark Langley. */

#include "rename.h"
  

#ifdef flex
extern char * yytext;
#else
/* lex mylex */
extern char yytext[];
#endif

static char * stringliterallist;
# define AUTO 257
# define DOUBLE 258
# define INT 259
# define STRUCT 260
# define BREAK 261
# define ELSE 262
# define LONG 263
# define SWITCH 264
# define CASE 265
# define ENUM 266
# define REGISTER 267
# define TYPEDEF 268
# define CHAR 269
# define EXTERN 270
# define RETURN 271
# define UNION 272
# define CONST 273
# define FLOAT 274
# define SHORT 275
# define UNSIGNED 276
# define CONTINUE 277
# define FOR 278
# define SIGNED 279
# define VOID 280
# define DEFAULT 281
# define GOTO 282
# define SIZEOF 283
# define VOLATILE 284
# define DO 285
# define IF 286
# define STATIC 287
# define WHILE 288
# define IDENTIFIER 289
# define STRINGliteral 290
# define FLOATINGconstant 291
# define INTEGERconstant 292
# define CHARACTERconstant 293
# define OCTALconstant 294
# define HEXconstant 295
# define TYPEDEFname 296
# define ARROW 297
# define ICR 298
# define DECR 299
# define LS 300
# define RS 301
# define LE 302
# define GE 303
# define EQ 304
# define NE 305
# define ANDAND 306
# define OROR 307
# define ELLIPSIS 308
# define MULTassign 309
# define DIVassign 310
# define MODassign 311
# define PLUSassign 312
# define MINUSassign 313
# define LSassign 314
# define RSassign 315
# define ANDassign 316
# define ERassign 317
# define ORassign 318
# define SYNTAX_ERROR 319
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

/* ----end of grammar----*/

#include <stdio.h>

#include "decl.h"

int numlig;
char * yyinfilename = 0;
extern char err_msg[];

yyerror(string)
     char *string;
{
  /* Attention, la forme du message est connue dans smacXcoral,c */

  if (! *err_msg)
    /* Pas une erreur (f)lex */
    if (yyinfilename)
      sprintf(err_msg, "parser error in %s line %d : %s",
	      yyinfilename, numlig, string);
    else
      sprintf(err_msg, "parser error line %d : %s", numlig, string);
}

int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 96,
	289, 180,
	-2, 169,
-1, 196,
	40, 172,
	91, 172,
	-2, 171,
-1, 308,
	41, 171,
	44, 171,
	-2, 170,
	};
# define YYNPROD 200
# define YYLAST 854
int yyact[]={

    42,   142,    62,    23,   154,    52,   174,   175,   156,   166,
   211,   263,   168,    24,   180,   181,    21,    10,    85,    86,
   216,   176,   318,   177,    25,    63,    88,   127,    23,   128,
    71,    15,    22,   127,    20,   128,   102,   262,    24,    27,
    93,   124,    89,   127,    81,   128,   257,   317,   295,    25,
   233,   265,   312,    84,    22,   294,    20,   134,   281,   155,
   167,    22,   191,    20,    61,   170,   138,    90,    78,    15,
    79,    36,   172,    73,   114,    83,   119,    74,    75,   264,
    76,   116,    11,   192,   287,    32,   105,   259,    11,    22,
   191,    20,   188,   232,   106,   126,    11,   195,    22,   191,
   103,   139,   125,   326,   320,    99,   274,   203,   209,   202,
   218,    78,    99,   139,    96,   191,    73,   111,    83,    22,
    74,    75,   124,    76,    96,    44,     4,   275,   296,   296,
   137,    26,    22,    30,    20,   136,   102,    37,   289,   130,
    99,   217,   219,   220,   126,   127,   112,   128,   169,    99,
   276,   193,    35,   129,   107,   108,    37,   110,    15,   225,
   120,    77,   139,   186,    92,    99,   330,    23,   184,    78,
    99,   191,   210,   185,    73,    99,    83,    24,    74,    75,
   308,    76,    78,    99,   221,   205,   201,    73,    25,    83,
   224,    74,    75,   279,    76,   111,   213,   214,   215,   237,
   206,    15,   199,    38,    77,    78,   207,   182,    95,   183,
    73,   230,    83,   269,    74,    75,   238,    76,   196,   222,
   223,   228,   273,   247,   248,   203,   169,   296,   165,    78,
   231,   226,    91,   126,    73,   204,    83,   227,    74,    75,
   254,    76,   239,   240,   328,   267,   126,   329,   153,   143,
   144,   145,   146,   147,   148,   149,   150,   151,   152,    15,
   135,   208,    77,   178,   179,   319,   158,   159,   139,   298,
   133,   299,   300,   301,   266,    77,    85,    86,   132,    78,
   303,    21,    85,    86,    73,   131,    83,   283,    74,    75,
   173,    76,    85,    86,    23,    15,    59,   123,    77,    54,
    50,   304,   297,    21,    24,    31,    60,   315,   322,   126,
    21,   102,    58,    57,   190,    25,    51,   290,    70,   325,
    56,    53,    77,    55,    21,    87,   327,    85,    86,   315,
   282,   332,   331,    67,    68,   109,   313,    23,   302,    59,
   126,    97,    54,    50,   283,   245,   246,    24,    22,    60,
    20,   314,   187,   316,   286,    58,    57,   285,    25,    51,
    13,    70,   126,    56,    53,   307,    55,    21,    87,    15,
    85,    86,    77,   324,    16,   206,    67,    68,   277,    78,
   234,   278,   272,    40,    73,   139,    83,   282,    74,    75,
   271,    76,   266,   139,    85,    86,   113,    59,    33,    94,
    54,    50,   270,   229,   260,   139,   139,    60,   261,   241,
   242,   243,   244,    58,    57,     2,    39,    51,     9,    70,
   212,    56,    53,   121,    55,    21,    87,   268,    85,    86,
    48,    15,    70,    59,    67,    68,    54,    50,    21,    87,
    47,    85,    86,    60,   234,   284,    46,    67,    68,    58,
    57,    45,    43,    51,   200,    70,     3,    56,    53,   252,
    55,    21,    87,   256,    85,    86,    41,   234,    78,   163,
    67,    68,    77,    73,   255,    83,    19,    74,    75,    70,
    76,    34,     6,   115,     5,    21,    87,    12,    85,    86,
    28,    49,     8,   141,    67,    68,   122,   140,   100,   117,
    14,   234,   284,    65,    69,   321,    14,    59,   305,   157,
    54,    50,    64,    66,    14,    72,   323,    60,    14,   189,
    82,    49,     1,    58,    57,     0,     0,    51,     0,    70,
     0,    56,    53,     0,    55,    21,    87,     0,    85,    86,
     0,    78,     0,     0,    67,    68,    73,     0,   161,     0,
    74,    75,     0,    76,     0,     0,    80,    17,   203,    98,
    18,    77,     0,    17,     0,   171,    18,    23,     0,     0,
     0,    17,     0,     0,    18,    17,     0,    24,    18,     0,
   160,   162,   164,     7,   249,   250,   251,     0,    25,     0,
    29,    17,     0,    17,   118,     0,   118,    21,   101,     0,
     0,   100,   104,     0,     0,    23,    17,     0,     0,   118,
     0,     0,     0,     0,     0,    24,     0,     0,     0,     0,
     0,     0,   194,   197,   198,     0,    25,     0,     0,    70,
   280,     0,     0,     0,    77,    21,    87,     0,    85,    86,
     0,     0,     0,     0,    67,    68,     0,   171,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   164,     0,     0,     0,
     0,   235,   258,     0,    17,    17,     0,   118,   118,   164,
   164,     0,     0,     0,     0,   164,   164,   164,   164,   164,
   164,   164,   164,   164,   164,   164,   164,   164,   164,   164,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    70,     0,
     0,     0,     0,     0,    21,    87,   197,    85,    86,     0,
     0,   236,     0,    67,    68,   235,   235,     0,     0,     0,
     0,     0,   291,   164,   253,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   292,     0,
     0,     0,    17,     0,     0,   118,     0,     0,     0,     0,
     0,   258,     0,     0,     0,   288,   100,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   164,     0,
     0,    70,   292,   292,     0,   236,   236,    21,    87,     0,
    85,    86,   306,   309,   310,     0,    67,    68,     0,     0,
     0,     0,     0,     0,     0,    17,     0,     0,   293,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   311,     0,     0,     0,     0,     0,     0,    17,
    17,     0,   293,   293 };
int yypact[]={

   308, -1000, -1000, -1000, -1000,   -54,    -8,   -92,    93,    78,
 -1000,    84, -1000, -1000,    -8, -1000, -1000, -1000,    -8, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,    33,   -54,   -92,
 -1000,   -92, -1000,    87,    -8, -1000,    -8, -1000, -1000,    35,
   172, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,    -8,
     0,    95,    80,   245,   238,   230,   246,   220,    76,    71,
   196,   118, -1000, -1000,   -60,   -59,   -32,   508,   508,   196,
   188,  -297, -1000, -1000, -1000, -1000, -1000, -1000, -1000,   -64,
 -1000, -1000,  -278,   346,   -22, -1000, -1000, -1000,   252,  -298,
   -39,  -286,   164,   126, -1000, -1000, -1000,     1,   130,   -10,
 -1000, -1000,   177,    74,   177, -1000,   435, -1000, -1000,   -92,
 -1000, -1000, -1000,   112, -1000, -1000,    79,    -8,    -8,    33,
 -1000,   136, -1000, -1000, -1000,    50, -1000,  -274,  -282, -1000,
 -1000,   196,   196,   196,  -268,   196, -1000, -1000,    51,   196,
   196, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,   196,   196,   196,   149, -1000, -1000, -1000,
 -1000,   196, -1000, -1000, -1000,  -231,   196,   196, -1000,   362,
   170,    92,   196,   196,   196,   196,   196,   196,   196,   196,
   196,   196,   196,   196,   196,   196,   196,  -273,     0, -1000,
  -231, -1000, -1000,    -6, -1000,    79, -1000, -1000, -1000,    79,
 -1000,   -16, -1000, -1000, -1000,    -8,    58, -1000, -1000, -1000,
 -1000, -1000,   246,   361,   349,   341,   182,    47, -1000, -1000,
 -1000,  -297,    69,    57, -1000,   337,   152,   -64,   -22, -1000,
   196, -1000, -1000, -1000, -1000,    92,    49,   252,  -298,   -39,
   -39,  -286,  -286,  -286,  -286,   164,   164,   126,   126, -1000,
 -1000, -1000,   313, -1000,    -9,   130,    94, -1000,    14, -1000,
 -1000, -1000,     4, -1000,   -16, -1000, -1000, -1000,   246, -1000,
   246,   246,   246,   196,   196,   196, -1000, -1000, -1000, -1000,
 -1000, -1000,   139,   130,   130,  -273, -1000, -1000, -1000,  -256,
 -1000, -1000,    14,    21, -1000,   102, -1000,     3, -1000,  -240,
 -1000, -1000,   224,    45, -1000,   196, -1000,    79, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,   102,   246,    44,
   196,   203, -1000, -1000, -1000, -1000, -1000,   125, -1000,   196,
   246, -1000, -1000 };
int yypgo[]={

     0,   522,   556,    44,   520,   515,    64,   513,   509,     2,
   508,   505,   512,   504,   399,    65,    40,   164,   232,    67,
    42,    26,    53,    70,    30,   503,    25,   497,   493,    51,
     5,    71,   456,   492,   481,    39,    86,   487,   476,   474,
   463,    46,    58,   483,   459,   454,    79,    55,    37,    11,
     0,   452,   125,   451,   446,   440,   430,   427,   420,   418,
   416,   383,   415,   583,   305,    85,   398,   396,   499,    17,
    81,   374,   559,   365,   519,    97,   208,   360,   352,    93,
    50,   341,   314 };
int yyr1[]={

     0,     2,     3,     3,     4,     4,     5,     5,     5,     5,
     7,     7,     7,     7,     7,     7,     8,    10,     8,    11,
    11,    12,    12,    12,    12,    12,    13,    13,    13,    13,
    13,    13,    14,    14,    16,    16,    16,    16,    17,    17,
    17,    18,    18,    18,    19,    19,    19,    19,    19,    20,
    20,    20,    21,    21,    22,    22,    23,    23,    24,    24,
    25,    25,    26,    26,     9,     9,    27,    28,    28,    28,
    28,    28,    28,    28,    28,    28,    28,    28,     6,     6,
    29,    29,    29,    30,    30,    31,    32,    33,    33,    34,
    37,    38,    38,    38,    39,    39,    40,    40,    41,    41,
    41,    44,    44,    15,    15,    36,    36,    46,    47,    45,
    45,    45,    49,    49,    49,    49,    48,    48,    50,    50,
    50,    50,    50,    50,    57,    51,    58,    51,    59,    52,
    52,    52,    52,    60,    60,    61,    61,    53,    54,    54,
    54,    55,    55,    55,    56,    56,    56,     1,     1,     1,
     1,    62,    62,    62,    62,    62,    62,    64,    64,    65,
    66,    67,    66,    35,    68,    43,    43,    69,    69,    72,
    73,    74,    75,    71,    71,    71,    70,    70,    63,    63,
    78,    77,    77,    77,    42,    42,    42,    76,    76,    82,
    76,    81,    81,    81,    79,    79,    80,    80,    80,    80 };
int yyr2[]={

     0,     3,     3,     3,     3,     5,     2,     2,     3,     6,
     2,     9,     7,     5,     5,     5,     7,     1,    13,     2,
     6,     2,     5,     5,     5,     9,     3,     3,     3,     3,
     3,     3,     2,     9,     2,     7,     7,     7,     2,     7,
     7,     2,     7,     7,     2,     7,     7,     7,     7,     2,
     7,     7,     2,     7,     2,     7,     2,     7,     2,     7,
     2,     7,     2,    11,     2,     7,     3,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     7,
     2,     4,     5,     1,     2,     3,     5,     6,     8,     2,
     2,     3,     3,     3,     2,     7,     2,     6,     3,     5,
     5,     2,     6,     2,     4,     1,     4,     3,     3,     6,
     8,     2,     6,     8,     2,     3,     2,     6,     2,     2,
     2,     2,     2,     2,     1,    10,     1,     8,     3,     5,
     7,     7,     9,     2,     4,     2,     4,     4,    11,    15,
    11,    11,    15,    19,     5,     5,     7,     3,     3,     3,
     1,     5,     7,     5,     7,     7,     9,     2,     4,     5,
     4,     1,     8,     2,     3,     2,     2,     2,     4,     3,
     3,     3,     3,     4,     6,     8,     2,     6,     2,     4,
     1,    11,     6,     8,     2,     2,     2,     3,     5,     1,
     9,     5,     7,     9,     2,     4,     6,     6,     6,     8 };
int yychk[]={

 -1000,    -1,   -62,   -32,   -52,   -43,   -34,   -63,   -33,   -59,
   -69,   -70,   -37,   -77,   -68,   123,   -71,    -2,   -72,   -38,
    42,   289,    40,   259,   269,   280,   -52,   -35,   -43,   -63,
   -52,   -64,   -65,   -66,   -34,    59,   -31,    44,   125,   -60,
   -61,   -32,   -50,   -51,   -52,   -53,   -54,   -55,   -56,   -34,
   265,   281,   -30,   286,   264,   288,   285,   278,   277,   261,
   271,    -6,    -9,   -26,   -12,   -25,    -7,   298,   299,   -13,
   283,   -24,    -5,    38,    42,    43,    45,   126,    33,   -23,
    -2,    -3,    -4,    40,   -22,   292,   293,   290,   -21,   -20,
   -19,   -18,   -17,   -16,   -14,   -76,    40,   -81,   -72,    91,
   -43,   -63,   -69,   -70,   -63,   -36,    61,   -52,   -52,   -64,
   -52,   -65,    59,   -67,   -35,   -43,   -70,   -68,   -72,   -35,
   125,   -61,   -32,   125,   -50,   -29,    -3,    43,    45,    58,
    59,    40,    40,    40,   -50,    40,    59,    59,   -30,    44,
   -27,   -28,    61,   309,   310,   311,   312,   313,   314,   315,
   316,   317,   318,   307,    63,    91,    40,    -8,   298,   299,
   -12,    40,   -12,   -14,   -12,    40,   306,   124,   290,    -6,
   -15,   -34,    94,    38,   304,   305,    60,    62,   302,   303,
   300,   301,    43,    45,    42,    47,    37,   -78,    91,   -74,
   -82,    41,    93,   -29,   -74,   -75,    41,   -74,   -74,   -75,
   -45,   -46,    -9,   123,   -52,   -31,   -70,   -36,   125,    58,
    -3,   292,   -58,    -6,    -6,    -6,   288,   -30,    59,    -9,
    -9,   -24,    -6,    -6,    41,    -9,   -15,   -23,   -22,    41,
    41,   -42,   -79,   -80,   -76,   -68,   -72,   -21,   -20,   -19,
   -19,   -18,   -18,   -18,   -18,   -17,   -17,   -16,   -16,   -14,
   -14,   -14,   -44,    -2,   -29,   -39,   -40,   -41,   -34,    93,
   -76,   -76,   -48,   -49,   -46,   -29,   290,   -35,   -57,   -50,
    41,    41,    41,    40,    59,    58,    93,    41,    44,    41,
   -14,   -42,   -79,   -80,   -76,    44,    41,    93,   -74,    44,
   -42,   -43,   -68,   -72,   -47,    44,   125,   -48,   -50,   -50,
   -50,   -50,    -6,   -30,   -26,   -10,   -74,   -73,    41,   -74,
   -74,    -2,   308,   -41,   -47,   -49,   -47,    44,   262,    41,
    59,   -11,    -9,   -76,   -47,   -50,    59,   -30,    41,    44,
    41,    -9,   -50 };
int yydef[]={

   150,    -2,   147,   148,   149,     0,     0,     0,     0,    83,
   165,   166,    89,   178,     0,   128,   167,   176,     0,    90,
   164,     1,   169,    91,    92,    93,   151,   105,   163,     0,
   153,     0,   157,   161,     0,    86,     0,    85,   129,    83,
    83,   133,   135,   118,   119,   120,   121,   122,   123,     0,
     0,     0,     0,     0,     0,     0,    83,     0,     0,     0,
    83,    84,    78,    64,    32,    62,    21,     0,     0,     0,
     0,    60,    10,    26,    27,    28,    29,    30,    31,    58,
     6,     7,     8,     0,    56,     2,     3,     4,    54,    52,
    49,    44,    41,    38,    34,   173,    -2,   187,   189,     0,
   168,   179,     0,     0,     0,    87,     0,   152,   154,     0,
   155,   158,   159,     0,   160,   163,   166,     0,     0,   105,
   130,    83,   134,   131,   136,     0,    80,     0,     0,   126,
   137,     0,     0,     0,     0,    83,   144,   145,     0,     0,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,     0,     0,    13,    14,    15,
    22,     0,    23,    24,    32,     0,     0,     0,     5,     0,
     0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     0,   171,   191,     0,   174,     0,    -2,   177,   182,     0,
   106,     0,   111,   107,   156,     0,     0,    88,   132,   124,
    81,    82,    83,     0,     0,     0,     0,     0,   146,    79,
    65,    61,     0,     0,    12,     0,     0,    59,    57,     9,
     0,   104,   184,   185,   186,   194,   189,    55,    53,    50,
    51,    45,    46,    47,    48,    42,    43,    39,    40,    35,
    36,    37,     0,   101,     0,     0,    94,    96,    98,   192,
   175,   183,     0,   116,     0,   114,   115,   162,    83,   127,
    83,    83,    83,     0,    83,     0,    11,    16,    17,    25,
    33,   195,     0,     0,     0,     0,   181,   193,   190,     0,
    99,   100,   194,   189,   109,     0,   108,     0,   125,   138,
   140,   141,     0,     0,    63,     0,   196,     0,    -2,   197,
   198,   102,    95,    97,   110,   117,   112,     0,    83,     0,
    83,     0,    19,   199,   113,   139,   142,     0,    18,     0,
    83,    20,   143 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"AUTO",	257,
	"DOUBLE",	258,
	"INT",	259,
	"STRUCT",	260,
	"BREAK",	261,
	"ELSE",	262,
	"LONG",	263,
	"SWITCH",	264,
	"CASE",	265,
	"ENUM",	266,
	"REGISTER",	267,
	"TYPEDEF",	268,
	"CHAR",	269,
	"EXTERN",	270,
	"RETURN",	271,
	"UNION",	272,
	"CONST",	273,
	"FLOAT",	274,
	"SHORT",	275,
	"UNSIGNED",	276,
	"CONTINUE",	277,
	"FOR",	278,
	"SIGNED",	279,
	"VOID",	280,
	"DEFAULT",	281,
	"GOTO",	282,
	"SIZEOF",	283,
	"VOLATILE",	284,
	"DO",	285,
	"IF",	286,
	"STATIC",	287,
	"WHILE",	288,
	"IDENTIFIER",	289,
	"STRINGliteral",	290,
	"FLOATINGconstant",	291,
	"INTEGERconstant",	292,
	"CHARACTERconstant",	293,
	"OCTALconstant",	294,
	"HEXconstant",	295,
	"TYPEDEFname",	296,
	"ARROW",	297,
	"ICR",	298,
	"DECR",	299,
	"LS",	300,
	"RS",	301,
	"LE",	302,
	"GE",	303,
	"EQ",	304,
	"NE",	305,
	"ANDAND",	306,
	"OROR",	307,
	"ELLIPSIS",	308,
	"MULTassign",	309,
	"DIVassign",	310,
	"MODassign",	311,
	"PLUSassign",	312,
	"MINUSassign",	313,
	"LSassign",	314,
	"RSassign",	315,
	"ANDassign",	316,
	"ERassign",	317,
	"ORassign",	318,
	"SYNTAX_ERROR",	319,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"_IDENTIFIER : IDENTIFIER",
	"constant : INTEGERconstant",
	"constant : CHARACTERconstant",
	"string_literal_list : STRINGliteral",
	"string_literal_list : string_literal_list STRINGliteral",
	"primary_expression : _IDENTIFIER",
	"primary_expression : constant",
	"primary_expression : string_literal_list",
	"primary_expression : '(' comma_expression ')'",
	"postfix_expression : primary_expression",
	"postfix_expression : postfix_expression '[' comma_expression ']'",
	"postfix_expression : postfix_expression '(' ')'",
	"postfix_expression : postfix_expression _argument_expression_list",
	"postfix_expression : postfix_expression ICR",
	"postfix_expression : postfix_expression DECR",
	"_argument_expression_list : '(' assignment_expression ')'",
	"_argument_expression_list : '(' assignment_expression ','",
	"_argument_expression_list : '(' assignment_expression ',' argument_expression_list ')'",
	"argument_expression_list : assignment_expression",
	"argument_expression_list : argument_expression_list ',' assignment_expression",
	"unary_expression : postfix_expression",
	"unary_expression : ICR unary_expression",
	"unary_expression : DECR unary_expression",
	"unary_expression : unary_operator cast_expression",
	"unary_expression : SIZEOF '(' type_name ')'",
	"unary_operator : '&'",
	"unary_operator : '*'",
	"unary_operator : '+'",
	"unary_operator : '-'",
	"unary_operator : '~'",
	"unary_operator : '!'",
	"cast_expression : unary_expression",
	"cast_expression : '(' type_name ')' cast_expression",
	"multiplicative_expression : cast_expression",
	"multiplicative_expression : multiplicative_expression '*' cast_expression",
	"multiplicative_expression : multiplicative_expression '/' cast_expression",
	"multiplicative_expression : multiplicative_expression '%' cast_expression",
	"additive_expression : multiplicative_expression",
	"additive_expression : additive_expression '+' multiplicative_expression",
	"additive_expression : additive_expression '-' multiplicative_expression",
	"shift_expression : additive_expression",
	"shift_expression : shift_expression LS additive_expression",
	"shift_expression : shift_expression RS additive_expression",
	"relational_expression : shift_expression",
	"relational_expression : relational_expression '<' shift_expression",
	"relational_expression : relational_expression '>' shift_expression",
	"relational_expression : relational_expression LE shift_expression",
	"relational_expression : relational_expression GE shift_expression",
	"equality_expression : relational_expression",
	"equality_expression : equality_expression EQ relational_expression",
	"equality_expression : equality_expression NE relational_expression",
	"AND_expression : equality_expression",
	"AND_expression : AND_expression '&' equality_expression",
	"exclusive_OR_expression : AND_expression",
	"exclusive_OR_expression : exclusive_OR_expression '^' AND_expression",
	"inclusive_OR_expression : exclusive_OR_expression",
	"inclusive_OR_expression : inclusive_OR_expression '|' exclusive_OR_expression",
	"logical_AND_expression : inclusive_OR_expression",
	"logical_AND_expression : logical_AND_expression ANDAND inclusive_OR_expression",
	"logical_OR_expression : logical_AND_expression",
	"logical_OR_expression : logical_OR_expression OROR logical_AND_expression",
	"conditional_expression : logical_OR_expression",
	"conditional_expression : logical_OR_expression '?' comma_expression ':' conditional_expression",
	"assignment_expression : conditional_expression",
	"assignment_expression : unary_expression _assignment_operator assignment_expression",
	"_assignment_operator : assignment_operator",
	"assignment_operator : '='",
	"assignment_operator : MULTassign",
	"assignment_operator : DIVassign",
	"assignment_operator : MODassign",
	"assignment_operator : PLUSassign",
	"assignment_operator : MINUSassign",
	"assignment_operator : LSassign",
	"assignment_operator : RSassign",
	"assignment_operator : ANDassign",
	"assignment_operator : ERassign",
	"assignment_operator : ORassign",
	"comma_expression : assignment_expression",
	"comma_expression : comma_expression ',' assignment_expression",
	"constant_expression : constant",
	"constant_expression : '+' constant",
	"constant_expression : '-' INTEGERconstant",
	"comma_expression_opt : /* empty */",
	"comma_expression_opt : comma_expression",
	"decl_comma : ','",
	"declaration : declaring_list ';'",
	"declaring_list : type_specifier declarator initializer_opt",
	"declaring_list : declaring_list decl_comma declarator initializer_opt",
	"type_specifier : basic_type_specifier",
	"basic_type_specifier : _basic_type_name",
	"_basic_type_name : INT",
	"_basic_type_name : CHAR",
	"_basic_type_name : VOID",
	"parameter_type_list : parameter_list",
	"parameter_type_list : parameter_list ',' ELLIPSIS",
	"parameter_list : parameter_declaration",
	"parameter_list : parameter_list ',' parameter_declaration",
	"parameter_declaration : type_specifier",
	"parameter_declaration : type_specifier abstract_declarator",
	"parameter_declaration : type_specifier identifier_declarator",
	"identifier_list : _IDENTIFIER",
	"identifier_list : identifier_list ',' _IDENTIFIER",
	"type_name : type_specifier",
	"type_name : type_specifier abstract_declarator",
	"initializer_opt : /* empty */",
	"initializer_opt : '=' initializer",
	"begin_initializer_list : '{'",
	"end_initializer_list : '}'",
	"initializer : begin_initializer_list initializer_list end_initializer_list",
	"initializer : begin_initializer_list initializer_list ',' end_initializer_list",
	"initializer : assignment_expression",
	"initializer_item : begin_initializer_list initializer_list end_initializer_list",
	"initializer_item : begin_initializer_list initializer_list ',' end_initializer_list",
	"initializer_item : constant_expression",
	"initializer_item : STRINGliteral",
	"initializer_list : initializer_item",
	"initializer_list : initializer_list ',' initializer_item",
	"statement : labeled_statement",
	"statement : compound_statement",
	"statement : expression_statement",
	"statement : selection_statement",
	"statement : iteration_statement",
	"statement : jump_statement",
	"labeled_statement : CASE constant_expression ':'",
	"labeled_statement : CASE constant_expression ':' statement",
	"labeled_statement : DEFAULT ':'",
	"labeled_statement : DEFAULT ':' statement",
	"begin_compound_statement : '{'",
	"compound_statement : begin_compound_statement '}'",
	"compound_statement : begin_compound_statement declaration_list '}'",
	"compound_statement : begin_compound_statement statement_list '}'",
	"compound_statement : begin_compound_statement declaration_list statement_list '}'",
	"declaration_list : declaration",
	"declaration_list : declaration_list declaration",
	"statement_list : statement",
	"statement_list : statement_list statement",
	"expression_statement : comma_expression_opt ';'",
	"selection_statement : IF '(' comma_expression ')' statement",
	"selection_statement : IF '(' comma_expression ')' statement ELSE statement",
	"selection_statement : SWITCH '(' comma_expression ')' statement",
	"iteration_statement : WHILE '(' comma_expression ')' statement",
	"iteration_statement : DO statement WHILE '(' comma_expression ')' ';'",
	"iteration_statement : FOR '(' comma_expression_opt ';' comma_expression_opt ';' comma_expression_opt ')' statement",
	"jump_statement : CONTINUE ';'",
	"jump_statement : BREAK ';'",
	"jump_statement : RETURN comma_expression_opt ';'",
	"external_definition : function_definition",
	"external_definition : declaration",
	"external_definition : compound_statement",
	"external_definition : /* empty */",
	"function_definition : identifier_declarator compound_statement",
	"function_definition : type_specifier identifier_declarator compound_statement",
	"function_definition : old_function_declarator compound_statement",
	"function_definition : type_specifier old_function_declarator compound_statement",
	"function_definition : old_function_declarator old_declaration_list compound_statement",
	"function_definition : type_specifier old_function_declarator old_declaration_list compound_statement",
	"old_declaration_list : old_declaration",
	"old_declaration_list : old_declaration_list old_declaration",
	"old_declaration : old_declaring_list ';'",
	"old_declaring_list : type_specifier declarator",
	"old_declaring_list : old_declaring_list",
	"old_declaring_list : old_declaring_list decl_comma declarator",
	"declarator : identifier_declarator",
	"asterisk : '*'",
	"identifier_declarator : unary_identifier_declarator",
	"identifier_declarator : paren_identifier_declarator",
	"unary_identifier_declarator : postfix_identifier_declarator",
	"unary_identifier_declarator : asterisk identifier_declarator",
	"begin_of_list : '('",
	"end_of_list : ')'",
	"forget_list : ')'",
	"forget_list_if_possible : ')'",
	"postfix_identifier_declarator : paren_identifier_declarator postfixing_abstract_declarator",
	"postfix_identifier_declarator : begin_of_list unary_identifier_declarator forget_list",
	"postfix_identifier_declarator : begin_of_list unary_identifier_declarator forget_list_if_possible postfixing_abstract_declarator",
	"paren_identifier_declarator : _IDENTIFIER",
	"paren_identifier_declarator : begin_of_list paren_identifier_declarator forget_list",
	"old_function_declarator : postfix_old_function_declarator",
	"old_function_declarator : asterisk old_function_declarator",
	"postfix_old_function_declarator : paren_identifier_declarator '('",
	"postfix_old_function_declarator : paren_identifier_declarator '(' identifier_list ')'",
	"postfix_old_function_declarator : begin_of_list old_function_declarator forget_list",
	"postfix_old_function_declarator : begin_of_list old_function_declarator forget_list_if_possible postfixing_abstract_declarator",
	"abstract_declarator : unary_abstract_declarator",
	"abstract_declarator : postfix_abstract_declarator",
	"abstract_declarator : postfixing_abstract_declarator",
	"postfixing_abstract_declarator : array_abstract_declarator",
	"postfixing_abstract_declarator : begin_of_list forget_list",
	"postfixing_abstract_declarator : begin_of_list",
	"postfixing_abstract_declarator : begin_of_list parameter_type_list forget_list",
	"array_abstract_declarator : '[' ']'",
	"array_abstract_declarator : '[' constant_expression ']'",
	"array_abstract_declarator : array_abstract_declarator '[' constant_expression ']'",
	"unary_abstract_declarator : asterisk",
	"unary_abstract_declarator : asterisk abstract_declarator",
	"postfix_abstract_declarator : begin_of_list unary_abstract_declarator forget_list",
	"postfix_abstract_declarator : begin_of_list postfix_abstract_declarator forget_list",
	"postfix_abstract_declarator : begin_of_list postfixing_abstract_declarator forget_list",
	"postfix_abstract_declarator : begin_of_list unary_abstract_declarator end_of_list postfixing_abstract_declarator",
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
		
case 1:{ memo_identifier(yytext); } break;
case 2:{ memo_integer(yytext); } break;
case 3:{ memo_char(yytext); } break;
case 4:{ stringliterallist = Strdup(yytext); } break;
case 5:{ 
		  char * s = Malloc(strlen(stringliterallist) +
				    strlen(yytext) - 1);
		  
		  strcpy(s, stringliterallist);
		  strcpy(s + strlen(s) - 1, yytext + 1);
		  
		  free(stringliterallist);
		  stringliterallist = s;
		} break;
case 8:{ memo_string(stringliterallist); free(stringliterallist); } break;
case 11:{ memo_array_access(); } break;
case 12:{ memo_funcall_0_arg(); } break;
case 13:{ memo_funcall_nargs(); } break;
case 14:{ memo_post_incr(); } break;
case 15:{ memo_post_decr(); } break;
case 16:{ mark_one_arg(); } break;
case 17:{ mark_first_arg(); } break;
case 18:{ mark_some_args(); } break;
case 22:{ memo_pre_incr(); } break;
case 23:{ memo_pre_decr(); } break;
case 24:{ memo_unary_oper(); } break;
case 25:{ memo_sizeof_type();} break;
case 26:{ memo_oper_ampersand(); } break;
case 27:{ memo_oper_asterisk(); } break;
case 28:{ memo_oper_mono_plus(); } break;
case 29:{ memo_oper_mono_minus();} break;
case 30:{ memo_oper_complement();} break;
case 31:{ memo_oper_not(); } break;
case 33:{ memo_cast(); } break;
case 35:{ memo_binary_oper("*"); } break;
case 36:{ memo_binary_oper("/"); } break;
case 37:{ memo_binary_oper("%"); } break;
case 39:{ memo_binary_oper("+"); } break;
case 40:{ memo_binary_oper("-"); } break;
case 42:{ memo_binary_oper("<<"); } break;
case 43:{ memo_binary_oper(">>"); } break;
case 45:{ memo_binary_oper("<"); } break;
case 46:{ memo_binary_oper(">"); } break;
case 47:{ memo_binary_oper("<="); } break;
case 48:{ memo_binary_oper(">="); } break;
case 50:{ memo_binary_oper("=="); } break;
case 51:{ memo_binary_oper("!="); } break;
case 53:{ memo_binary_oper("&"); } break;
case 55:{ memo_binary_oper("^"); } break;
case 57:{ memo_binary_oper("|"); } break;
case 59:{ memo_binary_oper("&&"); } break;
case 61:{ memo_binary_oper("||"); } break;
case 63:{ memo_arith_if(); } break;
case 65:{ memo_assignment(); } break;
case 66:{ memo_identifier(yytext); } break;
case 79:{ memo_comma(); } break;
case 82:{ memo_negate_integer(yytext); } break;
case 83:{ memo_no_comma_expression(); } break;
case 85:{ partial_declaration(); } break;
case 86:{ declaration(); } break;
case 91:{ memo_typedef(Type_Int); } break;
case 92:{ memo_typedef(Type_Char); } break;
case 93:{ memo_typedef(Type_Void); } break;
case 95:{ memo_ellipsis(); } break;
case 98:{ ignored_param(); } break;
case 99:{ ignored_param(); } break;
case 100:{ typed_param(); } break;
case 105:{ mark_no_initializer(); } break;
case 107:{ mark_begin_initializer_list(); } break;
case 108:{ memo_initializer_list(); } break;
case 115:{ memo_string(yytext); } break;
case 124:{ memo_case(); } break;
case 126:{ memo_default(); } break;
case 128:{ mark_begin_block(); } break;
case 129:{ memo_empty_block(); } break;
case 130:{ memo_block(); } break;
case 131:{ memo_block(); } break;
case 132:{ memo_block(); } break;
case 138:{ memo_if();} break;
case 139:{ memo_if_else();} break;
case 140:{ memo_switch();} break;
case 141:{ memo_while(); } break;
case 142:{ memo_do_while(); } break;
case 143:{ memo_for(); } break;
case 144:{ memo_continue(); } break;
case 145:{ memo_break(); } break;
case 146:{ memo_return(); } break;
case 147:{ YYACCEPT; } break;
case 148:{ YYACCEPT; } break;
case 149:{ YYACCEPT; } break;
case 150:{ if (*yytext)
				    yyerror(yytext);
				  YYABORT; } break;
case 151:{ memo_make_function_def(); } break;
case 152:{ memo_make_typedef_function_def(); } break;
case 153:{ memo_make_oldfunction_def_without_decllist(); } break;
case 154:{ memo_make_typedef_oldfunction_def_without_decllist(); } break;
case 155:{ memo_make_oldfunction_def(); } break;
case 156:{ memo_make_typedef_oldfunction_def(); } break;
case 159:{ mark_no_initializer(); declaration(); } break;
case 161:{ mark_no_initializer(); } break;
case 164:{ mark_asterisk(); } break;
case 169:{ mark_begin_of_list(); } break;
case 170:{ end_of_list(); } break;
case 171:{ forget_list(); } break;
case 172:{ forget_list_if_possible(); } break;
case 180:{ mark_begin_identifier_list(); } break;
case 181:{ end_identifier_list(); } break;
case 187:{ array_abstract_declarator(); } break;
case 188:{ memo_empty_function_parameter_type_list(); } break;
case 189:{ mark_begin_function_parameter_type_list(); } break;
case 190:{ end_function_parameter_type_list(); } break;
case 191:{ memo_array_decl_without_dim(); } break;
case 192:{ memo_array_decl_dim(); } break;
case 193:{ memo_array_decl_dim(); } break;
	}
	goto yystack;		/* reset registers in driver code */
}
