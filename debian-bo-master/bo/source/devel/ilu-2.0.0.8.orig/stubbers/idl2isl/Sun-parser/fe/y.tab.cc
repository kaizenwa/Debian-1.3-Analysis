
# line 72 "idl.yy"
#include <idl.hh>
#include <idl_extern.hh>

#include <fe_private.hh>

#include <stdio.h>

# line 84 "idl.yy"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
 {
  AST_Decl		*dcval;		/* Decl value		*/
  UTL_StrList		*slval;		/* String list		*/
  UTL_NameList		*nlval;		/* Name list		*/
  UTL_ExprList		*elval;		/* Expression list	*/
  UTL_LabelList		*llval;		/* Label list		*/
  UTL_DeclList		*dlval;		/* Declaration list	*/
  FE_InterfaceHeader	*ihval;		/* Interface header	*/
  AST_Expression	*exval;		/* Expression value	*/
  AST_UnionLabel	*ulval;		/* Union label		*/
  AST_Field		*ffval;		/* Field value		*/
  AST_Expression::ExprType etval;	/* Expression type	*/
  AST_Argument::Direction dival;	/* Argument direction	*/
  AST_Operation::Flags	ofval;		/* Operation flags	*/
  FE_Declarator		*deval;		/* Declarator value	*/
  idl_bool		bval;		/* Boolean value	*/
  unsigned long		ival;		/* Unsigned Long value	*/
  double		dval;		/* Double value		*/
  float			fval;		/* Float value		*/
  char			cval;		/* Char value		*/
  
  String		*sval;		/* String value		*/
  char			*strval;	/* char * value		*/
  Identifier		*idval;		/* Identifier		*/
  UTL_IdList		*idlist;	/* Identifier list	*/
} YYSTYPE;
# define IDENTIFIER 257
# define CONST 258
# define MODULE 259
# define INTERFACE 260
# define TYPEDEF 261
# define LONG 262
# define SHORT 263
# define UNSIGNED 264
# define DOUBLE 265
# define FLOAT 266
# define CHAR 267
# define WCHAR 268
# define OCTET 269
# define BOOLEAN 270
# define ANY 271
# define STRUCT 272
# define UNION 273
# define SWITCH 274
# define ENUM 275
# define SEQUENCE 276
# define STRING 277
# define WSTRING 278
# define EXCEPTION 279
# define CASE 280
# define DEFAULT 281
# define READONLY 282
# define ATTRIBUTE 283
# define ONEWAY 284
# define IDEMPOTENT 285
# define VOID 286
# define IN 287
# define OUT 288
# define INOUT 289
# define RAISES 290
# define CONTEXT 291
# define INTEGER_LITERAL 292
# define STRING_LITERAL 293
# define CHARACTER_LITERAL 294
# define FLOATING_PT_LITERAL 295
# define TRUETOK 296
# define FALSETOK 297
# define SCOPE_DELIMITOR 298
# define LEFT_SHIFT 299
# define RIGHT_SHIFT 300

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

# line 2183 "idl.yy"

/* programs */

/*
 * ???
 */
int
yywrap()
{
  return 1;
}

/*
 * Report an error situation discovered in a production
 *
 * This does not do anything since we report all error situations through
 * idl_global->err() operations
 */
void
yyerror(char *)
{
}
yytabelem yyexca[] ={
-1, 0,
	0, 3,
	-2, 0,
-1, 1,
	0, -1,
	-2, 0,
-1, 3,
	0, 3,
	125, 3,
	-2, 0,
-1, 23,
	123, 32,
	-2, 57,
-1, 131,
	91, 224,
	-2, 133,
-1, 146,
	257, 249,
	262, 249,
	263, 249,
	264, 249,
	265, 249,
	266, 249,
	267, 249,
	268, 249,
	269, 249,
	270, 249,
	271, 249,
	276, 249,
	277, 249,
	278, 249,
	283, 236,
	286, 249,
	298, 249,
	125, 25,
	-2, 0,
-1, 179,
	125, 3,
	-2, 0,
-1, 221,
	125, 240,
	-2, 0,
-1, 265,
	125, 156,
	-2, 0,
-1, 314,
	41, 252,
	-2, 254,
-1, 348,
	125, 179,
	-2, 0,
	};
# define YYNPROD 280
# define YYLAST 504
yytabelem yyact[]={

   168,    68,   152,   128,   125,   324,   336,    51,   164,   127,
    83,   339,   226,    83,   229,   225,   205,   206,    74,    84,
    85,    72,    73,    75,    76,    78,    77,    79,    19,    20,
   107,    21,    86,    87,    88,   166,    98,   216,   100,   101,
   102,    55,    97,   173,   105,   326,   327,   328,    50,   321,
    83,    69,   343,   238,    69,    74,    84,    85,    72,    73,
    75,    76,    78,    77,    79,    19,    20,    63,    21,    86,
    87,    88,   144,    83,   341,   340,   109,    62,    74,    84,
    85,    72,    73,    75,    76,    78,    77,    79,   115,   116,
   138,    69,    86,    87,    88,   166,    83,   230,   161,    61,
   162,    92,   241,    58,    13,   301,   109,    13,   131,   110,
   134,    93,    99,   126,    69,   221,    59,    83,   159,   157,
   140,   151,    74,    84,    85,    72,    73,    75,    76,    78,
    77,    79,   338,    91,    83,   150,    86,    87,    88,    74,
    84,    85,    72,    73,    75,    76,    78,    77,   199,   156,
    90,   261,     6,   155,    87,    88,   341,   340,    69,   160,
     5,    83,   158,   154,     4,   358,   297,    84,    85,   215,
   298,    75,    76,    78,    77,    69,   287,   286,   285,    21,
   202,   163,   231,   319,   219,    55,   145,   143,   190,    55,
    14,   142,   141,    10,   103,   203,   308,   200,   259,   258,
   240,    55,    69,   246,    19,    20,   111,    21,     2,   220,
   139,    15,    25,   114,   193,   113,   194,   195,     9,   375,
    14,    18,    24,    10,   112,   362,   309,   289,   276,   275,
    55,   274,    57,    12,    19,    20,    12,    21,   273,    56,
    11,    15,   272,    11,   271,   131,    48,   280,    47,    46,
    13,   279,    83,    45,    44,    43,   377,   364,   174,   175,
    41,   211,   207,   284,   208,   382,   209,   345,   131,   299,
   197,   210,   196,   380,    55,   374,   288,   296,   332,   322,
   310,   257,   366,    13,   344,   300,    55,   167,   173,   169,
   170,   171,   172,    69,   314,   227,   204,    60,   383,   187,
    67,    66,   373,   354,   355,   331,   357,   186,   131,   334,
   356,   185,    83,   333,   323,   318,   312,   317,   330,   316,
   311,   212,   213,   214,   313,   250,   251,   307,   254,   255,
   256,    94,   224,   278,    96,    95,   293,   262,   178,   119,
   265,    34,   346,    55,   306,   277,   294,   167,   173,   169,
   170,   171,   172,    69,   249,   359,   365,   248,    55,   131,
   367,   369,   368,   363,   126,   361,   247,   282,   292,   302,
   252,   253,   295,   131,   379,   376,   245,   149,   218,    12,
   137,    82,   217,   136,   384,   291,    11,    81,   222,   176,
   117,   201,   135,   281,   305,   270,   269,   228,   182,   123,
    38,   371,   372,   353,   352,   350,   370,   349,   348,   347,
   335,   329,    12,   315,   304,   268,   181,   122,    37,    11,
   267,   303,   266,   264,   223,   180,   121,    36,   243,   106,
    49,    32,   260,   177,   118,    33,   133,   108,   242,   237,
   236,   189,   235,   188,   234,   233,   232,   184,   104,    42,
   183,   146,   124,    39,    17,    16,   263,   179,   120,    35,
    31,    30,     8,    29,     7,    28,    27,    26,     3,     1,
    23,   191,   130,   129,   192,   325,    53,    71,    70,    64,
    89,   360,   165,   153,   283,    22,   148,   351,   337,   244,
   198,   320,    40,   147,   381,   378,   342,    80,   239,   132,
   290,    65,    54,    52 };
yytabelem yypact[]={

   -38,-10000000,-10000000,   -38,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,   202,-10000000,-10000000,   196,   195,   194,   190,
   189,   187,  -207,  -123,  -161,  -145,  -161,  -161,  -161,    71,
-10000000,-10000000,  -161,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,  -268,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,  -156,-10000000,-10000000,-10000000,-10000000,-10000000,
   162,   155,   153,-10000000,-10000000,  -174,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -268,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,  -247,-10000000,  -161,-10000000,  -161,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,  -172,-10000000,   150,  -161,    69,
    68,    64,  -202,    63,-10000000,-10000000,  -268,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,  -161,-10000000,    55,    55,    55,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,   -68,   228,   226,   106,
-10000000,-10000000,-10000000,    56,   101,   258,  -283,   219,   224,-10000000,
-10000000,    -5,    -5,    -5,  -268,-10000000,    55,-10000000,  -256,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -140,   148,-10000000,   -38,
  -244,   255,  -160,    57,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,  -230,  -184,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,   141,    55,    55,    55,    55,    55,    55,    55,    55,
    55,    55,-10000000,-10000000,-10000000,   240,-10000000,   137,   136,-10000000,
-10000000,  -244,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,   185,   183,   179,   172,   170,   169,-10000000,-10000000,
-10000000,-10000000,  -247,  -161,   106,    55,-10000000,   101,   258,  -283,
   219,   219,   224,   224,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
    55,-10000000,    53,    52,    51,  -244,  -161,   168,   -96,    45,
   225,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -140,  -152,  -268,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -268,  -186,-10000000,-10000000,
-10000000,-10000000,   103,   167,   239,  -160,  -161,   254,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,    60,  -241,   238,  -242,-10000000,
-10000000,-10000000,-10000000,   237,-10000000,-10000000,-10000000,-10000000,-10000000,  -124,
  -239,   244,-10000000,   223,  -140,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,    40,  -124,  -207,
   166,  -206,   199,    55,   242,  -247,  -242,  -161,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,   234,-10000000,-10000000,
   160,  -161,   198,  -250,-10000000,-10000000,-10000000,-10000000,   232,  -256,
-10000000,   221,-10000000,  -250,  -256 };
yytabelem yypgo[]={

     0,    15,     7,   503,   502,   501,   301,   239,   103,   500,
   232,   499,   498,   497,   300,     8,   496,   495,   494,     4,
   493,   492,   491,   490,   489,   488,   487,     9,   486,   485,
   484,     2,   483,   163,   153,   149,   119,   162,   118,   159,
   482,   121,   148,    11,   481,   480,   116,    99,    77,   297,
   479,   478,   477,   476,    67,   475,   474,     3,   473,   472,
   471,   470,     1,     0,   469,   208,   468,   164,   467,   160,
   466,   152,   465,   464,   463,   462,   461,   460,   459,   458,
   457,   456,   455,   454,   453,   452,   451,   450,   449,   448,
   447,   446,   445,   444,   443,   442,   441,   440,   439,   438,
   437,   436,   435,   434,   433,   432,   431,   430,   429,   428,
   427,   426,   425,   424,   423,   151,   115,   422,   421,   420,
   418,   417,   416,   415,   414,   413,   411,   410,   409,     6,
   408,   407,   406,   405,   404,   403,   402,   401,   400,   399,
   398,   397,   396,    14,   395,   394,   392,   391,   390,   389,
   387,   383,   382,   381,   380,   378,   377,   376,   369,   345,
   344,   341,   339,   338,   337,   333,   327,   324,   319,   318,
   317,   315,   314,     5,   313,   310,   309,   306,   305,   304,
   303,   302,   298 };
yytabelem yyr1[]={

     0,    64,    65,    65,    68,    66,    70,    66,    72,    66,
    74,    66,    76,    66,    77,    66,    78,    79,    80,    81,
    75,    73,    73,    84,    85,    87,    82,    88,    61,    29,
    89,    21,    21,    86,    86,    91,    90,    92,    90,    93,
    90,    95,    90,    97,    90,    98,    90,    19,    99,    20,
    20,    15,   100,    15,   101,    15,    62,    83,   102,   103,
   104,   105,    69,    45,    45,    45,    45,    45,    45,    45,
    45,    30,    31,    32,    32,    33,    33,    34,    34,    35,
    35,    35,    36,    36,    36,    37,    37,    37,    37,    38,
    38,    38,    38,    39,    39,    39,    40,    40,    40,    40,
    40,    40,    41,   106,    67,    67,    67,    67,   108,   107,
     1,     1,     2,     2,     2,    53,    53,    53,    53,    53,
    53,     4,     4,     4,     3,     3,     3,    27,   109,    28,
    28,    57,    57,    58,    59,    46,    46,    51,    51,    51,
    52,    52,    52,    49,    49,    49,    47,    47,    54,    48,
    50,   110,   111,   112,   114,     7,   113,   116,   116,   117,
   118,   115,   119,   115,   120,   121,   122,   123,   124,   125,
   126,   128,    10,     9,     9,     9,     9,     9,     9,   127,
   130,   130,   131,   132,   129,   133,   129,    25,    26,    26,
   134,    43,   135,   136,    43,   137,    44,   138,   139,   140,
   142,     8,   141,   145,   144,   144,   143,   146,   147,     5,
     5,   148,   149,    13,   151,   152,     6,     6,   150,   154,
   155,    14,    14,   153,   156,    11,    23,    24,    24,   157,
   158,    42,   159,   160,    94,    60,    60,   161,   162,   163,
   164,    71,   165,   166,   168,   169,    96,    56,    56,    56,
    12,    12,   170,   167,   171,   167,   172,   175,   174,   174,
   176,   177,   173,    55,    55,    55,   178,   179,    22,    22,
    63,    63,   180,   181,    16,    16,    17,   182,    18,    18 };
yytabelem yyr2[]={

     0,     2,     4,     0,     1,     7,     1,     7,     1,     7,
     1,     7,     1,     7,     1,     7,     1,     1,     1,     1,
    19,     2,     2,     1,     1,     1,    15,     1,     7,     5,
     1,     7,     1,     4,     0,     1,     7,     1,     7,     1,
     7,     1,     7,     1,     7,     1,     7,     5,     1,     9,
     1,     3,     1,     7,     1,     9,     3,     3,     1,     1,
     1,     1,    19,     2,     2,     2,     2,     2,     3,     3,
     3,     2,     2,     2,     7,     2,     7,     2,     7,     2,
     7,     7,     2,     7,     7,     2,     7,     7,     7,     2,
     5,     5,     5,     3,     2,     7,     3,     3,     3,     3,
     3,     3,     3,     1,     6,     2,     2,     2,     1,     7,
     2,     2,     3,     2,     3,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     5,     1,     9,
     1,     2,     2,     3,     3,     2,     2,     3,     5,     3,
     5,     7,     5,     3,     3,     5,     3,     3,     3,     3,
     3,     1,     1,     1,     1,    19,     4,     4,     0,     1,
     1,    11,     1,     7,     1,     1,     1,     1,     1,     1,
     1,     1,    35,     3,     3,     3,     3,     2,     3,     4,
     4,     0,     1,     1,    11,     1,     7,     5,     5,     1,
     1,     7,     1,     1,    11,     1,     7,     1,     1,     1,
     1,    19,     4,     1,     8,     0,     3,     1,     1,    13,
     5,     1,     1,    11,     1,     1,    13,     3,     3,     1,
     1,    13,     3,     3,     1,     7,     5,     5,     1,     1,
     1,    11,     1,     1,    13,     3,     1,     1,     1,     1,
     1,    19,     1,     1,     1,     1,    21,     3,     3,     1,
     2,     3,     1,     7,     1,     9,     4,     1,     8,     0,
     1,     1,    11,     3,     3,     3,     1,     1,    13,     1,
     2,     5,     1,     1,    13,     1,     5,     1,     9,     1 };
yytabelem yychk[]={

-10000000,   -64,   -65,   -66,   -67,   -69,   -71,   -73,   -75,   256,
   261,    -7,   -10,    -8,   258,   279,   -82,   -83,   259,   272,
   273,   275,   -29,   -61,   260,   -65,   -68,   -70,   -72,   -74,
   -76,   -77,  -106,  -102,  -161,   -78,  -110,  -120,  -138,   -84,
   -21,    58,   -88,    59,    59,    59,    59,    59,    59,  -107,
    -1,    -2,    -3,   -53,    -4,   -15,    -7,   -10,    -8,   -46,
   -49,   -47,   -48,   -54,   -50,    -5,    -6,   -14,   -62,   298,
   -51,   -52,   265,   266,   262,   267,   268,   270,   269,   271,
   -13,  -150,  -153,   257,   263,   264,   276,   277,   278,   -45,
   -46,   -47,   -54,   -48,   -49,    -6,   -14,   -15,   -62,   257,
   -62,   -62,   -62,   123,   -89,   -62,  -108,   298,  -100,   262,
   265,    44,    62,    60,    60,   262,   263,  -148,  -103,  -162,
   -79,  -111,  -121,  -139,   -85,   -19,   -15,   -27,   -57,   -58,
   -59,   -62,   -11,  -101,   -62,  -146,  -151,  -154,   262,    60,
   -62,   123,   123,   123,   274,   123,   -86,   -20,   -28,  -156,
   -62,   -41,   -31,   -32,   -33,   -34,   -35,   -36,   -37,   -38,
   -39,    43,    45,   126,   -15,   -40,    40,   292,   -63,   294,
   295,   296,   297,   293,   -41,   -41,  -149,  -104,  -163,   -80,
  -112,  -122,  -140,   -87,   -90,   -67,   -69,   -71,   -94,   -96,
   256,   -60,   -56,   282,   284,   285,    44,    44,   -23,   -42,
    91,  -147,   124,    94,    38,   299,   300,    43,    45,    42,
    47,    37,   -39,   -39,   -39,   -31,   293,  -152,  -155,    -2,
    61,  -116,   -65,  -113,  -115,    -1,   256,    40,  -141,  -143,
   257,   125,   -91,   -92,   -93,   -95,   -97,   -98,   283,   -12,
    -2,   286,   -99,  -109,   -24,  -157,    62,   -33,   -34,   -35,
   -36,   -36,   -37,   -37,   -38,   -38,   -38,    41,    62,    62,
  -105,  -115,  -164,   -81,  -114,  -116,  -117,  -119,  -123,  -142,
  -144,    59,    59,    59,    59,    59,    59,  -159,  -165,   -15,
   -57,   -42,   -41,   -30,   -31,   125,   125,   125,   -27,    59,
    -9,   -46,   -47,   -54,   -48,    -8,   -15,   262,   125,    44,
    -2,   257,  -158,  -118,  -124,  -145,  -160,  -166,    93,    59,
    41,  -143,   -27,  -167,    40,  -125,  -168,  -170,  -171,   123,
   -22,   290,    41,  -172,  -173,   -55,   287,   288,   289,  -126,
  -169,  -178,    41,  -174,  -176,  -127,  -129,   -25,   256,   -43,
   281,   280,   -16,   291,    40,    44,    -2,  -128,  -130,  -131,
  -133,   -26,  -134,  -135,  -180,  -179,  -175,  -177,   125,  -129,
   -44,    -1,    59,   -43,    58,   -31,    40,   -19,  -173,   -57,
  -132,  -137,  -136,  -181,    41,    59,   -57,    58,   -17,   -63,
    41,   -18,    44,  -182,   -63 };
yytabelem yydef[]={

    -2,    -2,     1,    -2,     4,     6,     8,    10,    12,    14,
   103,   105,   106,   107,    58,   237,    21,    22,    16,   151,
   164,   197,    23,    -2,    27,     2,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    29,    30,     0,     5,     7,     9,    11,    13,    15,   104,
   108,   110,   111,   112,   113,   114,   124,   125,   126,   115,
   116,   117,   118,   119,   120,   121,   122,   123,    51,    52,
   135,   136,   143,   144,   137,   146,   147,   149,   148,   150,
     0,   217,   222,    56,   139,     0,   211,   218,   223,    59,
    63,    64,    65,    66,    67,    68,    69,    70,   238,    17,
   152,   165,   198,    24,     0,    28,     0,    54,     0,   138,
   145,   207,   210,   214,   219,   140,   142,     0,     0,     0,
     0,     0,     0,     0,    34,    31,    50,   109,   130,   131,
   132,    -2,   134,     0,    53,     0,     0,     0,   141,   212,
    60,   239,    18,   153,   166,   199,    -2,    47,   127,     0,
    55,   208,   102,    72,    73,    75,    77,    79,    82,    85,
    89,     0,     0,     0,    93,    94,     0,    96,    97,    98,
    99,   100,   101,   270,   215,   220,     0,     0,   158,    -2,
     0,     0,     0,     0,    33,    35,    37,    39,    41,    43,
    45,     0,     0,   235,   247,   248,    48,   128,   225,   228,
   229,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    90,    91,    92,     0,   271,     0,     0,   213,
    61,    -2,    19,   154,   158,   159,   162,   167,   200,   205,
   206,    26,     0,     0,     0,     0,     0,     0,   232,   242,
   250,   251,     0,     0,   226,     0,   209,    74,    76,    78,
    80,    81,    83,    84,    86,    87,    88,    95,   216,   221,
     0,   157,     0,     0,     0,    -2,     0,     0,     0,     0,
   202,    36,    38,    40,    42,    44,    46,     0,     0,    49,
   129,   227,   230,    62,    71,   241,    20,   155,   160,   163,
   168,   173,   174,   175,   176,   177,   178,   137,   201,   203,
   233,   243,     0,     0,     0,     0,     0,     0,   231,   161,
   169,   204,   234,   244,    -2,     0,   269,     0,     0,   170,
   245,   266,   253,     0,   259,   260,   263,   264,   265,     0,
   275,     0,   255,   256,     0,   171,   181,   182,   185,   189,
   190,   192,   246,   272,   267,   257,   261,     0,    -2,     0,
     0,   187,     0,     0,     0,     0,     0,     0,   172,   180,
   183,   195,   186,   188,   191,   193,   273,     0,   258,   262,
     0,     0,     0,     0,   268,   184,   196,   194,     0,   279,
   274,   276,   277,     0,   278 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	1	/* allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"IDENTIFIER",	257,
	"CONST",	258,
	"MODULE",	259,
	"INTERFACE",	260,
	"TYPEDEF",	261,
	"LONG",	262,
	"SHORT",	263,
	"UNSIGNED",	264,
	"DOUBLE",	265,
	"FLOAT",	266,
	"CHAR",	267,
	"WCHAR",	268,
	"OCTET",	269,
	"BOOLEAN",	270,
	"ANY",	271,
	"STRUCT",	272,
	"UNION",	273,
	"SWITCH",	274,
	"ENUM",	275,
	"SEQUENCE",	276,
	"STRING",	277,
	"WSTRING",	278,
	"EXCEPTION",	279,
	"CASE",	280,
	"DEFAULT",	281,
	"READONLY",	282,
	"ATTRIBUTE",	283,
	"ONEWAY",	284,
	"IDEMPOTENT",	285,
	"VOID",	286,
	"IN",	287,
	"OUT",	288,
	"INOUT",	289,
	"RAISES",	290,
	"CONTEXT",	291,
	"INTEGER_LITERAL",	292,
	"STRING_LITERAL",	293,
	"CHARACTER_LITERAL",	294,
	"FLOATING_PT_LITERAL",	295,
	"TRUETOK",	296,
	"FALSETOK",	297,
	"SCOPE_DELIMITOR",	298,
	"LEFT_SHIFT",	299,
	"RIGHT_SHIFT",	300,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : definitions",
	"definitions : definition definitions",
	"definitions : /* empty */",
	"definition : type_dcl",
	"definition : type_dcl ';'",
	"definition : const_dcl",
	"definition : const_dcl ';'",
	"definition : exception",
	"definition : exception ';'",
	"definition : interface_def",
	"definition : interface_def ';'",
	"definition : module",
	"definition : module ';'",
	"definition : error",
	"definition : error ';'",
	"module : MODULE",
	"module : MODULE IDENTIFIER",
	"module : MODULE IDENTIFIER '{'",
	"module : MODULE IDENTIFIER '{' definitions",
	"module : MODULE IDENTIFIER '{' definitions '}'",
	"interface_def : interface",
	"interface_def : forward",
	"interface : interface_header",
	"interface : interface_header '{'",
	"interface : interface_header '{' exports",
	"interface : interface_header '{' exports '}'",
	"interface_decl : INTERFACE",
	"interface_decl : INTERFACE id",
	"interface_header : interface_decl inheritance_spec",
	"inheritance_spec : ':'",
	"inheritance_spec : ':' at_least_one_scoped_name",
	"inheritance_spec : /* empty */",
	"exports : exports export",
	"exports : /* empty */",
	"export : type_dcl",
	"export : type_dcl ';'",
	"export : const_dcl",
	"export : const_dcl ';'",
	"export : exception",
	"export : exception ';'",
	"export : attribute",
	"export : attribute ';'",
	"export : operation",
	"export : operation ';'",
	"export : error",
	"export : error ';'",
	"at_least_one_scoped_name : scoped_name scoped_names",
	"scoped_names : scoped_names ','",
	"scoped_names : scoped_names ',' scoped_name",
	"scoped_names : /* empty */",
	"scoped_name : id",
	"scoped_name : SCOPE_DELIMITOR",
	"scoped_name : SCOPE_DELIMITOR id",
	"scoped_name : scoped_name SCOPE_DELIMITOR",
	"scoped_name : scoped_name SCOPE_DELIMITOR id",
	"id : IDENTIFIER",
	"forward : interface_decl",
	"const_dcl : CONST",
	"const_dcl : CONST const_type",
	"const_dcl : CONST const_type id",
	"const_dcl : CONST const_type id '='",
	"const_dcl : CONST const_type id '=' expression",
	"const_type : integer_type",
	"const_type : char_type",
	"const_type : octet_type",
	"const_type : boolean_type",
	"const_type : floating_pt_type",
	"const_type : string_type_spec",
	"const_type : wstring_type_spec",
	"const_type : scoped_name",
	"expression : const_expr",
	"const_expr : or_expr",
	"or_expr : xor_expr",
	"or_expr : or_expr '|' xor_expr",
	"xor_expr : and_expr",
	"xor_expr : xor_expr '^' and_expr",
	"and_expr : shift_expr",
	"and_expr : and_expr '&' shift_expr",
	"shift_expr : add_expr",
	"shift_expr : shift_expr LEFT_SHIFT add_expr",
	"shift_expr : shift_expr RIGHT_SHIFT add_expr",
	"add_expr : mult_expr",
	"add_expr : add_expr '+' mult_expr",
	"add_expr : add_expr '-' mult_expr",
	"mult_expr : unary_expr",
	"mult_expr : mult_expr '*' unary_expr",
	"mult_expr : mult_expr '/' unary_expr",
	"mult_expr : mult_expr '' unary_expr",
	"unary_expr : primary_expr",
	"unary_expr : '+' primary_expr",
	"unary_expr : '-' primary_expr",
	"unary_expr : '~' primary_expr",
	"primary_expr : scoped_name",
	"primary_expr : literal",
	"primary_expr : '(' const_expr ')'",
	"literal : INTEGER_LITERAL",
	"literal : string_literal",
	"literal : CHARACTER_LITERAL",
	"literal : FLOATING_PT_LITERAL",
	"literal : TRUETOK",
	"literal : FALSETOK",
	"positive_int_expr : const_expr",
	"type_dcl : TYPEDEF",
	"type_dcl : TYPEDEF type_declarator",
	"type_dcl : struct_type",
	"type_dcl : union_type",
	"type_dcl : enum_type",
	"type_declarator : type_spec",
	"type_declarator : type_spec at_least_one_declarator",
	"type_spec : simple_type_spec",
	"type_spec : constructed_type_spec",
	"simple_type_spec : base_type_spec",
	"simple_type_spec : template_type_spec",
	"simple_type_spec : scoped_name",
	"base_type_spec : integer_type",
	"base_type_spec : floating_pt_type",
	"base_type_spec : char_type",
	"base_type_spec : boolean_type",
	"base_type_spec : octet_type",
	"base_type_spec : any_type",
	"template_type_spec : sequence_type_spec",
	"template_type_spec : string_type_spec",
	"template_type_spec : wstring_type_spec",
	"constructed_type_spec : struct_type",
	"constructed_type_spec : union_type",
	"constructed_type_spec : enum_type",
	"at_least_one_declarator : declarator declarators",
	"declarators : declarators ','",
	"declarators : declarators ',' declarator",
	"declarators : /* empty */",
	"declarator : simple_declarator",
	"declarator : complex_declarator",
	"simple_declarator : id",
	"complex_declarator : array_declarator",
	"integer_type : signed_int",
	"integer_type : unsigned_int",
	"signed_int : LONG",
	"signed_int : LONG LONG",
	"signed_int : SHORT",
	"unsigned_int : UNSIGNED LONG",
	"unsigned_int : UNSIGNED LONG LONG",
	"unsigned_int : UNSIGNED SHORT",
	"floating_pt_type : DOUBLE",
	"floating_pt_type : FLOAT",
	"floating_pt_type : LONG DOUBLE",
	"char_type : CHAR",
	"char_type : WCHAR",
	"octet_type : OCTET",
	"boolean_type : BOOLEAN",
	"any_type : ANY",
	"struct_type : STRUCT",
	"struct_type : STRUCT id",
	"struct_type : STRUCT id '{'",
	"struct_type : STRUCT id '{' at_least_one_member",
	"struct_type : STRUCT id '{' at_least_one_member '}'",
	"at_least_one_member : member members",
	"members : members member",
	"members : /* empty */",
	"member : type_spec",
	"member : type_spec at_least_one_declarator",
	"member : type_spec at_least_one_declarator ';'",
	"member : error",
	"member : error ';'",
	"union_type : UNION",
	"union_type : UNION id",
	"union_type : UNION id SWITCH",
	"union_type : UNION id SWITCH '('",
	"union_type : UNION id SWITCH '(' switch_type_spec",
	"union_type : UNION id SWITCH '(' switch_type_spec ')'",
	"union_type : UNION id SWITCH '(' switch_type_spec ')' '{'",
	"union_type : UNION id SWITCH '(' switch_type_spec ')' '{' at_least_one_case_branch",
	"union_type : UNION id SWITCH '(' switch_type_spec ')' '{' at_least_one_case_branch '}'",
	"switch_type_spec : integer_type",
	"switch_type_spec : char_type",
	"switch_type_spec : octet_type",
	"switch_type_spec : boolean_type",
	"switch_type_spec : enum_type",
	"switch_type_spec : scoped_name",
	"at_least_one_case_branch : case_branch case_branches",
	"case_branches : case_branches case_branch",
	"case_branches : /* empty */",
	"case_branch : at_least_one_case_label",
	"case_branch : at_least_one_case_label element_spec",
	"case_branch : at_least_one_case_label element_spec ';'",
	"case_branch : error",
	"case_branch : error ';'",
	"at_least_one_case_label : case_label case_labels",
	"case_labels : case_labels case_label",
	"case_labels : /* empty */",
	"case_label : DEFAULT",
	"case_label : DEFAULT ':'",
	"case_label : CASE",
	"case_label : CASE const_expr",
	"case_label : CASE const_expr ':'",
	"element_spec : type_spec",
	"element_spec : type_spec declarator",
	"enum_type : ENUM",
	"enum_type : ENUM id",
	"enum_type : ENUM id '{'",
	"enum_type : ENUM id '{' at_least_one_enumerator",
	"enum_type : ENUM id '{' at_least_one_enumerator '}'",
	"at_least_one_enumerator : enumerator enumerators",
	"enumerators : enumerators ','",
	"enumerators : enumerators ',' enumerator",
	"enumerators : /* empty */",
	"enumerator : IDENTIFIER",
	"sequence_type_spec : seq_head ','",
	"sequence_type_spec : seq_head ',' positive_int_expr",
	"sequence_type_spec : seq_head ',' positive_int_expr '>'",
	"sequence_type_spec : seq_head '>'",
	"seq_head : SEQUENCE",
	"seq_head : SEQUENCE '<'",
	"seq_head : SEQUENCE '<' simple_type_spec",
	"string_type_spec : string_head '<'",
	"string_type_spec : string_head '<' positive_int_expr",
	"string_type_spec : string_head '<' positive_int_expr '>'",
	"string_type_spec : string_head",
	"string_head : STRING",
	"wstring_type_spec : wstring_head '<'",
	"wstring_type_spec : wstring_head '<' positive_int_expr",
	"wstring_type_spec : wstring_head '<' positive_int_expr '>'",
	"wstring_type_spec : wstring_head",
	"wstring_head : WSTRING",
	"array_declarator : id",
	"array_declarator : id at_least_one_array_dim",
	"at_least_one_array_dim : array_dim array_dims",
	"array_dims : array_dims array_dim",
	"array_dims : /* empty */",
	"array_dim : '['",
	"array_dim : '[' positive_int_expr",
	"array_dim : '[' positive_int_expr ']'",
	"attribute : opt_readonly ATTRIBUTE",
	"attribute : opt_readonly ATTRIBUTE simple_type_spec",
	"attribute : opt_readonly ATTRIBUTE simple_type_spec at_least_one_declarator",
	"opt_readonly : READONLY",
	"opt_readonly : /* empty */",
	"exception : EXCEPTION",
	"exception : EXCEPTION id",
	"exception : EXCEPTION id '{'",
	"exception : EXCEPTION id '{' members",
	"exception : EXCEPTION id '{' members '}'",
	"operation : opt_op_attribute op_type_spec",
	"operation : opt_op_attribute op_type_spec IDENTIFIER",
	"operation : opt_op_attribute op_type_spec IDENTIFIER parameter_list",
	"operation : opt_op_attribute op_type_spec IDENTIFIER parameter_list opt_raises",
	"operation : opt_op_attribute op_type_spec IDENTIFIER parameter_list opt_raises opt_context",
	"opt_op_attribute : ONEWAY",
	"opt_op_attribute : IDEMPOTENT",
	"opt_op_attribute : /* empty */",
	"op_type_spec : simple_type_spec",
	"op_type_spec : VOID",
	"parameter_list : '('",
	"parameter_list : '(' ')'",
	"parameter_list : '('",
	"parameter_list : '(' at_least_one_parameter ')'",
	"at_least_one_parameter : parameter parameters",
	"parameters : parameters ','",
	"parameters : parameters ',' parameter",
	"parameters : /* empty */",
	"parameter : direction",
	"parameter : direction simple_type_spec",
	"parameter : direction simple_type_spec declarator",
	"direction : IN",
	"direction : OUT",
	"direction : INOUT",
	"opt_raises : RAISES",
	"opt_raises : RAISES '('",
	"opt_raises : RAISES '(' at_least_one_scoped_name ')'",
	"opt_raises : /* empty */",
	"string_literal : STRING_LITERAL",
	"string_literal : string_literal STRING_LITERAL",
	"opt_context : CONTEXT",
	"opt_context : CONTEXT '('",
	"opt_context : CONTEXT '(' at_least_one_string_literal ')'",
	"opt_context : /* empty */",
	"at_least_one_string_literal : string_literal string_literals",
	"string_literals : string_literals ','",
	"string_literals : string_literals ',' string_literal",
	"string_literals : /* empty */",
};
#endif /* YYDEBUG */
#if !defined(lint) && !defined(__cplusplus)
static  char __yaccpar_sccsid1[] = "@(#) 9/3/92 yaccpar 6.11 Copyr 1991 Sun Micro";
#endif

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
		
case 4:
# line 224 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_TypeDeclSeen);
        } break;
case 5:
# line 228 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 6:
# line 232 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstDeclSeen);
        } break;
case 7:
# line 236 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 8:
# line 240 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptDeclSeen);
        } break;
case 9:
# line 244 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 10:
# line 248 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_InterfaceDeclSeen);
        } break;
case 11:
# line 252 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 12:
# line 256 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ModuleDeclSeen);
        } break;
case 13:
# line 260 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 14:
# line 264 "idl.yy"
{
	  idl_global->err()->syntax_error(idl_global->parse_state());
	} break;
case 15:
# line 268 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
	  yyerrok;
	} break;
case 16:
# line 275 "idl.yy"
{
	    idl_global->set_parse_state(IDL_GlobalData::PS_ModuleSeen);
	  } break;
case 17:
# line 279 "idl.yy"
{
	    UTL_ScopedName 	*n =
		new UTL_ScopedName(new Identifier(yypvt[-0].strval, 1, 0, I_FALSE), NULL);
	    AST_Module		*m = NULL;
	    UTL_Scope		*s = idl_global->scopes()->top_non_null();
	    UTL_StrList		*p = idl_global->pragmas();	

	    idl_global->set_parse_state(IDL_GlobalData::PS_ModuleIDSeen);
	    /*
	     * Make a new module and add it to the enclosing scope
	     */
	    if (s != NULL) {
	      m = idl_global->gen()->create_module(n, p);
	      (void) s->fe_add_module(m);
	    }
	    /*
	     * Push it on the stack
	     */
	    idl_global->scopes()->push(m);
	  } break;
case 18:
# line 300 "idl.yy"
{
	    idl_global->set_parse_state(IDL_GlobalData::PS_ModuleSqSeen);
	  } break;
case 19:
# line 304 "idl.yy"
{
	    idl_global->set_parse_state(IDL_GlobalData::PS_ModuleBodySeen);
	  } break;
case 20:
# line 308 "idl.yy"
{
	    idl_global->set_parse_state(IDL_GlobalData::PS_ModuleQsSeen);
	    /*
	     * Finished with this module - pop it from the scope stack
	     */
	    idl_global->scopes()->pop();
	  } break;
case 23:
# line 324 "idl.yy"
{
	  UTL_Scope     *s = idl_global->scopes()->top_non_null();
	  AST_Interface *i = NULL;
	  AST_Decl	*v = NULL;
	  UTL_StrList   *p = idl_global->pragmas();
	  AST_Decl	*d = NULL;
	  AST_Interface *fd = NULL;

	  /*
	   * Make a new interface node and add it to its enclosing scope
	   */
	  if (s != NULL && yypvt[-0].ihval != NULL) {
	    i = idl_global->gen()->create_interface(yypvt[-0].ihval->interface_name(),
						    yypvt[-0].ihval->inherits(),
						    yypvt[-0].ihval->n_inherits(),
						    p);
	    if (i != NULL &&
		(d = s->lookup_by_name(i->name(), I_FALSE)) != NULL) {
	      /*
	       * See if we're defining a forward declared interface.
	       */
	      if (d->node_type() == AST_Decl::NT_interface) {
		/*
		 * Narrow to an interface
		 */
		fd = AST_Interface::narrow_from_decl(d);
		/*
		 * Successful?
		 */
		if (fd == NULL) {
		  /*
		   * Should we give an error here?
		   */
		}
		/*
		 * If it is a forward declared interface..
		 */
		else if (!fd->is_defined()) {
		  /*
		   * Check if redefining in same scope
		   */
		  if (fd->defined_in() != s) {
		    idl_global->err()
		       ->error3(UTL_Error::EIDL_SCOPE_CONFLICT,
				i,
				fd,
				ScopeAsDecl(s));
		  }
		  /*
		   * All OK, do the redefinition
		   */
		  else {
		    fd->set_inherits(yypvt[-0].ihval->inherits());
		    fd->set_n_inherits(yypvt[-0].ihval->n_inherits());
		    /*
		     * Update place of definition
		     */
		    fd->set_imported(idl_global->imported());
		    fd->set_in_main_file(idl_global->in_main_file());
		    fd->set_line(idl_global->lineno());
		    fd->set_file_name(idl_global->filename());
		    fd->add_pragmas(p);
		    /*
		     * Use full definition node
		     */
		    delete i;
		    i = fd;
		  }
	        }
	      }
	    }
	    /*
	     * Add the interface to its definition scope
	     */
	    (void) s->fe_add_interface(i);
	  }
	  /*
	   * Push it on the scope stack
	   */
	  idl_global->scopes()->push(i);
        } break;
case 24:
# line 406 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_InterfaceSqSeen);
	} break;
case 25:
# line 410 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_InterfaceBodySeen);
	} break;
case 26:
# line 414 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_InterfaceQsSeen);
	  /*
	   * Done with this interface - pop it off the scopes stack
	   */
	  idl_global->scopes()->pop();
	} break;
case 27:
# line 425 "idl.yy"
{
	   idl_global->set_parse_state(IDL_GlobalData::PS_InterfaceSeen);
	 } break;
case 28:
# line 429 "idl.yy"
{
	   idl_global->set_parse_state(IDL_GlobalData::PS_InterfaceIDSeen);
	   yyval.idval = yypvt[-0].idval;
	 } break;
case 29:
# line 437 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_InheritSpecSeen);
	  /*
	   * Create an AST representation of the information in the header
	   * part of an interface - this representation contains a computed
	   * list of all interfaces which this interface inherits from,
	   * recursively
	   */
	  yyval.ihval = new FE_InterfaceHeader(new UTL_ScopedName(yypvt[-1].idval, NULL), yypvt[-0].nlval);
	} break;
case 30:
# line 451 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_InheritColonSeen);
        } break;
case 31:
# line 455 "idl.yy"
{
	  yyval.nlval = yypvt[-0].nlval;
	} break;
case 32:
# line 459 "idl.yy"
{
	  yyval.nlval = NULL;
	} break;
case 35:
# line 471 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_TypeDeclSeen);
        } break;
case 36:
# line 475 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 37:
# line 479 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstDeclSeen);
        } break;
case 38:
# line 483 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 39:
# line 487 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptDeclSeen);
        } break;
case 40:
# line 491 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 41:
# line 495 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_AttrDeclSeen);
        } break;
case 42:
# line 499 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 43:
# line 503 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpDeclSeen);
        } break;
case 44:
# line 507 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
        } break;
case 45:
# line 511 "idl.yy"
{
	  idl_global->err()->syntax_error(idl_global->parse_state());
	} break;
case 46:
# line 515 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
	  yyerrok;
	} break;
case 47:
# line 523 "idl.yy"
{
	  yyval.nlval = new UTL_NameList(yypvt[-1].idlist, yypvt[-0].nlval);
	} break;
case 48:
# line 531 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SNListCommaSeen);
        } break;
case 49:
# line 535 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ScopedNameSeen);

	  if (yypvt[-3].nlval == NULL)
	    yyval.nlval = new UTL_NameList(yypvt[-0].idlist, NULL);
	  else {
	    yypvt[-3].nlval->nconc(new UTL_NameList(yypvt[-0].idlist, NULL));
	    yyval.nlval = yypvt[-3].nlval;
	  }
	} break;
case 50:
# line 546 "idl.yy"
{
	  yyval.nlval = NULL;
	} break;
case 51:
# line 553 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SN_IDSeen);

	  yyval.idlist = new UTL_IdList(yypvt[-0].idval, NULL);
	} break;
case 52:
# line 559 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ScopeDelimSeen);
        } break;
case 53:
# line 563 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SN_IDSeen);

	  yyval.idlist = new UTL_IdList(new Identifier(yypvt[-2].strval, 1, 0, I_FALSE),
			      new UTL_IdList(yypvt[-0].idval, NULL));
	} break;
case 54:
# line 571 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ScopeDelimSeen);
        } break;
case 55:
# line 575 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SN_IDSeen);

	  yypvt[-3].idlist->nconc(new UTL_IdList(yypvt[-0].idval, NULL));
	  yyval.idlist = yypvt[-3].idlist;
	} break;
case 56:
# line 584 "idl.yy"
{
            yyval.idval = new Identifier(yypvt[-0].strval, 1, 0, I_FALSE);
        } break;
case 57:
# line 591 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n = new UTL_ScopedName(yypvt[-0].idval, NULL);
	  AST_InterfaceFwd	*f = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_ForwardDeclSeen);
	  /*
	   * Create a node representing a forward declaration of an
	   * interface. Store it in the enclosing scope
	   */
	  if (s != NULL) {
	    f = idl_global->gen()->create_interface_fwd(n, p);
	    (void) s->fe_add_interface_fwd(f);
	  }
	} break;
case 58:
# line 611 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstSeen);
        } break;
case 59:
# line 615 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstTypeSeen);
        } break;
case 60:
# line 619 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstIDSeen);
        } break;
case 61:
# line 623 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstAssignSeen);
        } break;
case 62:
# line 627 "idl.yy"
{
	  UTL_ScopedName	*n = new UTL_ScopedName(yypvt[-4].idval, NULL);
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  AST_Constant		*c = NULL;
	  UTL_StrList		*p = idl_global->pragmas();
	  AST_Decl		*v = NULL;

	  idl_global->set_parse_state(IDL_GlobalData::PS_ConstExprSeen);
	  /*
	   * Create a node representing a constant declaration. Store
	   * it in the enclosing scope
	   */
	  if (yypvt[-0].exval != NULL && s != NULL) {
	    if (yypvt[-0].exval->coerce(yypvt[-6].etval) == NULL)
	      idl_global->err()->coercion_error(yypvt[-0].exval, yypvt[-6].etval);
	    else {
	      c =
		idl_global->gen()->create_constant(yypvt[-6].etval, yypvt[-0].exval, n, p);
	      (void) s->fe_add_constant(c);
	    }
	  }
	} break;
case 68:
# line 658 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_string;
	} break;
case 69:
# line 662 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_wstring;
        } break;
case 70:
# line 666 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  AST_Decl		*d = NULL;
	  AST_PredefinedType	*c = NULL;
	  AST_Typedef		*t = NULL;

	  /*
	   * If the constant's type is a scoped name, it must resolve
	   * to a scalar constant type
	   */
	  if (s != NULL && (d = s->lookup_by_name(yypvt[-0].idlist, I_TRUE)) != NULL) {
	    /*
	     * Look through typedefs
	     */
	    while (d->node_type() == AST_Decl::NT_typedef) {
	      t = AST_Typedef::narrow_from_decl(d);
	      if (t == NULL)
	        break;
	      d = t->base_type();
	    }
	    if (d == NULL)
	      yyval.etval = AST_Expression::EV_any;
	    else if (d->node_type() == AST_Decl::NT_pre_defined) {
	      c = AST_PredefinedType::narrow_from_decl(d);
	      if (c != NULL) {
	         yyval.etval = idl_global->PredefinedTypeToExprType(c->pt());
	      } else {
	         yyval.etval = AST_Expression::EV_any;
	      }
	    } else
	      yyval.etval = AST_Expression::EV_any;
	  } else
	    yyval.etval = AST_Expression::EV_any;
	} break;
case 74:
# line 708 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_or, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 76:
# line 716 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_xor, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 78:
# line 724 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_and, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 80:
# line 732 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_left,yypvt[-2].exval,yypvt[-0].exval);
	} break;
case 81:
# line 736 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_right,yypvt[-2].exval,yypvt[-0].exval);
	} break;
case 83:
# line 744 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_add, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 84:
# line 748 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_minus,yypvt[-2].exval,yypvt[-0].exval);
	} break;
case 86:
# line 756 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_mul, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 87:
# line 760 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_div, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 88:
# line 764 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_mod, yypvt[-2].exval, yypvt[-0].exval);
	} break;
case 90:
# line 772 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_u_plus,
					      yypvt[-0].exval,
					      NULL);
	} break;
case 91:
# line 778 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_u_minus,
					      yypvt[-0].exval,
					      NULL);
	} break;
case 92:
# line 784 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(AST_Expression::EC_bit_neg,
					      yypvt[-0].exval,
					      NULL);
	} break;
case 93:
# line 793 "idl.yy"
{
	  /*
	   * An expression which is a scoped name is not resolved now,
	   * but only when it is evaluated (such as when it is assigned
	   * as a constant value)
	   */
	  yyval.exval = idl_global->gen()->create_expr(yypvt[-0].idlist);
	} break;
case 95:
# line 803 "idl.yy"
{
	  yyval.exval = yypvt[-1].exval;
	} break;
case 96:
# line 810 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(yypvt[-0].ival);
	} break;
case 97:
# line 814 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(yypvt[-0].sval);
	} break;
case 98:
# line 818 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(yypvt[-0].cval);
	} break;
case 99:
# line 822 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr(yypvt[-0].dval);
	} break;
case 100:
# line 826 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr((idl_bool) I_TRUE,
					    AST_Expression::EV_bool);
	} break;
case 101:
# line 831 "idl.yy"
{
	  yyval.exval = idl_global->gen()->create_expr((idl_bool) I_FALSE,
					    AST_Expression::EV_bool);
	} break;
case 102:
# line 839 "idl.yy"
{
	    yypvt[-0].exval->evaluate(AST_Expression::EK_const);
	    yyval.exval = idl_global->gen()->create_expr(yypvt[-0].exval, AST_Expression::EV_ulong);
	} break;
case 103:
# line 847 "idl.yy"
{
	    idl_global->set_parse_state(IDL_GlobalData::PS_TypedefSeen);
	  } break;
case 108:
# line 858 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_TypeSpecSeen);
        } break;
case 109:
# line 862 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_DecllistActiveIterator *l;
	  FE_Declarator		*d = NULL;
	  AST_Typedef		*t = NULL;
	  AST_Decl		*v = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_DeclaratorsSeen);
	  /*
	   * Create a list of type renamings. Add them to the
	   * enclosing scope
	   */
	  if (s != NULL && yypvt[-2].dcval != NULL && yypvt[-0].dlval != NULL) {
	    l = new UTL_DecllistActiveIterator(yypvt[-0].dlval);
	    for (;!(l->is_done()); l->next()) {
	      d = l->item();
	      if (d == NULL) 
		continue;
              AST_Type * tp = d->compose(yypvt[-2].dcval);
              if (tp == NULL) 
		continue;
	      t = idl_global->gen()->create_typedef(tp, d->name(), p);
	      (void) s->fe_add_typedef(t);
	    }
	    delete l;
	  }
	} break;
case 112:
# line 899 "idl.yy"
{
	  yyval.dcval = idl_global->scopes()->bottom()->lookup_primitive_type(yypvt[-0].etval);
	} break;
case 114:
# line 904 "idl.yy"
{
	  UTL_Scope	*s = idl_global->scopes()->top_non_null();
	  AST_Decl	*d = NULL;

	  if (s != NULL)
	    d = s->lookup_by_name(yypvt[-0].idlist, I_TRUE);
	  if (d == NULL)
	    idl_global->err()->lookup_error(yypvt[-0].idlist);
	  yyval.dcval = d;
	} break;
case 127:
# line 939 "idl.yy"
{
	  yyval.dlval = new UTL_DeclList(yypvt[-1].deval, yypvt[-0].dlval);
	} break;
case 128:
# line 946 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_DeclsCommaSeen);
        } break;
case 129:
# line 950 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_DeclsDeclSeen);

	  if (yypvt[-3].dlval == NULL)
	    yyval.dlval = new UTL_DeclList(yypvt[-0].deval, NULL);
	  else {
	    yypvt[-3].dlval->nconc(new UTL_DeclList(yypvt[-0].deval, NULL));
	    yyval.dlval = yypvt[-3].dlval;
	  }
	} break;
case 130:
# line 961 "idl.yy"
{
	  yyval.dlval = NULL;
	} break;
case 133:
# line 973 "idl.yy"
{
	  yyval.deval = new FE_Declarator(new UTL_ScopedName(yypvt[-0].idval, NULL),
				 FE_Declarator::FD_simple, NULL);
	} break;
case 134:
# line 981 "idl.yy"
{
	  yyval.deval = new FE_Declarator(new UTL_ScopedName(yypvt[-0].dcval->local_name(), NULL),
				 FE_Declarator::FD_complex,
				 yypvt[-0].dcval);
	} break;
case 137:
# line 995 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_long;
	} break;
case 138:
# line 999 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_longlong;
        } break;
case 139:
# line 1003 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_short;
	} break;
case 140:
# line 1010 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_ulong;
	} break;
case 141:
# line 1014 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_ulonglong;
        } break;
case 142:
# line 1018 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_ushort;
	} break;
case 143:
# line 1025 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_double;
	} break;
case 144:
# line 1029 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_float;
	} break;
case 145:
# line 1033 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_longdouble;
        } break;
case 146:
# line 1040 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_char;
	} break;
case 147:
# line 1044 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_wchar;
        } break;
case 148:
# line 1051 "idl.yy"
{ 
          yyval.etval = AST_Expression::EV_octet;
	} break;
case 149:
# line 1058 "idl.yy"
{ 
	  yyval.etval = AST_Expression::EV_bool;
        } break;
case 150:
# line 1065 "idl.yy"
{
	  yyval.etval = AST_Expression::EV_any;
	} break;
case 151:
# line 1072 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StructSeen);
        } break;
case 152:
# line 1076 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n = new UTL_ScopedName(yypvt[-0].idval, NULL);
	  AST_Structure		*d = NULL;
	  UTL_StrList		*p = idl_global->pragmas();
	  AST_Decl		*v = NULL;

	  idl_global->set_parse_state(IDL_GlobalData::PS_StructIDSeen);
	  /*
	   * Create a node representing a struct declaration. Add it
	   * to the enclosing scope
	   */
	  if (s != NULL) {
	    d = idl_global->gen()->create_structure(n, p);
	    (void) s->fe_add_structure(d);
	  }
	  /*
	   * Push the scope of the struct on the scopes stack
	   */
	  idl_global->scopes()->push(d);
	} break;
case 153:
# line 1098 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StructSqSeen);
        } break;
case 154:
# line 1102 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StructBodySeen);
        } break;
case 155:
# line 1106 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StructQsSeen);
	  /*
	   * Done with this struct. Pop its scope off the scopes stack
	   */
	  if (idl_global->scopes()->top() == NULL)
	    yyval.dcval = NULL;
	  else {
	    yyval.dcval =
	      AST_Structure::narrow_from_scope(
				   idl_global->scopes()->top_non_null());
	    idl_global->scopes()->pop();
	  }
	} break;
case 159:
# line 1131 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_MemberTypeSeen);
        } break;
case 160:
# line 1135 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_MemberDeclsSeen);
        } break;
case 161:
# line 1139 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_DecllistActiveIterator *l = NULL;
	  FE_Declarator		*d = NULL;
	  AST_Field		*f = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_MemberDeclsCompleted);
	  /*
	   * Check for illegal recursive use of type
	   */
	  if (yypvt[-4].dcval != NULL && AST_illegal_recursive_type(yypvt[-4].dcval))
	    idl_global->err()->error1(UTL_Error::EIDL_RECURSIVE_TYPE, yypvt[-4].dcval);
	  /*
	   * Create a node representing a struct or exception member
	   * Add it to the enclosing scope
	   */
	  else if (s != NULL && yypvt[-4].dcval != NULL && yypvt[-2].dlval != NULL) {
	    l = new UTL_DecllistActiveIterator(yypvt[-2].dlval);
	    for (;!(l->is_done()); l->next()) {
	      d = l->item();
	      if (d == NULL) 
		continue;
 	      AST_Type *tp = d->compose(yypvt[-4].dcval);
	      if (tp == NULL) 
		continue;
	      f = idl_global->gen()->create_field(tp, d->name(), p);
	      (void) s->fe_add_field(f);
	    }
	    delete l;
	  }
	} break;
case 162:
# line 1172 "idl.yy"
{
	  idl_global->err()->syntax_error(idl_global->parse_state());
	} break;
case 163:
# line 1176 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
	  yyerrok;
	} break;
case 164:
# line 1184 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionSeen);
        } break;
case 165:
# line 1188 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionIDSeen);
        } break;
case 166:
# line 1192 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SwitchSeen);
        } break;
case 167:
# line 1196 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SwitchOpenParSeen);
        } break;
case 168:
# line 1200 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SwitchTypeSeen);
        } break;
case 169:
# line 1204 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n = new UTL_ScopedName(yypvt[-8].idval, NULL);
	  AST_Union		*u = NULL;
	  AST_Decl		*v = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_SwitchCloseParSeen);
	  /*
	   * Create a node representing a union. Add it to its enclosing
	   * scope
	   */
	  if (yypvt[-2].dcval != NULL && s != NULL) {
 	    AST_ConcreteType    *tp = AST_ConcreteType::narrow_from_decl(yypvt[-2].dcval);
            if (tp == NULL) {
              idl_global->err()->not_a_type(yypvt[-2].dcval);
            } else {
	      u = idl_global->gen()->create_union(tp, n, p);
	      (void) s->fe_add_union(u);
 	    }
	  }
	  /*
	   * Push the scope of the union on the scopes stack
	   */
	  idl_global->scopes()->push(u);
	} break;
case 170:
# line 1231 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionSqSeen);
        } break;
case 171:
# line 1235 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionBodySeen);
        } break;
case 172:
# line 1239 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionQsSeen);
	  /*
	   * Done with this union. Pop its scope from the scopes stack
	   */
	  if (idl_global->scopes()->top() == NULL)
	    yyval.dcval = NULL;
	  else {
	    yyval.dcval =
	      AST_Union::narrow_from_scope(
				idl_global->scopes()->top_non_null());
	    idl_global->scopes()->pop();
	  }
	} break;
case 173:
# line 1257 "idl.yy"
{
	  yyval.dcval = idl_global->scopes()->bottom()->lookup_primitive_type(yypvt[-0].etval);
	} break;
case 174:
# line 1261 "idl.yy"
{
	  yyval.dcval = idl_global->scopes()->bottom()->lookup_primitive_type(yypvt[-0].etval);
	} break;
case 175:
# line 1265 "idl.yy"
{
	  yyval.dcval = idl_global->scopes()->bottom()->lookup_primitive_type(yypvt[-0].etval);
	} break;
case 176:
# line 1269 "idl.yy"
{
	  yyval.dcval = idl_global->scopes()->bottom()->lookup_primitive_type(yypvt[-0].etval);
	} break;
case 178:
# line 1274 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  AST_Decl		*d = NULL;
	  AST_PredefinedType	*p = NULL;
	  AST_Typedef		*t = NULL;
	  long			found = I_FALSE;

	  /*
	   * The discriminator is a scoped name. Try to resolve to
	   * one of the scalar types or to an enum. Thread through
	   * typedef's to arrive at the base type at the end of the
	   * chain
	   */
	  if (s != NULL && (d = s->lookup_by_name(yypvt[-0].idlist, I_TRUE)) != NULL) {
	    while (!found) {
	      switch (d->node_type()) {
	      case AST_Decl::NT_enum:
		yyval.dcval = d;
		found = I_TRUE;
		break;
	      case AST_Decl::NT_pre_defined:
		p = AST_PredefinedType::narrow_from_decl(d);
		if (p != NULL) {
		  switch (p->pt()) {
		  case AST_PredefinedType::PT_long:
		  case AST_PredefinedType::PT_ulong:
		  case AST_PredefinedType::PT_longlong:
		  case AST_PredefinedType::PT_ulonglong:
		  case AST_PredefinedType::PT_short:
		  case AST_PredefinedType::PT_char:
		  case AST_PredefinedType::PT_wchar:
		  case AST_PredefinedType::PT_octet:
		  case AST_PredefinedType::PT_boolean:
		    yyval.dcval = p;
		    found = I_TRUE;
		    break;
		  default:
		    yyval.dcval = NULL;
		    found = I_TRUE;
		    break;
		  }
		}
		break;
	      case AST_Decl::NT_typedef:
		t = AST_Typedef::narrow_from_decl(d);
		if (t != NULL) d = t->base_type();
		break;
	      default:
		yyval.dcval = NULL;
		found = I_TRUE;
		break;
	      }
	    }
	  } else
	    yyval.dcval = NULL;

	  if (yyval.dcval == NULL)
	    idl_global->err()->lookup_error(yypvt[-0].idlist);
	} break;
case 182:
# line 1344 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionLabelSeen);
        } break;
case 183:
# line 1348 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionElemSeen);
        } break;
case 184:
# line 1352 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_LabellistActiveIterator *l = NULL;
	  AST_UnionLabelSet	*ulset = NULL;
	  AST_UnionLabel	*d = NULL;
	  AST_UnionBranch	*b = NULL;
	  AST_Field		*f = yypvt[-2].ffval;

	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionElemCompleted);
	  /*
	   * Create a node representing the branch of a union.
	   * Add it to the enclosing scope (the union scope)
	   */
	  if (s != NULL && yypvt[-4].llval != NULL && yypvt[-2].ffval != NULL) {
	    l = new UTL_LabellistActiveIterator(yypvt[-4].llval);
	    ulset = new AST_UnionLabelSet(yypvt[-4].llval->length());
	    for (;!(l->is_done()); l->next()) {
	      d = l->item();
	      if (d == NULL)
		continue;
	      ulset->add_element(d);
	    }
	    b = idl_global->gen()->create_union_branch(ulset,
						      f->field_type(),
						      f->name(),
						      f->pragmas());
	    (void) s->fe_add_union_branch(b);
	    delete l;
	  }
	} break;
case 185:
# line 1383 "idl.yy"
{
	  idl_global->err()->syntax_error(idl_global->parse_state());
	} break;
case 186:
# line 1387 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_NoState);
	  yyerrok;
	} break;
case 187:
# line 1395 "idl.yy"
{
	  yyval.llval = new UTL_LabelList(yypvt[-1].ulval, yypvt[-0].llval);
	} break;
case 188:
# line 1402 "idl.yy"
{
	  if (yypvt[-1].llval == NULL)
	    yyval.llval = new UTL_LabelList(yypvt[-0].ulval, NULL);
	  else {
	    yypvt[-1].llval->nconc(new UTL_LabelList(yypvt[-0].ulval, NULL));
	    yyval.llval = yypvt[-1].llval;
	  }
	} break;
case 189:
# line 1411 "idl.yy"
{
	  yyval.llval = NULL;
	} break;
case 190:
# line 1418 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_DefaultSeen);
        } break;
case 191:
# line 1422 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_LabelColonSeen);

	  yyval.ulval = idl_global->gen()->
	            create_union_label(AST_UnionLabel::UL_default,
				       NULL);
	} break;
case 192:
# line 1430 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_CaseSeen);
        } break;
case 193:
# line 1434 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_LabelExprSeen);
        } break;
case 194:
# line 1438 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_LabelColonSeen);

	  yyval.ulval = idl_global->gen()->create_union_label(AST_UnionLabel::UL_label,
						     yypvt[-2].exval);
	} break;
case 195:
# line 1448 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionElemTypeSeen);
        } break;
case 196:
# line 1452 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_UnionElemDeclSeen);
	  /*
	   * Check for illegal recursive use of type
	   */
	  if (yypvt[-2].dcval != NULL && AST_illegal_recursive_type(yypvt[-2].dcval))
	    idl_global->err()->error1(UTL_Error::EIDL_RECURSIVE_TYPE, yypvt[-2].dcval);
	  /*
	   * Create a field in a union branch
	   */
	  else if (yypvt[-2].dcval == NULL || yypvt[-0].deval == NULL)
	    yyval.ffval = NULL;
	  else {
	    AST_Type *tp = yypvt[-0].deval->compose(yypvt[-2].dcval);
	    if (tp == NULL)
	      yyval.ffval = NULL;
 	    else
	      yyval.ffval = idl_global->gen()->create_field(tp,
			        		   yypvt[-0].deval->name(),
			        		   idl_global->pragmas());
	  }
	} break;
case 197:
# line 1478 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_EnumSeen);
        } break;
case 198:
# line 1482 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n = new UTL_ScopedName(yypvt[-0].idval, NULL);
	  AST_Enum		*e = NULL;
	  AST_Decl		*v = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_EnumIDSeen);
	  /*
	   * Create a node representing an enum and add it to its
	   * enclosing scope
	   */
	  if (s != NULL) {
	    e = idl_global->gen()->create_enum(n, p);
	    /*
	     * Add it to its defining scope
	     */
	    (void) s->fe_add_enum(e);
	  }
	  /*
	   * Push the enum scope on the scopes stack
	   */
	  idl_global->scopes()->push(e);
	} break;
case 199:
# line 1507 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_EnumSqSeen);
        } break;
case 200:
# line 1511 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_EnumBodySeen);
        } break;
case 201:
# line 1515 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_EnumQsSeen);
	  /*
	   * Done with this enum. Pop its scope from the scopes stack
	   */
	  if (idl_global->scopes()->top() == NULL)
	    yyval.dcval = NULL;
	  else {
	    yyval.dcval = AST_Enum::narrow_from_scope(idl_global->scopes()->top_non_null());
	    idl_global->scopes()->pop();
	  }
	} break;
case 203:
# line 1534 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_EnumCommaSeen);
        } break;
case 206:
# line 1543 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n =
		new UTL_ScopedName(new Identifier(yypvt[-0].strval, 1, 0, I_FALSE), NULL);
	  AST_EnumVal		*e = NULL;
	  AST_Enum		*c = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  /*
	   * Create a node representing one enumerator in an enum
	   * Add it to the enclosing scope (the enum scope)
	   */
	  if (s != NULL && s->scope_node_type() == AST_Decl::NT_enum) {
	    c = AST_Enum::narrow_from_scope(s);
	    if (c != NULL) 
	      e = idl_global->gen()->create_enum_val(c->next_enum_val(), n, p);
	    (void) s->fe_add_enum_val(e);
	  }
	} break;
case 207:
# line 1567 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceCommaSeen);
        } break;
case 208:
# line 1571 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceExprSeen);
        } break;
case 209:
# line 1575 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceQsSeen);
	  /*
	   * Remove sequence marker from scopes stack
	   */
	  if (idl_global->scopes()->top() == NULL)
	    idl_global->scopes()->pop();
	  /*
	   * Create a node representing a sequence
	   */
	  if (yypvt[-2].exval == NULL || yypvt[-2].exval->coerce(AST_Expression::EV_ulong) == NULL) {
	    idl_global->err()->coercion_error(yypvt[-2].exval, AST_Expression::EV_ulong);
	    yyval.dcval = NULL;
	  } else if (yypvt[-5].dcval == NULL) {
	    yyval.dcval = NULL;
	  } else {
 	    AST_Type *tp = AST_Type::narrow_from_decl(yypvt[-5].dcval); 
	    if (tp == NULL) 
	      yyval.dcval = NULL;
	    else {
	      yyval.dcval = idl_global->gen()->create_sequence(yypvt[-2].exval, tp);
	      /*
	       * Add this AST_Sequence to the types defined in the global scope
	       */
	      (void) idl_global->root()
		        ->fe_add_sequence(AST_Sequence::narrow_from_decl(yyval.dcval));
 	    }
	  }
	} break;
case 210:
# line 1606 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceQsSeen);
	  /*
	   * Remove sequence marker from scopes stack
	   */
	  if (idl_global->scopes()->top() == NULL)
	    idl_global->scopes()->pop();
	  /*
	   * Create a node representing a sequence
	   */
	  if (yypvt[-1].dcval == NULL)
	    yyval.dcval = NULL;
	  else {
	    AST_Type *tp = AST_Type::narrow_from_decl(yypvt[-1].dcval);
	    if (tp == NULL)
	      yyval.dcval = NULL;
            else {
	      yyval.dcval =
	        idl_global->gen()->create_sequence(
		  	     idl_global->gen()->create_expr((unsigned long) 0),
			     tp);
	      /*
	       * Add this AST_Sequence to the types defined in the global scope
	       */
	      (void) idl_global->root()
		        ->fe_add_sequence(AST_Sequence::narrow_from_decl(yyval.dcval));
	    }
	  }
	} break;
case 211:
# line 1639 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceSeen);
	  /*
	   * Push a sequence marker on scopes stack
	   */
	  idl_global->scopes()->push(NULL);
	} break;
case 212:
# line 1647 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceSqSeen);
        } break;
case 213:
# line 1651 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_SequenceTypeSeen);
	  yyval.dcval = yypvt[-0].dcval;
        } break;
case 214:
# line 1660 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringSqSeen);
        } break;
case 215:
# line 1664 "idl.yy"
{
	   idl_global->set_parse_state(IDL_GlobalData::PS_StringExprSeen);
        } break;
case 216:
# line 1668 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringQsSeen);
	  /*
	   * Create a node representing a string
	   */
	  if (yypvt[-2].exval == NULL || yypvt[-2].exval->coerce(AST_Expression::EV_ulong) == NULL) {
	    idl_global->err()->coercion_error(yypvt[-2].exval, AST_Expression::EV_ulong);
	    yyval.dcval = NULL;
	  } else {
	    yyval.dcval = idl_global->gen()->create_string(yypvt[-2].exval);
	    /*
	     * Add this AST_String to the types defined in the global scope
	     */
	    (void) idl_global->root()
		      ->fe_add_string(AST_String::narrow_from_decl(yyval.dcval));
	  }
	} break;
case 217:
# line 1686 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringCompleted);
	  /*
	   * Create a node representing a string
	   */
	  yyval.dcval =
	    idl_global->gen()->create_string(
			 idl_global->gen()->create_expr((unsigned long) 0));
	  /*
	   * Add this AST_String to the types defined in the global scope
	   */
	  (void) idl_global->root()
                    ->fe_add_string(AST_String::narrow_from_decl(yyval.dcval));
	} break;
case 218:
# line 1704 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringSeen);
        } break;
case 219:
# line 1712 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringSqSeen);
        } break;
case 220:
# line 1716 "idl.yy"
{
	   idl_global->set_parse_state(IDL_GlobalData::PS_StringExprSeen);
        } break;
case 221:
# line 1720 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringQsSeen);
	  /*
	   * Create a node representing a string
	   */
	  if (yypvt[-2].exval == NULL || yypvt[-2].exval->coerce(AST_Expression::EV_ulong) == NULL) {
	    idl_global->err()->coercion_error(yypvt[-2].exval, AST_Expression::EV_ulong);
	    yyval.dcval = NULL;
	  } else {
	    yyval.dcval = idl_global->gen()->create_wstring(yypvt[-2].exval);
	    /*
	     * Add this AST_String to the types defined in the global scope
	     */
	    (void) idl_global->root()
		      ->fe_add_string(AST_String::narrow_from_decl(yyval.dcval));
	  }
	} break;
case 222:
# line 1738 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringCompleted);
	  /*
	   * Create a node representing a string
	   */
	  yyval.dcval =
	    idl_global->gen()->create_wstring(
			 idl_global->gen()->create_expr((unsigned long) 0));
	  /*
	   * Add this AST_String to the types defined in the global scope
	   */
	  (void) idl_global->root()
                    ->fe_add_string(AST_String::narrow_from_decl(yyval.dcval));
	} break;
case 223:
# line 1756 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_StringSeen);
        } break;
case 224:
# line 1763 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ArrayIDSeen);
        } break;
case 225:
# line 1767 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ArrayCompleted);
	  /*
	   * Create a node representing an array
	   */
	  if (yypvt[-0].elval != NULL) {
	     yyval.dcval = idl_global->gen()->create_array(new UTL_ScopedName(yypvt[-2].idval, NULL),
						  yypvt[-0].elval->length(), yypvt[-0].elval);
	  }
	} break;
case 226:
# line 1781 "idl.yy"
{
	  yyval.elval = new UTL_ExprList(yypvt[-1].exval, yypvt[-0].elval);
	} break;
case 227:
# line 1788 "idl.yy"
{
	  if (yypvt[-1].elval == NULL)
	    yyval.elval = new UTL_ExprList(yypvt[-0].exval, NULL);
	  else {
	    yypvt[-1].elval->nconc(new UTL_ExprList(yypvt[-0].exval, NULL));
	    yyval.elval = yypvt[-1].elval;
	  }
	} break;
case 228:
# line 1797 "idl.yy"
{
	  yyval.elval = NULL;
	} break;
case 229:
# line 1804 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_DimSqSeen);
        } break;
case 230:
# line 1808 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_DimExprSeen);
        } break;
case 231:
# line 1812 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_DimQsSeen);
	  /*
	   * Array dimensions are expressions which must be coerced to
	   * positive integers
	   */
	  if (yypvt[-2].exval == NULL || yypvt[-2].exval->coerce(AST_Expression::EV_ulong) == NULL) {
	    idl_global->err()->coercion_error(yypvt[-2].exval, AST_Expression::EV_ulong);
	    yyval.exval = NULL;
	  } else
	    yyval.exval = yypvt[-2].exval;
	} break;
case 232:
# line 1829 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_AttrSeen);
        } break;
case 233:
# line 1833 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_AttrTypeSeen);
        } break;
case 234:
# line 1837 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_DecllistActiveIterator *l = NULL;
	  AST_Attribute		*a = NULL;
	  FE_Declarator		*d = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_AttrCompleted);
	  /*
	   * Create nodes representing attributes and add them to the
	   * enclosing scope
	   */
	  if (s != NULL && yypvt[-2].dcval != NULL && yypvt[-0].dlval != NULL) {
	    l = new UTL_DecllistActiveIterator(yypvt[-0].dlval);
	    for (;!(l->is_done()); l->next()) {
	      d = l->item();
	      if (d == NULL)
		continue;
	      AST_Type *tp = d->compose(yypvt[-2].dcval);
 	      if (tp == NULL)
	 	continue;
	      a = idl_global->gen()->create_attribute(yypvt[-5].bval, tp, d->name(), p);
	      /*
	       * Add one attribute to the enclosing scope
	       */
	      (void) s->fe_add_attribute(a);
	    }
	    delete l;
	  }
	} break;
case 235:
# line 1871 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_AttrROSeen);
	  yyval.bval = I_TRUE;
	} break;
case 236:
# line 1876 "idl.yy"
{
	  yyval.bval = I_FALSE;
	} break;
case 237:
# line 1883 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptSeen);
	} break;
case 238:
# line 1887 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n = new UTL_ScopedName(yypvt[-0].idval, NULL);
	  AST_Exception		*e = NULL;
	  UTL_StrList		*p = idl_global->pragmas();
	  AST_Decl		*v = NULL;

	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptIDSeen);
	  /*
	   * Create a node representing an exception and add it to
	   * the enclosing scope
	   */
	  if (s != NULL) {
	    e = idl_global->gen()->create_exception(n, p);
	    (void) s->fe_add_exception(e);
	  }
	  /*
	   * Push the exception scope on the scope stack
	   */
	  idl_global->scopes()->push(e);
	} break;
case 239:
# line 1909 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptSqSeen);
        } break;
case 240:
# line 1913 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptBodySeen);
        } break;
case 241:
# line 1917 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_ExceptQsSeen);
	  /*
	   * Done with this exception. Pop its scope from the scope stack
	   */
	  idl_global->scopes()->pop();
	} break;
case 242:
# line 1929 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpTypeSeen);
        } break;
case 243:
# line 1933 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  UTL_ScopedName	*n =
		new UTL_ScopedName(new Identifier(yypvt[-0].strval, 1, 0, I_FALSE), NULL);
	  AST_Operation		*o = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_OpIDSeen);
	  /*
	   * Create a node representing an operation on an interface
	   * and add it to its enclosing scope
	   */
	  if (s != NULL && yypvt[-2].dcval != NULL) {
	    AST_Type *tp = AST_Type::narrow_from_decl(yypvt[-2].dcval);
            if (tp == NULL) {
              idl_global->err()->not_a_type(yypvt[-2].dcval);
            } else if (tp->node_type() == AST_Decl::NT_except) {
              idl_global->err()->not_a_type(yypvt[-2].dcval);
            } else {
	      o = idl_global->gen()->create_operation(tp, yypvt[-3].ofval, n, p);
	      (void) s->fe_add_operation(o);
	    }
	  }
	  /*
	   * Push the operation scope onto the scopes stack
	   */
	  idl_global->scopes()->push(o);
	} break;
case 244:
# line 1962 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpParsCompleted);
        } break;
case 245:
# line 1966 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpRaiseCompleted);
        } break;
case 246:
# line 1970 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  AST_Operation		*o = NULL;

	  idl_global->set_parse_state(IDL_GlobalData::PS_OpCompleted);
	  /*
	   * Add exceptions and context to the operation
	   */
	  if (s != NULL && s->scope_node_type() == AST_Decl::NT_op) {
	    o = AST_Operation::narrow_from_scope(s);

	    if (yypvt[-2].nlval != NULL && o != NULL)
	      (void) o->fe_add_exceptions(yypvt[-2].nlval);
	    if (yypvt[-0].slval != NULL)
	      (void) o->fe_add_context(yypvt[-0].slval);
	  }
	  /*
	   * Done with this operation. Pop its scope from the scopes stack
	   */
	  idl_global->scopes()->pop();
	} break;
case 247:
# line 1995 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpAttrSeen);
	  yyval.ofval = AST_Operation::OP_oneway;
	} break;
case 248:
# line 2000 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpAttrSeen);
	  yyval.ofval = AST_Operation::OP_idempotent;
	} break;
case 249:
# line 2005 "idl.yy"
{
	  yyval.ofval = AST_Operation::OP_noflags;
	} break;
case 251:
# line 2013 "idl.yy"
{
	  yyval.dcval =
	    idl_global->scopes()->bottom()
	       ->lookup_primitive_type(AST_Expression::EV_void);
	} break;
case 252:
# line 2022 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpSqSeen);
        } break;
case 253:
# line 2026 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpQsSeen);
        } break;
case 254:
# line 2030 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpSqSeen);
        } break;
case 255:
# line 2035 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpQsSeen);
        } break;
case 257:
# line 2045 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpParCommaSeen);
        } break;
case 260:
# line 2054 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpParDirSeen);
        } break;
case 261:
# line 2058 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpParTypeSeen);
        } break;
case 262:
# line 2062 "idl.yy"
{
	  UTL_Scope		*s = idl_global->scopes()->top_non_null();
	  AST_Argument		*a = NULL;
	  UTL_StrList		*p = idl_global->pragmas();

	  idl_global->set_parse_state(IDL_GlobalData::PS_OpParDeclSeen);
	  /*
	   * Create a node representing an argument to an operation
	   * Add it to the enclosing scope (the operation scope)
	   */
	  if (yypvt[-2].dcval != NULL && yypvt[-0].deval != NULL && s != NULL) {
	    AST_Type *tp = yypvt[-0].deval->compose(yypvt[-2].dcval);
	    if (tp != NULL) {
	      a = idl_global->gen()->create_argument(yypvt[-4].dival, tp, yypvt[-0].deval->name(), p);
	      (void) s->fe_add_argument(a);
	    }
	  }
	} break;
case 263:
# line 2084 "idl.yy"
{
	  yyval.dival = AST_Argument::dir_IN;
	} break;
case 264:
# line 2088 "idl.yy"
{
	  yyval.dival = AST_Argument::dir_OUT;
	} break;
case 265:
# line 2092 "idl.yy"
{
	  yyval.dival = AST_Argument::dir_INOUT;
	} break;
case 266:
# line 2099 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpRaiseSeen);
        } break;
case 267:
# line 2103 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpRaiseSqSeen);
        } break;
case 268:
# line 2108 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpRaiseQsSeen);
	  yyval.nlval = yypvt[-1].nlval;
	} break;
case 269:
# line 2113 "idl.yy"
{
	  yyval.nlval = NULL;
	} break;
case 271:
# line 2121 "idl.yy"
{
	  char *s1 = yypvt[-1].sval->get_string();
	  char *s2 = yypvt[-0].sval->get_string();
	  char *sr = new char[strlen(s1) + strlen(s2) + 1];

	  strcpy(sr, s1);
	  strcat(sr, s2);
	  delete yypvt[-1].sval;
	  delete yypvt[-0].sval;
	  yyval.sval = new String(sr);
	} break;
case 272:
# line 2136 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpContextSeen);
        } break;
case 273:
# line 2140 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpContextSqSeen);
        } break;
case 274:
# line 2145 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpContextQsSeen);
	  yyval.slval = yypvt[-1].slval;
	} break;
case 275:
# line 2150 "idl.yy"
{
	  yyval.slval = NULL;
	} break;
case 276:
# line 2157 "idl.yy"
{
	  yyval.slval = new UTL_StrList(yypvt[-1].sval, yypvt[-0].slval);
	} break;
case 277:
# line 2165 "idl.yy"
{
	  idl_global->set_parse_state(IDL_GlobalData::PS_OpContextCommaSeen);
        } break;
case 278:
# line 2169 "idl.yy"
{
	  if (yypvt[-3].slval == NULL)
	    yyval.slval = new UTL_StrList(yypvt[-0].sval, NULL);
	  else {
	    yypvt[-3].slval->nconc(new UTL_StrList(yypvt[-0].sval, NULL));
	    yyval.slval = yypvt[-3].slval;
	  }
	} break;
case 279:
# line 2178 "idl.yy"
{
	  yyval.slval = NULL;
	} break;
	}
	goto yystack;		/* reset registers in driver code */
}

