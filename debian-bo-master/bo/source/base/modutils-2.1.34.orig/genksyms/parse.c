
/*  A Bison parser, made from parse.y
 by  GNU Bison version 1.25
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	ASM_KEYW	258
#define	ATTRIBUTE_KEYW	259
#define	AUTO_KEYW	260
#define	CHAR_KEYW	261
#define	CONST_KEYW	262
#define	DOUBLE_KEYW	263
#define	ENUM_KEYW	264
#define	EXTERN_KEYW	265
#define	FLOAT_KEYW	266
#define	INLINE_KEYW	267
#define	INT_KEYW	268
#define	LONG_KEYW	269
#define	REGISTER_KEYW	270
#define	SHORT_KEYW	271
#define	SIGNED_KEYW	272
#define	STATIC_KEYW	273
#define	STRUCT_KEYW	274
#define	TYPEDEF_KEYW	275
#define	UNION_KEYW	276
#define	UNSIGNED_KEYW	277
#define	VOID_KEYW	278
#define	VOLATILE_KEYW	279
#define	EXPORT_SYMBOL_KEYW	280
#define	ASM_PHRASE	281
#define	ATTRIBUTE_PHRASE	282
#define	BRACE_PHRASE	283
#define	BRACKET_PHRASE	284
#define	EXPRESSION_PHRASE	285
#define	CHAR	286
#define	DOTS	287
#define	IDENT	288
#define	INT	289
#define	REAL	290
#define	STRING	291
#define	TYPE	292
#define	OTHER	293
#define	FILENAME	294

#line 24 "parse.y"

#include <assert.h>
#include "genksyms.h"

static int is_typedef;
static char *current_name;
static struct string_list *decl_spec;

static void yyerror(const char *);

static inline void
remove_node(struct string_list **p)
{
  struct string_list *node = *p;
  *p = node->next;
  free_node(node);
}

static inline void
remove_list(struct string_list **pb, struct string_list **pe)
{
  struct string_list *b = *pb, *e = *pe;
  *pb = e;
  free_list(b, e);
}

#ifndef YYSTYPE
#define YYSTYPE int
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



#define	YYFINAL		166
#define	YYFLAG		-32768
#define	YYNTBASE	49

#define YYTRANSLATE(x) ((unsigned)(x) <= 294 ? yytranslate[x] : 93)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    44,
    45,    43,     2,    42,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    48,    40,     2,
    46,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    47,     2,    41,     2,     2,     2,     2,     2,
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
    36,    37,    38,    39
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     5,     6,     9,    10,    14,    16,    18,    20,
    22,    25,    28,    32,    33,    35,    37,    41,    46,    47,
    49,    51,    54,    56,    58,    60,    62,    64,    66,    68,
    70,    72,    75,    78,    81,    85,    89,    93,    96,    99,
   102,   104,   106,   108,   110,   112,   114,   116,   118,   120,
   122,   125,   126,   128,   130,   133,   135,   137,   139,   142,
   144,   146,   151,   156,   159,   163,   167,   170,   172,   174,
   176,   181,   186,   189,   193,   197,   200,   202,   206,   207,
   209,   211,   215,   218,   221,   223,   224,   226,   228,   233,
   238,   241,   245,   249,   253,   254,   256,   259,   263,   267,
   268,   270,   272,   275,   279,   282,   283,   285,   287,   291,
   294,   297,   299,   302,   303,   305,   308,   309,   311
};

static const short yyrhs[] = {    50,
     0,    49,    50,     0,     0,    51,    52,     0,     0,    20,
    53,    54,     0,    54,     0,    78,     0,    90,     0,    92,
     0,     1,    40,     0,     1,    41,     0,    58,    55,    40,
     0,     0,    56,     0,    57,     0,    56,    42,    57,     0,
    68,    91,    89,    79,     0,     0,    59,     0,    60,     0,
    59,    60,     0,    61,     0,    62,     0,     5,     0,    15,
     0,    18,     0,    10,     0,    12,     0,    63,     0,    67,
     0,    19,    33,     0,    21,    33,     0,     9,    33,     0,
    19,    33,    81,     0,    21,    33,    81,     0,     9,    33,
    28,     0,     9,    28,     0,    19,    81,     0,    21,    81,
     0,     6,     0,    16,     0,    13,     0,    14,     0,    17,
     0,    22,     0,    11,     0,     8,     0,    23,     0,    37,
     0,    43,    65,     0,     0,    66,     0,    67,     0,    66,
    67,     0,     7,     0,    24,     0,    27,     0,    64,    68,
     0,    69,     0,    33,     0,    69,    44,    72,    45,     0,
    69,    44,     1,    45,     0,    69,    29,     0,    44,    68,
    45,     0,    44,     1,    45,     0,    64,    70,     0,    71,
     0,    33,     0,    37,     0,    71,    44,    72,    45,     0,
    71,    44,     1,    45,     0,    71,    29,     0,    44,    70,
    45,     0,    44,     1,    45,     0,    73,    32,     0,    73,
     0,    74,    42,    32,     0,     0,    74,     0,    75,     0,
    74,    42,    75,     0,    59,    76,     0,    64,    76,     0,
    77,     0,     0,    33,     0,    37,     0,    77,    44,    72,
    45,     0,    77,    44,     1,    45,     0,    77,    29,     0,
    44,    76,    45,     0,    44,     1,    45,     0,    58,    68,
    28,     0,     0,    80,     0,    46,    30,     0,    47,    82,
    41,     0,    47,     1,    41,     0,     0,    83,     0,    84,
     0,    83,    84,     0,    58,    85,    40,     0,     1,    40,
     0,     0,    86,     0,    87,     0,    86,    42,    87,     0,
    70,    89,     0,    33,    88,     0,    88,     0,    48,    30,
     0,     0,    27,     0,    26,    40,     0,     0,    26,     0,
    25,    44,    33,    45,    40,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    95,    97,   100,   103,   106,   108,   109,   110,   111,   112,
   113,   114,   117,   131,   133,   136,   145,   157,   163,   165,
   167,   169,   172,   180,   183,   184,   184,   184,   184,   187,
   189,   193,   195,   197,   201,   208,   215,   224,   225,   226,
   229,   231,   232,   233,   234,   235,   236,   237,   238,   239,
   242,   247,   249,   252,   254,   257,   258,   258,   261,   263,
   266,   276,   278,   280,   282,   284,   290,   292,   295,   297,
   298,   300,   302,   304,   306,   310,   312,   313,   316,   318,
   321,   323,   327,   332,   335,   338,   340,   349,   354,   356,
   358,   360,   362,   366,   375,   377,   381,   386,   388,   391,
   393,   396,   398,   401,   404,   408,   410,   413,   415,   418,
   420,   421,   424,   428,   430,   433,   437,   439,   442
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","ASM_KEYW",
"ATTRIBUTE_KEYW","AUTO_KEYW","CHAR_KEYW","CONST_KEYW","DOUBLE_KEYW","ENUM_KEYW",
"EXTERN_KEYW","FLOAT_KEYW","INLINE_KEYW","INT_KEYW","LONG_KEYW","REGISTER_KEYW",
"SHORT_KEYW","SIGNED_KEYW","STATIC_KEYW","STRUCT_KEYW","TYPEDEF_KEYW","UNION_KEYW",
"UNSIGNED_KEYW","VOID_KEYW","VOLATILE_KEYW","EXPORT_SYMBOL_KEYW","ASM_PHRASE",
"ATTRIBUTE_PHRASE","BRACE_PHRASE","BRACKET_PHRASE","EXPRESSION_PHRASE","CHAR",
"DOTS","IDENT","INT","REAL","STRING","TYPE","OTHER","FILENAME","';'","'}'","','",
"'*'","'('","')'","'='","'{'","':'","declaration_seq","declaration","@1","declaration1",
"@2","simple_declaration","init_declarator_list_opt","init_declarator_list",
"init_declarator","decl_specifier_seq_opt","decl_specifier_seq","decl_specifier",
"storage_class_specifier","type_specifier","simple_type_specifier","ptr_operator",
"cva_qualifier_seq_opt","cva_qualifier_seq","cva_qualifier","declarator","direct_declarator",
"nested_declarator","direct_nested_declarator","parameter_declaration_clause",
"parameter_declaration_list_opt","parameter_declaration_list","parameter_declaration",
"m_abstract_declarator","direct_m_abstract_declarator","function_definition",
"initializer_opt","initializer","class_body","member_specification_opt","member_specification",
"member_declaration","member_declarator_list_opt","member_declarator_list","member_declarator",
"member_bitfield_declarator","attribute_opt","asm_definition","asm_phrase_opt",
"export_definition", NULL
};
#endif

static const short yyr1[] = {     0,
    49,    49,    51,    50,    53,    52,    52,    52,    52,    52,
    52,    52,    54,    55,    55,    56,    56,    57,    58,    58,
    59,    59,    60,    60,    61,    61,    61,    61,    61,    62,
    62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
    64,    65,    65,    66,    66,    67,    67,    67,    68,    68,
    69,    69,    69,    69,    69,    69,    70,    70,    71,    71,
    71,    71,    71,    71,    71,    72,    72,    72,    73,    73,
    74,    74,    75,    76,    76,    77,    77,    77,    77,    77,
    77,    77,    77,    78,    79,    79,    80,    81,    81,    82,
    82,    83,    83,    84,    84,    85,    85,    86,    86,    87,
    87,    87,    88,    89,    89,    90,    91,    91,    92
};

static const short yyr2[] = {     0,
     1,     2,     0,     2,     0,     3,     1,     1,     1,     1,
     2,     2,     3,     0,     1,     1,     3,     4,     0,     1,
     1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     2,     2,     2,     3,     3,     3,     2,     2,     2,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     2,     0,     1,     1,     2,     1,     1,     1,     2,     1,
     1,     4,     4,     2,     3,     3,     2,     1,     1,     1,
     4,     4,     2,     3,     3,     2,     1,     3,     0,     1,
     1,     3,     2,     2,     1,     0,     1,     1,     4,     4,
     2,     3,     3,     3,     0,     1,     2,     3,     3,     0,
     1,     1,     2,     3,     2,     0,     1,     1,     3,     2,
     2,     1,     2,     0,     1,     2,     0,     1,     5
};

static const short yydefact[] = {     3,
     3,     1,     0,     2,     0,    25,    41,    56,    48,     0,
    28,    47,    29,    43,    44,    26,    42,    45,    27,     0,
     5,     0,    46,    49,    57,     0,     0,    58,    50,     4,
     7,    14,    20,    21,    23,    24,    30,    31,     8,     9,
    10,    11,    12,    38,    34,    32,     0,    39,    19,    33,
    40,     0,   116,    61,    52,     0,     0,    15,    16,     0,
   117,    60,    22,    37,    35,     0,   106,     0,     0,   102,
     6,    14,    36,     0,    51,    53,    54,     0,     0,    13,
     0,    59,   118,    94,   114,    64,     0,   105,    99,    69,
    70,     0,     0,     0,   114,    68,     0,   107,   108,   112,
    98,     0,   103,   117,     0,    55,    66,    65,    17,   115,
    95,     0,    86,     0,    77,    80,    81,   111,     0,    69,
     0,   113,    67,   110,    73,     0,   104,     0,   119,     0,
    18,    96,    63,    87,    50,     0,    86,    83,    85,    62,
    76,     0,    75,    74,     0,     0,   109,    97,     0,    88,
     0,    84,    91,     0,    78,    82,    72,    71,    93,    92,
     0,     0,    90,    89,     0,     0
};

static const short yydefgoto[] = {     1,
     2,     3,    30,    49,    31,    57,    58,    59,    67,    33,
    34,    35,    36,    37,    60,    75,    76,    38,   104,    62,
    95,    96,   114,   115,   116,   117,   138,   139,    39,   131,
   132,    48,    68,    69,    70,    97,    98,    99,   100,   111,
    40,    85,    41
};

static const short yypact[] = {-32768,
    16,-32768,   287,-32768,     2,-32768,-32768,-32768,-32768,   -14,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -26,
-32768,   -22,-32768,-32768,-32768,   -24,     8,-32768,-32768,-32768,
-32768,    63,   400,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,    -5,     6,    76,-32768,   400,     6,
-32768,    33,-32768,-32768,    11,    12,    28,    37,-32768,    63,
   -11,   -20,-32768,-32768,-32768,    19,    30,    31,   120,-32768,
-32768,    63,-32768,    57,-32768,    11,-32768,    65,    73,-32768,
    63,-32768,-32768,-32768,    95,-32768,   164,-32768,-32768,    75,
-32768,    21,   110,    68,    95,   -17,   105,   104,-32768,-32768,
-32768,   108,-32768,   123,   111,-32768,-32768,-32768,-32768,-32768,
   106,   109,   327,   113,   118,   114,-32768,-32768,   117,-32768,
   121,-32768,-32768,-32768,-32768,   205,-32768,    30,-32768,   125,
-32768,-32768,-32768,-32768,-32768,     7,    71,-32768,    17,-32768,
-32768,   367,-32768,-32768,   122,   139,-32768,-32768,   144,-32768,
   145,-32768,-32768,   246,-32768,-32768,-32768,-32768,-32768,-32768,
   147,   148,-32768,-32768,   159,-32768
};

static const short yypgoto[] = {-32768,
   193,-32768,-32768,-32768,   146,-32768,-32768,   116,     0,   -85,
   -33,-32768,-32768,-32768,   -66,-32768,-32768,   -45,   -27,-32768,
   -55,-32768,  -122,-32768,-32768,    56,   -61,-32768,-32768,-32768,
-32768,   -16,-32768,-32768,   130,-32768,-32768,    72,   112,   135,
-32768,-32768,-32768
};


#define	YYLAST		437


static const short yytable[] = {    63,
    94,   113,    32,   146,    61,    51,    46,   149,    86,    77,
    50,   125,    78,    44,    83,   165,    84,     8,    45,    52,
    47,   119,    64,    87,    47,    94,   126,    94,    79,    65,
   106,   162,    82,    73,    25,   -86,   121,    28,   123,   134,
   113,    42,    43,   150,    54,   153,   137,    53,    72,    55,
   136,   -86,    47,   120,    55,    56,   113,    91,    88,    89,
   154,    94,    90,    55,    92,    74,    91,    80,   113,   137,
   137,   101,    55,    92,   151,   152,    66,    93,    81,    63,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    54,    22,    23,    24,    25,
   120,   105,    28,   134,    91,    55,    56,   150,   -19,   107,
    55,    92,    29,    55,   136,   -19,  -100,   108,   -19,   -19,
   102,   110,    93,   -19,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,   122,
    22,    23,    24,    25,   127,   128,    28,    88,    83,   141,
   129,   130,   -19,   133,   148,   142,    29,   140,   166,   -19,
  -101,   143,   -19,   -19,   112,   144,   157,   -19,     6,     7,
     8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    18,    19,    20,   158,    22,    23,    24,    25,   159,   160,
    28,   163,   164,     4,    71,   -79,   109,   156,   103,   147,
    29,   118,     0,     0,     0,   145,     0,     0,   -79,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,     0,    22,    23,    24,    25,   124,
     0,    28,     0,     0,     0,     0,   -79,     0,     0,     0,
     0,    29,     0,     0,     0,     0,   161,     0,     0,   -79,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,     0,    22,    23,    24,    25,
     0,     0,    28,     0,     0,     0,     0,   -79,     0,     0,
     0,     0,    29,     0,     0,     0,     0,     5,     0,     0,
   -79,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    28,     0,     0,     0,     0,     0,   -19,
     0,     0,     0,    29,     0,     0,   -19,     0,     0,   -19,
   -19,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,     0,    22,    23,    24,
    25,     0,     0,    28,     0,     0,     0,     0,     0,   134,
     0,     0,     0,   135,     0,     0,     0,     0,     0,    55,
   136,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,     0,    22,    23,    24,
    25,     0,     0,    28,     0,     0,     0,     0,   155,     0,
     0,     0,     0,    29,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
    22,    23,    24,    25,     0,     0,    28,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    29
};

static const short yycheck[] = {    33,
    67,    87,     3,   126,    32,    22,    33,     1,    29,    55,
    33,    29,     1,    28,    26,     0,    28,     7,    33,    44,
    47,     1,    28,    44,    47,    92,    44,    94,    56,    46,
    76,   154,    60,    50,    24,    29,    92,    27,    94,    33,
   126,    40,    41,    37,    33,    29,   113,    40,    49,    43,
    44,    45,    47,    33,    43,    44,   142,    37,    40,    41,
    44,   128,    33,    43,    44,    33,    37,    40,   154,   136,
   137,    41,    43,    44,   136,   137,     1,    48,    42,   113,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    33,    21,    22,    23,    24,
    33,    45,    27,    33,    37,    43,    44,    37,    33,    45,
    43,    44,    37,    43,    44,    40,    41,    45,    43,    44,
     1,    27,    48,    48,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    30,
    21,    22,    23,    24,    40,    42,    27,    40,    26,    32,
    40,    46,    33,    45,    30,    42,    37,    45,     0,    40,
    41,    45,    43,    44,     1,    45,    45,    48,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    45,    21,    22,    23,    24,    45,    45,
    27,    45,    45,     1,    49,    32,    81,   142,    69,   128,
    37,    90,    -1,    -1,    -1,     1,    -1,    -1,    45,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    -1,    21,    22,    23,    24,    95,
    -1,    27,    -1,    -1,    -1,    -1,    32,    -1,    -1,    -1,
    -1,    37,    -1,    -1,    -1,    -1,     1,    -1,    -1,    45,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
    -1,    -1,    27,    -1,    -1,    -1,    -1,    32,    -1,    -1,
    -1,    -1,    37,    -1,    -1,    -1,    -1,     1,    -1,    -1,
    45,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    27,    -1,    -1,    -1,    -1,    -1,    33,
    -1,    -1,    -1,    37,    -1,    -1,    40,    -1,    -1,    43,
    44,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    -1,    21,    22,    23,
    24,    -1,    -1,    27,    -1,    -1,    -1,    -1,    -1,    33,
    -1,    -1,    -1,    37,    -1,    -1,    -1,    -1,    -1,    43,
    44,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    -1,    21,    22,    23,
    24,    -1,    -1,    27,    -1,    -1,    -1,    -1,    32,    -1,
    -1,    -1,    -1,    37,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    -1,
    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    37
};
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

#line 196 "/usr/lib/bison.simple"

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

case 3:
#line 101 "parse.y"
{ is_typedef = 0; current_name = NULL; decl_spec = NULL; ;
    break;}
case 4:
#line 103 "parse.y"
{ free_list(*yyvsp[0], NULL); *yyvsp[0] = NULL; ;
    break;}
case 5:
#line 107 "parse.y"
{ is_typedef = 1; ;
    break;}
case 6:
#line 108 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 11:
#line 113 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 12:
#line 114 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 13:
#line 119 "parse.y"
{ if (current_name) {
		    struct string_list *decl = (*yyvsp[0])->next;
		    (*yyvsp[0])->next = NULL;
		    add_symbol(current_name,
			       is_typedef ? SYM_TYPEDEF : SYM_NORMAL,
			       decl);
		    current_name = NULL;
		  }
		  yyval = yyvsp[0];
		;
    break;}
case 14:
#line 132 "parse.y"
{ yyval = NULL; ;
    break;}
case 16:
#line 138 "parse.y"
{ struct string_list *decl = *yyvsp[0];
		  *yyvsp[0] = NULL;
		  add_symbol(current_name,
			     is_typedef ? SYM_TYPEDEF : SYM_NORMAL, decl);
		  current_name = NULL;
		  yyval = yyvsp[0];
		;
    break;}
case 17:
#line 146 "parse.y"
{ struct string_list *decl = *yyvsp[0];
		  *yyvsp[0] = NULL;
		  free_list(*yyvsp[-1], NULL);
		  *yyvsp[-1] = decl_spec;
		  add_symbol(current_name,
			     is_typedef ? SYM_TYPEDEF : SYM_NORMAL, decl);
		  current_name = NULL;
		  yyval = yyvsp[0];
		;
    break;}
case 18:
#line 159 "parse.y"
{ yyval = yyvsp[0] ? yyvsp[0] : yyvsp[-1] ? yyvsp[-1] : yyvsp[-2] ? yyvsp[-2] : yyvsp[-3]; ;
    break;}
case 19:
#line 164 "parse.y"
{ decl_spec = NULL; ;
    break;}
case 21:
#line 168 "parse.y"
{ decl_spec = *yyvsp[0]; ;
    break;}
case 22:
#line 169 "parse.y"
{ decl_spec = *yyvsp[0]; ;
    break;}
case 23:
#line 174 "parse.y"
{ /* Version 2 checksumming ignores storage class, as that
		     is really irrelevant to the linkage.  */
		  if (checksum_version > 1)
		    remove_node(yyvsp[0]);
		  yyval = yyvsp[0];
		;
    break;}
case 32:
#line 194 "parse.y"
{ remove_node(yyvsp[-1]); (*yyvsp[0])->tag = SYM_STRUCT; yyval = yyvsp[0]; ;
    break;}
case 33:
#line 196 "parse.y"
{ remove_node(yyvsp[-1]); (*yyvsp[0])->tag = SYM_UNION; yyval = yyvsp[0]; ;
    break;}
case 34:
#line 198 "parse.y"
{ remove_node(yyvsp[-1]); (*yyvsp[0])->tag = SYM_ENUM; yyval = yyvsp[0]; ;
    break;}
case 35:
#line 202 "parse.y"
{ struct string_list *s = *yyvsp[0], *i = *yyvsp[-1], *r;
		  r = copy_node(i); r->tag = SYM_STRUCT;
		  r->next = (*yyvsp[-2])->next; *yyvsp[0] = r; (*yyvsp[-2])->next = NULL;
		  add_symbol(i->string, SYM_STRUCT, s);
		  yyval = yyvsp[0];
		;
    break;}
case 36:
#line 209 "parse.y"
{ struct string_list *s = *yyvsp[0], *i = *yyvsp[-1], *r;
		  r = copy_node(i); r->tag = SYM_UNION;
		  r->next = (*yyvsp[-2])->next; *yyvsp[0] = r; (*yyvsp[-2])->next = NULL;
		  add_symbol(i->string, SYM_UNION, s);
		  yyval = yyvsp[0];
		;
    break;}
case 37:
#line 216 "parse.y"
{ struct string_list *s = *yyvsp[0], *i = *yyvsp[-1], *r;
		  r = copy_node(i); r->tag = SYM_ENUM;
		  r->next = (*yyvsp[-2])->next; *yyvsp[0] = r; (*yyvsp[-2])->next = NULL;
		  add_symbol(i->string, SYM_ENUM, s);
		  yyval = yyvsp[0];
		;
    break;}
case 38:
#line 224 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 39:
#line 225 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 40:
#line 226 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 50:
#line 239 "parse.y"
{ (*yyvsp[0])->tag = SYM_TYPEDEF; yyval = yyvsp[0]; ;
    break;}
case 51:
#line 244 "parse.y"
{ yyval = yyvsp[0] ? yyvsp[0] : yyvsp[-1]; ;
    break;}
case 52:
#line 248 "parse.y"
{ yyval = NULL; ;
    break;}
case 55:
#line 254 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 59:
#line 262 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 61:
#line 268 "parse.y"
{ if (current_name != NULL) {
		    error_with_pos("unexpected second declaration name");
		    YYERROR;
		  } else {
		    current_name = (*yyvsp[0])->string;
		    yyval = yyvsp[0];
		  }
		;
    break;}
case 62:
#line 277 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 63:
#line 279 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 64:
#line 281 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 65:
#line 283 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 66:
#line 285 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 67:
#line 291 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 71:
#line 299 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 72:
#line 301 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 73:
#line 303 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 74:
#line 305 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 75:
#line 307 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 76:
#line 311 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 78:
#line 313 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 79:
#line 317 "parse.y"
{ yyval = NULL; ;
    break;}
case 82:
#line 324 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 83:
#line 329 "parse.y"
{ yyval = yyvsp[0] ? yyvsp[0] : yyvsp[-1]; ;
    break;}
case 84:
#line 334 "parse.y"
{ yyval = yyvsp[0] ? yyvsp[0] : yyvsp[-1]; ;
    break;}
case 86:
#line 339 "parse.y"
{ yyval = NULL; ;
    break;}
case 87:
#line 341 "parse.y"
{ /* For version 2 checksums, we don't want to remember
		     private parameter names.  */
		  if (checksum_version > 1)
		    remove_node(yyvsp[0]);
		  yyval = yyvsp[0];
		;
    break;}
case 88:
#line 350 "parse.y"
{ if (checksum_version > 1)
		    remove_node(yyvsp[0]);
		  yyval = yyvsp[0];
		;
    break;}
case 89:
#line 355 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 90:
#line 357 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 91:
#line 359 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 92:
#line 361 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 93:
#line 363 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 94:
#line 368 "parse.y"
{ struct string_list *decl = *yyvsp[-1];
		  *yyvsp[-1] = NULL;
		  add_symbol(current_name, SYM_NORMAL, decl);
		  yyval = yyvsp[0];
		;
    break;}
case 95:
#line 376 "parse.y"
{ yyval = NULL; ;
    break;}
case 97:
#line 383 "parse.y"
{ remove_list(yyvsp[0], &(*yyvsp[-1])->next); yyval = yyvsp[0]; ;
    break;}
case 98:
#line 387 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 99:
#line 388 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 100:
#line 392 "parse.y"
{ yyval = NULL; ;
    break;}
case 103:
#line 398 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 104:
#line 403 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 105:
#line 405 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 106:
#line 409 "parse.y"
{ yyval = NULL; ;
    break;}
case 109:
#line 415 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 110:
#line 419 "parse.y"
{ yyval = yyvsp[0] ? yyvsp[0] : yyvsp[-1]; ;
    break;}
case 111:
#line 420 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 113:
#line 425 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 114:
#line 429 "parse.y"
{ yyval = NULL; ;
    break;}
case 116:
#line 434 "parse.y"
{ yyval = yyvsp[0]; ;
    break;}
case 117:
#line 438 "parse.y"
{ yyval = NULL; ;
    break;}
case 119:
#line 444 "parse.y"
{ export_symbol((*yyvsp[-2])->string); yyval = yyvsp[0]; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 498 "/usr/lib/bison.simple"

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
#line 448 "parse.y"


static void
yyerror(const char *e)
{
  error_with_pos("%s", e);
}
