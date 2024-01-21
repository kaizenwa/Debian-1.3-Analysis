typedef union
{
  List *lst;
  Node *node;
  Cons *cons;
  Stmt *stmt;
  Expr *expr;
} YYSTYPE;
#define	tSYMBOL	258
#define	tREGEXP	259
#define	tSTRING	260
#define	tINTEGER	261
#define	tREAL	262
#define	tSUB	263
#define	tSTATE	264
#define	tSTART	265
#define	tSTARTRULES	266
#define	tNAMERULES	267
#define	tBEGIN	268
#define	tEND	269
#define	tRETURN	270
#define	tIF	271
#define	tELSE	272
#define	tLOCAL	273
#define	tWHILE	274
#define	tFOR	275
#define	tOR	276
#define	tAND	277
#define	tEQ	278
#define	tNE	279
#define	tGE	280
#define	tLE	281
#define	tDIV	282


extern YYSTYPE yylval;
