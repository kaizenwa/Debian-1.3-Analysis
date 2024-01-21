// This file was modified for -*- C++ -*-
// using $RCSfile: yacctoC.h,v $ $Revision: 1.6 $

typedef struct _IGNORED_YYSTYPE  {
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
} IGNORED_YYSTYPE;
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


extern IGNORED_YYSTYPE IGNORED_yylval;
