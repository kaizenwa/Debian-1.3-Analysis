typedef union {
  double number;        /* double number */
  int token;            /* token of command */
  char *string;         /* quoted string */
  char *symbol;         /* general symbol */
  char *strsym;         /* string symbol */
  int sep;              /* number of newlines as seperator */
} YYSTYPE;
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


extern YYSTYPE yylval;
