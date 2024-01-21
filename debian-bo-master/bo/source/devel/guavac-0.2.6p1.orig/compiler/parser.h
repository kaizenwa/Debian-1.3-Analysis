typedef union {
  unicode_string* text;
  deque<unicode_string>* stringList;
  unsigned long longNumber;   // also used for a single character, etc.
  unsigned long long doubleLong;
  float floatNumber;
  double doubleFloat;
  CJavaAccessFlags* modifiers;
  CJavaTypeSignature* typeSignature;
  CJavaFieldSignature* fieldSignature;
  deque<CJavaFieldSignature>* fieldList;
  CExpression* expression;
  ExpressionList* expressionList;
  deque<CVariableDeclaration*>* variableList;
  CStatement* statement;
  CCompoundStatement* compoundStatement;
  StatementList* statementList;
  deque<CCatchClause*>* catchList;
  CCatchClause* catchClause;
} YYSTYPE;
#define	ERROR	258
#define	ABSTRACT	259
#define	BOOLEAN	260
#define	BREAK	261
#define	BYTE	262
#define	CASE	263
#define	CATCH	264
#define	CHAR	265
#define	CLASS	266
#define	CONTINUE	267
#define	DEFAULT	268
#define	DO	269
#define	DOUBLE	270
#define	ELSE	271
#define	EXTENDS	272
#define	FALSE_TOKEN	273
#define	FINAL	274
#define	FINALLY	275
#define	FLOAT	276
#define	FOR	277
#define	IF	278
#define	IMPLEMENTS	279
#define	IMPORT	280
#define	INSTANCEOF	281
#define	INT	282
#define	INTERFACE	283
#define	LONG	284
#define	NATIVE	285
#define	NULL_TOKEN	286
#define	PACKAGE	287
#define	PRIVATE	288
#define	PROTECTED	289
#define	PUBLIC	290
#define	RETURN	291
#define	SHORT	292
#define	STATIC	293
#define	SUPER	294
#define	SWITCH	295
#define	SYNCHRONIZED	296
#define	THIS	297
#define	THROW	298
#define	THROWS	299
#define	TRANSIENT	300
#define	VOLATILE	301
#define	TRUE_TOKEN	302
#define	TRY	303
#define	VOID	304
#define	WHILE	305
#define	SHIFT_RIGHT_EQUALS	306
#define	FILL_SHIFT_RIGHT_EQUALS	307
#define	SHIFT_LEFT_EQUALS	308
#define	ADD_EQUALS	309
#define	SUB_EQUALS	310
#define	MUL_EQUALS	311
#define	DIV_EQUALS	312
#define	MOD_EQUALS	313
#define	AND_EQUALS	314
#define	XOR_EQUALS	315
#define	OR_EQUALS	316
#define	OR	317
#define	AND	318
#define	EQUAL_COMPARE	319
#define	NOT_EQUAL	320
#define	LTEQ	321
#define	GTEQ	322
#define	BITSHIFT_RIGHT	323
#define	FILL_SHIFT_RIGHT	324
#define	SHIFT_LEFT	325
#define	INCR	326
#define	DECR	327
#define	NEW	328
#define	INT_LITERAL	329
#define	CHARACTER_LITERAL	330
#define	LONG_LITERAL	331
#define	FLOAT_LITERAL	332
#define	DOUBLE_LITERAL	333
#define	SYMBOL	334
#define	STRING_LITERAL	335


extern YYSTYPE yylval;
