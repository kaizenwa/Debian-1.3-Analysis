/* @(#) field.h,v 1.3 1992/07/11 11:49:14 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * field.h:
 *	interface file for routines in field.c.
 */

/* types used in field.h */
struct token {				/* elements of token lists */
    struct token *succ;			/* next member in the queue */
    char *text;				/* text for this member */
    char *space;			/* preceding white space/comments */
    int form;				/* type of token represented */
};

/* token forms for token.form */
#define T_QUOTE	    0x01		/* literal text within quotes */
#define T_DOMLIT    0x02		/* domain literal in square brackets */
#define T_OPER	    0x04		/* single operator token */
#define T_TEXT	    0x08		/* text literal token */
#define T_END	    0x10		/* end of tokens */
#define T_ERROR	    0x20		/* error is error message */

/* macros for interpreting token forms */
#define QUOTETOK(f)	(( (f) & T_QUOTE ) != 0)	/* quote token */
#define DOMLITTOK(f)	(( (f) & T_DOMLIT ) != 0)	/* domain token */
#define OPERTOK(f)	(( (f) & T_OPER ) != 0)		/* operator token */
#define TEXTTOK(f)	(( (f) & T_TEXT ) != 0)		/* text token */
#define ENDTOK(f)	(( (f) & T_END ) != 0)		/* end of tokens */

/* WORDTOK - any token other than an operator */
#define WORDTOK(f)	(( (f) & (T_QUOTE|T_DOMLIT|T_TEXT) ) != 0)

/* values returned by various pattern matching routines */
#define T_NOMATCH	0		/* did not match pattern */
#define T_GENERAL	1		/* matched general address form */
#define T_ROUTE		2		/* matched a route */
#define T_GROUP		3		/* matched a group */
#define T_GROUPTERM	4		/* matched a group terminator */
#define T_MODIFIED	5		/* modified matched group or general */
#define T_MUTANT_FORM	6		/* okay mutant form found in route */

/* behavior flags for process_field */
#define F_LOCAL		0x01		/* process as locally generated mail */
#define F_STRICT	0x02		/* use strict RFC822 reformatting */
#define F_ALIAS		0x04		/* process as with aliases file */
