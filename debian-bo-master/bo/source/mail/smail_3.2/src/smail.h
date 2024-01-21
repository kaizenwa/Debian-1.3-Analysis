/*
#ident	"@(#)smail/src:RELEASE-3_2:smail.h,v 1.6 1996/02/28 14:26:40 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smail.h:
 *	miscellaneous macros used in the smail source files.
 *
 */

/* #defines for general use */
#ifdef	NULL				/* make sure NULL is 0 */
# undef NULL
#endif
#define NULL	0
#define TRUE	1		   /* All true wisdom is found on T-shirts */
#define FALSE	0			/* This fortune is false */
#define SUCCEED	0			/* function succeeded */
#define FAIL	(-1)			/* function failed */
#define SIZE_FILNAM	300		/* Size used for J-random filenames */
#define EQ(a,b)	(strcmp((a),(b)) == 0)	/* TRUE if strings a and b equal */
#define EQIC(a,b) (strcmpic((a),(b)) == 0) /* EQ but case is insignificant */
/*
 * compare a string with a header field to see if names match
 * return TRUE if match, FALSE if no match
 */
#define HDREQ(s,h)	(!strncmpic((s), (h), strlen((s))) &&	\
			 ((h)[strlen((s))] == ':' ||		\
			  isspace((h)[strlen((s))])))
/*
 * size for various names used in the configuration tables read from
 * the startup file, or compiled in with default.c.
 */
#define	CONFIG_NAMSIZ	16		/* 15 chars plus a nul byte */

/*
 * return the integer offset from the start of a given structure type
 * to a given tag.
 */
#define OFFSET(type, tag) \
    (int)((char *)(&((struct type *)0)->tag) - (char *)(struct type *)0)

/* return the number of elements in an array. */
#define TABLESIZE(table) \
    (sizeof(table)/sizeof((table)[0]))

/* return a pointer to the end of a table. */
#define ENDTABLE(table) \
    ((table) + TABLESIZE(table))

/*
 * types for general use
 */
struct list {				/* general purpose list entry */
    struct list *succ;			/* single forward link */
    char *text;				/* data associated with entry */
};
