/* @(#) parse.h,v 1.3 1992/07/11 11:49:53 tron Exp */

/*
 *    Copyright (C) 1987, 1988 by Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * parse.h:
 *	interface file for routines in parse.c.
 */

/* types used in parse.c */
/*
 * attributes returned by parse_entry are a linked list of the
 * following structure
 */
struct attribute {
    struct attribute *succ;		/* next attribute */
    char *name;				/* name of attribute */
    char *value;			/* value of the attribute */
};
