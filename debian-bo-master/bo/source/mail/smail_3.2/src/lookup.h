/* @(#) lookup.h,v 1.3 1992/07/11 11:49:34 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * lookup.:
 *	Interface file for lookup.c
 */

/* macros used in communicating with functions in lookup.c */
#define DB_FAIL		(-1)		/* unrecoverable failure */
#define DB_AGAIN	(-2)		/* retry operation at a later time */
#define DB_NOMATCH	(-3)		/* no match was found */
#define DB_SUCCEED	0		/* operation was successful */
#define FILE_SUCCEED	0		/* operation on file was successful */
#define FILE_FAIL	(-4)		/* unrecoverable database failure */
#define FILE_AGAIN	(-5)		/* try using database later */
#define FILE_NOMATCH	(-6)		/* no such file */
