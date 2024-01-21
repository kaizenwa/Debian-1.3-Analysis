/* @(#) pathalias.h,v 1.3 1992/07/11 11:50:32 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * pathalias.h:
 *	interface file for pathalias driver.
 */

/* macros local to the pathalias driver */

/* flag attributes */
#define PA_REOPEN	0x00010000	/* always reopen database to search */
#define PA_OPTIONAL	0x00020000	/* the paths file is optional */
#define PA_TRYAGAIN	0x00040000	/* defer address on open failure */
#define PA_CACHEOPEN	0x00080000	/* cache open file descriptor */
#define PA_OPENFAIL	0x00100000	/* internal - open got FILE_FAIL */
#define PA_OPENAGAIN	0x00200000	/* internal - open got FILE_AGAIN */
#define PA_OPENNOMATCH	0x00400000	/* internal - open got FILE_NOMATCH */

/* private information stored per router file entry */
struct pathalias_private {
    char *file;				/* file attribute */
    char *proto;			/* protocol name */
    char *domain;			/* optional domain names */
    char *required;			/* required domain names */
    int retries;			/* max count of retries */
    int interval;			/* sleep interval between retries */
    char *database;			/* internal - open database */
    char *error_text;			/* internal - error text from open */
};
