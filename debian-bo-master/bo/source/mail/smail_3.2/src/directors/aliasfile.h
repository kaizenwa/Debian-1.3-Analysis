/* @(#) aliasfile.h,v 1.3 1992/07/11 11:48:29 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * aliasfile.h:
 *	interface file for aliasfile driver.
 */

/* boolean attributes for director.flags field */
#define ALIAS_REOPEN	0x00010000	/* always reopen database to search */
#define ALIAS_OPTIONAL	0x00020000	/* alias file is optional */
#define ALIAS_TRYAGAIN	0x00040000	/* try again if open fails */
#define ALIAS_OPENFAIL	0x00080000	/* internal - open returned DB_FAIL */
#define ALIAS_OPENAGAIN	0x00100000	/* internal - open returned DB_AGAIN */

/* private information from director file entry */
struct aliasfile_private {
    char *file;				/* file attribute */
    char *proto;			/* file access method name */
    int modemask;			/* unsecure mode bits, ala umask(2) */
    char *owners;			/* ownership restrictions */
    char *owngroups;			/* group ownership restrictions */
    int retries;			/* max retries on open */
    int interval;			/* retry interval (seconds) */
    int flags_set;			/* temp - bits to set in addr.flags */
    char *database;			/* temp - open database */
    char *error_text;			/* temp - error from open */
};
