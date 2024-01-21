/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:altuser.h,v 1.1 1996/05/29 18:48:57 woods Exp"
 */

/*
 *    Copyright (C) 1995  Nigel Metheringham, PLAnet OnLine
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * altuser.h:
 *	interface file for alternate user director
 *	This is very heavily based on aliasfile with bits from user
 */

/* boolean attributes for director.flags field */
#define ALTUSER_REOPEN		0x00020000	/* always reopen database to search */
#define ALTUSER_TRYAGAIN	0x00040000	/* try again if open fails */
#define ALTUSER_OPENFAIL	0x00080000	/* internal - open returned DB_FAIL */
#define ALTUSER_OPENAGAIN	0x00100000	/* internal - open returned DB_AGAIN */

/* private information from director file entry */
struct altuser_private {
    char *pwfile;	       		/* file attribute */
    char *proto;			/* file access method name */
    int modemask;			/* unsecure mode bits, ala umask(2) */
    char *owners;			/* ownership restrictions */
    char *owngroups;			/* group ownership restrictions */
    int retries;			/* max retries on open */
    int interval;			/* retry interval (seconds) */
    int flags_set;			/* temp - bits to set in addr.flags */
    char *database;			/* temp - open database */
    char *transport;			/* name of the transport */
    char *error_text;			/* temp - error from open */
};
