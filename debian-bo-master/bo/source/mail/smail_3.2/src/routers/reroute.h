/* @(#) reroute.h,v 1.1 1992/09/20 13:02:08 tron Exp */

/*
 *    Copyright (C) 1992 Uwe Doering
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * reroute.h:
 *	interface file for reroute driver.
 */

/* macros local to the reroute driver */

/* flag attributes */
#define RR_REOPEN	0x00010000	/* always reopen database to search */
#define RR_OPTIONAL	0x00020000	/* the reroute file is optional */
#define RR_TRYAGAIN	0x00040000	/* defer address on open failure */
#define RR_CACHEOPEN	0x00080000	/* cache open file descriptor */
#define RR_OPENFAIL	0x00100000	/* internal - open got FILE_FAIL */
#define RR_OPENAGAIN	0x00200000	/* internal - open got FILE_AGAIN */
#define RR_OPENNOMATCH	0x00400000	/* internal - open got FILE_NOMATCH */
#define RR_MATCHALL	0x00800000	/* reroute all bang path addresses */
#define RR_MATCHLOCAL	0x01000000	/* match against local host names */
#define RR_MATCHDB	0x02000000	/* match against reroute database */
#define RR_BOUNCEONLY	0x04000000	/* restrict matchlocal to bounces */

/* private information stored per router file entry */
struct reroute_private {
    char *file;				/* file attribute */
    char *proto;			/* protocol name */
    char *domain;			/* optional domain names */
    char *required;			/* required domain names */
    int retries;			/* max count of retries */
    int interval;			/* sleep interval between retries */
    char *database;			/* internal - open database */
    char *error_text;			/* internal - error text from open */
};
