/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:include.h,v 1.4 1996/02/26 18:33:37 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * include.h:
 *	interface file for aliasinclude and forwardinclude drivers.
 */

/*
 * the private structures and boolean attributes are the same for
 * both of these drivers.
 */

/* boolean attribute flags for director.flags field */
#define COPY_SECURE	0x00010000	/* security flags from parent */
#define COPY_OWNERS	0x00020000	/* ownership restricts. from parent */

/* optional private information from director file entry */
struct include_private {
    int modemask;			/* unsecure permissions ala umask(2) */
    char *owners;			/* ownership restrictions */
    char *owngroups;			/* group ownership restrictions */
    char *matchdirector;		/* director to match against */
    int retries;			/* retries on opens */
    int interval;			/* interval between retries */
};
