/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:user.h,v 1.6 1996/05/29 18:49:01 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * user.h:
 *	interface file for driver code in user.c
 */

/* private information from director file entry */
struct user_private {
    char *transport;			/* name of the transport */
    char *prefix;			/* prefix, e.g., "real-" */
    char *pwfile;			/* name of password file */
};
