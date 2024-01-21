/* @(#) akcs.h,v 1.2 1990/10/24 05:17:12 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * user.h:
 *	interface file for driver code in user.c
 */

/* private information from director file entry */
struct akcs_private {
    char *transport;			/* name of the transport */
    char *prefix;			/* prefix, e.g., "real-" */
};
