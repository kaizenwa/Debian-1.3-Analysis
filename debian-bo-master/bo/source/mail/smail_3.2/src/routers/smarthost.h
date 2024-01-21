/* @(#) smarthost.h,v 1.3 1992/07/11 11:50:48 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smarthost.h:
 *	interface file for smarthost driver.
 */

/* private information stored per router file entry */
struct smarthost_private {
    char *path;				/* path from the localhost */
    char *host;				/* next_host to get to smarthost */
    char *route;			/* route from next_host to smarthost */
};
