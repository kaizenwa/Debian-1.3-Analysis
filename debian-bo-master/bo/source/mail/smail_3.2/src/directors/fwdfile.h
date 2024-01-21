/* @(#) fwdfile.h,v 1.4 1995/03/01 13:47:31 nm4 Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 *
 * namei master id: @(#) fwdfile.h,v 1.4 1995/03/01 13:47:31 nm4 Exp
 */

/*
 * forward.h:
 *	interface file for forward file driver.
 */

/* boolean attributes for director.flags field */
#define FWD_CHECKOWNER	0x00010000
#define FWD_FORWARDTO	0x00020000
#define FWD_LOCKFWDFILE	0x00040000

/* private information from director file entry */
struct forwardfile_private {
    char *file;				/* template for forward file */
    int modemask;			/* allowed mode bits ala umask(2) */
    char *caution;			/* users and directories not secure */
    char *unsecure;			/* unsecure users and directories */
    char *owners;			/* ownership restrictions */
    char *owngroups;			/* group ownership restrictions */
    char *prefix;			/* prefix required for a match */
    char *suffix;			/* suffix required for a match */
};
