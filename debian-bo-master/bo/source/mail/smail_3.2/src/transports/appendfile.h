/*
#ident	"@(#)smail/src/transports:RELEASE-3_2:appendfile.h,v 1.6 1996/02/26 18:40:44 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * appendfile.h:
 *	interface file for transport driver file.c.
 */

/* structure for pipe driver's private data */
struct appendfile_private {
    char *file;				/* form for the filename */
    char *dir;				/* directory for queueing */
    char *user;				/* run as this user */
    char *group;			/* run as this group */
    char *prefix;			/* string prefixed to message */
    char *suffix;			/* string appended to message */
    int mode;				/* mode for creation */
};

/* transport flags private to pipe.c */
#define APPEND_AS_USER	    0x00010000	/* use uid/gid from addr structure */
#define APPEND_EXPAND_USER  0x00020000	/* expand username before file name */
#define APPEND_CHECK_USER   0x00040000	/* make sure $user is safe */
#define APPEND_COMSAT       0x00080000	/* notify in.comsat of new mail */
