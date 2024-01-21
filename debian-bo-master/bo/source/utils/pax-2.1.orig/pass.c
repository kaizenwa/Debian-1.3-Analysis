/* $Source: /usr/local/src/pax/RCS/pass.c,v $
 *
 * $Revision: 2.2 $
 *
 * pass.c - handle the pass option of cpio
 *
 * DESCRIPTION
 *
 *	These functions implement the pass options in PAX.  The pass option
 *	copies files from one directory hierarchy to another.
 *
 * AUTHOR
 *
 *	Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
 *
 * COPYRIGHT
 *
 *	Copyright (c) 1989 Mark H. Colburn.  All rights reserved.
 *
 *	Redistribution and use in source and binary forms are permitted
 *	provided that the above copyright notice and this paragraph are
 *	duplicated in all such forms and that any documentation,
 *	advertising materials, and other materials related to such
 *	distribution and use acknowledge that the software was developed
 *	by Mark H. Colburn.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Log: pass.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: pass.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif /* ! lint */


/* Headers */

#include "pax.h"
#if defined (HAVE_UTIME_H)
#  include <utime.h>
#elif defined (HAVE_SYS_UTIME_H)
#  include <sys/utime.h>
#else
struct utimbuf {
    time_t	actime;
    time_t	modtime;
};
#endif


/* pass - copy within the filesystem
 *
 * DESCRIPTION
 *
 *	Pass copies the named files from the current directory hierarchy to
 *	the directory pointed to by dirname.
 *
 * PARAMETERS
 *
 *	char	*dirname	- name of directory to copy named files to.
 *
 */

#ifdef __STDC__
void		pass (char *dirname)
#else
void		pass (dirname)
    char	*dirname;
#endif
{
    char                name[PATH_MAX + 1];
    int                 fd;
    Stat                sb;

    while (name_next(name, &sb) >= 0 && (fd = openin(name, &sb)) >= 0) {

	if (rplhead != (Replstr *) NULL) {
	    rpl_name(name);
	}
	if (get_disposition("pass", name) || get_newname(name, sizeof(name))) {
	    /* skip file... */
	    if (fd) {
		/* FIXME: do error checking here */
		close(fd);
	    }
	    continue;
	}
	if (passitem(name, &sb, fd, dirname)) {
	    /* FIXME: do error checking here */
	    close(fd);
	}
	if (f_verbose) {
	    fprintf(stderr, "%s/%s\n", dirname, name);
	}
    }
}


/* passitem - copy one file
 *
 * DESCRIPTION
 *
 *	Passitem copies a specific file to the named directory
 *
 * PARAMETERS
 *
 *	char   *from	- the name of the file to open
 *	Stat   *asb	- the stat block associated with the file to copy
 *	int	ifd	- the input file descriptor for the file to copy
 *	char   *dir	- the directory to copy it to
 *
 * RETURNS
 *
 * 	Returns given input file descriptor or -1 if an error occurs.
 *
 * ERRORS
 */

#ifdef __STDC__
int		passitem (char *from, Stat * asb, int ifd, char *dir)
#else
int		passitem (from, asb, ifd, dir)
    char	*from;
    Stat	*asb;
    int		 ifd;
    char	*dir;
#endif
{
    int                 ofd;
    char                to[PATH_MAX + 1];
    struct utimbuf	tstamp;

    strcpy(to, dir);
    strcat(to, "/");
    strcat(to, from);

    if (nameopt(to) < 0) {
	return (-1);
    }
    if (asb->sb_nlink > 1) {
	linkto(to, asb);
    }
    if (f_link && islink(from, asb) == (Link *) NULL) {
	linkto(from, asb);
    }
    if ((ofd = openout(to, asb, islink(to, asb), 1)) < 0) {
	return (-1);
    }
    if (ofd > 0) {
	passdata(from, ifd, to, ofd);
    }

    tstamp.actime = asb->sb_atime;
    tstamp.modtime = f_mtime ? asb->sb_mtime : time((time_t *) 0);
    utime(to, &tstamp);
    return (ifd);
}
