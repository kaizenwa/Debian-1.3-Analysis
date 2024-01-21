/* $Source: /usr/local/src/pax/RCS/append.c,v $
 *
 * $Revision: 2.2 $
 *
 * append.c - append to a tape archive.
 *
 * DESCRIPTION
 *
 *	Routines to allow appending of archives
 *
 * AUTHORS
 *
 *     	Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
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
 * $Log: append.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:32:22  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: append.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif /* ! lint */


/* Headers */

#include "pax.h"
#ifdef HAVE_SYS_MTIO_H
#include <sys/mtio.h>
#endif

static off_t	    back_space __P ((off_t));

/* append_archive - main loop for appending to a tar archive
 *
 * DESCRIPTION
 *
 *	Append_archive reads an archive until the end of the archive is
 *	reached once the archive is reached, the buffers are reset and the
 *	create_archive function is called to handle the actual writing of
 *	the appended archive data.  This is quite similar to the
 *	read_archive function, however, it does not do all the processing.
 */

#ifdef __STDC__
void	append_archive (void)
#else
void	append_archive ()
#endif
{
    Stat                sb;
    char                name[PATH_MAX + 1];
    off_t             	blk;
    off_t	       	btotal = 0;
    off_t		skp;

    name[0] = '\0';
    while (get_header(name, &sb) == 0) {
	skp = (ar_format == TAR)
	    ? buf_skip(ROUNDUP((off_t) sb.sb_size, BLOCKSIZE))
		: buf_skip((off_t) sb.sb_size);
	if (buf_skip(skp) < 0) {
	    warn(name, "File data is corrupt");
	}
	/* the tar file header is written out in on BLOCKSIZE block */
	btotal += skp + BLOCKSIZE;
    }
    /* we have now gotten to the end of the archive... */

    /* backspace to the end of the last archive */
    if (ar_format == TAR) {
        blk = (off_t) (bufend - bufstart - (btotal % blocksize)) / BLOCKSIZE;
    	blk = back_space (blk);
    	if (blk < 0) {
    	    warn("can't backspace", "append archive may be no good");
	}
    }

    /* reset the buffer now that we have read the entire archive */
    bufend = bufidx = bufstart;
    create_archive();
}


/*
 * backspace - backspace a certain number of records on a file
 *
 * RETURNS
 *
 *	The number of bytes backed up, or -1 if an error occured.
 *
 * CAVEATS
 *
 *	A tar append without a warning message does not mean sucess, to be
 *	sure, do a tar tv to see if your device can support backspacing.
 */

#ifdef __STDC__
static off_t	back_space (off_t n)
#else
static off_t	back_space (n)
    off_t	n;
#endif
{
    off_t              pos;

#ifdef HAVE_SYS_MTIO_H
    struct mtop         mt;

    mt.mt_op = MTBSR;
    mt.mt_count = n;
    if (ioctl(archivefd, MTIOCTOP, &mt) < 0) {
	if (errno == EIO) {	/* try again */
	    if (ioctl(archivefd, MTIOCTOP, &mt) < 0) {
		warn(strerror (errno),
		     "probably can't backspace on device");
		return (-1);
	    }
	}
    } else {
	return (n);
    }
#endif 
    pos = LSEEK(archivefd, (off_t) - n * BLOCKSIZE, SEEK_CUR);
    if (pos == (off_t) -1) {
	warn(strerror (errno), "probably can't backspace on device");
	return (-1);
    }
    return (n);
}
