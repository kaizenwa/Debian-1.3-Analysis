/*
 * misc.c - miscellaneous functions for the Linux file system degragmenter.
 * $Id: misc.c,v 1.1 1994/05/22 22:56:56 linux Exp $
 *
 * Copyright (C) 1992, 1993 Stephen Tweedie (sct@dcs.ed.ac.uk)
 * 
 * This file may be redistributed under the terms of the GNU General
 * Public License.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <mntent.h>

#define MISC_C
#include "defrag.h"

void fatal_error (const char * fmt_string)
{
	if (voyer_mode) { 
             stat_line("");             
             done_screen(FALSE); /* Maybe screen is not initalised yet, */
                                 /* done_screen will check it itself    */
        }
        fflush (stdout);
	fprintf (stderr, fmt_string, program_name, device_name);
	exit (1);
}

void usage()
{
	fflush (stdout);
	fprintf (stderr, 
		 "Usage: %s [-V"
#ifndef NODEBUG
		 "d"
#endif
		 "rsv] [-i inode-list] [-b bad-inode] [-p pool-size] "
		 "/dev/name\n", 
		 program_name);
	fprintf (stderr, "  -V : print full version information\n");
#ifndef NODEBUG
	fprintf (stderr, "  -d : debugging mode\n");
#endif
	fprintf (stderr, "  -r : read_only (testing) mode (implies -s)\n");
	fprintf (stderr, "  -s : show summary information\n");
	fprintf (stderr, "  -v : verbose (-vv is even more so)\n");
	fprintf (stderr, "  -n : runs without a picture\n");
	exit (1);
}

void check_mount(char *device_name)
{
	FILE * f;
	struct mntent * mnt;

	if ((f = setmntent (MOUNTED, "r")) == NULL)
		return;
	while ((mnt = getmntent (f)) != NULL)
		if (strcmp (device_name, mnt->mnt_fsname) == 0)
			break;
	endmntent (f);
	if (!mnt)
		return;

	fprintf (stderr,"Cannot work on a mounted device: %s\n", device_name);
	exit (2);
}

__off_t nlseek (int fd, __off_t offset, int whence) {
   __off_t r;
   while ((r=lseek(fd,offset,whence))==-1 && errno == EINTR)
        ;
   return r;

}
ssize_t nread (int fd, __ptr_t buf, size_t nbytes) {
   ssize_t r;
   while ((r=read(fd,buf,nbytes))==-1 && errno == EINTR)
        ;
   return r;
}

ssize_t nwrite (int fd, __ptr_t buf, size_t nbytes) {
   ssize_t r;
   while ((r=write(fd,buf,nbytes))==-1 && errno == EINTR)
        ;
   return r;
}
