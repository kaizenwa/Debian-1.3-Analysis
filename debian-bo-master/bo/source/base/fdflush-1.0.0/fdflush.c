/*
 * Floppy-disk buffer flush program.
 * Copyright (C) 1995 Bruce Perens
 * This program is free software under the GNU General Public License
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/fd.h>

static const char	fd0[] = "/dev/fd0";

int
main(int argc, char * * argv)
{
	const char *	floppy = fd0;
	int		fd;

	if ( argc > 1 )
		floppy = argv[1];

	fd = open(floppy, O_RDONLY);

	if ( fd < 0 ) {
		fprintf(stderr, "%s: %s.\n", floppy, strerror(errno));
		return -1;
	}

	if ( ioctl(fd, FDFLUSH, 0) != 0 ) {
		fprintf(stderr, "%s: fdflush: %s.\n", floppy, strerror(errno));
		return -1;
	}
	return 0;
}
