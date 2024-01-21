/*
 * Get UTMP file entries for BSD compatibility
 * by Kevin Rosenberg <kevin@cyberport.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "config.h"

#ifdef	BSD
#include <stdlib.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <time.h>
#include <utmp.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "getutent.h"

char *getdomainname(char *dname, size_t maxsize)
{
    struct utsname st_uname;
    char *p;
    int len;

    if (uname (&st_uname) < 0) {
	fprintf( stderr, "Error calling uname\n");
	exit(1);
	fprintf( stderr, "Error calling uname\n");
	exit(1);
    }

/*  printf ("node name: %s\n", st_uname.nodename); */

    dname[0] = EOF;
    for (p = st_uname.nodename; *p; ++p)
	if (*p == '.') {
	    ++p;
	    for (len = 0; *p && len < maxsize-1; len++)
		dname[len] = *p++;

	    dname[len] = 0;
	    break;
	}
/*  printf ("domainname: %s\n", dname); */
    return dname;
}

static int utmp_fd = -1;

void setutent()
{
    if (utmp_fd < 0)
	if ((utmp_fd = open(_PATH_UTMP, O_RDONLY)) < 0) {
	    fprintf(stderr, "Can't open /var/run/utmp\n");
	    exit(1);
	}

    lseek(utmp_fd, (off_t) 0, SEEK_SET);
}

void endutent()
{
    if (utmp_fd > 0)
	close(utmp_fd);

    utmp_fd = -1;
}

struct utmp *getutent()  /* returns next utmp file entry   */
{
    static struct utmp s_utmp;
    int readval;

    if (utmp_fd < 0)
	setutent();

    if ((readval = read(utmp_fd, &s_utmp, sizeof(s_utmp))) < sizeof(s_utmp))
	if (readval == 0)
	    return NULL;
	else if (readval < 0) {
	    fprintf(stderr, "Error reading %s\n", _PATH_UTMP);
	    exit(1);
	} else {
	    fprintf(stderr, "Partial record in %s [%d bytes]\n", _PATH_UTMP, readval );
	    exit(1);
	}

    return &s_utmp;
}
#endif
 
