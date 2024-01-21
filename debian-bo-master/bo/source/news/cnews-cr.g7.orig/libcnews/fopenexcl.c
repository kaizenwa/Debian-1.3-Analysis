/*
 * fopenexcl(name) - fopen(name, "w+") with error if name exists
 */

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#ifndef O_RDONLY		/* using our fake fcntl.h; maybe it's 4.2? */
#include <sys/file.h>
#endif

#ifdef O_RDONLY			/* good, we have a 3-arg open() */
#define	OPENRW(f)	open(f, O_RDWR|O_CREAT|O_EXCL, 0666)
#else				/* don't -- must fake it */
#include <sys/stat.h>
static struct stat testbuf;
#define	OPENRW(f)	((stat(f, &testbuf) < 0) ? \
				((void)close(creat(f, 0666)), open(f, 2)) : \
				-1)
#endif

FILE *
fopenexcl(name)
register char *name;
{
	register int fd;

	fd = OPENRW(name);
	if (fd < 0)
		return NULL;		/* name existed or couldn't be made */
	else
		return fdopen(fd, "w+");
}
