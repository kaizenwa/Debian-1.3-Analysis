/*
 * symlink dummy
 */

#include <errno.h>
#include "fixerrno.h"

int
symlink(n1, n2)
char *n1, *n2;
{
	errno = 0;		/* kludge */
	return -1;
}

int
readlink(path, buf, size)
char *path, *buf;
int size;
{
	return -1;
}
