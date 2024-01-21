/*
 * fopen and set close-on-exec (to avoid leaking descriptors into children)
 */

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include "news.h"

FILE *
fopenwclex(name, mode)	/* open name; close-on-exec if OK, else warning */
char *name, *mode;
{
	register FILE *fp;

	if ((fp = fopenclex(name, mode)) == NULL)
		warning("can't open `%s'", name);	/* persistent? */
	return fp;
}

FILE *
fopenclex(file, mode)		/* open file and if OK, close-on-exec */
char *file, *mode;
{
	register FILE *fp;

	if ((fp = fopen(file, mode)) != NULL)
		(void) fcntl(fileno(fp), F_SETFD, 1);
	return fp;
}
