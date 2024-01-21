/*
 * simulate 4.4BSD fgetline using the more general fgetfln primitives
 */

#include <stdio.h>
#include <sys/types.h>
#include <fgetfln.h>

char *
fgetline(fp, lenp)
FILE *fp;
register size_t *lenp;
{
	int len;
	register char *line = fgetfln(fp, -1, &len);

	if (line != NULL)
		(void) dogets(line, &len);	/* stomp innocent newline */
	if (lenp != NULL)
		*lenp = len;
	return line;
}
