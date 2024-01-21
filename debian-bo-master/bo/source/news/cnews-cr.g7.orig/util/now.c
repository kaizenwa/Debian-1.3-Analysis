/*
 * now - get current time, or variations thereon
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "libc.h"

/*
 - main - do it all
 */
main(argc, argv)
int argc;
char *argv[];
{
	register time_t now;
	register char c;

	now = time((time_t *)NULL);
	if (argc == 2 && ((c = *argv[1]) == '-' || c == '+') &&
					argv[1][strlen(argv[1])-1] == 'd')
		now += atol(argv[1]) * 86400L;
	else if (argc != 1) {
		fprintf(stderr, "Usage: now [{+-}nnnd]\n");
		exit(2);
	}
	printf("%ld\n", (long)now);
	exit(0);
}
