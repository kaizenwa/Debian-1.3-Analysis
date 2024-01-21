/* @(#) util.c,v 1.1 1992/05/12 11:03:56 tron Exp */
#include "defs.h"
#ifdef SDBM
#include "sdbm.h"
#else
#include <ndbm.h>
#endif
#include <stdio.h>

#ifdef ANSI_C
#include <string.h>
#include <errno.h>
#else
extern int errno, sys_nerr;
extern char *sys_errlist[];
#endif

extern char *progname;

void
oops(s1, s2)
register char *s1;
register char *s2;
{
	int e = errno;
	char *emsg = NULL;

	if (progname)
		fprintf(stderr, "%s: ", progname);
	fprintf(stderr, s1, s2);
#ifdef ANSI_C
	emsg = strerror(e);
#else
	if (e > 0 && e < sys_nerr)
		emsg = sys_errlist[e];
#endif
	if (emsg)
		fprintf(stderr, " (%s)", emsg);
	fprintf(stderr, "\n");
	exit(1);
}

int
okpage(pag)
char *pag;
{
	register unsigned n;
	register off;
	register short *ino = (short *) pag;

	if ((n = ino[0]) > PBLKSIZ / sizeof(short))
		return 0;

	if (!n)
		return 1;

	off = PBLKSIZ;
	for (ino++; n; ino += 2) {
		if (ino[0] > off || ino[1] > off ||
		    ino[1] > ino[0])
			return 0;
		off = ino[1];
		n -= 2;
	}

	return 1;
}
