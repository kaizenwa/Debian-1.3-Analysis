#include <sys/types.h>
#include <sys/param.h>

/* if NOFILE wasn't defined in sys/param.h, try for sysconf()... */
#ifndef NOFILE
#include <unistd.h>
#define	NOFILE	((int)sysconf(_SC_OPEN_MAX))
#endif

void
closeall(leavestd)
int leavestd;
{
	register int i;
	register int openmax = NOFILE;

	for (i = (leavestd? 3: 0); i < openmax; i++)
		close(i);
}
