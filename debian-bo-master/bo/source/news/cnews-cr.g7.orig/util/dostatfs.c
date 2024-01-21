/*
 * dostatfs - the heart of spacefor.statfs
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>

/*
 * Unfortunately, the whereabouts of the necessary struct, the exact
 * calling convention for statfs(), and the name of the "units in which
 * free space is expressed" member, are rather system-specific.  Here's
 * a few attempts...
 */

/* First, pick up a few popular headers on general principles. */
#include <sys/param.h>
#include <sys/mount.h>

/* Second, assorted variations... */
#ifdef linux
#define	sun	1	/* Linux happens to be the same as Sun for this... */
#else
#ifdef __linux__
#define	sun	1	/* a Linux by any other name... */
#endif
#endif
#ifdef hpux
#define	sun	1	/* likewise HP */
#endif
#ifdef __FreeBSD__
#define	BSD4_4	1	/* and FreeBSD is sort of 4.4 */
#endif

#ifdef sun
#include <sys/vfs.h>
#define	UNIT	f_bsize
#endif

#ifdef _AIX
#include <sys/statfs.h>
#define	UNIT	f_fsize
#endif

#ifdef M_XENIX		/* SCO */
#include <sys/statfs.h>
#define	STATFS(fs, result)	statfs(fs, &result, (int)sizeof(result), 0)
#define	UNIT	f_fsize
#define	f_bavail	f_bfree	/* talk about kludges */
#endif

#ifdef BSD4_4
#define	UNIT	f_bsize
#endif

/* Finally, some defaults to simplify the above. */
#ifndef UNIT
#define	UNIT	f_fsize
#endif
#ifndef STATFS
#define	STATFS(fs, result)	statfs(fs, &result)
#endif


extern int debug;
extern void error();

/*
 - spacefor - do the work
 */
long
spacefor(filesize, fileonfs, wantspace, wantinodes, bperi)
long filesize;
char *fileonfs;
long wantspace;
long wantinodes;
long bperi;
{
	struct statfs info;
	register long n;
#	define	LOTS	10000
	register long iperfile = filesize/bperi + 1;

	if (STATFS(fileonfs, info) < 0)
		error("cannot do statfs(%s)", fileonfs);
	if (debug)
		fprintf(stderr, "bsize %ld, avail %ld, inodes %ld\n",
				info.UNIT, info.f_bavail, info.f_ffree);

	n = LOTS;
	if (info.f_bavail <= wantspace)
		n = 0;
	else if (info.UNIT > 0 && filesize > 0)
		n = (info.f_bavail - wantspace) / (filesize/info.UNIT + 1);

	if (info.f_ffree < 0)		/* information unavailable */
		;			/* bypass check, and pray */
	else if (info.f_ffree <= wantinodes)
		n = 0;
	else if ((info.f_ffree - wantinodes) / iperfile < n)
		n = (info.f_ffree - wantinodes) / iperfile;

	if (n < 0)
		n = 0;
	else if (n > LOTS)
		n = LOTS;	/* to avert 16-bit trouble elsewhere */

	return(n);
}
