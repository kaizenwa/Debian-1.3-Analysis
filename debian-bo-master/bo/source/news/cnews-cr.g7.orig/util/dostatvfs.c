/*
 * dostatvfs - the heart of spacefor.statvfs
 *
 * Our thanks to Paul Eggert for this code.
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/statvfs.h>

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
	struct statvfs info;
	register long n;
#	define	LOTS	10000
	register long iperfile = filesize/bperi + 1;

	if (statvfs(fileonfs, &info) < 0)
		error("cannot do statvfs(%s)", fileonfs);
	if (debug)
		fprintf(stderr, "frsize %lu, avail %lu, inodes %lu\n",
				info.f_frsize, info.f_bavail, info.f_favail);

	n = LOTS;
	if (info.f_bavail == -1 || info.f_bavail <= wantspace)
		n = 0;
	else if (info.f_frsize != -1 && filesize > 0)
		n = (info.f_bavail - wantspace) / (filesize/info.f_frsize + 1);

	if (info.f_favail == -1)	/* information unavailable */
		;			/* bypass check, and pray */
	else if (info.f_favail <= wantinodes)
		n = 0;
	else if ((info.f_favail - wantinodes) / iperfile < n)
		n = (info.f_favail - wantinodes) / iperfile;

	if (n < 0)
		n = 0;
	else if (n > LOTS)
		n = LOTS;	/* to avert 16-bit trouble elsewhere */

	return(n);
}
