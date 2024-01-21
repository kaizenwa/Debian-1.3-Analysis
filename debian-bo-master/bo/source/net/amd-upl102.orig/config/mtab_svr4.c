/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	%W% (Berkeley) %G%
 *
 * $Id: mtab_svr4.c,v 5.2.2.2 1992/07/18 18:57:03 jsp Exp jsp $
 *
 * How to manage the mount table file.  Based on other SVR3 ports.
 *	-Erez Zadok <ezk@cs.columbia.edu>
 */

#include "am.h"

#ifdef READ_MTAB_SVR4_STYLE

#include <sys/mnttab.h>
#include <sys/mntent.h>

/* fd for /etc/.mnt.lock */
/* also act as flag for: is_locked */
static int mtlckf = 0;
#ifdef MTAB_LOCK_FILE
static char mtlckname[] = MTAB_LOCK_FILE;
#else
static char mtlckname[] = "/etc/.mnt.lock";
#endif
static char mnttabname[] = MNTTAB;

static void
unlockmnttab()
{
	if (mtlckf) {
		close(mtlckf);
		mtlckf = 0;
	}
}

static
lockfile(fd, type)
int fd, type;
{
	struct flock lk;

	lk.l_type = type;
	lk.l_whence = 0;
	lk.l_start = 0;
	lk.l_len = 0;

	return fcntl(fd, F_SETLKW, &lk);
}

static
lockmnttab()
{
	if (mtlckf == 0) {	/* need lock on /etc/.mnt.lock */
		mtlckf = open(mtlckname, O_RDWR|O_CREAT, 0600);
		if (mtlckf >= 0) {
			if (lockfile(mtlckf, F_WRLCK) < 0) {
				close(mtlckf);
				mtlckf = 0;
#ifdef DEBUG
				dlog("lock lock failed %m");
#endif
			} else {
				return 0;
			}
		} else {
			plog(XLOG_ERROR, "Unable to open/creat %s: %m", mtlckname);
		}
	}

	plog(XLOG_ERROR, "Unable to lock %s: %m", mtlckname);
	return -1;
}

void
unlock_mntlist()
{
	unlockmnttab();
}


/* convert from solaris mnttab to Amd mntent */
static struct mntent *
mnt_dup(mtp)
struct mnttab *mtp;
{
	struct mntent *mep = ALLOC(mntent);

	mep->mnt_fsname = strdup(mtp->mnt_special);
	mep->mnt_dir = strdup(mtp->mnt_mountp);
	mep->mnt_type = strdup(mtp->mnt_fstype);
	mep->mnt_opts = strdup(mtp->mnt_mntopts);
	mep->mnt_freq = mep->mnt_passno = 0;

	return mep;
}


/*
 * Convert back (static alloc) from an mntent to an mnttab.
 */
static struct mnttab *mtab_of(mnt)
struct mntent *mnt;
{
static struct mnttab mt;

	struct timeval tv;
	char timestr[16];

	mt.mnt_special = strdup(mnt->mnt_fsname);
	mt.mnt_mountp = strdup(mnt->mnt_dir);
	mt.mnt_fstype = strdup(mnt->mnt_type);
	mt.mnt_mntopts = strdup(mnt->mnt_opts);

	if (gettimeofday(&tv) < 0)
		timestr[0] = '\0';
	else
		sprintf(&timestr[0], "%ld", tv.tv_sec);

	mt.mnt_time = strdup(timestr);

	return &mt;
}


/*
 * Read a mount table into memory
 */
mntlist *read_mtab(fs)
char *fs;
{
	/*
	 * Based on: Piete Brooks <pb@cl.cam.ac.uk>
	 */

	mntlist **mpp, *mhp;
	FILE *fp;
	struct mnttab mountbuf;
	int ret;

	if (lockmnttab() != 0)
		return (mntlist *)0;

	fp = fopen(mnttabname, "r");
	if (fp == (FILE *) NULL) {
		plog(XLOG_ERROR, "Can't open %s: %m", mnttabname);
		return (mntlist *)0;
	}

	mpp = &mhp;
	while ((ret = getmntent(fp, &mountbuf)) == 0) {
		/*
		 * Allocate a new slot
		 */
		*mpp = ALLOC(mntlist);

		/*
		 * Copy the data returned by getmntent
		 */
		(*mpp)->mnt = mnt_dup(&mountbuf);

		/*
		 * Move to next pointer
		 */
		mpp = &(*mpp)->mnext;
	}

	if (ret > 0) {
		plog(XLOG_ERROR, "read error on %s: %m", mnttabname);
		unlockmnttab();
		mhp =  (mntlist *)0;
	}
	*mpp = 0;
	
	fclose(fp);
	return mhp;
}


static int
write_mntent_to_mtab(fp, mnt)
FILE  *fp;
struct mntent *mnt;
{
	putmntent(fp, mtab_of(mnt));
	return 0;
}


void
rewrite_mtab(mp)
mntlist	*mp;
{
	FILE *fp;

	assert(mtlckf != 0);

	fp = fopen(mnttabname, "r+");
	if (fp == (FILE *) NULL) {
		plog(XLOG_ERROR, "Can't open %s: %m", mnttabname);
		unlockmnttab();
	}
		
	while(mp) {
		if (mp->mnt)
			write_mntent_to_mtab(fp, mp->mnt);
		mp = mp->mnext;
	}

	ftruncate(fileno(fp),ftell(fp));
	fclose(fp);
	unlockmnttab();
}


void
write_mntent(mp)
struct mntent *mp;
{
	FILE *fp;

	if (lockmnttab() == -1)
		return;

	fp = fopen(mnttabname, "a");
	if (fp == (FILE *) NULL) {
		plog(XLOG_ERROR, "Unable to append %s: %m", mnttabname);
		return;
	}

	write_mntent_to_mtab(fp, mp);

	fclose(fp);
	unlockmnttab();
}

#endif /* READ_MTAB_SVR4_STYLE */
