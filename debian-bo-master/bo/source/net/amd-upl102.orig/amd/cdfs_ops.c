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
 * $Id: cdfs_ops.c,v 5.2.2.1 1992/02/09 15:09:08 jsp beta $
 *
 */

#include "am.h"

#ifdef HAS_CDFS

#ifndef CDFS
#define CDFS
#endif /* !CDFS */

#include <sys/stat.h>
#include <sys/mount.h>

/*
 * High Sierra (CD-ROM) file system
 */

/*
 * CDFS needs remote filesystem.
 */
static char *cdfs_match P((am_opts *fo));
static char *cdfs_match(fo)
am_opts *fo;
{
	if (!fo->opt_dev) {
		plog(XLOG_USER, "cdfs: no source device specified");
		return 0;
	}

#ifdef DEBUG
	dlog("CDFS: mounting device \"%s\" on \"%s\"",
		fo->opt_dev, fo->opt_fs);
#endif /* DEBUG */

	/*
	 * Determine magic cookie to put in mtab
	 */
	return strdup(fo->opt_dev);
}

static int mount_cdfs(dir, fs_name, opts)
char *dir;
char *fs_name;
char *opts;
{
	struct cdfs_args cdfs_args;
	struct mntent mnt;
	int flags;

	/*
	 * Figure out the name of the file system type.
	 */
#ifdef M_NEWTYPE
	char *type = MOUNT_TYPE_CDFS;
#else
	int type = MOUNT_TYPE_CDFS;
#endif /* M_NEWTYPE */

	bzero((voidp) &cdfs_args, sizeof(cdfs_args));	/* Paranoid */

	/*
	 * Fill in the mount structure
	 */
	mnt.mnt_dir = dir;
	mnt.mnt_fsname = fs_name;
#ifdef MTAB_TYPE_CDFS
	mnt.mnt_type = MTAB_TYPE_CDFS;
#else
	mnt.mnt_type = "cdfs";
#endif /* MTAB_TYPE_CDFS */
	mnt.mnt_opts = opts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;

	flags = compute_mount_flags(&mnt);

	cdfs_args.fspec = fs_name;
#ifdef ROCK_RIDGE_EXTENSIONS
				/* XXX: need to provide norrip mount opt */
	cdfs_args.norrip = 0;	/* use Rock-Ridge Protocol extensions */
#endif /* ROCK_RIDGE_EXTENSIONS */

	/*
	 * Call generic mount routine
	 */
	return mount_fs(&mnt, flags, (caddr_t) &cdfs_args, 0, type);
}

/*ARGSUSED*/
static int cdfs_fmount(mf)
mntfs *mf;
{
	int error;

	error = mount_cdfs(mf->mf_mount, mf->mf_info, mf->mf_mopts);
	if (error) {
		errno = error;
		plog(XLOG_ERROR, "mount_cdfs: %m");
		return error;
	}

	return 0;
}

static int cdfs_fumount(mf)
mntfs *mf;
{
	return UMOUNT_FS(mf->mf_mount);
}

/*
 * Ops structure
 */
am_ops cdfs_ops = {
#ifdef MTAB_TYPE_CDFS
	MTAB_TYPE_CDFS,
#else
	"cdfs",
#endif /* MTAB_TYPE_CDFS */
	cdfs_match,
	0, /* cdfs_init */
	auto_fmount,
	cdfs_fmount,
	auto_fumount,
	cdfs_fumount,
	efs_lookuppn,
	efs_readdir,
	0, /* cdfs_readlink */
	0, /* cdfs_mounted */
	0, /* cdfs_umounted */
	find_afs_srvr,
	FS_MKMNT|FS_NOTIMEOUT|FS_UBACKGROUND|FS_AMQINFO
};

#endif /* HAS_CDFS */
