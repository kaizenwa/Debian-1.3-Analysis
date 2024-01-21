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
 * $Id: pcfs_ops.c,v 5.2.2.1 1992/02/09 15:09:08 jsp beta $
 *
 */

#include "am.h"

#ifdef HAS_PCFS

#ifndef PCFS
#define PCFS
#endif /* !PCFS */

#include <sys/stat.h>
#include <sys/mount.h>

/*
 * PC (MS-DOS) file system
 */

/*
 * PCFS needs remote filesystem.
 */
static char *pcfs_match P((am_opts *fo));
static char *pcfs_match(fo)
am_opts *fo;
{
	if (!fo->opt_dev) {
		plog(XLOG_USER, "pcfs: no source device specified");
		return 0;
	}

#ifdef DEBUG
	dlog("PCFS: mounting device \"%s\" on \"%s\"",
		fo->opt_dev, fo->opt_fs);
#endif /* DEBUG */

	/*
	 * Determine magic cookie to put in mtab
	 */
	return strdup(fo->opt_dev);
}

static int mount_pcfs(dir, fs_name, opts)
char *dir;
char *fs_name;
char *opts;
{
	struct pc_args pcfs_args;
	struct mntent mnt;
	int flags;

	/*
	 * Figure out the name of the file system type.
	 */
#ifdef M_NEWTYPE
	char *type = MOUNT_TYPE_PCFS;
#else
	int type = MOUNT_TYPE_PCFS;
#endif /* M_NEWTYPE */

	bzero((voidp) &pcfs_args, sizeof(pcfs_args));	/* Paranoid */

	/*
	 * Fill in the mount structure
	 */
	mnt.mnt_dir = dir;
	mnt.mnt_fsname = fs_name;
	mnt.mnt_type = MTAB_TYPE_PCFS;
	mnt.mnt_opts = opts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;

	flags = compute_mount_flags(&mnt);

	pcfs_args.fspec = fs_name;
#ifdef __svr4__
	pcfs_args.unknown = (char *) NULL;
#endif /* __svr4__ */

	/*
	 * Call generic mount routine
	 */
	return mount_fs(&mnt, flags, (caddr_t) &pcfs_args, 0, type);
}

/*ARGSUSED*/
static int pcfs_fmount(mf)
mntfs *mf;
{
	int error;

	error = mount_pcfs(mf->mf_mount, mf->mf_info, mf->mf_mopts);
	if (error) {
		errno = error;
		plog(XLOG_ERROR, "mount_pcfs: %m");
		return error;
	}

	return 0;
}

static int pcfs_fumount(mf)
mntfs *mf;
{
	return UMOUNT_FS(mf->mf_mount);
}

/*
 * Ops structure
 */
am_ops pcfs_ops = {
	"pcfs",
	pcfs_match,
	0, /* pcfs_init */
	auto_fmount,
	pcfs_fmount,
	auto_fumount,
	pcfs_fumount,
	efs_lookuppn,
	efs_readdir,
	0, /* pcfs_readlink */
	0, /* pcfs_mounted */
	0, /* pcfs_umounted */
	find_afs_srvr,
	FS_MKMNT|FS_NOTIMEOUT|FS_UBACKGROUND|FS_AMQINFO
};

#endif /* HAS_PCFS */
