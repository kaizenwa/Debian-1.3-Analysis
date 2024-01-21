/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
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
 * $Id$
 *
 * SunOS 5.0 definitions for Amd (automounter).  Port by Erez Zadok
 * <ezk@cs.columbia.edu> and James Tanis <jtt@cs.columbia.edu>.
 */


/*
 * Use for svr4 code.  GCC seems to define __svr4__ so that's what I'll be
 * using.  Erez Zadok <ezk@cs.columbia.edu>.
 */
#ifndef __svr4__
#define __svr4__
#endif /* __svr4__ */

/*
 * Does the compiler grok void *
 */
#define	VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	RPC_4

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	NFS_3

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#if defined(mc68010) || defined(mc68020) || defined(sparc)
#define	ARCH_ENDIAN	"big"
#endif
#if defined(i386) || defined(i860)
#define ARCH_ENDIAN	"little"
#endif

/*
 * Needed to get correct value of MAXHOSTNAMELEN
 */
#include <netdb.h>

/*
 * For struct nfs_args
 */
#include <sys/types.h>  /* needed for caddr_t in mount.h */
#define NFSCLIENT
#include <nfs/mount.h>
#undef NFSCLIENT


/*
 * No mntent on svr4.  We write our own.
 */
#undef	MNTENT_HDR
#define	MNTENT_HDR <sys/mntent.h>
struct mntent {
	char    *mnt_fsname;    /* name of mounted file system */
	char    *mnt_dir;       /* file system path prefix */
	char    *mnt_type;      /* MNTTYPE_* */
	char    *mnt_opts;      /* MNTOPT* */
	int     mnt_freq;       /* dump frequency, in days */
	int     mnt_passno;     /* pass number on parallel fsck */
/* 	long	mnt_time; */
/* 	short	mnt_ro; */
};
#undef READ_MTAB_FROM_FILE
#define READ_MTAB_SVR4_STYLE
#define NEED_MNTOPT_PARSER


/*
 * Types of mounts (type argument to mount(2) syscall.)  Note that the man
 * page is incorrect by saying that the type argument to mount(2) is an
 * integer.  I confirmed that the type is (char *) by using "truss" and
 * <sys/vfs.h>.  
 */
#include <sys/fstyp.h>
#define MOUNT_TYPE_UFS	sysfs(GETFSIND, "ufs")
#define MOUNT_TYPE_NFS	sysfs(GETFSIND, "nfs")

/*
 * Type of a file handle
 */
#undef NFS_FH_TYPE
#define	NFS_FH_TYPE	caddr_t

/*
 * How to copy an address into an NFS filehandle
 */
#undef NFS_SA_DREF
#define	NFS_SA_DREF(dst, src) { \
		(dst).addr->buf = (char *) (src); \
		(dst).addr->len = sizeof(struct sockaddr_in); \
		(dst).addr->maxlen = sizeof(struct sockaddr_in); \
	}

/*
 * Needed to get definition of svc_getcaller() from <rpc/svc_soc.h> which is
 * in <rpc/svc.h> which is in ../include/am.h
 */
#ifndef __svr4__
#define PORTMAP
#endif /* !__svr4__ */

/*
 * Type of the third argument ("in") to some svc_*args() RPC functions, such
 * as svc_getargs().
 */
#undef SVC_IN_ARGS_TYPE
#define SVC_IN_ARGS_TYPE caddr_t

/*
 * Name of Mount Table File (normally /etc/mtab)
 */
#define MOUNTED MNTTAB

/*
 * Map from MS_* to M_* mount options
 */
#define M_RDONLY MS_RDONLY
#define M_NOSUID MS_NOSUID

/*
 * How to mount(2) and umount(2).
 */
#define MOUNT_HELPER_SOURCE "mount_svr4.c"
#undef MOUNT_TRAP
#define MOUNT_TRAP(type, mnt, flags, mnt_data) \
	mount_svr4(mnt->mnt_fsname, mnt->mnt_dir, flags, type, mnt_data)
#undef UNMOUNT_TRAP
#define UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)

/*
 * To get definition of struct ufs_args.
 */
#define UFS_HDR "misc-svr4.h"

#ifndef SYS5_SIGNALS
#define SYS5_SIGNALS
#endif /* SYS5_SIGNALS */

/*
 * To get certain ioctl()'s working correctly (see <sys/ioctl.h>)
 */
#define BSD_COMP
