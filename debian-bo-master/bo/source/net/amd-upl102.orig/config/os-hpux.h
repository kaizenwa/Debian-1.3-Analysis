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
 * $Id: os-hpux.h,v 5.2.2.1 1992/02/09 15:10:23 jsp beta $
 *
 * HP/9000 HP-UX definitions for Amd (automounter)
 */

#include <unistd.h>

/*
 * Does the compiler grok void *
 */
#ifdef __GNUC__
#define	VOIDP
#endif

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#ifdef HPUX_9
#define	RPC_4
#else
#define	RPC_3
#endif /* HPUX_9 */

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#ifdef HPUX_9
#define	NFS_4
#else
#define	NFS_3
#endif /* HPUX_9 */

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#if defined(hp9000s200) || defined(hp9000s300) || defined(hp9000s800)
#define	ARCH_ENDIAN	"big"
#endif

#ifndef __hpux
#define	HPUX_VERSION_6
#endif

/*
 * No support for syslog() prior to 7.0
 */
#ifdef HPUX_VERSION_6
#undef HAS_SYSLOG
#endif

/*
 * No support for ndbm, unless HPUX 9.0
 */
#ifndef HPUX_9
#undef OS_HAS_NDBM
#endif /* !HPUX_9 */

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_UFS		MOUNT_UFS
#define MOUNT_TYPE_NFS		MOUNT_NFS

#if defined(CS_MANPATH) || defined(HPUX_9)
# define HAS_CDFS
# define MTAB_TYPE_CDFS		MNTTYPE_CDFS
# define MOUNT_TYPE_CDFS	MOUNT_CDFS

# define HAS_PCFS
# define MNTTYPE_PC		"pcfs"
# define MTAB_TYPE_PCFS		MNTTYPE_PC
# define MOUNT_TYPE_PCFS	MOUNT_PC
#endif

#undef MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	"hfs"

/*
 * Where to get NFS definitions
 */
#define	NFS_HDR "misc-hpux.h"

/*
 * Where to get union wait
 */
#ifdef HPUX_VERSION_6
#undef WAIT
#define	WAIT	"uwait.h"
#define WNOHANG         1       /* dont hang in wait */
#endif /* HPUX_VERSION_6 */
#define	_BSD
#ifdef HPUX_VERSION_6
#define SIGCHLD	SIGCLD
#endif
#define	SYS5_SIGNALS

/*
 * Miscellaneous HP-UX definitions
 */

#define NEED_XDR_POINTER
#define	NEED_CLNT_SPERRNO

/*
 * Use <fcntl.h> rather than <sys/file.h>
 */
#define USE_FCNTL

/*
 * Use fcntl() rather than flock()
 */
#define LOCK_FCNTL

/*
 * Additional fields in struct mntent
 * are fixed up here
 */
#ifdef	_CS_MANPATH	/* to distinguish between HPUX 7.0 and 8.0 */
#define FIXUP_MNTENT(mntp) { \
	(mntp)->mnt_time = clocktime(); \
	(mntp)->mnt_cnode = 0; \
}
#define FIXUP_MNTENT_DUP(mntp, mp) { \
	(mntp)->mnt_time = (mp)->mnt_time; \
	(mntp)->mnt_cnode = (mp)->mnt_cnode; \
}
#else	/* _CS_MANPATH */
#define FIXUP_MNTENT(mntp) { \
	(mntp)->mnt_time = clocktime(); \
}
#define FIXUP_MNTENT_DUP(mntp, mp) { \
	(mntp)->mnt_time = (mp)->mnt_time; \
}
#endif	/* _CS_MANPATH */

#define	bzero(ptr, len)		memset(ptr, 0, len)
#define bcmp(a, b, len)		memcmp((a), (b), (len))
#ifdef HPUX_VERSION_6
#define bcopy(from, to, len)	memcpy(to, from, len)
#else
#define bcopy(from, to, len)	memmove(to, from, len)
#endif /* HPUX_VERSION_6 */

#define getpagesize() (2048)

#undef MOUNT_TRAP
#define MOUNT_TRAP(type, mnt, flags, mnt_data) \
	vfsmount(type, mnt->mnt_dir, flags, mnt_data)
#undef UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)
#define NFDS	30	/* conservative */
#define	MOUNTED MNT_MNTTAB

#if !defined(pid_t)
#define pid_t int
#endif

/*
 * HPUX 9.x wants "ignore" for type of amd mount.  Useful when using
 * bdf instead of df.
 */
#ifdef HPUX_9
#undef MNTTYPE_AUTO
#define MNTTYPE_AUTO "ignore"
#endif /* HPUX_9 */

/*
 * No AUTH_DES for hpux-9
 */
#ifdef HPUX_9
#undef AUTH_DES
#endif /* HPUX_9 */

/*
 * HP-UX 6.x dies not define uid_t
 */
#ifdef HPUX_VERSION_6
typedef int uid_t;
#endif /* HPUX_VERSION_6 */
