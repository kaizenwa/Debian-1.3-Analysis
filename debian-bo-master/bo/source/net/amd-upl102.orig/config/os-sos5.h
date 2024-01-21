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
 * $Id: os-sos5.h,v 5.2.2.1 1992/02/09 15:10:41 jsp beta $
 *
 * SunOS 5.0 definitions for Amd (automounter).  Port by Erez Zadok
 * <ezk@cs.columbia.edu> and James Tanis <jtt@cs.columbia.edu>.
 */

/*
 * Set this macro for further use in amd.  (Solaris is not pure SYSV or BSD,
 * so I may need to special-case based on this macro.)  GCC seems to define
 * __svr4__ so that's what I'll be using.  Erez Zadok <ezk@cs.columbia.edu>.
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
 * Declarations of {set,get,end}netconfig{,ent} functions.
 */
#ifdef __STDC__
#include <sys/netconfig.h>
#else
extern void *setnetconfig();
extern struct netconfig *getnetconfig();
extern struct netconfig *getnetconfigent();
#endif /* __STDC__ */

/*
 * Needed for a lot of the code ported to pure TLI,
 * especially for host name/number mapping.
 */
#ifdef __svr4__
#include <netdir.h>
#endif /* __svr4__ */

/*
 * No mntent on sos5 (Solaris 2.1).  We write our own.
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

/*
 * If you are running on Solaris 2.2 or earlier, define this.
 */
#undef NEED_MNTOPT_PARSER

/*
 * For struct nfs_args
 */
#include <nfs/mount.h>

/*
 * Types of mounts (type argument to mount(2) syscall.)  Note that the man
 * page is incorrect by saying that the type argument to mount(2) is an
 * integer.  I confirmed that the type is (char *) by using "truss" and
 * <sys/vfs.h>.  
 */
#include <sys/fstyp.h>
#define MOUNT_TYPE_UFS	sysfs(GETFSIND, "ufs")
#define MOUNT_TYPE_NFS	sysfs(GETFSIND, "nfs")

#define HAS_LOFS
#define MTAB_TYPE_LOFS	MNTTYPE_LO
#define MOUNT_TYPE_LOFS	sysfs(GETFSIND, "lofs")
struct	lo_args {
	char    *fsdir;
};

#define HAS_CDFS
#define MTAB_TYPE_CDFS	MNTTYPE_HSFS
#define MOUNT_TYPE_CDFS	sysfs(GETFSIND, "hsfs")
struct cdfs_args {	/* arguments passed to mount(2) for cd-rom F/S */
  char *fspec;
  int norrip;
};
#define ROCK_RIDGE_EXTENSIONS

#define HAS_PCFS
#define MTAB_TYPE_PCFS	MNTTYPE_PC
#define MOUNT_TYPE_PCFS	sysfs(GETFSIND, "pcfs")
struct	pc_args {
	char	*fspec;
	/*
	 * I don't know what this field is for, but from truss I guessed
	 * that there had to be one...  -Erez.
	 */
	char	*unknown;
};


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
 * Also for definition of UDPMSGSIZE.
 */
#ifdef __svr4__
# ifndef UDPMSGSIZE
#  define UDPMSGSIZE 8800
# endif /* UDPMSGSIZE */
#else
# define PORTMAP
#endif /* __svr4__ */

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
 * Solaris 2.x does not use a special fstype for automounters ("auto" or
 * "ignore") but "nfs" and sets a mount OPTION to ignore.  See special code in
 * amd/afs_ops.c to add this option to all solaris machines.
 */
#undef MNTTYPE_AUTO
#define MNTTYPE_AUTO "nfs"

/*
 * System Vr4 / SunOS 4.1 compatibility
 * - put dev= in the options list
 *
 * From: Brent Callaghan <brent@eng.sun.com>
 */
#define	MNTINFO_DEV	"dev"
#define MNTINFO_PREF	""

/*
 * To get definition of struct ufs_args.
 */
#define UFS_HDR "misc-svr4.h"

/*
 * Definition of sigmask() needed for nfs_start(), taken from
 * /usr/usbinclude/sys/signal.h.
 */
#define sigmask(m)      (m > 32 ? 0 : (1 << ((m)-1)))

/*
 * In Solaris 2.1, _seterr_reply was renamed __seterr_reply (?!)
 */
#define _seterr_reply __seterr_reply

/*
 * Avoid and BSD "compatibility" modes.  SVR4 has mem* functions
 * instead of b* ones.
 */
#define	bzero(ptr, len)		memset((ptr), 0, (len))
#define bcopy(from, to, len)	memmove((to), (from), (len))
#define bcmp(a, b, len)		memcmp((a), (b), (len))

/*
 * For "purity" of svr4 port, use its own signal routines...
 */
#ifndef SYS5_SIGNALS
#define SYS5_SIGNALS
#endif /* SYS5_SIGNALS */

/*
 * os has plock()
 */
#define	HAS_PLOCK

#if notdef
/*
 * Solaris 2.x has NIS+
 * This turns on unverified code.  Use at your own risk.
 */
#define HAS_NISPLUS		/* do you use NIS+? */
#define HAS_NISPLUS_MAP		/* do you have NIS+ maps? */
#endif

/*
 * Making sure HAS_STRDUP is defined is needed for <fsinfo/fsinfo.h>
 */
#define HAS_STRDUP
