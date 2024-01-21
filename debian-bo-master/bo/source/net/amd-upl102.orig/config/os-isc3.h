/*
 * ix/386 2.2  definitions for Amd (automounter)
 * Contributed by Stuart Hayton <stuey@uk.co.tardisuk>
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	%W% (Berkeley) %G%
 */

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define RPC_3

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define NFS_3

/*
 * how to copy a file handle into nfs_args
 */
#undef NFS_FH_DREF
#define       NFS_FH_DREF(dst, src) { (dst) = *(src); }

/*
 * How to copy an address into an NFS filehandle
 */
#undef NFS_SA_DREF
#define	NFS_SA_DREF(dst, src) { \
		(dst).raddr.buf = (char *) (src); \
		(dst).raddr.len = SIZEOF_SOCKADDR_IN; \
		(dst).trans = 1; \
	}

#undef NFS_HN_DREF
#define NFS_HN_DREF(dst, src) { \
		strncpy((dst), (src), HOSTNAMESZ); \
		(dst)[HOSTNAMESZ] = '\0'; \
	}

/*
/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#define ARCH_ENDIAN	"little"

/*
 * Has support for syslog()
 */
#define HAS_SYSLOG

#define M_RDONLY	MS_RDONLY

/*
 * No support for ndbm
 */
/*#define OS_HAS_NDBM */

#define UPDATE_MTAB

#undef	MTAB_TYPE_NFS
#define MTAB_TYPE_NFS	"NFS"

#undef	MTAB_TYPE_UFS
#define MTAB_TYPE_UFS	"S51K"

#define MTAB_TYPE_AUTO	"AUTO"
/* use of MNTTYPE_AUTO is inconsistent get put into mtab */
/* I want df to say it in caps */
#undef	MNTTYPE_AUTO
#define MNTTYPE_AUTO	MTAB_TYPE_AUTO

/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_UFS	sysfs(GETFSIND, FSID_S51K)
#define MOUNT_TYPE_NFS	sysfs(GETFSIND, FSID_NFS)

/*
 * Use <fcntl.h> rather than <sys/file.h>
 */
#define USE_FCNTL

/*
 * Use fcntl() rather than flock()
 */
#define LOCK_FCNTL

#ifdef __GNUC__
#define alloca(sz) __builtin_alloca(sz)
#endif

#define FIXUP_MNTENT(mntp) { \
	(mntp)->mnt_time = time((int *)0); \
	(mntp)->mnt_ro = (hasmntopt(mnt, "ro") != NULL); \
}

#define bzero(ptr, len) memset(ptr, 0, len)
#define bcopy(from, to, len) memmove(to, from, len)

#undef MOUNT_TRAP
#define MOUNT_TRAP(type, mnt, flags, mnt_data) \
	mount_isc3(mnt->mnt_fsname, mnt->mnt_dir, flags, type, mnt_data)
#undef UNMOUNT_TRAP
#define UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)

#define NFDS	30	/* conservative */

/* Hacked umount_fs code to work */
/* will still umount if not in mnttab */
#define NEED_UMOUNT_FS
/* will do the umount even if not in mtab */
#define UMOUNT_ANYWAY

#define NFS_HDR "misc-isc3.h"
#define UFS_HDR "misc-isc3.h"

/* no mntent on isc3 */
#undef	MNTENT_HDR
/* No need to define this at all -Erez Zadok <ezk@cs.columbia.edu> */
/* #define	MNTENT_HDR "nullhdr.h" */

#define MOUNT_HELPER_SOURCE "mount_isc3.c"

#define NEED_MNTOPT_PARSER
#define MNTMAXSTR	128

struct mntent {
	char    *mnt_fsname;    /* name of mounted file system */
	char    *mnt_dir;       /* file system path prefix */
	char    *mnt_type;      /* MNTTYPE_* */
	char    *mnt_opts;      /* MNTOPT* */
	int     mnt_freq;       /* dump frequency, in days */
	int     mnt_passno;     /* pass number on parallel fsck */
	long	mnt_time;
	short	mnt_ro;
};

#define MOUNTED	"/etc/mnttab"

/* not included in sys/param.h */
#include <sys/types.h>

#define	MNTINFO_DEV	"fsid"
#define	MNTINFO_PREF	"0x"

#include <net/errno.h>
#undef ENAMETOOLONG
#undef ENOTEMPTY

#define MAXPATHLEN	1024
#define ESTALE		70

/* gives us <dirent> in amd/info_union.c  */

#define USE_DIRENT

/* Kernel has no lstat as has no direct support for symlinks */
/* So what are we doing here at all ... kernel NFS client has - phew */
#define lstat(X, Y)	stat(X, Y)

#undef READ_MTAB_FROM_FILE
#define READ_MTAB_IX386_STYLE

#define	getpagesize()	(4096)

/**/
#define wait3(S, O, U)	waitpid(0, S, O)

#ifdef _POSIX_SOURCE
#define signal(S, A)	error_shouldnt_call_signal(S, A)
#else
#define HAS_SYSV_SIGNALS
#endif
