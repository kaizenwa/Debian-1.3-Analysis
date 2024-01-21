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
 * $Id: os-linux.h,v 5.2.2.1 1992/02/09 15:10:17 jsp beta $
 *
 * Linux definitions.  Mitchum D'Souza (3rd July 1993)
 */

#ifndef OS_LINUX_H
#define OS_LINUX_H

/* We define the following, so that we don't traverse into linux's stdlib.h
 * as we seem to have a linked list qelem structure defined there, and it
 * conflicts with the definition in amd's header file am.h
 */
#define _STDLIB_H 

/* This is to say that Linux uses names (i.e. char *)
 * in it's types parameter to mount()
 */
#define M_NEWTYPE

#define FNDELAY O_NDELAY
#define NFS_FHSIZE 32
struct nfs_fh {
	char data[NFS_FHSIZE];
};
#include <rpc/rpc.h>
#include <linux/nfs_mount.h>
#define nfs_args nfs_mount_data
#define fh root

/*
 * What level of AMD are we backward compatible with?
 * This only applies to externally visible characteristics.
 * Rev.Minor.Branch.Patch (2 digits each)
 */
#define	AMD_COMPAT	5000000		/* 5.0 */

/*
 * What type is free(void*) returning?
 */
#define FREE_RETURN_TYPE	void

/*
 * Is the mount table mirrored in software
 */
#define	UPDATE_MTAB

/*
 * Linux and GNU's df "suffers" from necessity of
 * precise symlinks - Thanx Rick Sladkey (again :-)
 */
#define PRECISE_SYMLINKS

/*
 * Where to get union wait
 */
#define	WAIT	<sys/wait.h>

/*
 * Type of status argument passed to wait3 syscall
 */
#define WAIT_STATUS_TYPE int

/*
 * Where to get mount entry info
 */
#define	MNTENT_HDR	<mntent.h>

/*
 * Include support for syslog()
 */
#define	HAS_SYSLOG

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#define	ARCH_ENDIAN	"little"

/*
 * Name of filesystem types
 */


#define MOUNT_TYPE_NFS	"nfs"
#define	MTAB_TYPE_NFS	"nfs"

#undef MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	"vfs"

/*
 * Name of mount & unmount system calls
 *
 * NOTE:
 *  UNMOUNT_TRAP takes a struct mntent *
 */

#undef MOUNT_TRAP
#define	MOUNT_TRAP(type, mnt, flags, mnt_data) \
				linux_mount(type, mnt, flags, mnt_data)

#undef UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt) umount(mnt->mnt_dir)


/*
 * How to unmount filesystems.
 * NEED_UMOUNT_FS includes code to scan the mount table
 * to find the correct information for the unmount system
 * call.  Some systems, such as 4.4bsd, do not require
 * this - they can just do an unmount system call directly.
 *
*/
#define NEED_UMOUNT_FS

/*
 * Type of a file handle
 */
#undef NFS_FH_TYPE
#define	NFS_FH_TYPE	char *

#undef NFS_FH_DREF
#define	NFS_FH_DREF(dst, src) \
	bcopy((char *) src, (char *)&dst.data, sizeof(struct nfs_fh))
/*
 * How to copy an address into an NFS filehandle
 */
#undef NFS_SA_DREF
#define	NFS_SA_DREF(dst, src) \
	bcopy ((char *) src, (char *)&dst.addr, sizeof(struct sockaddr_in))

/*
 * Type of filesystem type
 */
#undef MTYPE_TYPE
#define	MTYPE_TYPE	char *

#undef NFS_HN_DREF
#define NFS_HN_DREF(dst, src) strncpy((dst), (src), MAXHOSTNAMELEN)

#define HOSTNAMESZ MAXHOSTNAMELEN
/*
 * How to get a mount list
 */
#define	READ_MTAB_FROM_FILE

/*
 * Make Amd automount points appear
 * to be zero sized.  undef this
 * if the O/S has a divide by zero
 * problem in df et al.
 */
#define	HAS_EMPTY_AUTOMOUNTS

/*
 * For the RE matcher
 */
#undef STRCSPN
#undef CHARBITS
#define RE_HDR "re.h"

/*
 * Type of the third argument ("in") to some svc_*args() RPC functions, such
 * as svc_getargs().
 */
#define SVC_IN_ARGS_TYPE char *

/*
 * Default type of signal handlers (return type)
 */
#define SIG_HNDL_TYP void

/* Any unknown flag must be zero as it is or'ed with the flags argument */

#define NFSMNT_HOSTNAME 0
#define NFSMNT_RETRANS 0
#define NFSMNT_TIMEO 0
#define NFSMNT_RSIZE 0
#define NFSMNT_WSIZE 0
#define NFSMNT_NOCTO NFS_MOUNT_NOCTO

#define MNTOPT_SOFT "soft"
#define NFSMNT_SOFT NFS_MOUNT_SOFT

#define MNTOPT_INTR "intr"
#define NFSMNT_INT NFS_MOUNT_INTR

#define MNTOPT_NOAC "noac"
#define NFSMNT_NOAC NFS_MOUNT_NOAC

#if 0
#define MNTOPT_POSIX "posix"
#define NFSMNT_POSIX NFS_MOUNT_POSIX
#endif

#define MNTOPT_ACREGMIN "acregmin"
#define NFSMNT_ACREGMIN 0

#define MNTOPT_ACREGMAX "acregmax"
#define NFSMNT_ACREGMAX 0

#define MNTOPT_ACDIRMIN "acdirmin"
#define NFSMNT_ACDIRMIN 0

#define MNTOPT_ACDIRMAX "acdirmax"
#define NFSMNT_ACDIRMAX 0

/* Let's set up some values */
#define PRESET_AC
#define ACREGMIN 3
#define ACREGMAX 60
#define ACDIRMIN 30
#define ACDIRMAX 60

#define M_RDONLY 1 /* mount read-only */
#define M_NOSUID 2 /* ignore suid and sgid bits */
#define M_NONDEV 4 /* disallow access to device special files */
#define M_NOEXEC 8 /* disallow program execution */
#define M_SYNC  16 /* writes are synced at once */
#define M_REMOUNT  32 /* alter flags of a mounted FS */

struct ufs_args {
	char	*fspec;				/* Block device */
};

#define	MOUNT_HELPER_SOURCE "mount_linux.c"

/*
 * No AUTH_DES
 */
#undef AUTH_DES

#endif /* OS_LINUX_H */
