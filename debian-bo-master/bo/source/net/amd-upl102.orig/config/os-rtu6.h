/* $Id: os-sos3.h,v 5.1.1.1 89/11/28 18:04:23 jsp Exp Locker: jsp $ */

/*
 * RTU 5.0 definitions for Amd (automounter)
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Imperial College of Science, Technology and Medicine, London, UK.
 * The names of the College and University may not be used to endorse
 * or promote products derived from this software without specific
 * prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	%W% (Berkeley) %G%
 */

/*
 * Does the compiler grok void *
 */
#define	VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	RPC_3

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
#define	ARCH_ENDIAN	"big"

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_UFS	MOUNT_UFS
#define MOUNT_TYPE_NFS	MOUNT_NFS

#undef HAS_SYSLOG

#undef MOUNT_TRAP
#define	MOUNT_TRAP(type, mnt, flags, mnt_data) \
	vmount(type, mnt->mnt_dir, flags, mnt_data)

/* addmntent seems to return 1 for success and 0 for failure on rtu 6.0A */
/* redefine it so we don't get problems */
#define addmntent(filep, mnt) \
	(fprintf(filep, "%s %s %s %s %d %d\n", \
		mnt->mnt_fsname, \
		mnt->mnt_dir, \
		mnt->mnt_type, \
		mnt->mnt_opts, \
		mnt->mnt_freq, \
		mnt->mnt_passno), 0)

/* #define MISC_RPC */
#define NEED_XDR_POINTER
#define NEED_CLNT_SPERRNO

#define USE_FCNTL

/*
 * Use varargs routines and passing convention where necessary
 */
#define VARARGS

/*
 * os has plock()
 */
#define	HAS_PLOCK
