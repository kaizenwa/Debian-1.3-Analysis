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
 * $Id: misc-hpux.h,v 5.2.2.1 1992/02/09 15:10:24 jsp beta $
 *
 */
/*
 * HP-UX specific definitions
 */

#include <unistd.h>

/*
 * These definitions are from <nfs/nfs.h>
 * Unfortunately, that file cannot be included
 * because it contains lots of structure definitions
 * that are not wanted (they produce name clashes).
 * Isn't HP-UX wonderful!
 */

#ifdef NFS_4
/*
 * This is the client view of an fhandle
 */
typedef struct {
	char	fh_data[NFS_FHSIZE];	/* opaque data */
} fhandle_t;
#endif /* NFS_4 */

struct nfs_args {
	struct sockaddr_in	*addr;		/* file server address */
	fhandle_t		*fh;		/* File handle to be mounted */
	int			flags;		/* flags */
	int			wsize;		/* write size in bytes */
	int			rsize;		/* read size in bytes */
	int			timeo;		/* initial timeout in .1 sec */
	int			retrans;	/* times to retry send */
	char			*hostname;	/* server's name */
#ifdef _CS_MANPATH /* XXX for HPUX 8.0 */
#ifdef HPUX_9
	int			acregmin;	/* attr cache file min secs */
	int			acregmax;	/* attr cache file max secs */
	int			acdirmin;	/* attr cache dir min secs */
	int			acdirmax;	/* attr cache dir max secs */
#endif /* HPUX_9 */
	char			*fsname;	/* server's fs path name */
#endif /* _CS_MANPATH */
};


/*
 * NFS mount option flags
 */
#define	NFSMNT_SOFT	0x001	/* soft mount (hard is default) */
#define	NFSMNT_WSIZE	0x002	/* set write size */
#define	NFSMNT_RSIZE	0x004	/* set read size */
#define	NFSMNT_TIMEO	0x008	/* set initial timeout */
#define	NFSMNT_RETRANS	0x010	/* set number of request retrys */
#define	NFSMNT_HOSTNAME	0x020	/* set hostname for error printf */
#define	NFSMNT_INT	0x040	/* set option to have interruptable mounts */
#define	NFSMNT_NODEVS   0x080   /* turn off device file access (default on) */
#ifdef		_CS_MANPATH	/* to distinguish between HPUX 7.0 and 8.0 */
#define NFSMNT_FSNAME   0x100	/* provide name of server's fs to system */
#endif		/* _CS_MANPATH */
/*
 * Changed from 8.07 to 9.01, needed more from nfs.h.
 * Tim A. Lentz <lentz@pvi.com> 4/14/93
 * Added rest of the NFSMNT_* options from <nfs/nfs.h> -Erez.
 */
#ifdef HPUX_9
#define NFSMNT_IGNORE   0x200   /* mark this file system as ignore in mnttab */
#define NFSMNT_NOAC     0x400   /* don't cache file attributes */
#define NFSMNT_NOCTO    0x800   /* don't get new attributes on open */
#define NFSMNT_DYNAMIC  0x1000  /* Use dynamic read and write sizes */
#define NFSMNT_ACREGMIN 0x02000 /* set min secs for file attr cache */
#define NFSMNT_ACREGMAX 0x04000 /* set max secs for file attr cache */
#define NFSMNT_ACDIRMIN 0x08000 /* set min secs for dir attr cache */
#define NFSMNT_ACDIRMAX 0x10000 /* set max secs for dir attr cache */
#endif /* end HPUX_9 */
