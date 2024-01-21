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
 * $Id: hlfs.h,v 1.9 1993/09/13 15:11:00 ezk Exp $
 *
 * HLFSD was written at Columbia University Computer Science Department, by
 * Erez Zadok <ezk@cs.columbia.edu> and Alexander Dupuy <dupuy@cs.columbia.edu>
 * It is being distributed under the same terms and conditions as amd does.
 */

#ifndef _hlfsd_hlfs_h
#define _hlfsd_hlfs_h

#ifdef sgi
#define _BSD_SIGNALS
#endif /* sgi */

#include "am.h"

#define HLFSD_VERSION "hlfsd 1.0 (November 9 1993)"

#define ROOTID -1		/* don't change this from being -1 */

#if defined(MACH2) || defined(NeXT) || defined(mc68010)
typedef int	pid_t;
#endif

struct uid2home_t {
	uid_t uid;
	pid_t child;
	char *home;
	char *uname;
	u_long last_access_time;
	int last_status;	/* 0=used $HOME/.hlfsspool; nonzero=used alt dir */
};
typedef struct uid2home_t uid2home_t;

struct username2uid_t {
	char *username;
	uid_t uid;
	char *home;
};
typedef struct username2uid_t username2uid_t;

#ifdef DEBUG
#define D_FORK 0x1000
#endif /* DEBUG */
#if defined(DEBUG) || defined(DEBUG_PRINT)
extern void plt_print P((int));
#endif

extern void fatal P((char *));
extern SIG_HNDL_TYP interlock P((int));
extern SIG_HNDL_TYP cleanup P((int));
extern void init_homedir(P_void);
extern char *homedir P((int));
extern username2uid_t *untab;	/* user name table */
extern uid2home_t *plt_search P((int));
extern char mboxfile[];
extern char *mailbox P((int, char *));

extern nfs_fh root;
extern nfstime startup;
extern int noverify;
extern int serverpid;
extern char *slinkname;
extern char *alt_spooldir;
extern char *home_subdir;
extern SVCXPRT *transp;
extern int cache_interval;

#ifdef hpux
#define seteuid(x) setresuid(-1,(x),-1)
#endif /* hpux */

#endif /* _hlfsd_hlfs_h */
