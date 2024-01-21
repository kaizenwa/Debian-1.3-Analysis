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
 * $Id: stubs.c,v 1.10 1993/09/13 15:11:00 ezk Exp $
 *
 * HLFSD was written at Columbia University Computer Science Department, by
 * Erez Zadok <ezk@cs.columbia.edu> and Alexander Dupuy <dupuy@cs.columbia.edu>
 * It is being distributed under the same terms and conditions as amd does.
 */

#include "hlfs.h"
#include <errno.h>

#ifdef POSIX
#include <unistd.h>
#endif /* POSIX */

/*
 * HLFSD won't compile if AUTH_DES is on (by default)
 * DES is not that important anyway...
 */
#if defined(hpux) || defined (__osf__) || defined (__BSDI__) || defined (__NetBSD__) || defined (__FreeBSD__)
#undef AUTH_DES
#endif /* hpux or __osf1__ */

#ifdef AUTH_DES
#include <rpc/auth_des.h>
#endif /* AUTH_DES */

#define SLINKID		-2	/* don't change this from being -2 ! */

#define DOTSTRING	"."

#ifdef DEBUG
#define WHEREAMI printf("FILE=\"%s\", line no.=%d\n",__FILE__,__LINE__)
#else
#define WHEREAMI
#endif /* DEBUG */

nfs_fh root = {ROOTID};
static nfs_fh slink = {SLINKID};
static nfs_fh un_fhandle = {-3};
extern int hlfs_gid;

/* ARGSUSED */
voidp nfsproc_null_2(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static char res;

	return (voidp) &res;
}

static fattr rootfattr = {NFDIR, 0040555, 2, 0, 0, 512, 512, 0, 1, 0, ROOTID};
static fattr slinkfattr = {NFLNK, 0120777, 1, 0, 0, NFS_MAXPATHLEN, 512, 0,
			       (NFS_MAXPATHLEN + 1) / 512, 0, SLINKID};
				/* user name file attributes */
static fattr un_fattr = {NFLNK, 0120777, 1, 0, 0, NFS_MAXPATHLEN, 512, 0,
			       (NFS_MAXPATHLEN + 1) / 512, 0, -3};

static int started;
static int getcreds P((struct svc_req *, uid_t *, int *));

/* compare if two filehandles are equal */
static int eq_fh(fh1, fh2)
nfs_fh *fh1, *fh2;
{
   return (!bcmp((char *)fh1, (char *) fh2, sizeof(nfs_fh)));
}

/* ARGSUSED */
attrstat *nfsproc_getattr_2(argp, rqstp)
nfs_fh *argp;
struct svc_req *rqstp;
{
	static attrstat res;
	uid_t uid = -3;
	int gid = -3;

	if (!started) {
		started++;
		rootfattr.ctime = startup;
		rootfattr.mtime = startup;
		slinkfattr.ctime = startup;
		slinkfattr.mtime = startup;
		un_fattr.ctime = startup;
		un_fattr.mtime = startup;
	}
	if (eq_fh(argp->data, root.data)) {
		res.status = NFS_OK;
		res.attrstat_u.attributes = rootfattr;
	} else if (eq_fh(argp->data, slink.data)) {
#if defined(HAVE_SYMLINK_CACHE) && !defined(NFSMNT_SYMTTL)
		/*
		 * This code is needed to defeat Solaris 2.4's symlink values
		 * cache.  It is not needed if the O/S has an nfs flag to
		 * turn off the symlink-cache at mount time (such as Irix
		 * 5.2 and 5.3).	-Erez.
		 */
		if (++slinkfattr.mtime.useconds == 0)
		    ++slinkfattr.mtime.seconds;
#endif /* HAVE_SYMLINK_CACHE && !NFSMNT_SYMTTL */
		res.status = NFS_OK;
		res.attrstat_u.attributes = slinkfattr;
	} else {
		if (getcreds(rqstp, &uid, &gid) < 0) {
			res.status = NFSERR_STALE;
			return &res;
		}
		if (gid != hlfs_gid) {
			res.status = NFSERR_STALE;
		} else {
			bzero((char *)&uid, sizeof(int));
			uid = *(unsigned int *) argp->data;
			if (plt_search(uid) != (uid2home_t *) NULL) {
				res.status = NFS_OK;
				un_fattr.fileid = uid;
				res.attrstat_u.attributes = un_fattr;
			} else {	/* not found */
				res.status = NFSERR_STALE;
			}
		}
	}
	return &res;
}

/* ARGSUSED */
attrstat *nfsproc_setattr_2(argp, rqstp)
sattrargs *argp;
struct svc_req *rqstp;
{
	static attrstat res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
voidp nfsproc_root_2(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static char res;

	return (voidp) &res;
}

/* ARGSUSED */
diropres *nfsproc_lookup_2(argp, rqstp)
diropargs *argp;
struct svc_req *rqstp;
{
	static diropres res;
	int idx;
	uid_t uid = -3;
	int gid = -3;

	if (!started) {
		started++;
		rootfattr.ctime = startup;
		rootfattr.mtime = startup;
		slinkfattr.ctime = startup;
		slinkfattr.mtime = startup;
		un_fattr.ctime = startup;
		un_fattr.mtime = startup;
	}
	if (eq_fh(argp->dir.data, slink.data)) {
		res.status = NFSERR_NOTDIR;
		return &res;
	}
	if (eq_fh(argp->dir.data, root.data)) {
		if (argp->name[0] == '.' &&
			(argp->name[1] == '\0' ||
			(argp->name[1] == '.' &&
			argp->name[2] == '\0'))) {
			res.diropres_u.diropres.file = root;
			res.diropres_u.diropres.attributes = rootfattr;
			res.status = NFS_OK;
			return &res;
		}
		if (strcmp(argp->name, slinkname) == 0) {
			res.diropres_u.diropres.file = slink;
			res.diropres_u.diropres.attributes = slinkfattr;
			res.status = NFS_OK;
			return &res;
		}
		if (getcreds(rqstp, &uid, &gid) < 0 || gid != hlfs_gid) {
			res.status = NFSERR_NOENT;
	 		return &res;
		}
		/* if get's here, gid == hlfs_gid */
		if ((idx = untab_index(argp->name)) < 0) {
			res.status = NFSERR_NOENT;
			return &res;
		} else {		/* entry found and gid is permitted */
			un_fattr.fileid = untab[idx].uid;
			res.diropres_u.diropres.attributes = un_fattr;
			bzero((char *)&un_fhandle, sizeof(nfs_fh));
			*(u_int *) un_fhandle.data = (u_int) untab[idx].uid;
			strncpy(&un_fhandle.data[sizeof(int)],
				untab[idx].username,
				sizeof(nfs_fh)-sizeof(int));
			res.diropres_u.diropres.file = un_fhandle;
			res.status = NFS_OK;
			return &res;
		}
	}	/* end of "if (eq_fh(argp->dir.data, root.data)) {" */
	res.status = NFSERR_STALE;
	return &res;
}


static int getcreds(rp, u, g)
struct svc_req *rp;
uid_t *u;
int *g;
{
	struct authunix_parms *aup = (struct authunix_parms *) NULL;
#ifdef AUTH_DES
	struct authdes_cred *adp;
#endif /* AUTH_DES */

	switch (rp->rq_cred.oa_flavor) {
	case AUTH_UNIX:
		aup = (struct authunix_parms *) rp->rq_clntcred;
		*u = aup->aup_uid;
		*g = aup->aup_gid;
		break;
#ifdef AUTH_DES
	case AUTH_DES:
		adp = (struct authdes_cred *) rp->rq_clntcred;
		*g = -3;	/* some unknown group id */
		if (sscanf(adp->adc_fullname.name, "unix.%d@", u) == 1)
			break;
		/* fall through */
#endif /* AUTH_DES */
	default:
		*u = *g = -3;	/* just in case */
		svcerr_weakauth(transp);
		return -1;
	}
	return 0;		/* everything is ok */
}

/* ARGSUSED */
readlinkres *nfsproc_readlink_2(argp, rqstp)
nfs_fh *argp;
struct svc_req *rqstp;
{
	static readlinkres res;
	uid_t userid = -3;
	int groupid = hlfs_gid+1; /* anything not hlfs_gid */
	int retval = 0;
	char *path_val = (char *) NULL;
	char *username;

	if (eq_fh(argp->data, root.data)) {
		res.status = NFSERR_ISDIR;
	} else if (eq_fh(argp->data, slink.data)) {
		if (getcreds(rqstp, &userid, &groupid) < 0)
			return (readlinkres *) NULL;

		(void) gettimeofday((struct timeval *) & slinkfattr.atime,
				    (struct timezone *) 0);

		res.status = NFS_OK;
		if (groupid == hlfs_gid) {
			res.readlinkres_u.data = DOTSTRING;
		} else if (!(res.readlinkres_u.data = path_val = homedir(userid))) {
			/*
			 * parent process (fork in homedir()) continues
			 * processing, by getting a NULL returned as a
			 * "special".  Child returns result.
			 */
			return (readlinkres *) NULL;
		}
	} else {		/* check if asked for user mailbox */
		if (getcreds(rqstp, &userid, &groupid) < 0) {
			return (readlinkres *) NULL;
		}
		if (groupid == hlfs_gid) {
			bzero((char *)&userid, sizeof(int));
			userid = *(unsigned int *) argp->data;
			username = &argp->data[sizeof(int)];
			if (!(res.readlinkres_u.data = mailbox(userid, username)))
				return (readlinkres *) NULL;
		} else {
			res.status = NFSERR_STALE;
		}
	}

	/* I don't think will pass this if -D nofork */
	if (serverpid == getpid())
		return &res;

	if (!svc_sendreply(transp, xdr_readlinkres, (SVC_IN_ARGS_TYPE) &res))
		svcerr_systemerr(transp);

	/*
	 * Child exists here.   We need to determine which
	 * exist status to return.  The exit status
	 * is gathered using wait() and determines
	 * if we returned $HOME/.hlfsspool or $ALTDIR.  The parent
	 * needs this info so it can update the lookup table.
	 */
	if (path_val && alt_spooldir && strcmp(path_val, alt_spooldir) == 0)
		retval = 1;	/* could not get real home dir (or uid 0 user) */
	else
		retval = 0;
#ifdef DEBUG
	/*
	 * If asked for -D nofork, then must return the value,
	 * NOT exit, or else the main hlfsd server exits.
	 * Bug where is that status information being collected?
	 */
	DebugNo(D_FORK)
		return &res;
#endif /* DEBUG */
#ifdef DEBUG
	dlog("exit in file %s:%d", __FILE__, __LINE__);
#endif /* DEBUG */
	exit(retval);
	/* NOTREACHED */
}

/* ARGSUSED */
readres *nfsproc_read_2(argp, rqstp)
readargs *argp;
struct svc_req *rqstp;
{
	static readres res = {NFSERR_ACCES};

	return &res;
}

/* ARGSUSED */
voidp nfsproc_writecache_2(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static char res;

	return (voidp) &res;
}

/* ARGSUSED */
attrstat *nfsproc_write_2(argp, rqstp)
writeargs *argp;
struct svc_req *rqstp;
{
	static attrstat res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
diropres *nfsproc_create_2(argp, rqstp)
createargs *argp;
struct svc_req *rqstp;
{
	static diropres res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
nfsstat *nfsproc_remove_2(argp, rqstp)
diropargs *argp;
struct svc_req *rqstp;
{
	static nfsstat res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
nfsstat *nfsproc_rename_2(argp, rqstp)
renameargs *argp;
struct svc_req *rqstp;
{
	static nfsstat res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
nfsstat *nfsproc_link_2(argp, rqstp)
linkargs *argp;
struct svc_req *rqstp;
{
	static nfsstat res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
nfsstat *nfsproc_symlink_2(argp, rqstp)
symlinkargs *argp;
struct svc_req *rqstp;
{
	static nfsstat res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
diropres *nfsproc_mkdir_2(argp, rqstp)
createargs *argp;
struct svc_req *rqstp;
{
	static diropres res = {NFSERR_ROFS};

	return &res;
}

/* ARGSUSED */
nfsstat *nfsproc_rmdir_2(argp, rqstp)
diropargs *argp;
struct svc_req *rqstp;
{
	static nfsstat res = {NFSERR_ROFS};

	return &res;
}

#define DOTCOOKIE 1
#define DOTDOTCOOKIE 2
#define SLINKCOOKIE 3

/* ARGSUSED */
readdirres *nfsproc_readdir_2(argp, rqstp)
readdirargs *argp;
struct svc_req *rqstp;
{
	static readdirres res;
	static entry slinkent = {SLINKID, 0, {SLINKCOOKIE}};
	static entry dotdotent = {ROOTID, "..", {DOTDOTCOOKIE}, &slinkent};
	static entry dotent = {ROOTID, ".", {DOTCOOKIE}, &dotdotent};

	slinkent.name = slinkname;

	if (eq_fh(argp->dir.data, slink.data)) {
		res.status = NFSERR_NOTDIR;
	} else if (eq_fh(argp->dir.data, root.data)) {
		(void) gettimeofday((struct timeval *) & rootfattr.atime,
				    (struct timezone *) 0);

		res.status = NFS_OK;
		switch (argp->cookie[0]) {
		case 0:
			res.readdirres_u.reply.entries = &dotent;
			break;
		case DOTCOOKIE:
			res.readdirres_u.reply.entries = &dotdotent;
			break;
		case DOTDOTCOOKIE:
			res.readdirres_u.reply.entries = &slinkent;
			break;
		case SLINKCOOKIE:
			res.readdirres_u.reply.entries = (entry *) 0;
			break;
		}
		res.readdirres_u.reply.eof = TRUE;
	} else {
		res.status = NFSERR_STALE;
	}
	return &res;
}

/* ARGSUSED */
statfsres *nfsproc_statfs_2(argp, rqstp)
nfs_fh *argp;
struct svc_req *rqstp;
{
	static statfsres res = {NFS_OK};

	res.statfsres_u.reply.tsize = 1024;
	res.statfsres_u.reply.bsize = 1024;
#ifdef HAS_EMPTY_AUTOMOUNTS
	res.statfsres_u.reply.blocks = 0;
#else
	res.statfsres_u.reply.blocks = 1;
#endif
	res.statfsres_u.reply.bfree = 0;
	res.statfsres_u.reply.bavail = 0;

	return &res;
}
