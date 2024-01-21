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
 * $Id: nfs_ops.c,v 5.2.2.3 1992/08/02 10:42:21 jsp Exp $
 *
 */

#include "am.h"
#include <sys/stat.h>

#ifdef HAS_NFS

#define NFS
#define NFSCLIENT
#ifdef NFS_3
typedef nfs_fh fhandle_t;
#endif /* NFS_3 */
#ifdef NFS_HDR
#include NFS_HDR
#endif /* NFS_HDR */
#include <sys/mount.h>
#include "mount.h"

#ifdef MACH2
typedef nfs_fh fhandle_t;
#include <nfs/nfs_mount.h>
#endif /* MACH2 */

/*
 * Network file system
 */

/*
 * Convert from nfsstat to UN*X error code
 */
#define unx_error(e)	((int)(e))

/*
 * The NFS layer maintains a cache of file handles.
 * This is *fundamental* to the implementation and
 * also allows quick remounting when a filesystem
 * is accessed soon after timing out.
 *
 * The NFS server layer knows to flush this cache
 * when a server goes down so avoiding stale handles.
 *
 * Each cache entry keeps a hard reference to
 * the corresponding server.  This ensures that
 * the server keepalive information is maintained.
 *
 * The copy of the sockaddr_in here is taken so
 * that the port can be twiddled to talk to mountd
 * instead of portmap or the NFS server as used
 * elsewhere.
 * The port# is flushed if a server goes down.
 * The IP address is never flushed - we assume
 * that the address of a mounted machine never
 * changes.  If it does, then you have other
 * problems...
 */
typedef struct fh_cache fh_cache;
struct fh_cache {
	qelem	fh_q;			/* List header */
	voidp	fh_wchan;		/* Wait channel */
	int	fh_error;		/* Valid data? */
	int	fh_id;			/* Unique id */
	int	fh_cid;			/* Callout id */
	struct fhstatus fh_handle;	/* Handle on filesystem */
	struct sockaddr_in fh_sin;	/* Address of mountd */
	fserver *fh_fs;			/* Server holding filesystem */
	char	*fh_path;		/* Filesystem on host */
};

/*
 * FH_TTL is the time a file handle will remain in the cache since
 * last being used.  If the file handle becomes invalid, then it
 * will be flushed anyway.
 */
#define	FH_TTL		(5 * 60)		/* five minutes */
#define	FH_TTL_ERROR	(30)			/* 30 seconds */

static int fh_id = 0;
#define	FHID_ALLOC()	(++fh_id)
extern qelem fh_head;
qelem fh_head = { &fh_head, &fh_head };

static int call_mountd P((fh_cache*, unsigned long, fwd_fun, voidp));

AUTH *nfs_auth;

static fh_cache *find_nfs_fhandle_cache P((voidp idv, int done));
static fh_cache *find_nfs_fhandle_cache(idv, done)
voidp idv;
int done;
{
	fh_cache *fp, *fp2 = 0;
	int id = (int) idv;

	ITER(fp, fh_cache, &fh_head) {
		if (fp->fh_id == id) {
			fp2 = fp;
			break;
		}
	}

#ifdef DEBUG
	if (fp2) {
		dlog("fh cache gives fp %#x, fs %s", fp2, fp2->fh_path);
	} else {
		dlog("fh cache search failed");
	}
#endif /* DEBUG */

	if (fp2 && !done) {
		fp2->fh_error = ETIMEDOUT;
		return 0;
	}

	return fp2;
}

/*
 * Called when a filehandle appears
 */
static void got_nfs_fh P((voidp pkt, int len, struct sockaddr_in *sa,
				struct sockaddr_in *ia, voidp idv, int done));
static void got_nfs_fh(pkt, len, sa, ia, idv, done)
voidp pkt;
int len;
struct sockaddr_in *sa, *ia;
voidp idv;
int done;
{
	fh_cache *fp = find_nfs_fhandle_cache(idv, done);
	if (fp) {
		fp->fh_error = pickup_rpc_reply(pkt, len, (voidp) &fp->fh_handle, xdr_fhstatus);
		if (!fp->fh_error) {
#ifdef DEBUG
			dlog("got filehandle for %s:%s", fp->fh_fs->fs_host, fp->fh_path);
#endif /* DEBUG */
			/*
			 * Wakeup anything sleeping on this filehandle
			 */
			if (fp->fh_wchan) {
#ifdef DEBUG
				dlog("Calling wakeup on %#x", fp->fh_wchan);
#endif /* DEBUG */
				wakeup(fp->fh_wchan);
			}
		}
	}
}

void flush_nfs_fhandle_cache P((fserver *fs));
void flush_nfs_fhandle_cache(fs)
fserver *fs;
{
	fh_cache *fp;
	ITER(fp, fh_cache, &fh_head) {
		if (fp->fh_fs == fs || fs == 0) {
			fp->fh_sin.sin_port = (u_short) 0;
			fp->fh_error = -1;
		}
	}
}

static void discard_fh P((fh_cache *fp));
static void discard_fh(fp)
fh_cache *fp;
{
	rem_que(&fp->fh_q);
#ifdef DEBUG
	dlog("Discarding filehandle for %s:%s", fp->fh_fs->fs_host, fp->fh_path);
#endif /* DEBUG */
	free_srvr(fp->fh_fs);
	free((voidp) fp->fh_path);
	free((voidp) fp);
}

/*
 * Determine the file handle for a node
 */
static int prime_nfs_fhandle_cache P((char *path, fserver *fs, struct fhstatus *fhbuf, voidp wchan));
static int prime_nfs_fhandle_cache(path, fs, fhbuf, wchan)
char *path;
fserver *fs;
struct fhstatus *fhbuf;
voidp wchan;
{
	fh_cache *fp, *fp_save = 0;
	int error;
	int reuse_id = FALSE;

#ifdef DEBUG
	dlog("Searching cache for %s:%s", fs->fs_host, path);
#endif /* DEBUG */

	/*
	 * First search the cache
	 */
	ITER(fp, fh_cache, &fh_head) {
		if (fs == fp->fh_fs && strcmp(path, fp->fh_path) == 0) {
			switch (fp->fh_error) {
			case 0:
				error = fp->fh_error = unx_error(fp->fh_handle.fhs_status);
				if (error == 0) {
					if (fhbuf)
						bcopy((voidp) &fp->fh_handle, (voidp) fhbuf,
							sizeof(fp->fh_handle));
					if (fp->fh_cid)
						untimeout(fp->fh_cid);
					fp->fh_cid = timeout(FH_TTL, discard_fh, (voidp) fp);
				} else if (error == EACCES) {
					/*
					 * Now decode the file handle return code.
					 */
					plog(XLOG_INFO, "Filehandle denied for \"%s:%s\"",
						fs->fs_host, path);
				} else {
					errno = error;	/* XXX */
					plog(XLOG_INFO, "Filehandle error for \"%s:%s\": %m",
						fs->fs_host, path);
				}

				/*
				 * The error was returned from the remote mount daemon.
				 * Policy: this error will be cached for now...
				 */
				return error;

			case -1:
				/*
				 * Still thinking about it, but we can re-use.
				 */
				fp_save = fp;
				reuse_id = TRUE;
				break;

			default:
				/*
				 * Return the error.
				 * Policy: make sure we recompute if required again
				 * in case this was caused by a network failure.
				 * This can thrash mountd's though...  If you find
				 * your mountd going slowly then:
				 * 1.  Add a fork() loop to main.
				 * 2.  Remove the call to innetgr() and don't use
				 *     netgroups, especially if you don't use YP.
				 */
				error = fp->fh_error;
				fp->fh_error = -1;
				return error;
			}
			break;
		}
	}

	/*
	 * Not in cache
	 */
	if (fp_save) {
		fp = fp_save;
		/*
		 * Re-use existing slot
		 */
		untimeout(fp->fh_cid);
		free_srvr(fp->fh_fs);
		free(fp->fh_path);
	} else {
		fp = ALLOC(fh_cache);
		bzero((voidp) fp, sizeof(*fp));
		ins_que(&fp->fh_q, &fh_head);
	}
	if (!reuse_id)
		fp->fh_id = FHID_ALLOC();
	fp->fh_wchan = wchan;
	fp->fh_error = -1;
	fp->fh_cid = timeout(FH_TTL, discard_fh, (voidp) fp);

	/*
	 * If the address has changed then don't try to re-use the
	 * port information
	 */
	if (fp->fh_sin.sin_addr.s_addr != fs->fs_ip->sin_addr.s_addr) {
		fp->fh_sin = *fs->fs_ip;
		fp->fh_sin.sin_port = 0;
	}
	fp->fh_fs = dup_srvr(fs);
	fp->fh_path = strdup(path);

	error = call_mountd(fp, MOUNTPROC_MNT, got_nfs_fh, wchan);
	if (error) {
		/*
		 * Local error - cache for a short period
		 * just to prevent thrashing.
		 */
		untimeout(fp->fh_cid);
		fp->fh_cid = timeout(error < 0 ? 2 * ALLOWED_MOUNT_TIME : FH_TTL_ERROR,
						discard_fh, (voidp) fp);
		fp->fh_error = error;
	} else {
		error = fp->fh_error;
	}
	return error;
}

int make_nfs_auth P((void))
{
#ifdef HAS_NFS_QUALIFIED_NAMES
	/*
	 * From: Chris Metcalf <metcalf@masala.lcs.mit.edu>
	 * Use hostd, not just hostname.  Note that uids
	 * and gids and the gidlist are type *int* and not the
	 * system uid_t and gid_t types.
	 */
	static int group_wheel = 0;
#ifdef __svr4__
	nfs_auth = authsys_create(hostd, 0, 0, 1, &group_wheel);
#else
	nfs_auth = authunix_create(hostd, 0, 0, 1, &group_wheel);
#endif /* __svr4__ */
#else
#ifdef __svr4__
	nfs_auth = authsys_create_default();
#else
	nfs_auth = authunix_create_default();
#endif /* __svr4__ */
#endif /* HAS_NFS_QUALIFIED_NAMES */
	if (!nfs_auth)
		return ENOBUFS;
	return 0;
}

static int call_mountd P((fh_cache *fp, u_long proc, fwd_fun f, voidp wchan));
static int call_mountd(fp, proc, f, wchan)
fh_cache *fp;
u_long proc;
fwd_fun f;
voidp wchan;
{
	struct rpc_msg mnt_msg;
	int len;
	char iobuf[8192];
	int error;

	if (!nfs_auth) {
		error = make_nfs_auth();
		if (error)
			return error;
	}

	if (fp->fh_sin.sin_port == 0) {
		u_short port;
		error = nfs_srvr_port(fp->fh_fs, &port, wchan);
		if (error)
			return error;
		fp->fh_sin.sin_port = port;
	}

	rpc_msg_init(&mnt_msg, MOUNTPROG, MOUNTVERS, (unsigned long) 0);
	len = make_rpc_packet(iobuf, sizeof(iobuf), proc,
			&mnt_msg, (voidp) &fp->fh_path, xdr_nfspath,  nfs_auth);

	if (len > 0) {
		error = fwd_packet(MK_RPC_XID(RPC_XID_MOUNTD, fp->fh_id),
			(voidp) iobuf, len, &fp->fh_sin, &fp->fh_sin, (voidp) fp->fh_id, f);
	} else {
		error = -len;
	}
/*
 * It may be the case that we're sending to the wrong MOUNTD port.  This
 * occurs if mountd is restarted on the server after the port has been
 * looked up and stored in the filehandle cache somewhere.  The correct
 * solution, if we're going to cache port numbers is to catch the ICMP
 * port unreachable reply from the server and cause the portmap request
 * to be redone.  The quick solution here is to invalidate the MOUNTD
 * port.
 */
      fp->fh_sin.sin_port = 0;

	return error;
}

/*-------------------------------------------------------------------------*/

/*
 * NFS needs the local filesystem, remote filesystem
 * remote hostname.
 * Local filesystem defaults to remote and vice-versa.
 */
static char *nfs_match(fo)
am_opts *fo;
{
	char *xmtab;
	if (fo->opt_fs && !fo->opt_rfs)
		fo->opt_rfs = fo->opt_fs;
	if (!fo->opt_rfs) {
		plog(XLOG_USER, "nfs: no remote filesystem specified");
		return FALSE;
	}
	if (!fo->opt_rhost) {
		plog(XLOG_USER, "nfs: no remote host specified");
		return FALSE;
	}
	/*
	 * Determine magic cookie to put in mtab
	 */
	xmtab = (char *) xmalloc(strlen(fo->opt_rhost) + strlen(fo->opt_rfs) + 2);
	sprintf(xmtab, "%s:%s", fo->opt_rhost, fo->opt_rfs);
#ifdef DEBUG
	dlog("NFS: mounting remote server \"%s\", remote fs \"%s\" on \"%s\"",
		fo->opt_rhost, fo->opt_rfs, fo->opt_fs);
#endif /* DEBUG */

	return xmtab;
}

/*
 * Initialise am structure for nfs
 */
static int nfs_init(mf)
mntfs *mf;
{
	if (!mf->mf_private) {
		int error;
		struct fhstatus fhs;
	
		char *colon = strchr(mf->mf_info, ':');
		if (colon == 0)
			return ENOENT;

		error = prime_nfs_fhandle_cache(colon+1, mf->mf_server, &fhs, (voidp) mf);
		if (!error) {
			mf->mf_private = (voidp) ALLOC(fhstatus);
			mf->mf_prfree = (void (*)()) free;
			bcopy((voidp) &fhs, mf->mf_private, sizeof(fhs));
		}
		return error;
	}

	return 0;
}

int mount_nfs_fh P((struct fhstatus *fhp, char *dir, char *fs_name, char *opts, mntfs *mf));
int mount_nfs_fh(fhp, dir, fs_name, opts, mf)
struct fhstatus *fhp;
char *dir;
char *fs_name;
char *opts;
mntfs *mf;
{
	struct nfs_args nfs_args;
	struct mntent mnt;
	int retry;
	char *colon;
	/*char *path;*/
	char host[MAXHOSTNAMELEN + MAXPATHLEN + 2];
	fserver *fs = mf->mf_server;
	int flags;
	char *xopts;
	int error;
#ifdef notdef
	unsigned short port;
#endif /* notdef */
#ifdef __svr4__
	struct netbuf nb;
#endif /* __svr4__ */
#if defined(MNTOPT_ACREGMIN) || defined(MNTOPT_ACREGMAX) || defined(MNTOPT_ACDIRMIN) || defined(MNTOPT_ACDIRMAX)
	int acval = 0;
#endif
	MTYPE_TYPE type = MOUNT_TYPE_NFS;

	bzero((voidp) &nfs_args, sizeof(nfs_args));	/* Paranoid */

	/*
	 * Extract host name to give to kernel
	 */
	if (!(colon = strchr(fs_name, ':')))
		return ENOENT;
#ifndef NFS_ARGS_NEEDS_PATH
	*colon = '\0';
#endif
	strncpy(host, fs_name, sizeof(host));
#ifndef NFS_ARGS_NEEDS_PATH
	*colon = ':';
#endif /* NFS_ARGS_NEEDS_PATH */
	/*path = colon + 1;*/

	if (mf->mf_remopts && *mf->mf_remopts && !islocalnet(fs->fs_ip->sin_addr.s_addr))
		xopts = strdup(mf->mf_remopts);
	else
		xopts = strdup(opts);

	mnt.mnt_dir = dir;
	mnt.mnt_fsname = fs_name;
	mnt.mnt_type = MTAB_TYPE_NFS;
	mnt.mnt_opts = xopts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;

	retry = hasmntval(&mnt, "retry");
	if (retry <= 0)
		retry = 1;	/* XXX */

/*again:*/

	/*
	 * set mount args
	 */
	NFS_FH_DREF(nfs_args.fh, (NFS_FH_TYPE) fhp->fhstatus_u.fhs_fhandle);

#ifdef __svr4__
				/* set up syncaddr field */
	nfs_args.syncaddr = (struct netbuf *) NULL;

				/* set up knconf field */
	if (get_knetconfig(&nfs_args.knconf) < 0) {
	  plog(XLOG_FATAL,"cannot fill knetconfig structure for nfs_args");
	  going_down(1);
	}
				/* update the flags field for knconf */
	nfs_args.flags |= NFSMNT_KNCONF;
#endif /* __svr4__ */

#ifdef ULTRIX_HACK
	nfs_args.optstr = mnt.mnt_opts;
#endif /* ULTRIX_HACK */

#ifdef HOSTNAMESZ
	/*
	 * Most kernels have a name length restriction.
	 */
	if (strlen(host) >= HOSTNAMESZ)
		strcpy(host + HOSTNAMESZ - 3, "..");
#endif /* HOSTNAMESZ */

#ifdef NFS_HN_DREF
	NFS_HN_DREF(nfs_args.hostname, host);
#else
	nfs_args.hostname = host;
#endif /* NFS_HN_DREF */
	nfs_args.flags |= NFSMNT_HOSTNAME;

#ifdef NFSMNT_FSNAME
	nfs_args.fsname = fs_name;
	nfs_args.flags |= NFSMNT_FSNAME;
#endif /* NFSMNT_FSNAME */

	if (nfs_args.rsize = hasmntval(&mnt, "rsize"))
		nfs_args.flags |= NFSMNT_RSIZE;

	if (nfs_args.wsize = hasmntval(&mnt, "wsize"))
		nfs_args.flags |= NFSMNT_WSIZE;

	if (nfs_args.timeo = hasmntval(&mnt, "timeo"))
		nfs_args.flags |= NFSMNT_TIMEO;

	if (nfs_args.retrans = hasmntval(&mnt, "retrans"))
		nfs_args.flags |= NFSMNT_RETRANS;

#ifdef NFSMNT_BIODS
	if (nfs_args.biods = hasmntval(&mnt, "biods"))
		nfs_args.flags |= NFSMNT_BIODS;

#endif /* NFSMNT_BIODS */

#ifdef notdef
/*
 * This isn't supported by the ping algorithm yet.
 * In any case, it is all done in nfs_init().
 */
 	if (port = hasmntval(&mnt, "port"))
		sin.sin_port = htons(port);
	else
		sin.sin_port = htons(NFS_PORT);	/* XXX should use portmapper */
#endif /* notdef */

	if (hasmntopt(&mnt, MNTOPT_SOFT) != NULL)
		nfs_args.flags |= NFSMNT_SOFT;

#ifdef NFSMNT_SPONGY
	if (hasmntopt(&mnt, "spongy") != NULL) {
		nfs_args.flags |= NFSMNT_SPONGY;
		if (nfs_args.flags & NFSMNT_SOFT) {
			plog(XLOG_USER, "Mount opts soft and spongy are incompatible - soft ignored");
			nfs_args.flags &= ~NFSMNT_SOFT;
		}
	}
#endif /* MNTOPT_SPONGY */

#ifdef MNTOPT_INTR
	if (hasmntopt(&mnt, MNTOPT_INTR) != NULL)
		nfs_args.flags |= NFSMNT_INT;
#endif /* MNTOPT_INTR */

#ifdef MNTOPT_NODEVS
	if (hasmntopt(&mnt, MNTOPT_NODEVS) != NULL)
		nfs_args.flags |= NFSMNT_NODEVS;
#endif /* MNTOPT_NODEVS */

#ifdef MNTOPT_COMPRESS
	if (hasmntopt(&mnt, "compress") != NULL)
		nfs_args.flags |= NFSMNT_COMPRESS;
#endif /* MNTOPT_COMPRESS */

#ifdef MNTOPT_NOCONN
	if (hasmntopt(&mnt, "noconn") != NULL)
		nfs_args.flags |= NFSMNT_NOCONN;
#endif /* MNTOPT_NOCONN */

#ifdef MNTOPT_NOAC		/* don't cache attributes */
	if (hasmntopt(&mnt, MNTOPT_NOAC) != NULL)
		nfs_args.flags |= NFSMNT_NOAC;
#endif /* MNTOPT_NOAC */


/*
 * Attribue cache settings for NFS mounts.
 * Scott Presnell, srp@cgl.ucsf.edu, Sat Nov 14 12:42:16 PST 1992
 */
#ifdef MNTOPT_ACTIMEO		/* attr cache timeout (sec) */
	if (acval = hasmntval(&mnt, MNTOPT_ACTIMEO)) {
#if defined(NFSMNT_ACREGMIN) && defined(NFSMNT_ACREGMAX) 
		nfs_args.flags |= (NFSMNT_ACREGMIN | NFSMNT_ACREGMAX);
#endif
		nfs_args.acregmin = acval;
		nfs_args.acregmax = acval;

#if defined(NFSMNT_ACDIRMIN) && defined(NFSMNT_ACDIRMAX) 
		nfs_args.flags |= (NFSMNT_ACDIRMIN | NFSMNT_ACDIRMAX);
#endif
		nfs_args.acdirmin = acval;
		nfs_args.acdirmax = acval;
	}

#endif /* MNTOPT_ACTIMEO */

#ifdef MNTOPT_ACREGMIN		/* min ac timeout for reg files (sec) */
	if (!acval && (nfs_args.acregmin = hasmntval(&mnt, MNTOPT_ACREGMIN)))
#ifdef NFSMNT_ACREGMIN
		nfs_args.flags |= NFSMNT_ACREGMIN;
#else
		;
#endif /* NFSMNT_ACREGMIN */
#endif /* MNTOPT_ACREGMIN */

#ifdef MNTOPT_ACREGMAX		/* max ac timeout for reg files (sec) */
	if (!acval && (nfs_args.acregmax = hasmntval(&mnt, MNTOPT_ACREGMAX)))
#ifdef NFSMNT_ACREGMAX
		nfs_args.flags |= NFSMNT_ACREGMAX;
#else
		;
#endif /* NFSMNT_ACREGMAX */
#endif /* MNTOPT_ACREGMAX */

#ifdef MNTOPT_ACDIRMIN		/* min ac timeout for dirs (sec) */
	if (!acval && (nfs_args.acdirmin = hasmntval(&mnt, MNTOPT_ACDIRMIN)))
#ifdef NFSMNT_ACDIRMIN
		nfs_args.flags |= NFSMNT_ACDIRMIN;
#else
		;
#endif /* NFSMNT_ACDIRMIN */
#endif /* MNTOPT_ACDIRMIN */

#ifdef MNTOPT_ACDIRMAX		/* max ac timeout for dirs (sec) */
	if (!acval && (nfs_args.acdirmax = hasmntval(&mnt, MNTOPT_ACDIRMAX)))
#ifdef NFSMNT_ACDIRMAX
		nfs_args.flags |= NFSMNT_ACDIRMAX;
#else
		;
#endif /* NFSMNT_ACDIRMAX */
#endif /* MNTOPT_ACDIRMAX */


/*
 * Some systems need the attribute cache values to be preset otherwise they
 * default to zero. If needed, define PRESET_AC and define the appropriate
 * AC[REG|DIR][MIN|MAX] preprocessor tags in your os-foo.h file (you may
 * have to undef them first).
 * Scott Presnell, srp@cgl.ucsf.edu, Sat Nov 14 12:42:16 PST 1992
 */
#ifdef PRESET_AC

#ifdef ACREGMIN
	if (!nfs_args.acregmin) {
		nfs_args.acregmin = ACREGMIN;
#ifdef NFSMNT_ACREGMIN
		nfs_args.flags |= NFSMNT_ACREGMIN;
#endif /* NFSMNT_ACREGMIN */
	}
#endif /* ACREGMIN */

#ifdef ACREGMAX
	if (!nfs_args.acregmax) {
		nfs_args.acregmax = ACREGMAX;
#ifdef NFSMNT_ACREGMAX
		nfs_args.flags |= NFSMNT_ACREGMAX;
#endif /* NFSMNT_ACREGMAX */
	}
#endif /* ACREGMAX */

#ifdef ACDIRMIN
	if (!nfs_args.acdirmin) {
		nfs_args.acdirmin = ACDIRMIN;
#ifdef NFSMNT_ACDIRMIN
		nfs_args.flags |= NFSMNT_ACDIRMIN;
#endif /* NFSMNT_ACDIRMIN */
	}
#endif /* ACDIRMIN */

#ifdef ACDIRMAX
	if (!nfs_args.acdirmax) {
		nfs_args.acdirmax = ACDIRMAX;
#ifdef NFSMNT_ACDIRMAX
		nfs_args.flags |= NFSMNT_ACDIRMAX;
#endif /* NFSMNT_ACDIRMAX */
	}
#endif /* ACDIRMAX */

#endif /* PRESET_AC */

#ifdef MNTOPT_PRIVATE		/* mount private, single-client tree */
	if (hasmntopt(&mnt, MNTOPT_PRIVATE) != NULL)
		nfs_args.flags |= NFSMNT_PRIVATE;
#endif /* MNTOPT_PRIVATE */

#ifdef MNTOPT_SYMTTL		/* symlink cache time-to-live */
	if (nfs_args.symttl = hasmntval(&mnt, MNTOPT_SYMTTL))
		nfs_args.flags |= NFSMNT_SYMTTL;
#endif /* MNTOPT_SYMTTL */

#ifdef NFSMNT_PGTHRESH
	if (nfs_args.pg_thresh = hasmntval(&mnt, "pgthresh"))
		nfs_args.flags |= NFSMNT_PGTHRESH;
#endif /* NFSMNT_PGTHRESH */

#ifdef __svr4__
	nfs_args.addr = &nb;	/* fill in some automatic space */
#endif /* __svr4__ */
	NFS_SA_DREF(nfs_args, fs->fs_ip);

	flags = compute_mount_flags(&mnt);

#ifdef NFSMNT_NOCTO
	if (hasmntopt(&mnt, "nocto") != NULL)
		nfs_args.flags |= NFSMNT_NOCTO;
#endif /* NFSMNT_NOCTO */

#ifdef HAS_TCP_NFS
	if (hasmntopt(&mnt, "tcp") != NULL)
		nfs_args.sotype = SOCK_STREAM;
#endif /* HAS_TCP_NFS */


#ifdef ULTRIX_HACK
	/*
	 * Ultrix passes the flags argument as part of the
	 * mount data structure, rather than using the
	 * flags argument to the system call.  This is
	 * confusing...
	 */
	if (!(nfs_args.flags & NFSMNT_PGTHRESH)) {
		nfs_args.pg_thresh = 64; /* 64k - XXX */
		nfs_args.flags |= NFSMNT_PGTHRESH;
	}
	nfs_args.gfs_flags = flags;
	flags &= M_RDONLY;
	if (flags & M_RDONLY)
		nfs_args.flags |= NFSMNT_RONLY;
#ifdef	NFSMNT_NOAC
	/* YUCK YUCK YUCK !!!!!
	 *
	 * Ultrix 3 doesn't have acregmin etc ...
	 * Nor does it have NFSMNT_NOAC ......
	 *
	 * SO: use NFSMNT_NOAC and conditional ...
	 *
	 * pb@cl.cam.ac.uk 93/02/11
	 */
	 
	if (nfs_args.acregmin <= 0)
		nfs_args.acregmin = ACREGMIN;
	if (nfs_args.acregmax <= 0)
		nfs_args.acregmax = ACREGMAX;
	if (nfs_args.acdirmin <= 0)
		nfs_args.acdirmin = ACDIRMIN;
	if (nfs_args.acdirmax <= 0)
		nfs_args.acdirmax = ACDIRMAX;
#endif
#endif /* ULTRIX_HACK */

	error = mount_fs(&mnt, flags, (caddr_t) &nfs_args, retry, type);
	free(xopts);
#ifdef __svr4__
	free_knetconfig(nfs_args.knconf);
#endif /* __svr4__ */
	return error;
}

static int mount_nfs(dir, fs_name, opts, mf)
char *dir;
char *fs_name;
char *opts;
mntfs *mf;
{
#ifdef notdef
	int error;
	struct fhstatus fhs;
	char *colon;

	if (!(colon = strchr(fs_name, ':')))
		return ENOENT;

#ifdef DEBUG
	dlog("locating fhandle for %s", fs_name);
#endif /* DEBUG */
	error = prime_nfs_fhandle_cache(colon+1, mf->mf_server, &fhs, (voidp) 0);

	if (error)
		return error;

	return mount_nfs_fh(&fhs, dir, fs_name, opts, mf);
#endif
	if (!mf->mf_private) {
		plog(XLOG_ERROR, "Missing filehandle for %s", fs_name);
		return EINVAL;
	}

	return mount_nfs_fh((struct fhstatus *) mf->mf_private, dir, fs_name, opts, mf);
}

static int nfs_fmount(mf)
mntfs *mf;
{
	int error;

	error = mount_nfs(mf->mf_mount, mf->mf_info, mf->mf_mopts, mf);

#ifdef DEBUG
	if (error) {
		errno = error;
		dlog("mount_nfs: %m");
	}
#endif /* DEBUG */
	return error;
}

static int nfs_fumount(mf)
mntfs *mf;
{
	int error = UMOUNT_FS(mf->mf_mount);

	/*
	 * Here is some code to unmount 'restarted' file systems.
	 * The restarted file systems are marked as 'nfs', not
	 * 'host', so we only have the map information for the
	 * the top-level mount.  The unmount will fail (EBUSY)
	 * if there are anything else from the NFS server mounted
	 * below the mount-point.  This code checks to see if there
	 * is anything mounted with the same prefix as the
	 * file system to be unmounted ("/a/b/c" when unmounting "/a/b").
	 * If there is, and it is a 'restarted' file system, we unmount
	 * it.  
	 * Added by Mike Mitchell, mcm@unx.sas.com, 09/08/93
	 */
	if (error == EBUSY) {
		mntfs *new_mf;
		int len = strlen(mf->mf_mount);
		int didsome = 0;
		extern qelem mfhead;

		ITER(new_mf, mntfs, &mfhead) {
			if (new_mf->mf_ops != mf->mf_ops  ||
				new_mf->mf_refc > 1  ||
				mf == new_mf ||
				(new_mf->mf_flags & (MFF_MOUNTED|MFF_UNMOUNTING|MFF_RESTART) == (MFF_MOUNTED|MFF_RESTART)))
				continue;
			if (strncmp(mf->mf_mount, new_mf->mf_mount, len) == 0  && 
				new_mf->mf_mount[len] == '/') {
				(void) UMOUNT_FS(new_mf->mf_mount);
				didsome = 1;
			}
		}
		if (didsome)
			error = UMOUNT_FS(mf->mf_mount);
	}

	if (error)
		return error;

	return 0;
}

static void nfs_umounted(mp)
am_node *mp;
{
#ifdef INFORM_MOUNTD
	/*
	 * Don't bother to inform remote mountd
	 * that we are finished.  Until a full
	 * track of filehandles is maintained
	 * the mountd unmount callback cannot
	 * be done correctly anyway...
	 */

	mntfs *mf = mp->am_mnt;
	fserver *fs;
	char *colon, *path;

	if (mf->mf_error || mf->mf_refc > 1)
		return;

	fs = mf->mf_server;

	/*
	 * Call the mount daemon on the server to
	 * announce that we are not using the fs any more.
	 *
	 * This is *wrong*.  The mountd should be called
	 * when the fhandle is flushed from the cache, and
	 * a reference held to the cached entry while the
	 * fs is mounted...
	 */
	colon = path = strchr(mf->mf_info, ':');
	if (fs && colon) {
		fh_cache f;
#ifdef DEBUG
		dlog("calling mountd for %s", mf->mf_info);
#endif /* DEBUG */
		*path++ = '\0';
		f.fh_path = path;
		f.fh_sin = *fs->fs_ip;
		f.fh_sin.sin_port = (u_short) 0;
		f.fh_fs = fs;
		f.fh_id = 0;
		f.fh_error = 0;
		(void) prime_nfs_fhandle_cache(colon+1, mf->mf_server, (struct fhstatus *) 0, (voidp) mf);
		(void) call_mountd(&f, MOUNTPROC_UMNT, (fwd_fun) 0, (voidp) 0);
		*colon = ':';
	}
#endif /* INFORM_MOUNTD */

#ifdef KICK_KERNEL
	/* This should go into the mainline code, not in nfs_ops... */

	/*
	 * Run lstat over the underlying directory in
	 * case this was a direct mount.  This will
	 * get the kernel back in sync with reality.
	 */
	if (mp->am_parent && mp->am_parent->am_path &&
	    STREQ(mp->am_parent->am_mnt->mf_ops->fs_type, "direct")) {
		struct stat stb;
		int pid;
		if ((pid = background()) == 0) {
			if (lstat(mp->am_parent->am_path, &stb) < 0) {
				plog(XLOG_ERROR, "lstat(%s) after unmount: %m", mp->am_parent->am_path);
#ifdef DEBUG
			} else {
				dlog("hack lstat(%s): ok", mp->am_parent->am_path);
#endif /* DEBUG */
			}
			_exit(0);
		}
	}
#endif /* KICK_KERNEL */
}

/*
 * Network file system
 */
am_ops nfs_ops = {
	"nfs",
	nfs_match,
	nfs_init,
	auto_fmount,
	nfs_fmount,
	auto_fumount,
	nfs_fumount,
	efs_lookuppn,
	efs_readdir,
	0, /* nfs_readlink */
	0, /* nfs_mounted */
	nfs_umounted,
	find_nfs_srvr,
	FS_MKMNT|FS_BACKGROUND|FS_AMQINFO
};

#endif /* HAS_NFS */
