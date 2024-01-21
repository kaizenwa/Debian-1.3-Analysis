/*
 * nfsd		This program handles RPC "NFS" data requests.
 *
 * Usage:	[rpc.]nfsd [-Fhnprv] [-f authfile] [-d debugfac]
 *
 * Authors:	Mark A. Shand, May 1988
 *		Donald J. Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Eric Kasten, <tigger@tigger.cl.msu.edu>
 *		Olaf Kirch, <okir@monad.swb.de>
 *
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

#include "nfsd.h"
#include <rpc/pmap_clnt.h>
#include "getopt.h"
#include "fsusage.h"
#include "rpcmisc.h"
#ifdef __linux__ /* XXX - MvS: for UNIX sockets. */
#  include <sys/un.h>
#endif

#define MULTIPLE_SERVERS

/* Flags for auth_fh */
#define CHK_READ	0
#define CHK_WRITE	1
#define CHK_NOACCESS	2

/* Make larger reads possible. Without crashing the machine :-) */
#undef NFS_MAXDATA
#define NFS_MAXDATA	(16 * 1024)

static char iobuf[NFS_MAXDATA];
static char pathbuf[NFS_MAXPATHLEN + NFS_MAXNAMLEN + 1];
static char pathbuf_1[NFS_MAXPATHLEN + NFS_MAXNAMLEN + 1];

extern char version[];
static char *program_name;

/*
 * Option table
 */
static struct option longopts[] =
{
	{ "debug", required_argument, 0, 'd' },
	{ "foreground", 0, 0, 'F' },
	{ "exports-file", required_argument, 0, 'f' },
	{ "help", 0, 0, 'h' },
	{ "allow-non-root", 0, 0, 'n' },
	{ "port", required_argument, 0, 'P' },
	{ "promiscuous", 0, 0, 'p' },
	{ "re-export", 0, 0, 'r', },
	{ "synchronous-writes", 0, 0, 's' },
	{ "no-spoof-trace", 0, 0, 't' },
	{ "version", 0, 0, 'v' },
	{ "no-cross-mounts", 0, 0, 'x' },
	{ NULL, 0, 0, 0 }
};

/*
 * Table of supported versions
 */
static int		nfsd_versions[] = {
	NFS_VERSION,
	0
};

nfs_client		*nfsclient = NULL;	/* the current client */
nfs_mount		*nfsmount = NULL;	/* the current mount point */
int			need_reinit = 0;	/* SIGHUP handling */
int			need_flush = 0;		/* flush fh cache */
int			read_only = 0;		/* Global ro forced */
int			cross_mounts = 1;	/* Transparently cross mnts */

static _PRO (nfsstat build_path, (struct svc_req *rqstp, char *buf,
				diropargs * da, int flags)		);
static _PRO (fhcache *auth_fh, (struct svc_req *rqstp, nfs_fh *fh, 
				nfsstat *statp, int flags)		);
static _PRO (void usage, (FILE *, int)					);
static _PRO (void terminate, (void)					);
static _PRO (RETSIGTYPE sigterm, (int sig)				);
#ifdef SUPPORT_CDF
static _PRO (char *cdf_translate, (char *tag)				);
#endif


#ifdef CALL_PROFILING
extern _PRO (void dump_stats, (int sig)					);
#endif

/*
 * auth_fh
 *
 * This function authenticates the file handle provided by the client.
 * It also takes care of caching the client and mount point structures
 * in the fh cache entry, although this may not be a huge benefit.
 */
static fhcache *auth_fh(rqstp, fh, statp, flags)
struct svc_req *rqstp;
nfs_fh *fh;
nfsstat *statp;
int flags;
{
	static int total = 0, cached = 0;
	fhcache *fhc;

	/* Try to map FH. If not cached, reconstruct path with root priv */
	if ((fhc = fh_find((svc_fh *)fh, FHFIND_FEXISTS)) == NULL) {
		*statp = NFSERR_STALE;
		return NULL;
	}

	/* Try to retrieve last client who accessed this fh */
	if (nfsclient == NULL) {
		struct in_addr	caddr;

		caddr = svc_getcaller(rqstp->rq_xprt)->sin_addr;
		if (fhc->last_clnt != NULL &&
		    fhc->last_clnt->clnt_addr.s_addr == caddr.s_addr) {
			nfsclient = fhc->last_clnt;
		} else if ((nfsclient = auth_clnt(rqstp)) == NULL) {
			*statp = NFSERR_ACCES;
			return NULL;
		}
	}

	if (fhc->last_clnt == nfsclient) {
		nfsmount = fhc->last_mount; /* get cached mount point */
		cached++;
	} else {
		nfsmount = auth_path(nfsclient, rqstp, fhc->path);
		if (nfsmount == NULL) {
			*statp = NFSERR_ACCES;
			return NULL;
		}
		fhc->last_clnt = nfsclient;
		fhc->last_mount = nfsmount;
	}
	total++;
	/*
	if (total % 1000 == 0)
		Dprintf(D_FHCACHE, "ratio of cached client ptrs %4.1f%%\n",
			100 * (double) cached / total);
	 */

	if (nfsmount->o.noaccess &&
	    ((flags & CHK_NOACCESS) || strcmp(nfsmount->path, fhc->path))) {
		struct in_addr	addr = svc_getcaller(rqstp->rq_xprt)->sin_addr;
		Dprintf(L_WARNING, "client %s tried to access %s (noaccess)\n",
				inet_ntoa(addr), fhc->path);
		*statp = NFSERR_ACCES;
		return NULL;
	}

	if ((flags & CHK_WRITE) && (nfsmount->o.read_only || read_only)) {
		*statp = NFSERR_ROFS;
		return NULL;
	}

	auth_user(nfsmount, rqstp);

	*statp = NFS_OK;
	return fhc;
}

static inline nfsstat build_path(rqstp, buf, da, flags)
struct svc_req *rqstp;
char *buf;
diropargs *da;
int flags;
{
	fhcache *fhc;
	nfsstat status;
	char *path = buf, *sp;

	if ((fhc = auth_fh(rqstp, &(da->dir), &status, flags)) == NULL)
		return status;

	sp = fhc->path;

	while (*sp)		/* strcpy(buf, fhc->path); */
		*buf++ = *sp++;
	*buf++ = '/';		/* strcat(buf, "/");  */
	sp = da->name;
	while (*sp) {		/* strcat(pathbuf, argp->where.name); */
		if (*sp == '/')
			return NFSERR_INVAL;
		*buf++ = *sp++;
	}
	*buf = '\0';

	if (strlen(path) > NFS_MAXPATHLEN)
		return NFSERR_NAMETOOLONG;

	if ((nfsmount = auth_path(nfsclient, rqstp, path)) == NULL) {
		return NFSERR_ACCES;
	}
	auth_user(nfsmount, rqstp);

	return (NFS_OK);
}

/*
 * The "wrappers" of the following functions came from `rpcgen -l nfs_prot.x`.
 * This normally generates the client routines, but it provides nice
 * prototypes for the server routines also.
 */
int nfsd_nfsproc_null_2(argp, rqstp)
void *argp;
struct svc_req	*rqstp;
{
	return (0);
}

int nfsd_nfsproc_getattr_2(argp, rqstp)
nfs_fh *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	fhcache *fhc;

	fhc = auth_fh(rqstp, argp, &status, CHK_READ);
	if (fhc == NULL)
		return status;

	return (fhc_getattr(fhc, &result.attrstat.attrstat_u.attributes,
						NULL, rqstp));
}

int nfsd_nfsproc_setattr_2(argp, rqstp)
sattrargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	fhcache *fhc;
	char *path;
	struct stat buf;

	fhc = auth_fh(rqstp, &(argp->file), &status, CHK_WRITE | CHK_NOACCESS);
	if (fhc == NULL)
		return status;
	path = fhc->path;

	errno = 0;
	/* Stat the file first and only change fields that are different. */
	if (lstat(path, &buf) < 0)
		goto failure;

	status = setattr(path, &argp->attributes, &buf, rqstp, SATTR_ALL);
	if (status != NFS_OK)
		return status;
	return (fhc_getattr(fhc, &(result.attrstat.attrstat_u.attributes),
						&buf, rqstp));

failure:
	return (nfs_errno());
}

int nfsd_nfsproc_root_2(argp, rqstp)
void *argp;
struct svc_req	*rqstp;
{
	return (0);
}

int nfsd_nfsproc_lookup_2(argp, rqstp)
diropargs *argp;
struct svc_req	*rqstp;
{
	diropokres	*dp = &result.diropres.diropres_u.diropres;
	fhcache		*fhc;
	nfsstat		status;
	struct stat	sbuf;
	struct stat	*sbp = &sbuf;

	/* Must authenticate dir FH to set fsuid/fsgid. Thanks to
	 * Stig Venaas for his bug report.
	 */
	if (auth_fh(rqstp, &(argp->dir), &status, CHK_READ) == NULL)
		return status;

#ifdef SUPPORT_CDF
	/* This is just some experimental support for context-dependent
	 * files for the benefit of administrators who have to maintain
	 * a pool of diskless clients */
	if (!strncmp(argp->name, "cdf:", 4)) {
		char		*oldname = argp->name;

		if ((argp->name = cdf_translate(argp->name + 4)) != NULL) {
			status = fh_compose(argp, &(dp->file), &sbp, -1, -1);
		} else
			status = NFSERR_NOENT;
		if (status == NFSERR_NOENT) {
			argp->name = "default";
			status = fh_compose(argp, &(dp->file), &sbp, -1, -1);
		}
		argp->name = oldname;
	} else
#endif
	status = fh_compose(argp, &(dp->file), &sbp, -1, -1);
	if (status != NFS_OK)
		return status;

	fhc = auth_fh(rqstp, &(dp->file), &status, CHK_READ);
	if (fhc == NULL)
		return status;

	status = fhc_getattr(fhc, &(dp->attributes), sbp, rqstp);
	if (status == NFS_OK)
		Dprintf(D_CALL, "\tnew_fh = %s\n", fh_pr(&(dp->file)));

	return (status);
}

#ifdef SUPPORT_CDF
static char *
cdf_translate(char *tag)
{
	static char	buffer[512];

	if (tag[0] == 'u' && !strcmp(tag, "uid"))
		sprintf(buffer, "%d", auth_uid);
	else if (tag[0] == 'h' && !strcmp(tag, "hostaddr"))
		sprintf(buffer, "%s", inet_ntoa(nfsclient->clnt_addr));
	else
		return NULL;
	return buffer;
}
#endif

int nfsd_nfsproc_readlink_2(argp, rqstp)
nfs_fh *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	fhcache *fhc;
	char *path;
	int cc;

	fhc = auth_fh(rqstp, argp, &status, CHK_READ | CHK_NOACCESS);
	if (fhc == NULL)
		return status;
	path = fhc->path;

	errno = 0;
	if ((cc = readlink(path, pathbuf, NFS_MAXPATHLEN)) < 0) {
		Dprintf(D_CALL, " >>> %s\n", strerror(errno));
		return (nfs_errno());
	}
	status = NFS_OK;
	pathbuf[cc] = '\0';	/* readlink() doesn't null terminate!! */
	result.readlinkres.readlinkres_u.data = pathbuf;

	if (nfsmount->o.link_relative && pathbuf[0] == '/') {
		/*
		 * We've got an absolute (locally) pathname, and we should
		 * translate to a relative pathname for the client.  We do
		 * this by prepending the correct number of "../"es to the
		 * path. This cannot work if the client does not mount the
		 * specified subtree of the filesystem.
		 */
		int slash_cnt = 0;
		char *p, *q;

		/* Count how many directories down we are. */
		for (p = path + 1; *p != '\0'; p++)
			if (*p == '/')
				slash_cnt++;

		/*
		 * Ok, now we are finished with the orginal file `path'
		 * and will only deal with the link target.
		 */
		p = &pathbuf[cc];	/* Point to the end and calculate */
		if (slash_cnt == 0)
			q = p + 1;	/* the extra space taken by a	*/
		else			/* prepended '.'  		*/
			q = p + 3 * slash_cnt - 1;	/* or '../.../..' */

		if (q >= pathbuf + NFS_MAXPATHLEN) {
			Dprintf(D_CALL, " [[NAME TOO LONG!!]]\n");
			return (NFSERR_NAMETOOLONG);
		} else {
			/* Add some space at the beginning of the string. */
			while (p >= pathbuf)
				*q-- = *p--;

			if (slash_cnt == 0)
				pathbuf[0] = '.';
			else {
				/*
				 * This overwrites the leading '/' on the
				 * last iteration.
				 */
				for (p = pathbuf; slash_cnt > 0; slash_cnt--) {
					*p++ = '.';
					*p++ = '.';
					*p++ = '/';
				}
			}
		}
	}
	Dprintf(D_CALL, " %s\n", result.readlinkres.readlinkres_u.data);
	return (NFS_OK);
}

int nfsd_nfsproc_read_2(argp, rqstp)
readargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	fhcache *fhc;
	readokres *res = &result.readres.readres_u.reply;
	int	fd, len;

	fhc = auth_fh(rqstp, &(argp->file), &status, CHK_READ | CHK_NOACCESS);
	if (fhc == NULL)
		return status;

	if ((fd = fh_fd(fhc, &status, O_RDONLY)) < 0) {
		return ((int) status);
	}
	errno = 0;
	(void) lseek(fd, (long) argp->offset, L_SET);
	if (!errno) {
		res->data.data_val = iobuf;
		if ((len = argp->count) > NFS_MAXDATA)
			len = NFS_MAXDATA;
		res->data.data_len = read(fd, iobuf, len);
	}
	fd_inactive(fd);
	if (errno)
		return (nfs_errno());
	return (fhc_getattr(fhc, &(res->attributes), NULL, rqstp));
}

int nfsd_nfsproc_writecache_2(argp, rqstp)
void *argp;
struct svc_req	*rqstp;
{
	return (0);
}

int nfsd_nfsproc_write_2(argp, rqstp)
writeargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	fhcache *fhc;
	int fd;

	fhc = auth_fh(rqstp, &(argp->file), &status, CHK_WRITE | CHK_NOACCESS);
	if (fhc == NULL)
		return status;

	if ((fd = fh_fd(fhc, &status, O_WRONLY)) < 0) {
		return ((int) status);
	}
	errno = 0;
	(void) lseek(fd, (long) argp->offset, L_SET);
	if (errno == 0) {	/* We should never fail. */
		if (write(fd, argp->data.data_val, argp->data.data_len) !=
		    argp->data.data_len) {
			Dprintf(D_CALL, " Write failure, errno is %d.\n", errno);
		}
	}
	fd_inactive(fd);
	if (errno)
		return (nfs_errno());
	return (fhc_getattr(fhc, &(result.attrstat.attrstat_u.attributes),
							NULL, rqstp));
}

/* This used to be O_RDWR, but O_WRONLY is correct */
#define CREATE_OMODE O_WRONLY

int nfsd_nfsproc_create_2(argp, rqstp)
createargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	diropokres *res;
	int tmpfd, flags;
	struct stat sbuf;
	struct stat *sbp = &sbuf;
	int is_borc;
	int dev;
	int exists;
#ifdef __linux__ /* XXX - MvS: to create UNIX sockets. */
	struct sockaddr_un sa;
	int s;
#endif

	/* We get the access status and file handle here, but check the
	 * status later. This is to let an "echo >/dev/null" from SunOS
	 * clients succeed on RO-filesystems.
	 */
	status = build_path(rqstp, pathbuf, &argp->where,
					CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK && status != NFSERR_ROFS)
		return ((int) status);
	Dprintf(D_CALL, "\tfullpath='%s'\n", pathbuf);
	errno = 0;

	exists = lstat(pathbuf, &sbuf) == 0;

	/* Compensate for a really bizarre bug in SunOS derived clients. */
	if ((argp->attributes.mode & S_IFMT) == 0) {
		argp->attributes.mode |= exists
			? (sbuf.st_mode & S_IFMT) : S_IFREG;
		if (!S_ISREG(argp->attributes.mode)) {
			/* This branch is excuted only if the file exists
			 * and is a special file. */
			status = NFS_OK;
			argp->attributes.size = sbuf.st_rdev;
		}
	}
	if (status != NFS_OK)
		return ((int)status);

	/* First handle any unusual file-types. */
	if (!S_ISREG(argp->attributes.mode)) {
		if (S_ISBLK(argp->attributes.mode)
		    || S_ISCHR(argp->attributes.mode)) {
			is_borc = 1;
#if 0
			/* This is probably better than just using
			   the size field by itself, but not by much. */
			dev = makedev(((argp->attributes.size >> 8) & 0xff),
			    (argp->attributes.size & 0xff));
#else
			/* We must not assume anything about the layout of
			 * the client's dev_t. Either the value fits into
			 * our dev_t or not...
			 */
			dev = (dev_t) argp->attributes.size;
			if (dev != argp->attributes.size)
				return NFSERR_INVAL;
#endif

			/* MvS: Some clients use chardev 0xFFFF for a FIFO. */
			if (S_ISCHR(argp->attributes.mode) && dev == 0xFFFF) {
				is_borc = 0;
				dev = 0;
				argp->attributes.mode &= ~S_IFMT;
				argp->attributes.mode |= S_IFIFO;
			}
		}
		else {
			is_borc = 0;
			dev = 0;
		}
		/* mknod will fail for EEXIST, we'll let it succeed. */
		if (!exists) {
#ifdef __linux__ /* XXX - MvS */
			/* Can't make UNIX sockets with mknod. */
			if (S_ISSOCK(argp->attributes.mode)) {
			  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) < 0)
			          return(nfs_errno());
			  sa.sun_family = AF_UNIX;
			  strncpy(sa.sun_path, pathbuf, sizeof(sa.sun_path));
			  if (bind(s, (struct sockaddr *)&sa, sizeof(sa)) < 0) {
				(void) close(s);
			        return(nfs_errno());
			  }
			  (void) close(s);
			} else
#endif
			if (mknod(pathbuf, argp->attributes.mode, dev) < 0)
				return (nfs_errno());
			if (stat(pathbuf, &sbuf) < 0)
				return (nfs_errno());
		}
		else {
			/* But make sure it's the same kind of special file. */
			if ((argp->attributes.mode & S_IFMT)
			    != (sbuf.st_mode & S_IFMT))
				return (NFSERR_EXIST);
			/* And that the major and minor numbers agree. */
			if (is_borc && dev != sbuf.st_rdev)
				return (NFSERR_EXIST);
		}
		tmpfd = -1;
	}
	else {
		flags = (argp->attributes.size == 0 ?
			CREATE_OMODE | O_TRUNC : CREATE_OMODE);
		if (!exists)
			flags |= O_CREAT;
		tmpfd = path_open(pathbuf, flags, 
				argp->attributes.mode & ~S_IFMT);
		if (tmpfd < 0)
			goto failure;
		(void) fstat(tmpfd, &sbuf);
	}

	/* creat() is equivalent to open(..., O_CREAT|O_TRUNC|O_WRONLY) */
	if (!exists) {
#ifndef ALLOW_SGIDDIR
		argp->attributes.gid = -1;
#endif
		/* Note: we ignore the size attribute because some clients
		 * create files with mode 0444. Since the file didn't exist
		 * previously, its length is zero anyway.
		 */
		status = setattr(pathbuf, &argp->attributes, &sbuf,
					rqstp, SATTR_ALL & ~SATTR_SIZE);
	} else {
		status = setattr(pathbuf, &argp->attributes, &sbuf,
					rqstp, SATTR_SIZE);
	}
	if (status != NFS_OK)
		return status;

	res = &result.diropres.diropres_u.diropres;
	status = fh_compose(&(argp->where), &(res->file), &sbp,
		tmpfd, CREATE_OMODE);
	if (status != NFS_OK)
		goto failure;
	status = fh_getattr(&(res->file), &(res->attributes), sbp, rqstp);
	if (status != NFS_OK) {
		tmpfd = -1;	/* fd already stored in fh cache */
		goto failure;
	}
	Dprintf(D_CALL, "\tnew_fh = %s\n", fh_pr(&(res->file)));
	return (status);

failure:
	Dprintf(D_CALL, "\tcreate failed -- errno returned=%d.\n", errno);
	if (tmpfd != -1)
		close(tmpfd);
	return (errno? nfs_errno(): status);
}

#undef CREATE_OMODE

int nfsd_nfsproc_remove_2(argp, rqstp)
diropargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;

	status = build_path(rqstp, pathbuf, argp, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);

	Dprintf(D_CALL, "\tfullpath='%s'\n", pathbuf);

	/* Remove the file handle from our cache. */
	fh_remove(pathbuf);

	if (unlink(pathbuf) != 0)
		return (nfs_errno());
	else
		return (NFS_OK);
}

int nfsd_nfsproc_rename_2(argp, rqstp)
renameargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;

	status = build_path(rqstp, pathbuf, &argp->from, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);
	status = build_path(rqstp, pathbuf_1, &argp->to, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);

	Dprintf(D_CALL, "\tpathfrom='%s' pathto='%s'\n", pathbuf, pathbuf_1);

	/* Remove any file handle from our cache. */
	fh_remove(pathbuf);
	fh_remove(pathbuf_1);

	if (rename(pathbuf, pathbuf_1) != 0)
		return (nfs_errno());

	return (NFS_OK);
}

/* For now, we disallow hardlinks between different volumes for
 * security reasons. If we tried harder, we might be able to 
 * support them, but I'm not sure if it's worth it...
 */
int nfsd_nfsproc_link_2(argp, rqstp)
linkargs *argp;
struct svc_req	*rqstp;
{
	nfs_mount *mountp1;
	nfsstat status;
	fhcache *fhc;
	char *path;

	fhc = auth_fh(rqstp, &(argp->from), &status, CHK_WRITE | CHK_NOACCESS);
	if (fhc == NULL)
		return status;
	mountp1 = nfsmount;
	path = fhc->path;

	status = build_path(rqstp, pathbuf_1, &argp->to, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);

	Dprintf(D_CALL, "\tpathfrom='%s' pathto='%s'\n", path, pathbuf_1);

	if (nfsmount != mountp1) {
		Dprintf(D_CALL, "\tdenied link between different exports\n");
		return NFSERR_ACCES;
	}

	if (link(path, pathbuf_1) != 0)
		return (nfs_errno());
	return (NFS_OK);
}

int nfsd_nfsproc_symlink_2(argp, rqstp)
symlinkargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;

	status = build_path(rqstp, pathbuf, &argp->from, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);

	Dprintf(D_CALL, "\tstring='%s' filename='%s'\n", argp->to, pathbuf);

	if (symlink(argp->to, pathbuf) != 0)
		return (nfs_errno());

	/*
         * NFS version 2 documentation says "On UNIX servers the
	 * attributes are never used...". IMHO, utimes and maybe even
	 * owner may still matter.
         */
#ifndef ALLOW_SGIDDIR
	argp->attributes.gid = -1;
#endif
	status = setattr(pathbuf, &argp->attributes, NULL, rqstp,
				SATTR_CHOWN|SATTR_UTIMES);

	return status;
}

int nfsd_nfsproc_mkdir_2(argp, rqstp)
createargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	struct stat sbuf;
	diropokres *res;
	struct stat *sbp = &sbuf;

	status = build_path(rqstp, pathbuf, &argp->where, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);

	Dprintf(D_CALL, "\tfullpath='%s'\n", pathbuf);

	if (mkdir(pathbuf, argp->attributes.mode) != 0)
		return (nfs_errno());

	res = &result.diropres.diropres_u.diropres;
	status = fh_compose(&(argp->where), &(res->file), &sbp, -1, -1);
	if (status != NFS_OK)
		return ((int) status);

#ifndef ALLOW_SGIDDIR
	argp->attributes.gid = -1;
#endif
	/* Inherit setgid bit from directory */
	argp->attributes.mode |= (sbuf.st_mode & S_ISGID);
	status = setattr(pathbuf, &argp->attributes, &sbuf, rqstp,
				SATTR_CHOWN|SATTR_CHMOD|SATTR_UTIMES);
	if (status != NFS_OK)
		return status;

	/* Note that the spb buffer is now invalid! */
	status = fh_getattr(&(res->file), &(res->attributes), NULL, rqstp);
	if (status == NFS_OK)
		Dprintf(D_CALL, "\tnew_fh = %s\n", fh_pr(&(res->file)));
	return ((int) status);
}

int nfsd_nfsproc_rmdir_2(argp, rqstp)
diropargs *argp;
struct svc_req	*rqstp;
{
	nfsstat status;

	status = build_path(rqstp, pathbuf, argp, CHK_WRITE | CHK_NOACCESS);
	if (status != NFS_OK)
		return ((int) status);

	Dprintf(D_CALL, "\tfullpath='%s'\n", pathbuf);

	/* Remove that file handle from our cache. */
	fh_remove(pathbuf);

	if (rmdir(pathbuf) != 0)
		return (nfs_errno());

	return (NFS_OK);
}

/* More Mark Shand code. */
static int dpsize(dp)
struct dirent *dp;
{
#define DP_SLOP	16
#define MAX_E_SIZE sizeof(entry) + NAME_MAX + DP_SLOP
	return (sizeof(entry) + NLENGTH(dp) + DP_SLOP);
}

int nfsd_nfsproc_readdir_2(argp, rqstp)
readdirargs *argp;
struct svc_req	*rqstp;
{
	static readdirres oldres;
	entry **e;
	__u32 dloc;
	DIR *dirp;
	struct dirent *dp;
	struct stat sbuf;
	int res_size;
	fhcache *h;
	nfsstat status;
	int hideit;

	/* Free the previous result, since it has 'malloc'ed strings.  */
	xdr_free((xdrproc_t)xdr_readdirres, (caddr_t) & oldres);

	h = auth_fh(rqstp, &(argp->dir), &status, CHK_READ);
	if (h == NULL)
		return status;
	hideit = ((!re_export && (h->flags & FHC_NFSMOUNTED))
			|| nfsmount->o.noaccess);

	/* This code is from Mark Shand's version */
	errno = 0;
	if (lstat(h->path, &sbuf) < 0 || !(S_ISDIR(sbuf.st_mode)))
		return (NFSERR_NOTDIR);
	if ((dirp = opendir(h->path)) == NULL)
		return ((errno ? nfs_errno() : NFSERR_NAMETOOLONG));

	res_size = 0;
	memcpy(&dloc, argp->cookie, sizeof(dloc));
	if (dloc != 0)
		seekdir(dirp, ntohl(dloc));
	e = &(result.readdirres.readdirres_u.reply.entries);
	while (((res_size + MAX_E_SIZE) < argp->count
		|| e == &(result.readdirres.readdirres_u.reply.entries))
	       && (dp = readdir(dirp)) != NULL) {
		if (hideit && strcmp(dp->d_name, ".") != 0
		    && strcmp(dp->d_name, "..") != 0) {
			dp = NULL;
			break;
		}
		*e = (entry *) xmalloc(sizeof(entry));
		(*e)->fileid = pseudo_inode(dp->d_ino, sbuf.st_dev);
		(*e)->name = xmalloc(NLENGTH(dp) + 1);
		strcpy((*e)->name, dp->d_name);
		dloc = htonl(telldir(dirp));
		memcpy(((*e)->cookie), &dloc, sizeof(nfscookie));
		e = &((*e)->nextentry);
		res_size += dpsize(dp);
	}
	*e = NULL;
	result.readdirres.readdirres_u.reply.eof = (dp == NULL);
	closedir(dirp);
	oldres = result.readdirres;
	return (result.readdirres.status);
}

/*
 * Only reports free space correctly for the filesystem that the
 * mount point is on.  Actually it will work fine for any file
 * handle (e.g. sub mounts) but the NFS spec calls for root_fh
 * to be used by the client when calling this.
 */
int nfsd_nfsproc_statfs_2(argp, rqstp)
nfs_fh *argp;
struct svc_req	*rqstp;
{
	nfsstat status;
	fhcache *fhc;
	char *path;
	struct fs_usage fs;

	fhc = auth_fh(rqstp, argp, &status, CHK_READ | CHK_NOACCESS);
	if (fhc == NULL)
		return status;
	path = fhc->path;

	if (get_fs_usage(path, NULL, &fs) < 0)
		return (nfs_errno());
	result.statfsres.status = NFS_OK;
	result.statfsres.statfsres_u.reply.tsize = 8*1024;
	result.statfsres.statfsres_u.reply.bsize = 512;
	result.statfsres.statfsres_u.reply.blocks = fs.fsu_blocks;
	result.statfsres.statfsres_u.reply.bfree = fs.fsu_bfree;
	result.statfsres.statfsres_u.reply.bavail = fs.fsu_bavail;

	return (NFS_OK);
}

int main(argc, argv)
int argc;
char *argv[];
{
	int c;
	char *auth_file = NULL;
	int foreground = 0;
	int nfsport = 0;
#ifdef MULTIPLE_SERVERS
	int ncopies = 0;
#endif

	program_name = argv[0];

	/* Parse the command line options and arguments. */
	opterr = 0;
	while ((c = getopt_long(argc, argv, "d:Ff:hnP:prtv", longopts, NULL)) != EOF)
		switch (c) {
		case 'h':
			usage(stdout, 0);
			break;
		case 'd':
			enable_logging(optarg);
			break;
		case 'F':
			foreground = 1;
			break;
		case 'f':
			auth_file = optarg;
			break;
		case 'n':
			allow_non_root = 1;
			break;
		case 'P':
			nfsport = atoi(optarg);
			if (nfsport <= 0) {
				fprintf(stderr, "nfsd: bad port number: %s\n",
					optarg);
				usage(stderr, 1);
			}
			break;
		case 'p':
			promiscuous = 1;
			break;
		case 'r':
			re_export = 1;
			break;
		case 't':
			trace_spoof = 0;
			break;
		case 'v':
			printf("%s\n", version);
			exit(0);
		case 'x':
			cross_mounts = 0;
			break;
		case 0:
			break;
		case '?':
		default:
			usage(stderr, 1);
		}

#ifdef MULTIPLE_SERVERS
	if (optind == argc-1 && isdigit(argv[optind][0])) {
		ncopies = atoi(argv[optind++]) - 1;
		if (ncopies < 0) {
			fprintf(stderr, 
			    "nfsd: illegal number of servers requested: %s\n",
						argv[optind]);
			exit (1);
		}
		if (foreground) {
			fprintf(stderr, "nfsd: warning: can run only "
					"one server in debug mode\n");
			ncopies = 0;
		}
	}
#endif

	/* No more arguments allowed. */
	if (optind != argc)
		usage(stderr, 1);

	/* Get the default NFS port */
	if (!nfsport) {
		struct servent	*sp;

		if (!(sp = getservbyname("nfs", "udp"))) {
			nfsport = NFS_PORT;
		} else {
			nfsport = ntohs(sp->s_port);
		}
	}

	/* Initialize logging. */
	log_open("nfsd", foreground);

	/* Initialize RPC stuff */
	rpc_init("nfsd", NFS_PROGRAM, nfsd_versions, nfs_dispatch,
				nfsport, NFS_MAXDATA);

	/* No more than 1 copy when run from inetd */
	if (_rpcpmstart && ncopies) {
		Dprintf(L_WARNING,
				"nfsd: warning: can run only "
				"one server in inetd mode\n");
		ncopies = 0;
	}

	/* Can't share writeable volumes yet */
	if (ncopies)
		read_only = 1;

	/* We first fork off a child. */
	if (!foreground) {
		if ((c = fork()) > 0)
			exit(0);
		if (c < 0) {
			Dprintf(L_FATAL, "nfsd: cannot fork: %s\n",
						strerror(errno));
		}

		/* No more logging to stderr */
		background_logging();

		/* Now we remove ourselves from the foreground. */
		close(0);
		close(1);
		close(2);
#ifdef HAVE_SETSID
		setsid();
#else
		{
			int fd;
	
			if ((fd = open("/dev/tty", O_RDWR)) >= 0) {
				ioctl(fd, TIOCNOTTY, (char *) NULL);
				close(fd);
			}
		}
#endif
	}

	/* Initialize the FH module. */
	fh_init();

	/* Initialize the AUTH module. */
	auth_init(auth_file);

#ifdef MULTIPLE_SERVERS
	/* Start multiple copies of the server */
	while (ncopies != 0) {
		Dprintf(D_GENERAL, "Forking server thread...\n");
		if ((c = fork()) < 0) {
			perror("fork");
		} else if (c == 0) {
			ncopies = 0;
		} else {
			ncopies--;
		}
	}
#endif

	/* Enable the LOG toggle with a signal. */
	signal(SIGUSR1, toggle_logging);
#ifdef CALL_PROFILING
	signal(SIGIOT, dump_stats);
#endif
	signal(SIGHUP, reinitialize);
	signal(SIGTERM, sigterm);
	atexit(terminate);

	/* Run the NFS server. */
	svc_run();

	Dprintf(L_ERROR, "Oh no Mr. Bill... nfs_server() returned!\n");
	exit(1);
}

static void usage(fp, n)
FILE *fp;
int n;
{
	fprintf(fp,"Usage: %s [-Fhnpv] [-d kind] [-f exports-file] [-P port]\n",
						program_name);
	fprintf(fp,"       [--debug kind] [--help] [--allow-non-root]\n");
	fprintf(fp,"       [--promiscuous] [--version] [--foreground]\n");
	fprintf(fp,"       [--exports-file=file] [--port port]\n");
	exit(n);
}

static RETSIGTYPE
sigterm(int sig)
{
	terminate();
	exit(1);
}

static void
terminate(void)
{
	rpc_exit(NFS_PROGRAM, nfsd_versions);
}

RETSIGTYPE reinitialize(sig)
{
	static volatile int	inprogress = 0;

	signal (SIGHUP, reinitialize);
	if (_rpcsvcdirty) {
		need_reinit = 1;
		return;
	}
	if (inprogress++)	/* Probably non-atomic. Yuck */
		return;
	fh_flush(1);
	auth_init(NULL);	/* auth_init saves the exports file name */
	inprogress = 0;
	need_reinit = 0;
}

