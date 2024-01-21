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
 * $Id: host_ops.c,v 5.2.2.2 1992/05/31 16:36:08 jsp Exp $
 *
 */

#include "am.h"

#ifdef HAS_HOST

#include "mount.h"
#include <sys/stat.h>

/*
 * NFS host file system.
 * Mounts all exported filesystems from a given host.
 * This has now degenerated into a mess but will not
 * be rewritten.  Amd 6 will support the abstractions
 * needed to make this work correctly.
 */

/*
 * Define HOST_RPC_UDP to use dgram instead of stream RPC.
 * Datagrams are generally much faster.
 */
/*#define	HOST_RPC_UDP*/

/*
 * Define HOST_MKDIRS to make Amd automatically try
 * to create the mount points.
 */
#define HOST_MKDIRS

/*
 * Determine the mount point
 */
#ifdef notdef
/* This was the old code, left for reference. -Erez */
#define MAKE_MNTPT(mntpt, ex, mf) \
{ \
	if (strcmp((ex)->ex_dir, "/") == 0) \
		strcpy((mntpt), (mf)->mf_mount); \
	else \
		sprintf((mntpt), "%s%s", (mf)->mf_mount, (ex)->ex_dir); \
}
#endif
/*
 * The next change we put in to better handle PCs.  This is a bit
 * disgusting, so you'd better sit down.  We change the MAKE_MNTPT macro to
 * look for exported file systems without a leading '/'.  If they don't have
 * a leading '/', we add one.  If the export is 'a:' through 'z:' (without a
 * leading slash), we change it to 'a%' (or b% or z%).  This allows the
 * entire PC disk to be mounted.
 * Added by Mike Mitchell, mcm@unx.sas.com, 09/08/93
 */
#define MAKE_MNTPT(mntpt, ex, mf)					      \
{									      \
	if ((ex)->ex_dir[0] == '/') {					      \
		if ((ex)->ex_dir[1] == 0)				      \
			strcpy((mntpt), (mf)->mf_mount);		      \
		else							      \
			sprintf((mntpt), "%s%s", (mf)->mf_mount, (ex)->ex_dir); \
	} else if ((ex)->ex_dir[0] >= 'a' &&				      \
			(ex)->ex_dir[0] <= 'z' &&			      \
			(ex)->ex_dir[1] == ':' &&			      \
			(ex)->ex_dir[2] == '/' &&			      \
			(ex)->ex_dir[3] == 0)				      \
		sprintf((mntpt), "%s/%c%%", (mf)->mf_mount, (ex)->ex_dir[0]); \
	else								      \
		sprintf((mntpt), "%s/%s", (mf)->mf_mount, (ex)->ex_dir);      \
}


/*
 * Execute needs the same as NFS plus a helper command
 */
static char *host_match P((am_opts *fo));
static char *host_match(fo)
am_opts *fo;
{
#ifdef HOST_EXEC
	if (!host_helper) {
		plog(XLOG_USER, "No host helper command given");
		return FALSE;
	}
#endif /* HOST_EXEC */

	/*
	 * Make sure rfs is specified to keep nfs_match happy...
	 */
	if (!fo->opt_rfs)
		fo->opt_rfs = "/";

	
	return (*nfs_ops.fs_match)(fo);
}

static int host_init(mf)
mntfs *mf;
{
	fserver *fs;
	u_short port;

	if (strchr(mf->mf_info, ':') == 0)
		return ENOENT;

	/*
	 * This is primarily to schedule a wakeup so that as soon
	 * as our fileserver is ready, we can continue setting up
	 * the host filesystem.  If we don't do this, the standard
	 * afs code will set up a fileserver structure, but it will
	 * have to wait for another nfs request from the client to come
	 * in before finishing.  Our way is faster since we don't have
	 * to wait for the client to resend its request (which could
	 * take a second or two).
	 */
	/*
	 * First, we find the fileserver for this mntfs and then call
	 * nfs_srvr_port with our mntfs passed as the wait channel.
	 * nfs_srvr_port will check some things and then schedule
	 * it so that when the fileserver is ready, a wakeup is done
	 * on this mntfs.   afs_cont() is already sleeping on this mntfs
	 * so as soon as that wakeup happens afs_cont() is called and
	 * this mount is retried.
	 */
	if (fs = mf->mf_server)
		/*
		 * We don't really care if there's an error returned.
		 * Since this is just to help speed things along, the
		 * error will get handled properly elsewhere.
		 */
		(void) nfs_srvr_port(fs, &port, (voidp) mf);

	return 0;
}

/*
 * Two implementations:
 * HOST_EXEC gets you the external version.  The program specified with
 * the -h option is called.  The external program is not published...
 * roll your own.
 *
 * Otherwise you get the native version.  Faster but makes the program
 * bigger.
 */

#ifndef HOST_EXEC

static bool_t
xdr_pri_free(xdr_args, args_ptr)
xdrproc_t xdr_args;
caddr_t args_ptr;
{
	XDR xdr;
	xdr.x_op = XDR_FREE;
	return ((*xdr_args)(&xdr, args_ptr));
}

static int do_mount P((fhstatus *fhp, char *dir, char *fs_name, char *opts, mntfs *mf));
static int do_mount(fhp, dir, fs_name, opts, mf)
fhstatus *fhp;
char *dir;
char *fs_name;
char *opts;
mntfs *mf;
{
	struct stat stb;
#ifdef DEBUG
	dlog("host: mounting fs %s on %s\n", fs_name, dir);
#endif /* DEBUG */
#ifdef HOST_MKDIRS
	(void) mkdirs(dir, 0555);
#endif /* HOST_MKDIRS */
	if (stat(dir, &stb) < 0 || (stb.st_mode & S_IFMT) != S_IFDIR) {
		plog(XLOG_ERROR, "No mount point for %s - skipping", dir);
		return ENOENT;
	}

	return mount_nfs_fh(fhp, dir, fs_name, opts, mf);
}

#ifdef NeXT
static int sortfun P((const void *a, const void *b));
static int sortfun(a, b)
const void *a,*b;
{
  return strcmp((*((exports *)a))->ex_dir, (*((exports *)b))->ex_dir);
}
#else
static int sortfun P((exports *a, exports *b));
static int sortfun(a, b)
exports *a,*b;
{
	return strcmp((*a)->ex_dir, (*b)->ex_dir);
}
#endif /* NeXT */

/*
 * Get filehandle
 */
static int fetch_fhandle P((CLIENT *client, char *dir, fhstatus *fhp));
static int fetch_fhandle(client, dir, fhp)
CLIENT *client;
char *dir;
fhstatus *fhp;
{
	struct timeval tv;
	enum clnt_stat clnt_stat;

	/*
	 * Pick a number, any number...
	 */
	tv.tv_sec = 20;
	tv.tv_usec = 0;

#ifdef DEBUG
	dlog("Fetching fhandle for %s", dir);
#endif /* DEBUG */
	/*
	 * Call the mount daemon on the remote host to
	 * get the filehandle.
	 */
	clnt_stat = clnt_call(client, MOUNTPROC_MNT, xdr_dirpath,
			      (SVC_IN_ARGS_TYPE) &dir, xdr_fhstatus,
			      (SVC_IN_ARGS_TYPE) fhp, tv);
	if (clnt_stat != RPC_SUCCESS) {
		extern char *clnt_sperrno();
		char *msg = clnt_sperrno(clnt_stat);
		plog(XLOG_ERROR, "mountd rpc failed: %s", msg);
		return EIO;
	}
	/*
	 * Check status of filehandle
	 */
	if (fhp->fhs_status) {
#ifdef DEBUG
		errno = fhp->fhs_status;
		dlog("fhandle fetch failed: %m");
#endif /* DEBUG */
		return fhp->fhs_status;
	}
	return 0;
}

/*
 * Scan mount table to see if something already mounted
 */
static int already_mounted P((mntlist *mlist, char*dir));
static int already_mounted(mlist, dir)
mntlist *mlist;
char *dir;
{
	mntlist *ml;

	for (ml = mlist; ml; ml = ml->mnext)
		if (strcmp(ml->mnt->mnt_dir, dir) == 0)
			return 1;
	return 0;
}

#ifdef __svr4__
/*
 * SVR4 VERSION:
 * Create an rpc client attached to the mount daemon. 
 */
static CLIENT *get_mount_client P((char *host, struct timeval *tv, int *sock));
static CLIENT *get_mount_client(host, tv, sock)
char *host;
struct timeval *tv;
int *sock;
{
  CLIENT *client;
  struct netbuf nb;
  struct netconfig *nc;
  struct sockaddr_in sin;

  nb.maxlen = sizeof(sin);
  nb.buf = (char *)&sin;

  /*
   * First try a TCP handler
   */

  /*
   * Find mountd address on TCP
   */
  if ((nc = getnetconfigent("tcp")) == NULL) {
    plog(XLOG_ERROR, "getnetconfig for tcp failed: %s", nc_sperror());
    goto tryudp;
  }

  if (!rpcb_getaddr(MOUNTPROG, MOUNTVERS, nc, &nb, host)) {
    /*
     * don't pring error messages here, since mountd might legitimately
     * serve udp only
     */
    goto tryudp;
  }

  /*
   * Create priviledged TCP socket
   */
  *sock = t_open(nc->nc_device, O_RDWR, 0);

  if (*sock < 0) {
    plog(XLOG_ERROR, "t_open %s: %m", nc->nc_device);
    goto tryudp;
  }
  if (bind_resv_port(*sock, (unsigned short *) 0) < 0)
    plog(XLOG_ERROR, "couldn't bind mountd socket to privileged port");

  if ((client = clnt_vc_create(*sock, &nb, MOUNTPROG, MOUNTVERS, 0, 0))
	== (CLIENT *) NULL)
  {
    plog(XLOG_ERROR, "clnt_vc_create failed");
    t_close(*sock);
    goto tryudp;
  }

  /* tcp succeeded */
#ifdef DEBUG
  dlog("get_mount_client: using tcp, port %d", sin.sin_port);
#endif /* DEBUG */
  return client;

 tryudp:
  /*
   * TCP failed so try UDP
   */

  /*
   * Find mountd address on UDP
   */
  if ((nc = getnetconfigent("udp")) == NULL) {
    plog(XLOG_ERROR, "getnetconfig for udp failed: %s", nc_sperror());
    return NULL;
  }

  if (!rpcb_getaddr(MOUNTPROG, MOUNTVERS, nc, &nb, host)) {
    plog(XLOG_ERROR, "%s",
	 clnt_spcreateerror("couldn't get mountd address on udp"));
    return NULL;
  }

  /*
   * Create priviledged UDP socket
   */
  *sock = t_open(nc->nc_device, O_RDWR, 0);

  if (*sock < 0) {
    plog(XLOG_ERROR, "t_open %s: %m", nc->nc_device);
    return NULL;		/* neither tcp not udp succeeded */
  }
  if (bind_resv_port(*sock, (unsigned short *) 0) < 0)
    plog(XLOG_ERROR, "couldn't bind mountd socket to privileged port");

  if ((client = clnt_dg_create(*sock, &nb, MOUNTPROG, MOUNTVERS, 0, 0))
	== (CLIENT *) NULL)
  {
    plog(XLOG_ERROR, "clnt_dg_create failed");
    t_close(*sock);
    return NULL;		/* neither tcp not udp succeeded */
  }
  if (clnt_control(client, CLSET_RETRY_TIMEOUT, (char *) tv) == FALSE) {
    plog(XLOG_ERROR, "clnt_control CLSET_RETRY_TIMEOUT for udp failed");
    clnt_destroy(client);
    return NULL;		/* neither tcp not udp succeeded */
  }

  /* udp succeeded */
#ifdef DEBUG
  dlog("get_mount_client: using udp, port %d", sin.sin_port);
#endif /* DEBUG */
  return client;
}

#else /* It is not __svr4__ */

/*
 * NON-SVR4 VERSION
 * Create an rpc client attached to the mount daemon.
 */
static CLIENT *get_mount_client P((struct sockaddr_in *sin, struct timeval *tv, int *sock));
static CLIENT *get_mount_client(sin, tv, sock)
struct sockaddr_in *sin;
struct timeval *tv;
int *sock;
{
  CLIENT *client;

  /*
   * First try a TCP socket
   */
  if ((*sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) > 0) {
    /*
     * Bind to a privileged port
     */
    if (bind_resv_port(*sock, (unsigned short *) 0) < 0)
      plog(XLOG_ERROR, "can't bind privileged port");

    /*
     * Find mountd port to connect to.
     * Connect to mountd.
     * Create a tcp client.
     */
    if ((sin->sin_port = htons(pmap_getport(sin, MOUNTPROG, MOUNTVERS, IPPROTO_TCP))) != 0) {
      if (connect(*sock, (struct sockaddr *) sin, sizeof(*sin)) >= 0
	  && ((client = clnttcp_create(sin, MOUNTPROG, MOUNTVERS, sock, 0, 0)) != NULL))
	return client;
    }

    /*
     * Failed so close socket
     */
    (void) close(*sock);
  }				/* tcp socket opened */

  /*
   * TCP failed so try UDP
   */
    if ((*sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
      plog(XLOG_ERROR, "Can't create socket to connect to mountd: %m");
      *sock = RPC_ANYSOCK;
      return NULL;
    }

    /*
     * Bind to a privileged port
     */
    if (bind_resv_port(*sock, (unsigned short *) 0) < 0)
      plog(XLOG_ERROR, "can't bind privileged port");

    /*
     * Zero out the port - make sure we recompute
     */
    sin->sin_port = 0;

    /*
     * Make a UDP client
     */
    if ((client = clntudp_create(sin, MOUNTPROG, MOUNTVERS, *tv, sock)) == NULL) {
      (void) close(*sock);
      *sock = RPC_ANYSOCK;
      return NULL;
    }
#ifdef DEBUG
      dlog("get_mount_client: Using udp, port %d", sin->sin_port);
#endif /* DEBUG */
      return client;
}
#endif /* __svr4__ */


/*
 * Mount the export tree from a host
 */
static int host_fmount P((mntfs *mf));
static int host_fmount(mf)
mntfs *mf;
{
	struct timeval tv2;
	CLIENT *client;
	enum clnt_stat clnt_stat;
	int n_export;
	int j, k;
	exports exlist = 0, ex;
	exports *ep = 0;
	fhstatus *fp = 0;
	char *host = mf->mf_server->fs_host;
	int error = 0;
	struct sockaddr_in sin;
	int sock = RPC_ANYSOCK;
	int ok = FALSE;
	mntlist *mlist;
	char fs_name[MAXPATHLEN], *rfs_dir;
	char mntpt[MAXPATHLEN];
	struct timeval tv;

/*
 * The original 10 second per try timeout is WAY too large, especially
 * if we're only waiting 10 or 20 seconds max for the response.
 * That would mean we'd try only once in 10 seconds, and we could
 * lose the transmitt or receive packet, and never try again.
 * A 2-second per try timeout here is much more reasonable.
 * 09/28/92 Mike Mitchell, mcm@unx.sas.com
 *    tv.tv_sec = 10; tv.tv_usec = 0;
 */
	tv.tv_sec = 2; tv.tv_usec = 0;


	/*
	 * Read the mount list
	 */
	mlist = read_mtab(mf->mf_mount);

	/*
	 * Unlock the mount list
	 */
	unlock_mntlist();

	/*
	 * Take a copy of the server address
	 */
	sin = *mf->mf_server->fs_ip;

	/*
	 * Create a client attached to mountd
	 */
#ifdef __svr4__
	if ((client = get_mount_client(host, &tv, &sock)) == NULL) {
#else
	if ((client = get_mount_client(&sin, &tv, &sock)) == NULL) {
#endif
		plog(XLOG_ERROR, "Failed to make rpc connection to mountd on %s", host);
		error = EIO;
		goto out;
	}

	if (!nfs_auth) {
		error = make_nfs_auth();
		if (error)
			goto out;
	}

	client->cl_auth = nfs_auth;

#ifdef DEBUG
	dlog("Fetching export list from %s", host);
#endif /* DEBUG */

	/*
	 * Fetch the export list
	 */
	tv2.tv_sec = 10; tv2.tv_usec = 0;
	clnt_stat = clnt_call(client, MOUNTPROC_EXPORT, xdr_void, 0,
			      xdr_exports, (SVC_IN_ARGS_TYPE) &exlist, tv2);
	if (clnt_stat != RPC_SUCCESS) {
		extern char *clnt_sperrno();
		char *msg = clnt_sperrno(clnt_stat);
		plog(XLOG_ERROR, "host_fmount rpc failed: %s", msg);
		/*clnt_perror(client, "rpc");*/
		error = EIO;
		goto out;
	}

	/*
	 * Figure out how many exports were returned
	 */
	for (n_export = 0, ex = exlist; ex; ex = ex->ex_next) {
		/*printf("export %s\n", ex->ex_dir);*/
		n_export++;
	}
#ifdef DEBUG
	/*dlog("%d exports returned\n", n_export);*/
#endif /* DEBUG */

	/*
	 * Allocate an array of pointers into the list
	 * so that they can be sorted.  If the filesystem
	 * is already mounted then ignore it.
	 */
	ep = (exports *) xmalloc(n_export * sizeof(exports));
	for (j = 0, ex = exlist; ex; ex = ex->ex_next) {
		MAKE_MNTPT(mntpt, ex, mf);
		if (!already_mounted(mlist, mntpt))
			ep[j++] = ex;
	}
	n_export = j;

	/*
	 * Sort into order.
	 * This way the mounts are done in order down the tree,
	 * instead of any random order returned by the mount
	 * daemon (the protocol doesn't specify...).
	 */
	qsort(ep, n_export, sizeof(exports), sortfun);

	/*
	 * Allocate an array of filehandles
	 */
	fp = (fhstatus *) xmalloc(n_export * sizeof(fhstatus));

	/*
	 * Try to obtain filehandles for each directory.
	 * If a fetch fails then just zero out the array
	 * reference but discard the error.
	 */
	for (j = k = 0; j < n_export; j++) {
		/* Check and avoid a duplicated export entry */
		if (j > k && ep[k] && strcmp(ep[j]->ex_dir, ep[k]->ex_dir) == 0) {
#ifdef DEBUG
			dlog("avoiding dup fhandle requested for %s", ep[j]->ex_dir);
#endif
			ep[j] = 0;
		} else {
			k = j;
			if (error = fetch_fhandle(client, ep[j]->ex_dir, &fp[j]))
				ep[j] = 0;
		}
	}

	/*
	 * Mount each filesystem for which we have a filehandle.
	 * If any of the mounts succeed then mark "ok" and return
	 * error code 0 at the end.  If they all fail then return
	 * the last error code.
	 */
	strncpy(fs_name, mf->mf_info, sizeof(fs_name));
	if ((rfs_dir = strchr(fs_name, ':')) == (char *) 0) {
		plog(XLOG_FATAL, "host_fmount: mf_info has no colon");
		error = EINVAL;
		goto out;
	}
	++rfs_dir;
	for (j = 0; j < n_export; j++) {
		ex = ep[j];
		if (ex) {
			strcpy(rfs_dir, ex->ex_dir);
			MAKE_MNTPT(mntpt, ex, mf);
			if (do_mount(&fp[j], mntpt, fs_name, mf->mf_mopts, mf) == 0)
				ok = TRUE;
		}
	}

	/*
	 * Clean up and exit
	 */
out:
	discard_mntlist(mlist);
	if (ep)
		free(ep);
	if (fp)
		free(fp);
	if (sock != RPC_ANYSOCK)
#ifdef __svr4__
		(void) t_close(sock);
#else
		(void) close(sock);
#endif /* __svr4__ */
	if (client)
		clnt_destroy(client);
	if (exlist)
		xdr_pri_free(xdr_exports, &exlist);
	if (ok)
		return 0;
	return error;
}

/*
 * Return true if pref is a directory prefix of dir.
 *
 * TODO:
 * Does not work if pref is "/".
 */
static int directory_prefix P((char *pref, char *dir));
static int directory_prefix(pref, dir)
char *pref;
char *dir;
{
	int len = strlen(pref);
	if (strncmp(pref, dir, len) != 0)
		return FALSE;
	if (dir[len] == '/' || dir[len] == '\0')
		return TRUE;
	return FALSE;
}

/*
 * Unmount a mount tree
 */
static int host_fumount P((mntfs *mf));
static int host_fumount(mf)
mntfs *mf;
{
	mntlist *ml, *mprev;
	int xerror = 0;

	/*
	 * Read the mount list
	 */
	mntlist *mlist = read_mtab(mf->mf_mount);

	/*
	 * Unlock the mount list
	 */
	unlock_mntlist();

	/*
	 * Reverse list...
	 */
	ml = mlist;
	mprev = 0;
	while (ml) {
		mntlist *ml2 = ml->mnext;
		ml->mnext = mprev;
		mprev = ml;
		ml = ml2;
	}
	mlist = mprev;

	/*
	 * Unmount all filesystems...
	 */
	for (ml = mlist; ml && !xerror; ml = ml->mnext) {
		char *dir = ml->mnt->mnt_dir;
		if (directory_prefix(mf->mf_mount, dir)) {
			int error;
#ifdef DEBUG
			dlog("host: unmounts %s", dir);
#endif /* DEBUG */
			/*
			 * Unmount "dir"
			 */
			error = UMOUNT_FS(dir);
			/*
			 * Keep track of errors
			 */
			if (error) {
				if (!xerror)
					xerror = error;
				if (error != EBUSY) {
					errno = error;
					plog("Tree unmount of %s failed: %m", ml->mnt->mnt_dir);
				}
			} else {
#ifdef HOST_MKDIRS
				(void) rmdirs(dir);
#endif /* HOST_MKDIRS */
			}
		}
	}

	/*
	 * Throw away mount list
	 */
	discard_mntlist(mlist);

	/*
	 * Try to remount, except when we are shutting down.
	 */
	if (xerror && amd_state != Finishing) {
		xerror = host_fmount(mf);
		if (!xerror) {
			/*
			 * Don't log this - it's usually too verbose
			plog(XLOG_INFO, "Remounted host %s", mf->mf_info);
			 */
			xerror = EBUSY;
		}
	}
	return xerror;
}

/*
 * Tell mountd we're done.
 * This is not quite right, because we may still
 * have other filesystems mounted, but the existing
 * mountd protocol is badly broken anyway.
 */
static void host_umounted(mp)
am_node *mp;
{
#ifdef INFORM_MOUNTD
	mntfs *mf = mp->am_mnt;
	char *host;
	CLIENT *client;
	enum clnt_stat clnt_stat;
	struct sockaddr_in sin;
	int sock = RPC_ANYSOCK;
	struct timeval tv;
	tv.tv_sec = 10; tv.tv_usec = 0;

	if (mf->mf_error || mf->mf_refc > 1 || ! mf->mf_server)
		return;

	host = mf->mf_server->fs_host;
	sin = *mf->mf_server->fs_ip;

	/*
	 * Create a client attached to mountd
	 */
#ifdef __svr4__
	if ((client = get_mount_client(host, &tv, &sock)) == NULL) {
#else
	if ((client = get_mount_client(&sin, &tv, &sock)) == NULL) {
#endif
		plog(XLOG_ERROR, "Failed to make rpc connection to mountd on %s", host);
		goto out;
	}

	if (!nfs_auth) {
		if (make_nfs_auth())
			goto out;
	}

	client->cl_auth = nfs_auth;

#ifdef DEBUG
	dlog("Unmounting all from %s", host);
#endif /* DEBUG */

	clnt_stat = clnt_call(client, MOUNTPROC_UMNTALL, xdr_void, 0, xdr_void, 0, tv);
	if (clnt_stat != RPC_SUCCESS && clnt_stat != RPC_SYSTEMERROR) {
		/* RPC_SYSTEMERROR seems to be returned for no good reason ...*/
		extern char *clnt_sperrno();
		char *msg = clnt_sperrno(clnt_stat);
		plog(XLOG_ERROR, "unmount all from %s rpc failed: %s", host, msg, clnt_stat);
		goto out;
	}

out:
	if (sock != RPC_ANYSOCK)
#ifdef __svr4__
		(void) t_close(sock);
#else
		(void) close(sock);
#endif /* __svr4__ */
	if (client)
		clnt_destroy(client);

#endif /* INFORM_MOUNTD */
}


#else /* HOST_EXEC */

static int host_exec P((char*op, char*host, char*fs, char*opts));
static int host_exec(op, host, fs, opts)
char *op;
char *host;
char *fs;
char *opts;
{
	int error;
	char *argv[7];

	/*
	 * Build arg vector
	 */
	argv[0] = host_helper;
	argv[1] = host_helper;
	argv[2] = op;
	argv[3] = host;
	argv[4] = fs;
	argv[5] = opts && *opts ? opts : "rw,default";
	argv[6] = 0;

	/*
	 * Put stdout to stderr
	 */
	(void) fclose(stdout);
	(void) dup(fileno(logfp));
	if (fileno(logfp) != fileno(stderr)) {
		(void) fclose(stderr);
		(void) dup(fileno(logfp));
	}
	/*
	 * Try the exec
	 */
#ifdef DEBUG
	Debug(D_FULL) {
		char **cp = argv;
		plog(XLOG_DEBUG, "executing (un)mount command...");
		while (*cp) {
	  		plog(XLOG_DEBUG, "arg[%d] = '%s'", cp-argv, *cp);
			cp++;
		}
	}
#endif /* DEBUG */
	if (argv[0] == 0 || argv[1] == 0) {
		errno = EINVAL;
		plog(XLOG_USER, "1st/2nd args missing to (un)mount program");
	} else {
		(void) execv(argv[0], argv+1);
	}
	/*
	 * Save error number
	 */
	error = errno;
	plog(XLOG_ERROR, "exec %s failed: %m", argv[0]);

	/*
	 * Return error
	 */
	return error;
}

static int host_mount P((am_node *mp));
static int host_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return host_exec("mount", mf->mf_server->fs_host, mf->mf_mount, mf->mf_opts);
}

static int host_umount P((am_node *mp));
static int host_umount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return host_exec("unmount", mf->mf_server->fs_host, mf->mf_mount, "xxx");
}

#endif /* HOST_EXEC */

/*
 * Ops structure
 */
am_ops host_ops = {
	"host",
	host_match,
	host_init,
	auto_fmount,
	host_fmount,
	auto_fumount,
	host_fumount,
	efs_lookuppn,
	efs_readdir,
	0, /* host_readlink */
	0, /* host_mounted */
#ifdef HOST_EXEC
	0, /* host_umounted */
#else
	host_umounted,
#endif
	find_nfs_srvr,
	FS_MKMNT|FS_BACKGROUND|FS_AMQINFO
};

#endif /* HAS_HOST */
