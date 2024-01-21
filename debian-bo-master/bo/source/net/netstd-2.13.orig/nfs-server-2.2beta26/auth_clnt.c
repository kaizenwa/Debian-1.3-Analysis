/*
 * auth_clnt.c	This module takes care of request authorization.
 *
 * Authors:	Don Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Olaf Kirch, <okir@monad.swb.de>
 *
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */


#include "nfsd.h"
#include "fakefsuid.h"

#ifndef svc_getcaller
#define svc_getcaller(x) ((struct sockaddr_in *) &(x)->xp_rtaddr.buf)
#endif


#if defined(HAVE_SETFSUID) || defined(MAYBE_HAVE_SETFSUID)
static _PRO(void setfsids, (uid_t, gid_t, gid_t *, int));
#endif
#ifndef HAVE_SETFSUID
static _PRO(void seteids,  (uid_t, gid_t, gid_t *, int));
#endif

uid_t		auth_uid = 0;		/* Current effective user ids */
gid_t		auth_gid = 0;
GETGROUPS_T	auth_gids[NGRPS];	/* Current supplementary gids */
int		auth_gidlen = -1;
uid_t		cred_uid;
gid_t		cred_gid;
gid_t		*cred_gids;
int		cred_len;

/* authdes_getucred is not exported in svcauth.h even if present. */
extern _PRO( int authdes_getucred, (caddr_t credptr, short *uid,
				short *gid, short *nrgids, int *groups)	);


nfs_client *auth_clnt(rqstp)
struct svc_req *rqstp;
{
	nfs_client	*cp = NULL;
	struct in_addr addr = svc_getcaller(rqstp->rq_xprt)->sin_addr;

	/* Check if this is a known host, and if it's allowed to mount this */
	if ((cp = auth_known_clientbyaddr(addr)) != NULL) {
		return cp;
	}

	/* No, it's not. Check against list of unknown hosts */
	if ((cp = auth_unknown_clientbyaddr(addr)) != NULL) {
		return cp;
	}

	/* We don't know you */
	if (trace_spoof) {
		Dprintf(L_ERROR, "Unauthorized access by NFS client %s.\n",
		    inet_ntoa(addr));
	}

	return (NULL);
}

nfs_mount *auth_path(cp, rqstp, path)
nfs_client *cp;
struct svc_req *rqstp;
char *path;
{
	nfs_mount	*mp;

	/* Check if the specified client is permitted to access this */
	if ((mp = auth_match_mount(cp, path)) == NULL) {
		if (cp->flags != 0 || trace_spoof) {
			Dprintf(L_ERROR, "NFS client %s tried to access %s\n",
			    cp->clnt_name, path);
		}
		return NULL;
	}

	/* Check request originated on a privileged port. */
	if (!allow_non_root && mp->o.secure_port &&
	    ntohs(svc_getcaller(rqstp->rq_xprt)->sin_port) >= IPPORT_RESERVED) {
		Dprintf(L_ERROR,
		    "NFS request from %s originated on insecure port, %s\n",
		    cp->clnt_name,
		    "psychoanalysis suggested");
		return (NULL);
	}

	if (logging_enabled(D_AUTH)) {
		Dprintf(D_AUTH, "auth_path(%s): mount point %s, (%s%s%s%s%s)\n",
			path, mp->path,
			mp->o.all_squash? "all_squash " : (
			 mp->o.root_squash? "root_squash " : ""),
			(mp->o.uidmap == map_daemon)? "uidmap " : "",
			mp->o.secure_port? "secure " : "insecure ",
			mp->o.link_relative? "linkrel " : "",
			mp->o.read_only? "ro" : "rw");
	}

	return mp;
}

void auth_user(mp, rqstp)
nfs_mount *mp;
struct svc_req *rqstp;
{
	uid_t		cuid;
	gid_t		cgid;
	GETGROUPS_T	cgids[NGRPS];
	int		squash = mp->o.all_squash;
	int		clen, i;

	if (rqstp->rq_cred.oa_flavor == AUTH_UNIX) {
		struct authunix_parms *unix_cred;

		unix_cred = (struct authunix_parms *) rqstp->rq_clntcred;
		cred_uid  = unix_cred->aup_uid;
		cred_gid  = unix_cred->aup_gid;
		cred_len  = unix_cred->aup_len;
		cred_gids = unix_cred->aup_gids;
	} else if (rqstp->rq_cred.oa_flavor == AUTH_DES) {
		short	uid, gid, grplen = NGRPS;
		int	groups[NGRPS], i;

		i = authdes_getucred(rqstp->rq_clntcred, &uid, &gid,
						&grplen, groups);
		if (!i)
			goto nobody;
		cred_uid  = (uid_t) uid;
		cred_gid  = (gid_t) gid;
		cred_len  = grplen;
		for (i = 0; i < grplen; i++)
			cred_gids[i] = groups[i];
	} else {
		/* We will want to support AUTH_DES/AUTH_KRB one day,
		 * but for now we treat all other authentication flavor
		 * as AUTH_NULL.
		 */
	nobody:
		squash = 1;
		cred_uid  = mp->o.nobody_uid;
		cred_gid  = mp->o.nobody_gid;
		cred_gids = NULL;
		cred_len  = 0;
	}

	if (cred_len > NGRPS)
		cred_len = NGRPS;

	if (!squash) {
		/* Do the uid/gid mapping here.
		 * Note that we check cred_uid (which is a short), not
		 * unix_cred->aup_uid to avoid the uid masking bug.
		 */
		cuid = luid(cred_uid, mp, rqstp);
		cgid = lgid(cred_gid, mp, rqstp);
		clen = cred_len;
		for (i = 0; i < cred_len; i++)
			cgids[i] = lgid(cred_gids[i], mp, rqstp);
	} else {
		cuid = mp->o.nobody_uid;
		cgid = mp->o.nobody_gid;
		/* Construct a list of one gid. */
		cgids[0] = cgid;
		clen = 1;
	}

	/* This code is a little awkward because setfsuid has been present
	 * in the Linux kernel for quite some time but not in libc.
	 * The startup code tests for the setfsuid syscall and sets
	 * have_setfsuid accordingly.
	 */
#if defined(HAVE_SETFSUID)
	setfsids(cuid, cgid, cgids, clen);
#else
#if defined(MAYBE_HAVE_SETFSUID)
	if (have_setfsuid)
		setfsids(cuid, cgid, cgids, clen);
	else
#endif
		seteids(cuid, cgid, cgids, clen);
#endif
}

/*
 * The following functions deal with setting the client's uid/gid.
 */
void auth_override_uid(uid)
uid_t uid;
{
#if defined(HAVE_SETFSUID)
	setfsuid(uid);
#else
#if defined(MAYBE_HAVE_SETFSUID)
	if (have_setfsuid)
		setfsuid(uid);
	else
#endif
		seteuid(uid);
#endif
}

#if defined(HAVE_SETFSUID) || defined(MAYBE_HAVE_SETFSUID)
static void setfsids(cred_uid, cred_gid, cred_gids, cred_len)
uid_t cred_uid;
gid_t cred_gid, *cred_gids;
int cred_len;
{
	/* First, set the user ID. */
	if (auth_uid != cred_uid) {
		if (setfsuid(cred_uid) < 0)
			Dprintf(L_ERROR, "Unable to setfsuid %d: %s\n",
			    cred_uid, strerror(errno));
		else
			auth_uid = cred_uid;
	}

	/* Next, the group ID. */
	if (auth_gid != cred_gid) {
		if (setfsgid(cred_gid) < 0)
			Dprintf(L_ERROR, "Unable to setfsgid %d: %s\n",
			    cred_gid, strerror(errno));
		else
			auth_gid = cred_gid;
	}

#ifdef HAVE_SETGROUPS
	/* Finally, set the supplementary group IDs if possible. */
	if (cred_len < 0 || cred_len > NGRPS)
		Dprintf(L_ERROR, "Negative or huge cred_len: %d\n", cred_len);
	else if (cred_len != auth_gidlen
	    || memcmp(cred_gids, auth_gids, auth_gidlen*sizeof(gid_t))) {
		if (setgroups(cred_len, cred_gids) < 0)
			Dprintf(L_ERROR, "Unable to setgroups: %s\n",
			    strerror(errno));
		else {
			memcpy(auth_gids, cred_gids, cred_len*sizeof(gid_t));
			auth_gidlen = cred_len;
		}
	}
#endif /* HAVE_SETGROUPS */

}
#endif

#if !defined(HAVE_SETFSUID)
static void seteids(cred_uid, cred_gid, cred_gids, cred_len)
uid_t cred_uid;
gid_t cred_gid, *cred_gids;
int cred_len;
{
	/* To set any IDs we first need to be root. What a pain. */

	/* First set the group ID. */
	if (auth_gid != cred_gid) {
		if (auth_uid != ROOT_UID) {
			if (seteuid(ROOT_UID) < 0)
				Dprintf(L_ERROR, "Unable to seteuid(%d): %s\n",
				    ROOT_UID, strerror(errno));
			else
				auth_uid = ROOT_UID;
		}
		if (setegid(cred_gid) < 0)
			Dprintf(L_ERROR, "Unable to setegid(%d): %s\n",
			    cred_gid, strerror(errno));
		else
			auth_gid = cred_gid;
	}

#ifdef HAVE_SETGROUPS
	/* Next set the supplementary group IDs if possible. */
	if (cred_len < 0 || cred_len > NGRPS)
		Dprintf(L_ERROR, "Negative or huge cred_len: %d\n", cred_len);
	else if (cred_len != auth_gidlen
	    || memcmp(cred_gids, auth_gids, auth_gidlen*sizeof(gid_t))) {
		if (auth_uid != ROOT_UID) {
			if (seteuid(ROOT_UID) < 0)
				Dprintf(L_ERROR, "Unable to seteuid(%d): %s\n",
				    ROOT_UID, strerror(errno));
			else
				auth_uid = ROOT_UID;
		}
		if (setgroups(cred_len, cred_gids) < 0)
			Dprintf(L_ERROR, "Unable to setgroups: %s\n",
			    strerror(errno));
		else {
			memcpy(auth_gids, cred_gids, cred_len*sizeof(gid_t));
			auth_gidlen = cred_len;
		}
	}
#endif /* HAVE_SETGROUPS */

	/* Finally, set the user ID. */
	if (auth_uid != cred_uid) {
		if (auth_uid != ROOT_UID && seteuid(ROOT_UID) < 0)
			Dprintf(L_ERROR, "Unable to seteuid(%d): %s\n", 
				ROOT_UID, strerror(errno));
		if (seteuid(cred_uid) < 0)
			Dprintf(L_ERROR, "Unable to seteuid(%d): %s\n",
			    cred_uid, strerror(errno));
		else
			auth_uid = cred_uid;
	}
}
#endif
