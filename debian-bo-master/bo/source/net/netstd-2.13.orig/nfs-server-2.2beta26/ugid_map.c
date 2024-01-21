/* UNFSD - copyright Mark A Shand, May 1988.
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 *
 * Redone from ground up by Olaf Kirch, April 1995.
 *
 * TODO: 
 *  -	time out uids/gids.
 *  -	Write protocol version 2 to allow bulk transfers and
 *	some more intelligent form of authentication.
 *
 *	Authors:
 *		Mark A. Shand
 *		Olaf Kirch, <okir@monad.swb.de>
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <pwd.h>
#include <grp.h>
#include <fcntl.h>
#include "nfsd.h"
#include "ugid.h"

#define UID_BITS	(sizeof(uid_t) * CHAR_BIT)
#define UID_MAX		(1L << UID_BITS)
#define UID_LIM1	(UID_MAX/2)
#define GID_BITS	(sizeof(gid_t) * CHAR_BIT)
#define GID_MAX		(1L << GID_BITS)
#define GID_LIM1	(GID_MAX/2)

/* This structure holds a uid/gid map. 
 * We do a little hack here so that we're able to hold ids at the lower 
 * end of the id space as well as those at the upper end (e.g. nobody)
 * without wasting precious K's inbetween.
 */
typedef struct ugid_map {
	struct uid_map {
		uid_t	*bot, *top;
		int	bot_lim, top_lim;
	} uid, rev_uid;	
	struct gid_map {
		gid_t	*bot, *top;
		int	bot_lim, top_lim;
	} gid, rev_gid;	
} ugid_map;

/* ugidd client handle cache (indexed by ugidd hostaddr).
 * 
 * MAXCACHE is the number of ugidd client handles cached. 
 * EXPCACHE defines (in seconds) how long invalid client handles
 * are cached. Otherwise, crashed ugidd servers would hang nfsd
 * during each lookup - a condition from which it would hardly
 * recover, because the nfs client will keep retransmitting the
 * same request over and over while nfsd still waits for ugidd
 * to reply.
 */
#define MAXCACHE	32
#define EXPCACHE	(15 * 60)

typedef struct clnt_cache {
	struct in_addr	addr;		/* ugidd host addr. */
	time_t		age, lru;	/* create and access time */
	CLIENT		*clnt;		/* the client itself */
} clnt_cache;
static clnt_cache	cache[MAXCACHE];
static int		initialized = 0;

/*
 * Prototypes and the like
 */
static _PRO(ugid_map *getmap, (nfs_mount *mountp)			);
static _PRO(uid_t *get_uid_mapent, (struct uid_map *map, uid_t uid, int));
static _PRO(gid_t *get_gid_mapent, (struct gid_map *map, gid_t gid, int));
static _PRO(int rlookup, (char *nam, int *id, int map, SVCXPRT *xprt)	);
static _PRO(CLIENT *getclnt, (SVCXPRT *xprt)				);
static _PRO(void delete_clnt, (CLIENT *clnt)				);

#ifdef sun
void xdr_free() {}
#endif

/*
 * Map a server uid to a client uid
 */
uid_t ruid(uid, mountp, rqstp)
uid_t		uid;
nfs_mount	*mountp;
struct svc_req	*rqstp;
{
	struct passwd	*pw;
	ugid_map	*map;
	uid_t		*up, *upr;
	int		id;

#ifdef DOSHACKS
	/* Reverse effects of all_squash for DOS clients */
	if (mountp->o.all_squash && uid == mountp->o.nobody_uid)
		return cred_uid;
#endif
	if (mountp->o.uidmap == identity)
		return uid;

	map = getmap(mountp);
	up  = get_uid_mapent(&(map->uid), uid, 1);
	if (*up == AUTH_UID_NONE) {
		if ((pw = getpwuid(uid)) != NULL &&
		    rlookup(pw->pw_name, &id, NAME_UID, rqstp->rq_xprt) &&
		    (id != AUTH_UID_NONE)) {
			*up = (uid_t)id;
			upr = get_uid_mapent(&(map->uid), *up, 1);
			*upr = uid;
		} else {
			*up = AUTH_UID_NOBODY;
		}
	}
	Dprintf(D_UGID, "ruid(%s, %d) = %d\n",
			inet_ntoa(mountp->client->clnt_addr), uid, *up);
	return *up;
}

/*
 * Map a server gid to a client gid.
 */
gid_t rgid(gid, mountp, rqstp)
gid_t		gid;
nfs_mount	*mountp;
struct svc_req	*rqstp;
{
	struct group	*gr;
	ugid_map	*map;
	gid_t		*gp, *gpr;
	int		id;

#ifdef DOSHACKS
	/* Reverse effects of all_squash for DOS clients */
	if (mountp->o.all_squash && gid == mountp->o.nobody_gid)
		return cred_gid;
#endif
	if (mountp->o.uidmap == identity)
		return gid;

	map = getmap(mountp);
	gp  = get_gid_mapent(&(map->gid), gid, 1);
	if (*gp == AUTH_GID_NONE) {
		if ((gr = getgrgid(gid)) != NULL &&
		    rlookup(gr->gr_name, &id, GROUP_GID, rqstp->rq_xprt) &&
		    (id != AUTH_GID_NONE)) {
			*gp = id;
			gpr = get_gid_mapent(&(map->rev_gid), *gp, 1);
			*gpr = gid;
		} else {
			*gp = AUTH_GID_NOBODY;
		}
	}
	Dprintf(D_UGID, "rgid(%s, %d) = %d\n",
			inet_ntoa(mountp->client->clnt_addr), gid, *gp);
	return *gp;
}

/*
 * Map a client uid to a server uid
 */
uid_t luid(uid, mountp, rqstp)
uid_t		uid;
nfs_mount	*mountp;
struct svc_req	*rqstp;
{
	struct passwd	*pw;
	ugid_map	*map;
	char		namebuf[MAXUGLEN];
	uid_t		*up, *upr, retuid;
	int		id;

	map = getmap(mountp);
	if (mountp->o.uidmap == map_daemon) {
		up = get_uid_mapent(&(map->rev_uid), uid, 1);
		if (*up == AUTH_UID_NONE) {
			id = uid;
			if (rlookup(namebuf, &id, UID_NAME, rqstp->rq_xprt) &&
			    ((pw = getpwnam(namebuf)) != NULL)) {
				*up = pw->pw_uid;
				upr = get_uid_mapent(&(map->uid), *up, 1);
				*upr = uid;
			} else {
				*up = mountp->o.nobody_uid;
			}
		}
		retuid = *up;
	} else {
		up = get_uid_mapent(&(map->rev_uid), uid, 0);
		if (up != NULL && *up != AUTH_UID_NONE) {
			if (*up == AUTH_UID_NOBODY)
				*up = mountp->o.nobody_uid;
			retuid = *up;
		} else {
			retuid = uid;
		}
	}

	if ((retuid == 0 && mountp->o.root_squash) || mountp->o.all_squash)
		retuid = mountp->o.nobody_uid;

	Dprintf(D_UGID, "luid(%s, %d) = %d\n",
			inet_ntoa(mountp->client->clnt_addr), uid, retuid);

	return retuid;
}

/*
 * Map a client gid to a server gid
 */
gid_t lgid(gid, mountp, rqstp)
gid_t		gid;
nfs_mount	*mountp;
struct svc_req	*rqstp;
{
	struct group	*gr;
	ugid_map	*map;
	char		namebuf[MAXUGLEN]; 
	gid_t		*gp, *gpr, retgid;
	int		id;

	map = getmap(mountp);
	if (mountp->o.uidmap == map_daemon) {
		gp = get_gid_mapent(&(map->rev_gid), gid, 1);
		if (*gp == AUTH_GID_NONE) {
			id = gid;
			if (rlookup(namebuf, &id, GID_GROUP, rqstp->rq_xprt) &&
			    ((gr = getgrnam(namebuf)) != NULL)) {
				*gp = gr->gr_gid;
				gpr = get_gid_mapent(&(map->gid), *gp, 1);
				*gpr = gid;
			} else {
				*gp = mountp->o.nobody_gid;
			}
		}
		retgid = *gp;
	} else {
		gp = get_gid_mapent(&(map->rev_gid), gid, 0);
		if (gp != NULL && *gp != AUTH_GID_NONE) {
			if (*gp == AUTH_GID_NOBODY)
				*gp = mountp->o.nobody_gid;
			retgid = *gp;
		} else {
			retgid = gid;
		}
	}

	if ((gid == 0 && mountp->o.root_squash) || mountp->o.all_squash)
		retgid = mountp->o.nobody_gid;

	Dprintf(D_UGID, "lgid(%s, %d) = %d\n",
			inet_ntoa(mountp->client->clnt_addr), gid, retgid);

	return retgid;
}

/*
 * Define client to server mapping records for a given uid or gid.
 */
void ugid_map_uid(mountp, from, to)
nfs_mount *mountp;
uid_t	from;
uid_t	to;
{
	ugid_map	*map;
	uid_t		*up;

	Dprintf(D_UGID, "%s:%s map uid rem %d -> loc %d\n",
			mountp->client->clnt_name, mountp->path, from, to);
	map = getmap(mountp);
	up = get_uid_mapent(&(map->rev_uid), from, 1);
	*up = to;
}

void ugid_map_gid(mountp, from, to)
nfs_mount *mountp;
gid_t	from;
gid_t	to;
{
	ugid_map	*map;
	gid_t		*gp;

	Dprintf(D_UGID, "%s:%s map gid rem %d -> loc %d\n",
			mountp->client->clnt_name, mountp->path, from, to);
	map = getmap(mountp);
	gp = get_gid_mapent(&(map->rev_gid), from, 1);
	*gp = to;
}

/*
 * Get the pointer to a uid map entry.
 */
static uid_t *get_uid_mapent(map, uid, create)
struct uid_map	*map;
uid_t 		uid;
int		create;
{
	uid_t	**tblp;
	int	*limp, newlim, i;

	if (uid < UID_LIM1) {
		tblp = &(map->bot);
		limp = &(map->bot_lim);
	} else {
		uid = UID_MAX - uid;
		tblp = &(map->top);
		limp = &(map->top_lim);
	}
	if (uid >= *limp) {
		if (!create) 
			return NULL;

		newlim = uid + 1;
		*tblp = (uid_t*) xrealloc(*tblp, newlim * sizeof(uid_t));
		for (i = *limp; i < newlim; i++)
			(*tblp)[i] = AUTH_UID_NONE;
		*limp = newlim;
	}
	return (*tblp) + uid;
}

/*
 * Get the pointer to a gid map entry.
 */
static gid_t *get_gid_mapent(map, gid, create)
struct gid_map	*map;
gid_t		gid;
int		create;
{
	gid_t	**tblp;
	int	*limp, newlim, i;

	if (gid < GID_LIM1) {
		tblp = &(map->bot);
		limp = &(map->bot_lim);
	} else {
		gid = GID_MAX - gid;
		tblp = &(map->top);
		limp = &(map->top_lim);
	}
	if (gid >= *limp) {
		if (!create)
			return NULL;

		newlim = gid + 1;
		*tblp = (gid_t*) xrealloc(*tblp, newlim * sizeof(gid_t));

		for (i = *limp; i < newlim; i++)
			(*tblp)[i] = AUTH_GID_NONE;
		*limp = newlim;
	}
	return (*tblp) + gid;
}

/*
 * Get the map for a given mount point. If it hasn't been initialized yet,
 * create it.
 */
static ugid_map *getmap(mountp)
nfs_mount *mountp;
{
	nfs_client	*clientp = mountp->client;
	ugid_map	*map;

	if ((map = clientp->umap) != NULL) 
		return map;

	map = (ugid_map *) xmalloc(sizeof(*map));

	map->uid.top         = map->uid.bot         = NULL;
	map->uid.top_lim     = map->uid.bot_lim     = 0;
	map->gid.top         = map->gid.bot         = NULL;
	map->gid.top_lim     = map->gid.bot_lim     = 0;
	map->rev_uid.top     = map->rev_uid.bot     = NULL;
	map->rev_uid.top_lim = map->rev_uid.bot_lim = 0;
	map->rev_gid.top     = map->rev_gid.bot     = NULL;
	map->rev_gid.top_lim = map->rev_gid.bot_lim = 0;

	clientp->umap = map;
	return map;
}


/*
 * Deallocate a uid map.
 */
void
ugid_free_map(map)
ugid_map	*map;
{
	int	i;

	/* invalidate cache of client FH's */
	if (initialized) {
		for (i = 0; i < MAXCACHE; i++) {
			if (cache[i].clnt != NULL)
				clnt_destroy(cache[i].clnt);
			cache[i].addr.s_addr = INADDR_ANY;
			cache[i].clnt = NULL;
		}
		initialized = 0;
	}

	if (map->uid.top)	free (map->uid.top);
	if (map->uid.bot)	free (map->uid.bot);
	if (map->gid.top)	free (map->gid.top);
	if (map->gid.bot)	free (map->gid.bot);
	if (map->rev_uid.top)	free (map->rev_uid.top);
	if (map->rev_uid.bot)	free (map->rev_uid.bot);
	if (map->rev_gid.top)	free (map->rev_gid.top);
	if (map->rev_gid.bot)	free (map->rev_gid.bot);

	free (map);
}

/* 
 * Obtain an RPC client handle for a given client host. We cache these
 * handles on a limited scale.
 */
static CLIENT *getclnt(xprt)
SVCXPRT		*xprt;
{
	struct sockaddr_in	addr;
	struct timeval		wait;
	CLIENT			*clnt;
	time_t			now, age;
	int			i, empty, oldest;
	int			sock;

	if (!initialized) {
		for (i = 0; i < MAXCACHE; i++) {
			cache[i].addr.s_addr = INADDR_ANY;
			cache[i].clnt = NULL;
		}
		initialized = 1;
	}

	/* Get current time */
	now = age = time(NULL);

	/* Check if the client is already cached */
	addr = *svc_getcaller(xprt);
	empty = oldest = -1;
	for (i = 0; i < MAXCACHE; i++) {
		if (cache[i].addr.s_addr == addr.sin_addr.s_addr)
			break;
		if (cache[i].clnt == NULL) {
			empty = i;
		} else if (cache[i].lru > age) {
			age = cache[i].lru;
			oldest = i;
		}
	}

	/* If the address was in the cache but the client was invalid,
	 * check if we should reattempt to obtain the handle
	 */
	if (i < MAXCACHE) {
		if ((clnt = cache[i].clnt) == NULL) {
			if (now - cache[i].age <= EXPCACHE) {
				Dprintf(D_UGID,
					"ugid: found invalid client %s\n",
					inet_ntoa(cache[i].addr));
				return NULL;
			}
			Dprintf(D_UGID, "ugid: found expired client %s\n",
				inet_ntoa(cache[i].addr));
			empty = i; i = MAXCACHE;	/* force lookup */
		} else {
			cache[i].lru = now;
			return clnt;
		}
	}

	/* If not found and there's no empty slot, free the oldest */
	if (i >= MAXCACHE && empty == -1) {
		Dprintf(D_UGID, "ugid: deleting oldest client %s slot %d\n",
			inet_ntoa(cache[oldest].addr), oldest);
		clnt_destroy(cache[oldest].clnt);
		cache[oldest].clnt = NULL; 
		empty = oldest;
	}

	/* Client is not in cache. Create it. */
	Dprintf(D_UGID, "ugid: create client %s slot %d\n",
		inet_ntoa(addr.sin_addr), empty);
	cache[empty].clnt = NULL;
	cache[empty].addr = addr.sin_addr;
	cache[empty].age  = now;
	cache[empty].lru  = now;

	addr.sin_port = 0;
	wait.tv_sec   = 10;
	wait.tv_usec  = 0;
	sock = RPC_ANYSOCK;

	clnt = clntudp_create(&addr, UGIDPROG, UGIDVERS, wait, &sock);
	if (clnt == NULL) {
		Dprintf(L_ERROR, "can't connect to ugidd on %s.\n", 
				inet_ntoa(addr.sin_addr));
		cache[empty].clnt = NULL;
		return NULL;
	}

	/* I'm not sure if we can count on addr.sin_port to contain
	 * the server's port after clntudp_create, so we fetch it
	 * explicitly.
	 */
	clnt_control(clnt, CLGET_SERVER_ADDR, &addr);
	if (ntohs(addr.sin_port) >= IPPORT_RESERVED) {
		Dprintf(L_ERROR, "ugidd on %s runs on unprivileged port.\n",
				inet_ntoa(addr.sin_addr));
		clnt_destroy(clnt);
		cache[empty].clnt = NULL;
		return NULL;
	}

	cache[empty].clnt = clnt;
	/* cache[empty].addr = addr.sin_addr; */

	return clnt;
}

static void delete_clnt(clnt)
CLIENT	*clnt;
{
	int		i;

	for (i = 0; i < MAXCACHE; i++) {
		if (cache[i].clnt == clnt) 
			break;
	}
	if (i < MAXCACHE) {
		Dprintf(L_ERROR,
			"Call to ugidd on %s failed. Blocked for %d seconds.",
			inet_ntoa(cache[i].addr), EXPCACHE);
		clnt_destroy(clnt);
		cache[i].clnt = NULL;
		cache[i].age = cache[i].lru;
	}
}


/*
 * Lookup a given uid or gid by calling the client's ugidd.
 *
 * This incarnation of rlookup doesn't use the authenticate call
 * anymore. This authentication required the ugidd server to open
 * a priviled port and send an integer. This can be accomplished
 * much more efficiently by requiring the server to run on a privileged
 * port in the first place.
 */
static int rlookup(nam, id, map, xprt)
char	*nam;
int	*id;
int	map;
SVCXPRT	*xprt;
{
	CLIENT		*clnt;
	int		*pi;
	char		**sp;
	int		ret = 0, retry = 0;

	if ((clnt = getclnt(xprt)) == NULL)
		return 0;

	do {
		switch (map) {
		case NAME_UID:
			pi = name_uid_1(&nam, clnt);
			if ((ret = (pi != NULL)))
				*id = *pi;
			Dprintf(D_UGID, "rlookup(NAME_UID, %s) %s\n",
					 nam, (pi != NULL)? "OK" : "FAIL");
			break;
		case GROUP_GID:
			pi = group_gid_1(&nam, clnt);
			if ((ret = (pi != NULL)))
				*id = *pi;
			Dprintf(D_UGID, "rlookup(GROUP_GID, %s) %s\n",
					 nam, (pi != NULL)? "OK" : "FAIL");
			break;
		case UID_NAME:
			sp = uid_name_1(id, clnt);
			if ((ret = (sp != NULL)))
				strcpy(nam, *sp);
			Dprintf(D_UGID, "rlookup(UID_NAME, %d) %s\n",
					*id, (sp != NULL)? "OK" : "FAIL");
			break;
		case GID_GROUP:
			sp = gid_group_1(id, clnt);
			if ((ret = (sp != NULL)))
				strcpy(nam, *sp);
			Dprintf(D_UGID, "rlookup(GID_GROUP, %d) %s\n",
					*id, (sp != NULL)? "OK" : "FAIL");
			break;
		default:
			return 0;
		}

		/* RPC error - check the status. When encountering errors that
		 * are likely to persist, we clear the client to make sure 
		 * no more lookups are attempted within the next EXPCACHE 
		 * seconds.
		 */
		if (!ret) {
			struct rpc_err	err;

			Dprintf(D_UGID, "ugidd call error: %s\n",
				clnt_sperror(clnt, ""));

			clnt_geterr(clnt, &err);
			switch (err.re_status) {
			case RPC_CANTSEND:	/* Maybe network failures */
			case RPC_CANTRECV:	/* should be transient */
			case RPC_TIMEDOUT:	/* This is the worst one */
			case RPC_VERSMISMATCH:	/* Cases of general bogosity */
			case RPC_AUTHERROR:
			case RPC_PROGVERSMISMATCH:
			case RPC_PROCUNAVAIL:
				Dprintf(D_UGID, "deleting client %lx\n", clnt);
				delete_clnt(clnt);
				break;
			case RPC_CANTDECODEARGS:/* retry operation */
			case RPC_CANTDECODERES:
				Dprintf(D_UGID, "retrying operation (%d)\n",
							retry);
				retry++;
				break;
			default:
				break;
			}
		}
	} while (retry && retry < 3);

	return ret;
}
