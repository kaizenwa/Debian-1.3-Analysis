/*
 * This module contains all generic functions used by the authentication
 * mechanism.
 *
 * Copyright (C) 1995 Olaf Kirch  <okir@monad.swb.de>
 *
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 */

#include "nfsd.h"

#define AUTH_DEBUG

static _PRO( int hostmatch, (const char *, const char *)		);
static _PRO( void auth_check_wildcards, (nfs_client *cp)		);
static _PRO( void auth_add_mountlist, (nfs_client *, nfs_mount *)	);
static _PRO( void auth_create_hashent, (nfs_client *, struct in_addr *)	);
static _PRO( void auth_free_list, (nfs_client **)			);
static _PRO( void auth_warn_anon, (void)				);

/* It appears to be an old and long-standing tradition on Unices not
 * to declare the netgroup functions in any header file
 */
extern _PRO( int setnetgrent, (char *netgroup)				);
extern _PRO( int getnetgrent, (char **host, char **user, char **domain)	);
extern _PRO( int endnetgrent, (void)					);
extern _PRO( int innetgr,     (const char *netgroup, const char *host,
			       const char *user, const char *domain)	);

#define IPHASH(a)	(((a)^((a)>>8)^((a)>>16)^((a)>>24)) & (IPHASHMAX-1))
#define IPHASHMAX	32

typedef struct nfs_hash_ent {
	struct nfs_hash_ent	*next;
	struct in_addr		addr;
	nfs_client		*client;
} nfs_hash_ent;

static nfs_hash_ent		*hashtable[IPHASHMAX];
static nfs_client		*known_clients = NULL;
static nfs_client		*unknown_clients = NULL;
static nfs_client		*wildcard_clients = NULL;
static nfs_client		*netgroup_clients = NULL;
static nfs_client		*netmask_clients = NULL;
static nfs_client		*anonymous_client = NULL;
static nfs_client		*default_client = NULL;
static int			initialized = 0;

static nfs_options		default_options = {
					identity,	/* uid mapping */
					1,		/* root squash */
					0,		/* all squash */
					1,		/* secure port */
					0,		/* read-only */
					0,		/* relative links */
					0,		/* noaccess */
					(uid_t)-2,	/* default uid */
					(gid_t)-2,	/* default gid */
				};

/*
 * Get a client entry for a specific name or pattern.
 * If necessary, this function performs a hostname lookup to
 * obtain the host's FQDN. This is for the benefit of those who
 * use aliases in /etc/exports, or mix qualified and unqualified
 * hostnames.
 *
 * FIXME: Make this function create the nfs_client entry if none
 * is found. Currently, this function is called only from auth_init
 * anyway, which creates the client entry if this function returns
 * NULL.
 */
nfs_client *
auth_get_client(hname)
char *hname;
{
	struct hostent	*hp;
	nfs_client	*cp;

	if (hname == NULL || *hname == '\0') {
		auth_warn_anon();
		return anonymous_client;
	}

	if (*hname == '@') {
		for (cp = netgroup_clients; cp != NULL; cp = cp->next) {
			if (!strcmp(cp->clnt_name, hname))
				return cp;
		}
		return NULL;
	}

	if (strchr(hname, '*') != NULL || strchr(hname, '?') != NULL) {
		for (cp = wildcard_clients; cp != NULL; cp = cp->next) {
			if (!strcmp(cp->clnt_name, hname))
				return cp;
		}
		return NULL;
	}

	for (cp = unknown_clients; cp != NULL; cp = cp->next) {
		if (!strcmp(cp->clnt_name, hname))
			return cp;
	}

	for (cp = netmask_clients; cp != NULL; cp = cp->next) {
		if (!strcmp(cp->clnt_name, hname))
			return cp;
	}

	for (cp = known_clients; cp != NULL; cp = cp->next) {
		if (!strcmp(cp->clnt_name, hname))
			return cp;
	}

	if ((hp = gethostbyname(hname)) != NULL) {
		for (cp = known_clients; cp != NULL; cp = cp->next) {
			if (!strcmp(cp->clnt_name, hp->h_name))
				return cp;
		}
	}

	return NULL;
}

/*
 * Given a client and a pathname, try to find the proper mount point.
 * This code relies on the mount list being sorted from largest to
 * smallest w.r.t strcmp.
 */
nfs_mount *
auth_match_mount(cp, path)
nfs_client *cp;
char *path;
{
	nfs_mount	*mp;
	char		c;

	if (path == NULL)
		return NULL;

	for (mp = cp->m; mp != NULL; mp = mp->next) {
		if (!strncmp(mp->path, path, mp->length) &&
		    ((c = path[mp->length]) == '/' || c == '\0')) {
			return mp;
		}
	}
	return NULL;
}

/*
 * Find a known client given its IP address.
 * The matching hash entry is moved to the list head. This may be useful
 * for sites with large exports list (e.g. due to huge netgroups).
 */
nfs_client *
auth_known_clientbyaddr(addr)
struct in_addr	addr;
{
	nfs_hash_ent	**htp, *hep, *prv;

	htp = hashtable + IPHASH(addr.s_addr);
	hep = *htp;
	for (prv = NULL; hep != NULL; prv = hep, hep = hep->next) {
		if (hep->addr.s_addr == addr.s_addr) {
			if (prv != NULL) {
				prv->next = hep->next;
				hep->next = *htp;
				*htp = hep;
			}
			hep->client->clnt_addr = addr;
			return hep->client;
		}
	}
	return NULL;
}

/*
 * Find a known client given its FQDN.
 */
nfs_client *
auth_known_clientbyname(hname)
char *hname;
{
	nfs_client	*cp;

	if (hname == NULL)
		return NULL;

	for (cp = known_clients; cp != NULL; cp = cp->next) {
		if (!strcmp(cp->clnt_name, hname))
			return cp;
	}
	return NULL;
}

/*
 * Find an unknown client given its IP address. This functions checks
 * previously unresolved hostnames, wildcard hostnames, the anon client,
 * and the default client.
 *
 * After we have walked this routine for a single host once, it is
 * either authenticated, and has been added to know_clients, or it
 * is denied access, in which case we just ignore it.
 *
 * XXX: may need rejected_clients to avoid denial of service by flooding
 * server with illegal requests. But then you can flood it anyway with
 * less exotic stuff.
 */
nfs_client *
auth_unknown_clientbyaddr(addr)
struct in_addr addr;
{
	struct hostent	*hp;
	nfs_client	*cp, *ncp = NULL;
	char		nambuf[256], *hname;

	Dprintf(D_AUTH, "check unknown clnt addr %s\n", inet_ntoa(addr));
	hp = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);

	if (hp != NULL) {
		nfs_client	**cpp;
		char		**ap;

		/* Keep temp copy of hostname */
		strncpy(nambuf, hp->h_name, sizeof(nambuf));
		nambuf[sizeof(nambuf)-1] = '\0';

		/*
		 * Do a reverse lookup
		 * (FIXME: resolver lib may already have done this).
		 */
		hp = gethostbyname(nambuf);

		if (hp == NULL) {
			hname = nambuf;
			if (trace_spoof)
			    Dprintf(L_ERROR,
				"couldn't verify address of host %s\n", hname);
			goto anonymous;
		}
		hname = (char *) hp->h_name;

		/*
		 * Make sure this isn't a spoof attempt.
		 */
		for (ap = hp->h_addr_list; *ap != NULL; ap++) {
			if (!memcmp(*ap, (char *)&addr, hp->h_length))
				break;
		}

		if (*ap == NULL) {
			Dprintf(L_ERROR,
				"spoof attempt by %s: pretends to be %s!\n",
				inet_ntoa(addr), hname);
			return NULL;
		}

		Dprintf(D_AUTH, "\tclient name is %s\n", hname);

		/*
		 * First, check for clients that couldn't be resolved during
		 * initialization.
		 */
		for (cpp = &unknown_clients; (cp = *cpp); cpp = &cp->next) {
			if (!strcmp((*cpp)->clnt_name, hname)) {
				Dprintf(D_AUTH,
					"Found previously unknown host %s\n",
					hname);
				cp->clnt_addr = addr;

				/*
				 * remove client from list of unknown and
				 * add it to list of known hosts.
				 *
				 * Wildcards clients will be checked in the
				 * next step.
				 */
				(*cpp) = cp->next;
				cp->next = known_clients;
				known_clients = cp;

				/* Add host to hashtable */
				for (ap = hp->h_addr_list; *ap != NULL; ap++) {
					auth_create_hashent(cp, 
							(struct in_addr *)*ap);
				}

				ncp = cp;
				break;
			}
		}

		/*
		 * Okay, now check for wildcard names. NB the wildcard
		 * patterns are sorted from most to least specific.
		 *
		 * The pattern matching should also be applied to
		 * all names in h_aliases.
		 */
		for (cpp = &wildcard_clients; (cp = *cpp); cpp = &cp->next) {
			if (hostmatch(hname, cp->clnt_name)) {
				Dprintf(D_AUTH,
					"client %s matched pattern %s\n",
					hname, cp->clnt_name);
				if (!ncp)
					ncp = auth_create_client(hname, hp);
				auth_add_mountlist(ncp, cp->m);
				/* continue, loop over all wildcards */
			}
		}

		/*
		 * Try netgroups next.
		 */
		for (cpp = &netgroup_clients; (cp = *cpp); cpp = &cp->next) {
			if (innetgr(cp->clnt_name+1, hname, NULL, NULL)) {
				Dprintf(D_AUTH,
					"client %s matched netgroup %s\n",
					hname, cp->clnt_name+1);
				if (!ncp) {
					ncp = auth_create_client(hname, hp);
				}
				auth_add_mountlist(ncp, cp->m);
				/* continue, loop over all netgroups */
			}
		}
	} else {
		hname = inet_ntoa(addr);
	}

	/*
	 * Final step: check netmask clients
	 */
	for (cp = netmask_clients; cp != NULL; cp = cp->next) {
		if (!((addr.s_addr^cp->clnt_addr.s_addr)&cp->clnt_mask.s_addr)){
			Dprintf(D_AUTH, "client %s matched %s\n",
				hname, cp->clnt_name);
			if (!ncp)
				ncp = auth_create_client(hname, hp);
			auth_add_mountlist(ncp, cp->m);
			/* continue, loop over all netmasks */
		}
	}

anonymous:
	if ((cp = anonymous_client) || (cp = default_client)) {
		if (!ncp) {
			Dprintf(D_AUTH, "Anonymous request from %s.\n", hname);
			ncp = auth_create_client(hname, hp);
		}
		auth_add_mountlist(ncp, cp->m);
	}

	return ncp;
}

/*
 * Create a client struct for the given hostname. The hp parameter
 * optionally contains hostent information obtained from a previous
 * gethostbyname.
 */
nfs_client *
auth_create_client(hname, hp)
const char *hname;
struct hostent *hp;
{
	nfs_client	*cp, **cpp;
	int		wildcards, netgroup, netmask, namelen;

	cp = (nfs_client *) xmalloc(sizeof(nfs_client));

	cp->clnt_addr.s_addr = INADDR_ANY;
	cp->flags = 0;
	cp->m = NULL;
	cp->umap = NULL;

	if (hname == NULL) {
		if (anonymous_client != NULL) {
			free (cp);
			cp = anonymous_client;
		} else {
			anonymous_client = cp;
		}
		cp->clnt_name = NULL;
		cp->next = NULL;
		cp->flags = AUTH_CLNT_ANONYMOUS;
		return cp;
	}

	wildcards = (strchr(hname, '*') != NULL || strchr(hname, '?') != NULL);
	netgroup = (hname[0] == '@');
	netmask = (strchr(hname, '/') != NULL);

	if (!wildcards && !netgroup && !netmask) {
		if (hp == NULL)
			hp = gethostbyname(hname);
	} else if (hp != NULL) {
		Dprintf(L_WARNING,
			"Whoa: client %s has weird/illegal name %s\n",
			inet_ntoa(*(struct in_addr *) hp->h_addr),
			hname);
	}

	if (hp != NULL) {
		if (hp->h_addrtype != AF_INET) {
			Dprintf(L_WARNING,
				"%s has address type %d != AF_INET.\n",
				hp->h_name, hp->h_addrtype);
			hp = NULL;
		} else {
			hname = hp->h_name;	/* use FQDN */
		}
	}

	cp->clnt_name = xstrdup(hname);

	if (wildcards) {
		/* We have a wildcard name. Wildcard names are sorted
		 * in order of descending pattern length. This way,
		 * pattern *.pal.xgw.fi is matched before *.xgw.fi.
		 */
		cp->flags = AUTH_CLNT_WILDCARD;
		cpp = &wildcard_clients;
		namelen = strlen(hname);
		while (*cpp != NULL && namelen <= strlen((*cpp)->clnt_name)) {
			cpp = &((*cpp)->next);
		}
	} else if (netgroup) {
		/* Netgroup name. */
		cp->flags = AUTH_CLNT_NETGROUP;
		cpp = &netgroup_clients;
	} else if (netmask) {
		/* Address/mask pair. */
		char	*slashp = strchr(hname, '/');
		cp->flags = AUTH_CLNT_NETMASK;
		cpp = &netmask_clients;
		*slashp = '\0';
		cp->clnt_mask.s_addr = inet_addr(slashp+1);
		cp->clnt_addr.s_addr = inet_addr(hname);
		if (cp->clnt_addr.s_addr == -1 || cp->clnt_mask.s_addr == -1) {
			*slashp = '/';
			Dprintf(L_ERROR, "bad addr/mask value: %s", hname);
		}
	} else if (hp == NULL) {
		cpp = &unknown_clients;
	} else {
		char	**ap;

		cpp = &known_clients;
		for (ap = hp->h_addr_list; *ap != NULL; ap++) {
			auth_create_hashent(cp, (struct in_addr *)*ap);
		}
	}
	cp->next = *cpp;
	*cpp = cp;

	return cp;
}

/*
 * Create the default client.
 */
nfs_client *
auth_create_default_client()
{
	nfs_client	*cp;

	if (default_client == NULL) {
		cp = (nfs_client *) xmalloc(sizeof(nfs_client));
		cp->clnt_name = NULL;
		cp->next = NULL;
		cp->flags = AUTH_CLNT_DEFAULT;
		cp->m = NULL;
		default_client = cp;
	}
	auth_warn_anon();
	return default_client;
}

static void
auth_warn_anon()
{
	static int	warned = 0;
	struct hostent	*hp;
	char		name[257];
	char		**ap;

	if (warned)
		return;
	warned = 1;

	if (gethostname(name, sizeof(name)) < 0) {
		Dprintf(L_ERROR, "can't get local hostname");
		return;
	}
	if ((hp = gethostbyname(name)) == NULL) {
		Dprintf(L_ERROR, "can't get my own address");
		return;
	}
	if (hp->h_addrtype != AF_INET) {
		Dprintf(L_ERROR, "local host address is not AF_INET?!");
		return;
	}
	for (ap = hp->h_addr_list; *ap; ap++) {
		struct in_addr	addr;
		unsigned long	net3, net2;

		addr = *(struct in_addr *) *ap;
		net3 = ntohl(addr.s_addr) & 0xff000000;
		net2 = ntohl(addr.s_addr) & 0x00ff0000;
		if (net3 == 0x0A000000 ||
		   (net3 == 0xAC000000 && 0x100000 <= net2 && net2 < 0x200000)||
		   (net3 == 0XC0000000 && 0xA80000 == net2))
			continue;
		Dprintf(L_WARNING, "exports file has anon entries, but host\n");
		Dprintf(L_WARNING, "has non-private IP address %s!\n",
					inet_ntoa(addr));
	}
}

/*
 * Create an entry in the hashtable of known clients.
 */
void
auth_create_hashent(cp, ap)
nfs_client	*cp;
struct in_addr	*ap;
{
	nfs_hash_ent	*hep;
	int		hash;

	hash = IPHASH(ap->s_addr);

	hep = (nfs_hash_ent *) xmalloc(sizeof(*hep));
	hep->client = cp;
	hep->addr = *ap;
	hep->next = hashtable[hash];
	hashtable[hash] = hep;
}

/*
 * After reading the entire exports file, this routine checks if any
 * of the known clients match any of the patterns in wildcard_clients.
 * If one does, the wildcard's mount points are added to its list of
 * mount points.
 */
void
auth_check_all_wildcards(void)
{
	nfs_client	*cp;

	for (cp = wildcard_clients; cp != NULL; cp = cp->next) {
		nfs_mount	*mp;

		Dprintf(D_AUTH, "wildcard clnt %s:\n", cp->clnt_name);
		for (mp = cp->m; mp != NULL; mp = mp->next)
			Dprintf(D_AUTH, "\texport %s:\n", mp->path);
	}

	for (cp = known_clients; cp != NULL; cp = cp->next) {
		auth_check_wildcards(cp);
	}
	for (cp = unknown_clients; cp != NULL; cp = cp->next) {
		auth_check_wildcards(cp);
	}
}

static void
auth_check_wildcards(cp)
nfs_client *cp;
{
	nfs_client	*wcp;
	nfs_mount	*mp;

	for (wcp = wildcard_clients; wcp != NULL; wcp = wcp->next) {
		if (hostmatch(cp->clnt_name, wcp->clnt_name)) {
			auth_add_mountlist(cp, wcp->m);
		}
	}
	if (anonymous_client != NULL) {
		auth_add_mountlist(cp, anonymous_client->m);
	}
	for (mp = cp->m; mp != NULL; mp = mp->next) {
		Dprintf(D_AUTH, "\tclnt %s exp %s\n",
			cp->clnt_name, mp->path);
	}
}

/*
 * Check all client structs that apply to a netgroup
 */
void
auth_check_all_netgroups(void)
{
#ifdef HAVE_GETNETGRENT
	nfs_client	*ncp, *cp;
	char		*group;

	for (cp = known_clients; cp != NULL; cp = cp->next) {
		for (ncp = netgroup_clients; ncp != NULL; ncp = ncp->next) {
			group = ncp->clnt_name+1;
			if (innetgr(group, cp->clnt_name, NULL, NULL)) {
				Dprintf(D_AUTH,
					"client %s matched netgroup %s\n",
					cp->clnt_name, cp->clnt_name+1);
				auth_add_mountlist(cp, ncp->m);
			}
		}
	}
#endif
}

/*
 * Check all client structs that match an addr/mask pair
 */
void
auth_check_all_netmasks(void)
{
	nfs_client	*ncp, *cp;
	nfs_hash_ent	*hp;
	int		i, match;

	for (ncp = netmask_clients; ncp != NULL; ncp = ncp->next) {
		for (i = 0; i < IPHASHMAX; i++) {
			for (hp = hashtable[i]; hp != NULL; hp = hp->next) {
				match = ((hp->addr.s_addr 
				        ^ ncp->clnt_addr.s_addr)
			                & ncp->clnt_mask.s_addr) == 0;
				if (match) {
					cp = hp->client;
					auth_add_mountlist(cp, ncp->m);
				}
			}
		}
	}
}

/*
 * Add a mount point to a client. Mount points are sorted from most
 * specific to least specific.
 */
nfs_mount *
auth_add_mount(cp, path)
nfs_client *cp;
char *path;
{
	nfs_mount	*mp, **mpp;
	int		len;

	len = strlen(path);

	mp = (nfs_mount*) xmalloc(sizeof(nfs_mount));
	memcpy (&mp->o, &default_options, sizeof(nfs_options));
	mp->client = cp;

	mp->path = xstrdup(path);
	while (len && mp->path[len-1] == '/')
		mp->path[--len] = '\0';
	mp->length = len;

	/* insert mount point into list of mounts. Insert more specific
	 * path before less specific path. For equal mount points, the
	 * first one entered wins so that exports entries like
	 *
	 * /foo/bar	(ro) host1(rw,no_root_squash)
	 *
	 * do the intuitive thing.
	 */
	for (mpp = &(cp->m); *mpp != NULL; mpp = &(*mpp)->next) {
		if (strcmp((*mpp)->path, path) < 0)
			break;
	}

	mp->next = *mpp;
	*mpp = mp;

	return mp;
}

/*
 * Add a list of mount points to an existing client. This code looks
 * somewhat sub-optimal, but we have to make sure the overall order
 * of mount points is preserved (i.e. most specific to least specific).
 * Few Linux machines will have more than a dozen or so paths in their
 * exports file anyway.
 */
static void
auth_add_mountlist(cp, mp)
nfs_client *cp;
nfs_mount *mp;
{
    nfs_mount	*nmp;

    while (mp != NULL) {
	nmp = auth_add_mount(cp, mp->path);
	memcpy (&(nmp->o), &(mp->o), sizeof(nfs_options));

	mp = mp->next;
    }
}

/*
 * Match a hostname against a pattern.
 */
static int
hostmatch(hname, pattern)
const char *hname;
const char *pattern;
{
	int seen_dot = 0;

	Dprintf(D_AUTH, "host matching %s to %s\n", hname, pattern);

	for (;;) {
		if (*hname == '\0' || *pattern == '\0')
			return (*hname == *pattern);
		switch (*pattern) {
		case '*':
			while (*hname != '.' && *hname != '\0')
				hname++;
			seen_dot = 1;
			pattern++;
			break;
		case '?':
			if (*hname == '.')
				return (0);
			hname++;
			pattern++;
			break;
		default:
			if (seen_dot) {
				if (tolower(*hname) != tolower(*pattern))
					return (0);
			}
			else if (*hname != *pattern)
				return (0);
			if (*pattern == '.')
				seen_dot = 1;
			hname++;
			pattern++;
			break;
		}
	}
}

/*
 * Initialize hash table. If the auth module has already been initialized, 
 * free all list entries first.
 */
void
auth_init_lists()
{
	struct passwd	*pw;
	int		i;
	uid_t		anon_uid;
	gid_t		anon_gid;

	if (initialized) {
		nfs_hash_ent	*hep, *next;

		auth_free_list(&known_clients);
		auth_free_list(&unknown_clients);
		auth_free_list(&wildcard_clients);
		auth_free_list(&netgroup_clients);
		auth_free_list(&anonymous_client);
		auth_free_list(&default_client);

		for (i = 0; i < IPHASHMAX; i++) {
			for (hep = hashtable[i]; hep != NULL; hep = next) {
				next = hep->next;
				free (hep);
			}
			hashtable[i] = NULL;
		}
	} else {
		for (i = 0; i < IPHASHMAX; i++) {
			hashtable[i] = NULL;
		}
	}

	/* Get the default anon uid/gid */
	if ((pw = getpwnam("nobody")) != NULL) {
		anon_uid = pw->pw_uid;
		anon_gid = pw->pw_gid;
	} else {
		anon_uid = (uid_t) -2;
		anon_gid = (gid_t) -2;
	}

	/* This protects us from stomping all over the place on installations
	 * that have given nobody a uid/gid of -1. This is quite bad for
	 * systems that don't have setfsuid, because seteuid(-1) is a no-op.
	 */
	if (anon_uid == (uid_t)-1) {
		Dprintf(L_ERROR,
			"Eek: user nobody has uid -1. Using -2 instead.\n");
		anon_uid = (uid_t) -2;
	}
	if (anon_gid == (gid_t)-1) {
		Dprintf(L_ERROR,
			"Eek: user nobody has gid -1. Using -2 instead.\n");
		anon_gid = (gid_t) -2;
	}

	default_options.nobody_uid = anon_uid;
	default_options.nobody_gid = anon_gid;

	initialized = 1;
}

/*
 * Free all members on a list of nfs_clients.
 */
static void
auth_free_list(cpp)
nfs_client	**cpp;
{
	nfs_client	*cp, *nxt_clnt;
	nfs_mount	*mp, *nxt_mp;

	for (cp = *cpp; cp != NULL; cp = nxt_clnt) {
		nxt_clnt = cp->next;
		if (cp->clnt_name != NULL) {
			free (cp->clnt_name);
		}
		for (mp = cp->m; mp != NULL; mp = nxt_mp) {
			nxt_mp = mp->next;
			free (mp->path);
			free (mp);
		}
		if (cp->umap != NULL) {
			ugid_free_map(cp->umap);
		}
		free (cp);
	}
	*cpp = NULL;
}

