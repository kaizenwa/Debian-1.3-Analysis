/*
 * auth.h	This module takes care of request authorization.
 *
 * Authors:	Mark A. Shand, May 1988
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

/* Global AUTH variables. */
extern int			allow_non_root;
extern int			promiscuous;
extern int			re_export;
extern int			trace_spoof;
extern exportnode		*export_list;
extern uid_t			cred_uid, auth_uid;
extern gid_t			cred_gid, auth_gid;

#if defined(linux) && defined(i386) && !defined(HAVE_SETFSUID)
#   define MAYBE_HAVE_SETFSUID
#endif

#ifdef MAYBE_HAVE_SETFSUID
extern int			have_setfsuid;
#endif

/*
 * These externs are set in the dispatcher (dispatch.c) and auth_fh
 * (nfsd.c) so that we can determine access rights, export options,
 * etc. pp.
 */		
extern struct nfs_client	*nfsclient;
extern struct nfs_mount		*nfsmount;
/* extern struct svc_req 		*svc_rqstp; */

/*
 * These are the structures used by the authentication module.
 */
typedef struct nfs_options {
	enum { map_daemon, identity }
				uidmap;		/* uid/gid mapping behavior */
	int			root_squash;
	int			all_squash;
	int			secure_port;
	int			read_only;
	int			link_relative;
	int			noaccess;
	uid_t			nobody_uid;
	gid_t			nobody_gid;
} nfs_options;

typedef struct nfs_mount {
	struct nfs_mount	*next;
	struct nfs_client	*client;
	int			length;
	char			*path;
	nfs_options		o;
} nfs_mount;

typedef struct nfs_client {
	struct nfs_client	*next;
	struct in_addr		clnt_addr;
	struct in_addr		clnt_mask;
	char			*clnt_name;
	unsigned short		flags;
	nfs_mount		*m;

	/* Things that will also go here:
	 *  -	uid/gid map for those who want to run ugidd. 
	 *  -	encryption info for the file handle
	 */
	struct ugid_map		*umap;
} nfs_client;

#define AUTH_CLNT_WILDCARD	0x0001
#define AUTH_CLNT_ANONYMOUS	0x0002
#define AUTH_CLNT_NETGROUP	0x0004
#define AUTH_CLNT_NETMASK	0x0008
#define AUTH_CLNT_DEFAULT	0x0010

#ifndef ROOT_UID
#define ROOT_UID		0
#endif

#define AUTH_UID_NONE		((uid_t)-1)
#define AUTH_GID_NONE		((uid_t)-1)
#define AUTH_UID_NOBODY		((uid_t)-2)
#define AUTH_GID_NOBODY		((uid_t)-2)

/* Global Function prototypes. */
extern _PRO( void       auth_init, (char *fname)			);
extern _PRO( void       auth_init_lists, (void)				);
extern _PRO( void	auth_free_lists, (void)				);
extern _PRO( nfs_client *auth_clnt, (struct svc_req *rqstp)		);
extern _PRO( nfs_mount  *auth_path, (nfs_client *, struct svc_req *, char *));
extern _PRO( void       auth_user, (nfs_mount *, struct svc_req *)	);

extern _PRO( nfs_client *auth_get_client, (char *)			);
extern _PRO( nfs_mount  *auth_match_mount, (nfs_client *, char *)	);
extern _PRO( nfs_client *auth_known_clientbyaddr, (struct in_addr)	);
extern _PRO( nfs_client *auth_known_clientbyname, (char *)		);
extern _PRO( nfs_client *auth_unknown_clientbyaddr, (struct in_addr)	);
extern _PRO( nfs_client *auth_create_client, (const char *, struct hostent *));
extern _PRO( nfs_client *auth_create_default_client, (void)		);
extern _PRO( nfs_mount  *auth_add_mount, (nfs_client *, char *)		);
extern _PRO( void       auth_check_all_wildcards, (void)		);
extern _PRO( void       auth_check_all_netgroups, (void)		);
extern _PRO( void       auth_check_all_netmasks, (void)			);

/* This function lets us set our euid/fsuid temporarily */
extern _PRO( void       auth_override_uid, (uid_t)			);

/* Prototypes for ugidd mapping */
extern _PRO( uid_t	ruid, (uid_t, nfs_mount *, struct svc_req *)	);
extern _PRO( gid_t	rgid, (gid_t, nfs_mount *, struct svc_req *)	);
extern _PRO( uid_t	luid, (uid_t, nfs_mount *, struct svc_req *)	);
extern _PRO( gid_t	lgid, (gid_t, nfs_mount *, struct svc_req *)	);
extern _PRO( void	ugid_free_map, (struct ugid_map *)		);
extern _PRO( void	ugid_map_uid, (nfs_mount *, uid_t fm, uid_t to)	);
extern _PRO( void	ugid_map_gid, (nfs_mount *, gid_t fm, gid_t to)	);

#if 0
#define ruid(u, mp, x)		(u)
#define rgid(g, mp, x)		(g)
#define luid(u, mp, x)	\
	(((!(u) && (mp)->o.root_squash) || (mp)->o.all_squash)? nobody_uid:(u))
#define lgid(g, mp, x)	\
	(((!(g) && (mp)->o.root_squash) || (mp)->o.all_squash)? nobody_gid:(g))
#define ugid_free_map(map)	/* NOP */
#endif

/* End of auth.h. */
