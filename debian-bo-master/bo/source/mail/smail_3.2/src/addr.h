/*
#ident	"@(#)smail/src:RELEASE-3_2:addr.h,v 1.10 1996/02/28 14:26:04 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * addr.h:
 *	interface file for routines in addr.c
 */

/* types used in addr.h */
/*
 * addr - Everything needed to understand an address is stored in
 *	  here somewhere.
 */
struct addr {
    struct addr *succ;		/* next addr in queue */
    long flags;			/* miscellaneous flags */
    int parseflags;		/* parse flags for parse_address() */
    struct addr *parent;	/* addr from which this one is derived */
    struct addr *true_addr;	/* point to addr which had an error */
    char *in_addr;		/* address from header or args */
    char *target;		/* next explicit destination */
    char *remainder;		/* address beyond explicit site */
    char *work_addr;		/* working area */
    int match_count;		/* chars of target matched by router */
    char *local_name;           /* domain matched as local */
    char *owner;                /* address owner (fwdfile/aliasfile) */
    char *route;		/* the route to the target */
    struct router *router;	/* router used for route and next_host */
    struct director *director;	/* director which matched address */
    char *next_host;		/* next-hop host to receive message */
    char *next_addr;		/* address to give next-hop host */
    struct transport *transport; /* transport to use for remote delivery */
    char *home;			/* home directory associated with login */
    int uid;			/* user for pipes/files perms or BOGUS_USER */
    int gid;			/* gid for pipes/files perms or BOGUS_GROUP */
    struct error *error;	/* error message associated */
    struct transport_hints *tphint_list; /* transport hints from the router */
};

/* structure for errors stored in the addr structure */
struct error {
    long info;				/* info associated with error */
    char *message;			/* error message */
};

/*
 * The identify_addr structure is used to uniquely identify specific addr
 * structures produced by resolve_addr_list().  The in_addr value in an
 * addr structure is insufficient, by itself, to identify an addr structure,
 * when that structure is a file or pipe form address.  It is, however,
 * sufficient to give the parent address as well, if one exists.
 */
struct identify_addr {
    struct identify_addr *succ;		/* this will be a linked list */
    char *address;			/* the address of interest */
    char *parent;			/* the parent of that address */
};

/*
 * The defer_addr structure is used to form a list of previously defer'd
 * addresses.  This list is formed from the per-message logfile data, and
 * is used to filter out redundant deferal messages.
 */
struct defer_addr {
    struct defer_addr *succ;		/* this will be a linked list */
    long error;				/* error number */
    char *message;			/* deferal message */
    char *address;			/* address defered */
    char *parent;			/* parent of defered address */
};

/*
 * The transport_hints structure can be used to pass additional
 * routing information between a router and a transport.  It is
 * organized as a linked list of tagged values, where the tags
 * describe the type of hint.  The bind router, for example, passes an
 * "mx" type transport hint for each possible SMTP mail exchanger to
 * the tcpsmtp transport.
 */
struct transport_hints {
	struct transport_hints * succ;	/* pointer to next hint */
	char * hint_name;		/* type of information */
	char * private;			/* pointer to information */
};

/*
 * bits used in addr.flag
 */
#define ADDR_CAUTION	0x00000010	/* be cautious of this address */
#define ADDR_UNSECURE	0x00000200	/* address from an unsecure source */
#define ADDR_PUTDOT	0x00000400	/* dot removed from end of target */
#define ADDR_MOVEDOT	0x00000800	/* end dot moved to front of target */
#define ADDR_ERROR	0x00001000	/* error in resolving address */
#define ADDR_FINISHED	0x00002000	/* address fully resolved */
#define ADDR_FULLMATCH	0x00004000	/* router fully matched target */
#define ADDR_DONTHASH	0x00008000	/* addr states should not be hashed */
#define ADDR_SMARTUSER	0x00010000	/* smart user director already used */
#define ADDR_SMARTHOST	0x00020000	/* smart host router already used */
#define ADDR_NOTUSER	0x00040000	/* address is not a local user */
#define ADDR_ISUSER	0x00080000	/* address is a local user */
#define ADDR_FWDTYPE	0x00100000	/* director used was forwarding type */
#define ADDR_ALIASTYPE	0x00200000	/* director used was aliasing type */
#define ADDR_LISTTYPE	0x00400000	/* director was mailinglist type */
#define ADDR_SHADOW	0x00800000	/* using shadow transport */
#define ADDR_PARTLOCAL	0x01000000	/* partially matched local host */
#define ADDR_RETRY_FILE	0x02000000	/* on failure, touch retry file */
#define ADDR_FORM_MASK	0x0000000f	/* form from parse_address */
/*
 * NOTE:
 *	Routers can set ADDR_SMARTHOST if the smarthost router should
 *	not be used.  For example, if the local host is a gateway for
 *	a domain and a hostname within that domain cannot be resolved,
 *	then the router can set ADDR_SMARTHOST to prevent an incorrect
 *	usage of the smarthost router.
 *	Directors can set ADDR_SMARTUSER in a similar manner if this
 *	ever proves useful.
 *
 * The ADDR_PARTLOCAL should also be set for partial matches to the local
 * host.  If ADDR_PARTLOCAL is set for an address and ADDR_FULLMATCH
 * is not set, the target is not considered to be resolved.
 */

/* bits stored in error.info */
#define ERR_MASK	0x0000ffffL	/* mask for the error number */
#define ERR_NSENDER	0x00010000L	/* notify sender of message */
#define ERR_NPOSTMAST	0x00020000L	/* notify postmaster */
#define ERR_NSOWNER	0x00040000L	/* notify address owner or sender */
#define ERR_NPOWNER	0x00080000L	/* notify owner or postmaster */
#define ERR_CONFERR	0x00100000L	/* configuration error encountered */
#define ERR_DONTLOG	0x00200000L	/* don't log this error */

/*
 * user and group ids are preset to BOGUS_USER and BOGUS_GROUP in order
 * to prevent them from being mistaken for root and wheel
 */
#define BOGUS_USER (-1)			/* not a valid user id */
#define BOGUS_GROUP (-1)		/* not a valid group id */

/* return values from parse_address */
#define PARSE_ERROR	0		/* error in parsing */
#define RFC_ROUTE	1		/* route part of a route-addr */
#define RFC_ENDROUTE	2		/* last component of a route */
#define MAILBOX		3		/* standard user@foo mailbox */
#define UUCP_ROUTE	4		/* uucp !-route */
#define PCT_MAILBOX	5		/* non-standard user%foo mailbox */
#define LOCAL		6		/* local address */
#define BERKENET	7		/* berkenet host:user form */
#define DECNET		8		/* decnet host::user form */

/* flag values for the parse_address flagp argument */
#define FOUND_MAILBOX	0x0001		/* found user@host address form */
