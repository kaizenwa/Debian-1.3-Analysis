/*
#ident	"@(#)smail/src:RELEASE-3_2:default.c,v 1.29 1996/05/30 03:54:26 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * default:
 *	default values to be used in the event that no other info is given
 *
 * Items are placed in this file ONLY if they represent the default behavior
 * of smail and it's utilities, and ONLY if the default behavior can be changed
 * by `config.h' or the `startup' file.
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "config.h"
#include "smail.h"
#include "direct.h"
#include "route.h"
#include "transport.h"
#if defined(HAVE_BIND) && defined(USE_BIND)
# include "bindlib.h"
#endif
#ifndef DEPEND
# include "extern.h"
#endif
#include "directors/include.h"
#include "directors/aliasfile.h"
#include "directors/fwdfile.h"
#include "directors/user.h"
#include "directors/smartuser.h"
#include "routers/gethost.h"
#include "routers/uuname.h"
#include "routers/pathalias.h"
#include "routers/smarthost.h"
#include "routers/reroute.h"
#include "transports/pipe.h"
#include "transports/appendfile.h"
#include "transports/tcpsmtp.h"
#include "transports/smtplib.h"

/*
 * SUCC is set to point to the previously defined director, router or
 * transport, and is redefined after each new definition, to make it
 * simpler to optionally include links in the various configuration
 * chains.
 *
 * NOTE:  directors, routers and transports are listed in reverse order.
 *	  This causes `forward' links to work without declaring them in
 *	  advance.
 */


/*
 * START OF DIRECTOR DEFINITION SECTION
 */

#undef SUCC
#define SUCC	NULL			/* end of director list */

#ifdef SMART_USER
static struct smartuser_private smartuser_director_priv = {
    SMART_USER,				/* address for unknown users */
};

static struct director smartuser_director = {
    "smart_user",			/* mailing lists */
    "smartuser",			/* use the smartuser driver */
    SUCC,				/* point to next director */
    SMARTUSER_WELLFORMED,		/* match only well-formed addresses */
    NULL,				/* no address owner */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&smartuser_director_priv,	/* local private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
# undef SUCC
# define SUCC (&smartuser_director)	/* setup for forward link */
#endif	/* SMART_USER */


#ifdef LIST_FILENAME
/*
 * LIST DIRECTOR:
 *
 * This matches mailing lists files stored in the defined mailing list
 * directory.
 */
static struct forwardfile_private list_director_priv = {
    LIST_FILENAME,			/* file in mailing list directory */
    0,					/* don't worry about file modes */
    NULL,				/* no specific caution warnings */
    NULL,				/* no specific unsecure areas */
    NULL,				/* anybody can own these */
    NULL,				/* any group can own these */
    NULL,				/* no prefix */
    NULL,				/* no suffix */
};

static struct director list_director = {
    "lists",				/* mailing lists */
    "forwardfile",			/* use the forwardfile driver */
    SUCC,				/* point to next director */
#if !defined(LISTS_REMOVE_SENDER)
    SENDER_OKAY |
#endif
	CAUTION_DIRECTOR |
	NOBODY_DIRECTOR,		/* no interesting flags */
    LIST_OWNER_FILENAME,		/* normal address owner owners */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&list_director_priv,	/* local private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
# undef SUCC
# define SUCC (&list_director)		/* setup for forward link */
#endif	/* LIST_FILENAME */


/*
 * REAL_USER DIRECTOR:
 *
 * This matches users on the local host, with a prefix of "real-" and
 * associates them with the transport that appends to user mailbox
 * files.
 *
 * This director allows a user's mailbox to specified literally with
 * a form that won't go through aliasing or forwarding (generally).
 * This is useful in scenarios that cannot easily be solved without
 * mail loops, such as people who wish all mail to be forwarded to
 * multiple machines.
 */
static struct user_private real_user_director_priv = {
    "local",				/* use the local transport */
    "real-",				/* prefix */
    NULL,				/* password file (use system) */
};

static struct director real_user_director = {
    "real_user",			/* directing to real local users */
    "user",				/* use the user driver */
    SUCC,				/* point to next director */
    USER_IGNORE_CASE,			/* ignore case while matching pw file */
    NULL,				/* no address owner */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&real_user_director_priv,	/* local private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&real_user_director)	/* setup for forward link */


/*
 * USER DIRECTOR:
 *
 * This matches users on the local host and associates them with the
 * transport that appends to user mailbox files.
 */
static struct user_private user_director_priv = {
    "local",				/* use the local transport */
    NULL,				/* no prefix */
    NULL,				/* password file (use system) */
};

static struct director user_director = {
    "user",				/* directing to local users */
    "user",				/* use the user driver */
    SUCC,				/* point to next director */
    USER_IGNORE_CASE,			/* ignore case while matching pw file */
    NULL,				/* no address owner */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&user_director_priv,	/* local private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&user_director)		/* setup for forward link */


#ifdef	FORWARDTO_FILE
/*
 * FORWARDTO DIRECTOR:
 *
 * This looks for forwarding information in user mailbox files, for
 * mailbox files that begin with "Forward to [addr list ...]".
 */
static struct forwardfile_private forwardto_director_priv = {
    FORWARDTO_FILE,			/* Where to find forward to files */
#ifndef UNIX_SYS5
    0002,				/* mail group must write in SysV */
#else
    0022,				/* should not be very writable */
#endif
#ifdef FORWARDTO_CAUTION
    FORWARDTO_CAUTION,
#else
    "0-10:uucp:daemon",			/* be cautious of some user's files */
#endif
    NULL,				/* nothing is `unsecure' */
    "root",				/* can be owned by root */
    NULL,				/* no group ownership restriction */
    NULL,				/* no prefix */
    NULL,				/* no suffix */
};

static struct director forwardto_director = {
    "forwardto",			/* directing through alias file */
    "forwardfile",			/* use the forwardfile driver */
    SUCC,				/* point to next director */
    /*
     * secure source of addresses, owner should be the user, use
     * "nobody" for unsecure forward files
     */
    FWD_CHECKOWNER |
	FWD_FORWARDTO |
	NOBODY_DIRECTOR |
	SENDER_OKAY,
    "Postmaster",			/* errors go to the user */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&forwardto_director_priv,	/* forwardto private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&forwardto_director)	/* setup for forward link */
#endif	/* FORWARDTO_FILE */


#ifndef	DISABLE_DOTFORWARD
/*
 * DOTFORWARD DIRECTOR:
 *
 * This looks for .forward files in a user's home directory.
 */
static struct forwardfile_private dotforward_director_priv = {
    "~/.forward",			/* .forward in home directories */
    0002,				/* should not be globally writable */
#ifdef DOTFWARD_CAUTION
    DOTFORWARD_CAUTION,
#else
    "0-10:uucp:daemon",			/* be cautious of some forward file */
#endif
    /* remote access directories are very unsecure */
    REMOTE_HOMES,
    NULL,				/* can be owned by root */
    NULL,				/* no group ownership restriction */
    NULL,				/* no prefix */
    NULL,				/* no suffix */
};

static struct director dotforward_director = {
    "dotforward",			/* directing through alias file */
    "forwardfile",			/* use the aliasfile driver */
    SUCC,				/* point to next director */
    /*
     * secure source of addresses, owner should be the user, use "nobody"
     * for unsecure forward files
     */
    FWD_CHECKOWNER |
	NOBODY_DIRECTOR |
	SENDER_OKAY,
    "real-$user",			/* address errors go to user */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&dotforward_director_priv,	/* dotforward private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&dotforward_director)	/* setup for forward link */
#endif	/* DISABLE_DOTFORWARD */


#ifdef	ALIASES_FILE
/*
 * ALIASFILE DIRECTOR:
 *
 * The name of the alias file and its access method type is defined in
 * config.h, if an aliasfile is configured.
 */
static struct aliasfile_private aliases_director_priv = {
    ALIASES_FILE,			/* this is set in config.h */
    ALIASES_PROTO,			/* file access method */
    0002,				/* should not be globally writable */
    NULL,				/* no ownership restriction */
    NULL,				/* no group ownership restriction */
#ifdef	HAVE_RENAME
    /*
     * if we have an atomic rename call, retries should not be required
     * since the file should always exist, even when being rebuilt
     *
     * if the file is not found, the aliases director is ignored.
     */
    0,					/* no retries */
#else	/* HAVE_RENAME */
    /*
     * without an atomic rename call, there will be short periods
     * of time when the file does not exist, while it is being
     * rebuilt
     */
    2,					/* two retries on open */
#endif	/* HAVE_RENAME */
    10,					/* two second intervals for retries */
    0,					/* temp - addr flags bit mask */
    NULL,				/* temp - open database */
    NULL,				/* temp - error from open */
};

static struct director aliases_director = {
    "aliases",				/* directing through alias file */
    "aliasfile",			/* use the aliasfile driver */
    SUCC,				/* point to next director */
#if !defined(ALIASES_REMOVE_SENDER)
    SENDER_OKAY |
#endif
#ifdef ALIASES_OPTIONAL
	ALIAS_OPTIONAL |
#endif
#ifdef ALIASES_TRYAGAIN
	ALIAS_TRYAGAIN |
#endif
	0,				/* secure source of addresses */
    "owner-$user",			/* errors go to owner-<alias-name> */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    (char *)&aliases_director_priv,	/* aliases private configuration */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&aliases_director)	/* setup for forward link */
#endif	/* ALIASES_FILE */


#if	!defined(DISABLE_DOTFORWARD) || defined(FORWARDTO_FILE)
/*
 * FORWARDINCLUDE DIRECTOR:
 *
 * This is configured only if the dotforward or forwardto directors
 * are also configured in.  It is setup to copy security and ownership
 * restrictions from the dotforward/forwardto directors.
 */
static struct director forwardinclude_director = {
    "forwardinclude",			/* directing through include file */
    "forwardinclude",			/* use the aliasinclude driver */
    SUCC,				/* point to next director */
    COPY_SECURE | COPY_OWNERS |		/* restrictions from forward file */
	NOBODY_DIRECTOR,		/*  */
    NULL,				/* no address owner */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    NULL,				/* no private data */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&forwardinclude_director)	/* setup for forward link */
#endif	/* !defined(DISABLE_DOTFORWARD) || defined(FORWARDTO_FILE) */


#ifdef	ALIASES_FILE
/*
 * ALIASINCLUDE DIRECTOR:
 *
 * This is configured only if the aliases director is also configured
 * in.  It is setup to copy security and ownership restrictions from
 * the aliases file director.
 */
static struct director aliasinclude_director = {
    "aliasinclude",			/* directing through include file */
    "aliasinclude",			/* use the aliasinclude driver */
    SUCC,				/* point to next director */
    COPY_SECURE | COPY_OWNERS |		/* restrictions from aliases */
	NOBODY_DIRECTOR,		/*  */
    NULL,				/* no address owner */
    NULL,				/* no default user */
    NULL,				/* no default group */
    NULL,				/* no default home director */
    NULL,				/* no specific domains */
    NULL,				/* no explicit user */
    NULL,				/* no explicit group */
    NULL,				/* no explicit home directory */
    NULL,				/* no private data */
    0,					/* cache - default uid */
    0,					/* cache - default gid */
    NULL,				/* cache - expanded default home */
    0,					/* cache - set uid */
    0,					/* cache - set gid */
    NULL,				/* cache - expanded set home */
};
#undef SUCC
#define SUCC (&aliasinclude_director)	/* setup for forward link */
#endif	/* ALIASES_FILE */


/* point to the first director */
struct director *directors = SUCC;


/*
 * START OF ROUTER DEFINITION SECTION
 */

#undef SUCC
#define SUCC	NULL			/* end of router list */

#ifdef USE_METHOD_TABLE			/* not defined by default */
/*
 * A method table used by the smart_host, uuname, pathalias, reroute,
 * force_paths, and force_smtp routers.
 */
static struct method method_table[] = {
/* you could list sites you wish to poll on demand, for example:
 *  { "ihnp4",	-1, -1, "demand" },
 *  { "mcvax",	-1, -1, "demand" },
 * if some sites require a different transport, make a transport
 * entry and give its name instead of "demand" or "uux".
 *
 * The demand transport calls uux without the -r flag so that uucico is
 * started immediately to perform delivery.
 *
 * The uux transport calls uux with the -r flag, so that requests are
 * just queued, uucico is not started to perform delivery immediately.
 */
    { "*/*-C",	-1, -1, "demand" },	/* use "demand" for high-grade messages */
    { "*",	-1, -1, "uux" },	/* use "uux" for all other hosts */
    { NULL,	-1, -1, 0},		/* end of first table */
};
# define METHOD_TABLE method_table

#else	/* not USE_METHOD_TABLE */

# define METHOD_TABLE NULL		/* default definition as per config.h */

#endif	/* not USE_METHOD_TABLE */


#if	defined(SMART_PATH)
/*
 * SMARTHOST ROUTER:
 *
 * This router declares a remote host to which mail should be sent for
 * hosts that we cannot resolve ourselves.  Presumably this remote
 * host has a more complete or more up-to-date collection of routing
 * databases than we do ourselves.
 */
static struct smarthost_private smarthost_router_priv = {
    SMART_PATH,
    NULL,
    NULL,
};

static struct router smarthost_router = {
    "smart_host",			/* routing to smarthost */
    "smarthost",			/* use the smarthost driver */
    SUCC,				/* point to next router */
    0,					/* no options turned on */
    METHOD_TABLE,			/* use method table, if non-NULL */
    SMART_TRANSPORT,			/* default transport given */
    (char *)&smarthost_router_priv,	/* smarthost private configuration */
};
#undef SUCC
#define SUCC (&smarthost_router)	/* setup for forward link */
#endif	/* defined(SMART_PATH) */


#ifdef	UUNAME_COMMAND
/*
 * UUNAME ROUTER:
 *
 * The name for the uuname or a uuname-style program is configured in
 * config.h, if one is to be used for finding neighboring host names.
 */
static struct uuname_private uuname_router_priv = {
    UUNAME_COMMAND,			/* cmd attribute */
    "uucp",				/* default domains */
    NULL,				/* required domains */
#ifdef UUCP_SYSTEM_FILE
    UUCP_SYSTEM_FILE,			/* file to stat for changes */
#else
    NULL,
#endif
    NULL,				/* internal - cached output from cmd */
    NULL,				/* internal - end of cached output */
    NULL,				/* internal - processing error */
    NULL,				/* internal - st_ino from stat */
    NULL,				/* internal - st_mtime from stat */
};

static struct router uuname_router = {
    "uucp_neighbors",			/* routing through uuname output */
    "uuname",				/* use the uuname driver */
    SUCC,				/* point to next router */
    0,					/* no options turned on */
    METHOD_TABLE,			/* use method table from pathalias */
    UUNAME_TRANSPORT,			/* default transport */
    (char *)&uuname_router_priv,	/* uuname private configuration */
};
#undef SUCC
#define SUCC (&uuname_router)		/* setup for forward link */
#endif	/* UUNAME_COMMAND */


#ifdef	PATHS_FILE
/*
 * PATHALIAS ROUTER:
 *
 * The name of the paths file and the access method type is defined in
 * config.h, if a pathalias database is configured.
 */
static struct pathalias_private pathalias_router_priv = {
    PATHS_FILE,				/* file attribute */
    PATHS_PROTO,			/* file access method */
    "uucp",				/* default domain names */
    NULL,				/* no required domain names */
#ifdef	HAVE_RENAME
    /*
     * if we have an atomic rename call, retries should not be required
     * since the file should always exist, even when being rebuilt
     *
     * if the file is not found, the pathalias director is ignored.
     */
    0,					/* no retries */
#else	/* HAVE_RENAME */
    /*
     * without an atomic rename call, there will be short periods
     * of time when the file does not exist, while it is being
     * rebuilt
     */
    2,					/* two retries on open */
#endif	/* HAVE_RENAME */
    10,					/* ten second intervals for retries */
    NULL,				/* internal - open database */
    NULL,				/* internal - error text from open */
};

static struct router pathalias_router = {
    "paths",				/* routing through pathalias file */
    "pathalias",			/* use the pathalias driver */
    SUCC,				/* point to next router */
#ifdef PATHS_OPTIONAL
    PA_OPTIONAL |			/* the paths file is optional */
#endif
#ifdef PATHS_TRYAGAIN
	PA_TRYAGAIN |			/* defer address on open failure */
#endif
	PA_REOPEN,			/* always reopen database to search */
    METHOD_TABLE,			/* use method table to find tport */
    PATHS_TRANSPORT,			/* default transport */
    (char *)&pathalias_router_priv,	/* pathalias private configuration */
};
#undef SUCC
#define SUCC (&pathalias_router)	/* setup for forward link */
#endif	/* PATHS_FILE */


#ifdef	USE_GETHOSTBYNAME
/*
 * GETHOSTBYNAME ROUTER:
 *
 * Define a fairly generic interface to gethostbyname(3N)
 */

static struct gethostbyname_private inet_hosts_router_priv = {
    NULL,				/* no optional domain names */
    NULL,				/* no required domain names */
};

static struct router inet_hosts_router = {
    "inet_hosts",			/* match hosts on an IP network */
    "gethostbyname",			/* use the gethostbyname driver */
    SUCC,				/* point to next router */
    0,					/* no options turned on */
    (struct method *)NULL,		/* no method table */
    GETHOSTBYNAME_TRANSPORT,		/* default transport */
    (char *)&inet_hosts_router_priv,	/* private configuration */
};
#undef SUCC
#define SUCC (&inet_hosts_router)	/* setup for forward link */
#endif	/* USE_GETHOSTBYNAME */


#if defined(HAVE_BIND) && defined(USE_BIND)
/*
 * BIND ROUTER:
 *
 * Define a fairly generic interface to resolver(3N)
 */

static struct bindlib_private bind_hosts_router_priv = BIND_TEMPLATE_ATTRIBUTES;

static struct router bind_hosts_router = {
    "bind_hosts",			/* match hosts on an IP network */
    "bind",				/* use the bind driver */
    SUCC,				/* point to next router */
# if 0
    BIND_DEFER_NO_CONN |		/* this is the default claimed by smail(5) */
	BIND_DEFNAMES,
# else
    BIND_DEFER_NO_CONN |		/* this makes more sense for modern DNS hosts */
	BIND_DOMAIN_REQUIRED,
# endif
    (struct method *)NULL,		/* no method table */
    BIND_TRANSPORT,			/* default transport */
    (char *)&bind_hosts_router_priv,	/* private configuration */
};
#undef SUCC
#define SUCC (&bind_hosts_router)	/* setup for forward link */
#endif	/* USE_BIND */


#ifdef	USE_GETHOSTBYADDR
/*
 * GETHOSTBYADDR ROUTER:
 *
 * Define a fairly generic interface to gethostbyaddr(3N) and the
 * inet_addr(3N) routines.
 */

static struct router inet_addrs_router = {
    "inet_addrs",			/* match IP addrs on an IP network */
    "gethostbyaddr",			/* use the gethostbyaddr driver */
    SUCC,				/* point to next router */
    GETHOST_FAIL_IFERR |
	GETHOST_CHECK_LOCAL,		/* fail_if_error + check_local */
    (struct method *)NULL,		/* no method table */
    GETHOSTBYADDR_TRANSPORT,		/* default transport */
    (char *)NULL,			/* no private configuration */
};
#undef SUCC
#define SUCC (&inet_addrs_router)	/* setup for forward link */
#endif	/* USE_GETHOSTBYADDR */


#ifdef EXPERIMENTAL_REROUTER

/*
 * REROUTE ROUTER:
 *
 * By default, this router is used only to collaps bang path addresses
 * in bounce mails.  This has to be the first router in the calling
 * sequence.
 */
static struct reroute_private reroute_router_priv = {
    NULL,				/* no file attribute */
    NULL,				/* no file access method */
    NULL,				/* no default domain names */
    NULL,				/* no required domain names */
    0,					/* no retries */
    0,					/* no retries */
    NULL,				/* internal - open database */
    NULL,				/* internal - error text from open */
};

static struct router reroute_router = {
    "reroute",				/* routing through rerouter */
    "reroute",				/* use the reroute driver */
    SUCC,				/* point to next router */
    RR_MATCHLOCAL | RR_BOUNCEONLY,	/* default options */
    METHOD_TABLE,			/* use method table to find tport */
    PATHS_TRANSPORT,			/* default transport */
    (char *)&reroute_router_priv,	/* reroute private configuration */
};
#undef SUCC
#define SUCC (&reroute_router)		/* setup for forward link */

#endif	/* EXPERIMENTAL_REROUTER */


#if defined(FORCE_PATHS_FILE)
/*
 * FORCE_PATH ROUTER:
 *
 * The name of the force_paths file and the access method type is
 * defined in config.h, if a forcepaths database is configured.
 */
static struct pathalias_private force_paths_router_priv = {
    FORCE_PATHS_FILE,			/* file attribute */
    FORCE_PATHS_PROTO,			/* file access method */
    NULL,				/* default domain names */
    NULL,				/* no required domain names */
    2,					/* two retries on open */
    10,					/* ten second intervals for retries */
    NULL,				/* internal - open database */
    NULL,				/* internal - error text from open */
};

static struct router force_paths_router = {
    "force_paths",			/* routing through forcepaths file */
    "pathalias",			/* use the pathalias driver */
    SUCC,				/* point to next router */
    USE_ALWAYS |
#ifdef FORCE_PATHS_OPTIONAL
	PA_OPTIONAL |			/* the paths file is optional */
#endif
#ifdef FORCE_PATHS_TRYAGAIN
	PA_TRYAGAIN |			/* defer address on open failure */
#endif
	PA_REOPEN,			/* always reopen database to search */
    METHOD_TABLE,			/* use method table to find transport */
    FORCE_PATHS_TRANSPORT,		/* default transport */
    (char *)&force_paths_router_priv,	/* force_paths private configuration */
};
#undef SUCC
#define SUCC (&force_paths_router)	/* setup for forward link */

#endif	/* FORCE_PATHS_FILE */


#if defined(FORCE_SMTP_FILE)
/*
 * FORCE_SMTP ROUTER:
 *
 * The name of the force_smtp file and the access method type is
 * defined in config.h, if a forcesmtp database is configured.
 */
static struct pathalias_private force_smtp_router_priv = {
    FORCE_SMTP_FILE,			/* file attribute */
    FORCE_SMTP_PROTO,			/* file access method */
    NULL,				/* default domain names */
    NULL,				/* no required domain names */
    2,					/* two retries on open */
    10,					/* ten second intervals for retries */
    NULL,				/* internal - open database */
    NULL,				/* internal - error text from open */
};

static struct router force_smtp_router = {
    "force_smtp",			/* routing through forcesmtp file */
    "pathalias",			/* use the pathalias driver */
    SUCC,				/* point to next router */
    USE_ALWAYS |
#ifdef FORCE_SMTP_OPTIONAL
	PA_OPTIONAL |			/* the paths file is optional */
#endif
#ifdef FORCE_SMTP_TRYAGAIN
	PA_TRYAGAIN |			/* defer address on open failure */
#endif
	PA_REOPEN,			/* always reopen database to search */
    (struct method *)NULL,		/* no method table */
    FORCE_SMTP_TRANSPORT,		/* default transport */
    (char *)&force_smtp_router_priv,	/* force_paths private configuration */
};
#undef SUCC
#define SUCC (&force_smtp_router)	/* setup for forward link */

#endif	/* FORCE_SMTP_FILE */


/* point to the first router */
struct router *routers = SUCC;


/*
 * START OF TRANSPORT DEFINITION SECTION
 *
 * The order is not important for transports.
 */

#undef SUCC
#define SUCC	NULL			/* end of transport list */

/*
 * LOCAL TRANSPORT:
 *
 * Local mail delivery is done either by appending to a file, or by
 * sending to a command.  If a command is defined, as LOCAL_MAIL_COMMAND
 * in config.h, use it.  Otherwise a filename must be defined as
 * LOCAL_MAIL_FILE.  Produce a compile error if nothing is configured.
 */

#ifdef	LOCAL_MAIL_COMMAND

static struct pipe_private local_transport_priv = {
    LOCAL_MAIL_COMMAND,			/* example "/bin/mail -d $($user$) */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport local_transport = {
    "local",				/* local transport to users */
    "pipe",				/* use the pipe driver */
    SUCC,				/* point to next transport */
    /* put Received:, Return-Path:, will do local delivery */
    PUT_RECEIVED | PUT_RETURNPATH | LOCAL_TPORT | LOCAL_XFORM, /* LOCAL_XFORM really???? */
# ifdef	LOCAL_MAIL_ADDRS
    /* if there is a specific limit on number of addrs from config.h,
     * use that in assigning transport instances, otherwise limit is 1,
     * 2000-char limit on addrs is an arbitrary large number.  Should
     * always be noticeably smaller than NCARGS */
    LOCAL_MAIL_ADDRS, 1, 2000,		/* limits for assigning transports */
# else	/* LOCAL_MAIL_ADDRS */
    1, 1, 0,				/* 1 addr, 1 host, size not useful */
# endif	/* LOCAL_MAIL_ADDRS */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&local_transport_priv,	/* local private configuration */
};
#undef SUCC
#define SUCC (&local_transport)		/* setup for forward link */

#else	/* LOCAL_MAIL_COMMAND */

static struct appendfile_private local_transport_priv = {
    LOCAL_MAIL_FILE,			/* e.g.: /usr/spool/mail/${lc:user} */
    NULL,				/* directory does not apply */
    NULL,				/* run as the particular user */
# if defined(UNIX_SYS5) && !defined(UNIX_SUN_OS) && !defined(UNIX_BSD)
    "mail",				/* under System V use group "mail" */
# else
    NULL,				/* otherwise, no group in particular */
# endif
    NULL,				/* no message prefix */
    "\n",				/* end message with extra newline */
    LOCAL_MAIL_MODE,			/* file creation mode */
};

static struct transport local_transport = {
    "local",				/* local transport to users */
    "appendfile",			/* use the appendfile driver */
    SUCC,				/* point to next transport */
    /* put Received:, Return-Path: and From_ line, will do local delivery */
    PUT_RECEIVED | PUT_RETURNPATH | LOCAL_TPORT | LOCAL_XFORM | PUT_FROM |
	APPEND_AS_USER | UNIX_FROM_HACK | APPEND_COMSAT,
    1, 1, 0,				/* 1 addr, 1 host, size not useful */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&local_transport_priv,	/* local private configuration */
};
#undef SUCC
#define SUCC (&local_transport)		/* setup for forward link */
#endif	/* LOCAL_MAIL_COMMAND */


/*
 * PIPE TRANSPORT:
 *
 * Run a shell command by piping the message to the shell.  Shell
 * commands can only be produced if the aliasfile and forwardfile
 * drivers are used.
 */
static struct pipe_private pipe_transport_priv = {
    "/bin/sh -c $user",			/* command is passed as next_addr */
    NULL,				/* use uid of the recipient */
    NULL,				/* use gid of the recipient */
    0022,				/* umask for child process */
};

static struct transport pipe_transport = {
    "pipe",				/* pipe transport to shell commands */
    "pipe",				/* use the pipe driver */
    SUCC,				/* point to next transport */
    /* put Recieved:, Return-Path: and From_ line, will do local delivery */
    PUT_RECEIVED | PUT_RETURNPATH | LOCAL_TPORT | LOCAL_XFORM | PUT_FROM |
	UNIX_FROM_HACK |
	PIPE_AS_USER | PIPE_PARENT_ENV | PIPE_IGNORE_STATUS | PIPE_IGNORE_WRERRS,
    1, 1, 0,				/* 1 addr, 1 host, size not useful */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&pipe_transport_priv,	/* pipe private configuration */
};
#undef SUCC
#define SUCC (&pipe_transport)		/* setup for forward link */


/*
 * FILE TRANSPORT:
 *
 * Append to a specific filename.  File addresses can only be produced
 * if the aliasfile and forwardfile drivers are used.
 */
static struct appendfile_private file_transport_priv = {
    "$user",				/* the file is passed as next_addr */
    NULL,				/* no dir attribute */
    NULL,				/* use uid of the recipient */
    NULL,				/* use gid of the recipient */
    NULL,				/* prefix string */
    "\n",				/* suffix string */
    0644,				/* mode for creation */
};

static struct transport file_transport = {
    "file",				/* append to specific files */
    "appendfile",			/* use the apppendfile driver */
    SUCC,				/* point to next transport */
    /* put Recieved:, Return-Path: and From_ line, will do local delivery */
    PUT_RECEIVED | PUT_RETURNPATH | LOCAL_TPORT | LOCAL_XFORM | PUT_FROM |
	UNIX_FROM_HACK |
	/* also append to file with uid/gid from addr structure */
	APPEND_AS_USER |
	/* expand the user name so ~ expansions can occur */
	APPEND_EXPAND_USER,
    1, 1, 0,				/* 1 addr, 1 host, size not useful */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&file_transport_priv,	/* pipe private configuration */
};
#undef SUCC
#define SUCC (&file_transport)		/* setup for forward link */


/*
 * UUX TRANSPORT:
 *
 * This calls uux with the -r flag so that actual delivery is not
 * attempted immediately.  Presumably a cronjob will start up uucico at
 * some later time to actually attempt to send the mail to the remote
 * machine.
 */
static struct pipe_private uux_transport_priv = {
    QUEUED_UUX_COMMAND,			/* queued uux to remote rmail */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport uux_transport = {
    "uux",				/* queued transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* only use !-routes in message envelope, supply a Received: field
     * and begin the message with a From_ line. */
    UUCP_XFORM | PUT_RECEIVED | PUT_FROM | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    5, 1, 200,				/* 5 addresses, 1 host, 200 chars */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&uux_transport_priv,	/* uux private configuration */
};
#undef SUCC
#define SUCC (&uux_transport)		/* setup for forward link */


/*
 * DEMAND TRANSPORT:
 *
 * This calls uux without the -r flag so that delivery is attempted
 * immediately.
 */
static struct pipe_private demand_transport_priv = {
    DEMAND_UUX_COMMAND,			/* demand uux to remote rmail */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport demand_transport = {
    "demand",				/* immediate transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* only use !-routes in message envelope, supply a Received: field
     * and begin the message with a From_ line. */
    UUCP_XFORM | PUT_RECEIVED | PUT_FROM | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    5, 1, 200,				/* 5 addresses, 1 host, 200 chars */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&demand_transport_priv,	/* demand private configuration */
};
#undef SUCC
#define SUCC (&demand_transport)	/* setup for forward link */


/*
 * LOCAL UUX TRANSPORT:
 *
 * This calls uux with the -r flag so that actual delivery is not
 * attempted immediately.  Presumably a cronjob will start up uucico at
 * some later time to actually attempt to send the mail to the remote
 * machine.
 *
 * The message is transferred in local format.
 */
static struct pipe_private local_uux_transport_priv = {
    QUEUED_UUX_COMMAND,			/* queued uux to remote rmail */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport local_uux_transport = {
    "local_uux",			/* queued transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* use local format, supply a Received: field
     * and begin the message with a From_ line. */
    LOCAL_XFORM | UUCP_XFORM | PUT_RECEIVED | PUT_FROM | PIPE_AS_SENDER |
	PIPE_LOG_OUTPUT,
    5, 1, 200,				/* 5 addresses, 1 host, 200 chars */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&local_uux_transport_priv,	/* uux private configuration */
};
#undef SUCC
#define SUCC (&local_uux_transport)	/* setup for forward link */


/*
 * LOCAL DEMAND TRANSPORT:
 *
 * This calls uux without the -r flag so that actual delivery is
 * attempted immediately.
 *
 * The message is transferred in local format.
 */
static struct pipe_private ldemand_transport_priv = {
    DEMAND_UUX_COMMAND,			/* demand uux to remote rmail */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport ldemand_transport = {
    "local_demand",			/* demand transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* use local format supply a Received: field
     * and begin the message with a From_ line. */
    LOCAL_XFORM | PUT_RECEIVED | PUT_FROM | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    5, 1, 200,				/* 5 addresses, 1 host, 200 chars */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&ldemand_transport_priv,	/* demand private configuration */
};
#undef SUCC
#define SUCC (&ldemand_transport)	/* setup for forward link */


/*
 * QUEUED UUSMTP TRANSPORT:
 *
 * This calls uux with the -r flag so that actual delivery is not
 * attempted immediately.  Presumably a cronjob will start up uucico at
 * some later time to actually attempt to send the mail to the remote
 * machine.
 *
 * The message is transferred in uucp format.
 */
static struct pipe_private uusmtp_transport_priv = {
    QUEUED_UUSMTP_COMMAND,		/* queued uux to remote rsmtp */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport uusmtp_transport = {
    "uusmtp",				/* demand transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* supply a Received: field
     * and put an SMTP envelope around the message. */
    UUCP_XFORM | PUT_RECEIVED | BSMTP_TPORT | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    0, 1, 0,				/* 1 host */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&uusmtp_transport_priv,	/* demand private configuration */
};
#undef SUCC
#define SUCC (&uusmtp_transport)	/* setup for forward link */


/*
 * DEMAND UUSMTP TRANSPORT:
 *
 * This calls uux without the -r flag so that actual delivery is
 * attempted immediately.
 *
 * The message is transferred in uucp format.
 */
static struct pipe_private demand_uusmtp_transport_priv = {
    DEMAND_UUSMTP_COMMAND,		/* demand uux to remote rsmtp */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport demand_uusmtp_transport = {
    "demand_uusmtp",			/* demand transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* supply a Received: field
     * and put an SMTP envelope around the message. */
    PUT_RECEIVED | BSMTP_TPORT | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    0, 1, 0,				/* 1 host */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&demand_uusmtp_transport_priv, /* demand private configuration */
};
#undef SUCC
#define SUCC (&demand_uusmtp_transport)	/* setup for forward link */


/*
 * LOCAL QUEUED UUSMTP TRANSPORT:
 *
 * This calls uux with the -r flag so that actual delivery is not
 * attempted immediately.  Presumably a cronjob will start up uucico at
 * some later time to actually attempt to send the mail to the remote
 * machine.
 *
 * The message is transferred in local format.
 */
static struct pipe_private local_uusmtp_transport_priv = {
    QUEUED_UUSMTP_COMMAND,		/* queued uux to remote rsmtp */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport local_uusmtp_transport = {
    "local_uusmtp",			/* demand transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* supply a Received: field
     * and put an SMTP envelope around the message. */
    LOCAL_XFORM | PUT_RECEIVED | BSMTP_TPORT | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    0, 1, 0,				/* 1 host */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&local_uusmtp_transport_priv, /* demand private configuration */
};
#undef SUCC
#define SUCC (&local_uusmtp_transport)	/* setup for forward link */


/*
 * LOCAL DEMAND UUSMTP TRANSPORT:
 *
 * This calls uux without the -r flag so that actual delivery is
 * attempted immediately.
 *
 * The message is transferred in local format.
 */
static struct pipe_private ldemand_uusmtp_transport_priv = {
    DEMAND_UUSMTP_COMMAND,		/* demand uux to remote rsmtp */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport ldemand_uusmtp_transport = {
    "local_demand_uusmtp",		/* demand transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* supply a Received: field
     * and put an SMTP envelope around the message. */
    LOCAL_XFORM | PUT_RECEIVED | BSMTP_TPORT | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    0, 1, 0,				/* 1 host */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&ldemand_uusmtp_transport_priv,
    					/* demand private configuration */
};
#undef SUCC
#define SUCC (&ldemand_uusmtp_transport) /* setup for forward link */


/*
 * INET QUEUED UUSMTP TRANSPORT:
 *
 * This calls uux with the -r flag so that actual delivery is not
 * attempted immediately.  Presumably a cronjob will start up uucico at
 * some later time to actually attempt to send the mail to the remote
 * machine.
 *
 * The message is transferred in internet format.
 */
static struct pipe_private inet_uusmtp_transport_priv = {
    QUEUED_UUSMTP_COMMAND,		/* queued uux to remote rsmtp */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport inet_uusmtp_transport = {
    "inet_uusmtp",			/* batched smtp transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* supply a Received: field
     * and put an SMTP envelope around the message. */
    INET_XFORM | PUT_RECEIVED | BSMTP_TPORT | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    0, 1, 0,				/* 1 host */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&inet_uusmtp_transport_priv, /* demand private configuration */
};
#undef SUCC
#define SUCC (&inet_uusmtp_transport)	/* setup for forward link */


/*
 * LOCAL DEMAND UUSMTP TRANSPORT:
 *
 * This calls uux without the -r flag so that actual delivery is
 * attempted immediately.
 *
 * The message is transferred in internet format.
 */
static struct pipe_private idemand_uusmtp_transport_priv = {
    DEMAND_UUSMTP_COMMAND,		/* demand uux to remote rsmtp */
    /* to run uux as other than the nobody user, put the login name here */
    NULL,				/* execute the program as nobody */
    NULL,				/* no specific group */
    0022,				/* umask for child process */
};

static struct transport idemand_uusmtp_transport = {
    "inet_demand_uusmtp",		/* demand transport over uux */
    "pipe",				/* uses pipe transport driver */
    SUCC,				/* point to next transport */
    /* supply a Received: field
     * and put an SMTP envelope around the message. */
    INET_XFORM | PUT_RECEIVED | BSMTP_TPORT | PIPE_AS_SENDER | PIPE_LOG_OUTPUT,
    0, 1, 0,				/* 1 host */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    NULL,				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&idemand_uusmtp_transport_priv,
    					/* demand private configuration */
};
#undef SUCC
#define SUCC (&idemand_uusmtp_transport) /* setup for forward link */


#ifdef USE_SMTP_TRANSPORT

#ifndef SMTP_SHORT_TIMEOUT
#define SMTP_SHORT_TIMEOUT (5*60)	/* short timeout, 5 minutes */
#endif

#ifndef SMTP_LONG_TIMEOUT
#define SMTP_LONG_TIMEOUT (5*60*60)	/* long timeout, 2 hours */
#endif

#ifndef SMTP_SERVICE_ENTRY
#define SMTP_SERVICE_ENTRY "smtp"	/* use the "smtp" service */
#endif

/*
 * SMTP TRANSPORTS:
 *
 * This initiates an SMTP conversation with a remote host over TCP/IP.
 *
 * The "smtp" transport transfers in ineternet format unless the
 * UUCP_ZONE flag is defined in the EDITME file.
 *
 * The "uucp_zone_smtp" transport transfers in uucp format.
 * The "inet_zone_smtp" transport transfers in internet format.
 * The "local_smtp" transport transfers in local format.
 */

static struct tcpsmtp_private smtp_transport_priv = {
    SMTP_SHORT_TIMEOUT,
    SMTP_LONG_TIMEOUT,
    SMTP_SERVICE_ENTRY,
#if defined(HAVE_BIND)
    BIND_TEMPLATE_ATTRIBUTES,
#endif
};

static struct transport smtp_transport = {
    "smtp",				/* smtp to remote host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
#ifdef UUCP_ZONE
    UUCP_XFORM |
#else
    INET_XFORM |
#endif
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* no private configuration */
};
#undef SUCC
#define SUCC (&smtp_transport)		/* setup for forward link */

static struct transport uucp_zone_smtp_transport = {
    "uucp_zone_smtp",			/* smtp to remote uucp-zone host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
    UUCP_XFORM |
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* generic smtp private configuration */
};
#undef SUCC
#define SUCC (&uucp_zone_smtp_transport) /* setup for forward link */

static struct transport inet_zone_smtp_transport = {
    "inet_zone_smtp",			/* smtp to remote internet host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
    INET_XFORM |
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* generic smtp private configuration */
};
#undef SUCC
#define SUCC (&inet_zone_smtp_transport) /* setup for forward link */

static struct transport local_smtp_transport = {
    "local_smtp",			/* smtp to local internet host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
    LOCAL_XFORM |
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* generic smtp private configuration */
};
#undef SUCC
#define SUCC (&local_smtp_transport) /* setup for forward link */

# if defined(HAVE_BIND) && defined(USE_BIND)
static struct transport inet_zone_bind_smtp_transport = {
    "inet_zone_bind_smtp",		/* smtp to remote host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
    BIND_DEFER_NO_CONN |		/* this makes more sense for modern DNS hosts */
	BIND_DOMAIN_REQUIRED |
	TCPSMTP_USE_BIND |
	INET_XFORM |
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* no private configuration */
};
#undef SUCC
#define SUCC (&inet_zone_bind_smtp_transport) /* setup for forward link */

static struct transport uucp_zone_bind_smtp_transport = {
    "uucp_zone_bind_smtp",		/* smtp to remote uucp-zone host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
    BIND_DEFER_NO_CONN |		/* this makes more sense for modern DNS hosts */
	BIND_DOMAIN_REQUIRED |
	TCPSMTP_USE_BIND |
	UUCP_XFORM |			/* this probably isn't of much use.... */
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* generic smtp private configuration */
};
#undef SUCC
#define SUCC (&uucp_zone_bind_smtp_transport) /* setup for forward link */

static struct transport local_bind_smtp_transport = {
    "local_bind_smtp",			/* smtp to local internet host */
    "tcpsmtp",				/* uses smtp transport driver */
    SUCC,				/* point to next transport */
    BIND_DEFER_NO_CONN |		/* this makes more sense for modern DNS hosts */
	BIND_DOMAIN_REQUIRED |
	TCPSMTP_USE_BIND |
	LOCAL_XFORM |			/* this probably isn't of much use.... */
	PUT_RECEIVED,			/* write a Received: field */
    0, 1, 0,				/* 1 host, no other limit */
    NULL, NULL, NULL,			/* hdrremove, hdrinsert, hdrappend */
    "smtp",				/* retry directory */
    NULL,				/* shadow transport */
    NULL,				/* error transport */
    (char *)&smtp_transport_priv,	/* generic smtp private configuration */
};
#undef SUCC
#define SUCC (&local_bind_smtp_transport) /* setup for forward link */

# endif /* HAVE_BIND && USE_BIND */

#endif	/* USE_SMTP_TRANSPORT */


/* point to the first transport */
struct transport *transports = SUCC;
struct transport *builtin_transports = SUCC;
