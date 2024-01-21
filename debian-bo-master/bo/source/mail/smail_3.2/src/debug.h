/*
#ident	"@(#)smail/src:RELEASE-3_2:debug.h,v 1.8 1996/05/29 18:48:18 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * debug.h:
 *	macros used to generate debugging output.
 */

/*
 * If the current debug level is above d, output the message, otherwise
 * be silent.
 */
#ifndef NODEBUG
# define DEBUG(d,m) \
     {						\
	 if (d <= debug && errfile) {		\
	     (void)fputs(m,errfile);		\
	     (void)fflush(errfile);		\
	 }					\
     }
# define DEBUG1(d,m,a) \
    {						\
	if (d <= debug && errfile) {		\
	    (void)dprintf(errfile,m,a);		\
	    (void)fflush(errfile);		\
	}					\
    }
# define DEBUG2(d,m,a,b) \
    {						\
	if (d <= debug && errfile) {		\
	    (void)dprintf(errfile,m,a,b);	\
	    (void)fflush(errfile);		\
	}					\
    }
# define DEBUG3(d,m,a,b,c) \
    {						\
	if (d <= debug && errfile) {		\
	    (void)dprintf(errfile,m,a,b,c);	\
	    (void)fflush(errfile);		\
	}					\
    }
# define DEBUG4(d,m,a,b,c,e) \
    {						\
	if (d <= debug && errfile) {		\
	    (void)dprintf(errfile,m,a,b,c,e);	\
	    (void)fflush(errfile);		\
	}					\
    }
# define DEBUG5(d,m,a,b,c,e,f) \
    {						\
	if (d <= debug && errfile) {		\
	    (void)dprintf(errfile,m,a,b,c,e,f);	\
	    (void)fflush(errfile);		\
	}					\
    }
#else	/* NODEBUG */
# define DEBUG(d,m)
# define DEBUG1(d,m,a)
# define DEBUG2(d,m,a,b)
# define DEBUG3(d,m,a,b,c)
# define DEBUG4(d,m,a,b,c,e)
# define DEBUG4(d,m,a,b,c,e,f)
#endif	/* NODEBUG */

/*
 * put various debug levels here so that we can change this file and
 * recompile, rather than changing the DEBUG statements themselves to
 * concentrate on a particular routine
 */

/* debug levels for functions in field.c */
#define DBG_FIELD_HI	50
#define DBG_FIELD_MID	9
#define DBG_FIELD_LO	6

/* debug levels for functions in addr.c */
#define DBG_ADDR_HI	50
#define DBG_ADDR_MID	9
#define DBG_ADDR_LO	6

/* debug levels for functions in log.c */
#define DBG_LOG_HI	50
#define DBG_LOG_MID	5
#define DBG_LOG_LO	1

/* debug levels for functions in queue.c */
#define DBG_QUEUE_HI	50
#define DBG_QUEUE_MID	5
#define DBG_QUEUE_LO	1

/* debug levels for functions in retry.c */
#define DBG_RETRY_HI	50
#define DBG_RETRY_MID	5
#define DBG_RETRY_LO	1

/* debug levels for functions in spool.c */
#define DBG_SPOOL_HI	50
#define DBG_SPOOL_MID	5
#define DBG_SPOOL_LO	2

/* debug levels for functions in header.c */
#define DBG_HEADER_HI	50
#define DBG_HEADER_MID	9
#define DBG_HEADER_LO	6

/* debug levels for directing code in direct.c */
#define DBG_DIRECT_HI	50
#define DBG_DIRECT_MID	8
#define DBG_DIRECT_LO	1

/* debug levels for routing code in route.c */
#define DBG_ROUTE_HI	50
#define DBG_ROUTE_MID	8
#define DBG_ROUTE_LO	1

/* debug levels for main code in main.c */
#define DBG_MAIN_HI	50
#define DBG_MAIN_MID	8
#define DBG_MAIN_LO	2

/* debug levels for notification code in notify.c */
#define DBG_NOTIFY_HI	50
#define DBG_NOTIFY_MID	5
#define DBG_NOTIFY_LO	1

/* debug levels for drivers */
#define DBG_DRIVER_HI	20
#define DBG_DRIVER_MID	2
#define DBG_DRIVER_LO	1

/* debug levels for resolving code in resolve.c */
#define DBG_RESOLVE_HI	50
#define DBG_RESOLVE_MID	8
#define DBG_RESOLVE_LO	1

/* debug levels for remote delivery code in remote.c */
#define DBG_REMOTE_HI	50
#define DBG_REMOTE_MID	5
#define DBG_REMOTE_LO	1

/* debug levels for aliasing code in alias.c */
#define DBG_ALIAS_HI	50
#define DBG_ALIAS_MID	8
#define DBG_ALIAS_LO	1

/* debug levels for hasing code in hash.c */
#define DBG_HASH_VHI   120
#define DBG_HASH_HI	90
#define DBG_HASH_MID	60
#define DBG_HASH_LO	30

/* debug levels for startup file processing code in startup.c */
#define DBG_STARTUP_HI	50
#define DBG_STARTUP_MID	25
#define DBG_STARTUP_LO	10

/* debug levels for startup file generation code in smailconf.c */
#define DBG_CONF_HI	50
#define DBG_CONF_MID	8
#define DBG_CONF_LO	1
