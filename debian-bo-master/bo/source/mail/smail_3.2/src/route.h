/* @(#) route.h,v 1.7 1996/05/29 18:48:25 woods Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

#ifndef	ROUTE_H
#define ROUTE_H

/*
 * route.h:
 *	interface file for route.c.  Also, types and macros for
 *	use by router drivers.
 */

/* structure of a router, as read from the configuration file */
struct router {
    char *name;				/* name of router */
    char *driver;			/* name of driver */
    struct router *succ;		/* next router in the list */
    long flags;				/* boolean flag values */
    struct method *method;		/* table of host/tport associations */
    char *default_transport;		/* name of default transport */
    char *private;			/* private data storage */
};

/* method - table associating hosts and transports */
struct method {
    char *host;				/* host name */
    int mingrade;			/* min grading for this entry */
    int maxgrade;			/* max grading for this entry */
    char *transport;			/* transport name */
};

/* compiled in route drivers */
struct route_driver {
    char *name;				/* name of route driver */
    void (*cache)();			/* function to cache routing info */
    void (*driver)();			/* function to perform routing */
    void (*verify)();			/* function to perform verification */
    void (*finish)();			/* function to free resources */
    char *(*builder)();			/* fun to read from router file */
    void (*dumper)();			/* function to dump config */
};

/*
 * structure for route information passed between the rt[dv]_standard
 * routines and the various router drivers that use them; also used by
 * bindlib, which is why it's here instead of routers/rtlib.h.
 */
struct rt_info {
    char *next_host;			/* next-hop host string */
    char *route;			/* route from next_host to target */
    int matchlen;			/* length of match on target */
    struct transport *transport;	/* optional transport */
    struct transport_hints *tphint_list; /* options transport hints */
};

/* values for router.flags field */
#define USE_ALWAYS	0x0001		/* if match, don't use next router */
#define RT_AFFECTS_USER 0x0002		/* This router can affect the user part */

#endif	/* ROUTE_H */
