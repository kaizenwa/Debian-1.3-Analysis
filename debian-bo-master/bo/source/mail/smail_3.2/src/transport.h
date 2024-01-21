/*
#ident	"@(#)smail/src:RELEASE-3_2:transport.h,v 1.10 1996/02/28 14:26:43 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

#ifndef TRANSPORT_H
#define TRANSPORT_H

/*
 * transport.h:
 *	types and macros for use by transport drivers.
 */

/* structure of a transport, as read from the configuration file */
struct transport {
    char *name;				/* name of transport */
    char *driver;			/* name of the associated driver */
    struct transport *succ;		/* next transport in the list */
    long flags;				/* boolean flag values */
    int max_addrs;			/* maximum addresses per instance */
    int max_hosts;			/* maximum hosts per instance */
    int max_chars;			/* max chars of address per instance */
    struct list *hdrremove;		/* headers to remove */
    struct list *hdrinsert;		/* headers to insert */
    struct list *hdrappend;		/* headers to append */
    char *retry_dir;			/* name of directory for retry files */
    char *shadow;			/* name of shadow transport */
    char *error_transport;		/* use this transport on errors */
    char *private;			/* private data storage */
};

/* compiled in transport drivers */
struct transport_driver {
    char *name;				/* name of transport driver */
    void (*cache)();			/* function to cache transport info */
    void (*driver)();			/* function to perform delivery */
    void (*finish)();			/* function to free resources */
    char *(*builder)();			/* fun to read from transport file */
    void (*dumper)();			/* fun to dump transport config */
};

/*
 * structure for associating a list of remote addresses with a specific
 * instance of a transport.  Note:  each entry in the addr list has
 * the same transport, so this information is not duplicated in the
 * assign_transport structure itself.
 */
struct assign_transport {
    struct assign_transport *succ;	/* point to next instance */
    struct addr *addr;			/* list of addresses */
};

/* values for transport.flags field */
#define STRICT_TPORT	0x0001		/* try to use strict RFC822 */
#define UUCP_XFORM	0x0002		/* only use uucp !-routes */
#define PUT_RECEIVED	0x0004		/* supply a Received: field */
#define PUT_RETURNPATH	0x0008		/* supply a Return-Path: field */
#define PUT_FROM	0x0010		/* supply a From_ line */
#define LOCAL_TPORT	0x0020		/* this is a local transport */
#define PUT_CRLF	0x0040		/* put \r\n on each output line */
#define BSMTP_TPORT	0x0080		/* write SMTP envelope around msg */
#define PUT_DOTS	0x0100		/* use hidden dot algorithm */
#define HBSMTP_TPORT	0x0200		/* half-baked SMTP envelope */
#define DEBUG_TPORT	0x0400		/* dump debug info, don't dump body */
#define UNIX_FROM_HACK	0x0800		/* put > in front of From's in body */
#define INET_XFORM	0x1000		/* transform using internet rules */
#define RT_NOXFORM	0x2000		/* routers should not transform addrs */
#define LOCAL_XFORM	0x4000		/* local form, but not local delivery */

/* for compatibility: */
#define UUCP_ONLY	UUCP_XFORM

/*
 * note transports that wish to do all next_addr processing themselves
 * should set RT_NOXFORM.  This will cause compute_next_addr() to
 * set the next_addr to the remainder, thus giving the transport
 * complete freedom to combine the remainder, route, and next_host
 * into a suitable recipient address.
 */

#endif /* TRANSPORT_H */
