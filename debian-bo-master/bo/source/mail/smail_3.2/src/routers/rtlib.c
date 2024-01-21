/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:rtlib.c,v 1.7 1996/05/29 18:49:34 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * rtlib.c:
 *	Support routines for smail routing drivers.  Simple routers
 *	can depend on these routines rather than having to duplicate
 *	this intelligence.
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "../smail.h"
#include "../addr.h"
#include "../route.h"
#include "../transport.h"
#include "../lookup.h"
#include "rtlib.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/*
 * rtd_standard - standard function for routing to a target
 * rtv_standard - standard function for verifying a target
 *
 * These routines calls a lookup function to route individual target
 * names.  This lookup routine will be called as follows:
 *
 *	success = (*lookup)(rp, addr, fl, &rt_info, &error);
 *	 int success;			- output: success value
 *	 struct router *rp;		- input: router entry
 *	 struct addr *addr;		- input: addr to route
 *	 int fl;			- input: flags affecting operation
 *	 struct rt_info rt_info;	- output: routing information
 *	 struct error *error;		- output: routing error
 *	 int len;			- output: target match length
 *
 * The lookup routine should route to the target specified in the
 * single stucture passed in 'addr'.  If routing was successful and a
 * match was found, the 'next_host', 'route' and 'len' values should
 * always be set as appropriate.  If routing was not successful due to
 * an error other than the host not being found, error should be set
 * to a structure defining the type of the error.
 *
 * The 'success' value should be a number from lookup.h corresponding
 * to one of:
 *
 *	DB_SUCCEED	The target was matched by the router.  The
 *			addr structure will be processed by the
 *			route_driver_finish() function with the
 *			values from the 'rt_info' structure.
 *
 *	DB_NOMATCH	The target was not matched by the router.  The
 *			addr structure will be linked to the 'out'
 *			or 'retry' list without modification.
 *
 *	DB_FAIL		An unrecoverable error in routing to the
 *			target.  This will cause the addr structure to
 *			be linked into the 'fail' output list, tagged
 *			with the 'error' structure.
 *
 *	DB_AGAIN	A temporary error in routing to the target.
 *			This will cause the addr structure to be
 *			linked into the 'defer' output list, tagged
 *			with the 'error' structure.
 *
 *	FILE_NOMATCH	The router considers itself optional and
 *			disabled.  This and all subsequent unfinished
 *			addr structures will be linked into the 'out'
 *			or 'retry' list unchanged.
 *
 *	FILE_FAIL	There was an unrecoverable error in the
 *			router.  This is treated as a configuration
 *			error, so the ERR_CONFERR flag is set in the
 *			'error' structure.  This and all subsequent
 *			unfinished addrs will be linked into the
 *			'defer' list, tagged with the 'error'
 *			structure.
 *
 *	FILE_AGAIN	There was a temporary error in the router.
 *			This and all subsequent unfinished addrs will
 *			be linked into the 'defer' list, tagged with
 *			the 'error' structure.
 *
 * The 'fl' value is a bit-wise or of the following values from
 * rtlib.h:
 *
 *	RT_VERIFY	Performing verification only.  In this case
 *			we are only interested in the success value,
 *			not in the 'rt_info' structure.  A router can
 *			optimize its operation accordingly.  For
 *			example, a router need only verify that any
 *			match exists, it need not find the best match.
 *
 * The 'rt_info' structure defines the next_host, targe match length
 * and, optionally, the route and transport determined by the router.
 */
void
rtd_standard(rp, in, out, defer, fail, lookup)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
    int (*lookup)();			/* function to actually do routing */
{
    register struct addr *cur;		/* current addr being processed */
    struct addr *next;			/* next addr to process */
    struct error *error = NULL;		/* error from last lookup */
    char *target = NULL;		/* last target that was checked */
    int success;			/* results of last lookup */
    int file_success = 0;		/* non-zero means router error */
    struct rt_info rt_info;		/* routing info from router */

    DEBUG1(DBG_DRIVER_HI, "rtd_%s called\n", rp->driver);
    for (cur = in; cur; cur = next) {

	next = cur->succ;

	if (cur->flags&ADDR_FINISHED) {
	    /* this addr has already been completed, skip it */
	    cur->succ = *out;
	    *out = cur;
	    continue;
	}

	if (file_success) {
	    switch (file_success) {

	    case FILE_NOMATCH:
		cur->succ = *out;
		*out = cur;
		continue;

	    case FILE_FAIL:
	    case FILE_AGAIN:
		cur->error = error;	/* use error from last lookup */
		cur->succ = *defer;
		*defer = cur;
		continue;
	    }
	}

	if ((rp->flags & RT_AFFECTS_USER) || /* This means routing may change on a per user basis */
	    (target == NULL) || 
	    (! EQIC(target, cur->target))) {
	    /*
	     * The current target differs from the previous target, so
	     * look it up.
	     */
	    rt_info.next_host = NULL;
	    rt_info.route = NULL;
	    rt_info.transport = NULL;
	    rt_info.tphint_list = (struct transport_hints *) 0;
	    error = NULL;
	    target = cur->target;
	    /* no flags to pass */
	    success = (*lookup)(rp, cur, 0, &rt_info, &error);
	}

	/* Finish up routing, based on lookup response code */
	switch (success) {
	case DB_SUCCEED:
	    /* found a successful match */
	    route_driver_finish(rp, cur, rt_info.matchlen, rt_info.next_host,
				rt_info.route, rt_info.transport,
				rt_info.tphint_list);
	    cur->succ = *out;
	    *out = cur;
	    break;

	case DB_NOMATCH:
	    /* No match was found.  Pass the address onto the next router. */
	    cur->succ = *out;
	    *out = cur;
	    break;

	case DB_FAIL:
	    /* The address should be failed, with an error of some kind. */
	    cur->error = error;
	    cur->succ = *fail;
	    *fail = cur;
	    break;

	case DB_AGAIN:
	    /* Routing for this address should be reattempted later. */
	    cur->error = error;
	    cur->succ = *defer;
	    *defer = cur;
	    break;

	case FILE_NOMATCH:
	    /* The file was not found, don't match any addresses. */
	    cur->succ = *out;
	    *out = cur;
	    file_success = FILE_NOMATCH;
	    break;

	case FILE_FAIL:
	    /* Permanent router error, this is a configuration error. */
	    error->info |= ERR_CONFERR;
	    cur->error = error;
	    cur->succ = *defer;
	    *defer = cur;
	    file_success = FILE_FAIL;
	    break;

	case FILE_AGAIN:
	    /* Temporary router database error, retry all addresses. */
	    cur->error = error;
	    cur->succ = *defer;
	    *defer = cur;
	    file_success = FILE_AGAIN;
	    break;
	}
    }
}

void
rtv_standard(rp, in, retry, okay, defer, fail, lookup)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
    int (*lookup)();			/* function to actually do routing */
{
    register struct addr *cur;		/* current addr being processed */
    struct addr *next;			/* next addr to process */
    struct error *error = NULL;		/* error from last lookup */
    char *target = NULL;		/* last target that was checked */
    int success;			/* results of last lookup */
    int file_success = 0;		/* non-zero means router error */
    struct rt_info rt_info;		/* routing info from router */

    DEBUG1(DBG_DRIVER_HI, "rtv_%s called\n", rp->driver);
    for (cur = in; cur; cur = next) {

	next = cur->succ;

	if (file_success) {
	    switch (file_success) {

	    case FILE_NOMATCH:
		cur->succ = *retry;
		*retry = cur;
		continue;

	    case FILE_FAIL:
	    case FILE_AGAIN:
		cur->error = error;	/* use error from last lookup */
		cur->succ = *defer;
		*defer = cur;
		continue;
	    }
	}

	if ((rp->flags & RT_AFFECTS_USER) || /* This means routing may change on a per user basis */
	    (target == NULL) || 
	    (! EQIC(target, cur->target))) {
	    /*
	     * The current target differs from the previous target, so
	     * look it up.
	     */
	    error = NULL;
	    target = cur->target;
	    success = (*lookup)(rp, cur, RT_VERIFY, &rt_info, &error);
	}

	/* Finish up routing, based on lookup response code */
	switch (success) {
	case DB_SUCCEED:
	    /* Found a successful match. */
	    cur->succ = *okay;
	    *okay = cur;
	    break;

	case DB_NOMATCH:
	    /* No match was found.  Pass the address onto the next router. */
	    cur->succ = *retry;
	    *retry = cur;
	    break;

	case DB_FAIL:
	    /* The address should be failed, with an error of some kind. */
	    cur->error = error;
	    cur->succ = *fail;
	    *fail = cur;
	    break;

	case DB_AGAIN:
	    /* Routing for this address should be reattempted later. */
	    cur->error = error;
	    cur->succ = *defer;
	    *defer = cur;
	    break;

	case FILE_NOMATCH:
	    /* The file was not found, don't match any addresses. */
	    cur->succ = *retry;
	    *retry = cur;
	    break;

	case FILE_FAIL:
	    /* Permanent router error, this is a configuration error. */
	    error->info |= ERR_CONFERR;
	    cur->error = error;
	    cur->succ = *defer;
	    *defer = cur;
	    file_success = FILE_FAIL;
	    break;

	case FILE_AGAIN:
	    /* Temporary router database error, retry all addresses. */
	    cur->error = error;
	    cur->succ = *defer;
	    *defer = cur;
	    file_success = FILE_AGAIN;
	    break;
	}
    }
}
