/*
#ident	"@(#)smail/src:RELEASE-3_2:route.c,v 1.23 1996/02/26 18:16:21 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * route.c:
 *	Compute route to target hosts and fill in the transport,
 *	next host and next address for a recipient address.
 *
 *	external functions: route_remote_addrs, verify_remote_addrs,
 *			    cache_routers, finish_routers,
 *			    match_end_domain, route_driver_finish,
 *			    find_router, find_route_driver,
 *			    read_router_file, read_method_file
 *			    dump_router_config
 */

#define NEED_SOCKETS
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "route.h"
#ifdef HAVE_BSD_NETWORKING
#  define USE_MX_HINTS
#  include "bindsmtpth.h"
#endif /* HAVE_BSD_NETWORKING */
#include "transport.h"
#include "addr.h"
#include "dys.h"
#include "log.h"
#include "exitcodes.h"
#include "parse.h"
#include "smailconf.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif

/* variables exported from this file */
int cached_routers = FALSE;		/* TRUE if cache_routers() called */

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static void premunge_remote_addrs();
static void compute_transport();
static void compute_next_addr();
static void next_addr_basic();
static void next_addr_uucp();
static char *route_to_route_addr();
static char *rebuild_work_addr();
static char *router_read_method();
static char *router_driv_function();

/* Config table */
static struct attr_table router_generic[] = {
    { "driver", t_string, NULL, NULL, OFFSET(router, driver) },
    { "transport", t_string, NULL, NULL,
	  OFFSET(router, default_transport) },
    { "method", t_proc, NULL, (tup *)router_read_method, 0 },
    { "always", t_boolean, NULL, NULL, USE_ALWAYS },
};


/*
 * route_remote_addrs - route addrs to transport, next host and next_addr
 */
void
route_remote_addrs(in, out, retry, defer, fail)
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **retry;		/* addr structures to reparse */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct router *rp;			/* temp for stepping thru routers */
    register struct addr *cur;		/* temp for stepping through addrs */
    struct addr *next;			/* next value for cur */
    struct transport_hints * hint;
#ifdef USE_MX_HINTS
    struct mx_transport_hint * mx_hint;
    struct ipaddr_hint * a_hint;
#endif /* USE_MX_HINTS */

    DEBUG(DBG_ROUTE_HI, "route_remote_addrs called\n");

    /* first munge the addr structure as required */
    premunge_remote_addrs(in);

    /* sort addresses to simplify duplicate elimination by rtd_standard() */
    in = addr_sort(in, OFFSET(addr, target));

    /*
     * give the complete input list to each router in turn.
     * router drivers must obey the rules about overriding
     * of routes produced by previous routers.  At the end
     * this routine will cleanup the addr structures.
     */
    for (rp = routers; rp; rp = rp->succ) {
	struct addr *new_in;		/* addrs to put through next router */
	/* look up the router driver by name */
	struct route_driver *driver = find_route_driver(rp->driver);

	if (driver == NULL) {
	    /*
	     * ERR_109 - router driver not found
	     *
	     * DESCRIPTION
	     *      A driver name was not in the table of router drivers.
	     *
	     * ACTIONS
	     *      Defer all input addresses with configuration errors.
	     *      Since it cannot be known if this router would have
	     *      overridden previous routers, all input addresses must be
	     *      deferred.
	     *
	     * RESOLUTION
	     *      The postmaster must check the router configuration
	     *      before deliver can be performed.
	     */
	    insert_addr_list(in,
			     defer,
			     note_error(ERR_CONFERR|ERR_109,
					xprintf(
					      "router %s: driver %s not found",
						rp->name,
						rp->driver)));
	    return;
	}

	/* call the driver */
	new_in = NULL;
	(*driver->driver)(rp, in, &new_in, defer, fail);
	in = new_in;
    }

    /*
     * now that all routers have had their chance we cleanup
     * the input list to form the output list of resolved
     * addresses, and note addresses that no router could
     * route to.
     */
    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if (cur->match_count == -1 ||
	    (cur->flags & (ADDR_PARTLOCAL|ADDR_FULLMATCH)) == ADDR_PARTLOCAL)
	{
	    /* no router could match the target, fail address */
	    exitvalue = EX_NOHOST;	/* set exit status */
	    /*
	     * ERR_101 - unknown host
	     *
	     * DESCRIPTION
	     *      The target in this addr structure was not matched by any
	     *      of the routers.
	     *
	     * ACTIONS
	     *      A message is sent to the owner of the address, or to the
	     *      sender if the address has no owner.
	     *
	     * RESOLUTION
	     *      The address owner or sender should determine the correct
	     *      domain name.  If the owner or sender believes the
	     *      message was in error, he or she should send mail to the
	     *      postmaster.
	     */
	    cur->error = note_error(ERR_NSOWNER|ERR_101, "unknown host");
	    cur->succ = *fail;
	    *fail = cur;
	    continue;
	}
	if (cur->next_host == NULL) {
	    /* routed to local host, reparse */
	    (void) strcpy(cur->work_addr, cur->remainder);
	    cur->flags &= ~(ADDR_FINISHED | ADDR_FULLMATCH);
	    cur->transport = NULL;
	    cur->tphint_list = NULL;
	    if (cur->route) {
		xfree(cur->route);
		cur->route = NULL;
	    }
	    cur->remainder = NULL;
	    cur->succ = *retry;
	    *retry = cur;
	    continue;
	}
	if (cur->transport == NULL) {
	    /* router did not compute a transport, we must */
	    compute_transport(cur);
	    if (cur->error && (cur->error->info & ERR_CONFERR)) {
		/* defer configuration errors */
		cur->succ = *defer;
		*defer = cur;
		continue;
	    }
	}
	if (cur->next_addr == NULL) {
	    compute_next_addr(cur);
	}
	if (cur->error) {
	    /* got an error, fail the address */
	    cur->succ = *fail;
	    *fail = cur;
	} else {
	    /* link into the output queue if things went okay */
	    if (cur->next_host) {
		DEBUG3(DBG_ROUTE_LO, "    routed %s --> %s at %s\n",
		       cur->in_addr, cur->next_addr, cur->next_host);
	    } else {
		DEBUG2(DBG_ROUTE_LO,
		       "    routed %s --> %s on the local host\n",
		       cur->in_addr, cur->next_addr);
	    }
	    for (hint = cur->tphint_list; hint; hint = hint->succ) {
#ifdef USE_MX_HINTS
	      if (EQ("mx",hint->hint_name))
		{
		  mx_hint = (struct mx_transport_hint *) hint->private;
		  DEBUG3(DBG_ROUTE_LO,
			 "      transport hint %s %d %s\n", hint->hint_name,
			 mx_hint->preference,
			 mx_hint->exchanger);
		  for (a_hint = mx_hint->ipaddrs; a_hint; a_hint = a_hint->succ)
		    {
		      DEBUG2(DBG_ROUTE_LO,
			     "        address hint %s %s\n",
			     a_hint->hostname,inet_ntoa(a_hint->addr));
		    }
		}
	      else
#endif /* USE_MX_HINTS */
		DEBUG1(DBG_ROUTE_LO,
		       "      transport hint %s\n", hint->hint_name);
	    }
	    cur->succ = *out;
	    *out = cur;
	}
    }
}


/*
 * verify_remote_addrs - perform quick verify of remote addresses
 *
 * form a list of okay (verified) addrs, plus deferred (not currently
 * determinable) addrs and failed (not deliverable) addrs.
 */
void
verify_remote_addrs(in, okay, defer, fail)
    struct addr *in;			/* input remote addr list */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    struct router *rp;			/* temp for stepping thru routers */
    register struct addr *cur;		/* temp for stepping through addrs */

    DEBUG(DBG_ROUTE_HI, "verify_remote_addrs called\n");

    /* first munge the addr structure as required */
    premunge_remote_addrs(in);

    /*
     * give the complete input list to each router in turn.
     * router drivers must obey the rules about overriding
     * of routes produced by previous routers.  At the end
     * this routine will cleanup the addr structures.
     */
    for (rp = routers; in && rp; rp = rp->succ) {
	struct addr *retry;		/* addrs to put through next router */
	/* look up the router driver by name */
	struct route_driver *driver = find_route_driver(rp->driver);

	if (driver == NULL) {
	    /*
	     * ERR_109 - router driver not found
	     *
	     * DESCRIPTION
	     *      A driver name was not in the table of router drivers.
	     *
	     * ACTIONS
	     *      Defer all input addresses with configuration errors.
	     *      Since it cannot be known if this router would have
	     *      overridden previous routers, all input addresses must be
	     *      deferred.
	     *
	     * RESOLUTION
	     *      The postmaster must check the router configuration
	     *      before delivery can be performed.
	     */
	    insert_addr_list(in,
			     defer,
			     note_error(ERR_CONFERR|ERR_109,
					xprintf(
					      "router %s: driver %s not found",
						rp->name,
						rp->driver)));
	    return;
	}

	/* call the driver */
	retry = NULL;
	(*driver->verify)(rp, in, &retry, okay, defer, fail);
	in = retry;
    }

    /*
     * some cleanup is required on the the addr list
     */
    for (cur = in; cur; cur = cur->succ) {
	if (cur->flags&ADDR_PUTDOT) {
	    /* a dot was removed from the end of the target, put it back */
	    cur->target[strlen(cur->target)] = '.';
	} else if (cur->flags&ADDR_MOVEDOT) {
	    /* move a dot back to the end of the target, from the front */
	    (void) strcpy(cur->target, cur->target + 1);
	    cur->target[strlen(cur->target)] = '.';
	}
    }
}


/*
 * cache_routers - call cache entrypoints for all routers
 *
 * cache information used by router drivers.  This can be called when
 * it is determined that there will be an attempt to deliver more than
 * one mail message, to increase the overall efficiency of the mailer.
 *
 * Daemons can call this periodically to recache stale data.
 */
void
cache_routers()
{
    struct router *rp;			/* temp for stepping thru routers */
    struct route_driver *driver;

    for (rp = routers; rp; rp = rp->succ) {
	driver = find_route_driver(rp->driver);
	if (driver && driver->cache) {
	    (*driver->cache)(rp);
	}
    }
    cached_routers = TRUE;
}

#ifdef notyet
/*
 * finish_routers - free resources used by all routers
 *
 * free information that was cached by routers or used by routers in
 * the process of routing.  Routers can cache data for efficiency, or
 * can maintain state between invocations.  This function is called
 * when routers will no longer be needed, allowing routers to free any
 * resources that they were using that will no longer be needed.  For
 * example, it is a good idea for routers to close any files that they
 * opened, as file descriptors are a precious resource in some
 * machines.
 */
void
finish_routers()
{
    struct router *rp;			/* temp for stepping thru routers */
    struct route_driver *driver;

    for (rp = routers; rp; rp = rp->succ) {
	driver = find_route_driver(rp->driver);
	if (driver && driver->finish) {
	    (*driver->finish)(rp);
	}
    }

    cached_routers = FALSE;
}
#endif


/*
 * premunge_remote_addrs - pre-routing munging on remote addr structures
 *
 * do a premunge on the target, by taking a dot at the end and
 * putting it at the front (if one is not already at the front).
 * Also, initialize the flags field, to remove any extraneously
 * set flags.
 */
static void
premunge_remote_addrs(list)
    struct addr *list;			/* list of remote addrs to premunge */
{
    register struct addr *cur;		/* current address being processed */

    for (cur = list; cur; cur = cur->succ) {
	register int len = strlen(cur->target);

	cur->flags &= ~(ADDR_PUTDOT |
			ADDR_MOVEDOT |
			ADDR_ERROR |
			ADDR_FINISHED |
			ADDR_FULLMATCH |
			ADDR_NOTUSER |
			ADDR_ISUSER);
	cur->match_count = -1;
	if (cur->target[len-1] == '.') {
	    if (cur->target[0] == '.') {
		cur->target[len-1] = '\0';
		cur->flags |= ADDR_PUTDOT;
	    } else {
		/* have to move the target so that a dot can be inserted */
		register char *p = cur->target;

		while (--len) {
		    p[len] = p[len - 1];
		}
		p[0] = '.';
		cur->flags |= ADDR_MOVEDOT;
	    }
	}
    }
}


/*
 * match_end_domain - try to match one of a list of domains against a target
 *
 * given a list of domains separated by colons, determine if the given
 * target ends in one of those domains.  If so, return a pointer to the
 * `.' that precedes the domain that matched, else return NULL.  The
 * list is scanned from left to right, with the first match returned.
 */
char *
match_end_domain(domains, target)
    char *domains;			/* colon separated list of domains */
    char *target;			/* target to test against */
{
    register char *cur;			/* current domain being checked */

    if (!domains)
	return NULL;

    for (cur = strcolon(domains); cur; cur = strcolon((char *)NULL)) {
	char *d = is_suffix(cur, target, TRUE);
	if (d && d != target) {
	    return d - 1;
	}
    }

    /* did not end in one of the domains */
    return NULL;
}

/*
 * is_suffix - try to match a domain against a target
 *
 * given a domain, determine if the given target ends in that domain.
 * If so, return a pointer to the domain in the target, else NULL.
 */

char *
is_suffix(domain, target, proper_suffix)
    char *domain;			/* domain name */
    char *target;			/* target to test against */
    int proper_suffix;			/* fail if target equals domain */
{
    int skip;

    if (*domain == '.')
	domain++;
    if (*domain == '\0')
	return NULL;
    skip = strlen(target) - strlen(domain);

    if (((skip == 0 && !proper_suffix) ||
	 (skip > 0 && target[skip - 1] == '.')) &&
	EQIC(target + skip, domain))
    {
	return target + skip;
    }

    return NULL;
}

/*
 * route_driver_finish - fill in addr structures for a route driver
 *
 * route drivers may call this routine for each addr structure they
 * have computed a route to.  This routine completes the addr structure
 * if it is determined that the computed route has precedence over any
 * previously computed route.  In the case that the computed route has
 * precedence over future routers, the ADDR_FINISHED flag is set in
 * the addr.flags struct element.
 */
void
route_driver_finish(rp, addr, match_count, next_host, route, transport, hints)
    struct router *rp;			/* the calling router */
    struct addr *addr;			/* addr structure in question */
    int match_count;			/* target match count */
    char *next_host;			/* computed next_host value */
    char *route;			/* computed route value */
    struct transport *transport;	/* optional transport from driver */
    struct transport_hints *hints;	/* transport hints from driver */
{
    if (addr->flags&ADDR_FINISHED) {
	/*
	 * a router erroneously found a new route for a finished addr,
	 * ignore it
	 */
	return;
    }

    /*
     * consider this a match, if it matched more characters than
     * any earlier router
     */

    if (match_count > addr->match_count) {

	/*
	 * if the router matched the address completely, then
	 * consider the address to have been matched completely
	 */

	if (strlen(addr->target) == match_count ||
	    (addr->target[0] == '.' &&
	     strlen(addr->target) == match_count - 1))
	{
	    addr->flags |= ADDR_FINISHED|ADDR_FULLMATCH;
	} else {
	    if (next_host == NULL) {

		/*
		 * the local host cannot be matched partially, so
		 * ignore any attempts to do so
		 */

		return;
	    }
	    if (rp->flags&USE_ALWAYS) {
		addr->flags |= ADDR_FINISHED;
	    }
	}

	/*
	 * this route has precedence over previous routes
	 */

	addr->router = rp;
	addr->match_count = match_count;
	if (addr->next_host) {
	    xfree(addr->next_host);
	}
	if (next_host) {
	    addr->next_host = COPY_STRING(next_host);
	} else {
	    addr->next_host = NULL;
	}
	if (addr->route) {
	    xfree(addr->route);
	}
	if (route) {
	    addr->route = COPY_STRING(route);
	} else {
	    addr->route = NULL;
	}
	addr->transport = transport;
	addr->tphint_list = hints;
	if (addr->next_addr) {
	    /*
	     * cleanup after a previous router that computed the
	     * next_addr itself
	     */
	    xfree(addr->next_addr);
	    addr->next_addr = NULL;
	}
	if (next_host) {
	    DEBUG3(DBG_ROUTE_LO, "%s: %s matched by %s:\n",
		   addr->in_addr, addr->target, addr->router->name);
	} else {
	    DEBUG3(DBG_ROUTE_LO, "%s: reparse address %s generated by %s:\n",
		   addr->in_addr, addr->remainder, addr->router->name);
	}
    }
}

/*
 * compute_next_addr - compute the next_addr for a routed recipient address
 */
static void
compute_next_addr(addr)
    struct addr *addr;			/* address to be completed */
{
    char *r_route;

    if (addr->error) {
	/* ignore the addr if it already has a pending error condition */
	return;
    }

    if (addr->flags&ADDR_PUTDOT) {
	/* a dot was removed from the end of the target, put it back */
	addr->target[strlen(addr->target)] = '.';
    } else if (addr->flags&ADDR_MOVEDOT) {
	/* move a dot back to the end of the target, from the front */
	(void) strcpy(addr->target, addr->target+1);
	addr->target[strlen(addr->target)] = '.';
    }

    if (! (addr->flags & ADDR_FULLMATCH)) {

	/*
	 * if the target was not matched completely, append the
	 * target to the route.
	 */

	if (addr->route) {
	    r_route = xprintf("%s!%s", addr->route, addr->target);
	    xfree(addr->route);
	} else {
	    r_route = COPY_STRING(addr->target);
	}
	addr->route = r_route;
    }

    /*
     * select a method for transforming the remainder/route/next_host
     * triplet into a next_addr.  Default is method suitable for the
     * inter-smail communication, but that's about it.
     */

    if (addr->transport->flags & RT_NOXFORM) {
	if (addr->route) {
	    r_route = route_to_route_addr(addr->route);
	    addr->next_addr = xprintf("%s:%s", r_route, addr->remainder);
	} else {
	    addr->next_addr = COPY_STRING(addr->remainder);
	}
    }
    if (addr->transport->flags & UUCP_XFORM) {
	next_addr_uucp(addr);
    } else if (addr->transport->flags & INET_XFORM) {
	next_addr_basic(addr, TRUE);
    } else {
	next_addr_basic(addr, FALSE);
    }
    return;
}

/*
 * next_addr_basic - compute a next_addr for a basic transport
 *
 * This transformation leaves the address alone to the greatest extent
 * possible, and makes an effort to do transformations that work well
 * in environments with mixed-zone (sendmail and smail3) mailers.
 *
 * If the inet flag is TRUE, then the address will be left conformant
 * to RFC822 and RFC1123.  However, routes will be included in
 * route-addr notation.
 */
static void
next_addr_basic(addr, inet)
    struct addr *addr;
    int inet;
{
    char *route = addr->route;
    char *remainder = addr->remainder;
    char *target = addr->target;
    char *next_host = addr->next_host;
    char *error;
    char * new_remainder = NULL; /* This should not be used! */
    int pflags;
    char *r_route, *r_last;
    char *p;

    /*
     * try to rebuild the original address, or at least keep the
     * spirit of the original address form, if a route is needed.
     */

    switch (addr->flags & ADDR_FORM_MASK) {
    case RFC_ROUTE:
    case RFC_ENDROUTE:

	/*
	 * For route-addrs, if no additional route is needed for
	 * delivery to the final destination, then just use the
	 * remainder of the route; otherwise, build an RFC822-style
	 * route out of the needed additional route.
	 */

	if (route == NULL)
	    addr->next_addr = COPY_STRING(remainder);
	else {
	    r_route = route_to_route_addr(route);
	    if ((addr->flags & ADDR_FORM_MASK) == RFC_ENDROUTE)
		addr->next_addr = xprintf("%s:%s", r_route, remainder);
	    else
		addr->next_addr = xprintf("%s,%s", r_route, remainder);
	    xfree(r_route);
	}
	return;

    case MAILBOX:

	/*
	 * for mailbox (user@host) addresses, get back the original
	 * address if there is no further routing; otherwise generate
	 * a mailbox address for a one-hop route; otherwise use RFC822
	 * route-addrs for routing (inet) or !-style routes (non-inet)
	 * if possible.
	 */

	pflags = addr->parseflags;
	switch (parse_address(remainder, (char **) NULL, &new_remainder, &pflags)) {
	case MAILBOX:

	    /*
	     * if the remainder is also a mailbox (which is actually
	     * an illegal form within a mailbox) then just send it if
	     * no further routing is needed; otherwise, make a
	     * route-addr or !-route out of it
	     */

	    if (route == NULL)
		addr->next_addr = COPY_STRING(remainder);
	    else {
		if (inet) {
		    r_route = route_to_route_addr(route);
		    addr->next_addr = xprintf("%s:%s", r_route, remainder);
		    xfree(r_route);
		} else {
		    p = build_partial_uucp_route(remainder,
				&error, addr->parseflags);
		    addr->next_addr = xprintf("%s!%s", route, p);
		}
	    }
	    return;

	case PCT_MAILBOX:
	    if (route && mixed_address(remainder)) {
		inet = TRUE;
		break;
	    }
	    break;

	case FAIL:
	    /*
	     * ERR_179 - malformed user part in remote address
	     *
	     * DESCRIPTION
	     *      The user part of the address choked parse_address()
	     *
	     * ACTIONS
	     *      A message is sent to the owner of the address, or to the
	     *      sender if the address has no owner.
	     *
	     * RESOLUTION
	     *      The address owner or sender should fix the address
	     */
	    addr->error = note_error(ERR_NSOWNER|ERR_179, 
				    xprintf("malformed user part in remote address - %s",
					    new_remainder));
	    return;
	}
	if (route && strchr(route, '!') == NULL) {
	    addr->next_addr = xprintf("%s@%s", remainder, route);
	} else if (route && inet) {
	    r_route = route_to_route_addr(route);
	    r_last = strrchr(r_route, ',');
	    if (r_last) {
		*r_last++ = '\0';
		r_last++;
		addr->next_addr =
		    xprintf("%s:%s@%s", r_route, remainder, r_last);
	    } else {
		addr->next_addr = xprintf("%s@%s", remainder, r_route + 1);
	    }
	    xfree(r_route);
	} else if (route) {
	    addr->next_addr = xprintf("%s!%s", route, remainder);
	} else {
	    addr->next_addr = xprintf("%s@%s", remainder, next_host);
	}
	return;

    case UUCP_ROUTE:
    case BERKENET:
    case DECNET:
	if (inet) {
	    if (route && strchr(route, '!') == NULL) {
		addr->next_addr = xprintf("%s@%s", remainder, route);
	    } else if (route) {
		r_route = route_to_route_addr(route);
		r_last = strrchr(r_route, ',');
		if (r_last) {
		    *r_last++ = '\0';
		    r_last++;
		    addr->next_addr =
			xprintf("%s:%s@%s", r_route, remainder, r_last);
		} else {
		    addr->next_addr = xprintf("%s@%s", remainder, r_route + 1);
		}
		xfree(r_route);
	    } else {
		addr->next_addr = xprintf("%s@%s", remainder, next_host);
	    }
	} else {
	    if (route)
		addr->next_addr = xprintf("%s!%s", route, remainder);
	    else
		addr->next_addr = COPY_STRING(remainder);
	}
	return;

    case PCT_MAILBOX:

	/*
	 * for user%foo addresses, keep in mind that RFC1123 may prove
	 * a problem if we need to add a route.  To manage this we do
	 * one of two things: keep the user%foo form or change it to
	 * a mailbox address of some kind.  What we do depends on the
	 * mixing of % and ! operators.  Mixed addresses and Internet
	 * conformant addresses are sent out as @route:user@foo, where
	 * the route is formed using RFC822 route syntax; otherwise,
	 * we use route!user%foo.
	 */

	if (route == NULL)
	    addr->next_addr = xprintf("%s@%s", remainder, next_host);
	else if (strchr(route, '!') == NULL) {
	    addr->next_addr = xprintf("%s@%s", remainder, route);
	} else {
	    if (mixed_address(remainder) || inet) {
		r_route = route_to_route_addr(route);
		r_last = strrchr(r_route, ',');
		if (r_last) {
		    *r_last++ = '\0';
		    r_last++;
		    addr->next_addr =
			xprintf("%s:%s@%s", r_route, remainder, r_last);
		} else {
		    addr->next_addr = xprintf("%s@%s", remainder, r_route + 1);
		}
		xfree(r_route);
	    } else {
		addr->next_addr = xprintf("%s!%s", route, remainder);
	    }
	}
	return;

    default:

	/*
	 * we should never reach here, since that is the end of the
	 * addressing forms that can be encountered here.  Log it
	 * and send as UUCP-style address.
	 */

	write_log(LOG_PANIC, "internal error: next_addr_local: unknown form: in_addr:%s target:%s remainder:%s",
		  addr->in_addr, target, remainder);

	if (route)
	    addr->next_addr = xprintf("%s!%s", route, remainder);
	else
	    addr->next_addr = COPY_STRING(remainder);
	return;
    }
}

/*
 * next_addr_uucp - compute a next_addr appropriate for uucp
 *
 * Compute the next_addr by concatenating the route and remainder
 * into a !-style address.  Any % operators in the remainder will
 * be left alone.  Other operators will be left alone.  However,
 * if the input address contained mixed-operators, including all
 * three of !-%-@, and we have to prepend a route, then remove all %
 * operators.  This last point is necessary to avoid ambiguities
 * between RFC1123 and RFC976 sites.
 */
static void
next_addr_uucp(addr)
    struct addr *addr;
{
    char *route = addr->route;
    char *remainder = addr->remainder;
    char *error;
    char *r_route;

    if (route && switch_percent_and_bang &&
	addr->parseflags & FOUND_MAILBOX &&
	mixed_address(remainder))
    {
	r_route = build_uucp_route(remainder, &error, addr->parseflags);
    } else {
	r_route = build_partial_uucp_route(remainder, &error, addr->parseflags);
    }
    if (r_route == NULL) {
	/* build_partial_uucp_route failed, fail the address */
	/*
	 * ERR_108 - error building path
	 *
	 * DESCRIPTION
	 *      build_partial_uucp_route() returned an error while
	 *      attempting to build a UUCP-path from the remainder in
	 *      the address.  The specific error was returned in
	 *      `error'.
	 *
	 * ACTIONS
	 *      Notify the sender or the owner of the address of the
	 *      problem.
	 *
	 * RESOLUTION
	 *      The sender or owner should correct the supplied address
	 *      to use acceptible addressing forms.
	 */
	addr->error =
	    note_error(ERR_NSOWNER|ERR_108,
		       xprintf("error building path: %s", error));
	exitvalue = EX_DATAERR;	/* set the exit status */
	return;
    }

    /*
     * build the next_addr by appending the remainder to the route
     */

    if (route) {
	addr->next_addr = xprintf("%s!%s", route, r_route);
	xfree(r_route);
    } else {
	addr->next_addr = r_route;
    }

    return;
}

static char *
route_to_route_addr(p)
    char *p;
{
    struct str str;
    int c;

    STR_INIT(&str);
    STR_NEXT(&str, '@');
    while (c = *p++) {
	if (c == '!') {
	    STR_NEXT(&str, ',');
	    STR_NEXT(&str, '@');
	} else {
	    STR_NEXT(&str, c);
	}
    }
    STR_NEXT(&str, '\0');
    STR_DONE(&str);

    return str.p;
}

/*
 * compute_transport - compute the transport from any available defaults
 *
 * some router drivers do not assign transports themselves but rely
 * on defaults in the router structure.  These defaults can either be
 * a single default transport or a table of transports associated with
 * host names, where the special name `*' is a catchall.
 */
static void
compute_transport(addr)
    struct addr *addr;
{
    struct router *rp = addr->router;

    DEBUG2(DBG_ROUTE_HI, "compute_transport called: host=%s, router=%s\n",
	   addr->next_host, rp->name);
    /* no transport yet assigned, look around for defaults */
    if (rp->method) {
	/* there is a "method", table of possible transports */
	struct method *mp;

	mp = rp->method;
	while (mp->host) {
	    if ((EQIC(addr->next_host, mp->host) || EQ(mp->host, "*")) &&
		  (mp->mingrade < 0 ||
		   (msg_grade >= mp->mingrade && msg_grade <= mp->maxgrade)))
	    {
		/* found the right transport, use it */
		addr->transport = find_transport(mp->transport);
		if (addr->transport == NULL) {
		    /* configuration error, the transport does not exist */
		    /*
		     * ERR_102 - method transport not found
		     *
		     * DESCRIPTION
		     *      The transport to be used for the method was not
		     *      found in the table of transports.
		     *
		     * ACTIONS
		     *      Defer the message as a configuration error.
		     *
		     * RESOLUTION
		     *      The postmaster needs to correct the eror in the
		     *      method file.
		     */
		    addr->error = note_error(ERR_CONFERR|ERR_102,
					     xprintf(
			"router %s: method transport %s not found for host %s",
						     rp->name,
						     mp->transport,
						     mp->host));
		    return;
		}

		DEBUG2(DBG_ROUTE_MID, "use transport %s for %s\n",
		       addr->transport->name, addr->in_addr);
		return;
	    }
	    mp++;
	}
    }

    if (rp->default_transport) {
	/* there is a default, use it */
	addr->transport = find_transport(rp->default_transport);
	if (addr->transport == NULL) {
	    /*
	     * ERR_103 - default transport not found
	     *
	     * DESCRIPTION
	     *      The default transport for the matching router is not in the
	     *      list of transports.
	     *
	     * ACTIONS
	     *      Defer the message as a configuration error.
	     *
	     * RESOLUTION
	     *      The postmaster should correct the router file.
	     */
	    addr->error = note_error(ERR_CONFERR|ERR_103,
				     xprintf(
				   "router %s: default transport %s not found",
					     rp->name,
					     rp->default_transport));
	    return;
	}

	DEBUG2(DBG_ROUTE_MID, "use default transport %s for %s\n",
	       addr->transport->name, addr->in_addr);
	return;
    } else {
	/*
	 * ERR_110 - no transport for router
	 *
	 * DESCRIPTION
	 *      No method or default transport was given for the router that
	 *      produced this address.
	 *
	 * ACTIONS
	 *      Defer the address as a configuration error.
	 *
	 * RESOLUTION
	 *      The postmaster must check the router configuration before
	 *      delivery to the address can be performed.
	 */
	addr->error = note_error(ERR_CONFERR|ERR_110,
				 xprintf("router %s: no transport found",
					 rp->name));
    }
}


/*
 * find_router - given a router's name, return the router structure
 *
 * return NULL if no router of that name exists.
 */
struct router *
find_router(name)
    register char *name;		/* search key */
{
    register struct router *rp;		/* temp for stepping thru routers */

    /* loop through all the routers */
    for (rp = routers; rp; rp = rp->succ) {
	if (EQ(rp->name, name)) {
	    /* found the router in question */
	    return rp;
	}
    }

    return NULL;			/* router not found */
}

/*
 * find_route_driver - given a driver's name, return the driver structure
 *
 * return NULL if driver does not exist.
 */
struct route_driver *
find_route_driver(name)
    register char *name;		/* search key */
{
    register struct route_driver *rdp;	/* pointer to table of drivers */

    for (rdp = route_drivers; rdp->name; rdp++) {
	if (EQ(rdp->name, name)) {
	    return rdp;			/* found the driver */
	}
    }

    return NULL;			/* driver not found */
}


/*
 * read_router_file - read router file
 *
 * read the router file and build a router list describing the
 * entries.  Return an error message or NULL.
 */
char *
read_router_file()
{
    FILE *f;				/* open router file */
    char *error;			/* error from read_standard_file() */
    struct stat statbuf;
    struct attr_table *end_router_generic = ENDTABLE(router_generic);
    static struct router router_template = {
	NULL,				/* name */
	"pathalias",			/* driver, a reasonable default */
	NULL,				/* succ will be assigned */
	0,				/* flags */
	NULL,				/* method */
	NULL,				/* default_transport */
	NULL,				/* private */
    };

    /*
     * try to open router file, note file stat if possible
     */
    if (router_file == NULL || EQ(router_file, "-")) {
	return NULL;
    }
    f = fopen(router_file, "r");
    if (f == NULL) {
	if (require_configs) {
	    return xprintf("%s: %s", router_file, strerror(errno));
	}

	add_config_stat(router_file, (struct stat *)NULL);
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(router_file, &statbuf);

    /* call read_standard_file to do the real work */
    error = read_standard_file(f,
			       (char *)&router_template,
			       sizeof(struct router),
			       OFFSET(router, name),
			       OFFSET(router, flags),
			       OFFSET(router, succ),
			       router_generic,
			       end_router_generic,
			       router_driv_function,
			       (char **)&routers);

    /* finish up */
    (void) fclose(f);

    /* return any error message */
    if (error) {
	return xprintf("%s: %s", router_file, error);
    }
    return NULL;
}


void
dump_router_config(f)
     FILE * f;
{
    struct router *rp;			/* temp for stepping thru routers */
    struct attr_table *end_router_generic = ENDTABLE(router_generic);
    char * name;			/* name of router */
    long flags;				/* flags for router */
    struct route_driver *driver;	/* drivers for router */

    DEBUG(DBG_ROUTE_HI, "dump router configs called\n");
    fputs("#\n# -- routers configuration\n", f);

    /*
     * work through each router in turn.
     */

    for (rp = routers; rp; rp = rp->succ) {
	name = rp->name;
	flags = rp->flags;
	fprintf(f, "#\n%s:\n", name);
	(void) dump_standard_config(f,
				    (char *) rp,
				    name,
				    flags,
				    router_generic,
				    end_router_generic);
	fputs("\t;\n", f);
	driver = find_route_driver(rp->driver);
	if (driver->dumper)
	    (*driver->dumper)(f, rp);
    }
    fputs("#\n# -- end of routers\n", f);
}


/*
 * router_read_method - handle method attribute for routers
 *
 * the method attribute specifies a file which contains a method table
 * associating hosts with transports.  Use of this attribute causes
 * the table to be read and turned into an in-core method table.
 */
static char *
router_read_method(struct_p, attr)
    char *struct_p;			/* passed router structure */
    struct attribute *attr;		/* parsed attribute */
{
    struct router *rp = (struct router *)struct_p;
    char *error;

    if (attr->value == off) {
	rp->method = NULL;
    } else if (attr->value == on) {
	return xprintf("%s: boolean form for non-boolean attribute",
		       attr->name);
    } else {
	int free_method_fn = FALSE;
	char *method_fn = attr->value;

	if (method_fn[0] != '/') {
	    if (method_dir == NULL) {
		return xprintf("%s: %s not absolute and no method_dir",
			       attr->name, method_fn);
	    }
	    method_fn = xmalloc(strlen(method_dir) + strlen(method_fn) +
				sizeof("/"));
	    (void) sprintf(method_fn, "%s/%s", method_dir, attr->value);
	    free_method_fn = TRUE;
	}
	rp->method = read_method_file(method_fn, &error);

	if (rp->method == NULL) {
	    return xprintf("%s %s: %s", attr->name, attr->value, error);
	}

	if (free_method_fn) {
	    xfree(method_fn);
	}
    }

    /* everything went okay */
    return NULL;
}

/*
 * read_method_file - read a method file and return an in-core method table
 *
 * return NULL if the method file could not be opened.
 */
struct method *
read_method_file(fn, error)
    char *fn;				/* name of method file */
    char **error;			/* store error message here */
{
    FILE *f = fopen(fn, "r");
    struct method *methods;		/* method table */
    int ct = 0;				/* count of methods */
    char *entry;			/* text of file entry */
    char *mgrade;			/* restricted grades for this method */
    struct stat statbuf;

    if (f == NULL) {
	*error = xprintf("open failed: %s", strerror(errno));
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(fn, &statbuf);

    /* allocate space for at least the ending record */
    methods = (struct method *)xmalloc(sizeof(*methods));

    /* loop and read all of the table entries in the method file */
    while (entry = read_entry(f)) {
	char *errptr;				/* not really used */
	struct attribute *new;

	new = parse_table(entry, &errptr);
	if (new == NULL) {
	    fclose(f);
	    return NULL;
	}
	methods = (struct method *)
	    xrealloc((char *)methods, (ct + 2)*sizeof(*methods));
	methods[ct].host = new->name;
	methods[ct].mingrade = -1;
	methods[ct].maxgrade = -1;
	methods[ct].transport = new->value;
	mgrade = strchr(new->name, '/');
	if (mgrade != NULL) {
	    *mgrade++ = '\0';
	    if (isalnum(mgrade[0])) {
		methods[ct].mingrade = mgrade[0];
		methods[ct].maxgrade = mgrade[0];
	    }
	    if (mgrade[1] == '-') {
		if (isalnum(mgrade[2])) {
		     methods[ct].maxgrade = mgrade[2];
		} else {
		     methods[ct].maxgrade = 255;
		}
		if (methods[ct].mingrade == -1)
		     methods[ct].mingrade = 0;
	    }
	}
	ct++;
    }

    /* zero out the ending record */
    methods[ct].host = NULL;
    methods[ct].transport = NULL;

    fclose(f);
    return methods;
}

static char *
router_driv_function(struct_p, driver_attrs)
    char *struct_p;			/* passed router structure */
    struct attribute *driver_attrs;	/* driver-specific attributes */
{
    struct router *rp = (struct router *)struct_p;
    struct route_driver *drv;

    if (rp->driver == NULL) {
	return xprintf("router %s: no driver attribute", rp->name);
    }
    drv = find_route_driver(rp->driver);
    if (drv == NULL) {
	return xprintf("router %s: unknown driver: %s",
		       rp->name, rp->driver);
    }
    if (drv->builder) {
	return (*drv->builder)(rp, driver_attrs);
    }

    return NULL;
}
