/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:smarthost.c,v 1.9 1996/02/28 06:48:00 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smarthost.c:
 *	route to a smarter host.  This is useful as the last router
 *	to send otherwise undeliverable addresses on to another host.
 *
 * Specifications for the smarthost routing driver:
 *
 *	associated transports:
 *	    No specific transport is set.
 *
 *	private attribute data:
 *	    host (string):  specifies the next_host value for
 *		the addr structure;  i.e., this is the next host to
 *		which the message should be sent.
 *
 *	    route (string):  specifies the route value for
 *		the addr structure;  i.e., this is the route past the
 *		next_host to get to the smart host.
 *
 *	    path (string):  and if host and route are not specified,
 *		then the path is parsed by parse_address, with the
 *		host attribute being set to the returned target, and
 *		the route attribute being set to the returned remainder.
 *
 *	private attribute flags:
 *	    No private flag attributes.
 *
 * NOTE:  If no path or host attribute is specified, the smart_path
 *	  config file attribute will be used.  If neither of these exist,
 *	  then the smarthost driver does not match anything.  This makes
 *	  it possible to have a compiled-in configuration that does not
 *	  match anything, while making it possible to enable the smartuser
 *	  driver using only a config file.  To help in this, as well,
 *	  a smart_transport config file attribute will be used to
 *	  define the transport as well.
 *
 * NOTE:  at the present time, a transport or method must be specified
 *	  in the generic attributes.  In the future, the routing
 *	  software is expected to be able to allow for secondary passes
 *	  through the routers to resolve the transport to the smarthost.
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../route.h"
#include "../transport.h"
#include "../dys.h"
#include "smarthost.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* functions local to this file */
static struct error *no_smarthost_info();
static struct error *smarthost_parse_error();

static struct attr_table smarthost_attributes[] = {
    { "path", t_string, NULL, NULL, OFFSET(smarthost_private, path) },
    { "host", t_string, NULL, NULL, OFFSET(smarthost_private, host) },
    { "route", t_string, NULL, NULL, OFFSET(smarthost_private, route) },
};
static struct attr_table *end_smarthost_attributes =
    ENDTABLE(smarthost_attributes);


/*
 * smarthost - route to smart host.
 */
/*ARGSUSED*/
void
rtd_smarthost(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    register struct addr *cur;		/* current addr being processed */
    struct addr *next;			/* next addr to process */
    struct smarthost_private *priv = (struct smarthost_private *)rp->private;
    static struct transport *smart_tp;	/* entry for smart_transport */

    DEBUG(DBG_DRIVER_HI, "rtd_smarthost called\n");
    if (priv->path == NULL && priv->host == NULL) {
	if (smart_path == NULL) {
	    /* there is no smarthost information, don't match anything */
	    *out = in;
	    return;
	}
	if (smart_transport && smart_tp == NULL) {
	    smart_tp = find_transport(smart_transport);
	}
    }

    for (cur = in; cur; cur = next) {
	next = cur->succ;
	if (! (cur->flags & ADDR_FINISHED)) {
	    /*
	     * don't match using a smarthost router more than once
	     * for a single addr.
	     */
	    if (cur->flags & ADDR_SMARTHOST) {
		DEBUG2(DBG_DRIVER_MID, "router %s:  did not match %s\n",
		       rp->name, cur->target);
	    } else if (! match_auth_domains(cur->target)) {
		if (priv->host == NULL) {
		    char *p;

		    if (priv->path == NULL && smart_path == NULL) {
			cur->error = no_smarthost_info(rp);
			cur->succ = *defer;
			*defer = cur;
			continue;
		    }

		    /* need a modifiable copy of the path */
		    p = COPY_STRING(priv->path? priv->path: smart_path);
		    if (parse_address(p, &priv->host, &priv->route, (int *)0)
			== FAIL) {
			/* error stored in priv->route */
			cur->error =
			    smarthost_parse_error(rp, priv->path, priv->route);

			cur->succ = *defer;
			*defer = cur;
			continue;
		    }

		    /*
		     * if the smart path parsed to local form, it really
		     * represents a single host with a null route.
		     */
		    if (priv->host == NULL) {
			priv->host = priv->route;
			priv->route = NULL;
		    }
		}

		route_driver_finish(rp, cur, 0, priv->host, priv->route,
				    smart_tp, (struct transport_hints *) 0);
		cur->flags |= ADDR_SMARTHOST;
	    }
	}

	/* link into the non-error output */
	cur->succ = *out;
	*out = cur;
    }
}

/*
 * rtv_smarthost - match anything, unless ADDR_SMARTHOST is set
 *
 * Also, don't match anything in the auth_domains list.
 */
/*ARGSUSED*/
void
rtv_smarthost(rp, in, retry, okay, defer, fail)
    struct router *rp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* current addr being processed */
    struct addr *next;			/* next addr to process */
    struct smarthost_private *priv = (struct smarthost_private *)rp->private;

    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if ((cur->flags & ADDR_SMARTHOST) ||
	    (priv->path == NULL && priv->host == NULL &&
	     smart_path == NULL) ||
	    match_auth_domains(cur->target)) {
	    /* no match if we are disabling smarthost routing here */
	    cur->succ = *retry;
	    *retry = cur;
	} else {
	    /* otherwise we match */
	    cur->succ = *okay;
	    *okay = cur;
	}
    }
}

/*
 * rtb_smarthost - read the configuration file attributes
 */
char *
rtb_smarthost(rp, attrs)
    struct router *rp;			/* router entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct smarthost_private smarthost_template = {
	NULL,				/* host */
	NULL,				/* route */
    };
    struct smarthost_private *priv;	/* new smarthost_private structure */

    /* copy the template private data */
    priv = (struct smarthost_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&smarthost_template, sizeof(*priv));

    rp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    smarthost_attributes,
			    end_smarthost_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * rtp_smarthost - dump the configuration attributes
 */
void
rtp_smarthost(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				smarthost_attributes,
				end_smarthost_attributes);
}


int
match_auth_domains(target)
    char *target;
{
    char *dom;

    if (auth_domains == NULL)
	return 0;
    for (dom = strcolon(auth_domains); dom; dom = strcolon((char *)NULL)) {
	if (is_suffix(dom, target, FALSE))
	    return 1;
    }
    return 0;
}


static struct error *
no_smarthost_info(rp)
    struct router *rp;
{
    char *error_text;

    /*
     * ERR_125 - no smarthost information
     *
     * DESCRIPTION
     *      Either a smart_path or smart_host attribute
     *      is required when using the smarthost driver.
     *      Neither was specified.
     *
     * ACTIONS
     *      Defer the message with a configuration error.
     *
     * RESOLUTION
     *      The smarthost router should be corrected to
     *      define either a smart_host/smart_route pair
     *      or a smart_path attribute.
     */
    error_text = xprintf("router %s: smart_host or smart_path required",
			 rp->name);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_125, error_text);
}

static struct error *
smarthost_parse_error(rp, path, parse_error)
    struct router *rp;
    char *path;
    char *parse_error;
{
    char *error_text;

    /*
     * ERR_126 - smart_path parse error
     *
     * DESCRIPTION
     *      parse_address() encountered an error while
     *      parsing the smart_path attribute, which
     *      should be a UUCP-style !-path.  An error is
     *	stored in priv->route.
     *
     * ACTIONS
     *      Defer the message with a configuration error.
     *
     * RESOLUTION
     *      The smart_path attribute in the router file
     *      should be corrected to be a valid !-path.
     */
    error_text = xprintf("router %s: error parsing %s: %s",
			 rp->name, path, parse_error);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_CONFERR|ERR_126, error_text);
}
