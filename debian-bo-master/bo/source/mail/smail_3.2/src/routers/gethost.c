/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:gethost.c,v 1.11 1996/02/28 14:27:32 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * gethost.c:
 *	The "gethostbyname" and "gethostbyaddr" routing drivers.  These
 *	drivers call on the networking library functions of the same name.
 *	To match hostnames.
 *
 * Specifications of the gethostbyname routing driver:
 *
 *	associated transports:
 *	    No specific transport is set.  In general, this should
 *	    be used with a uux transport, such as uux or demand.
 *
 *	private data:
 *	    domain	- domain to strip from the end of the target
 *	    required	- a required domain, the target must end in this
 *
 *	private flags:
 *	    only_local_domain - must either not have a domain portion
 *			  or the domain must have been removed by "domain"
 *			  attribute.
 *
 *	algorithm:
 *	    Pass the target to gethostbyname(), perhaps stripping a domain
 *	    component as specified by the domain attribute.  If
 *	    gethostbyname() returns a match, the proper name for the host is
 *	    returned as the next_host value.  Any initial dot in the target
 *	    is ignored.
 *
 *	    Always returns one-hop routes; i.e., a next_host value is
 *	    returned, but no route.
 *
 * Specifications for the gethostbyaddr routing driver:
 *
 *	associated transports:
 *	    No specific transport is set.  In general, this should
 *	    be used with a uux transport, such as uux or demand.
 *
 *	private data: none.
 *
 *	private flags:
 *	    fail_if_error - if set, fail an address if the target is a domain
 *			  literal but the form does not match an INET
 *			  address form.
 *
 *	    check_for_local - if set, call islocalhost() on the hostname
 *			  found by gethostbyaddr() to determine if the INET
 *			  address matches the local host.  If the special
 *			  hostname 'localhost' is returned, this will always
 *			  be considered a match for the local host.  This is
 *			  set by default.
 *
 *	algorithm:
 *	    If the target is of the form [number.number...number] then
 *	    convert the number into an INET address and call gethostbyaddr()
 *	    to find a proper name for the host at that address.  If no such
 *	    host is found, then return the proper four digit form for
 *	    internet numbers, as returned by net_itoa(), in square brackets,
 *	    as the next_host value.
 *
 *	    Examples:
 *		uts.amdahl.com		- will never be matched
 *		[127.0.0.1]		- will generally return localhost
 *		[192.257]		- might return [192.0.1.1]
 */

#define NEED_SOCKETS
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../dys.h"
#include "../addr.h"
#include "../route.h"
#include "../transport.h"
#include "../lookup.h"
#include "rtlib.h"
#include "gethost.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* functions local to this file */
static int gethostbyname_lookup();
static int gethostbyaddr_lookup();
static int bad_form();

static struct attr_table gethostbyname_attributes[] = {
    { "domain", t_string, NULL, NULL, OFFSET(gethostbyname_private, domain) },
    { "required", t_string, NULL, NULL, OFFSET(gethostbyname_private, required) },
    { "only_local_domain", t_boolean, NULL, NULL, GETHOST_ONLY_LOCAL },
};
static struct attr_table *end_gethostbyname_attributes =
    ENDTABLE(gethostbyname_attributes);

static struct attr_table gethostbyaddr_attributes[] = {
    { "fail_if_error", t_boolean, NULL, NULL, GETHOST_FAIL_IFERR },
    { "check_for_local", t_boolean, NULL, NULL, GETHOST_CHECK_LOCAL },
};
static struct attr_table *end_gethostbyaddr_attributes =
    ENDTABLE(gethostbyaddr_attributes);


/*
 * rtd_gethostbyname - call gethostbyname() for routing
 */
/* ARGSUSED */
void
rtd_gethostbyname(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, gethostbyname_lookup);
}

/*
 * rtv_gethostbyname - verify using the gethostbyname() library function
 */
/*ARGSUSED*/
void
rtv_gethostbyname(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, gethostbyname_lookup);
}

/*
 * rtb_gethostbyname - read the configuration file attributes
 */
char *
rtb_gethostbyname(rp, attrs)
    struct router *rp;			/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct gethostbyname_private gethostbyname_template = {
	NULL,				/* domain */
	NULL,				/* required */
    };
    struct gethostbyname_private *priv;	/* new private structure */

    /* copy the template private data */
    priv = (struct gethostbyname_private *)xmalloc(sizeof(*priv));
    (void)memcpy((char *)priv, (char *)&gethostbyname_template, sizeof(*priv));

    rp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    gethostbyname_attributes,
			    end_gethostbyname_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * rtp_gethostbyname - dump the configuration attributes
 */
void
rtp_gethostbyname(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				gethostbyname_attributes,
				end_gethostbyname_attributes);
}



/*
 * gethostbyname_lookup - route to a target using gethostbyname
 *
 * Use gethostbyname() to match hosts accessible through TCP/IP.
 *
 * Return one of the following values:
 *
 *	DB_SUCCEED	Matched the target host.
 *	DB_NOMATCH	Did not match the target host.
 */
/*ARGSUSED*/
static int
gethostbyname_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    struct hostent *hostentp;		/* host file entry */
    char *copy_target;			/* copy of the target */
    char *free_target;			/* free from this point */
    register char *p;			/* temp */
    struct gethostbyname_private *priv;

    priv = (struct gethostbyname_private *)rp->private;

    if (priv->required) {
	if (match_end_domain(priv->required, addr->target) == NULL) {
	    /* did not end in a required domain */
	    return DB_NOMATCH;
	}
    }

    free_target = copy_target = COPY_STRING(addr->target);
    if (copy_target[0] == '.') {
	/* ignore initial dot */
	copy_target++;
    }

    /*
     * strip any optional domain
     */
    if (priv->domain) {
	char *domain_part = match_end_domain(priv->domain, copy_target);
	if (domain_part) {
	    DEBUG1(DBG_DRIVER_HI, "strip \"%s\"\n", domain_part);
	    *domain_part = '\0';
	}
    }

    /* if we only want local names and there is any dot left, punt it */
    if ((rp->flags & GETHOST_ONLY_LOCAL) && index(copy_target, '.') != NULL) {
	xfree(free_target);
	return DB_NOMATCH;
    }

    /* look for a match, mapping upper case to lower case */
    for (p = copy_target; *p; p++) {
	if (isupper(*p)) {
	    *p = tolower(*p);
	}
    }

    hostentp = gethostbyname(copy_target);
    xfree(free_target);

    if (hostentp) {
	rt_info->next_host = hostentp->h_name;
	rt_info->matchlen = strlen(addr->target);

	return DB_SUCCEED;
    }

    return DB_NOMATCH;
}


/*
 * rtd_gethostbyaddr - call gethostbyaddr() for routing domain literals
 */
void
rtd_gethostbyaddr(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, gethostbyaddr_lookup);
}

/*
 * rtv_gethostbyaddr - verify using the gethostbyaddr() library function
 */
void
rtv_gethostbyaddr(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, gethostbyaddr_lookup);
}

/*
 * rtb_gethostbyaddr - read the configuration file attributes
 */
char *
rtb_gethostbyaddr(rp, attrs)
    struct router *rp;			/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;

    /* no private storage */
    rp->private = NULL;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)NULL,
			    attrs,
			    &rp->flags,
			    gethostbyaddr_attributes,
			    end_gethostbyaddr_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * rtp_gethostbyaddr - dump the configuration attributes
 */
void
rtp_gethostbyaddr(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				gethostbyaddr_attributes,
				end_gethostbyaddr_attributes);
}



/*
 * gethostbyaddr_lookup - route to an INET literal address.
 *
 * Match domain literal targets that specify INET addresses.
 *
 * Return one of the following values:
 *
 *	DB_SUCCEED	Matched the target.
 *	DB_NOMATCH	Did not match the target.
 *	DB_FAIL		Fail the address with the specified error.
 */
/*ARGSUSED*/
static int
gethostbyaddr_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    register char *p;			/* temp */
#if defined(INET_ADDR_USE_STRUCT) || defined(INET_NTOA_USE_STRUCT)
    struct in_addr s_inet;		/* internet address */
#endif
    unsigned long inet;			/* internet address */
    struct hostent *hostentp;		/* host file entry */

    /* check for a domain literal form */
    p = addr->target;
    if (index(p, '[') == NULL && index(p, ']') == NULL) {
	/* definitely not a domain literal */
	return DB_NOMATCH;
    }

    if (*p != '[') {
	return bad_form(rp, addr->target, error_p);
    }

    p = index(p, ']');
    if (p == NULL || *(p + 1) != '\0') {
	return bad_form(rp, addr->target, error_p);
    }

    /* see if the library likes the form */
    *p = '\0';			/* chop off ending `]', for now */
#ifdef INET_ADDR_USE_STRUCT
    s_inet = inet_addr(addr->target + 1);
    inet = s_inet.s_addr;
#else
    inet = inet_addr(addr->target + 1);
#endif
    *p = ']';			/* put it back */

    /* this is what the man page says to do, essentially */
    if (inet == (unsigned long)(-1)) {
	return bad_form(rp, addr->target, error_p);
    }

    if (fl & RT_VERIFY) {
	/* Only verifying, this is far enough */
	return DB_SUCCEED;
    }

    /* Do a reverse query to get the hostname */
    hostentp = gethostbyaddr((char *)&inet, sizeof(struct in_addr), AF_INET);

    if (hostentp) {
	/* found the address in the hosts file */
	rt_info->next_host = hostentp->h_name;
	rt_info->matchlen = strlen(addr->target);

	if ((rp->flags & GETHOST_CHECK_LOCAL) &&
	    (islocalhost(hostentp->h_name) ||
	     EQIC(hostentp->h_name, "localhost")))
	{
	    /* matched the local host */
	    rt_info->next_host = NULL;
	}
    } else {
	/* did not find host file entry, just use the inet address */
	static char sbuf[sizeof("[255.255.255.255]+some_slop")];

#ifdef INET_NTOA_USE_STRUCT
	s_inet.s_addr = inet;
	(void) sprintf(sbuf, "[%s]", inet_ntoa(s_inet));
#else
	(void) sprintf(sbuf, "[%s]", inet_ntoa(inet));
#endif
	rt_info->next_host = sbuf;
    }

    return DB_SUCCEED;
}


static int
bad_form(rp, target, error_p)
    struct router *rp;			/* router structure */
    char *target;			/* malformed target */
    struct error **error_p;		/* error structure to fill in */
{
    char *error_text;

    if (rp->flags & GETHOST_FAIL_IFERR) {
	/*
	 * ERR_157 - Malformed domain literal
	 *
	 * DESCRIPTION
	 *      A domain literal did not match the form of an INET address
	 *      and the `fail_if_error' attribute was set indicating that
	 *      addresses with such targets are to be failed.
	 *
	 * ACTIONS
	 *      The address is failed and returned to the sender or to the
	 *      address owner.
	 *
	 * RESOLUTION
	 *      The user should supply a correctly formed INET domain
	 *      literal.
	 */
	error_text = xprintf("router %s: Malformed domain literal: %s",
			     rp->name, target);

	DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

	*error_p = note_error(ERR_NSOWNER|ERR_157, error_text);

	return DB_FAIL;
    }

    return DB_NOMATCH;
}
