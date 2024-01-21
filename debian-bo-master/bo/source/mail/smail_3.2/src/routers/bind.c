/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:bind.c,v 1.27 1996/02/28 14:27:27 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * September 1991:
 *	Bind router substantially rewritten by Simon Leinen
 *	<simon@liasun3.epfl.ch>.
 */

/*
 * bind.c
 *	routing driver which connects to a Berkeley Internet Name
 *	Domain (BIND) server for routing mail.  At the present time
 *	this router has only been tested with BIND4.8, which was
 *	obtained from the archives on uunet.
 *
 * Specifications for this router driver can be found in "bindlib.c".
 *
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../route.h"
#include "../transport.h"
#include "../lookup.h"
#include "../dys.h"
#include "rtlib.h"
#include "bind.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* functions local to this file */

#ifdef ANSI_C
# define P_(x) x
#else
# define P_(x) ()
#endif

static int bind_lookup P_((struct router*,struct addr*,int,struct rt_info*,
			   struct error **));
#undef P_

static struct attr_table bind_attributes[] = {
    BIND_ATTRIBUTES(bind_private, bindlib_attr),
};
static struct attr_table *end_bind_attributes = ENDTABLE(bind_attributes);


/*
 * rtd_bind - route using Berkeley Internet Name Domain server
 */
void
rtd_bind(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, bind_lookup);
}

/*
 * rtv_bind - verify that a match exists for a list of addr structures
 */
/*ARGSUSED*/
void
rtv_bind(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, bind_lookup);
}

/*
 * rtb_bind - read the configuration file attributes
 */
char *
rtb_bind(rp, attrs)
    struct router *rp;			/* router entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct bind_private bind_template = {
	BIND_TEMPLATE_ATTRIBUTES,
    };
    struct bind_private *priv;		/* new bind_private structure */

    /* copy the template private data */
    priv = (struct bind_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&bind_template, sizeof(*priv));

    rp->private = (char *)priv;
    rp->flags |= (BIND_DEFNAMES|BIND_DEFER_NO_CONN);
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    bind_attributes,
			    end_bind_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * rtp_bind - dump the configuration attributes
 */
void
rtp_bind(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				bind_attributes,
				end_bind_attributes);
}


/*
 * bind_lookup - lookup a host in through the domain system
 *
 * Use the algorithm described at the top of this source file for
 * finding a match for a target.
 *
 * Return one of the following values:
 *
 * These return codes apply only to the specific address:
 *	DB_SUCCEED	Matched the target host.
 *	DB_NOMATCH	Did not match the target host.
 *	DB_FAIL		Fail the address with the given error.
 *	DB_AGAIN	Try to route with this address again at a
 *			later time.
 *
 * These return codes apply to this router in general:
 *	FILE_NOMATCH	There is no server running on this machine.
 *	FILE_AGAIN	Lost contact with server, or server is
 *			required to exist.  Try again later.
 *	FILE_FAIL	A major error has been caught in router,
 *			notify postmaster.
 */
/*ARGSUSED*/
static int
bind_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    struct bind_private *priv;
    char *what;
    int ret;

    priv = (struct bind_private *)rp->private;

    what = xprintf("router %s", rp->name);
    ret = bind_addr(addr->target, rp->flags, &priv->bindlib_attr,
		    what, rt_info, error_p);
    xfree(what);

    return ret;
}
