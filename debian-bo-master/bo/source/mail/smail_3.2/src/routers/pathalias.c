/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:pathalias.c,v 1.8 1996/03/04 13:28:52 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * pathalias.c
 *	routing driver which obtains UUCP-style paths from a database
 *	and returns matches to the target in next_host, route fashion
 *
 * Specifications for the pathalias routing driver:
 *
 *	associated transports:
 *	    No specific transport is set.
 *
 *	private attribute data:
 *	    file (string):  the name of the file which contains the
 *		host to route association database.  In the case of
 *		the dbm protocol this will actually be the basename
 *		for the two associated DBM files, ending in .pag and
 *		.dir.
 *
 *	    proto (name):  specifies the protocol used in accessing
 *		the database.  Can be one of:
 *
 *		lsearch - performs a linear search.
 *		bsearch - performs a straightforward binary search
 *		    on a sorted file (default).
 *		dbm - uses the Berkeley DBM routines.  If NDBM is
 *		    available, it is used instead.  If only the
 *		    older routines are available, exactly one DBM
 *		    database can be opened by smail.
 *
 *	    domain (string):  specifies the default domain for hosts
 *		in the database.  Targets ending in this domain will
 *		have the domain stripped (including a preceding dot)
 *		before the database is searched.  A target containing
 *		only the domain (e.g., .uucp) will remain .uucp.
 *
 *	private attribute flags:
 *	    reopen:  if set, then reopen the database for each call
 *		to the routing driver, and close before each return.
 *		This is necessary for systems that would not otherwise
 *		have a sufficient number of available file descriptors
 *		for all of their routing and directing needs.
 *	    optional:  if set, then if the open fails, assume an empty
 *		paths file.
 *	    tryagain:  if set, then if the open fails, try again on a
 *		later spool directory queue run.
 *
 *	algorithm:
 *	    For pathalias routing, given a target use the following
 *	    strategy:
 *
 *	    1.	if the domain begins with a dot, then look for a
 *		match including the dot.  If not found
 *		look for a match without the dot.
 *	    2.	if the domain does not begin with a dot, then look
 *		for a match without the dot.  If not found
 *		look for a match with a dot.
 *	    3.	if previous attempts failed, strip first component
 *		of the domain, leaving the initial dot on
 *		the second component.  Look for a match.
 *	    4.	if this fails, strip the next component and repeat
 *		from step 3 until no more components remain.
 *
 *	    If a path is found, the trailing !%s or %s is stripped.
 *	    Then the first path component is returned as the next_host
 *	    value, and the rest of the path is returned as the route.
 *
 *	    NOTE:  if a domain from the domain attribute was stripped
 *		   from the end of the target, the match count
 *		   returned will include the length of that domain.
 *		   Also remember that the target string passed must
 *		   be left intact for higher-level software.
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
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
#include "pathalias.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* functions local to this file */
static int pathalias_lookup();
static void close_if_reopen();
static int find_domain();
static char *strip_costs();
static struct error *bad_entry();
static struct error *open_failed();
static struct error *lookup_error();

static struct attr_table pathalias_attributes[] = {
    { "file", t_string, NULL, NULL, OFFSET(pathalias_private, file) },
    { "proto", t_string, NULL, NULL, OFFSET(pathalias_private, proto) },
    { "domain", t_string, NULL, NULL, OFFSET(pathalias_private, domain) },
    { "required", t_string, NULL, NULL,
	  OFFSET(pathalias_private, required) },
    { "retries", t_int, NULL, NULL, OFFSET(pathalias_private, retries) },
    { "interval", t_int, NULL, NULL, OFFSET(pathalias_private, interval) },
    { "reopen", t_boolean, NULL, NULL, PA_REOPEN },
    { "tryagain", t_boolean, NULL, NULL, PA_TRYAGAIN },
    { "optional", t_boolean, NULL, NULL, PA_OPTIONAL },
    { "cacheopen", t_boolean, NULL, NULL, PA_CACHEOPEN },
};
static struct attr_table *end_pathalias_attributes =
    ENDTABLE(pathalias_attributes);


/*
 * rtd_pathalias - route using a pathalias database
 */
void
rtd_pathalias(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, pathalias_lookup);
    close_if_reopen(rp);
}

/*
 * rtv_pathalias - verify that a match exists in a pathalias database
 */
void
rtv_pathalias(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, pathalias_lookup);
    close_if_reopen(rp);
}

#ifdef notyet
/*
 * rtc_pathalias - open a pathalias database in the server, if so requested
 */
void
rtc_pathalias(rp)
    struct router *rp;			/* router entry */
{
    struct pathalias_private *priv;
    int success;
    char *error_text;

    priv = (struct pathalias_private *)rp->private;
    if (rp->flags & PA_CACHEOPEN) {
	rp->flags &= ~(PA_OPENFAIL | PA_OPENAGAIN | PA_OPENNOMATCH);
	if (priv->error_text) {
	    xfree(priv->error_text);
	}
	success = cache_database(priv->file, priv->proto,
				 priv->retries, priv->interval,
				 (struct stat *)NULL,
				 &priv->database, &error_text);
	switch (success) {
	case FILE_FAIL:
	    rp->flags |= PA_OPENFAIL;
	    error_text = COPY_STRING(error_text);
	    break;

	case FILE_AGAIN:
	    rp->flags |= PA_OPENAGAIN;
	    error_text = COPY_STRING(error_text);
	    break;

	case FILE_NOMATCH:
	    rp->flags |= PA_OPENNOMATCH;
	}
    }
}
#endif

#ifdef notyet
/*
 * rtf_pathalias - close a pathalias database when smail is done with router
 */
void
rtf_pathalias(rp)
    struct router *rp;
{
    struct pathalias_private *priv;

    priv = (struct pathalias_private *)rp->private;
    rp->flags &= ~(PA_OPENFAIL | PA_OPENAGAIN | PA_OPENNOMATCH);
    if (priv->database) {
	close_database(priv->database);
    }
    if (priv->error_text) {
	xfree(priv->error_text);
    }
}
#endif

/*
 * rtb_pathalias - read the configuration file attributes
 */
char *
rtb_pathalias(rp, attrs)
    struct router *rp;			/* router entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct pathalias_private pathalias_template = {
	NULL,				/* file */
	"bsearch",			/* proto */
	NULL,				/* domains */
	NULL,				/* required */
#ifdef	HAVE_RENAME
	0,				/* retries */
#else	/* not HAVE_RENAME */
	1,				/* retries */
#endif	/* not HAVE_RENAME */
	2,				/* interval */
	NULL,				/* database -- for internal use */
    };
    struct pathalias_private *priv;	/* new pathalias_private structure */

    /* copy the template private data */
    priv = (struct pathalias_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&pathalias_template, sizeof(*priv));

    rp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    pathalias_attributes,
			    end_pathalias_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}


/*
 * rtp_pathalias - dump the configuration attributes
 */
void
rtp_pathalias(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				pathalias_attributes,
				end_pathalias_attributes);
}




/*
 * pathalias_lookup - lookup a host in a pathalias database
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
 *	FILE_NOMATCH	The pathalias database could not be opened and
 *			is optional.
 *	FILE_AGAIN	File is required to exist but does not,
 *			Try again later.
 *	FILE_FAIL	A major error has been caught in router,
 *			notify postmaster.
 */
/*ARGSUSED*/
static int
pathalias_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    int success;
    char *p;
    char *raw_path;
    char *match;

    success = find_domain(rp, addr, &raw_path, &match, error_p);

    switch (success) {
    case FILE_NOMATCH:
	if ((rp->flags & PA_OPTIONAL) == 0) {
	    struct pathalias_private *priv;

	    priv = (struct pathalias_private *)rp->private;
	    *error_p = open_failed(rp, priv->file, "Database not found");
	    success = FILE_FAIL;
	}
	/* FALLTHRU */

    case FILE_FAIL:
    case FILE_AGAIN:
    case DB_NOMATCH:
    case DB_FAIL:
    case DB_AGAIN:
	return success;
    }

    /* common cases for rt_info values */
    rt_info->next_host = NULL;
    rt_info->route = NULL;
    rt_info->matchlen = strlen(match);

    /* first form, local host */
    if (EQ(raw_path, "%s")) {
	return DB_SUCCEED;
    }

    p = index(raw_path, '!');
    if (p) {
	*p++ = '\0';
	rt_info->next_host = raw_path;
	if (EQ(p, "%s")) {
	    /* second form, host!%s, one hop to target */
	    return DB_SUCCEED;
	} else {
	    /* strip off trailing !%s */
	    char *q = p + strlen(p) - 3;

	    if (EQ(q, "!%s")) {
		/* third form, hop then route to target */
		*q = '\0';
		rt_info->route = p;
		return DB_SUCCEED;
	    }
	}
    }
    if (p) {
	*p = '!';
    }
    *error_p = bad_entry(rp, raw_path);
    return DB_AGAIN;
}

/*
 * close_if_reopen - close the database if the reopen flag is set.
 */
static void
close_if_reopen(rp)
    struct router *rp;
{
    register struct pathalias_private *priv;

    priv = (struct pathalias_private *)rp->private;
    if (priv->database && (rp->flags&PA_REOPEN)) {
	/*
	 * close the database if it was open and the
	 * reopen attribute is on stating that the database
	 * should be opened on every call to the router.
	 */
	close_database(priv->database);
	priv->database = NULL;
    }
}

/*
 * strip_costs - strip a raw path of any pathalias costs information
 *
 * given a pathalias path, make sure and remove all of the cost information
 * associated with the entry if it exsists.
 *
 * A pathalias database raw path may look like:
 *
 *    site1!site2!%s  -or-  site!site2!%s<spaces/tabs><cost>
 *
 * where cost is some numeric value.  If this is the case, then we
 * want to strip the cost off of the string, prior to returning it.
 * 
 * Return:
 *   a pointer to the raw_path without the cost information
 *
 * Mark Colburn (mark@jhereg.chi.il.us)
 *
 * Modifications by Ronald S. Karr (tron@uts.amdahl.com)
 */
static char *
strip_costs(raw_path)
    char *raw_path;           /* raw path returned by pathalias database */
{
    register char *cp, *cptab;

    cp = index(raw_path, ' ');
    if ((cptab = index(raw_path, '\t'))) {
	if (cp == NULL || cptab < cp) {
	    cp = cptab;
	}
    }
    if (cp) {
	*cp = '\0';
    }
    return (raw_path);
}

/*
 * find_domain - return the database entry for the given domain
 *
 * match all or part of the target.  If a match is found, the target
 * string that matched is returned in match.  Return one of:
 *
 * DB_SUCCEED	operation was successful and a match was found
 * DB_FAIL	unrecoverable error in lookup
 * DB_AGAIN	retry operation at a later time
 * DB_NOMATCH	no match was found for target
 * FILE_FAIL	unrecoverable database error
 * FILE_AGAIN	try using the database later
 * FILE_NOMATCH	the file was not found
 */
static int
find_domain(rp, addr, raw_path, match, error_p)
    struct router *rp;			/* router entry */
    struct addr *addr;			/* addr structure containing target */
    char **raw_path;			/* raw path returned by lookup */
    char **match;			/* store matched target here */
    struct error **error_p;		/* lookup error */
{
    struct pathalias_private *priv;	/* private data */
    char *savedomain = NULL;		/* saved position of removed domain */
    char *target = addr->target;	/* target being searched for */
    int len = strlen(target);		/* total length of target */
    int success;			/* return from function calls */
    char *error_text;			/* error messages from subroutines */

    priv = (struct pathalias_private *)rp->private;

    /*
     * check for a required domain.  If none from the list of required
     * domains is found at the end of the target, don't match
     */
    if (priv->required) {
	if (match_end_domain(priv->required, target) == NULL) {

	    /* did not end in a required domain */
	    return DB_NOMATCH;
	}
    }

    /* open the database if it is not already open */
    if (priv->database == NULL) {
	error_text = priv->error_text;
	if (rp->flags & PA_OPENFAIL) {
	    success = FILE_FAIL;
	} else if (rp->flags & PA_OPENAGAIN) {
	    success = FILE_AGAIN;
	} else if (rp->flags & PA_OPENNOMATCH) {
	    success = FILE_NOMATCH;
	} else {
#ifdef USE_TARGET_DOMAIN
	    success = open_database(expand_string(priv->file, addr, "", ""), priv->proto,
				    priv->retries, priv->interval,
				    (struct stat *)NULL, &priv->database,
				    &error_text);
#else
	    success = open_database(priv->file, priv->proto,
				    priv->retries, priv->interval,
				    (struct stat *)NULL, &priv->database,
				    &error_text);
#endif
	}
	if (success != FILE_SUCCEED) {
	    switch (success) {
	    case FILE_NOMATCH:
		rp->flags |= PA_OPENNOMATCH;
		return FILE_NOMATCH;

	    case FILE_FAIL:
		rp->flags |= PA_OPENFAIL;
		break;

	    case FILE_AGAIN:
		rp->flags |= PA_OPENAGAIN;
		break;
	    }
	    if (priv->error_text) {
		xfree(priv->error_text);
	    }
	    priv->error_text = COPY_STRING(error_text);
	    *error_p = open_failed(rp, priv->file, error_text);
	    return success;
	}
    }

    /*
     * check for a domain to be stripped.  If the target ends in one
     * of the domains listed in the domain attribute, that part of the
     * target is stripped.  The domain list is searched from left to
     * right and the first match found is used.
     */
    if (priv->domain) {
	savedomain = match_end_domain(priv->domain,
				      target[0] == '.'? target + 1: target);
	if (savedomain) {
	    *savedomain = '\0';
	}
    }

    /*
     * lookup the target as is
     */
    success = lookup_database(priv->database, target, raw_path, &error_text);
    if (success != DB_NOMATCH) {
	if (savedomain) {
	    *savedomain = '.';		/* restore the target */
	}
	*match = target;		/* return the match and the path */
	if (success == DB_SUCCEED) {
	    while (isspace(**raw_path)) (*raw_path)++;
	    (void) strip_costs(*raw_path); /* strip the associated costs */
	} else {
	    *error_p = lookup_error(rp, target, error_text);
	}
	return success;
    }
    if (target[0] == '.') {
	/*
	 * if it starts with a `.', look it up without the dot
	 */
	success = lookup_database(priv->database, target + 1,
				  raw_path, &error_text);
	if (success != DB_NOMATCH) {
	    if (savedomain) {
		*savedomain = '.';
	    }
	    *match = target + 1;
	    if (success == DB_SUCCEED) {
		while (isspace(**raw_path)) (*raw_path)++;
		(void) strip_costs(*raw_path); /* strip the associated costs */
	    } else {
		*error_p = lookup_error(rp, target, error_text);
	    }
	    return success;
	}
    } else {
	/*
	 * if it does not start with a '.', look it up with a dot.
	 * This involves making a temporary copy with a '.' at the
	 * beginning.
	 */
	char *p = xmalloc(len + 2);

	(void) sprintf(p, ".%s", target);
	success = lookup_database(priv->database, p, raw_path, &error_text);
	xfree(p);
	if (success != DB_NOMATCH) {
	    static char *new_raw_path = NULL;
	    int path_len;

	    if (savedomain) {
		*savedomain = '.';
	    }
	    *match = target;
	    if (success == DB_SUCCEED) {
		while (isspace(**raw_path)) (*raw_path)++;
		(void) strip_costs(*raw_path); /* strip the associated costs */
		path_len = strlen(*raw_path);
		if (path_len > 3 && EQ(*raw_path + path_len - 3, "!%s")) {
		    if (new_raw_path) {
			xfree(new_raw_path);
		    }
		    new_raw_path = xmalloc(strlen(target) + path_len + 2);
		    (*raw_path)[path_len - 2] = '\0';
		    (void)sprintf(new_raw_path, "%s%s!%%s", *raw_path, target);
		    *raw_path = new_raw_path;
		}
	    } else {
		*error_p = lookup_error(rp, target, error_text);
	    }
	    return success;
	}
    }

    /*
     * strip away leading domain parts until a match is found,
     * or no parts of the domain remain
     */
    while (target) {
	/* advance past an initial dot */
	if (target[0] == '.') {
	    target++;
	}

	/* advance to the next dot */
	target = index(target, '.');
	if (target) {
	    /* if there is anything left, look it up */
	    success = lookup_database(priv->database, target,
				      raw_path, &error_text );
	    if (success != DB_NOMATCH) {
		if (savedomain) {
		    *savedomain = '.';
		}
		*match = target;
		if (success == DB_SUCCEED) {
		    while (isspace(**raw_path)) (*raw_path)++;
		    (void) strip_costs(*raw_path); /* strip the costs */
		    if (EQ(*raw_path, "%s")) {
			/*
			 * partial matches are not valid if the
			 * corresponding route is to the local host.
			 * Also, for this case prevent a smarthost
			 * router from sending it elsewhere.  If we
			 * are a gateway for a host, one of the
			 * routers must match it.
			 */
			addr->flags |= ADDR_SMARTHOST | ADDR_PARTLOCAL;
			return DB_NOMATCH;
		    }
		} else {
		    *error_p = lookup_error(rp, target, error_text);
		}
		return success;
	    }
	}
    }

    /* no match found */
    if (savedomain) {
	*savedomain = '.';
    }

    return DB_NOMATCH;
}


/*
 * Create error structures for various errors.
 */

static struct error *
bad_entry(rp, raw_path)
    struct router *rp;
    char *raw_path;
{
    char *error_text;

    /*
     * ERR_124 - bad entry in pathalias database
     *
     * DESCRIPTION
     *      The pathalias line didn't match any of the expected patterns,
     *      which could be of the form %s and site!%s.
     *
     * ACTIONS
     *      Defer the message with a configuration error.
     *
     * RESOLUTION
     *      The postmaster should correct the pathalias database entry.
     */
    error_text = xprintf("router %s: bad entry in pathalias database: %s",
			 rp->name, raw_path);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_CONFERR|ERR_124, error_text);
}

static struct error *
open_failed(rp, file, open_error)
    struct router *rp;
    char *file;
    char *open_error;
{
    char *error_text;

    /*
     * ERR_123 - failed to open pathalias database
     *
     * DESCRIPTION
     *      open_database() failed to open a pathalias database.  The
     *      error encountered should be stored in errno.
     *
     * ACTIONS
     *      Defer all ofthe input addresses as configuration errors.
     *
     * RESOLUTION
     *      The postmaster should check the director entry against the
     *      database he wishes to use.
     */
    error_text = xprintf("router %s: path database %s, open failed: %s",
			 rp->name, file, open_error);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_124, error_text);
}

static struct error *
lookup_error(rp, target, lookup_error_text)
    struct router *rp;
    char *target;
    char *lookup_error_text;
{
    char *error_text;

    /*
     * ERR_159 - pathalias file lookup error
     *
     * DESCRIPTION
     *      lookup_database() returned an error.  Text describing the
     *	    error was returned by lookup_error().
     *
     * ACTIONS
     *      Action depends upon the error.
     *
     * RESOLUTION
     *      Unspecified.
     */
    error_text = xprintf("router %s: target %s, lookup failed: %s",
			 rp->name, target, lookup_error_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_159, error_text);
}
