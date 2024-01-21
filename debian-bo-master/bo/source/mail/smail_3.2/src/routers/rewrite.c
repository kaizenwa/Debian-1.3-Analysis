/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:rewrite.c,v 1.5 1996/05/29 18:49:31 woods Exp"
 */
/*
 * rewrite.c
 *	rewriting router driver for smail3.
 *	A P Barrett <barrett@ee.und.ac.za>, 28 Jan 1995.
 */

/*
 * New code and information here is Copyright (C) 1995, Alan P. Barrett.
 *
 * Most of this code was stolen from the pathalias driver,
 * which carries the following copyright notice.
 *
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * rewrite.c
 *	rewriting driver that matches addresses against a database and
 *	gives back a new address that is subsequently reparsed.
 *
 *	XXX: There really should be a better way of sharing code between
 *	drivers that use pathalias-style database search strategies
 *	but then use the data in different ways.
 *
 * Specifications for the rewrite driver:
 *
 *	associated transports:
 *	    No specific transport is set.
 *
 *	private attribute data:
 *	    file, proto, domain, required, retries, interval:
 *		same as for pathalias driver.
 *
 *	private attribute flags:
 *	    reopen, tryagain, optional, cacheopen:
 *		same as for pathalias driver.
 *
 *	algorithm:
 *	    Given a target (host or domain name), look it up in the
 *	    database, using the same search strategy as the pathalias
 *	    driver.  The best match wins, as with pathalias.
 *
 *	    If a match is found, the information on the matching line
 *	    specifies how to rewrite the address, or whether not to
 *	    rewrite it at all.	If the address is not rewritten, the
 *	    driver behaves as if it had not been matched at all.  If
 *	    the address is rewritten, the driver sets the input address
 *	    up as the parent of the rewritten address, and returns the
 *	    rewritten address to be re-parsed.
 *
 *	    The match_always attribute has no effect on this driver.
 *	    Partial matches are treated like full matches.
 *
 *	file format:
 *	    Each line in the rewrite database has the following format:
 *
 *		domain flag format
 *
 *	    domain: as with pathalias, this is a simple hostname
 *		or a fully qualified domain name, or a partial domain
 *		name beginning with a `.'.
 *
 *	    flag: One of the following:
 *		`-'	Rewriting should not be done.  The router then
 *			behaves as if the target had not been matched.
 *		`+'	Rewriting should be done, using the specified
 *			format if the remainder was a simple mailbox
 *			(user name), and leaving the remainder unchanged
 *			if it was more complex.
 *
 *	    format: The format is a string to be variable expanded
 *		to produce the rewritten address.  The expansion
 *		is performed in a context in which $user refers to the
 *		remainder and $host refers to the target.
 *
 *	    examples:
 *		.special.com	+	special-$user
 *			This rewrites "username@subdomain.special.com"
 *			and "username@special.com" to
 *			"special-username".  For this to be useful,
 *			some other part of the smail configuration
 *			should recognise "special-username" as a
 *			local alias.  Also, don't forget to install a
 *			local alias for "special-postmaster"!  This
 *			also rewrites "user%elsewhere@special.com" to
 *			"user%elsewhere", which will later be treated
 *			like "user@elsewhere".
 *		special.com	-
 *			This prevents rewriting of "anything@special.com",
 *			overriding the effect of the above example.  When
 *			both this and the above example are used together,
 *			"username@special.com" will not be rewritten, but
 *			"username@subdomain.special.com" will be rewritten.
 *		.foo.org	+	$user-$host
 *			This rewrites "username@foo.org" to
 *			"username-foo.org", and rewrites
 *			"username@subdomain.foo.org" to
 *			"username-subdomain.foo.org".
 *		frobozz.com	+	${lookup:user:lsearch{frobozz-aliases}{$value}{postmaster}}
 *			This rewrites "username@frobozz.com" by
 *			searching for "username" in the file
 *			"frobozz-aliases" (in the smail lib
 *			directory).  If the search fails, it rewrites
 *			to "postmaster". Note that the format of the
 *			file searched by the ${lookup...} expansion is a
 *			lot more restricted than the format of a normal
 *			alias file, and that each input must map to
 *			exactly one output in such a file.
 *		frobozz.com	+	${lookup:user:lsearch{frobozz-aliases}{$value}{$input_addr}}
 *			As for the previous case, except that if
 *			the file lookup fails it leaves the address
 *			unchanged.  The unchanged address will be passed
 *			on to subsequent routers, which might be able to
 *			route it or might fail it as un unknown host.
 *			XXX: There should probably be a way to fine-tune
 *			the error handling.
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
#include "rewrite.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/*
 * XXX: these error codes should go in error.h, or should be shared with
 * other pathalias-like routers.
 */
#define ERR_1201 1201L	/* bad entry in rewrite database */
#define ERR_1202 1202L	/* failed to open rewrite database */
#define ERR_1203 1203L	/* rewrite file lookup error */
#define ERR_1204 1204L	/* rewrite expansion failed */
#define ERR_1205 1205L	/* preparse error on rewritten address */

/* functions local to this file */
static int rewrite_lookup();
static void close_if_reopen();
static int find_domain();
static struct error *bad_entry();
static struct error *open_failed();
static struct error *lookup_error();
static struct error *expand_error();

static struct attr_table rewrite_attributes[] = {
    /* note that many of these are shared with pathalias */
    { "file", t_string, NULL, NULL, OFFSET(pathalias_private, file) },
    { "proto", t_string, NULL, NULL, OFFSET(pathalias_private, proto) },
    { "domain", t_string, NULL, NULL, OFFSET(pathalias_private, domain) },
    { "required", t_string, NULL, NULL, OFFSET(pathalias_private, required) },
    { "retries", t_int, NULL, NULL, OFFSET(pathalias_private, retries) },
    { "interval", t_int, NULL, NULL, OFFSET(pathalias_private, interval) },
    { "reopen", t_boolean, NULL, NULL, PA_REOPEN },
    { "tryagain", t_boolean, NULL, NULL, PA_TRYAGAIN },
    { "optional", t_boolean, NULL, NULL, PA_OPTIONAL },
    { "cacheopen", t_boolean, NULL, NULL, PA_CACHEOPEN },
};
static struct attr_table *end_rewrite_attributes =
    ENDTABLE(rewrite_attributes);



/*
 * rtd_rewrite - rewrite using a rewrite database
 */
void
rtd_rewrite(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, rewrite_lookup);
    close_if_reopen(rp);
}

/*
 * rtv_rewrite - verify that a match exists in a rewrite database
 */
void
rtv_rewrite(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, rewrite_lookup);
    close_if_reopen(rp);
}

#ifdef notyet
/*
 * rtc_rewrite - open a rewrite database in the server, if so requested
 */
void
rtc_rewrite(rp)
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
 * rtf_rewrite - close a rewrite database when smail is done with router
 */
void
rtf_rewrite(rp)
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
 * rtb_rewrite - read the configuration file attributes
 */
char *
rtb_rewrite(rp, attrs)
    struct router *rp;			/* router entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct rewrite_private rewrite_template = {
	{ /* XXX: keep this in synch with pathalias driver.
	   * should really do this the way bindlib does it. */
	NULL,				/* file */
	"lsearch",			/* proto */
	NULL,				/* domains */
	NULL,				/* required */
#ifdef	HAVE_RENAME
	0,				/* retries */
#else	/* not HAVE_RENAME */
	1,				/* retries */
#endif	/* not HAVE_RENAME */
	2,				/* interval */
	NULL,				/* database -- for internal use */
	},
    };
    struct rewrite_private *priv;	/* new rewrite_private structure */

    /* copy the template private data */
    priv = (struct rewrite_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&rewrite_template, sizeof(*priv));

    rp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    rewrite_attributes,
			    end_rewrite_attributes);

    /* 
     * This router can make changes on a per user basis, or 
     * have outputs affecting a user.
     * Hence this flag is set to prevent rtd_standard caching
     * the routing information from it.
     */
    rp->flags |= RT_AFFECTS_USER;

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * rtp_rewrite - dump the configuration attributes
 */
void
rtp_rewrite(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				rewrite_attributes,
				end_rewrite_attributes);
}



/*
 * rewrite_lookup - lookup a host in a rewrite database
 *
 * Use the algorithm described at the top of this source file for
 * finding a match for a target.
 *
 * Return one of the following values:
 *
 * These return codes apply only to the specific address:
 *	DB_SUCCEED	Matched the target host and rewrote the address
 *			to something different from before.  The new
 *			address is returned in addr->remainder.
 *	DB_NOMATCH	Did not match, or matched but did not rewrite,
 *			or matched and rewrote but rewritten address
 *			was unchanged.
 *	DB_FAIL		Fail the address with the given error.
 *	DB_AGAIN	Try to route with this address again at a
 *			later time.
 *
 * These return codes apply to this router in general:
 *	FILE_NOMATCH	The rewrite database could not be opened and
 *			is optional.
 *	FILE_AGAIN	File is required to exist but does not,
 *			Try again later.
 *	FILE_FAIL	A major error has been caught in router,
 *			notify postmaster.
 */
/*ARGSUSED*/
static int
rewrite_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    int success;	/* status of database lookup */
    char *p;		/* temp */
    char *raw_data;	/* raw data from matching line in database */
    char *match;	/* substring of target that was matched in database */
    char *flag;		/* flag field from raw_data */
    char *format;	/* format field from raw_data */
    int form;		/* form of remainder */
    char *error;	/* error from some lower-level functions */
    char *new_address;	/* rewritten address */

    /* is the target in the database? */
    success = find_domain(rp, addr, &raw_data, &match, error_p);

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

    /* parse raw_data into flag and format */
    p = raw_data;
    if (p) {
	while (*p && isspace(*p)) p++;
    }
    if (p == NULL || *p == '\0') {
	*error_p = bad_entry(rp, raw_data);
	return DB_AGAIN;
    }
    flag = p;
    while (*p && !isspace(*p)) p++;
    while (*p && isspace(*p)) p++;
    format = p; /* might be empty string.  check later if we care */

    /* does the database say we should rewrite this address? */
    switch (*flag) {
    case '-':	return DB_NOMATCH;
    case '+':	if (*format) {
		    break; /* rewrite it */
		} else {
		    *error_p = bad_entry(rp, raw_data);
		    return DB_AGAIN;
		}
    default:	*error_p = bad_entry(rp, raw_data);
		return DB_AGAIN;
    };

    /* is the remainder a simple user name? */
    form = parse_address(addr->remainder, (char **)0, &error, (int *)0);
    switch (form) {
    case LOCAL:
	    /* rewrite mailbox@target according to specified format */
	    {
		struct addr temp_addr;

		/*
		 * set temp_addr up so that $user and $host expand
		 * the way we want
		 */
		(void) memcpy((char *) &temp_addr, (char *) addr, sizeof(*addr));
		temp_addr.next_addr = addr->remainder;	/* for ${user} */
		temp_addr.next_host = addr->target;	/* for ${host} */

		new_address = expand_string(format,
					    &temp_addr, (char *)NULL, addr->remainder);
	    }
	    if (new_address == NULL) {
		*error_p = expand_error(rp, format);
		return DB_AGAIN;
	    }

	    /* preparse to strip comments etc. */
	    p = preparse_address(new_address, &error);
	    if (p == NULL) {
		*error_p = expand_error(rp, format);
		return DB_AGAIN;
	    }
	    new_address = p;
	    break;

    default:
	    /* rewrite otherstuff@target to just otherstuff */
	    new_address = COPY_STRING(addr->remainder);
	    break;
    }

    /* is rewritten address the same as before rewriting? */
    if (EQIC(new_address, addr->in_addr)) {
	DEBUG2(DBG_DRIVER_HI, "rewrite_lookup: not rewriting %s --> %s\n",
	       addr->in_addr, new_address);
	return DB_NOMATCH;
    }

    /*
     * It seems like a good idea to record the fact that the old address
     * is the parent of the new address (mostly so that the parent
     * address will appear in the logs).  The usual place where that
     * happens is in a director (not a router), and the usual way of
     * doing that is to allocate a new addr structure, fill the newly
     * allocated struct with the new info, and adjust pointers to link
     * the new struct into an appropriate list and leave the old struct
     * in limbo.  But we don't have access to the linked list pointers,
     * so we cheat, taking advantage of the way we know we will be
     * called from rtd_standard or rtv_standard, which does have access
     * to the lists.  What we do is allocate a new addr structure, copy
     * the old information to the newly allocated structure where it
     * is not on any of the linked lists, and modify the current addr
     * structure to contain the new information.  After that, the old
     * information is in limbo and the new information is linked into a
     * list, which is what we want.
     */
    {
	struct addr *new = alloc_addr();
	(void) memcpy((char *) new, (char *) addr, sizeof(*addr));
	addr->parent = new;
	addr->in_addr = COPY_STRING(new_address);
	addr->remainder = new_address;
    }

    DEBUG2(DBG_DRIVER_MID, "rewrite_lookup: rewrote %s --> %s\n",
	   addr->parent->in_addr, new_address);

    /*
     * all the following is necessary to tell higher level software
     * to re-parse the address passed back in addr->remainder.
     */
    rt_info->next_host = NULL;
    rt_info->route = NULL;
    rt_info->matchlen = strlen(addr->target);
    return DB_SUCCEED;
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
find_domain(rp, addr, raw_data, match, error_p)
    struct router *rp;			/* router entry */
    struct addr *addr;			/* addr structure containing target */
    char **raw_data;			/* raw data returned by lookup */
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
    success = lookup_database(priv->database, target, raw_data, &error_text);
    if (success != DB_NOMATCH) {
	if (savedomain) {
	    *savedomain = '.';		/* restore the target */
	}
	*match = target;		/* return the match and the data */
	if (success == DB_SUCCEED) {
	    while (isspace(**raw_data)) (*raw_data)++;
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
				  raw_data, &error_text);
	if (success != DB_NOMATCH) {
	    if (savedomain) {
		*savedomain = '.';
	    }
	    *match = target + 1;
	    if (success == DB_SUCCEED) {
		while (isspace(**raw_data)) (*raw_data)++;
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
	success = lookup_database(priv->database, p, raw_data, &error_text);
	xfree(p);
	if (success != DB_NOMATCH) {
	    if (savedomain) {
		*savedomain = '.';
	    }
	    *match = target;
	    if (success == DB_SUCCEED) {
		while (isspace(**raw_data)) (*raw_data)++;
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
				      raw_data, &error_text );
	    if (success != DB_NOMATCH) {
		if (savedomain) {
		    *savedomain = '.';
		}
		*match = target;
		if (success == DB_SUCCEED) {
		    while (isspace(**raw_data)) (*raw_data)++;
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
bad_entry(rp, raw_data)
    struct router *rp;
    char *raw_data;
{
    char *error_text;

    /*
     * ERR_1201 - bad entry in rewrite database
     *
     * DESCRIPTION
     *      The rewrite line didn't match any of the expected patterns.
     *
     * ACTIONS
     *      Defer the message with a configuration error.
     *
     * RESOLUTION
     *      The postmaster should correct the rewrite database entry.
     */
    error_text = xprintf("router %s: bad entry in rewrite database: %s",
			 rp->name, raw_data);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_CONFERR|ERR_1201, error_text);
}

static struct error *
open_failed(rp, file, open_error)
    struct router *rp;
    char *file;
    char *open_error;
{
    char *error_text;

    /*
     * ERR_1202 - failed to open rewrite database
     *
     * DESCRIPTION
     *      open_database() failed to open a rewrite database.  The
     *      error encountered should be stored in errno.
     *
     * ACTIONS
     *      Defer all of the input addresses as configuration errors.
     *
     * RESOLUTION
     *      The postmaster should check the router entry against the
     *      database he wishes to use.
     */
    error_text = xprintf("router %s: rewrite database %s, open failed: %s",
			 rp->name, file, open_error);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_1202, error_text);
}

static struct error *
lookup_error(rp, target, lookup_error_text)
    struct router *rp;
    char *target;
    char *lookup_error_text;
{
    char *error_text;

    /*
     * ERR_1203 - rewrite file lookup error
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

    return note_error(ERR_1203, error_text);
}

static struct error *
expand_error(rp, format)
    struct router *rp;
    char *format;
{
    char *error_text;

    /*
     * ERR_1204 - rewrite expansion failed
     *
     * DESCRIPTION
     *      expand_string() failed to expand the format
     *      for a rewrite router.
     *
     * ACTIONS
     *      Defer the message with a configuration error.
     *
     * RESOLUTION
     *      The postmaster should fix the entry in the
     *	    rewrite database.
     */
    error_text = xprintf("router %s: expansion of \"%s\" failed",
			 rp->name, format);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_CONFERR|ERR_1204, error_text);
}
