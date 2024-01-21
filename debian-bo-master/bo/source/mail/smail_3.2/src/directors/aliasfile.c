/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:aliasfile.c,v 1.12 1996/03/19 14:52:03 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * aliasfile.c:
 *	direct mail using an aliases database stored as key/value pairs
 *	where the key is a local address and the value is a string
 *	which can be run through process_field to extract addr
 *	structures.
 *
 * Specifications for the aliasfile directing driver:
 *
 *	private attribute data:
 *	    file (string):  the name of a file which contains the
 *		key/value association database.
 *
 *	    proto (name):  specifies the protocol used in accessing
 *		the database.  Can be one of:
 *
 *		lsearch - performs a linear serach of an ASCII file.
 *		bsearch - performs a straightforward binary search
 *		    on a sorted file of text lines.
 *		dbm - use V7/BSD DBM library to perform search.
 *
 *	    modemask (number):  specifies bits that are not allowed
 *		to be set.  If some of these bits are set, the
 *		ALIAS_SECURE flag is not set for the resultant
 *		addr structures.
 *
 *	    owners (string):  list of possible owners for the file.
 *		For files owned by others, the ADDR_CAUTION bit is
 *		set in the resultant addr structures.
 *
 *	    owngroups (string):  like the `owners' attribute except
 *		that it applies to groups.
 *
 *	    retries (number):  specifies how many retries should be
 *		attempted in opening the file.  Retries are useful
 *		in for a UN*X system that does not have an atomic
 *		rename system call, where unlink/link must be used
 *		rendering the database file nonexistent for some
 *		small period of time.
 *
 *	    interval (number):  specifies the retry interval.  Sleep
 *		is called with this number between each retry to open
 *		the database.
 *
 *	private attribute flags:
 *	    reopen:  if set, then reopen the database for each call
 *		to the directing driver, and close before each return.
 *		This is necessary for systems that would not otherwise
 *		have a sufficient number of available file descriptors
 *		for all of their routing and directing needs.
 *	    optional:  if set, then if the open fails, assume an empty
 *		alias file.
 *	    tryagain:  if set, then if the open fails, try again on a
 *		later spool directory queue run.
 */
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "../smail.h"
#include "../dys.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../field.h"
#include "../log.h"
#include "../direct.h"
#include "../exitcodes.h"
#include "../lookup.h"
#include "aliasfile.h"
#ifndef DEPEND
# include "../extern.h"
# include "../error.h"
# include "../debug.h"
#endif

/* functions local to this file */
static int alias_secure();

static struct attr_table aliasfile_attributes[] = {
    { "file", t_string, NULL, NULL, OFFSET(aliasfile_private, file) },
    { "proto", t_string, NULL, NULL, OFFSET(aliasfile_private, proto) },
    { "modemask", t_mode, NULL, NULL, OFFSET(aliasfile_private, modemask) },
    { "owners", t_string, NULL, NULL, OFFSET(aliasfile_private, owners) },
    { "owngroups", t_string, NULL, NULL,
	  OFFSET(aliasfile_private, owngroups) },
    { "retries", t_int, NULL, NULL, OFFSET(aliasfile_private, retries) },
    { "interval", t_int, NULL, NULL, OFFSET(aliasfile_private, interval) },
    { "reopen", t_boolean, NULL, NULL, ALIAS_REOPEN },
    { "optional", t_boolean, NULL, NULL, ALIAS_OPTIONAL },
    { "tryagain", t_boolean, NULL, NULL, ALIAS_TRYAGAIN },
};
static struct attr_table *end_aliasfile_attributes =
	ENDTABLE(aliasfile_attributes);
static struct aliasfile_private aliasfile_template = {
    NULL,				/* file */
    "bsearch",				/* proto */
    000,				/* modemask */
    NULL,				/* owners */
    NULL,				/* owngroups */
#ifdef	HAVE_RENAME
    0,					/* retries */
#else  /* not HAVE_RENAME */
    1,					/* retries */
#endif /* not HAVE_RENAME */
    2,					/* interval */
    0,					/* flags_set -- for internal use */
    NULL,				/* database -- for internal use */
};


/*
 * dtd_aliasfile - direct using aliases database
 */
/*ARGSUSED*/
struct addr *
dtd_aliasfile(dp, in, out, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **new;			/* output new addrs to resolve */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *pass = NULL;		/* addrs to pass to next director */
    struct addr *next;			/* next value for cur */
    struct aliasfile_private *priv;	/* private data area */
    char *error;			/* error message */
    char *owner = NULL;                 /* address owner */
    char *preparsed_owner = NULL;       /* preparsed version */

    if (! do_aliasing) {
	return in;
    }

    if (!(priv = (struct aliasfile_private *)dp->private))
	priv = &aliasfile_template;

    DEBUG(DBG_DRIVER_HI, "dtd_aliasfile called\n");
    if (priv->database == NULL) {
	struct stat statbuf;		/* get stat of database */
	int ret;

	if (dp->flags & ALIAS_OPENFAIL) {
	    ret = FILE_FAIL;
	} else if (dp->flags & ALIAS_OPENAGAIN) {
	    ret = FILE_AGAIN;
	} else {
	    if (priv->error_text) {
		xfree(priv->error_text);
	    }
#ifdef USE_TARGET_DOMAIN
	    ret = open_database(expand_string(priv->file, in, "", ""), priv->proto,
				priv->retries, priv->interval,
				&statbuf, &priv->database, &error);
#else
	    ret = open_database(priv->file, priv->proto,
				priv->retries, priv->interval,
				&statbuf, &priv->database, &error);
#endif
	}
	if (ret != FILE_SUCCEED) {
	    struct error *err;

	    if (ret == FILE_FAIL) {
		dp->flags |= ALIAS_OPENFAIL;
	    }
	    if (ret == FILE_NOMATCH) {
		dp->flags |= ALIAS_OPENFAIL;
		error = "Database not found";
	    }
	    if (ret == FILE_AGAIN) {
		dp->flags |= ALIAS_OPENAGAIN;
	    } else if (dp->flags & ALIAS_OPTIONAL) {
		return in;		/* optional and not found */
	    }

	    if (! priv->error_text) {
		priv->error_text = COPY_STRING(error);
	    }

	    /*
	     * ERR_112 - failed to open alias database
	     *
	     * DESCRIPTION
	     *      open_database() failed to open an alias database.  The
	     *      error encountered should be stored in errno.
	     *
	     * ACTIONS
	     *      Defer all of the input addresses as configuration
	     *      errors.  Do not call it a configuration error if
	     *	    `tryagain' is set or open_database returned DB_AGAIN.
	     *
	     * RESOLUTION
	     *      The postmaster should check the director entry against
	     *      the database he wishes to use.
	     */
	    error = xprintf("director %s: alias database %s: %s",
			    dp->name, priv->file, priv->error_text);
	    err = note_error((dp->flags & ALIAS_TRYAGAIN) || ret == DB_AGAIN?
				ERR_CONFERR|ERR_112:
				ERR_112,
			     error);
	    insert_addr_list(in, defer, err);
	    return NULL;
	}
	if (alias_secure(priv, &statbuf)) {
	    /* check succeeded */
	    priv->flags_set = ADDR_ALIASTYPE;
	} else {
	    /* security check failed, don't secure secure flag */
	    priv->flags_set = ADDR_ALIASTYPE|ADDR_CAUTION;
	}
    }

    /* If a director owner exists, compute the owner address for this
    mailing list and add it to each generated address. This is for
    the benefit of outgoing SMTP, which wants to use it in the
    envelope for error returns. However, don't waste time if there
    aren't any addresses to set the owner for! */

    if (!(dp->owner) ||
        !(owner = expand_string(dp->owner,
                           (struct addr *)NULL,
                           dp->default_home,
                           dp->default_user)) ||
        !(preparsed_owner = preparse_address(owner, &error)))
    {
        owner = NULL;

    } else {
        /* Must verify that this owner is sensible (to avoid things like
	 * xxxx-request-request).  To do this, use the pre-parsed address.
	 * However, verify may change the value of this address by removing the
	 * local domain name.  We don't want this, because it might be a
	 * non-standard local domain name for list handling.  So make a copy
	 * beforehand.  Don't waste time verifying "postmaster", which should
	 * always be valid. */
        
        owner = copy(preparsed_owner);       /* copy to use later */
         
        if (strcmp(preparsed_owner, "postmaster") != 0) { 
            struct addr *new2 = alloc_addr();
	    struct addr *defer2 = alloc_addr();
	    struct addr *fail2_safe = alloc_addr();
	    struct addr *fail2;
	    struct addr *okay = NULL;
            
	    fail2 = fail2_safe;			/* verify_addr_list can clobber last arg */
	    new2->in_addr = preparsed_owner;
	    new2->work_addr = preparsed_owner;
            
	    /* disable use of smartuser driver */
	    new2->flags = ADDR_SMARTUSER;
            
	    verify_addr_list(new2, &okay, &defer2, &fail2);
	    if (!okay) {
		xfree(owner);
		owner = NULL;
	    }
	    xfree(new2);
	    xfree(defer2);
	    xfree(fail2_safe);
        }     
    }

    /* Set up the new addresses */

    for (cur = in; cur; cur = next) {
	char *value;			/* value from lookup_database */
	struct addr *list;		/* list returned by lookup */
	int ret;

	next = cur->succ;

	ret = lookup_database(priv->database, cur->remainder, &value, &error);
	switch (ret) {
	    struct addr *next_list;

	case DB_NOMATCH:
	    DEBUG2(DBG_DRIVER_MID, "director <%s> did not match name <%s>\n",
		   dp->name, cur->remainder);
	    cur->succ = pass;
	    pass = cur;
	    break;

	case DB_AGAIN:
	case DB_FAIL:
	case FILE_AGAIN:
	case FILE_FAIL:
	    /*
	     * ERR_158 - aliasfile lookup error
	     *
	     * DESCRIPTION
	     *      database_lookup() returned an error, rather than
	     *      success or not found.
	     *
	     * ACTIONS
	     *      Defer the address with a configuration error, for
	     *      DB_FAIL, or without a configuration error, for
	     *      DB_AGAIN.
	     *
	     * RESOLUTION
	     *      For DB_FAIL, the postmaster will need to look into
	     *      why the database lookup failed.  For DB_AGAIN, a
	     *      later queue run will hopefully take care of the
	     *      problem.
	     */
	    DEBUG3(DBG_DRIVER_LO, "director %s: lookup failure on %s: %s\n",
		   dp->name, cur->remainder, error);
	    error = xprintf("director %s: lookup failure on %s: %s",
			    dp->name, cur->remainder, error);
	    cur->error =
		note_error(ret == DB_AGAIN || ret == FILE_AGAIN?
				ERR_CONFERR|ERR_158:
				ERR_158,
			   error);
	    cur->succ = *defer;
	    *defer = cur;
	    break;

	case DB_SUCCEED:
            DEBUG4(DBG_DRIVER_LO,
		   "director %s: matched %s, aliased to %s, \n  owner %s\n",
		   dp->name, cur->remainder, value,
		   owner? owner : "(none)");

	    cur->director = dp;		/* set the director which matched */

	    /* get the addr structures */
	    list = NULL;
	    error = NULL;
	    (void) process_field((char *)NULL, value, (char *)NULL,
				 (char *)NULL, &list, F_ALIAS, &error);
	    if (error) {
		/*
		 * ERR_113 - alias parsing error
		 *
		 * DESCRIPTION
		 *      process_field() found an error while parsing an
		 *      entry from an alias database.  The specific error is
		 *      stored in `error'.
		 *
		 * ACTIONS
		 *      Fail the address and send to the owner or to the
		 *      postmaster.
		 *
		 * RESOLUTION
		 *      The postmaster or alias owner should correct the
		 *      alias database entry.
		 */
		cur->error =
		    note_error(ERR_NPOWNER|ERR_113,
			       xprintf("director %s: error parsing alias: %s",
				       dp->name, error));
		cur->succ = *fail;
		*fail = cur;
		break;
	    }
	    for (; list; list = next_list) {
		next_list = list->succ;

                list->owner = owner;
		list->parent = cur;
		list->director = dp;
		list->flags = priv->flags_set;
		list->succ = *new;
		*new = list;
	    }
	    break;
	}
    }

    if (dp->flags&ALIAS_REOPEN) {
	close_database(priv->database);
	priv->database = NULL;
    }
    return pass;			/* return addrs for next director */
}

/*
 * dtv_aliasfile - verify using aliases database
 */
/*ARGSUSED*/
void
dtv_aliasfile(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next addr to process */
    struct aliasfile_private *priv;	/* private data area */
    char *error;			/* error message */

    if (! do_aliasing) {
	/* no aliasing, return all of them */
	insert_addr_list(in, retry, (struct error *)NULL);
	return;
    }

    if (!(priv = (struct aliasfile_private *)dp->private))
	priv = &aliasfile_template;

    DEBUG(DBG_DRIVER_HI, "dtv_aliasfile called\n");

    /* open the database, if it is not already open */
    if (priv->database == NULL) {
	int ret;

	if (dp->flags & ALIAS_OPENFAIL) {
	    ret = FILE_FAIL;
	} else if (dp->flags & ALIAS_OPENAGAIN) {
	    ret = FILE_AGAIN;
	} else {
	    if (priv->error_text) {
		xfree(priv->error_text);
	    }
#ifdef USE_TARGET_DOMAIN
	    ret = open_database(expand_string(priv->file, in, "", ""), priv->proto,
				priv->retries, priv->interval,
				(struct stat *)NULL, &priv->database,
				&error);
#else
	    ret = open_database(priv->file, priv->proto,
				priv->retries, priv->interval,
				(struct stat *)NULL, &priv->database,
				&error);
#endif
	}
	if (ret != FILE_SUCCEED) {
	    struct error *err;

	    if (ret == FILE_FAIL) {	
		dp->flags |= ALIAS_OPENFAIL;
	    }
	    if (ret == FILE_NOMATCH) {
		error = "Database not found";
		dp->flags |= ALIAS_OPENFAIL;
	    }
	    if (ret == FILE_AGAIN) {
		dp->flags |= ALIAS_OPENAGAIN;
	    } else if (dp->flags & ALIAS_OPTIONAL) {
		/* optional and not found */
		insert_addr_list(in, retry, (struct error *)NULL);
		return;
	    }

	    if (! priv->error_text) {
		priv->error_text = COPY_STRING(error);
	    }

	    /*
	     * ERR_112 - failed to open alias database
	     *
	     * DESCRIPTION
	     *      open_database() failed to open an alias database.  The
	     *      error encountered should be stored in errno.
	     *
	     * ACTIONS
	     *      Defer all of the input addresses as configuration
	     *      errors.  Do not call it a configuration error if
	     *	    `tryagain' is set or open_database returned DB_AGAIN.
	     *
	     * RESOLUTION
	     *      The postmaster should check the director entry against
	     *      the database he wishes to use.
	     */
	    error = xprintf("director %s: alias database %s: %s",
			    dp->name, priv->file, error);
	    err = note_error((dp->flags & ALIAS_TRYAGAIN) || ret == DB_AGAIN?
				ERR_112:
				ERR_CONFERR|ERR_112,
			     error);
	    insert_addr_list(in, defer, err);
	    return;
	}
    }

    /* loop through all of the input addrs */
    for (cur = in; cur; cur = next) {
	int ret;
	char *value;

	next = cur->succ;

	/* check for a match, without actually using the value */
	ret = lookup_database(priv->database, cur->remainder, &value, &error);
	switch (ret) {

	case DB_NOMATCH:
	    DEBUG2(DBG_DRIVER_MID, "director <%s> did not match name <%s>\n",
		   dp->name, cur->remainder);
	    /* no match */
	    cur->succ = *retry;
	    *retry = cur;
	    break;

	case DB_SUCCEED:
	    DEBUG2(DBG_DRIVER_LO, "director <%s> matched <%s>\n",
		   dp->name, cur->remainder);
	    /* match */
	    cur->succ = *okay;
	    *okay = cur;
	    break;

	case DB_AGAIN:
	case DB_FAIL:
	case FILE_AGAIN:
	case FILE_FAIL:
	    /*
	     * ERR_158 - aliasfile lookup error
	     *
	     * DESCRIPTION
	     *      database_lookup() returned an error, rather than
	     *      success or not found.
	     *
	     * ACTIONS
	     *      Defer the address with a configuration error, for
	     *      DB_FAIL, or without a configuration error, for
	     *      DB_AGAIN.
	     *
	     * RESOLUTION
	     *      For DB_FAIL, the postmaster will need to look into
	     *      why the database lookup failed.  For DB_AGAIN, a
	     *      later queue run will hopefully take care of the
	     *      problem.
	     */
	    DEBUG3(DBG_DRIVER_LO, "director %s: lookup failure on %s: %s\n",
		   dp->name, cur->remainder, error);
	    error = xprintf("director %s: lookup failure on %s: %s",
			    dp->name, cur->remainder, error);
	    cur->error =
		note_error(ret == DB_AGAIN || ret == FILE_AGAIN?
				ERR_CONFERR|ERR_158:
				ERR_158,
			   error);
	    cur->succ = *defer;
	    *defer = cur;
	    break;
	}
    }

    if (dp->flags&ALIAS_REOPEN) {
	close_database(priv->database);
	priv->database = NULL;
    }
}

/*
 * dtb_aliasfile - read the configuration file attributes
 */
char *
dtb_aliasfile(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    struct aliasfile_private *priv;	/* new aliasfile_private structure */

    /* copy the template private data */
    priv = (struct aliasfile_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&aliasfile_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    aliasfile_attributes,
			    end_aliasfile_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}


/*
 * dtp_aliasfile - dump aliasfile config
 */
void
dtp_aliasfile(f, dp)
     FILE * f;
     struct director *dp;
{
    (void) dump_standard_config(f,
				(dp->private) ? dp->private : (char *)&aliasfile_template,
				dp->name,
				dp->flags,
				aliasfile_attributes,
				end_aliasfile_attributes);
}

/*
 * alias_secure - determine if an alias file is secure
 *
 * return TRUE if an aliasfile is secure, given the stat structure
 * and the constraints in the private structure.
 */
static int
alias_secure(priv, statp)
    struct aliasfile_private *priv;	/* source of constraints */
    struct stat *statp;			/* source of data */
{
    /* first constraint, bits in modemask must not be set in st_mode */
    if (statp->st_mode & priv->modemask) {
	return FALSE;
    }

    /* look through the list of acceptible owners */
    if (priv->owners && priv->owners[0]) {
	char *temp = priv->owners;
	int found = FALSE;

	/* step through all of the owners */
	for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
	    struct passwd *pw = getpwbyname(temp);

	    /* ignore names not in the passwd file */
	    if (pw == NULL) {
		continue;
	    }

	    /* otherwise check for a match */
	    if (pw->pw_uid == statp->st_uid) {
		found = TRUE;
		break;
	    }
	}

	if (!found) {
	    return FALSE;
	}
    }

    /* check list of allowable owning groups */
    if (priv->owngroups && priv->owngroups[0]) {
	char *temp = priv->owngroups;
	int found = FALSE;

	/* step through all of the owners */
	for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
	    struct group *gr = getgrbyname(temp);

	    /* ignore names not in the passwd file */
	    if (gr == NULL) {
		continue;
	    }

	    /* otherwise check for a match */
	    if (gr->gr_gid == statp->st_gid) {
		found = TRUE;
		break;
	    }
	}

	if (!found) {
	    return FALSE;
	}
    }

    return TRUE;			/* checks out, by default */
}
