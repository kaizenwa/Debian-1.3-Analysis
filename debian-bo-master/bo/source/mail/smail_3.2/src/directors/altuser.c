/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:altuser.c,v 1.2 1996/05/29 20:30:09 woods Exp"
 */

/*
 *    Copyright (C) 1995  Nigel Metheringham, PLAnet OnLine
 *			  Heavily based on code written as follows
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * altuser.c:
 *	direct mail as per the user director, using an alternate
 *	passwd format file.  This file may be accessed by any of the
 *	standard search methods, and so the interface looks just like
 *	that of aliasfile.
 *
 * Specifications for the aliasfile directing driver:
 *
 *	private attribute data:
 *	    pwfile (string):  the name of a file which contains the
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
 *		ALTUSER_SECURE flag is not set for the resultant
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
#include "altuser.h"
#ifndef DEPEND
# include "../extern.h"
# include "../error.h"
# include "../debug.h"
#endif

#undef P_
#ifdef ANSI_C
# define P_(x) x
#else
# define P_(x) ()
#endif

/* functions local to this file */
static int altuserfile_secure P_((struct altuser_private *, struct stat *));
static int parse_pwline P_((char *, char *, int *, int *, char * *));

static struct attr_table altuser_attributes[] = {
    { "pwfile", t_string, NULL, NULL, OFFSET(altuser_private, pwfile) },
    { "proto", t_string, NULL, NULL, OFFSET(altuser_private, proto) },
    { "modemask", t_mode, NULL, NULL, OFFSET(altuser_private, modemask) },
    { "owners", t_string, NULL, NULL, OFFSET(altuser_private, owners) },
    { "owngroups", t_string, NULL, NULL,
	  OFFSET(altuser_private, owngroups) },
    { "retries", t_int, NULL, NULL, OFFSET(altuser_private, retries) },
    { "interval", t_int, NULL, NULL, OFFSET(altuser_private, interval) },
    { "reopen", t_boolean, NULL, NULL, ALTUSER_REOPEN },
    { "tryagain", t_boolean, NULL, NULL, ALTUSER_TRYAGAIN },
    { "transport", t_string, NULL, NULL, OFFSET(altuser_private, transport) },
};
static struct attr_table *end_altuser_attributes =
	ENDTABLE(altuser_attributes);
static struct altuser_private altuser_template = {
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
 * dtd_altuser - direct using altuseres database
 */
/*ARGSUSED*/
struct addr *
dtd_altuser(dp, in, out, new, defer, fail)
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
    struct altuser_private *priv;	/* private data area */
    int altuser_uid;			/* uid for altuser entry */
    int altuser_gid;			/* gid for altuser entry */
    char * altuser_home;		/* home for altuser entry */
    char *error;			/* error message */

    if (!(priv = (struct altuser_private *)dp->private))
	priv = &altuser_template;

    DEBUG(DBG_DRIVER_HI, "dtd_altuser called\n");
    if (priv->database == NULL) {
	struct stat statbuf;		/* get stat of database */
	int ret;

	if (dp->flags & ALTUSER_OPENFAIL) {
	    ret = FILE_FAIL;
	} else if (dp->flags & ALTUSER_OPENAGAIN) {
	    ret = FILE_AGAIN;
	} else {
	    if (priv->error_text) {
		xfree(priv->error_text);
	    }
	    ret = open_database(priv->pwfile, priv->proto,
				priv->retries, priv->interval,
				&statbuf, &priv->database, &error);
	}
	if (ret != FILE_SUCCEED) {
	    struct error *err;

	    if (ret == FILE_FAIL) {
		dp->flags |= ALTUSER_OPENFAIL;
	    }
	    if (ret == FILE_NOMATCH) {
		dp->flags |= ALTUSER_OPENFAIL;
		error = "Database not found";
	    }
	    if (ret == FILE_AGAIN) {
		dp->flags |= ALTUSER_OPENAGAIN;
	    } 

	    if (! priv->error_text) {
		priv->error_text = COPY_STRING(error);
	    }

	    /*
	     * ERR_112 - failed to open altuser database
	     *
	     * DESCRIPTION
	     *      open_database() failed to open an altuser database.  The
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
	    error = xprintf("director %s: altuser database %s: %s",
			    dp->name, priv->pwfile, priv->error_text);
	    err = note_error((dp->flags & ALTUSER_TRYAGAIN) || ret == DB_AGAIN?
				ERR_CONFERR|ERR_112:
				ERR_112,
			     error);
	    insert_addr_list(in, defer, err);
	    return NULL;
	}
	if (altuserfile_secure(priv, &statbuf)) {
	    /* check succeeded */
	    priv->flags_set = ADDR_ISUSER;
	} else {
	    /* security check failed, don't secure secure flag */
	    priv->flags_set = ADDR_ISUSER|ADDR_CAUTION;
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
	     * ERR_158 - altuser lookup error
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

	    if (parse_pwline(cur->remainder, value, 
			     &altuser_uid, &altuser_gid, &altuser_home) == 0) {
		DEBUG5(DBG_DRIVER_LO,
		       "director %s: matched %s, uid=%d, gid=%d, home=%s\n", 
		       dp->name, cur->remainder, altuser_uid, altuser_gid, altuser_home);

		/* attach the transport */
		cur->transport = find_transport(priv->transport);
		if (cur->transport == NULL) {
		    /*
		     * ERR_122 - user transport not specified
		     *
		     * DESCRIPTION
		     *      No transport attribute was specified for a user
		     *      director.  The attribute is required.
		     *
		     * ACTIONS
		     *      Defer the message with a configuration error.
		     *
		     * RESOLUTION
		     *      The director file should be edited to specify the
		     *      correct transport for local delivery.
		     */
		    cur->error = note_error(ERR_CONFERR|ERR_122,
					    xprintf("director %s: transport not defined",
						    dp->name));
		    cur->succ = *defer;
		    *defer = cur;
		    continue;
		}

		cur->flags |= ADDR_ISUSER;
		cur->flags &= ~ADDR_NOTUSER;
		cur->uid = altuser_uid;
		cur->gid = altuser_gid;
		cur->home = altuser_home;
		cur->succ = *out;
		cur->director = dp;		/* set the director which matched */
		cur->next_addr = COPY_STRING(cur->remainder);
		*out = cur;
	    } else {
		/*
		 * ERR_185 - altuser passwd file is unparsable
		 *
		 * DESCRIPTION
		 *      We got something back, but it cannot be parsed.
		 *      Either there are not enough fields, or the uid was zero.
		 *
		 * ACTIONS
		 *      Defer the address with a configuration error.
		 *
		 * RESOLUTION
		 *      The postmaster will need to look into
		 *      why the database format is duff.
		 */
		DEBUG2(DBG_DRIVER_LO, "director %s: passwd parse failure on %s\n",
		       dp->name, cur->remainder);
		error = xprintf("director %s: passwd parse failure on %s",
				dp->name, cur->remainder);
		cur->error =
		    note_error(ERR_CONFERR|ERR_185,
			       error);
		cur->succ = *defer;
		*defer = cur;
	    }
	    break;
	}
    }

    if (dp->flags&ALTUSER_REOPEN) {
	close_database(priv->database);
	priv->database = NULL;
    }
    return pass;			/* return addrs for next director */
}

/*
 * dtv_altuser - verify using altuser database
 */
/*ARGSUSED*/
void
dtv_altuser(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next addr to process */
    struct altuser_private *priv;	/* private data area */
    char *error;			/* error message */

    if (!(priv = (struct altuser_private *)dp->private))
	priv = &altuser_template;

    DEBUG(DBG_DRIVER_HI, "dtv_altuser called\n");

    /* open the database, if it is not already open */
    if (priv->database == NULL) {
	int ret;

	if (dp->flags & ALTUSER_OPENFAIL) {
	    ret = FILE_FAIL;
	} else if (dp->flags & ALTUSER_OPENAGAIN) {
	    ret = FILE_AGAIN;
	} else {
	    if (priv->error_text) {
		xfree(priv->error_text);
	    }
	    ret = open_database(priv->pwfile, priv->proto,
				priv->retries, priv->interval,
				(struct stat *)NULL, &priv->database,
				&error);
	}
	if (ret != FILE_SUCCEED) {
	    struct error *err;

	    if (ret == FILE_FAIL) {	
		dp->flags |= ALTUSER_OPENFAIL;
	    }
	    if (ret == FILE_NOMATCH) {
		error = "Database not found";
		dp->flags |= ALTUSER_OPENFAIL;
	    }
	    if (ret == FILE_AGAIN) {
		dp->flags |= ALTUSER_OPENAGAIN;
	    } 

	    if (! priv->error_text) {
		priv->error_text = COPY_STRING(error);
	    }

	    /*
	     * ERR_112 - failed to open altuser database
	     *
	     * DESCRIPTION
	     *      open_database() failed to open an altuser database.  The
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
	    error = xprintf("director %s: altuser database %s: %s",
			    dp->name, priv->pwfile, error);
	    err = note_error((dp->flags & ALTUSER_TRYAGAIN) || ret == DB_AGAIN?
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
	     * ERR_158 - altuser lookup error
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

    if (dp->flags&ALTUSER_REOPEN) {
	close_database(priv->database);
	priv->database = NULL;
    }
}

/*
 * dtb_altuser - read the configuration file attributes
 */
char *
dtb_altuser(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    struct altuser_private *priv;	/* new altuser_private structure */

    /* copy the template private data */
    priv = (struct altuser_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&altuser_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    altuser_attributes,
			    end_altuser_attributes);

    if (error) {
	return error;
    } else {
	if (priv->transport == NULL) {
	    return "transport attribute required for altuser driver";
	}
	if (find_transport(priv->transport) == NULL) {
	    return xprintf("unknown transport: %s", priv->transport);
	}
	return NULL;
    }
}


/*
 * dtp_altuser - dump altuser config
 */
void
dtp_altuser(f, dp)
     FILE * f;
     struct director *dp;
{
    (void) dump_standard_config(f,
				(dp->private) ? dp->private : (char *)&altuser_template,
				dp->name,
				dp->flags,
				altuser_attributes,
				end_altuser_attributes);
}

/*
 * altuserfile_secure - determine if an altuser file is secure
 *
 * return TRUE if an altuser is secure, given the stat structure
 * and the constraints in the private structure.
 */
static int
altuserfile_secure(priv, statp)
    struct altuser_private *priv;	/* source of constraints */
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


/*
 * parse_pwline
 *	parse a value returned back from the db lookup as a
 *	normal passwd line.
 *	We only need uid, gid & home, so ignore rest.
 *	Depending on how the db was built the key (username)
 *	may be repeated in the value - this is checked by the code
 *
 *	Returns 0 if OK, -1 otherwise.
 */

static int
parse_pwline(key, value, uid, gid, home)
     char * key;
     char * value;
     int * uid;
     int * gid;
     char * * home;
{
    char * tmpptr;

    if ((tmpptr = strcolon(value)) == NULL)	/* should be username or passwd */
	return(-1);
    if (EQ(tmpptr, key)) {
	/* Was username as first field of value - drop and get passwd */
	if ((tmpptr = strcolon(NULL)) == NULL)	/* should be passwd */
	    return(-1);
    }

    if ((tmpptr = strcolon(NULL)) == NULL)	/* should be uid */
	return(-1);
    *uid = atoi(tmpptr);
    if (*uid == 0) {
	return(-1);
    }

    if ((tmpptr = strcolon(NULL)) == NULL)	/* should be gid */
	return(-1);
    *gid = atoi(tmpptr);

    if ((tmpptr = strcolon(NULL)) == NULL)	/* should be gcos */
	return(-1);

    if ((tmpptr = strcolon(NULL)) == NULL)	/* should be home */
	return(-1);

    *home = COPY_STRING(tmpptr);
    return (0);
}
