/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:include.c,v 1.11 1996/02/26 18:32:42 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * include.c:
 *	direct mail for addresses which are mailing lists.  In the
 *	future there will be two sets of entry points, one set to
 *	match include files generated from aliases and another to
 *	match include files generated from forward files.  Also in
 *	the future security options will be added.  For now, just
 *	ensure that an include file address comes from an alias file
 *	or forward file.
 *
 * Specifications for the include directing driver:
 *
 *	private attribute data:
 *
 *	    modemask (number):  specifies bits that are not allowed
 *		to be set.  If some of these bits are set, the
 *		ALIAS_CAUTION flag is set for the resultant
 *		addr structures.
 *
 *	    owners (string):  list of possible owners for the file.
 *		For files owned by others, the ADDR_CAUTION bit is
 *		set in the resultant addr structures.
 *
 *	    owngroups (string):  like the `owners' attribute except
 *		that it applies to groups.
 *
 *	    matchdirector (string):  names the director that this
 *		instance of the include driver is a match for.
 *
 *	private attribute flags:
 *	    copyowners - get owners and owngroups from the director
 *		responsible for an addr.
 *	    copysecure - copy modemask from the director responsible
 *		for an addr.
 */
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include "defs.h"
#ifdef UNIX_BSD
# include <sys/file.h>
#endif
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../field.h"
#include "../log.h"
#include "../direct.h"
#include "../exitcodes.h"
#include "../dys.h"
#include "fwdfile.h"
#include "aliasfile.h"
#include "include.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static void include_internal();
static int include_secure();
static void forwardfile_call_include();
static void aliasfile_call_include();

static struct attr_table include_attributes[] = {
    { "modemask", t_mode, NULL, NULL, OFFSET(include_private, modemask) },
    { "owners", t_string, NULL, NULL, OFFSET(include_private, owners) },
    { "owngroups", t_string, NULL, NULL,
	  OFFSET(include_private, owngroups) },
    { "matchdirector", t_string, NULL, NULL,
	  OFFSET(include_private, matchdirector) },
    { "retries", t_int, NULL, NULL, OFFSET(include_private, retries) },
    { "interval", t_int, NULL, NULL, OFFSET(include_private, interval) },
    { "copysecure", t_boolean, NULL, NULL, COPY_SECURE },
    { "copyowners", t_boolean, NULL, NULL, COPY_OWNERS },
};
static struct attr_table *end_include_attributes = ENDTABLE(include_attributes);

/*
 * the following private data is used if the director.private field
 * is NULL.  Since the COPY_SECURE flag overrides the modemask field
 * and COPY_OWNERS overrides the owner and owngroup fields, this is
 * primarily useful as a source for the matchdirector, retries and
 * interval fields.
 */
static struct include_private default_include_priv = {
    0000,				/* no restrictions on mode */
    NULL,				/* no ownership checks */
    NULL,				/* no group ownership checks */
    NULL,				/* don't match a particular director */
    2,					/* two retries */
    2,					/* at two second intervals */
};

/*
 * include_internal - low level routine for the mailing list drivers
 */
/*ARGSUSED*/
static void
include_internal(dp, in, new, modemask, owners, owngroups, other_owner,
		 defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* one input local-form addr */
    struct addr **new;			/* output new addrs to resolve */
    int modemask;			/* copied mode mask */
    char *owners;			/* list of possible owners */
    char *owngroups;			/* list of possible groups */
    char *other_owner;			/* one additional owner */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct include_private *priv;
    int flags;				/* flags for new addr structures */
    FILE *f;				/* open list file */
    struct stat statbuf;		/* need stat to get size of file */
    int ct;				/* returned count from read */
    char *buf;				/* buffer for reading file */
    struct addr *list;			/* list from process_field */
    struct addr *list_next;		/* next element of list */
    char *fn;				/* name of file to open */
    char *error;			/* error messages */

    if (!(priv = (struct include_private *)dp->private))
	priv = &default_include_priv;

    DEBUG(DBG_DRIVER_HI, "dtd_include called\n");

    in->director = dp;			/* matched if we reached this point */

    /* mode mask from private structure, if `copy_secure' attribute is off */
    if ( !(dp->flags & COPY_SECURE) ) {
	modemask = priv->modemask;
    }

    /* owners come from private structure, if `copy_owners' attribute is off */
    if ( !(dp->flags & COPY_OWNERS) ) {
	owners = priv->owners;
	owngroups = priv->owngroups;
	other_owner = NULL;
    }

    /*
     * Try to open the file
     */
    fn = in->remainder + sizeof(":include:") - 1;
    while (isspace(*fn))
	fn++;
    fn = expand_string(fn, in->parent, (char *)NULL, (char *)NULL);
    if (fn == NULL) {
	/*
	 * ERR_116 - include name expansion failed
	 *
	 * DESCRIPTION
	 *      The filename part of a :include:filename address failed to
	 *      be expanded by expand_string().
	 *
	 * ACTIONS
	 *      Fail the address and send an error to the address owner or
	 *      to the postmaster.  Report the parent address which produced
	 *      the :include:filename address in the error message.
	 *
	 * RESOLUTION
	 *      The postmaster or address owner should correct the address.
	 */
	in->error = note_error(ERR_NPOWNER|ERR_116,
			       xprintf(
		      "director %s: include name expansion failed (parent %s)",
				       dp->name, in->parent->in_addr));
	in->succ = *fail;
	*fail = in;
	return;
    }
    fn = COPY_STRING(fn);

    DEBUG2(DBG_DRIVER_MID, "director %s: opened file %s\n",
	   dp->name, fn);
    if (!(f = fopen_as_user(fn, "r", in->uid != BOGUS_USER ? in->uid : nobody_uid,
			    in->gid != BOGUS_GROUP ? in->gid : nobody_gid))) {
	int left = priv->retries;

	while (left -- > 0) {
	    (void) sleep(priv->interval);
	    if (!(f = fopen_as_user(fn, "r", in->uid != BOGUS_USER ? in->uid : nobody_uid,
				    in->gid != BOGUS_GROUP ? in->gid : nobody_gid)))
		break;
	}
    }

    if (f == NULL) {
	/*
	 * ERR_117 - include file open failed
	 *
	 * DESCRIPTION
	 *      The open failed on an :include:filename address.  Errno
	 *      should detail the actual error.
	 *
	 * ACTIONS
	 *      The address is failed and an error is sent to the address
	 *      owner or to the postmaster.
	 *
	 * RESOLUTION
	 *      The address owner or postmaster should ensure that the
	 *      address is correct or that the file exists and is readable
	 *      from the mailer.
	 */
	in->error = note_error(ERR_NPOWNER|ERR_117,
			       xprintf(
		       "director %s: include file open failed (parent %s): %s",
				       dp->name,
				       in->parent->in_addr,
				       strerror(errno)));
	in->succ = *fail;
	*fail = in;
	return;
    }

#ifdef lock_fd_rd_wait
    lock_fd_rd_wait(fileno(f));
#endif
    (void) fstat(fileno(f), &statbuf);
    if (in->parent->flags & ADDR_CAUTION ||
	! include_secure(dp, &statbuf, modemask,
			 owners, owngroups, other_owner))
    {
	flags = ADDR_CAUTION|ADDR_LISTTYPE;
    } else {
	flags = ADDR_LISTTYPE;
    }

    buf = xmalloc((unsigned)statbuf.st_size + 1);
    ct = read(fileno(f), buf, (size_t) statbuf.st_size);
    (void) fclose(f);
    if (ct < 0) {
	/*
	 * ERR_118 - include file read failed
	 *
	 * DESCRIPTION
	 *      The read failed on an :include:filename address.  Errno
	 *      should detail the actual error.
	 *
	 * ACTIONS
	 *      The address is failed and an error is sent to the address
	 *      owner or to the postmaster.
	 *
	 * RESOLUTION
	 *      This generally means filesystem problems and the error
	 *      should be brought up with the site administrator.
	 */
	in->error = note_error(ERR_NPOWNER|ERR_118,
			       xprintf(
		       "director %s: include file read failed (parent %s): %s",
				       dp->name,
				       in->parent->in_addr,
				       strerror(errno)));
	in->succ = *fail;
	*fail = in;
	return;
    }
    buf[ct] = '\0';		/* terminate the region */
    list = NULL;
    error = NULL;
    (void) process_field((char *)NULL, buf, (char *)NULL,
			 (char *)NULL, &list, F_ALIAS, &error);
    if (error) {
	/*
	 * ERR_119 - include file parse error
	 *
	 * DESCRIPTION
	 *      process_field() encountered an error in parsing the contents
	 *      of the file specified by a :include:filename address.  The
	 *      actual parsing error was returned in `error'.
	 *
	 * ACTIONS
	 *      The address is failed and an error is sent to the address
	 *      owner or to the postmaster.
	 *
	 * RESOLUTION
	 *      The file should be changed to take care of the parsing error.
	 */
	in->error = note_error(ERR_NPOWNER|ERR_119,
			       xprintf(
		       "director %s: include file parse error (parent %s): %s",
				       dp->name,
				       in->parent->in_addr,
				       error));
	in->succ = *fail;
	*fail = in;
	return;
    }
    for (; list; list = list_next) {
	list_next = list->succ;

	list->parent = in;
	list->director = dp;
	list->flags = flags;
	list->home = in->home;
	list->uid = in->uid;
	list->gid = in->gid;
	list->succ = *new;
	*new = list;
    }
    DEBUG2(DBG_DRIVER_LO, "director %s matched <%s>\n",
	   dp->name, in->remainder);
    xfree(fn);
}

/*
 * include_secure - determine if a mailing list file is secure
 *
 * return TRUE if secure, otherwise false.
 */
/*ARGSUSED*/
static int
include_secure(dp, statp, modemask, owners, owngroups, other_owner)
    struct director *dp;		/* director */
    struct stat *statp;			/* stat of mailing list file */
    int modemask;			/* disallowed mode bits, ala umask */
    char *owners;			/* possible owners */
    char *owngroups;			/* possible groups */
    char *other_owner;			/* one more possible owner */
{
    int other_uid;			/* uid for other_owner */

    /* check the mode bits */
    if (modemask & statp->st_mode) {
	return FALSE;			/* not secure */
    }

    if (other_owner) {
	struct passwd *pw;		/* passwd entry for other_owner */

	pw = getpwbyname(other_owner);
	if (pw) {
	    other_uid = pw->pw_uid;
	} else {
	    other_owner = NULL;
	}
    }

    if (owners) {
	if (other_owner && other_uid != statp->st_uid) {
	    /* checkowner fails and owners attribute exists */
	    char *temp = owners;
	    int found = FALSE;

	    /* check against passwd file uid */
	    for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
		struct passwd *pw = getpwbyname(temp);

		if (pw == NULL) {
		    write_log(LOG_SYS, "include_secure: no such owner %s\n",
			      temp);
		} else {
		    if (statp->st_uid == pw->pw_uid) {
			found = TRUE;
			break;
		    }
		}
	    }

	    if (!found) {
		return FALSE;		/* not secure */
	    }
	}
    } else if (other_owner) {
	/* no owners attribute, so checkowner attribute is the final word */
	if (other_uid != statp->st_uid) {
	    return FALSE;		/* not secure */
	}
    }

    if (owngroups) {
	/* verify owning group */
	char *temp = owngroups;
	int found = FALSE;

	/* check against passwd file uid */
	for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
	    struct group *gr = getgrbyname(temp);

	    if (gr == NULL) {
		write_log(LOG_SYS, "include_secure: no such owngroup %s\n",
			  temp);
	    } else {
		if (statp->st_gid == gr->gr_gid) {
		    found = TRUE;
		    break;
		}
	    }
	}

	if (!found) {
	    return FALSE;		/* not secure */
	}
    }

    return TRUE;
}

/*
 * dtb_internal_include - read the configuration file attributes
 */
static char *
dtb_internal_include(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct include_private include_template = {
	002,				/* modemask */
	NULL,				/* owners */
	NULL,				/* owngroups */
	NULL,				/* matchdirector */
#ifdef	HAVE_RENAME
	0,				/* retries */
#else	/* not HAVE_RENAME */
	1,				/* retries */
#endif	/* not HAVE_RENAME */
	2,				/* interval */
    };
    struct include_private *priv;	/* new include_private structure */

    /* copy the template private data */
    priv = (struct include_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&include_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    include_attributes,
			    end_include_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * dtp_internal_include - dump include config
 */
static void
dtp_internal_include(f, dp)
     FILE * f;
     struct director *dp;
{
    (void) dump_standard_config(f,
				(dp->private) ? dp->private : (char *)&default_include_priv,
				dp->name,
				dp->flags,
				include_attributes,
				end_include_attributes);
}



/*
 * dtd_forwardinclude - mailing list driver complementing forwardfile driver
 */
/*ARGSUSED*/
struct addr *
dtd_forwardinclude(dp, in, out, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **new;			/* output new addrs to resolve */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct include_private *priv;	/* private storage */
    struct addr *pass = NULL;		/* addr structures to return */
    struct addr *cur;			/* addr being processed */
    struct addr *next;			/* next addr to process */
    int put_back = TRUE;		/* FALSE to put addr on pass list */
    struct addr *temp;			/* temp for scanning parent addrs */

    if (!(priv = (struct include_private *)dp->private))
	priv = &default_include_priv;

    for (cur = in; cur; cur = next) {
	next = cur->succ;
	if (strncmpic(cur->remainder, ":include:", sizeof(":include:")-1)) {
	    /* not equal to :include:* */
	    cur->succ = pass;
	    pass = cur;
	    continue;
	}

	/* scan backward for something that was not a mailing list */
	for (temp = cur; temp; temp = temp->parent) {
	    if ( !(temp->flags & ADDR_LISTTYPE) ) {
		if (temp->director &&
		    EQ(temp->director->driver, "forwardfile"))
		{
		    if (priv &&
			priv->matchdirector &&
			!EQ(priv->matchdirector, temp->director->name))
		    {
			continue;
		    }
		    forwardfile_call_include(dp, cur, temp, new, defer, fail);
		    put_back = FALSE;
		    break;
		}
	    }
	}

	/* we did not match the address */
	if (put_back) {
	    cur->succ = pass;
	    pass = cur;
	}
    }

    return pass;
}

/*ARGSUSED*/
void
dtv_forwardinclude(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    /*
     * mailing list form addresses must have a parent.  Since verifies
     * are only one level deep, mailing never match for verifies.
     */
    insert_addr_list(in, retry, (struct error *)NULL);
}

char *
dtb_forwardinclude(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    return dtb_internal_include(dp, attrs);
}

void
dtp_forwardinclude(f, dp)
     FILE * f;				/* file to dump to */
    struct director *dp;		/* director entry being defined */
{
    dtp_internal_include(f, dp);
}

static void
forwardfile_call_include(dp, in, fwdaddr, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* addr to process */
    struct addr *fwdaddr;		/* addr from forwadfile driver */
    struct addr **new;			/* new addr list */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct forwardfile_private *fpriv;
    char *other_owner = NULL;

    fpriv = (struct forwardfile_private *)fwdaddr->director->private;
    if (fwdaddr->director->flags & FWD_CHECKOWNER) {
	/* user is in the addr stucture that resolved to a forward file */
	other_owner = fwdaddr->parent->remainder;
    }

    include_internal(dp, in, new, fpriv->modemask, fpriv->owners,
		     fpriv->owngroups, other_owner, defer, fail);
}

/*
 * dtd_aliasinclude - mailing list driver complementing aliasfile driver
 */
/*ARGSUSED*/
struct addr *
dtd_aliasinclude(dp, in, out, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **new;			/* output new addrs to resolve */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct include_private *priv;	/* private storage */
    struct addr *pass = NULL;		/* addr structures to return */
    struct addr *cur;			/* addr being processed */
    struct addr *next;			/* next addr to process */

    priv = (struct include_private *)dp->private;

    for (cur = in; cur; cur = next) {
	next = cur->succ;
	if (strncmpic(cur->remainder, ":include:", sizeof(":include:")-1)) {
	    /* not equal to :include:* */
	    cur->succ = pass;
	    pass = cur;
	} else {
	    int put_back = TRUE;
	    struct addr *temp;

	    for (temp = cur; temp; temp = temp->parent) {
		if ( !(temp->flags & ADDR_LISTTYPE) ) {
		    if (temp->director &&
			EQ(temp->director->driver, "aliasfile"))
		    {
			if (priv &&
			    priv->matchdirector &&
			    !EQ(priv->matchdirector, temp->director->name))
			{
			    continue;
			}
			aliasfile_call_include(dp, cur, temp, new,
					       defer, fail);
			put_back = FALSE;
			break;
		    }
		}
	    }

	    if (put_back) {
		cur->succ = pass;
		pass = cur;
	    }
	}
    }

    return pass;
}

/*ARGSUSED*/
void
dtv_aliasinclude(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    /*
     * mailing list form addresses must have a parent.  Since verifies
     * are only one level deep, mailing never match for verifies.
     */
    insert_addr_list(in, retry, (struct error *)NULL);
}

char *
dtb_aliasinclude(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    return dtb_internal_include(dp, attrs);
}

void
dtp_aliasinclude(f, dp)
     FILE * f;				/* file to dump to */
    struct director *dp;		/* director entry being defined */
{
    dtp_internal_include(f, dp);
}

static void
aliasfile_call_include(dp, in, fwdaddr, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* addr to process */
    struct addr *fwdaddr;		/* addr from forwadfile driver */
    struct addr **new;			/* new addr list */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct aliasfile_private *apriv;

    apriv = (struct aliasfile_private *)fwdaddr->director->private;

    include_internal(dp, in, new, apriv->modemask, apriv->owners,
		     apriv->owngroups, (char *)NULL, defer, fail);
}

/*
 * dtd_genericinclude - generic mailing list driver
 *
 * the generic mailing list driver catches any mailing list addresses.
 * it is useful for catching mailing list addresses from directors
 * besides aliasfile and forwardfile-type directors, if any other
 * directors are considered secure address sources.
 */
/*ARGSUSED*/
struct addr *
dtd_genericinclude(dp, in, out, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **new;			/* output new addrs to resolve */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct include_private *priv;	/* private storage */
    struct addr *pass = NULL;		/* addr structures to return */
    struct addr *cur;			/* addr being processed */
    struct addr *next;			/* next addr to process */

    priv = (struct include_private *)dp->private;

    for (cur = in; cur; cur = next) {
	next = cur->succ;
	if (strncmpic(cur->remainder, ":include:", sizeof(":include:")-1)) {
	    /* not equal to :include:* */
	    cur->succ = pass;
	    pass = cur;
	} else {
	    /* a mailing list */
	    if (priv->matchdirector &&
		cur->parent &&
		cur->parent->director &&
		!EQ(priv->matchdirector, cur->parent->director->name))
	    {
		continue;
	    }
	    include_internal(dp, cur, new, 0, (char *)NULL,
			     (char *)NULL, (char *)NULL, defer, fail);
	}
    }

    return pass;
}

/*ARGSUSED*/
void
dtv_genericinclude(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    /*
     * mailing list form addresses must have a parent.  Since verifies
     * are only one level deep, mailing never match for verifies.
     */
    insert_addr_list(in, retry, (struct error *)NULL);
}

char *
dtb_genericinclude(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    return dtb_internal_include(dp, attrs);
}

void
dtp_genericinclude(f, dp)
     FILE * f;				/* file to dump to */
    struct director *dp;		/* director entry being defined */
{
    dtp_internal_include(f, dp);
}

