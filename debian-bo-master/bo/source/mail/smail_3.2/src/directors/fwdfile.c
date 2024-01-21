/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:fwdfile.c,v 1.21 1996/03/19 14:52:06 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * forward.c:
 *	direct mail using forward files.  In the future, the name of
 *	the file involved will be configurable.  Now, for each addr
 *	structure representing a local user, an attempt is made to
 *	open a .forward file in that user's home directory.  If the
 *	attempt succeeds then the contents are read in and transformed
 *	by process_field into a list of addr structures.
 *
 * Specifications for the forwardfile directing driver:
 *
 *	private attribute data:
 *	    file (string): specifies the template for the name of the
 *		forward file.  expand_string will be used to build the
 *		actual name.  If this does not begin with `/', it will
 *		be referenced relative to the smail_lib_dir directory.
 *
 *	    modemask (number):  specifies bits that are not allowed
 *		to be set.  If some of these bits are set, the
 *		ALIAS_SECURE flag is not set for the resultant
 *		addr structures.
 *
 *	    caution (string):  colon-separated list of users and
 *		directories considered not secure.  Directory names
 *		can begin with ~user.  Secure addresses will not have
 *		the ADDR_CAUTION bit set in the addr structure.
 *
 *	    unsecure (string):  colon-separated list of users and
 *		directories considered very unsecure.  Unsecure
 *		addresses will have the ADDR_UNSECURE bit set
 *		in the addr structure.
 *
 *	    owners (string):  list of possible owners for the file.
 *		For files owned by others, the ADDR_CAUTION bit is
 *		set in the resultant addr structures.
 *
 *	    owngroups (string):  like the `owners' attribute except
 *		that it applies to groups.
 *
 *	    prefix (string):  a string which must appear at the
 *		beginning of each address to be matched by this
 *		director.  The prefix is not included in any
 *		expansions of the $user variable.  The prefix is also
 *		compared independently of case.
 *
 *	    suffix (string):  a string which must appear at the end of
 *		each address to be matched by this director.  The
 *		suffix is not included in any expansions of the $user
 *		variable.  The suffix is also compared independently
 *		of case.
 *
 *	private attribute flags:
 *	    checkowner:  if set, the user associated with the forward
 *		file (remainder from the addr structure) is allowed
 *		as a possible owner for the file.
 *
 *	    forwardto:  if set, this is actually a Forward-to driver.
 *		In this case, the forward file is used only if the
 *		first line begins with "Forward to " and only the
 *		first line is actually used.
 *
 *	NOTE:  if none of the attributes `owners', `owngroups' or
 *	       `checkowner' is set, no checks are made for ownership
 *	       restrictions.
 */
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
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
#include "../dys.h"
#include "../exitcodes.h"
#include "../main.h"
#include "fwdfile.h"
#ifndef DEPEND
# include "../extern.h"
# include "../error.h"
# include "../debug.h"
#endif

long atol();

static int finish_forward();
static int forward_secure();
static int secure_match();

static struct attr_table forwardfile_attributes[] = {
    { "file", t_string, NULL, NULL, OFFSET(forwardfile_private, file) },
    { "modemask", t_mode, NULL, NULL,
	  OFFSET(forwardfile_private, modemask) },
    { "caution", t_string, NULL, NULL,
	  OFFSET(forwardfile_private, caution) },
    { "unsecure", t_string, NULL, NULL,
	  OFFSET(forwardfile_private, unsecure) },
    { "owners", t_string, NULL, NULL,
	  OFFSET(forwardfile_private, owners) },
    { "owngroups", t_string, NULL, NULL,
	  OFFSET(forwardfile_private, owngroups) },
    { "prefix", t_string, NULL, NULL,
	  OFFSET(forwardfile_private, prefix) },
    { "suffix", t_string, NULL, NULL,
	  OFFSET(forwardfile_private, suffix) },
    { "checkowner", t_boolean, NULL, NULL, FWD_CHECKOWNER },
    { "forwardto", t_boolean, NULL, NULL, FWD_FORWARDTO },
    { "lock", t_boolean, NULL, NULL, FWD_LOCKFWDFILE },
};
static struct attr_table *end_forwardfile_attributes =
    ENDTABLE(forwardfile_attributes);
static struct forwardfile_private forwardfile_template = {
    NULL,				/* file */
    000,				/* modemask */
    NULL,				/* caution */
    NULL,				/* unsecure */
    NULL,				/* owners */
    NULL,				/* owngroups */
};


/*
 * dtd_forwardfile - direct using per-user forward files
 */
/*ARGSUSED*/
struct addr *
dtd_forwardfile(dp, in, out, new, defer, fail)
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

    DEBUG(DBG_DRIVER_HI, "dtd_forwardfile called\n");

    /* loop over all of the input addrs */
    for (cur = in; cur; cur = next) {
	next = cur->succ;
	if (cur->remainder[0] == '.' || index(cur->remainder, '/')) {
	    /* do not match hidden files or files in subdirectories */
	    cur->succ = pass;
	    pass = cur;
	    continue;
	}
	/* fill in any user information */
	director_user_info(cur);

	/* finish up */
	if (finish_forward(dp, cur, new, defer, fail) == FAIL) {
	    /* if there was no forward file, pass it to the next director */
	    DEBUG2(DBG_DRIVER_MID, "director %s did not match name %s\n",
		   dp->name, cur->remainder);
	    cur->succ = pass;
	    pass = cur;
	}
    }

    return pass;			/* return addrs for next director */
}

/*
 * dtv_forwardfile - verify using forward file
 */
/*ARGSUSED*/
void
dtv_forwardfile(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next value for cur */
    register struct forwardfile_private *priv;

    if (!(priv = (struct forwardfile_private *)dp->private))
	priv = &forwardfile_template;

    DEBUG(DBG_DRIVER_HI, "dtv_forwardfile called\n");

    /* loop over all of the input addrs */
    for (cur = in; cur; cur = next) {
	char *fn;			/* forward filename */
	struct stat statbuf;		/* stat of forward file */
	char *remainder = cur->remainder;
	int c_save;			/* saved start of suffix */
	char *suffixp = NULL;		/* start of suffix in remainder */

	next = cur->succ;
	if (cur->remainder[0] == '.' || index(cur->remainder, '/')) {
	    /* do not match hidden files or files in subdirectories */
	    cur->succ = *retry;
	    *retry = cur;
	    continue;
	}

	/* fill in any user information */
	director_user_info(cur);

	/* remove prefix and suffix from the user name */
	if (priv->prefix) {
	    int len = strlen(priv->prefix);

	    if (strncmpic(priv->prefix, remainder, len) == 0) {
		remainder += len;
	    } else {
		cur->succ = *retry;
		*retry = cur;
		continue;
	    }
	}
	if (priv->suffix) {
	    int len = strlen(priv->suffix);
	    int rem_len = strlen(remainder);

	    if (rem_len > len &&
		EQIC(remainder + rem_len - len, priv->suffix))
	    {
		suffixp = remainder + rem_len - len;
		c_save = *suffixp;
		*suffixp = '\0';
	    } else {
		cur->succ = *retry;
		*retry = cur;
		continue;
	    }
	}
	/* compute the filename */
	fn = expand_string(priv->file, (struct addr *)NULL,
			   cur->flags&ADDR_ISUSER? cur->home: (char *)NULL,
			   remainder);
	if (suffixp) {
	    *suffixp = c_save;
	}
	if (fn == NULL) {
	    DEBUG2(DBG_DRIVER_MID,
		   "dtv_forwardfile: director %s failed to expand %s\n",
		   dp->name, priv->file);
	    cur->succ = *retry;
	    *retry = cur;
	    continue;
	}

	if (fn[0] == '/') {
	    fn = COPY_STRING(fn);
	} else {
	    fn = make_lib_fn(fn);
	}
	if (fn == NULL) {
	    /*
	     * ERR_114 - forward file not an absolute path, and no
	     *	     smail_lib_dir defined
	     *
	     * DESCRIPTION
	     *      A path from / (root) was not returned by expand_string()
	     *      for the file attribute, and no smail_lib_dir is defined.
	     *      Since the current directory for smail is not generally
	     *      useful, this can be considered a configuration error.
	     *
	     * ACTIONS
	     *      Defer the address as a configuration error.
	     *
	     * RESOLUTION
	     *      The postmaster should check the director to ensure that the
	     *      forward-file director will always either fail to expand
	     *      (if a $-variable is not defined in some context) or produce
	     *      an absolute pathname, or that the smail_lib_dir be defined.
	     */
	    cur->error =
		note_error(ERR_CONFERR|ERR_114,
			   xprintf("director %s: %s not an absolute path",
				   dp->name, priv->file));
	    cur->succ = *defer;
	    *defer = cur;
	    continue;
	}

	/*
	 * make sure that the file exists and is a regular non-zero
	 * length file.
	 */
	if (stat(fn, &statbuf) < 0 ||
	    (statbuf.st_mode&S_IFMT) != S_IFREG)
	{
	    DEBUG2(DBG_DRIVER_MID,
		   "dtv_forwardfile: director %s does not match %s\n",
		   dp->name, cur->remainder);
	    xfree(fn);
	    cur->succ = *retry;
	    *retry = cur;
	    continue;
	}
	if (statbuf.st_size == 0) {
	    DEBUG2(DBG_DRIVER_MID,
		   "dtv_forwardfile: director %s: file for %s has zero length, ignored\n",
		   dp->name, cur->remainder);
	    xfree(fn);
	    cur->succ = *retry;
	    *retry = cur;
	    continue;
	}

	xfree(fn);

	/* address was matched */
	cur->succ = *okay;
	*okay = cur;
    }
}

/*
 * dtb_forwardfile - read the configuration file attributes
 */
char *
dtb_forwardfile(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    struct forwardfile_private *priv;	/* new forwardfile_private structure */

    /* copy the template private data */
    priv = (struct forwardfile_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&forwardfile_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    forwardfile_attributes,
			    end_forwardfile_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * dtp_forwardfile - dump forwardfile config
 */
void
dtp_forwardfile(f, dp)
     FILE * f;
     struct director *dp;
{
    (void) dump_standard_config(f,
				(dp->private) ? dp->private : (char *)&forwardfile_template,
				dp->name,
				dp->flags,
				forwardfile_attributes,
				end_forwardfile_attributes);
}



/*
 * finish_forward - do the real work of finding and processing forward file
 *
 * return SUCCEED, if a forward file was found and processed,
 * FAIL if it was not found or processed.  If SUCCEED, `new' will
 * be properly filled in.
 */
static int
finish_forward(dp, addr, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *addr;			/* specific addr structure */
    struct addr **new;			/* output new addrs to resolve */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    register struct forwardfile_private *priv;
    char *fn;				/* name of forward file */
    FILE *fp;				/* open forward file */
    struct stat statbuf;		/* buf for fstat call */
    char *buf;				/* buffer for file contents */
    struct addr *cur;			/* addrs from process_field */
    struct addr *next;			/* next addr from process_field */
    int flags = ADDR_FWDTYPE;		/* addr structure flags value */
    char *error;			/* error message */
    struct str fwdtobuf;
    char *remainder = addr->remainder;
    int c_save;				/* saved start of suffix */
    char *suffixp = NULL;		/* start of suffix in remainder */
    char *owner = NULL;                 /* owner address */
    char *preparsed_owner = NULL;       /* preparsed version */

    if (!(priv = (struct forwardfile_private *)dp->private))
	priv = &forwardfile_template;

    /* remove prefix and suffix from the user name */
    if (priv->prefix) {
	int len = strlen(priv->prefix);

	if (strncmpic(priv->prefix, remainder, len) == 0) {
	    remainder += len;
	} else {
	    return FAIL;
	}
    }
    if (priv->suffix) {
	int len = strlen(priv->suffix);
	int rem_len = strlen(remainder);

	if (rem_len > len &&
	    EQIC(remainder + rem_len - len, priv->suffix))
	{
	    suffixp = remainder + rem_len - len;
	    c_save = *suffixp;
	    *suffixp = '\0';
	} else {
	    return FAIL;
	}
    }

    /* compute the file name */
    fn = expand_string(priv->file, (struct addr *)NULL,
		       addr->flags&ADDR_ISUSER? addr->home: (char *)NULL,
		       remainder);
    if (suffixp) {
	*suffixp = c_save;
    }
    if (fn == NULL) {
	DEBUG2(DBG_DRIVER_MID,
	       "dtd_forwardfile: director %s failed to expand %s\n",
	       dp->name, priv->file);
	return FAIL;
    }
    if (fn[0] == '/') {
	fn = COPY_STRING(fn);
    } else {
	fn = make_lib_fn(fn);
    }

    if (fn == NULL) {
	/*
	 * ERR_114 - forward file not an absolute path, and no
	 *	     smail_lib_dir defined
	 *
	 * DESCRIPTION
	 *      A path from / (root) was not returned by expand_string()
	 *      for the file attribute, and no smail_lib_dir is defined.
	 *      Since the current directory for smail is not generally
	 *      useful, this can be considered a configuration error.
	 *
	 * ACTIONS
	 *      Defer the address as a configuration error.
	 *
	 * RESOLUTION
	 *      The postmaster should check the director to ensure that the
	 *      forward-file director will always either fail to expand
	 *      (if a $-variable is not defined in some context) or produce
	 *      an absolute pathname, or that the smail_lib_dir be defined.
	 */
	addr->error =
	    note_error(ERR_CONFERR|ERR_114,
		       xprintf("director %s: %s not an absolute path",
			       dp->name, priv->file));
	addr->succ = *defer;
	*defer = addr;
	return SUCCEED;
    }

    DEBUG1(DBG_DRIVER_MID, "dtd_forwardfile:  opening forward file %s\n", fn);
    if (!(fp = fopen_as_user(fn, "r", addr->uid, addr->gid))) {
	xfree(fn);
	DEBUG1(DBG_DRIVER_HI, "dtd_forwardfile:  no forward file for %s\n",
	       addr->remainder);
	return FAIL;
    }

    (void) fstat(fileno(fp), &statbuf);

    /* ignore non-files and ignore zero-length forward files */
    if ((statbuf.st_mode & S_IFMT) != S_IFREG || statbuf.st_size == 0) {
	(void) fclose(fp);
	xfree(fn);
	return FAIL;
    }

    /* set security bits appropriately */
    flags |= forward_secure(dp, addr, &statbuf, fn);

#ifdef lock_fd_rd_wait
    if (dp->flags & FWD_LOCKFWDFILE) {
	/* Lock file to make sure no-one else is playing with it
	 * This might be superfluous since no-one believes that Unix
	 * Editors take the blindest bit of notice!
	 */
	lock_fd_rd_wait(fileno(fp));
    }
#endif

    if (dp->flags & FWD_FORWARDTO) {
	register int c;

	STR_INIT(&fwdtobuf);
	while ((c = getc(fp)) != EOF && c != '\n') {
	    STR_NEXT(&fwdtobuf, c);
	}
	if (ferror(fp)) {
	    write_log(LOG_SYS, "read failed on %s, director=%s", fn, dp->name);
	    (void) fclose(fp);
	    xfree(fn);
	    STR_FREE(&fwdtobuf);
	    return FAIL;
	}
	(void) fclose(fp);
	STR_NEXT(&fwdtobuf, '\0');
	if (strncmpic(fwdtobuf.p, "forward to ",
		      sizeof("forward to ") - 1) != 0) {
	    STR_FREE(&fwdtobuf);
	    xfree(fn);
	    return FAIL;
	}

	buf = fwdtobuf.p + sizeof("forward to ") - 1;
    } else {
	buf = xmalloc(statbuf.st_size + 1); /* leave room for trailing nul */

	if (fread(buf, (size_t) statbuf.st_size, 1, fp) < 1) {
	    write_log(LOG_SYS, "read failed on %s, director=%s", fn, dp->name);
	    (void) fclose(fp);
	    xfree(buf);
	    xfree(fn);
	    return FAIL;
	}

	(void) fclose(fp);		/* don't need the open file anymore */

	buf[statbuf.st_size] = '\0';	/* terminate the buffer */
	DEBUG1(DBG_DRIVER_HI, "read %ld bytes\n", (long) statbuf.st_size);
    }

    /* matched a forward file */
    DEBUG2(DBG_DRIVER_LO, "director %s: matched %s",
	   dp->name, addr->remainder);
    DEBUG1(DBG_DRIVER_HI, ", forwarded to %s", buf);
    DEBUG(DBG_DRIVER_LO, "\n");

    /*
     * extract addr structures from the file contents, then
     * mark them all as coming from the forward director
     */
    cur = NULL;
    error = NULL;
    (void) process_field((char *)NULL, buf, (char *)NULL,
			 (char *)NULL, &cur, F_ALIAS, &error);
    if (dp->flags & FWD_FORWARDTO) {
	STR_FREE(&fwdtobuf);
    } else {
	xfree(buf);			/* don't need buf anymore */
    }
    if (error) {
	/*
	 * ERR_115 - forward file parse error
	 *
	 * DESCRIPTION
	 *      process_field() found an error while parsing a forward file.
	 *	The specific error is stored in `error'.
	 *
	 * ACTIONS
	 *      Fail the address and send to the owner or to the postmaster.
	 *
	 * RESOLUTION
	 *      The postmaster or forward file owner should correct the
	 *      forward file.
	 */
	addr->error = note_error(ERR_NPOWNER|ERR_115,
				xprintf("director %s: error in file for %s: %s",
					dp->name, addr->in_addr, error));
	addr->succ = *fail;
	*fail = addr;
	addr->director = dp;
	return SUCCEED;
    }

    /*
     * if no addresses were found, then ignore the forward file.
     */
    if (cur == NULL) {
	DEBUG2(DBG_DRIVER_LO,
	      "director %s: file for %s has no addresses, ignored\n",
	      dp->name, addr->in_addr);
	return FAIL;
    }

    /* If the director has an owner, compute the owner address for this
    mailing list and add it to each generated address. This is for
    the benefit of outgoing SMTP, which wants to use it in the
    envelope for error returns. */

    if (!(dp->owner) ||
        !(owner = expand_string(dp->owner,
                           (struct addr *)NULL,
                           addr->home,
                           addr->remainder)) ||
        !(preparsed_owner = preparse_address(owner, &error)))
    {
        owner = NULL;

    } else {
	/* Must verify that this owner is sensible (to avoid things like
	 * xxxx-request-request).  To do this, use the pre-parsed address.
	 * However, verify may change the value of this address by removing the
	 * local domain name.  We don't want this, because it might be a
	 * non-standard local domain name for list handling.  So make a copy
	 * beforehand. */

	struct addr *new2 = alloc_addr();
	struct addr *defer2 = alloc_addr();
	struct addr *fail2_safe = alloc_addr();
	struct addr *fail2;
	struct addr *okay = NULL;

	fail2 = fail2_safe;			/* verify_addr_list can clobber last arg */
	owner = copy(preparsed_owner);		/* copy to use later */

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

    /* Set up the newly-created addresses */

    addr->director = dp;
    for (; cur; cur = next) {
	next = cur->succ;

        cur->owner = owner;
	cur->parent = addr;
	cur->director = dp;
	cur->flags = flags;
	cur->home = addr->home;
	cur->uid = addr->uid;
	cur->gid = addr->gid;
	cur->succ = *new;
	*new = cur;
    }

    DEBUG3(DBG_DRIVER_LO, "director <%s> matched <%s> owner <%s>\n",
           dp->name, addr->remainder,
           owner? owner : "none");
    xfree(fn);
    return SUCCEED;
}

/*
 * forward_secure - determine if a forward file is secure
 *
 * return ADDR_UNSECURE in the event that the `unsecure' attribute
 * affects the forward file, unset ADDR_CAUTION if other constraints
 * do not state that the file is not secure.
 */
static int
forward_secure(dp, addr, statp, fn)
    struct director *dp;		/* source director */
    struct addr *addr;			/* source addr structure */
    struct stat *statp;			/* source of file data */
    char *fn;				/* source of filename data */
{
    register struct forwardfile_private *priv;

    if (!(priv = (struct forwardfile_private *)dp->private))
	priv = &forwardfile_template;

    /* `unsecure' has priority */
    if (priv->unsecure) {
	char *temp = priv->unsecure;
	int found = FALSE;
	char *save_dir = rindex(fn, '/'); /* temp put a nul byte here */

	*save_dir = '\0';
	for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
	    if (secure_match(temp, addr->remainder, fn)) {
		found = TRUE;
		break;
	    }
	}
	*save_dir = '/';

	if (found) {
	    return ADDR_UNSECURE;
	}
    }

    /* check the mode bits */
    if (priv->modemask & statp->st_mode) {
	return ADDR_CAUTION;		/* not secure */
    }

    /* check the list of caution sources */
    if (priv->caution) {
	char *temp = priv->caution;
	int found = FALSE;
	char *save_dir = rindex(fn, '/'); /* temp put a nul byte here */

	/* pass the directory name to secure_match */
	*save_dir = '\0';
	for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
	    if (secure_match(temp, addr->remainder, fn)) {
		found = TRUE;
		break;
	    }
	}
	*save_dir = '/';

	if (found) {
	    return ADDR_CAUTION;	/* not secure */
	}
    }

    if (priv->owners) {
	if ((dp->flags & FWD_CHECKOWNER) == 0 || addr->uid != statp->st_uid) {
	    /* checkowner fails and owners attribute exists */
	    char *temp = priv->owners;
	    int found = FALSE;

	    /* check against passwd file uid */
	    for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
		struct passwd *pw = getpwbyname(temp);

		if (pw == NULL) {
		    write_log(LOG_SYS, "dtd_forwardfile: no such owner %s\n",
			      temp);
		} else {
		    if (statp->st_uid == pw->pw_uid) {
			found = TRUE;
			break;
		    }
		}
	    }

	    if (!found) {
		return ADDR_CAUTION;	/* not secure */
	    }
	}
    } else if (dp->flags & FWD_CHECKOWNER) {
	/* no owners attribute, so checkowner attribute is the final word */
	if (addr->uid != statp->st_uid) {
	    return ADDR_CAUTION;	/* not secure */
	}
    }

    if (priv->owngroups) {
	/* verify owning group */
	char *temp = priv->owngroups;
	int found = FALSE;

	/* check against passwd file uid */
	for (temp = strcolon(temp); temp; temp = strcolon((char *)NULL)) {
	    struct group *gr = getgrbyname(temp);

	    if (gr == NULL) {
		write_log(LOG_SYS, "dtd_forwardfile: no such owngroup %s\n",
			  temp);
	    } else {
		if (statp->st_gid == gr->gr_gid) {
		    found = TRUE;
		    break;
		}
	    }
	}

	if (!found) {
	    return ADDR_CAUTION;	/* not secure */
	}
    }

    return 0;				/* secure by default */
}

/*
 * secure_match - determine if a string matches a user or parent directory
 *
 * return TRUE if the given string matches the user associated with the
 * forward file, or the parent directory of the forward file.
 */
static int
secure_match(string, user, dirn)
    char *string;			/* string to be tested */
    char *user;				/* associated user name */
    char *dirn;				/* directory to verify against */
{
    char *p;
    char *delim;
    struct passwd *pw;

    delim=(char*)0;
    if (string[0] == '~' || string[0] == '/') {
	/* hmm, looks sort of like a directory name, expand it */
	char *dir = expand_string(string, (struct addr *)NULL,
				  (char *)NULL, (char *)NULL);

	if (dir == NULL) {
	    /* didn't expand, ignore */
	    return FALSE;
	}

	/* match if string matches directory */
	return EQ(dir, dirn);
    }

    if (isdigit(string[0])) {
	for (p = string; *p; p++) {
	    if (*p == '-') {
		if (delim)
		    goto not_range;
		delim = p;
		continue;
	    }
	    if (! isdigit(*p))
		goto not_range;
	}
	pw = getpwbyname(user);
	if (pw == NULL)
	    return FALSE;
	if (delim) {
	    if (atol(string) <= pw->pw_uid && pw->pw_uid <= atol(delim + 1))
		return TRUE;
	} else {
	    if (atol(string) == pw->pw_uid)
		return TRUE;
	}
	return FALSE;
    }

 not_range:
    /* check against the user */
    return EQ(string, user);
}
