/*
#ident	"@(#)smail/src/directors:RELEASE-3_2:user.c,v 1.11 1996/05/30 04:59:52 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * user.c:
 *	direct mail to a transport which will deliver to local user
 *	mailboxes.  Match only local addresses which are login names
 *	on the local host.
 *
 * Specifications for the user directing driver:
 *
 *	private attribute data:
 *	    transport (name):  the name of the transport to use in delivering
 *		mail to local users.
 */
#include <stdio.h>
#include <pwd.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../log.h"
#include "../direct.h"
#include "../transport.h"
#include "../exitcodes.h"
#include "../dys.h"
#include "user.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif


static struct attr_table user_attributes[] = {
    { "transport", t_string, NULL, NULL, OFFSET(user_private, transport) },
    { "prefix", t_string, NULL, NULL, OFFSET(user_private, prefix) },
    { "ignore-case", t_boolean, NULL, NULL, USER_IGNORE_CASE },
#ifdef HAVE_FGETPWENT
    { "pwfile", t_string, NULL, NULL, OFFSET(user_private, pwfile) },
#endif
};
static struct attr_table *end_user_attributes =
	ENDTABLE(user_attributes);
static struct user_private user_template = {
    "local",			/* transport */
    NULL,			/* prefix */
#if HAVE_GETPWENT
    NULL,			/* pwfile */
#endif
};

#undef P_
#ifdef ANSI_C
# define P_(x) x
#else
# define P_(x) ()
#endif

#ifdef HAVE_FGETPWENT
void direct_userinfo_pwfile P_((struct addr *, char *));
#endif


/*
 * dtd_user - direct to local user mailboxes
 */
/*ARGSUSED*/
struct addr *
dtd_user(dp, in, out, new, defer, fail)
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
    struct user_private *priv;

    if (!(priv = (struct user_private *)dp->private))
	priv = &user_template;

    DEBUG(DBG_DRIVER_HI, "dtd_user called\n");
    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if (priv->prefix) {
	    struct addr *new2;
	    int len = strlen(priv->prefix);

	    /*
	     * if we are testing against a prefix, strip the prefix and
	     * lookup the name in the passwd file.  If found, return a
	     * new fully resolved addr.
	     */
	    if (strncmpic(priv->prefix, cur->remainder, len) != 0) {
		cur->succ = pass;
		pass = cur;
		continue;		/* did not start with prefix */
	    }
	    new2 = alloc_addr();
	    new2->remainder = COPY_STRING(cur->remainder + len);
#if HAVE_GETPWENT
	    if (priv->pwfile)
		direct_userinfo_pwfile(new2, priv->pwfile);
	    else
#endif
		director_user_info(new2);

	    if ((new2->flags&ADDR_NOTUSER) || (new2->error)) {
		/* we did not match a user on the local host */
	        cur->error = new2->error;
		xfree(new2->remainder);
		xfree((char *)new2);
		if (cur->error) {
		    cur->succ = *defer;
		    *defer = cur;
		} else {
		    cur->succ = pass;
 		    pass = cur;
		}
		continue;
	    }
	    new2->in_addr = COPY_STRING(new2->remainder);
	    new2->director = dp;
	    new2->parent = cur;
	    cur = new2;
	} else {
	    /* fill in any user information */
#if HAVE_GETPWENT
	    if (priv->pwfile) {
		direct_userinfo_pwfile(cur, priv->pwfile);
		if (cur->error) {
		    cur->succ = *defer;
		    *defer = cur;
		    continue;
		}		    
	    } else
#endif
		director_user_info(cur);

	    /* don't match it if it is not a local user */
	    if (cur->flags&ADDR_NOTUSER) {
		cur->succ = pass;
		pass = cur;
		continue;
	    }
	}

	DEBUG2(DBG_DRIVER_LO, "director %s matched user %s\n",
	       dp->name, cur->remainder);
	cur->director = dp;		/* matched address */

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
	cur->next_addr = COPY_STRING(cur->remainder);
	cur->succ = *out;
	*out = cur;
    }

    return pass;			/* return addrs for next director */
}

/*
 * dtv_user - verify a user on the local host
 */
/*ARGSUSED*/
void
dtv_user(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next value for cur */
    struct user_private *priv;

    if (!(priv = (struct user_private *)dp->private))
	priv = &user_template;

    DEBUG(DBG_DRIVER_HI, "dtv_user called\n");

    /* loop over all of the input addrs */
    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if (priv->prefix) {
	    int len = strlen(priv->prefix);
	    char *dupe = NULL;

	    /*
	     * if we are testing against a prefix, strip the prefix and
	     * see if the remaining string matches a local user
	     */
	    if (strncmpic(priv->prefix, cur->remainder, len) == 0) {
#ifdef HAVE_FGETPWENT
		if (priv->pwfile) {
		    long flags;
		    struct addr * new2;

		    new2 = alloc_addr();
		    new2->remainder = COPY_STRING(cur->remainder + len);
		    direct_userinfo_pwfile(new2, priv->pwfile);
		    flags = new2->flags;
		    cur->error = new2->error;
		    xfree(new2->remainder);
		    xfree((char *) new2);
		    if (cur->error) {
			cur->succ = *defer;
			*defer = cur;
			continue;
		    }
		    if (flags&ADDR_ISUSER) {
			cur->succ = *okay;
			*okay = cur;
			continue;
		    }
		} else
#endif
		{
		    if (getpwbyname(dp->flags & USER_IGNORE_CASE ?
				str2lower((dupe = COPY_STRING(cur->remainder + len))) :
				cur->remainder + len)) {
			/* matched */
			cur->succ = *okay;
			*okay = cur;
			continue;
		    }
		}
	    }
	    if (dupe)
		free(dupe);
	} else {
	    /* fill in any user information */
#ifdef HAVE_FGETPWENT
	    if (priv->pwfile) {
		direct_userinfo_pwfile(cur, priv->pwfile);
		if (cur->error) {
		    cur->succ = *defer;
		    *defer = cur;
		    continue;
		}		    
	    } else
#endif
		director_user_info(cur);

	    /* match only if this is a user */
	    if (cur->flags&ADDR_ISUSER) {
		cur->succ = *okay;
		*okay = cur;
		continue;
	    }
	}

	/* didn't match */
	cur->succ = *retry;
	*retry = cur;
    }
}

/*
 * dtb_user - read the configuration file attributes
 */
char *
dtb_user(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    struct user_private *priv;		/* new user_private structure */

    /* copy the template private data */
    priv = (struct user_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&user_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    user_attributes,
			    end_user_attributes);

    if (error) {
	return error;
    } else {
	if (priv->transport == NULL) {
	    return "transport attribute required";
	}
	if (find_transport(priv->transport) == NULL) {
	    return xprintf("unknown transport: %s", priv->transport);
	}
	return NULL;
    }
}


/*
 * dtp_user - dump user config
 */
void
dtp_user(f, dp)
     FILE * f;
     struct director *dp;
{
    	(void) dump_standard_config(f,
				    (dp->private) ? dp->private : (char *)&user_template,
				    dp->name,
				    dp->flags,
				    user_attributes,
				    end_user_attributes);
}


#ifdef HAVE_FGETPWENT
/*
 * direct_userinfo_pwfile - like director_user_info using a custom pw file
 *
 */
void
direct_userinfo_pwfile(addr, pwfile)
    struct addr * addr;
    char * pwfile;
{
    FILE * pwfd;		/* password file descriptior */
    struct passwd *pw;		/* passwd structure */

    if ((pwfd = fopen(pwfile, "r")) == NULL) {
	/*
	 * ERR_185 - user transport not specified
	 *
	 * DESCRIPTION
	 *      A specified passwd file cannot be opened
	 *
	 * ACTIONS
	 *      Defer the message with a configuration error.
	 *
	 * RESOLUTION
	 *      The director file should be edited to specify the
	 *      correct passwd file.
	 */
	addr->error = note_error(ERR_CONFERR|ERR_185,
				 xprintf("director user: specified passwd file missing"));
	return;
    } 
    while (pw = (struct passwd *) fgetpwent(pwfd)) {
	if (strcmp(pw->pw_name, addr->remainder) == 0) {
	    /* passwd entry found */
	    addr->flags |= ADDR_ISUSER;
	    addr->uid = pw->pw_uid;
	    addr->gid = pw->pw_gid;
	    addr->home = COPY_STRING(pw->pw_dir);
	    fclose(pwfd);
	    return;
	}
    }
    addr->flags |= ADDR_NOTUSER;
    fclose(pwfd);
    return;
}
#endif
