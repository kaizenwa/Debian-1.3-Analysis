/* @(#) akcs.c,v 1.2 1990/10/24 05:17:09 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *
 *    Changed, modified, mangled and hacked by Karl Denninger
 *	(karl@ddsw1.MCS.COM); changes copyright 1989 Karl Denninger.
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * akcs.c:
 *	direct mail to a transport which will deliver to local AKCS(tm)
 *	mailboxes.  Match only local addresses which are login names
 *	for AKCS on the local host.
 *
 * Specifications for the akcs directing driver:
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
#include "akcs.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif


/*
 * akcs_user_info - fill an addr structure with user information
 *
 * if the remainder field if the addr structure matches a username,
 * fill in fields which pertain to the user's password file entry.
 * Note that this only checks the AKCS information files.
 *
 */
void
akcs_user_info(addr)
    struct addr *addr;			/* addr structure to check */
{
    char	tmp[512];		/* Temporary space */
    char	usr[32];		/* User space */
    char	basedir[132];
    FILE	*fid, *fid2;

    if (addr->flags & (ADDR_NOTUSER|ADDR_ISUSER)) {
	/* a previous call to akcs_user_info() already took care of this */
	return;
    }

    /* get the AKCS password entry if one exists */
    if ((fid = fopen("/etc/akcsparams", "r")) == NULL) {
	addr->flags |= ADDR_NOTUSER;	/* No parameter file, no AKCS! */
	return;				/* Pass on this one... */
    }
    while (fgets(tmp, 132, fid) != NULL) {
	if (sscanf(tmp, "Base: %[!-z]", basedir)) {
		sprintf(tmp, "%s/AKCS.p.users", basedir);
    		if ((fid2 = fopen(tmp, "r")) == NULL) {	/* Open user file */
			fclose(fid);		/* Close parameter file */
			addr->flags |= ADDR_NOTUSER;	/* Not there */
			return;
	        }
		while (fgets(tmp, 512, fid2) != NULL) {
			if (sscanf(tmp, "%[!-z]", usr) <= 0)
				continue;		/* There's a problem */
			if (!strcmp(usr, addr->remainder)) {	/* This user? */
				addr->flags |= ADDR_ISUSER;		/* Save it if so */
				sprintf(tmp, "%s/.users/%s", basedir, addr->remainder);
				addr->home = COPY_STRING(tmp);
				fclose(fid);	/* Close parameter file */
				fclose(fid2);
				return;			/* Exit with success */
			}
		}
		fclose(fid2);
	}
    }
    fclose(fid);
    addr->flags |= ADDR_NOTUSER;
    return;
}


/*
 * dtd_akcs - direct to local akcs mailboxes
 */
/*ARGSUSED*/
struct addr *
dtd_akcs(dp, in, out, new, defer, fail)
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
    struct akcs_private *priv = (struct akcs_private *)dp->private;

    DEBUG(DBG_DRIVER_HI, "dtd_akcs called\n");
    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if (priv->prefix) {
	    struct addr *new;
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
	    new = alloc_addr();
	    new->remainder = COPY_STRING(cur->remainder + len);
	    akcs_user_info(new);
	    if (new->flags&ADDR_NOTUSER) {
		/* we did not match a user on the local host */
		xfree(new->remainder);
		xfree((char *)new);
		cur->succ = pass;
		pass = cur;
		continue;
	    }
	    new->in_addr = COPY_STRING(new->remainder);
	    new->director = dp;
	    new->parent = cur;
	    cur = new;
	} else {		/* The prefix didn't match, ignore it */
		cur->succ = pass;
		pass = cur;
		continue;
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
 * dtv_akcs - verify a AKCS user on the local host
 */
/*ARGSUSED*/
void
dtv_akcs(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next value for cur */
    struct akcs_private *priv = (struct akcs_private *)dp->private;

    DEBUG(DBG_DRIVER_HI, "dtv_akcs called\n");

    /* loop over all of the input addrs */
    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if (priv->prefix) {
	    int len = strlen(priv->prefix);

	    /*
	     * if we are testing against a prefix, strip the prefix and
	     * see if the remaining string matches a local user
	     */
	    if (strncmpic(priv->prefix, cur->remainder, len) == 0 &&
		akcs_user_valid(cur->remainder + len))
	    {
		/* matched */
		cur->succ = *okay;
		*okay = cur;
		continue;
	    }
	} else {
	    /* fill in any user information */
	    akcs_user_info(cur);

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
dtb_akcs(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct attr_table akcs_attributes[] = {
	{ "transport", t_string, NULL, NULL, OFFSET(akcs_private, transport) },
	{ "prefix", t_string, NULL, NULL, OFFSET(akcs_private, prefix) },
    };
    static struct attr_table *end_akcs_attributes = ENDTABLE(akcs_attributes);
    static struct akcs_private akcs_template = {
	"akcs",
	NULL,
    };
    struct akcs_private *priv;		/* new akcs_private structure */

    /* copy the template private data */
    priv = (struct akcs_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&akcs_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    akcs_attributes,
			    end_akcs_attributes);

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
 * akcs_user_valid - is the address given valid?
 *
 * if the passed field matches a username, return non-zero.
 * 
 */
int
akcs_user_valid(addr)
    char	*addr;			/* addr to check */
{
    char	tmp[512];		/* Temporary space */
    char	usr[32];		/* User space */
    FILE	*fid, *fid2;
    char	basedir[132];

    /* get the AKCS password entry if one exists */
    if ((fid = fopen("/etc/akcsparams", "r")) == NULL) {
	return(0);				/* Pass on this one... */
    }
    while (fgets(tmp, 132, fid) != NULL) {
	if (sscanf(tmp, "Base: %[!-z]", basedir)) {
		sprintf(tmp, "%s/AKCS.p.users", basedir);
    		if ((fid2 = fopen(tmp, "r")) == NULL) {	/* Open user file */
			fclose(fid);		/* Close parameter file */
			return(0);
	        }
		while (fgets(tmp, 512, fid2) != NULL) {
			if (!strcmp(usr, addr)) {
				fclose(fid);	/* Close parameter file */
				fclose(fid2);
				return(-1);	/* It's ok */
			}
		}
		fclose(fid2);
	}
    }
    fclose(fid);
    return(0);
}

