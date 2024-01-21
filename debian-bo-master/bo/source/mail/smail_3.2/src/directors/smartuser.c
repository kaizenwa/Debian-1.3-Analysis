/* @(#) smartuser.c,v 1.8 1995/07/06 17:03:02 nm4 Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smartuser.c:
 *	direct mail to another address.  This is useful for directing mail
 *	that did not resolve to a user on the local machine to a user on
 *	a remote machine.
 *
 * Specifications for the smartuser directing driver:
 *
 *	private attribute data:
 *	    new_user: (a string) specifies a new address.  expand_string()
 *		is used to build the actual address, with the address'
 *		remainder being substituted for $user.
 *
 *	private attribute flags:
 *	    well_formed_only: if set, only match names which contain letters
 *		numbers, `.', `-', `_', and '+'.   White spaces and dots
 *		are folded into single occurances of the character `.'.
 *
 * NOTE:  If no new_user attribute is specified, the smart_user
 *	  config file attribute will be used.  If neither of these exist,
 *	  then the smart_user driver does not match anything.  This makes
 *	  it possible to have a compiled-in configuration that does not
 *	  match anything, while making it possible to enable the smartuser
 *	  driver using only a config file.
 */
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../field.h"
#include "../log.h"
#include "../direct.h"
#include "../dys.h"
#include "../exitcodes.h"
#include "smartuser.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* functions local to this file */
static char *make_well_formed();
static char *make_passable();

static struct attr_table smartuser_attributes[] = {
    { "new_user", t_string, NULL, NULL,
	  OFFSET(smartuser_private, new_user) },
    { "transport", t_string, NULL, NULL,
	  OFFSET(smartuser_private, transport) },
    { "well_formed_only", t_boolean, NULL, NULL, SMARTUSER_WELLFORMED },
};
static struct attr_table *end_smartuser_attributes =
	ENDTABLE(smartuser_attributes);
static struct smartuser_private smartuser_template = {
    NULL,				/* new_user */
    NULL,				/* transport */
    NULL,				/* transport_ptr */
};


/*
 * dtd_smartuser - direct to another address
 */
/*ARGSUSED*/
struct addr *
dtd_smartuser(dp, in, out, new, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **new;			/* output new addrs to resolve */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next value for cur */
    struct addr *pass = NULL;		/* addrs to pass to next director */
    char *error;
    char *temp;
    struct smartuser_private *priv;

    if (!(priv = (struct smartuser_private *)dp->private))
	    priv = &smartuser_template;

    DEBUG(DBG_DRIVER_HI, "dtd_smartuser called\n");
    if (priv->new_user == NULL && smart_user == NULL && priv->transport == NULL) {
	/* there is no smart user information, don't match anything */
	return in;
    }

    for (cur = in; cur; cur = next) {
	char *new_user;			/* expanded new_user attribute */
	char *remainder;		/* remainder from addr structure */
	struct addr *new_addr;		/* new addr structure */

	next = cur->succ;
	if (cur->flags & ADDR_SMARTUSER) {
	    /* do not match addresses which smart_user already rerouted */
	    cur->succ = pass;
	    pass = cur;
	    continue;
	}

	if (dp->flags & SMARTUSER_WELLFORMED) {
	    /*
	     * if required, copy the remainder, making it "well-formed"
	     * in the process
	     */
	    remainder = make_well_formed(cur->remainder);
	    if (remainder == NULL) {
		/* not a sufficiently well-formed, pass on the address */
		cur->succ = pass;
		pass = cur;
		continue;
	    }
	} else {
	    remainder = make_passable(cur->remainder);
	}

	cur->director = dp;		/* match anything, at this point */

	if ((priv->new_user != NULL) || (smart_user != NULL)) {
	    new_user = expand_string(priv->new_user? priv->new_user: smart_user,
				     (struct addr *)NULL, (char *)NULL, remainder);
	} else {
	    new_user = remainder;
	}
	if (new_user == NULL) {
	    /*
	     * ERR_120 - smartuser expansion failed
	     *
	     * DESCRIPTION
	     *      expand_string() failed to expand the new_user attribute
	     *      for a smartuser director.
	     *
	     * ACTIONS
	     *      Defer the message with a configuration error.
	     *
	     * RESOLUTION
	     *      Check the entry in the director file and fix the
	     *      new_user attribute.
	     */
	    cur->error = note_error(ERR_CONFERR|ERR_120,
				    xprintf(
				      "director %s: new_user expansion failed",
					    dp->name));
	    cur->succ = *defer;
	    *defer = cur;
	    continue;
	}

	temp = preparse_address(new_user, &error);
	if (temp == NULL) {
	    /*
	     * ERR_121 - smartuser parse error
	     *
	     * DESCRIPTION
	     *      preparse_address() found an error while parsing the
	     *      expanded new_user attribute for a smartuser director.
	     *      The actual error was returned in `error'.
	     *
	     * ACTIONS
	     *      Defer the message with a configuration error.
	     *
	     * RESOLUTION
	     *      Check the entry in the director file and fix the
	     *      new_user attribute.
	     */
	    cur->error = note_error(ERR_CONFERR|ERR_121,
				    xprintf(
				      "director %s: smartuser parse error: %s",
					    dp->name, error));
	    cur->succ = *defer;
	    *defer = cur;
	    continue;
	}
	new_addr = alloc_addr();
	new_addr->parent = cur;
	new_addr->work_addr = temp;
	new_addr->in_addr = COPY_STRING(new_user);
	new_addr->flags = ADDR_SMARTUSER;
	new_addr->director = dp;
	new_addr->transport = priv->transport_ptr;
	if (priv->transport == NULL) {
	    /* 
	     * We are just rewriting the address (no transport), so we
	     * put it on the reprocess pile
	     */
	    new_addr->succ = *new;
	    *new = new_addr;
	    DEBUG3(DBG_DRIVER_LO, "director %s: matched %s, aliased to %s\n",
		   dp->name, cur->remainder, new_user);
	} else {
	    /* 
	     * We have a transport, so we can mark this one as done and
	     * put it on the resolved pile.  We also need to set a couple
	     * more things....
	     */
	    new_addr->remainder = COPY_STRING(new_user);
	    new_addr->next_addr = COPY_STRING(new_user);
	    new_addr->succ = *out;
	    *out = new_addr;
	    DEBUG4(DBG_DRIVER_LO, "director %s: matched %s, aliased to %s, transport %s\n",
		   dp->name, cur->remainder, new_user, priv->transport);
	}
    }

    return pass;			/* return addrs for next director */
}

/*
 * dtv_smartuser - match just about anything (unless well_formed is set).
 */
/*ARGSUSED*/
void
dtv_smartuser(dp, in, retry, okay, defer, fail)
    struct director *dp;		/* director entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* temp for processing input */
    struct addr *next;			/* next addr to process */
    struct smartuser_private *priv;

    if (!(priv = (struct smartuser_private *)dp->private))
	    priv = &smartuser_template;

    DEBUG(DBG_DRIVER_HI, "dtv_smartuser called\n");

    if (priv->new_user == NULL && smart_user == NULL) {
	    /* smart-user director is not enabled */
	    insert_addr_list(in, retry, (struct error *)NULL);
	    return;
    }

    /* loop through all of the input addrs */
    for (cur = in; cur; cur = next) {
	char *remainder;		/* remainder from addr structure */

	next = cur->succ;
	if (cur->flags & ADDR_SMARTUSER) {
	    /* do not match addresses which smart_user already rerouted */
	    cur->succ = *retry;
	    *retry = cur;
	    continue;
	}
	if (dp->flags & SMARTUSER_WELLFORMED) {
	    /*
	     * if a well-formed addr is required, make sure it IS
	     * well formed.
	     */
	    remainder = make_well_formed(cur->remainder);
	    if (remainder == NULL) {
		/* not a sufficiently well-formed, pass on the address */
		cur->succ = *retry;
		*retry = cur;
		continue;
	    }
	}

	/* we match anything that passes the tests up to this point */
	cur->succ = *okay;
	*okay = cur;
    }
}

/*
 * dtb_smartuser - read the configuration file attributes
 */
char *
dtb_smartuser(dp, attrs)
    struct director *dp;		/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    struct smartuser_private *priv;	/* new smartuser_private structure */

    /* copy the template private data */
    priv = (struct smartuser_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&smartuser_template, sizeof(*priv));

    dp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &dp->flags,
			    smartuser_attributes,
			    end_smartuser_attributes);

    if (error) {
	return error;
    } else {
	/* There should be a check here to make sure that at least one
	 * of the transport or new_user attributes are specified, but
	 * this would cause problems with a default config - ie no
	 * directors file
	 */
	if ((priv->transport != NULL) 
	    && ((priv->transport_ptr = find_transport(priv->transport)) == NULL)) {
	    return xprintf("unknown transport: %s", priv->transport);
	}
	return NULL;
    }
}

/*
 * dtp_smartuser - dump smartuser config
 */
void
dtp_smartuser(f, dp)
     FILE * f;
     struct director *dp;
{
    (void) dump_standard_config(f,
				(dp->private) ? dp->private : (char *)&smartuser_template,
				dp->name,
				dp->flags,
				smartuser_attributes,
				end_smartuser_attributes);
}


/*
 * make_well_formed - check for well-formedness of a local address
 *
 * Check to make sure that characters in the input are in the set of
 * letters, numbers and the characters `.' and `-'.  Also,  any set
 * of 1 or more white space characters and `.' are transformed into
 * exactly one `.' character.
 *
 * returns NULL or a well-formed string.  The value returned points
 * to storage which may be reused on subsequent calls.
 */
static char *
make_well_formed(name)
    register char *name;		/* input string */
{
    static char *result = NULL;		/* region for forming result */
    static int result_size;		/* allocated region length */
    register char *p;			/* point to result */
    register int c;			/* char from name string */

    /* get a region at least big enough to store result */
    if (result == NULL) {
	result = xmalloc(result_size = strlen(name) + 1);
    } else {
	register int new_size = strlen(name) + 1;

	if (new_size > result_size) {
	    result = xrealloc(result, new_size);
	}
    }

    /*
     * copy name to result, performing any necessary transformations.
     * Stop if any characters are found which would make the input
     * not well-formed.
     *
     * Don't allow it to begin with `-'.
     */
    p = result;
    if (*name == '-') {
	return NULL;
    }
    while (c = *name++) {
	if (isalnum(c) || c == '-' || c == '_' || c == '+') {
	    *p++ = c;
	} else if (isspace(c) || c == '.') {
	    *p++ = '.';
	    while (isspace(c = *name) || c == '.') {
		name++;
	    }
	} else {
	    return NULL;
	}
    }
    *p++ = '\0';

    return result;
}

/*
 * make_passable - build a legal local-part address string from an input
 *
 * given an input string, surround that string in quotes, if required
 * to make a valid local-part address, according to the RFC822 rules.
 * If the address is put in quotes, the text within quotes is guarranteed
 * to be properly \-escaped where required.
 *
 * return value may point to storage which can be reused in subsequent
 * calls.  The value should be copied if it is to be retained.
 */
static char *
make_passable(name)
    char *name;				/* input local-part address */
{
    int need_quotes = FALSE;		/* true if we should put in quotes */
    int esc_ct = 0;			/* count of \'s required */
    static char *result = NULL;		/* region for building result */
    static int result_size;		/* size of result region */
    int new_size;			/* minimum size required */
    register char *p;			/* temp for building result */
    register char *q;			/* temp for scanning name */
    register int c;			/* char from name */

    for (q = name; c = *q; q++) {
	if (isalnum(c)) {
	    continue;
	} else if (c == '.' || c == '-') {
	    if (q == name || q[1] == '\0' ||
		q[-1] == '.' || q[-1] == '-')
	    {
		need_quotes = TRUE;
	    }
	} else {
	    need_quotes = TRUE;
	    if (c == '"' || c == '\\' || !isprint(c)) {
		esc_ct++;
	    }
	}
    }

    if (need_quotes) {
	new_size = strlen(name) + esc_ct + sizeof("\"\"");
	if (result == NULL) {
	    result = xmalloc(result_size = new_size);
	} else if (result_size < new_size) {
	    result = xrealloc(result, result_size = new_size);
	}

	/* copy name to result, inserting \ where appropriate */
	p = result;
	*p++ = '"';
	for (q = name; c = *q; q++) {
	    if (c == '"' || c == '\\' || !isprint(c)) {
		*p++ = '\\';
		*p++ = c;
	    } else {
		*p++ = c;
	    }
	}
	*p++ = '"';
	*p = '\0';

	return result;
    }

    return name;			/* the original string was fine */
}
