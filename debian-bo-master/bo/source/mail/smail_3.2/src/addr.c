/*
#ident	"@(#)smail/src:RELEASE-3_2:addr.c,v 1.16 1996/05/27 14:20:45 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * addr.c:
 *	routines to parse addresses
 *
 *	external functions:  preparse_address, parse_address,
 *			     build_uucp_route, build_partial_uucp_route,
 *			     address_token, back_address_token, alloc_addr,
 *			     insert_addr_list, addr_sort, split_addr_list
 */
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "defs.h"
#include "smail.h"
#include "addr.h"
#include "dys.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

/* functions local to this file */
static int check_target_and_remainder();
static char *escaped();
static char *internal_build_uucp_route();
static int addrcmp();


/*
 * preparse_address - do preliminary parsing that might be needed for address
 *
 * this routine should be used when an address is first extracted from a
 * source.  It transforms some mutant addressing forms into something more
 * managable.
 *
 * Transformations:
 *
 *	<string>		becomes just  string (recursively)
 *	host!(host!)*@route	becomes a pure !-route
 *
 * NOTE:  We don't handle @route:host!(host!)*@route, for now.  Maybe later.
 *
 * input:
 *	address	- address to be preparsed
 *	error	- error message
 *
 * output:
 *	parsed address, or NULL for parsing error, message returned in error
 *	output is guarranteed to not be a pointer to the input
 */
char *
preparse_address(address, error)
    char *address;			/* address to be preparsed */
    char **error;			/* return error message here */
{
    char * ignore;
    return preparse_address_1(address, error, & ignore);
}

char *
preparse_address_1(address, error, rest)
    char *address;
    char **error;
    char **rest;
{
    register char *ap;			/* temp for scanning address */
    char *mark_start = NULL;		/* marked position of < */
    char *mark_end = NULL;		/* marked position of > */
    int nest_cnt = 0;			/* nesting count for angle brackets */

    DEBUG1(DBG_ADDR_HI, "preparse_address(%s) entry:\n", address);
    /*
     * scan for < and > pairs and find the last or innermost matching
     * pair.
     */
    for (ap = address; ap && *ap; ap = address_token(ap)) {
	if (*ap == '<') {
	    nest_cnt++;
	    mark_start = ap + 1;
	    mark_end = NULL;
	} else if (*ap == '>') {
	    nest_cnt--;
	    if (mark_end == NULL) {
		mark_end = ap;
	    }
	}
    }
    if (ap == NULL) {
	*error = "bad address token";
	DEBUG1(DBG_ADDR_LO,
	       "preparse_address found error %s: returns (null)\n",
	       *error);
	return NULL;
    }
    if (mark_start && mark_end == NULL) {
	/* hmm, no match for the < token */
	*error = "no match for `<' in address";
	DEBUG1(DBG_ADDR_LO,
	       "preparse_address found error %s: returns (null)\n",
	       *error);
	return NULL;
    }
    if (nest_cnt != 0) {
	if (nest_cnt < 0) {
	    *error = "no match for > in address";
	} else {
	    *error = "no match for < in address";
	}
	DEBUG1(DBG_ADDR_LO,
	       "preparse_address found error %s: returns (null)\n",
	       *error);
	return NULL;
    }
    /* narrow to the route-addr */
    if (mark_end) {
	*mark_end = '\0';
	address = ap = mark_start;
    }

    /*
     * now search for the mutant form: path!@route-addr
     */
    if (*ap == '@') {
	/* valid route-addr, not a mutant one */
	ap = xmalloc((unsigned)(strlen(address) + 1));
	(void) strcpy(ap, address);
	if (mark_end) {
	    *mark_end++ = '>';		/* widden the original address */
	}
	DEBUG1(DBG_ADDR_HI, "preparse_address returns: %s\n", ap);
	*rest = mark_end;
	return ap;			/*  no transformations */
    }

    while (*ap) {
	ap = address_token(ap);
	if (ap == NULL) {
	    *error = "bad address token";
	    DEBUG1(DBG_ADDR_LO,
		   "preparse_address found error %s: returns (null)\n",
		   *error);
	    return NULL;
	}
	if (*ap != '!') {
	    ap = xmalloc((unsigned)(strlen(address) + 1));
	    (void) strcpy(ap, address);
	    if (mark_end) {
		*mark_end++ = '>';	/* widden the original address */
	    }
	    DEBUG1(DBG_ADDR_HI, "preparse address returns: %s\n", ap);
	    *rest = mark_end;
	    return ap;		/* address should be okay */
	}
	ap++;
	if (*ap == '@') {
	    /* matched host!(host!)*@route -- build the !-route */
	    register char *p = xmalloc((unsigned)strlen(address));
	    DEBUG(DBG_ADDR_MID, "found host!(host!)*@route form--ugh!\n");
	    /* first part already !-route */
	    (void) strncpy(p, address, (size_t) (ap - address));
	    if (mark_end) {
		*mark_end++ = '>';	/* widden the original address */
	    }
	    ap = build_uucp_route(ap, error, 0); /* build !-route */
	    if (ap == NULL) {
		DEBUG(DBG_ADDR_MID, "preparse_address returns: (null)\n");
		return NULL;
	    }
	    (void) strcat(p, ap);	/* concatenate together */
	    xfree(ap);
	    DEBUG1(DBG_ADDR_HI, "preparse_address returns: %s\n", p);
	    *rest = mark_end;
	    return p;			/* transformed */
	}
    }
    ap = xmalloc((unsigned)(strlen(address) + 1));
    (void) strcpy(ap, address);
    if (mark_end) {
	*mark_end++ = '>';	/* widden the original address */
    }
    DEBUG1(DBG_ADDR_HI, "preparse address returns: %s\n", ap);
    *rest = mark_end;
    return ap;				/* no transformations */
}


/*
 * parse_address - extract a target and remainder from an address
 *
 * using the rules in section 3.2 of the mailer.design document,
 * extract a target and a remainder from an address.
 *
 * The target is defined as the first destination host in an address,
 * the remainder is defined as the remaining parat of the address
 * after extracting the target.
 *
 * A short form of the rules for extraction is the following table
 * of addressing forms in order of lowest to highest precedence:
 *
 *	+---------------------------------------------------------------+
 *	| form			| description		| return	|
 *	|-----------------------|-----------------------|---------------|
 *	| @target,remainder	| route from route-addr	| RFC_ROUTE	|
 *	| @target:remainder	| route from route-addr	| RFC_ENDROUTE	|
 *	| remainder@target	| standard mailbox	| MAILBOX	|
 *	| target!remainder	| UUCP !-route		| UUCP_ROUTE	|
 *	| remainder%target	| obsolete mailbox hack	| PCT_MAILBOX	|
 *	| remainder		| local address form	| LOCAL		|
 *	+---------------------------------------------------------------+
 * If USE_BERKNET or USE_DECNET are defined:
 *	+---------------------------------------------------------------+
 *	| target::remainder	| decnet route		| DECNET	|
 *	| target:remainder	| obsolete berkenet	| BERKNET	|
 *	+---------------------------------------------------------------+
 *
 * The precedence of the % and ! operators can be switched for
 * addresses of the form a!b%c@d.  This switch will happen if the
 * variable switch_percent_and_bang is TRUE.
 *
 * inputs:
 *	address	- string containing the address to be parsed
 *	target	- where to store pointer to computed target
 *	remainder - where to store pointer to computed target
 *
 * outut:
 *	return the address form as described in the above table.  Also,
 *	return in target a pointer to to the target and return in
 *	remainder a pointer to the remainder.  If an error is detected
 *	return FAIL and load the remainder with an error message.
 *	If target is NULL, then only a form is returned, a target and
 *	remainder are not returned, though an error message may still
 *	be loaded into remainder.
 *
 * in-out:
 *	*flagp - flagp is used to maintain state between invocations
 *		 of parse_address() that are used to parse successive
 *		 remainder components.  It is used to manage the
 *		 variant rules used for RFC1123 compliance for the %
 *		 operator in the presense of a user@host address.
 *
 *		 When parse_address() is called to parse a complete
 *		 address, *flagp should be 0.  If parse_address is
 *		 used (perhaps successively) to parse generated
 *		 remainder strings, then the previous *flagp value should
 *		 be re-passed.  FOUND_MAILBOX will be or'd into *flagp
 *		 if a user@host form is encountered, in which case further
 *		 parses of remainder addresses may use the RFC1123
 *		 precedence interpretation of the % operator.
 *
 * NOTE:  address will be overwritten unless it is in local form, or
 *	  a target and remainder are not returned.
 *
 * calls: address_token, back_address_token
 * called by: build_uucp_route
 */
int
parse_address(address, target, remainder, flagp)
    char *address;			/* address to parse (destructively) */
    char **target;			/* store pointer to target host here */
    char **remainder;			/* store pointer to remainder here */
    int *flagp;				/* flag passed between invocations */
{
    char *ep;				/* pointer to end of address */
    register char *last_tokens;		/* start of second to last token */
    register char *ap;			/* pointer for scanning address */
    register char *p;			/* temp */
    int switch_flag;

    DEBUG1(DBG_ADDR_HI, "parse_address called: address=%s\n", address);
    /*
     * make sure we have an address
     */
    ap = address;
    if (*ap == '\0') {
	/* nothing to do with a zero-length address */
	*remainder = "null address";
	DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	return FAIL;
    }

    switch_flag = flagp && *flagp & FOUND_MAILBOX && switch_percent_and_bang;

    /*
     * does the address begin with @target[,:] ?
     */
    if (*ap == '@') {
	if (target) {
	    *target = ap + 1;			/* mark the target */
	}
	ap = address_token(ap + 1);		/* skip target */
	if (ap == NULL) {
	    *remainder = "bad address token";
	    DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	    return FAIL;
	}

	/* ensure that the `,' or `:' is in the address */
	if (!ap) {
	    /* interesting, address just contained '@' */
	    *remainder = "syntax error:  no target host";
	    DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	    return FAIL;
	}
	if (*ap == ',' || *ap == ':') {
	    int retval = (*ap==','? RFC_ROUTE: RFC_ENDROUTE);
	    if (target) {
		*ap++ = '\0';		/* null terminate target */
		*remainder = ap;
		if (check_target_and_remainder(target, remainder) == FAIL) {
		    return FAIL;
		}
		DEBUG2(DBG_ADDR_HI,
		       "parse_address: RFC_ROUTE: target=%s, remainder=%s\n",
		       *target, *remainder);
	    }
	    return retval;
	}
	/* we have a syntax error, missing , or : */
	*remainder = "syntax error: , or : missing in route-addr";
	DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	return FAIL;
    }

    /*
     * is the address a standard mailbox ?
     * i.e., does the address end in @target ?
     */
    ep = address + strlen(address);
    last_tokens = back_address_token(ap, ep);
    if (last_tokens && last_tokens > ap) {
	last_tokens = back_address_token(ap, last_tokens);
    }
    if (last_tokens == NULL) {
	*remainder = "bad address token";
	DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	return FAIL;
    }
    if (last_tokens > ap && *last_tokens == '@') {
	/*
	 * it matches @token, null terminate the remainder and finish up;
	 * also set FOUND_MAILBOX to turn on RFC1123-compliant parsing
	 * of %
	 */
	if (flagp)
	    *flagp |= FOUND_MAILBOX;
	if (target) {
	    *last_tokens = '\0';
	    *target = last_tokens+1;
	    *remainder = ap;
	    if (check_target_and_remainder(target, remainder) == FAIL) {
		return FAIL;
	    }
	    DEBUG2(DBG_ADDR_HI,
		   "parse_address: MAILBOX: target=%s, remainder=%s\n",
		   *target, *remainder);
	}
	return MAILBOX;
    }

    /*
     * HACK!!  goto percent processing if we are using RFC1123-compliant
     * % parsing
     */

    if (switch_flag)
	goto switch_order_percent;

 switch_order_bang:
    /*
     * is the address a UUCP !-route ?
     * i.e., does the address begin with target! ?
     */
    p = address_token(ap);
    if (p && *p == '!') {
	/* it matches target!, null terminate target and finish up */
	if (target) {
	    *p = '\0';
	    *target = ap;
	    *remainder = p+1;
	    if (check_target_and_remainder(target, remainder) == FAIL) {
		return FAIL;
	    }
	    DEBUG2(DBG_ADDR_HI,
		   "parse_address: UUCP_ROUTE: target=%s, remainder=%s\n",
		   *target, *remainder);
	}
	return UUCP_ROUTE;
    }

    /*
     * is the address a BERKENET or DECNET syntax?
     */
#if defined(USE_DECNET) || defined(USE_BERKENET)
    if (p && *p == ':') {
# if defined(USE_DECNET)
	if (*(p + 1) == ':') {
	    /* DECNET syntax */
	    if (target) {
		*p = '\0';
		*target = ap;
		*remainder = p + 2;
		if (check_target_and_remainder(target, remainder) == FAIL) {
		    return FAIL;
		}
		DEBUG2(DBG_ADDR_HI,
		       "parse_address: DECNET: target=%s, remainder=%s\n",
		       *target, *remainder);
	    }
	    return DECNET;
	}
# endif /* USE_DECNET */
# if defined(USE_BERKENET)
	/* Berkenet syntax */
	if (target) {
	    *p = '\0';
	    *target = ap;
	    *remainder = p + 1;
	    if (check_target_and_remainder(target, remainder) == FAIL) {
		return FAIL;
	    }
	    DEBUG2(DBG_ADDR_HI,
		   "parse_address: BERKENET: target=%s, remainder=%s\n",
		   *target, *remainder);
	}
	return BERKENET;
# endif /* USE_BERKENET */
    }
#endif /* USE_DECNET || USE_BERKENET */

    if (switch_flag)
	goto switch_order_local;

 switch_order_percent:
    /*
     * is the address a non-standard mailbox ?
     * i.e., does the address end in %target ?
     */
    if (last_tokens && last_tokens - ap > 0 && *last_tokens == '%') {
	/* it matches @target, null terminate the remainder and finish up */
	if (target) {
	    *last_tokens = '\0';
	    *target = last_tokens+1;
	    *remainder = ap;
	    if (check_target_and_remainder(target, remainder) == FAIL) {
		return FAIL;
	    }
	    DEBUG2(DBG_ADDR_HI,
		   "parse_address: PCT_MAILBOX: target=%s, remainder=%s\n",
		   *target, *remainder);
	}
	return PCT_MAILBOX;
    }

    if (switch_flag)
	goto switch_order_bang;

 switch_order_local:
    /*
     * we have a local form address
     */
    if (target) {
#ifndef USE_TARGET_DOMAIN
	*target = NULL;
#endif
	*remainder = ap;
	DEBUG1(DBG_ADDR_HI, "parse_address: LOCAL: remainder=%s\n",
	       *remainder);
    }
    return LOCAL;
}

/* check_target_and_remainder - check for glaring problems */
static int
check_target_and_remainder(target, remainder)
    char **target;
    char **remainder;
{
    register int c;
    register char *p;

    if ((*remainder)[0] == '\0') {
	*remainder = "no remainder address";
	DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	return FAIL;
    }
    /* the set of chars in the target should be limited to a small set */
    p = *target;
    if (*p == '-') {
	*remainder = "target cannot begin with `-'";
	DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	return FAIL;
    }
    if (*p == '[') {
	return SUCCEED;
    }
    while ((c = *p++)) {
	if ( !(isalnum(c) || c == '.' || c == '-' || c == '_' ||
	       c == '+' || c == '=') )
	{
	    *remainder = "illegal character in hostname";
	    DEBUG1(DBG_ADDR_MID, "parse_address: %s\n", *remainder);
	    return FAIL;
	}
    }
    return SUCCEED;
}



/*
 * mixed_address - check for mixed operators in an address
 *
 * Return TRUE if the given address contains both a % operator and
 * some set of !-like operators (i.e., !, :, or ::); otherwise,
 * return FALSE.
 */
int
mixed_address(address)
    char *address;
{
    int fndpct = 0;
    int fndbang = 0;
    char *p;

    for (p = address; p; p = address_token(p)) {
	switch (*p) {
	case ':':
	case '!':
	    if (fndpct)
		return TRUE;
	    fndbang = TRUE;
	    break;

	case '%':
	    if (fndbang)
		return TRUE;
	    fndpct = TRUE;
	    break;
	}
    }

    return FALSE;
}

/*
 * build_uucp_route - convert an address into a UUCP route.
 *
 * Given an address using any of the addressing forms known to the
 * parse_address() routine, convert that address into a pure uucp
 * !-route.  The return value is always freeable with xfree().
 *
 * If there is an error, return NULL.
 *
 * inputs:
 *	address	- the address to transform into a UUCP !-route
 *	error	- on error, set this to error message, if non-NULL
 *
 * output:
 *	transformed address, or NULL if a syntax error occured
 */
char *
build_uucp_route(address, error, flag)
    char *address;			/* address to transform into !-route */
    char **error;			/* return an error message here */
    int flag;				/* flag returned by parse_address() */
{
    return internal_build_uucp_route(address, error, FALSE, flag);
}

/*
 * build_partial_uucp_route - convert an address into a partial UUCP route.
 *
 * Given an address using any of the addressing forms known to the
 * parse_address routine, convert that address into a uucp !-route,
 * possibly with %-forms left at the end.  The return value is always
 * freeable with xfree().
 *
 * If there is an error, return NULL.
 *
 * inputs:
 *	address	- the address to transform into a UUCP !-route
 *	error	- on error, set this to error message, if non-NULL
 *
 * output:
 *	transformed address, or NULL if a syntax error occured
 */
char *
build_partial_uucp_route(address, error, flag)
    char *address;			/* address to transform into !-route */
    char **error;			/* return an error message here */
    int flag;				/* flag from parse_address() */
{
    return internal_build_uucp_route(address, error, TRUE, flag);
}

/*
 * internal_build_uucp_route - internal form for uucp-route building
 *
 * called from build_uucp_route and build_partial_uucp_route.  If the
 * `partial' flag is TRUE then the latter style is used, otherwise a
 * pure !-route is built.
 */
static char *
internal_build_uucp_route(address, error, partial, flag)
    char *address;			/* address to transform into !-route */
    char **error;			/* return an error message here */
    int partial;			/* TRUE to allow %-form in route */
    int flag;
{
    struct str str;
    register struct str *sp = &str;	/* dynamic string region */
    int uucp_route = TRUE;		/* TRUE if already pure !-route */
    char *target;			/* target returned by parse_address */
    char *remainder;			/* remainder from parse_address */
    char *storage;			/* malloc region for old address */

    DEBUG1(DBG_ADDR_HI, "internal_build_uucp_route entry: address=%s\n",
	   address);
    /*
     * allocate a new copy of the address so it can be examined destructively.
     */
    storage = remainder = xmalloc((unsigned)(strlen(address) + 1));
    (void)strcpy(storage, address);

    /* initialize for copy into string region */
    STR_INIT(sp);

    /* loop until we have a local form or a %-form an error occurs */
    for (;;) {
	int form = parse_address(remainder, &target, &remainder, &flag);

	switch (form) {

	case FAIL:			/* something went wrong, somewhere */
	    *error = remainder;
	    DEBUG(DBG_ADDR_MID, "internal_build_uucp_route returns: (null)\n")
	    return NULL;

	case UUCP_ROUTE:		/* okay, this part is a !-route */
	    STR_CAT(sp, target);	/* add target! to route */
	    STR_NEXT(sp, '!');
	    break;

	case PCT_MAILBOX:		/* matched something%host... */
	    /*
	     * If we are building a pure uucp route, then a%b is just
	     * another remote form.  Otherwise, finding this form ends
	     * the parsing process.
	     */
	    if (!partial) {
		goto remote_form;
	    }
	    /* FALLTHRU */

	case LOCAL:			/* local form, we are done */
	    /* if address was already a pure !-route, return the old one */
	    if (uucp_route) {
		/* free garbage */
		xfree(storage);
		STR_FREE(sp);
		DEBUG1(DBG_ADDR_HI,
		      "internal_build_uucp_route returns: %s (unchanged)\n",
		      address);
		return COPY_STRING(address);
	    } else {
		/* append final local-part */
		STR_CAT(sp, remainder);
		if (form == PCT_MAILBOX) {
		    /* %-form requires the target to be included */
		    STR_NEXT(sp, '%');
		    STR_CAT(sp, target);
		}
		STR_NEXT(sp, '\0');
		xfree(storage);		/* free garbage */
		STR_DONE(sp);
		DEBUG1(DBG_ADDR_HI, "internal_build_uucp_route returns: %s\n",
		       sp->p);
		return sp->p;		/* return completed !-route */
	    }
	    /*NOTREACHED*/

	default:			/* not pure !-route, other form */
	remote_form:
	    STR_CAT(sp, target);	/* add target! to route */
	    STR_NEXT(sp, '!');
	    uucp_route = FALSE;
	}
    }
}

/*
 * split_addr_list - split command arguments containing addresses
 *
 * Addresses specified in arguments on the command line should be
 * split on commas, never on spaces.  In addition, the group list
 * convention is not used, so that decnet and berkenet addresses can
 * be entered without requiring quoting.
 */
void
split_addr_list(arg, app)
    char *arg;
    struct addr **app;
{
    struct addr *new;
    char *p, *q;
    int c;
    int level;

    /*
     * remove extra white space and comments from the address
     */

    arg = COPY_STRING(arg);

    strip_rfc822_comments(arg);

    p = arg;
    while ((c = *p)) {

	/*
	 * rule 1: if the address begins with @, then it is a
	 * route-addr, so it should be left alone.
	 */

	if (c == '@') {
	    new = alloc_addr();
	    new->in_addr = p;
	    new->uid = nobody_uid;
	    new->gid = nobody_gid;
	    new->succ = *app;
	    *app = new;

	    return;
	}

	/*
	 * rule 2: if the address begins with <, then search for the
	 * ending >, then a comma.  If an error is encountered, leave
	 * the address alone.
	 */
	q = p;
	if (c == '<') {
	    level = 1;
	    q = p;
	    for (;;) {
		q = address_token(q);
		if (q == NULL)
		    break;
		if (*q == '>') {
		    --level;
		    if (level == 0)
			break;
		}
	    }
	    if (q == NULL) {
		new = alloc_addr();
		new->in_addr = p;
		new->uid = nobody_uid;
		new->gid = nobody_gid;
		new->succ = *app;
		*app = new;
		break;
	    }
	    /* FALLTHRU */
	}
	for (;;) {
	    q = address_token(q);
	    if (q == NULL)
		break;
	    if (*q == ',')
		break;
	}
	if (q) {
	    *q++ = '\0';
	}
	new = alloc_addr();
	new->in_addr = p;
	new->uid = nobody_uid;
	new->gid = nobody_gid;
	new->succ = *app;
	*app = new;
	if (q)
	    p = q;
	else
	    break;
    }

    return;
}

/*
 * strip_rfc822_comments - strip RFC822 comments and white space form a string
 */

void
strip_rfc822_comments(s)
    char *s;
{
    char *p, *q;
    int c;
    int level;
    static char delims[] = "@:;<>.,";
    int space = 0;

    p = q = s;
    while ((c = *p++)) {
	if (isspace(c)) {
	    space = 1;
	    continue;
	}
	if (c == '(') {
	    level = 1;

	    while ((c = *p)) {
		p++;
		if (c == '(') {
		    level++;
		    continue;
		}
		if (c == ')') {
		    --level;
		    if (level == 0)
			break;
		    continue;
		}
		if (c == '\\') {
		    if (*p)
			p++;
		}
	    }
	    continue;
	}
	if (space) {
	    space = 0;
	    if (q > s && !strchr(delims, *(q - 1)) && !strchr(delims, c)) {
		*q++ = ' ';
	    }
	}
	if (c == '\\') {
	    *q++ = c;
	    if ((c = *p)) {
		*q++ = c;
		p++;
	    }
	    continue;
	}
	if (c == '"') {
	    *q++ = c;
	    while ((c = *p)) {
		p++;
		*q++ = c;
		if (c == '"')
		    break;
		if (c == '\\') {
		    if ((c = *p)) {
			*q++ = c;
			p++;
		    }
		}
	    }
	    continue;
	}
	*q++ = c;
    }
    *q++ = '\0';
}


/*
 * address_token - scan forward one token in an address
 *
 * an address token is delimited by a character from the set [@!%:,]
 * a token can also be a domain literal between [ and ], or
 * a quoted literal between double quotes.  \ can precede a character
 * to take away its special properties.
 * domain literals and quoted literals and other tokens can be strung
 * together into one single token if they are separated by `.'.  Otherwise
 * a domain literal or quoted literal represents one token.
 *
 * input:
 *	ap	- pointer to start of a token
 *
 * output:
 *	the end of the input token.  Return NULL on error.
 *
 * called by: parse_address
 */
char *
address_token(ap)
    register char *ap;			/* address to be scanned */
{
    static enum state {			/* states for the state machine */
	s_normal,			/* not in a literal or \ escape */
	s_cquote,			/* previous char was \ */
	s_quote,			/* scanning quoted literal */
	s_domlit			/* scanning domain literal */
    } state;
    enum state save_state;		/* previous state for \ escape */
    int dot = FALSE;			/* TRUE if last char was unescaped . */

    /* setup initial state */
    switch (*ap++) {
    case '\0':				/* no tokens */
	return NULL;			/* error */

    case '@':				/* delimiters are one token a piece */
    case '!':
    case '%':
    case ':':
    case ',':
    case '>':
    case '<':
	return ap;			/* so return that single token */

    case '"':				/* start in a quoted literal */
	state = s_quote;
	break;

    case '[':				/* start in a domain literal */
	state = s_domlit;
	break;

    case '.':				/* start with an initial dot */
	state = s_normal;
	dot = TRUE;
	break;

    case '\\':				/* start initially with \ escape */
	save_state = s_normal;
	state = s_cquote;
	break;

    default:				/* otherwise begin in normal state */
	state = s_normal;
	break;
    }

    /*
     * scan until end of token
     */
    while (*ap) {
	switch (state) {

	case s_normal:			/* scan for token delimeter */
	    switch (*ap) {

	    case '\\':			/* \ escape, save state, then cquote */
		save_state = s_normal;
		state = s_cquote;
		break;

	    case '[':			/* domain continue if last char is . */
		if (dot) {
		    state = s_domlit;
		} else {
		    return ap;
		}
		break;

	    case '"':			/* quote continue if last char is . */
		if (dot) {
		    state = s_quote;
		} else {
		    return ap;
		}
		break;

	    case '@':
	    case '!':
	    case '%':
	    case ':':
	    case ',':
	    case '<':
	    case '>':
		return ap;		/* found the end of a token */
	    }
	    /* dot is TRUE if this char was a dot */
	    dot = ('.' == *ap++);
	    break;

	case s_quote:			/* scan for end of a quote */
	    if (*ap == '\\') {
		/* \ escape in quote */
		ap++;
		save_state = s_quote;
		state = s_cquote;
	    } else if (*ap++ == '"') {
		/* end of quote -- check for . after it */
		if (*ap == '.') {
		    /* if exists, continue scanning */
		    state = s_normal;
		} else {
		    /* otherwise we have a complete token */
		    return ap;
		}
	    }
	    break;

	case s_domlit:			/* scan for end of domain literal */
	    if (*ap == '\\') {
		/* \ escape in domain literal */
		ap++;
		save_state = s_domlit;
		state = s_cquote;
	    } else if (*ap++ == ']') {
		/* end of domain literal -- check for . after it */
		if (*ap == '.') {
		    /* if exists, continue scanning */
		    state = s_normal;
		} else {
		    /* otherwise we have a complete token */
		    return ap;
		}
	    }
	    break;

	case s_cquote:			/* process \ escape */
	    ap++;			/* just skip the char */
	    state = save_state;		/* and return to previous state */
	    break;
	}
    }

    /*
     * fell through -- error if we are not in the normal state
     */
    if (state != s_normal) {
	return NULL;
    }

    return ap;				/* all done, return the token */

}


/*
 * back_address_token - scan backward one token in an address
 *
 * see the rules in address_token for how to delimit an address token.
 * This procedure does it going backwards.
 *
 * Note:  this routine is more complex than address_token, because
 *	  addresses are intended to be scanned forward.
 *
 * inputs:
 *	ba	- beginning of an address (firewall)
 *	ap	- pointer to character past end of token
 *
 * output:
 *	return start of token that ap points past.  Return NULL on error.
 *
 * called by: parse_address
 * calls: escaped
 */
char *
back_address_token(ba, ap)
    register char *ba;			/* beginning of address (firewall) */
    register char *ap;			/* character past end of token */
{
    static enum state {			/* states for the state machine */
	s_normal,			/* not in a literal */
	s_quote,			/* scanning quoted literal */
	s_domlit			/* scanning domain literal */
    } state;
    int dot = FALSE;			/* TRUE if next char is unescaped . */
    register char *p;			/* temp */

    /*
     * trap no tokens
     */
    if (ba == ap) {
	return NULL;
    }

    /*
     * setup initial state
     */
    --ap;				/* backup to end of token */
    if ((p = escaped(ba, ap))) {
	/* if last char is escaped, we are in the normal state */
	state = s_normal;
	ap = p;
    } else {
	switch (*ap) {
	case '@':			/* delimiters are one token a piece */
	case '!':
	case '%':
	case ':':
	case ',':
	case '>':
	case '<':
	    return ap;			/* so return that single token */

	case '"':			/* start in a quoted literal */
	    state = s_quote;
	    break;

	case ']':			/* start in a domain literal */
	    state = s_domlit;
	    break;

	case '.':			/* start with an initial dot */
	    state = s_normal;
	    dot = TRUE;
	    break;

	default:			/* otherwise begin in normal state */
	    state = s_normal;
	    break;
	}
	--ap;				/* this char already processed */
    }

    /*
     * scan until beginning of token
     */
    while (ap - ba >= 0) {
	switch (state) {

	case s_normal:			/* scan for token delimeter */
	    /* trap escaped character */
	    if ((p = escaped(ba, ap))) {
		ap = p;
	    } else {
		/* not escaped, process it */
		switch (*ap) {

		case ']':		/* domain okay if next char is . */
		    if (dot) {
			state = s_domlit;
		    } else {
			return ap+1;
		    }
		    break;

		case '"':		/* quote okay if next char is . */
		    if (dot) {
			state = s_quote;
		    } else {
			return ap+1;
		    }
		    break;

		case '@':
		case '!':
		case '%':
		case ':':
		case ',':
		case '>':
		case '<':
		    return ap+1;	/* found the end of a token */
		}
		/* dot is TRUE if this char was a dot */
		dot = ('.' == *ap--);
	    }
	    break;

	case s_quote:			/* scan for end of a quote */
	    if ((p = escaped(ba, ap))) {
		/* trap \ escape */
		ap = p;
	    } else if (*ap-- == '"') {
		/* end of quote -- check for . before it */
		if (ap - ba >= 0 && *ap == '.' && !escaped(ba, ap)) {
		    /* if exists, continue scanning */
		    state = s_normal;
		} else {
		    /* otherwise we have a complete token */
		    return ap+1;
		}
	    }
	    break;

	case s_domlit:			/* scan for end of domain literal */
	    if ((p = escaped(ba, ap))) {
		/* trap \ escape */
		ap = p;
	    } else if (*ap-- == '[') {
		/* end of domain literal -- check for . before it */
		if (ap - ba >= 0 && *ap == '.' && !escaped(ba, ap)) {
		    /* if exists, continue scanning */
		    state = s_normal;
		} else {
		    /* otherwise we have a complete token */
		    return ap+1;
		}
	    }
	    break;
	}
    }

    /*
     * fell through -- error if we are not in the normal state
     */
    if (state != s_normal) {
	return NULL;
    }

    return ap+1;			/* all done, return the token */
}

/*
 * escaped - determine if a character is \ escaped, scanning backward
 *
 * given the beginning of a string and a character positition within
 * it, determine if that character is \ escaped or not, tracing through
 * multiple \ chars if necessary.  Basically, if the character position
 * is preceded by an odd number of \ chars, the current character is
 * \ escaped.
 *
 * inputs:
 *	ba	- beginning of string
 *	ap	- character position in string
 *
 * output:
 *	beginning of set of \ chars previous to ap, or NULL if the
 *	character at ap is not backslash escaped.
 *
 * called by: back_address_token
 */
static char *
escaped(ba, ap)
    register char *ba;			/* beginning of string */
    register char *ap;			/* character position in string */
{
    register unsigned i = 0;		/* count of \ characters */

    /*
     * count the number of preceding \ characters, but don't go past
     * the beginning of the string.
     */
    --ap;
    while (ap - ba >= 0 && *ap == '\\') {
	i++; --ap;
    }

    /* if odd number of \ chars, then backslash escaped */
    return (i%2==1)? ap: NULL;
}


/*
 * alloc_addr - allocate a struct addr
 *
 * NOTE: the caller must setup the addr fields correctly.  This routine
 *	 marks certain fields with improper values, which unless changed,
 *	 will results in other routines doing a panic().
 */
struct addr *
alloc_addr()
{
    register struct addr *addr;		/* our new address */

    /* grab it */
    addr = (struct addr *)xmalloc(sizeof(*addr));

    /* preset the proper values */
    bzero((char *)addr, sizeof(*addr));
    addr->match_count = -1;
    addr->uid = BOGUS_USER;		/* the identity is not known yet */
    addr->gid = BOGUS_GROUP;		/* the identity is not known yet */

    return addr;
}

/*
 * insert_addr_list - insert a list of addrs into another list
 *
 * insert each addr in an input list at the beginning of an output list.
 * In the process or in some addr flags and (possibly) set next_addr
 * to an error message.
 */
void
insert_addr_list(in, out, error)
    register struct addr *in;		/* input list */
    register struct addr **out;		/* output list */
    register struct error *error;	/* error structure (if non-NULL) */
{
    struct addr *next;

    DEBUG(DBG_ADDR_MID, "insert_addr_list() called:\n");
#ifndef NODEBUG
    if (error) {
	DEBUG2(DBG_ADDR_MID, "\tERR%ld: %s\n",
	       error->info & ERR_MASK, error->message);
    }
#endif	/* NODEBUG */
    /* loop over all of the input addrs */
    for (; in; in = next) {
	next = in->succ;

	DEBUG1(DBG_ADDR_MID, "\t%s\n", in->in_addr);
	if (error) {
	    in->error = error;		/* set the error message, if given */
	}
	in->succ = *out;
	*out = in;
    }
}

/*
 * addr_sort - sort an input list of addrs and return the new sorted list
 *
 * calling sequence is:
 *	sorted_list = addr_sort(input_list, OFFSET(addr, tag_name)
 *
 * where tag_name is the (char *) element name in the addr structure to
 * sort on.
 */
static int sort_offset;			/* pass offset to compare function */
struct addr *
addr_sort(in, offset)
    struct addr *in;
    int offset;
{
    struct addr **addrv;		/* array of addresses */
    register int addrc;			/* count of addresses */
    register struct addr **addrp;	/* temp addr pointer */
    register struct addr *a;		/* address list or current address */

    /* pass offset value to addrcmp() by setting file local variable */
    sort_offset = offset;

    /* count the input addresses */
    addrc = 0;
    for (a = in; a; a = a->succ) {
	addrc++;
    }

    /* allocate space for an array for that many pointers */
    addrv = (struct addr **) xmalloc((unsigned) (addrc * sizeof(*addrv)));

    /* build the array from the input list */
    for (addrp = addrv, a = in; a; a = a->succ) {
	*addrp++ = a;
    }

    /* sort the array */
    qsort((char *)addrv, addrc, sizeof(*addrv), addrcmp);

    /*
     * turn the sorted array into a sorted list
     * Start from the end of the array so the generated list will start
     * from the beginning.
     */
    for (addrp = addrv + addrc, a = NULL; addrc > 0; --addrc) {
	(*--addrp)->succ = a;
	a = *addrp;
    }

    return a;
}

/*
 * addrcmp - compare two addr structures based on a field at sort_offset.
 */
static int
addrcmp(a, b)
    char **a;
    char **b;
{
    return strcmp(*(char **)(*a + sort_offset), *(char **)(*b + sort_offset));
}

/*
 * note_error - create an error structure for inclusion in an addr structure
 */
struct error *
note_error(info, message)
    long info;
    char *message;
{
    struct error *ret = (struct error *)xmalloc(sizeof(*ret));

    ret->info = info;
    ret->message = message;

    return ret;
}

#ifdef STANDALONE

int return_to_sender = FALSE;
int exitvalue = 0;
FILE *errfile;

#ifdef DEBUG_LEVEL
int debug = DEBUG_LEVEL;
#else /* DEBUG_LEVEL */
int debug = 0;
#endif /* DEBUG_LEVEL */

/*
 * test the functions in addr by calling parse_address for each
 * argument given to the program.
 */
void
main(argc, argv)
    int argc;				/* count of arguments */
    char **argv;			/* vector of arguments */
{
    char *s;				/* temp string */
    char *addr;				/* preparsed address */
    char *error;			/* error message */
    int form;				/* form from parse_address */
    char *target;			/* target returned by parse_address */
    char *remainder;			/* remainder from parse_address */
    int i;

    errfile = stderr;

    /*
     * if first argument is -s then test the split_addr_list function
     */
    if (argc > 1 && EQ(argv[1], "-s")) {
	struct addr *alist;

	for (i = 2; i < argc; i++) {
	    alist = NULL;
	    split_addr_list(argv[i], &alist);
	    while (alist) {
		printf("%s\n", alist->in_addr);
		alist = alist->succ;
	    }
	}
	exit(0);
    }

    /*
     * if first argument is a number, change the debug level
     */
    if (argc > 1 && isdigit(argv[1][0])) {
	debug = atoi(*++argv);
	argc--;
    }

    /*
     * loop over all arguments or read from standard input if none
     */
    if (argc > 1) {
	while (*++argv) {
	    (void)fprintf(stderr, "input:  %s\n", *argv);

	    /* preparse the address to get rid of mutant forms */
	    addr = preparse_address(*argv, &error);
	    if (addr) {
		(void)fprintf(stderr, "preparse_address: %s\n", addr);
	    } else {
		(void)fprintf(stderr, "preparse_address: %s\n", error);
		break;
	    }

	    /* see what build_uucp_route yields */
	    s = build_uucp_route(addr, &error, 0);
	    if (s) {
		(void)fprintf(stderr, "build_uucp_route: %s\n", s);
	    } else {
		(void)fprintf(stderr, "build_uucp_route: %s\n", error);
	    }

	    /* see what parse_address yields */
	    form = parse_address(addr, &target, &remainder, (int *)0);
	    if (form == LOCAL) {
		(void)printf("LOCAL %s\n", remainder);
	    } else if (form == FAIL) {
		(void)fprintf(stderr, "parse_address: %s\n", remainder);
	    } else {
		(void)printf("REMOTE %s@%s\n", remainder, target);
	    }
	}
    } else {
	char line[4096];

	while (fgets(line, sizeof(line), stdin) != NULL) {
	    (void)fprintf(stderr, "input:  %s\n", line);

	    /* preparse the address to get rid of mutant forms */
	    addr = preparse_address(line, &error);
	    if (addr) {
		(void)fprintf(stderr, "preparse_address: %s\n", addr);
	    } else {
		(void)fprintf(stderr, "preparse_address: %s\n", error);
		break;
	    }

	    /* see what build_uucp_route yields */
	    s = build_uucp_route(addr, &error, 0);
	    if (s) {
		(void)fprintf(stderr, "build_uucp_route: %s\n", s);
	    } else {
		(void)fprintf(stderr, "build_uucp_route: %s\n", error);
	    }

	    /* see what parse_address yields */
	    form = parse_address(addr, &target, &remainder, (int *)0);
	    if (form == LOCAL) {
		(void)printf("LOCAL %s\n", remainder);
	    } else if (form == FAIL) {
		(void)fprintf(stderr, "parse_address: %s\n", remainder);
	    } else {
		(void)printf("REMOTE %s@%s\n", remainder, target);
	    }
	}
    }

    exit(exitvalue);
}

#endif	/* STANDALONE */
