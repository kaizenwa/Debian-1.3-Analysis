/* $Id: addr.c,v 1.3 1993/10/28 16:49:51 chip Exp $
 *
 * Operations on addresses, (ASCII representations of destinations).
 *
 * $Log: addr.c,v $
 * Revision 1.3  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.2  1991/05/23  17:23:19  chip
 * Follow RFC822 definition of header syntax.
 * Guard isxxx() macros against negative values.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"

/*----------------------------------------------------------------------
 * Check an address for cleanliness.
 * That is, make sure that it is devoid of shell metacharacters.
 * And make sure it's got at least one character, too.
 */

int
addr_clean(addr)
char *addr;
{
    char *p;
    static char sanitize[] = SANITIZE;

    if (*addr == 0)
	return FALSE;

    for (p = addr; *p; ++p)
    {
	if (strchr(sanitize, *p))
	    return FALSE;
    }

    return TRUE;
}

/*----------------------------------------------------------------------
 * Report the class of a string representation of a destination.
 */

DCLASS
addr_class(s)
char *s;
{
    char *p;

    for (p = s; *p; ++p)
    {
	switch (*p)
	{
	case '!':
	    return CL_UUCP;
	case ':':
	    return CL_MBOX;
	case '|':
	    return CL_PROG;
	case '?':
	    return CL_USER;
	}
    }

    return CL_USER;
}

/*----------------------------------------------------------------------
 * Parse a string representation of a destination.
 * If dfile_ct is non-NULL, then a missing user name refers to that user;
 *	otherwise, a missing user name refers to the effective user id.
 * If dfile_ct is NULL, then we only permit user names and UUCP addresses;
 *      otherwise, we permit all kinds of addresses.
 */

DEST *
addr_dest(s, dfile_ct)
char *s;
CONTEXT *dfile_ct;
{
    DEST *d;
    CONTEXT *ct;
    char *buf, *p, *q, *user, *param;
    char sep;
    int i, legal;

    /*
     * If this is NOT a delivery file, restrict address class
     * to user and UUCP.
     */

    if (!dfile_ct)
    {
	DCLASS class;

	class = addr_class(s);
	if ((class != CL_USER && class != CL_UUCP)
	    || strcmp(s, DFILE_DROP) == 0)
	{
	    d = dest(s, CL_USER, (char *) NULL);
	    dest_err(d, E_DFONLY);
	    return d;
	}
    }

    /*
     * Find the additional information after the user name.
     * Bang means UUCP, colon means mailbox, pipe means pipe,
     * question means error.
     */

    for (i = 0; (sep = s[i]) != 0; ++i)
    {
	if (sep == '!' || sep == ':' || sep == '|' || sep == '?')
	    break;
    }

    /*
     * Deal with UUCP paths before the user-name parsing begins.
     */

    if (sep == '!')
	return dest(s, CL_UUCP, (char *) NULL);

    /*
     * Copy string to local buffer so we can mangle it.
     * Chop it up into user name and parameter.
     */

    buf = copystr(s);
    if (buf[i])
	buf[i++] = 0;
    user = buf;
    param = buf + i;

    /*
     * If user name is blank, provide default.
     */

    if (*user == 0)
	user = (dfile_ct ? dfile_ct : eff_ct)->ct_name;

    /*
     * Determine if we can get there from here.
     */

    if ((ct = name_context(user)) == NULL)
	legal = TRUE;		/* we can report "no such user" later */
    else if (dfile_ct)
	legal = ok_context(dfile_ct->ct_uid, dfile_ct->ct_uid,
			   dfile_ct->ct_gid, ct);
    else
	legal = ok_context(eff_uid, real_uid, real_gid, ct);

    /*
     * Now take action based on address class.
     */

    /* Could be "user|program" ... */

    if (sep == '|')
	d = legal ? dest(user, CL_PROG, param) : NULL;

    /* Could be "user?error" ... */

    else if (sep == '?')
    {
	if (legal)
	{
	    d = dest(user, CL_USER, (char *) NULL);
	    dest_err(d, E_ERRMSG);
	    d->d_errmsg = copystr(param);
	}
	else
	    d = NULL;
    }

    /* Must be "user" or "user:mailbox" */

    else
    {
	/*
	 * Strip whitespace, eliminate duplicate slashes from
	 * mailbox filename.
	 */

	p = q = param;
	while (*p)
	{
	    if (isspace(*p & 0xFF))
		++p;
	    else if ((*q++ = *p++) == '/')
	    {
		while (*p == '/')
		    ++p;
	    }
	}
	*q = 0;

	/*
	 * Now that blanks are gone, we decide mailbox or not.
	 * Delaying this test means that "user" equals "user: ".
	 */

	if (*param)
	    d = legal ? dest(user, CL_MBOX, param) : NULL;
	else
	    d = dest(user, CL_USER, (char *) NULL);
    }

    /*
     * We allocated the buffer; now we free it.
     */

    free(buf);

    /* Return parsed destination. */

    return d;
}
