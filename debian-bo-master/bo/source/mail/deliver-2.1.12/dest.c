/* $Id: dest.c,v 1.2 1993/10/28 16:49:51 chip Exp $
 *
 * Operations on the list of mail destinations.
 *
 * $Log: dest.c,v $
 * Revision 1.2  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"

/*
 * Local data.
 */

static DEST deadhead = { &deadhead, &deadhead };
#define HEADPTR	(&deadhead)

/*
 * Local functions.
 */

static int destcmp();
static int ptrcmp();

/*----------------------------------------------------------------------
 * Consider mail to the given user as undeliverable.
 */

DEST *
dest_undel(name)
char *name;
{
    return dest(name, CL_MBOX, MBX_UNDEL);
}

/*----------------------------------------------------------------------
 * Add a new destination to the list (unless it already exists).
 * Return pointer to DEST.
 */

DEST *
dest(name, class, s)
char *name;
DCLASS class;
char *s;
{
    DEST *d;

    /* Make sure that parameter is provided when it's needed. */

    if ((class == CL_MBOX || class == CL_PROG) != (s != NULL))
	return NULL;

    /* Look for an already-existing copy of the given destination. */

    for (d = HEADPTR->d_next; d != HEADPTR; d = d->d_next)
    {
	if (d->d_class != class)
	    continue;

	if (strcmp(d->d_name, name) != 0)
	    continue;

	if (s && strcmp(d->d_param, s) != 0)
	    continue;

	/*
	 * Like, gnarly, dude!  It's already in the chain!
	 */

	return d;
    }

    /*
     * The given dest isn't in the list, so we have to add it.
     */

    d = talloc(DEST, 1);
    d->d_class = class;
    d->d_state = ST_WORKING;
    d->d_name = copystr(name);
    d->d_param = s ? copystr(s) : NULL;

    /*
     * Check address for validity.
     */

    if (!addr_clean(name))
	dest_err(d, E_IVADDR);
    else if (class != CL_UUCP && name_context(name) == NULL)
	dest_err(d, E_NSUSER);

    /*
     * Put new address at the end of of the chain.
     * (This is important!  Other code depends on it.)
     */

    d->d_prev = HEADPTR->d_prev;
    d->d_next = HEADPTR;
    d->d_prev->d_next = d;
    d->d_next->d_prev = d;

    return d;
}

/*----------------------------------------------------------------------
 * Return pointer to first DEST in the list.
 */

DEST *
first_dest()
{
    if (HEADPTR->d_next != HEADPTR)
	return HEADPTR->d_next;

    return NULL;
}

/*----------------------------------------------------------------------
 * Return pointer to next DEST in the list, or NULL.
 */

DEST *
next_dest(d)
DEST *d;
{
    if (d && (d = d->d_next) != HEADPTR)
	return d;

    return NULL;
}

/*----------------------------------------------------------------------
 * Return the number of destinations in the list.
 */

int
dest_count()
{
    DEST *d;
    int count;

    count = 0;
    for (d = HEADPTR->d_next; d && d != HEADPTR; d = d->d_next)
	++count;

    return count;
}

/*----------------------------------------------------------------------
 * Return an allocated array of DEST pointers, or NULL if none.
 * The given integer is set to the array size.
 * Note that the caller must FREE this array when he's done.
 */

DEST **
dest_array(countp)
int *countp;
{
    DEST **dv, *d;
    int i, count;

    if ((count = dest_count()) <= 0)
    {
	*countp = 0;
	return NULL;
    }

    dv = talloc(DEST *, count);

    i = 0;
    for (d = HEADPTR->d_next; d && d != HEADPTR; d = d->d_next)
    {
	if (i < count)
	    dv[i++] = d;
    }

    qsort((char *) dv, (unsigned) count, sizeof(dv[0]), destcmp);

    *countp = count;
    return dv;
}

/*----------------------------------------------------------------------
 * Compare two DEST pointers, for output sorting.
 */

static int
destcmp(p1, p2)
char *p1, *p2;
{
    DEST *d1, *d2;
    int cmp;

    d1 = *(DEST **) p1;
    d2 = *(DEST **) p2;

#if 0
    /* Errors go last. */
    if ((d1->d_state == ST_ERROR) != (d2->d_state == ST_ERROR))
	return (d1->d_state == ST_ERROR) ? 1 : -1;
#endif

#if 0
    /* By class. */
    if ((cmp = (int) d1->d_class - (int) d2->d_class) != 0)
	return (cmp < 0) ? -1 : 1;
#endif

    /* By user name. */
    if ((cmp = ptrcmp(d1->d_name, d2->d_name)) != 0)
	return cmp;

    /* By mailbox/program. */
    if ((cmp = ptrcmp(d1->d_param, d2->d_param)) != 0)
	return cmp;

    return 0;
}

/*----------------------------------------------------------------------
 * Compare two pointers, either of which may be NULL.
 */

static int
ptrcmp(p, q)
char *p, *q;
{
    if (p == q)
	return 0;
    if (p == NULL)
	return -1;
    if (q == NULL)
	return 1;
    return strcmp(p, q);
}

/*----------------------------------------------------------------------
 * Return a destination's error message.
 */

char *
derrmsg(d)
DEST *d;
{
    static char unknown_buf[40];
    static char no_error[] = "no error?!";

    if (!d || d->d_state != ST_ERROR)
	return no_error;

    switch (d->d_error)
    {
    case E_IVADDR:
	return "Invalid address string";
    case E_NSUSER:
	return "No such user";
    case E_NSHOST:
	return "No such UUCP host";
    case E_CTPERM:
	return "No permissions for that context";
    case E_CTLOST:
	return "Context lost (should never happen)";
    case E_MBOX:
	return "Can't write to mailbox";
    case E_PROG:
	return "Subprocess reported failure when exiting";
    case E_PIPE:
	return "Subprocess died while reading its standard input";
    case E_DFONLY:
	return "Address not valid from command line";
    case E_ERRMSG:
	return d->d_errmsg ? d->d_errmsg : no_error;
    }

    (void) sprintf(unknown_buf, "Unknown error %d", d->d_error);
    return unknown_buf;
}
