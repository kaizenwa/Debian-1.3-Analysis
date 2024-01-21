/* $Id: debug.c,v 1.2 1993/10/28 16:49:51 chip Exp $
 *
 * Debugging output.
 *
 * $Log: debug.c,v $
 * Revision 1.2  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"

/*----------------------------------------------------------------------
 * Print out a complete dump of all destinations
 */

void
dumpdests(when)
char *when;
{
    DEST *d, **dv;
    int i, count;

    message("Destinations %s:\n", when);

    /*
     * Get and sort the array of destinations.
     */

    if ((dv = dest_array(&count)) == NULL)
    {
	message("\tnone\n");
	return;
    }

    /*
     * Now print them in the order we just created.
     */

    for (i = 0; i < count; ++i)
    {
	char *e;
	int len;

	d = dv[i];

	message("\t%s", d->d_name);

	switch (d->d_class)
	{
	case CL_USER:
	    /* it's understood */
	    break;
	case CL_MBOX:
	    message(", mailbox='%s'", d->d_param);
	    break;
	case CL_UUCP:
	    message(" (UUCP)");
	    break;
	case CL_PROG:
	    message(", program='%s'", d->d_param);
	    break;
	}

	e = (d->d_state == ST_ERROR) ? derrmsg(d) : "";
	len = strlen(d->d_name) + strlen(e);
	if (d->d_param)
	    len += strlen(d->d_param);
	message(":%s", (len > 60) ? "\n\t\t" : " ");

	switch (d->d_state)
	{
	case ST_WORKING:
	    message("Working");
	    break;
	case ST_HOLD:
	    message("Hold");
	    break;
	case ST_DONE:
	    message("Done");
	    break;
	case ST_ERROR:
	    message("Error (%s)", derrmsg(d));
	    break;
	}
	message("\n");
    }

    /* It's our job to free the array. */

    free((char *) dv);
}
