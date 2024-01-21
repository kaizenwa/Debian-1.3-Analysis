/* $Id: uucp.c,v 1.3 1992/01/20 20:36:05 chip Exp $
 *
 * Handle mail destined for other hosts via UUCP.
 * Deliver is intended as a very low-level program, so we don't
 * do anything fancy here.  We just hand the message to uux.
 *
 * $Log: uucp.c,v $
 * Revision 1.3  1992/01/20  20:36:05  chip
 * Allow for UUX_OPTS to be a list.
 * Support UUX_DASH_A, so UUCP failure messages are mailed to original sender.
 *
 * Revision 1.2  1991/10/23  20:09:26  chip
 * Use tdup() to duplicate temp file fds.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"

/*
 * Local functions.
 */

static char *find_uux();
static int uucp_copy();

/*
 * Local data.
 */

#ifdef UUX_OPTS
static char *uux_opts[] = { UUX_OPTS };
#define UUX_OPTCOUNT (sizeof(uux_opts) / sizeof(uux_opts[0]))
#else
#define UUX_OPTCOUNT 0
#endif

/*----------------------------------------------------------------------
 * Send mail to UUCP addresses (if any).
 * Return count of UUCP addresses for which delivery was attempted.
 */

int
uucp_deliver()
{
    DEST *d;
    char *uav[UUX_ARGCOUNT + UUX_OPTCOUNT + 8];/* arguments for execv() */
    char **av;			/* remote addresses in uav[] */
    DEST *dv[UUX_ARGCOUNT];	/* destinations in av[] */
    char rmail[UUCP_NAMESIZE + 8];	/* "sysname!rmail" */
    char *uux;
    int uucpcount;

    uux = find_uux();

    av = uav;
    *av++ = "uux";

#ifdef UUX_OPTS
    {
	int i;

	for (i = 0; i < UUX_OPTCOUNT; ++i)
	    *av++ = uux_opts[i];
    }
#endif

#ifdef UUX_DASH_A
    {
	char *s, *p;

	/* Send failure notices to original sender, not local sender. */
	s = orig_sender;

	/* Strip smail-generated "hostname!" unless address contains "@". */
	if (strchr(s, '@') == NULL)
	{
	    size_t hostlen;

	    hostlen = strlen(hostname);
	    while (strncmp(s, hostname, hostlen) == 0 && s[hostlen] == '!')
		s += hostlen + 1;
	}

	/* Generate "-asender" option. */
	p = zalloc(sizeof("-a") + strlen(s));
	strcpy(p, "-a");
	strcat(p, s);
	*av++ = p;
    }
#endif

    *av++ = "-";
    *av++ = rmail;

    /*
     * Look for a UUCP address that is "working".  If we find one,
     * then we scan the rest of the list for other UUCP addresses
     * that begin with the same first hop.  If we find any, then
     * we handle them too.  Note that as we continue scanning,
     * we'll find those same addresses again; that's okay, though,
     * because their status fields will report that they're
     * already done.  Cool, eh?
     */

    uucpcount = 0;

    for (d = first_dest(); d; d = next_dest(d))
    {
	FILE *uux_fp;
	DEST *ud;
	DERROR e;
	char *bang;
	unsigned namesize;
	int argcount, argsize, problem, a;

	if (d->d_class != CL_UUCP || d->d_state != ST_WORKING)
	    continue;

	++uucpcount;

	if (printaddrs)
	    (void) printf("%s\n", d->d_name);

	if (dryrun)
	{
	    d->d_state = ST_DONE;
	    continue;
	}

	/*
	 * This is the first destination with the given system
	 * as the first hop.  Generate the rmail command.
	 */

	if ((bang = strchr(d->d_name, '!')) == NULL
	    || (namesize = bang - d->d_name) > UUCP_NAMESIZE)
	{
	    dest_err(d, E_NSHOST);
	    continue;
	}

	(void) strncpy(rmail, d->d_name, namesize);
	(void) strcpy(rmail + namesize, "!rmail");

	/*
	 * Now keep looking for addresses until a limit is reached,
	 * either max arguments or max argument size.
	 * We'll find them again, but their statuses will prevent us
	 * from trying to mail to them twice.
	 */

	argcount = 0;
	argsize = 0;
	for (ud = d; ud; ud = next_dest(ud))
	{
	    char *rest, *arg;

	    if (ud->d_class != CL_UUCP
		|| ud->d_state != ST_WORKING)
		continue;

	    if (strncmp(ud->d_name, d->d_name, namesize + 1) != 0)
		continue;

	    /*
	     * We have a match!  (Or, it could be the first one.)
	     * Be sure we don't exceed our configured maxima,
	     * except for the first address, which always goes.
	     */

	    rest = ud->d_name + namesize + 1;

	    if (argcount > 0)
	    {
		if (argcount + 1 > UUX_ARGCOUNT
		    || argsize + strlen(rest) + 2 > UUX_ARGSIZE)
		    break;
	    }

	    /*
	     * Generate a uux argument and save the destination.
	     */

	    arg = zalloc((unsigned) 3 + strlen(rest));
	    (void) sprintf(arg, "(%s)", rest);

	    av[argcount] = arg;
	    dv[argcount] = ud;

	    /*
	     * Keep track of arg count and total size.
	     */

	    ++argcount;
	    argsize += strlen(arg);
	}

	av[argcount] = NULL;

	/*
	 * Do the dirty deed.
	 * We have to remember the error code as a variable,
	 * since it may apply to multiple destinations.
	 */

	problem = 0;
	e = E_PIPE;		/* default error */

	if ((uux_fp = ct_fopenv(real_ct, uux, uav, "w")) == NULL)
	    problem = 1;
	else
	{
	    if (uucp_copy(uux_fp) < 0)
		problem = 1;

	    if (ct_fclose(uux_fp))
	    {
		/* "No such host" overrides piping problems. */
		e = E_NSHOST;
		problem = 1;
	    }
	}

	/*
	 * We're done.  Update each destination's status.
	 */

	for (a = 0; a < argcount; ++a)
	{
	    free(av[a]);

	    if (problem)
		dest_err(dv[a], e);
	    else
		dv[a]->d_state = ST_DONE;
	}

	/* Track the correct count of UUCP addresses found. */

	uucpcount += argcount - 1;
    }

    return uucpcount;
}

/*----------------------------------------------------------------------
 * Where is uux?
 * This function is allowed to be wrong; if a user without a
 * uux binary is trying to send UUCP mail, he's got problems.
 */

static char *
find_uux()
{
    static char uux1[] = "/bin/uux";
    static char uux2[] = "/usr/bin/uux";

    return exists(uux1) ? uux1 : uux2;
}

/*----------------------------------------------------------------------
 * Write the message for UUCP transmission to the given file.
 */

static int
uucp_copy(ofp)
FILE *ofp;
{
    FILE *ifp;
    char *p;
    register int c;
    int fd;
    char buf[BUFSIZ];

    if ((fd = tdup(tfd[T_HDR], ttype[T_HDR])) == -1)
	return -1;
    (void) lseek(fd, 0L, 0);
    if ((ifp = fdopen(fd, "r")) == NULL)
    {
	error("can't fdopen %s fd", ttype[T_HDR]);
	(void) close(fd);
	return -1;
    }

    /*
     * Copy the header, but tack "remote from" onto the end of the
     * From_ line.  (If it weren't for dealing with the From_ line,
     * I'd skip stream I/O altogether and use read/write.  Maybe
     * I should save the length of the From_ line when I copy it...)
     */

    (void) fgets(buf, GETSIZE(buf), ifp);
    if ((p = strchr(buf, '\n')) != NULL)
	*p = 0;

    if ((p = skipfrom(buf)) == NULL)
	(void) fputs(buf, ofp);	/* should never happen */
    else
    {
	unsigned hlen;

	hlen = strlen(hostname);
	if (strncmp(p, hostname, hlen) == 0 && *(p + hlen) == '!')
	    p += hlen + 1;
	(void) fputs("From ", ofp);
	(void) fputs(p, ofp);
    }

    (void) fprintf(ofp, " remote from %s\n", hostname);

    while ((c = getc(ifp)) != EOF)
	(void) putc(c, ofp);

    (void) fclose(ifp);

    /*
     * Copy the body
     */

    if ((fd = tdup(tfd[T_BODY], ttype[T_BODY])) == -1)
	return -1;
    (void) lseek(fd, 0L, 0);
    if ((ifp = fdopen(fd, "r")) == NULL)
    {
	error("can't fdopen %s fd", ttype[T_BODY]);
	(void) close(fd);
	return -1;
    }

    while ((c = getc(ifp)) != EOF)
	(void) putc(c, ofp);

    (void) fclose(ifp);
    return 0;
}
