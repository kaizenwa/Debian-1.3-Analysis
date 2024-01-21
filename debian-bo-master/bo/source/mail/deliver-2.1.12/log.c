/* $Id: log.c,v 1.7 1993/10/28 16:49:51 chip Exp $
 *
 * Deliver logging.
 *
 * $Log: log.c,v $
 * Revision 1.7  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.6  1991/11/25  20:49:42  chip
 * Prettify "Undel.mail" message for home directory of root.
 *
 * Revision 1.5  1991/11/12  20:44:47  chip
 * Use new logsize() function.  Seek to end of log before
 * writing to it or testing its size, since a child process
 * might have grown it.
 *
 * Revision 1.4  1991/08/27  15:39:45  chip
 * Use tmzone().
 *
 * Revision 1.3  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include <time.h>

static void logundel();
static void logheader();

/*----------------------------------------------------------------------
 * Open temporary log files.
 */

void
openlogs()
{
#ifdef ERRLOG
    /* If we're delivering and not being verbose, keep an error log. */

    if (!dryrun && !verbose)
    {
	t_errlogfile = tempfile();
	if ((errlog = ftcreate(t_errlogfile)) == NULL)
	    syserr("can't create %s", t_errlogfile);
    }
#endif

#ifdef LOG
    /* If we're delivering and the log file exists, keep data for it. */

    if (!dryrun && exists(f_logfile))
    {
	t_logfile = tempfile();
	if ((log = ftcreate(t_logfile)) == NULL)
	    syserr("can't create %s", t_logfile);
    }
#endif
}

/*------------------------------------------------------------------------
 * Discard temporary log files.
 */

void
tosslogs()
{
    if (t_logfile && unlink(t_logfile) == -1)
	syserr("can't remove %s", t_logfile);

    if (t_errlogfile && unlink(t_errlogfile) == -1)
	syserr("can't remove %s", t_errlogfile);
}

/*----------------------------------------------------------------------
 * Save contents of temporary logs in the real logfiles.
 */

void
savelogs()
{
    if (!log && !errlog)
	return;

    /* If temporary logs contain anything, append them to real logs. */

    if (logsize(log) || logsize(errlog))
    {
	if (log_lock() == 0)
	{
	    if (t_logfile)
		applog(&log, f_logfile);
	    errdone();
	    if (t_errlogfile)
		applog(&errlog, f_errlogfile);
	    (void) log_unlock();
	}
    }
}

/*----------------------------------------------------------------------
 * Append a temporary log file to a real logfile.
 * We pass a FILE **, so that it can be set to NULL when closed;
 * this is important, since errlog is used by syserr().
 *
 * Note:  It is assumed that the caller already called log_lock().
 * Nowever, on systems where kernel locking and modern open() are
 * available, log_lock() is just a nop.  We call k_lock() here,
 * which does the real locking on modern systems.
 */

void
applog(fpp, realfile)
FILE **fpp;
char *realfile;
{
    FILE *fp = fpp ? *fpp : NULL;
    int fd, realfd;

    /* If log data weren't kept, never mind. */

    if (fp == NULL)
	return;

    /* Flush buffered data. */

    (void) fflush(fp);

    /* If the file is empty, never mind. */

    if (logsize(fp) == 0)
    {
	(void) fclose(fp);
	*fpp = NULL;
	return;
    }

    /* Get an fd and close the stream. */

    if ((fd = dup(fileno(fp))) == -1)
    {
	syserr("can't dup log fd");
	(void) fclose(fp);
	*fpp = NULL;
	return;
    }
    (void) fclose(fp);
    *fpp = NULL;

    /*
     * Open the real logfile, creating it if necessary.
     *
     * Note that there is no race condition here, since if safe
     * creation isn't available, the logs are already locked.
     * See log_lock() and log_unlock() in lock.c.
     */

#ifdef SAFE_CREATE
    realfd = open(realfile, O_WRONLY | O_CREAT, 0666);
#else
    if ((realfd = open(realfile, O_WRONLY)) == -1)
	realfd = creat(realfile, 0666);
#endif
    if (realfd == -1)
	syserr("can't open %s for writing", realfile);
    else
    {
	if (k_lock(realfd) == -1)
	    syserr("can't lock %s", realfile);
	else
	{
	    /* Append the temporary log to the real log. */

	    (void) lseek(fd, 0L, 0);
	    (void) lseek(realfd, 0L, 2);
	    (void) copyfd(fd, realfd);
	    (void) k_unlock(realfd);
	}
	(void) close(realfd);
    }

    /* Close the temporary log. */

    (void) close(fd);
}

/*----------------------------------------------------------------------
 * Write a report to the log file.
 */

void
logreport(ac, av)
int ac;
char **av;
{
    int a;

    if (!log)
	return;

    logstart(log);

    if (local_sender)
	(void) fprintf(log, "local sender: %s\n", local_sender);
    if (orig_sender)
	(void) fprintf(log, "original sender: %s\n", orig_sender);
    if (boxdelivery)
	(void) fprintf(log, "mailbox%s:", (ac > 1) ? "es" : "");
    else
	(void) fprintf(log, "destination%s:", (ac > 1) ? "s" : "");
    for (a = 0; a < ac; ++a)
	(void) fprintf(log, " \"%s\"", av[a]);
    (void) fputc('\n', log);

    logstate("delivered", ST_DONE);
    logstate("failed", ST_ERROR);

    logdone(log);
}

/*----------------------------------------------------------------------
 * Log the destinations with the given state.
 * If any are found, the list is prefixed with the given description.
 */

void
logstate(desc, state)
char *desc;
DSTATE state;
{
    DEST *d;
    int dcount;

    dcount = 0;
    for (d = first_dest(); d; d = next_dest(d))
    {
	if (d->d_state != state)
	    continue;

	if (++dcount == 1)
	    (void) fprintf(log, "%s:", desc);
	(void) fprintf(log, " %s", d->d_name);
	if (d->d_class == CL_MBOX)
	    (void) fprintf(log, ":%s", d->d_param);
	else if (d->d_class == CL_PROG)
	    (void) fprintf(log, "|\"%s\"", d->d_param);
    }
    if (dcount)
	(void) fputc('\n', log);
}

/*----------------------------------------------------------------------
 * Record any interesting information in the error log file.
 */

void
logerrinfo()
{
    if (!errlog)
	return;

    /* Log undelivered mail. */

    logundel();

    /* If any errors have been logged, record the failed header. */

    if (logsize(errlog))
	logheader();
}

/*----------------------------------------------------------------------
 * Log undelivered mail.
 *
 * Note that this algorithm assumes that delivery to the MBX_UNDEL mailbox
 * is always worth reporting.
 */

static void
logundel()
{
    DEST *d;

    if (!errlog)
	return;

    for (d = first_dest(); d; d = next_dest(d))
    {
	if (d->d_state == ST_DONE
	    && d->d_class == CL_MBOX
	    && strcmp(d->d_param, MBX_UNDEL) == 0)
	{
	    CONTEXT *ct;
	    char *home;

	    if ((ct = name_context(d->d_name)) != NULL)
		home = ct->ct_home;
	    else
		home = "~";	/* should never happen */

	    errstart();
	    (void) fprintf(errlog, "Undelivered mail for %s put in %s/%s\n",
			   d->d_name, (strcmp(home, "/") == 0) ? "" : home,
			   MBX_UNDEL);
	}
    }
}

/*----------------------------------------------------------------------
 * Log the message header.
 */

static void
logheader()
{
    FILE *hfp;
    int hfd;

    if (!errlog)
	return;

    /* Copy the failed message's header (if any). */

    if (tfd[T_HDR] == -1)
	return;

    hfd = dup(tfd[T_HDR]);
    hfp = (hfd < 0) ? NULL : fdopen(hfd, "r");
    if (hfp == NULL)
    {
	(void) fprintf(errlog, "%s: can't open header file %s\n",
		       progname, tfile[T_HDR]);
    }
    else
    {
	int c, oc;

	(void) fprintf(errlog, "+ Header:\n");

	(void) fseek(hfp, 0L, 0);
	oc = '\n';
	while ((c = getc(hfp)) != EOF)
	{
	    if (oc != '\n' || c != '\n')
	    {
		if (oc == '\n')
		    (void) fputs("| ", errlog);
		(void) putc(c, errlog);
	    }
	    oc = c;
	}

	(void) fclose(hfp);
    }
}

/*----------------------------------------------------------------------
 * Record a time stamp in the error log file.
 */

void
errstart()
{
    if (!errlog)
	return;

    /* If we've already written a time stamp, don't do it again. */

    if (logsize(errlog))
	return;

    /* Write a time stamp and various useful info. */

    logstart(errlog);
    (void) fprintf(errlog, "process %d", getpid());
    if (rec_parent > 0)
	(void) fprintf(errlog, ", parent %d", rec_parent);
    (void) fprintf(errlog, ": %s %s\n", progname, version);
}

/*----------------------------------------------------------------------
 * Record the end of this process's error log entry.
 */

void
errdone()
{
    if (!errlog)
	return;

    /* If we never wrote to the error log file, do nothing. */

    if (logsize(errlog) == 0)
	return;

    /* Write a simple closing line for the error log entry. */

    (void) fprintf(errlog, "process %d", getpid());
    if (rec_parent > 0)
	(void) fprintf(errlog, ", parent %d", rec_parent);
    (void) fprintf(errlog, ": exit\n");

    logdone(errlog);
}

/*----------------------------------------------------------------------
 * Start a log entry.
 * Various useful info goes here -- especially a timestamp.
 */

void
logstart(fp)
FILE *fp;
{
    struct tm *lt;
    time_t now;
    static char month[12][4] =
    {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    };

    (void) time(&now);
    lt = localtime(&now);

    (void) fputc('\n', fp);
    if (rec_level)
	(void) fprintf(fp, "[%d]", rec_level);
    else
	(void) fputs("---", fp);
    (void) fputs("------------------------ ", fp);
    (void) fprintf(fp, "%d %s %d, %02d:%02d:%02d %s\n",
		   lt->tm_mday, month[lt->tm_mon], lt->tm_year + 1900,
		   lt->tm_hour, lt->tm_min, lt->tm_sec, tmzone(lt));
}

/*----------------------------------------------------------------------
 * Write a concluding marker to the given logfile.
 * This marker separates instances of Deliver at recursion level zero.
 */

void
logdone(fp)
FILE *fp;
{
    if (rec_level == 0)
	(void) fputs("===========================\n\n", fp);
}

/*----------------------------------------------------------------------
 * Report the size of an open log file.
 * Incidentally, seek to the end.
 */

long
logsize(fp)
FILE *fp;
{
    long pos;

    if (!fp)
	return 0L;

    if (fseek(fp, 0L, 2) == -1)
	return 0;

    pos = ftell(fp);
    return (pos == -1) ? 0 : pos;
}
