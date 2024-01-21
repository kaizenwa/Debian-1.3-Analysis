/* $Id: copymsg.c,v 1.6 1991/11/12 20:43:30 chip Exp $
 *
 * Take the message from standard input and write it to two temp files,
 * one for the header (including the empty line) and one for the body.
 *
 * $Log: copymsg.c,v $
 * Revision 1.6  1991/11/12  20:43:30  chip
 * Ignore return value of fflush().
 *
 * Revision 1.5  1991/10/23  20:02:46  chip
 * Use tdup().
 *
 * Revision 1.4  1991/08/27  15:39:15  chip
 * Add support for SYSV_FROM.
 *
 * Revision 1.3  1991/06/20  12:43:19  chip
 * Check return value of fclose().
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
#include <time.h>

/*----------------------------------------------------------------------
 * Copy the message on the standard input to two temp files:
 * one for the header and one for the body.
 */

int
copy_message()
{
    char buf[BUFSIZ];
    FILE *dfp[T_MAX];
    CONTEXT *sender_ct;
    char *p, *from_line, *fsender, *fdate, *fremote;
    int t, b, c, nl;
    int ret = 0;

    /*
     * Create temporary files to hold the header and message body.
     */

    for (t = T_HDR; t <= T_BODY; ++t)
    {
	int fd;

	tfile[t] = tempfile();
	if ((tfd[t] = tcreate(tfile[t])) == -1)
	    return -1;

	if ((fd = tdup(tfd[t], ttype[t])) == -1)
	    return -1;
	(void) lseek(fd, 0L, 0);
	if ((dfp[t] = fdopen(fd, "r+")) == NULL)
	{
	    error("can't fdopen %s fd", ttype[t]);
	    (void) close(fd);
	    return -1;
	}
    }

    /* Debugging message for later examination of temp files. */

    if (verbose)
    {
	message("header=%s, body=%s\n",
		tfile[T_HDR], tfile[T_BODY]);
    }

    /*
     * If there is a From_ line, find the sender name therein.
     */

    from_line = NULL;
    fsender = fdate = fremote = NULL;

    b = (fgets(buf, GETSIZE(buf), stdin) ? TRUE : FALSE);

    if (b && skipfrom(buf) && (p = strchr(buf, '\n')) != NULL)
    {
	b = FALSE;

	/* Make a mungable copy of the From_ line */

	from_line = copystr(buf);
	if ((p = strchr(from_line, '\n')) != NULL)
	    *p = 0;

	/* Find sender */

	p = from_line + FROMSIZE;
	while (isspace(*p & 0xFF))
	    ++p;
	fsender = p;
	while (*p && !isspace(*p & 0xFF))
	    ++p;
	if (*p)
	    *p++ = 0;

	/* Date received should be around here somewhere */

	fdate = p;

	/* Find 'remote from' phrase (if any) */

	for (; (p = strchr(p, 'r')) != NULL; ++p)
	{
	    if (strncmp(p, "remote from", 11) == 0)
	    {
		*p = 0;
		p += 11;
		while (isspace(*p & 0xFF))
		    ++p;
		if (*p)
		    fremote = p;
		break;
	    }
	}

	/*
	 * Advance to first non-space in date.
	 * Strip trailing spaces from date.
	 * If there is no date, clear the date pointer.
	 */

	while (isspace(*fdate & 0xFF))
	    ++fdate;
	p = fdate + strlen(fdate);
	while (p > fdate && isspace(*(p - 1) & 0xFF))
	    *--p = 0;
	if (*fdate == 0)
	    fdate = NULL;

	/*
	 * If sender is missing, or if date is invalid,
	 * we consider the entire From_ line invalid.
	 */

	if (*fsender == 0
	    || (fdate != NULL && unctime(fdate) == -1))
	{
	    /* Ignore everything we found. */

	    fsender = fdate = fremote = NULL;

	    /* Print invalid From_ line in a harmless way. */

	    (void) strcpy(from_line, buf);
	    (void) strcpy(buf, "Invalid-UUCP-From: ");
	    (void) strcat(buf, from_line);
	    b = TRUE;
	}
    }

    /*
     * Write a From_ line to the header file.
     */

    /* if caller specified original sender, use it */
    if (orig_sender)
	;			/* fine */

    /* else if we found a From_ line, use it */
    else if (fsender)
    {
	if (fremote)
	{
	    orig_sender = zalloc(strlen(fremote) + sizeof("!")
				 + strlen(fsender));
	    (void) sprintf(orig_sender, "%s!%s", fremote, fsender);
	}
	else
	    orig_sender = copystr(fsender);
    }

    /* else use the local sender */
    else
	orig_sender = local_sender;

    /* debugging message */

    if (verbose && strcmp(orig_sender, local_sender) != 0)
	message("original sender is \"%s\"\n", orig_sender);

    /*
     * Finally!  Write the From_ line.
     */

    (void) fputs("From ", dfp[T_HDR]);
    (void) fputs(orig_sender, dfp[T_HDR]);
    (void) fputc(' ', dfp[T_HDR]);

    if (fdate)
    {
	(void) fputs(fdate, dfp[T_HDR]);
	(void) fputc('\n', dfp[T_HDR]);
    }
    else
    {
	struct tm *ftm;
	time_t now;

	(void) time(&now);
	ftm = localtime(&now);
	p = asctime(ftm);
#ifdef SYSV_FROM
	(void) fprintf(dfp[T_HDR], "%.16s %.3s %.5s", p, tmzone(ftm), p + 20);
#else
	(void) fputs(p, dfp[T_HDR]);
#endif
    }

    /*
     * Free the From_ line if we allocated a copy of it.
     */

    if (from_line)
	free(from_line);

    /*
     * If a local sender is not a mailer, and if the original and
     * local senders differ, record the attempt at forgery.
     */

    if (!mailer_user
	&& ((sender_ct = name_context(orig_sender)) == NULL
	    || sender_ct->ct_uid != real_uid))
    {
	(void) fputs("X-Delivered: at request of ", dfp[T_HDR]);
	(void) fputs(local_sender, dfp[T_HDR]);
	(void) fputs(" on ", dfp[T_HDR]);
	(void) fputs(hostname, dfp[T_HDR]);
	(void) fputc('\n', dfp[T_HDR]);
    }

    /*
     * Copy the rest of the header (if any).
     */

    for (; !feof(stdin) && !ferror(stdin); b = FALSE)
    {
	if (!b)
	{
	    if (fgets(buf, GETSIZE(buf), stdin))
		b = TRUE;
	    else
		break;
	}

	/* Empty line means "end of header" */

	if (buf[0] == '\n')
	{
	    b = FALSE;		/* Don't put this line in the body. */
	    break;
	}

	/*
	 * A line too long to fit in buf[] can't be a header line.
	 * At least, that's my opinion... :-)
	 */

	if (!strchr(buf, '\n'))
	    break;

	/*
	 * If line begins with whitespace, it's a continuation.
	 * Else if line begins with From_ or '>', prepend '>'.
	 * Else if line doesn't look like a header, this must
	 * be the beginning of the body.
	 */

	if (isspace(buf[0] & 0xFF))
	    ;			/* continuation */
	else if (skipfrom(buf) || (buf[0] == '>'))
	    (void) fputc('>', dfp[T_HDR]);
	else
	{
	    /* look for the colon on a header label */

	    p = buf;
	    while (*p
		   && !isspace(*p & 0xFF)
		   && !iscntrl(*p & 0xFF)
		   && *p != ':')
		++p;
	    if ((p == buf) || (*p != ':'))
		break;		/* Not a header line! */
	}

	/* Write the line to the header file. */

	(void) fputs(buf, dfp[T_HDR]);
    }

    /*
     * End the header file with a blank line.
     * This enables us to simply concatenate it with the body file
     * to produce a valid message.
     */

    (void) fputc('\n', dfp[T_HDR]);

    /*
     * Copy the body (if any).
     * Ensure that it ends with a blank line.
     */

    if (b)
	(void) fputs(buf, dfp[T_BODY]);

    nl = 1;
    if (!feof(stdin) && !ferror(stdin))
    {
	while ((c = getchar()) != EOF)
	{
	    (void) fputc(c, dfp[T_BODY]);
	    if (c == '\n')
		++nl;
	    else
		nl = 0;
	}
    }
    for (; nl < 2; ++nl)
	(void) fputc('\n', dfp[T_BODY]);

    /*
     * If we encountered any trouble writing to the temp files,
     * let's not keep it secret.
     */

    for (t = T_HDR; t <= T_BODY; ++t)
    {
	(void) fflush(dfp[t]);
	if (ferror(dfp[t]))
	{
	    error("error writing to %s file %s",
		  ttype[t], tfile[t]);
	    ret = -1;
	}
	if (fclose(dfp[t]))
	{
	    error("error closing %s file %s",
		  ttype[t], tfile[t]);
	    ret = -1;
	}
    }

    /* Return error/success. */

    return ret;
}

/*----------------------------------------------------------------------
 * Don't bother copying message.
 * Put the original names in the environment.
 */

int
dont_copy()
{
    int r, t;

    for (r = T_HDR, t = T_HDRCOPY; r <= T_BODY; ++r, ++t)
    {
	if (tenv[t] && tfile[r])
	    alloc_env(tenv[t], tfile[r]);
    }

    if (verbose)
    {
	message("dont_copy: header is %s, body is %s\n",
		tfile[T_HDR], tfile[T_BODY]);
    }

    return 0;
}

/*----------------------------------------------------------------------
 * Create another copy of each temp file, for security reasons.
 * Also, put the names of the copies in the environment.
 */

int
copy_again()
{
    int r, t;

    for (r = T_HDR, t = T_HDRCOPY; r <= T_BODY; ++r, ++t)
    {
	/* If the file is open, close it. */

	if (tfd[t] != -1)
	{
	    (void) close(tfd[t]);
	    tfd[t] = -1;
	}

	/*
	 * If the file exists, remove it but keep its name.
	 * Otherwise, make a new name and put that name in
	 * the environment.
	 */

	if (tfile[t])
	    (void) unlink(tfile[t]);
	else
	{
	    tfile[t] = tempfile();
	    if (tenv[t])
		alloc_env(tenv[t], tfile[t]);
	}

	/*
	 * Create the file and copy the contents of the
	 * original file to it.
	 */

	if ((tfd[t] = tcreate(tfile[t])) == -1)
	    return -1;

	(void) lseek(tfd[r], 0L, 0);
	if (copyfd(tfd[r], tfd[t]) < 0)
	    return -1;
    }

    if (verbose)
    {
	message("copy_again: header to %s, body to %s\n",
		tfile[T_HDRCOPY], tfile[T_BODYCOPY]);
    }

    return 0;
}

/*----------------------------------------------------------------------
 * Copy a file via file descriptors.
 */

int
copyfd(src_fd, dest_fd)
int src_fd;
int dest_fd;
{
    char buf[BUFSIZ];
    int rd, wr;

    while ((rd = read(src_fd, buf, sizeof(buf))) > 0)
    {
	if ((wr = write(dest_fd, buf, (unsigned) rd)) != rd)
	{
	    if (wr == -1)
		syserr("can't write in copyfd");
	    else
		error("write error -- disk full?");
	    return -1;
	}
    }

    return 0;
}
