/*
#ident	"@(#)smail/src:RELEASE-3_2:queue.c,v 1.30 1996/02/26 18:13:37 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * queue.c:
 *	operations on the queue, such as reading them and writing to
 *	the queue through the spooling functions, and scanning for work.
 *
 *	external functions: queue_message, read_message, write_body,
 *			    log_incoming, check_grade, scan_queue,
 *			    swallow_smtp
 */
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <grp.h>
#include "defs.h"
#include "smail.h"
#include "spool.h"
#include "addr.h"
#include "main.h"
#include "log.h"
#include "field.h"
#include "dys.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

/* whence values for lseek(2) */
#ifndef SEEK_SET
# define SEEK_SET	0	/* set file offset to offset */
#endif
#ifndef SEEK_CUR
# define SEEK_CUR	1	/* set file offset to current plus offset */
#endif
#ifndef SEEK_END
# define SEEK_END	2	/* set file offset to EOF plus offset */
#endif

/* variables local to this file */
static off_t start_msg;			/* where message body begins */
long msg_body_size;			/* size of message body */

#define MAXUSER 16			/* maximum length of user name */

/* variables exported from this file */
int msg_grade;				/* grade level for this message */

/* functions local to this file */
static int queue_envelope();
static int queue_message_internal();
static int queue_message_with_dots();
static int queue_message_simple();
static int fixup_login_user();
static int put_line();
static int read_from_lines();
static char *forward_token();
static char *backward_token();
static void finish_sender();
static int queue_compare();

char *getenv();


/*
 * queue_message - spool a message.
 *
 * spool a message from a stdio file, possibly obeying the hidden-dot
 * protocol from the input.  It will always turn cr/lf into just a
 * newline, while never requiring this to be at the end of lines.
 *
 * If a caller wishes to log any errors that occured during spooling,
 * it must call log_spool_errors.
 *
 * return SUCCEED or FAIL.
 */
int
queue_message(f, dusage, list, error)
    register FILE *f;			/* input file */
    enum dot_usage dusage;		/* how to treat dots */
    struct addr *list;			/* list of recipient addr structures */
    char **error;			/* return error message here */
{
    DEBUG(DBG_QUEUE_HI, "queue_message called\n");
    /* create the spool file */
    if (creat_spool() == FAIL) {
	*error = "spool file creat error";
	if (dusage == SMTP_DOTS) {
	    (void) swallow_smtp(f);
	}
	return FAIL;
    }
    errno = 0;
    *error = NULL;
    /* fill it with stuff */
    if (queue_envelope(list) == FAIL ||
	queue_message_internal(f, dusage, error) == FAIL ||
	write_spool() == FAIL ||
	fixup_login_user() == FAIL)
    {
	if (*error == NULL) {
	    *error = "spool file write error";
	}
	unlink_spool();
	return FAIL;
    }
#ifdef HAVE_FSYNC
    if (fsync(spoolfile) < 0) {
	*error = "fsync write failure";
	unlink_spool();
	return FAIL;
    }
#endif

    return SUCCEED;
}

/*
 * swallow_smtp - read and throw away SMTP message contents
 *
 * Read lines up to a line containing a single '.'.  Do not do
 * anything with these lines.  Return SUCCEED if we successfully read
 * up to a line containing only a single '.'.  Return FAIL otherwise.
 *
 * This is used to read in a complete message from an SMTP transaction
 * which will not be using because of some error.
 */
int
swallow_smtp(f)
    register FILE *f;			/* input file */
{
    register int c;

    for (;;) {
	if ((c = getc(f)) == EOF) return FAIL;
	if (c == '.') {
	    if ((c = getc(f)) == EOF) return FAIL;
	    if (c == '\n') {
		return SUCCEED;
	    }
	    if (c == '\r') {
		if ((c = getc(f)) == EOF) return FAIL;
		if (c == '\n') {
		    return SUCCEED;
		}
	    }
	}
	while (c != '\n') {
	    if ((c = getc(f)) == EOF) return FAIL;
	}
    }
}

/*
 * queue_envelope - write out header filler and arguments for spool file
 *
 * save some space for the login username, which is computed last, and
 * write out any arguments, to the spool file.  The form used is one smail
 * command argument per line, allowing the use of process_args() in main
 * to decode the envelope information.
 */
static int
queue_envelope(list)
    struct addr *list;			/* input recipient list */
{
    register struct addr *cur;		/* current recipient addr to process */
    char *p;				/* temp */
    char buf[50];			/* temp storage */

    if (local_sender) {
	if (put_line(local_sender) == EOF)
	    return FAIL;
    }
    else {
	(void) memset(buf, ' ', MAXUSER);
	buf[MAXUSER] = '\0';
	if (put_line(buf) == EOF)
	    return FAIL;
    }

    /*
     * write out the real uid and effective gid, as numbers.  Validation
     * of trusted_users will be done from the ids stored here.
     */
    (void) sprintf(buf, "%d %d", real_uid, prog_egid);
    if (put_line(buf) == EOF) {
	return FAIL;
    }

    /*
     * write out important state information in the form of command arguments.
     */
    if (extract_addresses) {
	/* signal that addresses are to be taken from the header */
	if (put_line("-t") == EOF) {
	    return FAIL;
	}
    }

    /* tell which form of error recover is being used */
    switch (error_processing) {
    case MAIL_BACK:
	p = "-oem";
	break;
    case WRITE_BACK:
	p = "-oew";
	break;
    case TERMINAL:
	p = "-oep";
	break;
    default:
	p = "-oeq";
	break;
    }
    if (put_line(p) == EOF) {
	return FAIL;
    }
    if (dont_deliver) {
	put_line("-N");
    }
    if (hop_count >= 0) {
	(void) sprintf(buf, "-h%d", hop_count);
	if (put_line(buf) == EOF) {
	    return FAIL;
	}
    }
    if (! do_aliasing) {
	if (put_line("-n") == EOF) {
	    return FAIL;
	}
    }
    if (me_too) {
	if (put_line("-om") == EOF) {
	    return FAIL;
	}
    }

    /*
     * write the sender's full name, if one was given explicitly.  If it was
     * not given explicitly, we would not yet know what it is.
     */
    if (sender_name) {
	if (put_line("-F") == FAIL ||
	    put_line(sender_name) == FAIL)
	{
	    return FAIL;
	}
    }

    /*
     * write the sender address, if one was given explicitly.  If it was
     * not given explicitly, we would not yet know what it is.
     */
    if (sender) {
	if (put_line("-f") == FAIL ||
	    put_line(sender) == FAIL)
	{
	    return FAIL;
	}
    }

    /*
     * write the sending host, host address and sending protocol, if known
     */
    if (sender_host) {
	if (put_line("-oMs") == FAIL ||
	    put_line(sender_host) == FAIL)
	{
	    return FAIL;
	}
    }
    if (sender_host_addr) {
	if (put_line("-oMa") == FAIL ||
	    put_line(sender_host_addr) == FAIL)
	{
	    return FAIL;
	}
    }
    if (sender_proto) {
	if (put_line("-oMr") == FAIL ||
	    put_line(sender_proto) == FAIL)
	{
	    return FAIL;
	}
    }

    /*
     * write the ident of the sending person if known
     */
    if (ident_sender) {
	if (put_line("-oMu") == FAIL ||
	    put_line(ident_sender) == FAIL)
	{
	    return FAIL;
	}
    }

    /*
     * write the method used to determine the ident of sender if known
     */
    if (ident_method) {
	if (put_line("-oMv") == FAIL ||
	    put_line(ident_method) == FAIL)
	{
	    return FAIL;
	}
    }

    /*
     * write out the invoked program, too
     */
    if (put_line("-oMP") == FAIL ||
	put_line(program) == FAIL)
    {
	return FAIL;
    }

    /*
     * write out all of the recipient addresses.  If the -t flag was given,
     * this is the list of addresses that are explicitly not recipients.
     */
    for (cur = list; cur; cur = cur->succ) {
	if (put_line(cur->in_addr) == FAIL) {
	    return FAIL;
	}
    }

    /* envelope ends in one blank line */
    if (PUTSPOOL('\n') == EOF) {
	return FAIL;
    }

    return SUCCEED;
}


/*
 * queue_message_internal - read the message into a spool file.
 *
 * Call either queue_message_with_dots() or queue_message_simple() to
 * spool a message to a spool file.
 */
static int
queue_message_internal(f, dusage, error)
    FILE *f;
    enum dot_usage dusage;
    char **error;			/* optionally store error here */
{
    if (dusage == NO_DOT_PROTOCOL) {
	return queue_message_simple(f);
    }
    return queue_message_with_dots(f, dusage, error);
}

/*
 * queue_message_with_dots - read in a message and write it to the
 *			     spool file, with a dot protocol.
 *
 * Read in a message using one of the dot protocols (HIDDEN_DOTS,
 * DOT_ENDS_MESSAGE or SMTP_DOTS).  All of these protocols end a
 * message when a dot is found on a line by itself.  The HIDDEN_DOTS
 * protocol drops an initial dot from a line that contains other data.
 * The SMTP_DOTS protocol is like HIDDEN_DOTS except that a message
 * must be terminated by a '.', with an encouter of EOF being an
 * error.  Also, with SMTP_DOTS, the entire message is read from the
 * input even if a write error occured to the spool file.
 *
 * All of these protocols treat a CR/LF sequence as equivalent to a
 * single newline.
 *
 * return SUCCEED or FAIL.
 */
static int
queue_message_with_dots(f, dusage, error)
    register FILE *f;			/* input file */
    enum dot_usage dusage;		/* how to treat dots */
    char **error;			/* optionally store error here */
{
    register int c;
    register int put_success = SUCCEED;	/* FAIL if PUTSPOOL fails */

    for (;;) {
	if ((c = getc(f)) == EOF) break;
	if (c == '.') {
	    if ((c = getc(f)) == EOF) break;
	    if (c == '\n') {
		return put_success;
	    }
	    if (c == '\r') {
		if ((c = getc(f)) == EOF) break;
		if (c == '\n') {
		    return put_success;
		}
		if ((put_success == SUCCEED &&
		     dusage == DOT_ENDS_MESSAGE &&
		     PUTSPOOL('.') == FAIL) ||
		    PUTSPOOL('\r') == FAIL)
		{
		    if (dusage != SMTP_DOTS) {
			return FAIL;
		    }
		    put_success = FAIL;
		}
	    } else {
		if (put_success == SUCCEED &&
		    (dusage == DOT_ENDS_MESSAGE && PUTSPOOL('.') == FAIL))
		{
		    put_success = FAIL;
		}
	    }
	}
	for (;;) {
	    while (c != '\n' && c != '\r') {
		if (put_success == SUCCEED && PUTSPOOL(c) == FAIL) {
		    put_success = FAIL;
		}
		if ((c = getc(f)) == EOF) goto read_eof;
	    }
	    if (c == '\r') {
		if ((c = getc(f)) == EOF) goto read_eof;
		if (c != '\n') {
		    if (put_success == SUCCEED && PUTSPOOL('\r') == FAIL) {
			put_success = FAIL;
		    }
		    continue;
		}
	    }
	    if (put_success == SUCCEED && PUTSPOOL(c) == FAIL) {
		put_success = FAIL;
	    }
	    break;
	}
    }

read_eof:
    if (dusage != SMTP_DOTS) {
	return put_success;
    }
    *error = "Unexpected end of file";
    return FAIL;
}

/*
 * queue_message_simple - read in a message with no dot protocol.
 *
 * Read in a message without any specific protocol.  No CR/LF mapping
 * is done and the end of message is given only with an EOF.
 *
 * Return SUCCEED or FAIL.
 */
static int
queue_message_simple(f)
    register FILE *f;
{
    register int c;

    while ((c = getc(f)) != EOF) {
	if (PUTSPOOL(c) == FAIL) {
	    return FAIL;
	}
    }
    return SUCCEED;
}


/*
 * fixup_login_user - insert the login username at the start of the spoolfile
 *
 * to get the message into the spool file as fast as possible, the login
 * name of the user that ran smail is computed last and filled after
 * everything else is through.
 *
 * This is ugly, but it can take a significant amount of time to compute
 * the real user, and it is better to get the spool file written quickly,
 * thus narrowing the window of system crash vulnerability.
 */
static int
fixup_login_user()
{
    register int i;
    extern off_t lseek();

    if (local_sender)
	return SUCCEED;

    compute_local_sender();

    /*
     * truncate to 16 chars, if for some reason we got a
     * larger user name
     */
    i = (int)strlen(local_sender);
    if (i > MAXUSER)
	i = MAXUSER;

    /*
     * write the name to disk
     */
    (void) lseek(spoolfile, (off_t) 1L, SEEK_SET);
    if (write(spoolfile, local_sender, (size_t) i) < i) {
	return FAIL;
    }

    if (msg_foffset == 0) {
	/*
	 * if the current incore spool file region is at the
	 * beginning of the file, make a copy there too.
	 */
	(void) memcpy(msg_buf, local_sender, (size_t) i);
    }

    return SUCCEED;
}

/*
 * put_line - write a complete line to the spool file
 *
 * the string passed is assumed to be an atomic argument for smail, which
 * should end in a newline.  To guard against newlines in the text of the
 * string to end an argument, newline is encoded as \n and \ is encoded as
 * \\.
 */
static int
put_line(s)
    register char *s;
{
    if (PUTSPOOL('!') == EOF)
	return FAIL;
    while (*s) {
	if (*s == '\\') {
	    /* encode \ as \\ and newline as \n */
	    if (PUTSPOOL('\\') == EOF || PUTSPOOL('\\') == EOF) {
		return FAIL;
	    }
	} else if (*s == '\n') {
	    if (PUTSPOOL('\\') == EOF || PUTSPOOL('n') == EOF) {
		return FAIL;
	    }
	} else {
	    if (PUTSPOOL(*s) == EOF) {
		return FAIL;
	    }
	}

	s++;
    }

    if (PUTSPOOL('\n') == EOF) {
	return FAIL;
    }

    return SUCCEED;
}


/*
 * read_message - scan through an open spool file
 *
 * read through the current open spool file to grab From_ and Header:
 * information.  Putting the sender address in the sender variable,
 * and storing the header information in the headers variable as
 * side effects.
 *
 * return the argument vector in *argv
 *
 * return NULL on failure.  The reason for failure will be logged
 * in the system log file and the spool file should be closed and
 * ignored for now.
 */
char **
read_message()
{
    int a = 32;				/* vectors allocated thus far */
    int i;				/* temp */
    register int c;			/* character from spool file */
    register char **argv;		/* computed argument vector */
    char *p;
    char ls[MAXUSER + 1];		/* local sender from file */
    int just_trust = TRUE;		/* true if no "trusted" variables */
    int first;

    DEBUG(DBG_QUEUE_HI, "read_message called\n");
    /* seek to the beginning of the region */
    if (seek_spool((off_t) 0L) == FAIL) {
	return NULL;
    }

    /* get the login username from the spool file */
    i = 0;
    first = 1;
    while ((c = GETSPOOL()) != EOF && c != READ_FAIL &&
	   c != '\n' && c != ' ' && i < MAXUSER) {
	if (c != '!' || ! first)
	    ls[i++] = c;
	first = 0;
    }
    /* read to the end of the first line */
    while (c != '\n' && c != EOF && c != READ_FAIL) {
	c = GETSPOOL();
    }
    ls[i] = '\0';

    /* read the real uid and effective gid under which the mailer was exec'd */
    if (c != READ_FAIL && c != EOF) {
	char buf[50];			/* temp input buffer */
	char *p2 = buf;			/* temp for scanning buf */

	while ((c = GETSPOOL()) != '\n' && c != EOF && c != READ_FAIL) {
	    *p2++ = c;
	}
	*p2 = '\0';
	p2 = buf;
	if (*p2 == '!')
	    p2++;
	(void) sscanf(p2, "%d %d", &real_uid, &prog_egid);
    }
    if (c == READ_FAIL) {
	write_log(LOG_SYS|LOG_PANIC, "read failed: %s/%s: %s",
		  spool_dir, input_spool_fn, strerror(errno));
	return NULL;
    }
    if (c == EOF) {
	write_log(LOG_SYS, "unexpected end of file: %s/%s",
		  spool_dir, input_spool_fn);
    }

    /*
     * determine if the login user is a trusted user, using the real uid.
     */
    p = NULL;
    if (trusted && trusted[0]) {
	just_trust = FALSE;

	/* trusted string a colon separated list */
	for (p = strcolon(trusted); p; p = strcolon((char *)NULL)) {
	    struct passwd *pw = getpwbyname(p);
	    if (pw && real_uid == pw->pw_uid) {
		break;
	    }
	}
    }

    /*
     * trusted group string can also flag invoker as trusted
     */
    if (p == NULL && trusted_groups && trusted_groups[0]) {
	just_trust = FALSE;

	for (p = strcolon(trusted_groups); p; p = strcolon((char *)NULL)) {
	    struct group *gr = getgrbyname(p);
	    if (gr && prog_egid == gr->gr_gid) {
		break;
	    }
	}
    }

    if (p == NULL && !just_trust) {
	/* user is not verified, sender is the login user */
	DEBUG1(DBG_QUEUE_MID, "sender is not trusted\n", real_uid);
	sender = xmalloc(strlen(ls) + 1);
	strcpy(sender, ls);
	islocal = TRUE;
	sender_is_trusted = FALSE;
    }

    /*
     * if sender was already specified and the originator of the
     * message is trusted to supply a sender, see if the sender name
     * is in local form.
     */
    if (sender && sender_is_trusted) {
	char *error;			/* temp to store error message */

	islocal = parse_address(sender, (char **)0, &error, (int *)0) == LOCAL;
    }

    /*
     * read the argument vectors stored in the spool file
     */
    DEBUG1(DBG_QUEUE_HI, "read arg vectors from spool file (at %d)\n",
	   tell_spool());
    argv = (char **)xmalloc(a * sizeof(char *));
    i = 0;
    first = 1;
    while ((c = GETSPOOL()) != EOF && c != '\n') {
	struct str str;
	register struct str *sp = &str;	/* temp string */

	/* read one vector */
	STR_INIT(sp);
	do {
	    if (first) {
		first = 0;
		if (c == '!')
		    continue;
	    }
	    if (c == '\\') {
		c = GETSPOOL();
		if (c == EOF) {
		    STR_FREE(sp);
		    write_log(LOG_SYS, "unexpected end of file: %s/%s",
			      spool_dir, input_spool_fn);
		    return NULL;	/* no message */
		} else if (c == 'n') {
		    c = '\n';
		} else if (c != '\\') {
		    write_log(LOG_SYS, "format error in: %s/%s",
			      spool_dir, input_spool_fn);
		    return NULL;	/* format error */
		}
	    }
	    STR_NEXT(sp, c);
	} while ((c = GETSPOOL()) != EOF && c != '\n');

	/*
	 * finish off the argument string and store it in the vector
	 */
	STR_NEXT(sp, '\0');
	STR_DONE(sp);
	DEBUG1(DBG_QUEUE_HI, "read vector <%s> from spool file\n", sp->p);
	argv[i++] = sp->p;
	/* do we need a larger vector? */
	if (i >= a) {
	    a += 32;
	    argv = (char **) xrealloc((char *) argv, (unsigned) a * sizeof(char *));
	}
	first = 1;
    }

    /* finish off the argument vector */
    argv[i] = NULL;

    /*
     * login name and arguments processed, scan for From_ lines, if
     * any exist.  As a side effect, this may set the sender if it
     * has not been set already.
     */
    if (read_from_lines() == FAIL) {
	/* read_from_lines logged the error */
	return NULL;
    }

    /*
     * if the sender isn't known by now, then use the login user, or
     * postmaster if nothing else is available.  This may be overridden
     * later with a -f or -r flag.
     */
    if (sender == NULL) {
	if (ls[0]) {
	    sender = xmalloc(strlen(ls) + 1);
	    strcpy(sender, ls);
	} else {
	    /* love to gang up on that postmaster */
	    DEBUG(DBG_QUEUE_LO, "no login user, set sender to postmaster\n");
	    sender = xmalloc(sizeof("Postmaster"));
	    strcpy(sender, "Postmaster");
	}
	islocal = TRUE;			/* sender appears to be local */
    }

    /*
     * now grab the header lines.
     */
    DEBUG1(DBG_QUEUE_HI, "calling read_header (offset=%d)\n", tell_spool());
    if (read_header() == FAIL) {
	/* read_header logged the error */
	return NULL;
    }

    /* save the start position for the message */
    start_msg = tell_spool();
    msg_body_size = msg_size - start_msg;
    DEBUG1(DBG_SPOOL_HI, "body starts at %d\n", start_msg);

    /* all done, return the arguments */
    return argv;
}

/*
 * read_from_lines - scan From_ lines and build up a sender
 *
 * Scan through to the end of the From_ lines (if any exist).
 * As a side effect, build up a path to the sender, if the
 * sender is not already known.
 *
 * return SUCCESS or FAIL, and log reason for failure.
 */
static int
read_from_lines()
{
    struct str pstr;
    register struct str *path = &pstr;	/* build up sender path here */
    struct str lstr;
    register struct str *line = &lstr;	/* input line */
    int c;				/* input character */
    register char *lp;			/* temp for processing line */
    struct str ustr;
    register struct str *user = &ustr;	/* user name from From_ line */
    char *mark;				/* mark point in line */
    off_t line_mark;			/* seek position for start of line */

    DEBUG1(DBG_SPOOL_HI, "read_from_lines called (offset=%d)\n", tell_spool());
    /* if we already know the sender, then don't bother computing it */
    if (sender == NULL) {
	islocal = TRUE;			/* set FALSE if we find otherwise */
	STR_INIT(user);
	STR_INIT(path);
    }

    STR_INIT(line);

    /*
     * loop until we have read all of the From_ lines
     */
    for (;;) {
	/* read one complete line */
	line->i = 0;
	line_mark = tell_spool();
	while ((c = GETSPOOL()) != EOF && c != READ_FAIL && c != '\n') {
	    STR_NEXT(line, c);
	}
	if (c == READ_FAIL) {
	    write_log(LOG_SYS, "read failed: %s/%s: %s",
		      spool_dir, input_spool_fn, strerror(errno));
	}
	if (c != EOF) {
	    STR_NEXT(line, c);
	}
	STR_NEXT(line, '\0');
	lp = line->p;
	if (line->i < 20) {
	    /* less than 20 chars? couldn't possibly be a From_ line */
	    break;
	}
	/* skip beginning > character */
	if (*lp == '>') {
	    lp++;
	}
	/* do we have a From line? */
	if (strncmp(lp, "From", 4) != 0 || !isspace(lp[4])) {
	    /* no, end of From_ lines */
	    break;
	}
	if (sender) {
	    continue;			/* we know sender scan next line */
	}
	/* extract the user */
	lp += 5;
	while (isspace(*lp)) {	/* scan for start of user name */
	    lp++;
	}
	mark = lp;			/* mark the start of the user name */
	/* skip over the user name token */
	lp = forward_token(lp);
	*lp = '\0';
	user->i = 0;
	STR_CAT(user, mark);

	/* search for a remote from host */
	lp = line->p + line->i - 2;
	while (isspace(*lp)) {
	    lp--;
	}
	lp[1] = '\0';			/* terminate end of potential host */
	lp = backward_token(line->p, lp); /* back to start of host (?) */
	mark = lp;			/* host may be here */
	lp = backward_token(line->p, lp-1); /* back to start of from (?) */
	if (strncmp(lp, "from", 4) != 0 || !isspace(lp[4])) {
	    path->i = 0;		/* no remote from host, toss path */
	    DEBUG1(DBG_SPOOL_HI, "no remote from host in <%s>, toss path\n",
		   line->p);
	    continue;			/* next From_ line */
	}
	lp = backward_token(line->p, lp-1); /* back to start of remote (?) */
	if (strncmp(lp, "remote", 6) != 0 || !isspace(lp[6])) {
	    path->i = 0;		/* no remote from host, toss path */
	    DEBUG1(DBG_SPOOL_HI, "no remote from host in <%s>, toss path\n",
		   line->p);
	    continue;			/* next From_ line */
	}

	/*
	 * found a remote_from host, add it to the sender path
	 */
	if (path->i > 0) {
	    STR_NEXT(path, '!');	/* ! is separator */
	}
	STR_CAT(path, mark);
	islocal = FALSE;		/* not a local message */
	if (sender_host == NULL && sender_proto == NULL) {
	    sender_host = COPY_STRING(mark);
	    sender_proto = "uucp";
	}
    }

    STR_FREE(line);			/* finished processing input line */

    /*
     * if we are building up a sender, then finish doing so
     */
    if (sender == NULL) {
	STR_NEXT(path, '\0');		/* terminate path and user */
	STR_NEXT(user, '\0');
	finish_sender(path->p, user->p);
	STR_FREE(path);		/* free the storage used */
	STR_FREE(user);
    }

    /*
     * seek back to the beginning of the last line read, which was not
     * a From_ line
     */
    if (seek_spool((off_t) line_mark) == FAIL) {
	write_log(LOG_SYS, "seek failed on: %s/%s: %s",
		  spool_dir, input_spool_fn, strerror(errno));
	return FAIL;
    }
    return SUCCEED;
}

/*
 * forward_token - scan past the following token
 *
 * return the position of the character after the first token after p.
 * a token ends in a space, but can include any chars in quotes, or
 * after a \.
 */
static char *
forward_token(p)
    register char *p;			/* start search here */
{
    register int inquote = FALSE;	/* not in a quote */

    while (isspace(*p))			/* scan to start of token */
	p++;

    /* loop exits by return when done */
    for (;;) {
	switch (*p++) {
	case '\\':
	    if (*p)
		p++;
	    break;
	case ' ':
	case '\t':
	case '\n':
	    if (!inquote) {
		return p-1;		/* past end of token */
	    }
	    break;
	case '\0':
	    return p-1;			/* past end of token */
	case '"':			/* start or end of a quote */
	    inquote = !inquote;
	    break;
	}
    }
}

/*
 * backward_token - scan to beginning of previous token
 *
 * return the position of the first character of a group
 * of non white-space characters before p.
 *
 * NOTE:  we don't take into account text in quotes in scanning
 * backwards.
 */
static char *
backward_token(s,p)
    register char *s;			/* string starts here */
    register char *p;			/* start search here */
{
    /* scan before end of token */
    while (isspace(*p)) {
	if (p == s) {
	    return p;
	}
	p--;
    }

    /* scan to start of token */
    while (*p && !isspace(*p)) {
	if (p == s) {
	    return p;
	}
	p--;
    }

    return p+1;
}

/*
 * finish_sender - build the sender address
 *
 * Build the sender address out of the path and user computed in
 * read_from_lines.
 */
static void
finish_sender(path, user)
    register char *path;		/* path to sender */
    register char *user;		/* user name of sender */
{
    char *target;			/* parsed target */
    char *remainder;			/* parsed remainder */
    register int form;			/* form from parse_address */

    if (user[0] == '\0') {
	return;				/* didn't find anything */
    }

    /* parse the user address, which may yield more path information */
    form = parse_address(user, &target, &remainder, (int *)0);

    if (form == FAIL) {
	/*
	 * failed to parse user, store an error for logging later
	 */
	write_log(LOG_MLOG|LOG_SYS, "Error reading sender: %s", remainder);
	sender = COPY_STRING("Postmaster");
    } else if (form == LOCAL && path[0] == '\0' && !islocal) {
	/*
	 * message was flagged as non-local, but we no longer have any
	 * host information, send it to the postmaster
	 */
	write_log(LOG_MLOG|LOG_SYS, "Path to sender unknown:  sender = %s",
		  user);
	sender = COPY_STRING("Postmaster");
    } else if (path[0] == '\0' && form == LOCAL) {
	/*
	 * simple case: no path, simple user, just make a copy of user
	 */
	sender = COPY_STRING(remainder);
    } else if (path[0] == '\0' && form == UUCP_ROUTE) {
	/*
	 * user is in !-route form, so put the user back together
	 */
	sender = xprintf("%s!%s", target, remainder);
    } else if (form == LOCAL) {
	/*
	 * simple user form, add it to path
	 */
	sender = xprintf("%s!%s", path, remainder);
    } else if (form == UUCP_ROUTE) {
	/*
	 * user is in !-route form, put it back together and add to path
	 */
	sender = xprintf("%s!%s!%s", path, target, remainder);
    } else {
	/*
	 * the difficult case, need to construct a !-route for user
	 */
	char *error;			/* store error here */
	char *route = build_uucp_route(remainder, &error, 0);

	if (route == NULL) {
	    /* found an error building the route */
	    write_log(LOG_MLOG|LOG_SYS, "Error building sender: %s", error);
	} else if (path[0] == '\0') {
	    /*
	     * no path, just put the user back together
	     */
	    sender = xprintf("%s!%s", target, route);
	} else {
	    /*
	     * put user back together and add to path
	     */
	    sender = xprintf("%s!%s!%s", path, target, route);
	}
    }
}


/*
 * write_body - write the message body to a stdio FILE pointer.
 *
 * the body of the message will be sent to the given file.  Pass
 * the the transport flags to send_spool in writing.
 *
 * return SUCCEED, WRITE_FAIL or READ_FAIL.
 */
int
write_body(f, flags)
    FILE *f;				/* write to this file */
    long flags;				/* transport flags */
{
    if (seek_spool((off_t) start_msg) == FAIL) {
	write_log(LOG_SYS, "seek failed: %s/%s: %s",
		  spool_dir, input_spool_fn, strerror(errno));
	return READ_FAIL;
    }
    return send_spool(f, flags);
}


/*
 * check_grade - set grade for message to value from Precedence: field
 *
 * If a Precedence: field is specified for a message and this grade
 * is not the message's current grade, then change the grade for the
 * message, which will involve moving the file to a new name.
 */
void
check_grade()
{
    struct list *hq;			/* temp for scanning headers */

    /* grab the current grade */
    msg_grade = message_id[strlen(message_id) - 1];

    /* find the precedence field (or not) */
    for (hq = header; hq; hq = hq->succ) {
	if (HDREQ("precedence", hq->text)) {
	    /* found the precedence header */
	    int grade = parse_precedence(index(hq->text, ':') + 1);

	    if (grade && msg_grade != grade) {
		/*
		 * change the grade to the new grade, if we fail to
		 * change the grade, don't sweat it too badly.
		 */
		(void) new_grade(grade);
		msg_grade = grade;
	    }
	    break;
	}
    }
}

/*
 * log_incoming - put a message in the log file about the incoming message
 *
 * look for a message-id header field and log it, if it exists, otherwise
 * announce new mail without a previous message-id.
 */
void
log_incoming()
{
    struct list *hq;			/* temp for scanning headers */
    char *old_id = NULL;		/* previous message-id */
    char *trim_old_id = NULL;		/* trimmed previous message-id */
    char *resent_mid = NULL;		/* point to Resent-Message-Id field */
    char *mid = NULL;			/* point to Message-Id field */
    char *host_string = NULL;		/* combined host name and inet addr */

    for (hq = header; hq; hq = hq->succ) {
	if (HDREQ("resent-message-id", hq->text)) {
	    resent_mid = hq->text;
	    break;
	}
	if (HDREQ("message-id", hq->text)) {
	    mid = hq->text;
	}
    }
    if (resent_mid) {
	old_id = index(resent_mid, ':');
    } else if (mid) {
	old_id = index(mid, ':');
    }

    if (sender_host) {
	if (sender_host_addr) {
	    host_string = xprintf("%s [%s]", sender_host, sender_host_addr);
	} else {
	    host_string = xprintf("%s", sender_host);
	}
    } else if (sender_host_addr) {
	host_string = xprintf("[%s]", sender_host_addr);
    }

    if (old_id) {
	register char *p;

	trim_old_id = xmalloc(strlen(old_id));
	for (p = trim_old_id, old_id++; *old_id; old_id++) {
	    if (!isspace(*old_id)) {
		*p++ = *old_id;
	    }
	}
	*p = '\0';
    }


#if (SMAIL_LOG_STYLE == 2)
    /* Default new log style (as of 3.1.29) */
    /* This is SMAIL_LOG_STYLE=2 */
    write_log(LOG_SYS, "Received%s%s%s%s%s%s%s%s%s%s%s%ld%s%s%s%s",
	      " FROM:", sender,
	      host_string? " HOST:": "", host_string? host_string: "",
	      sender_proto? " PROTOCOL:": "", sender_proto? sender_proto: "",
	      " PROGRAM:", program,
	      trim_old_id? " ORIG-ID:": "", trim_old_id? trim_old_id: "",
	      " SIZE:", (long) msg_size,
	      ident_sender? " IDENT:": "", ident_sender? ident_sender: "",
	      ident_method? " ID-METHOD:": "", ident_method? ident_method: "");
#else
#if (!defined(SMAIL_LOG_STYLE)) || (SMAIL_LOG_STYLE == 1)
    /* Old log style (as used by 3.1.28) */
    /* This is SMAIL_LOG_STYLE=1 */
    write_log(LOG_SYS, "received%s%s%s%s%s%s%s%s%s%s%s%ld%s%s%s%s%s",
	      "\n|\t     from: ", sender,
		host_string?
	      "\n|\t     host: ": "", host_string? host_string: "",
		sender_proto?
	      "\n|\t protocol: ": "", sender_proto? sender_proto: "",
	      "\n|\t  program: ", program,
		trim_old_id?
	      "\n|\t  orig-id: ": "", trim_old_id? trim_old_id: "",
	      "\n|\t     size: ", (long) msg_size, " bytes",
	      ident_sender? 
	      "\n|\t    ident: ": "", ident_sender? ident_sender: "",
	      ident_method? 
	      "\n|\t ident-by: ": "", ident_method? ident_method: "");
#else
	ERROR -  SMAIL_LOG_STYLE set to invalid value.
#endif
#endif
    if (host_string) {
	xfree(host_string);
    }
    if (trim_old_id) {
	xfree(trim_old_id);
    }
}

/*
 * scan_spool_dirs - scan for work and return spool files in sorted order
 *
 * look through all of the spool directories, and sort by precedence and
 * by creation date.
 *
 * the returned filename vector contains data which may be reused on
 * subsequent calls to scan_spool_dirs().
 */
char **
scan_spool_dirs()
{
    static char **mv = NULL;		/* vector of messages */
    int mc = 0;				/* count of messages */
    static int mv_size = 32;		/* allocated vector entries */
    char *dn;				/* current directory to scan */
    static struct str str;		/* storage for filenames */
    register int i;			/* temp */

    /* get the initial vector and string storage allocation */
    if (mv == NULL) {
	mv = (char **)xmalloc(mv_size * sizeof(*mv));
	STR_INIT(&str);
    }

    str.i = 0;				/* clear out filename storage */

    /* loop through all spool directories looking for work */
    for (dn = strcolon(spool_dirs); dn; dn = strcolon((char *)NULL)) {
	char *fn;			/* filename from directory */
	char *in_dn = xprintf("%s/input", dn);

	for (fn = scan_dir(in_dn); fn; fn = scan_dir((char *)NULL)) {
	    int entry_grade;

	    /* see if the file matches the form of a spool file */
	    if (! isdigit(fn[0]) || strlen(fn) != SPOOL_FN_LEN) {
		continue;		/* nope */
	    }


	    /* If we are scanning the queue BUT recipients is also
	     * set, then we use the contents of recipients to select
	     * out required messages to process
	     */
	    if (recipients) {
		struct addr * ptr;
		char * msgid;
		int seen = FALSE;

		for (ptr=recipients; (ptr != NULL); ptr=ptr->succ) {
		    msgid = ptr->in_addr;
		    if (*msgid == 'm') /* Skip leading m if specified */
			msgid++;
		    if ((seen = EQ(fn, msgid)))
			break;
		}
		if (!seen)
		    continue;
	    }

	    /* See if the grade is within the range being processed */
	    /* But do not do this if printing the queue */
	    if (operation_mode != PRINT_QUEUE) {
		entry_grade =  fn[strlen(fn) - 1];
		if ((entry_grade < min_runq_grade) || (entry_grade > max_runq_grade)) {
		    continue;	/* outside grade range */
		}
	    }

	    /* add to the list of files */
	    if (mc >= mv_size - 1) {
		/* note: we allow, in advance, for the last NULL entry */
		mv_size += 32;		/* get more entries */
		mv = (char **) xrealloc((char *)mv, (unsigned) (mv_size * sizeof(*mv)));
	    }
	    /* use offsets for now, to be converted to (char *) later */
	    set_ptr(mv[mc++], str.i);
	    str_printf(&str, "%s/%s%N", in_dn, fn);	/* %N is nul byte */
	}
	xfree(in_dn);
    }

    /* close off the vectors */
    mv[mc] = NULL;

    /* convert the offsets in mv to (char *)'s */
    for (i = 0; i < mc; i++) {
	mv[i] = str.p + get_ptr(mv[i]);
    }

    /* sort it */
    (void) qsort((char *)mv, mc, sizeof(*mv), queue_compare);

    return mv;
}

/*
 * queue_compare - compare two filenames which represent queue files
 */
static int
queue_compare(a, b)
    char **a;
    char **b;
{
    int a_grade = (*a)[strlen(*a) - 1];
    int b_grade = (*b)[strlen(*b) - 1];

    if (a_grade != b_grade) {
	return a_grade - b_grade;
    }

    return strcmpic(rindex(*a, '/'), rindex(*b, '/'));
}
