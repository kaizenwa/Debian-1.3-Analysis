/*
#ident	"@(#)smail/src:RELEASE-3_2:log.c,v 1.17 1996/05/30 03:51:35 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * log.c:
 *	system and per-message logging functions
 *
 *	These functions send information to a per-system log file and to
 *	a per-message log file, manage the creation use and removal of
 *	per-message logs and handle panic and fatal messages.
 *
 *	external functions:  open_system_logs, close_system_logs,
 *			     open_msg_log, close_msg_log, unlink_msg_log,
 *			     panic, write_log, send_log, scan_msg_log
 */
#include "defs.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef ANSI_C
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "smail.h"
#include "dys.h"
#include "log.h"
#include "addr.h"
#include "main.h"
#include "spool.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

#if defined(UNIX_SYS5) || defined(POSIX_OS) || defined(USE_FCNTL)
# include <fcntl.h>
#else
# if defined(UNIX_BSD)
#  include <sys/file.h>
# endif
#endif

#ifdef STANDALONE
# define xmalloc malloc
# define xrealloc realloc
# define xfree free
#endif	/* STANDALONE */

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

/* exported variables */
FILE *msg_logfile = NULL;		/* open stream to per-message log */

/* variables local to this file */
static FILE *panicfile = NULL;		/* open stream to panic log file */
static FILE *logfile = NULL;		/* open stream to system log file */
static char *msg_log_fn;		/* name of per-message log file */

/* functions local to this file */
static void build_msg_log_fn();
static void try_mkdir();
static void write_log_va();

/* variables imported from the C library */
extern int errno;			/* system error number */


/*
 * open_system_logs - open the panic and the general information log files.
 *
 * Access to the system log should not require that much be functional
 * or that resources be allocated to send to the system log.  For that
 * reason, we allocate all resources in advance, while resources are
 * available and the system is assumed to be somewhat sane.
 *
 * If reasonable, the system logs should be opened after a message has
 * been read to a spool file so that if a panic occurs because a log
 * could not be opened we can recover mail later when the problem is
 * solved.
 */
void
open_system_logs()
{
    int fd;
    static char panicbuf[BUFSIZ];	/* stdio buffer to avoid mallocs */
    static char logbuf[BUFSIZ];		/* stdio buffer to avoid mallocs */

    /*
     * first open panic log so we can panic if we can't open
     * the system log
     */
#ifdef	O_APPEND
    fd = open(panic_fn, O_CREAT|O_APPEND|O_WRONLY, log_mode);
#else
    /* limited to V7 open semantics */
    fd = open(panic_fn, 1);
    if (fd < 0) {
	fd = creat(panic_fn, log_mode);
    } else {
	(void) lseek(fd, (off_t) 0L, SEEK_END);
    }
#endif
    if (fd < 0) {
	/* perhaps the directory just needs to be created */
	if (auto_mkdir && errno == ENOENT) {
	    try_mkdir(panic_fn);
	    /* try opening the file again */
#ifdef O_APPEND
	    fd = open(panic_fn, O_CREAT|O_APPEND|O_WRONLY, log_mode);
#else
	    fd = creat(panic_fn, log_mode);
#endif
	}
	if (fd < 0) {
	    panic(EX_OSFILE, "cannot open %s: %s", panic_fn, strerror(errno));
	    /*NOTREACHED*/
	}
    }
    panicfile = fdopen(fd, "a");
    (void) setbuf(panicfile, panicbuf);	/* associate stream and buffer */

    /*
     * next open the system log file after which we can start issuing
     * log messages.
     */
#ifdef	O_APPEND
    fd = open(log_fn, O_CREAT|O_APPEND|O_WRONLY, log_mode);
#else
    /* limited to V7 open semantics */
    fd = open(log_fn, 1);
    if (fd < 0) {
	fd = creat(log_fn, log_mode);
    } else {
	(void) lseek(fd, (off_t) 0L, SEEK_END);
    }
#endif
    if (fd < 0) {
	/* perhaps the directory just needs to be created */
	if (auto_mkdir && errno == ENOENT) {
	    try_mkdir(log_fn);
	    /* try opening the file again */
#ifdef	O_APPEND
	    fd = open(log_fn, O_CREAT|O_APPEND|O_WRONLY, log_mode);
#else
	    fd = creat(log_fn, log_mode);
#endif
	}
	if (fd < 0) {
	    panic(EX_OSFILE, "cannot open %s: %s", log_fn, strerror(errno));
	    /*NOTREACHED*/
	}
    }
    logfile = fdopen(fd, "a");
    (void) setbuf(logfile, logbuf);	/* associate stream and buffer */
}

/*
 * try_mkdir - try to build the directory for the given filename
 */
static void
try_mkdir(fn)
    char *fn;
{
    char *slash = rindex(fn, '/');
    char *dr;
    struct stat st;

    if (slash == NULL) {
	return;				/* ignore bad filename */
    }

    /* figure directory name */
    while (slash > fn && *(slash - 1) == '/')
	--slash;
    if (slash == fn)
	return;			/* root directory */
    dr = xmalloc((unsigned) (slash - fn + 1));
    (void) memcpy(dr, fn, (size_t) (slash - fn));
    dr[slash - fn] = '\0';

    DEBUG1(DBG_LOG_LO, "make directory %s\n", dr);
    (void) mkdir(dr, auto_mkdir_mode);

    if (stat(dr, &st) == -1 && errno == ENOENT) {
	char *slash2 = rindex(dr, '/');

	if (slash2) {
	    while (slash2 > dr && *(slash2 - 1) == '/')
		--slash2;
	    if (slash2 != dr) {
		*slash2 = '\0';
		DEBUG1(DBG_LOG_LO, "    make parent directory %s\n", dr);
		(void) mkdir(dr, auto_mkdir_mode);
		*slash2 = '/';
		(void) mkdir(dr, auto_mkdir_mode);
	    }
	}
    }

    xfree(dr);
}

/*
 * close_system_logs - close the panic and general info log files.
 */
close_system_logs()
{
    if (logfile) {
	(void) fclose(logfile);
	logfile = NULL;
    }
    if (panicfile) {
	(void) fclose(panicfile);
	panicfile = NULL;
    }
}


/*
 * open_msg_log - open message log file, one per message
 *
 * a per-message log should be opened once for each message and closed
 * when done processing a message.  It is intended to be information
 * that a sender might be interested in, and will be sent back to
 * the sender if the return_to_sender flag is set when processing of
 * the message is completed.
 */
void
open_msg_log()
{
    int fd;
    static char msgbuf[BUFSIZ];		/* stdio buffer to avoid mallocs */

    /*
     * if msg_fn not yet set up create a suitably unique value
     */
    if (msg_logfile) {
	(void) fclose(msg_logfile);
    }
    build_msg_log_fn();
#ifdef	O_APPEND
    fd = open(msg_log_fn, O_CREAT|O_APPEND|O_WRONLY, log_mode);
#else
    /* limited to V7 open semantics */
    fd = open(msg_log_fn, 1);
    if (fd < 0) {
	fd = creat(msg_log_fn, log_mode);
    } else {
	(void) lseek(fd, (off_t) 0L, SEEK_END);
    }
#endif
    if (fd < 0) {
	if (errno == ENOENT) {
	    /* the directory did not exist, try to create it */
	    DEBUG1(DBG_LOG_LO, "make directory %s/msglog\n",
		   spool_dir);
	    (void) mkdir("msglog", auto_mkdir_mode);
	} else {
	    /* alternate idea, permissions wrong so try to remove it */
	    (void) unlink(panic_fn);
	}
#ifdef	O_APPEND
	fd = open(msg_log_fn, O_CREAT|O_APPEND|O_WRONLY, log_mode);
#else
	/* limited to V7 open semantics */
	fd = open(msg_log_fn, 1);
	if (fd < 0) {
	    fd = creat(msg_log_fn, log_mode);
	} else {
	    (void) lseek(fd, (off_t) 0L, SEEK_END);
	}
#endif
	if (fd < 0) {
	    /*
	     * otherwise, panic.  We are assuming that the mail
	     * queue entry can be scanned at a later date
	     */
	    panic(EX_OSFILE, "cannot open %s/%s: %s",
		  spool_dir, msg_log_fn, strerror(errno));
	    /*NOTREACHED*/
	}
    }
    msg_logfile = fdopen(fd, "a");
    (void) setbuf(msg_logfile, msgbuf);	/* associate stream and buffer */
}

/*
 * build_msg_log_fn - build the name for the per-message log file
 */
static void
build_msg_log_fn()
{
    static char buf[sizeof("msglog/") + SPOOL_FN_LEN + 1];

    (void) sprintf(buf, "msglog/%s", spool_fn);
    msg_log_fn = buf;
}

/*
 * close_msg_log - close the per-message log file
 *
 * This should be called when further processing of a message is
 * being postponed to some point in the future.
 */
void
close_msg_log()
{
    if (msg_logfile) {
	(void) fclose(msg_logfile);
	msg_logfile = NULL;
    }
}

/*
 * unlink_msg_log - close and unlink the per-message log file
 *
 * use this when a message has been processed completely.
 */
void
unlink_msg_log()
{
    close_msg_log();
    if (msg_log_fn) {
	(void) unlink(msg_log_fn);
	msg_log_fn = NULL;
    }
}


/*
 * panic - panic and die, attempting to write to the panic log file.
 *
 * If we fail to write to the panic log file, write to the console.
 * In all cases try to write to stderr.
 *
 * If this is called after spooling a message, then the message will
 * stay around for further processing.
 *
 */
/*VARARGS2*/
#ifdef ANSI_C
void
panic(int exitcode, char *fmt, ...)
#else
void
panic(exitcode, fmt, va_alist)
    int exitcode;			/* we will call exit(exitcode) */
    char *fmt;				/* printf(3) format */
    va_dcl                              /* arguments for printf */
#endif
{
    static int panic_count = 0;		/* panic not yet called once */
    va_list ap;

    /*
     * if panic has been called before, but the panic log is not open
     * write to the console.
     *
     * NOTE:  This will happen when open_system_logs calls panic because
     *	      it could not open the panic log file.
     */
    if ((panicfile == NULL && panic_count == 1) || panic_count == 2) {
	FILE *consfile;
	static char consbuf[BUFSIZ];

	consfile = fopen(cons_fn, "a");
	if (consfile == NULL) {
	    /*
	     * not many situations are more difficult to handle than
	     * this one--punt.
	     */
	    if (force_zero_exitvalue) {
		/* if this is set, always exit with 0 */
		exit(0);
	    }
	    exit(exitcode);
	}
	(void)setbuf(consfile, consbuf); /* avoid use of malloc */
	(void)fprintf(consfile, "%s %s: mailer error: ",
		      program, time_stamp());
	if (message_id) {
	    (void)fprintf(consfile, "[%s] ", message_id);
	}
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)vfprintf(consfile, fmt, ap);
	va_end(ap);
	/* log messages don't come with \n */
	(void)fprintf(consfile, "\r\n");
	(void)fclose(consfile);		/* flush */
    } else {
	panic_count++;			/* help stamp out recursion */
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	write_log_va(LOG_PANIC, fmt, ap);
	va_end(ap);
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	if (errfile)
	    write_log_va(LOG_TTY, fmt, ap);
	va_end(ap);
    }

    if (daemon_pid != 0 && daemon_pid == getpid()) {
	write_log(LOG_SYS, "pid: smail daemon exiting on panic");
    }
    if (spool_fn) {
	defer_message();		/* put message in error/ directory */
    }
    if (force_zero_exitvalue) {
	exit(0);
    }
    exit(exitcode);
}

#define LOG_DEBUG 0x0020
/*
 * write_log - write to the per-message log, and perhaps the system log
 *
 * write a log message to the various log files, where `who' is the
 * bitwise OR of LOG_SYS, LOG_MLOG and/or LOG_PANIC.
 */
/*VARARGS2*/
#ifdef ANSI_C
void
write_log(int who, char *fmt, ...)
#else
void
write_log(who, fmt, va_alist)
    int who;				/* mask of log files to be written */
    char *fmt;				/* printf(3) format */
    va_dcl                              /* arguments for printf */
#endif
{
    va_list ap;

#ifndef NODEBUG
    if (debug && errfile && (debug > 1 || fmt[0] != 'X')) {
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	write_log_va(LOG_DEBUG, fmt, ap);
	va_end(ap);
    }
#endif
    if (errfile && ((who & LOG_TTY) ||
		   ((who & (LOG_MLOG|LOG_PANIC)) &&
		    error_processing == TERMINAL &&
		    fmt[0] != 'X'))) {
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	write_log_va(LOG_TTY, fmt, ap);
	va_end(ap);
    }
    if (who & LOG_SYS) {
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	write_log_va(LOG_SYS, fmt, ap);
	va_end(ap);
    }
    if (who & LOG_PANIC) {
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	write_log_va(LOG_PANIC, fmt, ap);
	va_end(ap);
    }
    if (who & LOG_MLOG) {
#ifdef ANSI_C
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	write_log_va(LOG_MLOG, fmt, ap);
	va_end(ap);
    }
}

static void
write_log_va(who, fmt, ap)
    int who;				/* mask of log files to be written */
    char *fmt;				/* printf(3) format */
    va_list ap;                         /* arguments for printf */
{
    switch (who) {
#ifndef NODEBUG
    case LOG_DEBUG: {
	int wct = 0;

	/* if we are debugging at all, print logging messages */
	(void) fprintf(errfile, "write_log:");

	/* when debugging, tell which log files are to be used */
	if (debug > 1 && (who & LOG_SYS)) {
	    (void) fprintf(errfile, " SYS");
	    wct++;
	}
	if (debug > 1 && (who & LOG_PANIC)) {
	    (void) fprintf(errfile, "%cPANIC", wct++? '|': ' ');
	}
	if (debug > 1 && (who & LOG_MLOG)) {
	    (void) fprintf(errfile, "%cMLOG", wct++? '|': ' ');
	}
	if (debug > 1 && (who & LOG_TTY)) {
	    (void) fprintf(errfile, "%cTTY", wct++? '|': ' ');
	}
	if (debug > 1) {
	    (void) fprintf(errfile, " ");
	}
	(void)vfprintf(errfile, fmt, ap);
	/* log messages don't come with \n */
	(void)fprintf(errfile, "\n");
	(void)fflush(errfile);
    }
    break;

#endif	/* NODEBUG */

    case LOG_TTY:
    {
	(void) fprintf(errfile, "%s: ", program);
	(void) vfprintf(errfile, fmt, ap);
	/* log messages don't come with \n */
	(void) fprintf(errfile, "\n");
	(void) fflush(errfile);
    }
    break;

    case LOG_SYS:
    {
	/* write out permanent log to the system log file */
	if (logfile == NULL || panicfile == NULL) {
	    open_system_logs();		/* if system log not open, open it */
	}
#ifdef LOG_EXTRA_NEWLINE
	(void)fprintf(logfile, "\n%s: ", time_stamp());
#else
	(void)fprintf(logfile, "%s: ", time_stamp());
#endif
	if (message_id) {
	    (void)fprintf(logfile, "[%s] ", message_id);
	}
	(void)vfprintf(logfile, fmt, ap);
	/* log messages don't come with \n */
	(void)fprintf(logfile, "\n");
	(void)fflush(logfile);
	if (ferror(logfile)) {
	    panic(EX_IOERR, "error writing %s: %s", log_fn, strerror(errno));
	    /*NOTREACHED*/
	}
    }
    break;

    case LOG_PANIC:
    {
	if (panicfile == NULL) {
	    open_system_logs();		/* if system log not open, open it */
	}
#ifdef LOG_EXTRA_NEWLINE
	(void)fprintf(panicfile, "\n%s: ", time_stamp());
#else
	(void)fprintf(panicfile, "%s: ", time_stamp());
#endif
	if (message_id) {
	    (void)fprintf(panicfile, "[%s] ", message_id);
	}
	(void)vfprintf(panicfile, fmt, ap);
	/* log messages don't come with \n */
	(void)fprintf(panicfile, "\n");
	(void)fflush(panicfile);
	if (ferror(panicfile)) {
	    panicfile = NULL;
	    panic(EX_IOERR, "error writing %s: %s", log_fn, strerror(errno));
	    /*NOTREACHED*/
	}
    }
    break;

    /*
     * NOTE:  if there is no spool_dir set, then a per-message logfile
     *	      cannot be created, so don't bother writing per-message
     *	      log entries.
     */
    case LOG_MLOG:
	if (spool_dir) {
	    if (msg_logfile == NULL) {
		open_msg_log();		/* if message log not open, open it */
	    }

	    /* write out the message to the per-message log file */
	    (void)vfprintf(msg_logfile, fmt, ap);
	    /* log messages don't come with \n */
	    (void)fprintf(msg_logfile, "\n");
	    (void)fflush(msg_logfile);
	    if (ferror(msg_logfile)) {
		panic(EX_IOERR, "error writing %s/%s: %s",
		      spool_dir, msg_log_fn, strerror(errno));
		/*NOTREACHED*/
	    }
	}
	break;

    }
}

/*
 * send_log - write out the per-message log file.
 *
 * Send the per-message log to a file with or without lines that begin
 * with the magic X character.  Also, an optional banner string will
 * be output before any log data.  If no log data is output then the
 * banner string is also not output.
 *
 * Note: we reopen to keep from changing the write position while
 *       reading.
 */
void
send_log(f, all, banner)
    FILE *f;				/* send to this file */
    int all;				/* if TRUE, also send X lines */
    char *banner;			/* if non-NULL precedes log output */
{
    FILE *mlog;				/* message log file */
    register int c;			/* for copying data between files */
    int last_c;

    build_msg_log_fn();
    mlog = fopen(msg_log_fn, "r");	/* reopen log file */
    if (mlog == NULL) {
	return;				/* no message file found */
    }
    last_c = '\n';
    while ((c = getc(mlog)) != EOF) {	/* copy from log to specified file */
	if (last_c == '\n' && c == 'X' && !all) {
	    /* ignore lines beginning with X */
	    while ((c = getc(mlog)) != EOF && c != '\n') ;
	    if (c == EOF) {
		break;
	    }
	    last_c = '\n';
	    continue;
	}
	if (last_c == '\n') {
	    if (banner) {
		(void)fputs(banner, f);	/* output banner at most once */
		banner = NULL;
	    }
	    (void)fputs(" ", f);
	}
	putc(c, f);
	last_c = c;
    }
    (void)fclose(mlog);			/* don't need this anymore */
}

/*
 * scan_msg_log - scan for X entries in the per-message logfile
 *
 * if first is TRUE, open the message log and scan for the first line
 * marked with an X, return that line.  On successive calls, where
 * first is FALSE, return successive lines marked with an X.  Return
 * NULL if no more such lines are found.
 *
 * returned value points to a string area which may be reused on subsequent
 * calls to scan_msg_log.
 */
char *
scan_msg_log(first)
    int first;				/* TRUE to open the message log */
{
    static FILE *mlog = NULL;		/* opened message log */
    static struct str line;		/* line marked with X */
    static inited_line = FALSE;		/* TRUE if STR_INIT called for line */
    register int c, last_c;

    build_msg_log_fn();
    if (first && mlog) {
	rewind(mlog);
    }
    if (mlog == NULL) {
	mlog = fopen(msg_log_fn, "r");	/* reopen log file */
	if (mlog == NULL) {
	    return NULL;
	}
    }
    last_c = '\n';

    /* scan for line beginning with X */
    while ((c = getc(mlog)) != EOF) {
	if (c == 'X' && last_c == '\n') {
	    break;
	}
	last_c = '\n';
    }
    if (c == EOF) {
	/* reached end of file without finding an X line */
	(void) fclose(mlog);
	mlog = NULL;
	return NULL;
    }
    /* found a line marked with X, read it in */
    if (! inited_line) {
	STR_INIT(&line);
	inited_line = TRUE;
    } else {
	line.i = 0;
    }
    STR_NEXT(&line, 'X');
    while ((c = getc(mlog)) != EOF && c != '\n') {
	STR_NEXT(&line, c);
    }
    STR_NEXT(&line, '\0');

    /* return that line */
    DEBUG1(DBG_LOG_MID, "scan_msg_log returns: %s\n", line.p);
    return line.p;
}


#ifdef STANDALONE
char *panic_fn = "/usr/lib/smail/paniclog";
char *log_fn = "/usr/lib/smail/log";
char *cons_fn = "/dev/console";
char *msg_log_dir = "/usr/tmp";
char *program = "this-is-a-test";
int exitvalue = 0;
enum er_proc error_processing = MAIL_BACK;
int return_to_sender = FALSE;
int islocal = TRUE;
char *sender = "nsc!tron";
int force_zero_exitvalue = FALSE;

/*
 * excersize the logging code by performing the various operations
 * in a sequence given at run time on the standard input.
 */
void
main()
{
    register int c;			/* hold key for current operation */
    char line[4096];			/* buffer to hold a line of input */
    char *p;

    while ((c = getchar()) != EOF) {
	switch (c) {
	case 'O':
	    open_system_logs();
	    break;
	case 'C':
	    close_system_logs();
	    break;
	case 'o':
	    open_msg_log();
	    break;
	case 'c':
	    close_msg_log();
	    break;
	case 's':
	    send_log(stdout, FALSE, "|----- log without X lines -----|\n");
	    break;
	case 'S':
	    send_log(stdout, TRUE, "|----- log with X lines -----|\n");
	    break;
	case 'X':
	    for (p = scan_msg_log(TRUE); p; p = scan_msg_log(FALSE)) {
		printf("%s\n", p);
	    }
	    break;
	case 'p':
	    fgets(line, sizeof(line), stdin);
	    panic(EX_OSERR, line);
	    break;
	case 'l':
	    fgets(line, sizeof(line), stdin);
	    write_log(LOG_SYS, line);
	    break;
	case 'm':
	    fgets(line, sizeof(line), stdin);
	    write_log(LOG_MLOG, line);
	    break;
	case 'L':
	    fgets(line, sizeof(line), stdin);
	    write_log(LOG_MLOG|LOG_SYS|LOG_PANIC);
	    break;
	case ' ':
	case '\t':
	case '\n':
	    break;
	default:
	    (void)fprintf(stderr, "%c -- huh?\n", c);
	    break;
	}
    }
    exit(EX_OK);
}
#endif	/* STANDALONE */
