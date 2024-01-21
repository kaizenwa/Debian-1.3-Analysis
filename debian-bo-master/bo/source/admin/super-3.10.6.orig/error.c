
/*
 *	Copyright (c) 1993 by California Institute of Technology.
 *	Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
    
 */

/********************************************************************/
/*
 * If you don't have localsys.h, supply the following:
 * #define HAVE_STDARG_H   if you have <stdarg.h> (ie ANSI C variadic arg lists)
 * #define HAVE_SYSLOG_H   if you have <syslog.h> and syslog() function
 */

/*
 * If you don't like the following "priority" and "facility" values for
 * use with syslog(), supply the following:
 * #define SYSLOG_PRIORITY nnn	...priority for logging if syslog() use is
 *					enabled; default is LOG_ERR.
 * #define SYSLOG_FACILITY nnn	...syslog() facility code if syslog() use is
 *					enabled; default is LOG_USER.

 * We call Strerror(e) to return error messages for error code e.  This
 * routine is expected to simply be strerror(e) + wrapper code to ensure
 * that e is in the valid range.
 */
#include "localsys.h"

/********************************************************************/

/* Error -- print error message, then optionally die.

 * Usage:	Error(show_perror, die, format, args... );
 *	Print error message according to format & args.
 *	If show_perror != 0 && errno != 0, follow error message with perror("").
 *	(If show_perror !=0, but errno==0, follow error msg with "\n").
 *	If die != 0, exit with exit(die).

 * There are several external variables that a calling program can modify:
 *	error_prog: program name preceding msgs to stderr.  Default off.
 *	error_log_prog: program name preceding msgs to logfile.  Default off.
 *	error_stderr: controls whether messages go to stderr.  Default enabled.
 *	error_logfile: controls whether messages go to a logfile. Default off.
 *	error_command: controls whether msgs go to a popen'd command. Def off.
 *	error_syslog: controls whether messages go to [r]syslog.  Default: off.
 *	error_rlog_host: host to receive rsyslog() messages.  Def: localhost.
 *	error_user: Controls username used when printing messages.
 *	error_tag: Controls use of error_{srcfile,line,nl}.  Default off.
 *	error_srcfile, error_line, error_nl: error_srcfile is a file in
 *			which an error was found, starting at line error_line
 *			and continuing over error_nl lines.
 * In detail:

 * If error_prog != NULL, then the message to stderr is preceded
 *	with "<error_prog>: ".

 * If error_stderr == 0, then the message is NOT directed to stderr.
 *	By default error_stderr == 1.

 * If error_logfile != NULL, then the message is also directed to
 *	that file, preceded with
 *		error_log_prog: user@hostname timestamp
 * 	If error_log_prog is NULL, it isn't printed.  We keep separate
 *	error_prog and error_log_prog so that the program name can be
 *	printed on stderr (where there might otherwise be confusion to the
 *	user about which program it is) but optionally not printed in the
 *	logfile, which is typically unique to the program, so that the
 *	program name is redundant.

 * If error_user != NULL, then the username used in messages is error_user,
 *	instead of the default name found by looking at the user's password
 *	entry.

 * If error_command != NULL and *error_command != '\0', then error_command
 *	is popen'd and the message is piped in.  If the popen fails, Error()
 *	is silent about the problem.  NOTE: the command is executed separately
 *	for each call to this routine.

 * If HAVE_SYSLOG_H is defined, and the caller sets error_syslog != 0,
 *	then the message is passed to [r]syslog(), at priority error_priority,
 *	using facility error_facility.  Note that depending on the
 *	"#if 1" setting, we compile with either rsyslog (the default) or syslog.
 *	If rsyslog is used, we send to the syslog facility of error_rlog_host
 *	(default "localhost"), to provide networked syslog messages.
 *	Otherwise we compile with standard syslog routines.
 *	In this case, the fmt string and the printf output must be less
 *	than MAXPRINT characters each. This is because syslog() accepts a
 *	printf-style variadic argument list, but it doesn't have a va_list
 *	version.  Therefore we print into a string and pass that onto syslog().
 *	As a side effect, you can't use syslog-specific "%m" in the fmt.
 *	If the error_prog string is non-null, then just before the first
 *	call to syslog, openlog is called with an ident string = error_prog.
 *	Note that this is done just once: you can't change error_prog
 *	between messages.

 * If error_tag is !0, or if the _first_ two characters of the output
 *	format are "%t", then the output is preceded with a message like
 *	the following:
 *  (a) error_srcfile == NULL:
 *	""				# error_line <= 0
 *	"line %d: "			# error_line > 0,  error_nl <= 1
 *	"lines %d..%d: "		# error_line > 0,  error_nl > 1
 *
 *  (b) error_srcfile == "-":
 *	"in <stdin>: "			# error_line <= 0
 *	"line %d in <stdin>: "		# error_line > 0,  error_nl <= 1
 *	"lines %d..%d in <stdin>: "	# error_line > 0,  error_nl > 1

 *  (c) error_srcfile == anothername:
 *	"in file `%s': "		# error_line <= 0
 *	"line %d in file `%s': "	# error_line > 0,  error_nl <= 1
 *	"lines %d..%d in file `%s': "	# error_line > 0, error_nl > 1

 * Notes:
 *	1. If error_prog and this line information is printed, then
 *	the program name is _not_ suffixed with ":" -- that way, the
 *	output looks something like:
 *		progxyz (lines 232..255 in file `.......'): errmsg
 *	2. In the event that the first two characters are "%t", they enable
 *	tagline printing but are not printed.

 * Return code is -1, so you can print error messages and return an error
 *	code with   return Error(...);
 */


FILE *error_logfile = NULL;
char *error_prog = NULL;
char *error_log_prog = NULL;
char *error_command = NULL;
char *error_user = NULL;

int error_stderr = 1;

int error_syslog = 0;
char *error_rlog_host = "localhost";

int error_line = -1;
int error_nl = -1;
char *error_srcfile = NULL;
int error_tag = 0;

#ifdef HAVE_SYSLOG_H

/* Default error priority */
#ifndef SYSLOG_PRIORITY
#define SYSLOG_PRIORITY LOG_ERR
#endif

/* Default error facility */
#ifndef SYSLOG_FACILITY
#ifdef LOG_USER
#define SYSLOG_FACILITY LOG_USER
#else
#define SYSLOG_FACILITY 0
#endif
#endif

#if 1
void ropenlog __P(( char *ident, int logopt, int facility, char *host ));
void rsyslog __P(( unsigned int level, char *fmt, ... ));

#define OpenLog(prog, opt, fac) ropenlog((prog), (opt), (fac), error_rlog_host)
#define SysLog(pri, buf) rsyslog((pri), (buf))

#else

#define OpenLog(prog, opt, fac) openlog((prog), (opt), (fac))
#define SysLog(pri, buf) syslog((pri), (buf))

#endif

int error_priority = SYSLOG_PRIORITY;
int error_facility = SYSLOG_FACILITY;
int openlog_done = 0;

#endif

static int uid = -1;
static char user[128] = "";
static char hostname[1024] = "";

extern char *Strerror();

#define MAXPRINT 1300

#define StrLCat(s1, s2, maxlen)  strncat(s1, s2, ((maxlen) - strlen(s1) - 1))

char *taglines();

#ifdef HAVE_STDARG_H
/* VARARGS3 */
int
Error(
    int show_perror,	/* If errno != 0, follow msg with perror("") */
    int die,		/* If !0, exit with exit(die) */
    char *fmt,		/* Print rest of args with fprintf(stderr, fmt, ...) */
    ... )
{
    va_list ap;
    int error;
    FILE *error_cmd = NULL;
    char *tag;
    int pctt;

    error = errno;

    /* Figure out line tagging */
    pctt = strncmp(fmt, "%t", 2) == 0;
    tag = ( pctt || error_tag ) ? taglines(3) : NULL;
    if (pctt)
	fmt += 2;

    /* Program name */
    if (error_stderr && error_prog)
	(void) fprintf(stderr, "%s%s ", error_prog, (tag && *tag) ? "" : ":");

    if (error_command && *error_command)
	error_cmd = popen(error_command, "w");

    if (error_log_prog) {
	if (error_logfile)
	    (void) fprintf(error_logfile, "%s%s ", error_log_prog,
						(tag && *tag) ? "" : ":");
	if (error_cmd)
	    (void) fprintf(error_cmd, "%s%s ", error_log_prog,
						(tag && *tag) ? "" : ":");
    }

    if (error_logfile || error_syslog || error_command) {
	if (getuid() != uid || *user == '\0') {
	    struct passwd *pw;
	    int e = errno;
	    pw = getpwuid((uid=getuid()));
	    if (pw) (void) strcpy(user, pw->pw_name);
	    errno = e;
	}
    }

    if (error_logfile || error_cmd) {
	/* user@hostname & timestamp */
	char *s;
	time_t tptr;
	if (*hostname == '\0')
	    (void) gethostname(hostname, sizeof(hostname));
	(void) time(&tptr);
	s = ctime(&tptr);
	s[strlen(s) - 1] = '\0';
	if (error_logfile)
	    (void) fprintf(error_logfile, "%s@%s %s\t",
			    error_user ? error_user : user,
			    hostname, s);
	if (error_cmd)
	    (void) fprintf(error_cmd, "%s@%s %s\t",
			    error_user ? error_user : user,
			    hostname, s);
    }

    if (error_stderr) {
	if (tag)
	    (void) fputs(tag, stderr);
	/* User's msg */
	va_start(ap, fmt);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
    }

    if (error_logfile) {
	if (tag)
	    (void) fputs(tag, error_logfile);
	/* User's msg */
	va_start(ap, fmt);
	(void) vfprintf(error_logfile, fmt, ap);
	va_end(ap);
    }

    if (error_cmd) {
	if (tag)
	    (void) fputs(tag, error_cmd);
	/* User's msg */
	va_start(ap, fmt);
	(void) vfprintf(error_cmd, fmt, ap);
	va_end(ap);
    }

    if (show_perror) {
	if (error) {
	    errno = error;
	    if (error_stderr)
		perror("");
	    if (error_logfile) {
		(void) fprintf(error_logfile, "%s\n", Strerror(error));
	    }
	    if (error_cmd) {
		(void) fprintf(error_cmd, "%s\n", Strerror(error));
	    }
	} else {
	    if (error_stderr)
		(void) fputc('\n', stderr);
	    if (error_logfile)
		(void) fputc('\n', error_logfile);
	    if (error_cmd)
		(void) fputc('\n', error_cmd);
	}
    }

#ifdef HAVE_SYSLOG_H
    if (error_syslog) {
	char newfmt[MAXPRINT], buf[MAXPRINT];
	if (!openlog_done && error_prog) {
	    OpenLog(error_prog, 0, error_facility);
	    openlog_done = 1;
	}
	sprintf(newfmt, "(%s) ", error_user ? error_user : user);
	StrLCat(newfmt, fmt, sizeof(newfmt));
	if (tag)
	    StrLCat(newfmt, tag, sizeof(newfmt));
	va_start(ap, fmt);
	(void) vsprintf(buf, newfmt, ap);
	va_end(ap);
	SysLog(error_priority, buf);
    }
#endif

    if (die)
	(void) exit(die);

    if (error_cmd)
	pclose(error_cmd);

    return -1;

}
#else

/* VARARGS3 */
int
Error( va_alist )
va_dcl
{
    va_list ap;
    int die, show_perror;
    char *fmt, *orig_fmt;
    int error;
    char *tag;
    int pctt;
    FILE *error_cmd = NULL;

    error = errno;

    /* Figure out line tagging */
    va_start(ap);
    show_perror = va_arg(ap, int);
    die = va_arg(ap, int);
    fmt = va_arg(ap, char *);
    va_end(ap);
    pctt = strncmp(fmt, "%t", 2) == 0;
    tag = ( pctt || error_tag ) ? taglines(3) : NULL;
    if (pctt)
	fmt += 2;

    /* Program name */
    if (error_stderr && error_prog)
	(void) fprintf(stderr, "%s%s ", error_prog, (tag && *tag) ? "" : ":");

    if (error_command && *error_command)
	error_cmd = popen(error_command, "w");

    if (error_log_prog) {
	if (error_logfile)
	    (void) fprintf(error_logfile, "%s%s ", error_log_prog,
						(tag && *tag) ? "" : ":");
	if (error_cmd)
	    (void) fprintf(error_cmd, "%s%s ", error_log_prog,
						(tag && *tag) ? "" : ":");
    }

    if (error_logfile || error_syslog || error_command) {
	if (getuid() != uid || *user == '\0') {
	    struct passwd *pw;
	    int e = errno;
	    pw = getpwuid((uid=getuid()));
	    if (pw) (void) strcpy(user, pw->pw_name);
	    errno = e;
	}
    }

    if (error_logfile || error_cmd) {
	/* user@hostname & timestamp */
	char *s;
	time_t tptr;
	if (*hostname == '\0')
	    (void) gethostname(hostname, sizeof(hostname));
	(void) time(&tptr);
	s = ctime(&tptr);
	s[strlen(s) - 1] = '\0';
	if (error_logfile)
	    (void) fprintf(error_logfile, "%s@%s %s\t",
				error_user ? error_user : user,
				hostname, s);
	if (error_cmd)
	    (void) fprintf(error_cmd, "%s@%s %s\t",
				error_user ? error_user : user,
				hostname, s);
    }

    if (error_stderr) {
	if (tag)
	    (void) fputs(tag, stderr);
	/* User's msg */
	va_start(ap);
	show_perror = va_arg(ap, int);
	die = va_arg(ap, int);
	orig_fmt = va_arg(ap, char *);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
    }

    if (error_logfile) {
	if (tag)
	    (void) fputs(tag, error_logfile);
	/* User's msg */
	va_start(ap);
	show_perror = va_arg(ap, int);
	die = va_arg(ap, int);
	orig_fmt = va_arg(ap, char *);
	(void) vfprintf(error_logfile, fmt, ap);
	va_end(ap);
    }

    if (error_cmd) {
	if (tag)
	    (void) fputs(tag, error_cmd);
	/* User's msg */
	va_start(ap);
	show_perror = va_arg(ap, int);
	die = va_arg(ap, int);
	orig_fmt = va_arg(ap, char *);
	(void) vfprintf(error_cmd, fmt, ap);
	va_end(ap);
    }

    /* Figure out if we do show_perror */
    va_start(ap);
    show_perror = va_arg(ap, int);
    die = va_arg(ap, int);
    va_end(ap);

    if (show_perror) {
	if (error) {
	    errno = error;
	    if (error_stderr)
		perror("");
	    if (error_logfile)
		(void) fprintf(error_logfile, "%s\n", Strerror(error));
	    if (error_cmd)
		(void) fprintf(error_cmd, "%s\n", Strerror(error));
	} else {
	    if (error_stderr)
		(void) fputc('\n', stderr);
	    if (error_logfile)
		(void) fputc('\n', error_logfile);
	    if (error_cmd)
		(void) fputc('\n', error_cmd);
	}
    }

#ifdef HAVE_SYSLOG_H
    if (error_syslog) {
	char newfmt[MAXPRINT], buf[MAXPRINT];
	va_start(ap);
	show_perror = va_arg(ap, int);
	die = va_arg(ap, int);
	orig_fmt = va_arg(ap, char *);
	if (!openlog_done && error_prog) {
	    OpenLog(error_prog, 0, error_facility);
	    openlog_done = 1;
	}
	sprintf(newfmt, "(%s) ", error_user ? error_user : user);
	StrLCat(newfmt, fmt, sizeof(newfmt));
	if (tag)
	    StrLCat(newfmt, tag, sizeof(newfmt));
	(void) vsprintf(buf, newfmt, ap);
	va_end(ap);
	SysLog(error_priority, buf);
    }
#endif

    if (die)
	(void) exit(die);

    if (error_cmd)
	pclose(error_cmd);

    return -1;

}
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* For tagging error text with a prefixing lines indicator. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
taglines(decorations)
int decorations;	/* 1 = enclose in parens; 2 = add a ": "; 3 = both */
{
    static char buf[MAXPRINT];
    int putparens = decorations & 01;
    int putcolon = decorations & 02;

    if (error_srcfile == NULL) {
	if (error_line <= 0)
	    return "";
	else if (error_nl <= 1)
	    (void) sprintf(buf, "%sline %d%s%s",
				putparens ? "(" : "",
				error_line,
				putparens ? ")" : "",
				putcolon ? ": " : "");
	else
	    (void) sprintf(buf, "%slines %d..%d%s%s",
				putparens ? "(" : "",
				error_line, error_line + error_nl - 1,
				putparens ? ")" : "",
				putcolon ? ": " : "");
    } else if (strcmp(error_srcfile, "-") == 0) {
	if (error_line <= 0)
	    (void) sprintf(buf, "%sin <stdin>%s%s",
				putparens ? "(" : "",
				putparens ? ")" : "",
				putcolon ? ": " : "");
	else if (error_nl <= 1)
	    (void) sprintf(buf, "%sline %d in <stdin>%s%s",
				putparens ? "(" : "",
				error_line,
				putparens ? ")" : "",
				putcolon ? ": " : "");
	else
	    (void) sprintf(buf, "%slines %d..%d in <stdin>%s%s",
				putparens ? "(" : "",
				error_line, error_line + error_nl - 1,
				putparens ? ")" : "",
				putcolon ? ": " : "");
    } else {
	if (error_line <= 0)
	    (void) sprintf(buf, "%sin file `%s'%s%s",
				putparens ? "(" : "",
				error_srcfile,
				putparens ? ")" : "",
				putcolon ? ": " : "");
	else if (error_nl <= 1)
	    (void) sprintf(buf, "%sline %d in file `%s'%s%s",
				putparens ? "(" : "",
				error_line, error_srcfile,
				putparens ? ")" : "",
				putcolon ? ": " : "");
	else
	    (void) sprintf(buf, "%slines %d..%d in file `%s'%s%s",
				putparens ? "(" : "",
				error_line, error_line + error_nl - 1,
				error_srcfile,
				putparens ? ")" : "",
				putcolon ? ": " : "");
    }
    return buf;
}
