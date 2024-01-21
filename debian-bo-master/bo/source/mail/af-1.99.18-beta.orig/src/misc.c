/* Misc.c - Miscellaneous utility routines for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <pwd.h>
#include <varargs.h>
#include <sys/types.h>
#include <sys/param.h>
#include "af.h"
#include "misc.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif /* HAVE_UNAME */

#ifdef HAVE_GETHOSTBYNAME
#include <netdb.h>
#endif /* HAVE_GETHOSTBYNAME */

#ifndef HAVE_SIGPROCMASK
#ifdef HAVE_SIGSETMASK
#include <psignal.h>
#endif /* HAVE_SIGSETMASK */
#endif /* ! HAVE_SIGPROCMASK */

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: misc.c,v 1.38 1997/05/05 02:50:01 malc Exp $";
static char *MiscId = MISCID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *malloc(), *realloc(), *strdup();
extern char *getlogin(), *getenv(), *getcwd();
extern int kill(), strncasecmp(), pclose();
extern int mkupper();
extern unsigned alarm();
extern time_t time();
extern void free(), panic();
extern struct passwd *getpwnam(), *getpwuid();
extern FILE *popen();
extern RETSIGTYPE (*signal())();

#ifdef HAVE_GETHOSTNAME
extern int gethostname();
#else /* ! HAVE_GETHOSTNAME */
#ifdef HAVE_UNAME
extern int uname();
#endif /* HAVE_UNAME */
#endif /* ! HAVE_GETHOSTNAME */

/* Local function declarations */

char *xmalloc(), *xrealloc(), *xstrdup();
char *vstrcat(), *get_user(), *get_home();
int is_header();

/****************************************************************************/
char *xmalloc(siz)
size_t siz;
{
	/* Allocate some memory and then check the return value */

	char *buf;

	if ((buf = malloc(siz)) == NULL) {
		panic("Fatal error: memory exhausted");
	}
	return(buf);
}
/****************************************************************************/
char *xrealloc(ptr, siz)
char *ptr;
size_t siz;
{
	/* Reallocate some memory and then check the return value */

	char *buf;

	if ((buf = realloc(ptr, siz)) == NULL) {
		panic("Fatal error: memory exhausted");
	}
	return(buf);
}
/****************************************************************************/
char *xstrdup(s)
char *s;
{
	/* Duplicate a string and then check the return value */

	char *buf;

	if ((buf = strdup(s)) == NULL) {
		panic("Fatal error: memory exhausted");
	}
	return(buf);
}
/****************************************************************************/
/*VARARGS*/
char *vstrcat(va_alist)
va_dcl
{
	/*
	 * Concatenate a list of strings and return the value as per
	 * strcat, except that the destination is reallocated to size.
	 * The calling sequence is :
	 * 	vstrcat(arg1, arg2, ... , NULL)
	 *
	 * All arguments must be strings.  This could be replaced
	 * by a function calling vsprintf, but there is little
	 * need, and vsprintf is not universally available.
	 */

	char *buf, *arg;
	va_list arglist;

	/* Initialise the varargs handling */

	va_start(arglist);

	/* Initialise the buffer */

	buf = xstrdup("");

	/* Now loop through the arguments, adding them */

	while ((arg = va_arg(arglist, char *)) != NULL) {
		buf = xrealloc(buf, strlen(buf) + strlen(arg) + 1);
		(void) strcat(buf, arg);
	}

	/* Clean up the varargs handling and return the buffer */

	va_end(arglist);
	return(buf);
}
/****************************************************************************/
char *get_host()
{
	/* Return the hostname of the machine we're running on */

	static char hostname[BUFSIZ] = "";

#ifdef HAVE_UNAME
	struct utsname uts;
#endif /* HAVE_UNAME */
#ifdef HAVE_GETHOSTBYNAME
	struct hostent *hdata;
#endif /* HAVE_GETHOSTBYNAME */

#ifdef HAVE_GETHOSTNAME
	/* We can use gethostname to get the name */

	gethostname(hostname, BUFSIZ);
#else /* ! HAVE_GETHOSTNAME */
#ifdef HAVE_UNAME
	/* We can get the hostname via uname() */

	(void) uname(&uts);
	(void) strcpy(hostname, uts.nodename);
#else /* ! HAVE_UNAME */
	/* We will have to rely on the compiled-in hostname */

	(void) strcpy(hostname, HOSTNAME);
#endif /* ! HAVE_UNAME */
#endif /* ! HAVE_GETHOSTNAME */

#ifdef HAVE_GETHOSTBYNAME
	/* We have gethostbyname to get the FQDN */

	if ((hdata = gethostbyname(hostname)) != NULL) {
		(void) strcpy(hostname, hdata->h_name);
	}
#endif /* HAVE_GETHOSTBYNAME */

	return(hostname);
}
/****************************************************************************/
char *get_user()
{
	/* Return the name of the current user of af */

	static char *username = NULL;

	char *login;
	uid_t uid;
	struct passwd *pw;

	/* If username is already set then don't recalculate it */

	if (username != NULL) {
		return(username);
	}

	/* Get the uid of the current user */

	uid = getuid();

	/* Try and find a user name via getlogin */

	if ((login = getlogin()) != NULL) {
		/* Check this is correct; may be wrong after an su */

		if ((pw = getpwnam(login)) != NULL && pw->pw_uid == uid) {
			username = xstrdup(login);
			return(username);
		}
	}

	/* If we've got this far then we must set username via getpwuid() */

	pw = getpwuid(uid);
	username = xstrdup((pw != NULL) ? pw->pw_name : "");
	return(username);
}
/****************************************************************************/
char *get_realname()
{
	/* Return the user's real name, if any available */

	static char *realname = NULL;

	char *p, *q, *name;
	struct passwd *pw;

	/* Don't bother to recalculate the real name */

	if (realname != NULL) {
		return(realname);
	}

	/* If the name environmental is set then use that */

	if ((name = getenv(NAME)) != NULL) {
		realname = xstrdup(name);
		return(realname);
	}

	/* Get the password file entry */

	pw = getpwnam(get_user());

	/* Allocate the real name to the maximum size */

	realname = xmalloc(strlen(pw->pw_gecos) + strlen(pw->pw_name));

	/* Initialise p to the start of the gecos field */

	p = pw->pw_gecos;

#ifdef SVR4_PASSWD
	/* Skip up to any '-' in the gecos field */

	p = ((q = strchr(pw->pw_gecos, '-')) != NULL) ? q + 1 : p;
#endif /* SVR4_PASSWD */

	/* Now copy the gecos field into the real name */

	q = realname;
	while (*p != '\0') {
#ifdef SVR4_PASSWD
		/* The real name may be delimited by '(' */

		if (*p == '(') {
			break;
		}

#else /* ! SVR4_PASSWD */
		
		/* Expand '&' to the user name, capitalised */

		if (*p == '&') {
			*q = mkupper(*(pw->pw_name));
			(void) strcpy(q + 1, pw->pw_name + 1);
			q += strlen(pw->pw_name);
			p++;
			continue;
		}

		/* The real name may be delimited by ',' */

		if (*p == ',') {
			break;
		}
#endif /* ! SVR4_PASSWD */

		/* Just copy the character across */

		*q++ = *p++;
	}

	/* Null terminate the string */

	*q = '\0';

	/* Reallocate the name to the actual size and return it */

	realname = xrealloc(realname, strlen(realname) + 1);
	return(realname);
}
/****************************************************************************/
char *get_home(user)
char *user;
{
	/* Return user's home directory, or the user's if user is NULL */

	static char *hdir = NULL;
	char *username, *home;
	struct passwd *pw;

	/* Free any previous return buffer */

	if (hdir != NULL) {
		free(hdir);
		hdir = NULL;
	}

	/* If user is NULL assume current user, try $HOME */

	if (user == NULL && (home = getenv(HOME)) != NULL) {
		hdir = xstrdup(home);
		return(hdir);
	}

	/* Set the name of the user */

	username = (user != NULL) ? user : get_user();

	/* Get any relevant entry from the password file */

	if ((pw = getpwnam(username)) != NULL) {
		hdir = xstrdup(pw->pw_dir);
	}

	/* Return any home directory found */

	return(hdir);
}
/****************************************************************************/
char *get_pwd(refresh)
int refresh;
{
	/* Return the current working directory */

	static char dir[MAXPATHLEN] = "";

	/* Return the stored value or recalculate it */

	return((!refresh && strlen(dir) > 0 ||
		getcwd(dir, MAXPATHLEN) != NULL) ? dir : NULL);
}
/****************************************************************************/
char *get_mailbox(user)
char *user;
{
	/* Return user's incoming mailbox, or the user's if user is NULL */

	static char *mailbox = NULL;
	char *username, *mail;

#ifdef DEFAULT_MAILFILE
	char *homedir;
#endif /* DEFAULT_MAILFILE */

	/* Free any previous return buffer */

	if (mailbox != NULL) {
		free(mailbox);
	}

	/* If user is NULL assume current user, try $MAIL */

	if (user == NULL && (mail = getenv(MAIL)) != NULL) {
		mailbox = xstrdup(mail);
		return(mailbox);
	}

	/* Set the name of the user */

	username = (user != NULL) ? user : get_user();

	/* Build the name of the user's mailbox */

#ifdef DEFAULT_POP3_HOST
	/* Incoming mailbox is POP3 mailbox :user@DEFAULT_POP3_HOST */

	mailbox = vstrcat(":", username, "@", DEFAULT_POP3_HOST, NULL);

#else /* ! DEFAULT_POP3_HOST */
#ifdef DEFAULT_MAILFILE

	/* Incoming mailbox is $HOME/DEFAULT_MAILFILE */

	if ((homedir = get_home(username)) == NULL) {
		return(NULL);
	}
	mailbox = vstrcat(homedir, "/", DEFAULT_MAILFILE, NULL);

#else /* ! DEFAULT_MAILFILE */

	/* Incoming mailbox is MAILDIR/username */

	mailbox = vstrcat(MAILDIR, "/", username, NULL);
#endif /* ! DEFAULT_MAILFILE */
#endif /* ! DEFAULT_POP3_HOST */

	return(mailbox);
}
/****************************************************************************/
char *utos(num)
unsigned num;
{
	/* Return a static buffer containing num as a string */

	static char buf[MAXUSTRLEN + 1];

	/* Simply fill and return the buffer */

	(void) sprintf(buf, "%u", num);
	return(buf);
}
/****************************************************************************/
char *get_line(fp, fold)
FILE *fp;
int fold;
{
	/*
	 * Read an arbitrarily long line from the file pointed
	 * to by fp into a string, returning an allocated string
	 * containing the line.
	 * If fold is true then process folded lines (newlines
	 * followed by white space are not line separators).
	 */

	char *line = NULL, *tmp;
	int c, in_fold = FALSE;

	/* Read physical lines until we have a full logical line */

	do {
		/* Allocate space for the physical line */

		tmp = xmalloc(BUFSIZ);

		/* Read the next physical line */

		if (fgets(tmp, BUFSIZ, fp) == NULL) {
			free(tmp);
			break;
		}

		/* Now add the plysical line to the logical */

		if (line == NULL) {
			line = tmp;
		} else {
			line = xrealloc(line, strlen(line) + strlen(tmp) + 1);
			(void) strcat(line, tmp);
			free(tmp);
		}

		/* Process folded headers if required */

		if (fold && is_header(line)) {
			/* Check for a fold */

			c = getc(fp);

			/* Push the character back */

			if (c != EOF) {
				(void) ungetc(c, fp);
			}

			/* Check for a linear-white-space character */

			in_fold = (c == ' ' || c == '\t');
		}
	} while (line[strlen(line) - 1] != '\n' || in_fold);

	/* Reallocate the line and return it */

	line = (line != NULL) ? xrealloc(line, strlen(line) + 1) : NULL;
	return(line);
}
/****************************************************************************/
int is_header(line)
char *line;
{
	/*
	 * Return TRUE if line contains a mail header, FALSE otherwise.
	 * This implies that the line must begin with a sequence of
	 * non-whitespace printable characters terminated by a colon.
	 */

	char *p;

	/* Loop through the line checking characters */

	for (p = line; *p != '\0'; p++) {
		if (*p == ':') {
			return(p != line);
		} else if (!isascii(*p) || !isprint(*p) || isspace(*p)) {
			return(FALSE);
		}
	}

	/* If we reach here its not a header */

	return(FALSE);
}
/****************************************************************************/
int is_fromline(line)
char *line;
{
	/* Return TRUE if the line is a 'From ' or '>From ' line */

	return(!strncmp(line, MFROM, strlen(MFROM)) ||
		!strncmp(line, UUCPFROM, strlen(UUCPFROM)));
}
/****************************************************************************/
int is_blank(line)
char *line;
{
	/* Return TRUE if line is blank */

	char *p;

	/* Loop through the line checking characters */

	for (p = line; *p != '\0'; p++) {
		if (!isspace(*p)) {
			return(FALSE);
		}
	}

	/* The line is indeed blank */

	return(TRUE);
}
/****************************************************************************/
int listed(line, list)
char *line, *list;
{
	/* Return TRUE if the header in line is given in list */

	/*
	 * This flag tells us the last header was found, so if
	 * we hit a fold then we treat it the same way.
	 */

	static int last_found = FALSE;

	char *hdr, *end;
	unsigned len;

	/* if list is NULL then no headers are listed */

	if (list == NULL) {
		return(FALSE);
	}

	/* If this is a fold, then just check last_found */

	if (isspace(*line)) {
		return(last_found);
	}

	/* Loop through checking line against the entry in noshow */

	hdr = list;
	while (hdr != NULL && *hdr != '\0') {
		/* Find the end of the not-displayed header */

		end = strchr(hdr + 1, ':');

		/* How long is the current header? */

		len = (end != NULL) ? end - hdr + 1 : strlen(hdr);

		/* Is this the header we're looking for? */

		if (!strncasecmp(line, hdr, len)) {
			last_found = TRUE;
			return(TRUE);
		}

		/* Update the loop counter */

		hdr = (end != NULL) ? end + 1 : NULL;
	}

	/* The header wasn't in the list */

	last_found = FALSE;
	return(FALSE);
}
/****************************************************************************/
unsigned save_atimer(atimer)
ATIMER *atimer;
{
	/* Disable any alarm timer, saving the details in atimer */

	sigset_t pending, sigset, old_sigset;

	/* Set up a sigset containing SIGALRM */

	(void) sigemptyset(&sigset);
	(void) sigaddset(&sigset, SIGALRM);

	/* Turn off the alarm timer, saving it's status */

	atimer->seconds = alarm(0);
	atimer->disabled = time(NULL);

	/* Check what signals are pending */

	(void) sigpending(&pending);

	/* Now ignore the alarm handler, saving the old one */

	atimer->handler = signal(SIGALRM, SIG_IGN);

	/* At last we can unblock the signal */

	(void) sigprocmask(SIG_UNBLOCK, &sigset, &old_sigset);

	/* And store whether the signal was blocked or pending */

	atimer->blocked = sigismember(&old_sigset, SIGALRM);
	atimer->pending = sigismember(&pending, SIGALRM);

	/* Return the number of seconds in the timer */

	return(atimer->seconds);
}
/****************************************************************************/
unsigned restore_atimer(atimer)
ATIMER *atimer;
{
	/* Restore an alarm timer, continuing it's count */

	long seconds;
	sigset_t sigset;

	/* First we should disable the alarm clock */

	(void) alarm(0);

	/* How long does the old timer have left to run? */

	seconds = atimer->seconds + (atimer->disabled - time(NULL));

	/* Reset the signal mask if required */

	if (atimer->blocked) {
		/* Set up a sigset containing SIGALRM */

		(void) sigemptyset(&sigset);
		(void) sigaddset(&sigset, SIGALRM);

		/* And block the signal for this process */

		(void) sigprocmask(SIG_BLOCK, &sigset, NULL);
	}

	/* Now reinstate the signal handler */

	(void) signal(SIGALRM, atimer->handler);

	/* Reinstate any pending signal */

	if (atimer->pending || atimer->seconds && seconds <= 0) {
		(void) kill(getpid(), SIGALRM);
	}

	/* Reinstate the alarm clock and return the time left */

	return(alarm((seconds > 0) ? seconds : 0));
}
/****************************************************************************/
