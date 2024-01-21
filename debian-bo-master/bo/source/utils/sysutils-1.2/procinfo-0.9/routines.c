/*

   routines.c

   routines for *info

   Date:        1995-04-15 23:51:55
   Last Change: 1996-04-24 16:13:31

   Copyright (C) 1995, 1996 Sander van Malssen <svm@kozmix.ow.nl>

   This software is released under the GNU Public Licence. See the
   file `COPYING' for details. Since you're probably running Linux I'm
   sure your hard disk is already infested with copies of this file,
   but if not, mail me and I'll send you one.

 */

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <termios.h>
#include <termcap.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/ioctl.h>

static char *rcsid = "$Id: routines.c,v 1.10 1996/04/24 14:15:16 svm Exp $";

#include "procinfo.h"

extern char *cd, *ce, *cl, *cm, *ho, *se, *so, *ve, *vi;
extern int co, li, sg;
extern int fs, redrawn;

FILE *
myfopen (char *name)
{
    FILE *fp;

    if ((fp = fopen (name, "r")) == NULL) {
	fprintf (stdout, "can't open file %s: %s\n", name, strerror (errno));
	exit (1);
    }
    return fp;
}

/* Note: we're using a static char array, so use this only once per printf. */
char *
hms (long t)
{
    int d, h, m, s;
    static char buf[22];

    d = (int) (t / 8640000);
    t = t - (long) (d * 8640000);
    h = (int) (t / 360000);
    t = t - (long) (h * 360000);
    m = (int) (t / 6000);
    t = t - (long) (m * 6000);
    s = (int) (t / 100);
    t = t - (long) (s * 100);
    if (d > 0)
	sprintf (buf, "%3dd %2d:%02d:%02d.%02d", d, h, m, s, (int) t);
    else
	sprintf (buf, "     %2d:%02d:%02d.%02d", h, m, s, (int) t);
    return buf;
}

/* Note: we're using a static char array, so use this only once per printf. */
char *
perc (long i, long t)
{
    int v;
    static char buf[16];

    v = (int) (i < 1000000 ?
	       ((1000 * i + t / 2) / t) :
	       ((i + t / 2000) / (t / 1000)));
    sprintf (buf, "%3d.%d%%", v / 10, v % 10);
    return buf;
}

void
bye (int i)
{
    if (i == SIGINT) {		/* The 'official' exit keystroke. */
	printf ("%s%s%s", ve, se, tgoto (cm, 0, li + 1));
	exit (0);
    } else {
	printf ("%s%s%s", ve, se, tgoto (cm, 0, li));
	printf ("[%s]\n", sys_siglist[i]);
	exit (128 + i);
    }
}

void
tstp (int i)
{
   /* Restore cursor & attributes, then do the actual suspend. Should be fun
      to try this unaltered on BSD-based systems. :-) */
    printf ("%s%s%s", ve, se, tgoto (cm, 0, li - 1));
    fflush (stdout);
    raise (SIGTSTP);
}

void
cont (int i)
{
    signal (SIGTSTP, tstp);
    signal (SIGCONT, cont);
    printf ("%s%s", cl, vi);
    fflush (stdout);
}

/* This function stolen from top(1) (kmem-version). */
extern FILE *versionfp;
extern char *version;

void
window_init (int i)
{
    struct winsize ws;
    struct sigaction sa;

    co = li = 0;
    if (ioctl (1, TIOCGWINSZ, &ws) >= 0) {
	co = ws.ws_col;
	li = ws.ws_row;
	version = make_version (versionfp);
    }
    if (co == 0)
	co = tgetnum ("co");
    if (li == 0)
	li = tgetnum ("li");
    li -= 2;
    if (!isatty(fileno(stdout)))
	co = 80;

    if (fs)
	redrawn = 1;
    sa.sa_handler = window_init;
    sa.sa_flags = 0;
    sigemptyset (&sa.sa_mask);
    sigaction (SIGWINCH, &sa, NULL);
    sigaction (SIGQUIT, &sa, NULL);
}

/*
   This stuff is either directly stolen or else adapted from the GNU
   termcap manual, and therefore presumed copyrighted by the FSF.
   See the file `COPYING' for licencing details.

   -svm
 */

void
fatal (const char *s,...)
{
    va_list l;

    va_start (l, s);
    vfprintf (stderr, s, l);
    va_end (l);

    exit (EXIT_FAILURE);
}

#ifdef __GNUC__
#define term_buffer 0
#else
static char term_buffer[2048];

#endif

void
init_terminal_data ()
{
    char *termtype;
    int success;

    if (!(termtype = getenv ("TERM")))
	fatal ("Specify a terminal type with `setenv TERM <yourtype>'.\n");

    success = tgetent (term_buffer, termtype);
    if (success < 0)
	fatal ("Could not access the termcap data base.\n");
    if (success == 0)
	fatal ("Terminal type `%s' is not defined.\n", termtype);
}

char *
my_tgets (char *te)
{
#ifdef __GNUC__
#define BUFFADDR 0
#else
   /* Here we assume that an explicit term_buffer was provided to tgetent. */
    char *buffer = (char *) malloc (strlen (term_buffer));

#define BUFFADDR &buffer
#endif
    char *temp;

   /* Extract information we will use. */
    if ((temp = tgetstr (te, BUFFADDR)) != NULL)
	return temp;
    else
	return "";
}

char *
make_version (FILE * versionfp)
{
    char line[1024], myname[65];
    static char wheee[1024];
    char *p = line, *here, *there;
    size_t len;

   /* These are the bits of /proc/version */
    char ver[64], host[1024], gcc[1024], date[1024];
    int compno;

    gethostname (myname, 65);

    fseek (versionfp, 0L, SEEK_SET);
    fgets (line, sizeof (line), versionfp);

    sscanf (line, "Linux version %s (%[^)]) (gcc version %[^)]) #%d %[^\n]", ver, host,
	    gcc, &compno, date);

    here = strdup (myname);
    there = strdup (host);

   /* First, let's see what happens if we put everything in. */
    sprintf (wheee, "Linux %s (%s) (gcc %s) #%d %s [%s]",
	     ver, host, gcc, compno, date, myname);

   /* Too long: truncate this system's name. */
    if ((len = strlen (wheee)) > co) {
	for (p = myname; *p; p++)
	    if (*p == '.') {
		*p = '\0';
		break;
	    }
	sprintf (wheee, "Linux %s (%s) (gcc %s) #%d %s [%s]",
		 ver, host, gcc, compno, date, myname);
    }
   /* Too long: truncate compiling system's name. */
    if ((len = strlen (wheee)) > co) {
	for (p = host; *p; p++)
	    if (*p == '.') {
		*p = '\0';
		break;
	    }
	sprintf (wheee, "Linux %s (%s) (gcc %s) #%d %s [%s]",
		 ver, host, gcc, compno, date, myname);
    }
   /* Restore hostnames, try again without date. */
    strcpy (myname, here);
    free (here);
    strcpy (host, there);
    free (there);

   /* Too long: try without date. */
    if ((len = strlen (wheee)) > co)
	sprintf (wheee, "Linux %s (%s) (gcc %s) #%d [%s]",
		 ver, host, gcc, compno, myname);

   /* Too long: truncate this system's name. */
    if ((len = strlen (wheee)) > co) {
	for (p = myname; *p; p++)
	    if (*p == '.') {
		*p = '\0';
		break;
	    }
	sprintf (wheee, "Linux %s (%s) (gcc %s) #%d [%s]",
		 ver, host, gcc, compno, myname);
    }
   /* Too long: truncate compiling system's name. */
    if ((len = strlen (wheee)) > co) {
	for (p = host; *p; p++)
	    if (*p == '.') {
		*p = '\0';
		break;
	    }
	sprintf (wheee, "Linux %s (%s) (gcc %s) #%d [%s]",
		 ver, host, gcc, compno, myname);
    }
   /* version = &wheee; */

    return wheee;
}
