/*
 * $XConsortium: sessreg.c,v 1.11 94/04/17 20:03:46 rws Exp $
 *
Copyright (c) 1990  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * sessreg
 *
 * simple wtmp/utmp frobber
 *
 * usage: sessreg [ -w <wtmp-file> ] [ -u <utmp-file> ]
 *		  [ -l <line> ]
 *		  [ -h <host-name> ]				/ BSD only
 *		  [ -s <slot-number> ] [ -x Xservers-file ]	/ BSD only
 *		  [ -t <ttys-file> ]				/ BSD only
 *	          [ -a ] [ -d ] user-name
 *
 * one of -a or -d must be specified
 */

# include	<X11/Xos.h>
# include	<X11/Xfuncs.h>
# include	<stdlib.h>
# include	<stdio.h>
# include	<utmp.h>
# include	<lastlog.h>
# include	<pwd.h>

#ifdef SVR4
#define SYSV			/* nice System V utmp interface still the same */
#endif

#ifndef WTMP_FILE
# define WTMP_FILE	"/usr/adm/wtmp"
#endif
#ifndef UTMP_FILE
# define UTMP_FILE	"/etc/utmp"
#endif
#ifndef _PATH_LASTLOG
# define _PATH_LASTLOG	"/usr/adm/lastlog"
#endif
#ifndef SYSV
# ifndef SERVERS_FILE
#  define SERVERS_FILE	"/usr/lib/X11/xdm/Xservers"
# endif
# ifndef TTYS_FILE
#  define TTYS_FILE	"/etc/ttys"
# endif
#endif

#ifdef X_NOT_STDC_ENV
#define Time_t long
extern Time_t time ();
#else
#include <time.h>
#define Time_t time_t
#endif
#ifdef X_NOT_POSIX
extern long	lseek ();
extern char	*ttyname ();
#endif

int	wflag, uflag, lflag;
char	*wtmp_file, *utmp_file, *line;
int	utmp_none, wtmp_none;
/*
 * BSD specific variables.  To make life much easier for Xstartup/Xreset
 * maintainers, these arguments are accepted but ignored for sysV
 */
int	hflag, sflag, xflag, tflag;
char	*host_name;
int	slot_number;
char	*xservers_file, *ttys_file;
char	*user_name;
int	aflag, dflag;

char	*program_name;
void    set_utmp (struct utmp *u, char *line, char *user, char *host, Time_t date, int addp);
void	lastlog();

int usage (int x)
{
	if (x) {
		fprintf (stderr, "%s: usage %s {-a -d} [-w wtmp-file] [-u utmp-file]\n", program_name, program_name);
		fprintf (stderr, "             [-t ttys-file] [-l line-name] [-h host-name]\n");
		fprintf (stderr, "             [-s slot-number] [-x servers-file] user-name\n");
		exit (1);
	}
	return x;
}

char *
getstring (avp, flagp)
char	***avp;
int	*flagp;
{
	char	**a = *avp;

	usage ((*flagp)++);
	if (*++*a)
		return *a;
	++a;
	usage (!*a);
	*avp = a;
	return *a;
}

long syserr (x, s)
long	x;
char	*s;
{
	if (x == -1) {
		perror (s);
		exit (1);
	}
	return x;
}

long sysnerr (x, s)
long	x;
char	*s;
{
	if (x == 0) {
		perror (s);
		exit (1);
	}
	return x;
}

int main (argc, argv)
int	argc;
char	**argv;
{
#ifndef SYSV
	int		utmp;
#endif
	char		*line_tmp;
	int		wtmp;
	Time_t		current_time;
	struct utmp	utmp_entry;

	program_name = argv[0];
	while (*++argv && **argv == '-') {
		switch (*++*argv) {
		case 'w':
			wtmp_file = getstring (&argv, &wflag);
			if (!strcmp (wtmp_file, "none"))
				wtmp_none = 1;
			break;
		case 'u':
			utmp_file = getstring (&argv, &uflag);
			if (!strcmp (utmp_file, "none"))
				utmp_none = 1;
			break;
		case 't':
			ttys_file = getstring (&argv, &tflag);
			break;
		case 'l':
			line = getstring (&argv, &lflag);
			break;
		case 'h':
			host_name = getstring (&argv, &hflag);
			break;
		case 's':
			slot_number = atoi (getstring (&argv, &sflag));
			break;
		case 'x':
			xservers_file = getstring (&argv, &xflag);
			break;
		case 'a':
			aflag++;
			break;
		case 'd':
			dflag++;
			break;
		default:
			usage (1);
		}
	}
	usage (!(user_name = *argv++));
	usage (*argv != 0);
	/*
	 * complain if neither aflag nor dflag are set,
	 * or if both are set.
	 */
	usage (!(aflag ^ dflag));
	usage (xflag && !lflag);
	/* set up default file names */
	if (!wflag)
		wtmp_file = WTMP_FILE;
	if (!uflag)
		utmp_file = UTMP_FILE;
#ifndef SYSV
	if (!tflag)
		ttys_file = TTYS_FILE;
	if (!sflag) {
		if (xflag)
			sysnerr (slot_number = Xslot (ttys_file, xservers_file, line), "Xslot");
		else
			sysnerr (slot_number = ttyslot (), "ttyslot");
	}
#endif
	if (!lflag) {
		sysnerr ((long) (line_tmp = ttyname (0)), "ttyname");
		line = strrchr(line_tmp, '/');
		if (line)
			line = line + 1;
		else
			line = line_tmp;
	} else {
		if ((line_tmp = strrchr(line,':')) != 0) {
			if (!hflag) 
				host_name = "console";
			line = line_tmp;
		}
	}
	current_time = time ((Time_t *) 0);
	set_utmp (&utmp_entry, line, user_name, host_name, current_time, aflag);
	if (!utmp_none) {
#ifdef SYSV
		utmpname (utmp_file);
		setutent ();
		(void) getutid (&utmp_entry);
		pututline (&utmp_entry);
		endutent ();
#else
		utmp = open (utmp_file, O_RDWR);
		if (utmp != -1) {
			syserr (lseek (utmp, (long) (slot_number - 1) * sizeof (struct utmp), 0), "lseek");
			sysnerr (write (utmp, (char *) &utmp_entry, sizeof (utmp_entry))
				        == sizeof (utmp_entry), "write utmp entry");
			close (utmp);
		}
#endif
	}
	if (!wtmp_none) {
		wtmp = open (wtmp_file, O_WRONLY|O_APPEND);
		if (wtmp != -1) {
			sysnerr (write (wtmp, (char *) &utmp_entry, sizeof (utmp_entry))
				        == sizeof (utmp_entry), "write wtmp entry");
			close (wtmp);
		}
	}
	return 0;
}

/*
 * fill in the appropriate records of the utmp entry
 */

void set_utmp (u, line, user, host, date, addp)
struct utmp	*u;
char		*line, *user, *host;
Time_t		date;
int		addp;
{
	if (line)
		(void) strncpy (u->ut_line, line, sizeof (u->ut_line));
	else
		bzero (u->ut_line, sizeof (u->ut_line));
	if (addp && user)
		(void) strncpy (u->ut_name, user, sizeof (u->ut_name));
	else
		bzero (u->ut_name, sizeof (u->ut_name));
#ifdef SYSV
	if (line) {
		int	i;
		/*
		 * this is a bit crufty, but
		 * follows the apparent conventions in
		 * the ttys file.  ut_id is only 4 bytes
		 * long, and the last 4 bytes of the line
		 * name are written into it, left justified.
		 */
		i = strlen (line);
		if (i >= sizeof (u->ut_id))
			i -= sizeof (u->ut_id);
		else
			i = 0;
		(void) strncpy (u->ut_id, line + i, sizeof (u->ut_id));
	} else
		bzero (u->ut_id, sizeof (u->ut_id));
	if (addp) {
		u->ut_pid = getppid ();
		u->ut_type = USER_PROCESS;
	} else {
		u->ut_pid = 0;
		u->ut_type = DEAD_PROCESS;
	}
#endif
	if (addp && host)
		(void) strncpy (u->ut_host, host, sizeof (u->ut_host));
	else
		bzero (u->ut_host, sizeof (u->ut_host));
	u->ut_time = date;

	if (addp)
		lastlog();
}

#ifndef SYSV
/*
 * compute the slot-number for an X display.  This is computed
 * by counting the lines in /etc/ttys and adding the line-number
 * that the display appears on in Xservers.  This is a poor
 * design, but is limited by the non-existant interface to utmp.
 */

Xslot (ttys_file, servers_file, display_name)
char	*ttys_file;
char	*servers_file;
char	*display_name;
{
	FILE	*ttys, *servers;
	int	c;
	int	slot = 1;
	int	column0 = 1;
	char	servers_line[1024];
	char	disp_name[512];
	int	len;
	char	*pos;

	/* remove screen number from the display name */
	strcpy(disp_name, display_name);
	pos = strrchr(disp_name, ':');
	if (pos) {
	    pos = strchr(pos, '.');
	    if (pos)
		*pos = '\0';
	}
	sysnerr (ttys = fopen (ttys_file, "r"), ttys_file);
	while ((c = getc (ttys)) != EOF)
		if (c == '\n') {
			++slot;
			column0 = 1;
		} else
			column0 = 0;
	if (!column0)
		++slot;
	(void) fclose (ttys);
	sysnerr (servers = fopen (servers_file, "r"), servers_file);
	len = strlen (disp_name);
	column0 = 1;
	while (fgets (servers_line, sizeof (servers_line), servers)) {
		if (column0 && *servers_line != '#') {
			if (!strncmp (disp_name, servers_line, len) &&
			    (servers_line[len] == ' ' ||
			     servers_line[len] == '\t'))
				return slot;
			++slot;
		}
		if (servers_line[strlen(servers_line)-1] != '\n')
			column0 = 0;
		else
			column0 = 1;
	}
	return 0;
}
#endif

void lastlog()
{
        struct lastlog ll;
        int fd;
	struct passwd *pwd = getpwnam(user_name);

        if ((fd = open(_PATH_LASTLOG, O_RDWR, 0)) >= 0) {
		syserr (lseek(fd, (off_t)pwd->pw_uid * sizeof(ll),  0), "lseek");
                memset((char *)&ll, 0, sizeof(ll));
                (void)time(&ll.ll_time);
                strncpy(ll.ll_line, line, sizeof(ll.ll_line));
                if (host_name)
                        strncpy(ll.ll_host, host_name, sizeof(ll.ll_host));
		sysnerr (write(fd, (char *)&ll, sizeof(ll)) == sizeof (ll), "write lastlog entry");
                (void)close(fd);
        }
}
