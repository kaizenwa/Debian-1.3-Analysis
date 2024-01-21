/*
 * Copyright 1991 - 1993, John F. Haugh II
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by John F. Haugh, II
 *      and other contributors.
 * 4. Neither the name of John F. Haugh, II nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY JOHN HAUGH AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL JOHN HAUGH OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <config.h>

#include "rcsid.h"
RCSID("$Id: logoutd.c,v 1.4 1996/09/25 03:20:02 marekm Exp $")

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <signal.h>
#include <utmp.h>
#include <fcntl.h>
#include "prototypes.h"
#include "defines.h"

#ifdef	SVR4
#include <libgen.h>
#endif

#ifdef	SVR4
#define	signal	sigset
#endif

static char	*Prog;

static char	*mesg_buf = "login time exceeded\r\n";
static int	mesg_len = 21;
static int	mesg_size;

#ifndef HUP_MESG_FILE
#define HUP_MESG_FILE "/etc/logoutd.mesg"
#endif

/*
 * reload_mesg - reload the message that is output when killing a process
 */

static RETSIGTYPE
reload_mesg(sig)
	int sig;
{
	int	fd;
	struct	stat	sb;

	signal (sig, reload_mesg);

	if (stat (HUP_MESG_FILE, &sb))
		return;

	if ((sb.st_mode & S_IFMT) != S_IFREG)
		return;

	if ((fd = open (HUP_MESG_FILE, O_RDONLY)) != -1) {
		if (sb.st_size + 1 > mesg_size) {
			if (mesg_buf && mesg_size)
				free (mesg_buf);

			mesg_len = sb.st_size;
			mesg_size = mesg_len + 1;
			if (! (mesg_buf = (char *) malloc (mesg_len + 1)))
				goto end;
		} else
			mesg_len = sb.st_size;

		if (read (fd, mesg_buf, mesg_len) != mesg_len) {
			mesg_len = 0;
			goto end;
		}
	} else
		return;

end:
	close (fd);
}

/*
 * logoutd - logout daemon to enforce /etc/porttime file policy
 *
 *	logoutd is started at system boot time and enforces the login
 *	time and port restrictions specified in /etc/porttime.  The
 *	utmp file is periodically scanned and offending users are logged
 *	off from the system.
 */

void
main(argc, argv)
	int argc;
	char **argv;
{
	int	i;
	int	status;
	struct	utmp	utmp;
	int	fd;
	char	tty_name[BUFSIZ];
	int	tty_fd;

#ifdef	NDEBUG
	for (i = 0;close (i) == 0;i++)
		;

	/* XXX - Which is better on Linux?  How to test this in
	   configure?  --marekm */
#ifdef	USG
	setpgrp ();
#endif	/* USG */
#if defined(BSD) || defined(SUN) || defined(SUN4) || defined(SVR4)
	setpgid (getpid (), getpid ());
#endif /* BSD || SUN || SUN4 */

	reload_mesg (SIGHUP);

	/*
	 * Put this process in the background.
	 */

	if ((i = fork ()))
		exit (i < 0 ? 1:0);
#endif	/* NDEBUG */

	/*
	 * Start syslogging everything
	 */

	Prog = Basename(argv[0]);

	openlog(Prog, LOG_PID|LOG_CONS|LOG_NOWAIT, LOG_AUTH);

	/*
	 * Scan the UTMP file once per minute looking for users that
	 * are not supposed to still be logged in.
	 */

	while (1) {
#ifdef	NDEBUG
		sleep (60);
#endif

		/* 
		 * Attempt to re-open the utmp file.  The file is only
		 * open while it is being used.
		 */

		if ((fd = open (_UTMP_FILE, 0)) == -1) {
			SYSLOG((LOG_ERR, "cannot open %s - aborting\n",
				_UTMP_FILE));
			closelog();
			exit (1);
		}

		/*
		 * Read all of the entries in the utmp file.  The entries
		 * for login sessions will be checked to see if the user
		 * is permitted to be signed on at this time.
		 */

		while (read (fd, (char *)&utmp, sizeof utmp) == sizeof utmp) {
#ifdef USER_PROCESS
			if (utmp.ut_type != USER_PROCESS)
				continue;
#endif
			if (utmp.UT_USER[0] == '\0')
				continue;
			if (isttytime (utmp.UT_USER, utmp.ut_line, time (0)))
				continue;

			/*
			 * Put the rest of this in a child process.  This
			 * keeps the scan from waiting on other ports to die.
			 */

			if (fork () != 0)
				continue;

			if (strncmp (utmp.ut_line, "/dev/", 5) != 0)
				strcpy (tty_name, "/dev/");
			else
				tty_name[0] = '\0';

			strcat (tty_name, utmp.ut_line);
#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif
			if ((tty_fd = open (tty_name,
					O_WRONLY|O_NDELAY|O_NOCTTY)) != -1)
			{
/* Suggested by Ivan Nejgebauar <ian@unsux.ns.ac.yu>: set OPOST
   before writing the message.  --marekm */
				TERMIO oldt, newt;

				GTTY(tty_fd, &oldt);
				newt = oldt;
#ifdef OPOST
				newt.c_oflag |= OPOST;
#else  /* XXX - I'm too young to know bsd sgtty, sorry :).  --marekm */
#endif
				STTY(tty_fd, &newt);
				write (tty_fd, mesg_buf, mesg_len);
				STTY(tty_fd, &oldt);
				close (tty_fd);
				sleep (5);
			}
#ifdef USER_PROCESS  /* USG_UTMP */
			kill (- utmp.ut_pid, SIGHUP);
			sleep (10);
			kill (- utmp.ut_pid, SIGKILL);
#endif
#if defined(BSD) || defined(SUN) || defined(SUN4)

			/*
			 * vhangup() the line to kill try and kill
			 * whatever is out there using it.
			 */

			strcat (strcpy (tty_name, "/dev/"), utmp.ut_line);
			if ((tty_fd = open (tty_name, O_RDONLY|O_NDELAY)) == -1)
				continue;

			vhangup (tty_fd);
			close (tty_fd);
#endif
			SYSLOG((LOG_NOTICE,
				"logged off user `%.*s' on `%.*s'\n",
				(int) sizeof(utmp.UT_USER), utmp.UT_USER,
				(int) sizeof(utmp.ut_line), utmp.ut_line));

			/*
			 * This child has done all it can, drop dead.
			 */

			exit (0);
		}

		/*
		 * Reap any dead babies ...
		 */

		while (wait (&status) != -1)
			;

		close (fd);
	}
}
