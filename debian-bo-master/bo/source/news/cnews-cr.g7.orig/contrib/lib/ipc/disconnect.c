#include <stdio.h>
#include <signal.h>
#include <sys/ioctl.h>

extern int errno;

/*
 * DISCONNECT -- remove our connection with any foreground process.
 * trys to insure that we are immune to vagaries of the controlling tty.
 * Excised from 4.3tahoe sendmail main.c by Geoff Collyer, geoff@utstat
 */

disconnect(fulldrop)
int fulldrop;			/* also drop controlling tty? */
{
	int fd;

	/* be sure we don't get nasty signals */
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGPIPE, SIG_IGN);

	/* all input from /dev/null */
	(void) freopen("/dev/null", "r", stdin);
	(void) fclose(stdout);
	(void) fclose(stderr);
	while ((fd = dup(0)) < 2 && fd > 0)
		continue;

#ifdef TIOCNOTTY
	/* drop our controlling TTY completely if possible */
	if (fulldrop) {
		fd = open("/dev/tty", 2);
		if (fd >= 0) {
			(void) ioctl(fd, (int)TIOCNOTTY, (char *)0);
			(void) close(fd);
		}
		(void) setpgrp(0, 0);
	}
#endif TIOCNOTTY
	errno = 0;
}

/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
