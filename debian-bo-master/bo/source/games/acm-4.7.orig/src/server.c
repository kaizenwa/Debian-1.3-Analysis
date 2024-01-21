/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1994  Riley Rainey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 dated June, 1991.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program;  if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
 */

#if defined(SVR4)
#include <sys/filio.h>
#endif

#include "manifest.h"
#include "patchlevel.h"
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <pwd.h>
#include <signal.h>
#include <sys/socket.h>
#include "pm.h"
#undef a

#ifdef SVR4
/*
 * This forces the inclusion of filio.h (and other things) in ioctl.h 
 * on NCR SVR4.
 */
#define BSD_COMP 1
#endif /* SVR4 */

#include <sys/ioctl.h>
#include <sys/time.h>
#ifdef _AIX
#include <sys/select.h>
#endif
#include <netinet/in.h>
#include <netdb.h>
#include <setjmp.h>

#ifdef SVR4
static sigset_t    sigset_mask, sigset_omask;
#endif /* SVR4 */

#if !defined(lint) && !defined(SABER)
static char sccsid[] = "@(#) acm by Riley Rainey; Revision 3.0";
#endif

extern struct servent *getservent();
int sdebug = 1;
int listen_socket;
char *sceneFile = (char *) NULL;
extern double atof();

void parseinfo (s, a, b, c)
char *s, *a, *b, *c; {

	char	*p;

	for (p=a; *s; ++s, ++p)
		if ((*p = *s) == ' ') {
			*p = '\0';
			break;
		}

	++ s;

	for (p=b; *s; ++s, ++p)
		if ((*p = *s) == ' ') {
			*p = '\0';
			break;
		}

	++ s;

	strcpy (c, s);

	return;
}

main (argc, argv)
int	argc;
char	*argv[]; {

	struct sockaddr_in sin;
#ifdef notdef
	struct	servent *sp;
#endif
	int	on = 1;
	int	i, background = 0;
	
	droneAggressiveness = DEFAULT_DRONE_FACTOR;

	ptblCount = ctblCount = 0;
	chaseOthers = 0;
	visibility = 50.0 * NM;

/*
 *  parse arguments
 */

	for (i=1; i<argc; ++i) {

		if (*argv[i] == '-')
			switch (*(argv[i]+1)) {

			case 'b':
				background = 1;
				break;

			case 's':
				sceneFile = argv[++i];
				break;

			case 'a':
			case 'g':
				arcadeMode = 1;
				break;

			case 'c':
				chaseOthers = 1;
				break;

			case 'd':
				if (strcmp (argv[i], "-da") == 0 && argv[++i]) {
				    droneAggressiveness = atof(argv[i]) * NM;
				    if (droneAggressiveness <= MIN_DRONE_FACTOR) {
					droneAggressiveness = MIN_DRONE_FACTOR;
				    }
				    else if (droneAggressiveness > 1.0) {
					droneAggressiveness = 1.0;
				    }
				    break;
				}

			case 'v':
				if (strcmp (argv[i], "-visibility") == 0 &&
				    argv[++i]) {
				    visibility = atof(argv[i]);
				    if (visibility < 1.0) {
				    	visibility = 1.0;
				    }
				    else if (visibility > 500.0) {
				    	visibility = 500.0;
				    }
				    visibility *= NM;
				}
				break;

			default:
				fprintf (stderr, "Invalid switch \"%s\"\n", argv[i]);
				break;
			}
	}

	if (sdebug) {
#ifdef notdef
		if ((sp = getservbyname ("acm", "tcp");
			fprintf (stderr, "can't find acm service\n");
			exit (1);
		}
#endif

		if ((listen_socket = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
			perror ("socket");
			exit (1);
		}

		sin.sin_family = AF_INET;
		sin.sin_addr.s_addr = INADDR_ANY;
		sin.sin_port = htons(ACM_PORT);

	    	(void) setsockopt(listen_socket, SOL_SOCKET, SO_REUSEADDR,
			(char *) &on, sizeof on);

		if (bind (listen_socket, (struct sockaddr *) &sin,
			sizeof(sin)) < 0) {
			perror ("bind");
			exit (1);
		}
	}
	else {
		listen_socket = 0;		/* inetd sets this up for us */
/*		freopen ("/people/riley/acm.error", "a", stderr); */
	}

	ioctl(listen_socket, FIONBIO, (char *) &on);

	if (listen (listen_socket, 5) < 0) {
		perror ("listen");
		close (listen_socket);
		exit (1);
	}

	if (background)
#ifdef HAVE_SETSID
		(void) setsid ();
#else
		(void) setpgrp (0, 1);
#endif

	printf ("\
ACM version %s,  Copyright (C) 1991-1994   Riley Rainey (rainey@netcom.com)\n\n\
ACM comes with ABSOLUTELY NO WARRANTY.\n\
This is free software, and you are welcome to distribute it under the\n\
conditions described in the COPYING file.\n\n", REVISION_STRING);

#if defined(NETAUDIO)
	printf ("This server supports netaudio.\n\n");
#else
#if defined(_HPUX_SOURCE)
	printf ("This server supports the HP AAPI.\n\n");
#endif
#endif

	init ();
	input();
#ifdef LINT
	return 0;
#endif

}

int peerdied = 0;

acm_sig_t
deadpeer ()
{
	fprintf (stderr, "SIGPIPE\n");
	peerdied = 1;
}

static struct	sigvec	alrm, spipe;
int doUpdate = 0;

acm_sig_t
myalarm ()
{
	doUpdate++;
	sigvec (SIGALRM, &alrm, (struct sigvec *) 0);
}

acm_sig_t
killed ()
{
	printf ("\ninterrupt\n");
	shutdown (listen_socket, 2);
	close (listen_socket);
	exit (0);
}

input () {

	fd_set	fdset, ofdset;
	int	news = -1, playerchange = 0, n, pno, addrlen;
	int	on = 1;
	struct	sockaddr addr;
#ifdef USE_ALARM
	struct	itimerval update;
#endif
	char	*bp, buf[128], name[64], display[64], args[256];
	struct  timeval zero_timeout, update_timeout;

	signal (SIGINT, killed);
	signal (SIGQUIT, killed);

	zero_timeout.tv_sec = 0;
	zero_timeout.tv_usec = 0;
	update_timeout.tv_sec = 0;
	update_timeout.tv_usec = UPDATE_INTERVAL;

#ifdef USE_ALARM
	alrm.sv_handler = myalarm;
	alrm.sv_mask = 0;
#ifdef hpux
	alrm.sv_flags = SV_BSDSIG;
#else
	alrm.sv_flags = SV_INTERRUPT;
#endif
	sigvec (SIGALRM, &alrm, (struct sigvec *) 0);
#endif

/*
 *  Set real time clock to interrupt us every UPDATE_INTERVAL usecs.
 */

#ifdef USE_ALARM
	update.it_interval.tv_sec = 0;
	update.it_interval.tv_usec = UPDATE_INTERVAL;
	update.it_value.tv_sec = 0;
	update.it_value.tv_usec = UPDATE_INTERVAL;
	setitimer (ITIMER_REAL, &update, 0);
#endif

	spipe.sv_handler = SIG_DFL;
#ifdef SVR4
	(void) sigemptyset(&sigset_mask);
	spipe.sv_mask = sigset_mask;
#else
	spipe.sv_mask = 0;
#endif /* SVR4 */

#ifdef hpux
	spipe.sv_flags = SV_BSDSIG;
#else
	spipe.sv_flags = SV_INTERRUPT;
#endif
	sigvec (SIGPIPE, &spipe, (struct sigvec *) 0);

	FD_ZERO (&ofdset);
	FD_ZERO (&fdset);
	FD_SET (listen_socket, &ofdset);

	for (;;) {

#ifdef SVR4
		(void) sigprocmask (SIG_SETMASK, &sigset_omask, NULL);
#else
		sigsetmask (0);
#endif /* SVR4 */

		fdset = ofdset;
#ifdef USE_ALARM
		pno = select (32, &fdset, (fd_set *) NULL, (fd_set *) NULL,
			(struct timeval *) NULL);
#else
		if (ptblCount == 0) {
		    pno = select (32, &fdset, (fd_set *) NULL, (fd_set *) NULL,
			&update_timeout);
		}
		else {
		    pno = select (32, &fdset, (fd_set *) NULL, (fd_set *) NULL,
			&zero_timeout);
		}
		doUpdate ++;
#endif

#ifdef SVR4
		(void) sigemptyset (&sigset_mask);
		(void) sigaddset (&sigset_mask, SIGALRM);
		(void) sigprocmask (SIG_BLOCK, &sigset_mask, &sigset_omask);
#else
                sigblock (sigmask(SIGALRM));
#endif /* SVR4 */

		if (pno < 0) {
			FD_CLR (listen_socket, &fdset);
			if (news > 0)
				FD_CLR (news, &fdset);
		}

		if (FD_ISSET (listen_socket, &fdset) ||
			(news > 0 && FD_ISSET(news, &fdset))) {
			if (news == -1) {
				addrlen = sizeof (addr);
				news = accept(listen_socket, &addr, &addrlen);
				if (news > 0) {
					peerdied = 0;
					spipe.sv_handler = deadpeer;
					sigvec(SIGPIPE, &spipe,
						(struct sigvec *) 0);
					ioctl (news, FIONBIO, &on);
					FD_SET (news, &ofdset);
					bp = buf;
				}
			}
			if (news > 0 && FD_ISSET (news, &fdset)) {
				if ((n = read (news, bp, 1)) > 0) {
				    if (*bp == '\n') {
					*bp = '\0';
					parseinfo (buf, display, name, args);
					if (newPlayer (news,
						display, name, args) == 0) {
						write (news, "Ready.\n", 7);
					}
					printf ("%s\n", display);
					close (news);
					news = -1;
				    }
				    else
					bp++;
				    playerchange = 1;
				}
				if (n == 0) {
					peerdied = 1;
				}
			}
		}

		if (peerdied) {
			close (news);
			news = -1;
		}

		if (playerchange) {
			FD_ZERO (&ofdset);
			FD_SET (listen_socket, &ofdset);
			if (news >= 0)
				FD_SET (news, &ofdset);
			spipe.sv_handler = SIG_DFL;
			sigvec (SIGPIPE, &spipe, (struct sigvec *) 0);
			playerchange = 0;
		}

		if (doUpdate) {
			doUpdate = 0;
			redraw ();
		}

	}
}
