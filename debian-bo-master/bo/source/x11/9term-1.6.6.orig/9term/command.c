/*
 *  Copyright © 1993 James Farrow, University of Sydney - all rights reserved
 *
 *  9term child process management
 */

#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>
#include <sys/wait.h>
#include <signal.h>
#include <grp.h>
#ifdef SOLARIS
#define __EXTENSIONS__
#include <termios.h>
#include <sys/stropts.h>
#else
#include <termio.h>
#endif

#include "9term.h"

#ifdef	RISCOS
#	define	setsid	setpgrp
#endif

int		utmpentry = 0;	/* should we put an entry in utmp? */

int		comm_fd = -1;	/* file descriptor to command interpreter */
int		slave_fd = -1;	/* file descriptor of slave pty */
static	int	comm_pid;	/* process id of command interpreter */
static	char	*slavename; 
static	int	run_command(char*, char*[]);

/*
 *	clean up the utmp file and reset the pty modes
 */
void
quit(int code)
{
	if (slavename) {
		clearutmp(slavename+5, comm_pid);
		chown(slavename, 0, 0);
		chmod(slavename, 0666);
	}
	exit(code);
}
/*
 *  handle SIGCHLD
 */
void
catch_child(void)
{
	if (wait((int *)0) == comm_pid)
		quit(0);
}
/*
 *  handle other signals
 */
void
catch_sig(void)
{
	quit(0);
}

/*
 *	start the indicated command interpreter connected to the slave
 *	end of a pseudo-tty.
 */
static int
run_command(char *command, char *argv[])
{
	char *cp;
	int ptyfd;
	int ttyfd;
	int i;
	int width, height;
	int uid, gid;

		/* Obtain initial tty modes */
	p9ttymodes();
	gttymodes(1);
		/* set up the pty */
	slavename = _getpty(&ptyfd, ORDWR, 0622, 0);
	if (slavename == 0) {
		perror("Can't open a pseudo teletype");
		return(-1);
	}
	fcntl(ptyfd, F_SETFL, O_NONBLOCK);

	/* parent needs a handle on the slave side to change its modes */
	if ((slave_fd = open(slavename, O_RDWR)) < 0) {
		error("could not open slave tty %s", slavename);
		exit(1);
	}
#ifdef SOLARIS
	/* set up the right streams modules for a tty */
	ioctl(slave_fd, I_PUSH, "ptem");        /* push ptem */
	ioctl(slave_fd, I_PUSH, "ldterm");      /* push ldterm */
#endif
#ifdef REMOTE
	ioctl(ptyfd, TIOCREMOTE, 1);
#endif

	/* spin off the command interpreter */
	signal(SIGHUP, catch_sig);
	signal(SIGINT, catch_sig);
	signal(SIGQUIT, catch_sig);
	signal(SIGTERM, catch_sig);
	signal(SIGCHLD, catch_child);
	comm_pid = fork();
	if (comm_pid < 0) {
		error("Can't fork");
		return(-1);
	}

	if (comm_pid == 0) {		/* command interpreter path */
#ifndef	__ultrix
		/* isolate it in a new process group */
		if (setsid() < 0)
			error("failed to set process group");
#else
		if ((ttyfd = open("/dev/tty", O_RDWR)) >= 0) {
			(void) ioctl(ttyfd, TIOCNOTTY, (char *) 0);
			(void) close(ttyfd);
		} else {
			(void) setpgrp(0, 0);
		}
#endif
		/* open slave end of pty */
		if ((ttyfd = open(slavename, O_RDWR)) < 0) {
			error("could not open slave tty %s",slavename);
			exit(1);
		}
#ifdef OSF1
		(void) ioctl(ttyfd, TIOCSCTTY, (char *) 0);
#endif
		/* reset EUID & EGID to real UID and GID */
		uid = getuid();
		gid = getgid();
		fchown(ttyfd, uid, gid);
		fchmod(ttyfd, 0600);
		setuid(uid);
		setgid(gid);

		/* redirect critical fd's and close the rest */
		close(0); dup(ttyfd, 0);
		close(1); dup(ttyfd, 1);
		close(2); dup(ttyfd, 2);
		i = getdtablesize();
		while (i > 2)
			close(i--);
		scr_get_size(&width, &height);
		tty_set_size(0, width, height, Dx(text->r), Dy(text->r));
			/* reset signals and spin off the command interpreter */
		sttymodes(0);
		signal(SIGCHLD, SIG_DFL);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
#ifdef	SIGTSTP
		/* mimick login's behavior by disabling the job control
		   signals; a shell that wants them can turn them back on. */
		signal(SIGTSTP, SIG_IGN);
		signal(SIGTTIN, SIG_IGN);
		signal(SIGTTOU, SIG_IGN);
#endif

		if (*command == '-') {
			command++;
			cp = strrchr(argv[0], '/');
			if (!cp)
				cp = argv[0];
			else
				cp++;
			argv[0] = (char *)malloc(strlen(cp)+2);
			strcpy(argv[0]+1, cp);
			*argv[0] = '-';
		}
		execvp(command, argv);
		error("Couldn't execute %s",command);
		exit(1);
	}
	if (utmpentry)
		updateutmp(slavename+5, comm_pid);
	return(ptyfd);		/* master end of pty */
}

/*
 *	Initialise the command interpreter.  Establish the X server
 *	connection first.
 */
void
init_command(char *command, char **argv)
{
	comm_fd = run_command(command,argv);
	if (comm_fd < 0) {
		error("Quitting");
		quit(1);
	}
}

/*
 *	_killpg: send signal to everyone in process group
 */

void
_killpg(int sig)
{
	if (comm_pid)
		kill(-comm_pid, sig);
}
