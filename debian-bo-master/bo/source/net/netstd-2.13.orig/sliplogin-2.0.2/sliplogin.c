/*-
 * Copyright (c) 1990 The Regents of the University of California
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
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char rcsid[] = "$Id: sliplogin.c,v 1.3 1995/11/12 14:59:31 root Exp root $";
#endif /* not lint */

/*
 * sliplogin.c
 * [MUST BE RUN SUID ROOT]
 *
 * This program initializes its own tty port to be an async TCP/IP interface.
 * It sets the line discipline to slip, invokes a shell script to initialize
 * the network interface, then pauses forever waiting for hangup.
 *
 * It is a remote descendant of several similar programs with incestuous ties:
 * - Kirk Smith's slipconf, modified by Richard Johnsson @ DEC WRL.
 * - slattach, probably by Rick Adams but touched by countless hordes.
 * - the original sliplogin for 4.2bsd, Doug Kingston the mover behind it.
 *
 * There are two forms of usage:
 *
 * "sliplogin"
 * Invoked simply as "sliplogin", the program looks up the username
 * in the file /etc/slip.hosts.
 * If an entry is found, the line on fd0 is configured for SLIP operation
 * as specified in the file.
 *
 * "sliplogin slipuser < /dev/ttyb"
 * Invoked by root with a username, the name is looked up in the
 * /etc/slip.hosts file and if found fd0 is configured as in case 1.
 */

#include "sliplogin.h"

int get_rx_packets(char *ifname, struct enet_statistics *stats);
void scan_sliphosts(struct slipinfo *sinfo);
int translate_speed(speed_t speed);
char *sigstr(int s);

struct slipinfo info;
struct enet_statistics stats;
volatile int HUP=-1;
#define LOGINARGS info.loginname,info.laddr,info.raddr,info.mask, \
				info.option[0],info.option[1],info.option[2]

void packet_watch()
{
static int rx_packets=0;
int actual;

 actual=get_rx_packets(info.unit, &stats);

 if ( (info.timeout<1) || (actual < 0) )
 {
  pause();
  return;
 }
 sleep(info.timeout);
 actual=get_rx_packets(info.unit, &stats);

 if (rx_packets == actual) HUP=SIGUSR1;
 else rx_packets=actual;
}

void close_timeout()
{
 syslog(LOG_INFO, "timeout %s's slip unit %s (%s)\n", info.loginname, info.unit,
           sigstr(HUP));
#ifdef linux
 vhangup();
#endif
 exit(-1);
}

void hup_handler(int s)
{
 HUP=s; /* this avoids race conditions */
}

void exit_sliplogin()
{
	char logoutfile[MAXPATHLEN];
    struct sigaction sa;

	(void)sprintf(logoutfile, "%s.%s", _PATH_LOGOUT, info.loginname);
	if (access(logoutfile, R_OK|X_OK) != 0)
		(void)strcpy(logoutfile, _PATH_LOGOUT);
	if (access(logoutfile, R_OK|X_OK) == 0) {
		char logincmd[2*MAXPATHLEN+32];

		(void) sprintf(logincmd, "%s %s %d %s %s %s %s %s %s %s",
		 logoutfile, info.unit, info.linespeed, LOGINARGS);
		(void) system(logincmd);
	}
	fcntl(0,F_SETFL,O_NONBLOCK); /* because close sometimes never returns */
    sa.sa_handler = close_timeout;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    alarm(15);
    sigaction(SIGALRM,&sa,NULL);
	(void) close(0);
    alarm(0);
    sa.sa_handler = SIG_DFL;
    sigaction(SIGALRM,&sa,NULL);
	syslog(LOG_INFO, "closed %s's slip unit %s (%s)\n", info.loginname,
		 info.unit, sigstr(HUP));
	syslog(LOG_INFO, "Packets sent: %d. Packets received: %d.\n",
			stats.rx_packets, stats.tx_packets);
#ifdef linux
    vhangup();
#endif
	exit(1);
}

int main(int argc, char **argv)
{
	int fd, s, ldisc, odisc, pid, unitnumber;
	speed_t speed;
	struct sigaction sa;
	char *name;
	struct termios tios, otios;
	struct passwd *pw;
	char logincmd[2*BUFSIZ+32];
    char *subenv[] = {
    "PATH=/bin:/usr/bin:/sbin:/etc:/usr/sbin",
    NULL
    };
	extern uid_t getuid();

	if ((name = strrchr(argv[0], '/')) == NULL)
		name = argv[0];
	s = (int) getdtablesize();
	for (fd = 3 ; fd < s ; fd++)
		(void) close(fd);
	openlog(name, LOG_PID, LOG_DAEMON);
	info.uid = getuid();

	/* security */
    environ = subenv;

	if (argc > 1) 
	{
	 if (info.uid!=0) 
	 {
	  fprintf(stderr,"Only root may use arguments\n");
	  exit(-1);
	 }
	 info.loginname=argv[1];
	}
	else
	{
		pw = getpwuid(info.uid);
		if (pw == NULL)
		{
			(void) fprintf(stderr, "access denied - no username\n");
			syslog(LOG_ERR, "access denied - getpwuid returned 0\n");
			exit(1);
		}
		info.loginname=pw->pw_name;
	}

    stats.rx_packets = 0;
    stats.tx_packets = 0;
	/*
	 * Go to background, become process group leader (if not already)
	 * and ensure that the slip line is our controlling terminal.
	 */

    /*
     * if (fork() > 0) exit(0);
     * maybe don't fork and don't go to background
     */

	/*
	 * setsid() to become process group leader ? hmm... no go...
	 * if (setsid() < 0) syslog(LOG_ERR,"can't setsid: %m");
	 */
#ifdef TIOCSCTTY
	if (ioctl(0, TIOCSCTTY, (caddr_t)0) < 0)
		perror("sliplogin: can't set my controlling tty -");
#endif
	scan_sliphosts(&info);
	fchmod(0, 0600);
#ifdef FUNMESSAGE
    fprintf(stderr, "Captain ! Something is lowering our shields!\n");
	fprintf(stderr, "On screen, Mr. Worf !\n");
	fprintf(stderr, "It's %s at coordinates %s\n",info.loginname,info.raddr);
	fprintf(stderr, "Mr. Data, try %s slip to %s\n",info.sm_name,info.laddr);
	fprintf(stderr, "We got him, captain.\n");
#else
#ifdef OLDMESSAGE
	fprintf(stderr, "Starting %s slip for %s\n",info.sm_name,info.loginname);
    fprintf(stderr, "Slip server is %s , you are %s\n", info.laddr,info.raddr);
#else
	fprintf(stderr, "Starting %s slip for %s\n",info.sm_name,info.loginname);
	fprintf(stderr, "Your IP address is %s , the server is %s\n",
					 info.raddr,info.laddr);
#endif
#endif
	fflush(stderr);
	sleep(2); /* Jon Lewis needs this to get the full printf output */
	/* set up the line parameters */
	if (tcgetattr(0, &tios) < 0) {
		syslog(LOG_ERR, "tcgetattr: %m");
		exit(1);
	}
	otios = tios;
	cfmakeraw(&tios);
	tios.c_iflag &= ~IMAXBEL;
	if (tcsetattr(0, TCSAFLUSH, &tios) < 0)
	{
		syslog(LOG_ERR, "tcsetattr: %m");
		exit(1);
	}

	speed = cfgetispeed(&tios);
	info.linespeed=translate_speed(speed);

	/* find out what ldisc we started with */
	if (ioctl(0, TIOCGETD, (caddr_t)&odisc) < 0)
	{
		syslog(LOG_ERR, "ioctl(TIOCGETD) (1): %m");
		exit(1);
	}
	/* switch to slip line discipline */
	ldisc = N_SLIP;
	if (ioctl(0, TIOCSETD, (caddr_t)&ldisc) < 0)
	{
		syslog(LOG_ERR, "ioctl(TIOCSETD): %m");
		exit(1);
	}
	/* find out what unit number we were assigned */
#ifndef linux
	if (ioctl(0, SLIOCGUNIT, (caddr_t) &unitnumber) < 0)
	{
		syslog(LOG_ERR, "ioctl (SLIOCGUNIT): %m");
		exit(1);
	}
	sprintf(info.unit,"sl%d",unitnumber);
#else
	if (ioctl(0, SIOCGIFNAME, (caddr_t) info.unit) < 0)
	{
		syslog(LOG_ERR, "ioctl (SIOCGIFNAME): %m");
		exit(1);
	}
#endif
    sa.sa_handler = hup_handler;
	sa.sa_flags = 0;
	if (sigemptyset(&sa.sa_mask) != 0)
	{
		syslog(LOG_ERR, "I have no signal control: %m");
		exit(1);
	}

	sigaction(SIGHUP,&sa,NULL);
	sigaction(SIGTERM,&sa,NULL);

	syslog(LOG_INFO, "attaching slip unit %s for %s\n",
				info.unit, info.loginname);
	pid=getpid();
	(void)sprintf(logincmd, "%s %s %d %d %s %s %s %s %s %s %s",
		info.loginfile, info.unit, info.linespeed, pid, LOGINARGS);
	syslog(LOG_INFO, logincmd);
	/*
	 * aim stdout and errout at /dev/null so logincmd output won't
	 * babble into the slip tty line.
	 */
	close(1);
	if ((fd = open(_PATH_DEVNULL, O_WRONLY)) != 1) {
		if (fd < 0) {
			syslog(LOG_ERR, "open /dev/null: %m");
			exit(1);
		}
		(void) dup2(fd, 1);
		(void) close(fd);
	}
	(void) dup2(1, 2);

	/*
	 * Run login and logout scripts as root (real and effective);
	 * current route(8) is setuid root, and checks the real uid
	 * to see whether changes are allowed (or just "route get").
	 */
	(void) setuid(0);
	s=system(logincmd);
	if (s!=0)
	{
		syslog(LOG_ERR, "%s login failed: exit status %d from %s",
		       info.loginname, s, info.loginfile);
		ioctl(0, TIOCSETD, (caddr_t)&odisc);
		exit(6);
	}
	/* set slip mode */
#ifndef linux
	if (ioctl(0, SLIOCSFLAGS, (caddr_t) &info.sm_value) < 0)
	{
		syslog(LOG_ERR, "ioctl(SLIOCSFLAGS): %m");
		exit(1);
	}
#else
	if (ioctl(0, SIOCSIFENCAP, (caddr_t) &info.sm_value) < 0)
	{
		syslog(LOG_ERR, "ioctl (SIOCSIFENCAP): %m");
		exit(1);
	}
#endif


	/* Wait for signal or timeout */
	while (HUP==-1) packet_watch();
	exit_sliplogin();

	/* NOTREACHED */
}
