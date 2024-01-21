/*
 * File:	xringd.c
 *
 * Synopsis:	Extensive modem ring daemon
 *
 * System:	Linux 1.3.48+
 *
 * Daemon that uses ringsm.c module to do interesting things with sequences
 * of modem RINGs. Does not disturb any getty running on the same
 * port as it only probes for the RI signal.
 * Device and OS dependent as it uses the TIOCMIWAIT/TIOCGICOUNT ioctls. The
 * ringsm can be used with "traditional" methods. This is just
 * cleaner as it does not disturb your system's getty.
 *
 * Configuration file default: /etc/xringd.conf
 * Config file lines are of this form:
 * R secs[-secs] [ R secs[-secs] ] ... : command
 * which specify the pattern of rings+delays leading to the exution of 
 * a command. 
 * 
 * Copyright (c) 1995-1996 Angelo Haritsis. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: xringd.c,v 1.5 1996/02/17 15:50:07 ah Exp ah $
 */

#ifndef linux
#error xringd only runs for linux - it uses 2 non-standard ioctls
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <paths.h>
#include <fcntl.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <signal.h>
#include <termios.h>
#include <getopt.h>
#include <syslog.h>
#include <pwd.h>
#include <ctype.h>
#include <linux/version.h>
#include <linux/serial.h>

#include "ringsm.h"

#if !defined(LINUX_VERSION_CODE) || \
    (LINUX_VERSION_CODE < ((1 * 65536) + (3 * 256) + 48))
#error xringd needs Linux 1.3.48 or greater
#endif

#define RING_SIGNALS	/* define for raising SIG_RING on RING detection */
#define SIG_RING SIGUSR1
#define SIG_FLAGS 0	/* 0 or SA_RESTART (syscall after a sig) */

#define MAXLINE 512

#ifndef MODEM_FILE
#define MODEM_FILE "/dev/modem"
#endif
#ifndef CONF_FILE
#define CONF_FILE "/etc/xringd.conf"
#endif
#ifndef INIT_TIMEOUT
#define INIT_TIMEOUT 15
#endif

#define PROG "xringd"

#if 0
static char pidfilename[MAXPATHLEN] = "\0";
#endif

char *config_file = NULL;
char *modem_file = NULL;
char *do_onring = NULL;
int modem = -1; 
time_t reset_timeout = INIT_TIMEOUT;
int debug = 0;
int log_level = 1;
int syntax_only = 0;
long msec_ignore = 0;	/* 2 rings within this many msecs => taken as one */
sigset_t sig_mask;
int ringsm_parsed = FALSE;

void set_signals(void);
int modem_open(char *file, int reopen);
void usage_and_die(void);
int parse_an_arg(char opt, char *s, int in_conf_line);
int parse_an_arg_conf(char opt, char *s, int line);
void parse_args(int argc, char **argv);
char *str_new(char *s, char *new);
void show_args(void);
int is_ring_near(struct timeval *tm_bef);
void seq_success(char *seq_cmd);
void log_msg(int priority, int level, char *fmt, ...);
void cback_log_msg(int level, char *fmt, ...);
void daemon_init(void);
void ring_cmd(void);
void proc_ring(int i);
void sig_child(int);
void sig_hup(int i);
void sig_alarm(int i);
void clean_exit(int code);
void sig_bye(int i);
void fork_cmd(char *cmd);


void
main (int argc, char *argv[])
{
	int arg;
	struct serial_icounter_struct c;
	struct timeval tm_bef;
	static char str_near[100];

	config_file = str_new(config_file, CONF_FILE);	/* initialise strings */
	modem_file = str_new(modem_file, MODEM_FILE);
	parse_args(argc, argv);
	if (access(config_file, R_OK) != 0) {
		log_msg(-1, 0, "cannot open config file %s", config_file);
		usage_and_die();
		exit(1);
	}
	if ((arg = ringsm_parse(config_file, &parse_an_arg_conf)) < 0) {
		log_msg(-1, 0, "error in config file, line %d", -arg);
		exit(1);
	}
	ringsm_parsed = TRUE;
	if (syntax_only) {
		log_msg(-1, 0, "config file %s -> syntax ok", config_file);
		exit(0);
	}
	modem = modem_open(modem_file, FALSE);
	if (ioctl(modem, TIOCGICOUNT, &c) != 0) {
		log_msg(-1, 0, "needs Linux 1.3.48+, 16xx0 uart");
		usage_and_die();
		exit(1);
	}
	show_args();

	/* Daemon proper */
	daemon_init();
	if (!debug && log_level > 0)
		openlog(PROG, LOG_PID, LOG_DAEMON);
	set_signals();
	ringsm_init(reset_timeout, &seq_success, &alarm, &cback_log_msg);
	str_near[0] = 0;
	if (msec_ignore > 0)
		sprintf(str_near, "; rings %ld msec or less apart ignored", msec_ignore);
	log_msg(LOG_INFO, 1, "version %s started by %s (uid %d)%s", VERSION, getpwuid(getuid())->pw_name, getuid(), str_near);

	gettimeofday(&tm_bef, NULL);
	/* main daemon loop */
	while (1) {
		/* wait on RI line */
		if (ioctl(modem, TIOCMIWAIT, TIOCM_RNG) != 0) {
			if (EINTR == errno) {
				log_msg(LOG_DEBUG, 1000, "ioctl restarting - signal");
				continue;
			}
			log_msg(LOG_ERR, 10, "%%m - reopening %s", modem_file);
			/*
			 * XXX EIO may occur: port hung up by other process!
			 * Reopen it - danger: reopening failing continously
			 */
			close(modem);
			modem = modem_open(modem_file, TRUE);
			continue;
		}
		/* we got a ring; if near the previous, ignore it */
		if (is_ring_near(&tm_bef))
			continue;
		/* ring is certain; notify the state machine */
#ifdef RING_SIGNALS
		raise(SIG_RING);
#else
		proc_ring(arg /*dummy*/);
#endif
		if (debug)
			ringsm_dump();
	}
	/* NOTREACHED */
}

void
show_args(void)
{
#ifdef DEBUG
	log_msg(-1, 100, "Modem: %s - Config: %s\nOnring: %s\n"
			"reset: %d - ignore: %d - debug: %d - log: %d\n",
			modem_file, config_file, do_onring,
			reset_timeout, msec_ignore, debug, log_level);
#endif
}

void
usage_and_die(void)
{
	fprintf(stderr, "xringd version " VERSION " - by A. Haritsis (ah@doc.ic.ac.uk)\n"
			"usage: %s [-a rngcmd] [-c cfgfile] [-d] [-h] [-i ignore_msec] [-l loglevel]\n"
			"              [-m modem_dev] [-n] [-t initime] [modem_dev]\n", PROG);
	exit(1);
}

void
set_signals(void)
{
#define SIGNAL(s, flags, handler, old) do { \
	sa.sa_handler = handler; \
	sa.sa_flags = flags; \
	if (sigaction(s, &sa, old) < 0) { \
		log_msg(LOG_ERR, 1, "error setting signal (%%m)"); \
		clean_exit(1); \
	} \
} while (0)

	struct sigaction sa;   

	sigemptyset(&sig_mask);
	sigaddset(&sig_mask, SIGALRM);	/* only one signal served at a time */
	sigaddset(&sig_mask, SIGCHLD);
	sigaddset(&sig_mask, SIGHUP);
	sigaddset(&sig_mask, SIGINT);
	sigaddset(&sig_mask, SIGTERM);
	sigaddset(&sig_mask, SIGQUIT);
#ifdef RING_SIGNALS
	sigaddset(&sig_mask, SIG_RING);
#endif
	sa.sa_mask = sig_mask;
	SIGNAL(SIGALRM, SIG_FLAGS, sig_alarm, NULL);
	SIGNAL(SIGCHLD, SIG_FLAGS, sig_child, NULL);
#ifdef RING_SIGNALS
	SIGNAL(SIG_RING, SIG_FLAGS, proc_ring, NULL);
#endif
	SIGNAL(SIGHUP, SIG_FLAGS, sig_hup, NULL);
	SIGNAL(SIGINT, SIG_FLAGS, sig_bye, NULL);
	SIGNAL(SIGTERM, SIG_FLAGS, sig_bye, NULL);
	SIGNAL(SIGQUIT, SIG_FLAGS, sig_bye, NULL);
#undef SIGNAL
}

void
log_msg(int priority, int level, char *fmt, ...)
{
	va_list p;
	char buf[MAXLINE];

	va_start(p, fmt);
	vsprintf(buf, fmt, p);
	if (log_level >= level)
		if (debug || -1 == priority) {
			fflush(stdout);
			fprintf(stderr, PROG ": %s\n", buf);
			fflush(stderr);
		} else
			syslog(priority, buf);
	va_end(p);
}


void
cback_log_msg(int level, char *fmt, ...)
{
	va_list p;
	char buf[MAXLINE];

	va_start(p, fmt);
	vsprintf(buf, fmt, p);
	log_msg(LOG_DEBUG, level, buf);
	va_end(p);
}

/*
 * is_ring_near -- is ring we just got nearer than msec_ignore msec to 
 *		   the previous?
 */
int
is_ring_near(struct timeval *tm_bef)
{
	struct timeval tm;
	long msec;
	int ret;

	if (0 == msec_ignore)
		return FALSE;

	gettimeofday(&tm, NULL);
	msec = (tm.tv_sec - tm_bef->tv_sec) * 1000L + 
	       (tm.tv_usec - tm_bef->tv_usec) / 1000L;
	*tm_bef = tm;	/* update the before time! */
	ret = (msec <= msec_ignore);
	if (ret)
		log_msg(LOG_DEBUG, 10, "RI near (%ld msec) - dumped", msec);
	return ret;
}

/*
 * called via ringsm_process_timeout
 */
void
seq_success(char *seq_cmd)
{
	log_msg(LOG_INFO, 1, "launching: %s", seq_cmd);
	fork_cmd(seq_cmd);
}

/*
 * Called when a RING signal appears
 */
void
proc_ring(int i)
{
	static int rings = 1;
	int ret;

	if (do_onring)
		ring_cmd();
	log_msg(LOG_DEBUG, 10, "RING #%d", rings++);
	ret = ringsm_process_a_ring();
	if (!ret)
		log_msg(LOG_DEBUG, 100, "ignored ring #%d",  rings-1);
}

void
sig_child(int i)
{
	int status;

	/* we should probably check the full exit status of the dead child */
	wait(&status);
}

void
sig_hup(int i)
{
	int arg;

	if (!ringsm_parsed)
		return;
	log_msg(LOG_INFO, 1, "HUP received: resetting");
	ringsm_reset(FALSE);
	/* close and reread config file */
	ringsm_close();
	if ((arg = ringsm_parse(config_file, &parse_an_arg_conf)) < 0)
		log_msg(LOG_ERR, 0, "error in config file, line %d - ignoring the next lines", -arg);
	if (modem >= 0) {
		close(modem);
		modem = modem_open(modem_file, TRUE);
	}
	ringsm_reset(TRUE);
	show_args();
}

void
sig_alarm(int i)
{
	ringsm_process_timeout();
}

void
clean_exit(int code)
{
	log_msg(LOG_INFO, 1, "Exit");
	/*if (unlink(pidfilename) < 0 && errno != ENOENT)
        	log_msg(LOG_WARNING, 1, "unable to delete pid file: %m");
        pidfilename[0] = 0;*/
	closelog();
	exit(code);
}

void
sig_bye(int i)
{
	ringsm_reset(FALSE);
	clean_exit(0);
}

#if 0
void
setdtr(int fd, int on)
{
	int modembits = TIOCM_DTR;

	ioctl(fd, (on ? TIOCMBIS : TIOCMBIC), &modembits);
}
#endif

int
modem_open(char *file, int reopen)
{
	int fd = -1;

	if ((fd = open(file, O_RDONLY | O_NOCTTY | O_NDELAY)) < 0) {
		if (reopen) {
			log_msg(LOG_ERR, 1, "error reopening %s (%%m)", file);
			usleep(3*1000000);	/* uses select - not SIGALRM */
			return fd;
		} else {
			log_msg(-1, 0, "error opening modem device");
			perror(NULL);
			exit(1); 
		}
	}
	return fd;
}

#if 0
void
write_pid(void)
{
	FILE *pidf;

        (void) sprintf(pidfilename, "%s%s.pid", _PATH_VARRUN, PROG);
        if ((pidf = fopen(pidfilename, "w")) != NULL) {
            fprintf(pidf, "%d\n", getpid());
            (void) fclose(pidf);
        } else {
            log_msg(LOG_ERR, 1, "Failed to create pid file %s: %m", pidfilename);
            pidfilename[0] = 0;
        } 
}
#endif

void
daemon_init(void)
{
	int pid;

	if (debug)
		return;
	if ( (pid = fork()) < 0) {
		perror(NULL);
		exit(1);
	} else if (pid != 0)
		exit(0);	/* parent goes */
	/* child */
	setsid();
	chdir("/");
	umask(S_IRWXG | S_IRWXO);
	close(STDIN_FILENO);
	close(STDOUT_FILENO);
	close(STDERR_FILENO);
}

void
ring_cmd(void)
{
	char *p;

	for (p = do_onring; *p != 0; p++)
		if (!isspace(*p))
			break;
	if (*p == 0)		/* all whitespace */
		return;
	log_msg(LOG_INFO, 10, "running (per ring): %s", do_onring);
	fork_cmd(do_onring);
}

/*
 * fork_cmd --- fork a shell to execute command cmd 
 *
 * Establish a "clean slate" first.
 */
void
fork_cmd(char *cmd)
{
	int pid;
	int fdnull;

	if ((pid = fork()) < 0) {
		log_msg(LOG_ERR, 1, "error forking for %s (%%m)", cmd);
		return;
	} else if (0 == pid) {
		/* child */
		/* unblock signals we use - may be blocked here */
		sigprocmask(SIG_UNBLOCK, &sig_mask, NULL);
		setsid();
		close(STDIN_FILENO);
		close(STDOUT_FILENO);
		close(STDERR_FILENO);
		if (modem >= 0)
			close(modem);
		/* connect /dev/null to std{in/out/err} */
		fdnull = open(_PATH_DEVNULL, O_RDWR);
		if (fdnull >= 0) {
			if (fdnull != 0) {
				dup2(fdnull, 0);
				close(fdnull);
			}
			dup2(0, 1);
			dup2(0, 2);
		}
		setuid(getuid());
		setgid(getgid());
		execl("/bin/sh", "sh", "-c", cmd, NULL);
		/* could not run cmd ; report it */
		log_msg(LOG_ERR, 1, "error running: /bin/sh -c %s (%%m)", cmd);
		exit(1);
	}
	/* parent */
}

int
parse_an_arg(char opt, char *s, int conf_line)
{
	switch (opt) {
	case 'a':
		do_onring = str_new(do_onring, s);
		break;
	case 'c':
		if (0 == conf_line)
			config_file = str_new(config_file, s);
		break;
	case 'd':
		debug++;
		log_level = 100;
		break;
	case 'i':
		msec_ignore = atoi(s);
		break;
	case 'l':
		log_level = atoi(s);
		break;
	case 'm':
		modem_file = str_new(modem_file, s);
		break;
	case 'n':
		if (0 == conf_line) {
			syntax_only++;
			debug++;
		}
		break;
	case 't':
		reset_timeout = atoi(s);
		break;

	case '?':
	case 'h':
		if (0 == conf_line)
			usage_and_die();
		break;
	default:
		if (0 == conf_line)
			usage_and_die();
		else
			log_msg(LOG_ERR, 0, "option %c error in conf file, line %d - ignoring", opt, conf_line);
		break;
	}
	return TRUE;
}

/* 
 * parse_an_arg_conf -- callback for an argument parse (from config file)
 */
int
parse_an_arg_conf(char opt, char *s, int line)
{
	return parse_an_arg(opt, s, line);
}

void
parse_args(int argc, char **argv)
{
	int opt;

	opterr = 0;
	while ( (opt = getopt(argc, argv, "a:c:dl:i:l:m:nt:h?")) != EOF )
		parse_an_arg((char) opt, optarg, 0);
	if (optind < argc)
		modem_file = str_new(modem_file, argv[optind]);
}

/* 
 * str_new -- free old space if alloc'ed; alloc new and assign new value
 * XXX new is NULL or "" !!
 */
char *
str_new(char *s, char *new)
{
	int newsize = strlen(new) + 1;
	char *p;

	if (NULL == s)
		p = malloc(newsize);
	else
		p = realloc(s, newsize);
	strcpy(p, new);
	return p;
}
