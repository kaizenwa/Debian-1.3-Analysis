/*
    Wn: A Server for the HTTP
    File: wn/init.c
    Version 1.17.0
    
    Copyright (C) 1995, 1996  <by John Franks>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include "../config.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>

#ifndef NO_UNISTD_H
#include <unistd.h>
#endif

#ifndef NO_SYSLOG
#include <syslog.h>
#endif

#include <fcntl.h>
#include "wn.h"
#include "version.h"

#ifdef USE_VIRTUAL_HOSTS
extern char	*vhostlist[][3];
#endif

/* this may be needed if unistd.h isn't included... */
#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifdef RFC931_TIMEOUT
extern void	get_rfc931();
#endif

#ifdef NO_VOID_PTR
#define	void	int
#endif

#ifndef BSD_LIKE
extern long	timezone;
#endif

extern char	*optarg;
extern int	optind;

#define		WN_COMMON	(0)
#define		WN_VERBOSE	(1)
#define		NCSA_LOG	(2)

#ifdef VERBOSELOG
static int	log_type = WN_VERBOSE;
#else
static int	log_type = WN_COMMON;
#endif

static void	dump_log();

char	rootdir[SMALLLEN],
	wnlogfile[SMALLLEN],
	errlogfile[SMALLLEN],
	pid_file[SMALLLEN],
	cfname[SMALLLEN],
	hostname[MAXHOSTNAMELEN],
	remotehost[MAXHOSTNAMELEN],
	remaddr[20];

int	admin_mode = FALSE,
	debug_log = FALSE;

unsigned	serv_perm = 0,
		interface_num,
		acache_id,
		cache_id;

#define		LNOLOG	(0)
#define		LFILE	(1)
#define		LSYSLOG	(2)

static int	log_func = LNOLOG;

#ifndef NO_FLOCK
static void	locklog(),
		unlocklog();
#endif

static void	log_logfile(),
		start_log(),
		reopen_wnlog();


static FILE	*logfp = NULL,
		*errlogfp = NULL;

#ifndef NO_SYSLOG
static void	log_syslog ();
#endif

static void	append_gmtoff();

void
wn_init( argc, argv)
int	argc;
char	*argv[];
{
	int	c,
		errflg = 0;

/*
 * Let's run as specified user and group if SET_UID defined.
 * This is imperative if inetd cannot change UID from root.
 */

#ifdef SET_UID
#ifndef USERID
#define USERID	(-2)
#define GROUPID	(-2)
#endif

#ifndef STANDALONE  /* Otherwise this is done in standalone.c */
 	setegid( (gid_t) GROUPID );
 	setgid(  (gid_t) GROUPID );   
 	seteuid( (uid_t) USERID );
 	setuid(  (uid_t) USERID );  /* This pulls rug out from under us */
#endif
#endif

	port = DEFAULT_PORT;
	mystrncpy (wnlogfile, WN_LOGFILE, SMALLLEN);
	mystrncpy (errlogfile, WN_ERRLOGFILE, SMALLLEN);
	if ( *wnlogfile)
		log_func = LFILE;
	mystrncpy( rootdir, ROOT_DIR, SMALLLEN);

#ifdef SWN_PID_FILE
	mystrncpy( pid_file, SWN_PID_FILE, SMALLLEN);
#else
	*pid_file = '\0';
#endif

	mystrncpy( cfname, CACHEFNAME, SMALLLEN);
	mystrncpy( hostname, WN_HOSTNAME, MAXHOSTNAMELEN);


#ifdef FORBID_CGI
	serv_perm |= WN_FORBID_EXEC;
#endif

	while ((c = getopt(argc, argv, 
				"a:A:eEh:L:l:p:q:St:T:uv:V:dxz:")) != -1) {
		switch ((char) c) {
			case 'a':
				acache_id = (unsigned) atoi( optarg);
				serv_perm |= WN_ATRUSTED_UID;
				break;
			case 'A':
				acache_id = (unsigned) atoi( optarg);
				serv_perm |= WN_ATRUSTED_GID;
				break;
			case 'e':
				serv_perm |= WN_FORBID_EXEC;
				break;
			case 'E':
				serv_perm |= WN_RESTRICT_EXEC;
				break;
			case 'h':
				mystrncpy( hostname, optarg, MAXHOSTNAMELEN);
				break;
			case 'L':
				mystrncpy( wnlogfile, optarg, SMALLLEN);
				log_func = LFILE;
				break;
			case 'l':
				mystrncpy( errlogfile, optarg, SMALLLEN);
				break;
#ifdef STANDALONE
			case 'p':
				port = atoi( optarg);
				break;
			case 'q':
				mystrncpy( pid_file, optarg, SMALLLEN);
				break;
#endif /* STANDALONE */

			case 'S':
#ifdef NO_SYSLOG
#ifdef STANDALONE
				fprintf( stderr,
					"Option -S unavailable, no syslog");
#endif /* STANDALONE */
				exit( 2);
#else
				wnlogfile[0] = '\0';
				log_func = LSYSLOG;
#endif /* NO_SYSLOG */
				break;
			case 't':
				cache_id = (unsigned) atoi( optarg);
				serv_perm |= WN_TRUSTED_UID;
				break;
			case 'T':
				cache_id = (unsigned) atoi( optarg);
				serv_perm |= WN_TRUSTED_GID;
				break;
			case 'u':
				serv_perm |= WN_COMP_UID;
				break;
			case 'v':
				if ( strcasecmp( optarg, "common") == 0) {
					log_type = WN_COMMON;
					break;
				}
				if ( strcasecmp( optarg, "verbose") == 0) {
					log_type = WN_VERBOSE;
					break;
				}
				if ( strcasecmp( optarg, "ncsa") == 0) {
					log_type = NCSA_LOG;
					break;
				}
				fprintf( stderr, "Unknown log type %s\n", 
					optarg);
				break;
#ifdef VIRTUAL_HOSTS_FILE
			case 'V':
				mystrncpy( vhostfile, optarg, SMALLLEN);
				break;
#endif
			case 'd':
				/*should only be used for logging to file*/
				debug_log = TRUE;
				log_type = WN_VERBOSE;
				break;
			case 'x':
				admin_mode = TRUE;
				break;

			WN_EXTRA_OPTS

			case '?':
				errflg++;
		}
	}
	if (errflg) {
#ifdef STANDALONE
		fprintf( stderr, "Usage: wn [-L logfile | -S] ");
		fprintf( stderr, "[-a uid |-A gid]\n");
		fprintf( stderr, "[-e | -E] [-p port] [-t uid | -T gid ] ");
		fprintf( stderr, "[-v log_type] [-V virtual host file]\n");
		fprintf( stderr, "[-u] [-q pid_file] [-h host] [topdir]\n");
#else
		fprintf( stderr, "Unknown option given to server");
#endif
		exit (2);
	}
	if ( *wnlogfile && !*errlogfile) {
		strcpy( errlogfile, wnlogfile);
	}

	if ( argv[optind] )
		mystrncpy( rootdir, argv[optind], SMALLLEN);


#ifndef NO_SYSLOG
	if (log_func == LSYSLOG)
	{

#ifdef LOG_DAEMON
		/* 4.3 style */
		openlog ("wn", LOG_PID | LOG_NDELAY, LOGFACILITY);
#else
		/* 4.2 style */
		openlog ("wn", LOG_PID);
#endif
	}
#endif
#ifndef STANDALONE
	open_wnlog( wnlogfile, errlogfile);
#endif

#ifdef USE_VIRTUAL_HOSTS
#ifdef VIRTUAL_HOSTS_FILE
	load_virtual();
#endif
#endif

	WN_EXTRA_INIT
}

void
open_wnlog( logfile, errlog)
char	*logfile,
	*errlog;
{
	if ( *logfile != '\0')
		if ( (logfp = fopen( logfile, "a")) == NULL ) {
#ifdef STANDALONE
			fprintf( stderr, "Can't open logfile: %s\n", logfile);
#else
			printf( "Can't open logfile %s\n", logfile);
#endif
			exit( 2);
		}
	if ( *errlog != '\0')
		if ( (errlogfp = fopen( errlog, "a")) == NULL ) {
#ifdef STANDALONE
			fprintf( stderr, 
				"Can't open error logfile: %s\n", errlog);
#else
			printf( "Can't open error logfile %s\n", errlog);
#endif
			exit( 2);
		}
#ifdef STANDALONE
	start_log( FALSE);
	signal( SIGHUP, reopen_wnlog);
#endif
}



static void
reopen_wnlog( )
{

#ifdef STANDALONE
	signal( SIGHUP, SIG_IGN);

#ifdef USE_VIRTUAL_HOSTS
#ifdef VIRTUAL_HOSTS_FILE
	load_virtual();
#endif
#endif
	if ( *wnlogfile) {
		if ( (logfp = freopen( wnlogfile, "a", logfp)) == NULL ) {
			fprintf( stderr, "Can't reopen logfile: %s\n",
					wnlogfile);
			exit( 2);
		}
	}

	if ( *errlogfile) {
		if ( (errlogfp = 
				freopen( errlogfile, "a", errlogfp)) == NULL ){
			fprintf( stderr, "Can't reopen error logfile: %s\n",
					errlogfile);
			exit( 2);
		}
	}

	start_log( TRUE);

	signal( SIGHUP, reopen_wnlog);
#endif
}


void
logerr(  msg, msg2)
char	*msg,
	*msg2;
{

#ifndef NO_FLOCK
	struct flock	lck;
#endif

	time_t	clock;
	struct tm *ltm;

	char	*rhost,
		status[TINYLEN],
		date[TINYLEN];


	if ((log_func == LNOLOG) && !*errlogfile)
		return;

	get_remote_info( );

#ifndef NO_SYSLOG
	if ( (log_func == LSYSLOG) && !*errlogfile) {
		log_syslog( ERRLOG_PRIORITY, this_rp, msg, msg2);
		return;
	}
#endif

	time(&clock);
	ltm = (struct tm *) localtime(&clock);
	strftime( date, TINYLEN, "%d/%h/%Y:%T", ltm);
	append_gmtoff( date, ltm);

#ifndef NO_FLOCK
	locklog( &lck, fileno( errlogfp) );
#endif

	if ( outheadp && (*outheadp->status)) {
		mystrncpy( status, outheadp->status, 4);
	}
	else
		strcpy( status, "500");

	rhost = ( *remotehost ? remotehost : remaddr);
	rhost = ( *rhost ? rhost : "unknown");

	fseek( errlogfp, 0L, 2);
	fprintf( errlogfp, "%s - - [%s] \"%s\" %s -", rhost,
				date, this_rp->request, status);

	if ( log_type == WN_VERBOSE ) {
		fprintf( errlogfp, " <(%d/%d) %s: %s>",
			this_conp->pid, this_conp->trans_cnt, msg, msg2);
	}
	fprintf( errlogfp, "\n");
	(void) fflush( errlogfp);


#ifndef NO_FLOCK
	unlocklog( &lck, fileno( errlogfp) );   
#endif

}

void
daemon_logerr(  msg, error)
char	*msg;
int	error;
{

	FILE		*errfp;
#ifndef NO_FLOCK
	struct flock	lck;
#endif
	time_t	clock;
	struct tm *ltm;

	char	buf[SMALLLEN],
		date[TINYLEN];


	if ((log_func == LNOLOG) && !*errlogfile)
		return;


#ifndef NO_SYSLOG
	if ( (log_func == LSYSLOG) && !*errlogfile) {
		sprintf( buf, "none - - [] \"none\" 500 0  <%s: errno=%d> ",
				msg, error);

		if ( log_type == WN_VERBOSE ) {
#ifdef USE_VIRTUAL_HOSTS
			syslog( ERRLOG_PRIORITY, "<> <> <> <0>\n");
#else
			syslog( ERRLOG_PRIORITY, "<> <> <>\n");
#endif
		}
		else
			strcat( buf, "\n");

		syslog( ERRLOG_PRIORITY, buf);
		return;
	}
#endif /* NO_SYSLOG */

	if ( errlogfp)
		errfp = errlogfp;
	else
		errfp = stderr;

	time(&clock);
	ltm = (struct tm *) localtime(&clock);
	strftime( date, TINYLEN, "%d/%h/%Y:%T", ltm);
	append_gmtoff( date, ltm);

#ifndef NO_FLOCK
	if ( errlogfp)
		locklog( &lck, fileno( errlogfp) );
#endif

	fseek( errfp, 0L, 2);   
	fprintf(errfp, "none - - [%s] \"none\" 500 0",  date);
	if ( log_type == WN_VERBOSE ) {
		fprintf( errfp, " <%s: errno=%d> <> <> <>", msg, error);
#ifdef USE_VIRTUAL_HOSTS
		fprintf( errfp, " <0>");
#endif
	}

	fprintf( errfp, "\n");
	(void) fflush( errfp);

#ifndef NO_FLOCK
	if ( errlogfp)
		unlocklog( &lck, fileno( errlogfp) );   
#endif
}


void
writelog( ip, msg, msg2)
Request	*ip;
char	*msg,
	*msg2;
{
	if (log_func == LFILE)
		log_logfile( ip, msg, msg2);
#ifndef NO_SYSLOG
	else if ( log_func == LSYSLOG)
		log_syslog( LOG_PRIORITY, ip, msg, msg2);
#endif
}


/* Write debug messages into the log file. */

void
write_debug(n, msg, msg2)
int	n; /*ignored for now, could become the debug level later*/
char	*msg, *msg2;
{
		fprintf(logfp, "%s %s\n", msg, msg2);
		(void) fflush( logfp);
}


static void
log_logfile( ip, msg, msg2)
Request	*ip;
char	*msg,
	*msg2;
{
	time_t	clock;
	struct tm *ltm;

	register char	*cp,
			*cp2;

	char	bytes[TINYLEN],
		status[TINYLEN],
		date[TINYLEN],
		lbuf[BIGLEN];

	if ( outheadp && (*outheadp->status)) {
		mystrncpy( status, outheadp->status, 4);
	}
	else
		strcpy( status, "200");

	if ( *(ip->length))
		strcpy( bytes, ip->length);
	else
		strcpy( bytes, "-");

	time(&clock);
	ltm = (struct tm *) localtime(&clock);
	strftime( date, TINYLEN, "%d/%h/%Y:%T", ltm);
	append_gmtoff( date, ltm);

	cp = cp2 = this_conp->log_ptr;

	sprintf( lbuf, "[%s] \"%s\" %s %s",
				date, ip->request, status, bytes);
	cp2 += strlen( lbuf);
	if ( cp2 >= this_conp->logbuf + LOGBUFLEN) {
		dump_log();
		cp = cp2 = this_conp->logbuf;
	}
	strcpy( cp , lbuf);
	cp = cp2;

	if ( log_type == WN_VERBOSE ) {
		sprintf( lbuf, " <(%d/%d) %s: %s>",
			this_conp->pid, this_conp->trans_cnt, msg, msg2);
		cp2 += strlen( lbuf);
		if ( cp2 >= this_conp->logbuf + LOGBUFLEN) {
			dump_log();
			cp = cp2 = this_conp->logbuf;
		}

		strcpy( cp , lbuf);
		cp = cp2;

		sprintf( lbuf, " <%s>", inheadp->ua);
		cp2 += strlen( lbuf);
		if ( cp2 >= this_conp->logbuf + LOGBUFLEN) {
			dump_log();
			cp = cp2 = this_conp->logbuf;
		}

		strcpy( cp , lbuf);
		cp = cp2;

		sprintf( lbuf, " <%s>", inheadp->referrer);
		cp2 += strlen( lbuf);
		if ( cp2 >= this_conp->logbuf + LOGBUFLEN) {
			dump_log();
			cp = cp2 = this_conp->logbuf;
		}
		strcpy( cp , lbuf);
		cp = cp2;

		sprintf( lbuf, " <%s>", inheadp->cookie);
		cp2 += strlen( lbuf);
		if ( cp2 >= this_conp->logbuf + (LOGBUFLEN - 32)) {
			dump_log();
			cp = cp2 = this_conp->logbuf;
		}
		strcpy( cp , lbuf);
		cp = cp2;

#ifdef USE_VIRTUAL_HOSTS
		sprintf( cp, " <%d>", interface_num);
		cp += strlen( cp);
#endif
	}
	else if ( log_type == NCSA_LOG) {
		sprintf( lbuf, " \"%s\"", inheadp->referrer);
		cp2 += strlen( lbuf);
		if ( cp2 >= this_conp->logbuf + LOGBUFLEN) {
			dump_log();
			cp = cp2 = this_conp->logbuf;
		}
		strcpy( cp , lbuf);
		cp = cp2;

		sprintf( lbuf, " \"%s\"", inheadp->ua);
		cp2 += strlen( lbuf);
		if ( cp2 >= this_conp->logbuf + LOGBUFLEN) {
			dump_log();
			cp = cp2 = this_conp->logbuf;
		}
		strcpy( cp , lbuf);
		cp = cp2;
	}

	*cp++ =  '\n';
	*cp =  '\0';
	this_conp->log_ptr = cp;
}


#ifndef NO_FLOCK
static void
locklog(lck, fd)
struct flock	*lck;
int	fd;
{
	lck->l_type = F_WRLCK;
	lck->l_whence = SEEK_SET;
	lck->l_start = 0L;
	lck->l_len = 0L;
	fcntl(fd, F_SETLKW, lck);
}

static void
unlocklog( lck, fd)
struct flock	*lck;
int	fd;
{
	lck->l_type = F_UNLCK;
	fcntl( fd, F_SETLKW, lck);
}
#endif


#ifndef NO_SYSLOG

static void
log_syslog( priority, ip, msg, msg2)
int	priority;
Request	*ip;
char	*msg,
	*msg2;
{
	char	*authname,
		*rfc931p,
		*rhost,
		bytes[TINYLEN],
		status[TINYLEN];

	if ( *outheadp->status) {
		mystrncpy( status, outheadp->status, 4);
	}
	else
		strcpy( status, "200");

	if ( *(ip->length))
		strcpy( bytes, ip->length);
	else
		strcpy( bytes, "-");


	rfc931p = ( *this_conp->rfc931name ? this_conp->rfc931name : "-");
	authname = ( *this_conp->authuser ? this_conp->authuser : "-");

	rhost = ( *remotehost ? remotehost : remaddr);
	rhost = ( *rhost ? rhost : "unknown");

	syslog( priority,"%s %s %s [] \"%s\" %s %s  <%s: %s>\n", 
		rhost, rfc931p, authname, ip->request, status, 
		bytes, msg, msg2);

	if ( log_type == WN_VERBOSE ) {
#ifdef USE_VIRTUAL_HOSTS
		syslog( priority, "<%s> <%s> <%s> <%d>\n", inheadp->ua,
			inheadp->referrer, inheadp->cookie, interface_num);
#else
		syslog( priority, "<%s> <%s> <%s>\n",
			inheadp->ua, inheadp->referrer, inheadp->cookie);
#endif
	}
}
#endif /* ifndef NO_SYSLOG */


static void
append_gmtoff( date, ltm)
char	*date;
struct tm *ltm;
{
	register char	*cp;
	long		tz;
	char 		sign;

#ifdef BSD_LIKE
	tz = ltm->tm_gmtoff;
#else
	tz = - timezone;
	if( ltm->tm_isdst)
		tz += 3600;
#endif
	sign = ( tz > 0 ? '+' : '-');
	tz = ( tz > 0 ? tz :  -tz);
	cp = date;
	while ( *cp)
		cp++;
	
	sprintf( cp, " %c%02ld%02ld", sign, tz/3600, tz % 3600);
}

static void
start_log( restart)
int	restart;
{
#ifdef STANDALONE
	time_t	clock;
	struct tm *ltm;
	char	*cp,
		startdate[TINYLEN];

	if ( log_func != LFILE)
		return;
	time(&clock);
	ltm = (struct tm *) localtime(&clock);
	strftime( startdate, TINYLEN, "%d/%h/%Y:%T", ltm);
	cp = ( restart ? "Restarting" : "Starting");
	fprintf(logfp, "\n%s: %s %s at port %d with pid %d\n", 
			startdate, cp, VERSION, port, getpid() );

	(void) fflush(logfp);
#endif
}


void
wn_exit( status)
int	status;
{

	flush_outbuf();
	if ( *inheadp->tmpfile)
		unlink( inheadp->tmpfile);

	shutdown( fileno( stdin), 0);

	dump_log();
	exit( status);
}

static void
dump_log()
{
	register char	*cp,
			*cp2;

	char		*rfc931p,
			*authname,
			*rhost,
			logline[MIDLEN];

#ifndef NO_FLOCK
	struct flock	lck;
#endif

	if ( this_conp->logbuf == this_conp->log_ptr)
		return;

	*(this_conp->log_ptr) = '\0';

	get_remote_info( );

#ifdef RFC931_TIMEOUT
	get_rfc931();
#endif

#ifndef NO_FLOCK
	locklog( &lck, fileno( logfp) );
#endif

	rfc931p = ( *this_conp->rfc931name ? this_conp->rfc931name : "-");
	authname = ( *this_conp->authuser ? this_conp->authuser : "-");

	rhost = ( *remotehost ? remotehost : remaddr);
	rhost = ( *rhost ? rhost : "unknown");

	fseek( logfp, 0L, 2);

	cp = this_conp->logbuf;
	cp2 = logline;

	while ( *cp && ( cp2 < (logline + MIDLEN))) {
		*cp2 = *cp;
		if ( *cp2 == '\n') {
			*++cp2 = '\0';
			fprintf( logfp, "%s %s %s %s", rhost, rfc931p,
					authname, logline);
			cp2 = logline;
			cp++;
			continue;
		}
		cp2++;
		cp++;
	}
	(void) fflush( logfp);
	this_conp->log_ptr = this_conp->logbuf;

#ifndef NO_FLOCK
	unlocklog( &lck, fileno( logfp) );   
#endif

}


