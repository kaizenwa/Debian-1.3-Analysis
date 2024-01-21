/*
    Wn: A Server for the HTTP
    File: wn/standalone.c
    Version 1.15.5
    
    Copyright (C) 1996  <by John Franks>

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
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <ctype.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <pwd.h>
#include <string.h>
#ifndef NO_UNISTD_H
#include <unistd.h>
#endif
#include "wn.h"

#define QUEBACKLOG	(5)

extern FILE	*logfp;

static void	timehack();

#if defined( BSD_LIKE) || defined( POSIX_SIGNALS)
static void	zombie();
#endif

extern int	daemon_init();

void
do_standalone()
{
	FILE	*pid_fp;
	char	mbuf[2*SMALLLEN];

	int 	user_id = USERID,
		group_id = GROUPID,
		sockdes,
		sd,
		len,
		pid,
		kav = 1,
		nagle = 1,
		on = TRUE;
	
	struct hostent *hptr;

	struct linger ling;
	struct sockaddr_in	sa_server,
				sa_client;

	umask( 077);

	timehack( );

	if ((sd = socket(AF_INET,SOCK_STREAM,IPPROTO_TCP)) == -1) {
		daemon_logerr( ERRMSG22, errno);
		exit(2);
	}

	ling.l_onoff = ling.l_linger = 0;


#ifndef NO_LINGER
	if ( setsockopt( sd, SOL_SOCKET, SO_LINGER, (char *) &ling,
					    sizeof (ling)) == -1) {
		daemon_logerr( ERRMSG23, errno);
		exit(2);
	}
#endif

	if ( (setsockopt( sd, SOL_SOCKET, SO_REUSEADDR,
					(char *) &on, sizeof( on))) == -1) {
		daemon_logerr( ERRMSG24, errno);
		exit(2);
	}


#ifndef DO_NAGLE
	if ( setsockopt(sd, IPPROTO_TCP, TCP_NODELAY,
				(char*)&nagle, sizeof(nagle))) {
		daemon_logerr( ERRMSG105, errno);
		/* Not fatal */
	}
#endif /* DO_NAGLE */

	if ( (setsockopt( sd, SOL_SOCKET, SO_KEEPALIVE,
					(char *) &kav, sizeof( kav))) == -1) {
		daemon_logerr( ERRMSG98, errno);
		exit(2);
	}

#ifdef BSD_LIKE
        signal( _WN_SIGCHLD, (void (*)())zombie );
#else /* not BSD_LIKE */
#ifndef POSIX_SIGNALS
        signal( _WN_SIGCHLD, SIG_IGN);
#else	
        signal( _WN_SIGCHLD, (void (*)())zombie );
#endif
#endif /* (not) BSD_LIKE */

	bzero((char *)&sa_server, sizeof( sa_server));
	sa_server.sin_family = AF_INET;
	sa_server.sin_port = htons( port);

#ifdef USE_VIRTUAL_HOSTS
	sa_server.sin_addr.s_addr = htonl( INADDR_ANY);
#else
	if ( *hostname && ((hptr = gethostbyname(hostname)) != NULL)) {
		bcopy(hptr->h_addr, (char *) &(sa_server.sin_addr.s_addr),
				hptr->h_length);
	}
	else {
		if ( *hostname) {
			sprintf( mbuf, ERRMSG91, hostname);
			strcat( mbuf, ERRMSG92);
			daemon_logerr( mbuf, errno);
		}
		sa_server.sin_addr.s_addr = htonl( INADDR_ANY);
	}
#endif

	if( bind( sd, (struct sockaddr *) &sa_server,
			sizeof(sa_server)) == -1) {
		perror( ERRMSG25);
		daemon_logerr( ERRMSG25, errno);
	        exit(2);
	}

	listen( sd, QUEBACKLOG);

	if ( getuid() == 0 ) {  /* Running as root */
		struct passwd	*pw;

		if ( (pw = getpwuid( (uid_t) user_id)) == (struct passwd *)NULL
			|| initgroups( pw->pw_name, group_id) == -1
			|| setgid( (gid_t) group_id) == -1) {
			daemon_logerr(  ERRMSG26, errno);
			exit( 2);
		}

		if (setuid( (uid_t)user_id) == -1) {
			daemon_logerr(  ERRMSG27, errno);
			exit( 2);
		}
	}


	if ( wnlogfile[0]) {		/* We are logging to this file */
		open_wnlog( wnlogfile, errlogfile);
			/* We delayed openning it until after setuid */
	}

	if ( *pid_file) {
		if ( (pid_fp = fopen( pid_file, "w")) != NULL) {
			fprintf( pid_fp, "%d\n", getpid());
			fclose( pid_fp);
		}
		else
			daemon_logerr(  ERRMSG97, errno);
	}
	else {
		sprintf( mbuf, "%d\n", getpid());
		write( 1, mbuf, strlen( mbuf));
	}

        len = sizeof(sa_client);
        if ( (sockdes = accept( sd, (struct sockaddr *) &sa_client,
							&len)) == -1 ) {
		if( errno != EINTR)
			daemon_logerr( ERRMSG28, errno);
	}


	get_local_info( sockdes);

	while ( TRUE) {


        	if((pid = fork()) == -1) {
			daemon_logerr( ERRMSG29, errno);
		}

		if ( pid == 0 ) { 		/* Child process */
			close(0);
			dup2( sockdes, 0);
			close(1);
			dup2( sockdes, 1);

			signal( SIGHUP, SIG_DFL);
			signal( SIGQUIT, SIG_DFL);
			signal( SIGINT, SIG_DFL);
			close(sd);
			close(sockdes);
			do_connection();
			exit (0);
		}

		close(sockdes);

	        while ( (sockdes = accept( sd,
				(struct sockaddr *) &sa_client, &len)) < 0 ) {
			if( errno != EINTR) {
				daemon_logerr( ERRMSG28, errno);
			}
		}
		errno = 0;
	}
}

#if defined( BSD_LIKE) || defined( POSIX_SIGNALS)
static void
zombie()
{
#ifndef NEXT
	int status;
#else
	union wait status;
#endif
	pid_t	pid;

	bzero( &status, sizeof( status));
	while( (pid = wait3(&status, WNOHANG, NULL)) > 0)
		;
#ifdef POSIX_SIGNALS
        signal( _WN_SIGCHLD, (void (*)())zombie );
#endif
}
#endif

/*
 * The following function adapted from Stevens, "Advanced Programming in the
 * Unix Environment", p. 418,  initializes the the standalone daemon.
 */

int
daemon_init()
{
	int	open_max,
		i;

	pid_t	pid,
		procgp;

	if ( (pid = fork()) < 0 )
		return (-1);
	else if ( pid != 0 ) {
		if ( admin_mode)
			fprintf( stdout, "%d\n", pid);
		exit( 0);
	}

#ifndef NO_SETSID
	if ( (procgp = setsid()) == -1 ) {
		daemon_logerr( ERRMSG30, errno);
		perror("setsid");
		exit( 2);
	}
#else
	if ( ( procgp = setpgrp( getpid(), 0) ) == -1) {
		daemon_logerr( ERRMSG31, errno);
		perror("setpgrp");
		exit( 2);
	}
#endif    
	chdir( "/");
	umask( 0);

#ifdef NEXT
	open_max = 32;
#else
#ifdef RISCOS
	open_max = getdtablesize ();
#else
	open_max = sysconf( _SC_OPEN_MAX);
#endif
#endif

	for ( i = 3; i < open_max; i++) {
		close( i);
	}
	return (0);
}



static void
timehack()
{
	char 		buf[TINYLEN];
	time_t		clock;
	struct tm 	*dummy;

	time(&clock);
	dummy = localtime (&clock);
	dummy = gmtime (&clock);

	strftime (buf, TINYLEN, "%d/%b/%Y:%H:%M:%S", dummy);
	gethostbyname( "localhost");
}



