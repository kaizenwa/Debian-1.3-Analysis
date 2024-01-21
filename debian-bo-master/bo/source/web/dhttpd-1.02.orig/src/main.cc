/*
 *   dhttpd/1.02 - Personal web page server version 1.02
 *   Copyright (C) 1997  David A. Bartold
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "config.hh"
#include "socket.hh"
#include "httpsock.hh"
#include "version.hh"

int numProc;

void handleChildTerm( int )
{
	if( waitpid( 0, NULL, WNOHANG )>0 )
	{
		numProc--;
	}
	signal( SIGCHLD, handleChildTerm );
}

void spork()
{
	struct passwd *pwent;
	int user_id, group_id;

	chdir( "/" );

	if( fork()!=0 )
	{
		exit( 0 );
	}

	setsid();

	user_id = UID;
	group_id = GID;
	
	/* If we're the super user, set user id, if new id is different */
	if( getuid()==0 && user_id!=0 )
	{
		/* Check password file */
		if ( (pwent = getpwuid( user_id ) ) == NULL)
		{
    			exit( 1 );
		}

		/* Reset groups attribute. */
		if( initgroups( pwent->pw_name, group_id ) == -1 )
		{
			exit( 1 );
		}

		/* Switch to our new user id.  We have to set the group first */
		if( setgid( group_id )==-1 )
		{
			exit( 1 );
		}
		if( setuid( user_id )==-1 )
		{
			exit( 1 );
		}
	}
}

int main( int argc, char *argv[] )
{
	int o;
	int portnum = DEFAULTPORT;

	for( ;; )
	{
		o = getopt( argc, argv, "p:h" );
		if( o==-1 )
		{
			break;
		}
		switch( o )
		{
			case 0:
				printf( "Invalid option!\n" );
				exit( 1 );
				break;

			case 'h':
				printf( "usage: %s [options]\n", argv[ 0 ] );
				printf( "  -p (port)  Use a different port than the default of %i\n", DEFAULTPORT );
				printf( "  -h         Help\n" );
				return 0;

			case 'p':
				sscanf( optarg, "%i", &portnum );
				break;
		}
	}

	ListenSocket listen( portnum );
	pid_t pid;
	int s;

	if( listen.geterror() )
	{
		fprintf( stderr, "Could not open port %i.  %s failed to start\n", portnum, DHTTPDVERSION );
		exit( 1 );
	}

	/* now let's become a daemon */
	spork();

	signal( SIGCHLD, handleChildTerm );
	signal( SIGHUP, SIG_IGN );

	numProc = 0;
	for( ;; )
	{
		if( numProc<MAXCHILDPROC )
		{
			s = listen.newsock();
		}
		else
		{
			s = -1;
			sleep( 1 );
		}
		
		if( s!=-1 )
		{
			numProc++;
			pid = fork();
			if( pid==0 )
			{
				/* Child process (fulfill request) */
				close( listen.getfd() );
				
				/* Handle Socket */ {
					HttpSocket sock( s );
					if( !sock.geterror() )
					{
						sock.handle();
						fflush( sock.getio() );
					}
				}
				exit( 0 );
			}
			else if( pid==-1 )
			{
				/* Error creating child */
				numProc--;
				close( s );
			}
			else
			{
				/* If we're the parent, close file */
				close( s );
			}
		}
	}

	/* make compiler happy */
	return 0;
}
