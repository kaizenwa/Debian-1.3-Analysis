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
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "socket.hh"

/* Socket::handle():  null handling function
 *
 *
 */
void Socket::handle()
{
}



/* Socket::Socket():  open a standard file struct for socket]
 *
 */
Socket::Socket( int s )
{
	sock = s;
	io = fdopen( sock, "r+" );
}



/* Socket::~Socket():  disconnect and shut down a socket
 *
 */
Socket::~Socket()
{
  if( sock!=-1 )
  {
    shutdown( sock, 2 );
    sock = -1;
  }

  if( io!=NULL )
  {
    fclose( io );
    io = NULL;
  }
}



/*
 * ListenSocket::ListenSocket():
 *   Start up the socket listening routines
 */
ListenSocket::ListenSocket( int port )
{
	int status;
	struct sockaddr_in sin;
	int one;
  
	status = 0;
	one = 1;
	memset( &sin, 0, sizeof( sin ) );
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = htonl( INADDR_ANY );
	sin.sin_port = htons( port );

	sock = socket( AF_INET, SOCK_STREAM, 0 );
 
	if( sock==-1 )
	{
		status = 1;
	}

	if( !status )
	{
		status = setsockopt( sock, SOL_SOCKET, SO_REUSEADDR, (char*) &one, sizeof( one ) );
	}
  
	if( !status )
	{
		status = bind( sock, (struct sockaddr *) &sin, sizeof( sin ) );
	}	
	if( !status )
	{
		status = listen( sock, 5 );
	}
	if( status )
	{
		shutdown( sock, 2 );
		sock = -1;
	}
}



/*
 * ListenSocket::~ListenSocket():
 *   done_socket():  shut down the socket handling routines
 *   note that this only shuts down the main socket,
 *   not any that were created using accept()
 */
ListenSocket::~ListenSocket()
{
	if( sock!=-1 )
	{
		shutdown( sock, 2 );
	}
}



/*
 * ListenSocket::newsock():  connect a user
 *
 * returns:
 *   #>=0 if successful
 *   -1   otherwise
 */
int ListenSocket::newsock()
{
	int cSidLen;
	int s;
	
	do
	{
		cSidLen = sizeof( cSid );
		s = accept( sock, &cSid, &cSidLen );
	}
	while( s==-1 );

	return s;
}
