#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrlsckt.c (Control Socket)
 * Purpose:	Open a pipe device for IO
 * Subroutine:	open_socket_listener()		returns: int
 * Subroutine:	accept_socket_connection()	returns: struct *connectRec
 * Subroutine:	close_socket()			returns: void
 * Subroutine:	flush_socket()			returns: void
 * Copyright:	1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} John Roll	initial version			 1 March 1990
 *		{1} MVH simplified for new connect module	10 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#ifndef VMS

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>		/* socket(), bind(), listen(), accept(), etc */
#include <netdb.h>
#include <netinet/in.h>		/* htonl(), struct sockaddr_in */
#ifdef SYSV
#include <string.h>		/* strlen(), strcpy(), strncpy(), strrchr() */
#else
#include <strings.h>		/* strlen(), strcpy(), strncpy(), rindex() */
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include "hfiles/control.h"	/* define struct connectRec */

/*
 * Subroutine:	open_socket_listener
 * Purpose:	Get a socket on which to listen for connections
 */
int open_socket_listener ( host_name, port_address )
     char **host_name;
     int port_address;
{
  struct hostent *host_port;
  struct sockaddr_in net_address;
  int ipc;
  static char *local_name;
  char *calloc_errchk();

  bzero(&net_address, sizeof(net_address));

  /* if host name not given, use the local host name of this machine */
  if( *host_name == NULL ) {
    /* if local host name not yet known, get it and remember it */
    if( local_name == NULL ) {
      char name[100];
      if( gethostname(name, 100) != 0 ) {
	perror("Can't get user's hostname for socket\n  ");
	return( -1 );
      }
      local_name = calloc_errchk(strlen(name)+2, 1, "socket host name");
      (void)strncpy(local_name, name, 100);
    }
    *host_name = local_name;
  }
  /* get host port address information */
  if( (host_port = gethostbyname(*host_name)) == NULL ) {
    (void)fprintf(stderr, "Unknown host for socket: %d on %s\n",
		  port_address, *host_name);
    return( -1 );
  }
  /* fill in the network address structure */
  net_address.sin_family = AF_INET;
  /* convert port address from host to network byte order */
  net_address.sin_port = htonl(port_address);
  bcopy(host_port->h_addr, &(net_address.sin_addr),
	sizeof(net_address.sin_addr));
  /* create a socket communication endpoint */
  /* use full-duplex byte stream & ARPA Internet protocol */
  if( (ipc = socket(PF_INET, SOCK_STREAM, 0)) < 0 ) {
    perror("Cannot create a socket connection\n  ");
    return( -1 );
  }
  /* bind the socket to the network address */
  if( bind(ipc, &net_address, sizeof(net_address)) ) {
    perror("cannot bind socket to host address\n  ");
    return( -1 );
  }
  /* start  listening for connections (allowing a backlog of 5) */
  if( listen(ipc, 5) ) {
    perror("Cannot listen on socket\n  ");
    return( -1 );
  }
  return( ipc );
}

/*
 * Subroutine:	accept_socket_connection
 * Purpose:	accept a connection requested of the socket listener
 */
struct connectRec *accept_socket_connection ( listener )
     struct connectRec *listener;
{
  struct sockaddr net_address;
  int ipc;
  int address_len;
  struct connectRec *connection;
  char *calloc_errchk();

  address_len = sizeof(net_address);
  if( (ipc = accept(listener->fd, &net_address, &address_len)) < 0 ) {
    perror("Cannot accept socket connection");
    return( NULL );
  }
  connection = (struct connectRec *)
   calloc_errchk(1, sizeof(struct connectRec), "socket descriptor");
  connection->fd = ipc;
  connection->type = IOP_socket;
  connection->direction = listener->direction;
  connection->protocol = listener->protocol;
  connection->func = listener->func;
  connection->name = listener->name;
  connection->address = listener->address;
  connection->affiliate = listener;
  listener->affiliate = connection;
  return( connection );
}

/*
 * Subroutine:	flush_socket
 * Purpose:	flush all pending input on this socket connection
 */
void flush_socket ( ipc, socketname )
     int ipc;
     char *socketname;
{
  extern void flush_disk();

  if( socketname == NULL )
    flush_disk(ipc, "socket");
  else
    flush_disk(ipc, socketname);
}

/*
 * Subroutine:	close_socket
 * Purpose:	close this socket's file descriptor
 */
void close_socket ( ipc )
     int ipc;
{
  if( close(ipc) )
    perror("Error closing socket\n  ");
}

#endif

