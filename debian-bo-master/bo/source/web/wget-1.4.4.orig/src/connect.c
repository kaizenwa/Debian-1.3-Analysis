/* Establishing and handling network connections.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* $Id: connect.c,v 1.1.1.1.2.2 1997/02/15 19:22:47 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#ifdef WINDOWS
#  include <winsock.h>
#  include "windecl.h"
#else
#  include <sys/socket.h>
#  include <netdb.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#endif /* WINDOWS */

#include <errno.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
#endif /* HAVE_SYS_SELECT_H */

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "connect.h"
#include "host.h"

#ifndef errno
extern int errno;
#endif

extern struct options opt;

/* Global variables share by bindport and acceptport: */
int msock = -1;
size_t addrlen;
struct sockaddr *addr;
struct sockaddr_in srv;


uerr_t
make_connection(int *sock, char *hostname, unsigned short port)
{
   struct sockaddr_in sock_name;
   /* struct hostent *hptr; */

   /* Get internet address of the host. We can do it either by calling
      ngethostbyname, or by calling store_hostaddress, from
      host.c. Storehostaddress is better since it caches calls to
      gethostbyname. */
#if 1
   if (!store_hostaddress((unsigned char *)&sock_name.sin_addr, hostname))
      return HOSTERR;
#else /* never */
   if (!(hptr = ngethostbyname(hostname)))
      return HOSTERR;
   /* Copy the address of the host to socket description. */
   memcpy(&sock_name.sin_addr, hptr->h_addr, hptr->h_length);
#endif
   /* Set port and protocol */
   sock_name.sin_family = AF_INET;
   sock_name.sin_port = htons(port);
   /* Make an internet socket, stream type. */
   if ((*sock = socket(AF_INET, SOCK_STREAM, 0)) == -1)
      return CONSOCKERR;

   /* Connect the socket to the remote host. */
   if (connect(*sock, (struct sockaddr *) &sock_name, sizeof (sock_name)))
   {
      if (errno == ECONNREFUSED)
	 return CONREFUSED;
      else
	 return CONERROR;
   }
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "Created fd %d.\n", *sock);
#endif
   return NOCONERROR;
}

/* bindport does all the work necessary to bind a port, creating a
   master socket msock. Use acceptport to call accept and receive the
   actual socket. */
uerr_t
bindport(unsigned short *port)
{
   int optval = 1;

   msock = -1;
   addr = (struct sockaddr *) & srv;
   addrlen = sizeof(srv);
   if ((msock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
      return CONSOCKERR;
   if (setsockopt(msock, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) < 0)
      return CONSOCKERR;
   srv.sin_family = AF_INET;
   srv.sin_addr.s_addr = htonl(INADDR_ANY);
   srv.sin_port = htons(*port);
   if (bind(msock, addr, addrlen) < 0)
   {
      CLOSE(msock);
      msock = -1;
      return BINDERR;
   }
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "Master socket fd %d bound.\n", msock);
#endif
   if (!*port)
   {
      if (getsockname(msock, addr, &addrlen) < 0)
      {
	 CLOSE(msock);
	 msock = -1;
	 return CONPORTERR;
      }
      *port = ntohs(srv.sin_port);
   }
   if (listen(msock, 1) < 0)
   {
      CLOSE(msock);
      msock = -1;
      return LISTENERR;
   }
   return BINDOK;
}

/* Acceptport uses msock and addrlen to call the accept system call
   and return the data socket of the connection. It blocks the caller,
   as per accept. */
uerr_t
acceptport(int *sock)
{
#ifdef HAVE_SELECT
   if (select_fd(msock, opt.timeout) <= 0)
      return ACCEPTERR;
#endif
   if ((*sock = accept(msock, addr, &addrlen)) < 0)
      return ACCEPTERR;
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "Created socket fd %d.\n", *sock);
#endif
   return ACCEPTOK;
}

/* Closes the sockets.  If sock is -1, close only msock (which is
   private to connect.c.  */
void
closeport(int sock)
{
   /*shutdown(sock, 2);*/
   if (sock != -1)
      CLOSE(sock);
   if (msock != -1)
      CLOSE(msock);
   msock = -1;
}

/* Return the local IP address associated with the connection on fd.
   It is returned in a static buffer. */
unsigned char *
conaddr(int fd)
{
   static unsigned char res[4];
   struct sockaddr_in mysrv;
   struct sockaddr *myaddr;
   size_t len = sizeof(mysrv);

   myaddr = (struct sockaddr *)(&mysrv);
   if (getsockname(fd, myaddr, &len) < 0)
      return NULL;
   memcpy(res, &mysrv.sin_addr, 4);
   return res;
}

#ifdef HAVE_SELECT
/* select_fd - Waits for file descriptor fd to be readable, where
   maxtime is the timeout in seconds. Returns 1 if fd is readable, 0
   for timeout and -1 for error. */
int
select_fd(int fd, int maxtime)
{
   fd_set readfds, exceptfds;
   struct timeval timeout;
   int nfds;
   
   FD_ZERO(&readfds);
   FD_SET(fd, &readfds);
   FD_ZERO(&exceptfds);
   FD_SET(fd, &exceptfds);
   timeout.tv_sec = maxtime;
   timeout.tv_usec = 0;
   /* This is just a minor HPUX change to avoid warnings. */
#ifndef __hpux
   nfds = select(fd + 1, &readfds, NULL, &exceptfds, &timeout);
#else  /* __hpux */
   nfds = select(fd + 1, (int *)&readfds, NULL, (int *)&exceptfds, &timeout);
#endif /* __hpux */
   return nfds;
}
#endif /* HAVE_SELECT */

/* The same like read, but takes care of EINTR and uses select to
   timeout the stale connections.  */
int
iread(int fd, char *buf, int len)
{
   int res;

   do
   {
#ifdef HAVE_SELECT
      if (opt.timeout)
      {
	 do
	 {
	    res = select_fd(fd, opt.timeout);
	 } while (res == -1 && errno == EINTR);
	 if (res <= 0)
	 {
	    /* Set errno to ETIMEDOUT on timeout. */
	    if (res == 0)
	       errno = ETIMEDOUT;
	    return -1;
	 }
      }
#endif
      res = READ(fd, buf, len);
   } while (res == -1 && errno == EINTR);
   
   return res;
}

/* Similar to iread, but doesn't do select.  */
int
iwrite(int fd, char *buf, int len)
{
   int res;

   do
   {
      res = WRITE(fd, buf, len);
   } while (res == -1 && errno == EINTR);
   
   return res;
}
