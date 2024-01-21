/* Utilities for VFS modules.

   Currently includes login and tcp open socket routines.
   
   Copyright (C) 1995, 1996 Miguel de Icaza
   
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
#include <config.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <sys/types.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <malloc.h>
#ifdef USE_TERMNET
#include <termnet.h>
#endif

#include <signal.h>
#include <errno.h>
#include "tcputil.h"
#include "../src/dialog.h"	/* for message () */
#include "../src/mem.h"		/* for bcopy */
#include "../src/util.h"	/* for unix_error_string */
#include "../src/mad.h"

int open_tcp_link  (char *host, int *port, int *version, char *caller)
{
    struct   sockaddr_in server_address;
    unsigned long inaddr;
    struct   hostent *hp;
    int      my_socket;

    if (!*host)
	return 0;
    
    bzero ((char *) &server_address, sizeof (server_address));
    server_address.sin_family = AF_INET;
    
    /*  Try to use the dotted decimal number */
    if ((inaddr = inet_addr (host)) != -1)
	bcopy ((char *) &inaddr, (char *) &server_address.sin_addr,
	       sizeof (inaddr));
    else {
	if ((hp = gethostbyname (host)) == NULL){
	    message_2s (1, caller, " Can't locate hostname: %s ", host);
	    return 0;
	}
	bcopy ((char *) hp->h_addr, (char *) &server_address.sin_addr,
	       hp->h_length);
    }

    /* Try to contact a remote portmapper to obtain the listening port */
    if (*port == 0){
	*port = get_remote_port (&server_address, version);
	if (*port < 1)
	    return 0;
    }
    server_address.sin_port = htons (*port);
    
    if ((my_socket = socket (AF_INET, SOCK_STREAM, 0)) < 0){
	message_2s (1, caller, " Can't create socket: %s ",
		 unix_error_string(errno));
	return 0;
    }
    if (connect (my_socket, (struct sockaddr *) &server_address,
	     sizeof (server_address)) < 0){
	message_2s (1, caller, " Can't connect to server: %s ",
		 unix_error_string (errno));
	close (my_socket);
	return 0;
    }
    return my_socket;
}

/* Extract the hostname and username from the path */
/* path is in the form: [user@]hostname:port/remote-dir, e.g.:
 *
 * ftp://sunsite.unc.edu/pub/linux
 * ftp://miguel@sphinx.nuclecu.unam.mx/c/nc
 * ftp://tsx-11.mit.edu:8192/
 * ftp://joe@foo.edu:11321/private
 *
 * If the user is empty, e.g. ftp://@roxanne/private, then your login name
 * is supplied.
 *
 * returns malloced host, user.
 * returns a malloced strings with the pathname relative to the host.
 * */

char *get_host_and_username (char *path, char **host, char **user, int *port,
			     int default_port, int default_is_anon,
			     char **pass)
{
    struct passwd *passwd_info;
    char *p, *q;

    *pass = NULL;
    *port = default_port;
    for (p = path; ((*p != '/') && (*p != ':')) && *p; p++)
	;

    q = strchr (path, '@');
    if (q != NULL && q < p) {
        if (q == path)
            *user = NULL;
        else {
            *user = (char *) xmalloc (q  - path + 1, "get_host_and_username");
            strncpy (*user, path, q - path);
            (*user) [q - path] = 0;
        }
        q++;
        *host = (char *) xmalloc (p - q + 1, "get_host_and_username");
        strncpy (*host, q, p - q);
        (*host) [p-q] = 0;
    } else {
        *host = (char *) xmalloc (p - path + 1, "get_host_and_username");
        strncpy (*host, path, p - path);
        (*host) [p-path] = 0;
        *user = NULL;
#ifdef USE_NETRC
        if (use_netrc)
            if (lookup_netrc (*host, user, pass) < 0) {
                if (*user) { free (*user); *user = NULL; }
                if (*pass) { free (*pass); *pass = NULL; }
            }
#endif
        if (*user == NULL && default_is_anon)
	    *user = strdup ("anonymous");
    }

    /* If we got a port spec ... */
    if (*p == ':'){
	
	q = ++p;

	for (;*q != '/' && *q; q++)
	    ;
	
	if (!*q && q == p){
	    if (!(*user))
		*user = strdup ("");
	    /* on return: *host and *user always malloced, *pass malloced
             * or NULL */
	    return 0;
	}

	*port = atoi (p);
	
	if (*port <= 0 || *port >= 65536)
	    *port = 21;
	p = q;
    }

    if (!*user){
	if ((passwd_info = getpwuid (geteuid ())) == NULL)
	    *user = strdup ("anonymous");
	else {
	    *user = strdup (passwd_info->pw_name);
	}
	endpwent ();
    }
    if (p && *p)
	return strdup (p);
     else
	return strdup ("/");
}
