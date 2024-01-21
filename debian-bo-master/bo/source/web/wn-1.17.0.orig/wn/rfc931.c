/************************************************************************
* Copyright 1995 by Wietse Venema.  All rights reserved.  Some individual
* files may be covered by other copyrights.
*
* This material was originally written and compiled by Wietse Venema at
* Eindhoven University of Technology, The Netherlands, in 1990, 1991,
* 1992, 1993, 1994 and 1995.
*
* Redistribution and use in source and binary forms are permitted
* provided that this entire copyright notice is duplicated in all such
* copies.  No charge, other than an "at-cost" distribution fee, may be
* charged for copies, derivations, or distributions of this material
* without the express written consent of the copyright holder.
*
* This software is provided "as is" and without any expressed or implied
* warranties, including, without limitation, the implied warranties of
* merchantibility and fitness for any particular purpose.
************************************************************************/

 /*
  * rfc931() speaks a common subset of the RFC 931, AUTH, TAP, IDENT and RFC
  * 1413 protocols. It queries an RFC 931 etc. compatible daemon on a remote
  * host to look up the owner of a connection. The information should not be
  * used for authentication purposes. This routine intercepts alarm signals.
  * 
  * Diagnostics are reported through syslog(3).
  * 
  * Author: Wietse Venema, Eindhoven University of Technology, The Netherlands.
  */

 /*
  * Originally from tcp_wrappers 7.2 package; modifications for WN/1.01
  * support by Christopher Davis <ckd@kei.com>
  *
  * Modifications include:
  *   - use of wn.h rather than tcpd.h for config information
  *   - use of logerr instead of tcpd_warn
  *   - use of mystrncpy instead of STRN_CPY
  *   - buffer sizes chosen from wn's common.h
  */


#ifndef lint
static char sccsid[] = "@(#) rfc931.c 1.10 95/01/02 16:11:34";
#endif



/* System libraries. */


#include "../config.h"

#ifdef RFC931_TIMEOUT   /* Only do RFC931 if this is defined */

#include <stdio.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>

/* Local stuff. */

#include "wn.h"


#define	BUFFSIZE	(8192)
#define	RFC931_PORT	113		/* Semi-well-known port */
#define	ANY_PORT	0		/* Any old port will do */

int     rfc931_timeout = RFC931_TIMEOUT;/* Global so it can be changed */

static jmp_buf timebuf;

/* fsocket - open stdio stream on top of socket */

static FILE *fsocket(domain, type, protocol)
int     domain;
int     type;
int     protocol;
{
    int     s;
    FILE   *fp;

    if ((s = socket(domain, type, protocol)) < 0) {
	logerr( ERRMSG77, "");
	return (0);
    } else {
	if ((fp = fdopen(s, "r+")) == 0) {
	    logerr( ERRMSG78, "");
	    close(s);
	}
	return (fp);
    }
}

/* timeout - handle timeouts */

static void timeout(sig)
int     sig;
{
    longjmp(timebuf, sig);
}

/* rfc931 - return remote user name, given socket structures */

void    
rfc931(rmt_sin, our_sin, dest)
struct sockaddr_in *rmt_sin;
struct sockaddr_in *our_sin;
char   *dest;
{
    unsigned rmt_port;
    unsigned our_port;
    struct sockaddr_in rmt_query_sin;
    struct sockaddr_in our_query_sin;
    char    user[SMALLLEN];
    char    buffer[BUFFSIZE];
    char   *cp;
    char   *result = "";
    FILE   *fp;

    /*
     * Use one unbuffered stdio stream for writing to and for reading from
     * the RFC931 etc. server. This is done because of a bug in the SunOS
     * 4.1.x stdio library. The bug may live in other stdio implementations,
     * too. When we use a single, buffered, bidirectional stdio stream ("r+"
     * or "w+" mode) we read our own output. Such behaviour would make sense
     * with resources that support random-access operations, but not with
     * sockets.
     */

    if ((fp = fsocket(AF_INET, SOCK_STREAM, 0)) != 0) {
	setbuf(fp, (char *) 0);

	/*
	 * Set up a timer so we won't get stuck while waiting for the server.
	 */

	if (setjmp(timebuf) == 0) {
	    signal(SIGALRM, timeout);
	    alarm(rfc931_timeout);

	    /*
	     * Bind the local and remote ends of the query socket to the same
	     * IP addresses as the connection under investigation. We go
	     * through all this trouble because the local or remote system
	     * might have more than one network address. The RFC931 etc.
	     * client sends only port numbers; the server takes the IP
	     * addresses from the query socket.
	     */

	    our_query_sin = *our_sin;
	    our_query_sin.sin_port = htons(ANY_PORT);
	    rmt_query_sin = *rmt_sin;
	    rmt_query_sin.sin_port = htons(RFC931_PORT);

	    if (bind(fileno(fp), (struct sockaddr *) & our_query_sin,
		     sizeof(our_query_sin)) >= 0 &&
		connect(fileno(fp), (struct sockaddr *) & rmt_query_sin,
			sizeof(rmt_query_sin)) >= 0) {

		/*
		 * Send query to server. Neglect the risk that a 13-byte
		 * write would have to be fragmented by the local system and
		 * cause trouble with buggy System V stdio libraries.
		 */

		fprintf(fp, "%u,%u\r\n",
			ntohs(rmt_sin->sin_port),
			ntohs(our_sin->sin_port));
		fflush(fp);

		/*
		 * Read response from server. Use fgets()/sscanf() so we can
		 * work around System V stdio libraries that incorrectly
		 * assume EOF when a read from a socket returns less than
		 * requested.
		 */

		if (fgets(buffer, sizeof(buffer), fp) != 0
		    && ferror(fp) == 0 && feof(fp) == 0
		    && sscanf(buffer, "%u , %u : USERID :%*[^:]:%255s",
			      &rmt_port, &our_port, user) == 3
		    && ntohs(rmt_sin->sin_port) == rmt_port
		    && ntohs(our_sin->sin_port) == our_port) {

		    /*
		     * Strip trailing carriage return. It is part of the
		     * protocol, not part of the data.
		     */

		    if (cp = strchr(user, '\r'))
			*cp = 0;
		    result = user;
		}
	    }
	    alarm(0);
	}
	fclose(fp);
    }
    mystrncpy(dest, result, SMALLLEN);
}
#endif /* RFC931_TIMEOUT */
