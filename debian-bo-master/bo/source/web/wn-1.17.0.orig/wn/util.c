/*
    Wn: A Server for the HTTP
    File: wn/util.c
    Version 1.15.0

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
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <netinet/in.h>
#include <netdb.h>
#include <grp.h>
#include <errno.h>
#include "wn.h"

extern	char		*inet_ntoa();

extern void		www_unescape();

static struct in_addr		ip_address;

#ifdef RFC931_TIMEOUT
extern void		rfc931();
#ifdef SOCKADDR
	static struct sockaddr	*mysin,
				*remsin;
#else
	static struct sockaddr_in	*mysin,
					*remsin;
#endif
#endif


/*
 * get_remote_ip() gets IP address of client via getpeername call and
 * puts it in global variable remaddr.
 */

void
get_remote_ip( )
{
	static struct sockaddr_in	saddr;
	int			size;

	size = sizeof(saddr);
	if ( getpeername(fileno(stdin), (struct sockaddr *) &saddr, &size)< 0){
		*remaddr = '\0';
		*remotehost = '\0';
		if (!isatty(fileno(stdin)))
			logerr(  ERRMSG48, "");
		return;
	}

	ip_address = saddr.sin_addr;
	strcpy(remaddr, inet_ntoa(ip_address));

#ifdef RFC931_TIMEOUT
	remsin = &saddr;
#endif
}



/*
 * get_remote_info() does DNS lookup of remotehost and places host name
 * in global variable remotehost.  
 * It also does RFC931 lookup if RFC931_TIMEOUT
 * is defined.  If possible the call to this function happens after the
 * entire transaction is complete so the user doesn't have to wait for these
 * lookups to happen.  This delay is not possible for CGI or when doing
 * authentication. (RFC931 stuff thanks to Chris Davis).
 * 
 */

void
get_remote_info( )
{
	char    dot_name[MAXHOSTNAMELEN + 1];
	struct hostent	*hostentp = NULL;

#ifndef NO_DNS_HOSTNAMES
	if ( *remotehost || (!*remaddr))
		return;
	/* if (*remotehost) we have already got info; if (!*remaddr)
	   then there is no hope -- we can't even get IP address */

	hostentp = gethostbyaddr((char *) &ip_address.s_addr,
			sizeof (ip_address.s_addr), AF_INET);
	if (hostentp) {
		strcpy( remotehost, hostentp->h_name);

		/* Check that the name has this address listed. */
#ifndef CHECK_DNS_HOSTNAMES
		if ( (!dir_p->accessfile) || (!*dir_p->accessfile)) {
			strlower( remotehost);
			return;
		}
			/* Assume ok unless used for access control */
#endif /* CHECK_DNS_HOSTNAMES */
		if (strlen( remotehost) >= MAXHOSTNAMELEN) {
			hostentp = gethostbyname( remotehost);
		} else {
			sprintf(dot_name, "%s.", remotehost);
			hostentp = gethostbyname( dot_name);
		}

		if (hostentp) {
			register char **ap;
			for (ap = hostentp->h_addr_list; *ap; ap++) {
				if (!memcmp( (char *) &ip_address.s_addr,
						*ap, hostentp->h_length)) {
					/* Value in remotehost is ok.*/
					strlower( remotehost);
					return;
				}
			}
		}
		/* No good name found */
		*remotehost = '\0';
	}
#endif /* NO_DNS_HOSTNAMES */
}


#ifdef RFC931_TIMEOUT
void
get_rfc931()
{
	if (remsin && mysin && (*this_conp->rfc931name == '\0'))
		rfc931(remsin, mysin, this_conp->rfc931name);
	signal( SIGALRM, SIG_DFL);
	alarm( 20);  	/* Give ourselves 20 seconds to get remote DNS data
			 * and write log entry.
			 */
}
#endif


/*
 * mystrncpy( s1, s2, n) is an strncpy() which guarantees a null
 * terminated string in s1.  At most (n-1) chars are copied.
 */

char *
mystrncpy( s1, s2, n)
char	*s1,
	*s2;
int	n;
{
	register char	*cp1,
			*cp2;
	cp1 = s1;
	cp2 = s2;
	n--;
	while ( *cp2 && (n > 0)) {
		n--;
		*cp1++ = *cp2++;
	}
	*cp1 = '\0';
	return s1;
}

/*
 * mystrncat( s1, s2, n) is an strncat() which guarantees a null
 * terminated string in s1.  At most (n-1) chars are appended.
 */

char *
mystrncat( s1, s2, n)
char	*s1,
	*s2;
int	n;
{
	register char	*cp1,
			*cp2;
	cp1 = s1;
	cp2 = s2;
	n--;

	while ( *cp1)
		cp1++;

	while ( *cp2 && (n > 0)) {
		n--;
		*cp1++ = *cp2++;
	}
	*cp1 = '\0';
	return s1;
}

char *
mymemcpy( p1, p2, n)
char	*p1,
	*p2;
int	n;
{
	if ( p1 == p2)
		return (p1);
	while ( n > 0 ) {
		n--;
		*p1++ = *p2++;
	}
	return (p1);
}

/*
 * chop( line)  Cut out CRLF at end of line, or just LF.  Return TRUE
 * if there is a LF at end, otherwise FALSE.
 */

int
chop( line)
char *line;
{
	register char	*cp;

	if ( *line == '\0')
		return FALSE;
	cp = line;
	while ( *cp )
		cp++;
	if ( *--cp == '\n') {
		*cp = '\0';
		if ( (cp > line) && *--cp == '\r')
			*cp = '\0';
		return TRUE;
	}
	return FALSE;
}


/*
 * safer_popen( command, args) calls popen after checking that "args"
 * are safe to pass to a shell.  First the URL escapes in args are decoded.
 * To pass muster decoded args must contain only alphanumerics or SPACE,
 * '/', '.', '%', '@' or '_'.  If '\r' or '\n' are encountered then args is
 * truncated at that point.  If args fails to pass the test then NULL
 * returned.  Otherwise a FILE* for the popened command is returned.
 */

FILE *
safer_popen( command, args)
char	*command,
	*args;
{
	register char	*cp,
			*cp2;

	char		*argptr,
			buf[BIGLEN],
			buf2[BIGLEN];

	strcpy( buf, command);
	argptr = buf + strlen( buf);

	if ( (cp = strrchr( buf, '/')) != NULL)  {
		*cp = '\0';
		if ( chdir( buf) != 0  )
			logerr( ERRMSG49, buf);
		*cp = '/';
	}
	strcpy( buf2, args);

	www_unescape( buf2, ' '); /* Change '+' to space and */
					  /* handle URL escapes */


	cp = buf2;
	if ( *cp) {
		cp2 = argptr;
		*cp2++ = ' ';
		while ( *cp != '\0') {
			switch (*cp) {
				case	'\n':
				case	'\r':
					*cp = *cp2 = '\0';
					break;

				case '/':
				case ' ':
				case '_':
				case '@':
				case '.':
				case '%':
					*cp2++ = *cp++;
					break;
				default:
				/* Anything else should be alphanumeric */
					if ( !isalnum( *cp)) {
						sprintf( buf, ERRMSG20,	*cp);
						logerr( buf, this_rp->request);
						return (NULL);
					}

					*cp2++ = *cp++;
					break;
			}
		}
		*cp2 = '\0';
	}
	else
		strcpy( buf, command);

	if ( *inheadp->tmpfile != '\0') {
		strcat( buf, " < ");
		strcat( buf, inheadp->tmpfile);
	}
	return (popen( buf, "r"));
}

/*
 * int amperline( p1, p2)  Copy p2 to p1 until 
 * p2 is exhausted.  Encode '<', '>', and &. 
 */

int
amperline ( p1, p2)
char	*p1,
	*p2;
{
	register char *cp;
	int found = FALSE;

	while ( *p2 ) {
		switch( *p2) {
		case '<':
			found = TRUE;
			strcpy( p1, "&lt;");
			p1 += 4;
			p2++;
			break;
		case '>':
			found = TRUE;
			strcpy( p1, "&gt;");
			p1 += 4;
			p2++;
			break;
		case '&':
			cp = p2;
			cp++;
			while ( isalnum( *cp))
				cp++;

			if ( *cp == ';') {
				*p1++ = *p2++;
			}
			else {
				found = TRUE;
				strcpy( p1, "&amp;");
				p1 += 5;
				p2++;
			}
			break;
		default:
			*p1++ = *p2++;
		}
	}
	*p1 = 0;
	return found;
}


/*
 * get_local_info() fills in hostname and port from the connected socket.
 */

void
get_local_info( sockdes)
int	sockdes;
{
	int size;
	static struct sockaddr_in      saddr;
	struct hostent  *hostentp;

	size = sizeof(saddr);
	/*  sockdes is our our descriptor for the socket. */
	if ( getsockname( sockdes, (struct sockaddr *) &saddr, &size) < 0 ) {
		daemon_logerr( ERRMSG73, errno);
		errno = 0;
		return;
	}

		/* Remember our port number */
	port = ntohs(saddr.sin_port);
#ifdef RFC931_TIMEOUT
	mysin = &saddr;
#endif
	/* Remember our hostname (or at least dotted quad) */
	if ( *hostname) 
		return;

	strcpy( hostname, inet_ntoa(saddr.sin_addr));

	/* Try for domain name */
	if (hostentp = gethostbyaddr((char *)  &saddr.sin_addr,
			sizeof (saddr.sin_addr.s_addr), AF_INET)) {
		strcpy(hostname, hostentp->h_name);
		strlower(hostname);
	}
}

#ifdef NEED_INITGROUPS
#ifdef STANDALONE

#ifndef NGROUPS_MAX
#define NGROUPS_MAX	(16)
#endif

int
initgroups(gp_name, group_id)
char	*gp_name;
gid_t	group_id;

{
	gid_t		groups[NGROUPS_MAX];
	struct group	*g;
	int		i;
	char		**names;

	groups[0] = group_id;

	for ( i = 1; i < NGROUPS_MAX; i++) {
		if ((g = getgrent()) == NULL)
			break;
		if (g->gr_gid == group_id)
			continue;

		for (names = g->gr_mem; *names != NULL; names++) {
		        if (!strcmp(*names, gp_name))
				groups[i] = g->gr_gid;
		}
	}

	return setgroups(i, groups);
}

#endif
#endif
