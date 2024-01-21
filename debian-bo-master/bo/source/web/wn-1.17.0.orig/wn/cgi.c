/*
    Wn: A Server for the HTTP
    File: wn/cgi.c
    Version 1.16.0
    
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
#include <string.h>
#include <errno.h>
#include "wn.h"
#include "version.h"
#include "cgi.h"

extern char	*malloc();

static void	cgi_headers();

static CGI_data	*cgip = NULL;

static char	cgi_content_type[SMALLLEN];

/*
 * sendcgi( ip)  Open pipe from "ip->filepath" command
 * and send output using CGI standards
 */

void
sendcgi( ip)
Request	*ip;
{
#ifndef FORBID_CGI
	FILE	*fp;

	register char	*cp;
	int		fdfp,
			buflen;

	char	command[MIDLEN],
		location[MIDLEN],
		cgibuf[OUT_BUFFSIZE + 4],
		buf[BIGLEN],
		*bufptr;

	exec_ok( ip);

	location[0] = '\0';
	*ip->length = '\0';  /* Don't send length of script!! */
	cgi_env( ip, FALSE);  /* Full CGI environment */

	mystrncpy( command, ip->filepath, MIDLEN);
	mystrncpy( buf, ip->filepath, MIDLEN);
	cp = strrchr( buf, '/');
	*cp = '\0';
	if ( chdir( buf) != 0  )
		logerr( ERRMSG106, buf);

	if ( ((inheadp->method == POST) || (inheadp->method == PUT))
			&& (*inheadp->tmpfile != '\0')) {
		strcat( command, " < ");
		strcat( command, inheadp->tmpfile);
	}

	if ( !*ip->query || (strchr( ip->query, '=') != NULL)) {
		/* '=' in query so don't use on command line */
		if ((fp = popen( command, "r")) == (FILE *) NULL ) {
			senderr( SERV_ERR, ERRMSG55, ip->filepath);
			wn_exit( 2);
		}
	} else { /* no '=' means its an isindex */
		www_unescape( ip->query, ' ');
		if ( (fp = safer_popen( ip->filepath, ip->query))
					== (FILE *) NULL )
			if ( (fp = popen( command, "r")) 
						== (FILE *) NULL ) {
				senderr( SERV_ERR, ERRMSG55, ip->filepath);
				wn_exit( 2);
			}
	}

	fdfp = fileno( fp);

	if ( ip->type == RTYPE_NPH_CGI) {  /* CGI handles headers */
		ip->attributes |= WN_UNBUFFERED;  /* Don't buffer CGI */
		send_out_fd( fdfp);
		pclose( fp);
		writelog( ip, LOGMSG7, ip->filepath);
		if ( *inheadp->tmpfile)
			unlink( inheadp->tmpfile);
		return;
	}

	/* It's a standard CGI, not an nph-CGI.  We do headers */
	cgi_headers( fp, cgibuf, location, &bufptr, &buflen);

	if ( !*location) {

		http_prolog( );
		ip->attributes |= WN_UNBUFFERED;  /* Don't buffer CGI */
		send_out_mem( bufptr, buflen);
		send_out_fd( fdfp);

		pclose( fp);
		writelog( ip, LOGMSG8, ip->filepath);
		if ( *inheadp->tmpfile)
			unlink( inheadp->tmpfile);
		return;  /* to end of process_url */
	}
	else {
		writelog( ip, LOGMSG9, location);
		sprintf( ip->request, "CGI redirect to %s", location);
		dolocation( location, ip);
		pclose( fp);
		if ( *inheadp->tmpfile)
			unlink( inheadp->tmpfile);
		return; /* to end of process_url */
	}
#endif
}


/*
 * cgi_env( ip, auth) Create environment variables required for CGI.
 * Also WN_ROOT and WN_DIR_PATH.  If auth = TRUE then only do a 
 * small subset of the variables for authentication. 
 */

void
cgi_env( ip, auth)
Request	*ip;
int	auth;
{
	register char	*cp;

	if ( ip->status & WN_CGI_SET)
		return;		/* CGI environment vars already set */

	if ( cgip == NULL) {
		if ((cgip = (CGI_data *) malloc(sizeof (CGI_data))) == NULL ) {
			senderr( SERV_ERR, ERRMSG55, ERRMSG64);
			wn_exit(2);
		}
	}

	strcpy( cgip->method, "REQUEST_METHOD=");
	switch ( inheadp->method) {
	case GET:
		strcat( cgip->method, "GET");
		break;
	case POST:
		strcat( cgip->method, "POST");
		strcpy( cgip->postfile, "HTTP_POST_FILE=");
		mystrncat( cgip->postfile, inheadp->tmpfile, SMALLLEN);
		putenv( cgip->postfile);
		break;
	case PUT:
		strcat( cgip->method, "PUT");
		strcpy( cgip->postfile, "HTTP_PUT_FILE=");
		mystrncat( cgip->postfile, inheadp->tmpfile, SMALLLEN);
		putenv( cgip->postfile);
		break;
	default:
		break;
	}
	putenv( cgip->method);

	strcpy( cgip->raddr, "REMOTE_ADDR=");
	mystrncat( cgip->raddr, remaddr, TINYLEN);
	putenv( cgip->raddr);

	strcpy( cgip->dirpath, "WN_DIR_PATH=");
	mystrncat( cgip->dirpath, ip->cachepath, SMALLLEN);
	if ( (cp = strrchr( cgip->dirpath, '/')) != NULL )
		*cp = '\0';
	putenv( cgip->dirpath);

	if ( *(this_conp->authuser)) {
		strcpy( cgip->ruser, "REMOTE_USER=");
		mystrncat( cgip->ruser, this_conp->authuser, SMALLLEN);
		putenv( cgip->ruser);
	}

	if ( *(dir_p->authtype)) {
		strcpy( cgip->authtype, "AUTH_TYPE=");
		mystrncat( cgip->authtype, dir_p->authtype, TINYLEN - 10);
		putenv( cgip->authtype);
	}

	/* End of auth set of CGI variables */
	if ( auth)
		return;

	strcpy( cgip->dataroot, "WN_ROOT=");
	strcat( cgip->dataroot, ip->rootdir);
	putenv( cgip->dataroot);


	if ( !*remotehost )	/* Get remote hostname if not already done */
		get_remote_info( );

	strcpy( cgip->rhost, "REMOTE_HOST=");
	mystrncat( cgip->rhost, remotehost, MAXHOSTNAMELEN);
	putenv( cgip->rhost);

	if ( *(ip->query)) {
		strcpy( cgip->query, "QUERY_STRING=");
		mystrncat( cgip->query, ip->query, MIDLEN);
		putenv( cgip->query);
	}

	strcpy( cgip->serv_protocol, "SERVER_PROTOCOL=");
	switch ( inheadp->protocol) {
	case HTTP0_9:
		strcat(cgip->serv_protocol, "HTTP/0.9");
		break;
	case HTTP1_0:
		strcat( cgip->serv_protocol, "HTTP/1.0");
		break;
	}

	putenv( cgip->serv_protocol);

	if ( *(ip->pathinfo) ) {
		strcpy( cgip->pathinfo, "PATH_INFO=");
		mystrncat( cgip->pathinfo, ip->pathinfo, MIDLEN);
		putenv( cgip->pathinfo);
	}

	strcpy( cgip->tpath, "PATH_TRANSLATED=");
	strcat( cgip->tpath, ip->rootdir);
	mystrncat( cgip->tpath,	ip->pathinfo, MIDLEN);
	putenv( cgip->tpath);

	strcpy( cgip->scrname, "SCRIPT_NAME=");
	cp = ip->filepath + strlen( ip->rootdir);
	mystrncat( cgip->scrname, cp, MIDLEN);
	putenv( cgip->scrname);

	strcpy( cgip->lochost, "SERVER_NAME=");
	strcat( cgip->lochost, hostname);
	putenv( cgip->lochost);

	sprintf( cgip->scheme,"URL_SCHEME=%s", this_conp->scheme);
	putenv( cgip->scheme);

	sprintf( cgip->lport,"SERVER_PORT=%d", port);
	putenv( cgip->lport);

	putenv("GATEWAY_INTERFACE=CGI/1.1");
	
	if ( *(inheadp->content) ) {
		strcpy( cgip->content, "CONTENT_TYPE=");
		mystrncat( cgip->content, inheadp->content, SMALLLEN);
		putenv( cgip->content);
	}

	if ( *(inheadp->length) ) {
		strcpy( cgip->length, "CONTENT_LENGTH=");
		mystrncat( cgip->length, inheadp->length, SMALLLEN);
		putenv( cgip->length);
	}

	strcpy( cgip->servsoft, "SERVER_SOFTWARE=");
	strcat( cgip->servsoft, VERSION);
	putenv( cgip->servsoft);

	if ( *(inheadp->accept) ) {
		strcpy( cgip->http_accept, "HTTP_ACCEPT=");
		mystrncat( cgip->http_accept, inheadp->accept, ACCEPTLEN);
		putenv( cgip->http_accept);
	}

	if ( *(inheadp->charset) ) {
		strcpy( cgip->http_charset, "HTTP_ACCEPT_CHARSET=");
		mystrncat( cgip->http_charset, inheadp->charset, ACCEPTLEN/4);
		putenv( cgip->http_charset);
	}

	if ( *(inheadp->lang) ) {
		strcpy( cgip->http_lang, "HTTP_ACCEPT_LANGUAGE=");
		mystrncat( cgip->http_lang, inheadp->lang, ACCEPTLEN/4);
		putenv( cgip->http_lang);
	}

	if ( *(inheadp->cookie) ) {
		strcpy( cgip->http_cookie, "HTTP_COOKIE=");
		mystrncat( cgip->http_cookie, inheadp->cookie, ACCEPTLEN);
		putenv( cgip->http_cookie);
	}

	if ( *(inheadp->range) ) {
		strcpy( cgip->range, "HTTP_RANGE=");
		mystrncat( cgip->range, inheadp->range, MIDLEN - TINYLEN);
		putenv( cgip->range);
	}

	if ( *(this_conp->rfc931name)) {
		strcpy( cgip->rident, "REMOTE_IDENT=");
		mystrncat( cgip->rident, this_conp->rfc931name, SMALLLEN);
		putenv( cgip->rident);
	}

	if ( *(inheadp->referrer) ) {
		strcpy( cgip->http_referrer, "HTTP_REFERER=");
		mystrncat( cgip->http_referrer, inheadp->referrer, MIDLEN);
		putenv( cgip->http_referrer);
	}

	if ( *(inheadp->ua) ) {
		strcpy( cgip->http_ua, "HTTP_USER_AGENT=");
		mystrncat( cgip->http_ua, inheadp->ua, SMALLLEN);
		putenv( cgip->http_ua);
	}

	if ( *(inheadp->from) ) {
		strcpy( cgip->http_from, "HTTP_FROM=");
		mystrncat( cgip->http_from, inheadp->from, SMALLLEN);
		putenv( cgip->http_from);
	}

	if ( *(inheadp->host_head) ) {
		strcpy( cgip->http_myhost, "HTTP_HOST=");
		mystrncat( cgip->http_myhost, inheadp->host_head, SMALLLEN);
		putenv( cgip->http_myhost);
	}

	ip->status |= WN_CGI_SET;  /* mark environment as setup */
}


static void
cgi_headers( fp, cgibuf, location, bufptr, buflen)
FILE	*fp;
char	*cgibuf,
	*location,
	**bufptr;

int	*buflen;
{

	register char	*beginl,
			*endl,
			*cp;

	int		fd,
			n,
			m;

	fd = fileno( fp);
	while ( (n = read( fd, cgibuf, OUT_BUFFSIZE )) <= 0 ) {
		if ( (n == -1) && (errno == EINTR))
			continue;
		senderr( SERV_ERR, ERRMSG76, "");
		pclose( fp);
		wn_exit( 2);
	}

	beginl = cgibuf;
	cgibuf[n] = '\0';

	while ( TRUE) {
		if ( ( endl = strchr( beginl, '\n')) == NULL) {
			/* not all headers have been read; read more */
			n -= (beginl - cgibuf);
			mymemcpy( cgibuf, beginl, n);
			while ( (m = read( fd, cgibuf + n, OUT_BUFFSIZE - n))
								<= 0 ) {
				if ( (m == -1) && (errno == EINTR))
					continue;
				senderr( SERV_ERR, ERRMSG104, "");
				pclose( fp);
				wn_exit( 2);
			}

			n += m;
			cgibuf[n] = '\0';
			beginl = cgibuf;
			continue;
		}
		if (endl > cgibuf && endl[-1] == '\r')
			endl[-1] = '\0';
		*endl++ = '\0';
		if ( *beginl == '\0' ) { 	/* blank line: end of header */
			beginl = endl;
			n -= (beginl - cgibuf);
			break;
		}

		if ( strncasecmp( "Content-type:", beginl, 13) == 0) {
			cp = beginl + 13;
			while ( isspace( *cp))
				cp++;
			mystrncpy( cgi_content_type, cp, SMALLLEN);
			this_rp->content_type = cgi_content_type;
		}
		else if ( (strncasecmp("Location:", beginl, 9) == 0)) {
			cp = beginl + 9;
			while ( isspace( *cp))
				cp++;
			mystrncpy( location, cp, MIDLEN);
		}
		else if ( (strncasecmp("Status:", beginl, 7) == 0)) {
			cp = beginl + 7;
			while ( isspace( *cp))
				cp++;
			mystrncpy( outheadp->status, cp, SMALLLEN);
		}
		else if ( (strncasecmp("Expires:", beginl, 8) == 0)) {
			mystrncpy( outheadp->expires, cp, SMALLLEN - 2);
			strcat( outheadp->expires, "\r\n");
		}
		else {
			if (strlen(beginl) + strlen(outheadp->list) < BIGLEN) {
				strcat( outheadp->list, beginl);
				strcat( outheadp->list, "\r\n");
			}
			else {
				senderr( SERV_ERR, ERRMSG56, "");
				pclose( fp);
				wn_exit( 2);
			}

		}
		beginl = endl;
	}
	*buflen = n;
	*bufptr = endl;
}
