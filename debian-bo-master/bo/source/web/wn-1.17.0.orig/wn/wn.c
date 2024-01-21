/*
    Wn: A Server for the HTTP
    File: wn/wn.c
    Version 1.17.0
    
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
#include <string.h>
#include <ctype.h>
#include <sys/types.h>

#ifdef AIX
#include <sys/select.h>
#endif

#include <sys/signal.h>
#include <sys/socket.h>
#ifndef NO_UNISTD_H
#include <unistd.h>
#endif
#include "wn.h"
#include "version.h"
#include "cgi.h"

#ifndef USE_WN_READ
#define WN_read(a,b,c)		read(a,b,c)
#endif

#define MAX_POST		(2048 * 1024)
#define KEEP_ALIVE_TIMEOUT	(20)
#define NETSCAPE2_BUG
#define AUTHENT_TIMEOUT		(30)
#define KEEP_ALIVE_MAX		(10)

#define AWAIT_REQUEST		(1)
#define AWAIT_HEADER		(2)
#define AWAIT_TRANSACTION	(3)

#ifndef STANDALONE
#include <sys/stat.h>
#endif

extern int	daemon_init();
extern long	atol();
extern void	parse_request(),
		write_debug();

extern time_t	time();

static void	do_post(),
		reset_buf(),
		get_header(),
		wn_timeout(),
		client_closed();

static Methodtype parse_header( );
static char	*get_input();

static int	chk_continue(),
		load_inbuf(),
		wn_getc(),
		await_state,
		await_timeout = KEEP_ALIVE_TIMEOUT;

Request		*this_rp = NULL;

Inheader	*inheadp = NULL;
Outheader	*outheadp = NULL;
Dir_info	*dir_p = NULL;
Connection	*this_conp = NULL;

int		port = 0;

main( argc, argv)
int	argc;
char	*argv[];

{
	wn_init( argc, argv);

#ifdef	STANDALONE
	daemon_init();

	signal( SIGQUIT, SIG_IGN);
	signal( SIGINT, SIG_IGN);

	do_standalone();
#else
	get_local_info( fileno( stdin));
	do_connection();
#endif
	return 	( 0);
}


void
do_connection()
{
	char		request[BIGLEN];
	Request		thisreq;
	Inheader	inheader;
	Outheader	outheader;
	Connection	thiscon;
	Dir_info	thisdir;

	Inbuffer	readbuf;

	char		buf[20];

	thiscon.keepalive = FALSE;
	thiscon.trans_cnt = 0;
	thiscon.authuser[0] = thiscon.rfc931name[0]
		= thiscon.logbuf[0] = '\0';
	thiscon.pid = getpid();
	readbuf.bcp = NULL;
	readbuf.cur_sz = 0;
	thiscon.bufp = &readbuf;
	this_rp = &thisreq;
	inheadp = &inheader;
	outheadp = &outheader;
	this_conp = &thiscon;
	this_conp->log_ptr = this_conp->logbuf;
	this_conp->out_ptr = this_conp->outbuf;
	this_conp->scheme = "http";
	


	 /* Start a debug entry if debugging enabled. */

	if ( debug_log) {
		write_debug( 0, "\nRequest starting: ", VERSION);
	}

	get_remote_ip( );
	signal( SIGALRM, wn_timeout );
	signal( SIGPIPE, client_closed );

	dir_p = &thisdir;


	WN_EXTRA_SETUP

	while ( this_conp->trans_cnt < KEEP_ALIVE_MAX) {
		alarm( await_timeout);
		this_conp->trans_cnt++;
		await_state = AWAIT_REQUEST;
		request[0] = '\0';
		clear_req( );
		bzero( (char *) inheadp, sizeof( Inheader));
		bzero( (char *) outheadp, sizeof( Outheader));

		if ( get_input( request, TRUE) == NULL )
			client_closed(); /* no one there? */

		if ( !*request) {
			/* extra CRLF at end of previous request ? */
			if (this_conp->keepalive == TRUE)
				continue;
			else
				break;
		}

		await_state = AWAIT_HEADER;

		strcpy( thisreq.request, request);

		if ( parse_header( request) == UNKNOWN ) {
			wn_exit( 0);
		}
		await_state = AWAIT_TRANSACTION;
		await_timeout = KEEP_ALIVE_TIMEOUT;
		/* await_timeout may have been set for authentication */
		/* reset it here */
		alarm( TRANSACTION_TIMEOUT);

#ifdef NO_KEEPALIVE
		this_conp->keepalive = FALSE;
#else
#ifdef NETSCAPE2_BUG
		if ( this_conp->keepalive &&
			(strncmp( inheadp->ua, "Mozilla/2.0", 11) == 0)) {
			this_conp->keepalive = FALSE;
		}
#endif
#endif
		if ( debug_log && this_conp->keepalive) {
			sprintf( buf, "pid = %d, count = %d\n",	this_conp->pid,
				this_conp->trans_cnt);
			write_debug( 1, "Keep-Alive: ", buf);
		}

		process_url(&thisreq, inheadp->url_path);
		if ( !(this_conp->keepalive)) {
			break;
			/* if not keepalive then exit */
		}
		flush_outbuf();
	}
	wn_exit(0);
}

static void
wn_timeout( )
{
	char	buf[SMALLLEN];

	signal( SIGALRM, SIG_DFL);
	alarm( 20);  	/* Give ourselves 20 seconds to get remote DNS data
			 * and write log entry.
			 */

	if ( await_state != AWAIT_REQUEST) {
		strcpy( outheadp->status, "503 Timed Out");
		sprintf( buf, "Process %d, await_state (%d) ",
			this_conp->pid, await_state);
		senderr( "503", ERRMSG60, buf);
	}
	wn_exit( 2);
}



static void
client_closed( )
{
	if ( debug_log ) {
		char	buf[SMALLLEN];
		sprintf( buf, "Client closed connection, pid=%d,  wait=%d ",
					this_conp->pid, await_state);
		write_debug( 1, buf, "");
	}
	if ( await_state != AWAIT_REQUEST) {
		*(this_rp->length) = '\0';
		get_remote_info();
		writelog( this_rp, LOGMSG19, (await_state == AWAIT_HEADER 
			? "header" : "transaction"));
	}
	wn_exit( 0);
}

void
process_url( ip, url_path)
Request	*ip;
char	*url_path;
{
	Reqtype	itemtype;

	/* These may need clearing if we came from redirect */
	ip->attributes = ip->filetype = ip->status = 0;

	parse_request( ip, url_path);

	if ( ip->type != RTYPE_FINISHED)
		chk_cntrl( ip);

	itemtype = ip->type;

	if ( (itemtype != RTYPE_DENIED) && (itemtype != RTYPE_NO_AUTH)
				&& (itemtype != RTYPE_FINISHED)) {
		switch (inheadp->method) {
		case HEAD:
			itemtype = RTYPE_HEADER;
			break;
		case CONDITIONAL_GET:
			if ( (ip->attributes & WN_DYNAMIC) 
				|| date_cmp( ip, inheadp->inmod_date, TRUE))
				inheadp->method = GET;
			else
				itemtype = RTYPE_NOT_MODIFIED;
			break;
		case UNLESS_GET:
			inheadp->method = GET;
			if ( date_cmp( ip, inheadp->inmod_date, TRUE)) {
				ip->filetype &= ~(WN_RFC_BYTERANGE
						+ WN_BYTERANGE
						+ WN_LINERANGE);
			}
			break;
		case POST:
		case PUT:
			do_post( inheadp);
			break;
		default:
			break;
		}
	}
	
	switch (itemtype) {
		case RTYPE_FINISHED:
			break;
		case RTYPE_FILE:
			if ( ip->attributes & WN_CGI )
				cgi_env( ip, FALSE);

			if ( ip->filetype & WN_TEXT )
				sendtext( ip);
			else
				sendbin( ip);
			break;
		case RTYPE_MARKLINE:
			if ( ip->attributes & WN_CGI )
				cgi_env( ip, FALSE);

			if ( ip->filetype & WN_TEXT )
				sendtext( ip);
			else
				senderr( DENYSTATUS, ERRMSG47, ip->filepath);
			break;
#ifndef FORBID_CGI
		case RTYPE_CGI:
		case RTYPE_NPH_CGI:
			sendcgi( ip);
			break;
#endif
		case RTYPE_GSEARCH:
		case RTYPE_CONTEXTSEARCH:
		case RTYPE_LINESSEARCH:
			sendgrep( ip);
			break;
		case RTYPE_TSEARCH:
		case RTYPE_KSEARCH:
		case RTYPE_TKSEARCH:
		case RTYPE_FIELDSEARCH:
			cache_search( ip);
			break;
		case RTYPE_ISEARCH:
			if ( ip->attributes & WN_CGI )
				cgi_env( ip, FALSE);
			send_isearch( ip);
			break;
		case RTYPE_LISTSEARCH:
			list_search( ip);
			break;
		case RTYPE_HEADER:
			http_prolog( );
			writelog( ip, LOGMSG6, "");
			break;
		case RTYPE_IMAGEMAP:
			image( );
			break;
		case RTYPE_INFO:
			sendinfo( ip);
			break;
		case RTYPE_REDIRECT:
			sendredirect( ip, "301 Moved Permanently", 
				outheadp->redirect);
			break;
		case RTYPE_NOT_MODIFIED:
			strcpy( outheadp->status, "304 Not Modified");
			http_prolog( );
			strcpy( ip->length, "0");
			writelog( ip, LOGMSG5, "");
			break;
		case RTYPE_NO_AUTH:
			break;
		case RTYPE_NOACCESS:
			if ( *(dir_p->noaccess_url) && 
				*dir_p->noaccess_url &&
				!streq( ip->relpath, dir_p->noaccess_url)) {
				sendredirect( ip, "302 Moved Temporarily", 
					dir_p->noaccess_url);
				break;
			}
		case RTYPE_DENIED:
			if ( ip->status & WN_CANT_STAT &&
				*dir_p->cantstat_url &&
				!streq( ip->relpath, dir_p->cantstat_url)) {
				sendredirect( ip, "302 Moved Temporarily", 
					dir_p->cantstat_url);
				break;
			}
		case RTYPE_UNCHECKED:
		default:
			senderr( DENYSTATUS, DENYMSG, ip->filepath);
	}

	return;

	/*
	 * End of process_url()
	 * We come here after every error free transaction 
	 * or to return above or perhaps exit for some parsed docs.
	 */
}


static Methodtype
parse_header( req)
char	*req;
{
	register char	*cp;
	char		errmsg[BIGLEN],
			method[SMALLLEN];
	Inheader	*ih;

	ih = inheadp;
	cp = req;
	while ( *cp && !isspace( *cp))
		cp++;
	*cp++ = '\0';
	mystrncpy( method, req, SMALLLEN);
	req = cp;
	while (  *req && isspace( *req))
		req++;
	cp = req;
	while (  *cp && !isspace( *cp))
		cp++;
	if ( *cp ) { /* There's more, check HTTP Version */
		*cp++ = '\0';
		while (  *cp && isspace( *cp))
			cp++;
	}
	ih->protocol = ( *cp ? HTTP1_0 : HTTP0_9);

	strcpy( ih->url_path, req); /* both arrays have size BIGLEN */

	if ( streq( method, "GET")) {
		ih->method = GET;
		if ( ih->protocol == HTTP1_0 ) 
			get_header( ih);
		return	(ih->method);

	}
	if ( streq( method, "POST") ) {
		if (serv_perm & WN_FORBID_EXEC) {
			senderr( "501", errmsg, "");
			return	(ih->method = UNKNOWN);
		}
		else {
			get_header( ih);
			return	(ih->method = POST);
		}
	}
	if ( streq( method, "PUT") ) {
		if (serv_perm & WN_FORBID_EXEC) {
			senderr( "501", errmsg, "");
			return	(ih->method = UNKNOWN);
		}
		else {
			get_header( ih);
			return	(ih->method = PUT);
		}
	}
	if ( streq( method, "HEAD") ) {
		get_header( ih);
		return	(ih->method = HEAD);
	}
	else {
		strcpy( errmsg, method);
		strcat( errmsg, ": Method not implemented");
		senderr( "501", errmsg, "");
		return	(ih->method = UNKNOWN);
	}
}



static void
get_header( ih)
Inheader	*ih;
{
	register char	*cp, *cp2;
	char	headerline[BIGLEN];
	int	acceptlen,
		cookielen;

	acceptlen = cookielen = 0;
	this_conp->keepalive = FALSE;

	while ( get_input( headerline, FALSE) != NULL) {
		if ( !*headerline)	/* Blank line, end of header */
			return;

		if ( strncasecmp( headerline, "Accept:", 7) == 0 ) {
			cp = headerline + 7;
			while ( isspace( *cp))
				cp++;
			if ( acceptlen > 0 )
				strcat( ih->accept, ", ");
			acceptlen += strlen( cp);
			if ( acceptlen > ACCEPTLEN)
				logerr( ERRMSG19, "");
			else {
				strcat( ih->accept, cp);
			}
			continue;
		}

		if ( strncasecmp( headerline, "Accept-Language:", 16) == 0 ) {
			cp = headerline + 16;
			while ( isspace( *cp))
				cp++;
			if ( strlen( cp) > ACCEPTLEN/4 )
				logerr( ERRMSG19, "");
			else {
				strcpy( ih->lang, cp);
			}
			continue;
		}

		if ( strncasecmp( headerline, "Accept-Charset:", 15) == 0 ) {
			cp = headerline + 15;
			while ( isspace( *cp))
				cp++;
			if ( strlen( cp) > ACCEPTLEN/4 )
				logerr( ERRMSG19, "");
			else {
				strcpy( ih->charset, cp);
			}
			continue;
		}

		if ( strncasecmp( headerline, "Cookie:", 7) == 0 ) {
			cp = headerline + 7;
			while ( isspace( *cp))
				cp++;
			if ( cookielen > 0 )
				strcat( ih->cookie, "; ");
			cookielen += strlen( cp);
			if ( cookielen > ACCEPTLEN )
				logerr( ERRMSG79, "");
			else {
				strcat( ih->cookie, cp);
			}
			continue;
		}

		if ( strncasecmp( headerline, "Authorization:", 14) == 0 ) {
			/* For digest use the URI from URI header */
			if ( ( cp = strstr( headerline, "uri=\"")) ||
					(cp = strstr( headerline, "URI=\""))) {
				cp2 = ih->url_path;
				cp += 5;
				while( *cp && ( *cp != '"'))
					*cp2++ = *cp++;
				*cp2 = '\0';
			}
			cp = headerline + 14;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->authorization, cp, MIDLEN);
			continue;
		}

		if ( strncasecmp( headerline, "Content-type:", 13) == 0 ) {
			cp = headerline + 13;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->content, cp, SMALLLEN);
			continue;
		}
		if ( strncasecmp( headerline, "Content-length:", 15) == 0 ) {
			cp = headerline + 15;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->length, cp, SMALLLEN);
			continue;
		}
		if ( strncasecmp( headerline, "Content-encoding:", 17) == 0 ) {
			cp =  headerline + 17;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->encoding, cp, SMALLLEN);
			continue;
		}
		if ( strncasecmp( headerline, "Host:", 5) == 0 ) {
			cp = headerline + 5;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->host_head,  cp, SMALLLEN);
			continue;
		}
		if ( strncasecmp( headerline, "Referer:", 8) == 0 ) {
			cp = headerline + 8;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->referrer,  cp, MIDLEN);
			continue;
		}
		if ( strncasecmp( headerline, 
				 "Connection: Keep-Alive", 22) == 0 ) {
			this_conp->keepalive = TRUE;
			continue;
		}
		if ( strncasecmp( headerline, "from:", 5) == 0 ) {
			cp = headerline + 5;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->from,  cp, SMALLLEN);
			continue;
		}

		if ( strncasecmp( headerline, "Range:", 6) == 0 ) {
			cp = headerline + 6;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->range,  cp, SMALLLEN);
			continue;
		}

		if ( strncasecmp( headerline, "User-Agent:", 11) == 0 ) {
			cp = headerline + 11;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->ua, cp, SMALLLEN); 
			continue;
		}
		if ( strncasecmp( headerline, "If-Modified-Since:", 18) == 0) {
			if ( ih->method == POST)
				continue;
			cp = headerline + 18;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->inmod_date, cp, SMALLLEN);
			ih->method = CONDITIONAL_GET;
			continue;
		}
		if ( strncasecmp( headerline, "If-Unmodified-Since:", 22)
									== 0) {
			if ( (ih->method == POST)
					|| (ih->method == UNLESS_GET))
				continue;
			cp = headerline + 22;
			while ( isspace( *cp))
				cp++;
			mystrncpy( ih->inmod_date, cp, SMALLLEN);
			ih->method = UNLESS_GET;
			continue;
		}
	}
}


static void
do_post( ih)
Inheader	*ih;
{
	long	len;
	int	c;
	char	tfile[SMALLLEN];
	FILE	*fp;

#ifndef STANDALONE
	umask( 077);
#endif
	strcpy( tfile, TEMPDIR);
	strcat( tfile, "/WNpostXXXXXX");
	mktemp( tfile);
	strcpy( ih->tmpfile, tfile);

	if ( (fp = fopen( tfile, "w")) == (FILE *) NULL ) {
		senderr( SERV_ERR, ERRMSG53, tfile);
		wn_exit( 2);
	}
	len = atol( ih->length);

	if ( (len > MAX_POST) || (len < 0)) {
		senderr( "403", ERRMSG74, tfile);
		wn_exit( 0);
	}

	/* Timeout after TRANSACTION_TIMEOUT seconds */
	alarm( TRANSACTION_TIMEOUT);

	while ( len && (c = wn_getc( )) != EOF) {
		len--;
		putc( c, fp);
	}
	fclose( fp);

}


static int
wn_getc()
{
	int		n,
			c;

	Inbuffer	*bp;

	bp = this_conp->bufp;

	if ( bp->bcp >= bp->buffer + bp->cur_sz) {
		n = load_inbuf( bp);
		if ( n <= 0 )
			return EOF;
	}
	c = (int) *(bp->bcp);
	(bp->bcp)++;
	return c;
}
/*
 * static char *get_input()
 * Returns NULL if no input (client quit), otherwise places next line
 * of input in "line".  Any CRLF are removed.  Thus *line == '\0' indicates
 * end of headers.  This function reads lines from a buffer thiscon.bufp.
 * If the buffer is empty or does not contain a complete line it reloads
 * it.  It looks ahead to see if the line continues (i.e. next line starts
 * with white space.  The lookahead is only done for header lines
 * (rline = FALSE) not for the request line.
 */

static char *
get_input( line, is_rline)
char	*line;
int	is_rline; 
{
	int	llen = 0,
		n = 0;
	Inbuffer	*bp;

	register char	*cp;

	bp = this_conp->bufp;
	if ( bp->bcp == NULL)
		bp->bcp = bp->buffer;

	*line = '\0';
	while ( TRUE) {
		if ( bp->cur_sz <= 0 ) {  /* fill the buffer */
			n = load_inbuf( bp);
			if ( n <= 0) {
				if ( debug_log) {
					char	tmpbuf[SMALLLEN];
					sprintf( tmpbuf,
					"PID %d got null or error read (%d) ",
						this_conp->pid, n);
					write_debug(1, tmpbuf, "");
				}
				return NULL;
			}
		}

		if ( ( cp = strchr( bp->bcp, '\n')) == NULL) {
			/* an incomplete header has been read */
			/* put what there is into line */
			bp->buffer[bp->cur_sz] = '\0';
			mystrncat( line, bp->bcp, BIGLEN - llen);
			llen = strlen( line);
			bp->cur_sz = 0;
			bp->bcp = bp->buffer;
			bp->buffer[0] ='\0';
			/* then read more */

			n = load_inbuf( bp);
			if ( n <= 0 ) {
				writelog( this_rp,  ERRMSG66, line);
				wn_exit( 2);
			}
			continue;
		}
		
		/* cp now points to next NL in the in_bufer */
		cp--;
		if ( ( cp >= bp->buffer) && *cp == '\r')
			*cp = '\0';
		cp++;
		*cp++ = '\0';

		mystrncpy( line + llen, bp->bcp, BIGLEN - llen);
		llen = strlen( line);
		bp->bcp = cp;

		reset_buf (bp);

		if ( debug_log)
			write_debug(1, " -> ", line);

		if ( !*line || is_rline)
			return ( line);
			/* Don't allow continuation for request line */


		if ( ! chk_continue( bp) ) 
			break; /* We're done with this line */
			/* otherwise its a continuation line */
	}
	return ( line);
}



/*
 * static int load_inbuf( bp)
 * Load and adjust input buffer.
 */

static int
load_inbuf( bp)
Inbuffer	*bp;
{
		char	*base;
		int	n = 0,
			size;

		register char	*cp;

		size = bp->cur_sz;
		base = &(bp->buffer)[0];
		cp = bp->bcp;

		if (cp > base) {
			size = ( base + size - cp);
			size = ( size < 0 ? 0 : size);
			mymemcpy( base, cp, size);
			bp->bcp = cp = base;
		}
		n = WN_read( (fileno( stdin)), (base + size), 
				((INBUFFSIZE - 4) - size));
		if ( n > 0 ) {
			size += n;
			bp->cur_sz = size;
			*(base + size) = '\0';
		}
		else if ( size <= 0) {
			bp->cur_sz = 0;
			bp->bcp = bp->buffer;
			*base ='\0';
		}
		return n;
}

	
/*
 * static int chk_continue( bp)
 * Check if input line continues (next line starts with whitespace .
 * If buffer is empty, reinitialize it.
 */

static int
chk_continue( bp)
Inbuffer	*bp;
{
	char	c;
	int	n;

	if ( bp->cur_sz == 0) {
		n = load_inbuf( bp);
		if ( n <= 0) {
			return FALSE;
		}
	}	
	c = *(bp->bcp);
	if ( ( c != ' ') && (c != '\t'))
		return FALSE;
	else
		return TRUE;
}


/*
 * static void reset_buf( bp)
 * Check if buffer is empty and reset it.
 */

static void
reset_buf( bp)
Inbuffer	*bp;
{

	if ( bp->bcp - bp->buffer < bp->cur_sz)
		return;  /* it's not empty yet */
	else {
		bp->bcp = bp->buffer;
		*(bp->bcp) = '\0';
		bp->cur_sz = 0;
	}
}


void
clear_req( )
{
	bzero( (char *) this_rp, sizeof( Request));
	this_rp->do_wrap_1st_time = TRUE;
}

/*
 * http_prolog() sends the HTTP headers (or does nothing for HTTP/0.9)
 * If it has already been called for this request it returns FALSE
 * and does nothing.  If it has not been called it returns TRUE after
 * writing appropriate headers to stdout.
 */

int
http_prolog( )
{

	struct tm	*gmt;
	time_t		clock,
			clock2;
	Request		*ip;
	char		buf[CACHELINE_LEN],
			datebuf[2*TINYLEN];
	unsigned	unbuffered;

	ip = this_rp;

	if ( ip->status & WN_PROLOGSENT )
		return FALSE;
	ip->status |= WN_PROLOGSENT;
	unbuffered = ip->attributes & WN_UNBUFFERED;
	ip->attributes &= ~(WN_UNBUFFERED);  /* always buffer HTTP headers */

	if ( inheadp->protocol ==  HTTP1_0) {
		if ( *outheadp->status)
			sprintf( buf, "%s %s\r\n",
					HTTPVERSION, outheadp->status);
		else {
			strcpy( buf, HTTPVERSION);
			strcat( buf, " 200 OK\r\n");
		}
		send_text_line( buf);
		sprintf( buf, "Server: %s\r\n", VERSION);
		send_text_line( buf);

		/* Find date and serve the HTTP Date header */
		time(&clock);
		gmt = gmtime(&clock);
		strftime( datebuf, SMALLLEN, 
				"Date: %a, %d %h %Y %T GMT\r\n", gmt);
		send_text_line( datebuf);

		if ( strncmp( outheadp->status, "204", 3) == 0 ) {
			send_text_line( "Content-length: 0\r\n\r\n");
			return TRUE;
		}

		if ( strncmp( outheadp->status, "401", 3) == 0 ) {
			send_text_line( outheadp->list);
			if ( this_conp->keepalive)
				send_text_line( "Connection: Keep-Alive\r\n");
			await_timeout = AUTHENT_TIMEOUT;
			sprintf( buf, "Content-length: %s\r\n\r\n",
						ip->length);
			send_text_line( buf);
			return TRUE;
		}

		if ( *outheadp->redirect) {
			send_text_line( "Content-type: text/html\r\n");
			if ( this_conp->keepalive)
				send_text_line( "Connection: Keep-Alive\r\n");
			send_text_line( "Location: ");
			send_text_line( outheadp->redirect);
			send_text_line( "\r\nContent-length: 0\r\n\r\n");
			return TRUE;
		}

		if ( ip->attributes & (WN_DYNAMIC + WN_NOCACHE)) {
			ip->mod_time = (time_t) 0;
			*ip->mod_date = '\0';
			*ip->length = '\0';
		}

			
		if ( ip->mod_time) {
			gmt = gmtime(&ip->mod_time);
			strftime( ip->mod_date, SMALLLEN,
				"Last-modified: %a, %d %h %Y %T GMT\r\n", gmt);
		}

		if ( *ip->mod_date) {
			send_text_line( ip->mod_date);
		}

		if ( ip->maxage && *ip->maxage) {
			long	delta;

			if ( *ip->maxage == 'L') {
				ip->maxage++;
				clock2 = ip->mod_time + atol( ip->maxage);
				delta = clock2 - clock;

				if ( delta > 0) {
					sprintf( buf, 
					"Cache-Control: max-age=%ld\r\n",
								delta);
					send_text_line( buf);
				}
			}
			else {
				clock2 = clock + atol( ip->maxage);
				sprintf( buf, "Cache-Control: max-age=%s\r\n",
						ip->maxage);
				send_text_line( buf);
			}
		}

		if ( ip->expires && *ip->expires) {
			sprintf( buf, "Expires: %s\r\n", ip->expires);
			send_text_line( buf);
		}
		else if ( *outheadp->expires) {
			send_text_line( outheadp->expires);
		}
		else if ( ip->maxage && *ip->maxage) {
			gmt = gmtime(&clock2);
			strftime( datebuf, SMALLLEN, 
				"Expires: %a, %d %h %Y %T GMT\r\n", gmt);
			send_text_line( datebuf);
		}

		if ( ip->maxage && *ip->maxage) {
		}

		if ( inheadp->method == CONDITIONAL_GET) {
			/* It's not modified */
			if ( this_conp->keepalive)
				send_text_line( "Connection: Keep-Alive\r\n");
			send_text_line( "\r\n");
			return TRUE;
		}

		if ( ip->content_type) {
			sprintf(buf, "Content-type: %s\r\n", ip->content_type);
			send_text_line( buf);
		}

		if ( *ip->length && !(ip->attributes 
				& (WN_PARSE + WN_FILTERED + WN_DYNAMIC))) {
			sprintf( buf, "Content-length: %s\r\n", ip->length);
			send_text_line( buf);

			if ( *outheadp->range )
				send_text_line( outheadp->range);

			if ( !(ip->filetype & WN_BYTERANGE) &&
					!*(outheadp->status)) {
				/* !*(outheadp->status)) means status 200 */
				send_text_line( "Accept-Ranges: bytes\r\n"); 
			}
		}
		else if ( inheadp->method != HEAD)
			this_conp->keepalive = FALSE;

		if ( this_conp->keepalive) {
			send_text_line( "Connection: Keep-Alive\r\n");
		}
				
		if ( ip->encoding && *ip->encoding) {
			sprintf( buf, "Content-encoding: %s\r\n",
				ip->encoding);
			send_text_line( buf);
		}

		if ( ip->title && *ip->title) {
			sprintf( buf, "Title: %s\r\n", ip->title);
			send_text_line( buf);
		}

		if ( ip->keywords && *ip->keywords) {
			sprintf( buf, "Keywords: %s\r\n", ip->keywords);
			send_text_line( buf);
		}

		if ( dir_p->dir_owner && *dir_p->dir_owner) {
			sprintf( buf,"Link: <%s>; rev=\"Made\"\r\n",
				dir_p->dir_owner);
			send_text_line( buf);
		}

		if ( *outheadp->list ) {
			send_text_line( outheadp->list);
		}
		send_text_line( "\r\n");
	}
	ip->attributes |= unbuffered;
	return TRUE;
}

