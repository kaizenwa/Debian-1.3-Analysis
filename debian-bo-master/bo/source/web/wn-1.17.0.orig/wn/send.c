/*
    Wn: A Server for the HTTP
    File: wn/send.c
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
#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <errno.h>

#ifndef NO_UNISTD_H
#include <unistd.h>
#endif

#include "wn.h"
#include "version.h"
#include "err.h"
#include "parse.h"
#include "reg.h"

#ifndef USE_WN_WRITE
#define WN_write(a,b,c)		write(a,b,c)
#endif

#define BYTECHUNK	(256*1024)
#define MAX_REDIRECT	(10)


static void	filter_open(),
		send_byterange(),
		sendsubrange();

static char	*enter_range();

extern long	atol();


void
senderr( status, msg, file)
char	*status,
	*msg,
	*file;
{
	char	buf[MIDLEN];

	get_remote_info( );

	sprintf( outheadp->status, "%s %s", status, msg);
	strcpy( outheadp->list, "Content-type: text/html\r\n");

	if ( streq( status, SERV_ERR))
		logerr( msg, file);
	else
		writelog( this_rp, msg, file);

	clear_req( );
	set_interface_root( );

	sprintf( buf,
		"<html>\n<head>\n<title>%s %s</title>\n</head>\n<body>\n", 
				status, msg);
	sprintf( buf + strlen( buf), 
			"<h2>Error code %s</h2>\n%s\n", status, msg);
	sprintf( buf + strlen( buf),
		"\n<hr>\n<address>%s</address>\n", VERSION);
	sprintf( buf + strlen(buf), "\n</body>\n</html>\n");
	sprintf( this_rp->length, "%d", strlen(buf));

	if ( http_prolog( ) ) {
		send_text_line( buf);
	}
}


void
sendinfo( ip)
Request	*ip;

{
	char	*cp,
		owner[MIDLEN],
		len[TINYLEN],
		con[SMALLLEN],
		enc[SMALLLEN],
		buf[2*BIGLEN];

	int	i;

	struct tm *gmt;
	
	
	if ( *ip->length) {
		mystrncpy( len, ip->length, TINYLEN);
		*ip->length = '\0';
	}
	mystrncpy( enc, ip->encoding, SMALLLEN);
	*ip->encoding = '\0';

	mystrncpy( con, ip->content_type, SMALLLEN);
	ip->content_type = "text/html";
	http_prolog( );

	sprintf(buf, "<html>\n<head>\n<title>URL information </title>\n");
	send_text_line( buf);
	cp = ( *dir_p->dir_owner ? dir_p->dir_owner : MAINTAINER);
	sprintf( owner, "<link rev=\"made\" href=\"%s\">\n", cp);
	sprintf(buf,"%s</head>\n<body>\n<h2>URL information</h2>\n", owner);
	send_text_line( buf);

	sprintf(buf, "<dl>\n<dt><b>Title:</b>\n<dd>%s\n", ip->title);
	send_text_line( buf);

	sprintf(buf, "<dt><b> Filename:</b>\n<dd>%s\n", ip->basename);
	send_text_line( buf);

	if ( *ip->keywords) {
		sprintf(buf, "<dt><b>Keywords:</b>\n<dd>%s\n", ip->keywords);
		send_text_line( buf);
	}
	for ( i = 0; i < NUMFIELDS; i++) {
		if ( *(ip->field[i])) {
			sprintf(buf, "<dt><b>User field %d:</b>\n<dd>%s\n",
				i, ip->field[i]);
			send_text_line( buf);
		}
	}
	if ( ip->expires && *ip->expires) {
		sprintf(buf, "<dt><b>Expires:</b>\n<dd>%s\n", ip->expires);
		send_text_line( buf);
	}

	if ( *len ) {
		sprintf(buf, "<dt><b>Size:</b>\n<dd>%s\n", len);
		send_text_line( buf);
	}
	sprintf(buf, "<dt><b>Content-type:</b>\n<dd>%s\n", con);
	send_text_line( buf);

	if ( *enc) {
		sprintf(buf, "<dt><b>Content-encoding:</b>\n<dd>%s\n", enc);
		send_text_line( buf);
	}

	if ( ip->mod_time) {
		gmt = gmtime(&ip->mod_time);
		strftime( buf, SMALLLEN,
		"<dt><b>Last-modified:</b>\n<dd> %a, %d %h %Y %T GMT\n", gmt);
		send_text_line( buf);
	}

	sprintf(buf, "<dt><b>Maintainer:</b>\n<dd>%s\n", cp);
	send_text_line( buf);

	sprintf(buf, "</dl>\n<hr>\n<address>%s</address>\n", VERSION);
	send_text_line( buf);

	send_text_line( "\n</body>\n</html>\n");

	writelog( ip, LOGMSG14, ip->relpath);
}

/*
 * void file_open( ip)
 * Call check_perm() to check permissions then open a file to be served, 
 * store the FILE pointer in ip->fp.  If the string dir_p->filemod is
 * non-empty then use it as a data base module to to produce the data.
 * The data base module gets its key (which is ip->basename) from the
 * environment variable WN_KEY.
 *
 */

void
file_open( ip)
Request *ip;
{
	char	envkey[2*SMALLLEN];


	if ( !*dir_p->filemod) {
		check_perm( ip, ip->filepath);
		if ( (ip->fp = fopen( ip->filepath, "r")) == (FILE *) NULL ) {
			senderr( DENYSTATUS, ERRMSG1, ip->filepath);
			wn_exit( 2);
		}
		ip->fptype = FRP_FILE;
		return;
	}	
	else {
		strcpy( envkey, "WN_KEY=");
		mystrncpy( envkey + strlen("WN_KEY="), ip->basename, SMALLLEN);
		putenv( envkey);

		if ((ip->fp = popen( dir_p->filemod, "r"))
					== (FILE *) NULL ) {
			senderr( SERV_ERR, ERRMSG39, dir_p->filemod);
			wn_exit( 2);
		}
		ip->fptype = FRP_PIPE;
	}
}

/*
 * void filter_open( ip)
 * Like file_open above, but additionally pipes the output of the 
 * file or data base module to the filter in ip->filter.  The FILE
 * pointer for the output from the filter is put in ip->fp.
 *
 */

static void
filter_open( ip)
Request *ip;
{
	char	commandbuf[2*MIDLEN],
		buf[MIDLEN],
		envkey[2*SMALLLEN];

	exec_ok( ip);
	check_perm( ip, ip->filter);
	getfpath( buf, ip->filter, ip);
	if ( !*dir_p->filemod) {
		check_perm( ip, ip->filepath);
		strcpy( commandbuf, buf);
		strcat( commandbuf, " < ");
		strcat( commandbuf, ip->filepath);
	}
	else {
		strcpy( envkey, "WN_KEY=");
		mystrncpy( envkey + strlen("WN_KEY="), ip->basename, SMALLLEN);
		putenv( envkey);
		sprintf( commandbuf, "%s %s | %s",
			dir_p->filemod, ip->basename, buf);
	}
	if ( (ip->fp = popen( commandbuf, "r")) == (FILE *) NULL ) {
		senderr( SERV_ERR, ERRMSG52, commandbuf);
		wn_exit( 2);
	}
	ip->fptype = FRP_PIPE;
}


/*
 * sendbin( ip)  Send a binary file.
 */

				
void
sendbin(  ip)
Request	*ip;

{
	if ( ip->filetype & WN_LINERANGE) {
		senderr( DENYSTATUS, ERRMSG54, ip->filepath);
		wn_exit( 2);
	}
	if ( ip->attributes & WN_FILTERED )
		filter_open( ip);
	else
		file_open( ip);

	if ( ip->filetype & (WN_BYTERANGE + WN_RFC_BYTERANGE)) {
		if ( !(ip->filetype & WN_RFC_BYTERANGE))
			ip->content_type = "application/octet-stream";
		send_byterange();
	}
	else {
		http_prolog( );
		send_out_fd( fileno( ip->fp));
	}

	writelog( ip, LOGMSG13, ip->relpath);

	if ( ip->fptype == FRP_PIPE)
		pclose( ip->fp);
	else
		fclose( ip->fp);
}

/*
 * sendtext( ip)  Send a text file.
 */

void
sendtext(  ip)
Request	*ip;
{

	static int	dontlog = 0;

	char	buf[OUT_BUFFSIZE];

	int	n;


	if ( ip->attributes & WN_FILTERED )
		filter_open( ip);
	else
		file_open( ip);

	if ( ip->filetype & (WN_BYTERANGE + WN_RFC_BYTERANGE + WN_LINERANGE)) {
		if ( ip->attributes & (WN_PARSE + WN_DYNAMIC + WN_FILTERED) ) {
			senderr( DENYSTATUS, ERRMSG94, ip->filepath);
			wn_exit( 2);
		}
		else if ( !(ip->filetype & WN_RFC_BYTERANGE))
			ip->content_type = "text/plain";
	}


	if ( ip->attributes & WN_PARSE ) { 
		dontlog++;
		/* Don't do http_prolog() until later */
		do_wrap( ip, SHOW_IT);
		dontlog--;
		if ( !dontlog)
			writelog( ip, LOGMSG13, ip->relpath);
		return;
	}
	else {
		if ( ip->filetype & (WN_BYTERANGE + WN_RFC_BYTERANGE)) {
			send_byterange();
		}
		else if ( ip->filetype & WN_LINERANGE) {
			long	startline,
				endline;
			char	*cp;

			cp = ip->range;
			enter_range( cp, &startline, &endline);
			*ip->length ='\0';
			http_prolog( );
			if ( endline == -1 )
				endline = ip->datalen;
			if ( startline == -1 )
				logerr( ERRMSG93, "");
			for ( n = 1; n <= endline; n++) {
				if ( !fgets( buf, BIGLEN, ip->fp))
					break;
				if ( n >= startline)
					send_text_line( buf);
			}
		}
		else if ( ip->type == RTYPE_MARKLINE ) {
			http_prolog( );
			send_markline_doc( ip, SHOW_IT);
		}
		else {
			http_prolog( );
			send_out_fd( fileno( ip->fp));
		}
	}

	if ( !dontlog)
		writelog( ip, LOGMSG13, ip->relpath);


	if ( ip->fptype == FRP_PIPE)
		pclose( ip->fp);
	else
		fclose( ip->fp);

}

void
send_text_line( line)
char	*line;
{
	send_out_mem( line, strlen(line));
}


void
sendredirect( ip, status, location)
Request	*ip;
char	*status,
	*location;
{
	static int	num = 0;

	num++;
	if ( num > MAX_REDIRECT) {
		senderr( SERV_ERR, ERRMSG55, ERRMSG64);
		wn_exit( 2);
	}

	if ( strncasecmp( location, "<null>", 6) == 0) {
		send204( ip);
		return;
	}

	if ( location != outheadp->redirect)
		mystrncpy( outheadp->redirect, location, MIDLEN);
	strcpy( outheadp->status, status);
	ip->content_type = ip->encoding = NULL;
	*ip->length = '\0';
	*outheadp->list = '\0';

	http_prolog( );

	writelog( ip, LOGMSG9, location);


	if ( ip->attributes & (WN_PARSE + WN_DYNAMIC +WN_FILTERED) )
		wn_exit(0);
	/*
	 * Exit since we aren't doing keepalive.  The problematic place
	 * for return is in dolocation call in parse.c
	 */

	/* If we are doing keepalive then return */
	return;
}

void
send204( ip)
Request	*ip;
{
	strcpy( outheadp->status, "204 No Response");

	http_prolog();

	writelog( ip, LOGMSG16, "");
	if ( ip->attributes & (WN_PARSE + WN_DYNAMIC +WN_FILTERED) )
		wn_exit(0);
	/*
	 * We aren't doing keepalive.  The problematic place
	 * for return is in dolocation call in parse.c
	 */
	return;
}


static void
send_byterange( )
{

	char	*nextrange,
		*save_content,
		sep[SMALLLEN],
		buf[SMALLLEN];

	long	startbyte,
		endbyte,
		temp_len,
		file_len,
		len_sent;

	int	multi = FALSE,
		firsttime = TRUE;

	len_sent = 0;
	file_len = this_rp->datalen;
	nextrange = this_rp->range;
	if  ( strchr( nextrange, ',') != NULL)
		multi = TRUE;
	while ( nextrange ) {
		nextrange = enter_range( nextrange, &startbyte, &endbyte);
		if ( startbyte == -1 ) {
			temp_len = endbyte;
			endbyte = file_len - 1;
			startbyte = file_len - temp_len;
		}
		else {
			if ( (endbyte == -1 ) || (endbyte >= this_rp->datalen))
				endbyte = this_rp->datalen - 1;
		}

		mystrncpy( outheadp->status,
					"206 Partial Content", SMALLLEN);

		if ( multi && (this_rp->filetype & WN_RFC_BYTERANGE) ) {
			if ( firsttime) {
				firsttime = FALSE;
				*(this_rp->length) = '\0';
				srand( this_conp->pid);
				sprintf( sep, "=%x=%x=%x=",
						rand(), rand(), rand());
				sprintf( buf,
				"multipart/x-byteranges; boundary=\"%s\"",
						sep);
				save_content = this_rp->content_type;
				this_rp->content_type = buf;
				http_prolog( );
				this_rp->content_type = save_content;
			}
			sprintf( buf, "\r\n--%s\r\n", sep);

			send_text_line( buf);
			len_sent += strlen( buf);

			sprintf( buf, "Content-type: %s\r\n",
						this_rp->content_type);
			send_text_line( buf);
			len_sent += strlen( buf);

			sprintf( buf,
				"Content-Range: bytes %ld-%ld/%ld\r\n\r\n",
					startbyte, endbyte, file_len);
			send_text_line( buf);
			len_sent += strlen( buf);

			sendsubrange( startbyte, endbyte);
			len_sent += (endbyte - startbyte + 1);

			if ( nextrange == (char *)NULL) {
				sprintf( buf, "\r\n--%s--\r\n", sep);
				send_text_line( buf);
				len_sent += strlen( buf);
				sprintf( this_rp->length, "%ld", len_sent);
				return;
			}
			else
				continue;
		}
		else {
			this_rp->datalen = endbyte - startbyte + 1;
			sprintf( this_rp->length, "%ld", this_rp->datalen);

			if ( this_rp->filetype & WN_RFC_BYTERANGE ) {
				sprintf( outheadp->range,
					"Content-Range: bytes %ld-%ld/%ld\r\n",
						startbyte, endbyte, file_len);
			}
			http_prolog( );
			sendsubrange( startbyte, endbyte);
			return;
		}
	}
}

static void
sendsubrange( start, end)
long	start,
	end;
{
	int	remlen,
		fdfp,
		len;

	long	remaining;

	remaining = end - start + 1;
	fdfp = fileno( this_rp->fp);
	lseek( fdfp, (off_t) start, 0 /* SEEK_SET */);

	remlen = this_conp->outbuf + OUT_BUFFSIZE - this_conp->out_ptr;
	remlen = ( remlen > remaining ? (int) remaining : remlen);

	while ( TRUE) {
		len = read( fdfp, this_conp->out_ptr, remlen);
		if ( (len == -1) && (errno == EINTR))
			continue;
		if ( len <= 0 )
			break;

		if ( this_conp->outbuf + OUT_BUFFSIZE <= 
					this_conp->out_ptr + len ) {  
			/* buffer is full */
			this_conp->out_ptr += len;
			flush_outbuf();
			remlen = ( OUT_BUFFSIZE > remaining ?
					(int) remaining : OUT_BUFFSIZE);
			remaining -= len;
			continue;
		}
		else {  /* buffer not full yet */
			remlen -= len;
			this_conp->out_ptr += len;
			remaining -= len;
		}
	}
	if ( (remaining > 0) || (len < 0) )
		logerr( ERRMSG76, "sendsubrange");


}


static char 
*enter_range(  value, start, end )
char	*value;
long	*start,
	*end;
{
	register char	*cp,
			*cp2;

	char		*next;

	/* if ip->param_value is "123-234" it is a file range from
	 * byte or line 123 to 234.  Put 123 in start and 234
	 * in end. For 123- use -1 for end range and for -456 use
	 * -1 for start.
	 */

	cp = value;

	if ( (cp2 = strchr( cp, ',')) != NULL) {
		*cp2++ = '\0';
		next = cp2;
	}
	else
		next = (char *)NULL;

	if ( (cp2 = strchr( cp, '-')) == NULL) {
		logerr( ERRMSG93, cp);
		return (char *)NULL;
	}

	*cp2++ = '\0';
	*start = ( *cp ? atol( cp ) : (-1));
	*end = ( *cp2 ? atol( cp2 ) : (-1));
	return (next);
}


void
send_out_fd( fd )
int	fd;
{

	int		remlen,
			len;

	remlen = this_conp->outbuf + OUT_BUFFSIZE - this_conp->out_ptr;
	while ( TRUE) {
		len = read( fd, this_conp->out_ptr, remlen);
		if ( (len == -1) && (errno == EINTR))
			continue;
		if ( len <= 0 )
			break;

		this_conp->out_ptr += len;
		if ( len == remlen ) {  /* buffer is full */
			flush_outbuf();
			remlen = OUT_BUFFSIZE;
			continue;
		}
		else {  /* buffer not full yet */
			remlen -= len;
			if ( this_rp->attributes & WN_UNBUFFERED)
				flush_outbuf();
		}
	}
	if ( len < 0 )
		logerr( ERRMSG76, "send_out_fd");
}

void
send_out_mem( buf, len)
char	*buf;
int	len;
{
	register char	*cp,
			*cp2,
			*end;

	int		remlen;

	end = this_conp->outbuf + OUT_BUFFSIZE;
	cp = buf;

	while ( len > 0 ){
		cp2 = this_conp->out_ptr;
		if ( end > cp2 + len ) {
			/* it all fits in buffer */
			memcpy( cp2, cp, len);
			this_conp->out_ptr += len;
			if ( this_rp->attributes & WN_UNBUFFERED)
				flush_outbuf();
			return;
		}
		else {
			remlen = this_conp->outbuf
					+ OUT_BUFFSIZE - cp2;
			len -= remlen;
			memcpy( cp2, cp, remlen);
			cp += remlen;
			this_conp->out_ptr = end;
			flush_outbuf();
		}
	}
}


void
flush_outbuf( )
{

	int		fdstdout,
			len,
			n;

	register char	*cp;

	cp = this_conp->outbuf;
	len = this_conp->out_ptr - cp;
	if ( len == 0 )
		return;
	fdstdout = fileno( stdout);
	while ( (n = WN_write( fdstdout, cp, len)) < len) {
		if ( n == -1 && errno == EINTR) 
			continue;
		if ( n <= 0) {
			if ( n == -1 && errno != EPIPE) {
				char   buf[TINYLEN];
				sprintf( buf, "flush, errno = %d", errno);
				logerr( ERRMSG75, buf);
			}
			break;
		}
		len -= n;
		cp += n;
		this_conp->bytecount += n;

	}
	if ( this_conp->bytecount >= BYTECHUNK) {
		alarm( TRANSACTION_TIMEOUT);
		this_conp->bytecount = 0L;
	}
	this_conp->out_ptr = this_conp->outbuf;
}

