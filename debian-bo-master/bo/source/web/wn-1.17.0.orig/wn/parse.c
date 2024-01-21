/*
    Wn: A Server for the HTTP
    File: wn/parse.c
    Version 1.15.3
    
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
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <string.h>
#include "wn.h"
#include "parse.h"
#include "reg.h"

#define MAX_REDIRECT	(10)

extern long	atol();

extern char	*getenv();

static void	send_fp(),
		send_include(),
		out_markline(),
		doc_parse();

static char	*in_anchor();

static FILE	*openfp();


static int	ifcnt = 0,
		hidden_ifcnt = 0;

static Parsedata	swpdata;


typedef struct File_or_Pipe {
	FILE	*fp;
	int	type;
} File_or_Pipe;

static File_or_Pipe	swrapfp_st = { NULL, FRP_FILE};


/*
 * do_wrap( ip, show)
 * Handle inserted files or files with wrappers.  
 */

void
do_wrap( ip, show)
Request		*ip;
unsigned	show;
{
	static enum { 	before_file,
			in_file,
			after_file } wrap_place;

	int		toplevel = FALSE;
	register char	*cp;
	File_or_Pipe	frp_st;

	Parsedata	pdata;

	if ( (ip->type != RTYPE_FILE) && (ip->type != RTYPE_MARKLINE))
			/* only do wrapping for files */
		return;

	pdata.show = show;
	pdata.currline = 0;

	/*
	 * ip->do_wrap_1st_time gets reinitialized in dolocation()
	 * and in keepalive loop in wn.c.
	 */
	if ( ip->do_wrap_1st_time) {
		wrap_place = before_file;
		toplevel = TRUE;
		ip->do_wrap_1st_time = FALSE;
		ifcnt = 0;
		hidden_ifcnt = 0;
	}

	cp = ip->inclptr;
	if ( !*cp ) {		/* end of wrappers send the file */
		pdata.currfile = ip->basename;
		switch ( wrap_place) {
		case before_file:
			wrap_place = in_file;
			ip->inclptr = ip->includes;

			if ( !(ip->filetype & WN_TEXT)) {
				senderr( DENYSTATUS, ERRMSG34,
					ip->relpath);
				wn_exit( 0);
			}
			send_fp( ip, &pdata);
			wrap_place = after_file;
			/* If some includes are left, tack them on the end */
			while ( toplevel && (*ip->inclptr))
				do_wrap( ip, pdata.show);
			return;

		case in_file:
		case after_file:
			if ( !*cp ) {
				logerr( ERRMSG35, "");
				return;
			}
		}
	}

	while ( *cp && *cp != ',')
		cp++;

	if ( *cp )
		*cp++ = '\0';
	pdata.currfile = ip->inclptr;
	if ( (openfp( ip->inclptr, ip, &frp_st)) == (FILE *) NULL ) {
		/* Error logged in openfp */
		ip->inclptr = cp;
		return;
	}
	else {
		ip->inclptr = cp;
		send_include( ip, frp_st, &pdata);

		switch ( wrap_place) {
		case before_file:
			do_wrap( ip, pdata.show);
			return;
		case in_file:
			return;
		case after_file: /* file done but but still some includes */
			while ( toplevel && (*ip->inclptr))
				do_wrap( ip, pdata.show);
			return;

		}
	}
}


/*
 * static void send_include( ip, frp_st, pdp)
 * Actually send the include or wrapper file. 
 */

static void
send_include( ip, frp_st, pdp)
Request	*ip;
File_or_Pipe	frp_st;
Parsedata	*pdp;
{
	char	linebuf[BIGLEN],
		insert_param[SMALLLEN];

	int	token;

	pdp->currline = 0;
	while ( fgets( linebuf, BIGLEN, frp_st.fp)) {
		pdp->currline++;
		token = get_parse_token( linebuf, insert_param, pdp);
		if ( ( token == NULLTOKEN) || (token == COMMENT)) {
			if ( (pdp->show & SHOW_IT) == SHOW_IT)
				send_text_line( linebuf);
			continue;
		}
		else {
			if ( set_show( token, pdp))
				continue;
			else
				doc_parse( ip, token, pdp, insert_param);
		}
	}
	if ( frp_st.type == FRP_PIPE)
		pclose( frp_st.fp);
	else
		fclose( frp_st.fp);
}

/*
 * send_fp( ip, pdp) Actually send the main file.
 */

static void
send_fp( ip, pdp)
Request	*ip;
Parsedata	*pdp;
{

	char		linebuf[BIGLEN],
			insert_param[SMALLLEN];

	int		token;

	if ( ip->type == RTYPE_MARKLINE ) {
		send_markline_doc( ip, pdp->show);
		return;
	}

	pdp->currline = 0;
	while ( fgets( linebuf, BIGLEN, ip->fp)) {
		pdp->currline++;
		token = get_parse_token( linebuf, insert_param, pdp);
		if ( ( token == NULLTOKEN) || (token == COMMENT)) {
			if ( (pdp->show & SHOW_IT) == SHOW_IT)
				send_text_line( linebuf);
			continue;
		}
		else {
			if ( set_show( token, pdp))
				continue;
			else
				doc_parse( ip, token, pdp, insert_param);
		}
	}

	if ( ip->fptype == FRP_PIPE)
		pclose( ip->fp);
	else
		fclose( ip->fp);
}


static void
doc_parse( ip, token, pdp, insert_ptr)
Request 	*ip;
int		token;
Parsedata	*pdp;
char		*insert_ptr;
{
	char		*cp,
			buf[MIDLEN + 1];

	unsigned	show2;

	switch ( token) {
	case FIELD:				/* field# */
		if ( (pdp->show & SHOW_IT) == SHOW_IT ) {
			mystrncpy( buf, ip->field[atoi(insert_ptr)], MIDLEN);
			strcat( buf, "\n");
			send_text_line( buf);
		}
		break;

	case ENVIRON:				/* environ variable */
		if ( (pdp->show & SHOW_IT) == SHOW_IT ) {
			if ( (cp = getenv(insert_ptr)) == NULL)
				break;
			mystrncpy( buf, cp, MIDLEN);
			strcat( buf, "\n");
			send_text_line( buf);
		}
		break;

	case INCLUDE:				/* include */
		do_wrap( ip, pdp->show);
		break;
	case REDIRECT:
		if ( (pdp->show & SHOW_IT) == SHOW_IT ) {
			if ( ip->status & WN_PROLOGSENT ) {
				logerr( PARSE_ERRMSG10, pdp->currfile);
				break;
			}
			dolocation( outheadp->redirect, ip);
		}
		break;
	case SECTION:
		show2 = pdp->show;
		if ( !(show2 & SECT_MSK)) {
			show2 |= SECT_MSK;
			show2 &= ~IN_SECT_MSK;
		}
		do_wrap( ip, show2);
		break;
	case QUERY:
		if ( (pdp->show & SHOW_IT) == SHOW_IT ) {
			mystrncpy( buf, ip->query, MIDLEN);
			strcat( buf, "\n");
			send_text_line( buf);
		}
		break;
	case TITLE:
		if ( (pdp->show & SHOW_IT) == SHOW_IT ) {
			mystrncpy( buf, ip->title, MIDLEN);
			strcat( buf, "\n");
			send_text_line( buf);
		}
		break;
	}
}





/*
 * static void openfp( name, ip, frp)
 * If name starts with '/' assume it is relative to system root,
 * if it starts with ~/ it is relative to WN root  otherwise
 * assume relative to current directory.  If name starts with '!'
 * it is a command to execute, so use popen( ).  If open fails log error
 * and return NULL.  Check UID and GID on file to see if they are
 * compatible with current security options in force.
 */

static FILE
*openfp( name, ip, frp)
char	*name;
Request	*ip;
File_or_Pipe	*frp;
{
	FILE	*ofp;
	char	buf[MIDLEN];

	if ( *name == '!' ) {
		name++;
		exec_ok( ip);
		if ( getfpath( buf, name, ip) == FALSE) {
			logerr( ERRMSG36, name);
			return NULL;
		}
		check_perm( ip, buf);
		if ( (ofp = popen( buf, "r")) == (FILE *) NULL ) {
			logerr( ERRMSG36, name);
			return NULL;
		}
		frp->fp = ofp;
		frp->type = FRP_PIPE;
		return ofp;
	}
		
	if ( getfpath( buf, name, ip) == FALSE) {
		logerr( ERRMSG1, buf);
		return NULL;
	}
	check_perm( ip, buf);
	if ( (ofp = fopen( buf, "r")) == (FILE *) NULL ) {
		logerr( ERRMSG1, buf);
		return NULL;
	}
	frp->fp = ofp;
	frp->type = FRP_FILE;
	return ofp;
}

/*
 * void do_nomatchsub( ip, location)
 * If the return is empty (e.g. a search with no matches) substitute 
 * another local file or a  URL specified in location.  If location
 * begins with '/' it is assumed to be a relative URL and sendredirect is
 * called.  This is necessary even if it is local since otherwise
 * the client would be confused about relative URLs.  If location
 * has no '/' then assume it is a file in the same directory and
 * process it as a URL immediately.
 */

void
do_nomatchsub( ip, location)
Request	*ip;
char	*location;
{
	Parsedata	pdata;
	File_or_Pipe	frp_st;


	if ( *location == '/') {
		sendredirect( ip, "302 Moved Temporarily", location);
		return;  /* to send_nomatchsub */
	}
	pdata.currline = 0;
	pdata.currfile = location;
	pdata.show = SHOW_IT;

	if ( openfp( location, ip, &frp_st) == (FILE *) NULL ) {
		logerr( ERRMSG83, location);
		return;
	}

	ip->fp = frp_st.fp;
	ip->fptype = frp_st.type;

	ip->attributes |= WN_PARSE;
	send_fp( ip, &pdata);

	writelog( ip, LOGMSG2, location);
	return;		/* to send_nomatchsub */
}



/*
 * send_markline_doc( ip, show) Send the main file main file when 
 * ip->type = RTYPE_MARKLINE.
 *
 */

void
send_markline_doc( ip, show)
Request		*ip;
unsigned	show;
{

	register char	*cp,
			*cp2;

	unsigned	markline,
			off1,
			off2,
			i = 0;


	char	linebuf[MIDLEN],
		linebuf2[MIDLEN],
		bigbuf[3*MIDLEN],
		insert_param[SMALLLEN],
		*astart,
		*s3,
		*begin,
		*end;

	int	token;

	Parsedata	pdata;

	pdata.show = show;
	pdata.currline = 0;
	pdata.currfile = ip->basename;

	cp = cp2 = ip->param_value;
	while ( *cp && (*cp != ','))
		cp++;
	if ( *cp)
		*cp++ = '\0';
	markline = (unsigned) atol( cp2);

	cp2 = cp;
	while ( *cp && (*cp != ','))
		cp++;
	if ( *cp)
		*cp++ = '\0';
	off1 = (unsigned) atol( cp2);
	off2 = (unsigned) atol( cp);

	if ( off1 > off2) {
			senderr( CLIENT_ERR, ERRMSG38, "");
			wn_exit( 2);
	}

	bigbuf[0] = '\0';

	while ( fgets( linebuf, MIDLEN, ip->fp)) {
		pdata.currline++;
		i++;

		token = get_parse_token( linebuf, insert_param, &pdata);
		if ( ( token == NULLTOKEN) || (token == COMMENT)) {
			if ( !((pdata.show & SHOW_IT) == SHOW_IT) )
				continue;
		}
		else {
			if ( set_show( token, &pdata))
				continue;
			else
				doc_parse( ip, token, &pdata, insert_param);
		}

		if ( (i == markline - 1) || (i == markline - 2)){
			mystrncat( bigbuf, linebuf, MIDLEN);
			continue;
		}
		if ( i == markline) {
			s3 = bigbuf + strlen(bigbuf);
			mystrncpy( s3, linebuf, MIDLEN);
			mystrncpy( linebuf2, linebuf, MIDLEN);
			strlower( linebuf2);
			if ( off2 <= strlen( linebuf2) ) {
				begin = s3 + off1;
				end = s3 + off2;
			}
			else {
				senderr( CLIENT_ERR, ERRMSG38, "");
				wn_exit( 2);
			}



			if ( astart = in_anchor( bigbuf, begin)) {
				*astart = '\0';
				send_text_line( bigbuf);
				sprintf( linebuf, "<a name=\"%s\">&#160;</a>",
						WN_HTML_MARK);
				send_text_line( linebuf);
					
				*astart = '<';

			}
			out_markline( (astart ? astart : bigbuf),
					begin, end, in_anchor( bigbuf, begin));

			continue;
		}
		send_text_line( linebuf);
	}
	if ( ip->fptype == FRP_PIPE)
		pclose( ip->fp);
	else
		fclose( ip->fp);
}

/*
 * char *in_anchor( buf, point) returns a pointer to the start of the
 * anchor ( <a href=...) containing point.  Returns NULL if point is 
 * not in an anchor.
 */

static char
*in_anchor( buf, point)
char	*buf,
	*point;
{
	register char	*cp;

	char	*start;
	int	in = FALSE;

	cp = buf;
	while ( cp < point) {
		if ( *cp == '<' ) {
			if ( strncasecmp( cp, "</a>", 4 ) == 0 ) {
				in = FALSE;
				cp += 4;
				continue;
			}
			if ( (strncasecmp( cp, "<a", 2) == 0)
				&& isspace( *(cp + 2)) ) {
				start = cp;
				in = TRUE;
				cp += 3;
				continue;
			}
		}
		cp++;
	}
	return ( in ? start : NULL);
}



/*
 * set_show( token, pdp)  See if token corresponds to 
 * "if", "else", "endif", "start" or "end".  If it does adjust
 * the variable "show" appropriately and return TRUE.  Else return
 * FALSE. 
 */

int
set_show( token, pdp)
int		token;
Parsedata	*pdp;
{

	if ( pdp->show & SHOW_MSK) {
		switch ( token) {
		case IF_TRUE:
			ifcnt++;
			break;

		case IF_ERR:
			pdp->show |= ERR_MSK;  /* fall through */
		case IF_FALSE:
			pdp->show &= ~SHOW_MSK;
			ifcnt++;
			break;

		case ELSE:
			if ( ifcnt > 0 ) {
				pdp->show &= ~SHOW_MSK;
			}
			else
				parse_html_err( token, pdp);
			break;

		case ENDIF:
			if ( ifcnt > 0 )
				ifcnt--;
			else
				parse_html_err( token, pdp);
			break;

		case START:
			if ( !(pdp->show & SECT_MSK))
				break;
			pdp->show |= IN_SECT_MSK;
			break;

		case END:
			if ( !(pdp->show & SECT_MSK))
				break;
			pdp->show &= ~IN_SECT_MSK;
			break;

		default:
			return (FALSE);
		}
	}
	else {  			/* SHOW_MSK is off */
		switch ( token) {
		case IF_ERR:
			pdp->show |= ERR_MSK;  /* fall through */
		case IF_TRUE:
		case IF_FALSE:
			ifcnt++;
			hidden_ifcnt++;
			break;

		case ELSE:
			if ( ifcnt <= 0 )
				parse_html_err( token, pdp);
			
			if ( (hidden_ifcnt <= 0) && !(pdp->show & ERR_MSK) )
				pdp->show |= SHOW_MSK;
			break;

		case ENDIF:
			if ( ifcnt > 0 )
				ifcnt--;
			else
				parse_html_err( token, pdp);

			if ( hidden_ifcnt > 0 )
				hidden_ifcnt--;
			else {
				pdp->show &= ~ERR_MSK;
				pdp->show |= SHOW_MSK;
			}
			break;

		case START:
			if ( !(pdp->show & SECT_MSK))
				break;
			pdp->show |= IN_SECT_MSK;
			break;

		case END:
			if ( !(pdp->show & SECT_MSK))
				break;
			pdp->show &= ~IN_SECT_MSK;
			break;

		default:
			return (FALSE);
		}
	}

	return (TRUE);
}

void
parse_html_err( token, pdp)
int	token;
Parsedata	*pdp;
{
	char	buf[SMALLLEN];

	if ( pdp->currfile == NULL)
		pdp->currfile = "unknown";
	sprintf( buf, "file=%s, line=%d", pdp->currfile, pdp->currline);

	switch ( token) {
		case ELSE:
			logerr( PARSE_ERRMSG1, buf);
			break;
		case ENDIF:
			logerr( PARSE_ERRMSG2, buf);
			break;

		case START:
			logerr( PARSE_ERRMSG3, buf);
			break;
		case END:
			logerr( PARSE_ERRMSG4, buf);
			break;
		case NULLTOKEN:
			logerr( PARSE_ERRMSG5, buf);
			break;
	}
}

int
get_parse_token( s, param_ptr, pdp)
char		*s,
		*param_ptr;
Parsedata	*pdp;
{
	char		buf[SMALLLEN];
	int		sgmlprocess,
			oldprocess;

	register char	*cp;

	cp = s;

	while ( isspace( *cp))
		cp++;

	sgmlprocess = (strncasecmp( cp, "<?wn", 4) == 0);
	oldprocess = (strncmp( cp, "<!--", 4) == 0 );

	if ( !( sgmlprocess || oldprocess) ) {
		if ( (pdp->show & SHOW_IT) == SHOW_IT) {
			http_prolog();
		}
		return (NULLTOKEN);
	}

	cp += 4;
	while ( *cp && isspace( *cp))
		cp++;

	if ( oldprocess) {
		if ( *cp != '#' ) {
			if ( (pdp->show & SHOW_IT) == SHOW_IT) {
				http_prolog();
			}
			return (COMMENT);
		}
	}
	if ( *cp == '#' )
		cp++;	/* skip the '#' */

	if ( strncasecmp( cp, "wn_", 3) == 0 )
		cp += 3;

	if ( strncasecmp( cp, "else", 4) == 0 ) {
		return (ELSE);
	}

	/* Careful here (endif vs end) */
	if ( strncasecmp( cp, "endif", 5) == 0 ) {
		return (ENDIF);
	}
	if ( strncasecmp( cp, "end", 3) == 0 ) {
		return (END);
	}

	if ( strncasecmp( cp, "field", 5) == 0 ) {
		cp += 5;

		while ( isspace( *cp) || (*cp == '#'))
			cp++;
		mystrncpy( param_ptr, cp, SMALLLEN);
		cp = param_ptr;
		while ( isdigit( *cp))
			cp++;
		*cp = '\0';
		return (FIELD);
	}

	if ( strncasecmp( cp, "environ", 7) == 0 ) {
		cp += 7;

		while ( isspace( *cp) || (*cp == '=') || (*cp == '"'))
			cp++;
		mystrncpy( param_ptr, cp, SMALLLEN);
		cp = param_ptr;
		while ( *cp && !isspace( *cp) && (*cp != '"'))
			cp++;
		*cp = '\0';
		return (ENVIRON);
	}

	if ( strncasecmp( cp, "if ", 3) == 0 ) {
		return (eval_if( cp + 3, pdp));

	}
	if ( strncasecmp( cp, "include", 7) == 0 ) {
		return (INCLUDE);
	}
	if ( strncasecmp( cp, "query", 5) == 0 ) {
		return (QUERY);
	}
	if ( strncasecmp( cp, "redirect", 8) == 0 ) {
		cp += 8;
		if ( (cp = strchr( cp, '"')) == NULL) {
			logerr( PARSE_ERRMSG11, s);
			return (COMMENT);
		}
		mystrncpy( outheadp->redirect, ++cp, MIDLEN);
		if ( (cp = strchr( outheadp->redirect, '"')) == NULL) {
			logerr( PARSE_ERRMSG11, s);
			return (COMMENT);
		}
		*cp = '\0';
		return (REDIRECT);
	}
	if ( strncasecmp( cp, "section", 7) == 0 ) {
		return (SECTION);
	}
	if ( strncasecmp( cp, "search_on", 9) == 0 ) {
		return (SEARCH_ON);
	}
	if ( strncasecmp( cp, "search_off", 10) == 0 ) {
		return (SEARCH_OFF);
	}
	if ( strncasecmp( cp, "start", 5) == 0 ) {
		return (START);
	}
	if ( strncasecmp( cp, "title", 5) == 0 ) {
		return (TITLE);
	}

	mystrncpy( buf, s, SMALLLEN);
	chop( buf);
	logerr( PARSE_ERRMSG5, buf);
	return (NULLTOKEN);
}

void
reset_parse_err( file, line, pdp)
char		*file;
int		line;
Parsedata	*pdp;

{
	pdp->currfile = file;
	pdp->currline = line;
}



/*
 * do_swrap( ip)
 * Handle search wrapper start and end
 */

void
do_swrap( ip)
Request	*ip;

{
	char	linebuf[BIGLEN],
		insert_param[SMALLLEN],
		*wrapfile;
	int	token;

	Parsedata	*pdp;
	static int	firsttime = TRUE;

	pdp = &swpdata;

	if ( swrapfp_st.fp == NULL) {  /* It's the first time so initialize */
		pdp->currline = 0;
		pdp->show = SHOW_IT;
		wrapfile = ( (iswndir( ip)) ? dir_p->swrapper :
						ip->swrapper);
		pdp->currfile = wrapfile;

		if ( openfp( wrapfile, ip, &swrapfp_st) == NULL ) {
			senderr( SERV_ERR, ERRMSG46, wrapfile);
			wn_exit( 2);
		}
	}

	while ( fgets( linebuf, BIGLEN, swrapfp_st.fp)) {
		pdp->currline++;
		token = get_parse_token( linebuf, insert_param, pdp);
		if ( token == INCLUDE) {
			if (firsttime ) {
				firsttime = FALSE;
				return;
			}
			else {
				logerr( ERRMSG84, wrapfile);
				wn_exit( 2);
			}
		}

		if ( ( token == NULLTOKEN) || (token == COMMENT)) {
			if ( (pdp->show & SHOW_IT) == SHOW_IT)
				send_text_line( linebuf);
			continue;
		}
		else {
			if ( set_show( token, pdp))
				continue;
			else
				doc_parse( ip, token, pdp, insert_param);
		}
	}
	if ( swrapfp_st.type == FRP_PIPE)
		pclose( swrapfp_st.fp);
	else
		fclose( swrapfp_st.fp);
}


/*  
 * If path starts with "http:" or '/' then do a 302 redirect.  Otherwise
 * consider it relative to current url and call process_url() to start
 * over on this new url.  If path is "<null>" send 204.  If path ends 
 * with '?' then append ip->query to it so that url query data will pass
 * to the new url.  
 */

void
dolocation( path, ip)
char	*path;
Request	*ip;
{
	static int	num = 0;

	char	*cp,
		buf[2*MIDLEN],
		loc[2*MIDLEN];

	num++;

	if ( num > MAX_REDIRECT) {
		senderr( SERV_ERR, ERRMSG55, "");
		wn_exit(2);
	}

	if ( strncasecmp( path, "<null>", 6) == 0) {
		send204( ip);
		return;
	}

	mystrncpy( buf, path, MIDLEN);

	if ( *(ip->query) 
		&& ((cp = strrchr( buf, '?')) != NULL) && (*(cp+1) == '\0'))
		mystrncpy( ++cp, ip->query, MIDLEN);
	/* ends with '?' so add the ip->query to it */

	if ( ((cp = strchr( buf, ':')) != NULL) && (*(cp+1) == '/'))
		{ /* URL rediretion */
		sendredirect( ip, "302 CGI redirection", buf);
		return;
	}

	if ( *buf == '/') {
		if ( port == STANDARD_PORT )
			sprintf( loc, "%s://%s", this_conp->scheme, hostname);
		else
			sprintf( loc, "%s://%s:%d", this_conp->scheme, 
					hostname, port);

		mystrncat( loc, buf, MIDLEN);
		sendredirect( ip, "302 CGI redirection", loc);
		return;
	}


	mystrncpy( loc, inheadp->url_path, MIDLEN);
	cp = strrchr( loc, '/');
	mystrncpy( ++cp, buf, MIDLEN);

	if ( (strchr( path, '#') != NULL) || (strchr( path, '/') != NULL)) {
		/* Relative URL with anchor designator requires redirect */
		/* and so does any relative URL with a '/'  */
		if ( port == STANDARD_PORT )
			sprintf( loc, "%s://%s", this_conp->scheme, hostname);
		else
			sprintf( loc, "%s://%s:%d", this_conp->scheme, 
					hostname, port);

		mystrncat( buf, loc, MIDLEN);
		sendredirect( ip, "302 CGI redirection", buf);
		return;
	}


	*(outheadp->redirect) = '\0';
	ip->do_wrap_1st_time = TRUE;

	process_url( ip, loc);

	if ( ip->attributes & (WN_PARSE + WN_DYNAMIC + WN_FILTERED) ) {
		wn_exit(0);
	/*
	 * We aren't doing keepalive.  The problematic place
	 * for return is in dolocation call in parse.c
	 */
	}
}


static void
out_markline( line, begin, end, inanch)
char	*line,
	*begin,
	*end,
	*inanch;

{
	register char	*cp;

	char	buf[BIGLEN];

	cp = buf;

	while ( line < begin )
		*cp++ = *line++;
	if (! inanch)
		sprintf( cp, "<b><a name=\"%s\">", WN_HTML_MARK);
	else
		sprintf( cp, "<b>");

	while ( *cp)
		cp++;

	while ( line < end )
		*cp++ = *line++;

	if (! inanch) {
		strcpy( cp, "</a></b>");
		cp +=8;
	}
	else {
		strcpy( cp, "</b>");
		cp +=4;
	}

	while ( *line )
		*cp++ = *line++;
	*cp = '\0';	
	send_text_line( buf);
}

