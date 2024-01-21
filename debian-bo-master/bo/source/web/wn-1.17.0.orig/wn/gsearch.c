/*
    Wn: A Server for the HTTP
    File: wn/gsearch.c
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
#include <string.h>
#include "wn.h"
#include "search.h"
#include "parse.h"
#include "reg.h"
#include "regi.h"

static char	curr_line[MIDLEN],
		*remove_tags();

static int	gfound_match(),
		cfound_match(),
		file_grep(),
		in_tag(),
		ok2mark = FALSE,
		begin_offset,
		end_offset,
		wrapped,
		ishtml;

static unsigned	curr_line_num;

static void	format_markline(),
		send_dirgrep(),
		send_filegrep(),
		send_grep_line();


static struct regprog	*regp,
			*htmlregp;


void
sendgrep( ip)
Request	*ip;
{
	*ip->length = '\0';
	if ( iswndir( ip))
		send_dirgrep( ip);
	else
		send_filegrep(ip);
}


/* Do regexp search of directory pointed to by ip->filepath */

static void
send_dirgrep( ip)
Request	*ip;
{
	FILE	*cfp,
		*gfp;

	register char	*cp;

	int	found = FALSE,
		max_matches = MAXMATCHES,
		num_matches;

	char	entryline[CACHELINE_LEN],
		commandbuf[2*MIDLEN],
		tmpbuf[2*MIDLEN];


	Parsedata	pdata;

	Cache_entry	*cep,
			entry;


	pdata.show = SHOW_IT;

	check_query( ip, &regp, &htmlregp);

	if ( ip->type == RTYPE_LINESSEARCH)
		max_matches = 4*MAXMATCHES;

	cep = &entry;
	cep->line = entryline;

	if ( (cfp = fopen( ip->cachepath, "r")) == (FILE *) NULL ) {
		senderr( SERV_ERR, "Can't open file", ip->cachepath);
		wn_exit( 2);
	}

	if ( dir_p->attributes & WN_DIRNOSEARCH) {
		senderr( "403", ERRMSG32, ip->relpath);
		wn_exit( 0);
	}

	search_prolog( ip, OUTMSG1);


	fgets( entryline, CACHELINE_LEN, cfp);  /* skip directory line */

	while ( read_cache_file( cep, cfp, (char *) NULL)) {
		if ( cep->attributes & WN_NOSEARCH)
			continue;
		if ( *(cep->redirect))
			continue;
		if ( strncmp( cep->content, "text", 4))
			continue;
		ishtml = streq( cep->content, "text/html");
		wrapped = (int) *cep->wrappers;
		mystrncpy( tmpbuf, ip->cachepath, MIDLEN);
		cp = strrchr( tmpbuf, '/');
		mystrncpy( cp + 1, cep->basename, SMALLLEN);

		check_perm( ip, tmpbuf);
		if ( cep->attributes & WN_FILTERED) {
			mystrncpy( commandbuf, cep->filter, SMALLLEN);
			strcat( commandbuf, " < ");
			strcat( commandbuf, tmpbuf);
			if ((gfp = popen( commandbuf, "r")) == (FILE *)NULL) {
				logerr( ERRMSG17, ip->filepath);
				logerr( ERRMSG18, ip->filter);
				continue;
			}
		}
                else if ( (gfp = fopen( tmpbuf, "r")) == (FILE *) NULL )
                        /* File may no longer be there, keep going */
                        continue;
		curr_line_num = num_matches = 0;
		if ( cep->attributes & WN_PARSE )
			reset_parse_err( tmpbuf, 0, &pdata);

		if ( (ip->type == RTYPE_CONTEXTSEARCH)
				|| (ip->type == RTYPE_LINESSEARCH)) {
						/* dir context search */
			while ( cfound_match( (ishtml ? htmlregp : regp),
							 gfp, &pdata) ) {
				num_matches++;
				found = TRUE;
				if ( num_matches < max_matches)
					send_grep_line( ip, cep->basename,
							cep->title);
				else {
					send_text_line( MAXEXCEEDED);
					break;
				}
			}
			ok2mark = FALSE;
		}
		else if ( gfound_match( (ishtml ? htmlregp : regp),
							gfp, &pdata) ) { 
			 /* dir grep search */
			found = TRUE;
			send_grep_line( ip, cep->basename, cep->title);
		}
		if ( cep->attributes & WN_FILTERED)
			pclose( gfp);
		else
			fclose( gfp);
	}
	fclose( cfp);

	if ( !found) {
		send_nomatch( ip, 'd');
		return;
	}
	
	if ( ip->type == RTYPE_CONTEXTSEARCH)
		send_text_line( "\t</ul>\n");
	send_text_line( "</ul>\n");

	if ( dir_p->attributes & WN_DIRWRAPPED) {
		do_swrap( ip);
	}
	else
		search_epilog( );

	writelog(  ip, LOGMSG10, "");
}

/*
 * Context search find match 
 */

static int
cfound_match( rp, fp, pdp)
struct regprog	*rp;
FILE	*fp;
Parsedata	*pdp;
{
	unsigned	searchoff;
	int		token;

	char	linebuf[MIDLEN],
		dummy,
		*cp,
		*startmatch,
		*endmatch;


	searchoff = FALSE;
	pdp->show = SHOW_MSK + IN_SECT_MSK;

	if ( wrapped)
		ok2mark = TRUE;
	while ( fgets( linebuf, MIDLEN, fp)) {
		curr_line_num++;
		pdp->currline++;
		token = get_parse_token( linebuf, &dummy, pdp);
		if ( token != NULLTOKEN) {
			switch ( token) {
			case SEARCH_ON:
				searchoff = FALSE;
				break;
			case SEARCH_OFF:
				searchoff = TRUE;
				break;
			default:
				set_show( token, pdp);
			}
			continue;
		}
		if ( searchoff || !( pdp->show & SHOW_MSK))
			continue;

		strcpy( curr_line, linebuf);
		strlower( linebuf);
		if ( !ok2mark) {
			if ( strstr( linebuf, "</head>"))
				ok2mark = TRUE;
			if ( curr_line_num > 12 )
				ok2mark = TRUE;
		}
		cp = linebuf;
		regfind( rp, cp);
		while ( regfind( rp, cp) ) {
			startmatch = reglp(0);
			endmatch = regrp(0);
			if ( in_tag( linebuf, startmatch, endmatch)) {
				cp = startmatch + 1;
				continue;
			}
			begin_offset = (int) ( startmatch - linebuf);
			end_offset = (int) ( endmatch - linebuf);
			return TRUE;
		}
	}
	return FALSE;
}

/*
 * in_tag( line, point, end) returns TRUE or FALSE depending on whether 
 * anything between point and end is inside of < and >.  The opening < 
 * may be in a  previous (unseen) line and the closing > may be in a
 * subsequent line.
 */

static int
in_tag( line, point, end)
char	*line,
	*point,
	*end;
{
	char		lastseen = '\0';

	while ( line < point ) {
		switch ( *line) {
		case '<':
		case '>':
			lastseen = *line++;
			break;
		default:
			line++;
		}
	}
	switch ( lastseen) {
	case '<':
		return TRUE;
	case '>':
		return FALSE;
	default:
		for (;;) {
			switch ( *line) {
			case '<':
				if ( line < end)
					return TRUE;
				else
					return FALSE;
			case '>':
				return TRUE;
			case '\0':
				return FALSE;
			default:
				line++;
			}
		}
	}
}


static int
gfound_match( rp, fp, pdp)
struct regprog	*rp;
FILE	*fp;
Parsedata	*pdp;
{
	char		dummy,
			linebuf[MIDLEN];
	unsigned	searchoff;
	int		token;

	searchoff = FALSE;
	pdp->show = SHOW_MSK + IN_SECT_MSK;

	while ( fgets( linebuf, MIDLEN, fp)) {
		pdp->currline++;
		token = get_parse_token( linebuf, &dummy, pdp);
		if ( token != NULLTOKEN) {
			switch ( token) {
			case SEARCH_ON:
				searchoff = FALSE;
				break;
			case SEARCH_OFF:
				searchoff = TRUE;
				break;
			default:
				set_show( token, pdp);
			}
			continue;
		}
		if ( searchoff || !( pdp->show & SHOW_MSK))
			continue;

		strcpy( curr_line, linebuf);
		strlower( linebuf);
		if ( regfind( rp, linebuf) )
			return TRUE;
	}
	return FALSE;
}


static void
send_grep_line( ip, name, title)
Request	*ip;
char	*name,
	*title;
{
	static char	prevname[MIDLEN];

	static int	first_matched_line = TRUE;
	char		buf[MIDLEN];

	if ( first_matched_line) {
		first_matched_line = FALSE;
		if ( iswndir( ip) && dir_p->attributes & WN_DIRWRAPPED)
			do_swrap( ip);
		else if ( ip->attributes & WN_SWRAPPED)
			do_swrap( ip);
		else {
			send_text_line( REGRES2);
			sprintf(buf,REGRES3, ip->param_value);
			send_text_line( buf);
			send_text_line( REGRES4);
			sprintf(buf, REGRES1, ip->query);
			send_text_line( buf);
		}
		send_text_line( "<ul>\n");
	}
	if ( ip->type == RTYPE_LINESSEARCH)
			format_markline( name);
	else if ( ip->type == RTYPE_CONTEXTSEARCH) {
		if ( streq( name, prevname) )
			format_markline( name);
		else {
			if ( *prevname)
				send_text_line("\t</ul>\n");
			mystrncpy( prevname, name, MIDLEN);
			sprintf( buf,
				"<li><b>Title:</b> <a href=\"%s\">%s</a>\n",
					name, title);
			send_text_line( buf);
			send_text_line(
				"\t<br><b>Matching lines:</b>\n\t<ul>\n");
			format_markline( name);
		}
	}
	else {
		sprintf( buf, "<li><a href=\"%s\">%s</a>\n", name, title);
		send_text_line( buf);
	}
}

static void
format_markline( base)
char	*base;
{
	register char	*cp;

	char	*beginmatch,
		*endmatch,
		buf[MIDLEN],
		mline[SMALLLEN];


	cp = buf;

	beginmatch = curr_line + begin_offset;
	endmatch = curr_line + end_offset;
	*mline = '\0';
	strcpy( buf, "\t<li> ");
	cp += 6;
	if ( ok2mark && ishtml)
		sprintf( mline, ";mark=%d,%d,%d#%s", curr_line_num,
				begin_offset, end_offset, WN_HTML_MARK);

	cp = remove_tags( cp, curr_line, beginmatch);

	sprintf( cp, "<a href=\"%s%s\">",
		 base, mline);

	while ( *cp)
		cp++;

	cp = remove_tags( cp, beginmatch, endmatch);

	strcpy( cp, "</a>");
	cp +=4;

	cp = remove_tags( cp, endmatch, (char *) NULL);
	*cp = '\0';	
	send_text_line( buf);
}

/*
 * char *remove_tags( p1, p2, end)  Copy p2 to p1 until end is reached
 * or p2 is exhausted.  If doc is not HTML URL encode '<', '>', and & and 
 * if it is remove tags.
 *  Use end = NULL to go to use all of p2. 
 */

static char
*remove_tags ( p1, p2, end)
char	*p1,
	*p2,
	*end;
{
	int	intag = FALSE;
	char	*start;

	start = p1;
	if ( end == NULL)
		end = p2 + strlen( p2);
	if ( ishtml) {
		while ( *p2 && p2 < end) {
			switch( *p2) {
			case '<':
				intag = TRUE;
				p2++;
				break;
			case '>':
				if ( !intag)
					p1 = start;
				p2++;
				intag = FALSE;
				break;
			default:
				if ( intag)
					p2++;
				else
					*p1++ = *p2++;
			}
		}
	}
	else { /* not in HTML amperfy stuff */
		while ( *p2 && p2 < end) {
			switch( *p2) {
			case '<':
				strcpy( p1, "&lt;");
				p1 += 4;
				p2++;
				break;
			case '>':
				strcpy( p1, "&gt;");
				p1 += 4;
				p2++;
				break;
			case '&':
				strcpy( p1, "&amp;");
				p1 += 5;
				p2++;
				break;
			default:
				*p1++ = *p2++;
			}
		}
	}
	*p1 = 0;
	return p1;
}

/* Do regexp search of file pointed to by ip->filepath */

static void
send_filegrep( ip)
Request	*ip;
{
	FILE	*gfp;
	int	found = FALSE;
	Parsedata	pdata;

	pdata.show = SHOW_IT;
	ip->content_type = "text/html";
	*ip->length = '\0';
	check_query( ip, &regp, &htmlregp);

	if ( (ip->attributes & WN_NOSEARCH) || 
			!(ip->filetype & WN_TEXT)) {
		senderr( "403", ERRMSG51, ip->relpath);
		wn_exit( 0);
	}

	search_prolog( ip, OUTMSG3 );

	ishtml = (ip->filetype & WN_ISHTML);

	check_perm( ip, ip->filepath);
        if ( (gfp = fopen( ip->filepath, "r")) == (FILE *) NULL )
		senderr( SERV_ERR, ERRMSG1, ip->relpath);

	curr_line_num = 0;
	if ( ip->attributes & WN_PARSE )
		reset_parse_err( ip->filepath, 0, &pdata);

	if ( ip->type == RTYPE_CONTEXTSEARCH) {
		while ( cfound_match( (ishtml ? htmlregp : regp), gfp,
							&pdata )) {
			found = TRUE;
			send_grep_line( ip, ip->basename, ip->title);
		}
		ok2mark = FALSE;
	}
	else
		found = file_grep( ip, (ishtml ? htmlregp : regp), gfp);

	fclose( gfp);

	if ( !found) {
		send_nomatch( ip, 'f');
		return;
	}
	
	if ( ip->type == RTYPE_CONTEXTSEARCH)
		send_text_line( "\t</ul>\n</ul>\n");

	if ( ip->attributes & WN_SWRAPPED)
		do_swrap( ip);
	else
		search_epilog( );

	writelog(  ip, LOGMSG10, "");
}

static int
file_grep( ip, rp, fp)
Request		*ip;
struct regprog	*rp;
FILE	*fp;
{
	char	linebuf[MIDLEN],
		buf[MIDLEN];
	int	fnd = FALSE,
		first = TRUE;


	while ( fgets( linebuf, MIDLEN, fp)) {
		strcpy( curr_line, linebuf);
		strlower( linebuf);
		if ( regfind( rp, linebuf) ) {
			if ( first && ip->attributes & WN_SWRAPPED) {
				do_swrap( ip);
				send_text_line( "<pre>\n");
			}
			else if ( first) {
				send_text_line( REGRES2);
				sprintf(buf,REGRES3, ip->param_value);
				send_text_line( buf);
				send_text_line( REGRES4);
				sprintf(buf, REGRES1, ip->query);
				send_text_line( buf);
				send_text_line( "<pre>\n");
			}
			amperline( linebuf, curr_line);
			send_text_line( linebuf);
			first = FALSE;
			fnd =  TRUE;
		}
	}
	if ( fnd)
		send_text_line( "</pre>\n");
	return fnd;
}


