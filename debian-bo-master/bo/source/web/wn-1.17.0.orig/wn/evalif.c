/*
    Wn: A Server for the HTTP
    File: wn/evalif.c
    Version 1.15.5
    
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
#include "access.h"
#include "parse.h"
#include "reg.h"
#include "regi.h"

static int	fieldnum = 0,
		chk_accessfile( ),
		chk_file_match(),
		chk_match();

int
eval_if( cp, pdp)
char		*cp;
Parsedata	*pdp;
{

	while ( *cp && isspace( *cp))
		cp++;
	if ( strncasecmp( cp, "true ", 5) == 0 )
		return (IF_TRUE);
	if ( strncasecmp( cp, "false ", 6) == 0 )
		return (IF_FALSE);
	if ( strncasecmp( cp, "accessfile", 10) == 0 )
		return (chk_accessfile( this_rp, cp));

	if ( strncasecmp( cp, "accept", 6) == 0 )
		return (chk_match( cp + 6, MATCH_ACCEPT, pdp));

	if ( strncasecmp( cp, "cookie", 6) == 0 )
		return (chk_match( cp + 6, MATCH_COOKIE, pdp));

	if ( strncasecmp( cp, "ua", 2) == 0 )
		return (chk_match( cp + 2, MATCH_UA, pdp));

	if ( strncasecmp( cp, "refer", 5) == 0 ) {
		while ( isalpha( *cp)) /* get referer, and referrer */
			cp++;
		return (chk_match( cp, MATCH_REFERRER, pdp));
	}

	if ( strncasecmp( cp, "host_header", 11) == 0 )
		return (chk_match( cp + 11, MATCH_HOST_HEAD, pdp));

	if ( strncasecmp( cp, "query", 5) == 0 )
		return (chk_match( cp + 5, MATCH_QUERY, pdp));

	if ( strncasecmp( cp, "param_field", 11) == 0 )
		return (chk_match( cp + 11, MATCH_PARAM_FIELD, pdp));

	if ( strncasecmp( cp, "param_value", 11) == 0 )
		return (chk_match( cp + 11, MATCH_PARAM_VALUE, pdp));

	if ( strncasecmp( cp, "request", 7) == 0 )
		return (chk_match( cp + 7, MATCH_REQUEST, pdp));

	if ( strncasecmp( cp, "hostname", 8) == 0 )
		return (chk_match( cp + 8, MATCH_HOST, pdp));

	if ( strncasecmp( cp, "ip", 2) == 0 )
		return (chk_match( cp + 2, MATCH_IP, pdp));

	if ( strncasecmp( cp, "before", 6) == 0 )
		return ( date_cmp( this_rp, cp + 6, FALSE) ? 
				IF_FALSE : IF_TRUE);

	if ( strncasecmp( cp, "after", 5) == 0 )
		return ( date_cmp( this_rp, cp + 5, FALSE) ? 
				IF_TRUE : IF_FALSE);

	if ( strncasecmp( cp, "field", 5) == 0 )  {
		/* Get the field number */
                cp += 5;
                while ( isspace( *cp) || (*cp == '#'))
                        cp++;

		fieldnum = atoi( cp);
		return (chk_match( cp, MATCH_FIELD, pdp));
	}

	parse_html_err( NULLTOKEN, pdp);
	return (NULLTOKEN);
}



static int
chk_accessfile( ip, s)
Request	*ip;
char	*s;
{
	register char	*cp;
	int val;

	char	buf[SMALLLEN];

	if ( (cp = strchr( s, '"')) == NULL ) {
		logerr( PARSE_ERRMSG6, s);
		return IF_ERR;
	}
	mystrncpy( buf, ++cp, SMALLLEN);	

	if ( (cp = strchr( buf, '"')) == NULL ) {
		logerr( PARSE_ERRMSG6, s);
		return IF_ERR;
	}
	*cp = '\0';

	val = chkaccess( ip, buf);

	if ( (val == ACCESS_GRANTED) || (val == ACCESS_PRIVILEGED))
		return IF_TRUE;
	else if ( val == ACCESS_DENIED )
		return IF_FALSE;
	else
		return IF_ERR;

}



static int
chk_match( s, type, pdp)
char		*s;
int		type;
Parsedata	*pdp;
{
	struct regprog	*rp;
	register char	*cp;

	char	buf[SMALLLEN];
	int	val,
		notflg = FALSE;

	cp = s;
	while ( *cp && isspace( *cp))
		cp++;

	if ( strncasecmp( cp, "file", 4) == 0) {
		return( chk_file_match( cp + 4, type, pdp));
	}

	if ( (*cp == '=') && (*(cp+1) == '~'))
		cp += 2;
	else if ( (*cp == '!') && (*(cp+1) == '~')) {
		cp += 2;
		notflg = TRUE;
	}
	else if ( *cp != '~') {
		logerr( PARSE_ERRMSG5, s);
		return IF_ERR;
	}
	else 
		cp++;

	if ( (cp = strchr( cp, '"')) == NULL ) {
		logerr( PARSE_ERRMSG7, s);
		return IF_ERR;
	}
	mystrncpy( buf, ++cp, SMALLLEN);	

	if ( (cp = strchr( buf, '"')) == NULL ) {
		logerr( PARSE_ERRMSG7, s);
		return IF_ERR;
	}
	*cp = '\0';

	if ( (rp = regcomp( buf)) == NULL ) {
		logerr( PARSE_ERRMSG7, s);
		return IF_ERR;
	}

	switch ( type) {
	case MATCH_ACCEPT:
		val = regfind( rp, inheadp->accept);
		break;
	case MATCH_COOKIE:
		val = regfind( rp, inheadp->cookie);
		break;
	case MATCH_UA:
		val = regfind( rp, inheadp->ua);
		break;
	case MATCH_REFERRER:
		val = regfind( rp, inheadp->referrer);
		break;
	case MATCH_HOST_HEAD:
		val = regfind( rp, inheadp->host_head);
		break;
	case MATCH_REQUEST:
		val = regfind( rp, this_rp->request);
		break;
	case MATCH_IP:
		val = regfind( rp, remaddr);
		break;
	case MATCH_HOST:
		get_remote_info();
		val = regfind( rp, remotehost);
		break;
	case MATCH_QUERY:
		val = regfind( rp, this_rp->query);
		break;
	case MATCH_PARAM_FIELD:
		val = regfind( rp, this_rp->param_field);
		break;
	case MATCH_PARAM_VALUE:
		val = regfind( rp, this_rp->param_value);
		break;
	case MATCH_FIELD:
		val = regfind( rp, this_rp->field[fieldnum]);
		break;
	default:
		parse_html_err( NULLTOKEN, pdp);
		return (IF_FALSE);
	}
	if ( notflg )
		return ( val ? IF_FALSE : IF_TRUE);
	else
		return ( val ? IF_TRUE : IF_FALSE);
}

static int
chk_file_match( s, type, pdp)
char		*s;
int		type;
Parsedata 	*pdp;
{
	FILE	*fp;
	int	notflg,
		val;

	struct regprog	*rp;

	char	*cp,
		*item,
		buf[MIDLEN],
		file[MIDLEN],
		linebuf[SMALLLEN];


	switch ( type) {
	case MATCH_ACCEPT:
		item = inheadp->accept;
		break;
	case MATCH_COOKIE:
		item = inheadp->cookie;
		break;
	case MATCH_UA:
		item = inheadp->ua;
		break;
	case MATCH_REFERRER:
		item = inheadp->referrer;
		break;
	case MATCH_IP:
		item = remaddr;
		break;
	case MATCH_QUERY:
		item = this_rp->query;
		break;
	case MATCH_REQUEST:
		item = this_rp->request;
		break;
	case MATCH_PARAM_FIELD:
		item = this_rp->param_field;
		break;
	case MATCH_PARAM_VALUE:
		item = this_rp->param_value;
		break;
	case MATCH_HOST:
		get_remote_info();
		item = remotehost;
		break;
	case MATCH_FIELD:
		item = this_rp->field[fieldnum];
		break;
	default:
		parse_html_err( NULLTOKEN, pdp);
		return (IF_ERR);
	}


	if ( (cp = strchr( s, '"')) == NULL ) {
		logerr( PARSE_ERRMSG6, s);
		return IF_ERR;
	}
	mystrncpy( file, ++cp, MIDLEN);	

	if ( (cp = strchr( file, '"')) == NULL ) {
		logerr( PARSE_ERRMSG6, s);
		return IF_ERR;
	}
	*cp = '\0';

	if ( getfpath2( buf, file, this_rp) == FALSE) {
		logerr( PARSE_ERRMSG6, s);
		return IF_ERR;
	}
	if ( serv_perm & WN_COMP_UID )
		check_perm( this_rp, buf);

	if ((fp = fopen( buf, "r")) == (FILE *)NULL ) {
		logerr( PARSE_ERRMSG9, buf);
		return IF_ERR;
	}

	while ( fgets( linebuf, SMALLLEN, fp)) {
		if ( !chop( linebuf)) {
			logerr( PARSE_ERRMSG8, buf);
			return IF_ERR;
		}

		cp = linebuf;
		if ( notflg = ( *cp == '!'))
			cp++;

		if ( !*cp || (*cp == '#') )
			continue;

		if ( (rp = regcomp( linebuf)) == NULL ) {
			logerr( PARSE_ERRMSG7, s);
			return IF_ERR;
		}

		val = regfind( rp, item);

		if ( (notflg ? !val : val) ) {
			fclose( fp);
			return IF_TRUE;
		}
	}
	fclose( fp);
	return IF_FALSE;
}
