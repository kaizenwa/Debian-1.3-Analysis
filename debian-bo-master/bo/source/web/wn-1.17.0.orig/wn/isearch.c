/*
    Wn: A Server for the HTTP
    File: wn/isearch.c
    Version 1.15.6
    
    Copyright (C) 1995  <by John Franks>

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
#include "regi.h"

/* Do index search of directory pointed to by ip->filepath */

void
send_isearch( ip)
Request	*ip;
{
	FILE	*sfp;

	int	c;

	char	linebuf[MIDLEN],
		querybuf[MIDLEN];

	*ip->length = '\0';
	check_query( ip, (struct regprog **)NULL, (struct regprog **)NULL);
	if ( *(ip->query)) {
		strcpy( querybuf, "QUERY_STRING=");
		mystrncat( querybuf, ip->query, MIDLEN - 20);
		putenv( querybuf);
	}

	cgi_env( ip, FALSE);  /* Full CGI environ */


	if ( dir_p->attributes & WN_DIRNOSEARCH) {
		senderr( "403", ERRMSG32, ip->relpath);
		wn_exit( 0);
	}

	search_prolog( ip, OUTMSG1);  

	if ( (sfp = popen( dir_p->indexmod, "r")) == (FILE *) NULL ) {
		senderr( SERV_ERR, ERRMSG33, dir_p->indexmod);
		wn_exit( 2);
	}

        if ( ( c = getc( sfp)) == EOF) {
		if ( pclose( sfp) < 0 ) {
			senderr( SERV_ERR, ERRMSG44, dir_p->indexmod);
			wn_exit( 2);
		}
		send_nomatch( ip, 'd');
		return;
	}
	else
                ungetc( c, sfp);

	if ( dir_p->attributes & WN_DIRWRAPPED) 
		do_swrap( ip);

	while ( fgets( linebuf, MIDLEN, sfp))
		send_text_line( linebuf);

	if ( dir_p->attributes & WN_DIRWRAPPED) {
		do_swrap( ip);
	}
	else
		search_epilog( );

	writelog(  ip, LOGMSG11, "");

}


