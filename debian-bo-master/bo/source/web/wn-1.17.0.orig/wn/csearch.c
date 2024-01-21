/*
    Wn: A Server for the HTTP
    File: wn/csearch.c
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
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include "wn.h"
#include "search.h"
#include "parse.h"
#include "reg.h"
#include "regi.h"

static char	*get_next_dir();

typedef struct Link_entry {
	char	line[BIGLEN],
		*url,
		*title,
		*keywords;
} Link_entry;


static void	send_match_line();

static int	csearch(),
		do_list_search();

static struct regprog	*regp;

static Dir_info	*dirinfo_p;

/*
 * If ip->query is empty send a form to get it.  If there, strlower
 * it and do regcomp putting result in *rp.  Then, if needed do
 * an amperline and regcomp again with result in *rp2.  If rp == rp2
 * don't do the 2nd regcomp.  If it isn't done set *rp2 = *rp
 * If both rp and rp2 are NULL don't do any regcomp stuff.
 */

void
check_query( ip, rp, rp2)
Request	*ip;
struct regprog	**rp,
		**rp2;
{
	char	buf[BIGLEN],
		owner[SMALLLEN],
		str[SMALLLEN],
		*cp;

	if ( !*ip->query) {
		bzero( (char *) outheadp, sizeof( Outheader));
		ip->content_type = "text/html";
		http_prolog( );
		cp = ( *dir_p->dir_owner ? dir_p->dir_owner : MAINTAINER);
		strcpy( owner, "<link rev=\"made\" href=\"");
		mystrncat( owner, cp, SMALLLEN - 30);
		strcat( owner, "\">\n");
		sprintf(str, REGMSG1);
		sprintf(buf,"<html>\n<head>\n<title> %s </title>\n", str);
		send_text_line( buf);
		sprintf(buf,"%s</head>\n<body>\n<h2>%s</h2>\n", owner, str);
		send_text_line( buf);
		sprintf(buf, REGRES5);
		send_text_line( buf);
		send_text_line( REGRES4);
		send_text_line( SERVER_LOGO);
		send_text_line( "</body>\n</html>\n" );
		writelog( ip, LOGMSG2, ip->relpath);
		wn_exit( 0);
	}

	www_unescape( ip->query, ' ');

	if ( rp && (*rp = regcomp( strlower( ip->query))) == NULL ) {
		senderr( SERV_ERR, ERRMSG81,
			ip->query);
		wn_exit( 2);
	}

	if ( rp == rp2)
		return;
	if ( amperline( buf, ip->query)) {
				/* amper escape '<', '>' and '&' */
		if ( (*rp2 = regcomp( strlower( buf))) == NULL ) {
			senderr( SERV_ERR, ERRMSG81, ip->query);
			wn_exit( 2);
		}
	}
	else if ( rp) {
		*rp2 = *rp;
	}
}


void
cache_search( ip)
Request	*ip;
{
	char	*cp,
		buf[BIGLEN],
		cdirpath[BIGLEN];
	Dir_info	csearch_dir;

	if ( !iswndir( ip)) {
		senderr(CLIENT_ERR, ERRMSG59, ip->request);
		wn_exit( 2);
	}
	
	dirinfo_p = &csearch_dir;
	check_query( ip, &regp, &regp);
	mystrncpy( cdirpath, ip->cachepath, MIDLEN);
	if ( (cp = strrchr( cdirpath, '/')) != NULL)
		*cp = '\0';
	else
		*cdirpath = '\0';

	sprintf(buf, "Document Title/Keyword Search");
	search_prolog( ip, buf);

	if (!csearch( cdirpath, "", ip, MAXDEPTH)) {
		send_nomatch( ip, 'd');
		return;
	}

	send_text_line( "</ul>\n");
	if ( isdirwrapped( dir_p))
		do_swrap( ip);
	else
		search_epilog( );

	writelog( ip, LOGMSG3, ip->relpath);
}

void
send_nomatch( ip, type)
Request	*ip;
char	type;
{
	char	buf[BIGLEN];

	if ( (type == 'd') && *(dir_p->nomatchsub)) {
		if ( isdirwrapped( dir_p)) {
			do_nomatchsub( ip, dir_p->nomatchsub);
			return; /* to cache_search, list_search etc.*/
		}
		else
			logerr( ERRMSG89, ip->relpath);
	}
	else if ( (type == 'f') && *(ip->nomatchsub)) {
		if ( *ip->swrapper) {
			do_nomatchsub( ip, ip->nomatchsub);
			return; /* to cache_search, list_search etc.*/
		}
		else
			logerr( ERRMSG89, ip->relpath);
	}

	send_text_line("<hr>\n<h2>Unsuccessful Search</h2>\n");
	sprintf( buf, REGMISS, ip->query);
	send_text_line( buf);
	send_text_line( REGRES2);
	if ( type == 'd' )
		sprintf(buf, REGRES3, ip->param_value);
	else
		sprintf(buf, REGRES5);
	send_text_line( buf);
	send_text_line( REGRES4);
	send_text_line( SERVER_LOGO);
	send_text_line( "</body>\n</html>\n" );

	writelog( ip, LOGMSG4, ip->relpath);
	return; /* to cache_search, list_search etc.*/
}


/*
 * search_prolog( ip, str) provides the response to a successful search.
 * If the file is wrapped don't do anything as the wrapper is
 * assumed to handle any messages.
 */

void
search_prolog(ip, str)
Request	*ip;
char	*str;
{

	char	*cp,
		owner[BIGLEN],
		buf[2*BIGLEN];

	if ((iswndir( ip)) ?  isdirwrapped( dir_p) :
			( ip->attributes & WN_SWRAPPED))
		return;

	http_prolog( );
		
	cp = ( *dir_p->dir_owner ? dir_p->dir_owner : MAINTAINER);
	sprintf( owner, "<link rev=\"made\" href=\"%s\">\n", cp);
	sprintf(buf,"<html>\n<head>\n<title> %s </title>\n", str);
	send_text_line( buf);
	sprintf(buf,"%s</head>\n<body>\n<h2>%s</h2>\n", owner, str);
	send_text_line( buf);
}	


void
search_epilog( )
{
	send_text_line( SERVER_LOGO);
	send_text_line( "</body>\n</html>\n" );
}


/*
 * csearch( cdir, srel, ip, depth) searches control cache in directory cdir for
 * lines which match the compiled regular expression pointed to by
 * static pointer regp.  When found call send_match_line.
 * For subdirs listed in the control cache
 * recursively call csearch.  Note: with symbolic links there could be
 * an infinite loop of "subdirectories" -- to handle this problem, go
 * only depth levels deep in the recursion.  This will still result in
 * many matches for the same item.
 */

static int
csearch( cdir, srel, ip, depth)
char	*cdir,
	*srel;
Request	*ip;
int	depth;
{
	int	ismatch;
	FILE	*cfp;

	Cache_entry	entry,
			*cep;

	int	foundmatch = FALSE,
		i;

	char	*endpath,
		*endrelpath,
		*dp,
		*dirp,
		entryline[CACHELINE_LEN],
		srelpath[MIDLEN],
		cdirbuf[MIDLEN],
		dirlist[MIDLEN],
		cfpath[MIDLEN + SMALLLEN],
		cbuf[MIDLEN];

	if ( depth <= 0 )
		return FALSE;

	mystrncpy( cdirbuf, cdir, MIDLEN);

	endpath = cdirbuf;
	while ( *endpath) /* endpath should point to / before cache name */
		endpath++;
	*endpath = '/';
	*(endpath + 1) = '\0';

	mystrncpy( srelpath, srel, MIDLEN);
	endrelpath = srelpath;
	while ( *endrelpath) 
		endrelpath++;
	if ( *srel) {
		*endrelpath++ = '/';
		*endrelpath = '\0';
	}
	/* endrelpath now points AFTER the '/' at the end */

	sprintf( cfpath, "%s%s", cdirbuf, cfname);

	if ( (cfp = fopen( cfpath, "r")) == (FILE *) NULL ) {
		logerr( ERRMSG41, cfpath);
		return FALSE;
	}


	read_cache_dirinfo( cfp, dirinfo_p);

	if ( (dirinfo_p->attributes & WN_DIRNOSEARCH) || 
			( *dirinfo_p->authmod
			&& !streq( dir_p->authrealm, dirinfo_p->authrealm))) {
		fclose( cfp);
		return FALSE;
	}

	/* Get list of subdirs from dir record in first line of index.cache */
	mystrncpy( dirlist, dirinfo_p->subdirs, MIDLEN);

	dirp = dirlist;
	cep = &entry;
	cep->line = entryline;


	while ( read_cache_file( cep, cfp, (char *) NULL)) {
		if ( cep->attributes & WN_NOSEARCH)
			continue;
		if ( *(cep->redirect))
			continue;

		mystrncpy( cbuf, entry.title, MIDLEN);

		switch( ip->type) {
		case RTYPE_TSEARCH:
			ismatch = regfind( regp, strlower( cbuf));
			break;
		case RTYPE_KSEARCH:
			ismatch = regfind( regp, strlower(entry.keywords));
			break;
		case RTYPE_TKSEARCH:
			ismatch = regfind( regp, strlower( cbuf))
				|| regfind( regp, strlower( entry.keywords));
			break;
		case RTYPE_FIELDSEARCH:
			i = atoi( ip->param_value + 5);
			if ( (i < 0) || (i >= NUMFIELDS)) {
				senderr( SERV_ERR, "Bad search parameter", "");
				wn_exit(2);
			}
			ismatch = regfind( regp, strlower(entry.field[i]));
			break;
		default:
			break;
		}
		if ( ismatch ) {
			foundmatch |= ismatch;
			send_match_line( ip, cep, srelpath);
		}
	}

	while ( dp = get_next_dir(  &dirp)) {
		mystrncpy( endpath + 1, dp, SMALLLEN);
		mystrncpy( endrelpath, dp, SMALLLEN);
		foundmatch |= csearch( cdirbuf, srelpath, ip,  depth - 1);
	}

	fclose( cfp);
	return foundmatch;
}

static char *
get_next_dir( dpp)
char	**dpp;
{
	register char	*cp;

	char	*dp2;

	cp = *dpp;
	if ( !*cp)
		return NULL;
	while ( *cp && ( (*cp == ',')  || isspace( *cp)))
		cp++;	/* Skip leading space or commas */
	dp2 = cp;

	while ( *cp && (*cp != ',') && !(isspace( *cp)))  
		cp++;
	if ( *cp ) {
		*cp = '\0';
		*dpp = ++cp;
	}
	else
		*dpp = cp;

	return dp2;
}



static void
send_match_line( ip, cep, srelpath)
Request	*ip;
Cache_entry	*cep;
char		*srelpath;
{
	static int	first_matched_line = TRUE;
	char		buf[MIDLEN];

	if ( first_matched_line) {
		first_matched_line = FALSE;
		if ( isdirwrapped( dir_p)) {
			do_swrap( ip);
			send_text_line( "<ul>\n");
		}
		else {
			send_text_line( REGRES2);
			sprintf(buf, REGRES3, ip->param_value);
			send_text_line( buf);
			send_text_line( REGRES4);
			sprintf(buf, REGRES1, ip->query);
			send_text_line( buf);
			send_text_line( "<ul>\n");
		}

	}
	if ( *cep->basename) {
		sprintf( buf, "<li><a href=\"%s%s\"> %s </a>\n",
				srelpath, cep->basename, cep->title);
		send_text_line( buf);
	}
	else {
		sprintf( buf, "<li><a href=\"%s\"> %s </a>\n",
				cep->url, cep->title);
		send_text_line( buf);
	}
}


void
list_search( ip)
Request	*ip;
{
	FILE	*gfp;

	int	found = FALSE;

	check_query( ip, &regp, &regp);

	if ( (ip->attributes & WN_NOSEARCH) || 
			!(ip->filetype & WN_TEXT)) {
		senderr( "403", ERRMSG51, ip->relpath);
		wn_exit( 0);
	}

	search_prolog( ip, "File Index Search");

	check_perm( ip, ip->filepath);
        if ( (gfp = fopen( ip->filepath, "r")) == (FILE *) NULL )
		senderr( SERV_ERR, ERRMSG1, ip->relpath);

	found = do_list_search( ip, regp, gfp);

	fclose( gfp);

	if ( !found) {
		send_nomatch( ip, 'f');
		return;
	}
	
	if ( ip->attributes & WN_SWRAPPED)
		do_swrap( ip);
	else
		search_epilog( );

	writelog(  ip, "Sent list search", "");
}

static int
do_list_search( ip, rp, fp)
Request		*ip;
struct regprog	*rp;
FILE	*fp;
{
	register char	*cp,
			*cp2;

	char	linebuf[BIGLEN],
		buf[BIGLEN];
	int	fnd = FALSE,
		first = TRUE;


	while ( fgets( linebuf, BIGLEN, fp)) {
		cp = linebuf;
		while ( isspace( *cp))
			cp++;
		if ( strncasecmp( cp, "<li>", 4))
			continue;
		strcpy( buf, cp + 4);
		if ( (cp2 = strchr( buf, '>')) == NULL )
			continue;
		if ( (cp = strchr( ++cp2, '<')) == NULL )
			continue;
		*cp = '\0';
		strlower( cp2);
		if ( regfind( rp, cp2) ) {
			if ( first && ip->attributes & WN_SWRAPPED) {
				do_swrap( ip);
				send_text_line( "<ul>\n");
			}
			else if ( first) {
				char		mbuf[MIDLEN];

				send_text_line( REGRES2);
				sprintf(mbuf, REGRES5);
				send_text_line( mbuf);
				send_text_line( REGRES4);
				sprintf(buf, REGRES1, ip->query);
				send_text_line( buf);
				send_text_line( "<ul>\n");
			}
			send_text_line( linebuf);
			first = FALSE;
			fnd =  TRUE;
		}
	}
	if ( fnd)
		send_text_line( "</ul>\n");
	return fnd;
}

