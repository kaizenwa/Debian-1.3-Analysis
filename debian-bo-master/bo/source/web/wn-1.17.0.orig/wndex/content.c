/*
    Wn: A Server for the HTTP
    File: wndex/content.c
    Version 1.14.1
    
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

#define WNDEX

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "wndex.h"
#include "content.h"

#define NOTHING		(0)
#define EXPIRES		(1)
#define KEYWORDS	(2)

extern char	*malloc();

static void	dometa();

static char	*findword();



void
getcontent( ep)
Entry	*ep;
{

	register char	*cp;

	char	suffix[SMALLBUF],
		buf[SMALLBUF];
	int	i = 0;

	if ( ep->flag & WN_ISURL)
		return;
	mystrncpy( buf, ep->file, SMALLBUF);
	strlower( buf);

	cp = strrchr( buf, '.');

	if ( cp == NULL ) { /*There's no suffix */
		if ( !hascontent( ep))
			addpair( "content", ep->defaultcontent, ep);
		return;
	}
	*cp++ = '\0';
	strcpy( suffix, cp);


	while ( (!hasencoding( ep)) && enclist[i][0] != NULL) {
		if ( streq( enclist[i][0], suffix)) {
			addpair( "encoding", enclist[i][1], ep);
			cp = strrchr( buf, '.');
			if ( cp == NULL	) {
				addpair( "content", ep->defaultcontent, ep);
				strcpy( ep->content, ep->defaultcontent);
				return;
			}
			strcpy( suffix, ++cp);
			break;
		}
		i++;
	}

	if ( hascontent( ep))
		return;
	i = 0;
	while ( list[i][0] != NULL) {
		if ( streq( list[i][0], suffix)) {
			addpair( "content", list[i][1], ep);
			strcpy( ep->content, list[i][1]);
			return;
		}
		i++;
	}

	if ( strcasecmp( suffix, "map") == 0) {
		ep->attributes |=  WN_ISMAP;
		addpair( "content", "text/plain", ep);
		strcpy( ep->content, "text/plain");
		return;
	}

	addpair( "content", ep->defaultcontent, ep);
	strcpy( ep->content, ep->defaultcontent);
	return;
}


void
loadmime()
{
	
	register char	*cp;
	char	buf[SMALLLEN];
	FILE	*mimefp;
	int	i = 0;

	if ( (mimefp = fopen( MIME_TYPE_FILE, "r")) == (FILE *) NULL) {
		if ( verboseflg)
			fprintf(stderr, ERRMSG9, MIME_TYPE_FILE );
		return;
	}

	while ( fgets( buf, SMALLLEN, mimefp)) {
		chop( buf);
		if ( !buf[0] || buf[0] == '#')
			continue;

		if ( (list[i][0] = malloc( SMALLBUF)) == NULL) {
			fprintf(stderr, ERRMSG10);
			exit( 2);	
		}
		strcpy( list[i][0], buf);
			
		if ( (cp = strchr( list[i][0], '\t')) == NULL) {
			fprintf(stderr, ERRMSG11, MIME_TYPE_FILE );
			fprintf(stderr, "Line = %s\n", buf);
			exit( 2);
		}

		*cp++ = '\0';
		list[i][1] = cp;
		i++;
		if ( i >= MAXMIME) {
			fprintf(stderr, ERRMSG12);
			exit( 2);
		}
	}
	list[i][0] = list[i][1] = NULL;
	fclose( mimefp);
}

void
getkeytitle( ep)
Entry	*ep;
{

	register char	*cp,
			*cp2;

	int	i = 0;

	FILE	*fp;
	char	filepath[MIDLEN],
		tbuf[MIDLEN],
		buf[MIDLEN];

	if ( ep->flag & WN_ISURL)
		return;

	strcpy( filepath, ep->cachefpath);
	if ( (cp = strrchr( filepath, '/')) == NULL) {
		strcpy( filepath, "./");
		cp = filepath + 1;
	}

	strcpy( ++cp, ep->file);

	if ( (fp = fopen( filepath, "r")) == (FILE *) NULL ) {
		if ( (!quiet) && (!ep->foundtitle) 
				&& (!strstr( ep->cacheline, "&redirect=")) ) {
			fprintf( stderr, ERRMSG14, filepath);
		}
		if ( !ep->foundtitle) {
			sprintf( buf, "File %s", ep->file);
			addpair ("title", buf, ep);
			strcpy( ep->title, buf);
		}
		return;
	}

	while ( fgets( buf, MIDLEN, fp) && i < NUM_TITLE_LINES ) {
		chop( buf);
		i++;
		if ( (!ep->foundtitle) && (cp = findword( buf, "<title")) ) {
			cp += 7;
			tbuf[0] = '\0';
			while ( cp && !(cp2 = findword( cp, "</title>")) ) {
				if ( strlen( tbuf) + strlen( cp) >= MIDLEN) {
					fprintf( stderr, ERRMSG17, tbuf);
					cp = cp2 = NULL;
					break;
				}
				strcat( tbuf, cp);
				if ( *cp)
					strcat( tbuf, " ");
				if ( (cp = fgets( buf, MIDLEN, fp)) == NULL)
					break;
				chop( buf);
				i++;
			}
			if ( cp2 && cp && (cp2 >= cp) ) {
				*cp2 = '\0';
				strcat( tbuf, cp);
				strcpy( buf, cp2 + 8);
				/* copy remainder of line to buf */
			}

			addpair( "title", tbuf, ep);
			strcpy( ep->title, tbuf);
			ep->foundtitle = TRUE;
			
		}

		if ( cp = findword( buf, "<meta") )
			dometa( cp, ep);

		if ( ep->foundkey && ep->foundtitle && ep->foundexp)
			break;
		if ( findword( buf, "</head>") ) 
			break;

	}
	if ( !*ep->title && !quiet && verboseflg) {
		fprintf( stderr, ERRMSG15, filepath);
	}
	if ( !*ep->title ) {
		sprintf( buf, "File %s", ep->file);
		addpair ("title", buf, ep);
		strcpy( ep->title, buf);
	}
	fclose( fp);
}


static void
dometa( linebuf, ep)
char	*linebuf;
Entry	*ep;
{
	register char	*cp,
			*cp2;

	int		httpequiv = NOTHING;

	if ( (cp = strchr( linebuf, '=')) == NULL )
		return;
	cp++;
	while ( isspace( *cp) || *cp == '"')
		cp++;
	if ( strncasecmp( cp, "keywords", 8) == 0 )
		httpequiv = KEYWORDS;
	if ( strncasecmp( cp, "expires", 7) == 0 )
		httpequiv = EXPIRES;
	if ( (cp = strchr( cp, '=')) == NULL )
		return;
	cp++;
	while ( isspace( *cp) || *cp == '"')
		cp++;
	if ( (cp2 = strchr( cp, '"')) == NULL )
		return;
	*cp2 = '\0';
	switch( httpequiv) {
	case KEYWORDS:
		if ( ep->foundkey)
			break;
		addpair( "keywords", cp, ep);
		ep->foundkey = TRUE;
		break;

	case EXPIRES:
		if ( ep->foundexp)
			break;
		addpair( "expires", cp, ep);
		ep->foundexp = TRUE;
		break;
	}
}

static char *
findword( line, word)
char	*line,
	*word;
{
	char	*cp,
		wordbuf[SMALLLEN],
		buf[MIDLEN];

	mystrncpy( buf, line, MIDLEN);
	mystrncpy( wordbuf, word, SMALLLEN);
	strlower( buf);
	strlower( wordbuf);
	if ( (cp = strstr( buf, wordbuf)) == NULL)
		return NULL;
	return ( line + ( cp - buf));
}

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

#ifdef NEED_STRCASECMP

/*
 *  Case insensitive comparison of two strings
 */

int
strcasecmp( s1, s2)
char	*s1,
	*s2;

{
	int	r;

	while ( *s1 && *s2 ) {
		if ( (r = (tolower( *s1) - tolower( *s2))) != 0 )
			return r;
		s1++;
		s2++;
	} 
	return ( *s1 - *s2);
}
#endif
