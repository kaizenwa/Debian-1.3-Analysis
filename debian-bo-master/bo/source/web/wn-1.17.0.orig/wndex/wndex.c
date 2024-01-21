/*
    Wn: A Server for the HTTP
    File: wndex/wndex.c
    Version 1.17.0
    
    Copyright (C) 1995, 1996  <by John Franks>

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

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include "wndex.h"
#include "reg.h"
#include "regi.h"


extern int	errno;

static FILE	*htmlopen();

static void	mkcache(),
		writetext(),
		writedir(),
		amperline(),
		htmlhead(),
		htmlclose(),
		do_recurse(),
		do_maxage(),
		onlyfirst(),
		doattrib(),
		doattrib_dir(),
		despace(),
		clear();

Entry		top;


main( argc, argv)
int	argc;
char	*argv[];
{

	Entry	*ep;

	ep = &top;
	clear( ep);
	init( argc, argv);
	mkcache( ep);
	return (0);
}


static void
clear( ep)
Entry	*ep;
{
	ep->content[0] = ep->cacheline[0] = ep->url[0]
			= ep->file[0] = ep->title[0] = '\0';
	ep->flag = ep->attributes = 0;
	ep->isindexfile = ep->foundtitle = ep->foundkey 
			= ep->foundexp = FALSE;
	ep->attributes = ep->defattributes = 0;
}

static void
mkcache( ep)
Entry	*ep;
{
	FILE	*cntlfp,
		*cachefp,
		*htmlfp;

	char	*cp,
		*text,
		*bufp,
		buf[MIDLEN],
		mbuf[MIDLEN],
		tmpfpath[MIDLEN],
		htmlpath[MIDLEN];


	ep->owner[0] = '\0';
	ep->firsttime = TRUE;
	ep->serveall = FALSE;
	clear_slist();
	strcpy( ep->defaultcontent, DEFAULT_CONTENT_TYPE);
	ep->doindex = ep->inlist = FALSE;
	
	mystrncpy( tmpfpath, ep->cachefpath, MIDLEN - 20);
	if ( (cp = strrchr( tmpfpath, '/')) == NULL )
		mystrncpy( tmpfpath, INDEX_TMPFILE, 20);
	else
		mystrncpy( cp + 1, INDEX_TMPFILE, 20);
		

	if ( stdioflg)
		cntlfp = stdin;
	else if ( (cntlfp = fopen( ep->cntlfpath, "r")) == (FILE *) NULL ) {
		fprintf( stderr, ERRMSG2, ep->cntlfpath); /* can't open */
		return;
	}

	if ( stdioflg)
		cachefp = stdout;
	else if ( (cachefp = fopen( tmpfpath, "w")) == (FILE *) NULL ) {
		fprintf( stderr, ERRMSG26, tmpfpath);  /* can't open */
		return;
	}

	while ( bufp = get_next_line(mbuf, cntlfp)) {
		cp = strchr( bufp, '=');
		*cp++ = '\0';
		text = cp;
		strlower( bufp);
		while ( (cp = strchr( bufp, '-')) != NULL )
			strcpy( cp, cp + 1);  /* delete any '-' */

		if ( streq( bufp, "indexfile")) {
			if ( ep->firsttime)
				writedir( cachefp, ep);
			else {
				fprintf( stderr, ERRMSG3);
				exit( 2);
			}
			ep->doindex = ep->isindexfile = TRUE;
			mystrncpy( ep->file, text, SMALLLEN);
			/* used by getcontent() */

			strcpy( htmlpath, ep->cachefpath);
			cp = strrchr( htmlpath, '/');
			strcpy( ++cp, text);

			add_to_slist( text);
			addpair( "file", text, ep);
			ep->flag |= WN_NOINDEX;

			htmlfp = htmlopen( htmlpath);

			continue;
		}

		if ( streq( bufp, "file") || streq( bufp, "link")) {
			if ( ep->firsttime) {
				writedir( cachefp, ep);
			}
			if ( *ep->cacheline)
				writeitem( cachefp, htmlfp, ep);
			mystrncpy( ep->file, text, SMALLLEN);
			add_to_slist( text);
			addpair( "file", text, ep);
			if ( *bufp == 'l')	/* It's Link= */
				ep->flag |= WN_ISLINK;
			continue;
		}

		if ( streq( bufp, "url")) {
			if ( ep->firsttime) {
				writedir( cachefp, ep);
			}
			if ( *ep->cacheline)
				writeitem( cachefp, htmlfp, ep);
			addpair( "url", text, ep);
			strcpy( ep->url, text);
			continue;
		}

		if ( streq( bufp, "text")) {
			if ( ep->firsttime) {
				writedir( cachefp, ep);
			}
			if ( *ep->cacheline)
				writeitem( cachefp, htmlfp, ep);

			if ( ep->doindex )
				writetext( cntlfp, htmlfp, ep);
			else
				fprintf( stderr, ERRMSG4);
			continue;
		}

		if ( streq( bufp, "authorizationrealm")) {
			if ( ep->firsttime)
				addpair( "authrealm", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "authdeniedfile")) {
			if ( ep->firsttime)
				addpair( "authdenied_file", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "authorizationmodule")) {
			if ( ep->firsttime)
				addpair( "authmod", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "authorizationtype")) {
			if ( ep->firsttime)
				addpair( "authtype", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "nosuchfileurl")) {
			if ( ep->firsttime)
				addpair( "nofile_url", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "accessdeniedurl")) {
			if ( ep->firsttime)
				addpair( "noaccess_url", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "cachemodule")) {
			if ( ep->firsttime)
				addpair( "cachemod", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "filemodule")) {
			if ( ep->firsttime)
				addpair( "filemod", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "searchmodule")) {
			if ( ep->firsttime)
				addpair( "indexmod", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}
		if ( streq( bufp, "accessfile")) {
			if ( ep->firsttime)
				addpair( "accessfile", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "owner")) {
			if ( ep->firsttime) {
				addpair( "owner", text, ep);
				strcpy( ep->owner, text);
			}
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "subdirs")) {
			if ( ep->firsttime)
				addpair( "subdirs", text, ep);
			else
				onlyfirst( bufp);

			if ( recurse)
				do_recurse( text, ep);
			continue;
		}

		if ( streq( bufp, "title")) {
			if ( ep->firsttime) {
				fprintf( stderr, ERRMSG23);
				exit( 2);
			}
			addpair( "title", text, ep);
			strcpy( ep->title, text);
			ep->foundtitle = TRUE;
			continue;
		}

		if ( streq( bufp, "header")) {
			addpair( "header", text, ep);
			continue;
		}

		if ( streq( bufp, "refresh")) {
			strcpy( buf, "Refresh: ");
			strcat( buf, text);
			addpair( "header", buf, ep);
			continue;
		}

		if ( streq( bufp, "setcookie")) {
			strcpy( buf, "Set-Cookie: ");
			strcat( buf, text);
			addpair( "header", buf, ep);
			continue;
		}

		if ( streq( bufp, "parse")) {
			ep->attributes |= WN_PARSE;
			continue;
		}

		if ( streq( bufp, "redirect")) {
			addpair( "redirect", text, ep);
			continue;
		}

		if ( streq( bufp, "keywords")) {
			ep->foundkey = TRUE;
			addpair( "keywords", text, ep);
			continue;
		}

		if ( strncmp( bufp, "field", 5) == 0) {
			addpair( bufp, text, ep);
			continue;
		}

		if ( streq( bufp, "contenttype")) {
			addpair( "content", text, ep);
			strcpy( ep->content, text);
			strlower( ep->content);
			ep->flag |= WN_HASCONTENT;
			continue;
		}

		if ( streq( bufp, "contentencoding")
			|| streq( bufp, "encoding")) {
			if ( !streq( text, "none")) {
				addpair( "encoding", text, ep);
			}
			ep->flag |= WN_HASENCODING;
			continue;
		}

		if ( streq( bufp, "includes")) {
			despace( text);
			addpair( "includes", text, ep);
			ep->attributes |= WN_INCLUDE;
			continue;
		}

		if ( strncmp( bufp, "wrapper", 7) == 0 ) {
			despace( text);
			if ( ep->firsttime)
				addpair( "dwrapper", text, ep);
			else {
				ep->attributes |= WN_WRAPPED;
				addpair( "wrappers", text, ep);
			}
			continue;
		}

		if ( streq( bufp, "searchwrapper")) {
			if ( ep->firsttime)
				addpair( "dwrapper", text, ep);
			else {
				ep->attributes |= WN_SWRAPPED;
				addpair( "swrapper", text, ep);
			}
			continue;
		}

		if ( streq( bufp, "emptysub")) {
			addpair( "nomatchsub", text, ep);
			continue;
		}

		if ( streq( bufp, "nomatchsub")) {
			addpair( "nomatchsub", text, ep);
			continue;
		}

		if ( streq( bufp, "filter")) {
			ep->attributes |= WN_FILTERED;
			addpair( "filter", text, ep);
			continue;
		}

		if ( streq( bufp, "expires")) {
			addpair( "expires", text, ep);
			ep->foundexp = TRUE;
			continue;
		}
		if ( streq( bufp, "maxage")) {
			char	minbuf[TINYLEN];

			do_maxage( text, minbuf);
			addpair( "maxage", minbuf, ep);
			ep->foundexp = TRUE;
			continue;
		}
		if ( streq( bufp, "nosearch")) {
			ep->attributes |= WN_NOSEARCH;
			continue;
		}

		if ( streq( bufp, "defaultcontent")) {
			/* must go to server because of serveall */
			strlower( text);
			if ( ep->firsttime) {
				strcpy( ep->defaultcontent, text);
				addpair( "default_content", text, ep);
			}
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "defaultwrapper")) {
			/* must go to server because of serveall */
			strlower( text);
			if ( ep->firsttime) {
				strcpy( ep->defwrapper, text);
				addpair( "defwrapper", text, ep);
			}
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "defaultincludes")) {
			/* must go to server because of serveall */
			strlower( text);
			if ( ep->firsttime) {
				strcpy( ep->defincludes, text);
				addpair( "defincludes", text, ep);
			}
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "defaultdocument")) {
			if ( ep->firsttime)
				addpair( "default_document", text, ep);
			else
				onlyfirst( bufp);
			continue;
		}

		if ( streq( bufp, "defaultmaxage")) {
			if ( ep->firsttime) {
				char	minbuf[TINYLEN];

				do_maxage( text, minbuf);
				addpair( "default_maxage", minbuf, ep);
			}
			else
				onlyfirst( bufp);
			continue;
		}

		if ( strncmp( bufp, "attribute", 9) == 0 ) {
			strlower( text);
			while ( (cp = strchr( text, '-')) != NULL )
				strcpy( cp, cp + 1);  /* delete any '-' */
			despace( text);
			if ( ep->firsttime)
				doattrib_dir( text, ep);
			else
				doattrib( text, ep, &(ep->attributes));
			continue;
		}

		if ( strncmp( bufp, "defaultattribute", 16) == 0 ) {
			if ( ep->firsttime) {
				strlower( text);
				while ( (cp = strchr( text, '-')) != NULL )
					strcpy( cp, cp + 1); /* delete '-' */
				despace( text);
				doattrib( text, ep, &(ep->defattributes));
			}
			else
				onlyfirst( bufp);
			continue;
		}

		fprintf( stderr, ERRMSG5, bufp);
	}	

	if ( ep->firsttime) {
		writedir( cachefp, ep);
	}
	if ( *ep->cacheline)
		writeitem( cachefp, htmlfp, ep);

	if ( ep->doindex && htmlfp) {
		htmlclose( htmlfp, ep);
		fclose( htmlfp);
	}
	if ( ep->serveall) {
		strcpy( buf, ep->cachefpath);
		if ( cp = strrchr( buf, '/'))
			*++cp = '\0';
		do_serveall( buf, cachefp, htmlfp, ep);
	}

	fclose( cntlfp);
	fclose( cachefp);

	if ( rename( tmpfpath, ep->cachefpath) != 0 ) {
		fprintf( stderr, ERRMSG25, tmpfpath, ep->cachefpath);
		return;
	}

	if ( !quiet)
		printf( MSG1, ep->cachefpath);

}

static void
do_recurse( subdirs, ep)
char	*subdirs;
Entry	*ep;
{
	int	done = FALSE;

	char	*cp,
		*cp2,
		*currsub,
		subs[MIDLEN];

	Entry	next;

	clear( &next);
	strcpy( subs, subdirs);
	cp = subs;
	while ( *cp ) {
		if ( isspace( *cp)) 
			strcpy( cp, cp + 1);
		cp++;
	}
	cp = subs;
	while ( !done && *cp  ) {
		if ( (cp2 = strchr( cp, ',')) == NULL)
			done = TRUE;
		else {
			*cp2 = '\0';
			cp2++;
		}
		currsub = cp;
		strcpy( next.cntlfpath, ep->cntlfpath);
		if ( (cp = strrchr( next.cntlfpath, '/')) == NULL ) {
			fprintf( stderr, ERRMSG6, ep->cntlfpath);
			return;
		}
		sprintf( cp, "/%s/%s", currsub, cntlfname);

		strcpy( next.cachefpath, ep->cachefpath);
		if ( (cp = strrchr( next.cachefpath, '/')) == NULL ) {
			fprintf( stderr, ERRMSG6, ep->cachefpath);
			return;
		}
		sprintf( cp, "/%s/%s", currsub, cachefname);
		mkcache( &next);
		clear( &next);
		cp = cp2;
	}
}



static void
onlyfirst( s)
char	*s;
{
	if ( quiet)
		return;
	fprintf( stderr, ERRMSG8, s);
}





void 
writeitem( cfp, hfp, ep)
FILE	*cfp,
	*hfp;
Entry	*ep;
{
	char	buf[MIDLEN];

	if ( ep->isindexfile && hfp)
		htmlhead( hfp, ep);
	getcontent( ep);
	if ( streq( ep->content, "text/html") && 
		((!ep->foundexp) || (!ep->foundtitle) || (!ep->foundkey)))
		getkeytitle( ep);

	if ( !*ep->title ) {
		sprintf( buf, "File %s", ep->file);
		addpair ("title", buf, ep);
		strcpy( ep->title, buf);
	}

	if ( ep->attributes) {
		sprintf( buf, "%ld", ep->attributes);
		addpair( "attributes", buf, ep);
	}

	if ( !(ep->flag & WN_ISLINK))
		fprintf( cfp, "%s\n", ep->cacheline);

	if ( ep->doindex && hfp && !(ep->flag & WN_NOINDEX) ) {
		if ( !ep->inlist) {
			ep->inlist = TRUE;
			fprintf( hfp, "<ul>\n");
		}	
		amperline( buf, ep->title);
		if ( *ep->file)
			fprintf( hfp, "<li> <a href=\"%s\">%s</a>\n",
				ep->file, buf);
		if ( *ep->url)
			fprintf( hfp, "<li> <a href=\"%s\">%s</a>\n",
				ep->url, buf);
	}
	clear( ep);
}


static void 
writedir( fp, ep)
FILE	*fp;
Entry	*ep;
{
	char	buf[MIDLEN];

	if ( ep->defattributes) {
		sprintf( buf, "%ld", ep->defattributes);
		addpair( "defattributes", buf, ep);
	}

	fprintf( fp, "%s\n\n", ep->cacheline);
	ep->firsttime = FALSE;
	clear( ep);
}

static void 
writetext( cfp, hfp, ep)
FILE	*cfp,
	*hfp;
Entry	*ep;
{
	char buf[MIDLEN];

	if ( !hfp)
		return;

	if ( ep->inlist) {
		ep->inlist = FALSE;
		fprintf( hfp, "</ul>\n\n");
	}

	while ( fgets( buf, MIDLEN, cfp)) {
		if ( strncasecmp( buf, "endtext=", 8) == 0 )
			break;
		fprintf( hfp, "%s", buf);
	}
	fprintf( hfp, "\n");
}

static void
htmlhead( hfp, ep)
FILE	*hfp;
Entry	*ep;
{
	char	buf[MIDLEN];

	if ( !hfp)
		return;
	amperline( buf, ep->title);
	if ( !*ep->owner)
		strcpy( ep->owner, MAINTAINER);
	fprintf( hfp, "<html>\n<head>\n<title>%s</title>\n", buf);
	fprintf( hfp, "<link rev=\"made\" href=\"%s\">\n", ep->owner);
	fprintf( hfp, "</head>\n<body>\n<h2>%s</h2>\n", buf);
}

static void
htmlclose( hfp, ep)
FILE	*hfp;
Entry	*ep;
{
	if ( !hfp)
		return;
	if ( ep->inlist)
		fprintf( hfp, "</ul>\n");
	fprintf( hfp, "</body>\n</html>\n");
}


/*
 * addpair( field, value, ep) 
 * Add &field=value to cacheline escaping any ampersands in
 * value and removing any trailing whitespace.
 */

void
addpair( field, value, ep)
char	*field,
	*value;
Entry	*ep;
{
	char	*cp,
		buf[BIGLEN];


	cp = value + strlen( value);
	if ( cp > value)
		cp--;
	while ( (*cp == ' ') && ( cp >= value ))
		*cp-- = '\0';

	cp = value;
	while ( (cp = strchr( cp, '&')) != NULL){
		strcpy( buf, cp);
		*cp++ = '\\';
		strcpy( cp, buf);
		cp++;
	}

	if ( strlen( ep->cacheline) + strlen( value) > CACHELINE_LEN - 20 ) {
		fprintf( stderr, ERRMSG1, ep->cacheline);
		exit( 2);
	}
	if ( *value ) {
		if ( *ep->cacheline) {
			strcat( ep->cacheline, "&");
		}
		strcat( ep->cacheline, field);
		strcat( ep->cacheline, "=");
		strcat( ep->cacheline, value);
	}
	else if ( !quiet) {
		fprintf( stderr, ERRMSG18, field, ep->cacheline);
	}
}

/* 
 * Read in line, skip lines with no "=" in them, deal with comments (#),
 * get rid of leading and trailing whitespace.  If line ends with '\' 
 * it continues on next line.  Maximum allowed size of a line is BIGLEN.
 */
			
char
*get_next_line( buf, fp)
char	*buf;
FILE	*fp;
{
	register char	*cp;

	char	*bufp,
		extrabuf[BIGLEN];

	while ( bufp = fgets( buf, BIGLEN, fp)) {
		chop( bufp);
		cp = buf + strlen(buf) - 1;
		while ( isspace( *cp) && ( cp >= buf))
			*cp-- = '\0';	/* remove trailing whitespace */

		while ( *cp == '\\') {
			*cp = '\0';
			fgets( extrabuf, BIGLEN, fp);
			chop( extrabuf);
			if ( strlen( buf) + strlen( extrabuf) 
						> CACHELINE_LEN - 20 ) {
				fprintf( stderr, ERRMSG1, buf);
				exit( 2);
			}
			cp = extrabuf;
			while ( isspace( *cp))
				cp++;
			strcat( buf, cp);
			cp = buf + strlen(buf) - 1;
		}
		if ( (cp = strchr( bufp, '#')) != NULL) {
			if ( ( cp != bufp) && (*(cp-1) == '\\'))
				strcpy( cp-1, cp);
			else
				*cp = '\0';
		}
		while ( (*bufp == '\t') || (*bufp == ' '))
			bufp++;
		if ( (cp = strchr( bufp, '=')) == NULL) {
			if ( !quiet && *bufp)
				fprintf( stderr, ERRMSG27, bufp);
			continue;
		}
		break;
	}
	return bufp;
}
	


static FILE
*htmlopen( path)
char	*path;
{
	FILE	*htmlfp;

	char	buf[SMALLLEN];

	strcpy( buf, path);
	strcat( buf, ".bak");

	if ( (rename( path, buf) < 0) && (errno != ENOENT)) {
		fprintf( stderr, ERRMSG16, path);
		return NULL;
	}
	if ( (htmlfp = fopen( path, "w")) == (FILE *) NULL ) {
		fprintf( stderr, ERRMSG2, path);
		return NULL;
	}
	if ( !quiet) 
		printf( MSG2, path);

	return htmlfp;
}


char *
strlower( st)
char	*st;
{
	register char	*cp;

	cp = st;
	while ( *cp) {
		*cp =  (isupper(*cp) ? *cp - 'A' + 'a' : *cp );
		cp++;
	}
	return (st);
}


/*
 * amperline( p1, p2)  Copy p2 to p1 until 
 * p2 is exhausted.  Encode '<', '>', and &. 
 */

static void
amperline ( p1, p2)
char	*p1,
	*p2;
{

	while ( *p2 ) {
		switch( *p2) {
		case '\\':
			if ( *(p2 + 1) == '&')
				strcpy( p2, p2+1);
			else
				*p1++ = *p2++;
			break;
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
			if ( isspace( *(p2 + 1))) {
				strcpy( p1, "&amp;");
				p1 += 5;
				p2++;
			}
			else
				*p1++ = *p2++;
			break;
		default:
			*p1++ = *p2++;
		}
	}
	*p1 = 0;
}


#ifdef NEED_STRNCASECMP

/*
 *  Case insensitive comparison of first n chars of two strings
 */

int
strncasecmp( s1, s2, n)
char	*s1,
	*s2;
int	n;

{
	int	r;

	while ( *s1 && *s2 && ( n > 0)) {
		if ( (r = (tolower( *s1) - tolower( *s2))) != 0 )
			return r;
		s1++;
		s2++;
		n--;
	}
	return ( n == 0 ? 0 : *s1 - *s2);
}
#endif


#ifdef NEED_STRSTR
/*
 * Find the first occurrence of find in s.
 *
 * For copyright, see ../wn/misc.c
 */
char *
strstr(s, find)
char	*s, *find;
{
	register char c,
	  sc;
	register size_t len;

	if ((c = *find++) != 0) {
		len = strlen(find);
		do {
			do {
				if ((sc = *s++) == 0)
					return (NULL);
			} while (sc != c);
		} while (strncmp(s, find, len) != 0);
		s--;
	}
	return ((char *) s);
}
#endif

/* Replace comma followed by spaces with comma only */

static void 
despace( s) 
char	*s;
{
	register char	*cp;

	cp = s;
	while ( *cp ) {
		if ( isspace(*cp) && (*(cp + 1) == ',')) {
			strcpy( cp, cp + 1);
			continue;
		}
		if ( (*cp == ',') && isspace(*(cp + 1))) {
			strcpy( cp + 1, cp + 2);
			continue;
		}
		cp++;
	}
}


static void
doattrib( text, ep, attrib)
char		*text;
Entry		*ep;
unsigned	*attrib;
{
	char	*word,
		*nextword;

	word = nextword = text;
	while ( nextword ) {
		word = nextword;
		if ( (nextword = strchr( word, ',')) != NULL)
			*nextword++ = '\0';

		if ( streq( word, "invisible") ) {
			ep->flag |= WN_NOINDEX;
			*attrib |= WN_NOSEARCH;
			continue;
		}
		if ( streq( word, "noindex") ) {
			ep->flag |= WN_NOINDEX;
			continue;
		}
		if ( streq( word, "nosearch") ) {
			*attrib |= WN_NOSEARCH;
			continue;
		}
		if ( streq( word, "imagemap") ) {
			*attrib |= WN_ISMAP;
			continue;
		}
		if ( streq( word, "dynamic") ) {
			*attrib |= WN_DYNAMIC;
			continue;
		}
		if ( streq( word, "nondynamic") ) {
			*attrib |= WN_NONDYNAMIC;
			continue;
		}
		if ( streq( word, "parse") ) {
			*attrib |= WN_PARSE;
			continue;
		}
		if ( streq( word, "noparse") ) {
			*attrib |= WN_NOPARSE;
			continue;
		}
		if ( streq( word, "cgi") ) {
			*attrib |= WN_CGI;
			continue;
		}
		if ( streq( word, "unbuffered") ) {
			*attrib |= WN_UNBUFFERED;
			continue;
		}
		fprintf( stderr, ERRMSG19, word);
			continue;
	}
}


static void
doattrib_dir( text, ep)
char		*text;
Entry		*ep;
{
	char	*word,
		*nextword;

	word = nextword = text;
	while ( nextword ) {
		word = nextword;
		if ( (nextword = strchr( word, ',')) != NULL)
			*nextword++ = '\0';

		if ( streq( word, "serveall") ) {
#ifndef NO_SERVEALL
			addpair( "serveall", "true", ep);
#endif
			ep->serveall = TRUE;
			continue;
		}
		if ( streq( word, "nosearch") ) {
			addpair( "nosearch", "true", ep);
			continue;
		}
		fprintf( stderr, ERRMSG20, word);
		continue;
	}
}

static void
do_maxage( words, seconds)
char	*words,
	*seconds;
{
	long	n,
		secs = 0;

	int	use_lmd = FALSE;

	char	*cp,
		buf[SMALLLEN];
	
	if ( (cp = strstr( words, "after")) != NULL) {
		*cp = '\0';
		use_lmd = TRUE;
	}

	buf[0] = '\0';
	sscanf( words, "%30ld %15s", &n, buf);

	switch ( tolower(buf[0])) {
	case 'm':
			secs = n * (60);
			break;

	case 'h':
			secs = n * (60 * 60);
			break;

	case 'd':
			secs = n * (24 * 60 * 60);
			break;

	case 'w':
			secs = n * (7 * 24 * 60 * 60);
			break;
	case 's':
	default:
			secs = n;
			break;
	}

	if ( use_lmd) 
		sprintf( seconds, "L%ld", secs);
	else
		sprintf( seconds, "%ld", secs);

}
