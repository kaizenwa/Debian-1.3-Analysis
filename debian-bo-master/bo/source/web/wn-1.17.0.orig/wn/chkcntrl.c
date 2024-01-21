/*
    Wn: A Server for the HTTP
    File: wn/chkcntrl.c
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

#include "../config.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "wn.h"
#include "chkcntrl.h"
#include "content.h"
#include "access.h"

#ifndef S_ISREG
#define	S_ISREG(m)	(((m)&S_IFMT) == S_IFREG)
#endif

extern int	chkauth();

extern long	atol();

static int	mask_match();

static void	setdirvalue(),
		setvalue(),
		do_serveall();


/*

 * chk_cntrl( ip) checks to see that the URL sent by the client is a
 * valid one, i.e. that basename exists in a control file in the
 * designated directory.  It also enters some additional fields in the
 * Request struct pointed to by ip, namely "title", "content_type",
 * "encoding", "wrappers", "includes", etc.  It gets this information
 * from the cached control file.

 */

void
chk_cntrl( ip)
Request	*ip;
{
	struct stat stat_buf;

	FILE	*fp;

	int	i,
		access_status = ACCESS_DENIED;

	Cache_entry	*cep,
			entry;

#ifdef ACCESS_DENIED_URL
	mystrncpy( dir_p->noaccess_url, ACCESS_DENIED_URL, MIDLEN/2);
#else
	*dir_p->noaccess_url = '\0';
#endif

#ifdef NO_SUCH_FILE_URL
	mystrncpy( dir_p->cantstat_url, NO_SUCH_FILE_URL, MIDLEN/2);
#else
	*dir_p->cantstat_url = '\0';
#endif

#ifdef AUTH_DENIED_FILE
	mystrncpy( dir_p->authdenied_file, AUTH_DENIED_FILE, MIDLEN/2);
#else
	*dir_p->authdenied_file = '\0';
#endif
	if ( ip->type == RTYPE_DENIED )  /* done in parse_request */
		return;


	if ( lstat( ip->cachepath, &stat_buf) != 0 ) {
		writelog( ip,  ERRMSG2, ip->cachepath);
		ip->type = RTYPE_DENIED;
		ip->status |= WN_CANT_STAT;
		return;
	}

	if ( !(S_ISREG(stat_buf.st_mode))) {
		senderr( SERV_ERR, ERRMSG13, ip->cachepath);
		wn_exit( 2);
	}
	ip->cache_uid = (unsigned) stat_buf.st_uid;
	ip->cache_gid = (unsigned) stat_buf.st_gid;

	if ( (!WN_OPT_U) && (WN_OPT_T) && (!is_trusted( ip)) ) {
		logerr( ERRMSG7, ip->cachepath);
		ip->type = RTYPE_DENIED;
		return;
	}

	if ( stat_buf.st_uid == (uid_t) USERID) {
		logerr( ERRMSG8, ip->cachepath);
		ip->type = RTYPE_DENIED;
		return;
	}

	if ( (fp = fopen( ip->cachepath, "r")) == (FILE *) NULL ) {
		logerr( ERRMSG9, ip->cachepath);
		ip->type = RTYPE_DENIED;
		return;
	}


	read_cache_dirinfo( fp, dir_p);

	access_status = chkaccess( ip,  dir_p->accessfile);

	switch( access_status) {
	case ACCESS_GRANTED:
	case ACCESS_PRIVILEGED:
		break;
	case ACCESS_DENIED:
		ip->type = RTYPE_NOACCESS;
		fclose( fp);
		return;
	default:
		ip->type = RTYPE_DENIED;
		fclose( fp);
		return;
	}

	if ( *(dir_p->defdoc) && ( ip->filetype & WN_DEFAULT_DOC)) {
		fclose( fp);
		dolocation( dir_p->defdoc, ip);
		ip->type = RTYPE_FINISHED;
		return;
	}

	if ( *(dir_p->authmod) && (access_status != ACCESS_PRIVILEGED) ) {
		if ( !(is_trusted( ip) || is_atrusted( ip)) ) {
			senderr( DENYSTATUS, ERRMSG90, dir_p->authmod);
			ip->type = RTYPE_FINISHED;
			fclose( fp);
			return;
		}

		exec_ok( ip);

		if ( !getfpath( dir_p->authmodule, dir_p->authmod, ip)) {
			senderr( SERV_ERR, ERRMSG45, dir_p->authmod);
			ip->type = RTYPE_FINISHED;
			fclose( fp);
			return;
		}
		dir_p->authmod = dir_p->authmodule;

		/* anything requiring authentication should not be cached */
		ip->attributes |= WN_NOCACHE;

		if ( !chkauth( ip)) {
			ip->type = RTYPE_NO_AUTH;
			fclose( fp);
			return;
		}
	}

	if ( *(dir_p->filemod)) {
		exec_ok( ip);
		if ( !getfpath( dir_p->filemodule, dir_p->filemod, ip)) {
			senderr( SERV_ERR, ERRMSG45, dir_p->filemod);
			wn_exit( 2);
		}
		dir_p->filemod = dir_p->filemodule;
		if ( WN_OPT_U )
			check_perm( ip, dir_p->filemodule);
	}

	if ( *(dir_p->cachemod)) {
		exec_ok( ip);
		if ( !getfpath( dir_p->cachemodule, dir_p->cachemod, ip)) {
			senderr( SERV_ERR, ERRMSG45, dir_p->cachemod);
			wn_exit( 2);
		}
		dir_p->cachemod = dir_p->cachemodule;
		if ( WN_OPT_U )
			check_perm( ip, dir_p->cachemodule);
	}

	if ( *(dir_p->indexmod)) {
		exec_ok( ip);
		if ( !getfpath( dir_p->indexmodule, dir_p->indexmod, ip)) {
			senderr( SERV_ERR, ERRMSG45, dir_p->indexmod);
			wn_exit( 2);
		}
		dir_p->indexmod = dir_p->indexmodule;
		if ( WN_OPT_U )
			check_perm( ip, dir_p->indexmodule);
	}

	if ( iswndir( ip)) {
		/* It's a title, keyword, grep, or index search of this dir */
		fclose( fp);
		ip->title = ip->encoding = ip->keywords = ip->filter = NULL;
		ip->content_type = "text/html";
		return;  
	}

	cep = &entry;
	cep->line = ip->cacheline;

	while ( read_cache_file( cep, fp, ip->basename)) {
		if ( !*cep->basename)
			continue;
		if ( streq( ip->basename, cep->basename)) {
			if ( ip->type == RTYPE_UNCHECKED)
				ip->type = RTYPE_FILE;
			ip->title = entry.title;
			ip->content_type = entry.content; 
			ip->encoding = entry.encoding; 
			ip->keywords = entry.keywords; 

			ip->includes = dir_p->defincludes;
			if ( *entry.includes)
				ip->includes = entry.includes;
			if ( (*ip->includes) &&
					!streq( ip->includes, "<none>"))
				ip->attributes |= WN_INCLUDE;
			else
				*ip->includes = '\0';

			ip->wrappers = dir_p->defwrapper;
			if ( *entry.wrappers)
				ip->wrappers = entry.wrappers; 
			if ( (*ip->wrappers) &&
					!streq( ip->wrappers, "<none>"))
				ip->attributes |= WN_WRAPPED;
			else
				*ip->wrappers = '\0';

			ip->inclptr = ip->wrappers; 

			ip->swrapper = entry.swrapper; 
			ip->nomatchsub = entry.nomatchsub; 
			ip->filter = entry.filter;
			ip->expires = entry.expires;
			ip->maxage = ((*entry.maxage) 
				? entry.maxage : dir_p->default_maxage);

			for ( i = 0; i < NUMFIELDS; i++) {
				ip->field[i] = entry.field[i];
			}

			ip->attributes |= entry.attributes;
			ip->attributes |= dir_p->defattributes;
			/* One of these two attributes is always zero */

			ip->filetype |= entry.filetype;
			if ( ip->attributes & (WN_INCLUDE + WN_WRAPPED)) {
				update_mod_time( ip);
				ip->attributes |= WN_PARSE;
			}

			if ( (ip->filetype & WN_IMAGEMAP) ||
					(ip->attributes & WN_ISMAP)) {
				ip->type = RTYPE_IMAGEMAP;
			}

			if ( *(entry.parse)){
				if ( streq(entry.parse, "true"))
					ip->attributes |= WN_PARSE;
				else	/* it's false */
					ip->attributes &= ~(WN_PARSE);
			}
			if ( ip->attributes & WN_NOPARSE)
				ip->attributes &= ~(WN_PARSE);

			/* By default CGI non-POST with no query is dynamic */
			if ( ((ip->type == RTYPE_CGI ) ||
						(ip->type == RTYPE_NPH_CGI ))
					&& ( inheadp->method != POST)
					&& ( !*(ip->query)))

				ip->attributes |= WN_DYNAMIC;

			/* But it can be non-dynamic if dynamic = false */
			if ( ip->attributes & WN_NONDYNAMIC)
				ip->attributes &= ~(WN_DYNAMIC);
			if ( *(entry.dynamic)){
				if ( streq(entry.dynamic, "true"))
					ip->attributes |= WN_DYNAMIC;
				else
					ip->attributes &= ~(WN_DYNAMIC);
			}

			if ( *(entry.cgi)){
				if ( streq(entry.cgi, "true"))
					ip->attributes |= WN_CGI;
				else
					ip->attributes &= ~(WN_CGI);
			}

			if ( strncasecmp( ip->content_type, "text", 4) == 0 )
				ip->filetype |= WN_TEXT;
			if ( streq( ip->content_type, "text/html"))
				ip->filetype |= WN_ISHTML;

			if ( *entry.headerlines)
				strcpy( outheadp->list, entry.headerlines);
				/* both have size BIGLEN */
			if ( *entry.redirect) {
				char 	*cp;

				ip->type = RTYPE_REDIRECT;
				mystrncpy( outheadp->redirect, 
						entry.redirect, SMALLLEN);

				cp = strrchr( outheadp->redirect, '?');
				if ( (cp != NULL) && (*(cp+1) == '\0') ) {
					if  ( *ip->query )
						mystrncpy( ++cp, ip->query, 
							MIDLEN - SMALLLEN);
					else
						*cp = '\0';
				}
			}
			else if ( (ip->status & WN_CANT_STAT) &&
						(*dir_p->filemod == '\0'))
				ip->type = RTYPE_DENIED;

			fclose( fp);
			return;
		}
	}
#ifdef NO_SERVEALL
	ip->type = RTYPE_DENIED;
#else
	if ( dir_p->attributes & WN_SERVEALL)
		do_serveall( ip);
	else
		ip->type = RTYPE_DENIED;
#endif

	fclose( fp);
	return;
}



/*
 * do_serveall( ip) The file ip->filename exists but is not in the index.cache
 * file.  This function sets ip->content (and ip->encoding if needed) based
 * on the filename suffix.
 */

static void
do_serveall( ip )
Request	*ip;
{
	register char	*cp,
			*cp2;

	int		i = 0;

	char		suffix[10];

	ip->title = ip->basename;

	if ( *(dir_p->filemod))
		/* skip check on file name */
		;
	else if ( (*(ip->title) == '.') || streq( ip->title, CACHEFNAME) ||
				(ip->status & WN_CANT_STAT) ||
				streq( ip->title, CONTROLFILE_NAME)) {
		ip->type = RTYPE_DENIED;
		return;
	}

	if ( !*(dir_p->default_content))
		dir_p->default_content = DEFAULT_CONTENT_TYPE;

	if ( ip->type == RTYPE_UNCHECKED)
		ip->type = RTYPE_FILE;

	if ( (ip->type == RTYPE_CGI ) || (ip->type == RTYPE_NPH_CGI ))
		ip->attributes |= WN_DYNAMIC;

	ip->content_type = dir_p->default_content;
	ip->maxage = dir_p->default_maxage;

	if ( (cp = strrchr( ip->basename, '.')) == NULL )
		return;

	mystrncpy( suffix, cp+1, 10);
	strlower( suffix);
	if ( streq( suffix, "gz") || streq( suffix, "z")) {
		ip->encoding = ( suffix[0] == 'g' ? "x-gzip" : "x-compress");
		*cp = '\0';
		if ( (cp2 = strrchr( ip->basename, '.')) == NULL ) {
			*cp = '.';
			ip->content_type = dir_p->default_content;
			return;
		}
		mystrncpy( suffix, cp2+1, 10);
		strlower( suffix);
		*cp = '.';
	}

	while ( list[i][0] != NULL) {
		if ( streq( list[i][0], suffix)) {
			ip->content_type = list[i][1];
			break;
		}
		i++;
	}

	if ( strncasecmp( ip->content_type, "text", 4) == 0 )
		ip->filetype |= WN_TEXT;
	if ( streq( ip->content_type, "text/html"))
		ip->filetype |= WN_ISHTML;

	return;
}


/*
 * chkaccess( ip, accessfile) checks whether the client's IP address
 * is in the allowed list in accessfile.  Returns ACCESS_PRIVILEGED if 
 * access is unconditionally allowed, ACESS_GRANTED if further
 * authentication (through an authentication module) may be required, 
 * ACCESS_DENIED if access is denied and ACCESS_ERR on error.
 */

int
chkaccess( ip, accessfile)
Request	*ip;
char	*accessfile;
{
	FILE	*fp;
	int	len,
		notflag,
		priv_flag,
		match = FALSE;

	char	*cp,
		*cp2,
		buf[MIDLEN],
		linebuf[SMALLLEN];

	if ( ! *accessfile)
		return ACCESS_GRANTED;	/* No access control */

	if ( getfpath2( buf, accessfile, ip) == FALSE)
		return ACCESS_ERR;

	get_remote_info( );

	if ( WN_OPT_U )
		check_perm( ip, buf);

	if ((fp = fopen( buf, "r")) == (FILE *)NULL ) {
		logerr( ERRMSG87, buf);
		return ACCESS_ERR;
	}

	while ( fgets( linebuf, SMALLLEN, fp)) {
		if ( !chop( linebuf)) {
			logerr( ERRMSG62, buf);
			return ACCESS_ERR;
		}

		cp = linebuf;

		if ( strncasecmp( cp, "access-denied-url=", 18) == 0) {
			mystrncpy( dir_p->noaccess_url, cp + 18, MIDLEN/2);
			continue;
		}

		if ( notflag = ( *cp == '!'))
			cp++;

		if ( priv_flag = ( *cp == '+'))
			cp++;

		len = strlen( cp);

		if ( (len == 0) || (*cp == '#') )
			continue;

		strlower( cp);

		if ( streq( cp, remaddr) || streq( cp, remotehost)) {
			match = TRUE;
		}
		else if ( (cp2 = strchr( cp, '/')) != NULL) {
			*cp2++ = '\0';
			match = mask_match( cp, cp2);
		}
		else {
			if ( (cp[len - 1] == '.') && 
					( strncmp( cp, remaddr, len) == 0 ))
				match = TRUE;
		}

		if ( match || wild_match( remotehost, cp)) {
			if ( notflag)
				break;
			else {
				fclose( fp);
				if ( priv_flag)
					return  ACCESS_PRIVILEGED;
				else
					return  ACCESS_GRANTED;
			}
		}
	}
	fclose( fp);
	return ACCESS_DENIED;
}

/*
 * static int mask_match( net, mask)
 * net is "nnn.nnn.nnn.nnn", mask is "mmm.mmm.mmm.mmm" and remaddr
 * is "rrr.rrr.rrr.rrr".  Return true if nnn == mmm | rrr for each
 * of the four segments of net.  Else return false.
 */

static int
mask_match( net, mask)
char	*net,
	*mask;

{
	int	ipnet[4],
		ipmask[4],
		iprem[4];

	sscanf( net, "%d.%d.%d.%d", 
			&ipnet[0], &ipnet[1], &ipnet[2], &ipnet[3]);
	sscanf( mask, "%d.%d.%d.%d", 
			&ipmask[0], &ipmask[1], &ipmask[2], &ipmask[3]);
	sscanf( remaddr, "%d.%d.%d.%d", 
			&iprem[0], &iprem[1], &iprem[2], &iprem[3]);

	return ( (ipnet[0] == (ipmask[0] & iprem[0]))
			&& ( ipnet[1] == (ipmask[1] & iprem[1]))
			&& ( ipnet[2] == (ipmask[2] & iprem[2]))
			&& ( ipnet[3] == (ipmask[3] & iprem[3])));
}


/*
 *  wild_match
 *
 *  String equality routine, including matching the '*' character.
 */

/*
 *  Borrowed from the ANU News sources V6.1b3 newsrtl.c.  Original sources
 *  Copyright 1989 by Geoff Huston.
 *
 */

int wild_match(l,p)
    char *l,
         *p;
{
    if (!*l) {
        if (!*p) return(1);
        else if (*p == '*') return(wild_match(l,p+1));
        else return(0);
        }
    if (*p == '*') {
        while (!wild_match(l,p+1)) {
            l++;
            if (!*l) {
                if (!*(p+1)) return(1);
                else return(0);
                }
            }
        return(1);
        }
    if (*p == '?') return(wild_match(l+1,p+1));
    return((*l == *p) && wild_match(l+1,p+1));
}


/*
 * Store the line from fp in dep->dirline.  This line consists of &
 * separated field value pairs (field=value).  Fields are access,
 * swrapper, nomatchsub, subdirs, and owner.
 * Change the &'s and ='s  to '\0' and  make dip->access, etc., point to
 * the right place in dip->line.  Return TRUE unless empty line then FALSE.
 */

void
read_cache_dirinfo( fp, dep)
FILE		*fp;
Dir_info	*dep;
{
	register char	*cp;
	char		*field,
			*value;

	cp = dep->dirline;
	if ( fgets( cp, BIGLEN, fp) == NULL) {
		*cp++ = '\n';
		*cp = '\0';
	}

	if ( strrchr( dep->dirline, '\n') == NULL) {
		senderr( SERV_ERR, ERRMSG63, "");
		wn_exit( 2);
	}

	dep->attributes = dep->defattributes = 0;

	while ( *cp)
		cp++;

	dep->accessfile = dep->swrapper = dep->defwrapper = dep->defincludes
	= dep->subdirs = dep->nomatchsub = dep->dir_owner 
	= dep->cachemod = dep->filemod = dep->indexmod
	= dep->authmod = dep->authtype = dep->authrealm 
	= dep->default_content = dep->defdoc = dep->default_maxage = cp;

	*dep->authmodule = *dep->cachemodule
	 = *dep->filemodule = *dep->indexmodule = '\0';

	cp = dep->dirline;

	if ( !*cp )
		return;

	field = cp++;
	while ( *cp) {
		switch (*cp) {
		case '=':
			*cp++ = '\0';
			value = cp;
			break;
		case '&':
			if ( *(cp-1) == '\\')	/* ignore escaped & */
				break;
			*cp = '\0';
			setdirvalue( field, value, dep);
			field = ++cp;
			break;
		case '\n':
			*cp = '\0';
			setdirvalue( field, value, dep);
			return;
		default:
			cp++;
		}
	}
}


static void
setdirvalue( field, value, dep)
char		*field,
		*value;
Dir_info	*dep;
{
	if ( !*value) {
		logerr( ERRMSG10, field);
		return;
	}
	switch (*field) {
	case 'a':
		if ( strncmp( field, "auth", 4) == 0) {
			switch( field[4]) {
			case 'd':		/* authdenied_file */
				mystrncpy( dep->authdenied_file, 
							value, MIDLEN/2);
				break;
			case 'm':
				dep->authmod = value;
				break;
			case 'r':
				dep->authrealm = value;
				break;
			case 't':
				dep->authtype = value;
				break;
			}
		}
		else {
			dep->accessfile = value;
		}
		break;

	case 'c':
		dep->cachemod = value;
		break;

	case 'd':
		if ( streq( field, "dwrapper")) {
			dep->attributes |= WN_DIRWRAPPED;
			dep->swrapper = value;
		}

		else if ( streq( field, "defwrapper")) {
			dep->defwrapper = value;
		}

		else if ( streq( field, "defincludes")) {
			dep->defincludes = value;
		}

		else if ( streq( field, "defattributes"))
			dep->defattributes = (unsigned) atol( value);

		else if ( streq(field, "default_content"))
			dep->default_content = value;

		else if ( streq(field, "default_document"))
				dep->defdoc = value;

		else if ( streq(field, "default_maxage"))
				dep->default_maxage = value;

		else
			logerr( ERRMSG11, field);
		break;

	case 'f':	/* file module */
		dep->filemod = value;
		break;

	case 'i':	/* index module */
		dep->indexmod = value;
		break;

	case 'n':
		switch ( *(field + 2)) {

		case 'a':		/* noaccess_url */
			mystrncpy( dep->noaccess_url, value, MIDLEN/2);
			break;

		case 'f':		/* nofile_url */
			mystrncpy( dep->cantstat_url, value, MIDLEN/2);
			break;

		case 'm':		/* nomatchsub */
			dep->nomatchsub = value;
			break;

		case 's':
			if ( streq( value, "true")) /* nosearch=true */
			dep->attributes |= WN_DIRNOSEARCH;
			break;

		default:
			logerr( ERRMSG11, field);
			break;
		}
		break;

	case 'o':	/* owner */
		dep->dir_owner = value;
		break;

	case 's':
		if ( field[1] == 'u')		/* subdirs */
			dep->subdirs = value;
		else {				/* serveall */
			if ( streq( value, "true"))
				dep->attributes |= WN_SERVEALL;
		}
		break;
		
	default:
		logerr( ERRMSG11, field);
	}
}

/*
 * Store the line from fp in cep->line.  This line consists of &
 * separated field value pairs (field=value).  Fields are basename,
 * title, keywords, content, maxage, encoding, type, includes, and wrappers.
 * Change the &'s and ='s  to '\0' and  make cep->basename, cep->title,
 * cep->keywords, cep->content, cep->encoding and cep->type point to
 * the right place in cep->line.  Return TRUE unless no more lines then FALSE.
 */

int
read_cache_file( cep, fp, key)
Cache_entry	*cep;
FILE		*fp;
char		*key;

{
	register char	*cp;
	char		*field,
			*value,
			envkey[SMALLLEN];
	int		c,
			i;

	static FILE	*lfp;

	cp = cep->line;

	if ( *dir_p->cachemod ) {  /* invoke cache module */
		if ( key != NULL ) {
			strcpy( envkey, "WN_KEY=");
			mystrncat( envkey, key, SMALLLEN - 7);
			putenv( envkey);
		}

		if ((lfp = popen( dir_p->cachemod, "r")) == (FILE *)NULL ) {
			senderr( SERV_ERR, ERRMSG40, dir_p->cachemod);
			wn_exit( 2);
		}

		if ( (c = getc( lfp)) == EOF ) {
			senderr( SERV_ERR, ERRMSG43, dir_p->cachemod);
			pclose( lfp);
			wn_exit( 2);
		}
		else
			ungetc( c, lfp);

		*dir_p->cachemod = '\0';  /* don't come back here again */
	}	
	else 
		lfp = fp;

	for (;;) {		/* read until non-empty line */
		if ( fgets( cp, CACHELINE_LEN, lfp) == NULL)
			return FALSE;
		if ( *cp != '\n')
			break;
	}

	if ( lfp != fp)    /* It's a cache module and we're done */
		pclose( lfp);

	if ( strrchr( cp, '\n') == NULL) {
		senderr( SERV_ERR, ERRMSG63, "");
		wn_exit( 2);
	}

	while ( *cp)
		cp++;
	cep->end = cep->basename = cep->title = cep->keywords = cep->dynamic
	= cep->content = cep->encoding = cep->includes = cep->cgi
	= cep->wrappers = cep->swrapper = cep->nomatchsub = cep->parse
	= cep->filter = cep->expires = cep->maxage = cep->url 
	= cep->redirect = cp;
	for ( i = 0; i < NUMFIELDS; i++) {
		cep->field[i] = cp;
	}
	*cep->headerlines = '\0';
	cep->attributes = cep->filetype = 0;
	cp = cep->line;

	field = cp;
	if ( (value = strchr( cp, '=' )) == NULL )
		cp = value = cep->end;
	else {
		*value++ ='\0';
		cp = value;
	}
	while ( *cp) {
		switch (*cp) {
		case '&':
			if ( *(cp-1) == '\\') {	  /* handle escaped & */
				strcpy( cp-1, cp);
				break;
			}
			*cp = '\0';
			setvalue( field, value, cep);
			field = ++cp;
			if ( (value = strchr( cp, '=' )) == NULL )
				cp = value = cep->end;
			else {
				*value++ ='\0';
				cp = value;
			}
			break;
		case '\n':
			*cp = '\0';
			setvalue( field, value, cep);
			return TRUE;
		default:
			cp++;
		}
	}
	return TRUE;
}

static void
setvalue( field, value, cep)
char	*field,
	*value;
Cache_entry	*cep;
{
	char	buf[SMALLLEN];
	int	i;

	if ( !*value) {
		strcpy( buf, field);
		strcat( buf, ":");
		mystrncat( buf, cep->basename, SMALLLEN - 30);
		logerr( ERRMSG5, buf);
		return;
	}
	switch (*field) {
	case 'a':		/* attributes */
		cep->attributes |= (unsigned) atol( value);
		dir_p->defattributes = 0;
		/* Explicit attrib given, don't use default */
		break;
	case 'c':
		if ( field[1] == 'g' )		/* cgi */
			cep->cgi = value;
		else
			cep->content = value;
		break;


		break;
	case 'd':
		cep->dynamic = value;
		break;
	case 'e':
		switch( field[1]) {
		case 'n':
			cep->encoding = value;
			break;
		case 'x':
			cep->expires = value;
			break;
		}
		break;
	case 'f':
		switch( field[3]) {
		case 'e':		/* file */
			cep->basename = value;
			*cep->url = '\0';  /* can't have url & basename */
			break;
		case 'l':		/* field */
			i = atoi( field + 5);
			cep->field[i] = value;
			break;
		case 't':		/* filter */
			cep->filter = value;
			cep->attributes |= WN_FILTERED;
			break;
		}
		break;
	case 'h':
		if ( strlen( cep->headerlines) + strlen( value) > BIGLEN ) {
			senderr( SERV_ERR, ERRMSG70, value);
			wn_exit( 2);
		}
		strcat( cep->headerlines, value);
		strcat( cep->headerlines, "\n");
		break;
	case 'i':
		switch( field[1]) {
		case 'n':
			cep->includes = value;
			cep->attributes |= WN_INCLUDE;
			break;
		case 'm':
			if ( streq( value, "true"))
				cep->filetype |= WN_IMAGEMAP;
			break;
		}
		break;
	case 'k':
		cep->keywords = value;
		break;
	case 'm':
		cep->maxage = value;
		break;
	case 'n':
		if ( *(field+2) == 'm')  /* nomatchsub */
			cep->nomatchsub = value;
		else if ( streq( value, "true")) /* nosearch=true */
			cep->attributes |= WN_NOSEARCH;
		break;
	case 'p':
		cep->parse = value;
		break;
	case 'r':
		cep->redirect = value;
		break;
	case 's':
		cep->swrapper = value;
		cep->attributes |= WN_SWRAPPED;
		break;
	case 't':		/* title */
		cep->title = value;
		break;
	case 'u':
		cep->url = value;
		*cep->basename = '\0';  /* can't have url & basename */
		break;
	case 'w':
		cep->wrappers = value;
		cep->attributes |= WN_WRAPPED;
		break;
	default:
		logerr( ERRMSG6, field);
	}
}


/*
 * check_perm( ip, buf)
 * This stats the file named in buf, then checks if the index.cache is
 * owned by trusted user or group (options -t & -T) or if the index.cache
 * owner must own the file ( option -u). If sends an error and quits
 * if permssion is not allowed.
 */

void
check_perm( ip, buf)
Request	*ip;
char	*buf;
{
	register char	*cp;
	char		filebuf[MIDLEN];

	struct stat stat_buf;

	if ( (!WN_OPT_T) && (!WN_OPT_U) )  /* No restrictions */
		return;
	else if ( is_trusted( ip) )
		return;

	if ( WN_OPT_U ) {
		mystrncpy( filebuf, buf, MIDLEN);
		cp = filebuf;
		while ( *cp && !isspace( *cp))
			cp++;
		*cp = '\0';
		if ( stat( filebuf, &stat_buf) != 0 ) {
			logerr( ERRMSG12, buf);
			return;
		}

		if (( ip->cache_uid == stat_buf.st_uid )
				 || ( cache_id == stat_buf.st_uid )
				 || ( cache_id == stat_buf.st_gid ))
			return;
	}
		
	senderr( DENYSTATUS, ERRMSG3, ip->cachepath);
	wn_exit( 2);
}


void
exec_ok( ip)
Request	*ip;
{
	
#ifdef FORBID_CGI
	senderr( DENYSTATUS, ERRMSG4, ip->cachepath);
	wn_exit( 2);
#else
	if ((serv_perm & WN_FORBID_EXEC) ||
			((serv_perm & WN_RESTRICT_EXEC) && !is_trusted( ip))) {
		senderr( DENYSTATUS, ERRMSG4, ip->cachepath);
		wn_exit( 2);
	}
#endif
}

