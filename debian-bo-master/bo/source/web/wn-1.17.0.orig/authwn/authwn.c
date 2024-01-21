/*
    WN: A Server for the HTTP
    File: authwn/authwn.c
    Version 1.15.7
    
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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/signal.h>

#ifdef DBM_AUTH
#include <dbm.h>
#endif

#include "../config.h"
#include "authwn.h"

#define AUTHWN_TIMEOUT	(60)

extern char *optarg;
extern int optind;

extern char	*getenv();

static char	group[SMALLLEN],
		*mystrncpy();

static void	chop(),
		authwn_timeout(),
		getpath();

static int	checkpw(),
		ingroup();

int		isdbm = FALSE;

main( argc, argv)
int	argc;
char	*argv[];

{
	register char	*cp;


	char	pwfile[SMALLLEN],
		pwpath[SMALLLEN],
		grpfile[SMALLLEN],
		grppath[SMALLLEN],
		authdata[MIDLEN + 2*SMALLLEN],
		decoded[MIDLEN];

	int	c;


	signal( SIGALRM, authwn_timeout);
	alarm( AUTHWN_TIMEOUT);

	grpfile[0] = pwfile[0] = '\0';

	while ((c = getopt(argc, argv, "Dg:G:P:")) != -1) {
		switch ((char) c) {
			case 'D':
				isdbm = TRUE;
				break;
			case 'g':
				strcpy( group, optarg);
				break;
			case 'G':
				strcpy( grpfile, optarg);
				break;
			case 'P':
				strcpy( pwfile, optarg);
				break;
		}
	}

	if ( argv[optind] && (!*pwfile)) {
		mystrncpy( pwfile, argv[optind], SMALLLEN);
	}

	if ( !*pwfile) {
		exit( AUTHERR_NUM7);

	}

	getpath( pwpath, pwfile);
	if ( *grpfile)
		getpath( grppath, grpfile);
	else
		*grppath = '\0';

	if ( fgets( authdata, SMALLLEN, stdin) == NULL) {
		exit( AUTHERR_NUM10);
	}

	chop( authdata);
	cp = authdata;
	while ( isspace( *cp))
		cp++;
	while ( *cp && !isspace( *cp))
		cp++;
	*cp++ = '\0';

	while ( isspace( *cp))
		cp++;

	strcpy( decoded, cp);

	if ( checkpw( decoded, pwpath, grppath) ) {
		exit( AUTH_GRANTED);
	}
	else {
		exit( AUTH_DENIED);
	}
}

static 
checkpw( userinfo, pwpath, grppath)
char	*userinfo,
	*pwpath,
	*grppath;
{
	register char	*cp,
			*cp2;
	char		*user,
			*pw,
			codedpw[SMALLLEN],
			linebuf[SMALLLEN],
			info[SMALLLEN];
	int		group_ok;
	FILE		*pwfp,
			*grfp;

#ifdef DBM_AUTH
	datum		content,
			key;
#endif


	codedpw[0] = '\0';
	strcpy( info, userinfo);

	if ( (cp = strchr( info, ':')) == NULL )
		exit( AUTHERR_NUM3);

	*cp = '\0';
	user = info;
	pw = ++cp;


	if ( !isdbm) {
		if ( ( pwfp = fopen( pwpath, "r")) == (FILE *)NULL)
			exit( AUTHERR_NUM4);

		while ( fgets( linebuf, SMALLLEN, pwfp)) {
			if ( (cp = strchr( linebuf, ':')) == NULL )
				continue;
			*cp++ = '\0';
			if ( streq( linebuf, user)) {
				if ( (cp2 = strchr( cp, ':')) != NULL )
					*cp2 = '\0';
				strcpy( codedpw, cp);
				chop( codedpw);
				break;
			}
		}

	}
	else {
#ifdef DBM_AUTH
		key.dptr = user;
		key.dsize = strlen(user);

		if ( dbminit( pwpath) < 0 ) {
			exit( AUTHERR_NUM5);
		}
		content = fetch( key);
		if ( content.dptr != (char *)NULL) {
			strncpy( codedpw, content.dptr, content.dsize);
			codedpw[content.dsize] = '\0';
		}
		dbmclose( pwpath);
#else
		exit( AUTHERR_NUM8);
#endif
	}

	if ( *grppath ) {
		if ( (grfp = fopen( grppath, "r")) == (FILE *)NULL)
			exit( AUTHERR_NUM6);

		group_ok = FALSE;
		while ( fgets( linebuf, SMALLLEN, grfp)) {
			if ( (cp = strchr( linebuf, ':')) == NULL )
				continue;
			*cp++ = '\0';
			if ( streq( linebuf, group)) {
				if ( (cp2 = strchr( cp, ':')) == NULL )
					continue;
				*cp2++ = '\0';
				if ( (cp2 = strchr( cp2, ':')) == NULL )
					continue;
				*cp2++ = '\0';
				chop( cp2);
				group_ok = ingroup( cp2, user);
				break;
			}
		}
		if ( !group_ok) {
			return FALSE;
		}
	}
	if ( *codedpw && strcmp( codedpw, (char *) crypt( pw, codedpw)) == 0)
		return TRUE;
	else
		return FALSE;

}



static int
ingroup( list, user)
char	*list,
	*user;
{
		int		len;

		len = strlen( user);
		while ( *list ) {
			if ( *list == ',')
				list++;
			if ( strncmp( user, list, len) == 0 ) {
				list += len;
				if ( (!*list) || (*list == ',')) {
					return TRUE;
				}
			}
			else {
				while ( *list && (*list != ','))
					list++;
			}
		}
		return FALSE;
}

static void
chop( line)
char *line;
{
	register char	*cp;

	if ( *line == '\0')
		return;
	cp = line;
	while ( *cp )
		cp++;
	if ( *--cp == '\n') {
		*cp = '\0';
	}
}


static void
getpath( path, file)
char	*path,
	*file;
{
	char	*cp;

	if ( *file == '/') {
		mystrncpy( path, file, SMALLLEN);
		return;
	}
	if ( *file == '~' && *(file + 1) == '/') {
		strcpy( path, ROOT_DIR);
		strcat( path, ++file);
		return;
	}
	if ( (cp = getenv( "WN_DIR_PATH")) != NULL ) {
		strcpy( path, cp);
		strcat( path, "/");
		strcat( path, file);
		return;
	}
	else {
		mystrncpy( path, file, SMALLLEN);
	}
}



/*
 * mystrncpy( s1, s2, n) is an strncpy() which guarantees a null
 * terminated string in s1.  At most (n-1) chars are copied.
 */

static char *
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

static void
authwn_timeout()
{
	signal( SIGALRM, SIG_DFL);
	exit( AUTHERR_NUM16);
}
