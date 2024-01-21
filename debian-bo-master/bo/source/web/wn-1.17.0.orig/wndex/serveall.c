/*
    Wn: A Server for the HTTP
    File: wndex/serveall.c
    Version 1.16.0
    
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
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include "wndex.h"
#ifndef NEED_DIR_H
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

#define LIST_SIZE	(2048)

static int	isafile(),
		slist_num;

static char	*slistdp,
		*slist[LIST_SIZE],
		slist_data[32*LIST_SIZE];

void
clear_slist( ) 
{
	slist_num = 0;
	slist[0] = slistdp = slist_data;
}

void
add_to_slist( name) 
char	*name;
{
	char	*cp;

	cp = slistdp + strlen( name) + 1;

	if ( (++slist_num >= LIST_SIZE) 
			|| ( cp >= slist_data + sizeof (slist_data))) {
		fprintf( stderr, ERRMSG21);
		exit( 2);
	}

	strcpy( slistdp, name);
	slistdp = cp;
	slist[ slist_num] = cp;
}	

static int
match_slist( file) 
char	*file;
{
	int	i;

	for ( i = 0; i < slist_num; i++ ) {
		if ( streq( file, slist[i]))
			return TRUE;
	}
	return FALSE;
}


void
do_serveall( dirpath, cfp, hfp, ep)
char	*dirpath;
FILE	*cfp,
	*hfp;
Entry	*ep;
{

	DIR	*dirp;
#ifndef NEED_DIR_H
	struct dirent	*dp;
#else
	struct direct	*dp;
#endif

	if ( (dirp = opendir( dirpath)) == NULL) {
		fprintf( stderr, ERRMSG22, dirpath);
		exit( 2);
	}
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
		if ( streq( dp->d_name, cachefname))
			continue;
		if ( streq( dp->d_name, INDEX_TMPFILE))
			continue;
		if ( streq( dp->d_name, cntlfname))
			continue;
		if ( *(dp->d_name) == '.')
			continue;
		if ( (dp->d_name)[strlen(dp->d_name) - 1] == '~')
			continue;
		if (match_slist( dp->d_name))
			continue;

		if ( !isafile( dirpath, dp->d_name))
			continue;
		mystrncpy( ep->file, dp->d_name, SMALLLEN);
		addpair("file", dp->d_name, ep);
		writeitem( cfp, hfp, ep);
		
	}
	closedir (dirp);
}


/*
 * mystrncat( s1, s2, n) is an strncat() which guarantees a null
 * terminated string in s1.  At most (n-1) chars are appended.
 */

static char *
mystrncat( s1, s2, n)
char	*s1,
	*s2;
int	n;
{
	register char	*cp1,
			*cp2;
	cp1 = s1;
	cp2 = s2;
	n--;

	while ( *cp1)
		cp1++;

	while ( *cp2 && (n > 0)) {
		n--;
		*cp1++ = *cp2++;
	}
	*cp1 = '\0';
	return s1;
}

/*
 * isafile( dirpath, fname)
 * Stats the file dirpath/fname.  If it fails issue warning return FALSE.
 * If it is a directory return FALSE, else TRUE.
 */

static int
isafile( dirpath, fname)
char	*dirpath,
	*fname;
{
	struct stat stat_buf;
	char	buf[MIDLEN];

	mystrncpy( buf, dirpath, MIDLEN);
	mystrncat( buf, "/", 2);
	mystrncat( buf, fname, MIDLEN +2 - strlen(fname));
	if ( stat( buf, &stat_buf) != 0 ) {
		if ( !quiet)
			fprintf( stderr, ERRMSG24, buf);

		return FALSE;
	}


	if ( S_ISDIR( stat_buf.st_mode)) {
		return FALSE;
	}
	return TRUE;
}

