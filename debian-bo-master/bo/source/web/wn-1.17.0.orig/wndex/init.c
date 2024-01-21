/*
    Wn: A Server for the HTTP
    File: wndex/init.c
    Version 1.15.8
    
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
#include <sys/types.h>
#include <sys/stat.h>
#include "wndex.h"
#include "../wn/version.h"

int	recurse = 0,
	stdioflg = 0,
	verboseflg = 0,
	quiet	= 0;

char	cntlfname[MIDLEN],
	cachefname[MIDLEN];

	
extern char *optarg;
extern int optind;


void
init( argc, argv)
int	argc;
char	*argv[];
{
	int	c,
		dflg = 0,
		errflg = 0;
	char	*dir;

	umask( 033);
	strcpy( cntlfname, CONTROLFILE_NAME);
	strcpy( cachefname, CACHEFNAME);

	while ((c = getopt(argc, argv, "Vqrvxd:i:c:")) != -1) {
		switch ((char) c) {
			case 'r':
				recurse++;
				break;
			case 'q':
				quiet++;
				break;
			case 'd':
				dflg++;
				dir = optarg;
				break;
			case 'i':
				strcpy( cntlfname, optarg);
				break;
			case 'c':
				strcpy( cachefname, optarg);
				break;
			case 'v':
				verboseflg++;
				break;
			case 'V':
				printf( "Wndex: version %s\n", VERSION);
				exit( 0);
			case 'x':
				stdioflg++;
				break;
			case '?':
				errflg++;
		}
	}

	if (errflg) {
		fprintf( stderr, 
	"Usage: %s [-r] [-q] [-v] [-d dir] [-i indexfile] [-c cachefile]\n",
		argv[0]);
		exit (2);
	}

	loadmime();

	if ( dflg ) {
		sprintf( top.cntlfpath, "%s/%s", dir, cntlfname);
		sprintf( top.cachefpath, "%s/%s", dir, cachefname);
	} else {
		if ( *cntlfname == '/') 
			strcpy(top.cntlfpath, cntlfname);
		else
			sprintf( top.cntlfpath, "./%s", cntlfname);
		if ( *cachefname == '/') 
			strcpy(top.cachefpath, cachefname);
		else
			sprintf( top.cachefpath, "./%s", cachefname);
	}

}

/* chop( line)  Cut out CRLF at end of line */

void
chop( line)
char *line;
{
	char	*p;

	if ( *line == '\0')
		return;
	if ( (p = strchr( line, '\n')) == (char *) NULL )
		return;
	if ( *--p != '\r')
		p++;
	*p = '\0';
}
