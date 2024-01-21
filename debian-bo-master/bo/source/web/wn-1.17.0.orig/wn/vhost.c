/*
    Wn: A Server for the HTTP
    File: wn/vhost.c
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

#include <errno.h>
#include "../config.h"
#include "wn.h"

#ifdef USE_VIRTUAL_HOSTS
#define	MAXVHOSTS	(64)

#ifndef VIRTUAL_HOSTS_FILE
#include "vhost.h"
#else

char *vhostlist[MAXVHOSTS][3] = 
{
	{ NULL, NULL, NULL }
};

extern char	*malloc();

char	vhostfile[SMALLLEN] = VIRTUAL_HOSTS_FILE;

void
load_virtual()
{
	register char	*cp;
	char	buf[SMALLLEN],
		buf2[SMALLLEN];
	FILE	*vhostfp;
	int	i = 0;

	if ( (vhostfp = fopen( vhostfile, "r")) == (FILE *) NULL) {
		sprintf( buf, "%s: %s", ERRMSG99, vhostfile);
		daemon_logerr( buf, errno);
		return;
	}

	while ( fgets( buf, MIDLEN, vhostfp)) {
		chop( buf);
		if ( !buf[0] || buf[0] == '#')
			continue;

		if ( (vhostlist[i][0] = malloc( strlen( buf) + 2)) == NULL) {
			daemon_logerr( ERRMSG64, errno);
			return;
		}
		strcpy( vhostlist[i][0], buf);
			
		cp = vhostlist[i][0];

		while ( *cp && isspace( *cp))
			cp++;

		if ( *cp )
			vhostlist[i][0] = cp;
		else {
			sprintf( buf2, "%s: %s", ERRMSG101, buf);
			daemon_logerr( buf2, 0);
			break;
		}

		while ( *cp && !isspace( *cp))
			cp++;
		if ( *cp)
			*cp++ = '\0';

		while ( *cp && isspace( *cp))
			cp++;

		if ( *cp )
			vhostlist[i][1] = cp;
		else {
			sprintf( buf2, "%s: %s", ERRMSG101, buf);
			daemon_logerr( buf2, 0);
			break;
		}

		while ( *cp && !isspace( *cp))
			cp++;
		if ( *cp )
			*cp++ = '\0';

		while ( *cp && isspace( *cp))
			cp++;

		if ( *cp )
			vhostlist[i][2] = cp;
		else {
			sprintf( buf2, "%s: %s", ERRMSG101, buf);
			daemon_logerr( buf2, 0);
			break;
		}

		while ( *cp && !isspace( *cp))
			cp++;
		if ( *cp )
			*cp = '\0';

		i++;
		if ( i >= MAXVHOSTS) {
			daemon_logerr( ERRMSG100, 0);
			return;
		}
	}
	vhostlist[i][0] = vhostlist[i][1] = vhostlist[i][2] = NULL;
	fclose( vhostfp);
}

#endif /* VIRTUAL_HOSTS_FILE */
#endif /* USE_VIRTUAL_HOSTS */



