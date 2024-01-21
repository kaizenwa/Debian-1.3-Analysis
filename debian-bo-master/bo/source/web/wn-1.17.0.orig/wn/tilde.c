/*
    Wn: A Server for the HTTP
    File: wn/tilde.c
    Version 1.15.0

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
#include <ctype.h>
#include <string.h>
#include <pwd.h>
#include "wn.h"

extern struct passwd *getpwnam();


static char	tus[] = TILDE_USER_STRING;

static int	get_user_dir( );

/*
 * tilde( path) translates the URI /~user/foo to /foo
 * and changes the rootdir to /home/user/PUB_HTML where 
 * /home/user is either the home directory of user and 
 * PUB_HTML is #defined in config.h.
 */

void
tilde( ip, path)
Request	*ip;
char	*path;
{
	int		i,
			tslen;
	register char	*cp,
			*cp2;

	char		name[SMALLLEN],
	 		tredirect[MIDLEN];

	tslen = sizeof( tus);
	tslen--;
	if ( strncmp( path, tus, tslen))
		return;

	cp2 = name;
	cp = path +  tslen;
	i = 0;
	while ( *cp && ( *cp != '/') && (i < SMALLLEN-1)) {
		i++;
		*cp2++ = *cp++;
	}
	*cp2 = '\0';

 	if (get_user_dir( name, tredirect)) {
		/* redirection needed */
	 	char	*p;
 		int	len;
 
 		len = sizeof (tredirect);
 		p = tredirect + strlen( tredirect);
 		len -= i;
		/* add path, removing excess '/' if necessary */
		if ((*cp == '/') && (*(p-1) == '/')) {
			p--;
			len++;
		}

 		mystrncpy (p, cp, len);

 		sendredirect( ip, "301 Moved Permanently", tredirect);
		ip->type = RTYPE_FINISHED;
		return;
 	}
	else
		strcpy( path, cp);
}

static int
get_user_dir( name, redirp)
char	*name,
	*redirp;
{
	int	found = FALSE;	/* user name found or not */
	int	redir = FALSE;	/* redirection indicated or not */

#ifdef TILDE_USER_PWFILE
  	struct passwd	*pws;
  
 	if ( ((pws = getpwnam( name)) != NULL)
				&& ( pws->pw_uid >= LEAST_UID)) {
 		mystrncpy( this_rp->rootdir, pws->pw_dir, SMALLLEN - 32);
 		mystrncat( this_rp->rootdir, PUB_HTML, 32 );
 		found = TRUE;
  	}
  
#else
#ifdef TILDE_TABLE

	register char	*cp;
	FILE	*fp;
	char	linebuf[MIDLEN];

	if ( (fp = fopen( TILDE_TABLE, "r")) == (FILE *) NULL ) {
		senderr( SERV_ERR, ERRMSG69, TILDE_TABLE);
		wn_exit( 2);
	}

	while ( fgets( linebuf, MIDLEN, fp)) {
		if ( !chop( linebuf)) {
			senderr( SERV_ERR, ERRMSG71, linebuf);
			wn_exit( 2);
		}

		if ( *linebuf == '#')
			continue;

		if ( (cp = strchr(linebuf, ':')) == NULL) {
			logerr( ERRMSG72, linebuf);
			continue;
		}
		*cp++ = '\0';
		if ( streq( name, linebuf)) {
			if (*cp == ':') { /* second colon = redirect */
				redir = TRUE;
				cp++;
	  			mystrncpy( redirp, cp, MIDLEN);
			}
			else {
	  			mystrncpy( this_rp->rootdir, cp, SMALLLEN);
			}
			found = TRUE;
			break;
  		}
	}
	fclose( fp);
#endif /* TILDE_TABLE */
#endif /* TILDE_USER_PWFILE */
	if (!found) {
		senderr( CLIENT_ERR, ERRMSG67, "");
		wn_exit( 2);
	}
	return (redir);
}







