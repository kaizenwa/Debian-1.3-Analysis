/*
    Wn: A Server for the HTTP
    File: wn/chkauth.c
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

#include "../config.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "wn.h"
#include "auth.h"

static void	sendauth(),
		decode64();

static int	send_noauth();

static short int tr[128]={
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
    52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,-1,0,1,2,3,4,5,6,7,8,9,
    10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,-1,26,27,
    28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,
    -1,-1,-1,-1,-1
};



static void
decode64( bufcoded, out)
char	*bufcoded,
	*out;
{

	register char	*in;
	char		buf[SMALLLEN + TINYLEN];
    
	
	while( isspace(*bufcoded))
		bufcoded++;

	mystrncpy( buf, bufcoded, SMALLLEN);
	in = buf;

	while( *in && (tr[*in &= 0177] >= 0))
		in++;
	*in++ = 0;
	*in++ = 0;
	*in++ = 0;
	*in = 0;
    
	in = buf;
    
	while ( in[3] ) {
        	*out++ = (unsigned char) (tr[in[0]] << 2 | tr[in[1]] >> 4);
	        *out++ = (unsigned char) (tr[in[1]] << 4 | tr[in[2]] >> 2);
        	*out++ = (unsigned char) (tr[in[2]] << 6 | tr[in[3]]);
		in += 4;
	}
	
    	if ( in[2] ) {
        	*out++ = (unsigned char) (tr[in[0]] << 2 | tr[in[1]] >> 4);
	        *out++ = (unsigned char) (tr[in[1]] << 4 | tr[in[2]] >> 2);
        	*out++ = (unsigned char) tr[in[2]] << 6;
	}
	else if ( in[1]) {
        	*out++ = (unsigned char) (tr[in[0]] << 2 | tr[in[1]] >> 4);
	        *out++ = (unsigned char) tr[in[1]] << 4;
	}
	else if ( in[0] ) {
        	*out++ = (unsigned char) tr[in[0]] << 2;
	}
	*out = '\0';
}


/*
 * chkauth( ip) check whether authorization is in use and whether the
 * client is authenticated.
 */

int
chkauth( ip )
Request	*ip;
{
	register char	*cp,
			*cp2;

	char	*authheadp,
		authcmd[MIDLEN + 2*SMALLLEN],
		buf[SMALLLEN];

	int	status,
		result;

	FILE	*fp;



        signal( _WN_SIGCHLD, SIG_DFL);

	mystrncpy( authcmd, dir_p->authmod, SMALLLEN);

#ifdef DIGEST_AUTHENTICATION
	if ( strcasecmp( dir_p->authtype, "Digest") == 0) {
		strcat( authcmd, " -r ");
		mystrncat( authcmd, dir_p->authrealm, SMALLLEN);
	}
#endif

	if ( *(inheadp->authorization)) {
		if ( ip->attributes & WN_CGI )
			cgi_env( ip, FALSE);
		else
			cgi_env( ip, TRUE);  /* Small (auth) CGI environ */

		if ((fp = popen( authcmd, "w"))  == (FILE *) NULL ) {
			senderr( SERV_ERR, ERRMSG14, authcmd);
			wn_exit( 2);
		}

		cp = inheadp->authorization;

		if ( strncasecmp( cp, "Basic", 5) == 0) {
			strcpy( buf, "Basic ");
			cp += 5;
			cp2 = buf + 6;
			decode64( cp, cp2);
			mystrncpy( this_conp->authuser, cp2, SMALLLEN);
			if ( ( cp = strchr( this_conp->authuser, ':')) != NULL)
				*cp = '\0';
			authheadp = buf;
		}
#ifdef DIGEST_AUTHENTICATION
		else if ( strncasecmp( cp, "Digest", 6) == 0) {
			cp2 = inheadp->authorization;
			if ( (cp = strstr( cp2, "username")) == NULL)
				cp = strstr( cp2, "Username");
			if ( cp != NULL) {
				cp2 = strchr( cp, '"');
				cp2++;
				mystrncpy( this_conp->authuser, cp2, SMALLLEN);
				if ( (cp = strchr( this_conp->authuser, '"'))
								!= NULL)
					*cp = '\0';
			}
			authheadp = inheadp->authorization;
		}

#endif
		else {
			mystrncpy( buf, inheadp->authorization, SMALLLEN);
			cp = buf;
			while( *cp && !isspace( *cp))
				cp++;
			*cp = '\0';
			senderr( SERV_ERR, AUTHERR9, buf);
			wn_exit( 2);
		}


		fprintf( fp, "%s\n", authheadp);

		status = pclose( fp);

#ifdef NEXT
                if ( (status != 0) && WIFEXITED( (union wait) status))
                        result = ((status >> 8) & 0377);
#else
		if ( (status != 0) && WIFEXITED( status))
			result = WEXITSTATUS( status);
#endif
		else
			result = status;

		switch (result) {
		case (-1):
			sendauth( ip, "-s false", AUTHERR11);
			wn_exit( 2);
		case AUTH_GRANTED:
			return TRUE;
		case AUTH_DENIED:
			sendauth( ip, "-s false", AUTHERR13);
			return FALSE;
		case AUTH_EXPIRED:
			sendauth( ip, "-s true", AUTHERR14);
			return FALSE;
		case (3):
			sendauth( ip, "-s false", AUTHERR3);
			wn_exit( 2);
		case (4):
			sendauth( ip, "-s false", AUTHERR4);
			wn_exit( 2);
		case (5):
			sendauth( ip, "-s false", AUTHERR5);
			wn_exit( 2);
		case (6):
			sendauth( ip, "-s false", AUTHERR6);
			wn_exit( 2);
		case (7):
			sendauth( ip, "-s false", AUTHERR7);
			wn_exit( 2);
		case (8):
			sendauth( ip, "-s false", AUTHERR8);
			wn_exit( 2);
		case (9):
			sendauth( ip, "-s false", AUTHERR9);
			wn_exit( 2);
		case (10):
			sendauth( ip, "-s false", AUTHERR10);
			wn_exit( 2);

		case (16):
			logerr( AUTHERR16, "");
			wn_exit( 2);
		default:
			sprintf( buf, "%s %d", AUTHERR_GENERIC, result);
			logerr( ERRMSG42, buf);
			sendauth( ip, "-s false", buf);
			return FALSE;
		}
	}
	sendauth( ip, "-s false", "");
	return FALSE;
}


static void
sendauth( ip, noncearg, logmsg)
Request	*ip;
char	*noncearg,
	*logmsg;
{
	char	authcmd[MIDLEN],
		buf[MIDLEN];
	FILE	*fp;

	strcpy( outheadp->status, "401 Unauthorized");
	if ( strcasecmp( dir_p->authtype, "basic") == 0) {
		sprintf( outheadp->list, 
				"WWW-Authenticate: Basic realm=\"%s\"\r\n",
				dir_p->authrealm);
	}
#ifdef DIGEST_AUTHENTICATION
	else if ( strcasecmp( dir_p->authtype, "Digest") == 0) {
		if ( ip->attributes & WN_CGI )
			cgi_env( ip, FALSE);
		else
			cgi_env( ip, TRUE);  /* Small (auth) CGI environ */

		sprintf( authcmd, "%s -r %s %s", dir_p->authmod,
			dir_p->authrealm, noncearg);
		if ((fp = popen( authcmd, "r"))  == (FILE *) NULL ) {
			senderr( SERV_ERR, ERRMSG14, authcmd);
			wn_exit( 2);
		}
		if ( fgets( outheadp->list, MIDLEN, fp) == NULL) {
			senderr( SERV_ERR, ERRMSG50, authcmd);
			pclose( fp);
			wn_exit( 2);
		}
		pclose( fp);
	}
#endif
	else {
		logmsg = AUTHERR14;
	}


	ip->encoding =  NULL;
	ip->mod_time = 0;
	ip->content_type = "text/html";

	if ( streq( logmsg, AUTHERR13) 	&& *(dir_p->authdenied_file) ) {
		if ( send_noauth()) {
			writelog( ip, LOGMSG1, logmsg);
			return;
		}
	}
		
	if ( logmsg == NULL)
		logmsg = ERRMSG15;

	sprintf( buf, "<head>\n<title>%s</title>\n</head>\n<body>\n",
			ERRMSG15);
	sprintf( buf + strlen(buf), "<h2>%s</h2>\n", ERRMSG15);

	sprintf( buf + strlen(buf), "%s\n%s\n </body>\n", logmsg, SERVER_LOGO);
	ip->datalen = strlen( buf);
	sprintf( ip->length, "%d", ip->datalen);

	http_prolog( );
	send_text_line(buf);

	writelog( ip, LOGMSG1, logmsg);
	return;
}


static int
send_noauth( )
{
	FILE	*fp;
	char	buf[MIDLEN];
	struct stat stat_buf;

	if ( getfpath2( buf, dir_p->authdenied_file, this_rp) == FALSE) {
		logerr( ERRMSG86, dir_p->authdenied_file);
		return FALSE;
	}
	if ( stat( buf, &stat_buf) != 0 ) {
		logerr( ERRMSG12, buf);
		return FALSE;
	}
	if ( (fp = fopen( buf, "r")) == (FILE *) NULL ) {
		logerr( ERRMSG1, buf);
		return FALSE;
	}

	this_rp->datalen = stat_buf.st_size;
	sprintf( this_rp->length, "%lu", (unsigned long) stat_buf.st_size);

	http_prolog();
	while ( fgets( buf, MIDLEN, fp)) {
		send_text_line( buf);
	}
	return TRUE;
}


