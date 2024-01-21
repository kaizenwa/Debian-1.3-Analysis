/*
 *   dhttpd/1.02 - Personal web page server version 1.02
 *   Copyright (C) 1997  David A. Bartold
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/stat.h>

#include "config.hh"
#include "socket.hh"
#include "httpsock.hh"
#include "version.hh"

#define OK 0
#define FORBIDDEN 1
#define NOT_FOUND 2
#define NOT_MOD 3

char *dayName[] = {
	"Sunday", "Monday", "Tuesday", "Wednesday",
	"Thursday", "Friday", "Saturday"
};

char *monthName[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

struct AssocType
{
	char *ext, *type;
};

AssocType assocNames[] =
{
	{ ".mp2", "audio/x-mpeg" },
	{ ".mpa", "audio/x-mpeg" },
	{ ".abs", "audio/x-mpeg" },
	{ ".mpega", "audio/x-mpeg" },
	{ ".mpeg", "video/mpeg" },
	{ ".mpg", "video/mpeg" },
	{ ".mpe", "video/mpeg" },
	{ ".mpv", "video/mpeg" },
	{ ".vbs", "video/mpeg" },
	{ ".mpegv", "video/mpeg" },
	{ ".bin", "application/octet-stream" },
	{ ".com", "application/octet-stream" },
	{ ".dll", "application/octet-stream" },
	{ ".bmp", "image/x-MS-bmp" },
	{ ".exe", "application/octet-stream" },
	{ ".mid", "audio/x-midi" },
	{ ".midi", "audio/x-midi" },
	{ ".htm", "text/html" },
	{ ".html", "text/html" },
	{ ".txt", "text/plain" },
	{ ".gif", "image/gif" },
	{ ".tar", "application/x-tar" },
	{ ".jpg", "image/jpeg" },
	{ ".jpeg", "image/jpeg" },
	{ ".png", "image/png" },
	{ ".ra", "audio/x-pn-realaudio" },
	{ ".ram", "audio/x-pn-realaudio" },
	{ ".sys", "application/octet-stream" },
	{ ".wav", "audio/x-wav" },
	{ ".xbm", "image/x-xbitmap" },
	{ ".zip", "application/x-zip" },
	{ NULL, NULL }
};

char *getMimeTime( tm *t )
{
	static char out[ 44 ];
	
	/* Format:  Someday, 01-Mon-01 01:01:01 GMT */
	
	sprintf( out, "%s, %02i-%s-%02i %02i:%02i:%02i GMT",
		dayName[ t->tm_wday ], t->tm_mday, monthName[ t->tm_mon ], t->tm_year%100,
		t->tm_hour, t->tm_min, t->tm_sec
	);

	return out;
}

char *curTime()
{
	time_t t;
	t = time( NULL );
	return getMimeTime( gmtime( &t ) );
}

char *guessType( char *f )
{
	int flen;
	int tlen;
	int g;
	
	flen = strlen( f );
	
	for( g=0; assocNames[ g ].ext; g++ )
	{
		tlen = strlen( assocNames[ g ].ext );
		if( flen>tlen )
		{
			if( !strcmp( &f[ flen-tlen ], assocNames[ g ].ext ) )
			{
				return assocNames[ g ].type;
			}
		}
	}
	
	return "application/x-unknown";
}

int findMonth( char *s )
{
	int g;

	for( g=0; g<12; g++ )
	{
		if( !strcmp( monthName[ g ], s ) )
		{
			return g;
		}
	}
	
	return -1;
}

int badFileName( char *s )
{
	if( strstr( s, ".." ) )
	{
		return 1;
	}
	
	if( strstr( s, "//" ) )
	{
		return 1;
	}
	
	return 0;
}

void error( FILE *out, int num, char *stat, char *msg, char *str )
{
	FILE *in;
	int numch;
	char file[ 1200 ];
	char buf[ 1024 ];
	struct stat fs;

	fprintf( out, "HTTP/1.0 %s\r\n", stat );
	fprintf( out, "Date: %s\n", curTime() );
	fprintf( out, "Server: %s\r\n", DHTTPDVERSION );
	fprintf( out, "MIME-version: 1.0\r\n" );
	fprintf( out, "Content-type: text/html\r\n" );
	fprintf( out, "\r\n" );

	sprintf( file, "%s/..ERROR%i.html", WEBDIRPREFIX, num );
	in = fopen( file, "r" );
	if( in!=NULL )
	{
		fstat( fileno( in ), &fs );
	
		if( ( (fs.st_mode&S_IFMT)==S_IFREG ) && !( fs.st_mode&S_IFDIR ) )
		{
			do
			{
				numch = fread( buf, 1, 1024, in );
				fwrite( buf, 1, numch, out );
			}
			while( numch );

			fclose( in );
			return;
		}
		fclose( in );
	}

	fprintf( out, "<html><head><title>%s: Error %s</title></head>", DHTTPDVERSION, msg );
	fprintf( out, "<h1>%s</h1><hr>", msg );
	fprintf( out, "%s <em>Sorry!</em></body></html>", str );
}

void screwed( FILE *out )
{
	error( out,
		400,
		"400 Bad Request",
		"400: You're Screwed!",
		"You can't do that! This server does not support the operation "
		"requested by your client!"
	);
}

void sendError( FILE *out, int status )
{
	switch( status )
	{
		case FORBIDDEN:
			error( out,
				403,
				"403 Forbidden",
				"403: Forbidden!",
				"You do not have permission to access that file."
			);
			break;
			
		case NOT_FOUND:
			error( out,
				404,
				"404 File Not Found",
				"404: File Not Found!",
				"File specified not found on server.  Check to make sure you have the "
				"correct file extension." 
			);
			break;
	}
}

/* Should the web browser used the cached version? */
int useCache( struct tm *modTime, char *s )
{
	char *pos;
	char mname[ 1024 ];
	int year = 0, month = 0, day = 0, hour = 0, min = 0, sec = 0, x;

	/* Skip over the week day */
	if( !( pos = strchr( s, ' ' ) ) )
	{
		return 0;
	}
	else
	{
		while( isspace( *pos ) )
		{
			pos++;
		}
	}

	if( isalpha( *pos ) )
	{
		/* ctime */
		sscanf( pos, "%s %d %d:%d:%d %*s %d", mname, &day, &hour, &min, &sec, &year );
	}
	else if( pos[ 2 ] == '-' )
	{
		/* RFC 850 (normal HTTP) */
		char t[ 1024 ];
		sscanf( pos, "%s %d:%d:%d", t, &hour, &min, &sec );
		t[ 2 ] = '\0';
		sscanf( t, "%i", &day );
		t[ 6 ] = '\0';
		strcpy( mname, &t[ 3 ] );
		sscanf( &t[ 7 ], "%i", &x );

		/* Prevent wraparound from ambiguity */
		if(x < 70)
		{
			x += 100;
		}
		year = 1900 + x;
	}
	else
	{
		/* RFC 822 */
		sscanf( pos, "%d %s %d %d:%d:%d", &day, mname, &year, &hour, &min, &sec );
	}
	month = findMonth( mname );

	if( (x = (1900+modTime->tm_year) - year) )
	{
		return x < 0;
	}

	if( (x = modTime->tm_mon - month) )
	{
		return x < 0;
	}

	if( (x = modTime->tm_mday - day) )
	{
		return x < 0;
	}

	if( (x = modTime->tm_hour - hour) )
	{
		return x < 0;
	}

	if( (x = modTime->tm_min - min) )
	{
		return x < 0;
	}

	if( (x = modTime->tm_sec - sec) )
	{
		return x < 0;
	}

	return 1;
}

int sendFile( FILE *out, char *name, char *modTime )
{
	FILE *in;
	struct stat fs;
	tm *tmMod;
	char buf[ 1024 ];
	int num;

	if( badFileName( name ) )
	{
		return FORBIDDEN;
	}
		
	in = fopen( name, "r" );
	if( in==NULL )
	{
		if( errno==EACCES || errno==ENOTDIR || errno==ELOOP )
		{
			return FORBIDDEN;
		}
		return NOT_FOUND;
	}
	
	fstat( fileno( in ), &fs );
	
	if( ( (fs.st_mode&S_IFMT)!=S_IFREG ) || ( fs.st_mode&S_IFDIR ) )
	{
		fclose( in );
		return FORBIDDEN;
	}
	
	tmMod = gmtime( &fs.st_mtime );
	
	if( modTime!=NULL && useCache( tmMod, modTime ) )
	{
		fclose( in );
		fprintf( out, "HTTP/1.0 304 Not modified\r\n" );
		fprintf( out, "Date: %s\r\n",curTime() );
		fprintf( out, "Server: %s\r\n", DHTTPDVERSION );
		fprintf( out, "MIME-version: 1.0\r\n" );
		fprintf( out, "\r\n" );
		return OK;
	}
	
	fprintf( out, "HTTP/1.0 200 OK\r\n" );
	fprintf( out, "Date: %s\n", curTime() );
	fprintf( out, "Server: %s\r\n", DHTTPDVERSION );
	fprintf( out, "MIME-version: 1.0\r\n" );
	fprintf( out, "Content-type: %s\r\n", guessType( name ) );
	fprintf( out, "Last-modified: %s\r\n", getMimeTime( tmMod ) );
	fprintf( out, "Content-length: %i\r\n", fs.st_size );
	fprintf( out, "\r\n" );

	do
	{
		num = fread( buf, 1, 1024, in );
		fwrite( buf, 1, num, out );
	}
	while( num );
	
	fclose( in );
	
	return OK;
}

void HttpSocket::handle()
{
	char line[ 1024 ];
	char cmd[ 1024 ];
	char file[ 1200 ];
	char ver[ 1024 ];
	char file2[ 1200 ];
	char fileloc[ 1200 ];
	char ifmod[ 1024 ];
	char *modTime;
	
	int num;
	int status;
	char *pos;

	fgets( line, 1024, io );
	num = sscanf( line, "%s %s %s", cmd, file, ver );

	strcpy( ifmod, "" );

	do
	{
		fgets( line, 1024, io );
		pos = strchr( line, ':' );
		if( pos!=NULL )
		{
			*pos++ = '\0';
				
			while( isspace( *pos ) )
			{
				pos++;
			}

			if( !strcasecmp( "If-modified-since", line ) )
			{
				strcpy( ifmod, pos );
			}
		}
	}
	while( strcmp( line, "\r\n" ) && strcmp( line, "\n" ) );

	/* This is necessary for some stupid *
         * operating system such as SunOS    */
	fflush( io );

	modTime = strcmp( ifmod, "" ) ? ifmod : (char*) NULL;
	
	if( !strcmp( cmd, "GET" ) )
	{
		sprintf( file2, "%s%s%s", WEBDIRPREFIX, file[ 0 ]=='/' ? "" : "/", file );
		status = sendFile( io, file2, modTime );
		if( status )
		{
			if( !strcmp( file, "/" ) )
			{
				sprintf( file2, "%s/index.html", WEBDIRPREFIX );
				if( sendFile( io, file2, modTime ) )
				{
					sendError( io, status );
				}
			}
			else
			{
				sendError( io, status );
			}
		}
	}
	else
	{
		screwed( io );
	}
}
