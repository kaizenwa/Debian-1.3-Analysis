/*
 * Copyright (c) 1996 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/param.h>
#include <stdio.h>
#include <syslog.h>

#include <atalk/adouble.h>

#include "directory.h"
#include "fork.h"

static struct ofork	*oforks[ ( NOFILE - 10 ) / 2 ];
int			nforks = sizeof( oforks ) / sizeof( oforks[ 0 ] );
static u_short		lastrefnum = 0;

pforkdesc( f )
    FILE	*f;
{
    u_short	ofrefnum;

    for ( ofrefnum = 0; ofrefnum < sizeof( oforks ) / sizeof( oforks[ 0 ] );
	    ofrefnum++ ) {
	if ( oforks[ ofrefnum ] != NULL ) {
	    fprintf( f, "%hu <%s>\n", ofrefnum, oforks[ ofrefnum ]->of_name );
	}
    }
}

of_flush()
{
    u_short	refnum;

    for ( refnum = 0; refnum < nforks; refnum++ ) {
	if ( oforks[ refnum ] != NULL &&
		flushfork( oforks[ refnum ] ) < 0 ) {
	    syslog( LOG_ERR, "of_flush: %m" );
	}
    }
    return( 0 );
}

    struct ofork *
of_alloc( dir, path, ofrefnum )
    struct dir		*dir;
    char		*path;
    u_short		*ofrefnum;
{
    u_short		refnum;
    int			i;

    for ( refnum = lastrefnum++, i = 0; i < nforks; i++, refnum++ ) {
	if ( oforks[ refnum % nforks ] == NULL ) {
	    break;
	}
    }
    if ( i == nforks ) {
	return( NULL );
    }

    if (( oforks[ refnum % nforks ] =
	    (struct ofork *)malloc( sizeof( struct ofork ))) == NULL ) {
	syslog( LOG_ERR, "of_alloc: malloc: %m" );
	exit( 1 );
    }
    oforks[ refnum % nforks ]->of_dir = dir;
    if (( oforks[ refnum % nforks ]->of_name =
	    (char *)malloc( strlen( path ) + 1 )) == NULL ) {
	syslog( LOG_ERR, "of_alloc: malloc: %m" );
	exit( 1 );
    }
    strcpy( oforks[ refnum % nforks ]->of_name, path );
    *ofrefnum = refnum;
    return( oforks[ refnum % nforks ] );
}

    struct ofork *
of_find( ofrefnum )
    u_short	ofrefnum;
{
    return( oforks[ ofrefnum % nforks ] );
}

of_dealloc( of )
    struct ofork	*of;
{
    u_short		refnum;

    for ( refnum = 0; refnum < nforks; refnum++ ) {
	if ( oforks[ refnum ] == of ) {
	    break;
	}
    }
    if ( refnum == nforks ) {
	syslog( LOG_ERR, "of_dealloc: OOPS!" );
	return;
    }

    free( of );
    free( of->of_name );
    oforks[ refnum ] = NULL;
    return;
}
