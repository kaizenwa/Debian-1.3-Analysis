/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/errno.h>
#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <netatalk/endian.h>
#include <atalk/adouble.h>
#include <atalk/afp.h>
#include <stdio.h>
#include <fcntl.h>
#include <dirent.h>

#include "directory.h"
#include "desktop.h"
#include "volume.h"
#include "file.h"
#include "globals.h"

extern int	errno;

char	getwdbuf[ MAXPATHLEN ];

afp_getfildirparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct stat		st;
    struct adouble	ad;
    struct vol		*vol;
    struct dir		*directory;
    int			did, buflen, adret, ret;
    char		*path;
    u_short		fbitmap, dbitmap, vid;

    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );

    if (( directory = dirsearch( vol, did )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }

    bcopy( ibuf, &fbitmap, sizeof( u_short ));
    fbitmap = ntohs( fbitmap );
    ibuf += sizeof( u_short );
    bcopy( ibuf, &dbitmap, sizeof( u_short ));
    dbitmap = ntohs( dbitmap );
    ibuf += sizeof( u_short );

    if (( path = cname( vol, directory, &ibuf )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }

    if ( stat( mtoupath( path ), &st ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }

    buflen = *rbuflen - 3 * sizeof( u_short );
    if ( st.st_mode & S_IFDIR ) {
	if (( ret = getdirparams( dbitmap, ".", curdir,
		&st, rbuf + 3 * sizeof( u_short ), &buflen )) != AFP_OK ) {
	    *rbuflen = 0;
	    return( ret );
	}
	*(rbuf + 2 * sizeof( u_short )) = 1<<7;	/* this is a directory */
    } else {
	if (( ret = getfilparams( fbitmap, path, curdir, &st,
		rbuf + 3 * sizeof( u_short ), &buflen )) != AFP_OK ) {
	    *rbuflen = 0;
	    return( ret );
	}
	*(rbuf + 2 * sizeof( u_short )) = 0;	/* this is a file */
    }
    *rbuflen = buflen + 3 * sizeof( u_short );
    fbitmap = htons( fbitmap );
    bcopy( &fbitmap, rbuf, sizeof( u_short ));
    rbuf += sizeof( u_short );
    dbitmap = htons( dbitmap );
    bcopy( &dbitmap, rbuf, sizeof( u_short ));
    rbuf += sizeof( u_short ) + sizeof( u_char );
    *rbuf = 0;

    return( AFP_OK );
}

afp_setfildirparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct stat	st;
    struct vol	*vol;
    struct dir	*dir;
    char	*path;
    u_short	vid, bitmap;
    int		did, rc;

    *rbuflen = 0;
    ibuf += 2;
    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );

    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );

    if (( dir = dirsearch( vol, did )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    bcopy( ibuf, &bitmap, sizeof( u_short ));
    bitmap = ntohs( bitmap );
    ibuf += sizeof( u_short );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    if ( stat( mtoupath( path ), &st ) < 0 ) {
	return( AFPERR_NOOBJ );
    }

    /*
     * If ibuf is odd, make it even.
     */
    if ((int)ibuf & 1 ) {
	ibuf++;
    }

    if (( st.st_mode & S_IFMT ) == S_IFDIR ) {
	rc = setdirparams( path, bitmap, ibuf );
    } else {
	rc = setfilparams( path, bitmap, ibuf );
    }
    if ( rc == AFP_OK ) {
	setvoltime( vol );
    }
    return( rc );
}

afp_rename( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct adouble	ad;
    struct stat		st;
    struct vol		*vol;
    struct dir		*dir, *odir;
    char		*path, *upath, newpath[ MAXNAMLEN ];
    char		newadpath[ MAXNAMLEN ];
    int			did, isdir = 0;
    int			plen;
    u_short		vid;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );
    if (( dir = dirsearch( vol, did )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    /* another place where we know about the path type */
    if ( *ibuf++ != 2 ) {
	return( AFPERR_PARAM );
    }
    plen = (unsigned char)*ibuf++;
    *( ibuf + plen ) = '\0';

    if ( *path == '\0' ) {
	++isdir;
	if ( curdir->d_parent == NULL ) {
	    return( AFPERR_ACCESS );
	}
	odir = curdir;
	path = curdir->d_name;
	if ( movecwd( vol, curdir->d_parent ) < 0 ) {
	    return( AFPERR_NOOBJ );
	}
    }

#ifdef notdef
    if ( strcasecmp( path, ibuf ) == 0 ) {
	return( AFP_OK );
    }
#endif notdef

    strcpy( newpath, mtoupath( ibuf ));
    if ( stat( newpath, &st ) == 0 ) {
	return( AFPERR_EXIST );
    }

    upath = mtoupath( path );
    if ( rename( upath, newpath ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }

    if ( !isdir ) {
	strcpy( newadpath, ad_path( newpath, 0 ));
	if ( rename( ad_path( upath, 0 ), newadpath ) < 0 ) {
	    if ( errno == ENOENT ) {	/* no adouble header file */
		if (( unlink( newadpath ) < 0 ) && ( errno != ENOENT )) {
		    return( AFPERR_PARAM );
		}
		goto out;
	    }
	    return( AFPERR_PARAM );
	}
	if ( ad_open( newpath, ADFLAGS_HF, O_RDWR|O_CREAT, 0666, &ad ) < 0 ) {
	    return( AFPERR_PARAM );
	}
    } else {
	if ( ad_open( newpath, ADFLAGS_HF|ADFLAGS_DIR, O_RDWR|O_CREAT,
		0666, &ad ) < 0 ) {
	    return( AFPERR_PARAM );
	}
	if (( odir->d_name = (char *) realloc( odir->d_name, plen + 1 ))
		== NULL ) {
	    syslog( LOG_ERR, "afp_rename: realloc: %m" );
	    exit( 1 );
	}
	strcpy( odir->d_name, ibuf );
    }

    ad_setentrylen( &ad, ADEID_NAME, plen );
    bcopy( ibuf, ad_entry( &ad, ADEID_NAME ), plen );
    ad_flush( &ad, ADFLAGS_HF );
    ad_close( &ad, ADFLAGS_HF );
out:
    setvoltime( vol );

    return( AFP_OK );
}


afp_delete( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol		*vol;
    struct dir		*dir;
    char		*path;
    int			did, rc;
    u_short		vid;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );
    if (( dir = dirsearch( vol, did )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    if ( *path == '\0' ) {
	rc = deletecurdir( vol );
    } else {
	rc = deletefile( mtoupath( path ));
    }
    if ( rc == AFP_OK ) {
	setvoltime( vol );
    }
    return( rc );
}

afp_moveandrename( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol;
    struct dir	*dir, *odir;
    char	newname[ MAXNAMLEN ], *path, *p;
    int		sdid, ddid, isdir = 0, rc;
    int		plen;
    short	vid;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( short ));
    ibuf += sizeof( short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &sdid, sizeof( int ));
    ibuf += sizeof( int );
    if (( dir = dirsearch( vol, sdid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &ddid, sizeof( int ));
    ibuf += sizeof( int );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }

    if ( *path != '\0' ) {
	strcpy( newname, path );
	p = ctoupath( vol, dir, newname );
    } else {
	odir = curdir;
	strcpy( newname, odir->d_name );
	p = ctoupath( vol, odir->d_parent, newname );
	isdir = 1;
    }
    /*
     * p now point to the full pathname of the source fs object.
     */

    if (( dir = dirsearch( vol, ddid )) == NULL ) {
	return( AFPERR_PARAM );
    }
    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }
    if ( *path != '\0' ) {
	return( AFPERR_BADTYPE );
    }

    /* one more place where we know about path type */
    if ( *ibuf++ != 2 ) {
	return( AFPERR_PARAM );
    }
    if (( plen = (unsigned char)*ibuf++ ) != 0 ) {
	strncpy( newname, ibuf, plen );
	newname[ plen ] = '\0';
    }

    if ( !isdir ) {
	rc = renamefile( p, mtoupath( newname ), newname );
    } else {
	rc = renamedir( p, mtoupath( newname ), odir, curdir, newname );
    }
    if ( rc == AFP_OK ) {
	setvoltime( vol );
    }
    return( rc );
}

char *ctoupath( vol, dir, name )
    struct vol	*vol;
    struct dir	*dir;
    char	*name;
{
    struct dir	*d;
    static char	path[ MAXPATHLEN ];
    char	*p, *u;
    int		len;

    p = path + sizeof( path ) - 1;
    *p = '\0';
    u = mtoupath( name );
    len = strlen( u );
    p -= len;
    strncpy( p, u, len );
    for ( d = dir; d->d_parent; d = d->d_parent ) {
	*--p = '/';
	u = mtoupath( d->d_name );
	len = strlen( u );
	p -= len;
	strncpy( p, u, len );
    }
    *--p = '/';
    len = strlen( vol->v_path );
    p -= len;
    strncpy( p, vol->v_path, len );

    return( p );
}
