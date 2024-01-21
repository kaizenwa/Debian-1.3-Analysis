/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <stdio.h>

#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/param.h>
#include <netatalk/endian.h>
#include <atalk/adouble.h>
#include <atalk/afp.h>
#include <stdio.h>
#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <pwd.h>

#include "directory.h"
#include "desktop.h"
#include "volume.h"
#include "file.h"
#include "globals.h"

extern int	errno;

struct dir	*curdir;

struct dir	rootpar = { 0, 0, 0, 0, 0, 0, 1, 0, 0 };

/*
 * These tree management routines need to be modified to support
 * AVL trees -- notice that the DIDs for any give volume are
 * monotonically increasing, and they are assigned as the directories
 * are inserted.
 */
    struct dir *
dirsearch( vol, did )
    struct vol	*vol;
    int		did;
{
    struct dir	*dir;

    if ( ntohl( did ) == 1 ) {
	rootpar.d_child = vol->v_dir;
	return( &rootpar );
    }

    dir = vol->v_did;

    while ( dir != NULL ) {
	if ( dir->d_did == did ) {
	    break;
	}
	if ( dir->d_did > did ) {
	    dir = dir->d_left;
	} else {
	    dir = dir->d_right;
	}
    }
    return( dir );
}

dirfree( vol, dir )
    struct vol	*vol;
    struct dir	*dir;
{
}

dirinsert( vol, dir )
    struct vol	*vol;
    struct dir	*dir;
{
    struct dir	*pdir;

    pdir = vol->v_did;
    for (;;) {
	if ( pdir->d_did == dir->d_did ) {
	    syslog( LOG_ERR, "panic: dirinsert: DID problem!" );
	    abort();
	    return( -1 );
	}
	if ( pdir->d_did > dir->d_did ) {
	    if ( pdir->d_left == NULL ) {
		pdir->d_left = dir;
		return( 0 );
	    }
	    pdir = pdir->d_left;
	} else {
	    if ( pdir->d_right == NULL ) {
		pdir->d_right = dir;
		return( 0 );
	    }
	    pdir = pdir->d_right;
	}
    }
}

/*
 * attempt to extend the current dir. tree to include path
 * as a side-effect, movecwd to that point and return the new dir
 */
    struct dir *
extenddir( vol, dir, path )
    struct vol	*vol;
    struct dir	*dir;
    char	*path;
{
    char	*p;
    struct stat	st;

    p = mtoupath( path );
    if ( stat( p, &st ) != 0 ) {
	return( NULL );
    }
    if (( st.st_mode & S_IFMT ) != S_IFDIR ) {
	return( NULL );
    }

    if (( dir = adddir( vol, dir, path, strlen( path ))) == NULL ) {
	return( NULL );
    }

    if ( movecwd( vol, dir ) < 0 ) {
	return( NULL );
    }

    return( dir );
}

    char *
cname( vol, dir, cpath )
    struct vol	*vol;
    struct dir	*dir;
    char	**cpath;
{
    struct dir		*cdir;
    static char		path[ MAXPATHLEN ];
    char		*data, *p;
    int			extend = 0;
    int			len;

    data = *cpath;
    if ( *data++ != 2 ) {			/* path type */
	return( NULL );
    }
    len = (unsigned char)*data++;
    *cpath += len + 2;
    *path = '\0';

    for ( ;; ) {
	if ( len == 0 ) {
	    if ( !extend && movecwd( vol, dir ) < 0 ) {
		return( NULL );
	    }
	    return( path );
	}

	if ( *data == '\0' ) {
	    data++;
	    len--;
	}

	while ( *data == '\0' && len > 0 ) {
	    if ( dir->d_parent == NULL ) {
		return( NULL );
	    }
	    dir = dir->d_parent;
	    data++;
	    len--;
	}

	p = path;
	while ( *data != '\0' && len > 0 ) {
	    *p++ = *data++;
	    len--;
	}
#ifdef notdef
	/*
	 * Dung Nguyen <ntd@adb.fr>
	 *
	 * AFPD cannot handle paths with "::" if the "::" notation is
	 * not at the beginning of the path. The following path will not
	 * be interpreted correctly:
	 *
	 * :a:b:::c:	(directory c at the same level as directory a)
	 */
	if ( len > 0 ) {
	    data++;
	    len--;
	}
#endif notdef
	*p = '\0';

	if ( p != path ) {
	    if ( !extend ) {
		for ( cdir = dir->d_child; cdir; cdir = cdir->d_next ) {
		    if ( strcmp( cdir->d_name, path ) == 0 ) {
			break;
		    }
		}
		if ( cdir == NULL ) {
		    ++extend;
		    if ( movecwd( vol, dir ) < 0 ) {
			return( NULL );
		    }
		    cdir = extenddir( vol, dir, path );
		}

	    } else {
		cdir = extenddir( vol, dir, path );
	    }

	    if ( cdir == NULL ) {
		if ( len > 0 ) {
		    return( NULL );
		}

	    } else {
		dir = cdir;
		*path = '\0';
	    }
	}
    }
}

/*
 * Move curdir to dir, with a possible chdir()
 */
movecwd( vol, dir )
    struct vol	*vol;
    struct dir	*dir;
{
    struct dir	*d;
    char	path[ MAXPATHLEN ], *p, *u;
    int		n;

    if ( dir == curdir ) {
	return( 0 );
    }
    if ( dir->d_did == 1 ) {
	return( -1 );
    }

    p = path + sizeof( path ) - 1;
    *p-- = '\0';
    *p = '.';
    for ( d = dir; d->d_parent != NULL && d != curdir; d = d->d_parent ) {
	*--p = '/';
	u = mtoupath( d->d_name );
	n = strlen( u );
	p -= n;
	strncpy( p, u, n );
    }
    if ( d != curdir ) {
	*--p = '/';
	n = strlen( vol->v_path );
	p -= n;
	strncpy( p, vol->v_path, n );
    }
    if ( chdir( p ) < 0 ) {
	return( -1 );
    }
    curdir = dir;
    return( 0 );
}

getdirparams( bitmap, upath, dir, st, buf, buflen )
    u_short		bitmap;
    char		*upath;
    struct dir		*dir;
    struct stat		*st;
    char		*buf;
    int			*buflen;
{
    struct maccess	ma;
    struct adouble	ad;
    char		*data, *nameoff = 0;
    DIR			*dp;
    struct dirent	*de;
    int			bit = 0, aint, isad = 1;
    u_short		ashort;

    if ( ad_open( upath, ADFLAGS_HF|ADFLAGS_DIR, O_RDONLY, 0777, &ad ) < 0 ) {
	isad = 0;
    }

    data = buf;
    while ( bitmap != 0 ) {
	while (( bitmap & 1 ) == 0 ) {
	    bitmap = bitmap>>1;
	    bit++;
	}

	switch ( bit ) {
	case DIRPBIT_ATTR :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_ATTR, &ashort,
			sizeof( u_short ));
	    } else {
		ashort = 0;
	    }
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case DIRPBIT_PDID :
	    if ( dir->d_parent == NULL ) {
		aint = htonl( 1 );
	    } else {
		aint = dir->d_parent->d_did;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_CDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_CREATE, &aint,
			sizeof( int ));
		if ( aint == 0 ) {
		    aint = htonl( st->st_mtime );
		}
	    } else {
		aint = htonl( st->st_mtime );
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_MDATE :
	    aint = htonl( st->st_mtime );
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_BDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_BACKUP, &aint,
			sizeof( int ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_FINFO :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FINDERI ), data, 32 );
	    } else {
		bzero( data, 32 );
	    }
	    data += 32;
	    break;

	case DIRPBIT_LNAME :
	    nameoff = data;
	    data += sizeof( u_short );
	    break;

	case DIRPBIT_SNAME :
	    ashort = 0;
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case DIRPBIT_DID :
	    bcopy( &dir->d_did, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_OFFCNT :
	    ashort = 0;
	    if (( dp = opendir( upath )) == NULL ) {
		ashort = 0;
	    } else {
		while (( de = readdir( dp )) != NULL ) {
		    if( de->d_name[ 0 ] != '.' ) {
			ashort++;
		    }
		}
		closedir( dp );
	    }

	    ashort = htons( ashort );
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case DIRPBIT_UID :
	    aint = st->st_uid;
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_GID :
	    aint = st->st_gid;
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case DIRPBIT_ACCESS :
	    utommode( st, &ma, dir );
#ifdef AFS
	    afsmode( upath, &ma, dir );
#endif AFS
	    bcopy( &ma, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	default :
	    if ( isad ) {
		ad_close( &ad, ADFLAGS_HF );
	    }
	    return( AFPERR_PARAM );
	}
	bitmap = bitmap>>1;
	bit++;
    }
    if ( nameoff != 0 ) {
	ashort = htons( data - buf );
	bcopy( &ashort, nameoff, sizeof( u_short ));

	aint = strlen( dir->d_name );
	aint = ( aint > 31 ) ? 31 : aint;

	*data++ = aint;
	bcopy( dir->d_name, data, aint );
	data += aint;
    }
    if ( isad ) {
	ad_close( &ad, ADFLAGS_HF );
    }
    *buflen = data - buf;
    return( AFP_OK );
}

afp_setdirparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
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

    /*
     * If ibuf is odd, make it even.
     */
    if ((int)ibuf & 1 ) {
	ibuf++;
    }

    if (( rc = setdirparams( path, bitmap, ibuf )) == AFP_OK ) {
	setvoltime( vol );
    }
    return( rc );
}

setdirparams( path, bitmap, buf )
    char		*path, *buf;
    short		bitmap;
{
    struct maccess	ma;
    struct adouble	ad;
    int			bit = 0, aint, isad = 1;
    u_short		ashort, bshort;

    if ( ad_open( mtoupath( path ), ADFLAGS_HF|ADFLAGS_DIR,
	    O_RDWR|O_CREAT, 0666, &ad ) < 0 ) {
	/*
	 * Check to see what we're trying to to set.  If it's anything
	 * but ACCESS, UID, or GID, give an error.  If it's any of those
	 * three, we don't need the ad to be open, so just continue.
	 */
	if ( bitmap &
		~((1<<DIRPBIT_ACCESS)|(1<<DIRPBIT_UID)|(1<<DIRPBIT_GID))) {
	    return( AFPERR_ACCESS );
	}
	isad = 0;
    } else {
	/*
	 * Check to see if a create was necessary. If it was, we'll want
	 * to set our name, etc.
	 */
	if ( ad_getoflags( &ad, ADFLAGS_HF ) & O_CREAT ) {
	    ad_setentrylen( &ad, ADEID_NAME, strlen( curdir->d_name ));
	    bcopy( curdir->d_name, ad_entry( &ad, ADEID_NAME ),
		    ad_getentrylen( &ad, ADEID_NAME ));
	}
    }

    while ( bitmap != 0 ) {
	while (( bitmap & 1 ) == 0 ) {
	    bitmap = bitmap>>1;
	    bit++;
	}

	switch( bit ) {
	case DIRPBIT_ATTR :
	    bcopy( buf, &ashort, sizeof( u_short ));
	    bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_ATTR, &bshort,
		    sizeof( u_short ));
	    if ( ntohs( ashort ) & ATTRBIT_SETCLR ) {
		bshort |= htons( ntohs( ashort ) & ~ATTRBIT_SETCLR );
	    } else {
		bshort &= ~ashort;
	    }
	    bcopy( &bshort, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_ATTR,
		    sizeof( u_short ));
	    buf += sizeof( u_short );
	    break;

	case DIRPBIT_CDATE :
	    bcopy( buf, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_CREATE,
		    sizeof( int ));
	    buf += sizeof( int );
	    break;

	case DIRPBIT_MDATE :
	    bcopy( buf, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_MODIFY,
		    sizeof( int ));
	    buf += sizeof( int );
	    break;

	case DIRPBIT_BDATE :
	    bcopy( buf, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_BACKUP,
		    sizeof( int ));
	    buf += sizeof( int );
	    break;

	case DIRPBIT_FINFO :
	    /*
	     * Alright, we admit it, this is *really* sick!
	     * The 4 bytes that we don't copy, when we're dealing
	     * with the root of a volume, are the directory's
	     * location information. This eliminates that annoying
	     * behavior one sees when mounting above another mount
	     * point.
	     */
	    if ( ntohl( curdir->d_did ) == 2 ) {
		bcopy( buf, ad_entry( &ad, ADEID_FINDERI ), 10 );
		bcopy( buf + 14, ad_entry( &ad, ADEID_FINDERI ) + 14, 18 );
	    } else {
		bcopy( buf, ad_entry( &ad, ADEID_FINDERI ), 32 );
	    }
	    buf += 32;
	    break;

	case DIRPBIT_UID :	/* What kind of loser mounts as root? */
	    buf += sizeof( int );
	    break;

	case DIRPBIT_GID :
	    bcopy( buf, &aint, sizeof( int ));
	    buf += sizeof( int );
	    if ( ntohl( curdir->d_did ) == 2 ) {
		setdeskowner( -1, aint );
	    }
	    if ( setdirowner( -1, aint ) < 0 ) {
		switch ( errno ) {
		case EPERM :
		case EACCES :
		    return( AFPERR_ACCESS );
		case EROFS :
		    return( AFPERR_VLOCK );
		default :
		    syslog( LOG_ERR, "setdirparam: setdirowner: %m" );
		    return( AFPERR_PARAM );
		}
	    }
	    break;

	case DIRPBIT_ACCESS :
	    bcopy( buf, &ma, sizeof( struct maccess ));
	    buf += sizeof( int );
	    if ( ntohl( curdir->d_did ) == 2 ) {
		setdeskmode( mtoumode( &ma ));
	    }
	    if ( setdirmode( mtoumode( &ma )) < 0 ) {
		switch ( errno ) {
		case EPERM :
		case EACCES :
		    return( AFPERR_ACCESS );
		case EROFS :
		    return( AFPERR_VLOCK );
		default :
		    syslog( LOG_ERR, "setdirparam: setdirmode: %m" );
		    return( AFPERR_PARAM );
		}
	    }
	    break;

	default :
	    return( AFPERR_BITMAP );
	}

	bitmap = bitmap>>1;
	bit++;
    }

    if ( isad ) {
	ad_flush( &ad, ADFLAGS_HF );
	ad_close( &ad, ADFLAGS_HF );
    }
    return( AFP_OK );
}

afp_createdir( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct adouble	ad;
    struct timeval	tv;
    struct vol		*vol;
    struct dir		*dir;
    char		*path, *upath;
    int			did;
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

    upath = mtoupath( path );
    if ( ad_mkdir( upath, 0777 ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EROFS :
	    return( AFPERR_VLOCK );
	case EACCES :
	    return( AFPERR_ACCESS );
	case EEXIST :
	    return( AFPERR_EXIST );
	case ENOSPC :
	case EDQUOT :
	    return( AFPERR_DFULL );
	default :
	    return( AFPERR_PARAM );
	}
    }
    dir = adddir( vol, curdir, path, strlen( path ));
    if ( movecwd( vol, dir ) < 0 ) {
	return( AFPERR_PARAM );
    }

    if ( ad_open( "", ADFLAGS_HF|ADFLAGS_DIR, O_RDWR|O_CREAT,
	    0666, &ad ) < 0 ) {
	return( AFPERR_ACCESS );
    }
    ad_setentrylen( &ad, ADEID_NAME, strlen( path ));
    bcopy( path, ad_entry( &ad, ADEID_NAME ),
	    ad_getentrylen( &ad, ADEID_NAME ));
    if ( gettimeofday( &tv, 0 ) < 0 ) {
	return( AFPERR_PARAM );
    }
    tv.tv_sec = htonl( tv.tv_sec );
    bcopy( &tv.tv_sec, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_CREATE,
	    sizeof( int ));
    bcopy( &tv.tv_sec, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_MODIFY,
	    sizeof( int ));
    ad_flush( &ad, ADFLAGS_HF );
    ad_close( &ad, ADFLAGS_HF );

    bcopy( &dir->d_did, rbuf, sizeof( int ));
    *rbuflen = sizeof( int );
    setvoltime( vol );
    return( AFP_OK );
}

renamedir( src, dst, dir, newparent, newname )
    char	*src, *dst, *newname;
    struct dir	*dir, *newparent;
{
    struct adouble	ad;
    struct stat		st;
    struct dir		*parent, *d;
    int			len;

    if ( stat( dst, &st ) == 0 ) {
	return( AFPERR_EXIST );
    }

    if ( rename( src, dst ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EACCES :
	    return( AFPERR_ACCESS );
	default : 
	    return( AFPERR_PARAM );
	}
    }

    if ( ad_open( dst, ADFLAGS_HF|ADFLAGS_DIR, O_RDWR, 0, &ad ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EACCES :
	    return( AFPERR_ACCESS );
	default : 
	    return( AFPERR_PARAM );
	}
    }
    len = strlen( newname );
    ad_setentrylen( &ad, ADEID_NAME, len );
    bcopy( newname, ad_entry( &ad, ADEID_NAME ), len );
    ad_flush( &ad, ADFLAGS_HF );
    ad_close( &ad, ADFLAGS_HF );

    if (( dir->d_name = (char *)realloc( dir->d_name, len + 1 )) == NULL ) {
	syslog( LOG_ERR, "renamedir: realloc: %m" );
	exit( 1 );
    }
    strcpy( dir->d_name, newname );

    if (( parent = dir->d_parent ) == NULL ) {
	return( AFP_OK );
    }
    if ( parent == newparent ) {
	return( AFP_OK );
    }
    if ( parent->d_child == dir ) {
	parent->d_child = dir->d_next;
    } else {
	for ( d = parent->d_child; d && d->d_next; d = d->d_next ) {
	    if ( d->d_next == dir ) {
		break;
	    }
	}
	if ( d->d_next != dir ) {
	    return( AFPERR_PARAM );
	}
	d->d_next = dir->d_next;
    }

    dir->d_parent = newparent;
    dir->d_next = newparent->d_child;
    newparent->d_child = dir;

    return( AFP_OK );
}

deletecurdir( vol )
    struct vol	*vol;
{
    struct dir	*fdir, *dir;

    if ( curdir->d_parent == NULL ) {
	return( AFPERR_ACCESS );
    }

    if ( curdir->d_child != NULL ) {
	return( AFPERR_DIRNEMPT );
    }

    fdir = curdir;

    if ( unlink( ad_path( "", ADFLAGS_DIR )) < 0 ) {
	switch ( errno ) {
	case EACCES :
	    return( AFPERR_ACCESS );
	case ENOENT :
	    break;
	default :
	    return( AFPERR_PARAM );
	}
    }
    if ( rmdir( ".AppleDouble" ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    break;
	case ENOTEMPTY :
	    return( AFPERR_DIRNEMPT );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }
    if ( movecwd( vol, curdir->d_parent ) < 0 ) {
	return( AFPERR_NOOBJ );
    }

    if ( rmdir( mtoupath( fdir->d_name )) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case ENOTEMPTY :
	    return( AFPERR_DIRNEMPT );
	case EACCES :
	    return( AFPERR_ACCESS );
	default : 
	    return( AFPERR_PARAM );
	}
    }

    if ( curdir->d_child == fdir ) {
	curdir->d_child = fdir->d_next;
    } else {
	for ( dir = curdir->d_child; dir && dir->d_next;
		dir = dir->d_next ) {
	    if ( fdir == dir->d_next ) {
		break;
	    }
	}
	if ( fdir != dir->d_next ) {
	    return( AFPERR_PARAM );
	}
	dir->d_next = fdir->d_next;
    }
    dirfree( vol, fdir );

    return( AFP_OK );
}

afp_mapid( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct passwd	*pw;
    struct group	*gr;
    char		*name;
    int			id, len, sfunc;

    ibuf++;
    sfunc = *ibuf++;
    bcopy( ibuf, &id, sizeof( int ));

    if ( id != 0 ) {
	switch ( sfunc ) {
	case 1 :
	    if (( pw = getpwuid( id )) == NULL ) {
		*rbuflen = 0;
		return( AFPERR_NOITEM );
	    }
	    name = pw->pw_name;
	    break;

	case 2 :
	    if (( gr = (struct group *)getgrgid( id )) == NULL ) {
		*rbuflen = 0;
		return( AFPERR_NOITEM );
	    }
	    name = gr->gr_name;
	    break;

	default :
	    *rbuflen = 0;
	    return( AFPERR_PARAM );
	}

	len = strlen( name );

    } else {
	len = 0;
	name = 0;
    }

    *rbuf++ = len;
    if ( len > 0 ) {
	bcopy( name, rbuf, len );
    }
    *rbuflen = len + 1;
    return( AFP_OK );
}

afp_mapname( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct passwd	*pw;
    struct group	*gr;
    int			len, sfunc, id;

    ibuf++;
    sfunc = *ibuf++;
    len = *ibuf++;
    ibuf[ len ] = '\0';

    if ( len != 0 ) {
	switch ( sfunc ) {
	case 3 :
	    if (( pw = (struct passwd *)getpwnam( ibuf )) == NULL ) {
		*rbuflen = 0;
		return( AFPERR_NOITEM );
	    }
	    id = pw->pw_uid;
	    break;

	case 4 :
	    if (( gr = (struct group *)getgrnam( ibuf )) == NULL ) {
		*rbuflen = 0;
		return( AFPERR_NOITEM );
	    }
	    id = gr->gr_gid;
	    break;
	default :
	    *rbuflen = 0;
	    return( AFPERR_PARAM );
	}
    } else {
	id = 0;
    }

    bcopy( &id, rbuf, sizeof( int ));
    *rbuflen = sizeof( int );
    return( AFP_OK );
}
