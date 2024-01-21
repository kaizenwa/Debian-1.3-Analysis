/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/errno.h>
#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/stat.h>
#if defined( sun ) && defined( __svr4__ )
#include </usr/ucbinclude/sys/file.h>
#else sun __svr4__
#include <sys/file.h>
#endif sun __svr4__
#include <netatalk/endian.h>
#include <atalk/adouble.h>
#include <atalk/afp.h>
#include <utime.h>
#include <fcntl.h>
#include <dirent.h>
#include <strings.h>
#include <stdio.h>
#include <unistd.h>

#include "directory.h"
#include "volume.h"
#include "file.h"
#include "globals.h"

extern int	errno;

u_char		ufinderi[] = {
    'T', 'E', 'X', 'T', 'U', 'N', 'I', 'X',
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
};

getfilparams( bitmap, path, dir, st, buf, buflen )
    u_short	bitmap;
    char	*path;
    struct dir	*dir;
    struct stat	*st;
    char	*buf;
    int		*buflen;
{
    struct stat		hst;
    struct adouble	ad;
    struct extmap	*em;
    char		*data, *nameoff = 0;
    int			bit = 0, isad = 1, aint;
    u_short		ashort;

    if ( ad_open( mtoupath( path ), ADFLAGS_HF, O_RDONLY, 0, &ad ) < 0 ) {
	isad = 0;
    } else {
	if ( fstat( ad_hfileno( &ad ), &hst ) < 0 ) {
	    syslog( LOG_ERR, "getfilparams fstat: %m" );
	}
    }

    data = buf;
    while ( bitmap != 0 ) {
	while (( bitmap & 1 ) == 0 ) {
	    bitmap = bitmap>>1;
	    bit++;
	}

	switch ( bit ) {
	case FILPBIT_ATTR :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_ATTR, &ashort,
			sizeof( u_short ));
	    } else {
		ashort = 0;
	    }
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case FILPBIT_PDID :
	    bcopy( &dir->d_did, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_CDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_CREATE, &aint,
			sizeof( int ));
	    } else {
		aint = htonl( st->st_mtime );
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_MDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_MODIFY, &aint,
			sizeof( int ));
		aint = ntohl( aint );
		if ( st->st_mtime > aint && hst.st_mtime < st->st_mtime ) {
		    aint = st->st_mtime;
		}
	    } else {
		aint = st->st_mtime;
	    }
	    aint = htonl( aint );
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_BDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_BACKUP, &aint,
			sizeof( int ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_FINFO :
	    if ( !isad ||
		    bcmp( ad_entry( &ad, ADEID_FINDERI ), ufinderi, 8 ) == 0 ) {
		bcopy( ufinderi, data, 32 );
		if (( em = getextmap( path )) != NULL ) {
		    bcopy( em->em_type, data, sizeof( em->em_type ));
		    bcopy( em->em_creator, data + 4, sizeof( em->em_creator ));
		}
	    } else {
		bcopy( ad_entry( &ad, ADEID_FINDERI ), data, 32 );
	    }
	    data += 32;
	    break;

	case FILPBIT_LNAME :
	    nameoff = data;
	    data += sizeof( u_short );
	    break;

	case FILPBIT_SNAME :
	    ashort = 0;
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case FILPBIT_FNUM :
	    /*
	     * What a fucking mess.  First thing:  DID and FNUMs are
	     * in the same space for purposes of enumerate (and several
	     * other wierd places).  While we consider this Apple's bug,
	     * this is the work-around:  In order to maintain constant and
	     * unique DIDs and FNUMs, we monotonically generate the DIDs
	     * during the session, and derive the FNUMs from the filesystem.
	     * Since the DIDs are small, we insure that the FNUMs are fairly
	     * large by setting thier high bits to the device number.
	     *
	     * AFS already does something very similar to this for the
	     * inode number, so we don't repeat the procedure.
	     */
#ifdef AFS
	    aint = st->st_ino;
#else AFS
	    aint = ( st->st_dev << 16 ) | ( st->st_ino & 0x0000ffff );
#endif AFS
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_DFLEN :
	    aint = htonl( st->st_size );
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_RFLEN :
	    if ( isad ) {
		aint = htonl( ad_getentrylen( &ad, ADEID_RFORK ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	default :
	    if ( isad ) {
		ad_close( &ad, ADFLAGS_HF );
	    }
	    return( AFPERR_BITMAP );
	}
	bitmap = bitmap>>1;
	bit++;
    }
    if ( nameoff != 0 ) {
	ashort = htons( data - buf );
	bcopy( &ashort, nameoff, sizeof( u_short ));
	aint = strlen( path );
	aint = ( aint > 31 ) ? 31 : aint;
	*data++ = aint;
	bcopy( path, data, aint );
	data += aint;
    }
    if ( isad ) {
	ad_close( &ad, ADFLAGS_HF );
    }
    *buflen = data - buf;
    return( AFP_OK );
}

afp_createfile( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct adouble	ad;
    struct timeval	tv;
    struct vol		*vol;
    struct dir		*dir;
    char		*path;
    int			creatf, did, openf;
    u_short		vid;

    *rbuflen = 0;
    ibuf++;
    creatf = *ibuf++;

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

    if ( creatf ) {
	openf = O_RDWR|O_CREAT|O_TRUNC;
    } else {
	openf = O_RDWR|O_CREAT|O_EXCL;
    }

    if ( ad_open( mtoupath( path ), ADFLAGS_DF|ADFLAGS_HF, openf,
	    0666, &ad ) < 0 ) {
	switch ( errno ) {
	case EEXIST :
	    return( AFPERR_EXIST );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }

    ad_setentrylen( &ad, ADEID_NAME, strlen( path ));
    bcopy( path, ad_entry( &ad, ADEID_NAME ),
	    ad_getentrylen( &ad, ADEID_NAME ));

    if ( gettimeofday( &tv, 0 ) < 0 ) {
	syslog( LOG_ERR, "afp_createfile: gettimeofday: %m" );
	exit( 1 );
    }
    tv.tv_sec = htonl( tv.tv_sec );
    bcopy( &tv.tv_sec, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_CREATE,
	    sizeof( tv.tv_sec ));
    bcopy( &tv.tv_sec, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_MODIFY,
	    sizeof( tv.tv_sec ));

    ad_flush( &ad, ADFLAGS_DF|ADFLAGS_HF );
    ad_close( &ad, ADFLAGS_DF|ADFLAGS_HF );
    setvoltime( vol );
    return( AFP_OK );
}

afp_setfilparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol;
    struct dir	*dir;
    char	*path;
    int		did, rc;
    u_short	vid, bitmap;

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

    if ((int)ibuf & 1 ) {
	ibuf++;
    }

    if (( rc = setfilparams( path, bitmap, ibuf )) == AFP_OK ) {
	setvoltime( vol );
    }

    return( rc );
}

setfilparams( path, bitmap, buf )
    char	*path, *buf;
    u_short	bitmap;
{
    struct adouble	ad;
    struct extmap	*em;
    int			bit = 0;
    u_short		ashort, bshort;
    u_long		along;
    time_t		atime;
    struct utimbuf	ut;

    if ( ad_open( mtoupath( path ), ADFLAGS_HF, O_RDWR|O_CREAT,
	    0666, &ad ) < 0 ) {
	syslog( LOG_INFO, "setfilparams: ad_open %s: %m", path );
	return( AFPERR_ACCESS );
    }

    if ( ad_getoflags( &ad, ADFLAGS_HF ) & O_CREAT ) {
	ad_setentrylen( &ad, ADEID_NAME, strlen( path ));
	bcopy( path, ad_entry( &ad, ADEID_NAME ),
		ad_getentrylen( &ad, ADEID_NAME ));
    }

    while ( bitmap != 0 ) {
	while (( bitmap & 1 ) == 0 ) {
	    bitmap = bitmap>>1;
	    bit++;
	}

	switch(  bit ) {
	case FILPBIT_ATTR :
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

	case FILPBIT_CDATE :
	    bcopy( buf, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_CREATE,
		    sizeof( int ));
	    buf += sizeof( int );
	    break;

	case FILPBIT_MDATE :
	    bcopy( buf, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_MODIFY,
		    sizeof( int ));
	    bcopy( buf, &along, sizeof( along ));
	    atime = ntohl( along );
	    ut.actime = atime;
	    ut.modtime = atime;
	    utime( mtoupath( path ), &ut );
	    buf += sizeof( int );
	    break;

	case FILPBIT_BDATE :
	    bcopy( buf, ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_BACKUP,
		    sizeof( int ));
	    buf += sizeof( int );
	    break;

	case FILPBIT_FINFO :
	    if (( ad_getoflags( &ad, ADFLAGS_HF ) & O_CREAT ) ||
		    bcmp( ad_entry( &ad, ADEID_FINDERI ), ufinderi, 8 ) == 0 ) {
		if (( em = getextmap( path )) != NULL ) {
		    if ( bcmp( buf, em->em_type, sizeof( em->em_type )) == 0 &&
			    bcmp( buf + 4, em->em_creator,
			    sizeof( em->em_creator )) == 0 ) {
			bcopy( ufinderi, buf, 8 );
		    }
		}
	    }
	    bcopy( buf, ad_entry( &ad, ADEID_FINDERI ), 32 );
	    buf += 32;
	    break;

	default :
	    return( AFPERR_BITMAP );
	}

	bitmap = bitmap>>1;
	bit++;
    }

    ad_flush( &ad, ADFLAGS_HF );
    if ( ad_close( &ad, ADFLAGS_HF ) < 0 ) {
	syslog( LOG_INFO, "setfilparams: ad_close %s: %m", path );
	return( AFPERR_PARAM );
    }
    return( AFP_OK );
}

/*
 * renamefile and copyfile take the old and new unix pathnames
 * and the new mac name.
 */
renamefile( src, dst, newname )
    char	*src, *dst, *newname;
{
    struct adouble	ad;
    struct stat		st;
    char		adsrc[ MAXPATHLEN ];
    int			len, rc;

    /*
     * Note that this is only checking the existance of the data file,
     * not the header file.  The thinking is that if the data file doesn't
     * exist, but the header file does, the right thing to do is remove
     * the data file silently.
     */
    if ( stat( dst, &st ) == 0 ) {
	return( AFPERR_EXIST );
    }

    if ( rename( src, dst ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EACCES :
	    return( AFPERR_ACCESS );
	case EXDEV :			/* Cross device move -- try copy */
	    if (( rc = copyfile( src, dst, newname )) != AFP_OK ) {
		deletefile( dst );
		return( rc );
	    }
	    rc = deletefile( src );
	    return( rc );
	default :
	    return( AFPERR_PARAM );
	}
    }

    strcpy( adsrc, ad_path( src, 0 ));
    if ( rename( adsrc, ad_path( dst, 0 )) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFP_OK );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }

    if ( ad_open( dst, ADFLAGS_HF, O_RDWR, 0666, &ad ) < 0 ) {
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

    return( AFP_OK );
}

afp_copyfile( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol;
    struct dir	*dir;
    char	newname[ MAXNAMLEN ], *path, *p;
    int		sdid, ddid;
    int		plen;
    short	svid, dvid;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &svid, sizeof( short ));
    ibuf += sizeof( short );
    if (( vol = getvolbyvid( svid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &sdid, sizeof( int ));
    ibuf += sizeof( int );
    if (( dir = dirsearch( vol, sdid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &dvid, sizeof( short ));
    ibuf += sizeof( short );
    bcopy( ibuf, &ddid, sizeof( int ));
    ibuf += sizeof( int );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }
    if ( *path == '\0' ) {
	return( AFPERR_BADTYPE );
    }
    strcpy( newname, path );

    p = ctoupath( vol, dir, newname );

    if (( vol = getvolbyvid( dvid )) == NULL ) {
	return( AFPERR_PARAM );
    }
    if (( dir = dirsearch( vol, ddid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }
    if ( *path != '\0' ) {
	return( AFPERR_BADTYPE );
    }

    /* one of the handful of places that knows about the path type */
    if ( *ibuf++ != 2 ) {
	return( AFPERR_PARAM );
    }
    if (( plen = (unsigned char)*ibuf++ ) != 0 ) {
	strncpy( newname, ibuf, plen );
	newname[ plen ] = '\0';
    }
    if ( copyfile( p, mtoupath( newname ), newname ) < 0 ) {
	return( AFPERR_ACCESS );
    }

    setvoltime( vol );
    return( AFP_OK );
}

copyfile( src, dst, newname )
    char	*src, *dst, *newname;
{
    struct adouble	ad;
    char		filebuf[ 8192 ]; 
    int			cc, len, sfd, dfd;

    if (( sfd = open( ad_path( src, 0 ), O_RDONLY, 0 )) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    break;
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    } else {
	if (( dfd = open( ad_path( dst, 0 ), O_WRONLY|O_CREAT,
		ad_mode( ad_path( dst, 0 ), 0666 ))) < 0 ) {
	    close( sfd );
	    switch ( errno ) {
	    case ENOENT :
		return( AFPERR_NOOBJ );
	    case EACCES :
		return( AFPERR_ACCESS );
	    default :
		return( AFPERR_PARAM );
	    }
	}

	while (( cc = read( sfd, filebuf, sizeof( filebuf ))) > 0 ) {
	    if ( write( dfd, filebuf, cc ) != cc ) {
		close( sfd );
		close( dfd );
		return( AFPERR_PARAM );
	    }
	}
	close( sfd );
	close( dfd );
    }

    if (( sfd = open( src, O_RDONLY, 0 )) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }
    if (( dfd = open( dst, O_WRONLY|O_CREAT, ad_mode( dst, 0666 ))) < 0 ) {
	close( sfd );
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOOBJ );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }

    while (( cc = read( sfd, filebuf, sizeof( filebuf ))) > 0 ) {
	if ( write( dfd, filebuf, cc ) != cc ) {
	    close( sfd );
	    close( dfd );
	    return( AFPERR_PARAM );
	}
    }
    close( sfd );
    close( dfd );

    if ( ad_open( dst, ADFLAGS_HF, O_RDWR|O_CREAT, 0666, &ad ) < 0 ) {
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

    return( AFP_OK );
}

deletefile( file )
    char		*file;
{
    struct adouble	ad;
    int			adflags;

    adflags = ADFLAGS_DF|ADFLAGS_HF;
    if ( ad_open( file, adflags, O_RDWR, 0, &ad ) < 0 ) {
	if ( errno == ENOENT ) {
	    ad_close( &ad, adflags );
	    adflags = ADFLAGS_DF;
	    if ( ad_open( file, adflags, O_RDWR, 0, &ad ) < 0 ) {
		ad_close( &ad, adflags );
		return( AFPERR_NOOBJ );
	    }
	} else {
	    ad_close( &ad, adflags );
	    if ( errno == EACCES ) {
		return( AFPERR_ACCESS );
	    } else {
		return( AFPERR_PARAM );
	    }
	}
    }

    if ( adflags & ADFLAGS_HF ) {
        if ( flock( ad_hfileno( &ad ), LOCK_EX|LOCK_NB ) < 0 ) {
	    ad_close( &ad, adflags );
	    return( AFPERR_BUSY );
	}
    }
    if ( flock( ad_dfileno( &ad ), LOCK_EX|LOCK_NB ) < 0 ) {
	ad_close( &ad, adflags );
	return( AFPERR_BUSY );
    }

    if ( unlink( ad_path( file, 0 )) < 0 ) {
	switch ( errno ) {
	case EACCES :
	    ad_close( &ad, adflags );
	    return( AFPERR_ACCESS );
	case ENOENT :
	    break;
	default :
	    ad_close( &ad, adflags );
	    return( AFPERR_PARAM );
	}
    }

    if ( unlink( file ) < 0 ) {
	switch ( errno ) {
	case EACCES :
	    ad_close( &ad, adflags );
	    return( AFPERR_ACCESS );
	case ENOENT :
	    ad_close( &ad, adflags );
	    return( AFPERR_NOOBJ );
	default :
	    ad_close( &ad, adflags );
	    return( AFPERR_PARAM );
	}
    }

    ad_close( &ad, adflags );
    return( AFP_OK );
}
