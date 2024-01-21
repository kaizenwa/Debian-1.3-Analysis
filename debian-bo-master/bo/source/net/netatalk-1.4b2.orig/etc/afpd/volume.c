/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/time.h>
#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <netatalk/endian.h>
#include <atalk/adouble.h>
#include <atalk/afp.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <pwd.h>

#include "directory.h"
#include "file.h"
#include "volume.h"
#include "globals.h"

struct vol	*volumes = NULL;
int		lastvid = 0;
char		*Trash = "\02\024Network Trash Folder";
struct extmap	*extmap = NULL, *defextmap = NULL;

afp_getsrvrparms( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct timeval	tv;
    struct stat		st;
    struct vol		*volume;
    struct passwd	*pwent;
    char		*data;
    int			vcnt, len;

    if ( !uservolfirst ) {
	readvolfile( systemvol, NULL, 0 );
    }
    if ( username == NULL ) {
	readvolfile( defaultvol, NULL, 1 );
    } else if (( pwent = getpwnam( username )) != NULL ) {
	/*
	 * Read user's AppleVolumes or .AppleVolumes file
	 * If neither are readable, read the default volumes file
	 */
#ifdef DOWNCASE
	if ( readvolfile( pwent->pw_dir, "applevolumes", 1 ) < 0 &&
	    readvolfile( pwent->pw_dir, ".applevolumes", 1 ) < 0 &&
	    defaultvol != NULL ) {
		if ( readvolfile( defaultvol, NULL, 1 ) < 0 ) {
		    creatvol( pwent->pw_dir, NULL );
		}
	}
#else !DOWNCASE
	if ( readvolfile( pwent->pw_dir, "AppleVolumes", 1 ) < 0 &&
	    readvolfile( pwent->pw_dir, ".AppleVolumes", 1 ) < 0 &&
	    defaultvol != NULL ) {
		if ( readvolfile( defaultvol, NULL, 1 ) < 0 ) {
		    creatvol( pwent->pw_dir, NULL );
		}
	}
#endif DOWNCASE
    }
    if ( uservolfirst ) {
	readvolfile( systemvol, NULL, 0 );
    }

    data = rbuf + 5;
    for ( vcnt = 0, volume = volumes; volume; volume = volume->v_next ) {
	if ( stat( volume->v_path, &st ) < 0 ) {
	    syslog( LOG_INFO, "afp_getsrvrparms: stat %s: %m", volume->v_path );
	    continue;		/* can't access directory */
	}
	if (( st.st_mode & S_IFDIR ) == 0 ) {
	    continue;		/* not a dir */
	}

	*data++ = 0;
	len = strlen( volume->v_name );
	*data++ = len;
	bcopy( volume->v_name, data, len );
	data += len;
	vcnt++;
    }

    *rbuflen = data - rbuf;
    data = rbuf;
    if ( gettimeofday( &tv, 0 ) < 0 ) {
	syslog( LOG_ERR, "afp_getsrvrparms: gettimeofday: %m" );
	exit( 1 );
    }
    tv.tv_sec = htonl( tv.tv_sec );
    bcopy( &tv.tv_sec, data, sizeof( tv.tv_sec ));
    data += sizeof( tv.tv_sec );
    *data = vcnt;
    return( AFP_OK );
}

creatvol( path, name )
    char	*path, *name;
{
    struct vol	*volume;
    int		vlen;

    if ( name == NULL || *name == '\0' ) {
	if (( name = rindex( path, '/' )) == NULL ) {
	    return;	/* Obviously not a fully qualified path */
	}
	name++;
    }

    for ( volume = volumes; volume; volume = volume->v_next ) {
	if ( strcasecmp( volume->v_name, name ) == 0 ) {
	    return;	/* Won't be able to access it, anyway... */
	}
    }

    vlen = strlen( name );
    if ( vlen > 27 ) {
	return;
    }

    if (( volume =
	    (struct vol *)malloc( sizeof( struct vol ))) == NULL ) {
	syslog( LOG_ERR, "creatvol: malloc: %m" );
	exit( 1 );
    }
    if (( volume->v_name =
	    (char *)malloc( vlen + 1 )) == NULL ) {
	syslog( LOG_ERR, "creatvol: malloc: %m" );
	exit( 1 );
    }
    if (( volume->v_path =
	    (char *)malloc( strlen( path ) + 1 )) == NULL ) {
	syslog( LOG_ERR, "creatvol: malloc: %m" );
	exit( 1 );
    }

    strcpy( volume->v_name, name );
    strcpy( volume->v_path, path );

    volume->v_dir = NULL;
    volume->v_did = NULL;
    volume->v_flags = 0;
#ifdef __svr4__
    volume->v_qfd = -1;
#else __svr4__
    volume->v_gvs = NULL;
#endif __svr4__
    volume->v_time = 0;
    volume->v_vid = lastvid++;
    volume->v_lastdid = 3;

    volume->v_next = volumes;
    volumes = volume;
}

char *myfgets( buf, size, fp )
    char	*buf;
    int		size;
    FILE	*fp;
{
    char	*p;
    int		c;

    p = buf;
    while ((( c = getc( fp )) != EOF ) && ( size > 0 )) {
	if ( c == '\n' || c == '\r' ) {
	    *p++ = '\n';
	    break;
	} else {
	    *p++ = c;
	}
	size--;
    }

    if ( p == buf ) {
	return( NULL );
    } else {
	*p = '\0';
	return( buf );
    }
}

/*
 * Read a volume configuration file and add the volumes contained within to
 * the global volume list.  If p2 is non-NULL, the file that is opened is
 * p1/p2
 *
 * Lines that begin with # and blank lines are ignored.
 * Volume lines are of the form:
 *		<unix path> [<volume name> [<flags>,...]]
 *		<extension> TYPE [CREATOR]
 */
readvolfile( p1, p2, user )
    char	*p1, *p2;
    int		user;
{
    FILE		*fp;
    char		path[ MAXPATHLEN ], tmp[ MAXPATHLEN ],
			volname[ 28 ], buf[ BUFSIZ ],
			type[ 5 ], creator[ 5 ];
    char		*u, *p;
    int			quoted = 0;
    struct passwd	*pw;

    strcpy( path, p1 );
    if ( p2 != NULL ) {
	strcat( path, "/" );
	strcat( path, p2 );
    }

    if (( fp = fopen( path, "r" )) == NULL ) {
	return( -1 );
    }

    while ( myfgets( buf, sizeof( buf ), fp ) != NULL ) {
	initline( strlen( buf ), buf );
	parseline( sizeof( path ) - 1, path );
	switch ( *path ) {
	case '\0' :
	case '#' :
	    continue;

	case '~' :
	    if (( p = index( path, '/' )) != NULL ) {
		*p++ = '\0';
	    }
	    u = path;
	    u++;
	    if ( *u == '\0' ) {
		u = username;
	    }
	    if ( u == NULL || ( pw = getpwnam( u )) == NULL ) {
		continue;
	    }
	    strcpy( tmp, pw->pw_dir );
	    if ( p != NULL && *p != '\0' ) {
		strcat( tmp, "/" );
		strcat( tmp, p );
	    }
	    strcpy( path, tmp );
	    /* fall through */

	case '/' :
	    parseline( sizeof( volname ) - 1, volname );
	    creatvol( path, volname );
	    break;

	case '.' :
	    parseline( sizeof( type ) - 1, type );
	    parseline( sizeof( creator ) - 1, creator );
	    setextmap( path, type, creator, user );
	    break;

	default :
	    break;
	}
    }
    if ( fclose( fp ) != 0 ) {
	syslog( LOG_ERR, "readvolfile: fclose: %m" );
    }
    return( 0 );
}

setextmap( ext, type, creator, user )
    char		*ext, *type, *creator;
    int			user;
{
    struct extmap	*em;

    for ( em = extmap; em; em = em->em_next ) {
	if ( strdiacasecmp( em->em_ext, ext ) == 0 ) {
	    break;
	}
    }

    if ( em == NULL ) {
	if (( em =
		(struct extmap *)malloc( sizeof( struct extmap ))) == NULL ) {
	    syslog( LOG_ERR, "setextmap: malloc: %m" );
	    exit( 1 );
	}
	em->em_next = extmap;
	extmap = em;
    } else if ( !user ) {
	return;
    }

    strcpy( em->em_ext, ext );

    if ( *type == '\0' ) {
	bcopy( "????", em->em_type, sizeof( em->em_type ));
    } else {
	bcopy( type, em->em_type, sizeof( em->em_type ));
    }
    if ( *creator == '\0' ) {
	bcopy( "UNIX", em->em_creator, sizeof( em->em_creator ));
    } else {
	bcopy( creator, em->em_creator, sizeof( em->em_creator ));
    }

    if ( strcmp( ext, "." ) == 0 ) {
	defextmap = em;
    }
}

    struct extmap *
getextmap( path )
    char	*path;
{
    char	*p;
    struct extmap	*em;

    if (( p = rindex( path, '.' )) == NULL ) {
	return( defextmap );
    }

    for ( em = extmap; em; em = em->em_next ) {
	if ( strdiacasecmp( em->em_ext, p ) == 0 ) {
	    break;
	}
    }
    if ( em == NULL ) {
	return( defextmap );
    } else {
	return( em );
    }
}

afp_openvol( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct stat	st;
    char	volname[ MAXNAMLEN ], *p;
    struct vol	*volume;
    struct dir	*dir;
    int		len, ret, buflen;
    u_short	bitmap;

    ibuf += 2;
    bcopy( ibuf, &bitmap, sizeof( u_short ));
    bitmap = ntohs( bitmap );
    ibuf += sizeof( u_short );
    if (( bitmap & (1<<VOLPBIT_VID)) == 0 ) {
	*rbuflen = 0;
	return( AFPERR_BITMAP );
    }

    len = *ibuf++;
    bcopy( ibuf, volname, len );
    volname[ len ] = '\0';

    for ( volume = volumes; volume; volume = volume->v_next ) {
	if ( strcasecmp( volname, volume->v_name ) == 0 ) {
	    break;
	}
    }
    if ( volume == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    if (( volume->v_flags & AFPVOL_OPEN  ) == 0 ) {
	if (( dir = (struct dir *)malloc( sizeof( struct dir ))) == NULL ) {
	    syslog( LOG_ERR, "afp_openvol: malloc: %m" );
	    exit( 1 );
	}
	dir->d_left = NULL;
	dir->d_right = NULL;
	dir->d_parent = NULL;
	dir->d_child = NULL;
	dir->d_next = NULL;
	dir->d_balance = 0;
	dir->d_did = htonl( 2 );
	dir->d_flags = 0;
	if (( dir->d_name = (char *)malloc( strlen( volume->v_name ) + 1 ))
		== NULL ) {
	    syslog( LOG_ERR, "afp_openvol: malloc: %m" );
	    exit( 1 );
	}
	strcpy( dir->d_name, volume->v_name );
	volume->v_dir = dir;
	volume->v_did = dir;
	volume->v_flags |= AFPVOL_OPEN;
    }

    if ( stat( volume->v_path, &st ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    buflen = *rbuflen - sizeof( u_short );
    if (( ret = getvolparams( bitmap, volume, &st,
	    rbuf + sizeof( u_short ), &buflen )) != AFP_OK ) {
	*rbuflen = 0;
	return( ret );
    }
    *rbuflen = buflen + sizeof( u_short );
    bitmap = htons( bitmap );
    bcopy( &bitmap, rbuf, sizeof( u_short ));

    /*
     * If you mount a volume twice, the second time the trash appears on
     * the desk-top.  That's because the Mac remembers the DID for the
     * trash (even for volumes in different zones, on different servers).
     * Just so this works better, we prime the DID cache with the trash,
     * fixing the trash at DID 3.
     */
    curdir = volume->v_dir;
    if ( chdir( volume->v_path ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }
    p = Trash;
    cname( volume, volume->v_dir, &p );

    return( AFP_OK );
}

afp_closevol( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol, *ovol;
    u_short	vid;

    *rbuflen = 0;
    ibuf += 2;
    bcopy( ibuf, &vid, sizeof( u_short ));
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    vol->v_flags &= ~AFPVOL_OPEN;
    for ( ovol = volumes; ovol; ovol = ovol->v_next ) {
	if ( ovol->v_flags & AFPVOL_OPEN ) {
	    break;
	}
    }
    if ( ovol != NULL ) {
	curdir = ovol->v_dir;
	if ( chdir( ovol->v_path ) < 0 ) {
	    return( AFPERR_PARAM );
	}
    }

    freedir( vol->v_did );
    vol->v_did = NULL;
    vol->v_dir = NULL;
    return( AFP_OK );
}

freedir( dir )
    struct dir	*dir;
{
    if ( dir->d_left != NULL ) {
	freedir( dir->d_left );
    }
    if ( dir->d_right != NULL ) {
	freedir( dir->d_right );
    }
    free( dir->d_name );
    free( dir );
}

afp_getvolparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct stat	st;
    struct vol	*vol;
    int		buflen, ret;
    u_short	vid, bitmap;

    ibuf += 2;
    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    bcopy( ibuf, &bitmap, sizeof( u_short ));
    bitmap = ntohs( bitmap );

    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    if ( stat( vol->v_path, &st ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    buflen = *rbuflen - sizeof( u_short );
    if (( ret = getvolparams( bitmap, vol, &st,
	    rbuf + sizeof( u_short ), &buflen )) != AFP_OK ) {
	*rbuflen = 0;
	return( ret );
    }
    *rbuflen = buflen + sizeof( u_short );
    bitmap = htons( bitmap );
    bcopy( &bitmap, rbuf, sizeof( u_short ));
    return( AFP_OK );
}

getvolspace( vol, bfree, btotal )
    struct vol	*vol;
    u_long	*bfree, *btotal;
{
    int		spaceflag, rc;
    u_long	qfree, qtotal;

    spaceflag = AFPVOL_GVSMASK & vol->v_flags;

#ifdef AFS
    if ( spaceflag == AFPVOL_NONE || spaceflag == AFPVOL_AFSGVS ) {
	if ( afs_getvolspace( vol, bfree, btotal ) == AFP_OK ) {
	    vol->v_flags = ( ~AFPVOL_GVSMASK & vol->v_flags ) | AFPVOL_AFSGVS;
	    return( AFP_OK );
	}
    }
#endif AFS

    if (( rc = ustatfs_getvolspace( vol, bfree, btotal )) != AFP_OK ) {
	return( rc );
    }

#define min(a,b)	((a)<(b)?(a):(b))
#if !defined( linux )
    if ( spaceflag == AFPVOL_NONE || spaceflag == AFPVOL_UQUOTA ) {
	if ( uquota_getvolspace( vol, &qfree, &qtotal ) == AFP_OK ) {
	    vol->v_flags = ( ~AFPVOL_GVSMASK & vol->v_flags ) | AFPVOL_UQUOTA;
	    *bfree = min( *bfree, min( qfree, 0x7fffffff ));
	    *btotal = min( *btotal, min( qtotal, 0x7fffffff ));
	    return( AFP_OK );
	}
    }
#endif linux

    *bfree = min( *bfree, 0x7fffffff );
    *btotal = min( *btotal, 0x7fffffff );
    vol->v_flags = ( ~AFPVOL_GVSMASK & vol->v_flags ) | AFPVOL_USTATFS;
    return( AFP_OK );
}

getvolparams( bitmap, vol, st, buf, buflen )
    u_short	bitmap;
    struct vol	*vol;
    struct stat	*st;
    char	*buf;
    int		*buflen;
{
    struct adouble	ad;
    int			bit = 0, aint, isad = 1;
    u_short		ashort;
    u_long		bfree, btotal;
    char		*data, *nameoff = 0;

    if ( ad_open( vol->v_path, ADFLAGS_HF|ADFLAGS_DIR,
	    O_RDONLY, 0, &ad ) < 0 ) {
	isad = 0;
    }

    if (( bitmap & ( (1<<VOLPBIT_BFREE)|(1<<VOLPBIT_BTOTAL) )) != 0 ) {
	if ( getvolspace( vol, &bfree, &btotal ) < 0 ) {
	    if ( isad ) {
		ad_close( &ad, ADFLAGS_HF );
	    }
	    return( AFPERR_PARAM );
	}
    }

    data = buf;
    while ( bitmap != 0 ) {
	while (( bitmap & 1 ) == 0 ) {
	    bitmap = bitmap>>1;
	    bit++;
	}

	switch ( bit ) {
	case VOLPBIT_ATTR :
	    ashort = 0;
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case VOLPBIT_SIG :
	    ashort = htons( AFPVOLSIG_FIX );
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case VOLPBIT_CDATE :
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

	case VOLPBIT_MDATE :
	    if ( st->st_mtime > vol->v_time ) {
		vol->v_time = st->st_mtime;
		aint = htonl( st->st_mtime );
	    } else {
		aint = htonl( vol->v_time );
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case VOLPBIT_BDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ad, ADEID_FILEI ) + FILEIOFF_BACKUP, &aint,
			sizeof( int ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case VOLPBIT_VID :
	    bcopy( &vol->v_vid, data, sizeof( vol->v_vid ));
	    data += sizeof( vol->v_vid );
	    break;

	case VOLPBIT_BFREE :
	    bfree = htonl( bfree );
	    bcopy( &bfree, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case VOLPBIT_BTOTAL :
	    btotal = htonl( btotal );
	    bcopy( &btotal, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case VOLPBIT_NAME :
	    nameoff = data;
	    data += sizeof( u_short );
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
	aint = strlen( vol->v_name );
	*data++ = aint;
	bcopy( vol->v_name, data, aint );
	data += aint;
    }
    if ( isad ) {
	ad_close( &ad, ADFLAGS_HF );
    }
    *buflen = data - buf;
    return( AFP_OK );
}

struct vol *
getvolbyvid( vid )
    short	vid;
{
    struct vol	*vol;

    for ( vol = volumes; vol; vol = vol->v_next ) {
	if ( vid == vol->v_vid ) {
	    break;
	}
    }
    if ( vol == NULL || ( vol->v_flags & AFPVOL_OPEN ) == 0 ) {
	return( NULL );
    }

    return( vol );
}

setvoltime( vol )
    struct vol	*vol;
{
    struct timeval	tv;

    if ( gettimeofday( &tv, 0 ) < 0 ) {
	syslog( LOG_ERR, "setvoltime: gettimeofday: %m" );
	exit( 1 );
    }
    vol->v_time = tv.tv_sec;
}
