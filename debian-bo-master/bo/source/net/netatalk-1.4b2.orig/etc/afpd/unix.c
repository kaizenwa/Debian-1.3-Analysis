/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/syslog.h>
#include <sys/time.h>
#include <netatalk/endian.h>
#include <dirent.h>
#include <limits.h>
#include <atalk/afp.h>
#include <strings.h>
#include <stdio.h>
#include <fcntl.h>
#include "auth.h"
#include "directory.h"
#include "volume.h"

#ifdef ultrix
#include <sys/mount.h>
#include <sys/quota.h>
#endif ultrix

#ifdef _IBMR2
#include <sys/statfs.h>
#endif _IBMR2

#if defined( sun ) || defined( ibm032 ) || defined( linux )
#include <sys/vfs.h>
#endif sun ibm032 linux

#if defined( __svr4__ )
#include <sys/fs/ufs_quota.h>
#include <sys/statvfs.h>
#include <sys/mnttab.h>
#endif __svr4__

#ifdef __FreeBSD__
#include <ufs/ufs/quota.h>
#include <sys/mount.h>
#define dqb_btimelimit	dqb_btime
#endif __FreeBSD__

#if !defined( linux ) && !defined( ultrix ) && !defined( __svr4__ ) && \
	!defined( __FreeBSD__ )
#include <ufs/quota.h>
#include <mntent.h>
#endif linux ultrix __svr4__

#ifdef ibm032
typedef unsigned short	mode_t;
#endif ibm032

#if defined( sun ) && !defined( __svr4__ )
#ifdef i386
typedef int	mode_t;
#endif i386
#endif sun __svr4__

#ifdef __svr4__
#define statfs statvfs
#else __svr4__
#define	f_frsize f_bsize
#endif __svr4__

/*
 * Get the free space on a partition.
 */
ustatfs_getvolspace( vol, bfree, btotal )
    struct vol	*vol;
    u_long	*bfree, *btotal;
{
#ifdef ultrix
    struct fs_data	sfs;
#else ultrix
    struct statfs	sfs;
#endif ultrix

    if ( statfs( vol->v_path, &sfs ) < 0 ) {
	return( AFPERR_PARAM );
    }

#ifdef ultrix
    *bfree = sfs.fd_req.bfreen * 1024;
#else
    *bfree = sfs.f_bavail * sfs.f_frsize;
#endif ultrix

#ifdef ultrix
    *btotal = ( sfs.fd_req.btot - ( sfs.fd_req.bfree - sfs.fd_req.bfreen )) *
	    1024;
#else ultrix
    *btotal = ( sfs.f_blocks - ( sfs.f_bfree - sfs.f_bavail )) * sfs.f_frsize;
#endif ultrix

    return( AFP_OK );
}

#ifndef linux

#ifdef __svr4__
/*
 * Return the mount point associated with the filesystem
 * on which "file" resides.  Returns NULL on failure.
 */
    char *
mountp( file )
    char	*file;
{
    struct stat			sb;
    FILE 			*mtab;
    dev_t			devno;
    static struct mnttab	mnt;

    if ( stat( file, &sb ) < 0 ) {
	return( NULL );
    }
    devno = sb.st_dev;

    if (( mtab = fopen( "/etc/mnttab", "r" )) == NULL ) {
	return( NULL );
    }

    while ( getmntent( mtab, &mnt ) == 0 ) {
	if ( stat( mnt.mnt_special, &sb ) == 0 && devno == sb.st_rdev ) {
	    fclose( mtab );
	    return( mnt.mnt_mountp );
	}
    }

    fclose( mtab );
    return( NULL );
}

#else __svr4__
#ifdef ultrix
/*
 * Return the block-special device name associated with the filesystem
 * on which "file" resides.  Returns NULL on failure.
 */

    char *
special( file )
    char *file;
{
    static struct fs_data	fsd;

    if ( getmnt(0, &fsd, 0, STAT_ONE, file ) < 0 ) {
	syslog( "special: getmnt %s: %m", file );
	return( NULL );
    }

    return( fsd.fd_req.devname );
}

#else ultrix
#ifdef __FreeBSD__

    char *
special( file )
    char	*file;
{
    static struct statfs	sfs;

    if ( statfs( file, &sfs ) < 0 ) {
	return( NULL );
    }
    return( sfs.f_mntfromname );
}

#else __FreeBSD__

    char *
special( file )
    char *file;
{
    struct stat		sb;
    FILE 		*mtab;
    dev_t		devno;
    struct mntent	*mnt;

    if ( stat( file, &sb ) < 0 ) {
	return( NULL );
    }
    devno = sb.st_dev;

    if (( mtab = setmntent( "/etc/mtab", "r" )) == NULL ) {
	return( NULL );
    }

    while (( mnt = getmntent( mtab )) != NULL ) {
	if ( stat( mnt->mnt_fsname, &sb ) == 0 && devno == sb.st_rdev ) {
	    endmntent( mtab );
	    return( mnt->mnt_fsname );
	}
    }

    endmntent( mtab );
    return( NULL );
}

#endif __FreeBSD__
#endif ultrix
#endif __svr4__

getquota( vol, dq )
    struct vol		*vol;
    struct dqblk	*dq;
{
    char		*p;
#ifdef __svr4__
    char		buf[ MAXNAMLEN ];
    struct quotctl	qc;

    if ( vol->v_qfd == -1 ) {
	if (( p = mountp( vol->v_path )) == NULL ) {
	    syslog( LOG_ERR, "getquota: mountp %s fails", vol->v_path );
	    return( AFPERR_PARAM );
	}

	sprintf( buf, "%s/quotas", p );
	if (( vol->v_qfd = open( buf, O_RDONLY, 0 )) < 0 ) {
	    syslog( LOG_INFO, "open %s: %m", buf );
	    return( AFPERR_PARAM );
	}
    }

    qc.op = Q_GETQUOTA;
    qc.uid = uuid;
    qc.addr = (caddr_t)dq;
    if ( ioctl( vol->v_qfd, Q_QUOTACTL, &qc ) < 0 ) {
	return( AFPERR_PARAM );
    }

#else __svr4__
    if ( vol->v_gvs == NULL ) {
	if (( p = special( vol->v_path )) == NULL ) {
	    syslog( LOG_ERR, "getquota: special %s fails", vol->v_path );
	    return( AFPERR_PARAM );
	}

	if (( vol->v_gvs = (char *)malloc( strlen( p ) + 1 )) == NULL ) {
	    syslog( LOG_ERR, "getquota: malloc: %m" );
	    exit( 1 );
	}
	strcpy( vol->v_gvs, p );
    }

#ifdef ultrix
    if ( quota( Q_GETDLIM, uuid, vol->v_gvs, dq ) != 0 ) {
	return( AFPERR_PARAM );
    }
#else ultrix
#ifdef __FreeBSD__
    if ( quotactl( vol->v_gvs, Q_GETQUOTA, uuid, (char *)dq ) != 0 ) {
	return( AFPERR_PARAM );
    }
#else __FreeBSD__
    if ( quotactl( Q_GETQUOTA, vol->v_gvs, uuid, dq ) != 0 ) {
	return( AFPERR_PARAM );
    }
#endif __FreeBSD__
#endif ultrix
#endif __svr4__

    return( 0 );
}

uquota_getvolspace( vol, bfree, btotal )
    struct vol	*vol;
    u_long	*bfree, *btotal;
{
    struct dqblk	dqblk;

    if ( getquota( vol, &dqblk ) != 0 ) {
	return( AFPERR_PARAM );
    }

    if ( overquota( &dqblk )) {
	*btotal = dbtob( dqblk.dqb_bhardlimit );
	*bfree = dbtob( dqblk.dqb_bhardlimit ) - dbtob( dqblk.dqb_curblocks );
    } else {
	*btotal = dbtob( dqblk.dqb_bsoftlimit );
	*bfree = dbtob( dqblk.dqb_bsoftlimit ) - dbtob( dqblk.dqb_curblocks );
    }

    return( AFP_OK );
}

overquota( dqblk )
    struct dqblk	*dqblk;
{
    struct timeval	tv;

    if ( dqblk->dqb_curblocks < dqblk->dqb_bsoftlimit ) {
	return( 0 );
    }
#ifdef ultrix
    if ( dqblk->dqb_bwarn ) {
	return( 0 );
    }
#else ultrix
    if ( gettimeofday( &tv, 0 ) < 0 ) {
	syslog( LOG_ERR, "overquota: gettimeofday: %m" );
	return( AFPERR_PARAM );
    }
    if ( !dqblk->dqb_btimelimit || dqblk->dqb_btimelimit > tv.tv_sec ) {
	return( 0 );
    }
#endif ultrix
    return( 1 );
}

#endif linux

utommode( stat, ma )
    struct stat		*stat;
    struct maccess	*ma;
{
    mode_t		mode;

    mode = stat->st_mode;

    ma->ma_world = utombits( mode );
    mode = mode >> 3;

    ma->ma_group = utombits( mode );
    mode = mode >> 3;

    ma->ma_owner = utombits( mode );

    if ( uuid == stat->st_uid ) {
	ma->ma_user = ma->ma_owner | AR_UOWN;
    } else if ( gmem( stat->st_gid )) {
	ma->ma_user = ma->ma_group;
    } else {
	ma->ma_user = ma->ma_world;
    }

    /*
     * There are certain things the mac won't try if you don't have
     * the "owner" bit set, even tho you can do these things on unix wiht
     * only write permission.  What were the things?
     */
    if ( ma->ma_user & AR_UWRITE ) {
	ma->ma_user |= AR_UOWN;
    }
}

utombits( bits )
    mode_t	bits;
{
    int		mbits;

    mbits = 0;

    mbits |= ( bits & ( S_IREAD >> 6 )) ? AR_UREAD : 0;
    mbits |= ( bits & ( S_IWRITE >> 6 )) ? AR_UWRITE : 0;
    mbits |= ( bits & ( S_IEXEC >> 6 )) ? AR_USEARCH : 0;

    return( mbits );
}

gmem( gid )
    int	gid;
{
    int		i;

    for ( i = 0; i < ngroups; i++ ) {
	if ( groups[ i ] == gid ) {
	    return( 1 );
	}
    }
    return( 0 );
}

mtoumode( ma )
    struct maccess	*ma;
{
    mode_t		mode;

    mode = 0;
    mode |= mtoubits( ma->ma_owner );
    mode = mode << 3;

    mode |= mtoubits( ma->ma_group );
    mode = mode << 3;

    mode |= mtoubits( ma->ma_world );

    return( mode );
}

mtoubits( bits )
    u_char	bits;
{
    mode_t	mode;

    mode = 0;

    mode |= ( bits & AR_UREAD ) ? ( S_IREAD >> 6 ) : 0;
    mode |= ( bits & AR_UWRITE ) ? ( S_IWRITE >> 6 ) : 0;
    mode |= ( bits & AR_USEARCH ) ? ( S_IEXEC >> 6 ) : 0;

    return( mode );
}

setdeskmode( mode )
    mode_t	mode;
{
    static char		wd[ MAXPATHLEN ];
    char		c, modbuf[ 12 ], *m;
    struct dirent	*deskp, *subp;
    DIR			*desk, *sub;

    if ( getwd( wd ) == NULL ) {
	return( -1 );
    }
    if ( chdir( ".AppleDesktop" ) < 0 ) {
	return( -1 );
    }
    if (( desk = opendir( "." )) == NULL ) {
	if ( chdir( wd ) < 0 ) {
	    syslog( LOG_ERR, "setdeskmode: chdir %s: %m", wd );
	    exit( 1 );
	}
	return( -1 );
    }
    for ( deskp = readdir( desk ); deskp != NULL; deskp = readdir( desk )) {
	if ( strcmp( deskp->d_name, "." ) == 0 ||
		strcmp( deskp->d_name, ".." ) == 0 || strlen( deskp->d_name ) > 2 ) {
	    continue;
	}
	strcpy( modbuf, deskp->d_name );
	strcat( modbuf, "/" );
	m = index( modbuf, '\0' );
	if (( sub = opendir( deskp->d_name )) == NULL ) {
	    continue;
	}
	for ( subp = readdir( sub ); subp != NULL; subp = readdir( sub )) {
	    if ( strcmp( subp->d_name, "." ) == 0 ||
		    strcmp( subp->d_name, ".." ) == 0 ) {
		continue;
	    }
	    *m = '\0';
	    strcat( modbuf, subp->d_name );
	    if ( chmod( modbuf, mode ) < 0 ) {
		syslog( LOG_DEBUG, "setdeskmode: chmod %s: %m", modbuf );
	    }
	}
	closedir( sub );
	if ( chmod( deskp->d_name, mode ) < 0 ) {
	    syslog( LOG_DEBUG, "setdeskmode: chmod %s: %m", deskp->d_name );
	}
    }
    closedir( desk );
    if ( chdir( wd ) < 0 ) {
	syslog( LOG_ERR, "setdeskmode: chdir %s: %m", wd );
	exit( 1 );
    }
    if ( chmod( ".AppleDesktop", mode ) < 0 ) {
	syslog( LOG_DEBUG, "setdeskmode: chmod .AppleDesktop: %m" );
    }
    return( 0 );
}

setdirmode( mode )
    mode_t	mode;
{
    static char		buf[ MAXPATHLEN ];
    struct stat		st;
    char		*m;
    struct dirent	*dirp;
    DIR			*dir;

    if (( dir = opendir( "." )) == NULL ) {
	syslog( LOG_ERR, "setdirmode: opendir .: %m" );
	return( -1 );
    }
    for ( dirp = readdir( dir ); dirp != NULL; dirp = readdir( dir )) {
	if ( *dirp->d_name == '.' ) {
	    continue;
	}
	if ( stat( dirp->d_name, &st ) < 0 ) {
	    syslog( LOG_DEBUG, "setdirmode: stat %s: %m", dirp->d_name );
	    continue;
	}
	if (( st.st_mode & S_IFMT ) == S_IFREG ) {
	    if ( chmod( dirp->d_name, mode ) < 0 ) {
		syslog( LOG_DEBUG, "setdirmode: chmod %s: %m", dirp->d_name );
	    }
	}
    }
    closedir( dir );
    if (( dir = opendir( ".AppleDouble" )) == NULL ) {
	syslog( LOG_ERR, "setdirmode: opendir .AppleDouble: %m" );
	return( -1 );
    }
    strcpy( buf, ".AppleDouble" );
    strcat( buf, "/" );
    m = index( buf, '\0' );
    for ( dirp = readdir( dir ); dirp != NULL; dirp = readdir( dir )) {
	if ( strcmp( dirp->d_name, "." ) == 0 ||
		strcmp( dirp->d_name, ".." ) == 0 ) {
	    continue;
	}
	*m = '\0';
	strcat( buf, dirp->d_name );
	if ( chmod( buf, mode ) < 0 ) {
	    syslog( LOG_DEBUG, "setdirmode: chmod %s: %m", buf );
	}
    }
    closedir( dir );
    if ( chmod( ".AppleDouble", mode ) < 0 ) {
	syslog( LOG_ERR, "setdirmode: chmod .AppleDouble: %m" );
	return( -1 );
    }
    if ( chmod( ".", mode ) < 0 ) {
	syslog( LOG_ERR, "setdirmode: chmod .: %m" );
	return( -1 );
    }
    return( 0 );
}

setdeskowner( uid, gid )
    int		uid;
    int		gid;
{
    static char		wd[ MAXPATHLEN ];
    char		c, modbuf[ 12 ], *m;
    struct dirent	*deskp, *subp;
    DIR			*desk, *sub;

    if ( getwd( wd ) == NULL ) {
	return( -1 );
    }
    if ( chdir( ".AppleDesktop" ) < 0 ) {
	return( -1 );
    }
    if (( desk = opendir( "." )) == NULL ) {
	if ( chdir( wd ) < 0 ) {
	    syslog( LOG_ERR, "setdeskowner: chdir %s: %m", wd );
	    exit( 1 );
	}
	return( -1 );
    }
    for ( deskp = readdir( desk ); deskp != NULL; deskp = readdir( desk )) {
	if ( strcmp( deskp->d_name, "." ) == 0 ||
		strcmp( deskp->d_name, ".." ) == 0 || strlen( deskp->d_name ) > 2 ) {
	    continue;
	}
	strcpy( modbuf, deskp->d_name );
	strcat( modbuf, "/" );
	m = index( modbuf, '\0' );
	if (( sub = opendir( deskp->d_name )) == NULL ) {
	    continue;
	}
	for ( subp = readdir( sub ); subp != NULL; subp = readdir( sub )) {
	    if ( strcmp( subp->d_name, "." ) == 0 ||
		    strcmp( subp->d_name, ".." ) == 0 ) {
		continue;
	    }
	    *m = '\0';
	    strcat( modbuf, subp->d_name );
	    if ( chown( modbuf, uid, gid ) < 0 ) {
		syslog( LOG_DEBUG, "setdeskown: chown %s: %m", modbuf );
	    }
	}
	closedir( sub );
	if ( chown( deskp->d_name, uid, gid ) < 0 ) {
	    syslog( LOG_DEBUG, "setdeskowner: chown %s: %m", deskp->d_name );
	}
    }
    closedir( desk );
    if ( chdir( wd ) < 0 ) {
	syslog( LOG_ERR, "setdeskowner: chdir %s: %m", wd );
	exit( 1 );
    }
    if ( chown( ".AppleDesktop", uid, gid ) < 0 ) {
	syslog( LOG_ERR, "setdeskowner: chown .AppleDesktop: %m" );
    }
    return( 0 );
}

setdirowner( uid, gid )
    int		uid;
    int		gid;
{
    static char		buf[ MAXPATHLEN ];
    struct stat		st;
    char		*m;
    struct dirent	*dirp;
    DIR			*dir;

    if (( dir = opendir( "." )) == NULL ) {
	return( -1 );
    }
    for ( dirp = readdir( dir ); dirp != NULL; dirp = readdir( dir )) {
	if ( *dirp->d_name == '.' ) {
	    continue;
	};
	if ( stat( dirp->d_name, &st ) < 0 ) {
	    syslog( LOG_DEBUG, "setdirowner: stat %s: %m", dirp->d_name );
	    continue;
	}
	if (( st.st_mode & S_IFMT ) == S_IFREG ) {
	    if ( chown( dirp->d_name, uid, gid ) < 0 ) {
		syslog( LOG_DEBUG, "setdirowner: chown %s: %m", dirp->d_name );
	    }
	}
    }
    closedir( dir );
    if (( dir = opendir( ".AppleDouble" )) == NULL ) {
	return( -1 );
    }
    strcpy( buf, ".AppleDouble" );
    strcat( buf, "/" );
    m = index( buf, '\0' );
    for ( dirp = readdir( dir ); dirp != NULL; dirp = readdir( dir )) {
	if ( strcmp( dirp->d_name, "." ) == 0 ||
		strcmp( dirp->d_name, ".." ) == 0 ) {
	    continue;
	}
	*m = '\0';
	strcat( buf, dirp->d_name );
	if ( chown( buf, uid, gid ) < 0 ) {
	    syslog( LOG_DEBUG, "setdirowner: chown %d/%d %s: %m",
		    uid, gid, buf );
	}
    }
    closedir( dir );

    /*
     * We cheat: we know that chown doesn't do anything.
     */
    if ( stat( ".AppleDouble", &st ) < 0 ) {
	syslog( LOG_ERR, "setdirowner: stat .AppleDouble: %m" );
	return( -1 );
    }
    if ( gid && gid != st.st_gid && chown( ".AppleDouble", uid, gid ) < 0 ) {
	return( -1 );
    }

    if ( stat( ".", &st ) < 0 ) {
	return( -1 );
    }
    if ( gid && gid != st.st_gid && chown( ".", uid, gid ) < 0 ) {
	return( -1 );
    }

    return( 0 );
}
