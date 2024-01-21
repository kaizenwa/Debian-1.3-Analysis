/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#ifdef AFS

#include <sys/types.h>
#include <sys/syslog.h>
#include <netatalk/endian.h>
#include <netinet/in.h>
#include <afs/venus.h>
#include <afs/afsint.h>
#include <atalk/afp.h>
#include <unistd.h>

#include "directory.h"
#include "volume.h"

afs_getvolspace( vol, bfree, btotal )
    struct vol	*vol;
    u_long	*bfree, *btotal;
{
    struct ViceIoctl	vi;
    struct VolumeStatus	*vs;
    char		venuspace[ sizeof( struct VolumeStatus ) + 3 ];
    int			total, free;

    vi.in_size = 0;
    vi.out_size = sizeof( venuspace );
    vi.out = venuspace;
    if ( pioctl( vol->v_path, VIOCGETVOLSTAT, &vi, 1 ) < 0 ) {
	return( AFPERR_PARAM );
    }

    vs = (struct VolumeStatus *)venuspace;

    if ( vs->PartBlocksAvail > 0 ) {
	if ( vs->MaxQuota != 0 ) {
#define min(x,y)	(((x)<(y))?(x):(y))
	    free = min( vs->MaxQuota - vs->BlocksInUse, vs->PartBlocksAvail );
	} else {
	    free = vs->PartBlocksAvail;
	}
    } else {
	free = 0;
    }

    if ( vs->MaxQuota != 0 ) {
	total = free + vs->BlocksInUse;
    } else {
	total = vs->PartMaxBlocks;
    }

    *bfree = free * 1024;
    *btotal = total * 1024;

    return( AFP_OK );
}

afp_getdiracl( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ViceIoctl	vi;
    struct vol		*vol;
    struct dir		*dir;
    char		*path;
    int			did;
    short		vid;

    ibuf += 2;
    bcopy( ibuf, &vid, sizeof( short ));
    ibuf += sizeof( short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );
    if (( dir = dirsearch( vol, did )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }
    if ( *path != '\0' ) {
	*rbuflen = 0;
	return( AFPERR_BITMAP );
    }

    vi.in_size = 0;
    vi.out_size = *rbuflen;
    vi.out = rbuf;
    if ( pioctl( ".", VIOCGETAL, &vi, 1 ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }
    *rbuflen = strlen( vi.out ) + 1;
    return( AFP_OK );
}

/*
 * Calculate the mode for a directory in AFS.  First, make sure the
 * directory is in AFS.  Could probably use something less heavy than
 * VIOCGETAL.  If the directory is on AFS, use access() calls to
 * estimate permission, a la mdw.
 */
afsmode( path, ma, dir )
    char		*path;
    struct maccess	*ma;
    struct dir		*dir;
{
    struct ViceIoctl	vi;
    char		buf[ 1024 ];

    if (( dir->d_flags & DIRF_FSMASK ) == DIRF_NOFS ) {
	vi.in_size = 0;
	vi.out_size = sizeof( buf );
	vi.out = buf;
	if ( pioctl( path, VIOCGETAL, &vi, 1 ) < 0 ) {
	    dir->d_flags |= DIRF_UFS;
	} else {
	    dir->d_flags |= DIRF_AFS;
	}
    }

    if (( dir->d_flags & DIRF_FSMASK ) != DIRF_AFS ) {
	return;
    }

    if ( access( path, R_OK|W_OK|X_OK ) == 0 ) {
	ma->ma_user = AR_UREAD|AR_UWRITE|AR_USEARCH|AR_UOWN;
	ma->ma_owner = AR_UREAD|AR_UWRITE|AR_USEARCH;
    } else if ( access( path, R_OK|X_OK ) == 0 ) {
	ma->ma_user = AR_UREAD|AR_USEARCH;
	ma->ma_owner = AR_UREAD|AR_USEARCH;
    } else {
	ma->ma_user = ma->ma_owner = 0;
	if ( access( path, R_OK ) == 0 ) {
	    ma->ma_user |= AR_UREAD;
	    ma->ma_owner |= AR_UREAD;
	}
	if ( access( path, X_OK ) == 0 ) {
	    ma->ma_user |= AR_USEARCH;
	    ma->ma_owner |= AR_USEARCH;
	}
	if ( access( path, W_OK ) == 0 ) {
	    ma->ma_user |= AR_UWRITE|AR_UOWN;
	    ma->ma_owner |= AR_UWRITE;
	}
    }

    return;
}

extern struct dir	*curdir;
/*
 * cmd | 0 | vid | did | pathtype | pathname | 0 | acl
 */
afp_setdiracl( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ViceIoctl	vi;
    struct vol		*vol;
    struct dir		*dir;
    char		*path, *iend;
    int			did;
    short		vid;

    *rbuflen = 0;
    iend = ibuf + ibuflen;
    ibuf += 2;
    bcopy( ibuf, &vid, sizeof( short ));
    ibuf += sizeof( short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );
    if (( dir = dirsearch( vol, did )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }
    if ( *path != '\0' ) {
	*rbuflen = 0;
	return( AFPERR_BITMAP );
    }

    if ((int)ibuf & 1 ) {
	ibuf++;
    }

    vi.in_size = iend - ibuf;
    vi.in = ibuf;
    vi.out_size = 0;

    if ( pioctl( ".", VIOCSETAL, &vi, 1 ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }
    pioctl( ".AppleDouble", VIOCSETAL, &vi, 1 );
    if ( ntohl( curdir->d_did ) == 2 ) {
	pioctl( ".AppleDesktop", VIOCSETAL, &vi, 1 );
    }

    return( AFP_OK );
}


#ifdef UAM_AFSKRB

#include <krb.h>
#include <des.h>
#include <afs/kauth.h>
#include <afs/kautils.h>

extern C_Block		seskey;
extern Key_schedule	seskeysched;

afp_afschangepw( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    char	name[ MAXKTCNAMELEN ], instance[ MAXKTCNAMELEN ];
    char	realm[ MAXKTCREALMLEN ];
    char	oldpw[ 9 ], newpw[ 9 ];
    int		len, rc;
    short	clen;
    struct ktc_encryptionKey	oldkey, newkey;
    struct ktc_token		adtok;
    struct ubik_client		*conn;

    *rbuflen = 0;
    ++ibuf;
    len = *ibuf++;
    ibuf[ len ] = '\0';
    *name = *instance = *realm = '\0';
    ka_ParseLoginName( ibuf, name, instance, realm );
    ucase( realm );
    if ( *realm == '\0' ) {
	if ( krb_get_lrealm( realm, 1 ) != KSUCCESS ) {
	    syslog( LOG_ERR, "krb_get_lrealm failed" );
	    return( AFPERR_BADUAM );
	}
    }

    if ( strlen( name ) < 2 || strlen( name ) > 18 ) {
	return( AFPERR_PARAM );
    }
    ibuf += len;

    bcopy( ibuf, &clen, sizeof( short ));
    clen = ntohs( clen );
    if ( clen % 8 != 0 ) {
	return( AFPERR_PARAM );
    }

    ibuf += sizeof( short );
    pcbc_encrypt((C_Block *)ibuf, (C_Block *)ibuf,
	    clen, seskeysched, seskey, DES_DECRYPT );

    len = *ibuf++;
    if ( len > 8 ) {
	return( AFPERR_PARAM );
    }
    bzero( oldpw, sizeof( oldpw ));
    bcopy( ibuf, oldpw, len );
    ibuf += len;
    oldpw[ len ] = '\0';

    len = *ibuf++;
    if ( len > 8 ) {
	return( AFPERR_PARAM );
    }
    bzero( newpw, sizeof( newpw ));
    bcopy( ibuf, newpw, len );
    ibuf += len;
    newpw[ len ] = '\0';

    syslog( LOG_INFO,
	"changing password for <%s>.<%s>@<%s>", name, instance, realm );

    ka_StringToKey( oldpw, realm, &oldkey );
    bzero( oldpw, sizeof( oldpw ));
    ka_StringToKey( newpw, realm, &newkey );
    bzero( newpw, sizeof( newpw ));

    rc = ka_GetAdminToken( name, instance, realm, &oldkey, 60, &adtok, 0 );
    bzero( &oldkey, sizeof( oldkey ));
    switch ( rc ) {
	case 0:
	    break;
	case KABADREQUEST:
	    bzero( &newkey, sizeof( newkey ));
	    return( AFPERR_NOTAUTH );
	default:
	    bzero( &newkey, sizeof( newkey ));
	    return( AFPERR_BADUAM );
    }
    if ( ka_AuthServerConn( realm, KA_MAINTENANCE_SERVICE, &adtok, &conn )
		!= 0 ) {
	bzero( &newkey, sizeof( newkey ));
	return( AFPERR_BADUAM );
    }

    rc = ka_ChangePassword( name, instance, conn, 0, &newkey );
    bzero( &newkey, sizeof( newkey ));
    if ( rc != 0 ) {
	return( AFPERR_BADUAM );
    }

    syslog( LOG_DEBUG, "password changed succeeded" );
    return( AFP_OK );
}
#endif UAM_AFSKRB
#endif AFS
