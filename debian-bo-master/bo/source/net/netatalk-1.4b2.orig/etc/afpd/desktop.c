/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/syslog.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/param.h>
#include <netatalk/at.h>
#include <netatalk/endian.h>
#include <atalk/atp.h>
#include <atalk/asp.h>
#include <atalk/afp.h>
#include <atalk/adouble.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <unistd.h>

#include "volume.h"
#include "directory.h"
#include "globals.h"
#include "desktop.h"

extern int	errno;

afp_opendt( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol;
    u_short	vid;

    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    bcopy( &vid, rbuf, sizeof( u_short ));
    *rbuflen = sizeof( u_short );
    return( AFP_OK );
}

afp_closedt( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    *rbuflen = 0;
    return( AFP_OK );
}

struct savedt	si = { { 0, 0, 0, 0 }, -1, 0 };

afp_addicon( ibuf, ibuflen, rbuf, rbuflen, asp )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
    ASP		asp;
{
    struct vol		*vol;
    struct iovec	iov[ 2 ];
    u_char		fcreator[ 4 ], imh[ 12 ], irh[ 12 ], *p;
    int			ftype, itype, itag, cc, iovcnt = 0, buflen;
    u_short		bsize, rsize, vid;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, fcreator, sizeof( fcreator ));
    ibuf += sizeof( int );

    bcopy( ibuf, &ftype, sizeof( int ));
    ibuf += sizeof( int );

    itype = *ibuf;
    ibuf += 2;

    bcopy( ibuf, &itag, sizeof( int ));
    ibuf += sizeof( int );

    bcopy( ibuf, &bsize, sizeof( u_short ));
    bsize = ntohs( bsize );

    if ( si.sdt_fd != -1 ) {
	(void)close( si.sdt_fd );
	si.sdt_fd = -1;
    }
    if ( iconopen( vol, fcreator, O_RDWR|O_CREAT, 0666 ) != AFP_OK ) {
	return( AFPERR_NOITEM );
    }

    if ( lseek( si.sdt_fd, 0L, SEEK_SET ) < 0 ) {
	si.sdt_fd = -1;
	return( AFPERR_PARAM );
    }

    /*
     * Read icon elements until we find a match to replace, or
     * we get to the end to insert.
     */
    p = imh;
    bcopy( &itag, p, sizeof( int ));
    p += sizeof( int );
    bcopy( &ftype, p, sizeof( int ));
    p += sizeof( int );
    *p++ = itype;
    *p++ = 0;
    bsize = htons( bsize );
    bcopy( &bsize, p, sizeof( u_short ));
    bsize = ntohs( bsize );
    while (( cc = read( si.sdt_fd, irh, sizeof( irh ))) > 0 ) {
	bcopy( irh + 10, &rsize, sizeof( u_short ));
	rsize = ntohs( rsize );
	/*
	 * Is this our set of headers?
	 */
	if ( bcmp( irh, imh, sizeof( irh ) - sizeof( u_short )) == 0 ) {
	    /*
	     * Is the size correct?
	     */
	    if ( bsize == rsize ) {
		break;
	    } else {
		return( AFPERR_ITYPE );
	    }
	}
	if ( lseek( si.sdt_fd, (long)rsize, SEEK_CUR ) < 0 ) {
	    syslog( LOG_ERR, "afp_addicon: lseek: %m" );
	    return( AFPERR_PARAM );
	}
    }

    /*
     * Some error occured, return.
     */
    if ( cc < 0 ) {
	syslog( LOG_ERR, "afp_addicon: read: %m" );
	return( AFPERR_PARAM );
    }

    buflen = bsize;
    if ( asp_wrtcont( asp, rbuf, &buflen ) < 0 || buflen != bsize ) {
	return( AFPERR_PARAM );
    }

    /*
     * We're at the end of the file, add the headers, etc.
     */
    if ( cc == 0 ) {
	iov[ 0 ].iov_base = (caddr_t)imh;
	iov[ 0 ].iov_len = sizeof( imh );
	iov[ 1 ].iov_base = rbuf;
	iov[ 1 ].iov_len = bsize;
	iovcnt = 2;
    }

    /*
     * We found an icon to replace.
     */
    if ( cc > 0 ) {
	iov[ 0 ].iov_base = rbuf;
	iov[ 0 ].iov_len = bsize;
	iovcnt = 1;
    }

    if ( writev( si.sdt_fd, iov, iovcnt ) < 0 ) {
	syslog( LOG_ERR, "afp_addicon: writev: %m" );
	return( AFPERR_PARAM );
    }

    close( si.sdt_fd );
    si.sdt_fd = -1;
    return( AFP_OK );
}

u_char	utag[] = { 0, 0, 0, 0 };
u_char	ucreator[] = { 'U', 'N', 'I', 'X' };
u_char	utype[] = { 'T', 'E', 'X', 'T' };
short	usize = 256;
u_char	uicon[] = {
0x1F, 0xFF, 0xFC, 0x00, 0x10, 0x00, 0x06, 0x00,
0x10, 0x00, 0x05, 0x00, 0x10, 0x00, 0x04, 0x80,
0x10, 0x00, 0x04, 0x40, 0x10, 0x00, 0x04, 0x20,
0x10, 0x00, 0x07, 0xF0, 0x10, 0x00, 0x00, 0x10,
0x10, 0x00, 0x00, 0x10, 0x10, 0x00, 0x00, 0x10,
0x10, 0x00, 0x00, 0x10, 0x10, 0x80, 0x02, 0x10,
0x11, 0x80, 0x03, 0x10, 0x12, 0x80, 0x02, 0x90,
0x12, 0x80, 0x02, 0x90, 0x14, 0x80, 0x02, 0x50,
0x14, 0x87, 0xC2, 0x50, 0x14, 0x58, 0x34, 0x50,
0x14, 0x20, 0x08, 0x50, 0x12, 0x16, 0xD0, 0x90,
0x11, 0x01, 0x01, 0x10, 0x12, 0x80, 0x02, 0x90,
0x12, 0x9C, 0x72, 0x90, 0x14, 0x22, 0x88, 0x50,
0x14, 0x41, 0x04, 0x50, 0x14, 0x49, 0x24, 0x50,
0x14, 0x55, 0x54, 0x50, 0x14, 0x5D, 0x74, 0x50,
0x14, 0x5D, 0x74, 0x50, 0x12, 0x49, 0x24, 0x90,
0x12, 0x22, 0x88, 0x90, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFC, 0x00, 0x1F, 0xFF, 0xFE, 0x00,
0x1F, 0xFF, 0xFF, 0x00, 0x1F, 0xFF, 0xFF, 0x80,
0x1F, 0xFF, 0xFF, 0xC0, 0x1F, 0xFF, 0xFF, 0xE0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
0x1F, 0xFF, 0xFF, 0xF0, 0x1F, 0xFF, 0xFF, 0xF0,
};

afp_geticoninfo( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol;
    u_char	fcreator[ 4 ], ih[ 12 ];
    u_short	vid, iindex, bsize;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, fcreator, sizeof( fcreator ));
    ibuf += sizeof( fcreator );
    bcopy( ibuf, &iindex, sizeof( u_short ));
    iindex = ntohs( iindex );

    if ( bcmp( fcreator, ucreator, sizeof( ucreator )) == 0 ) {
	if ( iindex > 1 ) {
	    return( AFPERR_NOITEM );
	}
	bcopy( utag, ih, sizeof( utag ));
	bcopy( utype, ih + sizeof( utag ), sizeof( utype ));
	*( ih + sizeof( utag ) + sizeof( utype )) = 1;
	*( ih + sizeof( utag ) + sizeof( utype ) + 1 ) = 0;
	bcopy( &usize, ih + sizeof( utag ) + sizeof( utype ) + 2,
		sizeof( usize ));
	bcopy( ih, rbuf, sizeof( ih ));
	*rbuflen = sizeof( ih );
	return( AFP_OK );
    }

    if ( iconopen( vol, fcreator, O_RDONLY, 0 ) != AFP_OK ) {
	return( AFPERR_NOITEM );
    }

    if ( iindex < si.sdt_index ) {
	if ( lseek( si.sdt_fd, 0L, SEEK_SET ) < 0 ) {
	    return( AFPERR_PARAM );
	}
	si.sdt_index = 1;
    }

    /*
     * Position to the correct spot.
     */
    for (;;) {
	if ( read( si.sdt_fd, ih, sizeof( ih )) != sizeof( ih )) {
	    close( si.sdt_fd );
	    si.sdt_fd = -1;
	    return( AFPERR_NOITEM );
	}
	bcopy( ih + 10, &bsize, sizeof( u_short ));
	if ( lseek( si.sdt_fd, (long)bsize, SEEK_CUR ) < 0 ) {
	    syslog( LOG_ERR, "afp_iconinfo: lseek: %m" );
	    return( AFPERR_PARAM );
	}
	if ( si.sdt_index == iindex ) {
	    bcopy( ih, rbuf, sizeof( ih ));
	    *rbuflen = sizeof( ih );
	    return( AFP_OK );
	}
	si.sdt_index++;
    }
}

afp_geticon( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol	*vol;
    int		rc;
    u_char	fcreator[ 4 ], ftype[ 4 ], itype, ih[ 12 ];
    u_short	vid, bsize, rsize;

    *rbuflen = 0;
    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, fcreator, sizeof( fcreator ));
    ibuf += sizeof( fcreator );
    bcopy( ibuf, ftype, sizeof( ftype ));
    ibuf += sizeof( ftype );
    itype = *ibuf++;
    ibuf++;
    bcopy( ibuf, &bsize, sizeof( u_short ));
    bsize = ntohs( bsize );

    if ( bcmp( fcreator, ucreator, sizeof( ucreator )) == 0 &&
	    bcmp( ftype, utype, sizeof( utype )) == 0 &&
	    itype == 1 &&
	    bsize == usize ) {
	bcopy( uicon, rbuf, sizeof( uicon ));
	*rbuflen = sizeof( uicon );
	return( AFP_OK );
    }

    if ( iconopen( vol, fcreator, O_RDONLY, 0 ) != AFP_OK ) {
	return( AFPERR_NOITEM );
    }

    if ( lseek( si.sdt_fd, 0L, SEEK_SET ) < 0 ) {
	si.sdt_fd = -1;
	return( AFPERR_PARAM );
    }

    si.sdt_index = 1;

    while (( rc = read( si.sdt_fd, ih, sizeof( ih ))) > 0 ) {
	si.sdt_index++;
	if ( bcmp( ih + sizeof( int ), ftype, sizeof( ftype )) == 0 &&
		*(ih + sizeof( int ) + sizeof( ftype )) == itype ) {
	    break;
	}
	bcopy( ih + 10, &rsize, sizeof( u_short ));
	rsize = ntohs( rsize );
	if ( lseek( si.sdt_fd, (long)rsize, SEEK_CUR ) < 0 ) {
	    syslog( LOG_ERR, "afp_geticon: lseek: %m" );
	    return( AFPERR_PARAM );
	}
    }

    if ( rc < 0 ) {
	return( AFPERR_PARAM );
    }

    if ( rc == 0 ) {
	return( AFPERR_NOITEM );
    }

    bcopy( ih + 10, &rsize, sizeof( u_short ));
    rsize = ntohs( rsize );
#define min(a,b)	((a)<(b)?(a):(b))
    rc = min( bsize, rsize );
    if ( read( si.sdt_fd, rbuf, rc ) < rc ) {
	return( AFPERR_PARAM );
    }
    *rbuflen = rc;
    return( AFP_OK );
}

static char		hexdig[] = "0123456789abcdef";
char *dtfile( vol, creator, ext )
    struct vol	*vol;
    u_char	creator[ 4 ];
    char	*ext;
{
    static char	path[ MAXPATHLEN ];
    char	*p;
    int		i;

    strcpy( path, vol->v_path );
    strcat( path, "/.AppleDesktop/" );
    for ( p = path; *p != '\0'; p++ )
	;

    if ( !isascii( creator[ 0 ] ) || creator[ 0 ] == '/' ) {
	*p++ = hexdig[ ( creator[ 0 ] & 0xf0 ) >> 4 ];
	*p++ = hexdig[ creator[ 0 ] & 0x0f ];
    } else {
	*p++ = creator[ 0 ];
    }

    *p++ = '/';

    for ( i = 0; i < sizeof( creator ); i++ ) {
	if ( !isascii( creator[ i ] ) || creator[ i ] == '/' ) {
	    *p++ = hexdig[ ( creator[ i ] & 0xf0 ) >> 4 ];
	    *p++ = hexdig[ creator[ i ] & 0x0f ];
	} else {
	    *p++ = creator[ i ];
	}
    }
    *p = '\0';
    strcat( path, ext );

    return( path );
}

char *
mtoupath( mpath )
    char	*mpath;
{
    static char	upath[ MAXNAMLEN ];
    char	*m, *u;
    int		i = 0;

    if ( *mpath == '\0' ) {
	return( "." );
    }

    m = mpath;
    u = upath;
    while ( *m != '\0' ) {
	if ( !isascii( *m ) || *m == '/' || ( i == 0 && *m == '.' )) {
	    *u++ = ':';
	    *u++ = hexdig[ ( *m & 0xf0 ) >> 4 ];
	    *u++ = hexdig[ *m & 0x0f ];
	} else {
#ifdef DOWNCASE
	    *u++ = ( isupper( *m )) ? tolower( *m ) : *m;
#else DOWNCASE
	    *u++ = *m;
#endif DOWNCASE
	}
	i++;
	m++;
    }
    *u = '\0';
    return( upath );
}

#define hextoint( c )	( isdigit( c ) ? c - '0' : c + 10 - 'a' )
#define islxdigit(x)	(!isupper(x)&&isxdigit(x))

char *
utompath( upath )
    char	*upath;
{
    static char	mpath[ MAXNAMLEN ];
    char	*m, *u;
    int		h;

    u = upath;
    m = mpath;
    while ( *u != '\0' ) {
	if ( *u == ':' && *(u+1) != '\0' && islxdigit( *(u+1)) &&
		*(u+2) != '\0' && islxdigit( *(u+2))) {
	    ++u;
	    h = hextoint( *u ) << 4;
	    ++u;
	    h |= hextoint( *u );
	    *m++ = h;
	} else {
	    *m++ = *u;
	}
	u++;
    }
    *m = '\0';
    return( mpath );
}

iconopen( vol, creator, flags, mode )
    struct vol	*vol;
    u_char	creator[ 4 ];
{
    char	*dtf, *adt, *adts;

    if ( si.sdt_fd != -1 ) {
	if ( bcmp( si.sdt_creator, creator, sizeof( creator )) == 0 &&
		si.sdt_vid == vol->v_vid ) {
	    return( AFP_OK );
	}
	close( si.sdt_fd );
	si.sdt_fd = -1;
    }

    dtf = dtfile( vol, creator, ".icon" );

    if (( si.sdt_fd = open( dtf, flags, ad_mode( dtf, mode ))) < 0 ) {
	if ( errno == ENOENT && ( flags & O_CREAT )) {
	    if (( adts = rindex( dtf, '/' )) == NULL ) {
		return( AFPERR_PARAM );
	    }
	    *adts = '\0';
	    if (( adt = rindex( dtf, '/' )) == NULL ) {
		return( AFPERR_PARAM );
	    }
	    *adt = '\0';
	    (void) ad_mkdir( dtf, 0777 );
	    *adt = '/';
	    (void) ad_mkdir( dtf, 0777 );
	    *adts = '/';

	    if (( si.sdt_fd = open( dtf, flags, ad_mode( dtf, mode ))) < 0 ) {
		syslog( LOG_ERR, "iconopen: open %s: %m", dtf );
		return( AFPERR_PARAM );
	    }
	} else {
	    return( AFPERR_PARAM );
	}
    }
    bcopy( creator, si.sdt_creator, sizeof( creator ));
    si.sdt_vid = vol->v_vid;
    si.sdt_index = 1;
    return( AFP_OK );
}

afp_addcomment( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct adouble	ad;
    struct vol		*vol;
    struct dir		*dir;
    char		*path, *name;
    int			did, clen;
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

    if ((int)ibuf & 1 ) {
	ibuf++;
    }

    clen = (u_char)*ibuf++;
    clen = min( clen, 199 );

    if ( ad_open( mtoupath( path ),
	    ( *path == '\0' ) ? ADFLAGS_HF|ADFLAGS_DIR : ADFLAGS_HF,
	    O_RDWR|O_CREAT, 0666, &ad ) < 0 ) {
	return( AFPERR_ACCESS );
    }

    if ( ad_getoflags( &ad, ADFLAGS_HF ) & O_CREAT ) {
	if ( *path == '\0' ) {
	    name = curdir->d_name;
	} else {
	    name = path;
	}
	ad_setentrylen( &ad, ADEID_NAME, strlen( name ));
	bcopy( name, ad_entry( &ad, ADEID_NAME ),
		ad_getentrylen( &ad, ADEID_NAME ));
    }

    ad_setentrylen( &ad, ADEID_COMMENT, clen );
    bcopy( ibuf, ad_entry( &ad, ADEID_COMMENT ), clen );
    ad_flush( &ad, ADFLAGS_HF );
    ad_close( &ad, ADFLAGS_HF );
    return( AFP_OK );
}

afp_getcomment( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct adouble	ad;
    struct vol		*vol;
    struct dir		*dir;
    char		*path;
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

    if ( ad_open( mtoupath( path ),
	    ( *path == '\0' ) ? ADFLAGS_HF|ADFLAGS_DIR : ADFLAGS_HF,
	    O_RDONLY, 0666, &ad ) < 0 ) {
	return( AFPERR_NOITEM );
    }

    /*
     * Make sure the AD file is not bogus.
     */
    if ( ad_getentrylen( &ad, ADEID_COMMENT ) < 0 ||
	    ad_getentrylen( &ad, ADEID_COMMENT ) > 199 ) {
	ad_close( &ad, ADFLAGS_HF );
	return( AFPERR_NOITEM );
    }

    *rbuf++ = ad_getentrylen( &ad, ADEID_COMMENT );
    bcopy( ad_entry( &ad, ADEID_COMMENT ), rbuf,
	    ad_getentrylen( &ad, ADEID_COMMENT ));
    *rbuflen = ad_getentrylen( &ad, ADEID_COMMENT ) + 1;
    ad_close( &ad, ADFLAGS_HF );
    return( AFP_OK );
}

afp_rmvcomment( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct adouble	ad;
    struct vol		*vol;
    struct dir		*dir;
    char		*path;
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

    if ( ad_open( mtoupath( path ),
	    ( *path == '\0' ) ? ADFLAGS_HF|ADFLAGS_DIR : ADFLAGS_HF,
	    O_RDWR, 0, &ad ) < 0 ) {
	switch ( errno ) {
	case ENOENT :
	    return( AFPERR_NOITEM );
	case EACCES :
	    return( AFPERR_ACCESS );
	default :
	    return( AFPERR_PARAM );
	}
    }

    ad_setentrylen( &ad, ADEID_COMMENT, 0 );
    ad_flush( &ad, ADFLAGS_HF );
    ad_close( &ad, ADFLAGS_HF );
    return( AFP_OK );
}
