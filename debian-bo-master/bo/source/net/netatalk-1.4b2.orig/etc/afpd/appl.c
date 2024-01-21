/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/syslog.h>
#include <netatalk/endian.h>
#include <sys/errno.h>
#include <atalk/afp.h>
#include <strings.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>

#include "volume.h"
#include "globals.h"
#include "directory.h"
#include "desktop.h"

extern int errno;
char *makemacpath();

struct savedt	sa = { { 0, 0, 0, 0 }, -1, 0 };

afp_addappl( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol		*vol;
    struct dir		*dir;
    int			did, tfd, cc;
    u_short		vid, mplen;
    char		*path, *dtf, *p, *mp;
    u_char		creator[ 4 ];
    u_char		appltag[ 4 ];
    char		mpath[ MAXPATHLEN ];
    char		tempfile[ MAXPATHLEN ];

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

    bcopy( ibuf, creator, sizeof( creator ));
    ibuf += sizeof( creator );

    bcopy( ibuf, appltag, sizeof( appltag ));
    ibuf += sizeof( appltag );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }
    if ( *path == '\0' ) {
	return( AFPERR_BADTYPE );
    }

    if ( applopen( vol, creator, O_RDWR|O_CREAT, 0666 ) != AFP_OK ) {
	return( AFPERR_PARAM );
    }
    if ( lseek( sa.sdt_fd, 0L, SEEK_SET ) < 0 ) {
	return( AFPERR_PARAM );
    }
    dtf = dtfile( vol, creator, ".appl.temp" );
    strcpy( tempfile, dtf );
    if (( tfd = open( tempfile, O_RDWR|O_CREAT, 0666 )) < 0 ) {
	return( AFPERR_PARAM );
    }
    mp = makemacpath( mpath, sizeof( mpath ), dir, path );
    mplen =  mpath + sizeof( mpath ) - mp;

    /* write the new appl entry at start of temporary file */
    p = mp - sizeof( u_short );
    mplen = htons( mplen );
    bcopy( &mplen, p, sizeof( u_short ));
    mplen = ntohs( mplen );
    p -= sizeof( appltag );
    bcopy( appltag, p, sizeof( appltag ));
    cc = mpath + sizeof( mpath ) - p;
    if ( write( tfd, p, cc ) != cc ) {
	unlink( tempfile );
	return( AFPERR_PARAM );
    }
    cc = copyapplfile( sa.sdt_fd, tfd, mp, mplen );
    close( tfd );
    close( sa.sdt_fd );
    sa.sdt_fd = -1;

    if ( cc < 0 ) {
	unlink( tempfile );
	return( AFPERR_PARAM );
    }
    if ( rename( tempfile, dtfile( vol, creator, ".appl" )) < 0 ) {
	return( AFPERR_PARAM );
    }
    return( AFP_OK );
}

afp_rmvappl( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol		*vol;
    struct dir		*dir;
    int			did, tfd, cc;
    u_short		vid, mplen;
    char		*path, *dtf, *mp;
    u_char		creator[ 4 ];
    char		mpath[ MAXPATHLEN ];
    char		tempfile[ MAXPATHLEN ];

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

    bcopy( ibuf, creator, sizeof( creator ));
    ibuf += sizeof( creator );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	return( AFPERR_NOOBJ );
    }
    if ( *path == '.' ) {
	return( AFPERR_BADTYPE );
    }

    if ( applopen( vol, creator, O_RDWR, 0666 ) != AFP_OK ) {
	return( AFPERR_NOOBJ );
    }
    if ( lseek( sa.sdt_fd, 0L, SEEK_SET ) < 0 ) {
	return( AFPERR_PARAM );
    }
    dtf = dtfile( vol, creator, ".appl.temp" );
    strcpy( tempfile, dtf );
    if (( tfd = open( tempfile, O_RDWR|O_CREAT, 0666 )) < 0 ) {
	return( AFPERR_PARAM );
    }
    mp = makemacpath( mpath, sizeof( mpath ), dir, path );
    mplen =  mpath + sizeof( mpath ) - mp;
    cc = copyapplfile( sa.sdt_fd, tfd, mp, mplen );
    close( tfd );
    close( sa.sdt_fd );
    sa.sdt_fd = -1;

    if ( cc < 0 ) {
	unlink( tempfile );
	return( AFPERR_PARAM );
    }
    if ( rename( tempfile, dtfile( vol, creator, ".appl" )) < 0 ) {
	return( AFPERR_PARAM );
    }
    return( AFP_OK );
}

afp_getappl( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct stat		st;
    struct vol		*vol;
    char		*p, *q;
    int			cc, buflen;
    u_short		vid, aindex, bitmap, len;
    u_char		creator[ 4 ];
    u_char		appltag[ 4 ];
    char		buf[ MAXPATHLEN ];
    char		cbuf[ MAXPATHLEN ];

    ibuf += 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, creator, sizeof( creator ));
    ibuf += sizeof( creator );

    bcopy( ibuf, &aindex, sizeof( aindex ));
    ibuf += sizeof( aindex );
    aindex = ntohs( aindex );
    if ( aindex != 0 ) {
	--aindex;
    }
    
    bcopy( ibuf, &bitmap, sizeof( bitmap ));
    bitmap = ntohs( bitmap );
    ibuf += sizeof( bitmap );

    if ( applopen( vol, creator, O_RDONLY, 0666 ) != AFP_OK ) {
	*rbuflen = 0;
	return( AFPERR_NOITEM );
    }
    if ( aindex < sa.sdt_index ) {
	if ( lseek( sa.sdt_fd, 0L, SEEK_SET ) < 0 ) {
	    *rbuflen = 0;
	    return( AFPERR_PARAM );
	}
	sa.sdt_index = 0;
    }

    /* position to correct spot within appl file */
    while (( cc = read( sa.sdt_fd, buf, sizeof( appltag )
	    + sizeof( u_short ))) > 0 ) {
	p = buf + sizeof( appltag );
	bcopy( p, &len, sizeof( u_short ));
	len = ntohs( len );
	p += sizeof( u_short );
	if (( cc = read( sa.sdt_fd, p, len )) < len ) {
	    break;
	}
	if ( sa.sdt_index == aindex ) {
	    break;
	}
	sa.sdt_index++;
    }
    if ( cc <= 0 || sa.sdt_index != aindex ) {
	*rbuflen = 0;
	return( AFPERR_NOITEM );
    }
    sa.sdt_index++;

#ifdef APPLCNAME
    /*
     * Check to see if this APPL mapping has an mpath or a upath.  If
     * there are any ':'s in the name, it is a upath and must be converted
     * to an mpath.  Hopefully, this code will go away.
     */
    {
#define hextoint( c )	( isdigit( c ) ? c - '0' : c + 10 - 'a' )
#define islxdigit(x)	(!isupper(x)&&isxdigit(x))

	static char	utomname[ MAXPATHLEN ];
	char		*u, *m;
	int		i, h;

	u = p;
	m = utomname;
	i = len;
	while ( i ) {
	    if ( *u == ':' && *(u+1) != '\0' && islxdigit( *(u+1)) &&
		    *(u+2) != '\0' && islxdigit( *(u+2))) {
		++u, --i;
		h = hextoint( *u ) << 4;
		++u, --i;
		h |= hextoint( *u );
		*m++ = h;
	    } else {
		*m++ = *u;
	    }
	    ++u, --i;
	}

	len = m - utomname;
	p = utomname;

	if ( p[ len - 1 ] == '\0' ) {
	    len--;
	}
    }
#endif APPLCNAME

    /* fake up a cname */
    q = cbuf;
    *q++ = 2;	/* long path type */
    *q++ = (unsigned char)len;
    bcopy( p, q, len );
    q = cbuf;

    if (( p = cname( vol, vol->v_dir, &q )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOITEM );
    }

    if ( stat( mtoupath( p ), &st ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_NOITEM );
    }
    buflen = *rbuflen - sizeof( bitmap ) - sizeof( appltag );
    if ( getfilparams( bitmap, p, curdir, &st, rbuf + sizeof( bitmap ) +
	    sizeof( appltag ), &buflen ) != AFP_OK ) {
	*rbuflen = 0;
	return( AFPERR_BITMAP );
    }

    *rbuflen = buflen + sizeof( bitmap ) + sizeof( appltag );
    bitmap = htons( bitmap );
    bcopy( &bitmap, rbuf, sizeof( bitmap ));
    rbuf += sizeof( bitmap );
    bcopy( appltag, rbuf, sizeof( appltag ));
    rbuf += sizeof( appltag );
    return( AFP_OK );
}

applopen( vol, creator, flags, mode )
    struct vol	*vol;
    u_char	creator[ 4 ];
{
    char	*dtf, *adt, *adts;

    if ( sa.sdt_fd != -1 ) {
	if ( !(flags & ( O_RDWR | O_WRONLY )) &&
		bcmp( sa.sdt_creator, creator, sizeof( creator )) == 0 &&
		sa.sdt_vid == vol->v_vid ) {
	    return( AFP_OK );
	}
	close( sa.sdt_fd );
	sa.sdt_fd = -1;
    }

    dtf = dtfile( vol, creator, ".appl" );

    if (( sa.sdt_fd = open( dtf, flags, ad_mode( dtf, mode ))) < 0 ) {
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

	    if (( sa.sdt_fd = open( dtf, flags, ad_mode( dtf, mode ))) < 0 ) {
		return( AFPERR_PARAM );
	    }
	} else {
	    return( AFPERR_PARAM );
	}
    }
    bcopy( creator, sa.sdt_creator, sizeof( creator ));
    sa.sdt_vid = vol->v_vid;
    sa.sdt_index = 0;
    return( AFP_OK );
}

/*
 * build mac. path (backwards) by traversing the directory tree
 *
 * The old way: dir and path refer to an app, path is a mac format
 * pathname.  makemacpath() builds something that looks like a cname,
 * but uses upaths instead of mac format paths.
 *
 * The new way: dir and path refer to an app, path is a mac format
 * pathname.  makemacpath() builds a cname.
 *
 * See afp_getappl() for the backward compatiblity code.
 */
char *
makemacpath( mpath, mpathlen, dir, path )
    char	*mpath;
    int		mpathlen;
    struct dir	*dir;
    char	*path;
{
    char	*p, *s;

    p = mpath + mpathlen;
    p -= strlen( path );
    strncpy( p, path, strlen( path ));

    while ( dir->d_parent != NULL ) {
	p -= strlen( dir->d_name ) + 1;
	strcpy( p, dir->d_name );
	dir = dir->d_parent;
    }
    return( p ); 
}

/*
 * copy appls to new file, deleting any matching (old) appl entries
 */
copyapplfile( sfd, dfd, mpath, mplen )
    int		sfd;
    int		dfd;
    char	*mpath;
    u_short	mplen;
{
    int		cc;
    char	*p;
    u_short	len;
    u_char	appltag[ 4 ];
    char	buf[ MAXPATHLEN ];

    while (( cc = read( sfd, buf, sizeof(appltag) + sizeof( u_short ))) > 0 ) {
	p = buf + sizeof(appltag);
	bcopy( p, &len, sizeof( u_short ));
	len = ntohs( len );
	p += sizeof( u_short );
	if (( cc = read( sa.sdt_fd, p, len )) < len ) {
	    break;
	}
	if ( pathcmp( mpath, mplen, p, len ) != 0 ) {
	    p += len;
	    if ( write( dfd, buf, p - buf ) != p - buf ) {
		cc = -1;
		break;
	    }
	}
    }
    return( cc );
}

pathcmp( p, plen, q, qlen )
    char	*p;
    u_short	plen;
    char	*q;
    u_short	qlen;
{
    return (( plen == qlen && bcmp( p, q, (int) plen ) == 0 ) ? 0 : 1 );
}
