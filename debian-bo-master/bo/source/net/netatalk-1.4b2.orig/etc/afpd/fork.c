/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/syslog.h>
#include <sys/param.h>
#include <sys/stat.h>
#if defined( sun ) && defined( __svr4__ )
#include </usr/ucbinclude/sys/file.h>
#else sun __svr4__
#include <sys/file.h>
#endif sun __svr4__
#include <sys/types.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/afp.h>
#include <atalk/atp.h>
#include <atalk/asp.h>
#include <atalk/adouble.h>
#include <stdio.h>
#include <fcntl.h>
#include <dirent.h>

#include "fork.h"
#include "file.h"
#include "globals.h"
#include "directory.h"
#include "volume.h"

extern int		errno;

struct ofork		*writtenfork;

afp_openfork( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct vol		*vol;
    struct dir		*dir;
    struct ofork	*ofork;
    int			did, buflen, oflags, ret, adflags;
    int			lockop = 0, lockfd;
    u_short		vid, bitmap, access, ofrefnum;
    char		fork, *path;

    ibuf++;
    fork = *ibuf++;
    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );

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

    bcopy( ibuf, &bitmap, sizeof( u_short ));
    bitmap = ntohs( bitmap );
    ibuf += sizeof( u_short );
    bcopy( ibuf, &access, sizeof( u_short ));
    access = ntohs( access );
    ibuf += sizeof( u_short );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOOBJ );
    }

    if (( ofork = of_alloc( curdir, path, &ofrefnum )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NFILE );
    }

    if ( access & OPENACC_WR ) {
	oflags = O_RDWR;
    } else {
	oflags = O_RDONLY;
    }
    if ( fork == OPENFORK_DATA ) {
	adflags = ADFLAGS_DF|ADFLAGS_HF;
    } else {
	adflags = ADFLAGS_HF;
    }

    if ( ad_open( mtoupath( path ), adflags, oflags, 0,
		&ofork->of_ad ) < 0 ) {
	if ( errno == ENOENT && adflags != ADFLAGS_HF ) {
	    ad_close( &ofork->of_ad, adflags );
	    if ( ad_open( mtoupath( path ), ADFLAGS_DF, oflags, 0,
		    &ofork->of_ad ) < 0 ) {
		ad_close( &ofork->of_ad, ADFLAGS_DF );
		of_dealloc( ofork );
		*rbuflen = 0;
		return( AFPERR_NOOBJ );
	    }
	} else {
	    of_dealloc( ofork );
	    *rbuflen = 0;
	    switch ( errno ) {
	    case EMFILE :
	    case ENFILE :
		return( AFPERR_NFILE );
	    case EISDIR :
		return( AFPERR_BADTYPE );
	    case ENOENT :
		return( AFPERR_NOOBJ );
	    case EACCES :
		return( AFPERR_ACCESS );
	    default :
		syslog( LOG_ERR, "afp_openfork: ad_open: %m" );
		return( AFPERR_PARAM );
	    }
	}
    }

    if ( ad_getoflags( &ofork->of_ad, ADFLAGS_HF ) & O_CREAT ) {
	ad_flush( &ofork->of_ad, adflags );
    }

    buflen = *rbuflen - 2 * sizeof( u_short );
    if (( ret = getforkparams( ofork, bitmap, rbuf + 2 * sizeof( u_short ),
	    &buflen )) != AFP_OK ) {
	*rbuflen = 0;
	ad_close( &ofork->of_ad, adflags );
	of_dealloc( ofork );
	return( ret );
    }

    *rbuflen = buflen + 2 * sizeof( u_short );
    bitmap = htons( bitmap );
    bcopy( &bitmap, rbuf, sizeof( u_short ));
    rbuf += sizeof( u_short );

    /*
     * Perform synchronization locks.  There are a couple of ways of
     * doing this.  The method we're using here, translates W & !R
     * to EX, and R & !W to SH.  This will allow multiple readers
     * and one writer.  An alternate method translates R & W to SH,
     * and !R & !W to EX.  This allows multiple writers, but limits
     * the reads to one, which doesn't make much sense, since most
     * apps will want to read *and* write.
     *
     * Note:  By doing this, there's pretty much no point in doing
     * byte range locking.  If you open for writing, you have a lock
     * no one else can break.
     *
     * One last thing:  We do the locking late, so we can still return
     * data on a "Deny Conflict" error.
     */
    if ( access & ( OPENACC_WR|OPENACC_DRD )) {		/* exclusive lock */
	lockop = LOCK_EX;
    } else if ( access & ( OPENACC_RD|OPENACC_DWR )) {	/* shared lock */
#ifdef AFS
	lockop = 0;
#else
	lockop = LOCK_SH;
#endif AFS
    }
    if ( fork == OPENFORK_DATA ) {
	lockfd = ad_dfileno( &ofork->of_ad );
    } else {
	lockfd = ad_hfileno( &ofork->of_ad );
    }
    if ( lockop && flock( lockfd, lockop|LOCK_NB ) < 0 ) {
	ret = errno;
	ad_close( &ofork->of_ad, adflags );
	of_dealloc( ofork );
	if ( ret == EWOULDBLOCK ) {			/* return data anyway */
	    ofrefnum = 0;
	    bcopy( &ofrefnum, rbuf, sizeof( u_short ));
	    return( AFPERR_DENYCONF );
	} else {
	    *rbuflen = 0;
	    syslog( LOG_ERR, "afp_openfork: flock: %m" );
	    return( AFPERR_PARAM );
	}
    }

    bcopy( &ofrefnum, rbuf, sizeof( u_short ));
    return( AFP_OK );
}

afp_setforkparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ofork	*ofork;
    long		intime;
    off_t		size;
    int			isize;
    u_short		ofrefnum;

    ibuf += 2;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));
    ibuf += sizeof( u_short );
    ibuf += sizeof( u_short );
    bcopy( ibuf, &isize, sizeof( int ));
    size = ntohl( isize );

    *rbuflen = 0;
    if (( ofork = of_find( ofrefnum )) == NULL ) {
	syslog( LOG_ERR, "afp_setforkparams: of_find: %m" );
	return( AFPERR_PARAM );
    }

    if ( ad_dfileno( &ofork->of_ad ) != -1 ) {
	if ( ad_dtruncate( &ofork->of_ad, size ) < 0 ) {
	    syslog( LOG_ERR, "afp_setforkparams: ad_dtruncate: %m" );
	    return( AFPERR_PARAM );
	}
    } else if ( ad_hfileno( &ofork->of_ad ) != -1 ) {
	bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, &intime, sizeof( intime ));
	ad_refresh( &ofork->of_ad, ADFLAGS_HF );
	bcopy( &intime, ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, sizeof( intime ));
	if ( ad_rtruncate( &ofork->of_ad, size ) < 0 ) {
	    syslog( LOG_ERR, "afp_setforkparams: ad_rtruncate: %m" );
	    return( AFPERR_PARAM );
	}
	if ( ad_flush( &ofork->of_ad, ADFLAGS_HF ) < 0 ) {
	    syslog( LOG_ERR, "afp_setforkparams: ad_flush: %m" );
	    return( AFPERR_PARAM );
	}
    }

#ifdef AFS
    if ( flushfork( ofork ) < 0 ) {
	syslog( LOG_ERR, "afp_setforkparams: flushfork: %m" );
    }
#endif AFS

    return( AFP_OK );
}

afp_bytelock( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    int		off;
    u_short	ofrefnum;
    
    ibuf += 2;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));
    ibuf += sizeof( u_short );
    bcopy( ibuf, &off, sizeof( int ));

    bcopy( &off, rbuf, sizeof( int ));
    *rbuflen = sizeof( int );
    return( AFP_OK );
}

#ifdef CRLF
crlf( of )
    struct ofork	*of;
{
    struct extmap	*em;

    if ( ad_hfileno( &of->of_ad ) == -1 ||
	    bcmp( ufinderi, ad_entry( &of->of_ad, ADEID_FINDERI ), 8 ) == 0 ) {
	if (( em = getextmap( of->of_name )) == NULL ||
		bcmp( "TEXT", em->em_type, sizeof( em->em_type )) == 0 ) {
	    return( 1 );
	} else {
	    return( 0 );
	}
    } else {
	if ( bcmp( ufinderi, ad_entry( &of->of_ad, ADEID_FINDERI ), 4 ) == 0 ) {
	    return( 1 );
	} else {
	    return( 0 );
	}
    }
}
#endif CRLF

afp_read( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ofork	*ofork;
    long		offset;
    int			reqcount, cc, rc, eid, eof = 0;
    u_short		ofrefnum;
    u_char		nlmask, nlchar;
    register char	*p, *q;

    ibuf += 2;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));
    ibuf += sizeof( u_short );

    if (( ofork = of_find( ofrefnum )) == NULL ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "afp_read: of_find: %m" );
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &offset, sizeof( long ));
    offset = ntohl( offset );
    ibuf += sizeof( long );
    bcopy( ibuf, &reqcount, sizeof( long ));
    reqcount = ntohl( reqcount );
    ibuf += sizeof( long );

    nlmask = *ibuf++;
    nlchar = *ibuf++;

    if ( ad_dfileno( &ofork->of_ad ) != -1 ) {
	eid = ADEID_DFORK;
    } else {
	eid = ADEID_RFORK;
    }

    if ( reqcount < 0 ) {
	*rbuflen = 0;
	return( AFPERR_EOF );
    }
#define min(a,b)	((a)<(b)?(a):(b))
    rc = min( reqcount, *rbuflen );
    cc = ad_read( &ofork->of_ad, eid, offset, rbuf, rc );
    if ( cc < 0 ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "afp_read: ad_read: %m" );
	return( AFPERR_PARAM );
    }
    if ( cc < rc ) {
	eof = 1;
    }

    /*
     * Do Newline check.
     */
    if ( nlmask != 0 ) {
	for ( p = rbuf, q = p + cc; p < q; ) {
	    if (( *p++ & nlmask ) == nlchar ) {
		break;
	    }
	}
	if ( p != q ) {
	    cc = p - rbuf;
	    eof = 0;
	}
    }

#ifdef CRLF
    /*
     * If this file is of type TEXT, then swap \012 to \015.
     */
    if ( eid == ADEID_DFORK && crlf( ofork )) {
	for ( p = rbuf, q = p + cc; p < q; p++ ) {
	    if ( *p == '\012' ) {
		*p = '\015';
	    }
	}
    }
#endif CRLF

    *rbuflen = cc;
    if ( eof ) {
	return( AFPERR_EOF );
    }
    return( AFP_OK );
}

afp_flush( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    u_short	ofrefnum;

    *rbuflen = 0;
    of_flush();
    return( AFP_OK );
}

afp_flushfork( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ofork	*ofork;
    u_short		ofrefnum;

    *rbuflen = 0;
    ibuf += 2;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));

    if (( ofork = of_find( ofrefnum )) == NULL ) {
	syslog( LOG_ERR, "afp_flushfork: of_find: %m" );
	return( AFPERR_PARAM );
    }

    if ( flushfork( ofork ) < 0 ) {
	syslog( LOG_ERR, "afp_flushfork: %m" );
    }

    return( AFP_OK );
}

flushfork( ofork )
    struct ofork	*ofork;
{
    int err = 0;

    if ( ad_dfileno( &ofork->of_ad ) != -1 &&
	    fsync( ad_dfileno( &ofork->of_ad )) < 0 ) {
	syslog( LOG_ERR, "flushfork: dfile %m" );
	err = -1;
    }
    if ( ad_hfileno( &ofork->of_ad ) != -1 &&
	    fsync( ad_hfileno( &ofork->of_ad )) < 0 ) {
	syslog( LOG_ERR, "flushfork: hfile %m" );
	err = -1;
    }
    return( err );
}

afp_closefork( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ofork	*ofork;
    long		intime, filetime;
    int			adflags, aint, doflush = 0;
    u_short		ofrefnum;

    *rbuflen = 0;
    ibuf += 2;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));

    if (( ofork = of_find( ofrefnum )) == NULL ) {
	syslog( LOG_ERR, "afp_closefork: of_find: %m" );
	return( AFPERR_PARAM );
    }

    adflags = 0;
    if ( ad_dfileno( &ofork->of_ad ) != -1 ) {
	adflags |= ADFLAGS_DF;
    }
    if ( ad_hfileno( &ofork->of_ad ) != -1 ) {
	adflags |= ADFLAGS_HF;

	aint = ad_getentrylen( &ofork->of_ad, ADEID_RFORK );
	bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, &intime, sizeof( intime ));
	ad_refresh( &ofork->of_ad, adflags );
	bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, &filetime, sizeof( filetime ));
	if ( intime != filetime ) {
	    bcopy( &intime, ad_entry( &ofork->of_ad, ADEID_FILEI )
		    + FILEIOFF_MODIFY, sizeof( filetime ));
	    doflush++;
	}

	/*
	 * Only set the rfork's length if we're closing the rfork.
	 */
	if (( adflags & ADFLAGS_DF ) == 0 && aint !=
		ad_getentrylen( &ofork->of_ad, ADEID_RFORK )) {
	    ad_setentrylen( &ofork->of_ad, ADEID_RFORK, aint );
	    doflush++;
	}
	if ( doflush ) {
	    ad_flush( &ofork->of_ad, adflags );
	}
    }

    if ( ad_close( &ofork->of_ad, adflags ) < 0 ) {
	syslog( LOG_ERR, "afp_closefork: ad_close: %m" );
	return( AFPERR_PARAM );
    }
    of_dealloc( ofork );
    return( AFP_OK );
}

afp_write( ibuf, ibuflen, rbuf, rbuflen, asp )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
    ASP		asp;
{
    struct ofork	*ofork;
    struct timeval	tv;
    register char	*p, *q;
    int			endflag, offset, reqcount, cc, eid;
    u_short		ofrefnum;

    ibuf += 1;
    endflag = *ibuf++;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));
    ibuf += sizeof( u_short );
    bcopy( ibuf, &offset, sizeof( int ));
    offset = ntohl( offset );
    ibuf += sizeof( int );
    bcopy( ibuf, &reqcount, sizeof( int ));
    reqcount = ntohl( reqcount );
    ibuf += sizeof( int );

    if (( ofork = of_find( ofrefnum )) == NULL ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "afp_write: of_find: %m" );
	return( AFPERR_PARAM );
    }
#ifdef AFS
    writtenfork = ofork;
#endif AFS

    if ( asp_wrtcont( asp, rbuf, rbuflen ) < 0 ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "afp_write: asp_wrtcont: %m" );
	return( AFPERR_PARAM );
    }

    if ( ad_dfileno( &ofork->of_ad ) != -1 ) {
	eid = ADEID_DFORK;
    } else {
	eid = ADEID_RFORK;
    }

#ifdef CRLF
    /*
     * If this file is of type TEXT, swap \015 to \012.
     */
    if ( eid == ADEID_DFORK && crlf( ofork )) {
	for ( p = rbuf, q = p + *rbuflen; p < q; p++ ) {
	    if ( *p == '\015' ) {
		*p = '\012';
	    }
	}
    }
#endif CRLF

    if (( cc = ad_write( &ofork->of_ad,
	    eid, offset, endflag, rbuf, *rbuflen )) < 0 ) {
	switch ( errno ) {
	case EDQUOT :
	case EFBIG :
	case ENOSPC :
	    *rbuflen = 0;
	    return( AFPERR_DFULL );
	default :
	    syslog( LOG_ERR, "afp_write: ad_write: %m" );
	    *rbuflen = 0;
	    return( AFPERR_PARAM );
	}
    }

    if ( ad_hfileno( &ofork->of_ad ) != -1 ) {
	if ( gettimeofday( &tv, 0 ) < 0 ) {
	    syslog( LOG_ERR, "afp_write: gettimeofday: %m" );
	    exit( 1 );
	}
	tv.tv_sec = htonl( tv.tv_sec );
	bcopy( &tv.tv_sec, ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, sizeof( tv.tv_sec ));
    }

    offset += cc;
    offset = htonl( offset );
    bcopy( &offset, rbuf, sizeof( int ));
    *rbuflen = 4;
    return( AFP_OK );
}

afp_getforkparams( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct ofork	*ofork;
    int			buflen, ret;
    u_short		ofrefnum, bitmap;

    ibuf += 2;
    bcopy( ibuf, &ofrefnum, sizeof( u_short ));
    ibuf += sizeof( u_short );
    bcopy( ibuf, &bitmap, sizeof( u_short ));
    bitmap = ntohs( bitmap );
    ibuf += sizeof( u_short );

    if (( ofork = of_find( ofrefnum )) == NULL ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "afp_getforkparams: of_find: %m" );
	return( AFPERR_PARAM );
    }

    buflen = *rbuflen - sizeof( u_short );
    if (( ret = getforkparams( ofork, bitmap,
	    rbuf + sizeof( u_short ), &buflen )) != AFP_OK ) {
	*rbuflen = 0;
	return( ret );
    }

    *rbuflen = buflen + sizeof( u_short );
    bitmap = htons( bitmap );
    bcopy( &bitmap, rbuf, sizeof( u_short ));
    return( AFP_OK );
}

getforkparams( ofork, bitmap, buf, buflen )
    struct ofork	*ofork;
    u_short		bitmap;
    char		*buf;
    int			*buflen;
{
    struct stat		st;
    struct extmap	*em;
    char		*data, *nameoff = 0;
    long		intime;
    int			bit = 0, isad = 1, aint;
    u_short		ashort;

    if ( ad_hfileno( &ofork->of_ad ) == -1 ) {
	isad = 0;
    }

    if ( isad ) {
	aint = ad_getentrylen( &ofork->of_ad, ADEID_RFORK );
	bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, &intime, sizeof( intime ));
	if ( ad_refresh( &ofork->of_ad ) < 0 ) {
	    syslog( LOG_ERR, "getforkparams: ad_refresh: %m" );
	    return( AFPERR_PARAM );
	}
	/* See afp_closefork() for why this is bad */
	ad_setentrylen( &ofork->of_ad, ADEID_RFORK, aint );
	bcopy( &intime, ad_entry( &ofork->of_ad, ADEID_FILEI ) +
		FILEIOFF_MODIFY, sizeof( intime ));
    }

    if (( bitmap & ( 1<<FILPBIT_DFLEN )) &&
	    ad_dfileno( &ofork->of_ad ) == -1 ) {
	return( AFPERR_BITMAP );
    }
    if ( bitmap & ( 1<<FILPBIT_DFLEN | 1<<FILPBIT_FNUM )) {
	if ( ad_dfileno( &ofork->of_ad ) == -1 ) {
	    if ( fstat( ad_hfileno( &ofork->of_ad ), &st ) < 0 ) {
		return( AFPERR_BITMAP );
	    }
	} else {
	    if ( fstat( ad_dfileno( &ofork->of_ad ), &st ) < 0 ) {
		return( AFPERR_BITMAP );
	    }
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
		bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI ) + FILEIOFF_ATTR,
			&ashort, sizeof( u_short ));
	    } else {
		ashort = 0;
	    }
	    bcopy( &ashort, data, sizeof( u_short ));
	    data += sizeof( u_short );
	    break;

	case FILPBIT_PDID :
	    bcopy( &ofork->of_dir->d_did, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_CDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI )
			+ FILEIOFF_CREATE, &aint, sizeof( int ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_MDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI )
			+ FILEIOFF_MODIFY, &aint, sizeof( int ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_BDATE :
	    if ( isad ) {
		bcopy( ad_entry( &ofork->of_ad, ADEID_FILEI )
			+ FILEIOFF_BACKUP, &aint, sizeof( int ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_FINFO :
	    if ( !isad || bcmp( ad_entry( &ofork->of_ad, ADEID_FINDERI ),
		    ufinderi, 8 ) == 0 ) {
		bcopy( ufinderi, data, 32 );
		if (( em = getextmap( ofork->of_name )) != NULL ) {
		    bcopy( em->em_type, data, sizeof( em->em_type ));
		    bcopy( em->em_creator, data + 4, sizeof( em->em_creator ));
		}
	    } else {
		bcopy( ad_entry( &ofork->of_ad, ADEID_FINDERI ), data, 32 );
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
	     * See file.c getfilparams() for why this is done this
	     * way.
	     */
#ifdef AFS
	    aint = st.st_ino;
#else AFS
	    aint = ( st.st_dev << 16 ) | ( st.st_ino & 0x0000ffff );
#endif AFS
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_DFLEN :
	    aint = htonl( st.st_size );
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	case FILPBIT_RFLEN :
	    if ( isad ) {
		aint = htonl( ad_getentrylen( &ofork->of_ad, ADEID_RFORK ));
	    } else {
		aint = 0;
	    }
	    bcopy( &aint, data, sizeof( int ));
	    data += sizeof( int );
	    break;

	default :
	    return( AFPERR_BITMAP );
	}
	bitmap = bitmap>>1;
	bit++;
    }

    if ( nameoff != 0 ) {
	ashort = htons( data - buf );
	bcopy( &ashort, nameoff, sizeof( u_short ));
	aint = strlen( ofork->of_name );
	aint = ( aint > 31 ) ? 31 : aint;
	*data++ = aint;
	bcopy( ofork->of_name, data, aint );
	data += aint;
    }

    *buflen = data - buf;
    return( AFP_OK );
}
