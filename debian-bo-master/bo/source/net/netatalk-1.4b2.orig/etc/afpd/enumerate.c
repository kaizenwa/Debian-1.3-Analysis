/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <netatalk/endian.h>
#include <sys/param.h>
#include <atalk/afp.h>
#include <atalk/adouble.h>
#include <stdio.h>
#include <dirent.h>

#include "desktop.h"
#include "directory.h"
#include "volume.h"
#include "globals.h"

struct dir *
adddir( vol, dir, name, namlen )
    struct vol	*vol;
    struct dir	*dir;
    char	*name;
    int		namlen;
{
    struct dir	*cdir;

    if (( cdir = (struct dir *)malloc( sizeof( struct dir ))) == NULL ) {
	syslog( LOG_ERR, "adddir: malloc: %m" );
	exit( 1 );
    }
    cdir->d_left = NULL;
    cdir->d_right = NULL;
    cdir->d_parent = dir;
    cdir->d_child = NULL;
    cdir->d_did = htonl( vol->v_lastdid++ );
    cdir->d_flags = 0;
    if (( cdir->d_name = (char *)malloc( namlen + 1 )) == NULL ) {
	syslog( LOG_ERR, "adddir: malloc: %m" );
	exit( 1 );
    }
    strcpy( cdir->d_name, name );
    if ( dirinsert( vol, cdir ) < 0 ) {
	return( NULL );
    }

    cdir->d_next = dir->d_child;
    dir->d_child = cdir;

    return( cdir );
}

/*
 * Struct to save directory reading context in. Used to prevent
 * O(n^2) searches on a directory.
 */
struct savedir {
    u_short	sd_vid;
    int		sd_did;
    int		sd_buflen;
    char	*sd_buf;
    char	*sd_last;
    int		sd_sindex;
};
#define SDBUFBRK	1024

afp_enumerate( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct stat			st;
    struct adouble		ad;
    static struct savedir	sd = { 0, 0, 0, 0, 0, 0 };
    struct vol			*vol;
    struct dir			*dir;
    struct dirent		*de;
    DIR				*dp;
    int				did, adret, ret, esz, len;
    char			*path, *data, *end, *start, dbuf[ MAXPATHLEN ];
    u_short			vid, fbitmap, dbitmap, reqcnt, actcnt = 0;
    u_short			sindex, maxsz, sz = 0;

    if ( sd.sd_buflen == 0 ) {
	if (( sd.sd_buf = (char *)malloc( SDBUFBRK )) == NULL ) {
	    syslog( LOG_ERR, "afp_enumerate: malloc: %m" );
	    exit( 1 );
	}
	sd.sd_buflen = SDBUFBRK;
    }

    ibuf += 2;
    ibuflen -= 2;

    bcopy( ibuf, &vid, sizeof( u_short ));
    ibuf += sizeof( u_short );
    ibuflen -= sizeof( u_short );
    if (( vol = getvolbyvid( vid )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    bcopy( ibuf, &did, sizeof( int ));
    ibuf += sizeof( int );
    ibuflen -= sizeof( int );
    if (( dir = dirsearch( vol, did )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NODIR );
    }

    bcopy( ibuf, &fbitmap, sizeof( u_short ));
    fbitmap = ntohs( fbitmap );
    ibuf += sizeof( u_short );
    ibuflen -= sizeof( u_short );

    bcopy( ibuf, &dbitmap, sizeof( u_short ));
    dbitmap = ntohs( dbitmap );
    ibuf += sizeof( u_short );
    ibuflen -= sizeof( u_short );

    bcopy( ibuf, &reqcnt, sizeof( u_short ));
    reqcnt = ntohs( reqcnt );
    ibuf += sizeof( u_short );
    ibuflen -= sizeof( u_short );

    bcopy( ibuf, &sindex, sizeof( u_short ));
    sindex = ntohs( sindex );
    ibuf += sizeof( u_short );
    ibuflen -= sizeof( u_short );

    bcopy( ibuf, &maxsz, sizeof( u_short ));
    maxsz = ntohs( maxsz );
    ibuf += sizeof( u_short );
    ibuflen -= sizeof( u_short );

    if (( path = cname( vol, dir, &ibuf )) == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NODIR );
    }
    data = rbuf + 3 * sizeof( u_short );
    sz = 3 * sizeof( u_short );

    /*
     * Read the directory into a pre-malloced buffer, stored
     *		len <name> \0
     * The end is indicated by a len of 0.
     */
    if ( sindex == 1 || curdir->d_did != sd.sd_did || vid != sd.sd_vid ) {
	sd.sd_last = sd.sd_buf;

	if (( dp = opendir( mtoupath( path ))) == NULL ) {
	    *rbuflen = 0;
	    return( AFPERR_NODIR );
	}

	end = sd.sd_buf + sd.sd_buflen;
	for ( de = readdir( dp ); de != NULL; de = readdir( dp )) {

	    if ( de->d_name[ 0 ] == '.' ) {	/* Don't show .login, etc */
		continue;
	    }

	    *(sd.sd_last)++ = strlen( de->d_name );

	    if ( sd.sd_last + strlen( de->d_name ) + 2 > end ) {
		start = sd.sd_buf;
		if (( sd.sd_buf = (char *)realloc( sd.sd_buf,
			sd.sd_buflen + SDBUFBRK )) == NULL ) {
		    syslog( LOG_ERR, "afp_enumerate: realloc: %m" );
		    exit( 1 );
		}

		sd.sd_buflen += SDBUFBRK;
		sd.sd_last = ( sd.sd_last - start ) + sd.sd_buf;
		end = sd.sd_buf + sd.sd_buflen;
	    }

	    bcopy( de->d_name, sd.sd_last, strlen( de->d_name ) + 1 );
	    sd.sd_last += strlen( de->d_name ) + 1;
	}
	*sd.sd_last = 0;

	sd.sd_last = sd.sd_buf;
	sd.sd_sindex = 1;

	closedir( dp );
	sd.sd_vid = vid;
	sd.sd_did = did;
    }

    /*
     * Position sd_last as dictated by sindex.
     */
    if ( sindex < sd.sd_sindex ) {
	sd.sd_sindex = 1;
	sd.sd_last = sd.sd_buf;
    }
    while ( sd.sd_sindex < sindex ) {
	len = *(sd.sd_last)++;
	if ( len == 0 ) {
	    sd.sd_did = -1;	/* invalidate sd struct to force re-read */
	    *rbuflen = 0;
	    return( AFPERR_NOOBJ );
	}
	sd.sd_last += len + 1;
	sd.sd_sindex++;
    }

    while (( len = *(sd.sd_last)) != 0 ) {
	/*
	 * If we've got all we need, send it.
	 */
	if ( actcnt == reqcnt ) {
	    break;
	}

	/*
	 * Save the start position, in case we exceed the buffer
	 * limitation, and have to back up one.
	 */
	start = sd.sd_last;
	sd.sd_last++;

	if ( stat( sd.sd_last, &st ) < 0 ) {
	    syslog( LOG_DEBUG, "afp_enumerate: stat %s: %m", sd.sd_last );
	    sd.sd_last += len + 1;
	    continue;
	}

	/*
	 * If a fil/dir is not a dir, it's a file. This is slightly
	 * inaccurate, since that means /dev/null is a file, /dev/printer
	 * is a file, etc.
	 */
	if ( st.st_mode & S_IFDIR ) {
	    if ( dbitmap == 0 ) {
		sd.sd_last += len + 1;
		continue;
	    }
	    path = utompath( sd.sd_last );

	    for ( dir = curdir->d_child; dir; dir = dir->d_next ) {
		if ( strcmp( dir->d_name, path ) == 0 ) {
		    break;
		}
	    }
	    if ( dir == NULL ) {
		dir = adddir( vol, curdir, path, strlen( path ));
	    }

	    if (( ret = getdirparams( dbitmap, sd.sd_last, dir,
		    &st, data + 2 * sizeof( u_char ), &esz )) != AFP_OK ) {
		*rbuflen = 0;
		return( ret );
	    }

	} else {
	    if ( fbitmap == 0 ) {
		sd.sd_last += len + 1;
		continue;
	    }
	    if (( ret = getfilparams( fbitmap, utompath( sd.sd_last ),
		    curdir, &st, data + 2 * sizeof( u_char ), &esz )) !=
		    AFP_OK ) {
		*rbuflen = 0;
		return( ret );
	    }
	}

	/*
	 * Make sure entry is an even length, possibly with a null
	 * byte on the end.
	 */
	if ( esz & 1 ) {
	    *(data + 2 * sizeof( u_char ) + esz ) = '\0';
	    esz++;
	}

	/*
	 * Check if we've exceeded the size limit.
	 */
	if ( maxsz < sz + esz + 2 * sizeof( u_char )) {
	    sd.sd_last = start;
	    break;
	}

	sz += esz + 2 * sizeof( u_char );
	*data++ = esz + 2 * sizeof( u_char );
	if ( st.st_mode & S_IFDIR ) {
	    *data++ = 1<<7;
	} else {
	    *data++ = 0;
	}
	data += esz;
	actcnt++;
	sd.sd_last += len + 1;
    }

    if ( actcnt == 0 ) {
	*rbuflen = 0;
	sd.sd_did = -1;		/* invalidate sd struct to force re-read */
	return( AFPERR_NOOBJ );
    }
    sd.sd_sindex = sindex + actcnt;

    /*
     * All done, fill in misc junk in rbuf
     */
    fbitmap = htons( fbitmap );
    bcopy( &fbitmap, rbuf, sizeof( u_short ));
    rbuf += sizeof( u_short );
    dbitmap = htons( dbitmap );
    bcopy( &dbitmap, rbuf, sizeof( u_short ));
    rbuf += sizeof( u_short );
    actcnt = htons( actcnt );
    bcopy( &actcnt, rbuf, sizeof( u_short ));
    rbuf += sizeof( u_short );
    *rbuflen = sz;
    return( AFP_OK );
}
