/*
 * Copyright (c) 1990,1991 Regents of The University of Michigan.
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of The University
 * of Michigan not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. This software is supplied as is without expressed or
 * implied warranties of any kind.
 *
 *	Research Systems Unix Group
 *	The University of Michigan
 *	c/o Mike Clark
 *	535 W. William Street
 *	Ann Arbor, Michigan
 *	+1-313-763-0525
 *	netatalk@itd.umich.edu
 */

#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <netatalk/endian.h>
#include <sys/syslog.h>
#include <atalk/adouble.h>
#include <strings.h>
#include <fcntl.h>
#include <unistd.h>

extern int		errno;

/*
 * Put the .AppleDouble where it needs to be:
 *
 *	    /	a/.AppleDouble/b
 *	a/b
 *	    \	b/.AppleDouble/.Parent
 */
char *
ad_path( path, adflags )
    char	*path;
    int		adflags;
{
    static char	pathbuf[ MAXPATHLEN ];
    char	c, *slash;

    if ( adflags & ADFLAGS_DIR ) {
	strcpy( pathbuf, path );
	if ( *path != '\0' ) {
	    strcat( pathbuf, "/" );
	}
	slash = ".Parent";
    } else {
	if (( slash = rindex( path, '/' )) != NULL ) {
	    c = *++slash;
	    *slash = '\0';
	    strcpy( pathbuf, path );
	    *slash = c;
	} else {
	    pathbuf[ 0 ] = '\0';
	    slash = path;
	}
    }
    strcat( pathbuf, ".AppleDouble/" );
    strcat( pathbuf, slash );

    return( pathbuf );
}

/*
 * Support inherited protection modes for AppleDouble files.  The supplied
 * mode is ANDed with the parent directory's mask value in lieu of "umask",
 * and that value is returned.
 */

#define DEFMASK 0700		/* be conservative */

int
ad_mode( path, mode )
    char		*path;
    int			mode;
{
    static char		modebuf[ MAXPATHLEN ];
    struct stat		stbuf;
    char 		*slash;

    if ( mode == 0 ) {
	return( mode );		/* save on syscalls */
    }

    if ( strlen( path ) >= MAXPATHLEN ) {
	return( mode & DEFMASK );  /* can't do it */
    }

    /*
     * For a path with directories in it, remove the final component
     * (path or subdirectory name) to get the name we want to stat.
     * For a path which is just a filename, use "." instead.
     */
    strcpy( modebuf, path );
    if (( slash = rindex( modebuf, '/' )) != NULL ) {
	*slash = '\0';		/* remove pathname component */
    } else {
	modebuf[0] = '.';	/* use current directory */
	modebuf[1] = '\0';
    }

    if ( stat( modebuf, &stbuf ) != 0 ) {
	return( mode & DEFMASK );	/* bail out... can't stat dir? */
    }

    return( mode & stbuf.st_mode );
}

/*
 * Use mkdir() with mode bits taken from ad_mode().
 */
int
ad_mkdir( path, mode )
    char		*path;
    int			mode;
{
    return mkdir( path, ad_mode( path, mode ) );
}

/*
 * It's not possible to open the header file O_RDONLY -- the read
 * will fail and return an error.
 */
ad_open( path, adflags, oflags, mode, ad )
    char		*path;
    int			adflags, oflags, mode;
    struct adouble	*ad;
{
    char		*slash, *ad_p;
    int			hoflags, admode;

    if ( adflags & ADFLAGS_DF ) {
	if (( ad->ad_df.adf_fd =
		open( path, oflags, ad_mode( path, mode ) )) < 0 ) {
	    return( -1 );
	}
	ad->ad_df.adf_off = 0;
	ad->ad_df.adf_flags = oflags;
    } else {
	ad->ad_df.adf_fd = -1;
    }

    if ( adflags & ADFLAGS_HF ) {
	ad_p = ad_path( path, adflags );
	admode = ad_mode( ad_p, mode );

	hoflags = oflags & ~O_CREAT;
	if (( ad->ad_hf.adf_fd = open( ad_p, hoflags, admode )) < 0 ) {
	    if ( errno == ENOENT && hoflags != oflags ) {
		/*
		 * We're expecting to create a new adouble header file,
		 * here.
		 */
		errno = 0;
		if (( ad->ad_hf.adf_fd = open( ad_p, oflags,
			admode )) < 0 ) {
		    /*
		     * Probably .AppleDouble doesn't exist, try to
		     * mkdir it.
		     */
		    if ( errno == ENOENT ) {
			if (( slash = rindex( ad_p, '/' )) == NULL ) {
			    ad_close( ad, adflags );
			    return( -1 );
			}
			*slash = '\0';
			errno = 0;
			if ( ad_mkdir( ad_p, 0777 ) < 0 ) {
			    ad_close( ad, adflags );
			    return( -1 );
			}
			*slash = '/';
			if (( ad->ad_hf.adf_fd = open( ad_p, oflags,
				ad_mode( ad_p, mode) )) < 0 ) {
			    ad_close( ad, adflags );
			    return( -1 );
			}
		    } else {
			ad_close( ad, adflags );
			return( -1 );
		    }
		}
		ad->ad_hf.adf_flags = oflags;
	    } else {
		ad_close( ad, adflags );
		return( -1 );
	    }
	} else {
	    ad->ad_hf.adf_flags = hoflags;
	}
	ad->ad_hf.adf_off = 0;

	/*
	 * This is a new adouble header file. Initialize the structure,
	 * instead of reading it.
	 */
	bzero( (char *)ad->ad_eid, sizeof( ad->ad_eid ));
	if ( ad->ad_hf.adf_flags & ( O_TRUNC | O_CREAT )) {
	    ad->ad_magic = AD_MAGIC;
	    ad->ad_version = AD_VERSION;
	    bzero( ad->ad_homefs, sizeof( ad->ad_homefs ));
	    bzero( ad->ad_data, sizeof( ad->ad_data ));

	    ad->ad_eid[ ADEID_RFORK ].ade_off = ADEDOFF_RFORK;
	    ad->ad_eid[ ADEID_RFORK ].ade_len = ADEDLEN_RFORK;
	    ad->ad_eid[ ADEID_NAME ].ade_off = ADEDOFF_NAME;
	    ad->ad_eid[ ADEID_NAME ].ade_len = ADEDLEN_NAME;
	    ad->ad_eid[ ADEID_COMMENT ].ade_off = ADEDOFF_COMMENT;
	    ad->ad_eid[ ADEID_COMMENT ].ade_len = ADEDLEN_COMMENT;
	    ad->ad_eid[ ADEID_FILEI ].ade_off = ADEDOFF_FILEI;
	    ad->ad_eid[ ADEID_FILEI ].ade_len = ADEDLEN_FILEI;
	    ad->ad_eid[ ADEID_FINDERI ].ade_off = ADEDOFF_FINDERI;
	    ad->ad_eid[ ADEID_FINDERI ].ade_len = ADEDLEN_FINDERI;

	} else {
	    /*
	     * Read the adouble header in and parse it.
	     */
	    if ( ad_refresh( ad ) < 0 ) {
		ad_close( ad, adflags );
		return( -1 );
	    }
	}
    } else {
	ad->ad_hf.adf_fd = -1;
    }
    return( 0 );
}

ad_refresh( ad )
    struct adouble	*ad;
{
    int			eid, len, off;
    char		*buf, *end;
    short		nentries;

    if ( ad->ad_hf.adf_fd == -1 ) {
	return( -1 );
    }

    if ( ad->ad_hf.adf_off != 0 ) {
	if ( lseek( ad->ad_hf.adf_fd, 0L, SEEK_SET ) < 0L ) {
	    return( -1 );
	}
	ad->ad_hf.adf_off = 0;
    }

    if ( read( ad->ad_hf.adf_fd, ad->ad_data,
	    sizeof( ad->ad_data )) != sizeof( ad->ad_data )) {
	if ( errno == 0 ) {
	    errno = EIO;
	}
	return( -1 );
    }
    ad->ad_hf.adf_off = sizeof( ad->ad_data );

    buf = ad->ad_data;
    end = ad->ad_data + sizeof( ad->ad_data );

    /*
     * we know that magic, version, homefs, and nentries are less
     * than data, so we don't check whether we exceed end.
     */
    bcopy( buf, (char *)&ad->ad_magic, sizeof( ad->ad_magic ));
    ad->ad_magic = ntohs( ad->ad_magic );
    buf += sizeof( ad->ad_magic );
    bcopy( buf, (char *)&ad->ad_version, sizeof( ad->ad_version ));
    ad->ad_version = ntohs( ad->ad_version );
    buf += sizeof( ad->ad_version );
    bcopy( buf, ad->ad_homefs, sizeof( ad->ad_homefs ));
    buf += sizeof( ad->ad_homefs );
    bcopy( buf, (char *)&nentries, sizeof( nentries ));
    nentries = ntohs( nentries );
    buf += sizeof( nentries );

    for (; nentries > 0; nentries-- ) {
	if ( buf + sizeof( eid ) + sizeof( off ) + sizeof( len ) > end ) {
	    errno = EIO;
	    return( -1 );
	}

	bcopy( buf, (char *)&eid, sizeof( eid ));
	eid = ntohl( eid );
	buf += sizeof( eid );
	bcopy( buf, (char *)&off, sizeof( off ));
	off = ntohl( off );
	buf += sizeof( off );
	bcopy( buf, (char *)&len, sizeof( len ));
	len = ntohl( len );
	buf += sizeof( len );

	if ( 0 < eid && eid < ADEID_MAX ) {
	    ad->ad_eid[ eid ].ade_off = off;
	    ad->ad_eid[ eid ].ade_len = len;
	} else {
	    syslog( LOG_DEBUG, "ad_refresh: nentries %hd  eid %d\n",
		    nentries, eid );
	}
    }

    return( 0 );
}
