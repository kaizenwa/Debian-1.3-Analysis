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
#include <netatalk/endian.h>
#include <atalk/adouble.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

extern int		errno;

ad_flush( ad, adflags )
    struct adouble	*ad;
    int			adflags;
{
    int			eid;
    short		nent;
    char		*buf, *nentp;

    if (( adflags & ADFLAGS_HF ) &&
	    ( ad->ad_hf.adf_flags & O_RDWR )) {
	/*
	 * Rebuild any header information that might have changed.
	 */
	buf = ad->ad_data;
	ad->ad_magic = htonl( ad->ad_magic );
	bcopy( (char *)&ad->ad_magic, buf, sizeof( ad->ad_magic ));
	ad->ad_magic = ntohl( ad->ad_magic );
	buf += sizeof( ad->ad_magic );

	ad->ad_version = htonl( ad->ad_version );
	bcopy( (char *)&ad->ad_version, buf, sizeof( ad->ad_version ));
	ad->ad_version = ntohl( ad->ad_version );
	buf += sizeof( ad->ad_version );

	bcopy( ad->ad_homefs, buf, sizeof( ad->ad_homefs ));
	buf += sizeof( ad->ad_homefs );
	nentp = buf;
	buf += sizeof( nent );

	for ( eid = 0, nent = 0; eid < ADEID_MAX; eid++ ) {
	    if ( ad->ad_eid[ eid ].ade_off == 0 ) {
		continue;
	    }
	    eid = htonl( eid );
	    bcopy( (char *)&eid, buf, sizeof( eid ));
	    eid = ntohl( eid );
	    buf += sizeof( eid );
	    ad->ad_eid[ eid ].ade_off = htonl( ad->ad_eid[ eid ].ade_off );
	    bcopy( (char *)&ad->ad_eid[ eid ].ade_off, buf,
		    sizeof( ad->ad_eid[ eid ].ade_off ));
	    ad->ad_eid[ eid ].ade_off = ntohl( ad->ad_eid[ eid ].ade_off );
	    buf += sizeof( ad->ad_eid[ eid ].ade_off );
	    ad->ad_eid[ eid ].ade_len = htonl( ad->ad_eid[ eid ].ade_len );
	    bcopy( (char *)&ad->ad_eid[ eid ].ade_len, buf,
		    sizeof( ad->ad_eid[ eid ].ade_len ));
	    ad->ad_eid[ eid ].ade_len = ntohl( ad->ad_eid[ eid ].ade_len );
	    buf += sizeof( ad->ad_eid[ eid ].ade_len );
	    nent++;
	}
	nent = htons( nent );
	bcopy( (char *)&nent, nentp, sizeof( nent ));

	if ( ad->ad_hf.adf_off != 0 ) {
	    if ( lseek( ad->ad_hf.adf_fd, 0L, SEEK_SET ) < 0L ) {
		return( -1 );
	    }
	    ad->ad_hf.adf_off = 0;
	}
	if ( write( ad->ad_hf.adf_fd, ad->ad_data, sizeof( ad->ad_data )) !=
		sizeof( ad->ad_data )) {
	    if ( errno == 0 ) {
		errno = EIO;
	    }
	    return( -1 );
	}
	ad->ad_hf.adf_off = sizeof( ad->ad_data );
    }
    return( 0 );
}

ad_close( ad, adflags )
    struct adouble	*ad;
    int			adflags;
{
    int			err = 0;

    if (( adflags & ADFLAGS_DF ) && ad->ad_df.adf_fd != -1 ) {
	if ( close( ad->ad_df.adf_fd ) < 0 ) {
	    err = -1;
	}
	ad->ad_df.adf_fd = -1;
    }
    if (( adflags & ADFLAGS_HF ) && ad->ad_hf.adf_fd != -1 ) {
	if ( close( ad->ad_hf.adf_fd ) < 0 ) {
	    err = -1;
	}
	ad->ad_hf.adf_fd = -1;
    }
    return( err );
}
