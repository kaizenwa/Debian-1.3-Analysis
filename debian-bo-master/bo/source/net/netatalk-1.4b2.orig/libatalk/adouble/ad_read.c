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

#include <sys/types.h>
#include <sys/param.h>
#include <atalk/adouble.h>
#include <fcntl.h>
#include <unistd.h>

ad_read( ad, eid, off, buf, buflen )
    struct adouble	*ad;
    int			eid, off, buflen;
    char		*buf;
{
    int			cc;

    /*
     * We're either reading the data fork (and thus the data file)
     * or we're reading anything else (and thus the header file).
     */
    if ( eid == ADEID_DFORK ) {
	if ( ad->ad_df.adf_off != off ) {
	    if ( lseek( ad->ad_df.adf_fd, off, SEEK_SET ) < 0 ) {
		perror( "df lseek" );
		return( -1 );
	    }
	    ad->ad_df.adf_off = off;
	}
	if (( cc = read( ad->ad_df.adf_fd, buf, buflen )) < 0 ) {
	    perror( "df read" );
	    return( -1 );
	}
	ad->ad_df.adf_off += cc;
    } else {
	if ( ad->ad_hf.adf_off != ad->ad_eid[ eid ].ade_off + off ) {
	    if ( lseek( ad->ad_hf.adf_fd,
		    ad->ad_eid[ eid ].ade_off + off, SEEK_SET ) < 0 ) {
		perror( "hf lseek" );
		return( -1 );
	    }
	    ad->ad_hf.adf_off = ad->ad_eid[ eid ].ade_off + off;
	}
	if (( cc = read( ad->ad_hf.adf_fd, buf, buflen )) < 0 ) {
	    perror( "hf read" );
	    return( -1 );
	}

	/*
	 * We've just read in bytes from the disk that we read earlier
	 * into ad_data. If we're going to write this buffer out later,
	 * we need to update ad_data.
	 */
	if ( ad->ad_hf.adf_off < sizeof( ad->ad_data )) {
	    if ( ad->ad_hf.adf_flags & O_RDWR ) {
#define min(a,b)	((a)<(b)?(a):(b))
		bcopy( ad->ad_data + ad->ad_hf.adf_off, buf,
			min( sizeof( ad->ad_data ) - ad->ad_hf.adf_off, cc ));
	    } else {
		bcopy( buf, ad->ad_data + ad->ad_hf.adf_off,
			min( sizeof( ad->ad_data ) - ad->ad_hf.adf_off, cc ));
	    }
	}

	ad->ad_hf.adf_off += cc;
    }
    return( cc );
}
