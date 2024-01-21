/*
 * Copyright (c) 1990,1995 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/file.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <unistd.h>
#include <atalk/adouble.h>

ad_write( ad, eid, off, end, buf, buflen )
    struct adouble	*ad;
    int			eid, off, end, buflen;
    char		*buf;
{
    struct stat		st;
    int			cc;

    if ( eid == ADEID_DFORK ) {
	if ( end ) {
	    if ( fstat( ad->ad_df.adf_fd, &st ) < 0 ) {
		return( -1 );
	    }
	    off = st.st_size - off;
	}
	if ( ad->ad_df.adf_off != off ) {
	    if ( lseek( ad->ad_df.adf_fd, off, L_SET ) < 0 ) {
		return( -1 );
	    }
	    ad->ad_df.adf_off = off;
	}
	cc = write( ad->ad_df.adf_fd, buf, buflen );
	if ( cc < 0 ) {
	    return( -1 );
	}
	ad->ad_df.adf_off += cc;
    } else {
	if ( end ) {
	    off = ad->ad_eid[ eid ].ade_len - off;
	}
	if ( ad->ad_hf.adf_off != ad->ad_eid[ eid ].ade_off + off ) {
	    if ( lseek( ad->ad_hf.adf_fd,
		    ad->ad_eid[ eid ].ade_off + off, L_SET ) < 0 ) {
		return( -1 );
	    }
	    ad->ad_hf.adf_off = ad->ad_eid[ eid ].ade_off + off;
	}
	cc = write( ad->ad_hf.adf_fd, buf, buflen );
	if ( cc < 0 ) {
	    return( -1 );
	}

	if ( ad->ad_hf.adf_off < sizeof( ad->ad_data )) {
#define min(a,b)	((a)<(b)?(a):(b))
	    bcopy( buf, ad->ad_data + ad->ad_hf.adf_off,
		    min( sizeof( ad->ad_data ) - ad->ad_hf.adf_off, cc ));
	}

	ad->ad_hf.adf_off += cc;
	if ( ad->ad_eid[ eid ].ade_len < off + cc ) {
	    ad->ad_eid[ eid ].ade_len = off + cc;
	}
    }

    return( cc );
}

ad_rtruncate( ad, size )
    struct adouble	*ad;
    off_t		size;
{
    if ( ftruncate( ad->ad_hf.adf_fd,
	    size + ad->ad_eid[ ADEID_RFORK ].ade_off ) < 0 ) {
	return( -1 );
    }
    ad->ad_eid[ ADEID_RFORK ].ade_len = size;
    if ( lseek( ad->ad_hf.adf_fd, 0L, L_SET ) < 0 ) {
	return( -1 );
    }
    ad->ad_hf.adf_off = 0;
    return( 0 );
}
