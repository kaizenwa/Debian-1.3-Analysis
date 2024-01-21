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

#define BPLEN	48
#include <stdio.h>
#include <ctype.h>
char	hexdig[] = "0123456789abcdef";

bprint( data, len )
    char	*data;
    int		len;
{
    char	out[ BPLEN ];
    int		i = 0;

    bzero( out, BPLEN );
    for ( ;; ) {
	if ( len < 1 ) {
	    printf( "\t%s\n", ( i == 0 ) ? "(end)" : out );
	    break;
	}

	if ( isgraph( (unsigned char)*data )) {
	    out[ i ] = ' ';
	    out[ i+1 ] = *data;
	} else {
	    out[ i ] = hexdig[ ( *data & 0xf0 ) >> 4 ];
	    out[ i+1 ] = hexdig[ *data & 0x0f ];
	}
	i += 2;
	len--;
	data++;

	if ( i > BPLEN - 2 ) {
	    printf( "\t%s\n", out );
	    bzero( out, BPLEN );
	    i = 0;
	    continue;
	}
	out[ i++ ] = ' ';
    }
}
