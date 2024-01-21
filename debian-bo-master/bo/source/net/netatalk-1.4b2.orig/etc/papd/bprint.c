#define BPXLEN	50
#define BPALEN	18
#include <ctype.h>
#include <syslog.h>
char	hexdig[] = "0123456789ABCDEF";

bprint( data, len )
    char	*data;
    int		len;
{
    char	xout[ BPXLEN ], aout[ BPALEN ];
    int		i = 0;

    bzero( xout, BPXLEN );
    bzero( aout, BPALEN );

    for ( i = 0; len; len--, *data++, i++ ) {
	if ( i == 16 ) {
	    syslog( LOG_DEBUG, "%-48s\t%-16s", xout, aout );
	    bzero( xout, BPXLEN );
	    bzero( aout, BPALEN );
	    i = 0;
	}

	if ( isascii( (unsigned char)*data ) &&
		isprint( (unsigned char)*data )) {
	    aout[ i ] = *data;
	} else {
	    aout[ i ] = '.';
	}

	xout[ (i*3) ] = hexdig[ ( *data & 0xf0 ) >> 4 ];
	xout[ (i*3) + 1 ] = hexdig[ *data & 0x0f ];
	xout[ (i*3) + 2 ] = ' ';
    }

    if ( i ) {
	syslog( LOG_DEBUG, "%-48s\t%-16s", xout, aout );
    }
    syslog( LOG_DEBUG, "%s", "(end)" );

    return;
}
