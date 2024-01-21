#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdio.h>
#include <strings.h>
#include <syslog.h>
#include <netatalk/endian.h>
#include "megatron.h"

#define	DEBUG		0

char		forkbuf[8192];
char		*forkname[] = { "data", "resource" };
char		*name[] = { "unhex",
			    "unbin",
			    "unsingle",
			    "macbinary",
			    "hqx2bin",
			    "single2bin",
			    "megatron" };

main( argc, argv )
    int		argc;
    char	**argv;
{
    int		rc, c;
    int		rv = 0;
    int		converts = sizeof(name) / sizeof(char *);
    int		module = -1;
    char	*progname;

    progname = rindex( argv[ 0 ], '/' );
    if (( progname == NULL ) || ( progname == '\0' )) {
	progname = argv[ 0 ];
    } else progname++;

#if DEBUG
    if ( CONVERTS != converts ) {
	fprintf( stderr, "megatron: list of program links messed up\n" );
	return( -1 );
    }
#endif

    for ( c = 0 ; (( c < converts ) && ( module < 0 )) ; ++c ) {
	if ( strcmp( name[ c ], progname ) == 0 ) module = c;
    }
    if ( module == -1 ) module = ( converts - 1 );

    if ( argc == 1 ) {
	return( megatron( STDIN, module ));
    }

    for ( c = 1 ; c < argc ; ++c ) {
	if ( rc = megatron( argv[ c ], module ) != 0 ) {
	    rv = rc;
	}
    }
    return( rv );
}

megatron( path, module )
    char	*path;
    int		module;
{
    struct stat		st;
    struct FHeader	fh;
    int			bufc;
    int			fork;
    int			forkred;

/*
 * If the source file is not stdin, make sure it exists and
 * that it is not a directory.
 */

    if ( strcmp( path, STDIN ) != 0 ) {
	if ( stat( path, &st ) < 0 ) {
	    perror( path );
	    return( -1 );
	}
	if ( S_ISDIR( st.st_mode )) {
	    fprintf( stderr, "%s is a directory.\n", path );
	    return( 0 );
	}
    }

/*
 * Open the source file and fill in the file header structure.
 */

    bzero( (char *)&fh, sizeof( fh ));
    if ( from_open( module, path, &fh ) < 0 ) {
	return( -1 );
    }

/*
 * Open the target file and write out the file header info.
 */

    if ( to_open( module, path, &fh ) < 0 ) {
	(void)from_close( module );
	return( -1 );
    }

/*
 * Read in and write out the data and resource forks.
 */

    for ( fork = 0; fork < NUMFORKS ; fork++ ) {
	forkred = 0;
	while(( bufc = from_read( module, fork, forkbuf, sizeof( forkbuf )))
		> 0 ) {
	    if ( to_write( module, fork, bufc ) != bufc ) {
		fprintf( stderr, "%s: Probable write error\n", path );
		to_close( module, TRASH );
		(void)from_close( module );
		return( -1 );
	    }
	    forkred += bufc;
	}
#if DEBUG
	fprintf( stderr, "megatron: forkred is \t\t%d\n", forkred );
	fprintf( stderr, "megatron: fh.forklen[%d] is \t%d\n", fork, 
		ntohl( fh.forklen[ fork ] ));
#endif
	if (( bufc < 0 ) || ( forkred != ntohl( fh.forklen[ fork ] ))) {
	    fprintf( stderr, "%s: Problem with input, dude\n", path );
	    to_close( module, TRASH );
	    (void)from_close( module );
	    return( -1 );
	}
    }

/*
 * Close up the files, and get out of here.
 */

    if ( to_close( module, KEEP ) < 0 ) {
	perror( "megatron:" );
	(void)to_close( module, TRASH );
    }
    return( from_close( module ));
}

from_open( un, file, fh )
    int			un;
    char		*file;
    struct FHeader	*fh;
{
    switch ( un ) {
	case MEGATRON :
	case HEX2NAD :
	case HEX2BIN :
	    return( hqx_open( file, O_RDONLY, fh ));
	    break;
	case BIN2NAD :
	    return( bin_open( file, O_RDONLY, fh ));
	    break;
	case NAD2BIN :
	    return( nad_open( file, O_RDONLY, fh ));
	    break;
	case SINGLE2NAD :
	case SINGLE2BIN :
	    return( single_open( file, O_RDONLY, fh ));
	default :
	    return( -1 );
	    break;
    }
}

from_read( un, fork, buf, len )
    int			un;
    int			fork;
    char		*buf;
    int			len;
{
    switch ( un ) {
	case MEGATRON :
	case HEX2NAD :
	case HEX2BIN :
	    return( hqx_read( fork, buf, len ));
	    break;
	case BIN2NAD :
	    return( bin_read( fork, buf, len ));
	    break;
	case NAD2BIN :
	    return( nad_read( fork, buf, len ));
	    break;
	case SINGLE2NAD :
	case SINGLE2BIN :
	    return( single_read( fork, buf, len ));
	default :
	    return( -1 );
	    break;
    }
}

from_close( un )
    int			un;
{
    switch ( un ) {
	case MEGATRON :
	case HEX2NAD :
	case HEX2BIN :
	    return( hqx_close( KEEP ));
	    break;
	case BIN2NAD :
	    return( bin_close( KEEP ));
	    break;
	case NAD2BIN :
	    return( nad_close( KEEP ));
	    break;
	case SINGLE2NAD :
	case SINGLE2BIN :
	    return( single_close( KEEP ));
	default :
	    return( -1 );
	    break;
    }
}

to_open( to, file, fh )
    int			to;
    char		*file;
    struct FHeader	*fh;
{
    switch ( to ) {
	case MEGATRON :
	case HEX2NAD :
	case BIN2NAD :
	case SINGLE2NAD :
	    return( nad_open( file, O_RDWR|O_CREAT|O_EXCL, fh ));
	    break;
	case NAD2BIN :
	case HEX2BIN :
	case SINGLE2BIN :
	    return( bin_open( file, O_RDWR|O_CREAT|O_EXCL, fh ));
	    break;
	default :
	    return( -1 );
	    break;
    }
}

to_write( to, fork, bufc )
    int			to;
    int			fork;
    int			bufc;
{
    switch ( to ) {
	case MEGATRON :
	case HEX2NAD :
	case BIN2NAD :
	case SINGLE2NAD :
	    return( nad_write( fork, forkbuf, bufc ));
	    break;
	case NAD2BIN :
	case HEX2BIN :
	case SINGLE2BIN :
	    return( bin_write( fork, forkbuf, bufc ));
	    break;
	default :
	    return( -1 );
	    break;
    }
}

to_close( to, keepflag )
    int			to;
    int			keepflag;
{
    switch ( to ) {
	case MEGATRON :
	case HEX2NAD :
	case BIN2NAD :
	case SINGLE2NAD :
	    return( nad_close( keepflag ));
	    break;
	case NAD2BIN :
	case HEX2BIN :
	case SINGLE2BIN :
	    return( bin_close( keepflag ));
	    break;
	default :
	    return( -1 );
	    break;
    }
}
