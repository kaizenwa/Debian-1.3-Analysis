#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <sys/param.h>
#include <fcntl.h>
#include <unistd.h>
#include <strings.h>
#include <syslog.h>
#include <ctype.h>
#include <stdio.h>
#include <atalk/adouble.h>
#include <netatalk/endian.h>
#include "megatron.h"

#define	DEBUG		0

/*	String used to indicate standard input instead of a disk
	file.  Should be a string not normally used for a file
 */
#ifndef	STDIN
#	define	STDIN	"-"
#endif

/*	Yes and no
 */
#define NOWAY		0
#define SURETHANG	1

/*	Size of a macbinary file header
 */
#define HEADBUFSIZ	128

u_short		updcrc();

/*	Both input and output routines use this struct and the
	following globals; therefore this module can only be used
	for one of the two functions at a time.
 */
struct bin_file_data {
    long		forklen[ NUMFORKS ];
    char		path[ MAXPATHLEN ];
    int			filed;
    u_short		headercrc;
} 		bin;

extern char	*forkname[];
u_char		head_buf[HEADBUFSIZ];

/* 
 * bin_open must be called first.  pass it a filename that is supposed
 * to contain a macbinary file.  an bin struct will be allocated and
 * somewhat initialized; bin_filed is set.
 */

bin_open( binfile, flags, fh )
    char		*binfile;
    int			flags;
    struct FHeader	*fh;
{
    int			maxlen;
    int			rc;

#if DEBUG
    fprintf( stderr, "entering bin_open\n" );
#endif

    if ( flags == O_RDONLY ) {
	if ( strcmp( binfile, STDIN ) == 0 ) {
	    bin.filed = fileno( stdin );
	} else if (( bin.filed = open( binfile, flags )) < 0 ) {
	    perror( binfile );
	    return( -1 );
	}
#if DEBUG
	fprintf( stderr, "opened %s for read\n", binfile );
#endif
	if ((( rc = test_header() ) > 0 ) && 
		( bin_header_read( fh, rc ) == 0 )) {
	    return( 0 );
	}
	bin_close( TRASH );
	fprintf( stderr, "%s\n", binfile );
	return( -1 );
    } else {
	maxlen = sizeof( bin.path ) - 1;
#if DEBUG
	fprintf( stderr, "sizeof bin.path\t\t\t%d\n", sizeof( bin.path ));
	fprintf( stderr, "maxlen \t\t\t\t%d\n", maxlen );
#endif
	strncpy( bin.path, fh->name, maxlen );
	strncpy( bin.path, mtoupath( bin.path ), maxlen );
	strncat( bin.path, ".bin", maxlen - strlen( bin.path ));
	if (( bin.filed = open( bin.path, flags, 0666 )) < 0 ) {
	    perror( bin.path );
	    return( -1 );
	}
#if DEBUG
	fprintf( stderr, "opened %s for write\n", bin.path );
#endif
	if ( bin_header_write( fh ) != 0 ) {
	    bin_close( TRASH );
	    fprintf( stderr, "%s\n", bin.path );
	    return( -1 );
	}
	return( 0 );
    }
}

/* 
 * bin_close must be called before a second file can be opened using
 * bin_open.  Upon successful completion, a value of 0 is returned.  
 * Otherwise, a value of -1 is returned.
 */

bin_close( keepflag )
    int			keepflag;
{
#if DEBUG
    fprintf( stderr, "entering bin_close\n" );
#endif
    if ( keepflag == KEEP ) {
	return( close( bin.filed ));
    } else if ( keepflag == TRASH ) {
	if (( strcmp( bin.path, STDIN ) != 0 ) && 
		( unlink( bin.path ) < 0 )) {
	    perror ( bin.path );
	}
	return( 0 );
    } else return( -1 );
}

/*
 * bin_read is called until it returns zero for each fork.  when it is
 * and finds that there is zero left to give, it seeks to the position
 * of the next fork (if there is one ).
 * bin_read must be called enough times to
 * return zero and no more than that.
 */

bin_read( fork, buffer, length )
    int			fork;
    char		*buffer;
    int			length;
{
    char		*buf_ptr;
    int			readlen;
    int			cc = 1;
    off_t		pos;

#if DEBUG >= 3
    fprintf( stderr, "bin_read: fork is %s\n", forkname[ fork ] );
    fprintf( stderr, "bin_read: remaining length is %d\n", bin.forklen[fork] );
#endif

    if ( bin.forklen[ fork ] < 0 ) {
	fprintf( stderr, "This should never happen, dude!\n" );
	return( bin.forklen[ fork ] );
    }

    if ( bin.forklen[ fork ] == 0 ) {
	if ( fork == DATA ) {
	    pos = lseek( bin.filed, 0, SEEK_CUR );
#if DEBUG
	    fprintf( stderr, "current position is %ld\n", pos );
#endif
	    pos = pos % HEADBUFSIZ;
	    pos = pos ? HEADBUFSIZ - pos : pos;
	    pos = lseek( bin.filed, pos, SEEK_CUR );
#if DEBUG
	    fprintf( stderr, "current position is %ld\n", pos );
#endif
	}
	return( 0 );
    }

    if ( bin.forklen[ fork ] < length ) {
	readlen = bin.forklen[ fork ];
    } else {
	readlen = length;
    }
#if DEBUG >= 3
    fprintf( stderr, "bin_read: readlen is %d\n", readlen );
    fprintf( stderr, "bin_read: cc is %d\n", cc );
#endif

    buf_ptr = buffer;
    while (( readlen > 0 ) && ( cc > 0 )) {
	if (( cc = read( bin.filed, buf_ptr, readlen )) > 0 ) {
#if DEBUG >= 3
	    fprintf( stderr, "bin_read: cc is %d\n", cc );
#endif
	    readlen -= cc;
	    buf_ptr += cc;
	}
    }
    if ( cc >= 0 ) {
	cc = buf_ptr - buffer;
	bin.forklen[ fork ] -= cc;
    }

#if DEBUG >= 3
    fprintf( stderr, "bin_read: chars read is %d\n", cc );
#endif
    return( cc );
}

/*
 * bin_write 
 */

bin_write( fork, buffer, length )
    int			fork;
    char		*buffer;
    int			length;
{
    char		*buf_ptr;
    int			writelen;
    int			cc = 0;
    off_t		pos;
    u_char		padchar = 0;

#if DEBUG >= 3
    fprintf( stderr, "bin_write: fork is %s\n", forkname[ fork ] );
    fprintf( stderr, "bin_write: remaining length is %d\n", bin.forklen[fork] );
#endif

    if (( fork == RESOURCE ) && ( bin.forklen[ DATA ] != 0 )) {
	fprintf( stderr, "Forklength error.\n" );
	return( -1 );
    }

    buf_ptr = (char *)buffer;
    if ( bin.forklen[ fork ] >= length ) {
	writelen = length;
    } else {
	fprintf( stderr, "Forklength error.\n" );
	return( -1 );
    }

#if DEBUG >= 3
    fprintf( stderr, "bin_write: write length is %d\n", writelen );
#endif

    while (( writelen > 0 ) && ( cc >= 0 )) {
	cc = write( bin.filed, buf_ptr, writelen );
	buf_ptr += cc;
	writelen -= cc;
    }
    if ( cc < 0 ) {
	perror( "Couldn't write to macbinary file:" );
	return( cc );
    }
    bin.forklen[ fork ] -= length;

    if ( bin.forklen[ fork ] < 0 ) {
	fprintf( stderr, "This should never happen, dude!\n" );
	return( bin.forklen[ fork ] );
    }

/*
 * add the padding at end of data and resource forks
 */

    if ( bin.forklen[ fork ] == 0 ) {
	pos = lseek( bin.filed, 0, SEEK_CUR );
#if DEBUG
	fprintf( stderr, "current position is %ld\n", pos );
#endif
	pos = pos % HEADBUFSIZ;
	pos = pos ? HEADBUFSIZ - pos : pos;
	pos = lseek( bin.filed, pos - 1, SEEK_CUR );
	if ( write( bin.filed, &padchar, 1 ) != 1 ) {
	    perror( "Couldn't write to macbinary file:" );
	    return( -1 );
	}
#if DEBUG
	fprintf( stderr, "current position is %ld\n", pos );
#endif
    }

#if DEBUG
	fprintf( stderr, "\n" );
#endif

    return( length );
}

/* 
 * bin_header_read is called by bin_open, and before any information can
 * read from the fh substruct.  it must be called before any
 * of the bytes of the other two forks can be read, as well.
 */

bin_header_read( fh, revision )
    struct FHeader	*fh;
    int			revision;
{
    u_short		mask;

/*
 * Set the appropriate finder flags mask for the type of macbinary
 * file it is, and copy the extra macbinary II stuff from the header.
 * If it is not a macbinary file revision of I or II, then return
 * negative.
 */

    switch ( revision ) {
	case 2 :
	    mask = htons( 0xfcee );
	    bcopy( (char *)&head_buf[ 101 ], 
		    ((char *)&fh->finder_info.fdFlags) + 1, 1 );
	    break;
	case 1 :
	    mask = htons( 0xfc00 );
	    break;
	default :
	    return( -1 );
	    break;
    }

/*
 * Go through and copy all the stuff you can get from the 
 * MacBinary header into the fh struct.  What fun!
 */

    bcopy( (char *)&head_buf[ 2 ], (char *)fh->name, head_buf[ 1 ] );
    bcopy( (char *)&head_buf[ 91 ], (char *)&fh->create_date, 4 );
    bcopy( (char *)&head_buf[ 95 ], (char *)&fh->mod_date, 4 );
    fh->backup_date = 0;
    bcopy( (char *)&head_buf[ 65 ], (char *)&fh->finder_info, 8 );
    bcopy( (char *)&head_buf[ 73 ], (char *)&fh->finder_info.fdFlags, 1 );
    fh->finder_info.fdFlags = fh->finder_info.fdFlags & mask;
    bcopy( (char *)&head_buf[ 75 ], (char *)&fh->finder_info.fdLocation, 4 );
    bcopy( (char *)&head_buf[ 79 ], (char *)&fh->finder_info.fdFldr, 2 );
    bcopy( (char *)&head_buf[ 83 ], (char *)&fh->forklen[ DATA ], 4 );
    bin.forklen[ DATA ] = ntohl( fh->forklen[ DATA ] );
    bcopy( (char *)&head_buf[ 87 ], (char *)&fh->forklen[ RESOURCE ], 4 );
    bin.forklen[ RESOURCE ] = ntohl( fh->forklen[ RESOURCE ] );
    fh->comment[0] = '\0';

#if DEBUG >= 5
    {
	short		flags;

	fprintf( stderr, "Values read by bin_header_read\n" );
	fprintf( stderr, "name length\t\t%d\n", head_buf[ 1 ] );
	fprintf( stderr, "file name\t\t%s\n", fh->name );
	fprintf( stderr, "get info comment\t%s\n", fh->comment );
	fprintf( stderr, "type\t\t\t%.*s\n", sizeof( fh->finder_info.fdType ),
		&fh->finder_info.fdType );
	fprintf( stderr, "creator\t\t\t%.*s\n", 
		sizeof( fh->finder_info.fdCreator ), 
		&fh->finder_info.fdCreator );
	bcopy( (char *)&fh->finder_info.fdFlags, (char *)&flags, 
		sizeof( flags ));
	flags = ntohs( flags );
	fprintf( stderr, "flags\t\t\t%x\n", flags );
	fprintf( stderr, "data fork length\t%ld\n", bin.forklen[DATA] );
	fprintf( stderr, "resource fork length\t%ld\n", bin.forklen[RESOURCE] );
	fprintf( stderr, "\n" );
    }
#endif

    return( 0 );
}

/* 
 * bin_header_write is called by bin_open, and relies on information
 * from the fh substruct.  it must be called before any
 * of the bytes of the other two forks can be written, as well.
 * bin_header_write and bin_header_read are opposites.
 */

bin_header_write( fh )
    struct FHeader	*fh;
{
    char		*write_ptr;
    int			wc;
    int			wr;

    bzero( (char *)head_buf, sizeof( head_buf ));
    head_buf[ 1 ] = (u_char)strlen( fh->name );
    bcopy( (char *)fh->name, (char *)&head_buf[ 2 ], head_buf[ 1 ] );
    bcopy( (char *)&fh->finder_info, (char *)&head_buf[ 65 ], 8 );
    bcopy( (char *)&fh->finder_info.fdFlags, (char *)&head_buf[ 73 ], 1 );
    bcopy( (char *)&fh->finder_info.fdLocation, (char *)&head_buf[ 75 ], 4 );
    bcopy( (char *)&fh->finder_info.fdFldr, (char *)&head_buf[ 79 ], 2 );
    bcopy( (char *)&fh->forklen[ DATA ], (char *)&head_buf[ 83 ], 4 );
    bcopy( (char *)&fh->forklen[ RESOURCE ], (char *)&head_buf[ 87 ], 4 );
    bcopy( (char *)&fh->create_date, (char *)&head_buf[ 91 ], 4 );
    bcopy( (char *)&fh->mod_date, (char *)&head_buf[ 95 ], 4 );
    bcopy( ((char *)&fh->finder_info.fdFlags) + 1,
	    (char *)&head_buf[ 101 ], 1 );
    head_buf[ 122 ] = 129;
    head_buf[ 123 ] = 129;

    bin.headercrc = htons( updcrc( (u_short) 0, head_buf, 124 ));
    bcopy( (char *)&bin.headercrc, (char *)&head_buf[ 124 ], 
	    sizeof( bin.headercrc ));

    bin.forklen[ DATA ] = ntohl( fh->forklen[ DATA ] );
    bin.forklen[ RESOURCE ] = ntohl( fh->forklen[ RESOURCE ] );

#if DEBUG >= 5
    {
	fprintf( stderr, "Values written by bin_header_write\n" );
	fprintf( stderr, "name length\t\t%d\n", head_buf[ 1 ] );
	fprintf( stderr, "file name\t\t%s\n", (char *)&head_buf[ 2 ] );
	fprintf( stderr, "type\t\t\t%.4s\n", (char *)&head_buf[ 65 ] );
	fprintf( stderr, "creator\t\t\t%.4s\n", (char *)&head_buf[ 69 ] );
	fprintf( stderr, "data fork length\t%ld\n", bin.forklen[DATA] );
	fprintf( stderr, "resource fork length\t%ld\n", bin.forklen[RESOURCE] );
	fprintf( stderr, "\n" );
    }
#endif

    write_ptr = (char *)head_buf;
    wc = sizeof( head_buf );
    wr = 0;
    while (( wc > 0 ) && ( wr >= 0 )) {
	wr = write( bin.filed, write_ptr, wc );
	write_ptr += wr;
	wc -= wr;
    }
    if ( wr < 0 ) {
	perror( "Couldn't write macbinary header:" );
	return( wr );
    }

    return( 0 );
}

/*
 * test_header is called from bin_open.  it checks certain values of the
 * first 128 bytes, determines if the file is a MacBinary, MacBinary II,
 * or non-MacBinary file, and returns a one, two, or negative one to 
 * indicate the file type.
 *
 * Bytes 0 and 74 must be zero for the file to be any type of MacBinary.
 * If the crc of bytes 0 through 123 equals the value at offset 124 then
 * it is a MacBinary II.  If not, then if byte 82 is zero, byte 2 is
 * a valid value for a mac filename length (between one and sixty-three),
 * and bytes 101 through 125 are all zero, then the file is a MacBinary.
 */

test_header()
{
    int			cc;
    u_short		header_crc;
    u_char		namelen;

#if DEBUG
    fprintf( stderr, "entering test_header\n" );
#endif

    cc = read( bin.filed, (char *)head_buf, sizeof( head_buf ));
    if ( cc < sizeof( head_buf )) {
	perror( "Premature end of file :" );
	return( -1 );
    }

#if DEBUG
    fprintf( stderr, "was able to read HEADBUFSIZ bytes\n" );
#endif

    if (( head_buf[ 0 ] != 0 ) || ( head_buf[ 74 ] != 0 )) {
	return( -1 );
    }

#if DEBUG
    fprintf( stderr, "byte 0 and 74 are both zero\n" );
#endif

    bin.headercrc = updcrc( (u_short) 0, head_buf, 124 );
    bcopy( &head_buf[ 124 ], (char *)&header_crc, sizeof( header_crc ));
    header_crc = ntohs( header_crc );
    if ( header_crc == bin.headercrc ) {
	return( 2 );
    }

#if DEBUG
    fprintf( stderr, "header crc didn't pan out\n" );
#endif

    if ( head_buf[ 82 ] != 0 ) {
	return( -1 );
    }
    bcopy( &head_buf[ 1 ], (char *)&namelen, sizeof( namelen ));
#if DEBUG
    fprintf( stderr, "name length is %d\n", namelen );
#endif
    if (( namelen < 1 ) || ( namelen > 63 )) {
	return( -1 );
    }

#if DEBUG
    fprintf( stderr, "byte 82 is zero and name length is cool\n" );
#endif

    return( 1 );
}
