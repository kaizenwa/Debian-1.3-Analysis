#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <sys/param.h>
#include <fcntl.h>
#include <strings.h>
#include <syslog.h>
#include <ctype.h>
#include <stdio.h>
#include <unistd.h>
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

/*	AppleSingle Entry IDs.
 */
#define ENTRYID_DATA		1
#define ENTRYID_RESOURCE	2
#define ENTRYID_NAME		3
#define ENTRYID_COMMENT		4
#define ENTRYID_ICN		5
#define ENTRYID_CICN		6
#define ENTRYID_FILEINFO	7
#define ENTRYID_DATES		8
#define ENTRYID_FINDERINFO	9
#define ENTRYID_MACFINFO	10
#define ENTRYID_PRODOSFINFO	11
#define ENTRYID_MSDOSFINFO	12
#define ENTRYID_AFPSHORTNAME	13
#define ENTRYID_AFPFINFO	14
#define ENTRYID_AFPDIRID	15
#define ENTRYID_MAX		16

#define ENTRY_INFO_SIZE	12

#define HEADERSIZ	26

/*	This structure holds an entry description, consisting of three
	four byte entities.  The first is the Entry ID, the second is
	the File Offset and the third is the Length.
 */

struct entry_info {
    long		offset;
    long		length;
};

/*	Both input and output routines use this struct and the
	following globals; therefore this module can only be used
	for one of the two functions at a time.
 */
struct single_file_data {
    int			filed;
    char		path[ MAXPATHLEN ];
    struct entry_info	entry[ ENTRYID_MAX ];
} 		single;

extern char	*forkname[];
u_char		header_buf[ HEADERSIZ ];

/* 
 * single_open must be called first.  pass it a filename that is supposed
 * to contain a AppleSingle file.  an single struct will be allocated and
 * somewhat initialized; single_filed is set.
 */

single_open( singlefile, flags, fh )
    char		*singlefile;
    int			flags;
    struct FHeader	*fh;
{
    int			maxlen;
    int			rc;

    if ( flags == O_RDONLY ) {
	if ( strcmp( singlefile, STDIN ) == 0 ) {
	    single.filed = fileno( stdin );
	} else if (( single.filed = open( singlefile, flags )) < 0 ) {
	    perror( singlefile );
	    return( -1 );
	}
	strncpy( single.path, singlefile, MAXPATHLEN );
#if DEBUG
	fprintf( stderr, "opened %s for read\n", single.path );
#endif
	if ((( rc = single_header_test()) > 0 ) && 
		( single_header_read( fh, rc ) == 0 )) {
	    return( 0 );
	}
	single_close( KEEP );
	return( -1 );
    }
}

/* 
 * single_close must be called before a second file can be opened using
 * single_open.  Upon successful completion, a value of 0 is returned.  
 * Otherwise, a value of -1 is returned.
 */

single_close( keepflag )
    int			keepflag;
{
    if ( keepflag == KEEP ) {
	return( close( single.filed ));
    } else if ( keepflag == TRASH ) {
	if (( strcmp( single.path, STDIN ) != 0 ) && 
		( unlink( single.path ) < 0 )) {
	    perror ( single.path );
	}
	return( 0 );
    } else return( -1 );
}

/* 
 * single_header_read is called by single_open, and before any information
 * can read from the fh substruct.  it must be called before any of the
 * bytes of the other two forks can be read, as well.
 */

single_header_read( fh, version )
    struct FHeader	*fh;
    int			version;
{
/*
 * entry_buf is used for reading in entry descriptors, and for reading in
 * 	the actual entries of FILEINFO, FINDERINFO, and DATES.
 */
    u_char		entry_buf[ 16 ];
    u_long		entry_id;
    u_long		time_seconds;
    u_short		mask = 0xfcee;
    u_short		num_entries;
    int			n;
    int			readlen;
    int			date_entry;
    off_t		pos;

/*
 * Go through and initialize the array of entry_info structs.  Read in the
 * number of entries, and then read in the info for each entry and save it
 * in the array.
 */

    for ( n = 0 ; n < ENTRYID_MAX; n++ ) {
	single.entry[ n ].offset = 0;
	single.entry[ n ].length = 0;
    }
    bcopy( (char *)&header_buf[ 24 ], (char *)&num_entries,
	    sizeof( num_entries ));
    num_entries = ntohs( num_entries );
#if DEBUG >= 2
    fprintf( stderr, "The number of entries is %d\n", num_entries );
#endif
    for ( ; num_entries > 0 ; num_entries-- ) {
	if ( read( single.filed, (char *)entry_buf, ENTRY_INFO_SIZE )
		!= ENTRY_INFO_SIZE ) {
	    perror( "Premature end of file :" );
	    return( -1 );
	}
	bcopy( (char *)entry_buf, (char *)&entry_id, sizeof( entry_id ));
	entry_id = ntohl( entry_id );
	bcopy( (char *)&entry_buf[ 4 ],
		(char *)&single.entry[ entry_id ].offset,
		sizeof( single.entry[ entry_id ].offset ));
	single.entry[ entry_id ].offset =
		ntohl( single.entry[ entry_id ].offset );
	bcopy( (char *)&entry_buf[ 8 ],
		(char *)&single.entry[ entry_id ].length,
		sizeof( single.entry[ entry_id ].length ));
	single.entry[ entry_id ].length =
		ntohl( single.entry[ entry_id ].length );
#if DEBUG >= 2
	fprintf( stderr, "entry_id\t%d\n", entry_id );
	fprintf( stderr, "\toffset\t\t%d\n", single.entry[ entry_id ].offset );
	fprintf( stderr, "\tlength\t\t%d\n", single.entry[ entry_id ].length );
#endif
    }

/*
 * Now that the entries have been identified, check to make sure
 * it is a Macintosh file if dealing with version two format file.
 */

    if ( version == 1 ) {
	if ( single.entry[ ENTRYID_FILEINFO ].length > 0 ) {
	    date_entry = ENTRYID_FILEINFO;
	} else {
	    date_entry = 0;
	}
    } else if ( version == 2 ) {
	if ( single.entry[ ENTRYID_DATES ].length > 0 ) {
	    date_entry = ENTRYID_DATES;
	} else {
	    date_entry = 0;
	}
    }
#if DEBUG
    fprintf( stderr, "date_entry = %d\n", date_entry );
#endif

/*
 * Go through and copy all the information you can get from 
 * the informational entries into the fh struct.  The ENTRYID_DATA
 * must be the last one done, because it leaves the file pointer in
 * the right place for the first read of the data fork.
 */
 
    if ( single.entry[ ENTRYID_NAME ].offset == 0 ) {
	fprintf( stderr, "%s has no name for the mac file.\n", single.path );
	return( -1 );
    } else {
	pos = lseek( single.filed, single.entry[ ENTRYID_NAME ].offset,
		SEEK_SET );
	readlen = single.entry[ ENTRYID_NAME ].length > NAMESIZ ? NAMESIZ
		: single.entry[ ENTRYID_NAME ].length;
	if ( read( single.filed, (char *)fh->name, readlen ) != readlen ) {
	    perror( "Premature end of file :" );
	    return( -1 );
	}
    }
    if (( single.entry[ ENTRYID_FINDERINFO ].length < 16 ) ||
	    ( single.entry[ ENTRYID_FINDERINFO ].offset <= HEADERSIZ )) {
	fprintf( stderr, "%s has bogus FinderInfo.\n", single.path );
	return( -1 );
    } else {
	pos = lseek( single.filed,
		single.entry[ ENTRYID_FINDERINFO ].offset, SEEK_SET );
	if ( read( single.filed, (char *)entry_buf, sizeof( entry_buf )) !=
		sizeof( entry_buf )) {
	    perror( "Premature end of file :" );
	    return( -1 );
	}
	bcopy( (char *)&entry_buf[ FINDERIOFF_TYPE ],
		(char *)&fh->finder_info.fdType,
		sizeof( fh->finder_info.fdType ));
	bcopy( (char *)&entry_buf[ FINDERIOFF_CREATOR ],
		(char *)&fh->finder_info.fdCreator,
		sizeof( fh->finder_info.fdCreator ));
	bcopy( (char *)&entry_buf[ FINDERIOFF_FLAGS ],
		(char *)&fh->finder_info.fdFlags,
		sizeof( fh->finder_info.fdFlags ));
	fh->finder_info.fdFlags = fh->finder_info.fdFlags & mask;
	bcopy( (char *)&entry_buf[ FINDERIOFF_LOC ],
		(char *)&fh->finder_info.fdLocation,
		sizeof( fh->finder_info.fdLocation ));
	bcopy( (char *)&entry_buf[ FINDERIOFF_FLDR ],
		(char *)&fh->finder_info.fdFldr,
		sizeof( fh->finder_info.fdFldr ));
#if DEBUG
	{
	    char		type[5];
	    char		creator[5];
 
	    strncpy( type, &fh->finder_info.fdType, 4 );
	    strncpy( creator, &fh->finder_info.fdCreator, 4 );
	    type[5] = creator[5] = '\0';
	    fprintf( stderr, "type is %s, creator is %s\n", type, creator );
	}
#endif
    }
    if (( single.entry[ ENTRYID_COMMENT ].length == 0 ) || 
	    ( single.entry[ ENTRYID_COMMENT ].offset <= HEADERSIZ )) {
	fh->comment[0] = '\0';
    } else {
	pos = lseek( single.filed, single.entry[ ENTRYID_COMMENT ].offset,
		SEEK_SET );
	readlen = single.entry[ ENTRYID_COMMENT ].length > COMMENTSIZ 
		? COMMENTSIZ : single.entry[ ENTRYID_COMMENT ].length;
	if ( read( single.filed, (char *)fh->comment, readlen ) != readlen ) {
	    perror( "Premature end of file :" );
	    return( -1 );
	}
    }
/*
 * If date_entry is 7, we have an AppleSingle version one, do the 
 * appropriate stuff.  If it is 8, we have an AppleSingle version two,
 * do the right thing.  If date_entry is neither, just use the current date.
 * Unless I can't get the current date, in which case use time zero.
 */
    if (( date_entry < 7 ) || ( date_entry > 8 )) {
	if (( time_seconds = time( NULL )) == -1 ) {
	    time_seconds = htonl( TIME_ZERO );
	} else {
	    time_seconds = htonl( time_seconds + TIME_DIFF );
	}
	bcopy( (char *)&time_seconds, (char *)&fh->create_date,
		sizeof( fh->create_date ));
	bcopy( (char *)&time_seconds, (char *)&fh->mod_date,
		sizeof( fh->mod_date ));
	fh->backup_date = htonl( TIME_ZERO );
    } else if ( single.entry[ date_entry ].length != 16 ) {
	fprintf( stderr, "%s has bogus FileInfo or File Dates Info.\n", 
		single.path );
	return( -1 );
    } else if ( date_entry == ENTRYID_FILEINFO ) {
	pos = lseek( single.filed,
		single.entry[ date_entry ].offset, SEEK_SET );
	if ( read( single.filed, (char *)entry_buf, sizeof( entry_buf )) !=
		sizeof( entry_buf )) {
	    perror( "Premature end of file :" );
	    return( -1 );
	}
	bcopy( (char *)&entry_buf[ 0 ], (char *)&fh->create_date,
		sizeof( fh->create_date ));
	bcopy( (char *)&entry_buf[ 4 ], (char *)&fh->mod_date,
		sizeof( fh->mod_date ));
	fh->backup_date = htonl( TIME_ZERO );
    } else if ( date_entry == ENTRYID_DATES ) {
	time_seconds = (u_long)946684800L + (u_long)TIME_DIFF;
	pos = lseek( single.filed,
		single.entry[ date_entry ].offset, SEEK_SET );
	if ( read( single.filed, (char *)entry_buf, sizeof( entry_buf )) !=
		sizeof( entry_buf )) {
	    perror( "Premature end of file :" );
	    return( -1 );
	}
	bcopy( (char *)&entry_buf[ 0 ], (char *)&fh->create_date,
		sizeof( fh->create_date ));
	fh->create_date = htonl( time_seconds - 
		( 0L - ntohl( fh->create_date )));
	bcopy( (char *)&entry_buf[ 4 ], (char *)&fh->mod_date,
		sizeof( fh->mod_date ));
	fh->mod_date = htonl( time_seconds - ( 0L - ntohl( fh->mod_date )));
	fh->backup_date = htonl( TIME_ZERO );
    }
    if ( single.entry[ ENTRYID_RESOURCE ].offset == 0 ) {
	fh->forklen[ RESOURCE ] = 0;
    } else {
	fh->forklen[ RESOURCE ] =
		htonl( single.entry[ ENTRYID_RESOURCE ].length );
    }
    if ( single.entry[ ENTRYID_DATA ].offset == 0 ) {
	fh->forklen[ DATA ] = 0;
    } else {
	fh->forklen[ DATA ] = htonl( single.entry[ ENTRYID_DATA ].length );
	pos = lseek( single.filed, single.entry[ ENTRYID_DATA ].offset, SEEK_SET );
    }

    return( 0 );
}

/*
 * single_header_test is called from single_open.  It checks certain
 * values of the file and determines if the file is an AppleSingle version
 * one file something else, and returns a one, or negative one to indicate
 * file type.
 *
 * The Magic Number of the file, the first four bytes, must be hex
 * 0x00051600.  Bytes 4 through 7 are the version number and must be hex
 * 0x00010000.  Bytes 8 through 23 identify the home file system, and we
 * are only interested in files from Macs.  Therefore these bytes must
 * contain hex 0x4d6163696e746f736820202020202020 which is ASCII
 * "Macintosh       " (that is seven blanks of padding).
 */
#define ASMAGIC		0x00051600L
#define VERSIONONE	0x00010000L
#define VERSIONTWO	0x00020000L
#define MACINTOSH	"Macintosh       "
u_char		sixteennulls[] = { 0, 0, 0, 0, 0, 0, 0, 0,
				    0, 0, 0, 0, 0, 0, 0, 0 };

single_header_test()
{
    int			cc;
    u_long		templong;
    u_short		header_crc;
    u_char		namelen;

    cc = read( single.filed, (char *)header_buf, sizeof( header_buf ));
    if ( cc < sizeof( header_buf )) {
	perror( "Premature end of file :" );
	return( -1 );
    }

    bcopy( (char *)&header_buf[ 0 ], (char *)&templong, sizeof( templong ));
    if ( ntohl( templong ) != ASMAGIC ) {
	fprintf( stderr, "%s is not an AppleSingle file.\n", single.path );
	return( -1 );
    }

    bcopy( (char *)&header_buf[ 4 ], (char *)&templong, sizeof( templong ));
    templong = ntohl( templong );
    if ( templong == VERSIONONE ) {
	cc = 1;
	if ( bcmp( MACINTOSH, &header_buf[ 8 ], sizeof( MACINTOSH ) - 1 ) 
		!= 0 ) {
	    fprintf( stderr, "%s is not a Macintosh AppleSingle file.\n", 
		    single.path );
	    return( -1 );
	}
    } else if ( templong == VERSIONTWO ) {
	cc = 2;
	if ( bcmp( sixteennulls, &header_buf[ 8 ], sizeof( sixteennulls ))
		!= 0 ) {
	    fprintf( stderr, 
		    "Warning:  %s may be a corrupt AppleSingle file.\n",
		    single.path );
	    return( -1 );
	}
    } else {
	fprintf( stderr, "%s is a version of AppleSingle I don't understand!\n",
		single.path );
	return( -1 );
    }

    return( cc );
}

/*
 * single_read is called until it returns zero for each fork.  When
 * it returns zero for the first fork, it seeks to the proper place
 * to read in the next, if there is one.  single_read must be called
 * enough times to return zero for each fork and no more.
 *
 */

single_read( fork, buffer, length )
    int			fork;
    char		*buffer;
    int			length;
{
    u_long		entry_id;
    char		*buf_ptr;
    int			readlen;
    int			cc = 1;
    off_t		pos;

    switch ( fork ) {
	case DATA :
	    entry_id = ENTRYID_DATA;
	    break;
	case RESOURCE :
	    entry_id = ENTRYID_RESOURCE;
	    break;
	default :
	    return( -1 );
	    break;
    }

    if ( single.entry[ entry_id ].length < 0 ) {
	fprintf( stderr, "single_read: Trying to read past end of fork!\n" );
	return( single.entry[ entry_id ].length );
    }
    if ( single.entry[ entry_id ].length == 0 ) {
	if ( fork == DATA ) {
	    pos = lseek( single.filed,
		single.entry[ ENTRYID_RESOURCE ].offset, SEEK_SET );
	}
	return( 0 );
    }

    if ( single.entry[ entry_id ].length < length ) {
	readlen = single.entry[ entry_id ].length;
    } else {
	readlen = length;
    }

    buf_ptr = buffer;
    while (( readlen > 0 ) && ( cc > 0 )) {
	if (( cc = read( single.filed, buf_ptr, readlen )) > 0 ) {
	    readlen -= cc;
	    buf_ptr += cc;
	}
    }
    if ( cc >= 0 ) {
	cc = buf_ptr - buffer;
	single.entry[ entry_id ].length -= cc;
    }

    return( cc );
}
