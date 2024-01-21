#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/uio.h>
#if BSD >= 199006
# include <machine/endian.h>
#else
# include <netinet/in.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <strings.h>
#include <syslog.h>
#include <dirent.h>
#include <fcntl.h>
#include <atalk/adouble.h>
#include "megatron.h"

#define	DEBUG		0

static char		hexdig[] = "0123456789abcdef";

char *mtoupath( mpath )
    char	*mpath;
{
    static char	upath[ MAXNAMLEN ];
    char	*m, *u;
    int		i = 0;

    m = mpath;
    u = upath;
    while ( *m != '\0' ) {
	if ( !isascii( *m ) || *m == '/' || ( i == 0 && *m == '.' )) {
	    *u++ = ':';
	    *u++ = hexdig[ ( *m & 0xf0 ) >> 4 ];
	    *u++ = hexdig[ *m & 0x0f ];
	} else {
#ifdef DOWNCASE
	    *u++ = ( isupper( *m )) ? tolower( *m ) : *m;
#else DOWNCASE
	    *u++ = *m;
#endif DOWNCASE
	}
	i++;
	m++;
    }
    *u = '\0';
    return( upath );
}

#if HEXOUTPUT
    int			hexfork[ NUMFORKS ];
#endif

struct nad_file_data {
    char		macname[ MAXPATHLEN ];
    char		adpath[ 2 ][ MAXPATHLEN ];
    int			offset[ NUMFORKS ];
    struct adouble	ad;
} nad;

nad_open( path, openflags, fh )
    char		*path;
    int			openflags;
    struct FHeader	*fh;
{
    struct stat		st;
    int			fork;

/*
 * Depending upon openflags, set up nad.adpath for the open.  If it 
 * is for write, then stat the current directory to get its mode.
 * Open the file.  Either fill or grab the adouble information.
 */

    if ( openflags == O_RDONLY ) {
	strcpy( nad.adpath[0], path );
	strcpy( nad.adpath[1], 
		ad_path( nad.adpath[0], ADFLAGS_DF|ADFLAGS_HF ));
	for ( fork = 0 ; fork < NUMFORKS ; fork++ ) {
	    if ( stat( nad.adpath[ fork ], &st ) < 0 ) {
		if ( errno == ENOENT ) {
		    fprintf( stderr, "%s is not an adouble file.\n", path );
		} else {
		    perror( "stat of adouble file failed" );
		}
		return( -1 );
	    }
	}

#if DEBUG
    fprintf(stderr, "%s is adpath[0]\n", nad.adpath[0]);
    fprintf(stderr, "%s is adpath[1]\n", nad.adpath[1]);
#endif
	if ( ad_open( nad.adpath[ 0 ], ADFLAGS_DF|ADFLAGS_HF,
		openflags, (int)( st.st_mode & 0666 ), &nad.ad ) < 0 ) {
	    perror( nad.adpath[ 0 ] );
	    return( -1 );
	}
	return( nad_header_read( fh ));

    } else {
	strcpy( nad.macname, fh->name );
	strcpy( nad.adpath[0], mtoupath( nad.macname ));
	strcpy( nad.adpath[1], 
		ad_path( nad.adpath[0], ADFLAGS_DF|ADFLAGS_HF ));
#if DEBUG
    fprintf(stderr, "%s\n", nad.macname);
    fprintf(stderr, "%s is adpath[0]\n", nad.adpath[0]);
    fprintf(stderr, "%s is adpath[1]\n", nad.adpath[1]);
#endif
	if ( stat( ".", &st ) < 0 ) {
	    perror( "stat of . failed" );
	    return( -1 );
	}
	(void)umask( 0 );
	if ( ad_open( nad.adpath[ 0 ], ADFLAGS_DF|ADFLAGS_HF,
		openflags, (int)( st.st_mode & 0666 ), &nad.ad ) < 0 ) {
	    perror( nad.adpath[ 0 ] );
	    return( -1 );
	}
	return( nad_header_write( fh ));
    }
}

nad_header_read( fh )
    struct FHeader	*fh;
{
    long		temptime;
    struct stat		st;

    bcopy( ad_entry( &nad.ad, ADEID_NAME ), nad.macname, 
	    ad_getentrylen( &nad.ad, ADEID_NAME ));
    nad.macname[ ad_getentrylen( &nad.ad, ADEID_NAME ) ] = '\0';
    strcpy( fh->name, nad.macname );
    if ( stat( nad.adpath[ DATA ], &st ) < 0 ) {
	perror( "stat of datafork failed" );
	return( -1 );
    }
    fh->forklen[ DATA ] = htonl( st.st_size );
    fh->forklen[ RESOURCE ] = htonl( ad_getentrylen( &nad.ad, ADEID_RFORK ));
    fh->comment[0] = '\0';

#if DEBUG
    fprintf( stderr, "macname of file\t\t\t%.*s\n", strlen( fh->name ), 
	    fh->name );
    fprintf( stderr, "size of data fork\t\t%d\n", 
	    ntohl( fh->forklen[ DATA ] ));
    fprintf( stderr, "size of resource fork\t\t%d\n", 
	    ntohl( fh->forklen[ RESOURCE ] ));
    fprintf( stderr, "get info comment\t\t\"%s\"\n", fh->comment );
#endif

    bcopy(( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_CREATE ), &temptime,
	    sizeof( temptime ));
    temptime = htonl( ntohl( temptime ) + TIME_DIFF );
    bcopy( &temptime, &fh->create_date, sizeof( temptime ));
    bcopy(( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_MODIFY ), &temptime,
	    sizeof( temptime ));
    temptime = htonl( ntohl( temptime ) + TIME_DIFF );
    bcopy( &temptime, &fh->mod_date, sizeof( temptime ));
    bcopy(( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_BACKUP ), &temptime,
	    sizeof( temptime ));
    temptime = htonl( ntohl( temptime ) + TIME_DIFF );
    bcopy( &temptime, &fh->backup_date, sizeof( temptime ));

#if DEBUG
    bcopy( &fh->create_date, &temptime, sizeof( temptime ));
    temptime = ntohl( temptime );
    fprintf( stderr, "create_date seconds\t\t%lu\n", temptime );
    bcopy( &fh->mod_date, &temptime, sizeof( temptime ));
    temptime = ntohl( temptime );
    fprintf( stderr, "mod_date seconds\t\t%lu\n", temptime );
    bcopy( &fh->backup_date, &temptime, sizeof( temptime ));
    temptime = ntohl( temptime );
    fprintf( stderr, "backup_date seconds\t\t%lu\n", temptime );
    fprintf( stderr, "size of finder_info\t\t%d\n", sizeof( fh->finder_info ));
#endif

    bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + FINDERIOFF_TYPE ),
	    (char *)&fh->finder_info.fdType,
	    sizeof( fh->finder_info.fdType ));
    bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + FINDERIOFF_CREATOR ),
	    (char *)&fh->finder_info.fdCreator,
	    sizeof( fh->finder_info.fdCreator ));
    bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + FINDERIOFF_FLAGS ),
	    (char *)&fh->finder_info.fdFlags,
	    sizeof( fh->finder_info.fdFlags ));
    bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + FINDERIOFF_LOC ),
	    (char *)&fh->finder_info.fdLocation,
	    sizeof( fh->finder_info.fdLocation ));
    bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + FINDERIOFF_FLDR ),
	    (char *)&fh->finder_info.fdFldr,
	    sizeof( fh->finder_info.fdFldr ));

#if DEBUG
    {
	short		flags;
	fprintf( stderr, "finder_info.fdType\t\t%.*s\n", 
		sizeof( fh->finder_info.fdType ), &fh->finder_info.fdType );
	fprintf( stderr, "finder_info.fdCreator\t\t%.*s\n", 
		sizeof( fh->finder_info.fdCreator ),
		&fh->finder_info.fdCreator );
	fprintf( stderr, "nad type and creator\t\t%.*s\n\n", 
		sizeof( fh->finder_info.fdType ) + 
		sizeof( fh->finder_info.fdCreator ), 
		ad_entry( &nad.ad, ADEID_FINDERI ));
	bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + 
		sizeof( fh->finder_info.fdType ) + 
		sizeof( fh->finder_info.fdCreator ) ), &flags, sizeof( flags ));
	fprintf( stderr, "nad.ad flags\t\t\t%x\n", flags );
	fprintf( stderr, "fh flags\t\t\t%x\n", fh->finder_info.fdFlags );
    }
#endif

    nad.offset[ DATA ] = nad.offset[ RESOURCE ] = 0;

    return( 0 );

}

nad_header_write( fh )
    struct FHeader	*fh;
{
    long		temptime;

    ad_setentrylen( &nad.ad, ADEID_NAME, strlen( nad.macname ));
    bcopy( nad.macname, ad_entry( &nad.ad, ADEID_NAME ), 
	    ad_getentrylen( &nad.ad, ADEID_NAME ));
    ad_setentrylen( &nad.ad, ADEID_RFORK, ntohl( fh->forklen[ RESOURCE ] ));

#if DEBUG
    fprintf( stderr, "ad_getentrylen\n" );
    fprintf( stderr, "ADEID_FINDERI\t\t\t%d\n", 
	    ad_getentrylen( &nad.ad, ADEID_FINDERI ));
    fprintf( stderr, "ADEID_RFORK\t\t\t%d\n", 
	    ad_getentrylen( &nad.ad, ADEID_RFORK ));
    fprintf( stderr, "ADEID_NAME\t\t\t%d\n",
	    ad_getentrylen( &nad.ad, ADEID_NAME ));
    fprintf( stderr, "ad_entry of ADEID_NAME\t\t%.*s\n",
	    ad_getentrylen( &nad.ad, ADEID_NAME ), 
	    ad_entry( &nad.ad, ADEID_NAME ));
#endif

    bcopy( &fh->create_date, &temptime, sizeof( temptime ));
    temptime = htonl( ntohl( temptime ) - TIME_DIFF );
    bcopy( &temptime, ( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_CREATE ), 
	    sizeof( temptime ));
    bcopy( &fh->mod_date, &temptime, sizeof( temptime ));
    temptime = htonl( ntohl( temptime ) - TIME_DIFF );
    bcopy( &temptime, ( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_MODIFY ),
	    sizeof( temptime ));

#if DEBUG
    bcopy(( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_CREATE ), 
	    &temptime, sizeof( temptime ));
    temptime = ntohl( temptime );
    fprintf(stderr, "FILEIOFF_CREATE seconds\t\t%ld\n", temptime );
    bcopy(( ad_entry( &nad.ad, ADEID_FILEI ) + FILEIOFF_MODIFY ), 
	    &temptime, sizeof( temptime ));
    temptime = ntohl( temptime );
    fprintf(stderr, "FILEIOFF_MODIFY seconds\t\t%ld\n", temptime );
#endif

    bzero( ad_entry( &nad.ad, ADEID_FINDERI ), ADEDLEN_FINDERI );
    bcopy( (char *)&fh->finder_info.fdType, 
	    ad_entry( &nad.ad, ADEID_FINDERI ), 
	    sizeof( fh->finder_info.fdType ));
    bcopy( (char *)&fh->finder_info.fdCreator, 
	    ad_entry( &nad.ad, ADEID_FINDERI ) + 
	    sizeof( fh->finder_info.fdType ), 
	    sizeof( fh->finder_info.fdCreator ));
    bcopy( (char *)&fh->finder_info.fdFlags, 
	    ad_entry( &nad.ad, ADEID_FINDERI ) + 
	    sizeof( fh->finder_info.fdType ) + 
	    sizeof( fh->finder_info.fdCreator ), 
	    sizeof( fh->finder_info.fdFlags ));

#if DEBUG
    {
	short		flags;
	bcopy(( ad_entry( &nad.ad, ADEID_FINDERI ) + 
		sizeof( fh->finder_info.fdType ) + 
		sizeof( fh->finder_info.fdCreator ) ), &flags, sizeof( flags ));
	fprintf( stderr, "nad.ad flags\t\t\t%x\n", flags );
	fprintf( stderr, "fh flags\t\t\t%x\n", fh->finder_info.fdFlags );
	fprintf( stderr, "type and creator\t\t%.*s\n\n", 
		sizeof( fh->finder_info.fdType ) + 
		sizeof( fh->finder_info.fdCreator ),
		ad_entry( &nad.ad, ADEID_FINDERI ));
    }
#endif

#if HEXOUTPUT
    hexfork[ DATA ] = open( "datafork", O_WRONLY|O_CREAT, 0622 );
    hexfork[ RESOURCE ] = open( "resfork", O_WRONLY|O_CREAT, 0622 );
#endif

    nad.offset[ DATA ] = nad.offset[ RESOURCE ] = 0;
    ad_flush( &nad.ad, ADFLAGS_DF|ADFLAGS_HF );

    return( 0 );
}

int			forkeid[] = { ADEID_DFORK, ADEID_RFORK };

nad_read( fork, forkbuf, bufc )
    int			fork;
    char		*forkbuf;
    int			bufc;
{
    int			cc = 0;

#if DEBUG
    fprintf( stderr, "Entering nad_read\n" );
#endif

    if (( cc = ad_read( &nad.ad, forkeid[ fork ], nad.offset[ fork ], 
	    forkbuf, bufc )) < 0 )  {
	perror( "Reading the appledouble file:" );
	return( cc );
    }
    nad.offset[ fork ] += cc;

#if DEBUG
    fprintf( stderr, "Exiting nad_read\n" );
#endif

    return( cc );
}

nad_write( fork, forkbuf, bufc )
    int			fork;
    char		*forkbuf;
    int			bufc;
{
    char		*buf_ptr;
    int			writelen;
    int			cc = 0;

#if DEBUG
    fprintf( stderr, "Entering nad_write\n" );
#endif

#if HEXOUTPUT
    write( hexfork[ fork ], forkbuf, bufc );
#endif

    writelen = bufc;
    buf_ptr = forkbuf;

    while (( writelen > 0 ) && ( cc >= 0 )) {
	cc =  ad_write( &nad.ad, forkeid[ fork ], nad.offset[ fork ], 
		0, buf_ptr, writelen );
	nad.offset[ fork ] += cc;
	buf_ptr += cc;
	writelen -= cc;
    }
    if ( cc < 0 ) {
	perror( "Writing the appledouble file:" );
	return( cc );
    }

    return( bufc );
}

nad_close( status )
int			status;
{
    int			rv;
    if ( status == KEEP ) {
	if (( rv = ad_flush( &nad.ad, ADFLAGS_DF|ADFLAGS_HF )) < 0 ) {
	    fprintf( stderr, "nad_close rv for flush %d\n", rv );
	    return( rv );
	}
	if (( rv = ad_close( &nad.ad, ADFLAGS_DF|ADFLAGS_HF )) < 0 ) {
	    fprintf( stderr, "nad_close rv for close %d\n", rv );
	    return( rv );
	}
    } else if ( status == TRASH ) {
	if ( unlink( nad.adpath[ 0 ] ) < 0 ) {
	    perror ( nad.adpath[ 0 ] );
	}
	if ( unlink( nad.adpath[ 1 ] ) < 0 ) {
	    perror ( nad.adpath[ 1 ] );
	}
	return( 0 );
    } else return( -1 );
}
