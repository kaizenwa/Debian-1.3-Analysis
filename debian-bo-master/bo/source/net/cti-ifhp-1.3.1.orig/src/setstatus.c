/*==========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                               =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 = Created by Patrick Powell  <papowell@sdsu.edu>                          =
 =   for LPRng software Sat Aug 26 06:54:25 PDT 1995                       =
 ==========================================================================*/


/*==========================================================================
  Version CTI-LPRng-1.0
  =========================================================================*/


/*
 * $Id: setstatus.c,v 1.6 1996/11/14 19:56:36 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"

/*
 * Error status on STDERR
 */

static int active;

void newstatus()
{
	if( Status_fd > 0 ){
		close(Status_fd);
	}
	Status_fd = 0;
}

void summary( char *s );

void setstatus( char *msg )
{
	static char *save;
	static int size, minsize;
	char *s, *str;
	int len, l;
	struct stat statb;

	/* append new status to end of old */

	str = 0;

	if( active ) return;
	++active;
	if( Status_fd == 0 && statusfile ){
		log(4, "setstatus: statusfile '%s'", statusfile );
		Status_fd = -1;
		if( Max_status_size == 0 ) Max_status_size = 2;
		size = Max_status_size * 1024;
		if( Min_status_size ){
			minsize = Min_status_size * 1024;
		} else {
			minsize = size / 4;
		}
		if( minsize > size ){
			minsize = size;
		}
		Status_fd = open( statusfile, O_RDWR|O_APPEND );
		if( Status_fd < 0 ){
			logerr( 1, "setstatus: cannot open '%s'", statusfile );
		}
		log(4, "setstatus: statusfile '%s', fd %d", statusfile, Status_fd );
	}
	if( Status_fd > 0 ){
		if( fstat( Status_fd, &statb ) < 0 ){
			logerr_die( 1, "setstatus: cannot stat '%s'", statusfile );
		}
		if( size > 0 && statb.st_size > size ){
			/* we truncate it */
			log(4,"setstatus: truncating '%s', size %d, file size %d",
				statusfile, size, statb.st_size );
			if( save == 0 ){
				save = malloc( minsize+1 );
				if( save == 0 ){
					logerr_die(1,"malloc failed");
				}
			}
			if( lseek( Status_fd, (off_t)(statb.st_size-minsize), 0 ) < 0 ){
				logerr_die( 1, "setstatus: cannot seek '%s'", statusfile );
			}
			for( len = minsize, str = save;
				len > 0 && (l = read( Status_fd, str, len ) ) > 0;
				str += l, len -= l );
			*str = 0;
			if( (s = strchr( save, '\n' )) ){
				str = s+1;
			} else {
				str = save;
			}
			log(5,"setstatus: truncating status file '%s', size %d",
				statusfile, statb.st_size );
			if( lseek( Status_fd, (off_t)0, SEEK_SET ) < 0 ){
				logerr_die(1, "setstatus: lseek failed '%s'",
					statusfile );
			}
			if( ftruncate( Status_fd, (off_t)0 ) < 0 ){
				logerr_die( 1, "setstatus: cannot truncate '%s'",
					statusfile );
			}
			if( fstat( Status_fd, &statb ) < 0 ){
				logerr_die( 1, "setstatus: cannot stat '%s'", statusfile );
			}
			log(5,"setstatus: truncated file size '%d', str size %d",
				statb.st_size, strlen(str) );
		}
		if( str ){
			len = strlen(str);
			if( writecn( Status_fd, str, len ) != len ){
				logerr_die( 1, "setstatus: write to status file failed" );
			}
		}
		len = strlen(msg);
		if( writecn( Status_fd, msg, len ) != len ){
			logerr_die( 1, "setstatus: write to status file failed" );
		}
	}
	if( summaryfile ){
		summary( msg );
	}
	--active;
}

/**********************************************
 * Support a simple one line summary message for status
 * This is handy for debugging and reporting information
 **********************************************/

int udp_open( char *device )
{
	int port, i, fd, err;
	struct hostent *hostent;
	struct sockaddr_in sin;
	struct servent *servent;
	char *s;

	log(6, "udp_open: '%s'\n",device );
	if( (s = strpbrk( device, "@%" )) == 0 ){
		log(1, "udp_open: missing port number '%s'\n",device );
		return( -1 );
	}
	if( strpbrk( s+1, "@%" ) ){
		log(1, "udp_open: two '@' or '%' in name '%s'\n",
			device );
		return( -1 );
	}
	port = atoi( s+1 );
	if( port <= 0 ){
		servent = getservbyname( s+1, "udp" );
		if( servent ){
			port = ntohs( servent->s_port );
		}
	}
	if( port <= 0 ){
		log(1, "udp_open: bad port number '%s'\n",s+1 );
		return( -1 );
	}
	i = *s;
	*s = 0;
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = -1;

	if( (hostent = gethostbyname(device)) ){
		/*
		 * set up the address information
		 */
		if( hostent->h_addrtype != AF_INET ){
			log(1, "udp_open: bad address type for host '%s'\n",
				device);
		}
		memcpy( &sin.sin_addr, hostent->h_addr, hostent->h_length );
	} else {
		sin.sin_addr.s_addr = inet_addr(device);
	}
	*s = i;
	if( sin.sin_addr.s_addr == -1){
		log(1,"udp_open: unknown host '%s'\n", device);
		return(-1);
	}
	sin.sin_port = htons( port );
	log(6, "udp_open: destination '%s' port %d\n",
		inet_ntoa( sin.sin_addr ), ntohs( sin.sin_port ) );
	fd = socket (AF_INET, SOCK_DGRAM, 0);
	err = errno;
	if (fd < 0) {
		log(1,"udp_open: socket call failed - %s\n", Errormsg(err) );
		return( -1 );
	}
	i = connect (fd, (struct sockaddr *) & sin, sizeof (sin));
	err = errno;

	if( i < 0 ){
		log(1,"udp_open: connect to '%s port %d' failed - %s\n",
			inet_ntoa( sin.sin_addr ), ntohs( sin.sin_port ),
			Errormsg(errno) );
		close(fd);
		fd = -1;
	}
	return( fd );
}

static int summary_fd = -1;
void summary( char *s )
{
	int err;
	int len;
	char msg[256];

	if( summary_fd < 0 ){
		if( summary_fd == -2 ) return;
		if( strpbrk( summaryfile, "%@" ) ){
			summary_fd = udp_open( summaryfile );
		} else {
			summary_fd = open( summaryfile, O_RDWR|O_CREAT|O_APPEND, 0644 );
			err = errno;
			if( summary_fd < 0 ){
				fprintf( stderr, "could not open '%s' - %s", summaryfile,
					Errormsg(err) );
				return;
			}
		}
		if( summary_fd < 0 ){
			summary_fd = -2;
			return;
		}
	}
	/* truncate the file - note - must be open R/W on some systems... */
	(void)ftruncate(summary_fd,0);
	/* add printer name */
	plp_snprintf( msg, sizeof(msg), "PRINTER=%s %s", printer, s );
	len = strlen( msg );
	writecn( summary_fd, msg, len );
}
