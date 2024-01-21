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
 * $Id: open_device.c,v 1.2 1995/09/10 14:43:23 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "errorcodes.h"

extern char *device;
void Do_stty();

void open_device()
{
	char *s;
	int i;
	int fd, port;
	struct hostent *hostent;
	struct sockaddr_in sin;

	log(3,"open_device: device '%s'", device);
	if( (device[0] == '/') ){
		fd = open( device, O_RDWR );
		if( fd < 0 ){
			logerr(0,"open_device: open '%s' failed");
			fexit( JABORT );
		}
		if( fd != 1 ){
			if( dup2( fd, 1 ) < 0 ){
				logerr(0,"open_device: open '%s' failed",device);
				fexit( JABORT );
			}
			close( fd );
		}
		if( isatty( 1 ) && stty_args){
			Do_stty( 1, stty_args );
		}
	} else {
		if( (s = strchr( device, '%' )) == 0 ){
			log(1,"open_device: missing port number '%s'",device );
			fexit( JABORT );
		}
		port = atoi( s+1 );
		if( port <= 0 ){
			log(1,"open_device: bad port number '%s'",s+1 );
			fexit( JABORT );
		}
		*s = 0;
		sin.sin_family = AF_INET;
		if( (hostent = gethostbyname(device)) ){
			/*
			 * set up the address information
			 */
			if( hostent->h_addrtype != AF_INET ){
				log(1,"open_device: bad address type for host '%s'", device);
				fexit( JABORT );
			}
			memcpy( &sin.sin_addr, hostent->h_addr, hostent->h_length );
		} else {
			sin.sin_addr.s_addr = inet_addr(device);
			if( sin.sin_addr.s_addr == -1){
				log(1,"open_device: getconnection: unknown host '%s'", device);
				fexit( JABORT );
			}
		}
		*s = '%';
		sin.sin_port = htons( port );
		log(2,"open_device: destination '%s' port %d",
			inet_ntoa( sin.sin_addr ), ntohs( sin.sin_port ) );
		fd = socket (AF_INET, SOCK_STREAM, 0);
		if (fd < 0) {
			logerr(0,"open_device: socket call failed - %s");
			fexit( JABORT );
		}
		i = connect (fd, (struct sockaddr *) & sin, sizeof (sin));
		if( i < 0 ){
			logerr(0,"open_device: connect to '%s port %d' failed",
				inet_ntoa( sin.sin_addr ), ntohs( sin.sin_port ) );
			fexit(JABORT);
		}
		if( fd != 1 ){
			log(4,"open_device: dup %d to %d", fd, 1 );
			if( dup2( fd, 1 ) < 0 ){
				logerr(0,"open_device: dup failed");
				fexit(JABORT);
			}
			close( fd );
		}
	}
}
