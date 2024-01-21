/*
 *
 * $Id: gwm.shar,v 1.115 1995/12/08 07:51:55 colas Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>        /* F_SETFL and likes */
#include <netinet/in.h>   /* sockaddr_in */
#include <netdb.h>        /* getservbyname() */


#define SPY_PORT 13667
/*
#define SPY_PORT 13400
*/
#define SPY_HOST "indri.inria.fr"

#define	SPY_PACKET_SIZE 1000
#define SPY_MAGIC_NUMBER (char) 0xf0

#ifdef __STDC__
int
KoalaSpy_SendPacket(char *host, /* 0 -> SPY_HOST */
		    int port,   /* 0 -> SPY_PORT */
		    char *progName,   /* defines the log file */
		    char *origin,    /* 0 -> hostname */
		    char *msg);
#endif
