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
 * $Id: port_read.c,v 1.5 1996/05/19 03:11:02 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * Read a line from a descriptor with timeout.
 *  Read the line looking for '\r', `\n` or '\014'
 * (FormFeed is the terminator for all responses
 * from the HP LaserJet 4 printers).  We store the line in the buffer,
 * then follow it with a null (the same as fgets(3)).
 * We return the number of characters up to, but not including,
 * the null (the same as strlen(3)).
 * return:
 *  n > 0 - number of characters read
 *  n = 0 - timeout, no chars read
 *  n < 0 - EOF
 */

int port_readline(fd, ptr, maxlen, timec )
int    fd;
char   *ptr;
int    maxlen;
int    timec;
{
    int                      n, doread, rc, err;
    struct timeval           timeout;
    fd_set                   readfds;

	log(4,"port_readline: fd %d, maxlen %d, timeout %d", fd, maxlen, timec );

	ptr[0] = 0;
	n = doread = rc = 0;
    do {
		doread = rc = 0;
		FD_ZERO(&readfds);
		FD_SET( fd, &readfds);
		memset(&timeout, 0, sizeof(timeout) );
		if( timec >= 0 ){
			timeout.tv_sec = timec;
		} else {
			timeout.tv_sec = 1024*1024;	/* whacking great number */
		}
		if( n > 0 ){
			timeout.tv_sec = 1;
		}
		doread = select(fd+1, &readfds,(fd_set *)0,(fd_set *)0, &timeout);
		err = errno;
		log(4,"port_readline: select returned %d", doread );
		if( doread > 0 ){
			rc = read(fd, &ptr[n], maxlen - n );
			err = errno;
			log(8,"port_readline: read returned %d", rc );
			if( rc > 0 ) n += rc;
			ptr[n] = 0;
		}
		if( strpbrk( ptr, "\014\n\r" ) ){
			break;
		}
    } while( doread > 0 && rc > 0 && n < maxlen );
	/* handle the error conditions or EOF easily */
	log(4,"port_readline: read %d, '%s'", n, ptr );
	/* check for EOF */
	/* this is when you have a pending read and get 0 bytes */
	if( n == 0 && doread > 0 && rc == 0 ){
		n = -1;
	}
    return(n);
}
