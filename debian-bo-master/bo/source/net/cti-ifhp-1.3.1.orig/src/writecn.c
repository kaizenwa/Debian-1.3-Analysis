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
 * $Id: writecn.c,v 1.1 1995/09/02 15:12:54 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

int writecn(fd,ptr,nbytes)
int fd;
char *ptr;
int nbytes;
{
	int nleft, nwritten;
	
	log(8,"writecn: fd %d, nbytes %d", fd, nbytes );
	nleft = nbytes;
	while (nleft > 0){
		nwritten = write( fd, ptr, nleft);
		log(8,"writecn: wrote %d", nwritten );
		if (nwritten == 0) {
			logerr(4,"writecn wrote nothing");
		} else if(nwritten < 0) {
			logerr(4,"writecn failure");
			break;
		}
		nleft -= nwritten;
		ptr += nwritten;
	}
	return(nbytes - nleft);
}
