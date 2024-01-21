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
 * $Id: file_read.c,v 1.2 1996/05/06 04:33:59 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

/*
 * Read a line from a descriptor.  Read the line one byte at a time,
 * looking for a newline.  We store the newline in the buffer,
 * then follow it with a null (the same as fgets(3)).
 * We return the number of characters up to, but not including,
 * the null (the same as strlen(3)).
 */

int
file_readline(fd, ptr, maxlen)
	int    fd;
	char   *ptr;
	int    maxlen;
{
    int  n, rc = 0;

    for( n = 0; n < maxlen && (rc = read(fd,&ptr[n],1)) > 0; n++ ){
		if( ptr[n] == '\n' || ptr[n] == '\r' ){
			n++;
			break;
		}
	}
	ptr[n] = 0;
	if( n == 0 && rc < 0 ){
		n = rc;
	}
    return(n);
}

