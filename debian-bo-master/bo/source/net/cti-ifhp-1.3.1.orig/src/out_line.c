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
 * $Id: out_line.c,v 1.1 1995/09/02 15:12:45 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

/*
 * Write the buffer out to the printer.
 */
void Out_line(char *sendline)
{
	int i;

	log(4,"banner: line '%s'", sendline );
	i = strlen(sendline);
	if (writecn( fileno(stdout), sendline, i ) != i){
		logerr(3,"Out_line: '%s' writecn failed\n", sendline );
	}
}
