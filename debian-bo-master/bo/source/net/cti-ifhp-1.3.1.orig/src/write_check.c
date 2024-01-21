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
 * $Id: write_check.c,v 1.1 1995/09/02 15:12:54 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void write_check(fd,str,msg,flg)
int  	 fd,   flg;
char	*str, *msg;
{
	int len;

	len = strlen(str);
	if ( writecn(fd, str, len) != len ){
		if ( flg==DIE ){
			logerr_die( 3, msg );
	 	} else {
			logerr( 3, msg );
		}
	}
}

