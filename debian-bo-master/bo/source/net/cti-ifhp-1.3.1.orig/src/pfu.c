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
 * $Id: pfu.c,v 1.2 1995/09/05 04:30:35 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 *
 */

void update_pfu(pfu_filename,fontfilename)
char *pfu_filename, *fontfilename;
{
	FILE *pfu;

	log(4,"pfu file '%s' value '%s'",
			pfu_filename, fontfilename );
	if ((pfu=fopen(pfu_filename,"w")) == NULL) {
		logerr(1,"Cannot open/create pfu file : %s",pfu_filename);
	} else {
		fprintf(pfu,"%s",fontfilename?fontfilename:"");
		fclose(pfu);
		log(4,"pfu file '%s' updated successfully", pfu_filename );
	}
}

void delete_pfu(pfu_filename)
	char *pfu_filename;
{
	log(4,"delete_pfu: unlinking '%s'", pfu_filename );
	if( unlink(pfu_filename) == -1){
		logerr(2,"Cannot unlink pfu file '%', possibly already deleted",
			pfu_filename);
	}
}

