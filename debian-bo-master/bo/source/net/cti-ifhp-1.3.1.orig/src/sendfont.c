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
 * $Id: sendfont.c,v 1.3 1996/04/21 19:14:31 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


int sendfont(fontfilename)
char *fontfilename;
{
	FILE *ffp;
	int n;
	char buffer[128];

	log(4,"sendfont: sending font '%s'", fontfilename );


	if ((ffp=fopen(fontfilename,"r")) == NULL) {
		logerr(1,"sendfont: cannot open file '%s'",fontfilename);
		return(1);
	}

	log(4,"sendfont: sending ASSIGN string '%s'", ASSIGN );

	write_check(STDOUT, ASSIGN, "sendfont: cannot send ASSIGN font string", DIE);

	while ((n = fread( buffer, 1, sizeof(buffer), ffp)) > 0 ){
		if( writecn( STDOUT, buffer, n ) != n ){
			errorcode = JABORT;
			fatal( "cannot load font file '%s' to printer", fontfilename );
		}
	}
	fclose(ffp);

	write_check(STDOUT, PGRESET, "Cannot send PGRESET font string", DIE);
	write_check(STDOUT, ASSIGN, "Cannot send ASSIGN string", DIE);
	write_check(STDOUT, MKPRIM, "Cannot send MKPRIM string", DIE);
	write_check(STDOUT, MKPERM, "Cannot send MKPERM string", DIE);

	log(4,"sendfont: done");
	return(0); /* success */
};

