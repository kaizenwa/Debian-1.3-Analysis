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
 * $Id: get_info.c,v 1.4 1996/07/19 23:40:13 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

void get_info()
{
	char *s, **t;
	if( version ){
		for( t = copyright; (s = *t); ++t ){
			fprintf( stderr, "%s\n", s );
		}
		fflush(stderr);
	}
	if( name && (s = strrchr(name, '/' )) ){
		name = s+1;
	}
	if( !strcasecmp( name, PSOF ) ){
		log(2,"filter '%s'", name );
		of_filter = 1;
	} else if( format && *format == 'o' ){
		log(2,"format '%s'", format );
		of_filter = 1;
	}
	log(2,"psof pid %d", getpid() );

	if( (s = getenv("RETRIES")) ){
		retries = atoi(s);
	}
	if( (s = getenv("SLEEPTIME")) ){
		wait_time = atoi(s);
	}
    if( (s = getenv("MODEL")) || (s = model_name) ){
        if ( ! strcasecmp(s, "IV") )   model = IV ;
        if ( ! strcasecmp(s, "III") )   model = III ;
        if ( ! strcasecmp(s, "IIID") )  model = IIID ;
        if ( ! strcasecmp(s, "IIISi") ) model = IIISi ;
    }
    if( (s = getenv("CARTRIDGE")) ){
		cartridge = ( !strcasecmp(s,"ON") || !strcasecmp(s,"YES") );
	}
	if( (s = getenv("RESOURCESAVE")) ){
		resourcesave = ( !strcasecmp(s,"ON") || !strcasecmp(s,"YES") );
	}
	if( (s = getenv("TRACE")) ){
		trace = ( !strcasecmp(s,"ON") || !strcasecmp(s,"YES") );
	}
	if( (s = getenv("WRAP")) ){
		wrap = ( !strcasecmp(s,"ON") || !strcasecmp(s,"YES") );
	}
	if( (s = getenv("AUTODETECT")) ){
		autodetect = ( !strcasecmp(s,"ON") || !strcasecmp(s,"YES") );
	}
}
