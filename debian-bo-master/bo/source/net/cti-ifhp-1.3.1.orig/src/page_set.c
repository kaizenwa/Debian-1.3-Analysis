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
 * $Id: page_set.c,v 1.5 1996/11/14 19:56:32 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

#define PSDUPLEX	"statusdict begin true setduplexmode false settumble end\r\n"
#define PSLDUPLEX	"statusdict begin true setduplexmode false settumble end\r\n"
#define PSSDUPLEX	"statusdict begin true setduplexmode true settumble end\r\n"
#define PSSIMPLEX	"statusdict begin false setduplexmode end\r\n"
#define PSUPPER		"statusdict begin 0 setpapertray end\r\n"
#define PSLOWER		"statusdict begin 1 setpapertray end\r\n"
#define PSMANUAL	"statusdict begin /manualfeed true store end\r\n"
#define PSENVELOP	"statusdict begin com10envelopetray end\r\n"
#define PSTOP		"statusdict begin 0 setoutputtray end\r\n"
#define PSBACK		"statusdict begin 1 setoutputtray end\r\n"

#define PCLDUPLEX	"\033&l1S"
#define PCLLDUPLEX	"\033&l1S"
#define PCLSDUPLEX	"\033&l2S"
#define PCLSIMPLEX	"\033&l0S"
#define PCLUPPER	"\033&l1H"
#define PCLLOWER	"\033&l4H"
#define PCLMANUAL	"\033&l2H"
#define PCLENVELOP	"\033&l1S\033&l1O\033&l81A"
#define PCLTOP		"\033&l1G"
#define PCLBACK		"\033&l2G"

void job_page_set()
{
	int len;
	char buffer[150], *ptr, *str, *s, *t;
	int lnd = 0;
	int duplex = 0;
	int incr = 0;

	log(4,"job_page_set: starting" );

	ptr = PGSTR;
	if( zopts ){
		if ( strstr(zopts, "landscape")!=NULL ) {
			log(2,"Setting PGL Landscape Mode");
			str = LANDSPJL;
			len = strlen(str);
			log(4,"job_page_set: sending '%s'", str );
			if( writecn(STDOUT,str,len) != len ){
				logerr(3,"Cannot send LANDSCAPE string");
			}
			lnd = 1 ;
			width = 132 ;
			length = 41;
		}
		if( strstr(zopts, "duplex") !=NULL ){
			log(2,"Setting PGL Duplex Mode");
			str = DUPLXPJL;
			len = strlen(str);
			log(4,"job_page_set: sending '%s'", str );
			if( writecn(STDOUT,str,len) != len ){
				logerr(3,"Cannot send DUPLEX string");
			}
			duplex = 1;
			ptr = PGSTRDPL;
			incr = 5;
		}
	}
	if( mode == POSTSCRIPT ){
		log(2,"Entering POSTSCRIPT Mode");
		setps_on();

		if ( zopts ){
			s = 0;
			if (strstr(zopts, "top") ){
			  s = PSTOP ;
			} else if (strstr(zopts, "back") ){
			  s = PSBACK ;
			}
			if( s && (len = strlen(s)) ){
			  log(4, "job_page_set: sending '%s'", s );
			  write_check(STDOUT,s,"Cannot send outtray string",0);
			  sleep(5);	/* allow tray time to change */
			}

			s = 0;
			if (strstr(zopts, "upper") ){
			  s = PSUPPER ;
			} else if (strstr(zopts, "lower") ){
			  s = PSLOWER ;
			} else if (strstr(zopts, "manual") ){
			  s = PSMANUAL ;
			}
			if( s && (len = strlen(s)) ){
			  log(4, "job_page_set: sending '%s'", s );
			  write_check(STDOUT,s,"Cannot send intray string",0);
			  sleep(5);	/* allow tray time to change */
			}

			s = PSDUPLEX; /* Default */
			if (strstr(zopts, "lduplex") ){
			  s = PSLDUPLEX ;
			} else if (strstr(zopts, "sduplex") ){
			  s = PSSDUPLEX ;
			} else if (strstr(zopts, "duplex") ){
			  s = PSDUPLEX ;
			} else if (strstr(zopts, "simplex") ){
			  s = PSSIMPLEX ;
			}
			if( s && (len = strlen(s)) ){
			  log(4, "job_page_set: sending '%s'", s );
			  write_check(STDOUT,s,"Cannot send page_set string",0);
			}
		}
	} else if( mode == PCL ) {
		log(2,"Entering PCL Mode");
		setpcl_on();
		if( zopts ){
			s = PCLDUPLEX; /* Default */
			if (strstr(zopts, (t = "lduplex")) ){
			  s = PCLDUPLEX ;
			} else if (strstr(zopts, (t = "sduplex")) ){
			  s = PCLSDUPLEX ;
			} else if (strstr(zopts, (t ="duplex")) ){
			  s = PCLDUPLEX ;
			} else if (strstr(zopts, (t = "simplex")) ){
			  s = PCLSIMPLEX ;
			}
			if( s && *s ){
			  log(4, "job_page_set: %s sending '%s'", t, s );
			  write_check(STDOUT,s,"Cannot send duplex string",0);
			}

			s = 0;
			if (strstr(zopts, (t = "upper")) ){
			  s = PCLUPPER ;
			} else if (strstr(zopts, (t ="lower")) ){
			  s = PCLLOWER ;
			} else if (strstr(zopts, (t = "manual")) ){
			  s = PCLMANUAL ;
			}
			if( s && *s ){
			  log(4, "job_page_set: %s sending '%s'", t, s );
			  write_check(STDOUT,s,"Cannot send intray string",0);
			  sleep(5);	/* allow tray time to change */
			}

			s = 0;
			if (strstr(zopts,(t = "top")) ){
			  s = PCLTOP ;
			} else if (strstr(zopts, (t = "back")) ){
			  s = PCLBACK ;
			}
			if( s && *s ){
			  log(4, "job_page_set: %s sending '%s'", t, s );
			  write_check(STDOUT,s,"Cannot send outtray string",0);
			  sleep(5);	/* allow tray time to change */
			}
			if( strstr(zopts, "wrap") ){
				wrap = 1;
			}
		}

		log(4, "job_page_set: CRLFSTR sending '%s'", CRLFSTR );
		write_check(STDOUT, CRLFSTR, "Cannot send CRLF string", 0);

		/* fix up font and other information */
		if( length == 0 ) length = 60;
		if( width == 0 ) width = 80;

		plp_snprintf(buffer, sizeof(buffer), ptr, lnd, width+incr, length);

		log(4,"page_set: lnd %d, width %d, length %d, sending '%s'",
			lnd, width, length, buffer );
		write_check(STDOUT,buffer,"Cannot send Control Page string",0);

		if( lnd ){
			s = PITCHLND;
			t = "landscape";
		} else if ( width==132 ){
			s = PITCH132;
			t = "132";
		} else {
			s = PITCH80;
			t = "80";
		}
		log(4,"page_set: pitch %s sending '%s'", t, s );
		len = strlen(s);
		write_check(STDOUT,s,"Cannot send PITCH string",0);

		if( duplex ){
			s = SHIFTDPL;
			t = "duplex";
		} else {
			s = SHIFT;
			t = "normal";
		}
		log(4,"page_set: shift %s sending '%s'", t, s );
		write_check(STDOUT,s,"Cannot send SHIFT string",0);

		/*
		 * If line wrapping is required the WRAP env variable should be set to ON.
		 */
		if( wrap ){
			log(4,"page_set: wrap %d sending '%s'", wrap, WRAPON );
			write_check(STDOUT, WRAPON, "Cannot send WRAPON string", 0);
		}

		selectfont();
	}
}
