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
 * $Id: pr_duplex.c,v 1.3 1996/11/14 19:56:33 papowell Exp papowell $
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

void pr_duplex()
{
	char *s = 0;
	int len;

	if ( zopts ){
	  if( mode==POSTSCRIPT ){
	    s = 0;
	    if (strstr(zopts, "top") ){
	      s = PSTOP ;
	    } else if (strstr(zopts, "back") ){
	      s = PSBACK ;
	    }
	    if( s && (len = strlen(s)) ){
	      log(4, "pr_duplex: sending '%s'", s );
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
	      log(4, "pr_duplex: sending '%s'", s );
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
	      log(4, "pr_duplex: sending '%s'", s );
	      write_check(STDOUT,s,"Cannot send page_set string",0);
	    }

	  } else {
	    s = PCLDUPLEX; /* Default */
	    if (strstr(zopts, "lduplex") ){
	      s = PCLDUPLEX ;
	    } else if (strstr(zopts, "sduplex") ){
	      s = PCLSDUPLEX ;
	    } else if (strstr(zopts, "duplex") ){
	      s = PCLDUPLEX ;
	    } else if (strstr(zopts, "simplex") ){
	      s = PCLSIMPLEX ;
	    }
	    if( s && (len = strlen(s)) ){
	      log(4, "pr_duplex: sending '%s'", s );
	      write_check(STDOUT,s,"Cannot send duplex string",0);
	    }

	    s = 0;
	    if (strstr(zopts, "upper") ){
	      s = PCLUPPER ;
	    } else if (strstr(zopts, "lower") ){
	      s = PCLLOWER ;
	    } else if (strstr(zopts, "manual") ){
	      s = PCLMANUAL ;
	    }
	    if( s && (len = strlen(s)) ){
	      log(4, "pr_duplex: sending '%s'", s );
	      write_check(STDOUT,s,"Cannot send intray string",0);
		  sleep(5);	/* allow tray time to change */
	    }

	    s = 0;
	    if (strstr(zopts, "top") ){
	      s = PCLTOP ;
	    } else if (strstr(zopts, "back") ){
	      s = PCLBACK ;
	    }
	    if( s && (len = strlen(s)) ){
	      log(4, "pr_duplex: sending '%s'", s );
	      write_check(STDOUT,s,"Cannot send outtray string",0);
		  sleep(5);	/* allow tray time to change */
	    }

	  }
	}
}
