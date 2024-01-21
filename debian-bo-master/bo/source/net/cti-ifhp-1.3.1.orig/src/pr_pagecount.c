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
 * $Id: pr_pagecount.c,v 1.4 1996/07/19 23:40:20 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * Note: this routine is used for explicitly asking for pagecount.
 */

int pr_pagecount(fd)
int fd;
{
	int i = 0;
	int j;

	log(2,"Asking for pagecount");

	npages = -1;

	query[i++]=INFOPAGECOUNT;

   /* The inner loop added by Bob Beck <beck@cs.ualberta.ca>
	* Jan 15 1996.
	* Give the status monitor a sporting chance to
	* read our pagecount back before firing off another
	* query to the printer At worst we'll time out on a
	* read a few times.
	*/
	do {
		pr_query(fd, i);
		j = 0;
		do{
			int err;
			err = readpipe((int *)0,3);
			if( err < 0 ){
				log(2,"EOF reading from printer");
				fexit(FILTABORT);
			}
		} while( npages < 0 && (j < 10) );
	} while( npages < 0 );

	log(3,"Pagecount %d", npages );
	return( npages );
}

 
 
 /* Created by Bob Beck <beck@cs.ualberta.ca> 
  * for LPRng FILTERS 1.0.7 Jan 15 1996.
  */
 
 /* ask for a pagecount, postscript style
  * analogous to pr_pagecount(), which does it in the new minty PJL flavour
  * if the printer supports it. Unfortunately not all HP4's are created equal
  * and those without a + after the name don't seem to support the PJL INFO
  * PAGECOUNT command.  This is used in do_monitor.c and ifhp.c after
  * the PJL command is attempted. If the PJL command returns a pagecount
  * of 0, then this gets tried. 
  *
  * This uses a UEL command before and after the postscript grubby in order
  * to set the printer up for postscript, then exit back out to PJL, as in 
  * the example in HP's PJL reference manual, Edition 1, Sept 1992, P 5-5.  
  * (Job Separation Commands)
  *
  * -Bob "Mr. Orphaned Braindead Printers" Beck, Jan 1996.
  * 
  */
 
int pr_pspagecount(fd)
int fd;
{
	int j=0;
	int len;
	char *querystring = "/ps { print flush } def (\tPAGECOUNT ) ps statusdict begin pagecount end == flush\r\n";

	npages=-1;

	do {
	 write_check(fd, UELPJL, "Cannot send UELPJL", 0);
	 setps_on();
	 len=strlen(querystring);
	 log(2, "Asking for pagecount, PostScript style");
	 if( write(fd, querystring, len) != len ){
	   logerr(1,"Cannot write query string <%s>", querystring);
	   fexit(FILTABORT);
	 }
	 do {
		int err;
	   err = readpipe((int *)0,3);
		if( err < 0 ){
			log (2,"EOF reading from printer", npages);
			fexit(FILTABORT);
		}
	   j++;
	 } while ((npages < 0) && (j < 10));
	 write_check(fd, UEL, "Cannot UEL reset printer", 0);
	} while ( npages < 0 );
	log (2,"Pagecount %d (courtesy PostScript Hack)", npages);
	return (npages) ;
}
