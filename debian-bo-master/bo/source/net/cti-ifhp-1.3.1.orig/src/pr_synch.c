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
 * $Id: pr_synch.c,v 1.6 1996/05/19 03:11:07 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * In the PJL manual, Chapter 9, Section: Determining if printer status
 * readback is available it states that if the application does not
 * receive a response within three seconds, then the application can 
 * assume printer status readback is currently unavailable.
 */

int pr_synch(int fd, int timeout)
{
	int   synchronised = 0, fftries = 0;
	char buf[MAXLINE+1];
	char sync[MAXLINE+1];
	char timed[MAXLINE+1];
	int len;
	int i = 0;
	long starttime, newtime, inittime;
	int delay = 1;

	if( wait_time <= 0 ) wait_time = 30;
	if( timeout <= 0 ) timeout = wait_time;

	log(2,"Synchronisation attempt, %d sec timeout", timeout);
	
	starttime = time( (time_t *)0 );
	inittime = starttime;
	query[i++]=UEL;
	query[i++]=PCLRESETSTR;
	query[i++]=UELPJL;
	query[i++]=USTPJLOFF;
	query[i++]=USTPJLDEV;
	query[i++]=USTPJLJOB;
	(void)plp_snprintf(timed,sizeof(timed)-1,USTPJLTIMED, wait_time);
	query[i++]=timed;
	(void)plp_snprintf(sync,sizeof(sync)-1,"Seq %d",getpid());
	(void)plp_snprintf(buf,sizeof(buf)-1,"@PJL ECHO %s\r\n",sync);
	query[i++]=buf;
	query[i++]=INFOSTATUS; /* job status on */

	delay = 1;
	do{
		pr_query(fd, i);
		log(2,"pr_synch: waiting %d, attempt %d", delay, ++fftries );
		len = readpipe((int *)0,delay);
		if( len < 0 ){
			break;
		}
		if( strstr(readprin,sync) ) {
			synchronised = TRUE;
		} else if( strstr(readprin, "@PJL ECHO") ) {
			log(2,"pr_synch: synch from previous job '%s'", readprin);
		}
		if( !synchronised ){
			/* increase the waiting time */
			if( delay < timeout ) delay += delay;
			newtime = time( (time_t *)0 );
			if( (newtime - starttime) > timeout ){
				starttime = newtime;
				len = newtime - inittime;
				log(0,"No response from printer for %d secs; offline?", len );
			}
		}
	} while( !synchronised );
	return( synchronised );
}

void header_info()
{
	int i = 0;
	query[i++]=UEL;
	query[i++]=PCLRESETSTR;
	query[i++]=UELPJL;
	pr_query(STDOUT, i);
}
