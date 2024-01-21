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
 * $Id: sendjob.c,v 1.9 1996/11/14 19:56:36 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

/*
 * send job to the remote printer
 */

void sendjob(fd, sockfd)
int    fd;
int    sockfd;
{
	int 	rdcnt;
	int i;
	char *s, *ps;
	int discard = 0;

	/* read an input line */
	log(9,"sendjob: reading input");
	/* set up the options using PJL.
     * Note: you may need to do this again in PCL or PostScript
	 */
	if( (rdcnt=file_readline(fd,sendline,sizeof(sendline)-1)) > 0){
		log(6,"sendjob: read %d '%s'", rdcnt,sendline);
		/* try to determine which mode you are in */
		if( literal || autodetect ){
			mode = LITERAL;
			job_page_set();
		} else {
			if( (ps = strstr(sendline, "%!" )) ){
				for( s = sendline; ps && s!=ps; ++s ){
					/* we must have a control char string */
					if( !iscntrl(*s) ) ps = 0;
				}
			}
			if( ps && cartridge ){
				mode = POSTSCRIPT ;
				job_page_set();
			} else {
				mode = PCL ;
				job_page_set();
				if( ps ){
					s = UNABLE;
					i = strlen(s);
					writecn(sockfd,s,i);
				}
			}
		}
		log(2,"Transferring file");
		if( !discard && rdcnt != writecn(sockfd, sendline, rdcnt) ){
			logerr_die(2,"Cannot write what read (%d bytes)",rdcnt);
		}
		log(4,"sendjob: wrote first line '%s'", sendline );
		/* now we read the rest of the file */
		if( !discard ){
			while( (rdcnt=read(fd,sendline,sizeof(sendline)-1)) > 0){
				log(4,"sendjob: next line '%s'", sendline );
				if( rdcnt != writecn(sockfd, sendline, rdcnt ) ){
					logerr_die(2,"Cannot write what read (%d bytes)",rdcnt);
				}
			}
		}
    }
	log(2,"Finished transferring file");
}
