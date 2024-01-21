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
 * $Id: doaccnt.c,v 1.12 1996/11/14 19:56:26 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 *  doaccnt()
 *  writes the accounting information to the accounting file
 *  This has the format: user host printer pages format date
 */

static int argc;
static char *parms[256];
static char list[1024];
static char list2[1024];

void inv( char *key, char *value )
{
	int len, len2;
	len = strlen( list );
	len2 = strlen( list2 );
	if( value && *value ){
		if( key ){
			plp_snprintf( list+len, sizeof(list)-len,
					" %s'%s'", key, value );
			plp_snprintf( list2+len2, sizeof(list2)-len2,
					" %s%s", key, value );
		} else {
			plp_snprintf( list+len, sizeof(list)-len,
					" '%s'", value );
			plp_snprintf( list2+len2, sizeof(list2)-len2,
					" %s", value );
		}
		parms[argc++] = &list2[len2+1];
	}
	parms[argc] = 0;
}

void doaccnt(int start)
{
	int fd;
	int i, err, pid;
	int status;
	char pid_str[32];
	char totalpages[32];
	char pages[32];
	char *sh;
	struct stat statb;
	char opt_str[3];

	strcpy( opt_str, "-x" );
	sh = Accounting_script;
	log(2,"Accounting script '%s'", Accounting_script );

	plp_snprintf( pid_str, sizeof(pid_str),  "%d", (int)(getpid()) );

	/* we first set up the output line and arguments */
	plp_snprintf( pages, sizeof(pages),  "%d", npages );

	if( start ){
		strcpy( list, "start " );
	} else {
		strcpy( list, "end " );
	}
	argc = 0;
	parms[argc++] = sh;
	for( i = 0; i < 26; ++i ){
		if( Loweropts[i] ){
			opt_str[1] = i + 'a';
			inv( opt_str, Loweropts[i] );
		}
	}
	for( i = 0; i < 26; ++i ){
		if( Upperopts[i] ){
			opt_str[1] = i + 'A';
			inv( opt_str, Upperopts[i] );
		}
	}
	if( !start ){
		plp_snprintf( totalpages, sizeof(totalpages), "%d", npages-initialpagecount );
		inv( "-b", totalpages );
	}
	inv( "-q", pid_str );
	inv( "-p", pages );
	inv( "-t", Time_str() );

	/* this is probably the best way to do the output */
	fd = Accounting_fd;
	if(fd >= 0 ||
		(accntfile && (fd = open(accntfile, O_WRONLY|O_APPEND )) >= 0 )) {
		i = strlen( list );
		list[i] = '\n';
		writecn(fd,list,i+1);
		if( fd != Accounting_fd ) (void)close(fd);
		list[i] = 0;
	}
	if( !start && sh && *sh ){
		if( stat( sh, &statb ) < 0 ){
			logerr(3,"Accounting script '%s' cannot stat- %s",
				sh, Errormsg(errno) );
			sh = 0;
		} else if( ! ( (statb.st_mode & 0001)
			|| ((statb.st_mode & 0010 ) && (statb.st_gid == getegid() ))
			|| ((statb.st_mode & 0100 ) && (statb.st_uid == geteuid() )) ) ){
			logerr(3,"Accounting script '%s' has bad exec perms or ownership",
				sh );
			sh = 0;
		}
		if( sh == 0 ) return;
		inv( 0, accntfile );
		log(4,"accounting '%s %s'", parms[0], list );
		/* fix up parm list */
		for( i = 1; i < argc; ++i ){
			parms[i][-1] = 0;
		}
		parms[argc] = 0;
		monitpid = fork();
		if( monitpid == -1 ) {
			errorcode = FILTABORT;
			logerr(1,"Cannot fork to update accounting");
		} else if( monitpid == 0 ){
			/* child - Exec the script to update pages */
			dup2(2,1);
			execve(sh,parms,Envp);
			errorcode = FILTABORT;
			logerr_die(1,"Cannot exec %s to update pages field",sh);
		} else {
			/* father - Wait for the update
			 * Child exits with 0 for Ok, 2 if signal
			 * terminated and 1 otherwise.
			 */
			do{
				status = 0;
				pid = waitpid( monitpid, &status, 0 );
				err = errno;
				log(4,"doaccnt: pid  %d, status 0x%x", pid, status );
			} while( pid == -1 && err != ECHILD );
			log(1,"Accounting process exited, status 0x%x", status);
			if( status ){
				errorcode = FILTABORT;
				fatal("Accounting process died, status 0x%x", status);
			}
		}
	}
}
