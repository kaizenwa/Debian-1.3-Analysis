/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Wei Xu, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: pmsg.c,v 1.3 94/11/01 11:58:44 mjr Exp ";


#include        "ulib.h"
#include        <syslog.h>


/*******************************************************
 *  perrno: 0 don't use sys_errlist like perror.
 *  uselog must be set before puting msg to the log 
 *         file which can also be used as debeg ouputs.
 *******************************************************/
void pmsg( msg, perrno )
char *msg;
int   perrno;
{
   char    buf[BUFSIZE];

   if( perrno ) {
       sprintf( buf, "%s: %s\n", msg, sys_errlist[errno] );
       msg=buf;
   }

   if( uselog ) syslog( LLEV, msg );
   else {
       fprintf( stderr, msg );
       fflush(stderr);
   }
}

void exitmsg( ontime, msg )
time_t	ontime;
char	*msg;	/* additional msgs */
{
	char    buf[256];
	time_t  offtime;

	time(&offtime);
	if( msg ) sprintf( buf,"%s exit duration=%d\n",msg,offtime-ontime );
	else	  sprintf( buf,"exit duration=%d\n",offtime-ontime );
	pmsg( buf,0 );

	if( uselog ) closelog();
}   

dump_struct(data,sz)
        caddr_t data;
	int     sz;
{
	int     i,cnt=0;
	u_char *p=(u_char *)data;

	printf("dump %d bytes struct:\n",sz);
	for(i=0;i<sz;i++) {
		printf("%02x ",*p++);
		cnt++;
		if(cnt==8) {
			printf("\n");
			cnt=0;
		}
	}
	printf("\n");
}

