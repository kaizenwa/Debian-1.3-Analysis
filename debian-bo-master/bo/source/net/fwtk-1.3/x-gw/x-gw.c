/*- 
 * Copyright (c) 1994, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 *
 *      Author: Wei Xu, Trusted Information Systems, Inc.
 */
static  char    RcsId[] = "Header: x-gw.c,v 1.3 94/11/01 11:58:54 mjr Exp ";

/*************
 * INCLUDES  *
 *************/
#include "ulib.h"
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <signal.h>
#include <syslog.h>


/*************
 * EXTERN    *
 *************/
 extern	char	*getarg();
 extern char    *setarg();

/****************
 * PRIVATE DATA *
 ****************/

#define BASE_PORT	6000
#define TIME_GRAIN	3
#define MAXIDLE		3600	/* 6 hours */	
static	time_t   	ontime;

/****************
 * PUBLIC DATA *
 ****************/
#ifdef WEI_DBG
int        uselog=0;
#else
int        uselog=1;
#endif


/****************
 * Functions   *
 ****************/

void usage()
{
    fprintf(stderr,
"usage: x-gw [-disp display/hostname] [-from hostname] [-user username]\n" );
    exit(1);
}

void	control_exit()
{
	exitmsg( ontime, "control process killed." );
	exit(1);
}

int exit_xfwd_cb(lsd,fadd,pids,fdset,user_data)
int	lsd;
struct  sockaddr_in fadd;
list_t  *pids;
fd_set	fdset;
void   *user_data;
{ 
	if( handle_sigpid((pid_t)user_data)<0 ) return 0;

	clearList(pids,kill,SIGKILL);
	exitmsg(ontime,"control");
	exit(1);
}

/* ************ query_connect *******************************************
 * return child pid if the connection request is not matched.
 * return -1 if error.
 ************************************************************************/
int  query_connect( querysock,outgoing,rest,wset,saddr,userdata )
int                querysock;
int		   outgoing;
fd_set		  *rest, *wset;
struct sockaddr_in saddr;
void              *userdata;
{
	Widget   dialog;
	char     rladdr[128], riaddr[128], buf[256];
	int      argc = 3;

	/* init x create popup on destination: (char*)userdata */
	static   char *argv[]={ "x-gw","-display",NULL,NULL };

        argv[2]=(char*)userdata;
	xInit( argv,(char*)userdata,argc,argv );

	/* get host name and addr from rladdr */
	sprintf(rladdr,"%s",(char*)&saddr.sin_addr);
	if( peername(0,rladdr,riaddr,sizeof(riaddr)) ) {
		pmsg("fwtksyserr: cannot get peer name\n",0);
		return -1;
	}

	sprintf( buf,"Allow X connection from %s?\0",rladdr);
	if(loopDialog(NULL,buf,"OK",OK,"CANCEL",CANCEL,0)!=OK) return -1;

	sprintf(buf,"permit host=%s/%s x-connection\n",rladdr,riaddr);
	pmsg(buf,0);
	return 0;
}

int main(argc, argv)
int	argc;
char	*argv[];
{
	struct   sockaddr_in	fwd;
	char			host[MAXHOSTNAMELEN], buf[256], *dpy, *p;
	int      		port=BASE_PORT+10; /* x port starts from
						    * 6010 */
	int      		toport=100+BASE_PORT, len;
	int			lsd, idlemax; 
	pid_t    		pid;
	sws_l			*switches=NULL;

	/* syslog */
        if( uselog ) {
#ifndef LOG_DAEMON
        	openlog("x-gw",LOG_PID);
#else
        	openlog("x-gw",LOG_PID|LOG_NDELAY,LFAC);
#endif
        }
	uselog = 0; /** errors go to stderr at setup **/

#ifdef  BINDDEBUG
        debugbind();
#endif
#ifdef FORK
	become_child();
#endif
	time(&ontime);

	if( get_local_hostname(host) ) goto out;  /* Get the proxy hostname */
		/* get switches */
	if(!(switches=(sws_l*)rd_sws(argc,argv,"from:user:disp:time")) 
	   && argc>1) usage(); 

	if( !(dpy=getarg("disp",switches)) && /* Get dpy from -disp switch */
	    !(dpy=getarg("from",switches)) && /* Get if from fromhost      */
	    !(dpy=getenv("DISPLAY")) ) {      /* Get it from environment   */
		sprintf( buf,"%s:0.0",host ); /* use proxy hostname        */
		dpy=setarg("disp",buf,switches);
    	} else sprintf(buf,"%s",dpy);         /* save dpy in the buf       */

	if( (len=strcspn(dpy,":"))>0 && !strstr(dpy,":") ) {
		sprintf(buf,"%s:0.0\0",dpy);
		dpy=setarg("disp",buf,switches);
	} 
	buf[len]='\0'; /* the display hostname */
	if( (pid=atoi(dpy+len+1)) < 0 || pid>(toport-BASE_PORT) ) {
		sprintf(buf,"Invalid display=%s. Must be 0-100\n",dpy+len+1); 
		pmsg(buf,0);
		goto out;
	}
	if( !XOpenDisplay(dpy) ) { /* check the display */
		fprintf( stderr,"Unable to open display=%s\n",dpy );
		goto out;
    	}

	/* lisen to an available port of the proxy */
	if( (lsd=serv_listen(NULL,AF_INET,NULL,&port,&toport)) < 0 )
		goto out;

	/** init fwd destination sd **/
	if( saddr_init(buf,AF_INET,pid+BASE_PORT,&fwd) < 0 ) goto out;

	sprintf(buf,"display port=%s:%d\n",host,port-BASE_PORT);
	pmsg(buf,0);

	/* clean up to conserve descriptors. use syslog for error outputs */
	close(0); close(1);
#ifndef WEI_DBG
	close(2); uselog=1;
#endif
	/* setup is ok now and popup and display msgs on the control window */
	sprintf( buf, "started from %s by %s to display=%s\n",
		 getarg("from",switches),getarg("user",switches),dpy );
	pmsg( buf,0 );

	setenv( "DISPLAY",dpy,1 );
	sprintf( buf,
		"Display port is %s:%d\nClick the button to exit x-gw\0",
		 host, port-BASE_PORT );
	pid = childDialog( "x-gw",dpy,NULL,buf,NULL,0,"EXIT",OK,0 );

	/** the main loop for x forward **/
	sigexit(control_exit);
	idlemax=(p=getarg("time",switches))?atoi(p):MAXIDLE;

	fwd_loop( lsd,               fwd, 
		  TIME_GRAIN,        idlemax, 
		  query_connect,     dpy, 
		  exit_xfwd_cb,      NULL,
		  (void*)pid,	     pid );

out:	fflush(stdout);
#ifndef WEI_DBG
	closelog();
#endif
	exit(1);
}
