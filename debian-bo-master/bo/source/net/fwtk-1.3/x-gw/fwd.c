/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

 /*
  *      Author: Wei Xu, Trusted Information Systems, Inc.
*/
static  char    RcsId[] = "Header: ";

#include "ulib.h"
#include <netinet/in.h>
#include <sys/socket.h>
#include <signal.h>

extern char *bncat();


int	readfd( readfd,rset,wset,pbuf,bsize )
int	*readfd;
fd_set  *rset, *wset;
char   **pbuf;
int	 bsize;
{
        char     buf[BUFSIZE+5];
	int	 cnt;

	switch( cnt=read(*readfd,buf,BUFSIZE) ) {
		case -1: printf(buf,"read fd:%d",readfd);  
			 pmsg(buf,1);
		case  0: clear_close_fd(*readfd,rset,wset);
			 return -1;
		default: if(cnt==BUFSIZE) FD_SET(*readfd,rset);
			 *pbuf=bncat(*pbuf,bsize,buf,cnt);
			 cnt+=bsize;
			 break;
	}
	return cnt;
}
int	writefd( writefd,rset,wset,pbuf,bsize )
int     *writefd;
fd_set  *rset, *wset;
char   **pbuf;
int	 bsize;
{
        char    buf[BUFSIZE+5], *p=NULL;
	int	cnt;

        if( !*pbuf ) return 0;
	switch( (cnt=write(*writefd,*pbuf,bsize)) ) {
		case -1: printf(buf,"write fd:%d",readfd);
			 pmsg(buf,1);
		case  0: clear_close_fd(*writefd,rset,wset);
			 free(*pbuf);
			 return -1;
		default: if(cnt<bsize) {
				p=bncat(NULL,0,&pbuf[cnt],bsize-cnt);
				FD_SET(*writefd,wset);
                         }
			 free(*pbuf);
			*pbuf= p;
			 break;
	}
	return bsize-cnt;
}

/* *****pipe2sockets******************************
 * connect two sockets. 
 * This is a blocking loop handling/forwarding data
 * between two sockets so that at both of other
 * ends of the sockets looks just connected.
 *************************************************/
void	pipe2sockets( dest,from,rset,wset,timemax,idlemax,ppid )
int	dest,    from;
fd_set  *rset,	 *wset;
int	timemax, idlemax;
pid_t   ppid;
{
	fd_set	 readable, writable;
	char    *pbuffrom=NULL; /* buf from dest to forward to from */
	char	*pbufdest=NULL; /* buf from from to forward to dest */
	int      szfrom=0,  szdest=0; /* buf size of pbufxxx */
	int	 ret, maxfd;

	maxfd=max(dest,from)+1;
	for(;;) {
		readable= *rset; writable= *wset;

		ret=serv_select( maxfd,&readable,&writable,timemax,idlemax );
		if( ret<0 ) {
			if( -ret==EINTR ) continue;
			break;
		}
		else if( !ret ) continue;

		if(FD_ISSET(from,&readable) && 
		   (szfrom=readfd(&from,rset,wset,&pbuffrom,szfrom))<0)
		   break;

		if(FD_ISSET(dest,&readable) &&
		   (szdest=readfd(&dest,rset,wset,&pbufdest,szdest))<0)
		   break;;

		if(pbufdest && FD_ISSET(from,&writable) &&
		   (szdest=writefd(&from,rset,wset,&pbufdest,szdest))<0)
		   break;

		if(pbuffrom && FD_ISSET(dest,&writable) &&
		   (szfrom=writefd(&dest,rset,wset,&pbuffrom,szfrom))<0)
		   break;

		if( ppid>=0 && ppid!=getppid() ) break;
	}
	if( pbuffrom ) free(pbuffrom);
	if( pbufdest ) free(pbufdest);
}

static pid_t	query_conn( fd, 	fwd, 
			    rset, 	wset,
			    conn_query_cb, userdata,
			    timemax, 	idlemax,
			    dependchild )
int     fd;
struct  sockaddr_in fwd;
fd_set  *rset,   *wset;
int   (*conn_query_cb)();
void   *userdata;
int     timemax, idlemax;
pid_t	dependchild;
{
	int                 querysock, outgoing,
			    saddrlen=sizeof(struct sockaddr_in);
	pid_t               pid;
	struct sockaddr_in  saddr;

       /* new connection */
       if( (querysock=accept(fd,(struct sockaddr *)&saddr,&saddrlen)) < 0 ) {
		if( errno != EINTR ) pmsg("accept",TRUE);
		return -1;
	}
	if( (outgoing=conn_sd(fwd))<0 ) {
		close(querysock);
		return -1;
	}
	FD_SET(querysock,rset);	FD_SET(querysock,wset);
	FD_SET(outgoing, rset);	FD_SET(outgoing, wset);

#ifndef WEI_DBG
	if( (pid=fork())< 0) {
		pmsg("Forking child failed",1);
		return -1;
	} else if( pid ) {
		close(querysock); close(outgoing);
		return pid;
	}
#endif
	/* ************************************************
	 * if conn_query_cb return <0, there is something
	 * error and to quit the query_conn
	 **************************************************/
	if( conn_query_cb( querysock,outgoing,rset,wset,saddr,userdata )>=0 ) {
		time_t		    ontime;
		time(&ontime);
		pipe2sockets(outgoing,querysock,rset,wset,timemax,idlemax,
			    (dependchild)?getppid():-1);
		exitmsg( ontime, "child" );
	} 
	close(querysock); close(outgoing);
	exit(1);
}

list_t *fwd_loop( lsock,         fwd,
	       timemax,       idlemax,
	       conn_query_cb, userdata,
	       app_cb,        cldexitcb,
	       app_data,
	       dependchild )/* != 0: the children will be killed
			     *       when its parent die.
			     *  0: otherwise the children will keep alive.
			     */
int      lsock;            /* the local socket		*/
struct   sockaddr_in fwd;  /* the forward addr		*/
int      timemax, idlemax; /* max timeout and idle time */
int    (*conn_query_cb)(); /* handle callback when connection requested */
void    *userdata;	   /* pass userdata for conn_query_cb 		*/
int    (*app_cb)();	   /* handle application routines while looping *
				return <0 to stop looping		*/
int    (*cldexitcb);	   /* handle children exit callback
			    * return < -1 to keep the pid in the list   */
void    *app_data;	   /* pass data for both app_cb and cldexitcb	*/
pid_t	 dependchild;
{
    register	int       n;

    int		fds=lsock+1;
    list_t	*pids=NULL;
    fd_set	allset,	rset, wset;
    pid_t	pid;        /* may need to handle child exit */

    FD_ZERO(&allset); FD_ZERO(&wset); FD_ZERO(&rset); FD_SET(lsock,&allset);
    
    while (1) {
	rset=allset;
        if( (n=serv_select( fds,&rset,NULL,timemax,idlemax )) <0 ) {
	    if( -n==EINTR ) continue;
	    else exit(1);
	}
	    
	if(FD_ISSET(lsock,&rset)) {
		pid = query_conn( lsock, 	 fwd, 
				  &rset,	 &wset,
				  conn_query_cb, userdata,
				  timemax,	 idlemax,
				  dependchild ); 
	       	if( pid<0 ) exit(1);
		pids=(list_t*)setList(pids,pid,(void*)pid);
	} 
	pids=(list_t*)chldsigs(pids,cldexitcb,app_data);

        if( app_cb && app_cb( lsock,fwd,pids,allset,app_data )<0 ) break;
    } 
    return pids;
}

