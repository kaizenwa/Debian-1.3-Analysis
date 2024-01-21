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
static	char	RcsId[] = "Header: socket.c,v 1.4 94/11/01 13:53:19 mjr Exp ";

#include	"ulib.h"
#include	<sys/time.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include        <sys/un.h>

extern  struct   hostent	*gethostname();


void	clear_close_fd( fd, rset, wset )
int	fd;
fd_set	*rset,	*wset;
{
	FD_CLR(fd,rset);
	FD_CLR(fd,wset);
	close(fd);
}

/* return: -1: error to get the local host
 *          0: OK
 * localhname: Must have length at leas MAXHOSTNAMELEN
 */
int	get_local_hostname( localhname )
char	localhname[];
{
	struct   hostent *host;
	char              buf[128];

	gethostname(localhname, MAXHOSTNAMELEN);
	if( !(host=gethostbyname(localhname)) ) {  
		sprintf( buf,
			"Can't lookup up my own hostname %s\n",
			localhname );
		pmsg( buf,0 );
		return -1;
	}
	sprintf(localhname,"%s\0",host->h_name);
	return 0;
}

/* return: timeout -99.
 *         error   -errno.
 *         >=0     selected.
 */
int serv_select( fds, readable, writable, timemax, idlemax )
int     fds;
int    *readable, *writable;
int     timemax,   idlemax;
{
	int     nready;
	char	buf[64];

	struct timeval    timeout;
	static time_t	  startime=NULL;
	
	timeout.tv_usec=0; timeout.tv_sec=timemax;

	nready = select(fds, (readable)?readable:NULL, 
			     (writable)?writable:NULL,
			     0, &timeout);
	if( nready == -1) {
	    if (errno != EINTR) pmsg("select",TRUE);
	    return -errno;
	}
	if(!startime) time(&startime);

	if (nready == 0) {
	    time_t	now;
	    time(&now);
	    if( (now-startime)>idlemax ) {
		sprintf(buf,"connections timed out in %d seconds\n",idlemax);
		pmsg(buf,0);
		return -99;
	    }
	} else {
	    /* reset timeout counter if there is some activity */
	    time(&startime);
	}
	return nready;
}

int  saddr_init( host, family, port, addr )
char  *host;
int    family;
int    port;
struct sockaddr *addr;
{
       struct sockaddr_in *in_name;   /* AF_INET */
       struct sockaddr_un *un_name;   /* AF_UNIX */
       struct hostent     *hptr;
       char               *p, buf[64];
       unsigned long       f;

       bzero(addr,sizeof(struct sockaddr));
       if( (p=host) ) {
/*	   while( *p != '\0' && (*p == '.' || isdigit(*p)) ) p++;
 */
	   while( *p != '\0' && *p == '.' ) p++;
           if( *p ) /* not all digits or dots */ {
               if( (hptr=gethostbyname(host)) == (struct hostent *)0 ) {
		   sprintf(buf,"hostname unknown: %s\n",host);
	           pmsg(buf,0);
	           return -1;
               }
           } else {
	           if( (f=inet_addr(host)) == -1L ) {
		       pmsg("hostname unparsable",0);
		       return(-1);
	           }
           }
       }

       switch( family )
	  {
            case AF_INET :  /* inet socket init */
		 in_name = ( struct sockaddr_in * )addr;
		 in_name->sin_family     =family;
		 in_name->sin_port       =htons(port);

#ifdef NOT_BSD
		 in_name->sin_addr.s_addr= (host && *host) ?
					   rhost(&host):INADDR_ANY;
#else
		 if( !host || !*host ) in_name->sin_addr.s_addr=INADDR_ANY;
		 else {
			if( *p )
			    memcpy( &in_name->sin_addr.s_addr,
			    	     hptr->h_addr,
				     hptr->h_length );
                        else if( f != -1L )
			    (void)bcopy( (char *)&f,
					 (char *)in_name->sin_addr.s_addr,
					 sizeof f );
		 }
#endif
	       break;

            case AF_UNIX: /* unix domain socket init */
                 un_name = ( struct sockaddr_un * ) addr;
		 memset( un_name, 0, sizeof(un_name) );

		 un_name ->sun_family = family;
		 if( !host ) break;
		 if( *p )
		      strncpy( un_name->sun_path,
			       hptr->h_addr,
			       hptr->h_length ); 
                 else  if( f != -1L )
		     (void)bcopy( (char *)&f,
				  (char *)un_name->sun_path,
				  sizeof f );
	       break;

            default:
		sprintf( buf,"The family(%d) not supported yet :-(\n",family );
		pmsg( buf,0 );
		return -1;
	       break;
       }
       return 0;
}

int  sock_init( host, family, port, type, addr, client )
char  *host;
int    family;
int    port;
int    type;
struct sockaddr *addr;
int    client;    /* client-side(0) or server-side(1) */
{
       int                 sd;

       if( ( sd=socket( family,type,0 ) )<0 ) {
	   pmsg( "socket init",TRUE );
	   return sd;
       }
       if( saddr_init( host,family,port,addr )<0 ) return -1;
       
       return sd;
}

int saddrlen( family, addr )
int    family;
struct sockaddr *addr;
{
	int    len=0;
	struct sockaddr_in *in_name;   /* AF_INET */
	struct sockaddr_un *un_name;   /* AF_UNIX */

	if( addr )
	switch( family ) {
		case AF_INET:	in_name = (struct sockaddr_in *)addr;
				len = sizeof(*in_name);
	  		 break;

		case AF_UNIX:	 un_name = (struct sockaddr_un *)addr;
				len = sizeof(un_name->sun_family)  +
				sizeof(un_name->sun_path)
#ifdef SCM_RIGHTS  /* 4.3BSD Reno and later */
                     + sizeof(un_name->sun_len) + 1
#endif
		       ;
	           break;
       	}
	return len;
}

int bindport( sd, family, fromport, toport, addr )
int    sd;
int    family;
int    fromport;
int    toport;
struct sockaddr *addr;
{
   int    len=saddrlen( family,addr );
   struct sockaddr_in *in_name=(struct sockaddr_in *)addr;   /* AF_INET */
   struct sockaddr_un *un_name=(struct sockaddr_un *)addr;   /* AF_UNIX */
		       
   if( fromport>toport ) toport=fromport;

   while( bind( sd, (struct sockaddr *)addr, len )<0 ) {
       if( errno == EADDRINUSE ) {
           fromport++;
           if( fromport > toport ) {
  	       pmsg( "All ports are in use\n",0 );
	       return -1;
	   }
	   switch( family ) {
	       defaults :
	       case AF_INET : in_name->sin_port = htons( fromport );
		  break;

	       case AF_UNIX : /* not sure if this is right ?? */
		    /* un_name->sun_path = htons( fromport ); */
		     return -1;
                  break;
	    }
	    len = saddrlen( family, addr );
	    continue;
       } else {
	       pmsg( "bindport", TRUE );
	       return -1;
       }
   }/*while*/
   return fromport;
}

int serv_listen( name, family, saddr, fromport, toport )
char            *name;
int              family;
struct sockaddr *saddr;    /* none null for returning socket addr */
int             *fromport; /* also return the actual port #
	   		    * the server listening to             */
int             *toport;   /* maybe parse a null for no prot selection
			    * when failed                         */
{
	int             sd, one=1, port;
	struct sockaddr addr;

	if( (sd=sock_init( name, family, *fromport, SOCK_STREAM, 
				(saddr)?saddr:&addr, 0 ) )<0 )
	    return sd;

        switch( family ) {
	    case AF_INET : if( setsockopt( sd, SOL_SOCKET,
					       SO_REUSEADDR,
					      &one,sizeof(one) )<0 ) {
				pmsg("SO_REUSEADDR",TRUE);
				return -2;
                 	   }
                	   port= (fromport)? *fromport:4;
		break;

            defaults:
            case AF_UNIX : port= -1;
		break;
        }

        if( ( port = bindport( sd, family, 
	  		           port, (toport)?*toport:NULL,
			          (saddr)?saddr:&addr )
	     ) <0 ) sd= -3;
        else {
	    if( fromport ) *fromport=port;
	    if( listen( sd, SOMAXCONN ) <0 ) {
		pmsg( "serv_listen",TRUE );
		sd= -4;
	    }
        }
	return sd;
}

int cli_conn( name, family, port, saddr )
char  *name;
int    family;
int    port;
struct sockaddr *saddr; /* none null for returning socket addr */
{
      int     sd;
      struct  sockaddr   addr;

      if( ( sd=sock_init( name,family,port,SOCK_STREAM,
			 (saddr)?saddr:&addr,0 ) )<0 )
	  return sd;

      if( connect( sd, (saddr)?saddr:&addr, sizeof addr ) <0 ) {
	  pmsg( "cli_conn",TRUE );
	  close(sd);
	  return -2;
      }
      return sd;
}

int	conn_sd( saddr )
struct sockaddr_in saddr;
{
	int	new_sd;

	if( (new_sd=socket(PF_INET,SOCK_STREAM,0))<0 ) {
		pmsg("socket",TRUE);
		return -1;
	}
	if( connect(new_sd,(struct sockaddr*)&saddr,sizeof(saddr))<0 ) {
		pmsg("connect:");
		close(new_sd);
		return -1;
	}
	return new_sd;
}

size_t writen( fd, vptr, n )  /* write n bytes to the fd */
int      fd;
void    *vptr;
size_t   n;
{
   size_t   nleft=n, nwritten;
   char    *ptr=vptr;

   while( nleft>0 ) {
      if( ( nwritten = write( fd, ptr, nleft ) ) <= 0 )
	  return nwritten; /* error */

      nleft -= nwritten;
      ptr   += nwritten;
   }
   return n;
}

size_t readn( fd, vptr, n ) /* read n bytes from  the fd */
int      fd;
void    *vptr;
size_t   n;
{
   size_t   nleft=n, nread;
   char    *ptr=vptr;

   while( nleft>0 ) {
      if( ( nread = read( fd, ptr, nleft ) ) < 0 )
	  return nread;   /* error */
      else if( !nread )   /* EOF */
	  break;

      nleft -= nread;
      ptr   += nread;
   }
   return n-nleft;
}
