/*
 * File:	net.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 * 
 * Contrib.:	Heiko Schlichting (heiko@fu-berlin.de)
 *
 * History:	11 Aug 95   Framstag	initial version
 *         	10 Sep 95   Framstag	some debugging
 *         	15 Nov 95   Framstag	improved sock_getline
 *         	21 Dec 95   Framstag	simplified sock_getline and getreply
 *         	17 Apr 96   Framstag	new error handling in open_connection
 * 		14 May 96   Framstag	included and modified send_data()
 * 		21 May 96   Framstag	gettimeofday() fix for Solaris-2
 * 		20 Jun 96   Framstag	always use gethostbyname()
 * 		 3 Aug 96   Framstag	corrected thruput value
 *               4 Sep 96   Heiko	some fixes for IRIX
 *              24 Sep 96   Heiko	added get_domainname()
 *
 * Network routines for the the sendfile client of the sendfile package.
 * Look at net.h for list of the functions.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */

/*
#ifdef NEXT
  typedef unsigned char	 u_char;
  typedef unsigned short u_short;
  typedef unsigned int	 u_int;
  typedef unsigned long	 u_long;
  typedef long	daddr_t;
  typedef char *caddr_t;
  typedef long  time_t;
  #define _TIME_T
#endif
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "config.h"	/* various definitions */
#include "string.h"	/* extended string functions */
#include "message.h"	/* information, warning and error messages */
#include "net.h"	/* network stuff */
#include "io.h"		/* socket read/write */


/* stupid AIX comes with no include files for networking and other stuff */
#if defined(AIX) || defined(ULTRIX)
  #include "bsd.h"
#endif

#ifdef SOLARIS2
  #ifdef _SVID_GETTOD
    int gettimeofday(struct timeval *);
  #else
    int gettimeofday(struct timeval *, void *);
  #endif
#endif

#if defined(AIX) || defined(ULTRIX)
  int gettimeofday(struct timeval *, struct timezone *);
#endif

/*
#ifdef IRIX
  u_short htons(u_short hostshort);
#endif
*/

/*
 * open_connection - open socket and connect to client
 *
 * INPUT:  adr  - ip address of server to connect to
 *         port - port number to connect to
 *
 * RETURN: socket file descriptor
 *         -1 if socket creation failed
 *         -2 if connection failed
 *         -3 if unknown host
 *
 * this function is derived from example code from
 * "Unix Networking Programming" by W. R. Stevens
 */
int open_connection(char *adr, int port)
{ int sockfd,			/* socket file descriptor */
      num=1;			/* flag for numeric ip address */
  struct sockaddr_in serv_addr;	/* internet socket */
  struct in_addr hostaddr;
  struct hostent *hostp;	/* host entity */
  char *cp,			/* character pointer */
       hostname[16];		/* server host name */
#ifdef DEBUG
  extern char *prg;		/* name of the game */
#endif

  /* open socket */
  sockfd=socket(AF_INET,SOCK_STREAM,0);
  if (sockfd<0) return(-1);
#ifdef DEBUG
  message(prg,'I',"socket ok");
#endif

  /* initialisize serv_addr */
  memset((char *) &serv_addr, 0, sizeof(serv_addr));

  /* numeric oder symbolic ip address? */
  for (cp=adr; *cp>0; cp++)
  { if (*cp>'@')
    { num=0;
      break;
    }
  }

  /* quick hack: gethostbyname() does also work with numeric addresses */
  num=0;  
  
  /* look for server host address */
  if (num)
  { hostaddr.s_addr=inet_addr(adr);
    hostp=gethostbyaddr((char *)&hostaddr,sizeof(hostaddr),AF_INET);
  } else
    hostp=gethostbyname(adr);
  if (hostp==NULL) return(-3);

  /* convert binary structure to ASCII hostname */
  strcpy(hostname,inet_ntoa(*(struct in_addr *) *hostp->h_addr_list));
#ifdef DEBUG
  printf("host: %s\n",hostname);
#endif

  /* fill out server address descriptor */
  serv_addr.sin_family     =AF_INET;
  serv_addr.sin_addr.s_addr=inet_addr(hostname);
  serv_addr.sin_port       =htons(SAFT);

  /* connect to server */
  if (connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0)
    return(-2);
#ifdef DEBUG
  message(prg,'I',"connect ok");
#endif

  return(sockfd);
}


/*
 * sock_getline - get a (command) line from the network socket
 *
 * INPUT:  fd     - socket file descriptor
 *         line   - empty string
 *
 * OUTPUT: line   - read string
 *
 * RETURN: number of read bytes, -1 on error
 *
 * this function is derived from example code from
 * "Unix Networking Programming" by W. R. Stevens
 */
int sock_getline(int fd, char *line)
{ int n, rc;
  unsigned char c;

  *line=0;

  for (n=0; n+1<MAXLEN; n++)
  { rc=read(fd,&c,1);
    if (rc==1)
    { line[n]=c;
      if (c=='\n') break;
    } else
      return(-1);
  }

  /* too much input? */
  if (n+1==MAXLEN && line[n] != '\n')
  { errno=0;
    message("",'F',"network socket data overrun");
  }

  /* remove trailing cr or lf */
  line[n]=0;
  if (n>0 && line[n-1]=='\r') line[--n]=0;

  return(n);
}


/*
 * sock_putline - send a line to the network socket
 *
 * INPUT:  fd     - socket file descriptor
 *         line   - string to send
 *
 * RETURN: number of send bytes
 */
int sock_putline(int fd, const char *line)
{ int n;		/* number of send bytes */
  char cmd[MAXLEN];	/* command line to send */
  extern int verbose;	/* flag for verbose mode */

  /* prepare string */
  strcpy(cmd,line);
  strcat(cmd,"\r\n");

  /* on verbose mode show what goes up */
  if (verbose) printf("-> %s\n",line);

  /* and up and away :-) */
  n=writen(fd,cmd,strlen(cmd));

  return(n);
}


/*
 * getreply - get the reply on a command from the server
 *
 * INPUT:  fd - socket file descriptor
 *
 * RETURN: the reply line string
 */
char *getreply(int fd)
{ int len;			/* reply message length */
  char msg[MAXLEN];		/* intermediate information/error message */
  static char reply[MAXLEN];	/* reply string from server */
  extern int verbose;		/* flag for verbose mode */
  extern char *prg;		/* name of the game */

  do
  {
    /* get the next reply line */
    len=sock_getline(fd,reply);

    /* link failure? */
    if (len<0)
    { errno=0;
      strcpy(msg,"server has closed the connection");
      if (*reply) sprintf(msg,"%s, last data: \"%s\"",msg,reply);
      message("",'F',msg);
    }

    /* reply message too short? */
    if (len<4)
    { errno=0;
      sprintf(msg,"corrupt reply: \"%s\"",reply);
      message(prg,'F',msg);
    }

    /* on verbose mode show the whole line */
    if (verbose) printf("%s\n",reply);

  } while (reply[3]=='-');

  /* quit if there was a fatal server error */
  if (reply[0]=='4')
  { errno=0;
    sprintf(msg,"server error: %s",&reply[4]);
    message(prg,'F',msg);
  }

  return(reply);
}


/*
 * sendheader - send a headerline and check the reply code
 *
 * INPUT:  fd	- socket file descriptor
 *         line	- header line
 *
 * RETURN: 0 on sucess, -1 on server error
 */
int sendheader(int fd, char *line)
{ char msg[MAXLEN],		/* intermediate information/error message */
       *reply;			/* reply string from server */
  extern char *prg;		/* name of the game */

  /* send the header line */
  sock_putline(fd,line);

  /* server reply ok? */
  reply=getreply(fd);
  if (!strbeq(reply,"200"))
  { errno=0;
    sprintf(msg,"server error: %s",&reply[4]);
    message(prg,'F',msg);
  }

  return(0);
}




/*
 * send_data - send file data
 *
 * INPUT:  sockfd	- socket file descriptor
 *         size		- bytes to send
 *         file		- file to send
 *         iso_name	- name of the original file
 * 	   compressed	- compressed flag
 * 	   quiet	- quiet flag
 *
 * RETURN: 0 if ok, 1 if already transfered, -1 if transfer failed
 */
int send_data(int sockfd, unsigned long size, const char *file,
	      const char *iso_name, int compressed, int quiet)
{ int
    n,			/* simple loop count */
    nblocks,		/* number of PACKET length blocks */
    bn,			/* block number to read */
    percent,		/* what percentage of file has been transmitted */
    ffd;		/* file to send file descriptor */
  unsigned long
    bytes,		/* bytes which has been sent */
    offset;		/* bytes already sent */
  char
    packet[PACKET],	/* data packet to send */
    tmp[MAXLEN],	/* temporary string */
    fname[MAXLEN],	/* file name and compress info */
    *reply;		/* reply string */
  time_t sec;		/* unix time */
  float thruput;	/* net throughput */
  unsigned long msec;	/* milliseconds */
  struct timeval tv;
#if !defined(SOLARIS2) && !defined(IRIX)
  struct timezone tz;
#endif

  offset=0;
  strcpy(fname,iso_name);
  if (compressed) strcat(fname," (compressed)");
  if (!quiet) printf("sending...                        \r");

  /* resend? */
  sock_putline(sockfd,"RESEND");
  reply=getreply(sockfd);

  /* correct answer? */
  if (!strbeq(reply,"500 ") && !strbeq(reply,"502 "))
  {
    /* error occured? */
    if (!strbeq(reply,"230 "))
    { if (quiet<3)
      { sprintf(tmp,"server error: %s",&reply[4]);
	message("",'F',tmp);
      }
      return(-1);
    }

    sscanf(&reply[4],"%ld",&offset);
  }

  /* prepare sending of data */
  sock_putline(sockfd,"DATA");
  reply=getreply(sockfd);

  /* file already transmitted? */
  if (strbeq(reply,"531 "))
  { sprintf(tmp,"file %s has been already transmitted - ignored.",iso_name);
    if (quiet<2) message("",'W',tmp);
    return(1);
  }

  /* server reply ok? */
  if (!strbeq(reply,"302 "))
  { if (quiet<3)
    { sprintf(tmp,"server error: %s",&reply[4]);
      message("",'F',tmp);
    }
    return(-1);
  }

  /* open file */
  ffd=open(file,O_RDONLY,0);
  if (ffd<0 || lseek(ffd,offset,SEEK_SET)<0)
  { if (quiet<3)
    { sprintf(tmp,"error reading %s",iso_name);
      message("",'E',tmp);
    }
    return(-1);
  }

  /* get time normal */
#if defined(SOLARIS2) || defined(IRIX)
   #ifdef _SVID_GETTOD
     gettimeofday(&tv);
   #else
     gettimeofday(&tv,NULL);
   #endif
#else
  gettimeofday(&tv,&tz);
#endif
  msec=tv.tv_usec/1000;
  sec=time(0);

  /* send the file data in PACKET size blocks */
  bytes=0;
  size=size-offset;
  nblocks=size/PACKET;
  for (bn=1; bn<=nblocks; bn++)
  { if (readn(ffd,packet,PACKET)<PACKET)
    { if (quiet<3)
      { if (!quiet) printf("\n");
	sprintf(tmp,"error reading %s",iso_name);
	message("",'E',tmp);
      }
      close(ffd);
      return(-1);
    }
    if (writen(sockfd,packet,PACKET)<PACKET)
    { if (quiet<3)
      { if (!quiet) printf("\n");
	message("",'F',"error sending data");
      }
      close(ffd);
      return(-1);
    }

    /* print transaction message */
    bytes+=PACKET;
    percent=bytes*100/size;
    if (!quiet) {
      printf("%s: %3d%%  (%ld KB)\r",fname,percent,bytes/1024);
      fflush(stdout);
    }
  }

  /* copy the last bytes to the spool file */
  if ((n=size-nblocks*PACKET) > 0)
  { if (readn(ffd,packet,n)<n)
    { if (quiet<3)
      { sprintf(tmp,"error reading %s",iso_name);
	message("",'E',tmp);
      }
      close(ffd);
      return(-1);
    }
    if (writen(sockfd,packet,n)<n)
      if (quiet<3)
        message("",'F',"error sending data");
      else
        return(-1);
    bytes+=n;
  }

  close(ffd);

  /* transfer ok? */
  if (!strbeq(getreply(sockfd),"201 "))
  { if (quiet<3)
    { sprintf(tmp,"transfer failed for %s",iso_name);
      message("",'E',tmp);
    }
    return(-1);
  }

  if (!quiet)
  {
    /* get time difference */
#if defined(SOLARIS2) || defined(IRIX)
   #ifdef _SVID_GETTOD
     gettimeofday(&tv);
   #else
     gettimeofday(&tv,NULL);
   #endif
#else
    gettimeofday(&tv,&tz);
#endif
    msec=(time(0)-sec)*1000ul+tv.tv_usec/1000ul-msec;
    if (msec<1) msec=1;

    /* print transfer statistics */
    thruput=bytes*1000/msec;
    if (bytes>9999)
      printf("%s: 100%%  (%ld KB, ",fname,bytes/1024);
    else
      printf("%s: 100%%  (%ld byte, ",fname,bytes);
    if (thruput>9999)
      printf("%.1f KB/s)\n",thruput/1024);
    else
      printf("%d byte/s)\n",(int)thruput);
    fflush(stdout);
  }

  return(0);
}


/*
 * get_domainname  - add domainname to host-string
 * 
 * INPUT:  host	- hostname
 * 
 * OUTPUT: host	- hostname.domainname or hostname (if no domain was found)
 * 
 * This is a ugly hack: we determin the domain by looking at /etc/resolf.conf
 */
void get_domainname(char *host)
{ char line[MAXLEN];
  char *cp, *domain;
  FILE *inf;
  
  inf=fopen("/etc/resolv.conf","r");
  if (!inf) return;
  
  while (fgets(line,MAXLEN-1,inf))
  { str_trim(line);
    if (strbeq(line,"domain") || strbeq(line,"search"))
    { if ((domain=strchr(line,' ')))
      { *domain='.';
	if ((cp=strchr(domain,' '))) *cp=0;
	strcat(host,domain);
	break;
      }
    }
  }
  
  fclose(inf);
}
