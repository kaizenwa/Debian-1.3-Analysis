/*
 Copyright (C) 1994 Rami el Charif, W. Schelter 

This file is part of GNU Common Lisp, herein referred to as GCL

GCL is free software; you can redistribute it and/or modify it under
the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
License for more details.

You should have received a copy of the GNU Library General Public License 
along with GCL; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#define IN_GUIS
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
#include <netinet/in.h>

  
#ifdef PLATFORM_NEXT
# include <bsd/netdb.h>
# include <libc.h>
#else
# include <netdb.h>
# include <arpa/inet.h>
#endif
/* #include <sys/types.h> */

#include <sys/time.h>
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#ifdef __cplusplus
#ifdef PLATFORM_NEXT
extern unsigned long inet_addr( char *cp );
extern char *inet_ntoa ( struct in_addr in );
#endif
}
#endif
#ifdef PLATFORM_LINUX
#include <termios.h>
#endif
#include <errno.h>

#ifdef __svr4__
#include <sys/file.h>
#endif

#ifdef PLATFORM_NEXT /* somehow, this is getting lost... */
#undef bzero
#define bzero(b,len) memset(b,0,len)
#endif


#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

#include "guis.h"


FILE *pstreamDebug = stderr;
int fDebugSockets;


#ifdef PLATFORM_SUNOS
static void notice_input( );
#else
static void notice_input();
#endif

int hdl = -1;

void TkX_Wish ();

pid_t parent;
 
int debug;

#include "comm.c"


/* Start up our Graphical User Interface connecting to 
   NETWORK-ADDRESS on PORT to process PID.  If fourth
   argument WAITING causes debugging flags to be turned
   on and also causes a wait in a loop for WAITING seconds
   (giving a human debugger time to attach to the forked process).
 */



int delay;
int
main(argc, argv)
     int argc;
     char *argv[];
{
  {int i = argc;
   while (--i > 3)
     { if (strcmp(argv[i],"-delay")==0)
	 { delay = atoi(argv[i+1]);}
       if (strcmp(argv[i],"-debug")==0)
	   {debug = 1; fDebugSockets = -1;}
       }
 }
  
  

  if (argc >= 4)
    {
      pid_t p;

      parent = atoi(argv[3]);
      dfprintf(stderr,"guis, parent is : %d\n", parent);

#ifndef linux
      p = fork();
#else
      p = vfork();
#endif
      dfprintf(stderr, "guis, vfork returned : %d\n", p);

      if (p == -1)
	{
	  dfprintf(stderr, "Error !!! vfork failed %d\n", errno);

	  return -1;
	}
      else if (p)
	{
	  dfprintf(stderr, "guis,vforked child : %d\n", p);

	  _exit(p);
/*
	  return p;
*/
	}
      else
	{

#ifndef SET_SESSION_ID	  
#if defined(__svr4__) || defined(ATT) 
#define SET_SESSION_ID() setsid()
#else
#ifdef BSD
#define SET_SESSION_ID() (setpgrp(0,0) ? -1 : 0)
#endif
#endif	  
#endif
	  
	  if (SET_SESSION_ID() == -1)
	    {   dfprintf(stderr, "Error !!! setsid failed : %d\n", errno);
	      }


	    
	  dsfd = sock_connect_to_name(argv[1], atoi(argv[2]), 0);
	  if (dsfd)
	    {
	      dfprintf(stderr, "connected to %s %s"
		      , argv[1], argv[2]);
	      /* give chance for someone to attach with gdb and
		 to set waiting to 0 */
	      while (-- delay >=0) sleep(1);
		
	      
	      {
		char *buf = "\0\0";
		TkX_Wish(argc, argv);
	      }

	      dfprintf(stderr, "Wish shell done\n");

	      sock_close_connection(dsfd);
	      return 0;
	    }
	  else
	    {
	      dfprintf(stderr,
      "Error !!! Can't connect to socket host=%s, port=%s, errno=%d\n"
		      , argv[1], argv[2], errno);
	      fflush(stderr);
	      return -1;
	    }
	}
    }
  else
    {
      dfprintf(stderr, "Error !!! Not enough arguments\n");
      fflush(stderr);
      return -1;
    }
}

struct connection_state *
sock_connect_to_name(host_id,  name, async)
     char *host_id;
     int name;
     int async;
     
{
  struct sockaddr_in addr;
  int fd, n, rc;

  fd = socket( PF_INET, SOCK_STREAM, 0 );

  addr.sin_family = PF_INET;
  addr.sin_port = name;
  addr.sin_addr.s_addr = inet_addr( host_id );
  memset( addr.sin_zero, 0, 8 );
    
  n = sizeof addr;
  rc = connect( fd, (struct sockaddr *)&addr, n );
  if (rc != 0)
    return 0;

  return setup_connection_state(fd);
}

void
sock_close_connection(sfd)
struct connection_state *sfd;     
{
  close( sfd->fd );
  free(sfd->read_buffer);
  free(sfd);
  
}
  

#ifdef PLATFORM_SUNOS
static void
notice_input( int sig, int code, struct sigcontext *s, char *a )
#else
static void
notice_input( sig )
     int sig;
#endif
{
  signal( SIGIO, notice_input );
  dfprintf(stderr, "\nNoticed input!\n" );

}

static int message_id;

int
sock_write_str2( sfd, type, hdr,
		hdrsize,text, length )

struct connection_state *sfd;
enum mtype type;
 char *hdr;
int hdrsize;
 char *text;
int length;
     
{
  char buf[0x1000];
  char *p = buf;
  int cb;
  int m;
  int n_written;
  struct message_header *msg;
  msg = (struct message_header *) buf;

  if (length == 0)
    length = strlen(text);
  m = length + hdrsize;

  msg->magic1=MAGIC1;
  msg->magic2=MAGIC2;
  msg->type = type;
  msg->flag = 0;
  STORE_3BYTES(msg->size,m);
  STORE_3BYTES(msg->msg_id,message_id);
  message_id++;
  p = buf + MESSAGE_HEADER_SIZE;
  bcopy(hdr,p,hdrsize);
  p+= hdrsize;
  
  if (sizeof(buf) >= (length + hdrsize + MESSAGE_HEADER_SIZE))
    { bcopy(text,p,length);
      n_written = write1(sfd,buf,(length + hdrsize + MESSAGE_HEADER_SIZE));
    }
  else
    { n_written = write1(sfd,buf, hdrsize + MESSAGE_HEADER_SIZE);
      n_written += write1(sfd, text, length);
    }

  if (n_written != (length + hdrsize + MESSAGE_HEADER_SIZE))
    {perror("sock_write_str: Did not write full message");
     return -1;}
  return n_written;
  
}


#define READ_BUF_STRING_AVAIL	1
#define READ_BUF_DATA_ON_PORT	2



#define DEFAULT_TIMEOUT_FOR_TK_READ (100 * HZ)



struct message_header *
guiParseMsg1(sfd,buf,bufleng)
  char *buf;
int bufleng;
struct connection_state *sfd;
{ int m;
  int body_length;
  int tot;
  char *p = buf;
  struct message_header *msg;
  msg = (struct message_header *) buf;
  m= read1(sfd,msg,MESSAGE_HEADER_SIZE,DEFAULT_TIMEOUT_FOR_TK_READ);
  if (m == MESSAGE_HEADER_SIZE)
    {
     if ( msg->magic1!=MAGIC1
	 ||  msg->magic2!=MAGIC2)
       { fprintf(stderr,"bad magic..flushing buffers");
	 while(read1(sfd,buf,bufleng,0) > 0);
	 return 0;}
      GET_3BYTES(msg->size,body_length);
      tot = body_length+MESSAGE_HEADER_SIZE;
      if (tot >= bufleng)
         {msg = (void *)malloc(tot+1);
	  bcopy(buf,msg,MESSAGE_HEADER_SIZE);}
     m = read1(sfd,&(msg->body),
		   body_length,DEFAULT_TIMEOUT_FOR_TK_READ);
     if (m == body_length)
       { return msg;}}
  if (m < 0) exit(1);
 fail:
  { static bad_read_allowed=4;
    if (bad_read_allowed-- < 0) exit(1);
  }
    
  dfprintf(stderr,"reading from lisp timed out or not enough read");
  return 0;
}  
      
error(s)
     char *s;
{ fprintf(stderr,"%s",s); abort();
}

write_timeout_error(s)
     char *s;
{ fprintf(stderr,"write timeout: %s",s); abort();
}
connection_failure(s)
     char *s;
{ fprintf(stderr,"connection_failure:%s",s); abort();
}




