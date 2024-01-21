/* -*- mode: C; mode: fold; -*- */
/* This file is a MESS.  For that reason, I wrote sltcp.c which I regard as 
 * much cleaner.  I tried to clean this up but with at least 4 different
 * TCP/IP implementations for VMS as well as TLI and BSD, there is not too
 * much hope.  The routines in sltcp.c should be adequate for most Unix 
 * systems as well as OS/2.  Until VMS standardizes its TCP/IP interface
 * the routines in this file will have to be used.
 * 
 * Much of this file has been totally re-written.  Now all read/writes are 
 * to a file descriptor.  All reads take place via the function 
 * 'client_read' and all writes are done in 'client_write'.  These functions
 * call the appropriate low level functions to perform the actual operation.
 * Specifically, client_read calls:
 *  
 *     socket_read (MULTINET)
 *     do_netlib_read  (NETLIB)   [Note: this function needs to be created.]
 *     read   (everything else)
 * 
 * and client_write calls:
 * 
 *     netlib_write (NETLIB)
 *     socket_write (MULTINET)
 *     write  (everything else)
 * 
 * JED.
 */

/*
 * This software is Copyright 1991 by Stan Barber.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction or this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made.
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk.
 *
 */


#if 0
# define DEBUG_FILE "slrn.log" 
#endif

#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <sys/types.h>

#ifdef VMS
/*{{{ VMS includes */
# include "vms.h"
# ifdef MULTINET
#  include "multinet_root:[multinet.include.vms]inetiodef.h"
#  include "multinet_root:[multinet.include.sys]types.h"
# else
#  if NETLIB
#   include <descrip.h>
#   include <types.h>
#   include <iodef.h>
#   include <stsdef.h>
#   include "netlib_dir:netlibdef.h"
#   define NNTP_PORT 	119
#   define INIT_SDESC(dsc, len, ptr) {(dsc).dsc$b_dtype = DSC$K_DTYPE_T;\
     (dsc).dsc$b_class = DSC$K_CLASS_S; (dsc).dsc$w_length = (len);\
     (dsc).dsc$a_pointer = (ptr);}
#  else
#   include <types.h>
#   include <iodef.h>
#   define	NNTP_PORT 119
#  endif
# endif
/*}}}*/
#endif

#include <ctype.h>


#ifdef TLI
/*{{{ TLI includes */
# include	<fcntl.h>
# include	<tiuser.h>
# include	<stropts.h>
# include	<sys/socket.h>
# define SLRN_LOADED_SYS_SOCKET_H
# ifdef WIN_TCP
#  include	<sys/in.h>
# else
#  include	<netinet/in.h>
#  define SLRN_LOADED_NETINET_IN_H
# endif
# define	IPPORT_NNTP	((unsigned short) 119)
# include 	<netdb.h>	/* All TLI implementations may not have this */
/*}}}*/
#else /* !TLI */
# ifdef VMS
/*{{{ More VMS includes */
#  ifdef MULTINET
#   include "multinet_root:[multinet.include]netdb.h"
#   include "multinet_root:[multinet.include.sys]socket.h"
#   include "multinet_root:[multinet.include.netinet]in.h"
#   define SLRN_LOADED_NETINET_IN_H
#   define SLRN_LOADED_SYS_SOCKET_H
#  endif
#  ifdef NETLIB
#   include <descrip.h>
#   include "netlib_dir:netlibdef.h"
#  endif
#  ifdef UCX
#   include <netdb.h>
#   include <socket.h>
#   include <in.h>
#   include <inet.h>
#  endif
/*}}}*/
# else /* !VMS */
#  include <sys/socket.h>
#  include <netinet/in.h>
#  define SLRN_LOADED_NETINET_IN_H
#  define SLRN_LOADED_SYS_SOCKET_H
#  ifndef EXCELAN
#   include <netdb.h>
#  endif /* !EXCELAN */
# endif /* !VMS */
#endif /* !TLI */
 

#ifdef HAVE_SYS_SOCKET_H
# ifndef SLRN_LOADED_SYS_SOCKET_H
#  include <sys/socket.h>
#  define SLRN_LOADED_SYS_SOCKET_H
# endif
#endif

#ifdef HAVE_NETINET_IN_H
# ifndef SLRN_LOADED_NETINET_IN_H
#  include <netinet/in.h>
#  define SLRN_LOADED_NETINET_IN_H
# endif
#endif

#ifdef HAVE_ARPA_INET_H
# ifndef SLRN_LOADED_ARPA_INET_H
#  include <arpa/inet.h>
#  define SLRN_LOADED_ARPA_INET_H
# endif
#endif

#ifdef __unix__
# ifdef MULTINET
#  undef MULTINET
# endif
# ifdef NETLIB
#  undef NETLIB
# endif
#endif

#ifdef EXCELAN
/*{{{ EXCELAN defines */
# define	NNTP_PORT	((unsigned short) 119)
# if __STDC__
int connect(int, struct sockaddr *);
unsigned short htons(unsigned short);
unsigned long rhost(char **);
int rresvport( int );
int socket( int, struct sockproto *, struct sockaddr_in *, int );
# endif
/*}}}*/
#endif

#ifdef DECNET
# include <netdnet/dn.h>
# include <netdnet/dnetdb.h>
#endif /* DECNET */

#include "clientlib.h"

#include "server.h"
#include "misc.h"

#ifndef NNTP_PORT
# define NNTP_PORT 119
#endif


#ifdef NEEDS_EXTERN_DECLARATIONS
# define DECLARE_EXTERN(x) extern x;
#else 
# define DECLARE_EXTERN(x)
#endif


#ifdef NETLIB
static void *Socket_Fd;
#else
static int Socket_Fd;
#endif

#ifdef DEBUG_FILE
static FILE *Debug_Fp;
#endif

static int Server_Has_Been_Closed = 1;


static void client_close_server(void);

static int get_tcp_socket(char *, int);
static void reset_read_buffer (void);

/*{{{ client_server_init_1 */
/*
 * client_server_init  Get a connection to the remote news server.
 *
 *	Parameters:	"machine" is the machine to connect to.
 *
 *	Returns:	-1 on error
 *			server's initial response code on success.
 *
 *	Side effects:	Connects to server.
 */

static int client_server_init_1 (char *machine, int port)
{
   int	sockt_rd;
#ifdef DECNET
   char	*cp;
   
   cp = slrn_strchr(machine, ':');
   
   if (cp && cp[1] == ':') 
     {
	*cp = '\0';
	sockt_rd = get_dnet_socket(machine);
     }
   else
     sockt_rd = get_tcp_socket(machine, port);
#else
   sockt_rd = get_tcp_socket (machine, port);
#endif
   
   if (sockt_rd < 0)
     return (-1);

#ifndef NETLIB
   Socket_Fd = sockt_rd;
#endif
   
   /* Now get the server's signon message */
#ifdef DEBUG_FILE
   Debug_Fp = fopen (DEBUG_FILE, "w");
#endif
   
   return 0;
}


/*}}}*/

/*{{{ get_tcp_socket */
/*
 * get_tcp_socket -- get us a socket connected to the news server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *
 *	Returns:	Socket connected to the news server if
 *			all is ok, else -1 on error.
 *
 *	Side effects:	Connects to server.
 *
 *	Errors:		Printed via perror.
 */

static int get_tcp_socket(char *machine, int nntpport)
{
#ifndef NETLIB
   int	s = -1;
   struct	sockaddr_in s_in;
#endif
#ifdef TLI 
   char 	*t_alloc();
   struct	t_call	*callptr;
   /*
    * Create a TCP transport endpoint.
    */
   if ((s = t_open("/dev/tcp", O_RDWR, (struct t_info*) 0)) < 0)
     {
	t_error("t_open: can't t_open /dev/tcp");
	return(-1);
     }
   if(t_bind(s, (struct t_bind *)0, (struct t_bind *)0) < 0)
     {
	t_error("t_bind");
	t_close(s);
	return(-1);
     }
   memset ((char *) &s_in, 0, sizeof(s_in));
   s_in.sin_family = AF_INET;

   if (nntpport < 0) nntpport = NNTP_PORT;
   s_in.sin_port = htons((unsigned short) nntpport);
   
   if (!isdigit(*machine) ||
       (long)(s_in.sin_addr.s_addr = inet_addr(machine)) == -1)
     {
	struct	hostent *gethostbyname(), *hp;
	if((hp = gethostbyname(machine)) == NULL)
	  {
	     fprintf(stderr,"gethostbyname: %s: host unknown\n",
		     machine);
	     t_close(s);
	     return(-1);
	  }
	memcpy ((char *) &s_in.sin_addr, hp->h_addr, hp->h_length);
     }
   /*
    * Allocate a t_call structure and initialize it.
    * Let t_alloc() initialize the addr structure of the t_call structure.
    */
   if ((callptr = (struct t_call *) t_alloc(s,T_CALL,T_ADDR)) == NULL)
     {
	t_error("t_alloc");
	t_close(s);
	return(-1);
     }
   
   callptr->addr.maxlen = sizeof(s_in);
   callptr->addr.len = sizeof(s_in);
   callptr->addr.buf = (char *) &s_in;
   callptr->opt.len = 0;			/* no options */
   callptr->udata.len = 0;			/* no user data with connect */
   
   /*
    * Connect to the server.
    */
   if (t_connect(s, callptr, (struct t_call *) 0) < 0) 
     {
	t_error("t_connect");
	t_close(s);
	return(-1);
     }
   
   /*
    * Now replace the timod module with the tirdwr module so that
    * standard read() and write() system calls can be used on the
    * descriptor.
    */
   
   if (ioctl(s,  I_POP,  (char *) 0) < 0)
     {
	perror("I_POP(timod)");
	t_close(s);
	return(-1);
     }
   
   if (ioctl(s,  I_PUSH, "tirdwr") < 0)
     {
	perror("I_PUSH(tirdwr)");
	t_close(s);
	return(-1);
     }
   
#else /* !TLI */
# ifdef NETLIB
   unsigned short port, nntp_port;
   struct dsc$descriptor ndsc, dsc;
   struct INADDRDEF addr;
   struct SINDEF s_in;
   unsigned int rc, size;
   
   if (nntpport < 0) nntpport = NNTP_PORT;
   nntp_port = (unsigned short) nntpport;
   port = netlib_hton_word(&nntp_port);
   
   rc = netlib_socket(&Socket_Fd);
   if (!$VMS_STATUS_SUCCESS(rc))
     return(-1);
   if (isdigit(*machine))
     {
	INIT_SDESC(dsc, strlen(machine), machine);
	rc = netlib_strtoaddr(&dsc, &addr);
	if (!$VMS_STATUS_SUCCESS(rc))
	  return(-1);
	s_in.sin_w_family = NETLIB_K_AF_INET;
	s_in.sin_w_port = netlib_hton_word(&nntp_port);
	s_in.sin_x_addr.inaddr_l_addr = addr.inaddr_l_addr;
	memset(&s_in.sin_x_mbz, 0, 8);
	size = sizeof(struct SINDEF);
	rc = netlib_connect(&Socket_Fd, &s_in, &size, 0,0,0);
	if (!$VMS_STATUS_SUCCESS(rc))
	  {
	     netlib_close(&Socket_Fd);
	     return(-1);
	  }
     }
   else
     {
	INIT_SDESC(ndsc, strlen(machine), machine);
	rc = netlib_connect_by_name(&Socket_Fd, &ndsc, &nntp_port);
	if (!$VMS_STATUS_SUCCESS(rc))
	  {
	     netlib_close(&Socket_Fd);
	     return(-1);
	  }
     }
   return(0);
# else
#  ifndef EXCELAN
   struct	servent *sp;
   struct	hostent *hp;
#   ifdef VMS
#   else
   DECLARE_EXTERN(struct servent *getservbyname())
   DECLARE_EXTERN(struct hostent *gethostbyname())
#   endif
#   ifdef h_addr
   int	x = 0;
   register char **cp;
   static char *alist[1];
#   endif /* h_addr */
   static struct hostent def;
   static struct in_addr defaddr;
   static char namebuf[ 256 ];
   DECLARE_EXTERN(unsigned long inet_addr())
#   ifndef UCX
   struct servent sp_buffer;
   
   if (nntpport >= 0)
     {
	sp = &sp_buffer;
	memset ((char *) sp, 0, sizeof (struct servent));
	sp->s_port = htons((unsigned short)nntpport);
     }
   else 
     {
	if ((sp = getservbyname("nntp", "tcp")) ==  NULL) 
	  {
	     fprintf(stderr, "nntp/tcp: Unknown service.\n");
	     return (-1);
	  }
     }
#   else
   if (nntpport < 0) nntpport = NNTP_PORT;
   sp = (struct servent *)malloc(sizeof(struct servent));
   sp->s_port = htons((unsigned short)nntpport);
#   endif
   /* If not a raw ip address, try nameserver */
   if (!isdigit(*machine) ||
       (long)(defaddr.s_addr = inet_addr(machine)) == -1)
     hp = gethostbyname(machine);
   else 
     {
	/* Raw ip address, fake  */
	(void) strcpy(namebuf, machine);
	def.h_name = namebuf;
#   ifdef h_addr
	def.h_addr_list = alist;
#   endif
	def.h_addr = (char *)&defaddr;
	def.h_length = sizeof(struct in_addr);
	def.h_addrtype = AF_INET;
	def.h_aliases = 0;
	hp = &def;
     }
   if (hp == NULL) 
     {
	fprintf(stderr, "%s: Unknown host.\n", machine);
	return (-1);
     }
   
   memset ((char *) &s_in, 0, sizeof(s_in));
   s_in.sin_family = hp->h_addrtype;
   s_in.sin_port = sp->s_port;
#  else /* EXCELAN */
   memset ((char *) &s_in, 0, sizeof(s_in));
   s_in.sin_family = AF_INET;
#  endif /* EXCELAN */
   
   /*
    * The following is kinda gross.  The name server under 4.3
    * returns a list of addresses, each of which should be tried
    * in turn if the previous one fails.  However, 4.2 hostent
    * structure doesn't have this list of addresses.
    * Under 4.3, h_addr is a #define to h_addr_list[0].
    * We use this to figure out whether to include the NS specific
    * code...
    */
   
#  ifdef	h_addr
   
   /* get a socket and initiate connection -- use multiple addresses */
   
   for (cp = hp->h_addr_list; cp && *cp; cp++) 
     {
	s = socket(hp->h_addrtype, SOCK_STREAM, 0);
	if (s < 0) 
	  {
	     perror("socket");
	     return (-1);
	  }
	memcpy ((char *)&s_in.sin_addr, *cp, hp->h_length);
	
	if (x < 0)
	  fprintf(stderr, "trying %s\n",
		  (char *) inet_ntoa(s_in.sin_addr));
	x = connect(s, (struct sockaddr *)&s_in, sizeof (s_in));
	if (x == 0)
	  break;
	fprintf(stderr, "connection to %s: ",
		(char *) inet_ntoa(s_in.sin_addr));
	perror("");
	(void) close(s);
     }
   if (x < 0) 
     {
	fprintf(stderr, "giving up...\n");
	return (-1);
     }
#  else	/* no name server */
#   ifdef EXCELAN
   if ((s = socket(SOCK_STREAM,(struct sockproto *)NULL,&s_in,SO_KEEPALIVE)) < 0)
     {
	/* Get the socket */
	perror("socket");
	return (-1);
     }
   memset ((char *) &s_in, 0, sizeof(s_in));
   s_in.sin_family = AF_INET;
   if (nntpport < 0) nntpport = NNTP_PORT;
   s_in.sin_port = htons((unsigned short) nntpport);
   /* set up addr for the connect */
   
   if ((s_in.sin_addr.s_addr = rhost(&machine)) == -1) 
     {
	fprintf(stderr, "%s: Unknown host.\n", machine);
	return (-1);
     }
   /* And then connect */
   
   if (connect(s, (struct sockaddr *)&s_in) < 0) 
     {
	perror("connect");
	(void) close(s);
	return (-1);
     }
#   else /* not EXCELAN */
   if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) 
     {
	perror("socket");
	return (-1);
     }
   
   /* And then connect */
   
   memcpy ((char *) &s_in.sin_addr, hp->h_addr, hp->h_length);
   if (connect(s, (struct sockaddr *) &s_in, sizeof(s_in)) < 0) 
     {
	perror("connect");
	(void) close(s);
	return (-1);
     }
   
#   endif /* !EXCELAN */
#  endif /* !h_addr */
# endif /* !NETLIB */
#endif /* !TLI */
   
#ifndef NETLIB
   return (s);
#endif
}

/*}}}*/

#ifdef DECNET
/*{{{ get_dnet_socket */
/*
 * get_dnet_socket -- get us a socket connected to the news server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *
 *	Returns:	Socket connected to the news server if
 *			all is ok, else -1 on error.
 *
 *	Side effects:	Connects to server.
 *
 *	Errors:		Printed via nerror.
 */

int get_dnet_socket (char *machine)
{
   int	s, area, node;
   struct	sockaddr_dn sdn;
   struct	nodeent *getnodebyname(), *np;
   
   memset ((char *) &sdn, 0, sizeof(sdn));
   
   switch (s = sscanf( machine, "%d%*[.]%d", &area, &node )) 
     {
      case 1:
	node = area;
	area = 0;
      case 2:
	node += area*1024;
	sdn.sdn_add.a_len = 2;
	sdn.sdn_family = AF_DECnet;
	sdn.sdn_add.a_addr[0] = node % 256;
	sdn.sdn_add.a_addr[1] = node / 256;
	break;
      default:
	if ((np = getnodebyname(machine)) == NULL) 
	  {
	     fprintf(stderr,
		     "%s: Unknown host.\n", machine);
	     return (-1);
	  }
	else 
	  {
	     memcpy((char *) sdn.sdn_add.a_addr, np->n_addr,
		   np->n_length);
	     sdn.sdn_add.a_len = np->n_length;
	     sdn.sdn_family = np->n_addrtype;
	  }
	break;
     }
   sdn.sdn_objnum = 0;
   sdn.sdn_flags = 0;
   sdn.sdn_objnamel = strlen("NNTP");
   memcpy (&sdn.sdn_objname[0], "NNTP", sdn.sdn_objnamel);
   
   if ((s = socket(AF_DECnet, SOCK_STREAM, 0)) < 0) 
     {
	nerror("socket");
	return (-1);
     }
   
   /* And then connect */
   
   if (connect(s, (struct sockaddr *) &sdn, sizeof(sdn)) < 0) 
     {
	nerror("connect");
	close(s);
	return (-1);
     }
   
   return (s);
}
/*}}}*/
#endif

/*{{{ Low-Level Read Routines */

#define TIMEOUT 60
#define BUFSIZE 8192

static unsigned char Read_Buffer [BUFSIZE];
static unsigned char *Read_Buffer_Ptr = Read_Buffer;
static unsigned char *Read_Buffer_Ptr_Max = Read_Buffer;

static void reset_read_buffer (void)
{
   Read_Buffer_Ptr_Max = Read_Buffer_Ptr = Read_Buffer;
}

static void cancel_read (int sig_unused)
{
   (void) sig_unused;
   slrn_error ("CANCEL_READ: NNTP read failed.");
}


static int client_read (void)
{
   int nb;
#if TIMEOUT
   void (*old_sigalrm)(int);
#endif
   
   reset_read_buffer ();
#if TIMEOUT
   old_sigalrm = signal (SIGALRM, cancel_read);
   alarm (TIMEOUT);
#endif
   
#ifdef NETLIB
   nb = do_netlib_read (Socket_Fd, Read_Buffer, BUFSIZE);
#else
# ifdef MULTINET
   nb = socket_read (Socket_Fd, Read_Buffer, BUFSIZE);
# else
   while (-1 == (nb = read (Socket_Fd, Read_Buffer, BUFSIZE)))
     {
#  ifdef EINTR
	if (errno == EINTR) continue;
#  endif
	break;
     }   
# endif				       /* MULTINET */
#endif				       /* NETLIB */
   
#if TIMEOUT
   alarm(0);
   (void) signal (SIGALRM, old_sigalrm);
#endif
   
   if (nb <= 0)
     return -1;
   
   Read_Buffer_Ptr_Max = Read_Buffer + nb;
   
   return 0;
}


static int client_fgets (char *buf, int len)
{
   unsigned char *r, *rmax;
   char *buf_max;
   
   if (Server_Has_Been_Closed) return -1;
   
   if (len == 0)
     {
	*buf = 0;
	return 0;
     }
   
   buf_max = buf + (len - 1);	       /* allow room for \0 */
   
   while (1)
     {
	r = Read_Buffer_Ptr;
	rmax = Read_Buffer_Ptr_Max;
   
	while (r < rmax)
	  {
	     unsigned char ch;
	     
	     if (buf == buf_max)
	       {
		  *buf = 0;
		  Read_Buffer_Ptr = r;
		  return 0;
	       }
	     
	     ch = *r++;
	     *buf++ = (char) ch;

	     if (ch == '\n')
	       {
		  *buf = 0;
		  Read_Buffer_Ptr = r;
		  return 0;
	       }

	  }
	     
	if (-1 == client_read ())
	  return -1;
     }
}

/*}}}*/


static void client_close_server(void) /*{{{*/
{
#if defined(VMS) && !defined(NETLIB)
   void (*save_sigalrm)(int);
#endif
   
   if (Server_Has_Been_Closed) return;

   if (
#ifdef NETLIB
       (Socket_Fd == NULL)
#else
       (Socket_Fd < 0)
#endif
       )
     {
	Server_Has_Been_Closed = 1;
	return;
     }
   
#ifdef NETLIB
   netlib_close (&Socket_Fd);
   Socket_Fd = NULL;
#else
# ifdef MULTINET
   save_sigalrm = signal (SIGALRM, cancel_read);
   alarm (10);
   socket_close (Socket_Fd);
   alarm (0);
   signal (SIGALRM, save_sigalrm);
# else
   close (Socket_Fd);
# endif				       /* MULTINET */
   Socket_Fd = -1;
#endif				       /* NETLIB */
}

/*}}}*/

static unsigned int client_write (char *buf, unsigned int len)
{
   int n;
   unsigned int total;
   
#ifdef NETLIB
   struct dsc$descriptor dsc;
   struct NETLIBIOSBDEF iosb;
   int rc;
   
   INIT_SDESC(dsc, len, buf);
   netlib_write(&Socket_Fd, &dsc, 0,0, &iosb,0,0);
   if ($VMS_STATUS_SUCCESS(rc))
     total = iosb.iosb_w_count;
   else total = 0;
#else
# ifdef MULTINET
   total = socket_write (Socket_Fd, buf, len);
# else
   total = 0;
   while (len != 0)
     {
	n = write (Socket_Fd, buf + total, len);
	if (n == -1)
	  {
#  ifdef EAGAIN
	     if (errno == EAGAIN) 
	       {
		  sleep (1);
		  continue;
	       }
#  endif
#  ifdef EWOULDBLOCK
	     if (errno == EWOULDBLOCK)
	       {
		  sleep (1);
		  continue;
	       }
#  endif
#  ifdef EINTR
	     if (errno == EINTR)
	       continue;
#  endif
	     break;
	  }
	len -= n;
	total += n;
     }
# endif				       /* MULTINET */
#endif				       /* NETLIB */
   
#ifdef DEBUG_FILE
   if ((Debug_Fp != NULL) && (total != 0))
     {
	fputs (buf, Debug_Fp);
	fflush (Debug_Fp);
     }
#endif
   
   return total;
}


static int client_fputs (char *s)
{
   unsigned int len = strlen (s);
   
   if (len != client_write (s, len))
     return -1;
   
   return 0;
}

static int client_vprintf (char *fmt, va_list ap)
{
   char buf [8192];
   vsprintf (buf, fmt, ap);
   return client_fputs (buf);
}

static int client_server_init (char *machine, int port)
{
   if (Server_Has_Been_Closed == 0) 
     client_close_server ();

   reset_read_buffer ();
   
   if (-1 == client_server_init_1 (machine, port))
     return -1;
   
   Server_Has_Been_Closed = 0;
   return 0;
}


static int client_flush_output (void)
{
   return 0;
}


static SLTCP_Type SLTCP_Buffer;

int sltcp_fputs (SLTCP_Type *s, char *buf)
{
   if (s == NULL) return -1;
   return client_fputs (buf);
}

int sltcp_vfprintf (SLTCP_Type *s, char *fmt, va_list ap)
{
   if (s == NULL) return -1;
   return client_vprintf (fmt, ap);
}

extern int sltcp_fgets (SLTCP_Type *s, char *buf, unsigned int len)
{
   if (s == NULL) return -1;
   return client_fgets (buf, (int) len);
}

extern int sltcp_flush_output (SLTCP_Type *s)
{
   if (s == NULL) return -1;
   return client_flush_output ();
}

extern int sltcp_close (SLTCP_Type *s)
{
   if (s == NULL) return -1;
   
   (void) client_flush_output ();
   (void) client_close_server ();

   SLTCP_Buffer.tcp_fd = -1;
   
   return 0;
}


extern unsigned int sltcp_write (SLTCP_Type *s, char *buf, unsigned int len)
{
   if (s == NULL) return 0;
   
   return client_write (buf, len);
}


extern SLTCP_Type *sltcp_open_connection (char *host, int port)
{
   if (-1 == client_server_init (host, port))
     return NULL;
   
   SLTCP_Buffer.tcp_fd = 1;
   return &SLTCP_Buffer;
}

extern int sltcp_map_service_to_port (char *srv)
{
   (void) srv;
   return NNTP_PORT;
}

extern int sltcp_close_socket (SLTCP_Type *s)
{
   if (s == NULL) return -1;
   client_close_server ();
   
   SLTCP_Buffer.tcp_fd = -1;

   return 0;
}

int (*SLTCP_Interrupt_Hook) (void);
