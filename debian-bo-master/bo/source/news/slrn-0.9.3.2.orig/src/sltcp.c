/* -*- mode: C; mode: fold; -*- */
/*  Copyright (c) 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"

/*{{{ Include Files */

#include <stdio.h>
#include <string.h>

#include <stdlib.h>
#include <unistd.h>

#include <errno.h>
#include <ctype.h>
#include <stdarg.h>

#include <setjmp.h>
#include <signal.h>

#include <sys/types.h>

#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif

#include <netdb.h>

#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif

#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif

#ifndef h_errno
extern int h_errno;
#endif

/* For select system call */
#ifndef VMS
# include <sys/time.h>
# if defined(__QNX__) || defined(__os2__)
#  include <sys/select.h>
# endif
# if defined (_AIX) && !defined (FD_SET)
#  include <sys/select.h>	/* for FD_ISSET, FD_SET, FD_ZERO */
# endif

# ifndef FD_SET
#  define FD_SET(fd, tthis) *(tthis) = 1 << (fd)
#  define FD_ZERO(tthis)    *(tthis) = 0
#  define FD_ISSET(fd, tthis) (*(tthis) & (1 << fd))
 typedef int fd_set;
# endif
#endif

#include <slang.h>

#include "sltcp.h"

/*}}}*/

#ifdef __BEOS__
# define SLTCP_CLOSE(x)		closesocket(x)
# define SLTCP_READ(x,y,z)	recv((x),(y),(z),0)
# define SLTCP_WRITE(x,y,z)	send((x),(y),(z),0)
#else
# define SLTCP_CLOSE(x)		close(x)
# define SLTCP_READ(x,y,z)	read((x),(y),(z))
# define SLTCP_WRITE(x,y,z)	write((x),(y),(z))
#endif

int (*SLTCP_Interrupt_Hook) (void);
int SLtcp_TimeOut_Secs = 120;

static int TCP_Verbose_Reporting = 0;

static int sys_call_interrupted_hook (void) /*{{{*/
{
   if (SLTCP_Interrupt_Hook == NULL)
     return 0;
   
   return (*SLTCP_Interrupt_Hook) ();
}

/*}}}*/

/* This function attempts to make a connection to a specified port on an
 * internet host.  It returns a socket descriptor upon success or -1
 * upon failure.
 */
static int get_tcp_socket_1 (char *host, int port) /*{{{*/
{
   char **h_addr_list;
   /* h_addr_list is NULL terminated if h_addr is defined.  If h_addr
    * is not defined, h_addr is the only element in the list.  When
    * h_addr is defined, its value is h_addr_list[0].
    */
   int h_length;
   int h_addr_type;
   char *fake_h_addr_list[2];
   unsigned long fake_addr;
   struct sockaddr_in s_in;
   int s;
   int not_connected;
   
   /* If it does not look like a numerical address, use nameserver */
   if (!isdigit(*host) || (-1L == (long)(fake_addr = inet_addr (host))))
     {
	struct hostent *hp;
	unsigned int max_retries = 3;
	
	while (NULL == (hp = gethostbyname (host)))
	  {
#ifdef TRY_AGAIN
	     max_retries--;
	     if (max_retries && (h_errno == TRY_AGAIN))
	       {
		  sleep (1);
		  continue;
	       }
#endif
	     fprintf(stderr, "%s: Unknown host.\n", host);
	     return -1;
	  }
	
#ifdef h_addr
	h_addr_list = hp->h_addr_list;
#else
	h_addr_list = fake_h_addr_list;
	h_addr_list [0] = hp->h_addr;
	h_addr_list [1] = NULL;
#endif
	h_length = hp->h_length;
	h_addr_type = hp->h_addrtype;
     }
   else 
     {
	h_addr_list = fake_h_addr_list;
	h_addr_list [0] = (char *) &fake_addr;
	h_addr_list [1] = NULL;
	
	h_length = sizeof(struct in_addr);
	h_addr_type = AF_INET;
     }

   memset ((char *) &s_in, 0, sizeof(s_in));
   s_in.sin_family = h_addr_type;
   s_in.sin_port = htons((unsigned short) port);

   if (-1 == (s = socket (h_addr_type, SOCK_STREAM, 0)))
     {
	perror("socket");
	return -1;
     }
	
   not_connected = -1;
   
   while (not_connected 
	  && (h_addr_list != NULL) 
	  && (*h_addr_list != NULL))
     {
	char *this_host;
	
	memcpy ((char *) &s_in.sin_addr, *h_addr_list, h_length);

	this_host = (char *) inet_ntoa (s_in.sin_addr);
	
	if (TCP_Verbose_Reporting) fprintf (stderr, "trying %s\n", this_host);
	
	not_connected = connect (s, (struct sockaddr *)&s_in, sizeof (s_in));
	
	if (not_connected == -1)
	  {
#ifdef EINTR
	     if (errno == EINTR) /* If interrupted, try again. */
	       {
		  if (0 == sys_call_interrupted_hook ())
		    continue;
	       }
#endif
	     fprintf (stderr, "connection to %s: ", (char *) this_host);
	     perror ("");
	  }
	h_addr_list++;
     }
   
   if (not_connected) 
     {
	fprintf(stderr, "Unable to make connection. Giving up.\n");
	(void) SLTCP_CLOSE (s);
	return -1;
     }
   return s;
}

/*}}}*/

#if HAVE_SIGLONGJMP
/*{{{ get_tcp_socket routines with sigsetjmp/siglongjmp */

static void restore_sigint_handler (void (*f)(int), int call_it) /*{{{*/
{
   if (f == SIG_ERR)
     return;
   
   (void) SLsignal_intr (SIGINT, f);

   if (call_it)
     kill (getpid (), SIGINT);
}

/*}}}*/

   
static sigjmp_buf Sigint_Jmp_Buf;

static void (*Old_Sigint_Handler) (int);
static volatile int Jump_In_Progress;

static void sigint_handler (int sig) /*{{{*/
{
   (void) sig;
   
   if (Jump_In_Progress) return;
   Jump_In_Progress = 1;

   siglongjmp (Sigint_Jmp_Buf, 1);
}

/*}}}*/

static int get_tcp_socket (char *host, int port) /*{{{*/
{
   int fd;

   Old_Sigint_Handler = SIG_ERR;
   
   Jump_In_Progress = 1;	       /* dont allow jump yet */
   if (0 != sigsetjmp (Sigint_Jmp_Buf, 1))   /* save signal mask */
     {
	restore_sigint_handler (Old_Sigint_Handler, 1);
	sys_call_interrupted_hook ();
	return -1;
     }
   
   Old_Sigint_Handler = SLsignal_intr (SIGINT, sigint_handler);
   
   if ((Old_Sigint_Handler == SIG_IGN) 
       || (Old_Sigint_Handler == SIG_DFL))
     {
	restore_sigint_handler (Old_Sigint_Handler, 0);
	Old_Sigint_Handler = SIG_ERR;
     }
   
   Jump_In_Progress = 0;	       /* now allow the jump */
   fd = get_tcp_socket_1 (host, port);
   Jump_In_Progress = 1;	       /* don't allow jump */
   
   restore_sigint_handler (Old_Sigint_Handler, 0);
   
   return fd;
}

/*}}}*/

/*}}}*/
#else
static int get_tcp_socket (char *host, int port) /*{{{*/
{
   return get_tcp_socket_1 (host, port);
}

/*}}}*/
#endif	   			       /* HAVE_SIGLONGJMP */
	   
SLTCP_Type *sltcp_open_connection (char *host, int port) /*{{{*/
{
   int fd;
   SLTCP_Type *tcp;
   
   tcp = (SLTCP_Type *) SLMALLOC (sizeof (SLTCP_Type));
   if (tcp == NULL)
     {
	fprintf (stderr, "Memory Allocation Failure.\n");
	return NULL;
     }
   memset ((char *) tcp, 0, sizeof (SLTCP_Type));
   
   tcp->tcp_fd = -1;
   tcp->tcp_read_ptr = tcp->tcp_read_ptr_max = tcp->tcp_read_buf;
   tcp->tcp_write_ptr = tcp->tcp_write_ptr_min = tcp->tcp_write_buf;
   
   fd = get_tcp_socket (host, port);
   if (fd == -1) 
     {
	SLFREE (tcp);
	return NULL;
     }

   tcp->tcp_fd = fd;
   
   return tcp;
}

/*}}}*/

static unsigned int do_write (SLTCP_Type *tcp, unsigned char *buf, unsigned int len) /*{{{*/
{
   unsigned int total;
   int nwrite;
   int fd;
   
   total = 0;
   fd = tcp->tcp_fd;
   
   while (total != len)
     {
	nwrite = SLTCP_WRITE (fd, (char *) buf, (len - total));
	
	if (nwrite == -1)
	  {
#ifdef EAGAIN
	     if (errno == EAGAIN) 
	       {
		  sleep (1);
		  continue;
	       }
#endif
#ifdef EWOULDBLOCK
	     if (errno == EWOULDBLOCK)
	       {
		  sleep (1);
		  continue;
	       }
#endif
#ifdef EINTR
	     if (errno == EINTR) 
	       {
		  if (-1 == sys_call_interrupted_hook ())
		    {
		       return total;
		    }
		  continue;
	       }
#endif
	     break;
	  }
	
	total += (unsigned int) nwrite;
     }
   
   tcp->bytes_out += total;

   return total;
}

/*}}}*/

int sltcp_flush_output (SLTCP_Type *tcp) /*{{{*/
{
   int fd;
   unsigned char *buf;
   unsigned int nwrite;
   unsigned int n;

   errno = 0;

   if ((tcp == NULL) || (-1 == (fd = tcp->tcp_fd)))
     return -1;
   
   buf = tcp->tcp_write_ptr_min;
   n = (unsigned int) (tcp->tcp_write_ptr - buf);
   
   nwrite = do_write (tcp, buf, n);
   
   if (nwrite != n)
     {
	tcp->tcp_write_ptr_min += nwrite;
	return -1;
     }
   
   tcp->tcp_write_ptr_min = tcp->tcp_write_ptr = tcp->tcp_write_buf;
   return 0;
}

/*}}}*/

int sltcp_close_socket (SLTCP_Type *tcp)
{
   int status;
   
   if ((tcp == NULL)
       || (tcp->tcp_fd == -1))
     return 0;
   
   status = 0;
   while (-1 == SLTCP_CLOSE (tcp->tcp_fd))
     {
	if (errno != EINTR)
	  {
	     status = -1;
	     break;
	  }
	
	if (-1 == sys_call_interrupted_hook ())
	  return -1;
     }
   tcp->tcp_write_ptr = tcp->tcp_write_ptr_min = tcp->tcp_write_buf;
   tcp->tcp_read_ptr_max = tcp->tcp_read_ptr = tcp->tcp_read_buf;
   tcp->tcp_fd = -1;
   return status;
}

int sltcp_close (SLTCP_Type *tcp) /*{{{*/
{
   errno = 0;

   if (tcp == NULL) return -1;
   
   if (-1 != tcp->tcp_fd)
     {
	if (-1 == sltcp_flush_output (tcp))
	  return -1;

	if (-1 == sltcp_close_socket (tcp))
	  return -1;
     }
   
   SLFREE (tcp);
   return 0;
}

/*}}}*/

static int wait_for_input (int fd) /*{{{*/
{
#ifndef VMS
   fd_set fds;
   struct timeval tv;
   
   if (fd == -1)
     return -1;
   
   while (1)
     {
	int ret;
	
	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	tv.tv_sec = SLtcp_TimeOut_Secs;
	tv.tv_usec = 0;

	ret = select(fd + 1, &fds, NULL, NULL, &tv);
	
	if (ret > 0)
	  return 0;
	
	if (ret == 0)
	  return -1;		       /* timed out */
	
	if (errno == EINTR)
	  {
	     if (-1 == sys_call_interrupted_hook ())
	       return -1;
	     
	     continue;
	  }
	
	return -1;
     }
#endif
}
/*}}}*/

static int do_read (SLTCP_Type *tcp) /*{{{*/
{   
   int nread, fd;
   
   if (tcp->tcp_flags & SLTCP_EOF_FLAG)
     return -1;

   fd = tcp->tcp_fd;
   
#ifndef VMS
   /* The wait_for_input function call is probably not necessary.  The reason
    * that I am using it is that select will return EINTR if interrupted by
    * a signal and most implementations will not restart it.  In any case, 
    * slrn attempts to set signals to NOT restart system calls.  In such a 
    * case, the read below can be interrupted.
    */
   if (-1 == wait_for_input (fd))
     return -1;
#endif

   while (-1 == (nread = SLTCP_READ (fd, (char *)tcp->tcp_read_ptr, SLTCP_BUF_SIZE)))
     {
	if (errno == EINTR)
	  {
	     if (-1 == sys_call_interrupted_hook ())
	       return -1;
	     continue;
	  }
	return -1;
     }
   
   if (nread == 0)
     tcp->tcp_flags |= SLTCP_EOF_FLAG;
	
	
   tcp->tcp_read_ptr_max += (unsigned int) nread;
   tcp->bytes_in += (unsigned int) nread;
   
   return nread;
}

/*}}}*/

unsigned int sltcp_read (SLTCP_Type *tcp, char *s, unsigned int len) /*{{{*/
{
   int fd;
   unsigned char *buf, *b;
   unsigned int blen;
   unsigned int total_len;
   
   if ((tcp == NULL) || (-1 == (fd = tcp->tcp_fd)))
     return 0;
   
   total_len = 0;
   
   while (1)
     {
	buf = tcp->tcp_read_ptr;
	b = tcp->tcp_read_ptr_max;
   
	blen = (unsigned int) (b - buf);
	if (blen >= len)
	  {
	     memcpy (s, (char *) buf, len);
	     tcp->tcp_read_ptr += len;
	     total_len += len;
	     return total_len;
	  }
   
	if (blen)
	  {
	     memcpy (s, (char *) buf, blen);
	     total_len += blen;
	     s += blen;
	     len -= blen;
	  }
	
	tcp->tcp_read_ptr = tcp->tcp_read_ptr_max = tcp->tcp_read_buf;
	
	if (-1 == do_read (tcp))
	  return total_len;
     }
}

/*}}}*/

unsigned int sltcp_write (SLTCP_Type *tcp, char *s, unsigned int len) /*{{{*/
{
   int fd;
   unsigned int blen;
   unsigned char *b;
   
   if ((tcp == NULL) || (-1 == (fd = tcp->tcp_fd)))
     return 0;
   
   b = tcp->tcp_write_ptr;
   blen = (unsigned int) ((tcp->tcp_write_buf + SLTCP_BUF_SIZE) - b);
   if (len <= blen)
     {
	memcpy ((char *) b, s, len);
	tcp->tcp_write_ptr += len;
	
	return len;
     }
   
   if (-1 == sltcp_flush_output (tcp))
     return 0;
   
   return do_write (tcp, (unsigned char *) s, len);
}

/*}}}*/
   
int sltcp_map_service_to_port (char *service) /*{{{*/
{
   struct servent *sv;
   
   sv = getservbyname (service, "tcp");
   if (sv == NULL) return -1;
   
   return (int) ntohs (sv->s_port);
}

/*}}}*/


int sltcp_fputs (SLTCP_Type *tcp, char *s) /*{{{*/
{
   unsigned int len;
   
   if (s == NULL)
     return -1;
   
   len = strlen (s);
   if (len != sltcp_write (tcp, s, len))
     return -1;
   
   return 0;
}

/*}}}*/

int sltcp_vfprintf (SLTCP_Type *tcp, char *fmt, va_list ap) /*{{{*/
{
   char buf [SLTCP_BUF_SIZE];
   
   if ((tcp == NULL) || (-1 == tcp->tcp_fd))
     return -1;
   
   vsprintf (buf, fmt, ap);
   
   return sltcp_fputs (tcp, buf);
}

/*}}}*/

int sltcp_fgets (SLTCP_Type *tcp, char *buf, unsigned int len) /*{{{*/
{
   unsigned char *r, *rmax;
   char *buf_max;
   int fd;
   
   if ((tcp == NULL) || (-1 == (fd = tcp->tcp_fd)) || (len == 0))
     return -1;
   
   buf_max = buf + (len - 1);	       /* allow room for \0 */
   
   while (1)
     {
	r = tcp->tcp_read_ptr;
	rmax = tcp->tcp_read_ptr_max;
   
	while (r < rmax)
	  {
	     unsigned char ch;
	     
	     if (buf == buf_max)
	       {
		  *buf = 0;
		  tcp->tcp_read_ptr = r;
		  return 0;
	       }
	     
	     ch = *r++;
	     *buf++ = ch;
	     
	     if (ch == '\n')
	       {
		  *buf = 0;
		  tcp->tcp_read_ptr = r;
		  return 0;
	       }
	  }
	
	tcp->tcp_read_ptr_max = tcp->tcp_read_ptr = tcp->tcp_read_buf;
	
	if (-1 == do_read (tcp))
	  return -1;
     }
}

/*}}}*/
