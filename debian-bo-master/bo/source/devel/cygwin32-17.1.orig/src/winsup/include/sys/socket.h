#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H

#include <features.h>
#include <cygwin32/socket.h>
#include <sys/time.h>

/* The cygwin32_ prefixes are because names in the standard 
   winsock library conflict with the standard unix names  - 
   they have the same name, but the meaning of the args is slightly
   different.  
   
   When compiling up a unix app, the #defines later on will make sure
   that the cygwin32_<foo> function are called, rather than the raw
   winsock versions. 

   */

#ifdef __cplusplus
extern "C"
{
#endif

  int cygwin32_accept (int, struct sockaddr *peer, int *);
  int cygwin32_bind (int, struct sockaddr *my_addr, int addrlen);
  int cygwin32_connect (int,const struct sockaddr *, int);
  int cygwin32_getpeername (int, struct sockaddr *peer, int *);
  int cygwin32_getsockname (int, struct sockaddr *addr, int *);
  int cygwin32_listen (int, int n);
  int cygwin32_recv (int, void *buff, int len, unsigned int flags);
  int cygwin32_recvfrom (int, char *buff, int len, int flags, 
			 struct sockaddr *from, int *fromlen);
  int cygwin32_send (int, const void *buff, int len, unsigned int flags);
  int cygwin32_sendto (int, const void *, int, unsigned int, const struct sockaddr *, int);
  int cygwin32_setsockopt (int s, int level, int optname, const void *optval, int optlen);
  int cygwin32_getsockopt (int s, int level, int optname, void *optval, int *optlen);
  int cygwin32_shutdown (int, int);
  int cygwin32_socket (int family, int type, int protocol);
  struct servent *cygwin32_getservbyname (const char *name, const char *proto);
  int cygwin32_select (int, fd_set * , fd_set *, fd_set * , struct timeval *);

#ifdef __cplusplus
};
#endif

#ifndef __INSIDE_CYGWIN_NET__
#define socket cygwin32_socket
#define connect cygwin32_connect
#define recv cygwin32_recv
#define recvfrom cygwin32_recvfrom
#define sendto cygwin32_sendto
#define send cygwin32_send
#define bind cygwin32_bind
#define listen cygwin32_listen
#define accept cygwin32_accept
#define setsockopt cygwin32_setsockopt
#define getsockopt cygwin32_getsockopt
#define shutdown cygwin32_shutdown
#define getsockname cygwin32_getsockname
#define getpeername cygwin32_getpeername
#define select cygwin32_select
#define inet_ntoa cygwin32_inet_ntoa
#endif

#endif /* _SYS_SOCKET_H */
