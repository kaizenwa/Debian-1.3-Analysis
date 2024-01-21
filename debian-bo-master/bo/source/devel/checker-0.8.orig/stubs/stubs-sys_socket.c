/* Checker stubs for functions defined in sys/socket.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/types.h>
#include <sys/socket.h>
#include "checker_api.h"

#undef HAVE_rcmd
#undef HAVE_rresvport
#undef HAVE_ruserok
#undef HAVE_rexec

#if 0
#define HAVE_socket
#define HAVE_socketpair
#define HAVE_bind
#define HAVE_connect
#define HAVE_listen
#define HAVE_accept
#define HAVE_getsockopt
#define HAVE_setsockopt
#define HAVE_getsockname
#define HAVE_getpeername
#define HAVE_send
#define HAVE_recv
#define HAVE_sendto
#define HAVE_recvfrom
#define HAVE_shutdown
#endif

#ifdef HAVE_chkr_func
/* Check the address of socket, according to the protocol. */
void
check_sockaddr (const struct sockaddr *addr, int len)
{
  if (len <= 0)
    return;
     
  stubs_chkr_check_addr ((PTR) addr, sizeof (short), CHKR_RO, "addr");
  switch (addr->sa_family)
    {
  case AF_INET:
      if (len > sizeof(short) + sizeof(short) + sizeof(long))
        len = sizeof(short) + sizeof(short) + sizeof(long);
      break;
    }
  stubs_chkr_check_addr ((PTR) addr, len, CHKR_RO, "addr");
}
#else
void check_sockaddr (const struct sockaddr *addr, int len);
#endif

/* compiled from: . */
#ifdef HAVE_socket
/* From `/usr/include/sys/socket.h:13'.  */
int
chkr$socket (int af, int type, int protocol)
{
  int res;
  res = socket (af, type, protocol);
  if (res != -1)
    fd_returned_by_system (res);
  return res;
}
#endif /* HAVE_socket */

#ifdef HAVE_socketpair
/* From `/usr/include/sys/socket.h:21'.  */
int
chkr$socketpair (int af, int type, int protocol, int sv[2])
{
  int res;
  stubs_chkr_check_addr (sv, 2 * sizeof (int), CHKR_MW, "sv");
  res = socketpair (af, type, protocol, sv);
  if (res != -1)
    {
      stubs_chkr_set_right (sv, 2 * sizeof (int), CHKR_RW);
      fd_returned_by_system (sv[0]);
      fd_returned_by_system (sv[1]);
    }
  return res;
}
#endif /* HAVE_socketpair */

#ifdef HAVE_bind
/* From `/usr/include/sys/socket.h:26'.  */
int
chkr$bind (int fd, struct sockaddr *addr, int addrlen)
{
  check_sockaddr (addr, addrlen);
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (bind);
#else
  return bind (fd, addr, addrlen);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_bind */

#ifdef HAVE_connect
/* From `/usr/include/sys/socket.h:33'.  */
int
chkr$connect (int fd, struct sockaddr *addr, int addrlen)
{
  fd_used_by_prog (fd);
  check_sockaddr (addr, addrlen);
#if USE_BI_JUMP
  __builtin_jump (connect);
#else
  return connect (fd, addr, addrlen);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_connect */

#ifdef HAVE_listen
/* From `/usr/include/sys/socket.h:38'.  */
int
chkr$listen (int s, int backlog)
{
  fd_used_by_prog (s);
#if USE_BI_JUMP
  __builtin_jump (listen);
#else
  return listen (s, backlog);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_listen */

#ifdef HAVE_accept
/* From `/usr/include/sys/socket.h:46'.  */
int
chkr$accept (int s, struct sockaddr *addr, int *addrlen)
{
  int res;
  
  fd_used_by_prog (s);
  stubs_chkr_check_addr (addrlen, sizeof (int), CHKR_RW, "addrlen");
  if (*addrlen > 0)
    stubs_chkr_check_addr (addr, *addrlen, CHKR_MW, "addr");
  res = accept (s, addr, addrlen);
  if (*addrlen > 0)
    stubs_chkr_set_right (addr, *addrlen, CHKR_RW);
  return res;
}
#endif /* HAVE_accept */

#ifdef HAVE_getsockopt
/* From `/usr/include/sys/socket.h:53'.  */
int
chkr$getsockopt (int s, int level, int optname, void *optval, int *optlen)
{
  int res;
  
  fd_used_by_prog (s);
  stubs_chkr_check_addr (optlen, sizeof (int), CHKR_RW, "optlen");
  if (*optlen > 0)
    stubs_chkr_check_addr (optval, *optlen, CHKR_MW, "optval");
  res = getsockopt (s, level, optname, optval, optlen);
  if (*optlen > 0)
    stubs_chkr_set_right (optval, *optlen, CHKR_RW);
  return res;
}
#endif /* HAVE_getsockopt */

#ifdef HAVE_setsockopt
/* From `/usr/include/sys/socket.h:59'.  */
int
chkr$setsockopt (int s, int level, int optname, const void *optval, int optlen)
{
  fd_used_by_prog (s);
  if (optlen > 0)
    stubs_chkr_check_addr (optval, optlen, CHKR_RO, "optval");
#if USE_BI_JUMP
  __builtin_jump (setsockopt);
#else
  return setsockopt (s, level, optname, optval, optlen);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setsockopt */

#ifdef HAVE_getsockname
/* From `/usr/include/sys/socket.h:63'.  */
int
chkr$getsockname (int s, struct sockaddr *addr, int *addrlen)
{
  int res;
  
  fd_used_by_prog (s);
  stubs_chkr_check_addr (addrlen, sizeof (int), CHKR_RW, "addrlen");
  stubs_chkr_check_addr (addr, *addrlen, CHKR_MW, "addr");
  res = getsockname (s, addr, addrlen);
  if (res == 0 && *addrlen > 0)
    stubs_chkr_set_right (addr, *addrlen, CHKR_RW);
  return res;
}
#endif /* HAVE_getsockname */

#ifdef HAVE_getpeername
/* From `/usr/include/sys/socket.h:68'.  */
int
chkr$getpeername (int s, struct sockaddr *addr, int *addrlen)
{
  int res;

  fd_used_by_prog (s);
  stubs_chkr_check_addr (addrlen, sizeof (int), CHKR_RW, "addrlen");
  stubs_chkr_check_addr (addr, *addrlen, CHKR_MW, "addr");
  res = getsockname (s, addr, addrlen);
  if (res == 0 && *addrlen > 0)
    stubs_chkr_set_right (addr, *addrlen, CHKR_RW);
  return res;
}
#endif /* HAVE_getpeername */

#ifdef HAVE_send
/* From `/usr/include/sys/socket.h:72'.  */
int
chkr$send (int s, const void *msg, int len, unsigned int flags)
{
  fd_used_by_prog (s);
  if (len > 0)
    stubs_chkr_check_addr (msg, len, CHKR_RO, "msg");
#if USE_BI_JUMP
  __builtin_jump (send);
#else
  return send (s, msg, len, flags);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_send */

#ifdef HAVE_recv
/* From `/usr/include/sys/socket.h:77'.  */
int
chkr$recv (int s, void *buf, int len, unsigned int flags)
{
  int res;
  
  fd_used_by_prog (s);
  if (len > 0)
    stubs_chkr_check_addr (buf, len, CHKR_MW, "buf");
  res = recv (s, buf, len, flags);
  if (res > 0)
    stubs_chkr_set_right (buf, res, CHKR_RW);
  return res;
}
#endif /* HAVE_recv */

#ifdef HAVE_sendto
/* From `/usr/include/sys/socket.h:83'.  */
int
chkr$sendto (int s, const void *msg, int len, unsigned int flags, const struct sockaddr *to, int tolen)
{
  fd_used_by_prog (s);
  if (len > 0)
    stubs_chkr_check_addr (msg, len, CHKR_RO, "msg");
  check_sockaddr (to, tolen);
#if USE_BI_JUMP
  __builtin_jump (sendto);
#else
  return sendto (s, msg, len, flags, to, tolen);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sendto */

#ifdef HAVE_recvfrom
/* From `/usr/include/sys/socket.h:91'.  */
int
chkr$recvfrom (int s, void *buf, int len, unsigned int flags, struct sockaddr *from, int *fromlen)
{
  int res;

  fd_used_by_prog (s);
  if (len > 0)
    stubs_chkr_check_addr (buf, len, CHKR_MW, "buf");
  stubs_chkr_check_addr (fromlen, sizeof (int), CHKR_RW, "fromlen");
  if (from && *fromlen > 0)
    stubs_chkr_check_addr (from, *fromlen, CHKR_MW, "from");
  res = recvfrom (s, buf, len, flags, from, fromlen);
  if (res > 0)
    {
      stubs_chkr_set_right (buf, res, CHKR_RW);
      if (from && *fromlen > 0)
        stubs_chkr_set_right (from, *fromlen, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_recvfrom */

#ifdef HAVE_shutdown
/* From `/usr/include/sys/socket.h:99'.  */
int
chkr$shutdown (int s, int how)
{
  fd_used_by_prog (s);
#if USE_BI_JUMP
  __builtin_jump (shutdown);
#else
  return shutdown (s, how);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_shutdown */

#ifdef HAVE_rcmd
/* From `/usr/include/sys/socket.h:105'.  */
int
chkr$rcmd (char ** arg0, short unsigned int arg1, const char * arg2, const char * arg3, const char * arg4, int * arg5)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char *), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg4, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg5, sizeof (int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (rcmd);
#else
  {
    int res;
    res = rcmd (arg0, arg1, arg2, arg3, arg4, arg5);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rcmd */

#ifdef HAVE_rresvport
/* From `/usr/include/sys/socket.h:106'.  */
int
chkr$rresvport (int * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (rresvport);
#else
  {
    int res;
    res = rresvport (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rresvport */

#ifdef HAVE_ruserok
/* From `/usr/include/sys/socket.h:108'.  */
int
chkr$ruserok (const char * arg0, int arg1, const char * arg2, const char * arg3)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (ruserok);
#else
  {
    int res;
    res = ruserok (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ruserok */

#ifdef HAVE_rexec
/* From `/usr/include/sys/socket.h:111'.  */
int
chkr$rexec (char ** arg0, int arg1, const char * arg2, const char * arg3, const char * arg4, int * arg5)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char *), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg4, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg5, sizeof (int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (rexec);
#else
  {
    int res;
    res = rexec (arg0, arg1, arg2, arg3, arg4, arg5);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rexec */

#endif /* HAVE_SYS_SOCKET_H */
