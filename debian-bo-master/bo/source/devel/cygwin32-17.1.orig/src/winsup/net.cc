/*  Network-related routines.  

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

/*#define DEBUG_NEST_ON 1*/

#define  __INSIDE_CYGWIN_NET__

#include "winsup.h"
#include <netdb.h>
#include <sys/socket.h>
#include "registry.h"

#define __INSIDE_CYGWIN32__

#include <mywinsock.h>

static void
fail (int n)
{
  debug_printf ("********%d*************\n",n);
}

int init;

static void
checkinit ()
{
  WSADATA p;
  if (!init) 
    {
      int res = WSAStartup ((2<<8) |2, &p);
      debug_printf ("res %d\n", res);
      debug_printf ("wVersion %d\n", p.wVersion);
      debug_printf ("wHighVersion %d\n", p.wHighVersion);
      debug_printf ("szDescription %s\n",p.szDescription);
      debug_printf ("szSystemStatus %s\n",p.szSystemStatus);
      debug_printf ("iMaxSockets %d\n", p.iMaxSockets);
      debug_printf ("iMaxUdpDg %d\n", p.iMaxUdpDg);
      debug_printf ("lpVendorInfo %d\n", p.lpVendorInfo);

      select_init ();

      if (FIONBIO  != REAL_FIONBIO)
	{
	  debug_printf ("****************  FIONBIO  != REAL_FIONBIO\n");
	}
    }
  init = 1;
  MARK();
}

extern "C" 
struct hostent *cygwin32_gethostbyaddr (const char *, int, int)
{
  checkinit ();
  fail (__LINE__);
  MARK();
  in ("gethostbyaddr");
  out ("gethostbyaddr");
  return 0;
}

extern "C" unsigned long int
htonl (unsigned long int x)
{
  MARK();
  return   ((((x & 0x000000ffU) << 24) | 
	     ((x & 0x0000ff00U) <<  8) | 
	     ((x & 0x00ff0000U) >>  8) | 
	     ((x & 0xff000000U) >> 24)));
}

extern "C" unsigned short
htons (unsigned short x)
{
  MARK();
  return   ((((x & 0x000000ffU) << 8) | 
	     ((x & 0x0000ff00U) >> 8)));
}

static void
dump_protoent (struct protoent *p)
{
  if (p)
    {
      debug_printf ("protoent %s %x %x\n", p->p_name, p->p_aliases, p->p_proto);
    }
}

extern "C" struct protoent *
cygwin32_getprotobyname (const char *p)
{
  MARK();
  checkinit ();
  MARK();
  in ("getprotobyname");

  struct protoent *res = getprotobyname (p);
  /* Fixme !! errno !! */
  dump_protoent (res);
  out ("getprotobyname");
  return res;
}

extern "C" char *
cygwin32_inet_ntoa (struct in_addr in) 
{
    in ("inet_ntoa");
    char *res = inet_ntoa (in);
    out ("inet_ntoa");
    return res;
}

extern "C" unsigned long
cygwin32_inet_addr (const char *cp) 
{ 
  in ("inet_addr");
  unsigned long res = inet_addr (cp);
  out ("inet_addr");
  return res;
}

struct tl 
{
  int w;
  const char *s;
  int e;
};

static struct tl errmap[] =
{{ WSAESOCKTNOSUPPORT,"WSAESOCKTNOSUPPORT", EAFNOSUPPORT},
 {WSAEPROTOTYPE ,"WSAEPROTOTYPE",EPROTOTYPE},
 {WSAENOTSOCK,"WSAENOTSOCK", ENOTSOCK},
 { WSAENOPROTOOPT,"WSAENOPROTOOPT",ENOPROTOOPT},
 {WSANOTINITIALISED,"WSANOTINITIALISED",EINVAL},

 {WSAEWOULDBLOCK,"WSAEWOULDBLOCK",EWOULDBLOCK},
 {WSAESHUTDOWN,"WSAESHUTDOWN",ESHUTDOWN},
 {WSAEADDRINUSE,"WSAEADDRINUSE",EADDRINUSE},
 {WSAECONNREFUSED,"WSAECONNREFUSED",ECONNREFUSED},
 {WSAECONNABORTED,"WSAECONNABORTED",ECONNABORTED},
{ 0}};

static void
set_winsock_errno ()
{
  int i;
  int why  = WSAGetLastError ();
  for (i = 0; errmap[i].w != 0; ++i)
    if (why == errmap[i].w)
      break;

  if (errmap[i].w != 0)
    {
      syscall_printf ("seterrno: %d (%s) -> %d\n", 
		      why, errmap[i].s, errmap[i].e);
      set_errno (errmap[i].e);
    }
  else
    {
      syscall_printf ("seterrno: unknown error %d!!\n",why);
      set_errno (EPERM);
    }
}

extern "C" int
cygwin32_socket (int af, int type, int protocol)
{
  int res = -1;
  in ("socket");
  pinfo *p = u->self;

  int fd = p->hmap.find_unused_handle (0);

  if (fd < 0)
    {
      set_errno (ENMFILE);
      goto done;
    }

  debug_printf ("socket (%d, %d, %d);\n", af, type, protocol);

  checkinit();  /* initialize WinSock */

  SOCKET soc = socket (af, type, protocol);

  if (soc == INVALID_SOCKET)
    {
      set_winsock_errno ();
      goto done;
    }

  fhandler_socket *h;
  p->hmap.vec[fd].h =h = new (&p->hmap.vec[fd].item) fhandler_socket;
  h->setup (soc);
  res = fd;

done:
  out ("socket");
  syscall_printf ("%d = socket (%d, %d, %d);\n", res, af, type, protocol);
  return res;
}

extern "C" int
cygwin32_sendto (int fd, 
		 const void *buf,
		 int len,
		 unsigned int flags,
		 const struct sockaddr *to, 
		 int tolen)
{
  fhandler_socket *h = (fhandler_socket *) u->self->hmap.vec[fd].h;
  in ("sendto");
  int res = sendto (h->get_socket (), (const char *)buf, len, flags, to, tolen);
  if (res == SOCKET_ERROR) {
    set_winsock_errno ();
    res = -1;
  }
  out ("sendto");
  return res;
}

extern "C" int
cygwin32_recvfrom (int fd, 
		   char *buf,
		   int len, 
		   int flags, 
		   struct sockaddr *from,
		   int *fromlen)
{
  fhandler_socket *h = (fhandler_socket *) u->self->hmap.vec[fd].h;
  in ("recvfrom");

  debug_printf ("recvfrom %d\n", h->get_socket ());
  int   res = recvfrom (h->get_socket (), buf, len, flags, from, fromlen);
  if (res == SOCKET_ERROR) {
    set_winsock_errno ();
    res = -1;
  }

  out ("recvfrom");
  return res;
}

fhandler_socket *
get (int fd)
{
  if (NOT_OPEN_FD (fd))
    {
      set_errno (EINVAL);
      return 0;
    }
  
  return u->self->hmap.vec[fd].h->is_socket ();
}

extern "C" int
cygwin32_setsockopt (int fd, 
		     int level,
		     int optname,
		     const void *optval, 
		     int optlen)
{
  in ("setsockopt");
  fhandler_socket *h = get (fd);
  int res = -1;
  const char *name = "error";
  if (h) 
    {
      switch (optname)
	{
	case SO_DEBUG      :name="SO_DEBUG";
	case SO_ACCEPTCONN :name="SO_ACCEPTCONN";
	case SO_REUSEADDR  :name="SO_REUSEADDR";
	case SO_KEEPALIVE  :name="SO_KEEPALIVE";
	case SO_DONTROUTE  :name="SO_DONTROUTE";
	case SO_BROADCAST  :name="SO_BROADCAST";
	case SO_USELOOPBACK:name="SO_USELOOPBACK";
	case SO_LINGER    :name="SO_LINGER";
	case SO_OOBINLINE :name="SO_OOBINLINE";
	}
      
      res = setsockopt (h->get_socket (), level, optname, (const char *)optval, optlen);
      
      if (optlen == 4)
	{
	  syscall_printf ("setsockopt optval=%x\n", *(long *)optval);
	}
      if (res)
	set_winsock_errno ();
    }

  out ("setsockopt");
  syscall_printf ("%d = setsockopt (%d, %d, %x (%s), %x, %d);\n",
		 res,
		 fd, 
		 level, optname,name, optval, optlen);
  return res;
}

int
cygwin32_getsockopt (int fd, 
		     int level,
		     int optname,
		     void *optval, 
		     int *optlen)
{
  in ("getsockopt");
  fhandler_socket *h = get (fd);
  int res = -1;
  const char *name = "error";
  if (h) 
    {
      switch (optname)
	{
	case SO_DEBUG      :name="SO_DEBUG";
	case SO_ACCEPTCONN :name="SO_ACCEPTCONN";
	case SO_REUSEADDR  :name="SO_REUSEADDR";
	case SO_KEEPALIVE  :name="SO_KEEPALIVE";
	case SO_DONTROUTE  :name="SO_DONTROUTE";
	case SO_BROADCAST  :name="SO_BROADCAST";
	case SO_USELOOPBACK:name="SO_USELOOPBACK";
	case SO_LINGER    :name="SO_LINGER";
	case SO_OOBINLINE :name="SO_OOBINLINE";
	}
      
      res = getsockopt (h->get_socket (), level, optname, ( char *)optval, (int *)optlen);
      
      if (res)
	set_winsock_errno ();
    }

  out ("getsockopt");
  syscall_printf ("%d = getsockopt (%d, %d, %x (%s), %x, %d);\n",
		  res,
		  fd, 
		  level, optname,name, optval, optlen);
  return res;
}

extern "C" int
cygwin32_connect (int fd,
		  const struct sockaddr * name,
		  int namelen)
{
  in ("connect");
  int res;
  fhandler_socket *s = get (fd);
  if (!s)
    {
      res = -1;
    }
  else
    {
      res= connect (s->get_socket (), name, namelen);
      if (res)
        set_winsock_errno ();
      out ("connect");
    }
  return res;
}

struct servent *
cygwin32_getservbyname (const char *name,
					 const char *proto)
{
  in ("getservbyname");
  checkinit ();
  struct servent *p = getservbyname (name, proto);
  if (!p)
    set_winsock_errno ();
  else
    {

    }
  syscall_printf ("%x = getservbyname (%s, %s);\n",
		 p, name, proto);
  out ("getservbyname");
  return p;
}

extern "C" struct hostent *
cygwin32_gethostbyname (const char *name)
{
  checkinit ();
  MARK();
  in ("gethostbyname");
  struct hostent *ptr = gethostbyname (name);
  if (!ptr)
    {
      set_winsock_errno ();
    }
else
  {
    debug_printf ("h_name %s\n", ptr->h_name);
  }
  out ("gethostbyname");
  return ptr;
}

int
fhandler_socket::write (const void *ptr, size_t len)
{
  int res = send (get_socket (), (const char *)ptr, len, 0);
  if (res == SOCKET_ERROR)
    {
      set_winsock_errno ();
    }
  return res;
}

int
fhandler_socket::read (void *ptr, size_t len)
{
  in ("read");
  int res = recv (get_socket (), (char *)ptr, len, 0);
  if (res == SOCKET_ERROR)
    {
      set_winsock_errno ();
    }
  out ("read");
  return res;
}

int
cygwin32_accept (int fd, struct sockaddr *peer, int *len)
{
  int res = -1;
  in ("accept");
  fhandler_socket *s = get (fd);
  if (s)
    {
      res = accept (s->get_socket (), peer, len);
      if ((SOCKET)res == (SOCKET)INVALID_SOCKET)
	set_winsock_errno ();
      else
	{
	  pinfo *p = u->self;
	  int res_fd = p->hmap.find_unused_handle (0);
	  fhandler_socket *h;
	  p->hmap.vec[res_fd].h = h = new (&p->hmap.vec[res_fd].item) fhandler_socket;
	  h->setup (res);
	  res = res_fd;
	}
    }
  out ("accept");
  syscall_printf ("%d = accept (%d, %x, %x);\n", res, fd, peer, len);
  return res;
}

int
cygwin32_bind (int fd, struct sockaddr *my_addr, int addrlen)
{
  int res = -1;
  in ("bind");
  fhandler_socket *s = get (fd);
  if (s)
    {
      res = bind (s->get_socket (), my_addr, addrlen);
      if (res)
	set_winsock_errno ();
    }
  out ("bind");
  syscall_printf ("%d = bind (%d, %x, %d);\n", res, fd, my_addr, addrlen);
  return res;
}

int
cygwin32_getsockname (int fd, struct sockaddr *addr, int *namelen)
{
  int res = -1;
  in ("getsockname");
  fhandler_socket *s = get (fd);
  if (s)
    {
      res = getsockname (s->get_socket (), addr, namelen);
      if (res)
	set_winsock_errno ();

    }
  out ("getsockname");
  syscall_printf ("%d = getsockname (%d, %x, %d);\n", res, fd, addr, namelen);
  return res;
}

int
cygwin32_listen (int fd, int backlog)
{
  int res = -1;
  in ("listen");

  fhandler_socket *s = get (fd);
  if (s)
    {
      res = listen (s->get_socket (), backlog);
      if (res)
	set_winsock_errno ();
    }
  out ("listen");
  syscall_printf ("%d = listen (%d, %d);\n", res, fd, backlog);
  return res;
}

int
cygwin32_shutdown (int, int how)
{
  fail (__LINE__);
  return 0;
}

void
herror (const char *p)
{
  fail (__LINE__);
}

void
cygwin32_herror (const char *s)
{
  fail (__LINE__);
}

int
cygwin32_send (int fd, const void *buf, int len, unsigned int flags)
{
  fhandler_socket *h = (fhandler_socket *) u->self->hmap.vec[fd].h;
  in ("send");

  int res = send (h->get_socket (),(const char *) buf, len, flags);
  if (res == SOCKET_ERROR) {
    set_winsock_errno ();
    res = -1;
  }

#if 0
  for (int i =0; i < len; i++)
    {
      small_printf ("send %d %x %c\n", i, ((char *)buf)[i], ((char *)buf)[i]);
    }
#endif

  syscall_printf ("%d = send (%d, %x, %d, %x);\n", res, fd, buf, len, flags);

  out ("send");
  return res;
}

int
fhandler_socket::close ()
{
  int res;
  if (closesocket (get_socket ())) 
    {
      set_winsock_errno ();
      res = -1;
    }
  else
    {
      res = 0;
    }
  return res;
}

extern "C" int
cygwin32_recv (int fd, void *buf, int len, unsigned int flags)
{
  fhandler_socket *h = (fhandler_socket *) u->self->hmap.vec[fd].h;
  in ("recv");

  int res = recv (h->get_socket (), (char *)buf, len, flags);
  if (res == SOCKET_ERROR)
    {
      set_winsock_errno ();
      res = -1;
    }

#if 0
  if (res > 0 && res < 200)
    {
      for (int i =0; i < res; i++)
        {
          small_printf ("%d %x %c\n", i, ((char *)buf)[i], ((char *)buf)[i]);
        }
    }
#endif

  syscall_printf ("%d = recv (%d, %x, %x, %x)\n",res, fd, buf,len,flags);

  out ("recv");
  return res;
}

int
cygwin32_getpeername (int fd, struct sockaddr *name, int *len)
{
  checkinit ();
  fhandler_socket *h = (fhandler_socket *) u->self->hmap.vec[fd].h;
  in ("getpeername");

  debug_printf ("getpeername %d\n", h->get_socket ());
  int res = getpeername (h->get_socket (), name, len);
  if (res)
    set_winsock_errno ();

  out ("getpeername");
  return res;
}

/*
 * This is a BSD derived function, not POSIX..
 */

extern "C" int
getdomainname (char *domain, int len)
{
  reg_key sys;
  reg_key controlset;
  reg_key services;
  reg_key tcp;
  reg_key params;

  /* Are registry keys case sensitive ? */
  if(sys.open(windows_95() ? "System" : "SYSTEM", HKEY_LOCAL_MACHINE, KEY_READ)!=ERROR_SUCCESS)
    {
      __seterrno();
      return -1;
    }
  if(controlset.open("CurrentControlSet", sys.get_key(), KEY_READ)!=ERROR_SUCCESS)
    {
      __seterrno();
      return -1;
    }
  if(services.open("Services", controlset.get_key(), KEY_READ)!=ERROR_SUCCESS)
    {
      __seterrno();
      return -1;
    }
  /*
   * This next line only works for Win95 if the machine is configured
   * to use MS-TCP. If a third-party TCP is being used this will fail.
   * Question : On Win95, is there a way to portably check the TCP stack in use
   * and include paths for the Domain name in each ?
   * Punt for now and assume MS-TCP on Win95.
   */
  if(tcp.open( windows_95() ? "MSTCP" : "Tcpip", services.get_key(), KEY_READ)!=ERROR_SUCCESS)
    {
      __seterrno();
      return -1;
    }
  if(params.open( "Parameters", tcp.get_key(), KEY_READ)!=ERROR_SUCCESS)
    {
      __seterrno();
      return -1;
    }
  if(params.get_string("Domain", domain, len, "")!=ERROR_SUCCESS)
    {
      __seterrno();
      return -1;
    }

  return 0;
}

int
fhandler_socket::ioctl (int cmd, void *p)
{
  int res = ioctlsocket (get_socket (), cmd, (unsigned long *)p);
  if (res == SOCKET_ERROR)
    {
      set_winsock_errno ();
    }
  if (cmd == (int)FIONBIO)
    {
      syscall_printf ("socket is now %d blocking\n", *(int *)p);
    }
  syscall_printf ("%d = ioctl_socket (%x, %x);\n", res, cmd, p);
  return res;
}

int
fhandler_socket::fstat (struct stat *buf)
{
  syscall_printf ("******************** FSTAT!\n");
  return -1;
}
