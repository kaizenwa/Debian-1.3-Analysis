/*
Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: newtcp.c,v 1.59 1996/07/11 07:23:28 spreitze Exp $ */
/* Last edited by Mike Spreitzer July 10, 1996 11:18 pm PDT */

#include "iluntrnl.h"

#include "transprt.h"
#include "mooring.h"

#include "ilusock.h"

#include "oscalls.h"

#ifdef XEROX_FIREWALL_HANDLING
#include "xerox_firewall.c"
#define connect(a,b,c)	xerox_firewall_connect((a),(b),(c))
#endif /* XEROX_FIREWALL_HANDLING */

#if (defined(WIN32) || defined(WIN16))

static int set_doer_non_blocking (int fd)
{
  unsigned long one = 1;
  return (OS_SOCKIOCTL(fd, FIONBIO, &one));
}
#define set_listener_non_blocking	set_doer_non_blocking

#elif (defined(_IS_POSIX) && (!defined(HAS_SOLARIS1_NONBLOCKING_BUG)))

#include <fcntl.h>
static int add_fcntl_flag (int fd, int dflags)
{
  int             flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0)
    return flags;
  return fcntl(fd, F_SETFL, flags | dflags);
}
#define set_listener_non_blocking(fd)	add_fcntl_flag((fd), O_NONBLOCK)
#define set_doer_non_blocking(fd)	add_fcntl_flag((fd), O_NONBLOCK)

#elif (defined(_IS_BSD) && defined(FIONBIO))

static int set_doer_non_blocking (int fd)
{
  int one = 1;
  return (OS_SOCKIOCTL(fd, FIONBIO, &one));
}
#define set_listener_non_blocking	set_doer_non_blocking

#elif (defined(_IS_BSD) && defined(HAS_SOLARIS1_NONBLOCKING_BUG))
static int set_doer_non_blocking (int fd)
{
  int one = 1;
  return (OS_SOCKIOCTL(fd, 0x8004667e, &one));
  return 0;
}
#define set_listener_non_blocking	set_doer_non_blocking

#else	/* not POSIX or BSD or Windows */

#error "Don't know how to do non-blocking I/O for anything but POSIX, Windows, and BSD just now"

#endif

#ifdef ILU_NEEDS_NETINET_TCP_H_FOR_NODELAY
#include <netinet/tcp.h>
#endif

#if 0
#undef XEROX_FIREWALL_HANDLING
#undef set_listener_non_blocking
#undef set_doer_non_blocking
#define set_listener_non_blocking(fd) 0
#define set_doer_non_blocking(fd) 0
#endif

typedef unsigned short ilutcpport_t;
/* WIN port needs arg of htons() to be an unsigned short */

typedef struct {
  /* L1, L2, Main unconstrained */

  struct sockaddr_in addr;
  /*
   * Local address for Mooring creation, peer address for Transport
   * creation.
   */
  ilu_string      tinfo_host;
  /* host part of tinfo; never NIL; owned by this struct */

  ilu_boolean     name_given;	/* did tinfo give name or address? */
  ilu_boolean     defaulted;	/* did tinfo give null name/address? */
}              *CreatorParms;

typedef struct {
  /* L1, L2, Main unconstrained */

  int             fd;		/* listening socket */
  ilu_boolean     buff_p;	/* buffering wanted */
  ilu_TIH        *tih;		/* !=NIL when req handler reg'd */
}              *MooringParms;

typedef struct {
  /* L1, L2, Main unconstrained */

  int             fd;		/* I/O socket */
  ilu_cardinal    inSize;	/* size of input buffer */
  ilu_boolean     inpReg;	/* got an inp handler reg'd? */
  ilu_TIH        *tih;		/* inpReg => tih != NIL */

}              *TCPParms;
/* What goes in the data field of a TCP ilu_Transport */

#define BUFFERSIZE	4096
#define DIRECT_THRESHOLD 1024

/*L1, L2, Main unconstrained*/

/**********************************************************************
***********************************************************************
***********************************************************************
***** First, the methods for the TCP Transport ************************
***********************************************************************
***********************************************************************
**********************************************************************/

/*L1, L2 unconstrained*/

static          CreatorParms
_tcp_InterpretInfo(ilu_string tinfo,
		   ILU_ERRS((no_memory, inv_objref)) * err)
{
  CreatorParms    cp;
  char            hostname[1000];
  long unsigned   port;

  if ((sscanf(tinfo, "tcp_%999[^_]_%lu", hostname, &port)) != 2)
    return (ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL));
  cp = (CreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (cp == NIL)
    return NIL;
  memset((ilu_string) & cp->addr, 0, sizeof(cp->addr));
  cp->addr.sin_family = AF_INET;
  cp->addr.sin_port = htons((ilu_shortcardinal) port);
  cp->addr.sin_addr.s_addr = inet_addr(hostname);
  cp->name_given = (cp->addr.sin_addr.s_addr == -1);
  cp->defaulted = (cp->addr.sin_addr.s_addr == 0 ||
		   strcmp(hostname, "localhost") == 0);
  if (cp->defaulted) {
    if (!ilu_EnterMutex(ilu_trmu, err))
      goto faild;
    if (cp->name_given)
      (void) _ilu_CurrentHostIPAddrString(&cp->tinfo_host,
					  &cp->addr.sin_addr, err);
    else
      cp->tinfo_host = _ilu_CurrentHostIPAddrString(NIL,
					  &cp->addr.sin_addr, err);
    if (ILU_ERRNOK(*err)) {
      /* Nothing else works; let's use the loopback address. */
      cp->addr.sin_addr.s_addr = INADDR_ANY;
      /* See comment at other use of INADDR_ANY */
      cp->tinfo_host = "127.0.0.1";
      ILU_HANDLED(*err);
      ILU_CLER(*err);
    }
    cp->tinfo_host = ilu_StrdupE(cp->tinfo_host, err);
    (void) ilu_ExitMutex(ilu_trmu, TRUE, err);
    if (ILU_ERRNOK(*err))
      goto faild;
  } else {
    if (cp->addr.sin_addr.s_addr == -1) {
      struct hostent *hp;
      if ((hp = gethostbyname(hostname)) != NIL
	  && hp->h_addr != NIL)
	memcpy((ilu_string) & cp->addr.sin_addr, hp->h_addr,
	       hp->h_length);
      if (cp->addr.sin_addr.s_addr == -1) {
	DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	      (stderr, "tcp:  Invalid host name (%s).\n",
	       hostname));
	ilu_free(cp);
	return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
      }
    }
    cp->tinfo_host = ilu_StrdupE(hostname, err);
    if (ILU_ERRNOK(*err)) {
      free(cp);
      return NIL;
    }
  }
  ILU_CLER(*err);
  return cp;
faild:
  ilu_free(cp);
  return NIL;
}

/*Main Invariant holds; L2 no forther constrained*/
static          ilu_boolean
_tcp_Interrupt(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  return _ilu_InterruptFD(p->fd, err);
}

/* L2, Main, unconstrained; L1_sup < trmu */

static          ilu_integer
_tcp_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  return (1);
}

static          ilu_integer
_tcp_CloseDFd(ilu_Transport self)
{
  return (1);
}

/*Main Invariant holds; L2 otherwise unconstrained*/
static void 
CallInpHandler(int fd, ilu_private rock)
{
  ilu_Transport   self = (ilu_Transport) rock;
  TCPParms        p = (TCPParms) self->tr_data;
  ilu_TIH        *tp = p->tih;
  (*tp->tih_proc) (tp->tih_rock);
  return;
}

/*tih => Main Invariant holds && L2 disjoint {conn's iomu, callmu}*/
static          ilu_boolean
SetInputHandler(ilu_Transport self, ilu_TransportInputHandler tih,
		ilu_refany tih_rock,
		ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  ilu_TIH        *tp = p->tih;
  if (p->inpReg)
    ilu_UnregisterInputSource(p->fd);
  p->inpReg = FALSE;
  if (tih != NULLFN) {
    if (tp == NIL) {
      tp = (ilu_TIH *) ilu_malloc(sizeof(*tp));
      if (tp == NIL)
	return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*tp), FALSE);
      p->tih = tp;
    }
    tp->tih_proc = tih;
    tp->tih_rock = tih_rock;
    if (!ilu_RegisterInputSource(p->fd, CallInpHandler, self))
      return ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mlreg,
			   FALSE);
    p->inpReg = TRUE;
  }
  return ILU_CLER(*err);
}

/*Main Invariant holds*/
/*L2 >= {conn's iomu}*/

static          ilu_boolean
_tcp_WaitForInput(ilu_Transport t, ilu_FineTime * limit,
		  ILU_ERRS((interrupted)) * err)
{
  TCPParms p = (TCPParms) t->tr_data;
  ilu_boolean     sure;
  ILU_CLER(*err);
  if (t->tr_inBuff != NIL && t->tr_inNext < t->tr_inLimit)
    return ilu_TRUE;
  _ilu_WaitForInputOnFD(p->fd, &sure, limit, err);
  return ILU_ERROK(*err);
}

static          ilu_boolean
_tcp_HasHiddenInput(ilu_Transport self, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return FALSE;
}

static          ilu_boolean
Write(ilu_Transport self, ilu_bytes buf, ilu_cardinal size,
      ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	(stderr, "tcp(%p):  writing %lu bytes from %p to FD %d.\n",
	 self, size, buf, p->fd));

#ifdef ENABLE_DEBUGGING

  if (((_ilu_DebugLevel & PACKET_DEBUG) != 0) &&
      (size > 0))
    _ilu_debug_DumpPacket(buf, size, "outgoing TCP");

#endif /* ENABLE_DEBUGGING */

  if (!_ilu_SockWrite(p->fd, buf, size, err))
    return FALSE;

  return TRUE;
}

static          ilu_ReadHeaderResultCode
_tcp_BeginMessage(ilu_Transport self,
		  ilu_boolean input_p,
		  ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_rhrc_error);
}

static          ilu_boolean
_tcp_EndMessage(ilu_Transport self,
		ilu_boolean flush,
		ilu_Message *msgh,
		ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       FALSE);
}

static          ilu_boolean
_tcp_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
		      ilu_Error * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable,
		       ilu_FALSE);
}

static          ilu_boolean
_tcp_WriteBytes(ilu_Transport self, ilu_bytes b,
		ilu_cardinal bufferSize,
		ilu_boolean flush,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    rem = self->tr_outLimit - self->tr_outNext;
  ilu_boolean direct;

  direct = (bufferSize >= DIRECT_THRESHOLD || flush && bufferSize > rem
	    || self->tr_outBuff == NIL);

  if (direct) {
    if (self->tr_outNext > 0) {
      if (!Write(self, self->tr_outBuff, self->tr_outNext, err))
	return FALSE;
      self->tr_outNext = 0;
    }
    if (!Write(self, b, bufferSize, err))
      return FALSE;
  } else {
    ilu_cardinal    l1 = MIN(rem, bufferSize);
    memcpy((void*)(self->tr_outBuff + self->tr_outNext), (void*)b, l1);
    b += l1;
    bufferSize -= l1;
    self->tr_outNext += l1;
    if (flush || self->tr_outNext == self->tr_outLimit) {
      if (!Write(self, self->tr_outBuff, self->tr_outNext, err))
	return FALSE;
      self->tr_outNext = 0;
    }
    if (bufferSize > 0) {
      memcpy((void*)self->tr_outBuff, (void*)b, bufferSize);
      self->tr_outNext += bufferSize;
    }
  }
  return ILU_CLER(*err);
}

static          ilu_cardinal
_tcp_ReadBytes(ilu_Transport self,
	       ilu_bytes buffer,
	       ilu_cardinal len,
	       ilu_TransportReport * rpt,
	       ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  ilu_cardinal	  toread;
  ilu_cardinal    lread;
  ilu_bytes	  actbuff;
  ilu_boolean     use_internal = ((buffer == NIL)
				  || (len < DIRECT_THRESHOLD));
  rpt->tr_eom = rpt->tr_eof = FALSE;
  if (self->tr_inBuff != NIL && self->tr_inNext != self->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  if (use_internal) {
    actbuff = self->tr_inBuff;
    toread = p->inSize;
    lread = _ilu_NbSockRead(p->fd, actbuff, toread, rpt, err);
    self->tr_inLimit = lread;
    self->tr_inNext = 0;
  } else {
    actbuff = buffer;
    toread = len;
    lread = _ilu_NbSockRead(p->fd, actbuff, toread, rpt, err);
  }
  if (ILU_ERRNOK(*err)) {

#ifdef ENABLE_DEBUGGING
    if (_ilu_DebugLevel & TCP_DEBUG)
      ILU_ERRPRINTF("tcp(%p): ReadBytes got error %s from FD %d!\n",
		    self, ILU_ERR_NAME(*err), p->fd);
#endif /* ENABLE_DEBUGGING */

  } else {
    DEBUG(TCP_DEBUG,
	  (stderr,
	   "tcp(%p): read %u bytes from FD %d, eof=%s\n",
	   self, lread, p->fd, rpt->tr_eof ? "T" : "F"));

#ifdef ENABLE_DEBUGGING
    if (((_ilu_DebugLevel & PACKET_DEBUG) != 0) &&
	(lread > 0))
      _ilu_debug_DumpPacket(actbuff, lread, "incoming TCP");
#endif /* ENABLE_DEBUGGING */

    if (use_internal) {
      toread = MIN(lread,len);
      memcpy ((void *) buffer, (void *) self->tr_inBuff, toread);
      self->tr_inNext += toread;
      return toread;
    }
  }
  return lread;
}


#if (defined (WIN32) || defined (WIN16))
/* **************************************************** */
/* returns 1 is the fd is ready for writing, else 0 */

static int _tcp_writeable_fd(int fd) {

	fd_set write_set;
	struct timeval time = {0, 0};
	
	FD_ZERO(&write_set);
	FD_SET(fd, &write_set);
		
	return select(fd + 1, NULL, &write_set, NULL, &time);
}
#endif


/*L1, L2, Main unconstrained*/

/*Main Invariant holds*/
static ilu_boolean 
_tcp_Connect(ilu_Transport self,
	     CreatorParms cp,
	     ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  int             fd, err1;
  struct sockaddr_in sinaddr = cp->addr;
  ilu_boolean     status = TRUE;
  unsigned        port;

  ILU_CLER(*err);
  _ilu_HandleSigPIPE();

  if (self == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, FALSE);

  port = ntohs(sinaddr.sin_port);
  
  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	(stderr, "tcp(%p):  connecting to host %s, port %u...\n",
	 self, inet_ntoa(sinaddr.sin_addr), port));

  if (OS_SOCKINV(fd = socket(AF_INET, SOCK_STREAM, 0))) {
    err1 = sockerrno;
    DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	  (stderr, "tcp(%p):  socket call failed:  %s.\n",
	   self, strerror(err1)));
    switch (err1) {
    case SOCKERRID(ACCES):		/* permission error */
      ILU_ERR_CONS0(no_permission, err, 0);
      break;

#if !(defined(WIN32) || defined(WIN16))
    case SOCKERRID(NFILE):
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENFILE, 0);
      break;
#endif

    case SOCKERRID(MFILE):		/* resource error */
    case SOCKERRID(NOBUFS):
      ILU_ERR_CONS1(no_resources, err, minor,
		    (err1 == SOCKERRID(MFILE))
		    ? ilu_nrm_EMFILE : ilu_nrm_ENOBUFS,
		    0);
      break;

    case SOCKERRID(PROTONOSUPPORT):	/* type error */
    case SOCKERRID(PROTOTYPE):
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_socket_type, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
    };
    return ilu_FALSE;
  } else {
#ifdef SO_REUSEADDR
    (void) setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,
		      (ilu_string) NIL, 0);
#endif				/* SO_REUSEADDR */

#ifdef SO_USELOOPBACK
    (void) setsockopt(fd, SOL_SOCKET, SO_USELOOPBACK,
		      (ilu_string) NIL, 0);
#endif				/* SO_USELOOPBACK */

    /*
     * The following was suggested by Ken Birman of the Cornell ISIS
     * project. On SunOS it prevents the kernel from delaying this
     * packet in hopes of merging it with a quickly-following one.
     */
#ifdef IPPROTO_TCP
#ifdef TCP_NODELAY
/*
 * This fails when IPPROTO_TCP or TCP_NODELAY is a constant instead
 * of a macro!
 */
    {
      int             one = 1;
      setsockopt(fd, IPPROTO_TCP, TCP_NODELAY,
		 (char *) &one, sizeof(one));
    }
#else
    "TCP_NODELAY un#defined";
    /* An expression is a valid, if boring, statement. */
#endif				/* TCP_NODELAY */
#else
    "IPPROTO_TCP un#defined";
#endif				/* IPPROTO_TCP */

    if (cp->defaulted) {
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "tcp(%p):  Can't connect to defaulted host.\n",
	     self));
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_networkAddr, 0);
      status = FALSE;
#ifndef XEROX_FIREWALL_HANDLING
      /* firewall code doesn't work with non-blocking connect */
    } else if (OS_SOCKERR(set_doer_non_blocking(fd))) {
      err1 = sockerrno;
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr,
	     "tcp(%p):  Failed to set socket (FD %d, host %s)"
	     " non-blocking, error \"%s\".\n",
	     self, fd, inet_ntoa(sinaddr.sin_addr),
	     strerror(err1)));
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_nonblock, 0);
      status = FALSE;
#endif /* XEROX_FIREWALL_HANDLING */
    } else {
      int             connres;
      connres = connect(fd, (struct sockaddr *) & sinaddr,
			sizeof(sinaddr));
      if (!OS_SOCKERR(connres)) {
	DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	      (stderr,
	       "tcp(%p):  connected to %s:%u on FD %d\n",
	       self, inet_ntoa(sinaddr.sin_addr), port, fd));
	p->fd = fd;
	status = TRUE;
#ifdef XEROX_FIREWALL_HANDLING
	/* do non-blocking after the connect */
	if (OS_SOCKERR(set_doer_non_blocking(fd))) {
	  err1 = sockerrno;
	  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		(stderr,
		 "tcp(%p):  Failed to set socket (FD %d, host %s)"
		 " non-blocking.  Error \"%s\".\n",
		 self, fd, inet_ntoa(sinaddr.sin_addr),
		 strerror(err1)));
	  close(fd);
	  p->fd = -1;
	  ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_nonblock, 0);
	  status = FALSE;
	}
#endif /* XEROX_FIREWALL_HANDLING */
      }
#if (defined(WIN32) || defined(WIN16))
      else if (sockerrno == SOCKERRID(INPROGRESS) ||
               sockerrno == SOCKERRID(WOULDBLOCK))
#else
      else if (sockerrno == SOCKERRID(INPROGRESS))
#endif				/* WIN32 or WIN16 */
      {
	struct sockaddr peername;
	SOCKET_SIZE_TYPE pnlen = sizeof(peername);
	ilu_boolean     sure;
	_ilu_WaitForOutputOnFD(fd, &sure, NIL, err);
	if (
#if (defined (WIN32) || defined (WIN16))
	/*
	 * getpeername is not a good test for connectivity on Win32,
	 * so check to make sure the _ilu_WaitForOutputOnFD returned
	 * because of writability, not because of an exception.
	 */
	    _tcp_writeable_fd(fd) > 0
#else
	    getpeername(fd, &peername, &pnlen) == 0
#endif
	  ) {
	  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		(stderr,
		 "tcp(%p):  eventually connected to %s:%u on FD %d\n",
		 self, inet_ntoa(sinaddr.sin_addr), port, fd));
	  p->fd = fd;
	  status = TRUE;
	} else {
	  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		(stderr,
		 "tcp(%p): connect FD %d to %s:%u failed "
		 "(no meaningful errno available).\n",
		 self, fd, inet_ntoa(sinaddr.sin_addr), port));
	  OS_SOCKLOSE(fd);
	  ILU_ERR_CONS1(comm_failure, err, minor,
			ilu_cfm_connect_failed, 0);
	  status = FALSE;
	}
      } else {
	int erri = sockerrno;
	DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	      (stderr, "tcp(%p): connect FD %d to %s:%u failed,"
	       " error %d (%s)\n",
	       self, fd, inet_ntoa(sinaddr.sin_addr),
	       port, erri, strerror(erri)));
	  OS_SOCKLOSE(fd);
	switch (erri)
	  {
	  case SOCKERRID(ADDRINUSE):
	  case SOCKERRID(ADDRNOTAVAIL):
	  case SOCKERRID(AFNOSUPPORT):
	  case SOCKERRID(FAULT):
	  case SOCKERRID(NETUNREACH):
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_bad_address,
			  0);
	    break;

	  case SOCKERRID(CONNREFUSED):
	    ILU_ERR_CONS1(comm_failure, err, minor,
			  ilu_cfm_connect_refused, 0);
	    break;

	  case SOCKERRID(TIMEDOUT):
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_timeout, 0);
	    break;

	  case SOCKERRID(ALREADY):
	  case SOCKERRID(INTR):
	  case SOCKERRID(ISCONN):
	  case SOCKERRID(BADF):
	  case SOCKERRID(NOTSOCK):
	  default:
	    ILU_ERR_CONS1(comm_failure, err, minor,
			  ilu_cfm_connect_failed, 0);
	    break;
	  }
	status = FALSE;
      }
    }
  }
  return (status);
}

/*L1, L2 unconstrained */
static ilu_boolean 
_tcp_Close(ilu_Transport self, ilu_integer * dfd,
	   ILU_ERRS((internal)) * err);

static struct _ilu_TransportClass_s tcpClass = {
  ilu_FALSE,			/* not boundaried */
  ilu_TRUE,			/* reliable */
  _tcp_CloseDFd,
  SetInputHandler,
  _tcp_WaitForInput,
  _tcp_Interrupt,
  _tcp_BeginMessage,
  _tcp_EndMessage,
  _tcp_SendWholeMessage,
  _tcp_WriteBytes,
  _tcp_ReadBytes,
  _tcp_Close
};

/*L1, L2 unconstrained */
static          ilu_Transport
NewT(ilu_boolean buffer, ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   ans;
  TCPParms parms;
  ans = (ilu_Transport) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  parms = (TCPParms)ilu_malloc(sizeof(*parms));
  if (parms == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*parms), NIL);
  parms->fd = -1;
  parms->inSize = BUFFERSIZE;
  parms->inpReg = FALSE;
  parms->tih = NIL;
  ans->tr_class = &tcpClass;
  ans->tr_data = parms;
  if (buffer) {
    ans->tr_inBuff = (ilu_bytes) malloc(parms->inSize);
    ans->tr_outBuff = (ilu_bytes) malloc(BUFFERSIZE);
    if (ans->tr_inBuff == NIL || ans->tr_outBuff == NIL) {
      ilu_Error       lerr;
      ilu_integer     cdfd;
      _tcp_Close(ans, &cdfd, &lerr);
      ILU_HANDLED(lerr);
      return ILU_ERR_CONS1(no_memory, err, nbytes,
	      (ans->tr_inBuff == NIL) ? parms->inSize : BUFFERSIZE,
			   ans);
    }
    ans->tr_inLimit = 0;
  } else {
    ans->tr_inBuff = ans->tr_outBuff = NIL;
    ans->tr_inLimit = BUFFERSIZE;	/* have no mercy on bugs */
  }
  ans->tr_outLimit = BUFFERSIZE;
  ans->tr_inNext = ans->tr_outNext = 0;
  ILU_CLER(*err);
  return ans;
}

/*Main Invariant holds*/
static ilu_Transport
  _tcp_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
		       ilu_integer * dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_Transport   ans;
  *dfd = 0;
  ans = NewT(buffer, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (_tcp_Connect(ans, cp, err))
    *dfd = 1;
  else {
    ilu_Error       lerr;
    ilu_integer     cdfd;
    _tcp_Close(ans, &cdfd, &lerr);
    ILU_HANDLED(lerr);
    return NIL;
  }
  return ans;
}


/*L1, L2 unconstrained*/
static          ilu_boolean
_tcp_Close(ilu_Transport self, ilu_integer * dfd,
	   ILU_ERRS((internal)) * err)
{
  TCPParms        parms;
  *dfd = 0;
  if (self == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, FALSE);
  parms = (TCPParms) self->tr_data;
  ILU_CLER(*err);
  if (parms->fd >= 0) {
    ilu_UnregisterInputSource(parms->fd);
    ilu_UnregisterOutputSource(parms->fd);
    OS_SOCKLOSE(parms->fd);
    *dfd = 1;
  }
  ilu_free(parms->tih);
  ilu_free(self->tr_inBuff);
  ilu_free(self->tr_outBuff);
  ilu_free(parms);
  ilu_free(self);
  return TRUE;
}

/**********************************************************************
***********************************************************************
***********************************************************************
**** Now the methods for the TCP Mooring ******************************
***********************************************************************
***********************************************************************
**********************************************************************/

/*L1, L2, Main unconstrained*/

static ilu_string
_tcp_FormInfo(ilu_string hostname, ilutcpport_t port,
	      ilu_boolean altfmt, ILU_ERRS((no_memory)) * err)
{
  ilu_string      retval;
  char            buf[1000];
  sprintf(buf, altfmt ? "TCP %s %u" : "tcp_%s_%u", hostname,
	  ((unsigned) port) & 0xFFFF);
  if ((retval = _ilu_Strdup(buf)) == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, strlen(buf) + 1, NIL);
  ILU_CLER(*err);
  return retval;
}

/*L1_sup < trmu*/
static          ilu_Transport
_tcp_AcceptClient(ilu_Mooring self, ilu_string *tinfo_out,
		  ilu_integer * dfd, ilu_Passport pp,
		  ILU_ERRS((IoErrs)) * err)
{
  MooringParms    mParms;
  TCPParms        tParms;
  int             ns;
  struct sockaddr_in sinaddr;
  ilu_Transport   newT = NIL;
  SOCKET_SIZE_TYPE addrlen;
  int             mfd;
  *dfd = 0;
  if (self == NIL)
    return (NIL);
    mParms = (MooringParms) self->mo_data;
  mfd = mParms->fd;
  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	(stderr,
	 "tcp(%p): accepting connection request on FD %d.\n",
	 self, mfd));
  addrlen = sizeof(sinaddr);
  if (OS_SOCKINV(ns = OS_ACCEPT(mfd, (struct sockaddr *) & sinaddr,
				&addrlen))) {
    int             theerr = sockerrno;
    ilu_no_resources_Minor nrm;
    switch (theerr) {
    case SOCKERRID(WOULDBLOCK):
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "tcp(%p): Spurrious call on accept(FD %d).\n",
	     self, mfd));
      ILU_CLER(*err);
      return NIL;
    case SOCKERRID(MFILE):
      nrm = ilu_nrm_EMFILE;
      goto raise_no_resources;
    case SOCKERRID(NOBUFS):
      nrm = ilu_nrm_ENOBUFS;
  raise_no_resources:
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr,
	     "tcp(%p): accept(FD %d) => errno %d, %s.\n",
	     self, mfd, theerr, strerror(theerr)));
      return ILU_ERR_CONS1(no_resources, err, minor, nrm, NIL);
#if (defined(WIN32) || defined(WIN16))
    case WSANOTINITIALISED:
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr,
	     "tcp(%p): Can't accept(FD %d) because WinSock not initialized.\n",
	     self, mfd));
      goto bitch_errno;
#endif
    default:
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "tcp(%p): accept(FD %d) => errno %d.\n",
	     self, mfd, theerr));
  bitch_errno:
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, NIL);
    }
  }
  *dfd = 1;
  if (OS_SOCKERR(set_doer_non_blocking(ns))) {
    int theerr = sockerrno;
    DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	  (stderr,
	   "tcp(%p):  Failed to set socket"
	   " (FD %d) non-blocking.  Error \"%s\".\n",
	   self, ns, strerror(theerr)));
  }
  /* See above comment on similar code below */
#ifdef IPPROTO_TCP
#ifdef TCP_NODELAY
  {
    int             one = 1;
    setsockopt(ns, IPPROTO_TCP, TCP_NODELAY,
	       (char *) &one, sizeof(one));
  }
#endif				/* TCP_NODELAY */
#endif				/* IPPROTO_TCP */

  if (tinfo_out != NIL) {
    if (ilu_EnterMutex(ilu_trmu, err)) {
      *tinfo_out = _tcp_FormInfo(inet_ntoa(sinaddr.sin_addr),
				 ntohs(sinaddr.sin_port), TRUE, err);
      (void) ilu_ExitMutex(ilu_trmu, TRUE, err);
    }
    if (ILU_ERRNOK(*err)) {
      OS_SOCKLOSE(ns);
      *dfd = 0;
      return NIL;
    }
  }
  newT = NewT(mParms->buff_p, err);
  if (newT == NIL)
    return NIL;
  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	(stderr,
	 "tcp(%p): new transport %p, on FD %d,"
	 " from IP host %s, port %u.\n",
	 self, newT, ns, inet_ntoa(sinaddr.sin_addr),
	 ntohs(sinaddr.sin_port)));
  tParms = (TCPParms) newT->tr_data;
  tParms->fd = ns;
  return (newT);
}

static          ilu_boolean
_tcp_CloseMooring(ilu_Mooring m, ilu_integer * dfd,
		  ILU_ERRS((bad_param, internal)) * err)
{
  MooringParms    mp = (MooringParms) m->mo_data;
  int             fd = mp->fd, res, err1;
  if (fd >= 0) {
    ilu_UnregisterInputSource(fd);
    *dfd = 1;
    while (((res = OS_SOCKLOSE(fd)) == -1)
	   && ((err1 = sockerrno) == SOCKERRID(INTR)));
    if (res < 0)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, FALSE);
  } else
    *dfd = 0;
  if (mp != NIL) {
    if (mp->tih != NIL)
      ilu_free(mp->tih);
    ilu_free(mp);
    m->mo_data = NIL;
  }
  return ILU_CLER(*err);
}

/*Main Invariant holds; L2 otherwise unconstrained*/
static void
CallReqHandler(int fd, ilu_private rock)
{
  ilu_TIH        *tp = (ilu_TIH *) rock;
  (*tp->tih_proc) (tp->tih_rock);
  return;
}

/*L1, L2 unconstrained*/
static          ilu_boolean
SetReqHandler(ilu_Mooring self, ilu_Server s,
	      ilu_TransportInputHandler tih, ilu_refany tih_rock,
	      ILU_ERRS((no_memory, imp_limit, no_resources,
			broken_locks, internal)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;
  ilu_TIH         *p = (ilu_TIH *) ilu_malloc(sizeof(*p));
  _ilu_Assert(mp->tih == NIL, "SetReqHandler");
  if (p == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*p), FALSE);
  p->tih_proc = tih;
  p->tih_rock = tih_rock;
  if (!ilu_RegisterInputSource(mp->fd, CallReqHandler, p))
    return ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mlreg, FALSE);
  mp->tih = p;
  return ILU_CLER(*err);
}

/*Before: L1 = {s},
          forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  After: Main Invariant holds*/
static          ilu_boolean
WaitForReq(ilu_Mooring self, ilu_Server s,
	   ILU_ERRS((interrupted, broken_locks)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;
  int             fd = mp->fd;
  ilu_boolean     sure;
  if (!ilu_ExitMutex(s->sr_lock, TRUE, err))
    return FALSE;
  _ilu_WaitForInputOnFD(fd, &sure, NIL, err);
  return ILU_ERROK(*err);
}

/*L1.sup < trmu; L2 unconstrained*/
static ilu_integer MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  return 1;
}

/*L1, L2 unconstrained*/
static struct _ilu_Mooring_s mooringProto = {
  MooringDFd,
  SetReqHandler,
  WaitForReq,
  _tcp_AcceptClient,
  _tcp_CloseMooring,
  NIL				/* data */
};

/*L1_sup < trmu*/
static ilu_Mooring 
_tcp_CreateMooring(ilu_TransportCreator self,
		   ilu_TransportInfo * tinfo_out,
		   ilu_boolean buffer,
		   ilu_integer *dfd,
		   ilu_Passport pp,	/* unused here */
		   ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  MooringParms    mp = NIL;
  struct sockaddr_in sinaddr = cp->addr;
  struct linger   linger = {0};
  SOCKET_SIZE_TYPE namelen;
  int             skt = -1;
  int             on = 1;
  ilu_Mooring     ans = NIL;
  int             err1;
  ilu_boolean     loopedback = FALSE;

  _ilu_HandleSigPIPE();
  *dfd = 0;

  ILU_CLER(*err);
  if (tinfo_out != NIL)
    *tinfo_out = NIL;

  /* setup the new server */

  if (OS_SOCKINV(skt = socket(AF_INET, SOCK_STREAM, 0))) {
    err1 = sockerrno;
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr, "tcp(%p): create Mooring socket failed:  %s.\n",
	   self, strerror(err1)));
    switch (err1) {
#if !(defined(WIN32) || defined(WIN16))
    case SOCKERRID(ACCES):		/* permission error */
      ILU_ERR_CONS0(no_permission, err, 0);
      break;

    case SOCKERRID(NFILE):
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENFILE, 0);
      break;
#endif

    case SOCKERRID(MFILE):		/* resource error */
    case SOCKERRID(NOBUFS):
      ILU_ERR_CONS1(no_resources, err, minor,
		    (err1 == EMFILE) ? ilu_nrm_EMFILE : ilu_nrm_ENOBUFS,
		    0);
      break;

    case SOCKERRID(PROTONOSUPPORT):	/* type error */
    case SOCKERRID(PROTOTYPE):
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_socket_type, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
    };
    return (NIL);
  }
  *dfd = 1;
  setsockopt(skt, SOL_SOCKET, SO_REUSEADDR, (char *) &on, sizeof(on));
  linger.l_onoff = 0;
  setsockopt(skt, SOL_SOCKET, SO_LINGER,
	     (char *) &linger, sizeof(struct linger));

  while (1) {
    int             err2;
    if (!OS_SOCKERR(bind(skt, (struct sockaddr *) & sinaddr,
			 sizeof(sinaddr))))
      break;
    err2 = sockerrno;
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr,
	   "tcp(%p): bind to %08lx:%u failed:  %s.\n",
	   self, ntohl(sinaddr.sin_addr.s_addr), ntohs(sinaddr.sin_port),
	   strerror(err2)));
    switch (err2) {
    case SOCKERRID(ADDRNOTAVAIL):
      /*
       * We should get this error when name-to-addr(hostname) isn't
       * a currently valid address of this machine; use the loopback
       * address instead.  Someday we'll clone the implementation of
       * `ifconfig` and go looking for a currently valid IP address.
       */
      if (!loopedback) {
	/*
	 * Set desired address to LOOPBACK, try again.  Linux won't
	 * let us bind(2) to INADDR_LOOPBACK --- but it will let us
	 * bind to INADDR_ANY.  So we bind to that, and report
	 * "127.0.0.1" (the loopback address) in the tinfo_out.
	 */
	sinaddr.sin_addr.s_addr = INADDR_ANY;
	loopedback = TRUE;
	goto tryagain;
      }
      /* else fall through */

    case SOCKERRID(ADDRINUSE):
    case SOCKERRID(FAULT):
    case SOCKERRID(INVAL):
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_bad_address, 0);
      break;

    case SOCKERRID(ACCES):	/* permission error */
      ILU_ERR_CONS0(no_permission, err, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
      break;
    };
    OS_SOCKLOSE(skt);
    *dfd = 0;
    goto failc;
tryagain:
    0;
  }
  
  /* If tinfo gave port 0 then discover kernel allocated port */
  if (sinaddr.sin_port == 0) {
    namelen = sizeof(sinaddr);
    if (OS_SOCKERR(getsockname(skt, (struct sockaddr *) & sinaddr,
			       &namelen))) {
      int             err2 = sockerrno;
      DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	 (stderr, "tcp(%p): getsockname failed:  %s.\n",
	  self, strerror(err2)));
      OS_SOCKLOSE(skt);
      *dfd = 0;
      switch (err2) {
      case SOCKERRID(FAULT):
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_badPointer, 0);
	break;

      case SOCKERRID(BADF):
      case SOCKERRID(NOTSOCK):
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_fd, 0);
	break;

      case SOCKERRID(NOBUFS):
	ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENOBUFS, 0);
	break;

      default:
	ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
	break;
      };
      goto failc;
    }
  }
  if (OS_SOCKERR(listen(skt, 4))) {
    int             err2 = sockerrno;
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr,
	   "tcp(%p):  listen on port %u, FD %d failed:  %s.\n",
	   self, ntohs(sinaddr.sin_port), skt, strerror(err2)));
    OS_SOCKLOSE(skt);
    *dfd = 0;
    switch (err2) {
    case SOCKERRID(BADF):
    case SOCKERRID(NOTSOCK):
    case SOCKERRID(OPNOTSUPP):
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_fd, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
      break;
    };
    goto failc;
  }
  if (OS_SOCKERR(set_listener_non_blocking(skt))) {
    int             err2 = sockerrno;
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr,
	   "tcp(%p):  Unable to set listening socket (FD %d) non-blocking: %s\n",
	   self, skt, strerror(err2)));
    OS_SOCKLOSE(skt);
    *dfd = 0;
    ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
    goto failc;
  }
  ans = (ilu_Mooring) ilu_MallocE(sizeof(*ans), err);
  if (ILU_ERRNOK(*err))
    goto faild;
  mp = (MooringParms) ilu_MallocE(sizeof(*mp), err);
  if (ILU_ERRNOK(*err))
    goto faild;
  *ans = mooringProto;
  ans->mo_data = mp;
  mp->tih = NIL;
  mp->fd = skt;
  mp->buff_p = buffer;
  DEBUG(TCP_DEBUG,
	(stderr, "tcp(%p): created Mooring %p on FD %d, port %u.\n",
	 self, ans, skt, ntohs(sinaddr.sin_port)));
  if (tinfo_out != NIL) {
    ilu_string      hostname, tinfo;
    if (!loopedback)
      hostname = cp->tinfo_host;
    else if (cp->name_given)
      hostname = "localhost";
    else
      hostname = "127.0.0.1";
    tinfo = _tcp_FormInfo(hostname, ntohs(sinaddr.sin_port), FALSE, err);
    if (ILU_ERRNOK(*err))
      goto faild;
    * tinfo_out = ilu_MallocE((2 * SIZEOF_VOID_P) + strlen(tinfo) + 1,
			      err);
    if (ILU_ERRNOK(*err))
      goto faild;
    (*tinfo_out)[0] = ((char *) (*tinfo_out)) + (2 * SIZEOF_VOID_P);
    (*tinfo_out)[1] = NIL;
    strcpy((*tinfo_out)[0], tinfo);
    ilu_free(tinfo);
  }
  return (ans);
faild:
  if (skt >= 0)
    OS_SOCKLOSE(skt);
  *dfd = 0;
failc:
  if (ans != NIL)
    ilu_free(ans);
  if (mp != NIL)
    ilu_free(mp);
  if (tinfo_out != NIL && *tinfo_out != NIL) {
    ilu_free(*tinfo_out);
    *tinfo_out = NIL;
  }
  return NIL;
}

static void 
_tcp_CloseCreator(ilu_TransportCreator self)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_free(cp->tinfo_host);
  ilu_free(cp);
  ilu_free(self);
  return;
}

static struct _ilu_TransportCreator_s creatorProto = {
  FALSE,			/* !boundaried */
  TRUE,				/* reliable */
  _tcp_FdUsage,
  _tcp_CreateTransport,
  _tcp_CreateMooring,
  _tcp_CloseCreator,
  NIL				/* data */
};

/*L1_sup < trmu*/
ilu_TransportCreator
_ilu_tcp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  ilu_TransportCreator ans;
  CreatorParms    cp;
  _ilu_AutoSetDebugLevel();
  cp = _tcp_InterpretInfo(tinfo[0], err);
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_TransportCreator) ilu_malloc(sizeof(*ans));
  *ans = creatorProto;
  ans->tcr_data = cp;
  return (ans);
}


