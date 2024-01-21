/*
Copyright (c) 1991-1994 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: bsdutils.c,v 1.34 1996/07/16 23:15:03 spreitze Exp $ */
/* Last edited by Mike Spreitzer July 16, 1996 4:14 pm PDT */

#include <signal.h>	/* for SIGPIPE, SIG_IGN, SIG_DFL */
#include <limits.h>	/* for INT_MAX */

#include "iluntrnl.h"
#include "oscalls.h"
#include "ilusock.h"


/* ================ BSD/WIN Hostname ================ */

/*L1, L2, Main unconstrained*/

#if (defined(_IS_BSD) || defined(WIN32) || defined(WIN16))

#if 0
/*
 * Linux doesn't like "int", and IRIX doesn't like "size_t", so
 * there's no way to make this declaration without getting a fatal
 * error.  So can it.  Who wanted it, anyway?
 */
#ifdef _IS_BSD
extern int      gethostname(char *, int);
#endif
#endif

ilu_string _ilu_Hostname(void)
{
  static ilu_string name = NIL;
  if (name == NIL) {
    char            hostname[1000];
    _ilu_Assert(gethostname(hostname, sizeof(hostname)) == 0,
		"gethostname failed");
    name = _ilu_Strdup(hostname);
  }
  return (name);
}

#elif defined(_IS_POSIX)

#include <sys/utsname.h>	/* for struct utsname */

ilu_string _ilu_Hostname(void)
{
  ilu_string      name = NIL;
  struct utsname  names;
  if (OS_UNAME(&names) >= 0)
    name = _ilu_Strdup(names.nodename);
  return (name);
}

#endif

/*L1 >= {trmu}; L2 unconstrained*/
ilu_string
_ilu_CurrentHostIPAddrString(ilu_string * host_out,
			     struct in_addr * addr_out,
			     ILU_ERRS((IoErrs)) * err)
{
  static ilu_string tempname, inetname = NIL;
  static struct in_addr myaddr;
  static char    *myname;
  struct hostent *he;
  struct in_addr *hea;
  if (inetname == NIL) {
    if ((myname = _ilu_Hostname()) == NIL) {
#if (defined(WIN32) || defined(WIN16))
      ILU_ERRPRINTF("no hostname for this machine! WSAGetLastError() = %i\n",
		    WSAGetLastError());
#else
      perror("no hostname for this machine!");
#endif
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_noHostName, NIL);
    }
    he = gethostbyname(myname);
    if (he == NIL || he->h_addr_list == NIL || he->h_addr_list[0] == NIL)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_noHostIpAddr,
			   NIL);
    hea = (struct in_addr *) (he->h_addr_list[0]);
    tempname = (ilu_string) inet_ntoa(myaddr = *hea);
    inetname = ilu_StrdupE(tempname, err);
    if (ILU_ERRNOK(*err))
      return NIL;
  }
  ILU_CLER(*err);
  if (addr_out != NIL)
    *addr_out = myaddr;
  if (host_out != NIL)
    *host_out = myname;
  return (inetname);
}

/* ================ BSD/WIN ilu_FineTime ================ */

/*L1, L2, Main unconstrained*/

#if (defined(_IS_BSD) || defined(WIN32) || defined(WIN16))

/*
 * "ilu_FineTime_Now" and "ilu_CoarseTime_Now" should be implemented
 * in the same module, because {ilu_CoarseTime_Now(), 0} needs to be
 * the corresponding ilu_FineTime, and both could use a different
 * origin than 1970.0.
 */

#include <time.h>	/* ANSI C req.s this for time() */

#ifdef _IS_BSD

#include <sys/types.h>	/* for time() */
#include <sys/time.h>	/* for time(), gettimeofday */

const ilu_cardinal ilu_FineTimeRate = 1000000;

ilu_FineTime ilu_FineTime_Now()
{
  struct timeval tv;
  ilu_FineTime ans;
  ASSERT(gettimeofday(&tv, NIL) == 0, buf,
	 (buf, "UNIX time.c:gettimeofday failed, errno=%d", errno));
  ans.ft_s = tv.tv_sec;
  ans.ft_t = tv.tv_usec;
  return ans;
}

#elif defined(WIN32)	/* must be WIN */

#include <sys/types.h>
#include <sys/timeb.h>
#include <process.h>

const ilu_cardinal    ilu_FineTimeRate = 1000;	/* milliseconds */
/* xxx note dll - don't think there is anyway to get usec on a PC! */

ilu_FineTime ilu_FineTime_Now()
{
  struct _timeb   tv;
  ilu_FineTime    ans;
  _ftime(&tv);
  ans.ft_s = tv.time;
  ans.ft_t = tv.millitm;
  return ans;
}

#else

#error "Don't know how to get FineTime for this OS!"

#endif /* _IS_BSD */

ilu_integer ilu_CoarseTime_Now()
{
  return time(NIL);
}

/* ================= BSD/WIN InventID ======================== */

/*L1, L2, Main unconstrained*/

static int need_seed = 1;

/*L1_sup < smu*/
ilu_string ilu_InventID ()
{
  char			buf[1000];
  ilu_FineTime		now;
  static ilu_string	iphostname = NIL;
  ilu_Error		lerr;

  _ilu_AcquireMutex(ilu_smu);
  if (need_seed) {
    srand(OS_GETPID());
    need_seed = 0;
  }
  if (iphostname == NIL)
    {
      /* use HostIPAddr instead of Hostname to get domain info */
      iphostname = _ilu_CurrentHostIPAddrString(NIL, NIL, &lerr);
      if (ILU_ERRNOK(lerr))
	{
	  iphostname = "unknownhost";
	  ILU_HANDLED(lerr);
	}
    }
  now = ilu_FineTime_Now();
  sprintf(buf, "%s.%lx.%lx.%lx", iphostname,
	  (long unsigned) OS_GETPID(),
	  (long unsigned) now.ft_s,
	  (long unsigned) rand());
  _ilu_ReleaseMutex(ilu_smu);
  return _ilu_Strdup(buf);
}

#endif /* BSD or WIN */

/* =================== SIGPIPE handling ==================*/

/*L2, Main unconstrained*/

/*L1 >= {trmu}*/
static ilu_boolean SigPIPEHandler = FALSE;

/*L1_sup < trmu*/

void
_ilu_HandleSigPIPE(void)
{
  /*
   * Ignore SIGPIPEs, since we can occasionally get them. [Thanks
   * to Dave Nichols <nichols@parc.xerox.com> for the original idea
   * of this code.]
   */

#if defined(_IS_POSIX) && defined(SA_NOCLDSTOP)

  _ilu_AcquireMutex(ilu_trmu);
  if (!SigPIPEHandler) {

    struct sigaction old_handler;
    static struct sigaction new_handler;

    if (sigaction(SIGPIPE, NIL, &old_handler) == 0) {
      if (old_handler.sa_handler == SIG_DFL) {	/* no one's using it */
	new_handler.sa_handler = SIG_IGN;
	if (sigaction(SIGPIPE, &new_handler, NIL) != 0) {
	  ILU_ERRPRINTF("_ilu_HandleSIGPIPE:  Couldn't ignore SIGPIPE signals!\n");
	}
      }
    } else
      ILU_ERRPRINTF("_ilu_HandleSIGPIPE:  Couldn't read handler for SIGPIPE!\n");
  }
  SigPIPEHandler = TRUE;
  _ilu_ReleaseMutex(ilu_trmu);

#elif (defined(WIN32) || defined(WIN16))

 /* do nothing for win32 as there is no SIGPIPE */

#else		/* not really POSIX -- use ANSI C form */

  typedef void    (*handler) (int);
  _ilu_AcquireMutex(ilu_trmu);
  if (!SigPIPEHandler) {
    handler         old_handler;

    if ((old_handler = (handler) signal(SIGPIPE, SIG_IGN)) != SIG_DFL)
      /* Oops!  Someone's using this one */
      signal(SIGPIPE, old_handler);

    SigPIPEHandler = TRUE;
  }
  _ilu_ReleaseMutex(ilu_trmu);

#endif				/* _IS_POSIX */
}

ilu_boolean
ilu_SIGPIPE_Handled(void)
{
  _ilu_AcquireMutex(ilu_trmu);
  if (!SigPIPEHandler) {
    SigPIPEHandler = TRUE;
    _ilu_ReleaseMutex(ilu_trmu);
    return TRUE;
  }
  _ilu_ReleaseMutex(ilu_trmu);
  return FALSE;
}

/* ================ BSD/WIN Socket I/O ================ */

/*Main Invariant holds*/
/*L2 >= {fd's connection's callmu, iomu}*/

ilu_cardinal
_ilu_NbSockRead(int fd, unsigned char *buf, ilu_cardinal bufLen,
		ilu_TransportReport * rpt,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    reql = bufLen;
  int             req, got, theerr;
  if (reql > INT_MAX)
    reql = INT_MAX;
  req = (int) reql;
  rpt->tr_eof = FALSE;
  while (1) {
#ifdef HAS_SOLARIS2_TCP_SOCKET_BUG
    got = read(fd, buf, req);
#else
    got = recv(fd, buf, req, 0);
#endif
    theerr = sockerrno;
    if (!OS_SOCKERR(got)) {
      rpt->tr_eof = (got == 0);
      if (got == 0)
	DEBUG(CONNECTION_DEBUG,
	      (stderr,
	       "_ilu_NbSockRead: Clean EOF detected on FD %d.\n",
	       fd));
      goto dun;
    }
    switch (theerr) {
    case SOCKERRID(INPROGRESS):
      /*ilu_DebugPrintf("NbSockRead(%d) got EINPROGRESS!\n", fd);*/
#if (defined(EWOULDBLOCK) || defined(WSAEWOULDBLOCK))
    case SOCKERRID(WOULDBLOCK):
#elif (defined(_IS_POSIX))
    case SOCKERRID(AGAIN):
#else
#error "no applicable errno error for 'Would Block'"
#endif
      got = 0;
      goto dun;
    case SOCKERRID(INTR):
      break;
    case SOCKERRID(NETDOWN):
    case SOCKERRID(CONNABORTED):
    case SOCKERRID(CONNRESET):
#if !(defined(WIN32) || defined(WIN16))
    case SOCKERRID(PIPE):
#endif
      DEBUG(CONNECTION_DEBUG,
	    (stderr,
	     "_ilu_NbSockRead: Connection failure (errno %d) from recv(%d).\n",
	     theerr, fd));
      rpt->tr_eof = TRUE;
      got = 0;
      goto dun;
    default:
      DEBUG(CONNECTION_DEBUG,
	    (stderr,
	 "_ilu_NbSockRead: Impossible errno (%d) from recv(%d).\n",
	     theerr, fd));
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno,
			   0);
    }
  }
dun:
  ILU_CLER(*err);
  return got;
}

ilu_boolean
_ilu_SockWrite(int fd, unsigned char *buf, ilu_cardinal nbytes,
	       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    reql, sofar = 0;
  int             req, got, theerr;
  ilu_boolean     sure = FALSE, first = TRUE;
  while (sofar < nbytes) {
    ilu_boolean     wasfirst = first;
    if (first)
      first = FALSE;
    else {
      _ilu_WaitForOutputOnFD(fd, &sure, NIL, err);
      if (ILU_ERRNOK(*err))
	return FALSE;
    }
    reql = nbytes - sofar;
    if (reql > INT_MAX)
      reql = INT_MAX;
    req = (int) reql;
#ifdef HAS_SOLARIS2_TCP_SOCKET_BUG
    got = write(fd, buf + sofar, req);
#else
    got = send(fd, buf + sofar, req, 0);
#endif
    theerr = sockerrno;
    if (!OS_SOCKERR(got)) {
      sofar += (unsigned) got;
      continue;
    }
    switch (theerr) {
#if (defined(EWOULDBLOCK) || defined(WSAEWOULDBLOCK))
    case SOCKERRID(WOULDBLOCK):
#elif (defined(_IS_POSIX))
    case SOCKERRID(AGAIN):
#else
#error "no applicable errno error for 'Would Block'"
#endif
      if (sure)
	DEBUG(CONNECTION_DEBUG,
	      (stderr,
	       "_ilu_SockWrite: Sure write to FD %d WOULDBLOCK!\n",
	       fd));
    case SOCKERRID(INTR):
      break;
    case SOCKERRID(NOBUFS):
    case SOCKERRID(NETRESET):
    case SOCKERRID(NETDOWN):
    case SOCKERRID(CONNABORTED):
    case SOCKERRID(CONNRESET):
#if !(defined(WIN32) || defined(WIN16))
    case SOCKERRID(PIPE):
#endif
      DEBUG(CONNECTION_DEBUG,
	    (stderr,
       "_ilu_SockWrite: Connection failure (errno %d) on FD %d.\n",
	     theerr, fd));
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			   FALSE);
    default:
      DEBUG(CONNECTION_DEBUG,
	    (stderr,
	     "_ilu_SockWrite: Impossible errno (%d) for FD %d.\n",
	     theerr, fd));
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, FALSE);
    }
  }
  return TRUE;
}

