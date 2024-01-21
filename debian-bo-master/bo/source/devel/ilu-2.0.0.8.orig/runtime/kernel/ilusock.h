/* $Id: ilusock.h,v 1.5 1996/02/20 06:23:00 spreitze Exp $ */
/* Last edited by Mike Spreitzer February 19, 1996 9:36 pm PST */

#ifndef _ILU_SOCKET_HEADERS
#define _ILU_SOCKET_HEADERS

#include <sys/types.h>

#if (defined(WIN32) || defined(WIN16)) 
/* sockets are in winsock.h for WIN32 */
#include <winsock.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>		/* for inet_addr, inet_ntoa */
#include <netdb.h>
#endif /* not WIN32 */

/* BSD socket network calls used:

   accept, bind, connect, getsockname, listen, setsockopt, socket, getpeername
   gethostbyname, inet_addr, inet_ntoa, ntohs

   We also use the "close" system call to close socket connections.
*/

#ifdef __GNU_LIBRARY__
typedef size_t SOCKET_SIZE_TYPE;
#else /* doesn't define size_t */
typedef int SOCKET_SIZE_TYPE;
#endif

/*L1 >= {trmu}; L2 unconstrained*/
ilu_string
_ilu_CurrentHostIPAddrString(ilu_string * host_out,
			     struct in_addr * addr_out,
			     ILU_ERRS((IoErrs)) * err);
/*
 * Returns an IP address, in string form, for this host.  Result
 * will never be freed.  (host_out) and (addr_out) are OUT
 * parameters, and may independently be NIL to indicate the caller
 * doesn't care to receive the respective value.  If (host_out !=
 * NIL), a statically allocated string will be returned; that string
 * will be a host name if possible, perhaps an address string
 * otherwise.  Sadly, this interface makes the fallacious assumption
 * that the answer will be valid forevermore.
 */

#endif /* ndef _ILU_SOCKET_HEADERS */
